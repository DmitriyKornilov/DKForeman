unit UDataBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, DateUtils,

  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_Dialogs, DK_StrUtils, DK_Const;

type

  { TDataBase }

  TDataBase = class (TSQLite3)
  public
    (**************************************************************************
                                     СПРАВОЧНИКИ
    **************************************************************************)
    procedure PostDictionaryLoad(const AComboBox: TComboBox;
                                 out APostIDs: TIntVector;
                                 const ASelectPostID: Integer = -1);



    (**************************************************************************
                                         ШТАТ
    **************************************************************************)

    {Cписок людей: True - ОК, False - список пуст;
     AOrderType - сортировка: 0-ФИО, 1-табельный номер, 2-должность, 3-дата рождения,
                              4-дата приема, 5-дата увольнения, 6-разряд;
     AListType - включить в список: 0-всех, 1-работающих, 2-уволенных, 3-без таб.№}
    function StaffListLoad(const AOrderType, AListType: Byte;
                           out AStaffIDs, ATabNumIDs, AGenders: TIntVector;
                           out ABornDates, ARecrutDates, ADismissDates: TDateVector;
                           out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;


    {Добавление данных нового человека: True - ОК, False - ошибка}
    function StaffMainAdd(out AStaffID: Integer;
                          const AFamily, AName, APatronymic: String;
                          const ABornDate: TDate; const AGender: Byte): Boolean;
    {Обновление данных человека: True - ОК, False - ошибка}
    function StaffMainUpdate(const AStaffID: Integer;
                          const AFamily, AName, APatronymic: String;
                          const ABornDate: TDate; const AGender: Byte): Boolean;
    {Получение данных человека: True - ОК, False - нет такого ID}
    function StaffMainLoad(const AStaffID: Integer;
                          out AFamily, AName, APatronymic: String;
                          out ABornDate: TDate; out AGender: Byte): Boolean;
    {Удаление данных человека: True - ОК, False - ошибка}
    function StaffMainDelete(const AStaffID: Integer): Boolean;
    {Получение списка людей: True - ОК, False - список пуст
     AFilterValue - фильтр по Ф.И.О.}
    function StaffMainListLoad(const AFilterValue: String;
                          out AStaffIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;


    {Список табельных номеров по ID человека: True - ОК, False - список пуст}
    function StaffTabNumListLoad(const AStaffID: Integer;
                          out ATabNumIDs, APostIDs: TIntVector;
                          out ATabNums, APostNames, ARanks: TStrVector;
                          out ARecrutDates, ADismissDates: TDateVector): Boolean;
    {Добавление нового таб. номера: True - ОК, False - ошибка}
    function StaffTabNumAdd(out ATabNumID: Integer;
                          const AStaffID: Integer;
                          const ATabNum: String;
                          const ARecrutDate: TDate): Boolean;
    {Обновление данных о приеме нового таб. номера: True - ОК, False - ошибка}
    function StaffTabNumUpdate(const ATabNumID: Integer;
                          const ATabNum: String;
                          const ARecrutDate: TDate): Boolean;
    {Обновление даты увольнения таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDismiss(const ATabNumID: Integer; const ADismissDate: TDate): Boolean;
    {Отмена увольнения таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDismissCancel(const ATabNumID: Integer): Boolean;
    {Удаление таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDelete(const ATabNumID: Integer): Boolean;
    {Проверка наличия таб. номера в записи с ID<>ATabNumID: True - да, False - нет}
    function StaffTabNumIsExists(const ATabNumID: Integer; const ATabNum: String): Boolean;


    {Актуальная постоянная должность таб номера на дату}
    procedure StaffPostForDate(const ATabNumID: Integer;
                               const ADate: TDate;
                               out APostID: Integer;
                               out APostName, ARank: String);
    {Список переводов по ID таб. номера: True - ОК, False - список пуст}
    function StaffPostLogListLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
    {Новая запись (перевод с последней должности) в таблице переводов: True - ОК, False - ошибка}
    function StaffPostLogAdd(const APostLogID, ATabNumID, APostID, APostTemp: Integer;
                          const ARank: String;
                          const AFirstDate: TDate): Boolean;
    {Обновление записи в таблице переводов: True - ОК, False - ошибка}
    function StaffPostLogUpdate(const APrevPostLogID, APostLogID, APostID, APostTemp: Integer;
                          const ARank: String;
                          const AFirstDate: TDate): Boolean;
    {Удаление записи из таблицы переводов: True - ОК, False - ошибка}
    function StaffPostLogDelete(const APrevPostLogID, APostLogID: Integer;
                          const ALastDate: TDate): Boolean;

  end;

var
  DataBase: TDataBase;

implementation

{ TDataBase }

procedure TDataBase.PostDictionaryLoad(const AComboBox: TComboBox;
                                       out APostIDs: TIntVector;
                                       const ASelectPostID: Integer = -1);
begin
  KeyPickLoad(AComboBox, APostIDs, 'STAFFPOST', 'PostID', 'PostName', 'PostName',
              True{ID>0}, EmptyStr, ASelectPostID);
end;

function TDataBase.StaffListLoad(const AOrderType, AListType: Byte;
                               out AStaffIDs, ATabNumIDs, AGenders: TIntVector;
                               out ABornDates, ARecrutDates, ADismissDates: TDateVector;
                               out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;
var
  SQLStr: String;
  i, PostID: Integer;
  PostName, Rank: String;
  Indexes: TIntVector;
begin
  Result:= False;

  AStaffIDs:= nil;
  ATabNumIDs:= nil;
  AGenders:= nil;
  ABornDates:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;
  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;

  //список людей на текущую дату
  SQLStr:=
    'SELECT t1.StaffID, t1.Name, t1.Patronymic, t1.Family, t1.BornDate, t1.Gender, '+
           'tt.TabNumID, tt.TabNum, tt.RecrutDate, tt.DismissDate ' +
    'FROM STAFFMAIN t1 ' +
    'LEFT OUTER JOIN ( ' +
           'SELECT t2.StaffID, t2.TabNumID, t2.TabNum, t2.RecrutDate, t2.DismissDate ' +
           'FROM STAFFTABNUM t2 ' +
           ') tt ON (tt.StaffID=t1.StaffID) ' ;

  if AListType>0 then
  begin
    SQLStr:= SQLStr + 'WHERE ';
    case AListType of
    1: SQLStr:= SQLStr + '(((tt.DismissDate >= :ADate) OR (tt.DismissDate IS NULL)) AND (tt.TabNum IS NOT NULL)) ';
    2: SQLStr:= SQLStr + '(((tt.DismissDate < :ADate) OR (tt.DismissDate IS NULL)) AND (tt.TabNum IS NOT NULL)) ';
    3: SQLStr:= SQLStr + '(tt.TabNum IS NULL) ';
    end;
  end;
  SQLStr:= SQLStr + 'ORDER BY ';
  case AOrderType of
  0,2: SQLStr:= SQLStr + 't1.Family, t1.Name, t1.Patronymic ';
  1: SQLStr:= SQLStr + 'tt.TabNum, t1.Family, t1.Name, t1.Patronymic ';
  3: SQLStr:= SQLStr + 't1.BornDate, t1.Family, t1.Name, t1.Patronymic ';
  4: SQLStr:= SQLStr + 'tt.RecrutDate, t1.Family, t1.Name, t1.Patronymic ';
  5: SQLStr:= SQLStr + 'tt.DismissDate, t1.Family, t1.Name, t1.Patronymic ';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('ADate', Date);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStaffIDs, QFieldInt('StaffID'));
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AGenders, QFieldInt('Gender'));
      VAppend(ABornDates, QFieldDT('BornDate'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));
      VAppend(ADismissDates, QFieldDT('DismissDate'));
      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на текущую дату
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], Date, PostID, PostName, Rank);
    VAppend(APostNames, PostName);
    VAppend(ARanks, Rank);
  end;

  //сортировка по наименованию должности
  if AOrderType<>2 then Exit;
  VSort(APostNames, Indexes);

  AStaffIDs:= VReplace(AStaffIDs, Indexes);
  ATabNumIDs:= VReplace(ATabNumIDs, Indexes);
  AGenders:= VReplace(AGenders, Indexes);
  ABornDates:= VReplace(ABornDates, Indexes);
  ARecrutDates:= VReplace(ARecrutDates, Indexes);
  ADismissDates:= VReplace(ADismissDates, Indexes);
  AFs:= VReplace(AFs, Indexes);
  ANs:= VReplace(ANs, Indexes);
  APs:= VReplace(APs, Indexes);
  ATabNums:= VReplace(ATabNums, Indexes);
  APostNames:= VReplace(APostNames, Indexes);
  ARanks:= VReplace(ARanks, Indexes);
end;

function TDataBase.StaffMainAdd(out AStaffID: Integer;
                                const AFamily, AName, APatronymic: String;
                                const ABornDate: TDate; const AGender: Byte): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись основных данных
    QSetSQL(
      sqlINSERT('STAFFMAIN', ['Family', 'Name', 'Patronymic', 'BornDate', 'Gender', 'FullName'])
    );
    QParamStr('Family', AFamily);
    QParamStr('Name', AName);
    QParamStr('Patronymic', APatronymic, not SEmpty(APatronymic));
    QParamDT('BornDate', ABornDate);
    QParamInt('Gender', AGender);
    QParamStr('FullName', SUpper(SNameLong(AFamily, AName, APatronymic)));
    QExec;
    //получение ID сделанной записи
    AStaffID:= LastWritedInt32ID('STAFFMAIN');
    //запись ID в таблицу размеров СИЗ
    QSetSQL(
      sqlINSERT('SIZSTAFFSIZE', ['StaffID'])
    );
    QParamInt('StaffID', AStaffID);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffMainUpdate(const AStaffID: Integer;
                                const AFamily, AName, APatronymic: String;
                                const ABornDate: TDate; const AGender: Byte): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('STAFFMAIN', ['Family', 'Name', 'Patronymic', 'BornDate', 'Gender', 'FullName']) +
      'WHERE StaffID = :StaffID'
    );
    QParamInt('StaffID', AStaffID);
    QParamStr('Family', AFamily);
    QParamStr('Name', AName);
    QParamStr('Patronymic', APatronymic, not SEmpty(APatronymic));
    QParamDT('BornDate', ABornDate);
    QParamInt('Gender', AGender);
    QParamStr('FullName', SUpper(SNameLong(AFamily, AName, APatronymic)));
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffMainLoad(const AStaffID: Integer;
                                 out AFamily, AName, APatronymic: String;
                                 out ABornDate: TDate; out AGender: Byte): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('STAFFMAIN', ['Family', 'Name', 'Patronymic', 'BornDate', 'Gender']) +
    'WHERE StaffID = :StaffID'
  );
  QParamInt('StaffID', AStaffID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    AFamily:=  QFieldStr('Family');
    AName:= QFieldStr('Name');
    APatronymic:= QFieldStr('Patronymic');
    ABornDate:= QFieldDT('BornDate');
    AGender:= QFieldInt('Gender');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffMainDelete(const AStaffID: Integer): Boolean;
begin
  Result:= Delete('STAFFMAIN', 'StaffID', AStaffID);
end;

function TDataBase.StaffMainListLoad(const AFilterValue: String;
                          out AStaffIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  AStaffIDs:= nil;
  AGenders:= nil;
  ABornDates:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;

  SQLStr:= sqlSELECT('STAFFMAIN', ['StaffID', 'Family', 'Name', 'Patronymic', 'BornDate', 'Gender']);
  if not SEmpty(AFilterValue) then
    SQLStr:= SQLStr + 'WHERE (FullName LIKE :FilterValue)';
  SQLStr:= SQLStr + 'ORDER BY Family, Name, Patronymic, BornDate';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamStr('FilterValue', '%'+AFilterValue+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStaffIDs, QFieldInt('StaffID'));
      VAppend(AGenders, QFieldInt('Gender'));
      VAppend(ABornDates, QFieldDT('BornDate'));
      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffTabNumListLoad(const AStaffID: Integer;
                          out ATabNumIDs, APostIDs: TIntVector;
                          out ATabNums, APostNames, ARanks: TStrVector;
                          out ARecrutDates, ADismissDates: TDateVector): Boolean;
var
  i, PostID: Integer;
  PostName, Rank: String;
begin
  Result:= False;

  ATabNumIDs:= nil;
  APostIDs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;

  if AStaffID<=0 then Exit;
  //табельные номера с периодами работы
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT TabNumID, TabNum, RecrutDate, DismissDate '+
    'FROM STAFFTABNUM ' +
    'WHERE (StaffID = :StaffID) ' +
    'ORDER BY TabNum, RecrutDate'
  );
  QParamInt('StaffID', AStaffID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));
      VAppend(ADismissDates, QFieldDT('DismissDate'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;
  //актуальные должности и разряды на текущую дату
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], Date, PostID, PostName, Rank);
    VAppend(APostIDs, PostID);
    VAppend(APostNames, PostName);
    VAppend(ARanks, Rank);
  end;
end;

function TDataBase.StaffTabNumAdd(out ATabNumID: Integer;
                          const AStaffID: Integer;
                          const ATabNum: String;
                          const ARecrutDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись данных в таблицу табельных номеров
    QSetSQL(
      sqlINSERT('STAFFTABNUM', ['StaffID', 'TabNum', 'RecrutDate', 'DismissDate'])
    );
    QParamInt('StaffID', AStaffID);
    QParamStr('TabNum', ATabNum);
    QParamDT('RecrutDate', ARecrutDate);
    QParamDT('DismissDate', INFDATE);
    QExec;
    //получение ID записанного табельного номера
    ATabNumID:= LastWritedInt32ID('STAFFTABNUM');
    //заносим первую запись в табицу переводов
    QSetSQL(
      sqlINSERT('STAFFPOSTLOG', ['TabNumID', 'FirstDate', 'LastDate'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('FirstDate', ARecrutDate);
    QParamDT('LastDate', INFDATE);
    QExec;
    //заносим первую запись в таблицу персональных графиков
    QSetSQL(
      sqlINSERT('STAFFSCHEDULE', ['TabNumID', 'BeginDate', 'EndDate'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('BeginDate', ARecrutDate);
    QParamDT('EndDate', INFDATE);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffTabNumUpdate(const ATabNumID: Integer;
                                     const ATabNum: String;
                                     const ARecrutDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //обновление данных в таблице таб. номеров
    QSetSQL(
      sqlUPDATE('STAFFTABNUM', ['TabNum', 'RecrutDate']) +
      'WHERE TabNumID = :TabNumID'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamStr('TabNum', ATabNum);
    QParamDT('RecrutDate', ARecrutDate);
    QExec;
    //удаление более ранних периодов из таблицы переводов
    QSetSQL(
      'DELETE FROM STAFFPOSTLOG ' +
      'WHERE (TabNumID = :TabNumID) AND (LastDate < :RecrutDate)'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('RecrutDate', ARecrutDate);
    QExec;
    //меняем данные в таблице переводов
    QSetSQL(
      sqlUPDATE('STAFFPOSTLOG', ['FirstDate']) +
      'WHERE (TabNumID = :TabNumID) AND ' +
                      '(FirstDate = (' +
                             'SELECT FirstDate FROM STAFFPOSTLOG ' +
                             'WHERE (TabNumID = :TabNumID) ' +
                             'ORDER BY FirstDate ' +
                             'LIMIT 1' +
                      ')) '
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('FirstDate', ARecrutDate);
    QExec;
    //удаляем более ранние периоды из таблицы графиков
    QSetSQL(
      'DELETE FROM STAFFSCHEDULE ' +
      'WHERE (TabNumID = :TabNumID) AND (EndDate < :RecrutDate)'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('RecrutDate', ARecrutDate);
    QExec;
    //меняем данные в таблице графиков
    QSetSQL(
      sqlUPDATE('STAFFSCHEDULE', ['BeginDate']) +
      'WHERE (TabNumID = :TabNumID) AND ' +
                      '(BeginDate = (' +
                             'SELECT BeginDate FROM STAFFSCHEDULE ' +
                             'WHERE (TabNumID = :TabNumID) ' +
                             'ORDER BY BeginDate ' +
                             'LIMIT 1' +
                      ')) '
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('BeginDate', ARecrutDate);
    QExec;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffTabNumDismiss(const ATabNumID: Integer; const ADismissDate: TDate): Boolean;
begin
  Result:= UpdateInt32ID('STAFFTABNUM', 'DismissDate', 'TabNumID', ATabNumID, ADismissDate);
end;

function TDataBase.StaffTabNumDismissCancel(const ATabNumID: Integer): Boolean;
begin
  Result:= StaffTabNumDismiss(ATabNumID, INFDATE);
end;

function TDataBase.StaffTabNumDelete(const ATabNumID: Integer): Boolean;
begin
  Result:= Delete('STAFFTABNUM', 'TabNumID', ATabNumID);
end;

function TDataBase.StaffTabNumIsExists(const ATabNumID: Integer; const ATabNum: String): Boolean;
begin
  Result:= IsValueInTableNotMatchInt32ID('STAFFTABNUM', 'TabNum', ATabNum,
                                         'TabNumID', ATabNumID);
end;

procedure TDataBase.StaffPostForDate(const ATabNumID: Integer;
                                     const ADate: TDate;
                                     out APostID: Integer;
                                     out APostName, ARank: String);
begin
  APostName:= EmptyStr;
  ARank:= EmptyStr;
  APostID:= 0;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.Rank, t1.PostID, t2.PostName ' +
    'FROM STAFFPOSTLOG t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'WHERE (t1.TabNumID = :TabNumID) AND (t1.PostTemp = 0) AND (t1.FirstDate <= :DateValue) ' +
    'ORDER BY t1.FirstDate DESC ' +
    'LIMIT 1'
  );
  QParamDT('DateValue', ADate);
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    APostName:= QFieldStr('PostName');
    ARank:= QFieldStr('Rank');
    APostID:= QFieldInt('PostID');
  end;
  QClose;
end;

function TDataBase.StaffPostLogListLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
begin
  Result:= False;

  APostLogIDs:= nil;
  APostIDs:= nil;
  APostTemps:= nil;
  APostNames:= nil;
  ARanks:= nil;
  AFirstDates:= nil;
  ALastDates:= nil;

  if ATabNumID<=0 then Exit;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ID, t1.FirstDate, t1.LastDate, t1.PostTemp, t1.Rank, t1.PostID, t2.PostName ' +
    'FROM STAFFPOSTLOG t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'WHERE TabNumID = :TabNumID ' +
    'ORDER BY t1.FirstDate DESC'
  );
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(APostLogIDs, QFieldInt('ID'));
      VAppend(APostIDs, QFieldInt('PostID'));
      VAppend(APostTemps, QFieldInt('PostTemp'));
      VAppend(ARanks, QFieldStr('Rank'));
      VAppend(APostNames, QFieldStr('PostName'));
      VAppend(AFirstDates, QFieldDT('FirstDate'));
      VAppend(ALastDates, QFieldDT('LastDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffPostLogAdd(const APostLogID, ATabNumID, APostID, APostTemp: Integer;
                                  const ARank: String;
                                  const AFirstDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //меняем конечную дату текущего периода
    UpdateInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APostLogID, IncDay(AFirstDate, -1), False{no commit});
    QSetSQL(
      sqlINSERT('STAFFPOSTLOG', ['TabNumID', 'PostID', 'FirstDate', 'LastDate', 'PostTemp', 'Rank'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('PostID', APostID);
    QParamDT('FirstDate', AFirstDate);
    QParamDT('LastDate', INFDATE);
    QParamInt('PostTemp', APostTemp);
    QParamStr('Rank', ARank, not SEmpty(ARank));
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffPostLogUpdate(const APrevPostLogID, APostLogID, APostID, APostTemp: Integer;
                                      const ARank: String;
                                      const AFirstDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //меняем конечную дату предыдущего периода
    UpdateInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APrevPostLogID, IncDay(AFirstDate, -1), False{no commit});
    //изменяем начальную дату  и должность текущего периода
    QSetSQL(
      sqlUPDATE('STAFFPOSTLOG', [ 'PostID', 'FirstDate', 'PostTemp', 'Rank']) +
      'WHERE ID = :PostLogID'
    );
    QParamInt('PostLogID', APostLogID);
    QParamInt('PostID', APostID);
    QParamDT('FirstDate', AFirstDate);
    QParamInt('PostTemp', APostTemp);
    QParamStr('Rank', ARank, not SEmpty(ARank));
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffPostLogDelete(const APrevPostLogID, APostLogID: Integer;
                                      const ALastDate: TDate): Boolean;
begin
  Result:= False;
  try
    //удаление записи
    Delete('STAFFPOSTLOG', 'ID', APostLogID, False {no commit});
    //замена конечной даты предыдущего периода на конечную дату этого периода
    UpdateInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APrevPostLogID, ALastDate, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

end.

