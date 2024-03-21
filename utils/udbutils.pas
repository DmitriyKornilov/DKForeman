unit UDBUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, DateUtils,

  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_Dialogs, DK_StrUtils, DK_Const;

type

  { TDataBase }

  TDataBase = class (TSQLite3)
  private
    {GetNextPeriodFirstDate достает из таблицы ATableName
     начальную дату ANextPeriodFirstDate периода,
     следующего за периодом с начальной датой AThisPeriodFirstDate
    ADateFieldName - имя поля с начальной датой периода
    ATabNumID - ID табельного номера
    Если следующего периода нет, то возвращает False и ANextDate:=INFDATE+1}
    function GetNextPeriodFirstDate(const ATableName, ADateFieldName: String;
                             const ATabNumID: Integer;
                             const AThisPeriodFirstDate: TDate;
                             out ANextPeriodFirstDate: TDate): Boolean;
    {GetPrevBeginDate - то же самое для предшествующего периода
    Если предыдущего периода нет, возвращает False и  APrevDate:=AThisPeriodFirstDate-1}
    function GetPrevPeriodFirstDate(const ATableName, ADateFieldName: String;
                             const ATabNumID: Integer;
                             const AThisPeriodFirstDate: TDate;
                             out APrevPeriodFirstDate: TDate): Boolean;
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
                          const AStaffID, APostID: Integer;
                          const ATabNum, ARank: String;
                          const ARecrutDate: TDate): Boolean;
    {Обновление данных о приеме нового таб. номера: True - ОК, False - ошибка}
    function StaffTabNumUpdate(const ATabNumID, APostID: Integer;
                          const ATabNum, ARank: String;
                          const ARecrutDate: TDate): Boolean;
    {Обновление даты увольнения таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDismiss(const ATabNumID: Integer; const ADismissDate: TDate): Boolean;
    {Отмена увольнения таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDismissCancel(const ATabNumID: Integer): Boolean;
    {Удаление таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDelete(const ATabNumID: Integer): Boolean;
    {Проверка наличия таб. номера в записи с ID<>ATabNumID: True - да, False - нет}
    function StaffTabNumIsExists(const ATabNumID: Integer; const ATabNum: String): Boolean;



    {Список переводов по ID таб. номера: True - ОК, False - список пуст}
    function StaffPostLogListLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
    {Начальная дата предыдущего периода в должности, False - если предыдущего периода нет}
    function StaffPostLogPrevPeriodFirstDate(const ATabNumID: Integer;
                          const AThisPeriodFirstDate: TDate;
                          out APrevPeriodFirstDate: TDate): Boolean;
    {Начальная дата следующего периода в должности, False - если следующего периода нет}
    function StaffPostLogNextPeriodFirstDate(const ATabNumID: Integer;
                          const AThisPeriodFirstDate: TDate;
                          out ANextPeriodFirstDate: TDate): Boolean;
    {Наличие другой постоянной должности}
    //function StaffPostLogIsOtherConstPostExists(const ATabNumID: Integer; const AFirstDate: TDate): Boolean;
  end;

var
  DataBase: TDataBase;

implementation

{ TDataBase }

function TDataBase.GetNextPeriodFirstDate(const ATableName, ADateFieldName: String;
                             const ATabNumID: Integer;
                             const AThisPeriodFirstDate: TDate;
                             out ANextPeriodFirstDate: TDate): Boolean;
var
  S: String;
begin
  S:= SqlEsc(ADateFieldName);
  QSetQuery(FQuery);
  QSetSQL(
     'SELECT'+ S +
     'FROM' + SqlEsc(ATableName) +
     'WHERE (TabNumID = :TabNumID) AND (' + S +' > :ThisPeriodFirstDate) ' +
     'ORDER BY' + S +
     'LIMIT 1');
   QParamInt('TabNumID', ATabNumID);
   QParamDT('ThisPeriodFirstDate', AThisPeriodFirstDate);
   QOpen;
   Result:= not QIsEmpty;
   if Result then
     ANextPeriodFirstDate:= QFieldDT(ADateFieldName)
   else
     ANextPeriodFirstDate:= IncDay(INFDATE, 1);
   QClose;
end;

function TDataBase.GetPrevPeriodFirstDate(const ATableName, ADateFieldName: String;
                             const ATabNumID: Integer;
                             const AThisPeriodFirstDate: TDate;
                             out APrevPeriodFirstDate: TDate): Boolean;
var
  S: String;
begin
  S:= SqlEsc(ADateFieldName);
  QSetQuery(FQuery);
  QSetSQL(
   'SELECT'+ S +
   'FROM' + SqlEsc(ATableName) +
   'WHERE (TabNumID = :TabNumID) AND (' + S +' < :ThisPeriodFirstDate) ' +
   'ORDER BY' + S + 'DESC ' +
   'LIMIT 1');
  QParamInt('TabNumID', ATabNumID);
  QParamDT('ThisPeriodFirstDate', AThisPeriodFirstDate);
  QOpen;
  Result:= not QIsEmpty;
  if Result then
    APrevPeriodFirstDate:= QFieldDT(ADateFieldName)
  else
    APrevPeriodFirstDate:= IncDay(AThisPeriodFirstDate, -1);
  QClose;
end;

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

  SQLStr:=
    'SELECT t1.StaffID, t1.Name, t1.Patronymic, t1.Family, t1.BornDate, t1.Gender, '+
           'tt.TabNumID, tt.TabNum, tt.RecrutDate, tt.DismissDate, tt.Rank, tt.PostName ' +
    'FROM STAFFMAIN t1 ' +
    'LEFT OUTER JOIN ( ' +
           'SELECT t2.StaffID, t2.TabNumID, t2.TabNum, t2.RecrutDate, t2.DismissDate, ' +
                  'ttt.Rank, ttt.PostID, ttt.PostName ' +
           'FROM STAFFTABNUM t2 ' +
           'LEFT OUTER JOIN ( ' +
                   'SELECT t3.TabNumID, t3.Rank, t3.PostID, t4.PostName ' +
                   'FROM STAFFPOSTLOG t3 ' +
                   'INNER JOIN STAFFPOST t4 ON (t3.PostID=t4.PostID) ' +
                   'WHERE (t3.PostTemp = 0) AND (t3.FirstDate <= :DateValue) ' +
                   'ORDER BY t3.FirstDate DESC ' +
                   'LIMIT 1' +
                   ') ttt ON (ttt.TabNumID=t2.TabNumID) ' +
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
  0: SQLStr:= SQLStr + 't1.Family, t1.Name, t1.Patronymic ';
  1: SQLStr:= SQLStr + 'tt.TabNum, t1.Family, t1.Name, t1.Patronymic ';
  2: SQLStr:= SQLStr + 'tt.PostName, t1.Family, t1.Name, t1.Patronymic ';
  3: SQLStr:= SQLStr + 't1.BornDate, t1.Family, t1.Name, t1.Patronymic ';
  4: SQLStr:= SQLStr + 'tt.RecrutDate, t1.Family, t1.Name, t1.Patronymic ';
  5: SQLStr:= SQLStr + 'tt.DismissDate, t1.Family, t1.Name, t1.Patronymic ';
  6: SQLStr:= SQLStr + 'tt.Rank, t1.Family, t1.Name, t1.Patronymic ';
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
      VAppend(APostNames, QFieldStr('PostName'));
      VAppend(ARanks, QFieldStr('Rank'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
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

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.TabNumID, t1.TabNum, t1.RecrutDate, t1.DismissDate, tt.Rank, tt.PostName, tt.FirstDate '+
    'FROM STAFFTABNUM t1 ' +
    'LEFT OUTER JOIN ( ' +
           'SELECT t2.TabNumID, t2.Rank, t2.FirstDate, t3.PostName ' +
           'FROM STAFFPOSTLOG t2 ' +
           'INNER JOIN STAFFPOST t3 ON (t2.PostID=t3.PostID) ' +
           'WHERE (t2.PostTemp = 0) AND (t2.FirstDate <= :DateValue) ' +
           ') tt ON (tt.TabNumID=t1.TabNumID) ' +
    'WHERE (t1.StaffID = :StaffID) ' +
    'ORDER BY t1.TabNum, tt.FirstDate DESC ' +
    'LIMIT 1'
  );
  QParamDT('DateValue', Date);
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
      VAppend(ARanks, QFieldStr('Rank'));
      VAppend(APostNames, QFieldStr('PostName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffTabNumAdd(out ATabNumID: Integer;
                          const AStaffID, APostID: Integer;
                          const ATabNum, ARank: String;
                          const ARecrutDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись данных в таблицу табельных номеров
    QSetSQL(
      sqlINSERT('STAFFTABNUM', ['StaffID', 'PostID', 'TabNum', 'Rank', 'RecrutDate', 'DismissDate'])
    );
    QParamInt('StaffID', AStaffID);
    QParamInt('PostID', APostID);
    QParamStr('TabNum', ATabNum);
    QParamStr('Rank', ARank);
    QParamDT('RecrutDate', ARecrutDate);
    QParamDT('DismissDate', INFDATE);
    QExec;
    //получение ID записанного табельного номера
    ATabNumID:= LastWritedInt32ID('STAFFTABNUM');
    //заносим первую запись в табицу переводов
    QSetSQL(
      sqlINSERT('STAFFPOSTLOG', ['TabNumID', 'PostID', 'FirstDate', 'LastDate', 'Rank'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('PostID', APostID);
    QParamDT('FirstDate', ARecrutDate);
    QParamDT('LastDate', INFDATE);
    QParamStr('Rank', ARank);
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

function TDataBase.StaffTabNumUpdate(const ATabNumID, APostID: Integer;
                                     const ATabNum, ARank: String;
                                     const ARecrutDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //обновление данных в таблице таб. номеров
    QSetSQL(
      sqlUPDATE('STAFFTABNUM', ['PostID', 'TabNum', 'Rank', 'RecrutDate']) +
      'WHERE TabNumID = :TabNumID'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('PostID', APostID);
    QParamStr('TabNum', ATabNum);
    QParamStr('Rank', ARank);
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

  //Result:= False;
  //QSetQuery(FQuery);
  //try
  //  //обновление даты увольнения в таблице таб. номеров
  //  QSetSQL(
  //    sqlUPDATE('STAFFTABNUM', ['DismissDate']) +
  //    'WHERE TabNumID = :TabNumID'
  //  );
  //  QParamInt('TabNumID', ATabNumID);
  //  QParamDT('DismissDate', ADismissDate);
  //  QExec;
  //
  //  QCommit;
  //  Result:= True;
  //except
  //  QRollback;
  //end;
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

function TDataBase.StaffPostLogPrevPeriodFirstDate(const ATabNumID: Integer;
                                    const AThisPeriodFirstDate: TDate;
                                    out APrevPeriodFirstDate: TDate): Boolean;
begin
  Result:= GetPrevPeriodFirstDate('STAFFPOSTLOG', 'FirstDate',
                        ATabNumID, AThisPeriodFirstDate, APrevPeriodFirstDate);
end;

function TDataBase.StaffPostLogNextPeriodFirstDate(const ATabNumID: Integer;
                                    const AThisPeriodFirstDate: TDate;
                                    out ANextPeriodFirstDate: TDate): Boolean;
begin
  Result:= GetNextPeriodFirstDate('STAFFPOSTLOG', 'FirstDate',
                        ATabNumID, AThisPeriodFirstDate, ANextPeriodFirstDate);
end;

//function TDataBase.StaffPostLogIsOtherConstPostExists(const ATabNumID: Integer; const AFirstDate: TDate): Boolean;
//begin
//  QSetQuery(FQuery);
//  QSetSQL(
//     'SELECT FirstDate FROM STAFFPOSTLOG ' +
//     'WHERE (TabNumID = :TabNumID) AND (FirstDate <> :FirstDate) AND (PostTemp = 0) ' +
//     'LIMIT 1');
//   QParamInt('TabNumID', ATabNumID);
//   QParamDT('FirstDate', AFirstDate);
//   QOpen;
//   Result:= not QIsEmpty;
//   QClose;
//end;

end.

