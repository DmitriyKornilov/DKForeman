unit UDBUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,

  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_Dialogs, DK_StrUtils;

type

  { TDataBase }

  TDataBase = class (TSQLite3)
  private

  public
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
    {Получение списка людей: True - ОК, False - список пуст
     AFilterValue - фильтр по Ф.И.О.}
    function StaffMainListLoad(const AFilterValue: String;
                          out AStaffIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;

    {Список табельных номеров по ID человека: True - ОК, False - список пуст}
    function StaffTabNumListLoad(const AStaffID: Integer; out ATabNumIDs: TIntVector;
                          out ATabNums, APostNames, ARanks: TStrVector;
                          out ARecrutDates, ADismissDates: TDateVector): Boolean;
    {Список переводов по ID таб. номера: True - ОК, False - список пуст}
    function StaffPostLogLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
  end;

var
  DataBase: TDataBase;

implementation

{ TDataBase }

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
           'SELECT t2.StaffID, t2.TabNumID, t2.TabNum, t2.RecrutDate, t2.DismissDate, t2.Rank, ' +
                  't3.PostName ' +
           'FROM STAFFTABNUM t2 ' +
           'INNER JOIN STAFFPOST t3 ON (t2.PostID=t3.PostID) ' +
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

function TDataBase.StaffTabNumListLoad(const AStaffID: Integer; out ATabNumIDs: TIntVector;
                          out ATabNums, APostNames, ARanks: TStrVector;
                          out ARecrutDates, ADismissDates: TDateVector): Boolean;
begin
  Result:= False;

  ATabNumIDs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;

  if AStaffID<=0 then Exit;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.TabNumID, t1.TabNum, t1.RecrutDate, t1.DismissDate, t1.Rank, t2.PostName ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'WHERE t1.StaffID = :StaffID ' +
    'ORDER BY t1.TabNum'
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
      VAppend(ARanks, QFieldStr('Rank'));
      VAppend(APostNames, QFieldStr('PostName'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));
      VAppend(ADismissDates, QFieldDT('DismissDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffPostLogLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
begin
  Result:= False;

  APostLogIDs:= nil;
  APostTemps:= nil;
  APostNames:= nil;
  ARanks:= nil;
  AFirstDates:= nil;
  ALastDates:= nil;

  if ATabNumID<=0 then Exit;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ID, t1.FirstDate, t1.LastDate, t1.PostTemp, t1.Rank, t2.PostName ' +
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

end.

