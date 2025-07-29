unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Const, DK_StrUtils, DK_VSTTables, DK_SheetTables;


  //ID for reselect
  function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  function GetSelectedID(const ATable: TCustomSheetTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  //Settings
  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;

  //Periods
  function PeriodToStr(const ABeginDate, AEndDate: TDate): String;
  function VPeriodToStr(const ABeginDates, AEndDates: TDateVector): TStrVector;
  function MPeriodToStr(const ABeginDates, AEndDates: TDateMatrix): TStrMatrix;

  //Staff
  function StaffFullName(const AFamily, AName, APatronymic: String;
                         const AIsShortName: Boolean): String;
  function StaffFullName(const AFamilies, ANames, APatronymics: TStrVector;
                         const AIsShortName: Boolean): TStrVector;
  function StaffFullName(const AStaffName, ATabNum: String): String;
  function StaffFullName(const AFamily, AName, APatronymic, ATabNum: String;
                         const AIsShortName: Boolean): String;
  function StaffFullName(const AFamilies, ANames, APatronymics, ATabNums: TStrVector;
                         const AIsShortName: Boolean): TStrVector;
  function StaffFullName(const AFamily, AName, APatronymic, ATabNum, APostName: String;
                         const AIsShortName: Boolean): String;
  function StaffFullName(const AFamilies, ANames, APatronymics, ATabNums, APostNames: TStrVector;
                         const AIsShortName: Boolean): TStrVector;

implementation

function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                       const ASelectedID: Integer = -1): Integer;
begin
  Result:= -1;
  if VIsNil(AIDValues) then Exit;
  if ASelectedID>0 then
    Result:= ASelectedID
  else if Assigned(ATable) and ATable.IsSelected then
    Result:= AIDValues[ATable.SelectedIndex];
end;

function GetSelectedID(const ATable: TCustomSheetTable; const AIDValues: TIntVector;
                       const ASelectedID: Integer  = -1): Integer;
begin
  Result:= -1;
  if VIsNil(AIDValues) then Exit;
  if ASelectedID>0 then
    Result:= ASelectedID
  else if Assigned(ATable) and ATable.IsSelected then
    Result:= AIDValues[ATable.SelectedIndex];
end;

function SettingByName(const AName: String; const ANames: TStrVector;
  const AValues: TIntVector): Integer;
begin
  VSameIndexValue(AName, ANames, AValues, Result);
end;

function PeriodToStr(const ABeginDate, AEndDate: TDate): String;
begin
  Result:= FormatDateTime('dd.mm.yyyy — ', ABeginDate);
  if SameDate(AEndDate, INFDATE) then
    Result:= Result + 'наст. время'
  else
    Result:= Result + FormatDateTime('dd.mm.yyyy', AEndDate);
end;

function VPeriodToStr(const ABeginDates, AEndDates: TDateVector): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to High(ABeginDates) do
    VAppend(Result, PeriodToStr(ABeginDates[i], AEndDates[i]));
end;

function MPeriodToStr(const ABeginDates, AEndDates: TDateMatrix): TStrMatrix;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to High(ABeginDates) do
    MAppend(Result, VPeriodToStr(ABeginDates[i], AEndDates[i]));
end;

function StaffFullName(const AFamily, AName, APatronymic: String;
                       const AIsShortName: Boolean): String;
begin
  Result:= EmptyStr;
  if AIsShortName then
    Result:= SNameShort(AFamily, AName, APatronymic)
  else
    Result:= SNameLong(AFamily, AName, APatronymic);
end;

function StaffFullName(const AFamilies, ANames, APatronymics: TStrVector;
                       const AIsShortName: Boolean): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(AFamilies) then Exit;
  for i:= 0 to High(AFamilies) do
    VAppend(Result, StaffFullName(AFamilies[i], ANames[i], APatronymics[i],
                                  AIsShortName));
end;

function StaffFullName(const AStaffName, ATabNum: String): String;
begin
  if SEmpty(AStaffName) or SEmpty(ATabNum) then
    Result:= EmptyStr
  else
    Result:= AStaffName + ' [таб.№ ' + ATabNum + ']';
end;

function StaffFullName(const AFamily, AName, APatronymic, ATabNum: String;
                       const AIsShortName: Boolean): String;
var
  StaffName: String;
begin
  StaffName:= StaffFullName(AFamily, AName, APatronymic, AIsShortName);
  Result:= StaffFullName(StaffName, ATabNum);
end;

function StaffFullName(const AFamilies, ANames, APatronymics, ATabNums: TStrVector;
                       const AIsShortName: Boolean): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(AFamilies) then Exit;
  for i:= 0 to High(AFamilies) do
    VAppend(Result, StaffFullName(AFamilies[i], ANames[i], APatronymics[i],
                                  ATabNums[i], AIsShortName));
end;

function StaffFullName(const AFamily, AName, APatronymic, ATabNum, APostName: String;
                       const AIsShortName: Boolean): String;
begin
  Result:= StaffFullName(AFamily, AName, APatronymic, ATabNum, AIsShortName);
  if SEmpty(Result) then Exit;
  if Sempty(APostName) or SSame(APostName, '<не указана>') then
    Result:= Result + ' - <должность не указана>'
  else
    Result:= Result + ' - ' + APostName;
end;

function StaffFullName(const AFamilies, ANames, APatronymics, ATabNums, APostNames: TStrVector;
                       const AIsShortName: Boolean): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(AFamilies) then Exit;
  for i:= 0 to High(AFamilies) do
    VAppend(Result, StaffFullName(AFamilies[i], ANames[i], APatronymics[i],
                                  ATabNums[i], APostNames[i], AIsShortName));
end;

end.

