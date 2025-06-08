unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Const, DK_VSTTables, DK_SheetTables;


  //ID for reselect
  function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  function GetSelectedID(const ATable: TCustomSheetTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  //Settings
  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;

  function PeriodToStr(const ABeginDate, AEndDate: TDate): String;
  function VPeriodToStr(const ABeginDates, AEndDates: TDateVector): TStrVector;
  function MPeriodToStr(const ABeginDates, AEndDates: TDateMatrix): TStrMatrix;



implementation

function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                       const ASelectedID: Integer = -1): Integer;
begin
  Result:= -1;
  if ASelectedID>0 then
    Result:= ASelectedID
  else if Assigned(ATable) and ATable.IsSelected then
    Result:= AIDValues[ATable.SelectedIndex];
end;

function GetSelectedID(const ATable: TCustomSheetTable; const AIDValues: TIntVector;
                       const ASelectedID: Integer  = -1): Integer;
begin
  Result:= -1;
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

end.

