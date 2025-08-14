unit USIZUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_StrUtils, DK_Const, DK_Vector, DK_Matrix,
  //Project utils
  UConst, USIZSizes;

function SIZLifeInYears(const AMonths: Extended): String;
function SIZLifeInMonths(const AMonths: Extended): String;
function SIZLifeInMonthAndYears(const AMonths: Extended): String;
function SIZLifeInMonthOrYears(const AReceivingCount, ANum, ALife: Integer): String;

function SIZPeriod(const ALife: Integer;
                   //выводить "1" перед месяц/год, когда Life=1/12
                   const ANeedLifeCountIfSingle: Boolean = True): String;
function SIZNumForPeriod(const ANum, ALife: Integer): String;
function SIZUnitCommaNumForPeriod(const AUnit: String; const ANum, ALife: Integer): String;
function SIZUnitCycle(const AUnit: String; const ALife: Integer; const ANeedBreak: Boolean): String;

function SIZLifeInMonths(const AReceivingCount, ANum, ALife: Integer): Extended;
function SIZWriteoffDate(const AReceivingDate, AReturnDate: TDate;
                         const AReceivingCount, ANum, ALife: Integer): TDate;

function SIZFullSize(const ASizeType, ASizeID: Integer;
                      const AHeightID: Integer = 0;
                      const ANotDefineValue: String = ''): String;
function SIZFullSize(const ASizeTypes, ASizeIDs, AHeightIDs: TIntVector;
                      const ANotDefineValue: String = ''): TStrVector;
function SIZFullSize(const ASizeTypes, ASizeIDs, AHeightIDs: TIntMatrix;
                      const ANotDefineValue: String = ''): TStrMatrix;

function SIZNormFullName(const ANormName, ANormNote: String): String;

function SIZDocFullName(const ADocName, ADocNum: String;
                        const ADocDate: TDate;
                        const ANeedEmptyNumMark: Boolean = False): String;
function SIZDocFullName(const ADocNames, ADocNums: TStrVector;
                        const ADocDates: TDateVector;
                        const ANeedEmptyNumMark: Boolean = False): TStrVector;
function SIZDocFullName(const ADocNames, ADocNums: TStrMatrix;
                        const ADocDates: TDateMatrix;
                        const ANeedEmptyNumMark: Boolean = False): TStrMatrix;

implementation

function SIZLifeInYears(const AMonths: Extended): String;
var
  M: Integer;
begin
  Result:= EmptyStr;
  if AMonths<12 then Exit;
  M:= Trunc(AMonths);
  if M=AMonths then  //целое кол-во месяцев
  begin
    if (M mod 12) = 0 then //целое кол-во лет
      Result:= SYears(IntToStr(M div 12))
    else begin
      if ((2*M) mod 12) = 0 then //кол-во лет кратное половине года
        Result:= Format('%.1f года', [M/12])
      else
        Result:= Format('%.2f года', [M/12]);
    end;
  end
  else
    Result:= Format('%.2f года', [AMonths/12]);
end;

function SIZLifeInMonths(const AMonths: Extended): String;
var
  M: Integer;
begin
  M:= Trunc(AMonths);
  if M=AMonths then  //целое кол-во месяцев
    Result:= SMonths(IntToStr(M))
  else begin
    if Trunc(2*AMonths) = 2*AMonths then //кол-во кратное половине месяца
      Result:= Format('%.1f месяца', [AMonths])
    else
      Result:= Format('%.2f месяца', [AMonths]);
  end;
end;

function SIZLifeInMonthAndYears(const AMonths: Extended): String;
var
  MonthStr, YearStr: String;
begin
  if AMonths=0 then
  begin
    Result:= '0 месяцев';
    Exit;
  end;
  MonthStr:= SIZLifeInMonths(AMonths);
  YearStr:= SIZLifeInYears(AMonths);
  Result:= MonthStr;
  if YearStr<>EmptyStr then
    Result:= MonthStr + ' (' + YearStr + ')';
end;

function SIZLifeInMonthOrYears(const AReceivingCount, ANum, ALife: Integer): String;
var
  Months: Extended;
begin
  if ALife<=0 then //особый срок службы
    Result:= SIZ_LIFE_PICKS[ALife]
  else begin
    Months:= SIZLifeInMonths(AReceivingCount, ANum, ALife);
    if Months<12 then
      Result:= SIZLifeInMonths(Months)
    else
      Result:= SIZLifeInYears(Months);
  end;
end;

function SIZPeriod(const ALife: Integer; const ANeedLifeCountIfSingle: Boolean = True): String;
begin
  if ALife<=0 then //особый срок службы
    Result:= SIZ_LIFE_PICKS[ALife]
  else begin
    if ALife<12 then //меньше года
      Result:= SMonths(IntToStr(ALife), True, ANeedLifeCountIfSingle)
    else begin //больше года
      if (ALife mod 12) = 0 then //целое кол-во лет
        Result:= SYears(IntToStr(ALife div 12), True, ANeedLifeCountIfSingle)
      else begin
        if ((2*ALife) mod 12) = 0 then //кол-во лет кратное половине года
          Result:= Format('%.1f года', [ALife/12])
        else
          Result:= Format('%.2f года', [ALife/12]);
      end;
    end;
  end;
end;

function SIZNumForPeriod(const ANum, ALife: Integer): String;
begin
  Result:= IntToStr(ANum);
  if ALife>0 then
    Result:= Result + ' на';
  Result:= Result + SYMBOL_SPACE + SIZPeriod(ALife);
end;

function SIZUnitCommaNumForPeriod(const AUnit: String; const ANum, ALife: Integer): String;
begin
  Result:= SIZNumForPeriod(ANum, ALife);
  if not SEmpty(STrim(AUnit)) then
    Result:= AUnit + ', ' + Result;
end;

function SIZUnitCycle(const AUnit: String; const ALife: Integer;
                      const ANeedBreak: Boolean): String;
begin
  if ALife<=0 then //особый срок службы
    Result:= SIZ_LIFE_PICKS[ALife]
  else begin
    Result:= '1 раз в ' + SIZPeriod(ALife, False{не выводить 1 перед месяц/год, когда Life=1/12});
  end;
  if not SEmpty(STrim(AUnit)) then
  begin
    if ANeedBreak then
      Result:= SYMBOL_BREAK + Result;
    Result:= AUnit + ', ' + Result;
  end;
end;

function SIZLifeInMonths(const AReceivingCount, ANum, ALife: Integer): Extended;
begin
  if ANum>0 then
    Result:= AReceivingCount*ALife/ANum
  else
    Result:= 0;
end;

function SIZWriteoffDate(const AReceivingDate, AReturnDate: TDate;
                         const AReceivingCount, ANum, ALife: Integer): TDate;
var
  Months: Extended;
begin
  if AReturnDate>0 then
    Result:= AReturnDate
  else if ALife<=0 then
    Result:= INFDATE
  else begin
    Months:= SIZLifeInMonths(AReceivingCount, ANum, ALife);
    Result:= IncMonthExt(AReceivingDate, Months);
  end;
end;

function SIZFullSize(const ASizeType, ASizeID: Integer;
                        const AHeightID: Integer = 0;
                        const ANotDefineValue: String = ''): String;
begin
  Result:= ANotDefineValue;
  if (ASizeType=0) or ((ASizeID=0) and (AHeightID=0)) then Exit;
  case ASizeType of
  1: begin
       if (ASizeID>0) and (AHeightID>0) then
         Result:= CLOTHES[ASizeID] + '/' + PERSONHEIGHTS[AHeightID]
       else if ASizeID>0 then
         Result:= CLOTHES[ASizeID]
       else
         Result:= PERSONHEIGHTS[ASizeID];
     end;
  2: Result:= SHOES[ASizeID];
  3: Result:= HEADDRESS[ASizeID];
  4: Result:= HANDS[ASizeID];
  5: Result:= GASMASKS[ASizeID];
  6: Result:= RESPIRATORS[ASizeID];
  7,8: Result:= IntToStr(ASizeID);
  end;
end;

function SIZFullSize(const ASizeTypes, ASizeIDs, AHeightIDs: TIntVector;
                     const ANotDefineValue: String = ''): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ASizeTypes) then Exit;

  VDim(Result, Length(ASizeTypes));
  for i:= 0 to High(ASizeTypes) do
    Result[i]:= SIZFullSize(ASizeTypes[i], ASizeIDs[i], AHeightIDs[i], ANotDefineValue);
end;

function SIZFullSize(const ASizeTypes, ASizeIDs, AHeightIDs: TIntMatrix;
                     const ANotDefineValue: String): TStrMatrix;
var
  i: Integer;
begin
  Result:= nil;
  if MIsNil(ASizeTypes) then Exit;

  MDim(Result, Length(ASizeTypes));
  for i:= 0 to High(ASizeTypes) do
    Result[i]:= SIZFullSize(ASizeTypes[i], ASizeIDs[i], AHeightIDs[i], ANotDefineValue);
end;

function SIZNormFullName(const ANormName, ANormNote: String): String;
begin
  Result:= EmptyStr;
  if SEmpty(ANormName) and SEmpty(ANormNote) then Exit;
  if (not SEmpty(ANormName)) and (not SEmpty(ANormNote)) then
    Result:= ANormName + ' (' + ANormNote + ')'
  else if not SEmpty(ANormName) then
    Result:= ANormName
  else
    Result:= ANormNote;
end;

function SIZDocFullName(const ADocName, ADocNum: String;
                        const ADocDate: TDate;
                        const ANeedEmptyNumMark: Boolean = False): String;
var
  S: String;
begin
  Result:= EmptyStr;
  if (ADocDate=0) or SameDate(ADocDate, NULDATE) then Exit;
  Result:= ADocName;
  S:= ADocNum;
  if SEmpty(S) and ANeedEmptyNumMark then
    S:= 'б/н';
  if not SEmpty(S) then
    Result:= Result + ' № ' + S;
  Result:= Result + ' от ' + FormatDateTime('dd.mm.yyyy', ADocDate);
end;

function SIZDocFullName(const ADocNames, ADocNums: TStrVector;
                        const ADocDates: TDateVector;
                        const ANeedEmptyNumMark: Boolean = False): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ADocNames) then Exit;

  VDim(Result, Length(ADocNames));
  for i:= 0 to High(ADocNames) do
    Result[i]:= SIZDocFullName(ADocNames[i], ADocNums[i], ADocDates[i], ANeedEmptyNumMark);
end;

function SIZDocFullName(const ADocNames, ADocNums: TStrMatrix;
                        const ADocDates: TDateMatrix;
                        const ANeedEmptyNumMark: Boolean = False): TStrMatrix;
var
  i: Integer;
begin
  Result:= nil;
  if MIsNil(ADocNames) then Exit;

  MDim(Result, Length(ADocNames));
  for i:= 0 to High(ADocNames) do
    Result[i]:= SIZDocFullName(ADocNames[i], ADocNums[i], ADocDates[i], ANeedEmptyNumMark);
end;

end.

