unit USIZUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_StrUtils, DK_Const, DK_Vector, DK_Matrix,
  //Project utils
  UConst, USIZSizes;

function SIZLifeInYearsStr(const AMonths: Extended): String;
function SIZLifeInMonthsStr(const AMonths: Extended): String;
function SIZLifeInMonthAndYears(const AMonths: Extended): String;

function SIZPeriod(const ALife: Integer;
                   //выводить 1 перед месяц/год, когда Life=1/12
                   const ANeedLifeCountIfSingle: Boolean = True): String;
function SIZNumForPeriod(const ANum, ALife: Integer): String;
function SIZUnitCommaNumForPeriod(const AUnit: String; const ANum, ALife: Integer): String;
function SIZUnitCycle(const AUnit: String; const ALife: Integer; const ANeedBreak: Boolean): String;

function SIZLifeInMonthsFromDates(const AReceivingDate, AWritingDate: TDate): Extended;
function SIZLifeInMonths(const AReceivingCount, ANormCount, ANormLife: Integer): Extended;

function SIZWriteoffDate(const AReceivingDate: TDate;
                         const AReceivingCount, ANormCount, ANormLife: Integer): TDate;
function SIZWriteoffDateStr(const AReceivingDate: TDate;
                            const AReceivingCount, ANormCount, ANormLife: Integer): String;
function SIZWriteoffDateStr(const AWiteoffDate: TDate): String;

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

procedure SIZStatusInfoVerifyDates(const AReportDate: TDate;
                                const ALogIDs: TInt64Vector;
                                const ASizNames:TStrVector;
                                const ASizCounts: TIntVector;
                                const AReceivingDates, AWriteoffDates: TDateVector;
                                out AOutLogIDs: TInt64Vector;
                                out AOutSizNames: TStrVector;
                                out AOutSizCounts: TIntVector;
                                out AOutReceivingDates, AOutWriteoffDates: TDateVector);

implementation

function SIZLifeInYearsStr(const AMonths: Extended): String;
var
  X, Y, M: Integer;
begin
  Result:= EmptyStr;
  if AMonths<12 then Exit;
  M:= Trunc(AMonths);
  if M=AMonths then  //целое кол-во месяцев
  begin
    if (M mod 12) = 0 then //целое кол-во лет
    begin
      Y:= M div 12;
      Result:= IntToStr(Y);
      if (Y>=11) and (Y<=14) then
        Result:= Result + ' лет'
      else begin
        X:= SDigit(Result, SLength(Result));
        case X of
        0, 5..9: Result:= Result + ' лет';
        1: Result:= Result + ' год';
        2..4: Result:= Result + ' года';
        end;
      end;
    end
    else begin
      if ((2*M) mod 12) = 0 then //кол-во лет кратное половине года
        Result:= Format('%.1f года', [M/12])
      else
        Result:= Format('%.2f года', [M/12]);
    end;
  end
  else begin
    Result:= Format('%.2f года', [AMonths/12]);
  end;
end;

function SIZLifeInMonthsStr(const AMonths: Extended): String;
var
  X, M: Integer;
begin
  M:= Trunc(AMonths);
  if M=AMonths then  //целое кол-во месяцев
  begin
    Result:= IntToStr(M);
    if (M>=11) and (M<=14) then
      Result:= Result + ' месяцев'
    else begin
      X:= SDigit(Result, SLength(Result));
      case X of
      1: Result:= Result + ' месяц';
      2..4: Result:= Result + ' месяца';
      0, 5..9: Result:= Result + ' месяцев';
      end;
    end;
  end
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
  MonthStr:= SIZLifeInMonthsStr(AMonths);
  YearStr:= SIZLifeInYearsStr(AMonths);
  Result:= MonthStr;
  if YearStr<>EmptyStr then
    Result:= MonthStr + ' (' + YearStr + ')';
end;

function SIZPeriod(const ALife: Integer; const ANeedLifeCountIfSingle: Boolean = True): String;
var
  PeriodStr: String;
  X: Integer;
begin
  if ALife<=0 then //особый срок службы
    Result:= SIZ_LIFE_PICKS[ALife]
  else if ALife=1 then //ровно 1 месяц
  begin
    if ANeedLifeCountIfSingle then
      Result:= '1 месяц'
    else
      Result:= 'месяц';
  end
  else if ALife=12 then //ровно 1 год
  begin
    if ANeedLifeCountIfSingle then
      Result:= '1 год'
    else
      Result:= 'год';
  end
  else begin
    PeriodStr:= IntToStr(ALife);
    if ALife<12 then //меньше года
    begin
      Result:= PeriodStr + 'месяц';
      if (ALife>=2) and (ALife<=4) then
        Result:= Result + 'а'
      else
        Result:= Result + 'ев';
    end
    else begin //больше года
      if (ALife mod 12) = 0 then //целое кол-во лет
      begin
        X:= ALife div 12;
        Result:= IntToStr(X);
        if (X>=2) and (X<=4) then
          Result:= Result + ' года'
        else
          Result:= Result + ' лет';
      end
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

function SIZLifeInMonthsFromDates(const AReceivingDate, AWritingDate: TDate): Extended;
var
  D: TDate;
  IntMonths: Integer;
  FracMonths: Extended;
begin
  D:= AReceivingDate;
  IntMonths:= 0;
  while CompareDate(D, AWritingDate)<=0 do
  begin
    IntMonths:= IntMonths + 1;
    D:= IncMonth(AReceivingDate, IntMonths);
  end;
  if SameDate(D, AWritingDate) then
  begin
    Result:= IntMonths;
  end
  else begin
    IntMonths:= IntMonths - 1;
    D:= IncMonth(AReceivingDate, IntMonths);
    FracMonths:= DaysBetweenDates(D, AWritingDate)/30;
    Result:= IntMonths + FracMonths;
  end;
end;

function SIZLifeInMonths(const AReceivingCount, ANormCount, ANormLife: Integer): Extended;
begin
  if ANormCount>0 then
    Result:= AReceivingCount*ANormLife/ANormCount
  else
    Result:= 0;
end;

function SIZWriteoffDate(const AReceivingDate: TDate;
                         const AReceivingCount, ANormCount, ANormLife: Integer): TDate;
var
  DeltaMonth: Extended;
begin
  if ANormLife>0 then
  begin
    DeltaMonth:= SIZLifeInMonths(AReceivingCount, ANormCount, ANormLife);
    Result:= IncMonthExt(AReceivingDate, DeltaMonth);
  end
  else
    Result:= INFDATE;
end;

function SIZWriteoffDateStr(const AReceivingDate: TDate;
                            const AReceivingCount, ANormCount, ANormLife: Integer): String;
var
  D: TDate;
begin
  if ANormLife=0 then
    Result:= '-'
  else begin
    D:= SIZWriteoffDate(AReceivingDate, AReceivingCount, ANormCount, ANormLife);
    Result:= FormatDateTime('dd.mm.yyyy', D);
  end;
end;

function SIZWriteoffDateStr(const AWiteoffDate: TDate): String;
begin
  if SameDate(AWiteoffDate, INFDATE) then
    Result:= '-'
  else
    Result:= FormatDateTime('dd.mm.yyyy', AWiteoffDate);
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

procedure SIZStatusInfoVerifyDates(const AReportDate: TDate;
                                const ALogIDs: TInt64Vector;
                                const ASizNames:TStrVector;
                                const ASizCounts: TIntVector;
                                const AReceivingDates, AWriteoffDates: TDateVector;
                                out AOutLogIDs: TInt64Vector;
                                out AOutSizNames: TStrVector;
                                out AOutSizCounts: TIntVector;
                                out AOutReceivingDates, AOutWriteoffDates: TDateVector);
var
  i: Integer;
  MaxWD: TDate;
begin
  AOutLogIDs:= nil;
  AOutSizNames:= nil;
  AOutSizCounts:= nil;
  AOutReceivingDates:= nil;
  AOutWriteoffDates:= nil;
  //выбираем непросроченные сиз
  for i:= 0 to High(ASizNames) do
  begin
    if CompareDate(AWriteoffDates[i], AReportDate)>=0 then
    begin
      VAppend(AOutLogIDs, ALogIDs[i]);
      VAppend(AOutSizNames, ASizNames[i]);
      VAppend(AOutSizCounts, ASizCounts[i]);
      VAppend(AOutReceivingDates, AReceivingDates[i]);
      VAppend(AOutWriteoffDates, AWriteoffDates[i]);
    end;
  end;
  //если есть непросроченные, выходим
  if Length(AOutSizNames)>0 then Exit;
  //последняя дата списания
  MaxWD:= VMaxDate(AWriteoffDates);
  //выбираем самые последние просроченые СИЗ
  for i:= 0 to High(ASizNames) do
  begin
    if SameDate(AWriteoffDates[i], MaxWD) then
    begin
      VAppend(AOutLogIDs, ALogIDs[i]);
      VAppend(AOutSizNames, ASizNames[i]);
      VAppend(AOutSizCounts, ASizCounts[i]);
      VAppend(AOutReceivingDates, AReceivingDates[i]);
      VAppend(AOutWriteoffDates, AWriteoffDates[i]);
    end;
  end;
end;

end.

