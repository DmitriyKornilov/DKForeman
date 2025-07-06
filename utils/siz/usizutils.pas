unit USIZUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_StrUtils, DK_Const, DK_Vector,
  //Project utils
  UConst, USIZSizes;

function SIZLifeInYearsStr(const AMonths: Extended): String;
function SIZLifeInMonthsStr(const AMonths: Extended): String;
function SIZLifeInMonthAndYears(const AMonths: Extended): String;
//function SIZLifeStr(const ALife: Integer): String;
function SIZNumLifeStr(const ANum, ALife: Integer): String;
function SIZLifePeriod(const ALife: Integer): String;
function SIZLifeInMonthsFromDates(const AGivingDate, AWritingDate: TDate): Extended;
function SIZLifeInMonths(const AGettingCount, ANormCount, ANormLife, AWearPercent: Integer;
                            const AUseWearPercent: Boolean = True): Extended;

function SIZWriteoffDate(const AGettingDate: TDate;
                            const AGettingCount, ANormCount, ANormLife, AWearPercent: Integer;
                            const AUseWearPercent: Boolean = True): TDate;
function SIZWriteoffDateStr(const AGettingDate: TDate;
                            const AGettingCount, ANormCount, ANormLife, AWearPercent: Integer;
                            const AUseWearPercent: Boolean = True): String;
function SIZWriteoffDateStr(const AWiteoffDate: TDate): String;

function SIZFullSize(const ASizeType, ASizeID: Integer;
                        const AHeightID: Integer = 0;
                        const ANotDefineValue: String = ''): String;

function SIZNormFullName(const ANormName, ANormNote: String): String;

procedure SIZChooseFromStaffAndSpecSizes(const ASpecSizeID, ASpecHeightID,
                                      AStaffSizeID, AStaffHeightID: Integer;
                                      out ASizeID, AHeightID: Integer);

function SIZDocFullName(const ADocName, ADocNum: String;
                        const ADocDate: TDate;
                        const ANeedEmptyNumMark: Boolean = False): String;
function SIZDocFullName(const ADocNames, ADocNums: TStrVector;
                        const ADocDates: TDateVector;
                        const ANeedEmptyNumMark: Boolean = False): TStrVector;

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

{function SIZLifeStr(const ALife: Integer): String;
//var
//  X: Integer;
begin
  if ALife=1 then
    Result:= '1 месяц'
  else if ALife=12 then
    Result:= '1 год'
  else
    Result:= SIZLifePeriod(ALife);

  //if ALife<=0 then
  //begin
  //  Result:= SIZ_LIFE_PICKS[ALife];
  //  Exit;
  //end;
  //
  //if ALife=12 then
  //begin
  //  Result:= '1 год';
  //  Exit;
  //end;
  //if ALife<12 then //меньше года
  //begin
  //  Result:= IntToStr(ALife) + ' месяц';
  //  if (ALife>=2) and (ALife<=4) then
  //    Result:= Result + 'а'
  //  else
  //    Result:= Result + 'ев';
  //end
  //else begin //больше года
  //  if (ALife mod 12) = 0 then //целое кол-во лет
  //  begin
  //    X:= ALife div 12;
  //    Result:= IntToStr(X);
  //    if (X>=2) and (X<=4) then
  //      Result:= Result + ' года'
  //    else
  //      Result:= Result + ' лет';
  //  end
  //  else begin
  //    if ((2*ALife) mod 12) = 0 then //кол-во лет кратное половине года
  //      Result:= Format('%.1f года', [ALife/12])
  //    else
  //      Result:= Format('%.2f года', [ALife/12]);
  //  end;
  //end;
end;   }

function SIZNumLifeStr(const ANum, ALife: Integer): String;
var
  NumStr, PeriodStr: String;
  X: Integer;
begin
  NumStr:= IntToStr(ANum);
  if ALife<=0 then //особый срок службы
    Result:= NumStr + ' ' + SIZ_LIFE_PICKS[ALife]
  else if ALife=1 then //ровно 1 месяц
    Result:= NumStr + ' на 1 месяц'
  else if ALife=12 then //ровно 1 год
      Result:= NumStr + ' на 1 год'
  else begin
    PeriodStr:= IntToStr(ALife);
    if ALife<12 then //меньше года
    begin
      Result:= NumStr + ' на ' + PeriodStr + ' месяц';
      if (ALife>=2) and (ALife<=4) then
        Result:= Result + 'а'
      else
        Result:= Result + 'ев';
    end
    else begin //больше года
      if (ALife mod 12) = 0 then //целое кол-во лет
      begin
        X:= ALife div 12;
        Result:= NumStr + ' на ' + IntToStr(X);
        if (X>=2) and (X<=4) then
          Result:= Result + ' года'
        else
          Result:= Result + ' лет';
      end
      else begin
        if ((2*ALife) mod 12) = 0 then //кол-во лет кратное половине года
          Result:= NumStr + Format(' на %.1f года', [ALife/12])
        else
          Result:= NumStr + Format(' на %.2f года', [ALife/12]);
      end;
    end;
  end;
end;

function SIZLifePeriod(const ALife: Integer): String;
var
  PeriodStr: String;
  X: Integer;
begin
  if ALife<=0 then //особый срок службы
    Result:= SIZ_LIFE_PICKS[ALife]
  else if ALife=1 then //ровно 1 месяц
    Result:= 'месяц'
  else if ALife=12 then //ровно 1 год
      Result:= 'год'
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

function SIZLifeInMonthsFromDates(const AGivingDate, AWritingDate: TDate): Extended;
var
  D: TDate;
  IntMonths: Integer;
  FracMonths: Extended;
begin
  D:= AGivingDate;
  IntMonths:= 0;
  while CompareDate(D, AWritingDate)<=0 do
  begin
    IntMonths:= IntMonths + 1;
    D:= IncMonth(AGivingDate, IntMonths);
  end;
  if SameDate(D, AWritingDate) then
  begin
    Result:= IntMonths;
  end
  else begin
    IntMonths:= IntMonths - 1;
    D:= IncMonth(AGivingDate, IntMonths);
    FracMonths:= DaysBetweenDates(D, AWritingDate)/30;
    Result:= IntMonths + FracMonths;
  end;
end;

function SIZLifeInMonths(const AGettingCount, ANormCount, ANormLife, AWearPercent: Integer;
                            const AUseWearPercent: Boolean = True): Extended;
begin
  if AUseWearPercent and (AWearPercent>0) then
    Result:= ((100-AWearPercent)/100)*AGettingCount*ANormLife/ANormCount
  else
    Result:= AGettingCount*ANormLife/ANormCount;
end;

function SIZWriteoffDate(const AGettingDate: TDate;
                            const AGettingCount, ANormCount, ANormLife, AWearPercent: Integer;
                            const AUseWearPercent: Boolean = True): TDate;
var
  DeltaMonth: Extended;
begin
  if ANormLife>0 then
  begin
    DeltaMonth:= SIZLifeInMonths(AGettingCount, ANormCount, ANormLife,
                                   AWearPercent, AUseWearPercent);
    Result:= IncMonthExt(AGettingDate, DeltaMonth);
  end
  else
    Result:= INFDATE;
end;

function SIZWriteoffDateStr(const AGettingDate: TDate;
                            const AGettingCount, ANormCount, ANormLife, AWearPercent: Integer;
                            const AUseWearPercent: Boolean = True): String;
var
  D: TDate;
begin
  if ANormLife=0 then
    Result:= '-'
  else begin
    D:= SIZWriteoffDate(AGettingDate, AGettingCount, ANormCount, ANormLife,
                         AWearPercent, AUseWearPercent);
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
  4,5: Result:= HANDS[ASizeID];
  6: Result:= GASMASKS[ASizeID];
  7: Result:= RESPIRATORS[ASizeID];
  end;
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

procedure SIZChooseFromStaffAndSpecSizes(const ASpecSizeID, ASpecHeightID,
                                      AStaffSizeID, AStaffHeightID: Integer;
                                      out ASizeID, AHeightID: Integer);
begin
  if (ASpecSizeID>0) then
  begin
    ASizeID:= ASpecSizeID;
    AHeightID:= ASpecHeightID;
  end
  else begin
    ASizeID:= AStaffSizeID;
    AHeightID:= AStaffHeightID;
  end;
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

end.

