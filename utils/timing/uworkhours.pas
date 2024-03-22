unit UWorkHours;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils;

const
  {преобразование целочисленного представления рабочих часов в фактическое дробное}
  //кол-во знаков после запятой в рабочих часах
  FRACTION_DIGITS_IN_WORKHOURS = 2;
  //10^FRACTION_DIGITS_IN_WORKHOURS - делитель целой части часов в дробную
  WORKHOURS_DENOMINATOR        = 100 ;
  //кол-во часов, на котрое уменьшается рабочая смена в предпраздничный день
  REDUCE_HOURS_COUNT_IN_BEFORE = 1;

type
  {Рабочие часы}
  TWorkHours = class (TObject)
    protected
      FTotal: TIntVector; //вектор фактических часов (общее кол-во за смену)
      FNight: TIntVector; //вектор ночных часов
      function GetSumTotal: Integer;
      function GetSumNight: Integer;
      procedure SetTotal(const AVector: TIntVector);
      procedure SetNight(const AVector: TIntVector);
    public
      procedure Clear;
      procedure Add(const ATotal: Integer; const ANight: Integer = 0);
      procedure Copy(const ADestination: TWorkHours);
      property Total: TIntVector read FTotal write SetTotal;
      property Night: TIntVector read FNight write SetNight;
      property SumTotal: Integer read GetSumTotal;  //сумма отработанных часов
      property SumNight: Integer read GetSumNight;  //сумма ночных часов
  end;

  {ФУНКЦИИ ПЕРЕСЧЕТА ПРЕДСТАВЛЕНИЙ РАБОЧИХ ЧАСОВ}
  function WeekHoursToWorkHoursInt(const AHoursInWeek: Byte): Integer; //кол-во часов за смену
  function WeekHoursToWorkHoursFrac(const AHoursInWeek: Byte): Double; //кол-во часов за смену
  function WorkHoursIntToFrac(const AWorkHours: Integer): Double;      //кол-во часов из целоисленного (как в базе) в дробное
  function WorkHoursIntToFracStr(const AWorkHours: Integer): String;
  function WorkHoursFracToInt(const AWorkHours: Double): Integer;      //кол-во часов из дробного в целочисленное (для базы)
  {WorkHoursToStr переводит кол-во часов AWorkHours, заданное в целочисленном
  формате в строковый вид с дробями
  AFractionDigitsCount - кол-во знаков после запятой, которые хранятся в числе
  ANeedEndZerosInFrac - флаг вывода нулей в конце дробной части
  WorkHoursToStr(1100, 2, true)  = '11,00'
  WorkHoursToStr(1100, 2, false) = '11'
  WorkHoursToStr(1100, 3, true)  = '1,100'
  WorkHoursToStr(1100, 3, false) = '1,1'}
  function WorkHoursToStr(const AWorkHours: Integer;
                          const AFractionDigitsCount: Byte = FRACTION_DIGITS_IN_WORKHOURS;
                          const ANeedEndZerosInFrac: Boolean = False): String;
  function CalcShortenedDayHours(const AHours: Integer): Integer;

implementation

function CalcShortenedDayHours(const AHours: Integer): Integer;
var
  dH: Integer;
begin
  dH:= REDUCE_HOURS_COUNT_IN_BEFORE*WORKHOURS_DENOMINATOR;
  if AHours<dH then
    Result:= 0
  else
    Result:= AHours - dH;
end;

{TWorkHours}

procedure TWorkHours.Clear;
begin
  FTotal:= nil;
  FNight:= nil;
end;

procedure TWorkHours.SetTotal(const AVector: TIntVector);
begin
  FTotal:= VCut(AVector);
end;

procedure TWorkHours.SetNight(const AVector: TIntVector);
begin
  FNight:= VCut(AVector);
end;

procedure TWorkHours.Add(const ATotal: Integer; const ANight: Integer = 0);
begin
  VAppend(FTotal, ATotal);
  VAppend(FNight, ANight);
end;

procedure TWorkHours.Copy(const ADestination: TWorkHours);
begin
  ADestination.Total:= FTotal;
  ADestination.Night:= FNight;
end;

function TWorkHours.GetSumTotal: Integer;
begin
  GetSumTotal:= VSum(FTotal);
end;

function TWorkHours.GetSumNight: Integer;
begin
  GetSumNight:= VSum(FNight);
end;

{ФУНКЦИИ ПЕРЕСЧЕТА ПРЕДСТАВЛЕНИЙ ЧАСОВ}
function WeekHoursToWorkHoursInt(const AHoursInWeek: Byte): Integer;
begin
  WeekHoursToWorkHoursInt:= Trunc(AHoursInWeek*WORKHOURS_DENOMINATOR/5);
end;

function WeekHoursToWorkHoursFrac(const AHoursInWeek: Byte): Double;
begin
  WeekHoursToWorkHoursFrac:= AHoursInWeek/5;
end;

function WorkHoursFracToInt(const AWorkHours: Double): Integer;
begin
  WorkHoursFracToInt:= Round(AWorkHours*WORKHOURS_DENOMINATOR);
end;

function WorkHoursIntToFrac(const AWorkHours: Integer): Double;
begin
  WorkHoursIntToFrac:= AWorkHours/WORKHOURS_DENOMINATOR;
end;

function WorkHoursIntToFracStr(const AWorkHours: Integer): String;
begin
  WorkHoursIntToFracStr:= FloatToStr(AWorkHours/WORKHOURS_DENOMINATOR);
end;

function WorkHoursToStr(const AWorkHours: Integer;
                        const AFractionDigitsCount: Byte = FRACTION_DIGITS_IN_WORKHOURS;
                        const ANeedEndZerosInFrac: Boolean = False): String;
var
  S, S1: String;
  n,i: Byte;
begin
  Result:= EmptyStr;
  S1:= '0';
  S:= EmptyStr;
  n:= 0;
  if AWorkHours>0 then
  begin
    S:= IntToStr(AWorkHours);
    n:= Length(S);
    if n>AFractionDigitsCount then
    begin
      S1:= SDelCount(S, n - AFractionDigitsCount + 1, AFractionDigitsCount);
      S:= SDelCount(S, 1, n - AFractionDigitsCount);
    end;
    if not ANeedEndZerosInFrac then
    begin
      n:= Length(S);
      i:= n;
      repeat
        if not SSame(SSymbol(S, i), '0') then break;
        Dec(i);
      until i=0;
      if i<n then
        S:= SDelCount(S, i+1, n-i);
    end;
  end;
  Result:= S1;
  if (not SEmpty(S)) or ANeedEndZerosInFrac then
  begin
    Result:= Result + DefaultFormatSettings.DecimalSeparator;
    for i:= n+1 to AFractionDigitsCount do
      Result:= Result + '0';
  end;
  Result:= Result + S;
end;

end.

