unit UCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils,
  //Project utils
  UWorkHours;

const
  {статус дня}
  DAY_STATUS_UNKNOWN = 0; //0 - неизвестный
  DAY_STATUS_HOLIDAY = 1; //1 - праздничный
  DAY_STATUS_OFFDAY  = 2; //2 - выходной
  DAY_STATUS_BEFORE  = 3; //3 - предпраздничный (рабочий с сокращенным временем)
  DAY_STATUS_WEEKDAY = 4; //4 - будний (обычный рабочий)

  MAIN_COLOR_INDEX      = 0;  //scTransparent
  HOLIDEY_COLOR_INDEX   = 1;
  OFFDAY_COLOR_INDEX    = 2;
  BEFORE_COLOR_INDEX    = 3;
  WEEKDAY_COLOR_INDEX   = 4;
  MONTHNAME_COLOR_INDEX = 5;
  DAYNAME_COLOR_INDEX   = 6;
  HIGHLIGHT_COLOR_INDEX = 7;
  QUARTER_COLOR_INDEX   = 8;
  HALFYEAR_COLOR_INDEX  = 9;
  YEAR_COLOR_INDEX      = 10;

type

  {Простой календарь}
  TCustomCalendar = class (TObject)
    protected
      FCalculated         : Boolean;     //рассчитан ли календарь
      FDates              : TDateVector; //вектор дат
      FDayStatuses        : TIntVector;  //статус дня: 2 - выходной 4 - будний (обычный рабочий)
      FDayNumsInWeek      : TIntVector;  //номер дня в неделе
      FWeekNumsInMonth    : TIntVector;  //номер недели в месяце
    private
      function GetWeekDaysCount: Integer;
      function GetOffDaysCount: Integer;
      function GetBeginDate: TDate;
      function GetDaysCount: Integer;
      function GetEndDate: TDate;
    public
      procedure Clear;
      procedure Calc(const ABeginDate, AEndDate: TDate);
      property Calculated: Boolean read FCalculated;
      property BeginDate: TDate read GetBeginDate;   //начальная дата
      property EndDate: TDate read GetEndDate;       //конечная дата
      property DaysCount: Integer read GetDaysCount; //кол-во дней
      property Dates: TDateVector read FDates;
      property DayStatuses: TIntVector read FDayStatuses;
      property DayNumsInWeek: TIntVector read FDayNumsInWeek;
      property WeekNumsInMonth: TIntVector read FWeekNumsInMonth;
      property WeekDaysCount: Integer read GetWeekDaysCount;//кол-во будних дней
      property OffDaysCount: Integer read GetOffDaysCount;  //кол-во выходных дней
  end;

  {Корректировки производственного календаря}
  TCalendarCorrections = record
    Dates   : TDateVector;
    Statuses: TIntVector;
    SwapDays: TIntVector;
  end;

const
  CalendarCorrectionsEmpty: TCalendarCorrections =
    (Dates   : nil;
     Statuses: nil;
     SwapDays: nil;
    );

type

  {Производственный календарь}
  TCalendar = class (TCustomCalendar)
    protected
      FSwapDays: TIntVector;  //заменяемый день для корректировки выходного дня на статус "рабочий"
    private
      function GetHoliDaysCount: Integer;
      function GetBeforeCount: Integer;
      function GetWorkDaysCount: Integer;
      function GetNotWorkDaysCount: Integer;
    public
      procedure Clear;
      procedure Calc(const ABeginDate, AEndDate: TDate; const ACorrections: TCalendarCorrections);
      function Cut(const ABeginDate, AEndDate: TDate; var ACutCalendar: TCalendar): Boolean;
      function SumWorkHoursInt(const AHoursInWeek: Byte): Integer; //сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24) в целочисленном формате
      function SumWorkHoursFrac(const AHoursInWeek: Byte): Double; //сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24) в дробном формате
      property SwapDays: TIntVector read FSwapDays;
      property HoliDaysCount: Integer read GetHoliDaysCount;       //кол-во праздничных дней
      property BeforeCount: Integer read GetBeforeCount;           //кол-во предпраздничных дней
      property WorkDaysCount: Integer read GetWorkDaysCount;       //кол-во рабочих дней (будни+предпраздничные)
      property NotWorkDaysCount: Integer read GetNotWorkDaysCount; //кол-во нерабочих дней (выходные+праздничные)
  end;

  {MonthGridToDate расчет даты дня, взятого из ячейки [ARow, ACol]
   таблицы месячного календаря 6х7 для месяца AMonth;
  false - если вне месяца}
  function MonthGridToDate(const AWeekInMonth{ARow}, ADayInWeek{ACol}, AMonth, AYear: Integer;
                           out ADate: TDate): Boolean;

implementation



function MonthGridToDate(const AWeekInMonth, ADayInWeek, AMonth, AYear: Integer; out ADate: TDate): Boolean;
var
  i: Integer;
  D: TDate;
begin
  Result:= False;
  ADate:= 0;
  for i:= 1 to DaysInAMonth(AYear, AMonth) do
  begin
    D:= EncodeDate(AYear, AMonth, i);
    if (DayNumberInWeek(D)=ADayInWeek) and (WeekNumberInMonth(D)=AWeekInMonth) then
    begin
      ADate:= D;
      Result:= True;
      break;
    end;
  end;
end;

{TCustomCalendar}

procedure TCustomCalendar.Clear;
begin
  FCalculated:= False;
  FDates:= nil;
  FDayStatuses:= nil;
  FDayNumsInWeek:= nil;
  FWeekNumsInMonth:= nil;
end;

procedure TCustomCalendar.Calc(const ABeginDate, AEndDate: TDate);
var
  i, DC: Integer;
begin
  Clear;
  DC:= DaysBetweenDates(ABeginDate, AEndDate)+1;
  VDim(FDates, DC);
  VDim(FDayNumsInWeek, DC);
  VDim(FWeekNumsInMonth, DC);
  VDim(FDayStatuses, DC);
  for i:= 0 to DC - 1 do
  begin
      FDates[i]:= IncDay(ABeginDate, i);
      FDayNumsInWeek[i]:= DayNumberInWeek(FDates[i]);
      FWeekNumsInMonth[i]:= WeekNumberInMonth(FDates[i]);
      if FDayNumsInWeek[i]<6 then
        FDayStatuses[i]:= DAY_STATUS_WEEKDAY
      else
        FDayStatuses[i]:= DAY_STATUS_OFFDAY;
  end;
  FCalculated:= True;
end;

function TCustomCalendar.GetBeginDate: TDate;
begin
  GetBeginDate:= FDates[0];
end;

function TCustomCalendar.GetDaysCount: Integer;
begin
  GetDaysCount:= Length(FDates);
end;

function TCustomCalendar.GetEndDate: TDate;
begin
  GetEndDate:= FDates[High(FDates)];
end;

function TCustomCalendar.GetWeekDaysCount: Integer;
begin
  GetWeekDaysCount:= VCountIf(FDayStatuses, DAY_STATUS_WEEKDAY);
end;

function TCustomCalendar.GetOffDaysCount: Integer;
begin
  GetOffDaysCount:= VCountIf(FDayStatuses, DAY_STATUS_OFFDAY);
end;

{TCalendar}

procedure TCalendar.Clear;
begin
  inherited Clear;
  FSwapDays:= nil;
end;

procedure TCalendar.Calc(const ABeginDate, AEndDate: TDate; const ACorrections: TCalendarCorrections);
var
  i, n: Integer;
begin
  Clear;
  inherited Calc(ABeginDate, AEndDate);
  FCalculated:= False;
  VDim(FSwapDays, DaysCount, 0);
  for i:= 0 to High(ACorrections.Dates) do
  begin
    n:= DaysBetweenDates(BeginDate, ACorrections.Dates[i]);
    //добавляем только даты, входящие в период календаря
    if (n>=0) and (n<DaysCount) then
    begin
      FDayStatuses[n]:= ACorrections.Statuses[i];
      FSwapDays[n]:= ACorrections.SwapDays[i];
    end;
  end;
  FCalculated:= True;
end;

function TCalendar.Cut(const ABeginDate, AEndDate: TDate; var ACutCalendar: TCalendar): Boolean;
var
  BD, ED: TDate;
  I1, I2: Integer;
begin
  Cut:= False;
  //проверяем, рассчиатн ли исходный календарь
  if not FCalculated then Exit;
  //проверяем, пересекаются ли периоды обрезки и исходного календаря, запоминаем период пересечения
  if not IsPeriodIntersect(ABeginDate, AEndDate, BeginDate, EndDate, BD, ED) then Exit;
  //определяем индексы среза
  I1:= DaysBetweenDates(BeginDate, BD);
  I2:= DaysBetweenDates(BeginDate, ED);
  //проверяем на существование календаря-среза, создаем при отсутствии
  if not Assigned(ACutCalendar) then
    ACutCalendar:= TCalendar.Create
  else
    ACutCalendar.Clear;
  //заполняем данные
  ACutCalendar.FDates:= VCut(FDates, I1, I2);
  ACutCalendar.FSwapDays:= VCut(FSwapDays, I1, I2);
  ACutCalendar.FDayStatuses:= VCut(FDayStatuses, I1, I2);
  ACutCalendar.FDayNumsInWeek:= VCut(FDayNumsInWeek, I1, I2);
  ACutCalendar.FWeekNumsInMonth:= VCut(FWeekNumsInMonth, I1, I2) ;
  ACutCalendar.FCalculated:= True;
  Cut:= True;
end;

function TCalendar.SumWorkHoursInt(const AHoursInWeek: Byte): Integer; //сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24)
var
  x: Integer;
begin
  Result:= 0;
  if AHoursInWeek>0 then
  begin
    x:= WeekHoursToWorkHoursInt(AHoursInWeek);
    Result:= x*WeekDaysCount +
            (x - WORKHOURS_DENOMINATOR*REDUCE_HOURS_COUNT_IN_BEFORE)*BeforeCount;
  end;
end;

function TCalendar.SumWorkHoursFrac(const AHoursInWeek: Byte): Double; //сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24) в дробном формате
begin
  Result:= 0;
  if AHoursInWeek>0 then
    Result:= AHoursInWeek*WorkDaysCount/5   -
             REDUCE_HOURS_COUNT_IN_BEFORE*BeforeCount;
end;

function TCalendar.GetHoliDaysCount: Integer;
begin
  GetHoliDaysCount:= VCountIf(FDayStatuses, DAY_STATUS_HOLIDAY);
end;

function TCalendar.GetBeforeCount: Integer;
begin
  GetBeforeCount:= VCountIf(FDayStatuses, DAY_STATUS_BEFORE);
end;

function TCalendar.GetWorkDaysCount: Integer;
begin
  GetWorkDaysCount:= WeekDaysCount + BeforeCount;
end;

function TCalendar.GetNotWorkDaysCount: Integer;
begin
  GetNotWorkDaysCount:= HolidaysCount + OffDaysCount;
end;

end.

