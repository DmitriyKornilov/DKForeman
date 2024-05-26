unit UCalendar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils, DK_Const,
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
  CALENDAR_COLOR_COUNT  = 11;

type

  {Простой календарь}

  { TCustomCalendar }

  TCustomCalendar = class (TObject)
  protected
    FIsCalculated         : Boolean;     //рассчитан ли календарь
    FDates              : TDateVector; //вектор дат
    FDayStatuses        : TIntVector;  //статус дня: 2 - выходной 4 - будний (обычный рабочий)
    FDayNumsInWeek      : TIntVector;  //номер дня в неделе
    FWeekNumsInMonth    : TIntVector;  //номер недели в месяце
  private
    function GetBeginDate: TDate;
    function GetEndDate: TDate;
    function GetTypedDaysCount(const ABeginDate, AEndDate: TDate; const ADayType: Integer): Integer;
    function GetIntersectionIndexes(const ABeginDate, AEndDate: TDate;
                                    out AFirstIndex, ALastIndex: Integer): Boolean;
  public
    procedure Clear;
    procedure Calc(const ABeginDate, AEndDate: TDate);
    property IsCalculated: Boolean read FIsCalculated;
    property BeginDate: TDate read GetBeginDate;   //начальная дата
    property EndDate: TDate read GetEndDate;       //конечная дата
    property Dates: TDateVector read FDates;
    property DayStatuses: TIntVector read FDayStatuses;
    property DayNumsInWeek: TIntVector read FDayNumsInWeek;
    property WeekNumsInMonth: TIntVector read FWeekNumsInMonth;
    //кол-во дней
    function DaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
    //кол-во будних дней
    function WeekDaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
    //кол-во выходных дней
    function OffDaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
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

  { TCalendar }

  TCalendar = class (TCustomCalendar)
  private
    FSwapDays: TIntVector;  //заменяемый день для корректировки выходного дня на статус "рабочий"
  public
    procedure Clear;
    procedure Calc(const ABeginDate, AEndDate: TDate; const ACorrections: TCalendarCorrections);
    function Cut(const ABeginDate, AEndDate: TDate; var ACutCalendar: TCalendar): Boolean;
    //сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24) в целочисленном формате
    function SumWorkHoursInt(const AHoursInWeek: Byte;
      const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
    //сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24) в дробном формате
    function SumWorkHoursFrac(const AHoursInWeek: Byte;
      const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Double;
    property SwapDays: TIntVector read FSwapDays;
    //кол-во праздничных дней
    function HoliDaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
    //кол-во предпраздничных дней
    function BeforeDaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
    //кол-во рабочих дней (будни+предпраздничные)
    function WorkDaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
    //кол-во нерабочих дней (выходные+праздничные)
    function NotWorkDaysCount(const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
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
  FIsCalculated:= False;
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
  FIsCalculated:= True;
end;

function TCustomCalendar.GetBeginDate: TDate;
begin
  Result:= VFirst(FDates);
end;

function TCustomCalendar.DaysCount(const ABeginDate: TDate = NULDATE;
                                   const AEndDate: TDate = INFDATE): Integer;
var
  I1, I2: Integer;
begin
  Result:= 0;
  if not GetIntersectionIndexes(ABeginDate, AEndDate, I1, I2) then Exit;
  Result:= I2-I1+1;
end;

function TCustomCalendar.GetEndDate: TDate;
begin
  Result:= VLast(FDates);
end;

function TCustomCalendar.GetTypedDaysCount(const ABeginDate, AEndDate: TDate;
  const ADayType: Integer): Integer;
var
  I1, I2: Integer;
begin
  Result:= 0;
  if not GetIntersectionIndexes(ABeginDate, AEndDate, I1, I2) then Exit;
  Result:= VCountIf(FDayStatuses, ADayType, I1, I2);
end;

function TCustomCalendar.GetIntersectionIndexes(const ABeginDate, AEndDate: TDate;
                         out AFirstIndex, ALastIndex: Integer): Boolean;
var
  BD, ED: TDate;
begin
  AFirstIndex:= -1;
  ALastIndex:= -1;
  //проверяем, пересекаются ли периоды запроса и календаря, запоминаем период пересечения
  Result:= IsPeriodIntersect(ABeginDate, AEndDate, BeginDate, EndDate, BD, ED);
  if not Result then Exit;
  //определяем индексы дат периода пересечения
  AFirstIndex:= DaysBetweenDates(BeginDate, BD);
  ALastIndex:= DaysBetweenDates(BeginDate, ED);
end;

function TCustomCalendar.WeekDaysCount(const ABeginDate: TDate = NULDATE;
                                       const AEndDate: TDate = INFDATE): Integer;
begin
  Result:= GetTypedDaysCount(ABeginDate, AEndDate, DAY_STATUS_WEEKDAY);
end;

function TCustomCalendar.OffDaysCount(const ABeginDate: TDate = NULDATE;
                                       const AEndDate: TDate = INFDATE): Integer;
begin
  Result:= GetTypedDaysCount(ABeginDate, AEndDate, DAY_STATUS_OFFDAY);
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
  FIsCalculated:= False;
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
  FIsCalculated:= True;
end;

function TCalendar.Cut(const ABeginDate, AEndDate: TDate; var ACutCalendar: TCalendar): Boolean;
var
  I1, I2: Integer;
begin
  Result:= False;
  //проверяем, рассчиатн ли исходный календарь
  if not FIsCalculated then Exit;
  //определяем индексы среза
  if not GetIntersectionIndexes(ABeginDate, AEndDate, I1, I2) then Exit;
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
  ACutCalendar.FIsCalculated:= True;
  Result:= True;
end;

//сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24)
function TCalendar.SumWorkHoursInt(const AHoursInWeek: Byte;
  const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Integer;
var
  x: Integer;
begin
  Result:= 0;
  if AHoursInWeek>0 then
  begin
    x:= WeekHoursToWorkHoursInt(AHoursInWeek);
    Result:= x * WeekDaysCount(ABeginDate, AEndDate)  +
            (x - WORKHOURS_DENOMINATOR*REDUCE_HOURS_COUNT_IN_BEFORE)*BeforeDaysCount(ABeginDate, AEndDate);
  end;
end;

//сумма рабочих часов в зависимости от кол-ва часов в неделю AHoursInWeek (40, 36, 24) в дробном формате
function TCalendar.SumWorkHoursFrac(const AHoursInWeek: Byte;
  const ABeginDate: TDate = NULDATE; const AEndDate: TDate = INFDATE): Double;
begin
  Result:= 0;
  if AHoursInWeek>0 then
    Result:= AHoursInWeek*WorkDaysCount(ABeginDate, AEndDate)/5   -
             REDUCE_HOURS_COUNT_IN_BEFORE*BeforeDaysCount(ABeginDate, AEndDate);
end;

function TCalendar.HoliDaysCount(const ABeginDate: TDate = NULDATE;
                                 const AEndDate: TDate = INFDATE): Integer;
begin
  Result:= GetTypedDaysCount(ABeginDate, AEndDate, DAY_STATUS_HOLIDAY);
end;

function TCalendar.BeforeDaysCount(const ABeginDate: TDate = NULDATE;
                                 const AEndDate: TDate = INFDATE): Integer;
begin
  Result:= GetTypedDaysCount(ABeginDate, AEndDate, DAY_STATUS_BEFORE);
end;

function TCalendar.WorkDaysCount(const ABeginDate: TDate = NULDATE;
                                 const AEndDate: TDate = INFDATE): Integer;
begin
  Result:= WeekDaysCount(ABeginDate, AEndDate) + BeforeDaysCount(ABeginDate, AEndDate);
end;

function TCalendar.NotWorkDaysCount(const ABeginDate: TDate = NULDATE;
                                 const AEndDate: TDate = INFDATE): Integer;
begin
  Result:= HolidaysCount(ABeginDate, AEndDate) + OffDaysCount(ABeginDate, AEndDate);
end;

end.

