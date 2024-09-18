unit UTimetable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils, DK_StrUtils, DK_Const,
  //Project utils
  UWorkHours, UCalendar, USchedule;

const
  SPACE1 = ' ';
  SPACE2 = '/';//'   ';
  SPACE3 = '/';
  {типы меток табеля}
  MARKTYPE_UNKNOWN  = 0;
  MARKTYPE_PRESENCE = 1; //явка
  MARKTYPE_ABSENCE  = 2; //неявка
  MARKTYPE_OVERTIME = 3; //сверхурочно
  MARKTYPE_FREEDAY  = 4; //выходной
  {ID графика для ручной корректировки табеля}
  MANUAL_SCHEDULEID = -1;
  {отсутствие в течение всей смены}
  FULLSHIFT_SKIPHOURS = -1;
  {флаги изменения вручную}
  MANUAL_NO  = 0;
  MANUAL_YES = 1;
  {флаги неявки}
  ABSENCE_NO  = 0;
  ABSENCE_YES = 1;
  {флаг наличия в базе}
  INBASE_NO   = 0;
  INBASE_YES  = 1;
  {флаги существования}
  EXISTS_NO  = 0;
  EXISTS_YES = 1;
  {флаги заданности графика}
  DEFINE_NO  = 0;
  DEFINE_YES = 1;

  STRMARK_NIGHT       = 'Н';
  STRMARK_OVER        = 'С';
  OUTSIDEMONTHSTR     = 'X'; //метка дня за пределом месяца
  STRMARK_NONEXISTENT = 'X';

  GRID_COLOR_INDEX         = 0;
  MANUAL_COLOR_INDEX       = 1;
  HOLIDAY_COLOR_INDEX      = 2;
  BEFORE_COLOR_INDEX       = 3;
  TITLE_COLOR_INDEX        = 4;
  OUTSIDEMONTH_COLOR_INDEX = 5;
  NOTDEFINE_COLOR_INDEX    = 6;
  HIGHLIGHT_COLOR_INDEX    = 7;
  NOTWORK_COLOR_INDEX      = 8;
  TIMETABLE_COLOR_COUNT    = 9;

type
  TTimetableDay = record
    ScheduleHours: Integer;
    TotalHours: Integer;
    NightHours: Integer;
    OverHours:  Integer;
    SkipHours:  Integer;
    DigMark  :  Integer;
    SkipMark :  Integer;
    ScheduleID: Integer;
    ShiftNum: Integer;
  end;

const
  TimetableDayEmpty:  TTimetableDay =
  ( ScheduleHours: 0;
    TotalHours: 0;
    NightHours: 0;
    OverHours:  0;
    SkipHours:  0;
    DigMark  :  0; //?
    SkipMark :  0;
    ScheduleID: 0;
    ShiftNum: 0;
  );

type
  //итоговые данные за год  (1,2,3,4 - данные за соответствующий квартал, 5,6 - 1 и 2  полугодие, 7 - за год
  TTimetableTotals = record
    ShiftCount: array [1..7] of Integer;
    WorkDaysCount: array [1..7] of Integer;
    NotWorkDaysCount: array [1..7] of Integer;
    TotalHours: array [1..7] of Integer;
    NightHours: array [1..7] of Integer;
    OverHours: array [1..7] of Integer;
    HolidayHours: array [1..7] of Integer;
    SkipDaysCount: array [1..7] of Integer;
    SkipHours: array [1..7] of Integer;
    SkipMarksStr: array [1..7] of AnsiString;
    SkipDaysHoursStr: array [1..7] of AnsiString;
  end;

  { Табель на месяц }

  { TTimetable }

  TTimetable = class (TObject)
    private
      FTabNumID           : Integer;   //ID табельного номера
      FTabNum             : String;    //табельный номер
      FRecrutDate         : TDate;     //дата приема на работу
      FDismissDate        : TDate;     //дата увольнения

      FMonthCalendar      : TCalendar;
      FNightMarkStr       : String;
      FOverMarkStr        : String;

      FDates              : TDateVector;
      FScheduleIDs        : TIntVector;   //вектор значений ID графиков на каждый день
      FShiftNums          : TIntVector;     //вектор знаяений номеров смен на каждый день
      FIsExists           : TIntVector;  //вектор флагов существоваия табеля
      FIsManualChanged    : TIntVector;  //вектор флагов корректировок (изменения вручную)
      FIsAbsence          : TIntVector;  //вектор флагов неявки (TIMETABLEMARK.Typemark=2)
      FIsDayInBase        : TIntVector;  //вектор флагов существования дня в базе
      FMarksMainDig       : TIntVector;  //цифровые коды основной метки табеля
      FMarksMainStr       : TStrVector;  //буквенные коды основной метки табеля
      FMarksSkipDig       : TIntVector;  //цифровые коды метки отсутствия табеля
      FMarksSkipStr       : TStrVector;  //буквенные коды метки отсутствия табеля
      FMarksStr           : TStrVector;  {'Я/Н/C'}
      FHoursStr           : TStrVector;  {'5/2/1'}
      FHoursTotal         : TIntVector;  {5}
      FHoursNight         : TIntVector;  {2}
      FHoursOver          : TIntVector;  {1}
      FHoursSkip          : TIntVector;  {0}
      FHoursSchedule      : TIntVector;  {4}

      FSumHoursTotalHalf1 : Integer; //сумма часов за 1 половину месяца
      FSumHoursTotalHalf2 : Integer; //сумма часов за 2 половину месяца
      FSumHoursTotalMonth : Integer; //сумма часов за месяц
      FSumHoursNightHalf1 : Integer; //сумма ночных часов за 1 половину месяца
      FSumHoursNightHalf2 : Integer; //сумма ночных часов за 2 половину месяца
      FSumHoursNightMonth : Integer; //сумма ночных часов за месяц
      FWorkDaysCountHalf1 : Integer; //кол-во рабочих дней за 1 половину месяца
      FWorkDaysCountHalf2 : Integer; //кол-во рабочих дней за 2 половину месяца
      FWorkDaysCountMonth : Integer; //кол-во рабочих дней за месяц
      FSumHoursOverHalf1  : Integer; //сумма сверхурочных часов за 1 половину месяца
      FSumHoursOverMonth  : Integer; //сумма сверхурочных часов за месяц
      FSumHoursHolidayHalf1: Integer;//сумма праздничных часов за 1 половину месяца
      FSumHoursHolidayMonth: Integer;//сумма праздничных часов за месяц
      FShiftCountHalf1: Integer;   //кол-во смен за 1 половину месяца
      FShiftCountHalf2: Integer;   //кол-во смен за 2 половину месяца
      FShiftCountMonth: Integer;   //кол-во смен за месяц
      FSumHoursSkipHalf1  : Integer; //кол-во часов неявок за 1 половину месяцa
      FSumHoursSkipMonth  : Integer; //кол-во часов неявок за месяц
      FSkipDaysCountHalf1 : Integer; //кол-во дней неявок за 1 половину месяцa
      FSkipDaysCountMonth : Integer; //кол-во дней неявок за месяц
      FMarksSkipStrMonth  : String; //строка перечня кодов неявок за месяц
      FDaysHoursSkipStrMonth: String; //строка перечня дней (часов) неявок за месяц
      FMarksSkipStrHalf1  : String; //строка перечня кодов неявок за 1 половину месяца
      FDaysHoursSkipStrHalf1: String; //строка перечня дней (часов) неявок за 1 половину месяца
      FMarksSkipStrHalf2  : String; //строка перечня кодов неявок за 2 половину месяца
      FDaysHoursSkipStrHalf2: String; //строка перечня дней (часов) неявок за 2 половину месяца
      FNotWorkDaysCountHalf1: Integer; //кол-во нерабочих (выходных и праздников) за 1 половину месяца
      FNotWorkDaysCountMonth: Integer; //кол-во нерабочих (выходных и праздников) в месяц
      FSumHoursQuartFullMonth: Integer; //отработано часов за текущий квартал с учетом полного текущего месяца
      FSumHoursQuartHalfMonth: Integer; //отработано часов за текущий квартал с учетом половины текущего месяца
      FSumHoursYearFullMonth : Integer; //отработано часов за текущий год с учетом полного текущего месяца
      FSumHoursYearHalfMonth : Integer; //отработано часов за текущий год с учетом половины текущего месяца

      function GetBeginDate: TDate;
      function GetEndDate: TDate;
      procedure GetSkipStrings(const ABeginInd, AEndInd: Integer;
                               out AMarksSkipStr, ADaysHoursSkipStr: String);

      function DateIndex(const ADate: TDate; out AIndex: Integer): Boolean;
      function FromToIndexes(const AFromDate, AToDate: TDate; out AIndex1, AIndex2: Integer): Boolean;

      procedure WriteDefault;
      procedure WriteExistions(const APostScheduleInfo: TPostScheduleInfo);
      procedure WriteData;
      procedure WriteResume;
      procedure WriteHoursInReportPeriod;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure Calc(const ATabNumID: Integer; const ATabNum: String;
                     const ARecrutDate, ADismissDate: TDate;
                     const AMonthCalendar: TCalendar;
                     const APostScheduleInfo: TPostScheduleInfo;
                     const ANightMarkStr: String = STRMARK_NIGHT;
                     const AOverMarkStr: String = STRMARK_OVER);

      class function CalcShiftCount(const AScheduleIDs, AShiftNums: TIntVector;
                                    const ABeginIndex, AEndIndex: Integer): Integer;

      property Calendar: TCalendar read FMonthCalendar;
      property TabNumID: Integer read FTabNumID;
      property TabNum: String read FTabNum;
      property RecrutDate: TDate read FRecrutDate;
      property DismissDate: TDate read FDismissDate;

      property Dates: TDateVector read FDates;
      property BeginDate: TDate read GetBeginDate;
      property EndDate: TDate read GetEndDate;

      property IsExists: TIntVector read FIsExists;
      property IsManualChanged: TIntVector read FIsManualChanged;
      property IsAbsence: TIntVector read FIsAbsence;
      property IsDayInBase: TIntVector read FIsDayInBase;
      function IsDateExists(const ADate: TDate): Boolean;

      property MarksMainDig: TIntVector read FMarksMainDig;
      property MarksMainStr: TStrVector read FMarksMainStr;
      property MarksSkipDig: TIntVector read FMarksSkipDig;
      property MarksSkipStr: TStrVector read FMarksSkipStr;

      property MarksStr: TStrVector read FMarksStr;
      property HoursStr: TStrVector read FHoursStr;

      property HoursTotal: TIntVector read FHoursTotal;
      property HoursNight: TIntVector read FHoursNight;
      property HoursOver: TIntVector read FHoursOver;
      property HoursSkip: TIntVector read FHoursSkip;
      property HoursSchedule: TIntVector read FHoursSchedule;

      property SumHoursTotalHalf1: Integer read FSumHoursTotalHalf1;
      property SumHoursTotalHalf2: Integer read FSumHoursTotalHalf2;
      property SumHoursTotalMonth: Integer read FSumHoursTotalMonth;
      property SumHoursNightHalf1: Integer read FSumHoursNightHalf1;
      property SumHoursNightHalf2: Integer read FSumHoursNightHalf2;
      property SumHoursNightMonth: Integer read FSumHoursNightMonth;

      property WorkDaysCountHalf1: Integer read FWorkDaysCountHalf1;
      property WorkDaysCountHalf2: Integer read FWorkDaysCountHalf2;
      property WorkDaysCountMonth: Integer read FWorkDaysCountMonth;

      property ShiftCountHalf1: Integer read FShiftCountHalf1;
      property ShiftCountHalf2: Integer read FShiftCountHalf2;
      property ShiftCountMonth: Integer read FShiftCountMonth;

      property SumHoursOverMonth: Integer read FSumHoursOverMonth;
      property SumHoursOverHalf1: Integer read FSumHoursOverHalf1;
      property SumHoursHolidayHalf1: Integer read FSumHoursHolidayHalf1;
      property SumHoursHolidayMonth: Integer read FSumHoursHolidayMonth;

      property NotWorkDaysCountHalf1: Integer read FNotWorkDaysCountHalf1;
      property NotWorkDaysCountMonth: Integer read FNotWorkDaysCountMonth;
      property SkipDaysCountHalf1: Integer read FSkipDaysCountHalf1;
      property SkipDaysCountMonth: Integer read FSkipDaysCountMonth;
      property SumHoursSkipHalf1: Integer read FSumHoursSkipHalf1;
      property SumHoursSkipMonth: Integer read FSumHoursSkipMonth;
      property MarksSkipStrMonth: String read FMarksSkipStrMonth;
      property DaysHoursSkipStrMonth: String read FDaysHoursSkipStrMonth;
      property MarksSkipStrHalf1: String read FMarksSkipStrHalf1;
      property DaysHoursSkipStrHalf1: String read FDaysHoursSkipStrHalf1;
      property MarksSkipStrHalf2: String read FMarksSkipStrHalf2;
      property DaysHoursSkipStrHalf2: String read FDaysHoursSkipStrHalf2;
      property SumHoursQuartFullMonth: Integer read FSumHoursQuartFullMonth;
      property SumHoursQuartHalfMonth: Integer read FSumHoursQuartHalfMonth;
      property SumHoursYearFullMonth: Integer read FSumHoursYearFullMonth;
      property SumHoursYearHalfMonth: Integer read FSumHoursYearHalfMonth;
      property ScheduleIDs: TIntVector read FScheduleIDs;
      property ShiftNums: TIntVector read FShiftNums;
  end;

  TTimetableVector = array of TTimetable;
  procedure VTCreate(var V: TTimetableVector; const Count: Integer);
  procedure VTClear(var V: TTimetableVector);
  procedure VTAppend(var V: TTimetableVector; const NewValue: TTimetable);
  procedure VTDel(var V: TTimetableVector; Index1: Integer = -1; Index2: Integer = -1);
  procedure VTSwap(var V: TTimetableVector; const Index1, Index2: Integer);

  {ВСПОМ ПРОЦЕДУРЫ}
//даные табеля в строку итогов за день
function TimetableDataToDayStr(const AMainMark, ASkipMark, ANightMark, AOverMark: String;
                           const ATotalHours, ANightHours, AOverHours, ASkipHours: Integer): String;
//даные табеля в строку кодов ('Я/Н') и строку часов ('4/2') за день
procedure TimetableDataToMarksAndHoursStr(const AMainMark, ASkipMark,ANightMark, AOverMark: String;
                           const ATotalHours, ANightHours, AOverHours, ASkipHours: Integer;
                           out AMarksStr, AHoursStr: String);
//получаем данные по индексу AIndex из графика ASchedule для табеля
function TimetableDayDataFromSchedule(const ASchedule: TPersonalSchedule;
                                  const ADayStatus, AIndex: Integer): TTimetableDay;

function TimetableIsManualChanged(const AScheduleID: Integer): Integer;
function TimetableIsAbsence(const AMarkType: Integer): Integer;

implementation

uses UTimingUtils;

procedure VTCreate(var V: TTimetableVector; const Count: Integer);
var
  i: Integer;
begin
  SetLength(V, Count);
  for i:= 0 to High(V) do
    V[i]:= TTimetable.Create;
end;

procedure VTClear(var V: TTimetableVector);
var
  i: Integer;
begin
  for i:= 0 to High(V) do
    V[i].Clear;
end;

procedure VTAppend(var V: TTimetableVector; const NewValue: TTimetable);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);
  V[N]:= NewValue;
end;

procedure VTDel(var V: TTimetableVector; Index1: Integer; Index2: Integer);
var
  i, OldSize, DelLength: Integer;
begin
  //старый размер вектора
  OldSize:= Length(V);
  //если вектор пуст - выход
  if OldSize=0 then Exit;
  //если индексы не заданы - удаляем весь вектор
  if (Index1=-1) and (Index2=-1) then
  begin
    Index1:=0;
    Index2:=High(V);
  end
  else begin
    //если второй индекс меньше первого - операция сводится к удалению одного элемента
    if Index2< Index1 then Index2:= Index1;
    //проверка корректности индексов
    i:= High(V);
    if not (CheckIndex(i, Index1) and CheckIndex(i, Index2)) then Exit;
  end;
  //длина удаляемого сегмента
  DelLength:= Index2 - Index1 + 1;
  //уничтожаем элементы от Index1 до Index2
  for i:= Index1 to Index2 do if Assigned(V[i]) then FreeAndNil(V[i]);
  //переносим элементы с конца вектора на места удаленных
  for i:= Index2+1 to OldSize-1 do  V[i-DelLength]:= V[i];
  //усекаем вектор
  SetLength(V, OldSize - DelLength);
end;

procedure VTSwap(var V: TTimetableVector; const Index1, Index2: Integer);
var
  TmpValue: TTimetable;
begin
  if not CheckIndexes(High(V), Index1, Index2) then Exit;
  TmpValue:= V[Index1];
  V[Index1]:= V[Index2];
  V[Index2]:= TmpValue;
end;

{TTimetable}

procedure TTimetable.Clear;
begin
  FTabNumID:= 0;
  FTabNum:= EmptyStr;
  FRecrutDate:= 0;
  FDismissDate:= 0;

  //FIsWritedInBase:= False;
  FMonthCalendar.Clear;
  FNightMarkStr:= STRMARK_NIGHT;
  FOverMarkStr:= STRMARK_OVER;

  FDates:= nil;
  FIsExists:= nil;
  FIsManualChanged:= nil;
  FIsAbsence:= nil;
  FIsDayInBase:= nil;
  FMarksMainDig:= nil;
  FMarksMainStr:= nil;
  FMarksSkipDig:= nil;
  FMarksSkipStr:= nil;
  FMarksStr:= nil;
  FHoursStr:= nil;
  FHoursTotal:= nil;
  FHoursNight:= nil;
  FHoursOver:= nil;
  FHoursSkip:= nil;
  FHoursSchedule:= nil;
  FSumHoursTotalHalf1:= 0;
  FSumHoursTotalHalf2:= 0;
  FSumHoursTotalMonth:= 0;
  FSumHoursNightHalf1:= 0;
  FSumHoursNightHalf2:= 0;
  FSumHoursNightMonth:= 0;
  FWorkDaysCountHalf1:= 0;
  FWorkDaysCountHalf2:= 0;
  FWorkDaysCountMonth:= 0;
  FSumHoursOverHalf1:= 0;
  FSumHoursOverMonth:= 0;
  FSumHoursHolidayHalf1:= 0;
  FSumHoursHolidayMonth:= 0;
  FShiftCountHalf1:= 0;
  FShiftCountHalf2:= 0;
  FShiftCountMonth:= 0;
  FSumHoursSkipHalf1:= 0;
  FSumHoursSkipMonth:= 0;
  FSkipDaysCountHalf1:= 0;
  FSkipDaysCountMonth:= 0;
  FMarksSkipStrMonth:= EmptyStr;
  FDaysHoursSkipStrMonth:= EmptyStr;
  FMarksSkipStrHalf1:= EmptyStr;
  FDaysHoursSkipStrHalf1:= EmptyStr;
  FMarksSkipStrHalf2:= EmptyStr;
  FDaysHoursSkipStrHalf2:= EmptyStr;
  FNotWorkDaysCountHalf1:= 0;
  FNotWorkDaysCountMonth:= 0;
  FSumHoursQuartFullMonth:= 0;
  FSumHoursQuartHalfMonth:= 0;
  FSumHoursYearFullMonth:= 0;
  FSumHoursYearHalfMonth:= 0;
  FScheduleIDs:= nil;
  FShiftNums:= nil;
  //FHolidayDates:= nil;
end;

procedure TTimetable.Calc(const ATabNumID: Integer; const ATabNum: String;
                     const ARecrutDate, ADismissDate: TDate;
                     const AMonthCalendar: TCalendar;
                     const APostScheduleInfo: TPostScheduleInfo;
                     const ANightMarkStr: String = STRMARK_NIGHT;
                     const AOverMarkStr: String = STRMARK_OVER);
begin
  Clear;

  FTabNumID:= ATabNumID;
  FTabNum:= ATabNum;
  FRecrutDate:= ARecrutDate;
  FDismissDate:= ADismissDate;
  //FPostScheduleInfo:= APostScheduleInfo;

  FNightMarkStr:= ANightMarkStr;
  FOverMarkStr:= AOverMarkStr;
  AMonthCalendar.Cut(FMonthCalendar);
  //FHolidayDates:= AHolidayDates;

  WriteDefault;
  WriteExistions(APostScheduleInfo);
  WriteData;
  WriteResume;
  WriteHoursInReportPeriod;
end;

class function TTimetable.CalcShiftCount(const AScheduleIDs, AShiftNums: TIntVector;
                         const ABeginIndex, AEndIndex: Integer): Integer;
var
  i, ShiftNum, SchedID: Integer;
begin
  Result:= 0;
  ShiftNum:= 0;
  SchedID:= 0;
  for i:=ABeginIndex to AEndIndex do
  begin
    if AShiftNums[i]>0 then //есть смена
    begin
      if AScheduleIDs[i]<>SchedID then  //изменился график
      begin
        SchedID:= AScheduleIDs[i]; //запоминаем новый график
        ShiftNum:= AShiftNums[i]; //запоминаем новую смену
        Inc(Result);
      end
      else begin //график тот-же
        if AShiftNums[i]<>ShiftNum then //изменилась смена в этом графике
        begin
          ShiftNum:= AShiftNums[i]; //запоминаем новую смену
          Inc(Result);
        end;
      end;
    end
    else begin
      ShiftNum:= 0;
      SchedID:= 0;
    end;
  end;
end;

function TTimetable.IsDateExists(const ADate: TDate): Boolean;
var
  Index: Integer;
begin
  Result:= DateIndex(ADate, Index);
  if not Result then Exit;
  Result:= IsExists[Index]=EXISTS_YES;
end;

procedure TTimetable.WriteResume;
var
  i, a, b: Integer;
begin
  a:= 0;
  b:= High(FDates);
  //кол-во смен
  FShiftCountHalf1:= TTimetable.CalcShiftCount(FScheduleIDs, FShiftNums, a, 14);
  FShiftCountHalf2:= TTimetable.CalcShiftCount(FScheduleIDs, FShiftNums, 15, b);
  FShiftCountMonth:= TTimetable.CalcShiftCount(FScheduleIDs, FShiftNums, a, b);
  //суммы часов
  FSumHoursTotalHalf1:= VSum(FHoursTotal, a, 14);
  FSumHoursTotalHalf2:= VSum(FHoursTotal, 15, b);
  FSumHoursTotalMonth:= FSumHoursTotalHalf1 + FSumHoursTotalHalf2;
  //суммы ночных часов
  FSumHoursNightHalf1:= VSum(FHoursNight, a, 14);
  FSumHoursNightHalf2:= VSum(FHoursNight, 15, b);
  FSumHoursNightMonth:= FSumHoursNightHalf1 + FSumHoursNightHalf2;
  //кол-во рабочих дней
  FWorkDaysCountHalf1:= VCountIfNot(FHoursTotal, 0, a, 14);
  FWorkDaysCountHalf2:= VCountIfNot(FHoursTotal, 0, 15, b);
  FWorkDaysCountMonth:= FWorkDaysCountHalf1 + FWorkDaysCountHalf2;
  //сумма сверхурочных часов
  FSumHoursOverHalf1:= VSum(FHoursOver, 0, 14);
  FSumHoursOverMonth:= VSum(FHoursOver);
  //сумма праздничных часов
  FSumHoursHolidayHalf1:= VSumIf(FHoursTotal, FMarksMainDig, 3 {РВ=03}, 0, 14);
  FSumHoursHolidayMonth:= VSumIf(FHoursTotal, FMarksMainDig, 3 {РВ=03});
  //кол-во часов неявок за месяц
  FSumHoursSkipHalf1:= VSumIfNot(FHoursSkip, FHoursSkip, -1, 0, 14) +
                       VSumIf(FHoursSchedule, FHoursSkip, -1, 0, 14);
  FSumHoursSkipMonth:= VSumIfNot(FHoursSkip, FHoursSkip, -1) +
                       VSumIf(FHoursSchedule, FHoursSkip, -1);  //сумма пропущенных часов части смены + сумма пропущенных часов полных смен
  //кол-во дней неявок за месяц
  FSkipDaysCountHalf1:= VCountIfNot(FMarksSkipDig, 0 {нет неявки}, 0, 14);
  FSkipDaysCountMonth:= VCountIfNot(FMarksSkipDig, 0 {нет неявки});
  //строка перечня кодов неявок за 1 и 2 половины месяца
  //строка перечня дней (часов) неявок за 1 и 2 половины месяца
  GetSkipStrings(a, 14, FMarksSkipStrHalf1, FDaysHoursSkipStrHalf1);
  GetSkipStrings(15, b, FMarksSkipStrHalf2, FDaysHoursSkipStrHalf2);
  //строка перечня кодов неявок за месяц
  //строка перечня дней (часов) неявок за месяц
  GetSkipStrings(a, b, FMarksSkipStrMonth, FDaysHoursSkipStrMonth);
  //кол-во нерабочих (выходных и праздников) в месяц
  FNotWorkDaysCountMonth:= 0;
  for i:=0 to b do
  begin
    If (FHoursTotal[i]=0) and (FMarksSkipDig[i]=0) then
      FNotWorkDaysCountMonth:= FNotWorkDaysCountMonth + 1;
  end;
  FNotWorkDaysCountHalf1:= 0;
  for i:=0 to 14 do
  begin
    If (FHoursTotal[i]=0) and (FMarksSkipDig[i]=0) then
      FNotWorkDaysCountHalf1:= FNotWorkDaysCountHalf1 + 1;
  end;
end;

procedure TTimetable.WriteHoursInReportPeriod;
var
  M, Y, D: Word;
  H: Integer;
  BD, ED: TDate;
begin
  DecodeDate(BeginDate, Y, M, D);
  //ЧАСЫ ЗА ТЕКУЩИЙ КВАРТАЛ
  BD:= FirstDayInQuarter(QuarterNumber(M), Y); //дата первого дня в квратале
  if SameDate(BeginDate, BD) then  //этот месяц первый в квартале
  begin
    FSumHoursQuartFullMonth:= FSumHoursTotalMonth;
    FSumHoursQuartHalfMonth:= FSumHoursTotalHalf1;
  end
  else begin
    ED:= IncDay(BeginDate, -1); //последний день предыдущего месяца
    //достаем часы, отработанные в квартале до текущего месяца
    H:= TimetableSumTotalHoursInPeriod(FTabNumID, BD, ED);
    //добавляем часы, отработанные в текущем месяце
    FSumHoursQuartFullMonth:= H + FSumHoursTotalMonth;
    FSumHoursQuartHalfMonth:= H + FSumHoursTotalHalf1;
  end;
  //ЧАСЫ ЗА ТЕКУЩИЙ ГОД
  BD:= EncodeDate(Y,1,1); //дата первого дня в году
  if M=1 then //текущий месяц первый в году
  begin
    FSumHoursYearFullMonth:= FSumHoursTotalMonth;
    FSumHoursYearHalfMonth:= FSumHoursTotalHalf1;
  end
  else begin
    ED:= IncDay(BeginDate, -1); //последний день предыдущего месяца
    //достаем часы, отработанные в году до текущего месяца
    H:= TimetableSumTotalHoursInPeriod(FTabNumID, BD, ED);
    //добавляем часы, отработанные в текущем месяце
    FSumHoursYearFullMonth:= H + FSumHoursTotalMonth;
    FSumHoursYearHalfMonth:= H + FSumHoursTotalHalf1;
  end;
end;

constructor TTimetable.Create;
begin
  inherited Create;
  FMonthCalendar:= TCalendar.Create;
  Clear;
end;

destructor TTimetable.Destroy;
begin
  FreeAndNil(FMonthCalendar);
  inherited Destroy;
end;

procedure TTimetable.WriteDefault;
var
  N: Integer;
begin
  //дефолтные значения векторов данных
  FDates:= VCut(Calendar.Dates);

  N:= Calendar.DaysCount;

  VDim(FScheduleIDs, N, 0);
  VDim(FShiftNums, N, 0);

  VDim(FIsExists, N, EXISTS_NO);
  VDim(FIsManualChanged, N, MANUAL_NO);
  VDim(FIsAbsence, N, ABSENCE_NO);
  VDim(FIsDayInBase, N, INBASE_NO);

  VDim(FHoursTotal, N, 0);
  VDim(FHoursNight, N, 0);
  VDim(FHoursOver, N, 0);
  VDim(FHoursSkip, N, 0);
  VDim(FHoursSchedule, N, 0);

  VDim(FMarksMainDig, N, 0);
  VDim(FMarksSkipDig, N, 0);
  VDim(FMarksMainStr, N, EmptyStr);
  VDim(FMarksSkipStr, N, EmptyStr);

  VDim(FMarksStr, N, EmptyStr);
  VDim(FHoursStr, N, EmptyStr);
end;

procedure TTimetable.WriteExistions(const APostScheduleInfo: TPostScheduleInfo);
var
  i, j, m, n, I1, I2: Integer;
  BD, ED: TDate;
  ShiftScheduleInfo: TShiftScheduleInfo;
begin
  if VIsNil(APostScheduleInfo.PostIDs) then
  begin
    if not IsPeriodIntersect(RecrutDate, DismissDate, BeginDate, EndDate, BD, ED) then Exit;
    FromToIndexes(BD, ED, I1, I2);
    //меняем флаги в интервале существования
    VChangeIn(FIsExists, EXISTS_YES, I1, I2);
    Exit;
  end;

  for i:= 0 to High(APostScheduleInfo.PostIDs) do
    for j:= 0 to High(APostScheduleInfo.FirstDates[i]) do
    begin
      ShiftScheduleInfo:= APostScheduleInfo.Infos[i, j];
      for m:= 0 to High(ShiftScheduleInfo.ScheduleIDs) do
        for n:= 0 to High(ShiftScheduleInfo.FirstDates[m]) do
        begin
          //подпериод в сменном графике
          BD:= ShiftScheduleInfo.FirstDates[m, n];
          ED:= ShiftScheduleInfo.LastDates[m, n];
          if not IsPeriodIntersect(BeginDate, EndDate, BD, ED, BD, ED) then continue;
          if not IsPeriodIntersect(RecrutDate, DismissDate, BD, ED, BD, ED) then continue;
          FromToIndexes(BD, ED, I1, I2);
          //меняем флаги в интервале существования
          VChangeIn(FIsExists, EXISTS_YES, I1, I2);
        end;
    end;
end;

procedure TTimetable.WriteData;
var
  i, j: Integer;
  DayDates: TDateVector;
  SchedIDs, ShNums, TotalHours, NightHours, OverHours, SkipHours,
  SchedHours, MainMarkDig, SkipMarkDig, ManualChanged, Absence: TIntVector;
  MainMarkStr, SkipMarkStr: TStrVector;
begin
  //достаем из базы данные по табелю
  if not TimetableDataVectorsLoad(FTabNumID, BeginDate, EndDate,
                       DayDates, SchedIDs, ShNums,
                       TotalHours, NightHours, OverHours, SkipHours, SchedHours,
                       MainMarkDig, SkipMarkDig, ManualChanged, Absence,
                       MainMarkStr, SkipMarkStr) then Exit;
  //записываем в вектора
  for i:= 0 to High(DayDates) do
  begin
    if not DateIndex(DayDates[i], j) then continue;
    if FIsExists[j]=EXISTS_NO then continue;

    FScheduleIDs[j]:= SchedIDs[i];
    FShiftNums[j]:= ShNums[i];

    FIsManualChanged[j]:= ManualChanged[i];
    FIsAbsence[j]:= Absence[i];
    FIsDayInBase[j]:= INBASE_YES;

    FHoursTotal[j]:= TotalHours[i];
    FHoursNight[j]:= NightHours[i];
    FHoursOver[j]:= OverHours[i];
    FHoursSkip[j]:= SkipHours[i];
    FHoursSchedule[j]:= SchedHours[i];

    FMarksMainDig[j]:= MainMarkDig[i];
    FMarksSkipDig[j]:= SkipMarkDig[i];
    FMarksMainStr[j]:= MainMarkStr[i];
    FMarksSkipStr[j]:= SkipMarkStr[i];

    TimetableDataToMarksAndHoursStr(MainMarkStr[i], SkipMarkStr[i],
                                FNightMarkStr, FOverMarkStr,
                                TotalHours[i], NightHours[i], OverHours[i], SkipHours[i],
                                FMarksStr[j], FHoursStr[j]);
  end;
end;

function TTimetable.DateIndex(const ADate: TDate; out AIndex: Integer): Boolean;
begin
  Result:= DateIndexInPeriod(ADate, BeginDate, EndDate, AIndex);
end;

function TTimetable.FromToIndexes(const AFromDate, AToDate: TDate;
                                  out AIndex1, AIndex2: Integer): Boolean;
begin
  Result:= DateIndex(AFromDate, AIndex1) and DateIndex(AToDate, AIndex2);
end;

procedure TTimetable.GetSkipStrings(const ABeginInd, AEndInd: Integer;
                               out AMarksSkipStr, ADaysHoursSkipStr: String);
var
  i,j,N,SumSkipDays, SumSkipHours: Integer;
  SkipMarkDig: TIntVector;
  SkipMarkStr: TStrVector;
begin
  AMarksSkipStr:= EmptyStr;
  ADaysHoursSkipStr:= EmptyStr;
  SkipMarkStr:= nil;
  SkipMarkDig:= nil;
  N:= 0;
  for i:= ABeginInd to AEndInd do   //выбираем из значений векторов кодов неявок список уникальных кодов
  begin
    if FMarksSkipDig[i]=0 then continue;
    if VIndexOf(SkipMarkDig, FMarksSkipDig[i])<0 then
    begin
      Inc(N);
      VReDim(SkipMarkDig, N, FMarksSkipDig[i]);
      VReDim(SkipMarkStr, N, FMarksSkipStr[i]);
    end;
  end;
  for i:= 0 to N-1 do  //пробегаем по списку уникальных кодов
  begin
    AMarksSkipStr:= AMarksSkipStr + SYMBOL_BREAK + SkipMarkStr[i]; //запись строки буквенных кодов
    SumSkipDays:= 0;
    SumSkipHours:= 0;
    for j:= ABeginInd to AEndInd do //подсчитываем кол-во дней и часов соответствующей неявки
    begin
      if FMarksSkipDig[j]=SkipMarkDig[i] then
      begin
        Inc(SumSkipDays);
        if FHoursSkip[j]=-1 then
          SumSkipHours:= SumSkipHours + FHoursSchedule[j]
        else
          SumSkipHours:= SumSkipHours + FHoursSkip[j];
      end;
    end;
    //запись строки дней (часов) неявок
    ADaysHoursSkipStr:= ADaysHoursSkipStr + SYMBOL_BREAK +
                        IntToStr(SumSkipDays) + ' (' + WorkHoursIntToFracStr(SumSkipHours) + ')';
  end;
  AMarksSkipStr:= STrim(AMarksSkipStr);
  ADaysHoursSkipStr:= STrim(ADaysHoursSkipStr);
end;

function TTimetable.GetBeginDate: TDate;
begin
  Result:= VFirst(FDates);
end;

function TTimetable.GetEndDate: TDate;
begin
  Result:= VLast(FDates);
end;

{ВСПОМ ПРОЦЕДУРЫ}

function TimetableDataToDayStr(const AMainMark, ASkipMark, ANightMark, AOverMark: String;
                           const ATotalHours, ANightHours, AOverHours, ASkipHours: Integer): String;
begin
  if ATotalHours=0 then //нет рабочих часов или пропущено все
    Result:= AMainMark //напр-р ОТ или В
  else begin // есть раб часы
    if ANightHours=0 then //нет ночных (только дневные)
    begin
      if ASkipHours=0 then //нет пропуска часов
        Result:= AMainMark + SPACE1 + WorkHoursToStr(ATotalHours)
      else //пропущена часть часов
        Result:= AMainMark + SPACE1 + WorkHoursToStr(ATotalHours) + SPACE2 +
                 ASkipMark + SPACE1 + WorkHoursToStr(ASkipHours);
    end
    else begin //есть ночные
      if ATotalHours = ANightHours then  //только ночные
      begin
        if ASkipHours=0 then //нет пропуска часов
          Result:= ANightMark + SPACE1 + WorkHoursToStr(ANightHours)
        else //пропущена часть часов
          Result:= ANightMark + SPACE1 + WorkHoursToStr(ANightHours) + SPACE2 +
                   ASkipMark + SPACE1 + WorkHoursToStr(ASkipHours);
      end
      else //и дневные и ночные
        if ASkipHours=0 then //нет пропуска часов
          Result:= AMainMark + SPACE1 + WorkHoursToStr(ATotalHours) + SPACE2 +
                   ANightMark + SPACE1 + WorkHoursToStr(ANightHours)
        else //пропущена часть часов
          Result:= AMainMark + SPACE1 + WorkHoursToStr(ATotalHours) + SPACE2 +
                   ANightMark + SPACE1 + WorkHoursToStr(ANightHours) + SPACE2 +
                   ASkipMark + SPACE1 + WorkHoursToStr(ASkipHours);
    end;
    if AOverHours>0 then //есть сверхурочные
      Result:= Result + SPACE2 + AOverMark + SPACE1 + WorkHoursToStr(AOverHours);
  end;
end;

procedure TimetableDataToMarksAndHoursStr(const AMainMark, ASkipMark, ANightMark, AOverMark: String;   //даные табеля в строку кодов ('Я/Н') и строку часов ('4/2') за день
                            const ATotalHours, ANightHours, AOverHours, ASkipHours: Integer;
                            out AMarksStr, AHoursStr: String);
begin
  AMarksStr:= AMainMark;
  AHoursStr:= EmptyStr;
  if ATotalHours>0 then // есть раб часы
  begin
    AHoursStr:= WorkHoursToStr(ATotalHours);
    if ANightHours>0 then //есть ночные
    begin
      if ATotalHours=ANightHours then  //только ночные
        AMarksStr:= ANightMark
      else begin//и дневные и ночные
        AMarksStr:= AMarksStr + SPACE3 + ANightMark;
        AHoursStr:= AHoursStr + SPACE3 + WorkHoursToStr(ANightHours);
      end;
    end;
    if ASkipHours>0 then
    begin
      AMarksStr:= AMarksStr + SPACE3 + ASkipMark;
      AHoursStr:= AHoursStr + SPACE3 + WorkHoursToStr(ASkipHours);
    end;
    if AOverHours>0 then
    begin
      AMarksStr:= AMarksStr + SPACE3 + AOverMark;
      AHoursStr:= AHoursStr + SPACE3 + WorkHoursToStr(AOverHours);
    end;
  end;
end;

function TimetableDayDataFromSchedule(const ASchedule: TPersonalSchedule;
                                 const ADayStatus, AIndex: Integer): TTimetableDay;
var
  X: Integer;
begin
  Result:= TimetableDayEmpty;
  if ASchedule.ScheduleIDs[AIndex]<=0 then Exit;

  Result.ScheduleHours:= ASchedule.HoursCorrect.Totals[AIndex];  {WorkHoursCorrect - потому что в графиковом времени нужно учитывать раб часы без учета отпуска, но  с корректировками}
  Result.TotalHours:=  ASchedule.HoursCorrectVacation.Totals[AIndex]; {WorkHoursCorrectVacation - потому что в табеле учитывается и отпуск}
  Result.NightHours:=  ASchedule.HoursCorrectVacation.Nights[AIndex];
  //определяем основную метку табеля
  Result.DigMark:= ASchedule.MarkDIGCorrect[AIndex]; //берем с графика
  X:= ASchedule.IsVacations[AIndex];
  if X>VACATION_NO then  //есть отпуск
  begin
    if X=VACATION_HOLIDAY then
      Result.DigMark:= -1 {B}
    else begin
      case X of
        VACATION_MAIN     : Result.DigMark:= 9;  {ОТ}
        VACATION_ADDITION : Result.DigMark:= 10; {ОД}
      end;
    end;
    Result.SkipHours:= -1 {вся смена};
    Result.SkipMark:= Result.DigMark;
  end
  else begin //нет отпуска
    //если это праздничный день и есть рабочие часы
    if (ADayStatus=DAY_STATUS_HOLIDAY) and (Result.TotalHours>0) then
      Result.DigMark:= 3; {РВ}
  end;
  Result.ScheduleID:= ASchedule.ScheduleIDs[AIndex];
  Result.ShiftNum:= ASchedule.ShiftNumbersCorrectVacation[AIndex];
end;

function TimetableIsManualChanged(const AScheduleID: Integer): Integer;
begin
  if AScheduleID = FULLSHIFT_SKIPHOURS then
    Result:= MANUAL_YES
  else
    Result:= MANUAL_NO;
end;

function TimetableIsAbsence(const AMarkType: Integer): Integer;
begin
  if AMarkType = MARKTYPE_ABSENCE then
    Result:= ABSENCE_YES
  else
    Result:= ABSENCE_NO;
end;

end.

