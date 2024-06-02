unit USchedule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_DateUtils, DK_Const,
  //Project utils
  UWorkHours, UCalendar;

const
  {флаги наличия корректировки}
  CORRECTION_NO  = 0;
  CORRECTION_YES = 1;
  {флаги существования}
  EXISTS_NO  = 0;
  EXISTS_YES = 1;
  {флаги заданности графика}
  DEFINE_NO      = 0;
  DEFINE_YES     = 1;
  {флаги наличия отпуска}
  VACATION_NO       = 0; // нет отпуска
  VACATION_MAIN     = 1; // основной отпуск
  VACATION_ADDITION = 2; // дополнительный отпуск
  VACATION_HOLIDAY  = 3; // отпуск, выпавший на праздничный день
  DIGMARK_UNKNOWN          = 0;  STRMARK_UNKNOWN          = '?';
  DIGMARK_NONEXISTENT      = 0;  STRMARK_NONEXISTENT      = 'X';
  DIGMARK_VACATIONMAIN     = 9;  STRMARK_VACATIONMAIN     = 'ОТ';
  DIGMARK_VACATIONADDITION = 10; STRMARK_VACATIONADDITION = 'ОД';
  DIGMARK_VACATIONHOLIDAY  = -1; STRMARK_VACATIONHOLIDAY  = 'В';

  OUTSIDEMONTHSTR = 'X'; //метка дня за пределом месяца

  MAIN_COLOR_INDEX         = 0;  //scTransparent
  CORRECT_COLOR_INDEX      = 1;
  WORKDAY_COLOR_INDEX      = 1;
  NOTWORK_COLOR_INDEX      = 2;
  TITLE_COLOR_INDEX        = 3;
  OUTSIDEMONTH_COLOR_INDEX = 4;
  HIGHLIGHT_COLOR_INDEX    = 5;
  SHIFT_COLOR_COUNT        = 6;

  NOTDEFINE_COLOR_INDEX    = 6;
  PERSONAL_COLOR_COUNT     = 7;

  MONTHNAME_COLOR_INDEX    = 366;
  DAYNAME_COLOR_INDEX      = 367;


type
  {Цикл графика}
  TScheduleCycle = record
    ScheduleID: Integer;
    IsWeek    : Boolean;      //true - ежедневный, false - цикловой
    Count     : Integer;      //количество дней в цикле (0 при IsWeek=true)
    Dates     : TDateVector;  //даты
    HoursTotal: TIntVector;   //часов всего
    HoursNight: TIntVector;   //ночных часов
    DigMarks  : TIntVector;   //цифровые коды табеля
    StrMarks  : TStrVector;   //буквенные метки табеля
    ShiftNums : TIntVector;   //вектор номеров смен
  end;

const
  ScheduleCycleEmpty: TScheduleCycle =
    (ScheduleID: 0;
     IsWeek    : True;
     Count     : -1;
     Dates     : nil;
     HoursTotal: nil;
     HoursNight: nil;
     DigMarks  : nil;
     StrMarks  : nil;
     ShiftNums : nil;
    );

type
  {Корректировки графика}
  TScheduleCorrections = record
    Dates     : TDateVector;
    HoursTotal: TIntVector;
    HoursNight: TIntVector;
    DigMarks  : TIntVector;
    StrMarks  : TStrVector;
    ShiftNums : TIntVector;
  end;

  function ScheduleCorrectCopy(ACorrect: TScheduleCorrections): TScheduleCorrections;

const
  EmptyScheduleCorrections: TScheduleCorrections =
    (Dates     : nil;
     HoursTotal: nil;
     HoursNight: nil;
     DigMarks  : nil;
     StrMarks  : nil;
     ShiftNums : nil;
    );

type
  TVacationCustomInfo = record
    BeginDate: TDate;
    CountMain: SmallInt;
    CountAdd : SmallInt;
  end;

  TVacationInfo = record
    Plan : TVacationCustomInfo;
    Fact1: TVacationCustomInfo;
    Fact2: TVacationCustomInfo;
  end;

const
  EmptyVacationCustomInfo: TVacationCustomInfo =
    (BeginDate: NULDATE;
     CountMain: -1;
     CountAdd : -1;
    );
  EmptyVacationInfo: TVacationInfo =
    (Plan : (BeginDate: NULDATE; CountMain: -1; CountAdd : -1);
     Fact1: (BeginDate: NULDATE; CountMain: -1; CountAdd : -1);
     Fact2: (BeginDate: NULDATE; CountMain: -1; CountAdd : -1);
    );

type

  {График сменности}
  TCustomShiftSchedule = class (TObject)
  protected
    FCalculated: Boolean;
    FDates: TDateVector;             //вектор дат
    FHoursDefault: TWorkHours;       //рабочие часы (типовые)
    FHoursCorrect: TWorkHours;       //рабочие часы c учетом корректировок
    FIsCorrection: TIntVector;       //вектор флагов наличия корректировок (CORRECTION_NO - нет, CORRECTION_YES - есть)
    FMarkSTRDefault: TStrVector;     //вектор буквенных табельных кодов графика по умолчанию
    FMarkSTRCorrect: TStrVector;     //вектор буквенных табельных кодов графика с корректировками
    FMarkDIGDefault: TIntVector;     //вектор цифровых табельных кодов графика по умолчанию
    FMarkDIGCorrect: TIntVector;     //вектор цифровых табельных кодов графика с корректировками
    FScheduleIDs: TIntVector;        //вектор ID графика для каждого дня
    FShiftNumsDefault: TIntVector;   //вектор номеров смен
    FShiftNumsCorrect: TIntVector;   //вектор номеров смен с учетом корректировок
    procedure SetMarks(const AInd, ADigMark: Integer; const AStrMark: String);
    function GetShiftCount(const AShiftNums: TIntVector): Integer;
  private
    procedure WriteCycle(const ACycle: TScheduleCycle);
    procedure WriteCalendarSpecDays(const ACalendar: TCalendar; const ACycle: TScheduleCycle);
    procedure WriteCorrections(const ACorrect: TScheduleCorrections);
    function GetDaysCountDefault: Integer;
    function GetDaysCountCorrect: Integer;
    function GetBeginDate: TDate;
    function GetEndDate: TDate;
    function GetDaysCount: Integer;
    function GetShiftCountCorrect: Integer;
    function GetShiftCountDeafult: Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    function Cut(const ABeginDate, AEndDate: TDate; var ACutSchedule: TCustomShiftSchedule;
                 out AInd1, AInd2: Integer): Boolean;
    procedure Calc(const ACalendar: TCalendar;
                   const ACycle: TScheduleCycle;
                   const ACorrect: TScheduleCorrections);
    property BeginDate: TDate read GetBeginDate;
    property EndDate  : TDate read GetEndDate;
    property DaysCount: Integer read GetDaysCount;  //общее кол-во дней
    property DaysCountDefault: Integer read GetDaysCountDefault;  //кол-во отработанных дней (типовые)
    property DaysCountCorrect: Integer read GetDaysCountCorrect;  //кол-во отработанных дней с учетом корректировок
    property Dates: TDateVector read FDates;
    property IsCorrection: TIntVector read FIsCorrection;
    property MarkSTRDefault: TStrVector read FMarkSTRDefault;
    property MarkSTRCorrect: TStrVector read FMarkSTRCorrect;
    property MarkDIGDefault: TIntVector read FMarkDIGDefault;
    property MarkDIGCorrect: TIntVector read FMarkDIGCorrect;
    property ScheduleIDs: TIntVector read FScheduleIDs;
    property ShiftNumbersDefault: TIntVector read FShiftNumsDefault;
    property ShiftNumbersCorrect: TIntVector read FShiftNumsCorrect;
    property HoursDefault: TWorkHours read FHoursDefault; //рабочие часы (типовые)
    property HoursCorrect: TWorkHours read FHoursCorrect; //рабочие часы c учетом корректировок
    property Calculated: Boolean read FCalculated;
    property ShiftCountDefault: Integer read GetShiftCountDeafult;
    property ShiftCountCorrect: Integer read GetShiftCountCorrect;
  end;

  TShiftSchedule = class (TCustomShiftSchedule)
  private
    FCycle: TScheduleCycle; //структура графика
    FHoursInWeek: Byte;     //типовое кол-во часов в неделю
  public
    procedure Clear;
    function Cut(const ABeginDate, AEndDate: TDate; var ACutSchedule: TShiftSchedule): Boolean;
    procedure Calc(const ACalendar: TCalendar;
                   const AHoursInWeek: Byte;
                   const ACycle: TScheduleCycle;
                   const ACorrect: TScheduleCorrections);
    property HoursInWeek: Byte read FHoursInWeek;
    property Cycle: TScheduleCycle read FCycle;
  end;

  {одиночный график работы в должности и в графике внутри общего периода запроса, заданного ACalendar}

  { TPostSchedule }

  TPostSchedule = class (TShiftSchedule)
  private
    FIsExists: TIntVector;  //вектор флагов существования графика (EXISTS_NO - нет, EXISTS_YES - есть)
    FIsDefine: TIntVector;  //(DEFINE_NO - не задан, DEFINE_YES - график задан)
    FFirstDay, FLastDay: TDate; //начальная и конечная даты подпериода общего периода запроса для данного графика
    procedure CalcExistion(const AScheduleBD, AScheduleED, APostBD, APostED: TDate);
  public
    procedure Clear;
    procedure Calc(const ACalendar: TCalendar;
                   const AHoursInWeek: Byte;
                   const ACycle: TScheduleCycle;
                   const ACorrect: TScheduleCorrections;
                   const AScheduleBD: TDate = NULDATE; const AScheduleED: TDate = INFDATE; //период работы в графике
                   const APostBD: TDate = NULDATE; const APostED: TDate = INFDATE //период работы в должности
                   );
    property IsExists: TIntVector read FIsExists;
    property IsDefine: TIntVector read FIsDefine;
    property FirstDay: TDate read FFirstDay;
    property LastDay: TDate read FLastDay;
  end;

  {накопление графиков работы в должности и графике}

  { TCustomPersonalSchedule }

  TCustomPersonalSchedule = class (TCustomShiftSchedule)
  protected
    FIsExists: TIntVector;    //вектор флагов существования графика (EXISTS_NO - нет, EXISTS_YES - есть)
    FIsDefine: TIntVector;    //(DEFINE_NO - не задан, DEFINE_YES - график задан)
    FScheduleCount: Integer;  //кол-во добавленных графиков
    function SumStrMarks(V1, V2: TStrVector): TStrVector;
  public
    constructor Create;
    procedure Clear;
    function Cut(const ABeginDate, AEndDate: TDate;
      var ACutSchedule: TCustomPersonalSchedule; out AInd1, AInd2: Integer): Boolean;
    procedure Add(const APostSchedule: TPostSchedule; const AIsLast: Boolean = False);
    function IsDateExists(const ADate: TDate): Boolean;
    property IsExists: TIntVector read FIsExists;
    property IsDefine: TIntVector read FIsDefine;
  end;

  {персональный график - с учетом отпусков}
  TPersonalSchedule = class (TCustomPersonalSchedule)
  private
    FTabNumID: Integer;
    FTabNum: String;
    FRecrutDate: TDate;
    FDismissDate: TDate;
    FIsVacation: TIntVector;
    //FVacationBeginDate: TDate;
    FHoursDefaultVacation: TWorkHours;  //рабочие часы (типовые с учетом отпуска)
    FHoursCorrectVacation: TWorkHours;  //рабочие часы c учетом корректировок и отпуска
    FStrMarkVacationMain: String;
    FStrMarkVacationAddition: String;
    FStrMarkVacationHoliday: String;
    FMarkSTRDefaultVacation: TStrVector;    //вектор буквенных табельных кодов графика по умолчанию c учетом отпуска
    FMarkSTRCorrectVacation: TStrVector;    //вектор буквенных табельных кодов графика с корректировками и c учетом отпуска
    FMarkDIGDefaultVacation: TIntVector;    //вектор цифровых табельных кодов графика по умолчанию c учетом отпуска
    FMarkDIGCorrectVacation: TIntVector;    //вектор цифровых табельных кодов графика с корректировками и c учетом отпуска
    FShiftNumsDefaultVacation: TIntVector;  //вектор номеров смен (типовые с учетом отпуска)
    FShiftNumsCorrectVacation: TIntVector;  //вектор номеров смен с учетом корректировок и отпуска
    FPersonalCorrect: TScheduleCorrections;
    function GetDaysCountDefaultVacation: Integer;
    function GetDaysCountCorrectVacation: Integer;
    function GetShiftCountCorrectVacation: Integer;
    function GetShiftCountDeafultVacation: Integer;
    procedure WriteVacationHours;
    procedure CalcExistion;
    procedure Calc;
    procedure SetMarksVacation(const AInd, ADigMark: Integer; const AStrMark: String);
    procedure WritePersonalCorrections;
    function GetPersonalCorrect: TScheduleCorrections;
  public
    constructor Create(const ATabNumID: Integer; const ATabNum: String;
                       const ARecrutDate, ADismissDate: TDate;
                       const AIsVacation: TIntVector;
                       const APersonalCorrect: TScheduleCorrections;
                       const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                       const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                       const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY);
    destructor  Destroy; override;
    procedure Clear;
    function Cut(const ABeginDate, AEndDate: TDate; var ACutSchedule: TPersonalSchedule): Boolean;
    procedure Add(const APostSchedule: TPostSchedule; const AIsLast: Boolean = False);
    property IsVacation: TIntVector read FIsVacation;
    property TabNumID: Integer read FTabNumID;
    property TabNum: String read FTabNum;
    property RecrutDate: TDate read FRecrutDate;
    property DismissDate: TDate read FDismissDate;
    property DaysCountDefaultVacation: Integer read GetDaysCountDefaultVacation; //кол-во отработанных дней (типовые с учтом отпуска)
    property DaysCountCorrectVacation: Integer read GetDaysCountCorrectVacation; //кол-во отработанных дней с учетом корректировок и отпуска
    property HoursDefaultVacation: TWorkHours read FHoursDefaultVacation;
    property HoursCorrectVacation: TWorkHours read FHoursCorrectVacation;
    property MarkSTRDefaultVacation: TStrVector read FMarkSTRDefaultVacation;
    property MarkSTRCorrectVacation: TStrVector read FMarkSTRCorrectVacation;
    property MarkDIGDefaultVacation: TIntVector read FMarkDIGDefaultVacation;
    property MarkDIGCorrectVacation: TIntVector read FMarkDIGCorrectVacation;
    property StrMarkVacationMain    : String read FStrMarkVacationMain;
    property StrMarkVacationAddition: String read FStrMarkVacationAddition;
    property StrMarkVacationHoliday : String read FStrMarkVacationHoliday;
    property ShiftCountDefaultVacation: Integer read GetShiftCountDeafultVacation;
    property ShiftCountCorrectVacation: Integer read GetShiftCountCorrectVacation;
    property ShiftNumbersDefaultVacation: TIntVector read FShiftNumsDefaultVacation;
    property ShiftNumbersCorrectVacation: TIntVector read FShiftNumsCorrectVacation;
    property PersonalCorrect: TScheduleCorrections read GetPersonalCorrect;
    //property VacationBeginDate: TDate read FVacationBeginDate;
  end;

  TShiftScheduleVector = array of TShiftSchedule;
  procedure VSAppend(var V: TShiftScheduleVector; const NewValue: TShiftSchedule);
  procedure VSDel(var V: TShiftScheduleVector; Index1: Integer = -1; Index2: Integer = -1);

type
  TPostScheduleVector = array of TPostSchedule;
  TPostScheduleMatrix = array of TPostScheduleVector;
  procedure VSAppend(var V: TPostScheduleVector; const NewValue: TPostSchedule);
  procedure VSAppend(var V: TPostScheduleVector; const NewVector: TPostScheduleVector);
  procedure VSDel(var V: TPostScheduleVector; Index1: Integer = -1; Index2: Integer = -1);
  procedure MSAppend(var M: TPostScheduleMatrix; const NewVector: TPostScheduleVector);
  procedure MSDel(var M: TPostScheduleMatrix; Index1: Integer = -1; Index2: Integer = -1;
                  NeedFree: Boolean = True);
  procedure MSSwap(var M: TPostScheduleMatrix; const Index1, Index2: Integer);

type
  TPersonalScheduleVector = array of TPersonalSchedule;
  procedure VSAppend(var V: TPersonalScheduleVector; const NewValue: TPersonalSchedule);
  procedure VSDel(var V: TPersonalScheduleVector; Index1: Integer = -1; Index2: Integer = -1);
  procedure VSSwap(var V: TPersonalScheduleVector; const Index1, Index2: Integer);

  function DateToCycleIndex(const ACycle: TScheduleCycle; const ADate: TDate): Byte;
  procedure DateToCycleStage(const ACycle: TScheduleCycle; const ADate: TDate;
                             out AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                             out AStrMark: String);

type
  TVacationPlane = record
    Schedule: TShiftSchedule;
    ScheduleName: String;
    TabNums, StaffNames: TStrVector;
    PlaneDays, FactDays: TIntMatrix;
  end;
  procedure VacationPlaneClear(var AVacationPlane: TVacationPlane);
  procedure VacationPlaneNew(var AVacationPlane: TVacationPlane;
                             const ASchedule: TShiftSchedule;
                             const AScheduleName: String);
  procedure VacationPlaneAdd(var AVacationPlane: TVacationPlane;
                             const ATabNum, AStaffName: String;
                             const APlaneDays, AFactDays: TIntVector);
  procedure VacationPlaneCopy(var AVacationPlane: TVacationPlane;
                              const ASource: TVacationPlane);
type
  TVacationPlaneVector = array of TVacationPlane;
  procedure VacationPlaneVectorDel(var V: TVacationPlaneVector);
  procedure VacationPlaneVectorAdd(var V: TVacationPlaneVector; const AVacationPlane: TVacationPlane);

implementation

uses UDataBase;

procedure VacationPlaneVectorDel(var V: TVacationPlaneVector);
var
  i: Integer;
begin
  for i:= 0 to High(V) do
    VacationPlaneClear(V[i]);
  V:= nil;
end;

procedure VacationPlaneVectorAdd(var V: TVacationPlaneVector; const AVacationPlane: TVacationPlane);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);
  VacationPlaneCopy(V[N], AVacationPlane);
end;

procedure VacationPlaneClear(var AVacationPlane: TVacationPlane);
begin
  if Assigned(AVacationPlane.Schedule) then
    FreeAndNil(AVacationPlane.Schedule);
  AVacationPlane.ScheduleName:= EmptyStr;
  AVacationPlane.TabNums:= nil;
  AVacationPlane.StaffNames:= nil;
  AVacationPlane.PlaneDays:= nil;
  AVacationPlane.FactDays:= nil;
end;

procedure VacationPlaneNew(var AVacationPlane: TVacationPlane;
                             const ASchedule: TShiftSchedule;
                             const AScheduleName: String);
begin
  VacationPlaneClear(AVacationPlane);
  AVacationPlane.Schedule:= TShiftSchedule.Create;
  ASchedule.Cut(ASchedule.BeginDate, ASchedule.EndDate, AVacationPlane.Schedule);
  AVacationPlane.ScheduleName:= AScheduleName;
end;

procedure VacationPlaneAdd(var AVacationPlane: TVacationPlane;
                             const ATabNum, AStaffName: String;
                             const APlaneDays, AFactDays: TIntVector);
begin
  VAppend(AVacationPlane.TabNums, ATabNum);
  VAppend(AVacationPlane.StaffNames, AStaffName);
  MAppend(AVacationPlane.PlaneDays, APlaneDays);
  MAppend(AVacationPlane.FactDays, AFactDays);
end;

procedure VacationPlaneCopy(var AVacationPlane: TVacationPlane;
                              const ASource: TVacationPlane);
begin
  VacationPlaneNew(AVacationPlane, ASource.Schedule, ASource.ScheduleName);
  AVacationPlane.TabNums:= VCut(ASource.TabNums);
  AVacationPlane.StaffNames:= VCut(ASource.StaffNames);
  AVacationPlane.PlaneDays:= MCut(ASource.PlaneDays);
  AVacationPlane.FactDays:= MCut(ASource.FactDays);
end;

function ScheduleCorrectCopy(ACorrect: TScheduleCorrections): TScheduleCorrections;
begin
  Result.Dates:= VCut(ACorrect.Dates);
  Result.HoursTotal:= VCut(ACorrect.HoursTotal);
  Result.HoursNight:= VCut(ACorrect.HoursNight);
  Result.DigMarks:= VCut(ACorrect.DigMarks);
  Result.StrMarks:= VCut(ACorrect.StrMarks);
  Result.ShiftNums:= VCut(ACorrect.ShiftNums);
end;

procedure DateToCycleStage(const ACycle: TScheduleCycle; const ADate: TDate;
                             out AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                             out AStrMark: String);
var
  n: Integer;
begin
  n:= DateToCycleIndex(ACycle, ADate); //номер фазы цикла на дату
  AHoursTotal:= ACycle.HoursTotal[n];
  AHoursNight:= ACycle.HoursNight[n];
  AStrMark:= ACycle.StrMarks[n];
  ADigMark:= ACycle.DigMarks[n];
  AShiftNum:= ACycle.ShiftNums[n];
end;

function DateToCycleIndex(const ACycle: TScheduleCycle; const ADate: TDate): Byte;
var
  Delta, CycleCount: Integer;
begin
  if ACycle.IsWeek then
    Result:= DayNumberInWeek(ADate)
  else begin
    Delta:= DaysBetweenDates(ACycle.Dates[0], ADate);
    CycleCount:= Length(ACycle.Dates);
    if Delta>=0 then
      Result:= ((CycleCount+Delta) mod CycleCount) + 1
    else
      Result:= ((1         +Delta) mod CycleCount) + CycleCount;
  end;
  Result:= Result - 1;
end;

procedure VSAppend(var V: TShiftScheduleVector; const NewValue: TShiftSchedule);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);
  V[N]:= NewValue;
end;

procedure VSAppend(var V: TPostScheduleVector; const NewValue: TPostSchedule);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);
  V[N]:= NewValue;
end;

procedure VSAppend(var V: TPersonalScheduleVector; const NewValue: TPersonalSchedule);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);

  V[N]:= NewValue;
end;

procedure VSAppend(var V: TPostScheduleVector; const NewVector: TPostScheduleVector);
var
  i: Integer;
begin
  for i:=0 to High(NewVector) do VSAppend(V, NewVector[i]);
end;

procedure MSAppend(var M: TPostScheduleMatrix; const NewVector: TPostScheduleVector);
var
  N: Integer;
begin
  N:= Length(M);
  SetLength(M, N+1);
  VSAppend(M[N], NewVector);
end;

procedure VSDel(var V: TShiftScheduleVector; Index1: Integer = -1; Index2: Integer = -1);
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

procedure VSDel(var V: TPostScheduleVector; Index1: Integer = -1; Index2: Integer = -1);
var
  i, OldSize, DelLength: Integer;
begin
  OldSize:= Length(V);
  if OldSize=0 then Exit;
  if (Index1=-1) and (Index2=-1) then
  begin
    Index1:=0;
    Index2:=High(V);
  end
  else begin
    if Index2< Index1 then Index2:= Index1;
    i:= High(V);
    if not (CheckIndex(i, Index1) and CheckIndex(i, Index2)) then Exit;
  end;
  DelLength:= Index2 - Index1 + 1;
  for i:= Index1 to Index2 do if Assigned(V[i]) then FreeAndNil(V[i]);
  for i:= Index2+1 to OldSize-1 do  V[i-DelLength]:= V[i];
  SetLength(V, OldSize - DelLength);
end;

procedure VSDel(var V: TPersonalScheduleVector; Index1: Integer = -1; Index2: Integer = -1);
var
  i, OldSize, DelLength: Integer;
begin
  OldSize:= Length(V);
  if OldSize=0 then Exit;
  if (Index1=-1) and (Index2=-1) then
  begin
    Index1:=0;
    Index2:=High(V);
  end
  else begin
    if Index2< Index1 then Index2:= Index1;
    i:= High(V);
    if not (CheckIndex(i, Index1) and CheckIndex(i, Index2)) then Exit;
  end;
  DelLength:= Index2 - Index1 + 1;
  for i:= Index1 to Index2 do if Assigned(V[i]) then FreeAndNil(V[i]);
  for i:= Index2+1 to OldSize-1 do  V[i-DelLength]:= V[i];
  SetLength(V, OldSize - DelLength);
end;

procedure MSDel(var M: TPostScheduleMatrix; Index1: Integer = -1; Index2: Integer = -1;
                              NeedFree: Boolean = True);
var
  i, OldSize, DelLength: Integer;
begin
  OldSize:= Length(M);
  if OldSize=0 then Exit;
  if (Index1=-1) and (Index2=-1) then
  begin
    Index1:=0;
    Index2:=High(M);
  end
  else begin
    if Index2< Index1 then Index2:= Index1;
    i:= High(M);
    if not (CheckIndex(i, Index1) and CheckIndex(i, Index2)) then Exit;
  end;
  DelLength:= Index2 - Index1 + 1;
  if NeedFree then for i:= Index1 to Index2 do if Length(M[i])>0 then VSDel(M[i]);
  for i:= Index2+1 to OldSize-1 do  M[i-DelLength]:= M[i];
  SetLength(M, OldSize - DelLength);
end;

procedure VSSwap(var V: TPersonalScheduleVector; const Index1, Index2: Integer);
var
  TmpValue: TPersonalSchedule;
begin
  if not CheckIndexes(High(V), Index1, Index2) then Exit;
  TmpValue:= V[Index1];
  V[Index1]:= V[Index2];
  V[Index2]:= TmpValue;
end;

procedure MSSwap(var M: TPostScheduleMatrix; const Index1, Index2: Integer);
var
  TmpValue: TPostScheduleVector;
begin
  if not CheckIndexes(High(M), Index1, Index2) then Exit;
  TmpValue:= M[Index1];
  M[Index1]:= M[Index2];
  M[Index2]:= TmpValue;
end;

{ TPersonalSchedule }

function TPersonalSchedule.GetDaysCountDefaultVacation: Integer;
begin
  Result:= VCountIfNot(FHoursDefaultVacation.Total, 0);
end;

function TPersonalSchedule.GetDaysCountCorrectVacation: Integer;
begin
  Result:= VCountIfNot(FHoursCorrectVacation.Total, 0);
end;

function TPersonalSchedule.GetShiftCountCorrectVacation: Integer;
begin
  Result:= GetShiftCount(FShiftNumsCorrectVacation);
end;

function TPersonalSchedule.GetShiftCountDeafultVacation: Integer;
begin
  Result:= GetShiftCount(FShiftNumsDefaultVacation);
end;

procedure TPersonalSchedule.WriteVacationHours;
var
  i: Integer;
begin
  FHoursDefaultVacation.Clear;
  FHoursCorrectVacation.Clear;
  //записываем данные с учетом отпуска
  for i:= 0 to DaysCount-1 do
  begin
    if (FIsExists[i]=EXISTS_NO) or (FIsDefine[i]=DEFINE_NO) then
      FIsVacation[i]:= VACATION_NO;
    if FIsVacation[i]=VACATION_NO then
    begin
      FHoursDefaultVacation.Add(FHoursDefault.Total[i], FHoursDefault.Night[i]);
      FHoursCorrectVacation.Add(FHoursCorrect.Total[i], FHoursCorrect.Night[i]);
    end
    else begin
      FHoursDefaultVacation.Add(0, 0);
      FHoursCorrectVacation.Add(0, 0);
    end;
  end;
end;

procedure TPersonalSchedule.CalcExistion;
var
  I1, I2: Integer;
  V: TIntVector;
begin
  V:= nil;
  //определяем подпериод существования графика
  //запоминяем соответствующие индексы
  if VCrossInd(FDates, FRecrutDate, FDismissDate, I1, I2) then
  begin
    VDim(V, DaysCount, EXISTS_NO);
    //устанавливаем в период пересечения флаги существования
    VChangeIn(V, EXISTS_YES, I1, I2);
    //рассчитываем вектор флагов существования
    FIsExists:= VMult(FIsExists, V);
    //меняем данные вне периода существования
    VChangeNotIn(FIsCorrection, CORRECTION_NO, I1, I2);
    VChangeNotIn(FShiftNumsDefault, 0, I1, I2);
    VChangeNotIn(FShiftNumsCorrect, 0, I1, I2);
    V:= FHoursDefault.Total; VChangeNotIn(V, 0, I1, I2); FHoursDefault.Total:= V;
    V:= FHoursDefault.Night; VChangeNotIn(V, 0, I1, I2); FHoursDefault.Night:= V;
    V:= FHoursCorrect.Total; VChangeNotIn(V, 0, I1, I2); FHoursCorrect.Total:= V;
    V:= FHoursCorrect.Night; VChangeNotIn(V, 0, I1, I2); FHoursCorrect.Night:= V;
  end
  else begin //график НЕ существует на всем периоде запроса
    VDim(FIsCorrection, DaysCount, CORRECTION_NO);
    VDim(FShiftNumsDefault, DaysCount, 0);
    VDim(FShiftNumsCorrect, DaysCount, 0);
    VDim(FIsExists, DaysCount, EXISTS_NO);
    VDim(V, DaysCount, 0);
    FHoursDefault.Total:= V;
    FHoursDefault.Night:= V;
    FHoursCorrect.Total:= V;
    FHoursCorrect.Night:= V;
  end;
end;

procedure TPersonalSchedule.Calc;
var
  i: Integer;
begin
  WritePersonalCorrections;
  CalcExistion;
  FIsDefine:= VMult(FIsExists, FIsDefine);
  WriteVacationHours;
  for i:=0 to DaysCount-1 do
  begin
    if FIsExists[i]=EXISTS_NO then
      SetMarks(i, DIGMARK_NONEXISTENT, STRMARK_NONEXISTENT)
    else if FIsDefine[i]=DEFINE_NO then
      SetMarks(i, DIGMARK_UNKNOWN, STRMARK_UNKNOWN);
  end;
  FMarkSTRDefaultVacation:= VCut(MarkSTRDefault);
  FMarkSTRCorrectVacation:= VCut(MarkSTRCorrect);
  FMarkDIGDefaultVacation:= VCut(MarkDIGDefault);
  FMarkDIGCorrectVacation:= VCut(MarkDIGCorrect);
  FShiftNumsDefaultVacation:= VCut(FShiftNumsDefault);
  FShiftNumsCorrectVacation:= VCut(FShiftNumsCorrect);
  for i:=0 to DaysCount-1 do
  begin
    if IsVacation[i]>VACATION_NO then
    begin
      FShiftNumsDefaultVacation[i]:= 0;
      FShiftNumsCorrectVacation[i]:= 0;
      case IsVacation[i] of
      VACATION_MAIN    : SetMarksVacation(i, DIGMARK_VACATIONMAIN, FStrMarkVacationMain);
      VACATION_ADDITION: SetMarksVacation(i, DIGMARK_VACATIONADDITION, FStrMarkVacationAddition);
      VACATION_HOLIDAY : SetMarksVacation(i, DIGMARK_VACATIONHOLIDAY, FStrMarkVacationHoliday);
      end;
    end;
  end;
end;

procedure TPersonalSchedule.SetMarksVacation(const AInd, ADigMark: Integer;
  const AStrMark: String);
begin
  FMarkSTRDefaultVacation[AInd]:= AStrMark;
  FMarkSTRCorrectVacation[AInd]:= AStrMark;
  FMarkDIGDefaultVacation[AInd]:= ADigMark;
  FMarkDIGCorrectVacation[AInd]:= ADigMark;
end;

procedure TPersonalSchedule.WritePersonalCorrections;
var
  i, n: Integer;
begin
  if VIsNil(FPersonalCorrect.Dates) then Exit;
  for i:=0 to High(FPersonalCorrect.Dates) do
  begin
    n:= DaysBetweenDates(BeginDate, FPersonalCorrect.Dates[i]);
    if (n>=0) and (n<DaysCount) then
    begin
      FHoursCorrect.Total[n]:= FPersonalCorrect.HoursTotal[i];
      FHoursCorrect.Night[n]:= FPersonalCorrect.HoursNight[i];
      FIsCorrection[n]:= CORRECTION_YES;
      FMarkSTRCorrect[n]:= FPersonalCorrect.StrMarks[i];
      FMarkDIGCorrect[n]:= FPersonalCorrect.DigMarks[i];
      FShiftNumsCorrect[n]:= FPersonalCorrect.ShiftNums[i];
    end;
  end;
end;

function TPersonalSchedule.GetPersonalCorrect: TScheduleCorrections;
begin
  Result:= ScheduleCorrectCopy(FPersonalCorrect);
end;

constructor TPersonalSchedule.Create(const ATabNumID: Integer; const ATabNum: String;
                       const ARecrutDate, ADismissDate: TDate;
                       const AIsVacation: TIntVector;
                       const APersonalCorrect: TScheduleCorrections;
                       const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                       const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                       const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY);
begin
  inherited Create;
  FTabNumID:= ATabNumID;
  FTabNum:= ATabNum;
  FRecrutDate:= ARecrutDate;
  FDismissDate:= ADismissDate;
  FIsVacation:= VCut(AIsVacation);
  FStrMarkVacationMain:= AStrMarkVacationMain;
  FStrMarkVacationAddition:= AStrMarkVacationAddition;
  FStrMarkVacationHoliday:= AStrMarkVacationHoliday;
  FHoursDefaultVacation:= TWorkHours.Create;
  FHoursCorrectVacation:= TWorkHours.Create;
  FPersonalCorrect:= ScheduleCorrectCopy(APersonalCorrect);
end;

destructor TPersonalSchedule.Destroy;
begin
  FreeAndNil(FHoursDefaultVacation);
  FreeAndNil(FHoursCorrectVacation);
  inherited Destroy;
end;

procedure TPersonalSchedule.Clear;
begin
  inherited Clear;
  FHoursDefaultVacation.Clear;
  FHoursCorrectVacation.Clear;
  FMarkSTRDefaultVacation:= nil;
  FMarkSTRCorrectVacation:= nil;
  FMarkDIGDefaultVacation:= nil;
  FMarkDIGCorrectVacation:= nil;
  FPersonalCorrect:= EmptyScheduleCorrections;
end;

function TPersonalSchedule.Cut(const ABeginDate, AEndDate: TDate;
  var ACutSchedule: TPersonalSchedule): Boolean;
var
  I1, I2: Integer;
begin
  Result:= inherited Cut(ABeginDate, AEndDate, TCustomPersonalSchedule(ACutSchedule), I1, I2);
  if not Result then Exit;
  ACutSchedule.FIsVacation:= VCut(FIsVacation, I1, I2);
  ACutSchedule.FMarkSTRDefaultVacation := VCut(FMarkSTRDefaultVacation, I1, I2);
  ACutSchedule.FMarkSTRCorrectVacation := VCut(FMarkSTRCorrectVacation, I1, I2);
  ACutSchedule.FMarkDIGDefaultVacation := VCut(FMarkDIGDefaultVacation, I1, I2);
  ACutSchedule.FMarkDIGCorrectVacation := VCut(FMarkDIGCorrectVacation, I1, I2);
  ACutSchedule.FHoursDefaultVacation.Total:= VCut(FHoursDefaultVacation.Total, I1, I2);
  ACutSchedule.FHoursDefaultVacation.Night:= VCut(FHoursDefaultVacation.Night, I1, I2);
  ACutSchedule.FHoursCorrectVacation.Total:= VCut(FHoursCorrectVacation.Total, I1, I2);
  ACutSchedule.FHoursCorrectVacation.Night:= VCut(FHoursCorrectVacation.Night, I1, I2);
  ACutSchedule.FShiftNumsDefaultVacation := VCut(FShiftNumsDefaultVacation, I1, I2);
  ACutSchedule.FShiftNumsCorrectVacation := VCut(FShiftNumsCorrectVacation, I1, I2);
end;

procedure TPersonalSchedule.Add(const APostSchedule: TPostSchedule;
  const AIsLast: Boolean);
begin
  inherited Add(APostSchedule, AIsLast);
  FCalculated:= False;
  if AIsLast then Calc;
  FCalculated:= AIsLast;
end;

{ TCustomPersonalSchedule }

function TCustomPersonalSchedule.SumStrMarks(V1, V2: TStrVector): TStrVector;
begin
  VChangeIf(V1, STRMARK_NONEXISTENT, EmptyStr);
  VChangeIf(V1, STRMARK_UNKNOWN, EmptyStr);
  VChangeIf(V2, STRMARK_NONEXISTENT, EmptyStr);
  VChangeIf(V2, STRMARK_UNKNOWN, EmptyStr);
  Result:= VSum(V1,V2);
end;

constructor TCustomPersonalSchedule.Create;
begin
  inherited Create;
  FScheduleCount:= 0;
end;

procedure TCustomPersonalSchedule.Clear;
begin
  inherited Clear;
  FIsExists:= nil;
  FIsDefine:= nil;
  FScheduleCount:= 0;
end;

function TCustomPersonalSchedule.Cut(const ABeginDate, AEndDate: TDate;
  var ACutSchedule: TCustomPersonalSchedule; out AInd1, AInd2: Integer): Boolean;
var
  I1, I2: Integer;
begin
  Result:= inherited Cut(ABeginDate, AEndDate, TCustomShiftSchedule(ACutSchedule), I1, I2);
  if not Result then Exit;
  ACutSchedule.FIsExists:= VCut(FIsExists, I1, I2);
  ACutSchedule.FIsDefine:= VCut(FIsDefine, I1, I2);
  ACutSchedule.FScheduleCount:= FScheduleCount;
  AInd1:= I1;
  AInd2:= I2;
end;

procedure TCustomPersonalSchedule.Add(const APostSchedule: TPostSchedule;
  const AIsLast: Boolean);
var
  i: Integer;
begin
  if not APostSchedule.Calculated then Exit;
  Inc(FScheduleCount);
  if FScheduleCount=1 then  //это первый график
  begin
    FDates:= VCut(APostSchedule.Dates);
    APostSchedule.HoursDefault.Copy(FHoursDefault);
    APostSchedule.HoursCorrect.Copy(FHoursCorrect);
    FIsCorrection:= VCut(APostSchedule.IsCorrection);
    FIsExists:= VCut(APostSchedule.IsExists);
    FIsDefine:= VCut(APostSchedule.IsDefine);
    FScheduleIDs:= VCut(APostSchedule.FScheduleIDs);
    FShiftNumsDefault:= VCut(APostSchedule.FShiftNumsDefault);
    FShiftNumsCorrect:= VCut(APostSchedule.FShiftNumsCorrect);
    FMarkSTRCorrect:= VCut(APostSchedule.MarkSTRCorrect);
    FMarkSTRDefault:= VCut(APostSchedule.MarkSTRDefault);
    FMarkDIGDefault:= VCut(APostSchedule.MarkDIGDefault);
    FMarkDIGCorrect:= VCut(APostSchedule.MarkDIGCorrect);
  end else //следующие графики
  begin
   FHoursDefault.Total:= VSum(FHoursDefault.Total, APostSchedule.HoursDefault.Total);
   FHoursDefault.Night:= VSum(FHoursDefault.Night, APostSchedule.HoursDefault.Night);
   FHoursCorrect.Total:= VSum(FHoursCorrect.Total, APostSchedule.HoursCorrect.Total);
   FHoursCorrect.Night:= VSum(FHoursCorrect.Night, APostSchedule.HoursCorrect.Night);
   FIsCorrection:= VSum(FIsCorrection, APostSchedule.IsCorrection);
   FScheduleIDs:= VSum(FScheduleIDs, APostSchedule.FScheduleIDs);
   FShiftNumsDefault:= VSum(FShiftNumsDefault, APostSchedule.FShiftNumsDefault);
   FShiftNumsCorrect:= VSum(FShiftNumsCorrect, APostSchedule.FShiftNumsCorrect);
   FIsExists:= VSum(FIsExists, APostSchedule.IsExists);
   FIsDefine:= VSum(FIsDefine, APostSchedule.IsDefine);
   FMarkSTRDefault:= SumStrMarks(FMarkSTRDefault, APostSchedule.MarkSTRDefault);
   FMarkSTRCorrect:= SumStrMarks(FMarkSTRCorrect, APostSchedule.MarkSTRCorrect);
   FMarkDIGDefault:= VSum(FMarkDIGDefault, APostSchedule.MarkDIGDefault);
   FMarkDIGCorrect:= VSum(FMarkDIGCorrect, APostSchedule.MarkDIGCorrect);
  end;
  if AIsLast then
  begin
    for i:=0 to DaysCount-1 do
    begin
      if FIsExists[i]=EXISTS_NO then
        SetMarks(i, DIGMARK_NONEXISTENT, STRMARK_NONEXISTENT)
      else if FIsDefine[i]=DEFINE_NO then
        SetMarks(i, DIGMARK_UNKNOWN, STRMARK_UNKNOWN);
    end;
  end;
  FCalculated:= AIsLast;
end;

function TCustomPersonalSchedule.IsDateExists(const ADate: TDate): Boolean;
begin
  Result:= IsDateInPeriod(ADate, BeginDate, EndDate);
  if not Result then Exit;
  Result:= IsExists[DaysBetweenDates(BeginDate, ADate)]=EXISTS_YES;
end;

{ TPostSchedule }

procedure TPostSchedule.CalcExistion(const AScheduleBD, AScheduleED, APostBD, APostED: TDate);
var
  I1, I2: Integer;
  BD, ED: TDate;
  ScheduleExists: Boolean;
  V: TIntVector;
begin
  I1:= 0;
  I2:= 0;
  //создаем вектор флагов с флагами несуществования EXISTS_NO
  VDim(FIsExists, DaysCount, EXISTS_NO);
  //определяем подпериод существования графика
  ScheduleExists:= False;
  if IsPeriodIntersect(AScheduleBD, AScheduleED, APostBD, APostED, BD, ED) then
  begin
    //запоминяем соответствующие индексы
    if VCrossInd(FDates, BD, ED, I1, I2) then
      ScheduleExists:= True;
  end;
  if ScheduleExists then //график существует
  begin
    //заполнение данных
    FFirstDay:= BD;
    FLastDay:= ED;
    //устанавливаем в период пересечения флаги существования
    VChangeIn(FIsExists, EXISTS_YES, I1, I2);
    //меняем данные вне периода существования
    VChangeNotIn(FIsCorrection, CORRECTION_NO, I1, I2);
    VChangeNotIn(FScheduleIDs, 0, I1, I2);
    VChangeNotIn(FShiftNumsDefault, 0, I1, I2);
    VChangeNotIn(FShiftNumsCorrect, 0, I1, I2);
    V:= FHoursDefault.Total; VChangeNotIn(V, 0, I1, I2); FHoursDefault.Total:= V;
    V:= FHoursDefault.Night; VChangeNotIn(V, 0, I1, I2); FHoursDefault.Night:= V;
    V:= FHoursCorrect.Total; VChangeNotIn(V, 0, I1, I2); FHoursCorrect.Total:= V;
    V:= FHoursCorrect.Night; VChangeNotIn(V, 0, I1, I2); FHoursCorrect.Night:= V;
  end
  else begin //график НЕ существует на всем периоде запроса
    VDim(FIsCorrection, DaysCount, CORRECTION_NO);
    VDim(FScheduleIDs, DaysCount, 0);
    VDim(FShiftNumsDefault, DaysCount, 0);
    VDim(FShiftNumsCorrect, DaysCount, 0);
    VDim(V, DaysCount, 0);
    FHoursDefault.Total:= V;
    FHoursDefault.Night:= V;
    FHoursCorrect.Total:= V;
    FHoursCorrect.Night:= V;
  end;
end;

procedure TPostSchedule.Clear;
begin
  inherited Clear;
  FIsExists:= nil;
  FIsDefine:= nil;
  FLastDay:= NULDATE;
  FFirstDay:= NULDATE;
end;

procedure TPostSchedule.Calc(const ACalendar: TCalendar;
  const AHoursInWeek: Byte; const ACycle: TScheduleCycle;
  const ACorrect: TScheduleCorrections; const AScheduleBD: TDate;
  const AScheduleED: TDate; const APostBD: TDate; const APostED: TDate);
var
  i: Integer;
begin
  inherited Calc(ACalendar, AHoursInWeek, ACycle, ACorrect);
  FCalculated:= False;
  CalcExistion(AScheduleBD, AScheduleED, APostBD, APostED);
  if VIsNil(ACycle.Dates) then
    VDim(FIsDefine, DaysCount, DEFINE_NO)
  else begin
    VDim(FIsDefine, DaysCount, DEFINE_YES);
    FIsDefine:= VMult(FIsExists, FIsDefine);
  end;
  for i:=0 to DaysCount-1 do
  begin
    if FIsExists[i]=EXISTS_NO then
      SetMarks(i, DIGMARK_NONEXISTENT, STRMARK_NONEXISTENT)
    else if FIsDefine[i]=DEFINE_NO then
      SetMarks(i, DIGMARK_UNKNOWN, STRMARK_UNKNOWN);
  end;
  FCalculated:= True;
end;

{ TShiftSchedule }

procedure TShiftSchedule.Clear;
begin
  inherited Clear;
  FHoursInWeek:= 0;
  FCycle:= ScheduleCycleEmpty;
end;

function TShiftSchedule.Cut(const ABeginDate, AEndDate: TDate;
  var ACutSchedule: TShiftSchedule): Boolean;
var
  x: Integer;
begin
  Result:= inherited Cut(ABeginDate, AEndDate, TCustomShiftSchedule(ACutSchedule), x,x);
  if not Result then Exit;
  ACutSchedule.FCycle:= FCycle;
  ACutSchedule.FHoursInWeek:= FHoursInWeek;
end;

procedure TShiftSchedule.Calc(const ACalendar: TCalendar;
  const AHoursInWeek: Byte; const ACycle: TScheduleCycle;
  const ACorrect: TScheduleCorrections);
begin
  inherited Calc(ACalendar, ACycle, ACorrect);
  FCycle:= ACycle;
  FHoursInWeek:= AHoursInWeek;
end;

{TCustomShiftSchedule}

constructor TCustomShiftSchedule.Create;
begin
  inherited Create;
  FHoursDefault:= TWorkHours.Create;
  FHoursCorrect:= TWorkHours.Create;
end;

destructor TCustomShiftSchedule.Destroy;
begin
  FreeAndNil(FHoursDefault);
  FreeAndNil(FHoursCorrect);
  inherited Destroy;
end;

function TCustomShiftSchedule.Cut(const ABeginDate, AEndDate: TDate;
  var ACutSchedule: TCustomShiftSchedule; out AInd1, AInd2: Integer): Boolean;
var
  BD, ED: TDate;
  I1, I2: Integer;
begin
  Result:= False;
  ACutSchedule.Clear;
  //проверяем, рассчиатн ли исходный график
  if not FCalculated then Exit;
  //проверяем, пересекаются ли периоды обрезки и исходного графика, запоминаем период пересечения
  if not IsPeriodIntersect(ABeginDate, AEndDate, BeginDate, EndDate, BD, ED) then Exit;
  //определяем индексы среза
  I1:= DaysBetweenDates(BeginDate, BD);
  I2:= DaysBetweenDates(BeginDate, ED);
  //заполняем данные
  ACutSchedule.FDates:= VCut(FDates, I1, I2);
  ACutSchedule.FHoursDefault.Total:= VCut(FHoursDefault.Total, I1, I2);
  ACutSchedule.FHoursDefault.Night:= VCut(FHoursDefault.Night, I1, I2);
  ACutSchedule.FHoursCorrect.Total:= VCut(FHoursCorrect.Total, I1, I2);
  ACutSchedule.FHoursCorrect.Night:= VCut(FHoursCorrect.Night, I1, I2);
  ACutSchedule.FIsCorrection:= VCut(FIsCorrection, I1, I2);
  ACutSchedule.FMarkSTRDefault := VCut(FMarkSTRDefault, I1, I2);
  ACutSchedule.FMarkSTRCorrect := VCut(FMarkSTRCorrect, I1, I2);
  ACutSchedule.FMarkDIGDefault := VCut(FMarkDIGDefault, I1, I2);
  ACutSchedule.FMarkDIGCorrect := VCut(FMarkDIGCorrect, I1, I2);
  ACutSchedule.FShiftNumsDefault := VCut(FShiftNumsDefault, I1, I2);
  ACutSchedule.FShiftNumsCorrect := VCut(FShiftNumsCorrect, I1, I2);
  ACutSchedule.FScheduleIDs := VCut(FScheduleIDs, I1, I2);
  AInd1:= I1;
  AInd2:= I2;
  FCalculated:= True;
  Result:= True;
end;

procedure TCustomShiftSchedule.SetMarks(const AInd, ADigMark: Integer;
  const AStrMark: String);
begin
  FMarkSTRDefault[AInd]:= AStrMark;
  FMarkSTRCorrect[AInd]:= AStrMark;
  FMarkDIGDefault[AInd]:= ADigMark;
  FMarkDIGCorrect[AInd]:= ADigMark;
end;

function TCustomShiftSchedule.GetShiftCount(const AShiftNums: TIntVector): Integer;
var
  i, ShiftNum, SchedID: Integer;
begin
  Result:=0;
  ShiftNum:= 0;
  SchedID:= 0;
  for i:=0 to DaysCount-1 do
  begin
    if AShiftNums[i]>0 then //есть смена
    begin
      if FScheduleIDs[i]<>SchedID then  //изменился график
      begin
        SchedID:= FScheduleIDs[i]; //запоминаем новый график
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

procedure TCustomShiftSchedule.WriteCorrections(const ACorrect: TScheduleCorrections);
var
  i, n: Integer;
begin
  FHoursCorrect.Total:= FHoursDefault.Total;
  FHoursCorrect.Night:= FHoursDefault.Night;
  FMarkSTRCorrect:= VCut(FMarkSTRDefault);
  FMarkDIGCorrect:= VCut(FMarkDIGDefault);
  FShiftNumsCorrect:= VCut(FShiftNumsDefault);
  if VIsNil(ACorrect.Dates) then Exit;
  for i:=0 to High(ACorrect.Dates) do
  begin
    n:= DaysBetweenDates(BeginDate, ACorrect.Dates[i]);
    if (n>=0) and (n<DaysCount) then
    begin
      FHoursCorrect.Total[n]:= ACorrect.HoursTotal[i];
      FHoursCorrect.Night[n]:= ACorrect.HoursNight[i];
      FIsCorrection[n]:= CORRECTION_YES;
      FMarkSTRCorrect[n]:= ACorrect.StrMarks[i];
      FMarkDIGCorrect[n]:= ACorrect.DigMarks[i];
      FShiftNumsCorrect[n]:= ACorrect.ShiftNums[i];
    end;
  end;
end;

procedure TCustomShiftSchedule.Clear;
begin
  FCalculated:= False;
  FDates:= nil;
  FIsCorrection:= nil;
  FMarkSTRDefault:= nil;
  FMarkSTRCorrect:= nil;
  FMarkDIGDefault:= nil;
  FMarkDIGCorrect:= nil;
  FShiftNumsDefault:= nil;
  FShiftNumsCorrect:= nil;
  FScheduleIDs:= nil;
  FHoursDefault.Clear;
  FHoursCorrect.Clear;
end;

procedure TCustomShiftSchedule.WriteCycle(const ACycle: TScheduleCycle);
var
  HrsTotal, HrsNight, DMark, SNum, i: Integer;
  SMark: String;
begin
  if VIsNil(ACycle.Dates) then //график не задан
  begin
    for i:= 0 to DaysCount - 1 do
      FHoursDefault.Add(0, 0);
  end
  else begin
    for i:= 0 to DaysCount - 1 do
    begin
      DateToCycleStage(ACycle, FDates[i], HrsTotal, HrsNight, DMark, SNum, SMark);
      FHoursDefault.Add(HrsTotal, HrsNight);
      FMarkSTRDefault[i]:= SMark;
      FMarkDIGDefault[i]:= DMark;
      FShiftNumsDefault[i]:= SNum;
    end;
  end;
end;

procedure TCustomShiftSchedule.WriteCalendarSpecDays(const ACalendar: TCalendar;
  const ACycle: TScheduleCycle);
var
  i,n: Integer;
  NotWorkStr: String;
begin
  NotWorkStr:= DataBase.ValueStrInt32ID('TIMETABLEMARK', 'StrMark', 'DigMark', 26 {В});
  for i:=0 to High(ACalendar.Dates) do
  begin
    if ACalendar.DayStatuses[i] in [DAY_STATUS_HOLIDAY, DAY_STATUS_OFFDAY] then  //праздник или выходной
    begin
      FHoursDefault.Total[i]:= 0;
      FHoursDefault.Night[i]:= 0;
      FMarkSTRDefault[i]:= NotWorkStr;
      FMarkDIGDefault[i]:= 26; {В}
      FShiftNumsDefault[i]:= 0;
    end
    else begin  //будний или предпраздничный сокращенный
      //день, установленный на замену
      if ACalendar.SwapDays[i]>0 then
      begin
        n:= ACalendar.SwapDays[i] - 1;  //индекс заменяемого дня
        FHoursDefault.Total[i]:= ACycle.HoursTotal[n];
        FHoursDefault.Night[i]:= ACycle.HoursNight[n];
        FMarkSTRDefault[i]:= ACycle.StrMarks[n];
        FMarkDIGDefault[i]:= ACycle.DigMarks[n];
        FShiftNumsDefault[i]:= ACycle.ShiftNums[n];
      end;
      if ACalendar.DayStatuses[i]= DAY_STATUS_BEFORE then  //сокращенный день
      begin
        FHoursDefault.Total[i]:= CalcShortenedDayHours(FHoursDefault.Total[i]);
        FHoursDefault.Night[i]:= CalcShortenedDayHours(FHoursDefault.Night[i]);
      end;
    end;
  end;
end;

procedure TCustomShiftSchedule.Calc(const ACalendar: TCalendar;
                              //const AHoursInWeek: Byte;
                              const ACycle: TScheduleCycle;
                              const ACorrect: TScheduleCorrections);
begin
  Clear;
  //берем основные параметры с календаря
  FDates:= VCut(ACalendar.Dates);
  VDim(FIsCorrection, DaysCount, CORRECTION_NO);
  VDim(FShiftNumsDefault, DaysCount, 0);
  //вектор пустых значений кодов табеля
  VDim(FMarkSTRDefault, DaysCount, STRMARK_UNKNOWN);
  VDim(FMarkDIGDefault, DaysCount, DIGMARK_UNKNOWN);
  //ID графика
  VDim(FScheduleIDs, DaysCount, ACycle.ScheduleID);
  //записываем цикл
  WriteCycle(ACycle);
  //записываем особые дни производственного календаря, если график ежедневный
  if ACycle.IsWeek then WriteCalendarSpecDays(ACalendar, ACycle);
  //заполняем корректировки
  WriteCorrections(ACorrect);
  FCalculated:= True;
end;

function TCustomShiftSchedule.GetDaysCountDefault: Integer;
begin
  Result:= VCountIfNot(FHoursDefault.Total, 0);
end;

function TCustomShiftSchedule.GetDaysCountCorrect: Integer;
begin
  Result:= VCountIfNot(FHoursCorrect.Total, 0);
end;

function TCustomShiftSchedule.GetBeginDate: TDate;
begin
  Result:= VFirst(FDates);
end;

function TCustomShiftSchedule.GetEndDate: TDate;
begin
  Result:= VLast(FDates);
end;

function TCustomShiftSchedule.GetDaysCount: Integer;
begin
  Result:= Length(FDates);
end;

function TCustomShiftSchedule.GetShiftCountCorrect: Integer;
begin
  Result:= GetShiftCount(FShiftNumsCorrect);
end;

function TCustomShiftSchedule.GetShiftCountDeafult: Integer;
begin
  Result:= GetShiftCount(FShiftNumsDefault);
end;

end.

