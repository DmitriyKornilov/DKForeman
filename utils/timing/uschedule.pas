unit USchedule;

{$mode ObjFPC}{$H+}

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
  EmptyScheduleCycle: TScheduleCycle =
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
  TShiftScheduleInfo = record
    ScheduleIDs: TIntVector; //список уникальных ID графиков
    Cycles: array of TScheduleCycle;
    Corrections: array of array of TScheduleCorrections;
    FirstDates, LastDates: TDateMatrix; //график с одним ID может попадаться в периоде несколько раз
  end;

  TPostScheduleInfo = record
    PostIDs: TIntVector; //список уникальных ID должностей
    Infos: array of array of TShiftScheduleInfo;
    FirstDates, LastDates: TDateMatrix; //должность с одним ID может попадаться в периоде несколько раз
  end;

const
  EmptyShiftScheduleInfo: TShiftScheduleInfo =
    (ScheduleIDs : nil;
     Cycles      : nil;
     Corrections : nil;
     FirstDates  : nil;
     LastDates   : nil;
    );

  EmptyPostScheduleInfo: TPostScheduleInfo =
    (PostIDs    : nil;
     Infos      : nil;
     FirstDates : nil;
     LastDates  : nil;
    );

type

  { TCustomSchedule }

  TCustomSchedule = class(TObject)
  protected
    FIsCalculated: Boolean;

    FDates: TDateVector;             //вектор дат
    FScheduleIDs: TIntVector;        //вектор ID графика для каждого дня
    FIsExists: TIntVector;           //флаги существования графика (EXISTS_NO / EXISTS_YES)
    FIsCorrections: TIntVector;      //вектор флагов наличия корректировок графика (CORRECTION_NO - нет, CORRECTION_YES - есть)

    FHoursDefault: TWorkHours;       //рабочие часы (типовые)
    FHoursCorrect: TWorkHours;       //рабочие часы c учетом корректировок

    FMarkSTRDefault: TStrVector;     //вектор буквенных табельных кодов графика по умолчанию
    FMarkSTRCorrect: TStrVector;     //вектор буквенных табельных кодов графика с корректировками
    FMarkDIGDefault: TIntVector;     //вектор цифровых табельных кодов графика по умолчанию
    FMarkDIGCorrect: TIntVector;     //вектор цифровых табельных кодов графика с корректировками

    FShiftNumsDefault: TIntVector;   //вектор номеров смен
    FShiftNumsCorrect: TIntVector;   //вектор номеров смен с учетом корректировок

    function GetShiftCount(const AShiftNums: TIntVector): Integer;
    function DateIndex(const ADate: TDate; out AIndex: Integer): Boolean;
    function FromToIndexes(const AFromDate, AToDate: TDate; out AIndex1, AIndex2: Integer): Boolean;

    procedure WriteDefault(const ACalendar: TCalendar); virtual;
    procedure WriteCycle(const ACycle: TScheduleCycle;
                         const AFromDate, AToDate: TDate);
    procedure WriteCalendarSpecDays(const ACalendar: TCalendar;
                         const ACycle: TScheduleCycle;
                         const AFromDate, AToDate: TDate);
    procedure WriteCorrections(const ACorrect: TScheduleCorrections;
                         const AFromDate, AToDate: TDate); virtual;

    procedure SetHours(const AIndex, ATotalHours, ANightHours: Integer); virtual;
    procedure SetMarks(const AIndex, ADigMark: Integer; const AStrMark: String); virtual;
    procedure SetShiftNums(const AIndex, AShiftNumber: Integer); virtual;
    procedure SetDay(const AIndex, ATotalHours, ANightHours, AShiftNumber, ADigMark: Integer;
                     const AStrMark: String);
  private
    function GetBeginDate: TDate;
    function GetEndDate: TDate;

    function GetDaysCount: Integer;
    function GetDaysCountCorrect: Integer;
    function GetDaysCountDefault: Integer;
    function GetShiftCountCorrect: Integer;
    function GetShiftCountDeafult: Integer;

    function GetIsDefines(const AIndex: Integer): Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear; virtual;

    function Cut(const AFromDate, AToDate: TDate;
                 var ACutSchedule: TCustomSchedule): Boolean;

    property IsCalculated: Boolean read FIsCalculated;
    property ScheduleIDs: TIntVector read FScheduleIDs;

    property IsExists: TIntVector read FIsExists;
    property IsCorrections: TIntVector read FIsCorrections;
    property IsDefines[const AIndex: Integer]: Integer read GetIsDefines;
    function IsDateDefines(const ADate: TDate): Boolean;
    function IsDateExists(const ADate: TDate): Boolean;

    property Dates: TDateVector read FDates;
    property BeginDate: TDate read GetBeginDate;
    property EndDate: TDate read GetEndDate;

    property DaysCount: Integer read GetDaysCount;  //общее кол-во дней
    property DaysCountDefault: Integer read GetDaysCountDefault;  //кол-во отработанных дней (типовые)
    property DaysCountCorrect: Integer read GetDaysCountCorrect;  //кол-во отработанных дней с учетом корректировок

    property HoursDefault: TWorkHours read FHoursDefault; //рабочие часы (типовые)
    property HoursCorrect: TWorkHours read FHoursCorrect; //рабочие часы c учетом корректировок

    property MarkSTRDefault: TStrVector read FMarkSTRDefault;
    property MarkSTRCorrect: TStrVector read FMarkSTRCorrect;
    property MarkDIGDefault: TIntVector read FMarkDIGDefault;
    property MarkDIGCorrect: TIntVector read FMarkDIGCorrect;

    property ShiftNumbersDefault: TIntVector read FShiftNumsDefault;
    property ShiftNumbersCorrect: TIntVector read FShiftNumsCorrect;
    property ShiftCountDefault: Integer read GetShiftCountDeafult;
    property ShiftCountCorrect: Integer read GetShiftCountCorrect;
  end;

  { TShiftSchedule }

  TShiftSchedule = class(TCustomSchedule)
  private
    FCycle: TScheduleCycle; //структура графика
    FHoursInWeek: Byte;     //типовое кол-во часов в неделю
  public
    procedure Clear; override;
    procedure Calc(const ACalendar: TCalendar;
                   const AHoursInWeek: Byte;
                   const ACycle: TScheduleCycle;
                   const ACorrect: TScheduleCorrections);
    function Cut(const AFromDate, AToDate: TDate;
                 var ACutSchedule: TShiftSchedule): Boolean;
    property HoursInWeek: Byte read FHoursInWeek;
    property Cycle: TScheduleCycle read FCycle;
  end;

  TShiftScheduleVector = array of TShiftSchedule;
  procedure VSAppend(var V: TShiftScheduleVector; const NewValue: TShiftSchedule);
  procedure VSDel(var V: TShiftScheduleVector; Index1: Integer = -1; Index2: Integer = -1);

type

  { TPersonalSchedule }

  TPersonalSchedule = class(TCustomSchedule)
  protected
    FPostIDs: TIntVector;        //вектор ID должности
    FIsVacations: TIntVector;    //флаги отпуска

    FTabNumID: Integer;
    FTabNum: String;
    FRecrutDate, FDismissDate: TDate;

    FHoursDefaultVacation: TWorkHours;  //рабочие часы (типовые с учетом отпуска)
    FHoursCorrectVacation: TWorkHours;  //рабочие часы c учетом корректировок и отпуска

    FMarkSTRDefaultVacation: TStrVector;    //вектор буквенных табельных кодов графика по умолчанию c учетом отпуска
    FMarkSTRCorrectVacation: TStrVector;    //вектор буквенных табельных кодов графика с корректировками и c учетом отпуска
    FMarkDIGDefaultVacation: TIntVector;    //вектор цифровых табельных кодов графика по умолчанию c учетом отпуска
    FMarkDIGCorrectVacation: TIntVector;    //вектор цифровых табельных кодов графика с корректировками и c учетом отпуска

    FShiftNumsDefaultVacation: TIntVector;  //вектор номеров смен (типовые с учетом отпуска)
    FShiftNumsCorrectVacation: TIntVector;  //вектор номеров смен с учетом корректировок и отпуска

    FStrMarkVacationMain: String;
    FStrMarkVacationAddition: String;
    FStrMarkVacationHoliday: String;

    procedure WriteDefault(const ACalendar: TCalendar); override;
    procedure WriteCorrections(const ACorrect: TScheduleCorrections;
                         const AFromDate, AToDate: TDate); override;

    procedure SetHours(const AIndex, ATotalHours, ANightHours: Integer); override;
    procedure SetMarks(const AIndex, ADigMark: Integer; const AStrMark: String); override;
    procedure SetShiftNums(const AIndex, AShiftNumber: Integer); override;

  private
    procedure WriteExistions(const ARecrutDate, ADismissDate: TDate);
    procedure WriteInfo(const ACalendar: TCalendar; const APostScheduleInfo: TPostScheduleInfo);
    procedure WriteVacations(const AIsVacations: TIntVector);
    procedure SetMarksVacation(const AIndex, ADigMark: Integer; const AStrMark: String);

    function GetDaysCountDefaultVacation: Integer;
    function GetDaysCountCorrectVacation: Integer;
    function GetShiftCountCorrectVacation: Integer;
    function GetShiftCountDeafultVacation: Integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear; override;

    procedure Calc(const ATabNumID: Integer; const ATabNum: String;
                   const ACalendar: TCalendar;
                   const APostScheduleInfo: TPostScheduleInfo;
                   const AIsVacations: TIntVector;
                   const APersonalCorrect: TScheduleCorrections;
                   const ARecrutDate, ADismissDate: TDate;
                   const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                   const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                   const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY);

    function Cut(const AFromDate, AToDate: TDate;
                 var ACutSchedule: TPersonalSchedule): Boolean;

    property TabNumID: Integer read FTabNumID;
    property TabNum: String read FTabNum;
    property RecrutDate: TDate read FRecrutDate;
    property DismissDate: TDate read FDismissDate;

    property IsVacations: TIntVector read FIsVacations;

    property HoursDefaultVacation: TWorkHours read FHoursDefaultVacation;
    property HoursCorrectVacation: TWorkHours read FHoursCorrectVacation;

    property DaysCountDefaultVacation: Integer read GetDaysCountDefaultVacation; //кол-во отработанных дней (типовые с учтом отпуска)
    property DaysCountCorrectVacation: Integer read GetDaysCountCorrectVacation; //кол-во отработанных дней с учетом корректировок и отпуска

    property ShiftCountDefaultVacation: Integer read GetShiftCountDeafultVacation;
    property ShiftCountCorrectVacation: Integer read GetShiftCountCorrectVacation;
    property ShiftNumbersDefaultVacation: TIntVector read FShiftNumsDefaultVacation;
    property ShiftNumbersCorrectVacation: TIntVector read FShiftNumsCorrectVacation;

    property MarkSTRDefaultVacation: TStrVector read FMarkSTRDefaultVacation;
    property MarkSTRCorrectVacation: TStrVector read FMarkSTRCorrectVacation;
    property MarkDIGDefaultVacation: TIntVector read FMarkDIGDefaultVacation;
    property MarkDIGCorrectVacation: TIntVector read FMarkDIGCorrectVacation;

    property StrMarkVacationMain    : String read FStrMarkVacationMain;
    property StrMarkVacationAddition: String read FStrMarkVacationAddition;
    property StrMarkVacationHoliday : String read FStrMarkVacationHoliday;
  end;

  TPersonalScheduleVector = array of TPersonalSchedule;
  procedure VSAppend(var V: TPersonalScheduleVector; const NewValue: TPersonalSchedule);
  procedure VSDel(var V: TPersonalScheduleVector; Index1: Integer = -1; Index2: Integer = -1);
  procedure VSSwap(var V: TPersonalScheduleVector; const Index1, Index2: Integer);

  function DateToCycleIndex(const ACycle: TScheduleCycle; const ADate: TDate): Byte;
  procedure DateToCycleStage(const ACycle: TScheduleCycle; const ADate: TDate;
                             out AHoursTotal, AHoursNight, AShiftNum, ADigMark: Integer;
                             out AStrMark: String);

  //Corrections
  function ScheduleCorrectionsCreate(const ADates: TDateVector;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  function ScheduleCorrectionsCreate(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  function ScheduleCorrectionsCopy(const ASource: TScheduleCorrections): TScheduleCorrections;

implementation

function ScheduleCorrectionsCreate(const ADates: TDateVector;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
var
  n: Integer;
begin
  Result:= EmptyScheduleCorrections;
  Result.Dates:= VCut(ADates);
  n:= Length(ADates);
  VDim(Result.HoursTotal, n, AHoursTotal);
  VDim(Result.HoursNight, n, AHoursNight);
  VDim(Result.DigMarks, n, ADigMark);
  VDim(Result.ShiftNums, n, AShiftNum);
  VDim(Result.StrMarks, n, AStrMark);
end;

function ScheduleCorrectionsCreate(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
var
  Dates: TDateVector;
begin
  Dates:= VCreateDate(ABeginDate, AEndDate);
  Result:= ScheduleCorrectionsCreate(Dates, AHoursTotal, AHoursNight, ADigMark,
                                  AShiftNum, AStrMark);
end;

function ScheduleCorrectionsCopy(const ASource: TScheduleCorrections): TScheduleCorrections;
begin
  Result.Dates:= VCut(ASource.Dates);
  Result.HoursTotal:= VCut(ASource.HoursTotal);
  Result.HoursNight:= VCut(ASource.HoursNight);
  Result.DigMarks:= VCut(ASource.DigMarks);
  Result.StrMarks:= VCut(ASource.StrMarks);
  Result.ShiftNums:= VCut(ASource.ShiftNums);
end;

procedure VSAppend(var V: TShiftScheduleVector; const NewValue: TShiftSchedule);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);
  V[N]:= NewValue;
end;

procedure VSDel(var V: TShiftScheduleVector; Index1: Integer; Index2: Integer);
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

procedure VSAppend(var V: TPersonalScheduleVector; const NewValue: TPersonalSchedule);
var
  N: Integer;
begin
  N:= Length(V);
  SetLength(V, N+1);
  V[N]:= NewValue;
end;

procedure VSDel(var V: TPersonalScheduleVector; Index1: Integer; Index2: Integer);
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

procedure VSSwap(var V: TPersonalScheduleVector; const Index1, Index2: Integer);
var
  TmpValue: TPersonalSchedule;
begin
  if not CheckIndexes(High(V), Index1, Index2) then Exit;
  TmpValue:= V[Index1];
  V[Index1]:= V[Index2];
  V[Index2]:= TmpValue;
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

procedure DateToCycleStage(const ACycle: TScheduleCycle; const ADate: TDate;
                             out AHoursTotal, AHoursNight, AShiftNum, ADigMark: Integer;
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

{ TCustomSchedule }

function TCustomSchedule.GetShiftCount(const AShiftNums: TIntVector): Integer;
var
  i, ShiftNum, SchedID: Integer;
begin
  Result:=0;
  ShiftNum:= 0;
  SchedID:= 0;
  for i:=0 to DaysCount-1 do
  begin
    if AShiftNums[i]<=0 then //нет смены
    begin
      ShiftNum:= 0;
      SchedID:= 0;
      continue;     // переход к следующему дню
    end;

    if FScheduleIDs[i]<>SchedID then  //изменился график
    begin
      SchedID:= FScheduleIDs[i]; //запоминаем новый график
      ShiftNum:= AShiftNums[i];  //запоминаем новую смену
      Inc(Result);               //+1  в кол-во смен
    end
    else begin //график тот-же
      if AShiftNums[i]<>ShiftNum then //изменилась смена в этом графике
      begin
        ShiftNum:= AShiftNums[i]; //запоминаем новую смену
        Inc(Result);              //+1  в кол-во смен
      end;
    end;

  end;
end;

function TCustomSchedule.DateIndex(const ADate: TDate; out AIndex: Integer): Boolean;
begin
  Result:= DateIndexInPeriod(ADate, BeginDate, EndDate, AIndex);
end;

function TCustomSchedule.FromToIndexes(const AFromDate, AToDate: TDate;
                                   out AIndex1, AIndex2: Integer): Boolean;
begin
  Result:= DateIndex(AFromDate, AIndex1) and DateIndex(AToDate, AIndex2);
end;

procedure TCustomSchedule.WriteCycle(const ACycle: TScheduleCycle; const AFromDate, AToDate: TDate);
var
  HrsTotal, HrsNight, DMark, SNum, i, I1, I2: Integer;
  SMark: String;
begin
  if ACycle.ScheduleID=0 then Exit; //график не задан

  FromToIndexes(AFromDate, AToDate, I1, I2);
  for i:= I1 to I2 do
  begin
    if FIsExists[i]=EXISTS_NO then continue;
    FScheduleIDs[i]:= ACycle.ScheduleID;
    DateToCycleStage(ACycle, FDates[i], HrsTotal, HrsNight, SNum, DMark, SMark);
    SetDay(i, HrsTotal, HrsNight, SNum, DMark, SMark);
  end;
end;

procedure TCustomSchedule.WriteCalendarSpecDays(const ACalendar: TCalendar;
                         const ACycle: TScheduleCycle;
                         const AFromDate, AToDate: TDate);
var
  i, n, I1, I2: Integer;
begin
  FromToIndexes(AFromDate, AToDate, I1, I2);
  for i:=I1 to I2 do
  begin
    if FIsExists[i]=EXISTS_NO then continue;
    if ACalendar.DayStatuses[i] in [DAY_STATUS_HOLIDAY, DAY_STATUS_OFFDAY] then  //праздник или выходной
      SetDay(i, 0, 0, 0, 26{В}, 'В')
    else begin  //будний или предпраздничный сокращенный
      //день, установленный на замену
      if ACalendar.SwapDays[i]>0 then
      begin
        n:= ACalendar.SwapDays[i] - 1;  //индекс заменяемого дня
        SetDay(i, ACycle.HoursTotal[n], ACycle.HoursNight[n], ACycle.ShiftNums[n],
                  ACycle.DigMarks[n], ACycle.StrMarks[n]);
      end;
      if ACalendar.DayStatuses[i]= DAY_STATUS_BEFORE then  //сокращенный день
        SetHours(i, CalcShortenedDayHours(FHoursDefault.Totals[i]),
                    CalcShortenedDayHours(FHoursDefault.Nights[i]));
    end;
  end;
end;

procedure TCustomSchedule.WriteCorrections(const ACorrect: TScheduleCorrections;
                         const AFromDate, AToDate: TDate);
var
  i, n: Integer;
begin
  if VIsNil(ACorrect.Dates) then Exit;

  for i:=0 to High(ACorrect.Dates) do
  begin
    if not DateIndexInPeriod(ACorrect.Dates[i], AFromDate, AToDate, n) then continue;
    if FIsExists[n]=EXISTS_NO then continue;
    FIsCorrections[n]:= CORRECTION_YES;
    FHoursCorrect.Totals[n]:= ACorrect.HoursTotal[i];
    FHoursCorrect.Nights[n]:= ACorrect.HoursNight[i];
    FMarkSTRCorrect[n]:= ACorrect.StrMarks[i];
    FMarkDIGCorrect[n]:= ACorrect.DigMarks[i];
    FShiftNumsCorrect[n]:= ACorrect.ShiftNums[i];
  end;
end;

procedure TCustomSchedule.SetHours(const AIndex, ATotalHours, ANightHours: Integer);
begin
  FHoursDefault.Totals[AIndex]:= ATotalHours;
  FHoursDefault.Nights[AIndex]:= ANightHours;
  FHoursCorrect.Totals[AIndex]:= ATotalHours;
  FHoursCorrect.Nights[AIndex]:= ANightHours;
end;

procedure TCustomSchedule.SetMarks(const AIndex, ADigMark: Integer; const AStrMark: String);
begin
  FMarkSTRDefault[AIndex]:= AStrMark;
  FMarkSTRCorrect[AIndex]:= AStrMark;
  FMarkDIGDefault[AIndex]:= ADigMark;
  FMarkDIGCorrect[AIndex]:= ADigMark;
end;

procedure TCustomSchedule.SetShiftNums(const AIndex, AShiftNumber: Integer);
begin
  FShiftNumsDefault[AIndex]:= AShiftNumber;
  FShiftNumsCorrect[AIndex]:= AShiftNumber;
end;

procedure TCustomSchedule.SetDay(const AIndex, ATotalHours, ANightHours,
  AShiftNumber, ADigMark: Integer; const AStrMark: String);
begin
  SetHours(AIndex, ATotalHours, ANightHours);
  SetMarks(AIndex, ADigMark, AStrMark);
  SetShiftNums(AIndex, AShiftNumber);
end;

function TCustomSchedule.GetBeginDate: TDate;
begin
  Result:= VFirst(FDates);
end;

function TCustomSchedule.GetDaysCountCorrect: Integer;
begin
  Result:= VCountIfNot(FHoursCorrect.Totals, 0);
end;

function TCustomSchedule.GetDaysCountDefault: Integer;
begin
  Result:= VCountIfNot(FHoursDefault.Totals, 0);
end;

function TCustomSchedule.GetShiftCountCorrect: Integer;
begin
  Result:= GetShiftCount(FShiftNumsCorrect);
end;

function TCustomSchedule.GetShiftCountDeafult: Integer;
begin
  Result:= GetShiftCount(FShiftNumsDefault);
end;

function TCustomSchedule.GetIsDefines(const AIndex: Integer): Integer;
begin
  if FScheduleIDs[AIndex]>0 then
    Result:= DEFINE_YES
  else
    Result:= DEFINE_NO;
end;

function TCustomSchedule.GetDaysCount: Integer;
begin
  Result:= Length(FDates);
end;

function TCustomSchedule.GetEndDate: TDate;
begin
  Result:= VLast(FDates);
end;

constructor TCustomSchedule.Create;
begin
  inherited Create;
  FHoursDefault:= TWorkHours.Create;
  FHoursCorrect:= TWorkHours.Create;
  Clear;
end;

destructor TCustomSchedule.Destroy;
begin
  FreeAndNil(FHoursDefault);
  FreeAndNil(FHoursCorrect);
  inherited Destroy;
end;

procedure TCustomSchedule.Clear;
begin
  FIsCalculated:= False;
  FDates:= nil;
  FScheduleIDs:= nil;
  FIsCorrections:= nil;
  FIsExists:= nil;
  FMarkSTRDefault:= nil;
  FMarkSTRCorrect:= nil;
  FMarkDIGDefault:= nil;
  FMarkDIGCorrect:= nil;
  FShiftNumsDefault:= nil;
  FShiftNumsCorrect:= nil;
  FHoursDefault.Clear;
  FHoursCorrect.Clear;
end;

function TCustomSchedule.Cut(const AFromDate, AToDate: TDate;
                 var ACutSchedule: TCustomSchedule): Boolean;
var
  I1, I2: Integer;
begin
  Result:= False;
  ACutSchedule.Clear;
  if not (Assigned(ACutSchedule) and IsCalculated) then Exit;

  //определяем индексы среза
  FromToIndexes(AFromDate, AToDate, I1, I2);
  //заполняем данные
  ACutSchedule.FDates:= VCut(FDates, I1, I2);
  ACutSchedule.FScheduleIDs:= VCut(FScheduleIDs, I1, I2);
  ACutSchedule.FIsExists:= VCut(FIsExists, I1, I2);
  ACutSchedule.FIsCorrections:= VCut(FIsCorrections, I1, I2);

  ACutSchedule.FHoursDefault.Totals:= VCut(FHoursDefault.Totals, I1, I2);
  ACutSchedule.FHoursDefault.Nights:= VCut(FHoursDefault.Nights, I1, I2);
  ACutSchedule.FHoursCorrect.Totals:= VCut(FHoursCorrect.Totals, I1, I2);
  ACutSchedule.FHoursCorrect.Nights:= VCut(FHoursCorrect.Nights, I1, I2);

  ACutSchedule.FMarkSTRDefault:= VCut(FMarkSTRDefault, I1, I2);
  ACutSchedule.FMarkSTRCorrect:= VCut(FMarkSTRCorrect, I1, I2);
  ACutSchedule.FMarkDIGDefault:= VCut(FMarkDIGDefault, I1, I2);
  ACutSchedule.FMarkDIGCorrect:= VCut(FMarkDIGCorrect, I1, I2);

  ACutSchedule.FShiftNumsDefault:= VCut(FShiftNumsDefault, I1, I2);
  ACutSchedule.FShiftNumsCorrect:= VCut(FShiftNumsCorrect, I1, I2);

  ACutSchedule.FIsCalculated:= True;
  Result:= True;
end;

function TCustomSchedule.IsDateDefines(const ADate: TDate): Boolean;
var
  Index: Integer;
begin
  Result:= DateIndex(ADate, Index);
  if not Result then Exit;
  Result:= IsDefines[Index]=DEFINE_YES;
end;

function TCustomSchedule.IsDateExists(const ADate: TDate): Boolean;
var
  Index: Integer;
begin
  Result:= DateIndex(ADate, Index);
  if not Result then Exit;
  Result:= IsExists[Index]=EXISTS_YES;
end;

procedure TCustomSchedule.WriteDefault(const ACalendar: TCalendar);
begin
  FDates:= VCut(ACalendar.Dates);
  VDim(FScheduleIDs, DaysCount, 0);
  VDim(FIsCorrections, DaysCount, CORRECTION_NO);
  VDim(FIsExists, DaysCount, EXISTS_YES);

  VDim(FMarkSTRDefault, DaysCount, STRMARK_UNKNOWN);
  VDim(FMarkDIGDefault, DaysCount, DIGMARK_UNKNOWN);
  VDim(FMarkSTRCorrect, DaysCount, STRMARK_UNKNOWN);
  VDim(FMarkDIGCorrect, DaysCount, DIGMARK_UNKNOWN);

  VDim(FShiftNumsDefault, DaysCount, 0);
  VDim(FShiftNumsCorrect, DaysCount, 0);

  FHoursDefault.Zeros(DaysCount);
  FHoursCorrect.Zeros(DaysCount);
end;

{ TShiftSchedule }

procedure TShiftSchedule.Clear;
begin
  inherited Clear;
  FHoursInWeek:= 0;
  FCycle:= EmptyScheduleCycle;
end;

procedure TShiftSchedule.Calc(const ACalendar: TCalendar;
                   const AHoursInWeek: Byte;
                   const ACycle: TScheduleCycle;
                   const ACorrect: TScheduleCorrections);
begin
  Clear;
  //значения по умолчанию
  WriteDefault(ACalendar);
  //записываем цикл
  WriteCycle(ACycle, BeginDate, EndDate);
  //записываем особые дни производственного календаря, если график ежедневный
  if ACycle.IsWeek then
    WriteCalendarSpecDays(ACalendar, ACycle, BeginDate, EndDate);
  //заполняем корректировки
  WriteCorrections(ACorrect, BeginDate, EndDate);
  FCycle:= ACycle;
  FHoursInWeek:= AHoursInWeek;
  FIsCalculated:= True;
end;

function TShiftSchedule.Cut(const AFromDate, AToDate: TDate;
                            var ACutSchedule: TShiftSchedule): Boolean;
begin
  Result:= inherited Cut(AFromDate, AToDate, TCustomSchedule(ACutSchedule));
  ACutSchedule.FCycle:= FCycle;
  ACutSchedule.FHoursInWeek:= FHoursInWeek;
end;

{ TPersonalSchedule }

procedure TPersonalSchedule.WriteDefault(const ACalendar: TCalendar);
begin
  inherited WriteDefault(ACalendar);
  VDim(FPostIDs, DaysCount, 0);
  VDim(FIsVacations, DaysCount, VACATION_NO);

  VDim(FMarkSTRDefaultVacation, DaysCount, STRMARK_UNKNOWN);
  VDim(FMarkDIGDefaultVacation, DaysCount, DIGMARK_UNKNOWN);
  VDim(FMarkSTRCorrectVacation, DaysCount, STRMARK_UNKNOWN);
  VDim(FMarkDIGCorrectVacation, DaysCount, DIGMARK_UNKNOWN);

  VDim(FShiftNumsDefaultVacation, DaysCount, 0);
  VDim(FShiftNumsCorrectVacation, DaysCount, 0);

  FHoursDefaultVacation.Zeros(DaysCount);
  FHoursCorrectVacation.Zeros(DaysCount);
end;

procedure TPersonalSchedule.WriteCorrections(const ACorrect: TScheduleCorrections;
                   const AFromDate, AToDate: TDate);
var
  i, n: Integer;
begin
  if VIsNil(ACorrect.Dates) then Exit;

  inherited WriteCorrections(ACorrect, AFromDate, AToDate);

  for i:=0 to High(ACorrect.Dates) do
  begin
    if not DateIndexInPeriod(ACorrect.Dates[i], AFromDate, AToDate, n) then continue;
    if FIsExists[n]=EXISTS_NO then continue;
    FIsCorrections[n]:= CORRECTION_YES;
    FHoursCorrectVacation.Totals[n]:= ACorrect.HoursTotal[i];
    FHoursCorrectVacation.Nights[n]:= ACorrect.HoursNight[i];
    FMarkSTRCorrectVacation[n]:= ACorrect.StrMarks[i];
    FMarkDIGCorrectVacation[n]:= ACorrect.DigMarks[i];
    FShiftNumsCorrectVacation[n]:= ACorrect.ShiftNums[i];
  end;
end;

procedure TPersonalSchedule.SetHours(const AIndex, ATotalHours, ANightHours: Integer);
begin
  inherited SetHours(AIndex, ATotalHours, ANightHours);
  FHoursDefaultVacation.Totals[AIndex]:= ATotalHours;
  FHoursDefaultVacation.Nights[AIndex]:= ANightHours;
  FHoursCorrectVacation.Totals[AIndex]:= ATotalHours;
  FHoursCorrectVacation.Nights[AIndex]:= ANightHours;
end;

procedure TPersonalSchedule.SetMarks(const AIndex, ADigMark: Integer; const AStrMark: String);
begin
  inherited SetMarks(AIndex, ADigMark, AStrMark);
  FMarkSTRDefaultVacation[AIndex]:= AStrMark;
  FMarkSTRCorrectVacation[AIndex]:= AStrMark;
  FMarkDIGDefaultVacation[AIndex]:= ADigMark;
  FMarkDIGCorrectVacation[AIndex]:= ADigMark;
end;

procedure TPersonalSchedule.SetShiftNums(const AIndex, AShiftNumber: Integer);
begin
  inherited SetShiftNums(AIndex, AShiftNumber);
  FShiftNumsDefaultVacation[AIndex]:= AShiftNumber;
  FShiftNumsCorrectVacation[AIndex]:= AShiftNumber;
end;

procedure TPersonalSchedule.WriteExistions(const ARecrutDate, ADismissDate: TDate);
var
  i, I1, I2: Integer;
begin
  //определяем подпериод существования графика, запоминяем соответствующие индексы
  if not VCrossInd(FDates, ARecrutDate, ADismissDate, I1, I2) then Exit;

  //устанавливаем вне периода пересечения флаги несуществования
  VChangeNotIn(FIsExists, EXISTS_NO, I1, I2);
  //устанавливаем вне периода существования коды табеля
  for i:= 0 to I1-1 do
    SetMarks(i, DIGMARK_NONEXISTENT, STRMARK_NONEXISTENT);
  for i:= I2+1 to DaysCount-1 do
    SetMarks(i, DIGMARK_NONEXISTENT, STRMARK_NONEXISTENT);
end;

procedure TPersonalSchedule.WriteInfo(const ACalendar: TCalendar;
                                      const APostScheduleInfo: TPostScheduleInfo);
var
  i, j, m, n, I1, I2: Integer;
  BD, ED: TDate;
  ShiftScheduleInfo: TShiftScheduleInfo;
begin
  {TShiftScheduleInfo = record
    ScheduleIDs: TIntVector; //список уникальных ID графиков
    Cycles: array of TScheduleCycle;
    Corrections: array of array of TScheduleCorrections;
    FirstDates, LastDates: TDateMatrix; //график с одним ID может попадаться в периоде несколько раз
  end;

  TPostScheduleInfo = record
    PostIDs: TIntVector; //список уникальных ID должностей
    Infos: array of array of TShiftScheduleInfo;
    FirstDates, LastDates: TDateMatrix; //должность с одним ID может попадаться в периоде несколько раз
  end;     }

  for i:= 0 to High(APostScheduleInfo.PostIDs) do
    for j:= 0 to High(APostScheduleInfo.FirstDates[i]) do
    begin
      //подпериод в должности
      BD:= APostScheduleInfo.FirstDates[i, j];
      ED:= APostScheduleInfo.LastDates[i, j];
      FromToIndexes(BD, ED, I1, I2);
      VChangeIf(FPostIDs, APostScheduleInfo.PostIDs[i], FIsExists, EXISTS_YES, I1, I2);

      ShiftScheduleInfo:= APostScheduleInfo.Infos[i, j];
      for m:= 0 to High(ShiftScheduleInfo.ScheduleIDs) do
        for n:= 0 to High(ShiftScheduleInfo.FirstDates[m]) do
        begin
          //подпериод в сменном графике
          BD:= ShiftScheduleInfo.FirstDates[m, n];
          ED:= ShiftScheduleInfo.LastDates[m, n];
          WriteCycle(ShiftScheduleInfo.Cycles[m], BD, ED);
          if ShiftScheduleInfo.Cycles[m].IsWeek then
            WriteCalendarSpecDays(ACalendar, ShiftScheduleInfo.Cycles[m], BD, ED);
          WriteCorrections(ShiftScheduleInfo.Corrections[m, n], BD, ED);
        end;
    end;
end;

procedure TPersonalSchedule.WriteVacations(const AIsVacations: TIntVector);
var
  i: Integer;
begin
  for i:=0 to DaysCount-1 do
  begin
    if (FIsExists[i]=EXISTS_NO) or (AIsVacations[i]=VACATION_NO) then continue;
    FIsVacations[i]:= AIsVacations[i];

    FHoursDefaultVacation.Totals[i]:= 0;
    FHoursDefaultVacation.Nights[i]:= 0;
    FHoursCorrectVacation.Totals[i]:= 0;
    FHoursCorrectVacation.Nights[i]:= 0;

    FShiftNumsDefaultVacation[i]:= 0;
    FShiftNumsCorrectVacation[i]:= 0;

    case AIsVacations[i] of
      VACATION_MAIN    : SetMarksVacation(i, DIGMARK_VACATIONMAIN, FStrMarkVacationMain);
      VACATION_ADDITION: SetMarksVacation(i, DIGMARK_VACATIONADDITION, FStrMarkVacationAddition);
      VACATION_HOLIDAY : SetMarksVacation(i, DIGMARK_VACATIONHOLIDAY, FStrMarkVacationHoliday);
    end;
  end;
end;

procedure TPersonalSchedule.SetMarksVacation(const AIndex, ADigMark: Integer;
  const AStrMark: String);
begin
  FMarkSTRDefaultVacation[AIndex]:= AStrMark;
  FMarkSTRCorrectVacation[AIndex]:= AStrMark;
  FMarkDIGDefaultVacation[AIndex]:= ADigMark;
  FMarkDIGCorrectVacation[AIndex]:= ADigMark;
end;

function TPersonalSchedule.GetDaysCountDefaultVacation: Integer;
begin
  Result:= VCountIfNot(FHoursDefaultVacation.Totals, 0);
end;

function TPersonalSchedule.GetDaysCountCorrectVacation: Integer;
begin
  Result:= VCountIfNot(FHoursCorrectVacation.Totals, 0);
end;

function TPersonalSchedule.GetShiftCountCorrectVacation: Integer;
begin
  Result:= GetShiftCount(FShiftNumsCorrectVacation);
end;

function TPersonalSchedule.GetShiftCountDeafultVacation: Integer;
begin
  Result:= GetShiftCount(FShiftNumsDefaultVacation);
end;

constructor TPersonalSchedule.Create;
begin
  FHoursDefaultVacation:= TWorkHours.Create;
  FHoursCorrectVacation:= TWorkHours.Create;
  inherited Create;
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

  FTabNumID:= 0;
  FTabNum:= EmptyStr;
  FRecrutDate:= 0;
  FDismissDate:= 0;

  FStrMarkVacationMain:= STRMARK_VACATIONMAIN;
  FStrMarkVacationAddition:= STRMARK_VACATIONADDITION;
  FStrMarkVacationHoliday:= STRMARK_VACATIONHOLIDAY;

  FPostIDs:= nil;
  FIsVacations:= nil;
  FMarkSTRDefaultVacation:= nil;
  FMarkSTRCorrectVacation:= nil;
  FMarkDIGDefaultVacation:= nil;
  FMarkDIGCorrectVacation:= nil;
  FShiftNumsDefaultVacation:= nil;
  FShiftNumsCorrectVacation:= nil;
  FHoursDefaultVacation.Clear;
  FHoursCorrectVacation.Clear;
end;

procedure TPersonalSchedule.Calc(const ATabNumID: Integer; const ATabNum: String;
                   const ACalendar: TCalendar;
                   const APostScheduleInfo: TPostScheduleInfo;
                   const AIsVacations: TIntVector;
                   const APersonalCorrect: TScheduleCorrections;
                   const ARecrutDate, ADismissDate: TDate;
                   const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                   const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                   const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY);
begin
  Clear;

  FTabNumID:= ATabNumID;
  FTabNum:= ATabNum;
  FRecrutDate:= ARecrutDate;
  FDismissDate:= ADismissDate;

  FStrMarkVacationMain:= AStrMarkVacationMain;
  FStrMarkVacationAddition:= AStrMarkVacationAddition;
  FStrMarkVacationHoliday:= AStrMarkVacationHoliday;

  WriteDefault(ACalendar);
  WriteExistions(ARecrutDate, ADismissDate);
  WriteInfo(ACalendar, APostScheduleInfo);
  WriteCorrections(APersonalCorrect, BeginDate, EndDate);
  WriteVacations(AIsVacations);

  FIsCalculated:= True;
end;

function TPersonalSchedule.Cut(const AFromDate, AToDate: TDate;
                               var ACutSchedule: TPersonalSchedule): Boolean;
var
  I1, I2: Integer;
begin
  Result:= False;
  ACutSchedule.Clear;
  if not (Assigned(ACutSchedule) and IsCalculated) then Exit;

  Result:= inherited Cut(AFromDate, AToDate, TCustomSchedule(ACutSchedule));
  if not Result then Exit;

  ACutSchedule.FIsCalculated:= False;

  ACutSchedule.FTabNumID:= FTabNumID;
  ACutSchedule.FTabNum:= FTabNum;
  ACutSchedule.FRecrutDate:= FRecrutDate;
  ACutSchedule.FDismissDate:= FDismissDate;

  ACutSchedule.FStrMarkVacationMain:= FStrMarkVacationMain;
  ACutSchedule.FStrMarkVacationAddition:= FStrMarkVacationAddition;
  ACutSchedule.FStrMarkVacationHoliday:= FStrMarkVacationHoliday;

  //определяем индексы среза
  FromToIndexes(AFromDate, AToDate, I1, I2);

  //заполняем данные
  ACutSchedule.FIsVacations:= VCut(FIsVacations, I1, I2);
  ACutSchedule.FPostIDs:= VCut(FPostIDs, I1, I2);

  ACutSchedule.FMarkSTRDefaultVacation:= VCut(FMarkSTRDefaultVacation, I1, I2);
  ACutSchedule.FMarkSTRCorrectVacation:= VCut(FMarkSTRCorrectVacation, I1, I2);
  ACutSchedule.FMarkDIGDefaultVacation:= VCut(FMarkDIGDefaultVacation, I1, I2);
  ACutSchedule.FMarkDIGCorrectVacation:= VCut(FMarkDIGCorrectVacation, I1, I2);

  ACutSchedule.FShiftNumsDefaultVacation:= VCut(FShiftNumsDefaultVacation, I1, I2);
  ACutSchedule.FShiftNumsCorrectVacation:= VCut(FShiftNumsCorrectVacation, I1, I2);

  ACutSchedule.FHoursDefaultVacation.Totals:= VCut(FHoursDefaultVacation.Totals, I1, I2);
  ACutSchedule.FHoursDefaultVacation.Nights:= VCut(FHoursDefaultVacation.Nights, I1, I2);
  ACutSchedule.FHoursCorrectVacation.Totals:= VCut(FHoursCorrectVacation.Totals, I1, I2);
  ACutSchedule.FHoursCorrectVacation.Nights:= VCut(FHoursCorrectVacation.Nights, I1, I2);

  ACutSchedule.FIsCalculated:= True;
  Result:= True;
end;

end.

