unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BCPanel, BCButton, DateUtils,
  //DK packages utils
  DK_CtrlUtils, DK_Color, DK_Vector, DK_DateUtils, DK_Const, DK_Fonts,
  DK_StrUtils, DK_VSTEdit, DK_VSTTables, DK_VSTCore, DK_PPI, DK_VSTDropDownConst,

  //Project utils
  UDataBase, UConst, UWorkHours, UCalendar, USchedule;

  //UI
  procedure SetToolPanels(const AControls: array of TControl);
  procedure SetCaptionPanels(const AControls: array of TBCPanel);
  procedure SetToolButtons(const AControls: array of TControl);
  procedure SetCategoryButtons(const AControls: array of TBCButton);
  function GetControlCaptionWidth(const AControl: TControl; const ACaption: String): Integer;
  function GetBCButtonWidth(const AControl: TControl; const ACaption: String): Integer;

  //ID for TVSTTable.ReSelect
  function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  //Settings
  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;

  //Staff list
  function StaffNameForTiming(const AF, AN, AP, ATabNum, APostName: String;
                              const ANeedLongName: Boolean = False): String;
  function StaffNamesForTiming(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                              const ANeedLongName: Boolean = False): TStrVector;


  //Calendar
  procedure CalendarForPeriod(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
  procedure CalendarForYear(const AYear: Word; var ACalendar: TCalendar);
  procedure CalendarForMonth(const AMonth, AYear: Word; var ACalendar: TCalendar);
  //Calendar corrections
  function GetCalendarCorrections(const ADates: TDateVector;
                         const AStatus, ASwapDay: Integer): TCalendarCorrections;
  function GetCalendarCorrections(const ABeginDate, AEndDate: TDate;
                         const AStatus, ASwapDay: Integer): TCalendarCorrections;
  //Schedule corrections
  function GetScheduleCorrections(const ADates: TDateVector;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  function GetScheduleCorrections(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  //ScheduleShift
  procedure ScheduleCycleToWeek(var ACycle: TScheduleCycle);
  procedure ScheduleCycleToCount(var ACycle: TScheduleCycle; const ACount: Integer; const AFirstDate: TDate);
  procedure ScheduleCycleDateColumnSet(const ATable: TVSTCustomSimpleTable; const ACycle: TScheduleCycle; out AStrDates: TStrVector);
  procedure ScheduleCycleDraw(const ATable: TVSTEdit; const ACycle: TScheduleCycle);
  procedure ScheduleCycleDraw(const ATable: TVSTTable; const ACycle: TScheduleCycle);
  procedure ScheduleShiftByCalendar(const AScheduleID: Integer;
                         const ACalendar: TCalendar; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftVectorByCalendar(const AScheduleIDs: TIntVector;
                         const ACalendar: TCalendar; var ASchedules: TShiftScheduleVector);
  procedure ScheduleShiftForPeriod(const AScheduleID: Integer;
                         const ABeginDate, AEndDate: TDate; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForYear(const AScheduleID: Integer;
                         const AYear: Word; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForMonth(const AScheduleID: Integer;
                         const AMonth, AYear: Word; var ASchedule: TShiftSchedule);
  //SchedulePersonal

  { расчет нормы часов ANormHours и кол-ва рабочих дней AWorkDaysCount
    для таб номера c ATabNumID за период  ABeginDate, AEndDate
   (внутри периода могут быть графики разных рабочих недель (40ч, 36ч и т.д.))
   Если указать даты приема/увольнения ARecrutDate и ADismissDate, то будет
   расчет с их учетом (например на неполный месяц)}
  procedure NormHoursAndWorkDaysCounInPeriod(const ATabNumID: Integer;
                               const ABeginDate, AEndDate: TDate; //период
                               const ACalendar: TCalendar; //производственный календарь, перекрывающий период
                               out AWorkDaysCount, ANormHours: Integer;
                               const ARecrutDate: TDate = NULDATE;
                               const ADismissDate: TDate = INFDATE);

implementation

procedure SetToolPanels(const AControls: array of TControl);
var
  i: Integer;
begin
  for i:= 0 to High(AControls) do
    ControlHeight(AControls[i], TOOL_PANEL_HEIGHT_DEFAULT);
end;

procedure SetCaptionPanels(const AControls: array of TBCPanel);
var
  i, h: Integer;
  c: TColor;
begin
  h:= Round(TOOL_PANEL_HEIGHT_DEFAULT*0.65);
  c:= cl3DLight;
  //c:= ColorIncLightness(clBtnFace, -15);
  for i:= 0 to High(AControls) do
  begin
    ControlHeight(AControls[i], h);
    AControls[i].Background.Color:= c;
    AControls[i].Border.Color:= clActiveBorder;
    AControls[i].Rounding.RoundX:= 0;
    AControls[i].Rounding.RoundY:= 0;
  end;
end;

procedure SetToolButtons(const AControls: array of TControl);
var
  i: Integer;
begin
  for i:= 0 to High(AControls) do
    ControlWidth(AControls[i], TOOL_BUTTON_WIDTH_DEFAULT);
end;

procedure SetCategoryButtons(const AControls: array of TBCButton);
var
  i: Integer;
  c: TColor;
begin
  c:= cl3DLight;
  //c:= ColorIncLightness(clBtnFace, -15);
  for i:= 0 to High(AControls) do
  begin
    AControls[i].StateNormal.Background.Color:= c;
    AControls[i].StateNormal.Border.Color:= clActiveBorder;
  end;
end;

function GetControlCaptionWidth(const AControl: TControl; const ACaption: String): Integer;
var
  AFontName: String;
  AFontSize: Single;
  DesignTimePPI: Integer;
begin
  LoadFontFromControl(AControl, AFontName, AFontSize);
  Result:= SWidth(ACaption, AFontName, AFontSize);
  DesignTimePPI:= ControlDesignTimePPI(AControl);
  Result:= SizeFromDefaultToDesignTime(Result, DesignTimePPI);
end;

function GetBCButtonWidth(const AControl: TControl; const ACaption: String): Integer;
var
  DesignTimePPI: Integer;
begin
  DesignTimePPI:= ControlDesignTimePPI(AControl);
  Result:= GetControlCaptionWidth(AControl, ACaption) +
           SizeFromDefaultToDesignTime(DROPDOWN_WIDTH_DEFAULT, DesignTimePPI);
end;

function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                       const ASelectedID: Integer = -1): Integer;
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

function StaffNameForTiming(const AF, AN, AP, ATabNum, APostName: String;
                            const ANeedLongName: Boolean = False): String;
begin
  if ANeedLongName then
    Result:= SNameLong(AF, AN, AP)
  else
    Result:= SNameShort(AF, AN, AP);
  Result:= Result + ' [таб.№ ' + ATabNum + '] - ' + APostName;
end;

function StaffNamesForTiming(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                            const ANeedLongName: Boolean = False): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ATabNums) then Exit;
  VDim(Result, Length(ATabNums));
  for i:= 0 to High(Result) do
    Result[i]:= StaffNameForTiming(AFs[i], ANs[i], APs[i], ATabNums[i], APostNames[i], ANeedLongName);
end;

procedure CalendarForPeriod(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
begin
  if not Assigned(ACalendar) then
    ACalendar:= TCalendar.Create;
  DataBase.CalendarLoad(ABeginDate, AEndDate, ACalendar);
end;

procedure CalendarForYear(const AYear: Word; var ACalendar: TCalendar);
var
  BD, ED: TDate;
begin
  FirstLastDayInYear(AYear, BD, ED);
  CalendarForPeriod(BD, ED, ACalendar);
end;

procedure CalendarForMonth(const AMonth, AYear: Word; var ACalendar: TCalendar);
var
  BD, ED: TDate;
begin
  FirstLastDayInMonth(AMonth, AYear, BD, ED);
  CalendarForPeriod(BD, ED, ACalendar);
end;

function GetCalendarCorrections(const ADates: TDateVector;
                       const AStatus, ASwapDay: Integer): TCalendarCorrections;
var
  n: Integer;
begin
  Result:= CalendarCorrectionsEmpty;
  Result.Dates:= VCut(ADates);
  n:= Length(ADates);
  VDim(Result.Statuses, n, AStatus);
  VDim(Result.SwapDays, n, ASwapDay);
end;

function GetCalendarCorrections(const ABeginDate, AEndDate: TDate;
  const AStatus, ASwapDay: Integer): TCalendarCorrections;
var
  Dates: TDateVector;
begin
  Dates:= VCreateDate(ABeginDate, AEndDate);
  Result:= GetCalendarCorrections(Dates, AStatus, ASwapDay);
end;

function GetScheduleCorrections(const ADates: TDateVector;
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

function GetScheduleCorrections(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
var
  Dates: TDateVector;
begin
  Dates:= VCreateDate(ABeginDate, AEndDate);
  Result:= GetScheduleCorrections(Dates, AHoursTotal, AHoursNight, ADigMark,
                                  AShiftNum, AStrMark);
end;

procedure ScheduleCycleToWeek(var ACycle: TScheduleCycle);
var
  i, x, n: Integer;
  S: String;
begin
  ACycle.IsWeek:= True;
  ACycle.Count:= 0;
  n:= Length(ACycle.Dates);

  VRedim(ACycle.Dates, 7);
  for i:= 0 to 6 do
    ACycle.Dates[i]:= IncDay(MONDAY_DATE, i);
  if n=7 then Exit;
  S:= DataBase.TimetableStrMarkLoad(1);
  VRedim(ACycle.HoursTotal, 7, 0);
  VRedim(ACycle.HoursNight, 7, 0);
  VRedim(ACycle.DigMarks, 7, 1);
  VRedim(ACycle.StrMarks, 7, S);
  VRedim(ACycle.ShiftNums, 7, 0);
  if n>7 then Exit;

  S:= DataBase.TimetableStrMarkLoad(26);
  if n<=5 then
  begin
    x:= 8*WORKHOURS_DENOMINATOR;
    for i:= n to 4 do
    begin
      ACycle.HoursTotal[i]:= x;
      ACycle.ShiftNums[i]:= i+1;
    end;
    for i:= 5 to 6 do
    begin
      ACycle.DigMarks[i]:= 26 {B};
      ACycle.StrMarks[i]:= S;
    end;
  end
  else begin
    for i:= n-1 to 6 do
    begin
      ACycle.DigMarks[i]:= 26 {B};
      ACycle.StrMarks[i]:= S;
    end;
  end;
end;

procedure ScheduleCycleToCount(var ACycle: TScheduleCycle; const ACount: Integer;
                               const AFirstDate: TDate);
var
  i, n: Integer;
begin
  if ACycle.IsWeek then
    n:= 7
  else
    n:= ACycle.Count;
  ACycle.IsWeek:= False;
  ACycle.Count:= ACount;

  VRedim(ACycle.Dates, ACount);
  for i:= 0 to ACount-1 do
    ACycle.Dates[i]:= IncDay(AFirstDate, i);
  if n=ACount then Exit;

  VRedim(ACycle.HoursTotal, ACount, 0);
  VRedim(ACycle.HoursNight, ACount, 0);
  VRedim(ACycle.DigMarks, ACount, 26);
  VRedim(ACycle.StrMarks, ACount, DataBase.TimetableStrMarkLoad(26));
  VRedim(ACycle.ShiftNums, ACount, 0);
end;

procedure ScheduleCycleDateColumnSet(const ATable: TVSTCustomSimpleTable;
                                     const ACycle: TScheduleCycle;
                                     out AStrDates: TStrVector);
begin
  if ACycle.IsWeek then
  begin
    ATable.RenameColumn(0, 'День');
    AStrDates:= VCreateStr(WEEKDAYSSHORT);
  end
  else begin
    ATable.RenameColumn(0, SCHEDULE_CORRECTION_COLUMN_NAMES[0]);
    AStrDates:= VDateToStr(ACycle.Dates);
  end;
end;

procedure ScheduleCycleDraw(const ATable: TVSTEdit; const ACycle: TScheduleCycle);
var
  StrDates: TStrVector;
begin
  ScheduleCycleDateColumnSet(ATable, ACycle, StrDates);

  ATable.Visible:= False;
  try
    ATable.ValuesClear;
    ATable.SetColumnRowTitles(StrDates);
    ATable.SetColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[1], ACycle.ShiftNums);
    ATable.SetColumnDouble(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursIntToFrac(ACycle.HoursTotal));
    ATable.SetColumnDouble(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursIntToFrac(ACycle.HoursNight));
    ATable.SetColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[4], ACycle.DigMarks);
    ATable.Draw;
  finally
    ATable.Visible:= True;
  end;

end;

procedure ScheduleCycleDraw(const ATable: TVSTTable; const ACycle: TScheduleCycle);
var
  StrDates, StrShiftNums: TStrVector;
begin
  ScheduleCycleDateColumnSet(ATable, ACycle, StrDates);
  StrShiftNums:= VIntToStr(ACycle.ShiftNums);
  VChangeIf(StrShiftNums, '0', EMPTY_MARK);

  ATable.Visible:= False;
  try
    ATable.ValuesClear;
    ATable.SetColumn({SCHEDULE_CORRECTION_COLUMN_NAMES[0]}0, StrDates);
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], StrShiftNums);
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursToStr(ACycle.HoursTotal));
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursToStr(ACycle.HoursNight));
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], ACycle.StrMarks);
    ATable.Draw;
  finally
    ATable.Visible:= True;
  end;
end;

procedure ScheduleShiftByCalendar(const AScheduleID: Integer;
  const ACalendar: TCalendar; var ASchedule: TShiftSchedule);
var
  Cycle: TScheduleCycle;
  V: TIntVector;
  WeekHours: Integer;
  Correct: TScheduleCorrections;
begin
  if not Assigned(ASchedule) then
    ASchedule:= TShiftSchedule.Create;
  WeekHours:= DataBase.ValueInt32Int32ID('SCHEDULEMAIN', 'WeekHours', 'ScheduleID', AScheduleID);
  DataBase.ScheduleCycleLoad(AScheduleID, V, Cycle);
  DataBase.ScheduleShiftCorrectionsLoad(AScheduleID, V, Correct, ACalendar.BeginDate, ACalendar.EndDate);
  ASchedule.Calc(ACalendar, WeekHours, Cycle, Correct);
end;

procedure ScheduleShiftVectorByCalendar(const AScheduleIDs: TIntVector;
  const ACalendar: TCalendar; var ASchedules: TShiftScheduleVector);
var
  i: Integer;
  Schedule: TShiftSchedule;
begin
  VSDel(ASchedules);
  for i:= 0 to High(AScheduleIDs) do
  begin
    Schedule:= TShiftSchedule.Create;
    ScheduleShiftByCalendar(AScheduleIDs[i], ACalendar, Schedule{%H-});
    VSAppend(ASchedules, Schedule);
  end;
end;

procedure ScheduleShiftForPeriod(const AScheduleID: Integer;
  const ABeginDate, AEndDate: TDate; var ASchedule: TShiftSchedule);
var
  Calendar: TCalendar;
begin
  Calendar:= TCalendar.Create;
  try
    CalendarForPeriod(ABeginDate, AEndDate, Calendar);
    ScheduleShiftByCalendar(AScheduleID, Calendar, ASchedule);
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure ScheduleShiftForYear(const AScheduleID: Integer; const AYear: Word;
  var ASchedule: TShiftSchedule);
var
  BD, ED: TDate;
begin
  FirstLastDayInYear(AYear, BD, ED);
  ScheduleShiftForPeriod(AScheduleID, BD, ED, ASchedule);
end;

procedure ScheduleShiftForMonth(const AScheduleID: Integer;
  const AMonth, AYear: Word; var ASchedule: TShiftSchedule);
var
  BD, ED: TDate;
begin
  FirstLastDayInMonth(AMonth, AYear, BD, ED);
  ScheduleShiftForPeriod(AScheduleID, BD, ED, ASchedule);
end;

procedure NormHoursAndWorkDaysCounInPeriod(const ATabNumID: Integer;  //таб номер
                               const ABeginDate, AEndDate: TDate; //период
                               const ACalendar: TCalendar; //производственный календарь, перекрывающий период
                               out AWorkDaysCount, ANormHours: Integer;
                               const ARecrutDate: TDate = NULDATE;
                               const ADismissDate: TDate = INFDATE);
var
  CutCalendar: TCalendar;
  WeekHours, IDs, ScheduleIDs: TIntVector;
  BDs, EDs: TDateVector;
  ScheduleNames: TStrVector;
  i: Integer;
  BD, ED: TDate;
begin
  AWorkDaysCount:= 0;
  ANormHours:= 0;
  //определяем период пересечения запрашиваемого периода и периода работы
  if not IsPeriodIntersect(ABeginDate, AEndDate, ARecrutDate, ADismissDate, BD, ED) then Exit;
  //определяем кол-во дней
  CutCalendar:= nil;
  ACalendar.Cut(BD, ED, CutCalendar);
  AWorkDaysCount:= CutCalendar.WorkDaysCount;
  FreeAndNil(CutCalendar);
  //достаем из базы список рабочих часов в неделю с соответствующими периодами
  if not DataBase.StaffScheduleHistoryLoad(ATabNumID, IDs, ScheduleIDs, WeekHours,
    BDs, EDs, ScheduleNames, ABeginDate, AEndDate) then Exit;
  //суммируем кол-во рабочих часов по вытащенным периодам
  for i:=0 to High(WeekHours) do
  begin
    ACalendar.Cut(BDs[i], EDs[i], CutCalendar);
    ANormHours:= ANormHours + CutCalendar.SumWorkHoursInt(WeekHours[i]);
    FreeAndNil(CutCalendar);
  end;
end;

end.

