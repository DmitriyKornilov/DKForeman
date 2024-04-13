unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BCPanel, BCButton, DateUtils,
  //DK packages utils
  DK_CtrlUtils, DK_Color, DK_Vector, DK_DateUtils, DK_Const,
  DK_VSTEdit, DK_VSTTables, DK_VSTCore,

  //Project utils
  UDataBase, UConst, UWorkHours, UCalendar, USchedule;

  //UI
  procedure SetToolPanels(const AControls: array of TControl);
  procedure SetCaptionPanels(const AControls: array of TBCPanel);
  procedure SetToolButtons(const AControls: array of TControl);
  procedure SetCategoryButtons(const AControls: array of TBCButton);

  //ID for TVSTTable.ReSelect
  function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  //Settings
  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;

  //Calendar load/creation
  procedure CalendarForPeriod(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
  procedure CalendarForYear(const AYear: Word; var ACalendar: TCalendar);
  procedure CalendarForMonth(const AMonth, AYear: Word; var ACalendar: TCalendar);
  //Calendar corrections load
  function GetCalendarCorrections(const ADates: TDateVector;
                         const AStatus, ASwapDay: Integer): TCalendarCorrections;
  function GetCalendarCorrections(const ABeginDate, AEndDate: TDate;
                         const AStatus, ASwapDay: Integer): TCalendarCorrections;
  //Schedule corrections load
  function GetScheduleCorrections(const ADates: TDateVector;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  function GetScheduleCorrections(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  //ScheduleShift load/create/draw
  function ScheduleCycleWeek(const AScheduleID: Integer = 0): TScheduleCycle;
  procedure ScheduleCycleDateColumnSet(const ATable: TVSTCustomSimpleTable; const ACycle: TScheduleCycle; out AStrDates: TStrVector);
  procedure ScheduleCycleDraw(const ATable: TVSTEdit; const ACycle: TScheduleCycle);
  procedure ScheduleCycleDraw(const ATable: TVSTTable; const ACycle: TScheduleCycle);
  procedure ScheduleShiftByCalendar(const AScheduleID: Integer;
                         const ACalendar: TCalendar; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForPeriod(const AScheduleID: Integer;
                         const ABeginDate, AEndDate: TDate; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForYear(const AScheduleID: Integer;
                         const AYear: Word; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForMonth(const AScheduleID: Integer;
                         const AMonth, AYear: Word; var ASchedule: TShiftSchedule);


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

function ScheduleCycleWeek(const AScheduleID: Integer = 0): TScheduleCycle;
var
  i, x: Integer;
  S: String;
begin
  Result.ScheduleID:= AScheduleID;
  Result.IsWeek:= True;
  Result.Count:= 0;
  for i:= 0 to 6 do
    VAppend(Result.Dates, IncDay(MONDAY_DATE, i));

  VDim(Result.HoursTotal, 7, 0);
  x:= 8*WORKHOURS_DENOMINATOR;
  for i:= 0 to 4 do
    Result.HoursTotal[i]:= x;
  VDim(Result.HoursNight, 7, 0);

  VDim(Result.DigMarks, 7, 1);
  Result.DigMarks[5]:= 26 {В};
  Result.DigMarks[6]:= 26 {В};

  S:= DataBase.TimetableStrMarkLoad(1);
  VDim(Result.StrMarks, 7, S);
  S:= DataBase.TimetableStrMarkLoad(26);
  Result.StrMarks[5]:= S;
  Result.StrMarks[6]:= S;

  Result.ShiftNums:= VOrder(7);
  Result.ShiftNums[5]:= 0;
  Result.ShiftNums[6]:= 0;

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

end.

