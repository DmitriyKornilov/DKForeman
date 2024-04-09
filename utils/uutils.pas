unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BCPanel, BCButton,
  //DK packages utils
  DK_CtrlUtils, DK_Color, DK_Vector, DK_VSTTables, DK_DateUtils,
  //Project utils
  UDataBase, UCalendar, USchedule;

  procedure SetToolPanels(const AControls: array of TControl);
  procedure SetCaptionPanels(const AControls: array of TBCPanel);
  procedure SetToolButtons(const AControls: array of TControl);
  procedure SetCategoryButtons(const AControls: array of TBCButton);

  function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;

  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;

  procedure CalendarForPeriod(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
  procedure CalendarForYear(const AYear: Word; var ACalendar: TCalendar);
  procedure CalendarForMonth(const AMonth, AYear: Word; var ACalendar: TCalendar);

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
  h:= Round(TOOL_PANEL_HEIGHT_DEFAULT*0.6);
  c:= cl3DLight;
  //c:= ColorIncLightness(clBtnFace, -15);
  for i:= 0 to High(AControls) do
  begin
    ControlHeight(AControls[i], h);
    AControls[i].Background.Color:= c;
    AControls[i].Border.Color:= clActiveBorder;
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
    //AControls[i].StateHover.Background.Color:= DefaultSelectionBGColor;
    //AControls[i].StateHover.Border.Color:= clHighlight;
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

procedure ScheduleShiftByCalendar(const AScheduleID: Integer;
  const ACalendar: TCalendar; var ASchedule: TShiftSchedule);
var
  Cycle: TScheduleCycle;
  V: TIntVector;
  WeekHours: Integer;
  Correct: TScheduleCorrect;
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

