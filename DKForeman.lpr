program DKForeman;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, UCalendarForm, lazcontrols, datetimectrls, UDataBase,
  UStaffForm, UScheduleShiftForm, UVacationPlaneForm, USchedulePersonalForm,
  UTimetableForm, USIZForm, USSOForm, UStudyForm, UConst, UTypes,
  UStaffMainEditForm, UUtils, UCalendar, UWorkHours, UStaffTabNumEditForm,
  UStaffPostLogEditForm, UCalendarSheet, USchedule, UDateSheet,
  UScheduleSheet, UCalendarEditForm, UScheduleCorrectionEditForm, 
UChooseForm, UScheduleShiftEditForm, UScheduleShiftMonthForm, 
UScheduleShiftCalendarForm, USchedulePersonalEditForm, UScheduleVacationForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

