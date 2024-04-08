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
  UStaffPostLogEditForm, UCalendarSheet, USchedule, UTimingSheet,
  UScheduleShiftSheet, UCalendarEditForm, UScheduleCorrectionEditForm, 
UChooseForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCalendarEditForm, CalendarEditForm);
  Application.CreateForm(TScheduleCorrectionEditForm, ScheduleCorrectionEditForm
    );
  Application.CreateForm(TChooseForm, ChooseForm);
  Application.Run;
end.

