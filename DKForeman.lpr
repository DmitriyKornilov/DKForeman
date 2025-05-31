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
  UStaffForm, UScheduleShiftForm, UVacationPlanForm, USchedulePersonalForm,
  UTimetableForm, USIZForm, USSOForm, UStudyForm, UConst, UTypes,
  UStaffMainEditForm, UUtils, UCalendar, UWorkHours, UStaffTabNumEditForm,
  UStaffPostLogEditForm, UCalendarSheet, UTimingSheet, UScheduleSheet,
  UTimetable, UCalendarEditForm, UScheduleCorrectionEditForm,
  UScheduleShiftEditForm, UScheduleShiftMonthForm, UScheduleShiftCalendarForm,
  USchedulePersonalEditForm, UVacationScheduleForm, USchedulePersonalMonthForm,
  UTimetableEditForm, UTimetableMonthForm, UChooseForm, UImages,
  UTimetableSheet, uschedule, UTimingUtils, UVacationPlanEditForm,
  UVacationPlanningForm, USIZNormForm, USIZNormSheet, USIZSizes, USIZTypes,
  USIZUtils, USIZNormEditForm, USIZNormItemEditForm, USIZNormSubItemEditForm,
  USearchForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.

