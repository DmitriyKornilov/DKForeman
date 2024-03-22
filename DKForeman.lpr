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
  UStaffForm, UShiftScheduleForm, UVacationPlaneForm, UPersonalScheduleForm,
  UTimetableForm, USIZForm, USSOForm, UStudyForm, UConst, UTypes,
  UStaffMainEditForm, UUtils, UCalendar, UWorkHours, UStaffTabNumEditForm,
  UStaffPostLogEditForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

