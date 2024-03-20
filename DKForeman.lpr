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
  Forms, UMainForm, UCalendarForm, lazcontrols, datetimectrls, UDBUtils,
  UStaffForm, UShiftScheduleForm, UVacationPlaneForm, UPersonalScheduleForm,
  UTimetableForm, USIZForm, USSOForm, UStudyForm, UConst, UTypes,
  UStaffMainEditForm, UUtils, UStaffTabNumEditForm, UStaffPostLogEditForm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TStaffPostlogEditForm, StaffPostlogEditForm);
  Application.Run;
end.

