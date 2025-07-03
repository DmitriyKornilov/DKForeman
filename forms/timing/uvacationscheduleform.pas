unit UVacationScheduleForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Spin, DividerBevel, fpspreadsheetgrid,
  //DK packages utils
  DK_Vector, DK_Zoom, DK_CtrlUtils, DK_SheetTypes,
  //Project utils
  UVars, UConst, UScheduleSheet;

type

  { TVacationScheduleForm }

  TVacationScheduleForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    ExportButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TVacationScheduleSheet;
    StaffNames, TabNums, PostNames: TStrVector;
    FirstDates: TDateVector;
    TotalCounts: TIntVector;

    procedure ScheduleChange;
    procedure ScheduleDraw(const AZoomPercent: Integer);

    procedure SettingsLoad;
    procedure SettingsSave;
  public

  end;

var
  VacationScheduleForm: TVacationScheduleForm;

  procedure VacationScheduleFormShow(const AYear: Word);

implementation

{$R *.lfm}

procedure VacationScheduleFormShow(const AYear: Word);
var
  Form: TVacationScheduleForm;
begin
  Form:= TVacationScheduleForm.Create(nil);
  try
    Form.YearSpinEdit.Value:= AYear;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{ TVacationScheduleForm }

procedure TVacationScheduleForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[3];

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  Sheet:= TVacationScheduleSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
end;

procedure TVacationScheduleForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;
  FreeAndNil(Sheet);
end;

procedure TVacationScheduleForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    CloseButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton
  ]);

  ScheduleChange;
end;

procedure TVacationScheduleForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TVacationScheduleForm.ExportButtonClick(Sender: TObject);
begin
  SheetFromGridSave(Sheet, ZoomPercent, @ScheduleDraw,
                    YearSpinEdit.Text, 'Выполнено!', True);
end;

procedure TVacationScheduleForm.YearSpinEditChange(Sender: TObject);
begin
  ScheduleChange;
end;

procedure TVacationScheduleForm.ScheduleChange;
begin
  DataBase.VacationScheduleLoad(YearSpinEdit.Value,
                 StaffNames, TabNums, PostNames, FirstDates, TotalCounts);
  ScheduleDraw(ZoomPercent);
end;

procedure TVacationScheduleForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(YearSpinEdit.Value, StaffNames, TabNums, PostNames, FirstDates, TotalCounts);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TVacationScheduleForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_VACATIONSCHEDULEFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TVacationScheduleForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_VACATIONSCHEDULEFORM, SettingValues);
end;

end.

