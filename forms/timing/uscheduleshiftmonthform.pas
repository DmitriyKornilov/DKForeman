unit UScheduleShiftMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  Buttons, Spin, VirtualTrees, fpspreadsheetgrid,

  //DK packages utils
  DK_Vector, DK_StrUtils, DK_Fonts, DK_Const, DK_VSTDropDown, DK_DateUtils,
  DK_VSTTableTools, DK_Zoom,
  //Project utils
  UDataBase, UUtils, UCalendar, USchedule, UScheduleShiftSheet;

type

  { TScheduleShiftMonthForm }

  TScheduleShiftMonthForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    CloseButton: TSpeedButton;
    ExportButton: TBCButton;
    LeftSplitter: TSplitter;
    MonthBCButton: TBCButton;
    LeftPanel: TPanel;
    CheckAllButton: TSpeedButton;
    UncheckAllButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    VT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDrawSchedule: Boolean;
    ZoomPercent: Integer;
    MonthDropDown: TVSTDropDown;

    Calendar: TCalendar;
    Sheet: TShiftScheduleMonthSheet;

    ScheduleList: TVSTCheckList;
    ScheduleIDs, WeekHours, CycleCounts: TIntVector;
    ScheduleNames: TStrVector;
    Schedules: TShiftScheduleVector;

    procedure ScheduleLoad;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRefresh;

  public
    ResumeType: Integer;
    NeedNight, NeedCorrect, NeedMarks, ScheduleNotWorkColor: Boolean;
    Colors: TColorVector;
  end;

var
  ScheduleShiftMonthForm: TScheduleShiftMonthForm;

  procedure ScheduleShiftMonthFormCreate(const AYear, AResumeType: Integer;
         const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean;
         const AColors: TColorVector);

implementation

uses UMainForm;

{$R *.lfm}

procedure ScheduleShiftMonthFormCreate(const AYear, AResumeType: Integer;
         const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean;
         const AColors: TColorVector);
var
  Frm: TScheduleShiftMonthForm;
begin
  Frm:= TScheduleShiftMonthForm.Create(nil);
  try
    Frm.ResumeType:= AResumeType;
    Frm.NeedNight:= ANeedNight;
    Frm.NeedCorrect:= ANeedCorrect;
    Frm.NeedMarks:= ANeedMarks;
    Frm.ScheduleNotWorkColor:= AScheduleNotWorkColor;
    Frm.YearSpinEdit.Value:= AYear;
    Frm.Colors:= AColors;
    Frm.ShowModal;
  finally
    FreeAndNil(Frm);
  end;
end;

{ TScheduleShiftMonthForm }

procedure TScheduleShiftMonthForm.FormCreate(Sender: TObject);
begin
  CanDrawSchedule:= False;
  Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[11];
  Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CloseButton
  ]);

  SetCategoryButtons([
    ExportButton
  ]);

  MonthDropDown:= TVSTDropDown.Create(MonthBCButton);
  MonthDropDown.OnChange:= @ScheduleRefresh;
  MonthDropDown.Items:= VCreateStr(MONTHSNOM);
  MonthDropDown.ItemIndex:= MonthOfDate(Date) - 1;
  MonthDropDown.DropDownCount:= 12;


  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  Calendar:= TCalendar.Create;
  Sheet:= TShiftScheduleMonthSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont, ResumeType);

  DataBase.ScheduleMainListLoad(ScheduleIDs, WeekHours, CycleCounts, ScheduleNames);
  ScheduleList:= TVSTCheckList.Create(VT, EmptyStr, ScheduleNames, @ScheduleRefresh);

  CanDrawSchedule:= True;
end;

procedure TScheduleShiftMonthForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Calendar);
  FreeAndNil(MonthDropDown);
  FreeAndNil(ScheduleList);
  FreeAndNil(Sheet);
  VSDel(Schedules);
end;

procedure TScheduleShiftMonthForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TScheduleShiftMonthForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TScheduleShiftMonthForm.CheckAllButtonClick(Sender: TObject);
begin
  ScheduleList.CheckAll(True);
end;

procedure TScheduleShiftMonthForm.FormShow(Sender: TObject);
begin
  MonthBCButton.Width:= GetBCButtonWidth(MonthBCButton, MONTHSNOM[9]);
  ScheduleRefresh;
end;

procedure TScheduleShiftMonthForm.UncheckAllButtonClick(Sender: TObject);
begin
  ScheduleList.CheckAll(False);
end;

procedure TScheduleShiftMonthForm.YearSpinEditChange(Sender: TObject);
begin
  //CalendarForYear(YearSpinEdit.Value, Calendar);
  ScheduleRefresh;
end;

procedure TScheduleShiftMonthForm.ScheduleLoad;
var
  i, M, Y: Integer;
  Schedule: TShiftSchedule;
begin
  M:= MonthDropDown.ItemIndex+1;
  Y:= YearSpinEdit.Value;
  CalendarForMonth(M, Y, Calendar);
  VSDel(Schedules);
  for i:= 0 to High(ScheduleIDs) do
  begin
    Schedule:= TShiftSchedule.Create;
    ScheduleShiftByCalendar(ScheduleIDs[i], Calendar, Schedule{%H-});
    VSAppend(Schedules, Schedule);
  end;
end;

procedure TScheduleShiftMonthForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  ZoomPercent:= AZoomPercent;
  Sheet.Zoom(ZoomPercent);
  Sheet.Draw(Calendar, Schedules, ScheduleNames,
             NeedNight, NeedCorrect, NeedMarks, ScheduleNotWorkColor,
             ScheduleList.Selected);
  if not VIsNil(Colors) then
    Sheet.ColorsUpdate(Colors)
end;

procedure TScheduleShiftMonthForm.ScheduleRefresh;
begin
  if not CanDrawSchedule then Exit;
  ScheduleLoad;
  ScheduleDraw(ZoomPercent);
end;

end.

