unit UScheduleShiftMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  Buttons, Spin, VirtualTrees, fpspreadsheetgrid,

  //DK packages utils
  DK_Vector, DK_Fonts, DK_Const, DK_VSTDropDown, DK_DateUtils, DK_StrUtils,
  DK_VSTTableTools, DK_Zoom, DK_SheetExporter,
  //Project utils
  UDataBase, UUtils, UCalendar, USchedule, UScheduleShiftSheet,
  //Forms
  UChooseForm;

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
    procedure ExportButtonClick(Sender: TObject);
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

    procedure CalendarAndSchedulesUpdate(const AMonth, AYear: Word;
                                         var ACalendar: TCalendar;
                                         var ASchedules: TShiftScheduleVector);

    procedure ScheduleLoad;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleReDraw;
    procedure ScheduleRefresh;

    procedure ScheduleExport;

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
    Frm.Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[11];
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
  ScheduleList:= TVSTCheckList.Create(VT, EmptyStr, ScheduleNames, @ScheduleReDraw);
  ScheduleList.StopSelectEventWhileCheckAll:= True;

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

procedure TScheduleShiftMonthForm.ExportButtonClick(Sender: TObject);
begin
  ScheduleExport;
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
  ScheduleRefresh;
end;

procedure TScheduleShiftMonthForm.CalendarAndSchedulesUpdate(const AMonth, AYear: Word;
         var ACalendar: TCalendar;
         var ASchedules: TShiftScheduleVector);
var
  i: Integer;
  Schedule: TShiftSchedule;
begin
  CalendarForMonth(AMonth, AYear, ACalendar);
  VSDel(ASchedules);
  for i:= 0 to High(ScheduleIDs) do
  begin
    Schedule:= TShiftSchedule.Create;
    ScheduleShiftByCalendar(ScheduleIDs[i], ACalendar, Schedule{%H-});
    VSAppend(ASchedules, Schedule);
  end;
end;

procedure TScheduleShiftMonthForm.ScheduleLoad;
var
  M, Y: Integer;
begin
  if not CanDrawSchedule then Exit;
  M:= MonthDropDown.ItemIndex+1;
  Y:= YearSpinEdit.Value;
  CalendarAndSchedulesUpdate(M, Y, Calendar, Schedules);
end;

procedure TScheduleShiftMonthForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  if not CanDrawSchedule then Exit;
  ZoomPercent:= AZoomPercent;
  Sheet.Zoom(ZoomPercent);
  Sheet.Draw(Calendar, Schedules, ScheduleNames,
             NeedNight, NeedCorrect, NeedMarks, ScheduleNotWorkColor,
             ScheduleList.Selected);
  if not VIsNil(Colors) then
    Sheet.ColorsUpdate(Colors);
end;

procedure TScheduleShiftMonthForm.ScheduleReDraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TScheduleShiftMonthForm.ScheduleRefresh;
begin
  ScheduleLoad;
  ScheduleReDraw;
end;

procedure TScheduleShiftMonthForm.ScheduleExport;
var
  Exporter: TSheetsExporter;
  i: Integer;
  V: TStrVector;
  S: String;

  procedure ScheduleToSheet(const AWorksheet: TsWorksheet;
                            const ACalendar: TCalendar;
                            const ASchedules: TShiftScheduleVector);
  var
    ExpSheet: TShiftScheduleMonthSheet;
  begin
    ExpSheet:= TShiftScheduleMonthSheet.Create(AWorksheet, nil, MainForm.GridFont, ResumeType);
    try
      ExpSheet.Draw(ACalendar, ASchedules, ScheduleNames,
             NeedNight, NeedCorrect, NeedMarks, ScheduleNotWorkColor,
             ScheduleList.Selected);
      if not VIsNil(Colors) then
      ExpSheet.ColorsUpdate(Colors)
    finally
      FreeAndNil(ExpSheet);
    end;
  end;

  procedure ExportMonth(const AMonth, AYear: Word);
  var
    ExpCalendar: TCalendar;
    ExpSchedules: TShiftScheduleVector;
    Worksheet: TsWorksheet;
  begin
    ExpCalendar:= nil;
    ExpSchedules:= nil;
    CalendarAndSchedulesUpdate(AMonth, AYear, ExpCalendar, ExpSchedules);
    try
      Worksheet:= Exporter.AddWorksheet(SFirstUpper(MONTHSNOM[AMonth]) + ' ' + IntToStr(AYear));
      ScheduleToSheet(Worksheet, ExpCalendar, ExpSchedules);
      Exporter.PageSettings(spoLandscape);
    finally
      FreeAndNil(ExpCalendar);
      VSDel(ExpSchedules);
    end;
  end;

begin
  S:= 'Сохранить в файл:';
  V:= VCreateStr([
    'Сводный график на ' + MonthDropDown.Text + ' ' + YearSpinEdit.Text + ' года',
    'Сводные графики на все месяцы ' + YearSpinEdit.Text + ' года'
  ]);
  i:= Choose(S, V);
  if i=0 then Exit;

  Exporter:= TSheetsExporter.Create;
  try
    if i=1 then //график на выбранный месяц
      ExportMonth(MonthDropDown.ItemIndex+1, YearSpinEdit.Value)
    else begin //график на все месяцы
      for i:= 1 to 12 do
        ExportMonth(i, YearSpinEdit.Value);
    end;
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

end.

