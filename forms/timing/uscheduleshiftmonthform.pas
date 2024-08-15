unit UScheduleShiftMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  Buttons, Spin, DividerBevel, VirtualTrees, fpspreadsheetgrid,

  //DK packages utils
  DK_Vector, DK_Fonts, DK_Const, DK_StrUtils, DK_VSTTableTools, DK_Zoom,
  DK_SheetExporter, DK_ColorLegend,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UUIUtils, UCalendar, USchedule, UScheduleSheet,
  //Forms
  UChooseForm;

type

  { TScheduleShiftMonthForm }

  TScheduleShiftMonthForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    ExportButton: TBCButton;
    LeftSplitter: TSplitter;
    LegendPanel: TPanel;
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
    SheetBottomPanel: TPanel;
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
    CanDraw: Boolean;
    ZoomPercent: Integer;
    MonthDropDown: TMonthDropDown;

    Calendar: TCalendar;
    Sheet: TShiftMonthScheduleSheet;

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

    procedure LegendCreate;

    procedure SettingsLoad;
    procedure SettingsSave;
  public
    ResumeType: Integer;
    NeedNight, NeedCorrect, NeedMarks, ScheduleNotWorkColor: Boolean;
    Colors: TColorVector;
  end;

var
  ScheduleShiftMonthForm: TScheduleShiftMonthForm;

  procedure ScheduleShiftMonthFormShow(const AYear, AResumeType: Integer;
         const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean;
         const AColors: TColorVector);

implementation

uses UMainForm;

{$R *.lfm}

procedure ScheduleShiftMonthFormShow(const AYear, AResumeType: Integer;
         const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean;
         const AColors: TColorVector);
var
  Form: TScheduleShiftMonthForm;
begin
  Form:= TScheduleShiftMonthForm.Create(nil);
  try
    Form.ResumeType:= AResumeType;
    Form.NeedNight:= ANeedNight;
    Form.NeedCorrect:= ANeedCorrect;
    Form.NeedMarks:= ANeedMarks;
    Form.ScheduleNotWorkColor:= AScheduleNotWorkColor;
    Form.YearSpinEdit.Value:= AYear;
    Form.Colors:= AColors;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{ TScheduleShiftMonthForm }

procedure TScheduleShiftMonthForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[11];

  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    CloseButton, CheckAllButton, UncheckAllButton
  ]);
  SetCategoryButtons([
    ExportButton
  ]);

  CanDraw:= False;

  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, @ScheduleRefresh);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);
  LegendCreate;

  Calendar:= TCalendar.Create;
  Sheet:= TShiftMonthScheduleSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont, ResumeType);

  DataBase.ScheduleMainListLoad(ScheduleIDs, WeekHours, CycleCounts, ScheduleNames);
  ScheduleList:= TVSTCheckList.Create(VT, EmptyStr, ScheduleNames, @ScheduleReDraw);
  ScheduleList.StopSelectEventWhileCheckAll:= True;

  CanDraw:= True;
end;

procedure TScheduleShiftMonthForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;
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
  MonthDropDown.AutoWidth;
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
begin
  CalendarForMonth(AMonth, AYear, ACalendar);
  ScheduleShiftVectorByCalendar(ScheduleIDs, ACalendar, ASchedules);
end;

procedure TScheduleShiftMonthForm.ScheduleLoad;
begin
  if not CanDraw then Exit;
  CalendarAndSchedulesUpdate(MonthDropDown.Month, YearSpinEdit.Value, Calendar, Schedules);
end;

procedure TScheduleShiftMonthForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  if not CanDraw then Exit;

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);

    Sheet.Draw(Calendar, Schedules, ScheduleNames,
               NeedNight, NeedCorrect, NeedMarks, ScheduleNotWorkColor,
               ScheduleList.Selected);
    if not VIsNil(Colors) then
      Sheet.ColorsUpdate(Colors);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
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
    ExpSheet: TShiftMonthScheduleSheet;
  begin
    ExpSheet:= TShiftMonthScheduleSheet.Create(AWorksheet, nil, MainForm.GridFont, ResumeType);
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
  if not Choose(S, V, i) then Exit;

  Exporter:= TSheetsExporter.Create;
  try
    if i=0 then //график на выбранный месяц
      ExportMonth(MonthDropDown.Month, YearSpinEdit.Value)
    else begin //график на все месяцы
      for i:= 1 to 12 do
        ExportMonth(i, YearSpinEdit.Value);
    end;
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TScheduleShiftMonthForm.LegendCreate;
var
  C: TColorVector;
  S: TStrVector;
begin
  C:= VCreateColor([
    COLOR_SCHEDULE_NOTWORK,
    COLOR_SCHEDULE_CORRECTION
  ]);
  S:= VCreateStr([
    '- нерабочий день',
    '- корректировка'
  ]);

  ColorLegendCreate(LegendPanel, C, S);
end;

procedure TScheduleShiftMonthForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SCHEDULESHIFTMONTHFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TScheduleShiftMonthForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SCHEDULESHIFTMONTHFORM, SettingValues);
end;

end.

