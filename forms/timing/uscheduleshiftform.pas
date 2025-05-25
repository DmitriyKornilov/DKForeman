unit UScheduleShiftForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, VirtualTrees, Spin, DividerBevel, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UTimingUtils, UImages, UWorkHours, UCalendar,
  USchedule, UScheduleSheet,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_Vector, DK_Const, DK_Dialogs, DK_CtrlUtils,
  DK_Zoom, DK_DateUtils, DK_Color, DK_SheetExporter, DK_Progress, DK_ColorLegend,
  //Forms
  UChooseForm, UScheduleShiftEditForm, UScheduleCorrectionEditForm,
  UScheduleShiftCalendarForm, UScheduleShiftMonthForm;

type

  { TScheduleShiftForm }

  TScheduleShiftForm = class(TForm)
    SheetCaptionPanel: TPanel;
    StructureCaptionPanel: TPanel;
    MonthButton: TSpeedButton;
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    ExportButton: TSpeedButton;
    CalendarButton: TSpeedButton;
    LegendPanel: TPanel;
    CopyCancelButton: TSpeedButton;
    CopyDelButton: TSpeedButton;
    CopyPanel: TPanel;
    CopySaveButton: TSpeedButton;
    CopyToolPanel: TPanel;
    SettingCaptionPanel: TPanel;
    ListCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    MainPanel: TPanel;
    ScheduleAddButton: TSpeedButton;
    DayAddButton: TSpeedButton;
    DayCopyButton: TSpeedButton;
    ScheduleDelButton: TSpeedButton;
    DayDelButton: TSpeedButton;
    ScheduleEditButton: TSpeedButton;
    DayEditButton: TSpeedButton;
    DayPanel: TPanel;
    DayToolPanel: TPanel;
    CorrectionsPanel: TPanel;
    SheetPanel: TPanel;
    ListToolPanel: TPanel;
    LeftSplitter: TSplitter;
    ListPanel: TPanel;
    EditingPanel: TPanel;
    ScheduleListVT: TVirtualStringTree;
    ScheduleListPanel: TPanel;
    SettingPanel: TPanel;
    EditingSplitter: TSplitter;
    Splitter2: TSplitter;
    CorrectionsCaptionPanel: TPanel;
    StructurePanel: TPanel;
    StructureVT: TVirtualStringTree;
    ToolPanel: TPanel;
    DayVT: TVirtualStringTree;
    CopyVT: TVirtualStringTree;
    ViewGrid: TsWorksheetGrid;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    SheetBottomPanel: TPanel;
    ZoomPanel: TPanel;
    procedure CalendarButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CopyCancelButtonClick(Sender: TObject);
    procedure CopyDelButtonClick(Sender: TObject);
    procedure CopySaveButtonClick(Sender: TObject);
    procedure DayAddButtonClick(Sender: TObject);
    procedure DayCopyButtonClick(Sender: TObject);
    procedure DayDelButtonClick(Sender: TObject);
    procedure DayEditButtonClick(Sender: TObject);
    procedure DayVTDblClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MonthButtonClick(Sender: TObject);
    procedure ScheduleAddButtonClick(Sender: TObject);
    procedure ScheduleDelButtonClick(Sender: TObject);
    procedure ScheduleEditButtonClick(Sender: TObject);
    procedure ScheduleListVTDblClick(Sender: TObject);
    procedure ViewGridDblClick(Sender: TObject);
    procedure ViewGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDraw: Boolean;
    ZoomPercent: Integer;
    ModeType: TModeType;

    Colors: TColorVector;

    ParamList: TVSTParamList;

    ScheduleList: TVSTTable;
    Structure: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    ScheduleIDs, WeekHours, CycleCounts: TIntVector;
    ScheduleNames: TStrVector;

    Calendar: TCalendar;
    CorrectIDs: TIntVector;
    Corrections: TScheduleCorrections;
    Cycle: TScheduleCycle;
    Schedule: TShiftSchedule;
    Sheet: TShiftYearScheduleSheet;

    SelectedHoursTotal, SelectedHoursNight, SelectedDigMark, SelectedShiftNum: Integer;
    SelectedStrMark: String;
    IsCopyDates: Boolean;

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);
    function DayInListSelect(const ADate: TDate): Boolean;

    procedure ColorsLoad;

    procedure ParamListCreate;

    procedure EditingTablesCreate;
    procedure CorrectionsLoad(const SelectedID: Integer = -1);
    procedure CorrectionDelete;
    procedure CorrectionSelect;
    procedure CorrectionEdit;
    procedure CopyListLoad(const ASelectedDate: TDate=0);
    procedure CopySelect;

    procedure ScheduleListCreate;
    procedure ScheduleListSelect;
    procedure ScheduleListLoad(const SelectedID: Integer = -1);
    procedure ScheduleListDelItem;
    procedure ScheduleListEditItem;

    procedure CycleLoad;
    procedure ScheduleLoad;
    procedure YearChange;
    procedure ScheduleChange(const ANeedCycleLoad: Boolean;
                             const ANeedCorrectionsLoad: Boolean = True);
    procedure ScheduleToSheet(var ASheet: TShiftYearScheduleSheet;
                              const AWorksheet: TsWorksheet;
                              const AGrid: TsWorksheetGrid;
                              const ACalendar: TCalendar;
                              const ASchedule: TShiftSchedule;
                              const AScheduleName: String = '');
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleExport;

    procedure ScheduleCorrectionEditFormOpen(const ADate: TDate);
    procedure ScheduleShiftEditFormOpen(const AEditingType: TEditingType);

    procedure LegendCreate;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  ScheduleShiftForm: TScheduleShiftForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TScheduleShiftForm }

procedure TScheduleShiftForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TScheduleShiftForm.CalendarButtonClick(Sender: TObject);
begin
  ScheduleShiftCalendarFormShow(YearSpinEdit.Value,
                                  ScheduleIDs[ScheduleList.SelectedIndex],
                                  ScheduleNames[ScheduleList.SelectedIndex]);
end;

procedure TScheduleShiftForm.CopyCancelButtonClick(Sender: TObject);
begin
  CopyEnd(False);
end;

procedure TScheduleShiftForm.CopyDelButtonClick(Sender: TObject);
begin
  Sheet.Unselect(Sheet.SelectedDates[VSTCopy.SelectedIndex]);
  CopyListLoad;
end;

procedure TScheduleShiftForm.CopySaveButtonClick(Sender: TObject);
begin
  CopyEnd(True);
end;

procedure TScheduleShiftForm.DayAddButtonClick(Sender: TObject);
begin
  ScheduleCorrectionEditFormOpen(NULDATE);
end;

procedure TScheduleShiftForm.DayCopyButtonClick(Sender: TObject);
begin
  CopyBegin;
end;

procedure TScheduleShiftForm.DayDelButtonClick(Sender: TObject);
begin
  CorrectionDelete;
end;

procedure TScheduleShiftForm.DayEditButtonClick(Sender: TObject);
begin
  CorrectionEdit;
end;

procedure TScheduleShiftForm.DayVTDblClick(Sender: TObject);
begin
  CorrectionEdit;
end;

procedure TScheduleShiftForm.ExportButtonClick(Sender: TObject);
begin
  ScheduleExport;
end;

procedure TScheduleShiftForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  SetToolPanels([
    ToolPanel, ListToolPanel, DayToolPanel, CopyToolPanel
  ]);
  SetCaptionPanels([
    SettingCaptionPanel, ListCaptionPanel, StructureCaptionPanel, CorrectionsCaptionPanel,
    SheetCaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    ScheduleAddButton, ScheduleDelButton, ScheduleEditButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton, CopyCancelButton
  ]);

  Images.ToButtons([
    ExportButton, CalendarButton, MonthButton,
    CloseButton,
    ScheduleAddButton, ScheduleDelButton, ScheduleEditButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton, CopyCancelButton
  ]);

  Calendar:= TCalendar.Create;
  Schedule:= TShiftSchedule.Create;

  CanDraw:= False;

  ScheduleListCreate;
  ParamListCreate;
  EditingTablesCreate;
  YearSpinEdit.Value:= YearOfDate(Date);
  IsCopyDates:= False;

  ColorsLoad;
  LegendCreate;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  CanDraw:= True;
end;

procedure TScheduleShiftForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(ScheduleList);

  FreeAndNil(Structure);
  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);

  FreeAndNil(Calendar);
  FreeAndNil(Schedule);
  if Assigned(Sheet) then FreeAndNil(Sheet);
end;

procedure TScheduleShiftForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  H:= MainPanel.Height div 3;
  EditingPanel.Height:= 2*H;
  CorrectionsPanel.Height:= H;
end;

procedure TScheduleShiftForm.MonthButtonClick(Sender: TObject);
var
  V: TColorVector;
begin
  V:= nil;
  if ParamList.Checked['ViewParams', 3] then
    V:= Colors;
  ScheduleShiftMonthFormShow(YearSpinEdit.Value,
             ParamList.Selected['CountType'],
             ParamList.Checked['ViewParams', 0],
             ParamList.Checked['ViewParams', 1],
             ParamList.Checked['ViewParams', 2],
             ParamList.Selected['ColorType']=0,
             V);
end;

procedure TScheduleShiftForm.ScheduleAddButtonClick(Sender: TObject);
begin
  ScheduleShiftEditFormOpen(etAdd);
end;

procedure TScheduleShiftForm.ScheduleDelButtonClick(Sender: TObject);
begin
  ScheduleListDelItem;
end;

procedure TScheduleShiftForm.ScheduleEditButtonClick(Sender: TObject);
begin
  ScheduleListEditItem;
end;

procedure TScheduleShiftForm.ScheduleListVTDblClick(Sender: TObject);
begin
  ScheduleListEditItem;
end;

procedure TScheduleShiftForm.ViewGridDblClick(Sender: TObject);
var
  DayDate: TDate;
begin
  if ModeType<>mtEditing then Exit;
  if not Sheet.GridToDate(ViewGrid.Row, ViewGrid.Col, DayDate) then Exit;
  VSTDays.ReSelect(Corrections.Dates, DayDate, False);
  ScheduleCorrectionEditFormOpen(DayDate);
end;

procedure TScheduleShiftForm.ViewGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R, C: Integer;
  D: TDate;
begin
  if ModeType<>mtEditing then Exit;
  if Button=mbLeft then
  begin
    (Sender as TsWorksheetGrid).MouseToCell(X, Y, C, R);
    if not Sheet.GridToDate(R, C, D) then Exit;
    if DayInListSelect(D) then Exit;
    Sheet.DayInGridSelect(D);
    if IsCopyDates then CopyListLoad;
  end
  else if Button=mbRight then
  begin
    if IsCopyDates then
    begin
      VSTCopy.ValuesClear;
      Sheet.SelectionClear;
    end
    else
      VSTDays.UnSelect;
  end;
end;

procedure TScheduleShiftForm.YearSpinEditChange(Sender: TObject);
begin
  YearChange;
end;

procedure TScheduleShiftForm.CopyBegin;
begin
  IsCopyDates:= True;
  Sheet.MultiSelect:= True;
  DayPanel.Visible:= False;
  DayPanel.Align:= alBottom;
  CopyPanel.Align:= alClient;
  CopyPanel.Visible:= True;

  SelectedHoursTotal:= Corrections.HoursTotal[VSTDays.SelectedIndex];
  SelectedHoursNight:= Corrections.HoursNight[VSTDays.SelectedIndex];
  SelectedDigMark:= Corrections.DigMarks[VSTDays.SelectedIndex];
  SelectedShiftNum:= Corrections.ShiftNums[VSTDays.SelectedIndex];
  SelectedStrMark:= Corrections.StrMarks[VSTDays.SelectedIndex];
  VSTDays.Unselect;
end;

procedure TScheduleShiftForm.CopyEnd(const ANeedSave: Boolean);
var
  C: TScheduleCorrections;
begin
  if Sheet.IsSelected then
  begin
    if ANeedSave then //apply copies
    begin
      C:= ScheduleCorrectionsCreate(Sheet.SelectedDates, SelectedHoursTotal, SelectedHoursNight,
                                 SelectedDigMark, SelectedShiftNum, SelectedStrMark);
      DataBase.ScheduleShiftCorrectionsUpdate(ScheduleIDs[ScheduleList.SelectedIndex], C);
      ScheduleChange(False{no cycle reload});
    end
    else //cancel copies
      Sheet.SelectionClear;
    VSTCopy.ValuesClear;
  end;

  IsCopyDates:= False;
  Sheet.MultiSelect:= False;
  CopyPanel.Visible:= False;
  CopyPanel.Align:= alBottom;
  DayPanel.Align:= alClient;
  DayPanel.Visible:= True;
end;

function TScheduleShiftForm.DayInListSelect(const ADate: TDate): Boolean;
var
  Ind: Integer;
begin
  Result:= False;
  if IsCopyDates then Exit;

  Ind:= VIndexOfDate(Corrections.Dates, ADate);
  if Ind>=0 then
    VSTDays.Select(Ind)
  else
    VSTDays.UnSelect;
  Result:= Ind>=0;
end;

procedure TScheduleShiftForm.ColorsLoad;
begin
  Colors:= nil;
  VDim(Colors, SHIFT_COLOR_COUNT);
  Colors[CORRECT_COLOR_INDEX]:= COLOR_SCHEDULE_CORRECTION;
  Colors[NOTWORK_COLOR_INDEX]:= COLOR_SCHEDULE_NOTWORK;
  Colors[TITLE_COLOR_INDEX]:= COLOR_SCHEDULE_TITLE;
  Colors[OUTSIDEMONTH_COLOR_INDEX]:= COLOR_SCHEDULE_OUTSIDEMONTH;
  Colors[HIGHLIGHT_COLOR_INDEX]:= DefaultSelectionBGColor;
end;

procedure TScheduleShiftForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= VIEW_PARAMS_CAPTION;
  V:= VCreateStr([
    'отображать строку ночных часов',
    'учитывать корректировки графика',
    'коды табеля для нерабочих дней',
    'использовать цвета'
  ]);
  ParamList.AddCheckList('ViewParams', S, V, @ScheduleRedraw);

  S:= 'Отображать в итогах количество:';
  V:= VCreateStr([
    'дней',
    'смен',
    'дней и смен'
  ]);
  ParamList.AddStringList('CountType', S, V, @ScheduleRedraw);

  S:= 'Выделять цветом нерабочие дни:';
  V:= VCreateStr([
    'по графику сменности',
    'по производственному календарю'
  ]);
  ParamList.AddStringList('ColorType', S, V, @ScheduleRedraw);
end;

procedure TScheduleShiftForm.ScheduleLoad;
begin
  if ScheduleList.IsSelected then
    Schedule.Calc(Calendar, WeekHours[ScheduleList.SelectedIndex], Cycle, Corrections)
  else
    Schedule.Clear;
end;

procedure TScheduleShiftForm.EditingTablesCreate;
var
  i: Integer;
begin
  Structure:= TVSTTable.Create(StructureVT);
  Structure.SetSingleFont(MainForm.GridFont);
  Structure.HeaderFont.Style:= [fsBold];
  Structure.CanSelect:= False;
  for i:= 0 to High(SCHEDULE_CORRECTION_COLUMN_WIDTHS) do
    Structure.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i],
                        SCHEDULE_CORRECTION_COLUMN_WIDTHS[i]);
  Structure.Draw;

  VSTDays:= TVSTTable.Create(DayVT);
  VSTDays.OnSelect:= @CorrectionSelect;
  VSTDays.OnDelKeyDown:= @CorrectionDelete;
  VSTDays.OnReturnKeyDown:= @CorrectionEdit;
  VSTDays.SetSingleFont(MainForm.GridFont);
  VSTDays.HeaderFont.Style:= [fsBold];
  VSTDays.CanSelect:= True;
  for i:= 0 to High(SCHEDULE_CORRECTION_COLUMN_WIDTHS) do
    VSTDays.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i],
                      SCHEDULE_CORRECTION_COLUMN_WIDTHS[i]);
  VSTDays.Draw;

  VSTCopy:= TVSTTable.Create(CopyVT);
  VSTCopy.OnSelect:= @CopySelect;
  VSTCopy.SetSingleFont(MainForm.GridFont);
  VSTCopy.HeaderFont.Style:= [fsBold];
  VSTCopy.CanSelect:= True;
  for i:= 0 to High(SCHEDULE_CORRECTION_COLUMN_WIDTHS) do
    VSTCopy.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i],
                      SCHEDULE_CORRECTION_COLUMN_WIDTHS[i]);
  VSTCopy.Draw;
end;

procedure TScheduleShiftForm.CycleLoad;
var
  CycleIDs: TIntVector;
begin
  Structure.ValuesClear;
  StructureCaptionPanel.Caption:= '  Структура ';

  if not ScheduleList.IsSelected then Exit;

  StructureCaptionPanel.Caption:= StructureCaptionPanel.Caption + ': ' +
                                  ScheduleNames[ScheduleList.SelectedIndex];

  DataBase.ScheduleCycleLoad(ScheduleIDs[ScheduleList.SelectedIndex], CycleIDs, Cycle);
  ScheduleCycleDraw(Structure, Cycle);
end;

procedure TScheduleShiftForm.CorrectionsLoad(const SelectedID: Integer = -1);
var
  SelectedCorrectionID: Integer;
  Dates, ShiftNums: TStrVector;
  BD, ED: TDate;
begin
  SelectedCorrectionID:= GetSelectedID(VSTDays, CorrectIDs, SelectedID);

  VSTDays.ValuesClear;
  CorrectionsCaptionPanel.Caption:= '  Корректировки';

  if not ScheduleList.IsSelected then Exit;

  CorrectionsCaptionPanel.Caption:= CorrectionsCaptionPanel.Caption +  ': ' +
                                  ScheduleNames[ScheduleList.SelectedIndex];

  FirstLastDayInYear(YearSpinEdit.Value, BD, ED);
  DataBase.ScheduleShiftCorrectionsLoad(ScheduleIDs[ScheduleList.SelectedIndex],
                                        CorrectIDs, Corrections, BD, ED);

  ShiftNums:= VIntToStr(Corrections.ShiftNums);
  VChangeIf(ShiftNums, '0', EMPTY_MARK);
  Dates:= VDateToStr(Corrections.Dates);

  VSTDays.Visible:= False;
  try
    VSTDays.ValuesClear;
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[0], Dates);
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], ShiftNums);
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursToStr(Corrections.HoursTotal));
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursToStr(Corrections.HoursNight));
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], Corrections.StrMarks);
    VSTDays.Draw;
    VSTDays.ReSelect(CorrectIDs, SelectedCorrectionID);
  finally
    VSTDays.Visible:= True;
  end;
end;

procedure TScheduleShiftForm.CorrectionDelete;
var
  Ind: Integer;
begin
  if not VSTDays.IsSelected then Exit;
  Ind:= VSTDays.SelectedIndex;
  DataBase.ScheduleShiftCorrectionDelete(CorrectIDs[Ind]);
  ScheduleChange(False {no cycle load});
  if VIsNil(CorrectIDs) then Exit;
  if Ind>High(CorrectIDs) then Dec(Ind);
  VSTDays.Select(Ind);
  VSTDays.SetFocus;
end;

procedure TScheduleShiftForm.CorrectionSelect;
begin
  if VSTDays.IsSelected then
    Sheet.DayInGridSelect(Corrections.Dates[VSTDays.SelectedIndex])
  else if Assigned(Sheet) then
    Sheet.SelectionClear;

  DayDelButton.Enabled:= VSTDays.IsSelected;
  DayEditButton.Enabled:= DayDelButton.Enabled;
  DayCopyButton.Enabled:= DayDelButton.Enabled;
end;

procedure TScheduleShiftForm.CorrectionEdit;
begin
  if not VSTDays.IsSelected then Exit;
  ScheduleCorrectionEditFormOpen(Corrections.Dates[VSTDays.SelectedIndex]);
end;

procedure TScheduleShiftForm.CopyListLoad(const ASelectedDate: TDate);
var
  Dates, TotalHours, NightHours, StrMarks, ShiftNums: TStrVector;
begin
  Dates:= VDateToStr(Sheet.SelectedDates);
  VDim(TotalHours{%H-}, Length(Dates), WorkHoursToStr(SelectedHoursTotal));
  VDim(NightHours{%H-}, Length(Dates), WorkHoursToStr(SelectedHoursNight));
  VDim(StrMarks{%H-}, Length(Dates), SelectedStrMark);
  VDim(ShiftNums{%H-}, Length(Dates), IntToStr(SelectedShiftNum));
  VChangeIf(ShiftNums, '0', EMPTY_MARK);

  VSTCopy.Visible:= False;
  try
    VSTCopy.ValuesClear;
    VSTCopy.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[0], Dates);
    VSTCopy.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], ShiftNums);
    VSTCopy.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], TotalHours);
    VSTCopy.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], NightHours);
    VSTCopy.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], StrMarks);
    VSTCopy.Draw;
    VSTCopy.ReSelect(Sheet.SelectedDates, ASelectedDate);
  finally
    VSTCopy.Visible:= True;
  end;

  CopySaveButton.Enabled:= Sheet.IsSelected;
end;

procedure TScheduleShiftForm.CopySelect;
begin
  CopyDelButton.Enabled:= VSTCopy.IsSelected;
end;

procedure TScheduleShiftForm.ScheduleListCreate;
begin
  ScheduleList:= TVSTTable.Create(ScheduleListVT);
  ScheduleList.CanSelect:= True;
  ScheduleList.CanUnselect:= False;
  ScheduleList.OnSelect:= @ScheduleListSelect;
  ScheduleList.OnDelKeyDown:= @ScheduleListDelItem;
  ScheduleList.OnReturnKeyDown:= @ScheduleListEditItem;
  ScheduleList.SetSingleFont(MainForm.GridFont);
  ScheduleList.HeaderFont.Style:= [fsBold];

  ScheduleList.AddColumn('№ п/п', 50);
  ScheduleList.AddColumn('Наименование графика', 300);
  ScheduleList.AddColumn('Часов в неделю', 120);
  ScheduleList.AutosizeColumnEnable('Наименование графика');
  ScheduleList.Draw;
end;

procedure TScheduleShiftForm.ScheduleListSelect;
begin
  ScheduleDelButton.Enabled:= ScheduleList.IsSelected;
  ScheduleEditButton.Enabled:= ScheduleList.IsSelected;
  DayAddButton.Enabled:= ScheduleList.IsSelected;
  CalendarButton.Enabled:= ScheduleList.IsSelected;
  ScheduleChange(True{cycle load});
end;

procedure TScheduleShiftForm.YearChange;
begin
  CalendarForYear(YearSpinEdit.Value, Calendar);
  ScheduleChange(False{no cycle reload});
end;

procedure TScheduleShiftForm.ScheduleChange(const ANeedCycleLoad: Boolean;
                             const ANeedCorrectionsLoad: Boolean = True);
begin
  if not CanDraw then Exit;
  if ANeedCycleLoad then CycleLoad;
  if ANeedCorrectionsLoad then CorrectionsLoad;
  ScheduleLoad;
  ScheduleRedraw;
end;

procedure TScheduleShiftForm.ScheduleToSheet(var ASheet: TShiftYearScheduleSheet;
  const AWorksheet: TsWorksheet; const AGrid: TsWorksheetGrid;
  const ACalendar: TCalendar; const ASchedule: TShiftSchedule;
  const AScheduleName: String = '');
begin
  if Assigned(ASheet) then FreeAndNil(ASheet);
  ASheet:= TShiftYearScheduleSheet.Create(AWorksheet, AGrid, MainForm.GridFont,
                                          ParamList.Selected['CountType']);

  if Assigned(AGrid) then
    ASheet.Zoom(ZoomPercent);
  ASheet.Draw(ACalendar, ASchedule, AScheduleName,
              ParamList.Checked['ViewParams', 0],
              ParamList.Checked['ViewParams', 1],
              ParamList.Checked['ViewParams', 2],
              ParamList.Selected['ColorType']=0);
  if ParamList.Checked['ViewParams', 3] then
    ASheet.ColorsUpdate(Colors)
  else
    ASheet.ColorsClear;
end;

procedure TScheduleShiftForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  if not CanDraw then Exit;
  if not Calendar.IsCalculated then Exit;
  if not Schedule.IsCalculated then Exit;

  SheetCaptionPanel.Caption:= '  График сменности на ' + YearSpinEdit.Text + ' год';
  if not ScheduleList.IsSelected then Exit;
  SheetCaptionPanel.Caption:= SheetCaptionPanel.Caption + ': ' +
                              ScheduleNames[ScheduleList.SelectedIndex];

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    ScheduleToSheet(Sheet, ViewGrid.Worksheet, ViewGrid, Calendar, Schedule);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TScheduleShiftForm.ScheduleRedraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TScheduleShiftForm.ScheduleExport;
var
  V: TStrVector;
  S: String;
  ChooseIndex: Integer;

  procedure ExportSingleSchedule;
  var
    Exporter: TSheetsExporter;
    Worksheet: TsWorksheet;
    ExpSheet: TShiftYearScheduleSheet;
  begin
    ExpSheet:= nil;
    Exporter:= TSheetsExporter.Create;
    try
      Worksheet:= Exporter.AddWorksheet(YearSpinEdit.Text);
      ScheduleToSheet(ExpSheet, Worksheet, nil, Calendar, Schedule,
                      ScheduleNames[ScheduleList.SelectedIndex]);
      Exporter.PageSettings(spoLandscape);
      Exporter.Save('Выполнено!', ScheduleNames[ScheduleList.SelectedIndex]);
    finally
      if Assigned(ExpSheet) then FreeAndNil(ExpSheet);
      FreeAndNil(Exporter);
    end;
  end;

  procedure ExportSeveralSchedules;
  var
    Exporter: TBooksExporter;
    Worksheet: TsWorksheet;
    ExpSheet: TShiftYearScheduleSheet;
    TmpSchedule: TShiftSchedule;
    i: Integer;
    Progress: TProgress;
  begin
    ExpSheet:= nil;
    Exporter:= TBooksExporter.Create;
    if not Exporter.BeginExport then
    begin
      FreeAndNil(Exporter);
      Exit;
    end;
    try
      Progress:= TProgress.Create(nil);
      try
        Progress.WriteLine1('Экспорт графика');
        Progress.WriteLine2(EmptyStr);
        Progress.Show;
        TmpSchedule:= TShiftSchedule.Create;
        try
          for i:=0 to High(ScheduleIDs) do
          begin
            Progress.WriteLine2(ScheduleNames[i]);
            ScheduleShiftByCalendar(ScheduleIDs[i], Calendar, TmpSchedule);
            Worksheet:= Exporter.AddWorksheet(YearSpinEdit.Text);
            ScheduleToSheet(ExpSheet, Worksheet, nil, Calendar, TmpSchedule, ScheduleNames[i]);
            Exporter.PageSettings(spoLandscape);
            Exporter.Save(ScheduleNames[i]);
          end;
        finally
          FreeAndNil(TmpSchedule);
        end;
      finally
        FreeAndNil(Progress);
      end;
      Exporter.EndExport('Выполнено!');
    finally
      if Assigned(ExpSheet) then FreeAndNil(ExpSheet);
      FreeAndNil(Exporter);
    end;
  end;

begin
  if not ScheduleList.IsSelected then Exit;

  S:= 'Сохранить в файл:';
  V:= VCreateStr([
    'График "' + ScheduleNames[ScheduleList.SelectedIndex] + '" на ' + YearSpinEdit.Text + ' год',
    'Все графики на ' + YearSpinEdit.Text + ' год'
  ]);
  if not Choose(S, V, ChooseIndex) then Exit;

  case ChooseIndex of
  0: ExportSingleSchedule;
  1: ExportSeveralSchedules;
  end;
end;

procedure TScheduleShiftForm.ScheduleCorrectionEditFormOpen(const ADate: TDate);
var
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;
  Ind: Integer;
begin
  if not ScheduleList.IsSelected then Exit;

  ScheduleCorrectionEditForm:= TScheduleCorrectionEditForm.Create(nil);
  try
    if CycleCounts[ScheduleList.SelectedIndex]>0 then
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.MaxValue:= CycleCounts[ScheduleList.SelectedIndex]
    else
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.MaxValue:= 7; //недельный график
    ScheduleCorrectionEditForm.Year:= YearSpinEdit.Value;
    ScheduleCorrectionEditForm.ScheduleID:= ScheduleIDs[ScheduleList.SelectedIndex];
    ScheduleCorrectionEditForm.FirstDatePicker.Date:= ADate;
    Ind:= VIndexOfDate(Corrections.Dates, ADate);
    if Ind>=0 then
    begin
      ScheduleCorrectionEditForm.DigMark:= Corrections.DigMarks[VSTDays.SelectedIndex];
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursTotal[VSTDays.SelectedIndex]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursNight[VSTDays.SelectedIndex]);;
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Corrections.ShiftNums[VSTDays.SelectedIndex];
    end;
    if ScheduleCorrectionEditForm.ShowModal=mrOK then
      ScheduleChange(False{no cycle reload});
  finally
    FreeAndNil(ScheduleCorrectionEditForm);
  end;
end;

procedure TScheduleShiftForm.ScheduleShiftEditFormOpen(const AEditingType: TEditingType);
var
  ScheduleShiftEditForm: TScheduleShiftEditForm;
begin
  ScheduleShiftEditForm:= TScheduleShiftEditForm.Create(nil);
  try
    if AEditingType=etEdit then
    begin
      ScheduleShiftEditForm.Cycle:= Cycle;
      ScheduleShiftEditForm.FirstDate:= VFirst(Cycle.Dates);
      ScheduleShiftEditForm.NameEdit.Text:= ScheduleNames[ScheduleList.SelectedIndex];
      ScheduleShiftEditForm.WeekHoursSpinEdit.Value:= WeekHours[ScheduleList.SelectedIndex];
    end
    else
      ScheduleShiftEditForm.FirstDate:= Date;
    if ScheduleShiftEditForm.ShowModal=mrOK then
    begin
      Cycle:= ScheduleShiftEditForm.Cycle;
      if AEditingType=etAdd then
        ScheduleListLoad(Cycle.ScheduleID)
      else
        ScheduleChange(True{cycle reload}, False {co corrections reload});
    end;
  finally
    FreeAndNil(ScheduleShiftEditForm);
  end;
end;

procedure TScheduleShiftForm.LegendCreate;
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

procedure TScheduleShiftForm.ScheduleListLoad(const SelectedID: Integer = -1);
var
  SelectedScheduleID: Integer;
begin
  SelectedScheduleID:= GetSelectedID(ScheduleList, ScheduleIDs, SelectedID);

  DataBase.ScheduleMainListLoad(ScheduleIDs, WeekHours, CycleCounts, ScheduleNames);

  ScheduleList.Visible:= False;
  try
    ScheduleList.ValuesClear;
    ScheduleList.SetColumn('№ п/п', VIntToStr(VOrder(Length(ScheduleIDs))));
    ScheduleList.SetColumn('Наименование графика', ScheduleNames, taLeftJustify);
    ScheduleList.SetColumn('Часов в неделю', VIntToStr(WeekHours));
    ScheduleList.Draw;
    ScheduleList.ReSelect(ScheduleIDs, SelectedScheduleID, True);  //возвращаем выделение строки
  finally
    ScheduleList.Visible:= True;
  end;
end;

procedure TScheduleShiftForm.ScheduleListDelItem;
begin
  if not ScheduleList.IsSelected then Exit;
  if not Confirm('Удалить график  "' +
                 ScheduleNames[ScheduleList.SelectedIndex] +
                 '"?') then Exit;
  DataBase.ScheduleShiftDelete(ScheduleIDs[ScheduleList.SelectedIndex]);
  ScheduleListLoad;
end;

procedure TScheduleShiftForm.ScheduleListEditItem;
begin
  if not ScheduleList.IsSelected then Exit;
  ScheduleShiftEditFormOpen(etEdit);
end;

procedure TScheduleShiftForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SCHEDULESHIFTFORM);
  ZoomPercent:= SettingValues[0];
  ParamList.Params:= VCut(SettingValues, 1);
end;

procedure TScheduleShiftForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= nil;
  VAppend(SettingValues, ZoomPercent);
  SettingValues:= VAdd(SettingValues, ParamList.Params);
  DataBase.SettingsUpdate(SETTING_NAMES_SCHEDULESHIFTFORM, SettingValues);
end;

procedure TScheduleShiftForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
  SettingPanel.Visible:= False;
  ListPanel.Visible:= False;
  try
    ModeType:= AModeType;
    if IsCopyDates then CopyEnd(False);

    ListToolPanel.Visible:= ModeType=mtEditing;
    VSTDays.CanSelect:= ModeType=mtEditing;
    if ModeType=mtEditing then
    begin
      EditingPanel.Visible:= True;
      EditingSplitter.Visible:= True;
    end
    else begin
      if VSTDays.IsSelected then
        VSTDays.UnSelect;
      EditingSplitter.Visible:= False;
      EditingPanel.Visible:= False;
    end;

    LeftSplitter.Align:= alRight;
    if ModeType=mtSetting then
      SettingPanel.Visible:= True
    else
      ListPanel.Visible:= True;
    LeftSplitter.Align:= alLeft;

    ScheduleListLoad;
  finally
    MainPanel.Visible:= True;
  end;
end;

end.

