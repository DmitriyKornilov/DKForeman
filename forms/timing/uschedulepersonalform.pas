unit USchedulePersonalForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, VirtualTrees, Spin, StdCtrls, DividerBevel, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UTimingUtils, UImages, UWorkHours, UCalendar,
  USchedule, UScheduleSheet,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_VSTEdit, DK_Vector, DK_Const, DK_Dialogs,
  DK_DateUtils, DK_Color, DK_SheetExporter, DK_StrUtils, DK_CtrlUtils,
  DK_Zoom, DK_Progress, DK_Filter, DK_ColorLegend,
  //Forms
  UChooseForm, USchedulePersonalEditForm, UScheduleCorrectionEditForm,
  UVacationScheduleForm, USchedulePersonalMonthForm;

type

  { TSchedulePersonalForm }

  TSchedulePersonalForm = class(TForm)
    AscendingButton: TSpeedButton;
    CloseButton: TSpeedButton;
    CopyCancelButton: TSpeedButton;
    DescendingButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    ExportButton: TSpeedButton;
    StaffCaptionPanel: TPanel;
    ViewCaptionPanel: TPanel;
    VacationCaptionPanel: TPanel;
    SettingCaptionPanel: TPanel;
    HistoryCaptionPanel: TPanel;
    CorrectionsCaptionPanel: TPanel;
    VacationScheduleButton: TSpeedButton;
    MonthScheduleButton: TSpeedButton;
    FilterPanel: TPanel;
    LegendPanel: TPanel;
    ListCaptionPanel: TPanel;
    OrderButtonPanel: TPanel;
    OrderLabel: TLabel;
    ListFilterToolPanel: TPanel;
    ListOrderToolPanel: TPanel;
    FIORadioButton: TRadioButton;
    PostRadioButton: TRadioButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    TabNumRadioButton: TRadioButton;
    VacationEraseButton: TSpeedButton;
    VacationCancelButton: TSpeedButton;
    CopyDelButton: TSpeedButton;
    VacationDelButton: TSpeedButton;
    CopyPanel: TPanel;
    CopySaveButton: TSpeedButton;
    VacationSaveButton: TSpeedButton;
    CopyToolPanel: TPanel;
    VacationToolPanel: TPanel;
    CopyVT: TVirtualStringTree;
    CorrectionsPanel: TPanel;
    DayAddButton: TSpeedButton;
    DayCopyButton: TSpeedButton;
    DayDelButton: TSpeedButton;
    HistoryDelButton: TSpeedButton;
    DayEditButton: TSpeedButton;
    HistoryEditButton: TSpeedButton;
    DayPanel: TPanel;
    DayToolPanel: TPanel;
    HistoryAddButton: TSpeedButton;
    DayVT: TVirtualStringTree;
    EditingPanel: TPanel;
    LeftSplitter: TSplitter;
    BottomEditingPanel: TPanel;
    HistoryPanel: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    ViewPanel: TPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    ViewGridPanel: TPanel;
    ListPanel: TPanel;
    StaffListVT: TVirtualStringTree;
    ToolPanel: TPanel;
    VacationPanel: TPanel;
    HistoryToolPanel: TPanel;
    HistoryVT: TVirtualStringTree;
    ViewGrid: TsWorksheetGrid;
    VacationVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    SheetBottomPanel: TPanel;
    ZoomPanel: TPanel;
    procedure CopyCancelButtonClick(Sender: TObject);
    procedure CopyDelButtonClick(Sender: TObject);
    procedure CopySaveButtonClick(Sender: TObject);
    procedure DayAddButtonClick(Sender: TObject);
    procedure DayCopyButtonClick(Sender: TObject);
    procedure DayDelButtonClick(Sender: TObject);
    procedure DayEditButtonClick(Sender: TObject);
    procedure DayVTDblClick(Sender: TObject);
    procedure DescendingButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure AscendingButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure HistoryAddButtonClick(Sender: TObject);
    procedure HistoryDelButtonClick(Sender: TObject);
    procedure HistoryEditButtonClick(Sender: TObject);
    procedure HistoryVTDblClick(Sender: TObject);
    procedure MonthScheduleButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
    procedure VacationCancelButtonClick(Sender: TObject);
    procedure VacationDelButtonClick(Sender: TObject);
    procedure VacationEraseButtonClick(Sender: TObject);
    procedure VacationSaveButtonClick(Sender: TObject);
    procedure VacationScheduleButtonClick(Sender: TObject);
    procedure ViewGridDblClick(Sender: TObject);
    procedure ViewGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDraw: Boolean;
    ZoomPercent: Integer;
    FilterString: String;
    ModeType: TModeType;

    Calendar: TCalendar;
    Schedule: TPersonalSchedule;
    Sheet: TPersonalYearScheduleSheet;

    CorrectIDs: TIntVector;
    Corrections: TScheduleCorrections;

    Colors: TColorVector;

    ViewYear, ViewTabNumID: Integer;

    ParamList: TVSTParamList;

    StaffList: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    VacationEdit: TVSTEdit;
    VacationDates: TDateVector;
    VacationCounts, VacationAddCounts: TIntVector;
    TmpVacationDates: TDateVector;
    TmpVacationCounts, TmpVacationAddCounts: TIntVector;

    History: TVSTTable;
    HistoryIDs, HistoryScheduleIDs, HistoryWeekHours: TIntVector;
    HistoryBeginDates, HistoryEndDates: TDateVector;
    HistoryScheduleNames: TStrVector;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames, ScheduleNames: TStrVector;
    RecrutDates, DismissDates, Holidays: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    SelectedHoursTotal, SelectedHoursNight, SelectedDigMark, SelectedShiftNum: Integer;
    SelectedStrMark: String;
    IsCopyDates: Boolean;

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);
    function DayInListSelect(const ADate: TDate): Boolean;

    procedure ParamListCreate;

    procedure EditingTablesCreate;
    procedure CorrectionsLoad(const SelectedID: Integer = -1);
    procedure CorrectionDelete;
    procedure CorrectionSelect;
    procedure CorrectionEdit;
    procedure CopyListLoad(const ASelectedDate: TDate = 0);
    procedure CopySelect;

    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListCreate;
    procedure StaffListSelect;
    procedure StaffListLoad(const SelectedID: Integer = -1);

    procedure ScheduleLoad;
    procedure ScheduleUpdate;
    procedure ScheduleChange;
    procedure ScheduleToSheet(var ASheet: TPersonalYearScheduleSheet;
                              const AWorksheet: TsWorksheet;
                              const AGrid: TsWorksheetGrid;
                              const ACalendar: TCalendar;
                              const ASchedule: TPersonalSchedule;
                              const AScheduleName: String = '');
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleExport;

    procedure HistoryCreate;
    procedure HistoryLoad(const SelectedID: Integer = -1);
    procedure HistorySelect;
    procedure HistoryDelItem;
    procedure HistoryEditItem;

    procedure VacationEditCreate;
    procedure VacationEditLoad;
    procedure VacationEditSetColumns(const ADates: TDateVector; const ACounts, AAddCounts: TIntVector);
    procedure VacationEditGetColumns(out ADates: TDateVector; out ACounts, AAddCounts: TIntVector);
    procedure VacationEditSelect;
    procedure VacationEditingBegin;
    procedure VacationEditDelete;

    procedure SchedulePersonalMonthFormOpen;
    procedure ScheduleCorrectionEditFormOpen(const ADate: TDate);
    procedure SchedulePersonalEditFormOpen(const AEditingType: TEditingType);

    procedure ColorsLoad;
    procedure LegendCreate;
    procedure CaptionsUpdate;
    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);

  end;

var
  SchedulePersonalForm: TSchedulePersonalForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSchedulePersonalForm }

procedure TSchedulePersonalForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSchedulePersonalForm.DescendingButtonClick(Sender: TObject);
begin
  DescendingButton.Visible:= False;
  AscendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TSchedulePersonalForm.DayAddButtonClick(Sender: TObject);
begin
  ScheduleCorrectionEditFormOpen(NULDATE);
end;

procedure TSchedulePersonalForm.CopySaveButtonClick(Sender: TObject);
begin
  CopyEnd(True);
end;

procedure TSchedulePersonalForm.CopyDelButtonClick(Sender: TObject);
begin
  Sheet.Unselect(Sheet.SelectedDates[VSTCopy.SelectedIndex]);
  CopyListLoad;
end;

procedure TSchedulePersonalForm.CopyCancelButtonClick(Sender: TObject);
begin
  CopyEnd(False);
end;

procedure TSchedulePersonalForm.DayCopyButtonClick(Sender: TObject);
begin
  CopyBegin;
end;

procedure TSchedulePersonalForm.DayDelButtonClick(Sender: TObject);
begin
  CorrectionDelete;
end;

procedure TSchedulePersonalForm.DayEditButtonClick(Sender: TObject);
begin
  CorrectionEdit;
end;

procedure TSchedulePersonalForm.DayVTDblClick(Sender: TObject);
begin
  CorrectionEdit;
end;

procedure TSchedulePersonalForm.AscendingButtonClick(Sender: TObject);
begin
  AscendingButton.Visible:= False;
  DescendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TSchedulePersonalForm.ExportButtonClick(Sender: TObject);
begin
  ScheduleExport;
end;

procedure TSchedulePersonalForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  SetToolPanels([
    ToolPanel, ListFilterToolPanel, ListOrderToolPanel, HistoryToolPanel,
    VacationToolPanel, DayToolPanel, CopyToolPanel
  ]);
  SetCaptionPanels([
    StaffCaptionPanel, SettingCaptionPanel, ListCaptionPanel, HistoryCaptionPanel,
    VacationCaptionPanel, CorrectionsCaptionPanel, ViewCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton,
    HistoryAddButton, HistoryDelButton, HistoryEditButton,
    VacationSaveButton, VacationDelButton, VacationEraseButton, VacationCancelButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton,CopyCancelButton
  ]);


  Images.ToButtons([
    ExportButton, MonthScheduleButton, VacationScheduleButton,
    CloseButton, AscendingButton, DescendingButton,
    HistoryAddButton, HistoryDelButton, HistoryEditButton,
    VacationSaveButton, VacationDelButton, VacationEraseButton, VacationCancelButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton, CopyCancelButton
  ]);

  Calendar:= TCalendar.Create;

  ViewYear:= 0;
  ViewTabNumID:= 0;

  CanDraw:= False;

  VacationEditCreate;
  HistoryCreate;
  StaffListCreate;
  ParamListCreate;
  EditingTablesCreate;
  YearSpinEdit.Value:= YearOfDate(Date);

  IsCopyDates:= False;

  ColorsLoad;
  LegendCreate;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter, 300);

  CanDraw:= True;
end;

procedure TSchedulePersonalForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);

  FreeAndNil(VacationEdit);
  FreeAndNil(StaffList);
  FreeAndNil(History);

  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);

  FreeAndNil(Calendar);
  if Assigned(Schedule) then FreeAndNil(Schedule);
  if Assigned(Sheet) then FreeAndNil(Sheet);
end;

procedure TSchedulePersonalForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  H:= Round(0.6*(ClientHeight - ToolPanel.Height - StaffCaptionPanel.Height));
  BottomEditingPanel.Height:= H;
  H:= VacationCaptionPanel.Height + VacationToolPanel.Height + VacationEdit.TotalHeight + 10;
  VacationPanel.Height:= H;
  StaffListLoad;
end;

procedure TSchedulePersonalForm.FIORadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSchedulePersonalForm.HistoryAddButtonClick(Sender: TObject);
begin
  SchedulePersonalEditFormOpen(etAdd);
end;

procedure TSchedulePersonalForm.HistoryDelButtonClick(Sender: TObject);
begin
  HistoryDelItem;
end;

procedure TSchedulePersonalForm.HistoryEditButtonClick(Sender: TObject);
begin
  HistoryEditItem;
end;

procedure TSchedulePersonalForm.HistoryVTDblClick(Sender: TObject);
begin
  HistoryEditItem;
end;

procedure TSchedulePersonalForm.SchedulePersonalMonthFormOpen;
var
  SchedulePersonalMonthForm: TSchedulePersonalMonthForm;
begin
  SchedulePersonalMonthForm:= TSchedulePersonalMonthForm.Create(nil);
  try
    SchedulePersonalMonthForm.YearSpinEdit.Value:= YearSpinEdit.Value;
    SchedulePersonalMonthForm.ShowModal;
    ScheduleUpdate;
  finally
    FreeAndNil(SchedulePersonalMonthForm);
  end;
end;

procedure TSchedulePersonalForm.MonthScheduleButtonClick(Sender: TObject);
begin
  SchedulePersonalMonthFormOpen;
end;

procedure TSchedulePersonalForm.PostRadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSchedulePersonalForm.TabNumRadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSchedulePersonalForm.VacationCancelButtonClick(Sender: TObject);
begin
  VacationEdit.UnSelect;
  VacationEditSetColumns(VacationDates, VacationCounts, VacationAddCounts);
  VacationSaveButton.Enabled:= False;
  VacationCancelButton.Enabled:= False;
end;

procedure TSchedulePersonalForm.VacationDelButtonClick(Sender: TObject);
var
  R, C: Integer;
begin
  if not VacationEdit.IsSelected then Exit;
  C:= VacationEdit.SelectedColIndex;
  R:= VacationEdit.SelectedRowIndex;

  VacationEdit.UnSelect;
  VacationEditGetColumns(TmpVacationDates, TmpVacationCounts, TmpVacationAddCounts);
  case C of
  1: TmpVacationDates[R]:= 0;
  2: TmpVacationCounts[R]:= 0;
  3: TmpVacationAddCounts[R]:= 0;
  end;
  VacationEditSetColumns(TmpVacationDates, TmpVacationCounts, TmpVacationAddCounts);
  VacationEdit.Select(R, C);
  VacationSaveButton.Enabled:= True;
  VacationCancelButton.Enabled:= True;
end;

procedure TSchedulePersonalForm.VacationEraseButtonClick(Sender: TObject);
begin
  VacationEdit.UnSelect;
  VDim(TmpVacationDates{%H-}, 4);
  VDim(TmpVacationCounts{%H-}, 4);
  VDim(TmpVacationAddCounts{%H-}, 4);
  VacationEditSetColumns(TmpVacationDates, TmpVacationCounts, TmpVacationAddCounts);
  VacationCancelButton.Enabled:= True;
end;

procedure TSchedulePersonalForm.VacationSaveButtonClick(Sender: TObject);
var
  VDates: TDateVector;
  VCounts, VAddCounts: TIntVector;
begin
  VacationEdit.UnSelect;
  VacationEditGetColumns(VDates, VCounts, VAddCounts);

  if VDates[0]=0 then
  begin
    Inform('Не указана дата начала планируемого отпуска (1 часть)!');
    Exit;
  end;
  if VCounts[0]=0 then
  begin
    Inform('Не указано количество дней планируемого основного отпуска (1 часть)!');
    Exit;
  end;

  if not ((VDates[1]=0) and (VCounts[1]=0)) then
  begin
    if VDates[1]=0 then
    begin
      Inform('Не указана дата начала фактического отпуска (1 часть)!');
      Exit;
    end
    else if VCounts[1]=0 then
    begin
      Inform('Не указано количество дней фактического основного отпуска (1 часть)!');
      Exit;
    end;
  end;

  if not ((VDates[2]=0) and (VCounts[2]=0)) then
  begin
    if VDates[2]=0 then
    begin
      Inform('Не указана дата начала планируемого отпуска (2 часть)!');
      Exit;
    end
    else if VCounts[2]=0 then
    begin
      Inform('Не указано количество дней планируемого основного отпуска (2 часть)!');
      Exit;
    end;
  end;

  if not ((VDates[3]=0) and (VCounts[3]=0)) then
  begin
    if VDates[3]=0 then
    begin
      Inform('Не указана дата начала фактического отпуска (2 часть)!');
      Exit;
    end
    else if VCounts[3]=0 then
    begin
      Inform('Не указано количество дней фактического основного отпуска (2 часть)!');
      Exit;
    end;
  end;

  VacationEditGetColumns(VacationDates, VacationCounts, VacationAddCounts);

  if VacationDates[1]=0 then
    VacationDates[1]:= VacationDates[0];
  if VacationCounts[1]=0 then
    VacationCounts[1]:= VacationCounts[0];

  DataBase.VacationsEditingUpdate(TabNumIDs[StaffList.SelectedIndex], YearSpinEdit.Value,
                                VacationDates, VacationCounts, VacationAddCounts);
  VacationEditLoad;
  ScheduleUpdate;
end;

procedure TSchedulePersonalForm.VacationScheduleButtonClick(Sender: TObject);
begin
  VacationScheduleFormShow(YearSpinEdit.Value);
end;

procedure TSchedulePersonalForm.ViewGridDblClick(Sender: TObject);
var
  DayDate: TDate;
begin
  if ModeType<>mtEditing then Exit;
  if not Sheet.GridToDate(ViewGrid.Row, ViewGrid.Col, DayDate) then Exit;
  ScheduleCorrectionEditFormOpen(DayDate);
end;

procedure TSchedulePersonalForm.ViewGridMouseDown(Sender: TObject;
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
    if not Schedule.IsDateExists(D) then Exit;
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

procedure TSchedulePersonalForm.YearSpinEditChange(Sender: TObject);
var
  SelectedTabNumID: Integer;
  BD, ED: TDate;
begin
  CalendarForYear(YearSpinEdit.Value, Calendar);
  Holidays:= DataBase.HolidaysLoad(YearSpinEdit.Value);

  SelectedTabNumID:= -1;
  if StaffList.IsSelected then
    SelectedTabNumID:= TabNumIDs[StaffList.SelectedIndex];
  StaffListLoad(SelectedTabNumID);

  FirstLastDayInYear(YearSpinEdit.Value, BD, ED);
  VacationEdit.SetColumnMinMaxDate(VACATION_EDIT_COLUMN_NAMES[1], BD, ED);
end;

procedure TSchedulePersonalForm.CopyBegin;
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

procedure TSchedulePersonalForm.CopyEnd(const ANeedSave: Boolean);
var
  C: TScheduleCorrections;
begin
  if Sheet.IsSelected then
  begin
    if ANeedSave then //apply copies
    begin
      C:= ScheduleCorrectionsCreate(Sheet.SelectedDates, SelectedHoursTotal, SelectedHoursNight,
                                 SelectedDigMark, SelectedShiftNum, SelectedStrMark);
      DataBase.SchedulePersonalCorrectionsUpdate(TabNumIDs[StaffList.SelectedIndex], C);
      ScheduleUpdate;
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

function TSchedulePersonalForm.DayInListSelect(const ADate: TDate): Boolean;
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

procedure TSchedulePersonalForm.ParamListCreate;
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
    'учитывать отпуск',
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

procedure TSchedulePersonalForm.EditingTablesCreate;
var
  i: Integer;
begin
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

procedure TSchedulePersonalForm.CorrectionsLoad(const SelectedID: Integer = -1);
var
  SelectedCorrectionID: Integer;
  Dates, ShiftNums: TStrVector;
  BD, ED: TDate;
begin
  SelectedCorrectionID:= GetSelectedID(VSTDays, CorrectIDs, SelectedID);

  VSTDays.ValuesClear;

  if not StaffList.IsSelected then Exit;

  FirstLastDayInYear(YearSpinEdit.Value, BD, ED);
  DataBase.SchedulePersonalCorrectionsLoad(TabNumIDs[StaffList.SelectedIndex],
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

procedure TSchedulePersonalForm.CorrectionDelete;
var
  Ind: Integer;
begin
  if not VSTDays.IsSelected then Exit;
  Ind:= VSTDays.SelectedIndex;
  DataBase.SchedulePersonalCorrectionDelete(CorrectIDs[Ind]);
  ScheduleUpdate;
  if VIsNil(CorrectIDs) then Exit;
  if Ind>High(CorrectIDs) then Dec(Ind);
  VSTDays.Select(Ind);
  VSTDays.SetFocus;
end;

procedure TSchedulePersonalForm.CorrectionSelect;
begin
  if VSTDays.IsSelected then
    Sheet.DayInGridSelect(Corrections.Dates[VSTDays.SelectedIndex])
  else if Assigned(Sheet) then
    Sheet.SelectionClear;

  DayDelButton.Enabled:= VSTDays.IsSelected;
  DayEditButton.Enabled:= DayDelButton.Enabled;
  DayCopyButton.Enabled:= DayDelButton.Enabled;
end;

procedure TSchedulePersonalForm.CorrectionEdit;
begin
  if not VSTDays.IsSelected then Exit;
  ScheduleCorrectionEditFormOpen(Corrections.Dates[VSTDays.SelectedIndex]);
end;

procedure TSchedulePersonalForm.CopyListLoad(const ASelectedDate: TDate = 0);
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

procedure TSchedulePersonalForm.CopySelect;
begin
  CopyDelButton.Enabled:= VSTCopy.IsSelected;
end;

procedure TSchedulePersonalForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TSchedulePersonalForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffListVT);
  StaffList.CanSelect:= True;
  StaffList.CanUnselect:= False;
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(MainForm.GridFont);
  StaffList.HeaderFont.Style:= [fsBold];

  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Сотрудник', 300);
  StaffList.AutosizeColumnEnable('Сотрудник');
  StaffList.Draw;
end;

procedure TSchedulePersonalForm.StaffListSelect;
begin
  DayAddButton.Enabled:= StaffList.IsSelected;
  ScheduleChange;
end;

procedure TSchedulePersonalForm.StaffListLoad(const SelectedID: Integer = -1);
var
  SelectedTabNumID: Integer;
  BD, ED: TDate;
  OrderType: Byte;
  IsDescOrder: Boolean;
begin
  SelectedTabNumID:= GetSelectedID(StaffList, TabNumIDs, SelectedID);
  FirstLastDayInYear(YearSpinEdit.Value, BD, ED);

  if FIORadioButton.Checked then
    OrderType:= 0
  else if TabNumRadioButton.Checked then
    OrderType:= 1
  else if PostRadioButton.Checked then
    OrderType:= 2;

  IsDescOrder:= not DescendingButton.Visible;

  DataBase.StaffListForPersonalTimingLoad(STrimLeft(FilterString),
                       BD, ED, OrderType, IsDescOrder,
                       TabNumIDs, RecrutDates, DismissDates,
                       Families, Names, Patronymics, TabNums, PostNames);
  StaffLongNames:= StaffNamesForPersonalTiming(Families, Names, Patronymics, TabNums, PostNames, True);
  StaffShortNames:= StaffNamesForPersonalTiming(Families, Names, Patronymics, TabNums, PostNames, False);
  ScheduleNames:= StaffNamesForScheduleNames(Families, Names, Patronymics, TabNums, True);

  StaffList.Visible:= False;
  try
    StaffList.ValuesClear;
    StaffList.SetColumn('№ п/п', VIntToStr(VOrder(Length(TabNumIDs))));
    StaffList.SetColumn('Сотрудник', StaffShortNames, taLeftJustify);
    StaffList.Draw;
    StaffList.ReSelect(TabNumIDs, SelectedTabNumID, True);  //возвращаем выделение строки
  finally
    StaffList.Visible:= True;
  end;
end;

procedure TSchedulePersonalForm.ScheduleLoad;
begin
  if Assigned(Schedule) then FreeAndNil(Schedule);
  if not StaffList.IsSelected then Exit;

  Schedule:= SchedulePersonalByCalendar(TabNumIDs[StaffList.SelectedIndex],
    TabNums[StaffList.SelectedIndex],
    RecrutDates[StaffList.SelectedIndex], DismissDates[StaffList.SelectedIndex],
    Calendar, Holidays, False);
end;

procedure TSchedulePersonalForm.ScheduleUpdate;
begin
  CorrectionsLoad;
  ScheduleLoad;
  ScheduleRedraw;
end;

procedure TSchedulePersonalForm.ScheduleChange;
begin
  if not CanDraw then Exit;
  if not StaffList.IsSelected then Exit;

  if (TabNumIDs[StaffList.SelectedIndex]=ViewTabNumID) and
     (YearSpinEdit.Value = ViewYear) then Exit;
  ViewYear:= YearSpinEdit.Value;
  ViewTabNumID:= TabNumIDs[StaffList.SelectedIndex];

  CaptionsUpdate;
  HistoryLoad;
  VacationEditLoad;
  ScheduleUpdate;
end;

procedure TSchedulePersonalForm.ScheduleToSheet(var ASheet: TPersonalYearScheduleSheet;
                              const AWorksheet: TsWorksheet;
                              const AGrid: TsWorksheetGrid;
                              const ACalendar: TCalendar;
                              const ASchedule: TPersonalSchedule;
                              const AScheduleName: String = '');
begin
  if Assigned(ASheet) then FreeAndNil(ASheet);
  ASheet:= TPersonalYearScheduleSheet.Create(AWorksheet, AGrid, MainForm.GridFont,
                                           ParamList.Selected['CountType']);

  if Assigned(AGrid) then
    ASheet.Zoom(ZoomPercent);
  ASheet.Draw(ACalendar, ASchedule, AScheduleName,
              ParamList.Checked['ViewParams', 0],
              ParamList.Checked['ViewParams', 1],
              ParamList.Checked['ViewParams', 2],
              ParamList.Checked['ViewParams', 3],
              ParamList.Selected['ColorType']=0);
  if ParamList.Checked['ViewParams', 4] then
    ASheet.ColorsUpdate(Colors);
end;

procedure TSchedulePersonalForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  if not Calendar.IsCalculated then Exit;
  if (not Assigned(Schedule)) or (not Schedule.IsCalculated) then Exit;

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

procedure TSchedulePersonalForm.ScheduleRedraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TSchedulePersonalForm.ScheduleExport;
var
  V: TStrVector;
  S: String;
  ChooseIndex: Integer;

  procedure ExportSingleSchedule;
  var
    Exporter: TSheetsExporter;
    Worksheet: TsWorksheet;
    ExpSheet: TPersonalYearScheduleSheet;
  begin
    ExpSheet:= nil;
    Exporter:= TSheetsExporter.Create;
    try
      Worksheet:= Exporter.AddWorksheet(YearSpinEdit.Text);
      ScheduleToSheet(ExpSheet, Worksheet, nil, Calendar, Schedule,
                      ScheduleNames[StaffList.SelectedIndex]);
      Exporter.PageSettings(spoLandscape);
      Exporter.Save('Выполнено!', ScheduleNames[StaffList.SelectedIndex]);
    finally
      if Assigned(ExpSheet) then FreeAndNil(ExpSheet);
      FreeAndNil(Exporter);
    end;
  end;

  procedure ExportSeveralSchedules;
  var
    Exporter: TBooksExporter;
    Worksheet: TsWorksheet;
    ExpSheet: TPersonalYearScheduleSheet;
    TmpSchedule: TPersonalSchedule;
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
        for i:=0 to High(TabNumIDs) do
        begin
          Progress.WriteLine2(ScheduleNames[i]);
          TmpSchedule:= SchedulePersonalByCalendar(TabNumIDs[i], TabNums[i],
                 RecrutDates[i], DismissDates[i], Calendar, Holidays, False);
          try
            Worksheet:= Exporter.AddWorksheet(YearSpinEdit.Text);
            ScheduleToSheet(ExpSheet, Worksheet, nil, Calendar, TmpSchedule, ScheduleNames[i]);
            Exporter.PageSettings(spoLandscape);
            Exporter.Save(ScheduleNames[i]);
          finally
            FreeAndNil(TmpSchedule);
          end;
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
  if not StaffList.IsSelected then Exit;
  if not Calendar.IsCalculated then Exit;

  S:= 'Сохранить в файл:';
  V:= VCreateStr([
    'График работы на ' + YearSpinEdit.Text + ' год: ' + ScheduleNames[StaffList.SelectedIndex],
    'Графики работы всех сотрудников на ' + YearSpinEdit.Text + ' год'
  ]);
  if not Choose(S, V, ChooseIndex) then Exit;

  case ChooseIndex of
  0: ExportSingleSchedule;
  1: ExportSeveralSchedules;
  end;
end;

procedure TSchedulePersonalForm.HistoryCreate;
begin
  History:= TVSTTable.Create(HistoryVT);
  History.CanSelect:= True;
  History.CanUnselect:= False;
  History.OnSelect:= @HistorySelect;
  History.OnDelKeyDown:= @HistoryDelItem;
  History.OnReturnKeyDown:= @HistoryEditItem;
  History.SetSingleFont(MainForm.GridFont);
  History.HeaderFont.Style:= [fsBold];

  History.AddColumn('Дата начала', 100);
  History.AddColumn('Дата окончания', 100);
  History.AddColumn('График', 300);
  History.AutosizeColumnEnable('График');
  History.Draw;
end;

procedure TSchedulePersonalForm.HistoryLoad(const SelectedID: Integer = -1);
var
  SelectedHistoryID: Integer;
  StrBeginDates, StrEndDates: TStrVector;
begin
  History.ValuesClear;
  History.Draw;
  if not StaffList.IsSelected then Exit;

  SelectedHistoryID:= GetSelectedID(History, HistoryIDs, SelectedID);

  DataBase.StaffScheduleHistoryLoad(TabNumIDs[StaffList.SelectedIndex],
    HistoryIDs, HistoryScheduleIDs, HistoryWeekHours,
    HistoryBeginDates, HistoryEndDates, HistoryScheduleNames);

  StrBeginDates:= VFormatDateTime('dd.mm.yyyy', HistoryBeginDates);
  StrEndDates:= VFormatDateTime('dd.mm.yyyy', HistoryEndDates, True);
  VChangeIf(StrEndDates, EmptyStr, EMPTY_MARK);

  History.Visible:= False;
  try
    History.ValuesClear;
    History.SetColumn('Дата начала', StrBeginDates);
    History.SetColumn('Дата окончания', StrEndDates);
    History.SetColumn('График', HistoryScheduleNames, taLeftJustify);
    History.Draw;
    History.ReSelect(HistoryIDs, SelectedHistoryID, True);  //возвращаем выделение строки
  finally
    History.Visible:= True;
  end;
end;

procedure TSchedulePersonalForm.HistorySelect;
var
  IsOK: Boolean;
begin
  IsOK:= History.IsSelected;
  HistoryAddButton.Enabled:= IsOK and (History.SelectedIndex=0 {последняя запись});
  HistoryDelButton.Enabled:= IsOK and (History.SelectedIndex<High(HistoryIDs) {не самая первая должность});
  HistoryEditButton.Enabled:= IsOK;
end;

procedure TSchedulePersonalForm.HistoryDelItem;
begin
  if not HistoryDelButton.Enabled then Exit;
  if not Confirm('Удалить информацию о выбранном периоде работы в графике?') then Exit;
  if not DataBase.StaffScheduleHistoryDelete(HistoryIDs[History.SelectedIndex + 1],
                              HistoryIDs[History.SelectedIndex],
                              HistoryEndDates[History.SelectedIndex]) then Exit;

  HistoryLoad;
  ScheduleUpdate;
end;

procedure TSchedulePersonalForm.HistoryEditItem;
begin
  if not History.IsSelected then Exit;
  SchedulePersonalEditFormOpen(etEdit);
end;

procedure TSchedulePersonalForm.VacationEditCreate;
begin
  VacationEdit:= TVSTEdit.Create(VacationVT);
  VacationEdit.SetSingleFont(MainForm.GridFont);
  VacationEdit.OnEdititingBegin:= @VacationEditingBegin;
  VacationEdit.OnSelect:= @VacationEditSelect;
  VacationEdit.OnDeleteCellText:= @VacationEditDelete;

  VacationEdit.HeaderFont.Style:= [fsBold];
  VacationEdit.AddColumnRowTitles(VACATION_EDIT_COLUMN_NAMES[0],
                               VACATION_EDIT_COLUMN_WIDTHS[0]);

  VacationEdit.AddColumnDate(VACATION_EDIT_COLUMN_NAMES[1],
                             'dd.mm.yyyy',
                             VACATION_EDIT_COLUMN_WIDTHS[1]);
  VacationEdit.AddColumnInteger(VACATION_EDIT_COLUMN_NAMES[2], 0, 366,
                             VACATION_EDIT_COLUMN_WIDTHS[2]);
  VacationEdit.AddColumnInteger(VACATION_EDIT_COLUMN_NAMES[3], 0, 366,
                             VACATION_EDIT_COLUMN_WIDTHS[3]);
  VacationEdit.AutosizeColumnRowTitlesEnable;
  VacationEdit.SetColumnRowTitles(VACATION_EDIT_ROW_NAMES, taLeftJustify);
  VacationEdit.Draw;
end;

procedure TSchedulePersonalForm.VacationEditLoad;
begin
  VacationSaveButton.Enabled:= False;
  VacationCancelButton.Enabled:= False;
  VacationEdit.ValuesClear;
  VacationEdit.Draw;
  if not StaffList.IsSelected then Exit;

  DataBase.VacationsEditingLoad(TabNumIDs[StaffList.SelectedIndex], YearSpinEdit.Value,
                                VacationDates, VacationCounts, VacationAddCounts);
  VacationEditSetColumns(VacationDates, VacationCounts, VacationAddCounts);
end;

procedure TSchedulePersonalForm.VacationEditSetColumns(const ADates: TDateVector;
  const ACounts, AAddCounts: TIntVector);
begin
  VacationEdit.SetColumnRowTitles(VACATION_EDIT_ROW_NAMES, taLeftJustify);
  VacationEdit.SetColumnDate(VACATION_EDIT_COLUMN_NAMES[1], ADates);
  VacationEdit.SetColumnInteger(VACATION_EDIT_COLUMN_NAMES[2], ACounts);
  VacationEdit.SetColumnInteger(VACATION_EDIT_COLUMN_NAMES[3], AAddCounts);
  VacationEdit.Draw;
end;

procedure TSchedulePersonalForm.VacationEditGetColumns(out ADates: TDateVector;
  out ACounts, AAddCounts: TIntVector);
begin
  VacationEdit.ColumnAsDate(ADates, VACATION_EDIT_COLUMN_NAMES[1]);
  VacationEdit.ColumnAsInteger(ACounts, VACATION_EDIT_COLUMN_NAMES[2]);
  VacationEdit.ColumnAsInteger(AAddCounts, VACATION_EDIT_COLUMN_NAMES[3]);
end;

procedure TSchedulePersonalForm.VacationEditSelect;
begin
  VacationDelButton.Enabled:= VacationEdit.IsSelected;
end;

procedure TSchedulePersonalForm.VacationEditingBegin;
begin
  VacationCancelButton.Enabled:= True;
  VacationSaveButton.Enabled:= True;
end;

procedure TSchedulePersonalForm.VacationEditDelete;
begin
  VacationSaveButton.Enabled:= True;
  VacationCancelButton.Enabled:= True;
end;

procedure TSchedulePersonalForm.ScheduleCorrectionEditFormOpen(const ADate: TDate);
var
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;
  Ind: Integer;
begin
  if not StaffList.IsSelected then Exit;

  ScheduleCorrectionEditForm:= TScheduleCorrectionEditForm.Create(nil);
  try
    ScheduleCorrectionEditForm.Year:= YearSpinEdit.Value;
    ScheduleCorrectionEditForm.TabNumID:= TabNumIDs[StaffList.SelectedIndex];
    ScheduleCorrectionEditForm.FirstDatePicker.Date:= ADate;
    Ind:= VIndexOfDate(Corrections.Dates, ADate);
    if Ind>=0 then
    begin
      ScheduleCorrectionEditForm.DigMark:= Corrections.DigMarks[VSTDays.SelectedIndex];
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursTotal[VSTDays.SelectedIndex]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursNight[VSTDays.SelectedIndex]);
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Corrections.ShiftNums[VSTDays.SelectedIndex];
    end;
    if ScheduleCorrectionEditForm.ShowModal=mrOK then
      ScheduleUpdate;
  finally
    FreeAndNil(ScheduleCorrectionEditForm);
  end;
end;

procedure TSchedulePersonalForm.SchedulePersonalEditFormOpen(const AEditingType: TEditingType);
var
  SchedulePersonalEditForm: TSchedulePersonalEditForm;
  ThisBeginDate, PrevBeginDate, NextBeginDate, D: TDate;
begin
  SchedulePersonalEditForm:= TSchedulePersonalEditForm.Create(nil);
  try
    ThisBeginDate:= HistoryBeginDates[History.SelectedIndex];
    SchedulePersonalEditForm.EditingType:= AEditingType;
    SchedulePersonalEditForm.TabNumID:= TabNumIDs[StaffList.SelectedIndex];
    SchedulePersonalEditForm.ScheduleID:= HistoryScheduleIDs[History.SelectedIndex];
    SchedulePersonalEditForm.HistoryID:= HistoryIDs[History.SelectedIndex];
    case AEditingType of
      etAdd: //перевод с последнего графика
        begin
          D:= IncDay(ThisBeginDate, 1);
          SchedulePersonalEditForm.FirstDatePicker.Date:= MaxDate(Date, D);
          SchedulePersonalEditForm.FirstDatePicker.MinDate:= D;
          SchedulePersonalEditForm.FirstDatePicker.MaxDate:= IncDay(INFDATE, -1);
        end;
      etEdit: //редактирование графика
        begin
          SchedulePersonalEditForm.FirstDatePicker.Date:= ThisBeginDate;
          if SameDate(ThisBeginDate, RecrutDates[StaffList.SelectedIndex]) then
          begin //первая запись
            SchedulePersonalEditForm.FirstDatePicker.Enabled:= False;//нельзя менять дату начала работы (приема)
          end
          else begin //последующие записи
            PrevBeginDate:= HistoryBeginDates[History.SelectedIndex + 1];
            SchedulePersonalEditForm.PrevHistoryID:= HistoryIDs[History.SelectedIndex + 1];
            SchedulePersonalEditForm.FirstDatePicker.MinDate:= IncDay(PrevBeginDate, 1);
            if History.SelectedIndex=0 then //последняя запись
              NextBeginDate:= IncDay(INFDATE, 1)
            else //промежуточная запись
              NextBeginDate:= IncDay(HistoryBeginDates[History.SelectedIndex - 1]);
            SchedulePersonalEditForm.FirstDatePicker.MaxDate:= IncDay(NextBeginDate, -1);
          end;
        end;
    end;
    if SchedulePersonalEditForm.ShowModal=mrOK then
    begin
      HistoryLoad(SchedulePersonalEditForm.HistoryID);
      ScheduleUpdate;
    end;
  finally
    FreeAndNil(SchedulePersonalEditForm);
  end;
end;

procedure TSchedulePersonalForm.ColorsLoad;
begin
  Colors:= nil;
  VDim(Colors, PERSONAL_COLOR_COUNT);
  Colors[CORRECT_COLOR_INDEX]:= COLOR_SCHEDULE_CORRECTION;
  Colors[NOTWORK_COLOR_INDEX]:= COLOR_SCHEDULE_NOTWORK;
  Colors[TITLE_COLOR_INDEX]:= COLOR_SCHEDULE_TITLE;
  Colors[OUTSIDEMONTH_COLOR_INDEX]:= COLOR_SCHEDULE_OUTSIDEMONTH;
  Colors[HIGHLIGHT_COLOR_INDEX]:= DefaultSelectionBGColor;
  Colors[NOTDEFINE_COLOR_INDEX]:= COLOR_SCHEDULE_NOTDEFINE;
end;

procedure TSchedulePersonalForm.LegendCreate;
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

procedure TSchedulePersonalForm.CaptionsUpdate;
begin
  StaffCaptionPanel.Caption:= EmptyStr;
  ViewCaptionPanel.Caption:= '  График ';
  CorrectionsCaptionPanel.Caption:= '  Корректировки графика на ' + YearSpinEdit.Text + ' год';
  VacationCaptionPanel.Caption:= '  График отпусков на ' + YearSpinEdit.Text + ' год';

  if not StaffList.IsSelected then Exit;

  StaffCaptionPanel.Caption:= StaffLongNames[StaffList.SelectedIndex];
  StaffCaptionPanel.Visible:= ModeType=mtEditing;

  ViewCaptionPanel.Caption:= '  График работы на ' + YearSpinEdit.Text + ' год';
  if ModeType<>mtEditing then
    ViewCaptionPanel.Caption:= ViewCaptionPanel.Caption + ': ' +
                                StaffLongNames[StaffList.SelectedIndex];
end;

procedure TSchedulePersonalForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SCHEDULEPERSONALFORM);
  ZoomPercent:= SettingValues[0];
  ParamList.Params:= VCut(SettingValues, 1);
end;

procedure TSchedulePersonalForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= nil;
  VAppend(SettingValues, ZoomPercent);
  SettingValues:= VAdd(SettingValues, ParamList.Params);
  DataBase.SettingsUpdate(SETTING_NAMES_SCHEDULEPERSONALFORM, SettingValues);
end;

procedure TSchedulePersonalForm.ViewUpdate(const AModeType: TModeType);
begin
  ViewPanel.Visible:= False;
  SettingPanel.Visible:= False;
  ListPanel.Visible:= False;
  EditingPanel.Visible:= False;
  try
    ModeType:= AModeType;
    if IsCopyDates then CopyEnd(False);

    if (ModeType<>mtEditing) and VSTDays.IsSelected then
      VSTDays.UnSelect;
    VSTDays.CanSelect:= ModeType=mtEditing;

    LeftSplitter.Align:= alRight;
    EditingPanel.Visible:= ModeType=mtEditing;
    SettingPanel.Visible:= ModeType=mtSetting;
    ListPanel.Visible:= ModeType=mtView;
    LeftSplitter.Align:= alLeft;

    CaptionsUpdate;

  finally
    ViewPanel.Visible:= True;
  end;
end;

end.

