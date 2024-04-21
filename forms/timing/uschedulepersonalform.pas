unit USchedulePersonalForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, BCPanel, BCButton, VirtualTrees, Spin, EditBtn, StdCtrls,
  DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UWorkHours, UCalendar, USchedule,

  //DK packages utils
  DK_VSTTables, DK_VSTTableTools, DK_VSTEdit, DK_Vector, DK_Const, DK_Dialogs,
  DK_Zoom, DK_DateUtils, DK_Color, DK_SheetExporter, DK_StrUtils,
  //Forms
  UChooseForm, USchedulePersonalEditForm;

type

  { TSchedulePersonalForm }

  TSchedulePersonalForm = class(TForm)
    DescendingButton: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    CloseButton: TSpeedButton;
    ColorTypeVT: TVirtualStringTree;
    CopyCancelButton: TSpeedButton;
    FilterEdit: TEditButton;
    FilterLabel: TLabel;
    FilterPanel: TPanel;
    AscendingButton: TSpeedButton;
    OrderLabel: TLabel;
    ListFilterToolPanel: TPanel;
    ListOrderToolPanel: TPanel;
    FIORadioButton: TRadioButton;
    PostRadioButton: TRadioButton;
    TabNumRadioButton: TRadioButton;
    VacationEraseButton: TSpeedButton;
    VacationScheduleButton: TBCButton;
    VacationCancelButton: TSpeedButton;
    CopyDelButton: TSpeedButton;
    VacationDelButton: TSpeedButton;
    CopyPanel: TPanel;
    CopySaveButton: TSpeedButton;
    VacationSaveButton: TSpeedButton;
    CopyToolPanel: TPanel;
    VacationToolPanel: TPanel;
    CopyVT: TVirtualStringTree;
    CorrectionsCaptionPanel: TBCPanel;
    CorrectionsPanel: TPanel;
    CountTypeVT: TVirtualStringTree;
    DayAddButton: TSpeedButton;
    DayCopyButton: TSpeedButton;
    DayDelButton: TSpeedButton;
    HistoryDelButton: TSpeedButton;
    DayEditButton: TSpeedButton;
    HistoryEditButton: TSpeedButton;
    DayPanel: TPanel;
    DayToolPanel: TPanel;
    HistoryCaptionPanel: TBCPanel;
    HistoryAddButton: TSpeedButton;
    DayVT: TVirtualStringTree;
    EditingPanel: TPanel;
    ExportButton: TBCButton;
    LeftSplitter: TSplitter;
    ListCaptionPanel: TBCPanel;
    BottomEditingPanel: TPanel;
    HistoryPanel: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    StaffCaptionPanel: TBCPanel;
    MainPanel: TPanel;
    MonthScheduleButton: TBCButton;
    ParamListVT: TVirtualStringTree;
    SettingCaptionPanel: TBCPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    SheetCaptionPanel: TBCPanel;
    SheetPanel: TPanel;
    ListPanel: TPanel;
    StaffListVT: TVirtualStringTree;
    ToolPanel: TPanel;
    VacationCaptionPanel: TBCPanel;
    VacationPanel: TPanel;
    HistoryToolPanel: TPanel;
    HistoryVT: TVirtualStringTree;
    ViewGrid: TsWorksheetGrid;
    VacationVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure DescendingButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure AscendingButtonClick(Sender: TObject);
    procedure FilterEditButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure HistoryAddButtonClick(Sender: TObject);
    procedure HistoryEditButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
    procedure VacationCancelButtonClick(Sender: TObject);
    procedure VacationDelButtonClick(Sender: TObject);
    procedure VacationEraseButtonClick(Sender: TObject);
    procedure VacationSaveButtonClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDrawSchedule: Boolean;
    ZoomPercent: Integer;
    ModeType: TModeType;

    Calendar: TCalendar;
    Schedule: TPersonalSchedule;

    Colors: TColorVector;

    ParamList: TVSTCheckList;
    CountType: TVSTStringList;
    ColorType: TVSTStringList;

    StaffList: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    VacationEdit: TVSTEdit;
    VacationDates: TDateVector;
    VacationCounts, VacationAddCounts: TIntVector;
    TmpVacationDates: TDateVector;
    TmpVacationCounts, TmpVacationAddCounts: TIntVector;

    History: TVSTTable;
    HistoryIDs: TIntVector;
    HistoryBeginDates, HistoryEndDates: TDateVector;
    HistoryScheduleNames: TStrVector;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames: TStrVector;
    RecrutDates, DismissDates: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;


    SelectedHoursTotal, SelectedHoursNight, SelectedDigMark, SelectedShiftNum: Integer;
    SelectedStrMark: String;
    IsCopyDates: Boolean;

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);
    function DayInListSelect(const ADate: TDate): Boolean;

    procedure ParamListCreate;
    procedure CountTypeCreate;
    procedure ColorTypeCreate;

    procedure EditingTablesCreate;
    procedure CorrectionsLoad(const SelectedID: Integer = -1);
    procedure CorrectionDelete;
    procedure CorrectionSelect;
    procedure CopyListLoad(const ASelectedDate: TDate = 0);
    procedure CopySelect;

    procedure StaffListCreate;
    procedure StaffListSelect;
    procedure StaffListLoad(const SelectedID: Integer = -1);

    procedure ScheduleLoad;
    procedure ScheduleChange;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;

    procedure HistoryCreate;
    procedure HistoryLoad(const SelectedID: Integer = -1);
    procedure HistorySelect;

    procedure VacationEditCreate;
    procedure VacationEditLoad;
    procedure VacationEditSetColumns(const ADates: TDateVector; const ACounts, AAddCounts: TIntVector);
    procedure VacationEditGetColumns(out ADates: TDateVector; out ACounts, AAddCounts: TIntVector);
    procedure VacationEditSelect;
    procedure VacationEditingBegin;
    procedure VacationEditDelete;

    procedure SchedulePersonalEditFormOpen(const AEditingType: TEditingType);

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

procedure TSchedulePersonalForm.AscendingButtonClick(Sender: TObject);
begin
  AscendingButton.Visible:= False;
  DescendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TSchedulePersonalForm.FilterEditButtonClick(Sender: TObject);
begin
  FilterEdit.Text:= EmptyStr;
end;

procedure TSchedulePersonalForm.FilterEditChange(Sender: TObject);
begin
  StaffListLoad;
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
    VacationCaptionPanel, CorrectionsCaptionPanel, SheetCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton,
    HistoryAddButton, HistoryDelButton, HistoryEditButton,
    VacationSaveButton, VacationDelButton, VacationCancelButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton,CopyCancelButton
  ]);
  SetCategoryButtons([
    ExportButton, MonthScheduleButton
  ]);

  Calendar:= TCalendar.Create;
  //Schedule:= TShiftSchedule.Create;

  CanDrawSchedule:= False;

  VacationEditCreate;
  HistoryCreate;
  StaffListCreate;
  ParamListCreate;
  CountTypeCreate;
  ColorTypeCreate;
  EditingTablesCreate;
  YearSpinEdit.Value:= YearOfDate(Date);

  IsCopyDates:= False;

  //ColorsLoad;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  CanDrawSchedule:= True;
end;

procedure TSchedulePersonalForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(CountType);
  FreeAndNil(ColorType);

  FreeAndNil(VacationEdit);
  FreeAndNil(StaffList);
  FreeAndNil(History);

  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);

  FreeAndNil(Calendar);
  if Assigned(Schedule) then FreeAndNil(Schedule);
  //if Assigned(Sheet) then FreeAndNil(Sheet);
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

procedure TSchedulePersonalForm.HistoryEditButtonClick(Sender: TObject);
begin
  SchedulePersonalEditFormOpen(etEdit);
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
begin
  VacationEdit.UnSelect;
  VacationEditGetColumns(VacationDates, VacationCounts, VacationAddCounts);

  if VacationDates[0]=0 then
  begin
    ShowInfo('Не указана дата начала планируемого отпуска (1 часть)!');
    Exit;
  end;
  if VacationCounts[0]=0 then
  begin
    ShowInfo('Не указано количество дней планируемого основного отпуска (1 часть)!');
    Exit;
  end;

  if VacationDates[1]=0 then
    VacationDates[1]:= VacationDates[0];
  if VacationCounts[1]=0 then
    VacationCounts[1]:= VacationCounts[0];

  DataBase.VacationsEditingUpdate(TabNumIDs[StaffList.SelectedIndex], YearSpinEdit.Value,
                                VacationDates, VacationCounts, VacationAddCounts);

  ScheduleChange;
  //TmpVacationDates:= VCut(VacationDates);
  //TmpVacationCounts:= VCut(VacationCounts);
  //TmpVacationAddCounts:= VCut(VacationAddCounts);
  //VacationSaveButton.Enabled:= False;
  //VacationCancelButton.Enabled:= False;
end;

procedure TSchedulePersonalForm.YearSpinEditChange(Sender: TObject);
begin
  CalendarForYear(YearSpinEdit.Value, Calendar);
  ScheduleChange;
end;

procedure TSchedulePersonalForm.CopyBegin;
begin

end;

procedure TSchedulePersonalForm.CopyEnd(const ANeedSave: Boolean);
begin

end;

function TSchedulePersonalForm.DayInListSelect(const ADate: TDate): Boolean;
begin

end;

procedure TSchedulePersonalForm.ParamListCreate;
var
  V: TStrVector;
begin
  V:= VCreateStr([
    'Отображать строку ночных часов',
    'Учитывать корректировки графика',
    'Коды табеля для нерабочих дней',
    'Учитывать отпуск',
    'Использовать цвета'
  ]);
  ParamList:= TVSTCheckList.Create(ParamListVT, VIEW_PARAMS_CAPTION, V, @ScheduleRedraw);
end;

procedure TSchedulePersonalForm.CountTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Отображать в итогах количество:';
  V:= VCreateStr([
    'дней',
    'смен',
    'дней и смен'
  ]);
  CountType:= TVSTStringList.Create(CountTypeVT, S, @ScheduleRedraw);
  CountType.Update(V);
end;

procedure TSchedulePersonalForm.ColorTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Выделять цветом нерабочие дни:';
  V:= VCreateStr([
    'по графику сменности',
    'по производственному календарю'
  ]);
  ColorType:= TVSTStringList.Create(ColorTypeVT, S, @ScheduleRedraw);
  ColorType.Update(V);
end;

procedure TSchedulePersonalForm.EditingTablesCreate;
var
  i: Integer;
begin

  VSTDays:= TVSTTable.Create(DayVT);
  VSTDays.OnSelect:= @CorrectionSelect;
  VSTDays.OnDelKeyDown:= @CorrectionDelete;
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
begin

end;

procedure TSchedulePersonalForm.CorrectionDelete;
begin

end;

procedure TSchedulePersonalForm.CorrectionSelect;
begin

end;

procedure TSchedulePersonalForm.CopyListLoad(const ASelectedDate: TDate = 0);
begin

end;

procedure TSchedulePersonalForm.CopySelect;
begin

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

  DataBase.StaffListForTimingLoad(STrimLeft(FilterEdit.Text),
                       BD, ED, OrderType, IsDescOrder,
                       TabNumIDs, RecrutDates, DismissDates,
                       Families, Names, Patronymics, TabNums, PostNames);
  StaffLongNames:= StaffNamesForTiming(Families, Names, Patronymics, TabNums, PostNames, True);
  StaffShortNames:= StaffNamesForTiming(Families, Names, Patronymics, TabNums, PostNames, False);

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

end;

procedure TSchedulePersonalForm.ScheduleChange;
begin
  if not CanDrawSchedule then Exit;
  CaptionsUpdate;
  HistoryLoad;
  VacationEditLoad;
  ScheduleLoad;
  ScheduleRedraw;
end;

procedure TSchedulePersonalForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  //if not CanDrawSchedule then Exit;
  if not Calendar.Calculated then Exit;
  //if not Schedule.Calculated then Exit;

  ViewGrid.Visible:= False;
  try
    ZoomPercent:= AZoomPercent;
    //ScheduleToSheet(Sheet, ViewGrid.Worksheet, ViewGrid, Calendar, Schedule);
  finally
    ViewGrid.Visible:= True;
  end;
end;

procedure TSchedulePersonalForm.ScheduleRedraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TSchedulePersonalForm.HistoryCreate;
begin
  History:= TVSTTable.Create(HistoryVT);
  History.CanSelect:= True;
  History.CanUnselect:= False;
  History.OnSelect:= @HistorySelect;
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
    HistoryIDs, HistoryBeginDates, HistoryEndDates, HistoryScheduleNames);

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

procedure TSchedulePersonalForm.VacationEditCreate;
begin
  VacationEdit:= TVSTEdit.Create(VacationVT);
  VacationEdit.SetSingleFont(MainForm.GridFont);
  VacationEdit.OnEdititingBegin:= @VacationEditingBegin;
  VacationEdit.OnSelect:= @VacationEditSelect;
  VacationEdit.OnDeleteCellText:= @VacationEditDelete;

  //VacationEdit.IsShowZeros:= True;
  VacationEdit.HeaderFont.Style:= [fsBold];
  VacationEdit.AddColumnRowTitles(VACATION_EDIT_COLUMN_NAMES[0],
                               VACATION_EDIT_COLUMN_WIDTHS[0]);

  VacationEdit.AddColumnDate(VACATION_EDIT_COLUMN_NAMES[1],
                             'dd.mm.yyyy',
                             VACATION_EDIT_COLUMN_WIDTHS[1]);
  VacationEdit.AddColumnInteger(VACATION_EDIT_COLUMN_NAMES[2],
                             VACATION_EDIT_COLUMN_WIDTHS[2]);
  VacationEdit.AddColumnInteger(VACATION_EDIT_COLUMN_NAMES[3],
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
  //TmpVacationDates:= VCut(VacationDates);
  //TmpVacationCounts:= VCut(VacationCounts);
  //TmpVacationAddCounts:= VCut(VacationAddCounts);
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

procedure TSchedulePersonalForm.SchedulePersonalEditFormOpen(const AEditingType: TEditingType);
var
  SchedulePersonalEditForm: TSchedulePersonalEditForm;
begin
  SchedulePersonalEditForm:= TSchedulePersonalEditForm.Create(nil);
  try
    if AEditingType=etEdit then
    begin

    end;
    if SchedulePersonalEditForm.ShowModal=mrOK then
    begin

    end;
  finally
    FreeAndNil(SchedulePersonalEditForm);
  end;
end;

procedure TSchedulePersonalForm.CaptionsUpdate;
begin
  StaffCaptionPanel.Caption:= EmptyStr;
  SheetCaptionPanel.Caption:= 'График: ';
  CorrectionsCaptionPanel.Caption:= 'Корректировки графика на ' + YearSpinEdit.Text + ' год';
  VacationCaptionPanel.Caption:= 'График отпусков на ' + YearSpinEdit.Text + ' год';

  if not StaffList.IsSelected then Exit;

  StaffCaptionPanel.Caption:= StaffLongNames[StaffList.SelectedIndex];
  StaffCaptionPanel.Visible:= ModeType=mtEditing;

  SheetCaptionPanel.Caption:= 'График работы на ' + YearSpinEdit.Text + ' год';
  if ModeType<>mtEditing then
    SheetCaptionPanel.Caption:= StaffLongNames[StaffList.SelectedIndex] + ': ' +
                                SheetCaptionPanel.Caption;
end;

procedure TSchedulePersonalForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SCHEDULEPERSONALFORM);
  ZoomPercent:= SettingValues[0];
  ParamList.Checked[0]:= SettingValues[1]=1;
  ParamList.Checked[1]:= SettingValues[2]=1;
  ParamList.Checked[2]:= SettingValues[3]=1;
  ParamList.Checked[3]:= SettingValues[4]=1;
  ParamList.Checked[4]:= SettingValues[5]=1;
  CountType.ItemIndex:= SettingValues[6];
  ColorType.ItemIndex:= SettingValues[7];
end;

procedure TSchedulePersonalForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent,
                              Ord(ParamList.Checked[0]),
                              Ord(ParamList.Checked[1]),
                              Ord(ParamList.Checked[2]),
                              Ord(ParamList.Checked[3]),
                              Ord(ParamList.Checked[4]),
                              CountType.ItemIndex,
                              ColorType.ItemIndex
                             ]);
  DataBase.SettingsUpdate(SETTING_NAMES_SCHEDULEPERSONALFORM, SettingValues);
end;

procedure TSchedulePersonalForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
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
    MainPanel.Visible:= True;
  end;
end;

end.

