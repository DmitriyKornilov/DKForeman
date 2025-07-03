unit UTimetableForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, VirtualTrees, Spin, StdCtrls, DividerBevel, DateUtils,
  BCButton,
  //Project utils
  UVars, UConst, UTypes, UTimingUtils,  UCalendar, USchedule,
  UTimetable, UTimetableSheet,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_Vector, DK_Const, DK_Dialogs,
  DK_DateUtils, DK_Color, DK_SheetExporter, DK_StrUtils, DK_CtrlUtils,
  DK_Progress, DK_Zoom, DK_Filter, DK_ColorLegend,
  //Forms
  UChooseForm, UTimetableEditForm, UTimetableMonthForm;

type

  { TTimetableForm }

  TTimetableForm = class(TForm)
    AscendingButton: TSpeedButton;
    CloseButton: TSpeedButton;
    CopyButton: TSpeedButton;
    CopyCancelButton: TSpeedButton;
    CopyDelButton: TSpeedButton;
    CopyPanel: TPanel;
    CopySaveButton: TSpeedButton;
    CopyToolPanel: TPanel;
    CopyVT: TVirtualStringTree;
    DescendingButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    DividerBevel5: TDividerBevel;
    EditButton: TSpeedButton;
    ViewCaptionPanel: TPanel;
    EditingPanel: TPanel;
    DayToolPanel: TPanel;
    EraseButton: TSpeedButton;
    ExportButton: TSpeedButton;
    StaffCaptionPanel: TPanel;
    SettingCaptionPanel: TPanel;
    MonthTimetableButton: TSpeedButton;
    FilterPanel: TPanel;
    FIORadioButton: TRadioButton;
    DayPanel: TPanel;
    DayVT: TVirtualStringTree;
    LegendPanel: TPanel;
    MonthBCButton: TBCButton;
    MonthPanel: TPanel;
    ListCaptionPanel: TPanel;
    EditingCaptionPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ViewGridPanel: TPanel;
    ViewPanel: TPanel;
    LeftSplitter: TSplitter;
    ListFilterToolPanel: TPanel;
    ListOrderToolPanel: TPanel;
    ListPanel: TPanel;
    OrderButtonPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    StaffListVT: TVirtualStringTree;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    WriteButton: TSpeedButton;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    SheetBottomPanel: TPanel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure CopyCancelButtonClick(Sender: TObject);
    procedure CopyDelButtonClick(Sender: TObject);
    procedure CopySaveButtonClick(Sender: TObject);
    procedure DayVTDblClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure EraseButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MonthTimetableButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure TabNumRadioButtonChange(Sender: TObject);
    procedure ViewGridDblClick(Sender: TObject);
    procedure ViewGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure WriteButtonClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDraw: Boolean;
    ZoomPercent: Integer;
    FilterString: String;
    ModeType: TModeType;
    MonthDropDown: TMonthDropDown;

    ViewYear, ViewTabNumID: Integer;

    ParamList: TVSTParamList;
    Colors: TColorVector;

    StaffList: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    IsCopyDates: Boolean;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames, TimetableNames: TStrVector;
    RecrutDates, DismissDates, Holidays: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    Calendar: TCalendar;
    TimetableTotals: TTimetableTotals;
    Timetables: TTimetableVector;
    Sheet: TYearTimetableSheet;

    MonthDates: TDateVector;
    MonthTimetableStrings, MonthScheduleNames: TStrVector;
    MonthTotalHours, MonthNightHours, MonthOverHours, MonthSkipHours,
    MonthSchedHours, MonthMainMarks, MonthSkipMarks, MonthSchedIDs, MonthShiftNums: TIntVector;

    SelectedTimetableString, SelectedScheduleName: String;
    SelectedDay: TTimetableDay;

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);
    function DayInListSelect(const ADate: TDate): Boolean;

    procedure ParamListCreate;

    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListCreate;
    procedure StaffListSelect;
    procedure StaffListLoad(const SelectedID: Integer = -1);

    procedure EditingTablesCreate;
    procedure MonthTimetableLoad;
    procedure MonthTimetableChange;
    procedure MonthTimetableDaySelect;
    procedure MonthTimetableDayEdit;
    procedure CopyListLoad(const ASelectedDate: TDate = 0);
    procedure CopySelect;

    procedure TimetableLoad;
    procedure TimetableChange;
    procedure TimetableUpdate;
    procedure TimetableToSheet(const ASheet: TYearTimetableSheet;
                               const ATimetables: TTimetableVector;
                               const ATimetableTotals: TTimetableTotals;
                               const ARecrutDate, ADismissDate: TDate;
                               const ACaption: String = '');
    procedure TimetableDraw(const AZoomPercent: Integer);
    procedure TimetableRedraw;
    procedure TimetableExport;

    procedure TimetableMonthFormOpen;
    procedure TimetableEditFormOpen(const ADate: TDate);

    procedure ColorsLoad;
    procedure LegendCreate;
    procedure CaptionsUpdate;
    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  TimetableForm: TTimetableForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TTimetableForm }

procedure TTimetableForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  ViewYear:= 0;
  ViewTabNumID:=0;

  CanDraw:= False;

  ParamListCreate;
  StaffListCreate;
  EditingTablesCreate;
  Calendar:= TCalendar.Create;
  VTCreate(Timetables, 12);
  Sheet:= TYearTimetableSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
  YearSpinEdit.Value:= YearOfDate(Date);
  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, @MonthTimetableLoad);

  IsCopyDates:= False;
  ColorsLoad;
  LegendCreate;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @TimetableDraw, True);
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter, 1000 {1c});

  CanDraw:= True;
end;

procedure TTimetableForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(StaffList);
  FreeAndNil(MonthDropDown);

  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);

  FreeAndNil(Calendar);
  VTDel(Timetables);
  FreeAndNil(Sheet);
end;

procedure TTimetableForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, ListFilterToolPanel, ListOrderToolPanel, DayToolPanel, CopyToolPanel
  ]);
  SetCaptionPanels([
    StaffCaptionPanel, SettingCaptionPanel, ListCaptionPanel, EditingCaptionPanel,
    ViewCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton,
    WriteButton, EraseButton, EditButton, CopyButton,
    CopySaveButton, CopyDelButton, CopyCancelButton
  ]);

  Images.ToButtons([
    ExportButton, MonthTimetableButton,
    CloseButton, AscendingButton, DescendingButton,
    WriteButton, EraseButton, EditButton, CopyButton,
    CopySaveButton, CopyDelButton, CopyCancelButton
  ]);

  MonthDropDown.AutoWidth;

  DataUpdate;
end;

procedure TTimetableForm.TimetableMonthFormOpen;
var
  TimetableMonthForm: TTimetableMonthForm;
begin
  TimetableMonthForm:= TTimetableMonthForm.Create(nil);
  try
    TimetableMonthForm.YearSpinEdit.Value:= YearSpinEdit.Value;
    TimetableMonthForm.ShowModal;
    TimetableUpdate;
  finally
    FreeAndNil(TimetableMonthForm);
  end;
end;

procedure TTimetableForm.MonthTimetableButtonClick(Sender: TObject);
begin
  TimetableMonthFormOpen;
end;

procedure TTimetableForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TTimetableForm.CopyButtonClick(Sender: TObject);
begin
  CopyBegin;
end;

procedure TTimetableForm.CopyCancelButtonClick(Sender: TObject);
begin
  CopyEnd(False);
end;

procedure TTimetableForm.CopyDelButtonClick(Sender: TObject);
begin
  Sheet.Unselect(Sheet.SelectedDates[VSTCopy.SelectedIndex]);
  CopyListLoad;
end;

procedure TTimetableForm.CopySaveButtonClick(Sender: TObject);
begin
  CopyEnd(True);
end;

procedure TTimetableForm.DayVTDblClick(Sender: TObject);
begin
  MonthTimetableDayEdit;
end;

procedure TTimetableForm.EditButtonClick(Sender: TObject);
begin
  MonthTimetableDayEdit;
end;

procedure TTimetableForm.FIORadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TTimetableForm.PostRadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TTimetableForm.TabNumRadioButtonChange(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TTimetableForm.ViewGridDblClick(Sender: TObject);
var
  DayDate: TDate;
begin
  if ModeType<>mtEditing then Exit;
  if not Sheet.GridToDate(ViewGrid.Row, ViewGrid.Col, DayDate) then Exit;
  //VSTDays.ReSelect(MonthDates, DayDate, False);
  TimetableEditFormOpen(DayDate);
end;

procedure TTimetableForm.ViewGridMouseDown(Sender: TObject;
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
    if not Timetables[MonthOfDate(D)-1].IsDateExists(D) then Exit;
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

procedure TTimetableForm.YearSpinEditChange(Sender: TObject);
var
  SelectedTabNumID: Integer;
begin
  CalendarForYear(YearSpinEdit.Value, Calendar);
  Holidays:= DataBase.HolidaysLoad(YearSpinEdit.Value);

  SelectedTabNumID:= -1;
  if StaffList.IsSelected then
    SelectedTabNumID:= TabNumIDs[StaffList.SelectedIndex];
  StaffListLoad(SelectedTabNumID);
end;

procedure TTimetableForm.CopyBegin;
begin
  IsCopyDates:= True;
  Sheet.MultiSelect:= True;
  DayPanel.Visible:= False;
  DayPanel.Align:= alBottom;
  CopyPanel.Align:= alClient;
  CopyPanel.Visible:= True;

  SelectedTimetableString:= MonthTimetableStrings[VSTDays.SelectedIndex];
  SelectedScheduleName:= MonthScheduleNames[VSTDays.SelectedIndex];

  SelectedDay.ScheduleHours:= MonthSchedHours[VSTDays.SelectedIndex];
  SelectedDay.TotalHours:= MonthTotalHours[VSTDays.SelectedIndex];
  SelectedDay.NightHours:= MonthNightHours[VSTDays.SelectedIndex];
  SelectedDay.OverHours:= MonthOverHours[VSTDays.SelectedIndex];
  SelectedDay.SkipHours:= MonthSkipHours[VSTDays.SelectedIndex];
  SelectedDay.DigMark:= MonthMainMarks[VSTDays.SelectedIndex];
  SelectedDay.SkipMark:= MonthSkipMarks[VSTDays.SelectedIndex];
  SelectedDay.ScheduleID:= MANUAL_SCHEDULEID; //MonthSchedIDs[VSTDays.SelectedIndex];
  SelectedDay.ShiftNum:= MonthShiftNums[VSTDays.SelectedIndex];

  VSTDays.Unselect;
end;

procedure TTimetableForm.CopyEnd(const ANeedSave: Boolean);
begin
  if Sheet.IsSelected then
  begin
    if ANeedSave then //apply copies
    begin
      DataBase.TimetableDaysReplace(TabNumIDs[StaffList.SelectedIndex],
                                    Sheet.SelectedDates, SelectedDay);
      TimetableUpdate;
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

function TTimetableForm.DayInListSelect(const ADate: TDate): Boolean;
var
  Ind: Integer;
begin
  Result:= False;
  if IsCopyDates then Exit;

  MonthDropDown.Month:= MonthOfDate(ADate);

  Ind:= VIndexOfDate(MonthDates, ADate);
  if Ind>=0 then
    VSTDays.Select(Ind)
  else
    VSTDays.UnSelect;
  Result:= Ind>=0;
end;

procedure TTimetableForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= VIEW_PARAMS_CAPTION;
  V:= VCreateStr([
    'использовать цвета'
  ]);
  ParamList.AddCheckList('ViewParams', S, V, @TimetableRedraw);

   S:= 'Отображать в итогах количество:';
  V:= VCreateStr([
    'дней',
    'смен',
    'дней и смен'
  ]);
  ParamList.AddStringList('CountType', S, V, @TimetableRedraw);
end;

procedure TTimetableForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TTimetableForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffListVT);
  StaffList.CanSelect:= True;
  StaffList.CanUnselect:= False;
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(GridFont);
  StaffList.HeaderFont.Style:= [fsBold];

  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Сотрудник', 300);
  StaffList.AutosizeColumnEnable('Сотрудник');
  StaffList.Draw;
end;

procedure TTimetableForm.StaffListSelect;
begin
  TimetableChange;
  WriteButton.Enabled:= StaffList.IsSelected;
end;

procedure TTimetableForm.StaffListLoad(const SelectedID: Integer);
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
  TimetableNames:= StaffNamesForScheduleNames(Families, Names, Patronymics, TabNums, True);

  ExportButton.Enabled:= not VIsNil(TabNumIDs);
  MonthTimetableButton.Enabled:= ExportButton.Enabled;
  WriteButton.Enabled:= ExportButton.Enabled;
  EraseButton.Enabled:= ExportButton.Enabled;

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

procedure TTimetableForm.EditingTablesCreate;
var
  i: Integer;
begin
  VSTDays:= TVSTTable.Create(DayVT);
  VSTDays.OnSelect:= @MonthTimetableDaySelect;
  VSTDays.OnReturnKeyDown:= @MonthTimetableDayEdit;
  VSTDays.SetSingleFont(GridFont);
  VSTDays.HeaderFont.Style:= [fsBold];
  VSTDays.CanSelect:= True;
  for i:= 0 to High(TIMETABLE_CORRECTION_COLUMN_WIDTHS) do
    VSTDays.AddColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[i],
                      TIMETABLE_CORRECTION_COLUMN_WIDTHS[i]);
  VSTDays.AutosizeColumnEnable(2);
  VSTDays.Draw;

  VSTCopy:= TVSTTable.Create(CopyVT);
  VSTCopy.OnSelect:= @CopySelect;
  VSTCopy.SetSingleFont(GridFont);
  VSTCopy.HeaderFont.Style:= [fsBold];
  VSTCopy.CanSelect:= True;
  for i:= 0 to High(TIMETABLE_CORRECTION_COLUMN_WIDTHS) do
    VSTCopy.AddColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[i],
                      TIMETABLE_CORRECTION_COLUMN_WIDTHS[i]);
  VSTCopy.AutosizeColumnEnable(2);
  VSTCopy.Draw;
end;

procedure TTimetableForm.MonthTimetableLoad;
var
  Dates, ShiftNums: TStrVector;
begin
  if not CanDraw then Exit;
  VSTDays.ValuesClear;
  if not StaffList.IsSelected then Exit;

  DataBase.TimetableDataMonthForEditLoad(TabNumIDs[StaffList.SelectedIndex],
                      MonthDropDown.Month, YearSpinEdit.Value,
                      MonthDates, MonthTimetableStrings, MonthScheduleNames,
                      MonthTotalHours, MonthNightHours, MonthOverHours, MonthSkipHours,
                      MonthSchedHours, MonthMainMarks, MonthSkipMarks,
                      MonthSchedIDs, MonthShiftNums);

  ShiftNums:= VIntToStr(MonthShiftNums);
  VChangeIf(ShiftNums, '0', EMPTY_MARK);
  Dates:= VDateToStr(MonthDates);

  VSTDays.Visible:= False;
  try
    VSTDays.ValuesClear;
    VSTDays.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[0], Dates);
    VSTDays.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[1], MonthTimetableStrings);
    VSTDays.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[2], MonthScheduleNames);
    VSTDays.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[3], ShiftNums);
    VSTDays.Draw;
    //VSTDays.ReSelect(CorrectIDs, SelectedCorrectionID);
  finally
    VSTDays.Visible:= True;
  end;

  EraseButton.Enabled:= not VIsNil(MonthDates);
end;

procedure TTimetableForm.MonthTimetableChange;
var
  M, Y: Word;
  TabNumID: Integer;
  BD, ED: TDate;
  MonthCalendar: TCalendar;
begin
  MonthTimetableLoad;
  M:= MonthDropDown.Month;
  Y:= YearSpinEdit.Value;
  FirstLastDayInMonth(M, Y, BD, ED);
  TabNumID:= TabNumIDs[StaffList.SelectedIndex];
  MonthCalendar:= TCalendar.Create;
  try
    Calendar.Cut(BD, ED, MonthCalendar);
    Timetables[M-1].Calc(TabNumID, TabNums[StaffList.SelectedIndex],
                         RecrutDates[StaffList.SelectedIndex],
                         DismissDates[StaffList.SelectedIndex],
                         MonthCalendar, EmptyPostScheduleInfo);
  finally
    FreeAndNil(MonthCalendar);
  end;

  TimetableTotals:= TimetableYearTotalsLoad(TabNumID, Y); //итоговые данные
  Sheet.MonthRedraw(M, Timetables[M-1], TimetableTotals);
  if ParamList.Checked['ViewParams', 0] then
    Sheet.ColorsUpdate(Colors);
end;

procedure TTimetableForm.MonthTimetableDaySelect;
begin
  if VSTDays.IsSelected then
    Sheet.DayInGridSelect(MonthDates[VSTDays.SelectedIndex])
  else //if Assigned(Sheet) then
    Sheet.SelectionClear;

  EditButton.Enabled:= VSTDays.IsSelected;
  CopyButton.Enabled:= EditButton.Enabled;
end;

procedure TTimetableForm.MonthTimetableDayEdit;
begin
  if not VSTDays.IsSelected then Exit;
  TimetableEditFormOpen(MonthDates[VSTDays.SelectedIndex]);
end;

procedure TTimetableForm.CopyListLoad(const ASelectedDate: TDate);
var
  Dates, TimetableStrings, ScheduleNames, ShiftNums: TStrVector;
begin
  Dates:= VDateToStr(Sheet.SelectedDates);
  VDim(TimetableStrings{%H-}, Length(Dates), SelectedTimetableString);
  VDim(ScheduleNames{%H-}, Length(Dates), SelectedScheduleName);
  VDim(ShiftNums{%H-}, Length(Dates), IntToStr(SelectedDay.ShiftNum));
  VChangeIf(ShiftNums, '0', EMPTY_MARK);

  VSTCopy.Visible:= False;
  try
    VSTCopy.ValuesClear;
    VSTCopy.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[0], Dates);
    VSTCopy.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[1], TimetableStrings);
    VSTCopy.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[2], ScheduleNames);
    VSTCopy.SetColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[3], ShiftNums);
    VSTCopy.Draw;
    VSTCopy.ReSelect(Sheet.SelectedDates, ASelectedDate);
  finally
    VSTCopy.Visible:= True;
  end;

  CopySaveButton.Enabled:= Sheet.IsSelected;
end;

procedure TTimetableForm.CopySelect;
begin
  CopyDelButton.Enabled:= VSTCopy.IsSelected;
end;

procedure TTimetableForm.WriteButtonClick(Sender: TObject);
var
  BD, ED: TDate;
begin
  if not StaffList.IsSelected then Exit;
  FirstLastDayInMonth(MonthDropDown.Month, YearSpinEdit.Value, BD, ED);
  if not TimetableForPeriodUpdate(TabNumIDs[StaffList.SelectedIndex],
            RecrutDates[StaffList.SelectedIndex], DismissDates[StaffList.SelectedIndex],
            BD, ED, Holidays, False) then Exit;
  MonthTimetableChange;
end;

procedure TTimetableForm.EraseButtonClick(Sender: TObject);
var
  BD, ED: TDate;
begin
  if not Confirm('Удалить данные табеля за ' + MonthDropDown.Text + ' ' +
                 YearSpinEdit.Text + '?') then Exit;
  FirstLastDayInMonth(MonthDropDown.Month, YearSpinEdit.Value, BD, ED);
  if not DataBase.TimetableDaysDelete(TabNumIDs[StaffList.SelectedIndex], BD, ED) then Exit;
  MonthTimetableChange;
end;

procedure TTimetableForm.ExportButtonClick(Sender: TObject);
begin
  TimetableExport;
end;

procedure TTimetableForm.TimetableLoad;
var
  i: Integer;
  MonthCalendar: TCalendar;
  BD, ED: TDate;
begin
  VTClear(Timetables);
  if not StaffList.IsSelected then Exit;
  if not Calendar.IsCalculated then Exit;

  Screen.Cursor:= crHourGlass;
  try
    MonthCalendar:= TCalendar.Create;
    try
      for i:=1 to 12 do
      begin
        FirstLastDayInMonth(i, YearSpinEdit.Value, BD,ED);
        Calendar.Cut(BD,ED, MonthCalendar);
        Timetables[i-1].Calc(TabNumIDs[StaffList.SelectedIndex],
          TabNums[StaffList.SelectedIndex],
          RecrutDates[StaffList.SelectedIndex], DismissDates[StaffList.SelectedIndex],
          MonthCalendar, EmptyPostScheduleInfo
          );
      end;
    finally
      FreeAndNil(MonthCalendar);
    end;
    //итоговые данные
    TimetableTotals:= TimetableYearTotalsLoad(TabNumIDs[StaffList.SelectedIndex],
                                              YearSpinEdit.Value);

  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TTimetableForm.TimetableChange;
begin
  if not CanDraw then Exit;
  if not StaffList.IsSelected then Exit;

  if (TabNumIDs[StaffList.SelectedIndex]=ViewTabNumID) and
     (YearSpinEdit.Value = ViewYear) then Exit;
  ViewYear:= YearSpinEdit.Value;
  ViewTabNumID:= TabNumIDs[StaffList.SelectedIndex];

  CaptionsUpdate;
  TimetableUpdate;
end;

procedure TTimetableForm.TimetableUpdate;
begin
  TimetableLoad;
  TimetableRedraw;
  MonthTimetableLoad;
end;

procedure TTimetableForm.TimetableToSheet(const ASheet: TYearTimetableSheet;
                               const ATimetables: TTimetableVector;
                               const ATimetableTotals: TTimetableTotals;
                               const ARecrutDate, ADismissDate: TDate;
                               const ACaption: String = '');
begin
  ASheet.Draw(ATimetables, ATimetableTotals, YearSpinEdit.Value,
              ACaption, ARecrutDate, ADismissDate, ParamList.Selected['CountType']);
  if ParamList.Checked['ViewParams', 0] then
    Sheet.ColorsUpdate(Colors);
end;

procedure TTimetableForm.TimetableDraw(const AZoomPercent: Integer);
begin
  if not CanDraw then Exit;

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    TimetableToSheet(Sheet, Timetables, TimetableTotals,
                     RecrutDates[StaffList.SelectedIndex],
                     DismissDates[StaffList.SelectedIndex]);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TTimetableForm.TimetableRedraw;
begin
  TimetableDraw(ZoomPercent);
end;

procedure TTimetableForm.TimetableExport;
var
  V: TStrVector;
  S: String;
  ChooseIndex: Integer;

  procedure ExportSingleTimetable;
  var
    Exporter: TSheetsExporter;
    Worksheet: TsWorksheet;
    ExpSheet: TYearTimetableSheet;
  begin
    ExpSheet:= nil;
    Exporter:= TSheetsExporter.Create;
    try
      Worksheet:= Exporter.AddWorksheet(YearSpinEdit.Text);
      ExpSheet:= TYearTimetableSheet.Create(Worksheet, nil, GridFont);
      TimetableToSheet(ExpSheet, Timetables, TimetableTotals,
                     RecrutDates[StaffList.SelectedIndex],
                     DismissDates[StaffList.SelectedIndex],
                     TimetableNames[StaffList.SelectedIndex]);
      Exporter.PageSettings();
      Exporter.Save('Выполнено!', TimetableNames[StaffList.SelectedIndex]);
    finally
      if Assigned(ExpSheet) then FreeAndNil(ExpSheet);
      FreeAndNil(Exporter);
    end;
  end;

  procedure ExportSeveralTimetables;
  var
    Exporter: TBooksExporter;
    Worksheet: TsWorksheet;
    ExpSheet: TYearTimetableSheet;
    MonthCalendar: TCalendar;
    TmpTimetables: TTimetableVector;
    TmpTimetableTotals: TTimetableTotals;
    i, j: Integer;
    BD, ED: TDate;
    Progress: TProgress;
  begin
    ExpSheet:= nil;
    TmpTimetables:= nil;
    Exporter:= TBooksExporter.Create;
    if not Exporter.BeginExport then
    begin
      FreeAndNil(Exporter);
      Exit;
    end;
    try
      Progress:= TProgress.Create(nil);
      MonthCalendar:= TCalendar.Create;
      VTCreate(TmpTimetables, 12);
      try
        Progress.WriteLine1('Экспорт табеля');
        Progress.WriteLine2(EmptyStr);
        Progress.Show;
        for i:=0 to High(TabNumIDs) do
        begin
          Progress.WriteLine2(StaffLongNames[i]);
          for j:=1 to 12 do
          begin
            FirstLastDayInMonth(j, YearSpinEdit.Value, BD, ED);
            Calendar.Cut(BD, ED, MonthCalendar);
            Progress.Go;
            TmpTimetables[j-1].Calc(TabNumIDs[i], TabNums[i],
                                    RecrutDates[i], DismissDates[i],
                                    MonthCalendar, EmptyPostScheduleInfo);
            Progress.Go;
          end;
          Progress.Go;
          TmpTimetableTotals:= TimetableYearTotalsLoad(TabNumIDs[i], YearSpinEdit.Value);

          Worksheet:= Exporter.AddWorksheet(YearSpinEdit.Text);
          ExpSheet:= TYearTimetableSheet.Create(Worksheet, nil, GridFont);
          Progress.Go;
          TimetableToSheet(ExpSheet, TmpTimetables, TmpTimetableTotals,
                           RecrutDates[i], DismissDates[i], TimetableNames[i]);
          Progress.Go;
          Exporter.PageSettings();
          Exporter.Save(TimetableNames[i]);
        end;

      finally
        FreeAndNil(Progress);
        FreeAndNil(MonthCalendar);
        VTDel(TmpTimetables);
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
    'Табель за ' + YearSpinEdit.Text + ' год: ' + TimetableNames[StaffList.SelectedIndex],
    'Табели всех сотрудников за ' + YearSpinEdit.Text + ' год'
  ]);
  if not Choose(S, V, ChooseIndex) then Exit;

  case ChooseIndex of
  0: ExportSingleTimetable;
  1: ExportSeveralTimetables;
  end;
end;

procedure TTimetableForm.TimetableEditFormOpen(const ADate: TDate);
var
  TimetableEditForm: TTimetableEditForm;
begin
  if not StaffList.IsSelected then Exit;

  TimetableEditForm:= TTimetableEditForm.Create(nil);
  try
    TimetableEditForm.TabNumID:= TabNumIDs[StaffList.SelectedIndex];
    TimetableEditForm.RecrutDate:= RecrutDates[StaffList.SelectedIndex];
    TimetableEditForm.DismissDate:= DismissDates[StaffList.SelectedIndex];
    TimetableEditForm.FirstDate:= ADate;
    if TimetableEditForm.ShowModal=mrOK then
      TimetableUpdate;
  finally
    FreeAndNil(TimetableEditForm);
  end;
end;

procedure TTimetableForm.ColorsLoad;
begin
  Colors:= nil;
  VDim(Colors, TIMETABLE_COLOR_COUNT);
  Colors[MANUAL_COLOR_INDEX]:= COLOR_TIMETABLE_MANUAL;
  Colors[HOLIDAY_COLOR_INDEX]:= COLOR_TIMETABLE_HOLIDAY;
  Colors[BEFORE_COLOR_INDEX]:= COLOR_TIMETABLE_BEFORE;
  Colors[TITLE_COLOR_INDEX]:= COLOR_TIMETABLE_TITLE;
  Colors[OUTSIDEMONTH_COLOR_INDEX]:= COLOR_TIMETABLE_OUTSIDEMONTH;
  Colors[NOTDEFINE_COLOR_INDEX]:= COLOR_TIMETABLE_NOTDEFINE;
  Colors[HIGHLIGHT_COLOR_INDEX]:= DefaultSelectionBGColor;
  Colors[NOTWORK_COLOR_INDEX]:= COLOR_TIMETABLE_NOTWORK;
end;

procedure TTimetableForm.LegendCreate;
var
  C: TColorVector;
  S: TStrVector;
begin
  C:= VCreateColor([
    COLOR_TIMETABLE_HOLIDAY,
    COLOR_TIMETABLE_NOTWORK,
    COLOR_TIMETABLE_BEFORE,
    COLOR_SCHEDULE_CORRECTION
  ]);
  S:= VCreateStr([
    '- праздничный день',
    '- нерабочий день',
    '- сокращенный день',
    '- корректировка'
  ]);

  ColorLegendCreate(LegendPanel, C, S);
end;

procedure TTimetableForm.CaptionsUpdate;
begin
  StaffCaptionPanel.Caption:= EmptyStr;
  ViewCaptionPanel.Caption:= '  Табель';

  if not StaffList.IsSelected then Exit;

  StaffCaptionPanel.Caption:= StaffLongNames[StaffList.SelectedIndex];
  StaffCaptionPanel.Visible:= ModeType=mtEditing;

  ViewCaptionPanel.Caption:= '  Табель за ' + YearSpinEdit.Text + ' год: ';
  if ModeType<>mtEditing then
    ViewCaptionPanel.Caption:= ViewCaptionPanel.Caption +
                               StaffLongNames[StaffList.SelectedIndex];
end;

procedure TTimetableForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_TIMETABLEFORM);
  ZoomPercent:= SettingValues[0];
  ParamList.Params:= VCut(SettingValues, 1);
end;

procedure TTimetableForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= nil;
  VAppend(SettingValues, ZoomPercent);
  SettingValues:= VAdd(SettingValues, ParamList.Params);
  DataBase.SettingsUpdate(SETTING_NAMES_TIMETABLEFORM, SettingValues);
end;

procedure TTimetableForm.ViewUpdate(const AModeType: TModeType);
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

procedure TTimetableForm.DataUpdate;
begin
  StaffListLoad;
  ViewTabNumID:= 0;
  TimetableChange;
end;

end.

