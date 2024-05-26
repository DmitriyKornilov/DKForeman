unit UTimetableForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, BCPanel, BCButton, VirtualTrees, Spin, StdCtrls,
  DividerBevel, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UWorkHours, UCalendar, UTimetable,
  UTimetableSheet,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_Vector, DK_Const, DK_Dialogs,
  DK_DateUtils, DK_Color, DK_SheetExporter, DK_StrUtils,
  DK_Progress, DK_Zoom, DK_Filter,
  //Forms
  UChooseForm;

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
    EditingPanel: TPanel;
    DayToolPanel: TPanel;
    EraseButton: TSpeedButton;
    ExportButton: TBCButton;
    FilterPanel: TPanel;
    FIORadioButton: TRadioButton;
    DayPanel: TPanel;
    DayVT: TVirtualStringTree;
    MonthBCButton: TBCButton;
    MonthPanel: TPanel;
    ViewCaptionPanel: TBCPanel;
    ViewGrid: TsWorksheetGrid;
    ViewGridPanel: TPanel;
    ViewPanel: TPanel;
    EditingCaptionPanel: TBCPanel;
    LeftSplitter: TSplitter;
    ListCaptionPanel: TBCPanel;
    ListFilterToolPanel: TPanel;
    ListOrderToolPanel: TPanel;
    ListPanel: TPanel;
    MonthTimetableButton: TBCButton;
    OrderButtonPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    SettingCaptionPanel: TBCPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    StaffCaptionPanel: TBCPanel;
    StaffListVT: TVirtualStringTree;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    WriteButton: TSpeedButton;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure TabNumRadioButtonChange(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDraw: Boolean;
    ZoomPercent: Integer;
    FilterString: String;
    ModeType: TModeType;
    MonthDropDown: TMonthDropDown;

    ParamList: TVSTParamList;
    Colors: TColorVector;

    StaffList: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    IsCopyDates: Boolean;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames: TStrVector;
    RecrutDates, DismissDates, Holidays: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    Calendar: TCalendar;
    TimetableTotals: TTimetableTotals;
    Timetables: TTimetableVector;
    TimetableSheet: TYearTimetableSheet;

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);

    procedure ParamListCreate;

    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListCreate;
    procedure StaffListSelect;
    procedure StaffListLoad(const SelectedID: Integer = -1);

    procedure EditingTablesCreate;

    procedure TimetableLoad;
    procedure TimetableChange;
    procedure TimetableDraw(const AZoomPercent: Integer);
    procedure TimetableRedraw;

    procedure ColorsLoad;
    procedure CaptionsUpdate;
    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
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
    CopySaveButton, CopyDelButton,CopyCancelButton
  ]);
  SetCategoryButtons([
    ExportButton, MonthTimetableButton
  ]);

  CanDraw:= False;

  ParamListCreate;
  StaffListCreate;
  EditingTablesCreate;
  Calendar:= TCalendar.Create;
  TimetableSheet:= TYearTimetableSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
  YearSpinEdit.Value:= YearOfDate(Date);
  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, nil{@MonthTimetableLoad});

  IsCopyDates:= False;
  ColorsLoad;
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
  FreeAndNil(TimetableSheet);
end;

procedure TTimetableForm.FormShow(Sender: TObject);
begin
  ParamList.Show;
  MonthDropDown.AutoWidth;
  StaffListLoad;
end;

procedure TTimetableForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
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

end;

procedure TTimetableForm.CopyEnd(const ANeedSave: Boolean);
begin

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
  StaffList.SetSingleFont(MainForm.GridFont);
  StaffList.HeaderFont.Style:= [fsBold];

  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Сотрудник', 300);
  StaffList.AutosizeColumnEnable('Сотрудник');
  StaffList.Draw;
end;

procedure TTimetableForm.StaffListSelect;
begin
  TimetableChange;
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
  //VSTDays.OnSelect:= @CorrectionSelect;
  VSTDays.SetSingleFont(MainForm.GridFont);
  VSTDays.HeaderFont.Style:= [fsBold];
  VSTDays.CanSelect:= True;
  for i:= 0 to High(TIMETABLE_CORRECTION_COLUMN_WIDTHS) do
    VSTDays.AddColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[i],
                      TIMETABLE_CORRECTION_COLUMN_WIDTHS[i]);
  VSTDays.AutosizeColumnEnable(2);
  VSTDays.Draw;

  VSTCopy:= TVSTTable.Create(CopyVT);
  //VSTCopy.OnSelect:= @CopySelect;
  VSTCopy.SetSingleFont(MainForm.GridFont);
  VSTCopy.HeaderFont.Style:= [fsBold];
  VSTCopy.CanSelect:= True;
  for i:= 0 to High(TIMETABLE_CORRECTION_COLUMN_WIDTHS) do
    VSTCopy.AddColumn(TIMETABLE_CORRECTION_COLUMN_NAMES[i],
                      TIMETABLE_CORRECTION_COLUMN_WIDTHS[i]);
  VSTCopy.AutosizeColumnEnable(2);
  VSTCopy.Draw;

end;

procedure TTimetableForm.TimetableLoad;
var
  i: Integer;
  Timetable: TTimetable;
  MonthCalendar: TCalendar;
  BD, ED: TDate;
  Progress: TProgress;
begin
  VTDel(Timetables);
  if not StaffList.IsSelected then Exit;
  if not Calendar.IsCalculated then Exit;

  Screen.Cursor:= crHourGlass;
  try
    Progress:= TProgress.Create(nil);
    try
      MonthCalendar:= TCalendar.Create;
      try
        Progress.WriteLine1('Актуализация и расчет табеля');
        Progress.WriteLine2(EmptyStr);
        Progress.Show;
        for i:=1 to 12 do
        begin
          Progress.WriteLine2(MONTHSNOM[i] + ' ' + YearSpinEdit.Text);
          FirstLastDayInMonth(i, YearSpinEdit.Value, BD,ED);
          Calendar.Cut(BD,ED, MonthCalendar);
          Timetable:= TTimetable.Create(TabNumIDs[StaffList.SelectedIndex],
            TabNums[StaffList.SelectedIndex],
            RecrutDates[StaffList.SelectedIndex], DismissDates[StaffList.SelectedIndex],
            Holidays, MonthCalendar
            );
          VTAppend(Timetables, Timetable);
        end;
      finally
        FreeAndNil(MonthCalendar);
      end;

      //итоговые данные
      TimetableTotals:= TimetableYearTotalsLoad(TabNumIDs[StaffList.SelectedIndex],
                                                YearSpinEdit.Value);

    finally
      FreeAndNil(Progress);
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TTimetableForm.TimetableChange;
begin
  if not CanDraw then Exit;

  CaptionsUpdate;
  TimetableLoad;
  TimetableRedraw;
end;

procedure TTimetableForm.TimetableDraw(const AZoomPercent: Integer);
begin
  if Length(Timetables)=0 then Exit;

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    TimetableSheet.Zoom(ZoomPercent);
    TimetableSheet.Draw(Timetables, TimetableTotals, YearSpinEdit.Value,
                        EmptyStr{StaffLongNames[StaffList.SelectedIndex]},
                        RecrutDates[StaffList.SelectedIndex],
                        DismissDates[StaffList.SelectedIndex],
                        ParamList.Selected['CountType']);
    if ParamList.Checked['ViewParams', 0] then
      TimetableSheet.ColorsUpdate(Colors)
    else
      TimetableSheet.ColorsClear;
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TTimetableForm.TimetableRedraw;
begin
  TimetableDraw(ZoomPercent);
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

procedure TTimetableForm.CaptionsUpdate;
begin
  StaffCaptionPanel.Caption:= EmptyStr;
  ViewCaptionPanel.Caption:= 'Табель: ';

  if not StaffList.IsSelected then Exit;

  StaffCaptionPanel.Caption:= StaffLongNames[StaffList.SelectedIndex];
  StaffCaptionPanel.Visible:= ModeType=mtEditing;

  ViewCaptionPanel.Caption:= 'Табель за ' + YearSpinEdit.Text + ' год: ';
  if ModeType<>mtEditing then
    ViewCaptionPanel.Caption:= ViewCaptionPanel.Caption +
                               StaffLongNames[StaffList.SelectedIndex];
end;

procedure TTimetableForm.SettingsLoad;
begin
  ZoomPercent:= 100;
end;

procedure TTimetableForm.SettingsSave;
begin

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

end.

