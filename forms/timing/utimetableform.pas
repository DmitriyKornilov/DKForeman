unit UTimetableForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, BCPanel, BCButton, VirtualTrees, Spin, EditBtn, StdCtrls,
  DividerBevel, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UWorkHours, UCalendar, USchedule,

  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_Vector, DK_Const, DK_Dialogs,
  DK_Zoom, DK_DateUtils, DK_Color, DK_SheetExporter, DK_StrUtils, DK_Progress,
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
    FilterButton: TSpeedButton;
    FilterEdit: TEdit;
    FilterLabel: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CanDraw: Boolean;
    ZoomPercent: Integer;
    ModeType: TModeType;
    MonthDropDown: TMonthDropDown;

    ParamList: TVSTParamList;

    StaffList: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    IsCopyDates: Boolean;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames: TStrVector;
    RecrutDates, DismissDates, Holidays: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);

    procedure ParamListCreate;

    procedure StaffListCreate;
    procedure StaffListSelect;
    procedure StaffListLoad(const SelectedID: Integer = -1);

    procedure EditingTablesCreate;

    procedure TimetableDraw(const AZoomPercent: Integer);
    procedure TimetableRedraw;

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

procedure TTimetableForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

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
    CloseButton, FilterButton, AscendingButton, DescendingButton,
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
  YearSpinEdit.Value:= YearOfDate(Date);
  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, nil{@MonthTimetableLoad});

  IsCopyDates:= False;
  //ColorsLoad;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @TimetableDraw, True);

  CanDraw:= True;
end;

procedure TTimetableForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(StaffList);
  FreeAndNil(MonthDropDown);

  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);
end;

procedure TTimetableForm.FormShow(Sender: TObject);
begin
  ParamList.Show;
  MonthDropDown.AutoWidth;
  StaffListLoad;
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

  DataBase.StaffListForPersonalTimingLoad(STrimLeft(FilterEdit.Text),
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

procedure TTimetableForm.TimetableDraw(const AZoomPercent: Integer);
begin
  ZoomPercent:= AZoomPercent;

end;

procedure TTimetableForm.TimetableRedraw;
begin
  TimetableDraw(ZoomPercent);
end;

procedure TTimetableForm.CaptionsUpdate;
begin
  StaffCaptionPanel.Caption:= EmptyStr;
  ViewCaptionPanel.Caption:= 'Табель: ';

  if not StaffList.IsSelected then Exit;

  StaffCaptionPanel.Caption:= StaffLongNames[StaffList.SelectedIndex];
  StaffCaptionPanel.Visible:= ModeType=mtEditing;

  ViewCaptionPanel.Caption:= 'Табель учета рабочего времени за ' + YearSpinEdit.Text + ' год: ';
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

