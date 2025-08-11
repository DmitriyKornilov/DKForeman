unit UVacationPlanningForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel, Spin, StdCtrls, VirtualTrees, fpspreadsheetgrid, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_VSTTables, DK_VSTCategoryTables, DK_Zoom, DK_Progress,
  DK_DateUtils, DK_ColorLegend, DK_CtrlUtils,
  //Project utils
  UVars, UConst, UUtils, UTimingUtils, UScheduleSheet, UCalendar, USchedule,
  //Forms
  UVacationPlanEditForm;

type

  { TVacationPlanningForm }

  TVacationPlanningForm = class(TForm)
    CheckAllButton: TSpeedButton;
    CloseButton: TSpeedButton;
    CollapseAllButton: TSpeedButton;
    DividerBevel5: TDividerBevel;
    EditButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    EditPanel: TPanel;
    ExpandAllButton: TSpeedButton;
    ExportButton: TSpeedButton;
    FIORadioButton: TRadioButton;
    ListButton: TSpeedButton;
    PlanCaptionPanel: TPanel;
    ListOrderToolPanel: TPanel;
    ListPanel: TPanel;
    ListToolPanel: TPanel;
    MStaffListVT: TVirtualStringTree;
    NextMonthButton: TSpeedButton;
    ListCaptionPanel: TPanel;
    OrderLabel: TLabel;
    LegendPanel: TPanel;
    PlanButton: TSpeedButton;
    StatCaptionPanel: TPanel;
    ZoomPanel: TPanel;
    StatGrid: TsWorksheetGrid;
    StatSheetPanel: TPanel;
    Splitter1: TSplitter;
    StatPanel: TPanel;
    PostRadioButton: TRadioButton;
    PrevDayButton: TSpeedButton;
    NextDayButton: TSpeedButton;
    PrevMonthButton: TSpeedButton;
    PlanPanel: TPanel;
    ScheduleRadioButton: TRadioButton;
    PlanToolPanel: TPanel;
    SheetPanel: TPanel;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    ViewGrid: TsWorksheetGrid;
    VStaffListVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    SheetBottomPanel: TPanel;
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure NextDayButtonClick(Sender: TObject);
    procedure NextMonthButtonClick(Sender: TObject);
    procedure PlanButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure PrevDayButtonClick(Sender: TObject);
    procedure PrevMonthButtonClick(Sender: TObject);
    procedure ScheduleRadioButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
    procedure ViewGridDblClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanLoadAndDraw: Boolean;
    ZoomPercent: Integer;
    OrderType: Integer;

    Holidays: TDateVector;
    Calendar: TCalendar;
    Schedules: TPersonalScheduleVector;
    Sheet: TVacationPlanSheet;
    StatSheet: TVacationStatSheet;

    VStaffList: TVSTCheckTable;
    MStaffList: TVSTCategoryCheckTable;

    VTabNumIDs: TIntVector;
    VStaffNames, VTabNums, VPostNames, VScheduleNames: TStrVector;

    CategoryNames: TStrVector;
    MTabNumIDs: TIntMatrix;
    MStaffNames, MTabNums, MPostNames, MScheduleNames: TStrMatrix;

    TabNumIDs: TIntVector;
    StaffNames, TabNums, PostNames: TStrVector;
    Plan1Dates, Plan2Dates: TDateVector;
    Plan1Counts, Plan1AddCounts, Plan2Counts, Plan2AddCounts: TIntVector;

    procedure StaffListCreate;
    procedure StaffListLoad;
    procedure VStaffListSelect;
    procedure MStaffListSelect;

    function OrderTypeChange: Boolean;

    procedure PlanLoad;
    procedure PlanDraw(const AZoomPercent: Integer);
    procedure PlanRedraw;
    procedure PlanSelect;
    procedure PlanStat;
    procedure PlanLine(const ANeedColumnShow: Boolean);
    procedure PlanDateMove(const ADelta: Integer; const AMonth: Boolean);
    procedure PlanExport;

    procedure VacationLoad(const AIndex: Integer);
    procedure VacationPlanEditFormOpen;

    procedure LegendCreate;

    procedure SettingsSave;
    procedure SettingsLoad;
  public

  end;

var
  VacationPlanningForm: TVacationPlanningForm;

implementation

{$R *.lfm}

{ TVacationPlanningForm }

procedure TVacationPlanningForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[6];

  LegendCreate;

  CanLoadAndDraw:= False;
  StatSheet:= TVacationStatSheet.Create(StatGrid.Worksheet, StatGrid, GridFont);
  StaffListCreate;
  Calendar:= TCalendar.Create;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @PlanDraw, True);

  CanLoadAndDraw:= True;
end;

procedure TVacationPlanningForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;
  FreeAndNil(VStaffList);
  FreeAndNil(MStaffList);
  FreeAndNil(Calendar);
  if Assigned(Sheet) then FreeAndNil(Sheet);
  FreeAndNil(StatSheet);
  VSDel(Schedules);
end;

procedure TVacationPlanningForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, ListOrderToolPanel
  ]);
  SetCaptionPanels([
    ListCaptionPanel, PlanCaptionPanel, StatCaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton,
    EditButton, PrevMonthButton, PrevDayButton, NextDayButton, NextMonthButton
  ]);

  Images.ToButtons([
    PlanButton, ListButton, ExportButton,
    CloseButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton,
    EditButton, PrevMonthButton, PrevDayButton, NextDayButton, NextMonthButton
  ]);

  OrderType:= 0;
  StaffListLoad;
end;

procedure TVacationPlanningForm.ListButtonClick(Sender: TObject);
begin
  PlanToolPanel.Visible:= False;
  ListToolPanel.Visible:= True;
  PlanPanel.Visible:= False;
  PlanPanel.Align:= alBottom;
  ListPanel.Align:= alClient;
  ListPanel.Visible:= True;
end;

procedure TVacationPlanningForm.PlanButtonClick(Sender: TObject);
begin
  ListToolPanel.Visible:= False;
  PlanToolPanel.Visible:= True;
  ListPanel.Visible:= False;
  ListPanel.Align:= alBottom;
  PlanPanel.Align:= alClient;
  PlanPanel.Visible:= True;

  PlanLoad;
end;

procedure TVacationPlanningForm.FIORadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TVacationPlanningForm.PostRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TVacationPlanningForm.PrevDayButtonClick(Sender: TObject);
begin
  PlanDateMove(-1, False);
end;

procedure TVacationPlanningForm.PrevMonthButtonClick(Sender: TObject);
begin
  PlanDateMove(-1, True);
end;

procedure TVacationPlanningForm.NextDayButtonClick(Sender: TObject);
begin
  PlanDateMove(1, False);
end;

procedure TVacationPlanningForm.NextMonthButtonClick(Sender: TObject);
begin
  PlanDateMove(1, True);
end;

procedure TVacationPlanningForm.ScheduleRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TVacationPlanningForm.TabNumRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TVacationPlanningForm.StaffListCreate;
var
  i: Integer;
begin
  VStaffList:= TVSTCheckTable.Create(VStaffListVT);
  VStaffList.OnSelect:= @VStaffListSelect;
  VStaffList.StopSelectEventWhileCheckAll:= True;
  VStaffList.SetSingleFont(GridFont);
  VStaffList.SelectedBGColor:= VStaffListVT.Color;
  VStaffList.HeaderFont.Style:= [fsBold];
  for i:= 0 to High(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES) do
    VStaffList.AddColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[i],
                        VACATION_PLANNING_STAFFLIST_COLUMN_WIDTHS[i]);
  VStaffList.AutosizeColumnDisable;
  VStaffList.Draw;

  MStaffList:= TVSTCategoryCheckTable.Create(MStaffListVT);
  MStaffList.OnSelect:= @MStaffListSelect;
  MStaffList.TreeLinesVisible:= False;
  MStaffList.Span:= True;
  MStaffList.StopSelectEventWhileCheckAll:= True;
  MStaffList.SetSingleFont(GridFont);
  MStaffList.HeaderFont.Style:= [fsBold];
  for i:= 0 to High(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES) do
    MStaffList.AddColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[i],
                        VACATION_PLANNING_STAFFLIST_COLUMN_WIDTHS[i]);
  MStaffList.AutosizeColumnDisable;
  MStaffList.Draw;
end;

procedure TVacationPlanningForm.StaffListLoad;
  procedure CategoryListLoad;
  begin
    StaffListForVacationPlanningLoad(YearSpinEdit.Value, OrderType, CategoryNames,
                   MTabNumIDs, MStaffNames, MTabNums, MPostNames, MScheduleNames);

    MStaffList.SetCategories(CategoryNames);
    MStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[0], MStaffNames, taLeftJustify);
    MStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[1], MTabNums);
    MStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[2], MPostNames, taLeftJustify);
    MStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[3], MScheduleNames, taLeftJustify);
    MStaffList.Draw;
    MStaffList.ExpandAll(True);
    MStaffList.CheckAll(True);
    MStaffList.ShowFirst;
  end;

  procedure SimpleListLoad;
  begin
    StaffListForVacationPlanningLoad(YearSpinEdit.Value, OrderType,
                VTabNumIDs, VStaffNames, VTabNums, VPostNames, VScheduleNames);
    VStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[0], VStaffNames, taLeftJustify);
    VStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[1], VTabNums);
    VStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[2], VPostNames, taLeftJustify);
    VStaffList.SetColumn(VACATION_PLANNING_STAFFLIST_COLUMN_NAMES[3], VScheduleNames, taLeftJustify);
    VStaffList.Draw;
    VStaffList.CheckAll(True);
  end;

begin
  if not CanLoadAndDraw then Exit;

  PlanCaptionPanel.Caption:= 'График работы на ' + YearSpinEdit.Text + ' год';

  VStaffList.Visible:= False;
  MStaffList.Visible:= False;
  VStaffListVT.Align:= alBottom;
  MStaffListVT.Align:= alBottom;

  try
    if OrderType<=1 then
      CategoryListLoad
    else
      SimpleListLoad;
  finally
    if OrderType<=1 then
    begin
      MStaffListVT.Align:= alClient;
      MStaffList.Visible:= True;
    end
    else begin
      VStaffListVT.Align:= alClient;
      VStaffList.Visible:= True;
    end;
  end;
end;

procedure TVacationPlanningForm.VStaffListSelect;
begin
  PlanButton.Enabled:= VStaffList.IsSelected;
end;

procedure TVacationPlanningForm.MStaffListSelect;
begin
  PlanButton.Enabled:= MStaffList.IsSelected;
end;

function TVacationPlanningForm.OrderTypeChange: Boolean;
var
  NewOrderType: Byte;
begin
  Result:= False;
  if ScheduleRadioButton.Checked then
    NewOrderType:= 0
  else if PostRadioButton.Checked then
    NewOrderType:= 1
  else if FIORadioButton.Checked then
    NewOrderType:= 2
  else if TabNumRadioButton.Checked then
    NewOrderType:= 3;

  if OrderType=NewOrderType then Exit;

  OrderType:= NewOrderType;

  CollapseAllButton.Enabled:= OrderType in [0, 1];
  ExpandAllButton.Enabled:= CollapseAllButton.Enabled;

  Result:= True;
end;

procedure TVacationPlanningForm.PlanLoad;
var
  i: Integer;
  S: String;
  BD, ED: TDate;
  Progress: TProgress;

  procedure DataClear;
  begin
    if Assigned(Sheet) then
      FreeAndNil(Sheet);
    VSDel(Schedules);
    Plan1Dates:= nil;
    Plan2Dates:= nil;
    Plan1Counts:= nil;
    Plan1AddCounts:= nil;
    Plan2Counts:= nil;
    Plan2AddCounts:= nil;
  end;

  procedure GetCategoryValues;
  var
    Flags: TBoolMatrix;
  begin
    Flags:= MStaffList.Selected;
    TabNumIDs:= MToVector(MTabNumIDs, Flags);
    StaffNames:= MToVector(MStaffNames, Flags);
    PostNames:= MToVector(MPostNames, Flags);
    TabNums:= MToVector(MTabNums, Flags);
  end;

  procedure GetSimpleValues;
  var
    Flags: TBoolVector;
  begin
    Flags:= VStaffList.Checkeds;
    TabNumIDs:= VCut(VTabNumIDs, Flags);
    StaffNames:= VCut(VStaffNames, Flags);
    PostNames:= VCut(VPostNames, Flags);
    TabNums:= VCut(VTabNums, Flags);
  end;

  procedure DataPrepare;
  begin
    Holidays:= DataBase.HolidaysLoad(YearSpinEdit.Value);
    CalendarForYear(YearSpinEdit.Value, Calendar);

    Sheet:= TVacationPlanSheet.Create(Calendar, ViewGrid.Worksheet, ViewGrid, GridFont);
    Sheet.OnSelect:= @PlanSelect;
    Sheet.Zoom(ZoomPercent);

    SetLength(Schedules, Length(TabNumIDs));
    VDim(Plan1Dates, Length(TabNumIDs));
    VDim(Plan2Dates, Length(TabNumIDs));
    VDim(Plan1Counts, Length(TabNumIDs));
    VDim(Plan1AddCounts, Length(TabNumIDs));
    VDim(Plan2Counts, Length(TabNumIDs));
    VDim(Plan2AddCounts, Length(TabNumIDs));

    FirstLastDayInYear(YearSpinEdit.Value, BD, ED);
  end;

begin
  if not CanLoadAndDraw then Exit;

  DataClear;

  if OrderType<=1 then
    GetCategoryValues
  else
    GetSimpleValues;

  if VIsNil(TabNumIDs) then Exit;

  DataPrepare;

  Progress:= TProgress.Create(nil);
  try
    Progress.WriteLine1('Расчет графиков');
    Progress.WriteLine2(EmptyStr);
    Progress.Show;
    Sheet.BeginDraw;
    for i:= 0 to High(TabNumIDs) do
    begin
      //обновляем строку в окне прогресса
      S:= StaffNames[i] + ' [таб.№ ' + TabNums[i] + '] - ' + PostNames[i];
      Progress.WriteLine2(S);
      //рассчитываем графики
      Schedules[i]:= SchedulePersonalByCalendar(TabNumIDs[i], TabNums[i],
                              BD, ED, Calendar, Holidays, True{plan vacations});
      //данные о планируемых отпусках
      VacationLoad(i);
      //отрисовка плана
      Sheet.LineDraw(i, StaffNames[i], TabNums[i], Schedules[i],
                     Plan1Dates[i], Plan2Dates[i],
                     Plan1Counts[i], Plan1AddCounts[i], Plan2Counts[i], Plan2AddCounts[i]);
    end;
    Sheet.EndDraw(Length(StaffNames));
    StatSheet.Draw;
    PlanStat;
  finally
    FreeAndNil(Progress);
  end;
end;

procedure TVacationPlanningForm.PlanDraw(const AZoomPercent: Integer);
var
  i, j: Integer;
begin
  if not CanLoadAndDraw then Exit;
  if not Assigned(Sheet) then Exit;

  if Sheet.IsSelected then
  begin
    i:= Sheet.SelectedIndex;
    j:= Sheet.SelectedCol;
  end
  else begin
    i:= -1;
    j:= -1;
  end;

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(StaffNames, TabNums, Schedules, Plan1Dates, Plan2Dates,
               Plan1Counts, Plan1AddCounts, Plan2Counts, Plan2AddCounts);
    if i>=0 then
      Sheet.Select(i, j);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TVacationPlanningForm.PlanRedraw;
begin
  PlanDraw(ZoomPercent);
end;

procedure TVacationPlanningForm.PlanSelect;
begin
  PlanLine(True);
end;

procedure TVacationPlanningForm.PlanStat;
var
  V: TIntVector;
  i, m: Integer;
begin
  VDim(V{%H-}, 12);
  for i:= 0 to High(TabNumIDs) do
  begin
    if Plan1Dates[i]>0 then
    begin
      m:= MonthOf(Plan1Dates[i]) - 1;
      Inc(V[m]);
    end;
    if Plan2Dates[i]>0 then
    begin
      m:= MonthOf(Plan2Dates[i]) - 1;
      Inc(V[m]);
    end;
  end;
  StatSheet.Update(V);
end;

procedure TVacationPlanningForm.PlanLine(const ANeedColumnShow: Boolean);
var
  n: Integer;
  D: TDate;
  b: Boolean;
begin
  D:= 0;
  if Sheet.IsSelected then
  begin
    if Sheet.SelectedPart= 1 then
      D:= Plan1Dates[Sheet.SelectedIndex]
    else
      D:= Plan2Dates[Sheet.SelectedIndex];

    if ANeedColumnShow and (D>0) then
    begin
      ViewGrid.Visible:= False;
      try
        ViewGrid.Col:= 369;
        ViewGrid.Col:= 4 + DayNumberInYear(D);
      finally
        ViewGrid.Visible:= True;
      end;
    end;
  end;

  EditButton.Enabled:= Sheet.IsSelected;
  b:= Sheet.IsSelected and (D>0);
  PrevMonthButton.Enabled:= b and (MonthOf(D)>1);
  NextMonthButton.Enabled:= b and (MonthOf(D)<12);
  n:= Calendar.HoliDaysCount(Calendar.BeginDate, IncDay(D,-1));
  PrevDayButton.Enabled:= b and (DayNumberInYear(D)>1+n);
  n:= Calendar.HoliDaysCount(IncDay(D), Calendar.EndDate);
  NextDayButton.Enabled:= b and (DayNumberInYear(D)<DaysInYear(D)-n);
end;

procedure TVacationPlanningForm.PlanExport;
var
  i,j: Integer;
begin
  i:= 0;
  j:= 0;
  if Sheet.IsSelected then
  begin
    i:= Sheet.SelectedIndex;
    j:= Sheet.SelectedCol;
    Sheet.Unselect(False);
  end;
  Sheet.Save(YearSpinEdit.Text);
  if (i>0) and (j>0) then
    Sheet.Select(i, j);
end;

procedure TVacationPlanningForm.VacationLoad(const AIndex: Integer);
var
  Plan1Date, Plan2Date: TDate;
  Plan1Count, Plan1AddCount, Plan2Count, Plan2AddCount: Integer;
begin
  if not DataBase.VacationPlanLoad(YearSpinEdit.Value, TabNumIDs[AIndex],
                                   Plan1Date, Plan2Date,
                                   Plan1Count, Plan1AddCount,
                                   Plan2Count, Plan2AddCount) then Exit;
  Plan1Dates[AIndex]:= Plan1Date;
  Plan2Dates[AIndex]:= Plan2Date;
  Plan1Counts[AIndex]:= Plan1Count;
  Plan1AddCounts[AIndex]:= Plan1AddCount;
  Plan2Counts[AIndex]:= Plan2Count;
  Plan2AddCounts[AIndex]:= Plan2AddCount;
end;

procedure TVacationPlanningForm.VacationPlanEditFormOpen;
var
  i: Integer;
  S: String;
begin
  i:= Sheet.SelectedIndex;

  S:= StaffFullName(StaffNames[i], TabNums[i]);
  if VacationPlanEditFormShow(S, YearSpinEdit.Value, TabNumIDs[i],
                          Plan1Dates[i], Plan2Dates[i],
                          Plan1Counts[i], Plan1AddCounts[i],
                          Plan2Counts[i], Plan2AddCounts[i]) <> mrOK then Exit;

  VacationLoad(i);
  Sheet.VacationLineDraw(i, Plan1Dates[i], Plan2Dates[i], Plan1Counts[i],
                         Plan1AddCounts[i], Plan2Counts[i], Plan2AddCounts[i]);
  PlanSelect;
  PlanStat;
end;

procedure TVacationPlanningForm.PlanDateMove(const ADelta: Integer; const AMonth: Boolean);
var
  D: TDate;
  i, p: Integer;
begin
  if not Sheet.IsSelected then Exit;
  i:= Sheet.SelectedIndex;
  p:= Sheet.SelectedPart;

  if p=1 then
  begin
    if AMonth then
      D:= IncMonth(Plan1Dates[i], ADelta)
    else
      D:= IncDay(Plan1Dates[i], ADelta);
    Plan1Dates[i]:= D;
  end
  else begin
    if AMonth then
      D:= IncMonth(Plan2Dates[i], ADelta)
    else
      D:= IncDay(Plan2Dates[i], ADelta);
    Plan2Dates[i]:= D;
  end;

  if not DataBase.VacationPlanDateUpdate(YearSpinEdit.Value,
                                    TabNumIDs[i], p, D) then Exit;

  Sheet.VacationLineDraw(i, Plan1Dates[i], Plan2Dates[i], Plan1Counts[i],
                         Plan1AddCounts[i], Plan2Counts[i], Plan2AddCounts[i]);
  PlanLine(AMonth);
  PlanStat;
end;

procedure TVacationPlanningForm.LegendCreate;
var
  C: TColorVector;
  S: TStrVector;
begin
  C:= VCreateColor([
    COLORS_CALENDAR[DAY_STATUS_HOLIDAY],
    COLORS_CALENDAR[DAY_STATUS_OFFDAY],
    COLORS_CALENDAR[DAY_STATUS_BEFORE]
  ]);
  S:= VCreateStr([
    '- праздничный день',
    '- основной отпуск',
    '- дополнительный отпуск'
  ]);

  ColorLegendCreate(LegendPanel, C, S);
end;

procedure TVacationPlanningForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= nil;
  VAppend(SettingValues, ZoomPercent);
  DataBase.SettingsUpdate(SETTING_NAMES_VACATIONPLANNINGFORM, SettingValues);
end;

procedure TVacationPlanningForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_VACATIONPLANNINGFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TVacationPlanningForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TVacationPlanningForm.EditButtonClick(Sender: TObject);
begin
  VacationPlanEditFormOpen;
end;

procedure TVacationPlanningForm.ExportButtonClick(Sender: TObject);
begin
  PlanExport;
end;

procedure TVacationPlanningForm.ExpandAllButtonClick(Sender: TObject);
begin
  MStaffList.ExpandAll(True);
end;

procedure TVacationPlanningForm.CollapseAllButtonClick(Sender: TObject);
begin
  MStaffList.ExpandAll(False);
end;

procedure TVacationPlanningForm.CheckAllButtonClick(Sender: TObject);
begin
  if OrderType<=1 then
    MStaffList.CheckAll(True)
  else
    VStaffList.CheckAll(True);
end;

procedure TVacationPlanningForm.UncheckAllButtonClick(Sender: TObject);
begin
  if OrderType<=1 then
    MStaffList.CheckAll(False)
  else
    VStaffList.CheckAll(False);
end;

procedure TVacationPlanningForm.ViewGridDblClick(Sender: TObject);
begin
  if not Sheet.IsSelected then Exit;
  if Sheet.RowToIndex(Sheet.ClickedRow)<>Sheet.SelectedIndex then Exit;
  VacationPlanEditFormOpen;
end;

procedure TVacationPlanningForm.YearSpinEditChange(Sender: TObject);
begin
  StaffListLoad;
end;

end.

