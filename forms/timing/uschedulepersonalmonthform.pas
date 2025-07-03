unit USchedulePersonalMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Spin, StdCtrls, DividerBevel, VirtualTrees, fpspreadsheetgrid, DateUtils,
  BCButton,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Math, DK_Fonts, DK_Const, DK_DateUtils,
  DK_StrUtils, DK_VSTTables, DK_VSTParamList, DK_Zoom, DK_SheetExporter,
  DK_Progress, DK_CtrlUtils,
  //Project utils
  UVars, UConst, UTimingUtils, UCalendar, USchedule, UScheduleSheet,
  UWorkHours, UTypes,
  //Forms
  UScheduleCorrectionEditForm, UChooseForm;

type

  { TSchedulePersonalMonthForm }

  TSchedulePersonalMonthForm = class(TForm)
    CheckAllButton: TSpeedButton;
    CloseButton: TSpeedButton;
    DayEditButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    EditingButton: TSpeedButton;
    EditPanel: TPanel;
    ExportButton: TSpeedButton;
    SettingCaptionPanel: TPanel;
    ScheduleButton: TSpeedButton;
    ListCaptionPanel: TPanel;
    RowDownButton: TSpeedButton;
    RowMergeButton: TSpeedButton;
    RowUpButton: TSpeedButton;
    ListButton: TSpeedButton;
    SettingButton: TSpeedButton;
    ScheduleCaptionPanel: TPanel;
    SettingSplitter: TSplitter;
    RightPanel: TPanel;
    ScheduleToolPanel: TPanel;
    FIORadioButton: TRadioButton;
    ListToolPanel: TPanel;
    MonthBCButton: TBCButton;
    SchedulePanel: TPanel;
    ScheduleRadioButton: TRadioButton;
    ListOrderToolPanel: TPanel;
    ListPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    SheetPanel: TPanel;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    MStaffListVT: TVirtualStringTree;
    ViewGrid: TsWorksheetGrid;
    VStaffListVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DayEditButtonClick(Sender: TObject);
    procedure EditingButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure RowDownButtonClick(Sender: TObject);
    procedure RowMergeButtonClick(Sender: TObject);
    procedure RowUpButtonClick(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure ScheduleRadioButtonClick(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
    procedure ViewGridDblClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanLoadAndDraw: Boolean;
    ZoomPercent: Integer;
    MonthDropDown: TMonthDropDown;
    OrderType: Integer;

    Holidays: TDateVector;
    MonthCalendar, BeforeCalendar, YearCalendar: TCalendar;
    Schedules: TPersonalScheduleVector;
    BeforeSchedules: TPersonalScheduleVector; //для расчета часов за учетный период
    PostScheduleInfos: TPostScheduleInfoVector;
    Sheet: TPersonalMonthScheduleSheet;

    ParamList: TVSTParamList;

    VStaffList: TVSTCheckTable;
    MStaffList: TVSTCategoryCheckTable;

    VTabNumIDs: TIntVector;
    VStaffNames, VTabNums, VPostNames, VScheduleNames: TStrVector;
    VRecrutDates, VDismissDates, VPostBDs, VPostEDs, VScheduleBDs, VScheduleEDs: TDateVector;

    CategoryNames: TStrVector;
    MTabNumIDs: TIntMatrix;
    MStaffNames, MTabNums, MPostNames, MScheduleNames: TStrMatrix;
    MRecrutDates, MDismissDates, MPostBDs, MPostEDs, MScheduleBDs, MScheduleEDs: TDateMatrix;

    TabNumIDs, NormHours: TIntVector;
    StaffNames, TabNums, PostNames: TStrVector;
    RecrutDates, DismissDates, PostBDs, PostEDs, ScheduleBDs, ScheduleEDs: TDateVector;

    procedure ParamListCreate;

    procedure PeriodTypeSelect;
    function OrderTypeChange: Boolean;
    procedure EditButtonsEnabled;

    procedure StaffListCreate;
    procedure StaffListLoad;
    procedure VStaffListSelect;
    procedure MStaffListSelect;

    procedure ScheduleCreate(const AIndex: Integer);
    procedure ScheduleUpdate(const ATabNumID: Integer);
    procedure ScheduleLoad;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleSheetRecreate;
    procedure ScheduleSelect;
    procedure ScheduleExport;

    procedure ScheduleCorrectionFormOpen;

    procedure SelectionMove(const ADirection: TMoveDirection);
    procedure RowsMerge;

    procedure SettingsSave;
    procedure SettingsLoad;

    procedure ViewUpdate;
  public

  end;

var
  SchedulePersonalMonthForm: TSchedulePersonalMonthForm;

implementation

{$R *.lfm}

{ TSchedulePersonalMonthForm }

procedure TSchedulePersonalMonthForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[4];

  CanLoadAndDraw:= False;

  MonthCalendar:= TCalendar.Create;
  BeforeCalendar:= TCalendar.Create;
  YearCalendar:= TCalendar.Create;

  ParamListCreate;

  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, @StaffListLoad);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  StaffListCreate;

  CanLoadAndDraw:= True;
end;

procedure TSchedulePersonalMonthForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;

  FreeAndNil(ParamList);

  FreeAndNil(MonthCalendar);
  FreeAndNil(BeforeCalendar);
  FreeAndNil(YearCalendar);
  FreeAndNil(MonthDropDown);
  FreeAndNil(VStaffList);
  FreeAndNil(MStaffList);
  FreeAndNil(Sheet);

  VSDel(Schedules);
  VSDel(BeforeSchedules);
end;

procedure TSchedulePersonalMonthForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, ListOrderToolPanel
  ]);
  SetCaptionPanels([
    ListCaptionPanel, SettingCaptionPanel, ScheduleCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, CheckAllButton, UncheckAllButton, EditingButton, SettingButton,
    DayEditButton, RowUpButton, RowDownButton, RowMergeButton
  ]);

  Images.ToButtons([
    ExportButton, ScheduleButton, ListButton,
    CloseButton, CheckAllButton, UncheckAllButton, EditingButton, SettingButton,
    DayEditButton, RowUpButton, RowDownButton, RowMergeButton
  ]);

  MonthDropDown.AutoWidth;
  OrderType:= 0;
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.CheckAllButtonClick(Sender: TObject);
begin
  if OrderType<=1 then
    MStaffList.CheckAll(True)
  else
    VStaffList.CheckAll(True);
end;

procedure TSchedulePersonalMonthForm.UncheckAllButtonClick(Sender: TObject);
begin
  if OrderType<=1 then
    MStaffList.CheckAll(False)
  else
    VStaffList.CheckAll(False);
end;

procedure TSchedulePersonalMonthForm.ViewGridDblClick(Sender: TObject);
begin
  ScheduleCorrectionFormOpen;
end;

procedure TSchedulePersonalMonthForm.YearSpinEditChange(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.ParamListCreate;
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
    'учитывать отпуск'
  ]);
  ParamList.AddCheckList('ViewParams', S, V, @ScheduleRedraw);

  S:= 'Отображать в итогах количество:';
  V:= VCreateStr([
    'дней',
    'смен',
    'дней и смен'
  ]);
  ParamList.AddStringList('CountType', S, V, @ScheduleSheetRecreate);

  S:= 'Учетный период:';
  V:= VCreateStr([
    'год',
    'квартал',
    'месяц'
  ]);
  ParamList.AddStringList('PeriodType', S, V, @PeriodTypeSelect);

  S:= 'Дополнительные столбцы:';
  V:= VCreateStr([
    'Порядковый номер',
    'Должность (профессия)',
    'Табельный номер',
    'Количество дней/часов за месяц',
    'Сумма часов за учетный период',
    'Норма часов за учетный период',
    'Отклонение от нормы часов'
  ]);
  ParamList.AddCheckList('ExtraColumns', S, V, @ScheduleSheetRecreate);

  S:= 'Ознакомление с графиком:';
  V:= VCreateStr([
    'не выводить',
    'столбцы Дата/Подпись',
    'список под таблицей'
  ]);
  ParamList.AddStringList('SignType', S, V, @ScheduleSheetRecreate);

  S:= 'Параметры экспорта:';
  V:= VCreateStr([
    'заголовок таблицы на каждой странице',
    'номера страниц в нижнем колонтитуле'
  ]);
  ParamList.AddCheckList('ExportParams', S, V, nil);
end;

procedure TSchedulePersonalMonthForm.PeriodTypeSelect;
var
  i, d, h: Integer;
  BD, ED: TDate;
begin
  if not CanLoadAndDraw then Exit;

  Screen.Cursor:= crHandPoint;
  try
    AccountingPeriodWithMonth(MonthDropDown.Month, YearSpinEdit.Value,
                              ParamList.Selected['PeriodType'], BD, ED);
    for i:= 0 to High(TabNumIDs) do
    begin
      NormHoursAndWorkDaysCounInPeriod(TabNumIDs[i], BD, ED, YearCalendar, d, h);
      NormHours[i]:= h;
    end;

    if Length(BeforeSchedules)>0 then
    begin
      AccountingPeriodBeforeMonth(MonthDropDown.Month, YearSpinEdit.Value,
                              ParamList.Selected['PeriodType'], BD, ED);
      YearCalendar.Cut(BD, ED, BeforeCalendar);
      for i:= 0 to High(TabNumIDs) do
        BeforeSchedules[i]:= SchedulePersonalByCalendar(TabNumIDs[i], TabNums[i],
          RecrutDates[i], DismissDates[i], BeforeCalendar, Holidays, False{fact vacations},
          STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY);
    end;

    ScheduleSheetRecreate;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSchedulePersonalMonthForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSchedulePersonalMonthForm.DayEditButtonClick(Sender: TObject);
begin
  ScheduleCorrectionFormOpen;
end;

procedure TSchedulePersonalMonthForm.EditingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TSchedulePersonalMonthForm.ExportButtonClick(Sender: TObject);
begin
  ScheduleExport;
end;

procedure TSchedulePersonalMonthForm.ListButtonClick(Sender: TObject);
begin
  ScheduleToolPanel.Visible:= False;
  ListToolPanel.Visible:= True;
  SchedulePanel.Visible:= False;
  SchedulePanel.Align:= alBottom;
  ListPanel.Align:= alClient;
  ListPanel.Visible:= True;
  EditingButton.Down:= False;
  SettingButton.Down:= False;
  EditingButton.Visible:= False;
  SettingButton.Visible:= False;
  ViewUpdate;
end;

procedure TSchedulePersonalMonthForm.ScheduleButtonClick(Sender: TObject);
begin
  ListToolPanel.Visible:= False;
  ScheduleToolPanel.Visible:= True;
  ListPanel.Visible:= False;
  ListPanel.Align:= alBottom;
  SchedulePanel.Align:= alClient;
  SchedulePanel.Visible:= True;
  SettingButton.Visible:= True;
  EditingButton.Visible:= True;

  ScheduleLoad;
  ScheduleSheetRecreate;
  ViewUpdate;
  EditButtonsEnabled;
end;

procedure TSchedulePersonalMonthForm.FIORadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.PostRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.RowDownButtonClick(Sender: TObject);
begin
  SelectionMove(mdDown);
end;

procedure TSchedulePersonalMonthForm.RowMergeButtonClick(Sender: TObject);
begin
  RowsMerge;
end;

procedure TSchedulePersonalMonthForm.RowUpButtonClick(Sender: TObject);
begin
  SelectionMove(mdUp);
end;

procedure TSchedulePersonalMonthForm.ScheduleRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.SettingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TSchedulePersonalMonthForm.TabNumRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.StaffListCreate;
var
  i: Integer;
begin
  VStaffList:= TVSTCheckTable.Create(VStaffListVT);
  VStaffList.OnSelect:= @VStaffListSelect;
  VStaffList.StopSelectEventWhileCheckAll:= True;
  VStaffList.SetSingleFont(GridFont);
  VStaffList.SelectedBGColor:= VStaffListVT.Color;
  VStaffList.HeaderFont.Style:= [fsBold];
  for i:= 0 to High(TIMING_MONTH_STAFFLIST_COLUMN_NAMES) do
    VStaffList.AddColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[i],
                         TIMING_MONTH_STAFFLIST_COLUMN_WIDTHS[i]);
  VStaffList.AutosizeColumnDisable;
  VStaffList.Draw;

  MStaffList:= TVSTCategoryCheckTable.Create(MStaffListVT);
  MStaffList.OnSelect:= @MStaffListSelect;
  MStaffList.TreeLinesVisible:= False;
  MStaffList.StopSelectEventWhileCheckAll:= True;
  MStaffList.SetSingleFont(GridFont);
  MStaffList.HeaderFont.Style:= [fsBold];
  for i:= 0 to High(TIMING_MONTH_STAFFLIST_COLUMN_NAMES) do
    MStaffList.AddColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[i],
                         TIMING_MONTH_STAFFLIST_COLUMN_WIDTHS[i]);
  MStaffList.AutosizeColumnDisable;
  MStaffList.Draw;
end;

function TSchedulePersonalMonthForm.OrderTypeChange: Boolean;
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
  Result:= True;
end;

procedure TSchedulePersonalMonthForm.EditButtonsEnabled;
begin
  DayEditButton.Enabled:= Sheet.IsDateSelected;
  RowUpButton.Enabled:= Sheet.IsRowSelected and (Sheet.SelectedIndex>0);
  RowDownButton.Enabled:= Sheet.IsRowSelected and (Sheet.SelectedIndex<High(Schedules));
  RowMergeButton.Enabled:= Sheet.IsDoubleRowSelected;
end;

procedure TSchedulePersonalMonthForm.StaffListLoad;

  procedure CategoryListLoad;
  begin
    GetStaffListForCommonTiming(YearSpinEdit.Value, MonthDropDown.Month,
                     OrderType, CategoryNames, MTabNumIDs,
                     MStaffNames, MTabNums, MPostNames, MScheduleNames,
                     MRecrutDates, MDismissDates, MPostBDs, MPostEDs, MScheduleBDs, MScheduleEDs,
                     False{short names});
    MStaffList.SetCategories(CategoryNames);
    MStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[0], MStaffNames, taLeftJustify);
    MStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[1], MTabNums);
    MStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[2], MPostNames, taLeftJustify);
    MStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[3], MPeriodToStr(MPostBDs, MPostEDs), taLeftJustify);
    MStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[4], MScheduleNames, taLeftJustify);
    MStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[5], MPeriodToStr(MScheduleBDs, MScheduleEDs), taLeftJustify);
    MStaffList.Draw;
    MStaffList.ExpandAll(True);
    MStaffList.CheckAll(True);
    MStaffList.ShowFirst;
  end;

  procedure SimpleListLoad;
  begin
    GetStaffListForCommonTiming(YearSpinEdit.Value, MonthDropDown.Month,
                     OrderType, VTabNumIDs,
                     VStaffNames, VTabNums, VPostNames, VScheduleNames,
                     VRecrutDates, VDismissDates, VPostBDs, VPostEDs, VScheduleBDs, VScheduleEDs,
                     False{short names});
    VStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[0], VStaffNames, taLeftJustify);
    VStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[1], VTabNums);
    VStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[2], VPostNames, taLeftJustify);
    VStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[3], VPeriodToStr(VPostBDs, VPostEDs), taLeftJustify);
    VStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[4], VScheduleNames, taLeftJustify);
    VStaffList.SetColumn(TIMING_MONTH_STAFFLIST_COLUMN_NAMES[5], VPeriodToStr(VScheduleBDs, VScheduleEDs), taLeftJustify);
    VStaffList.Draw;
    VStaffList.CheckAll(True);
  end;

begin
  if not CanLoadAndDraw then Exit;

  ScheduleCaptionPanel.Caption:= '  График работы на ' + MonthDropDown.Text + ' ' +
                                 YearSpinEdit.Text + ' года';

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

procedure TSchedulePersonalMonthForm.VStaffListSelect;
begin
  ScheduleButton.Enabled:= VStaffList.IsSelected;
end;

procedure TSchedulePersonalMonthForm.MStaffListSelect;
begin
  ScheduleButton.Enabled:= MStaffList.IsSelected;
end;

procedure TSchedulePersonalMonthForm.ScheduleCreate(const AIndex: Integer);
var
  d, h: Integer;
  PeriodBD, PeriodED: TDate;
begin
  //Создаем график по Info
  Schedules[AIndex]:= SchedulePersonalByInfo(TabNumIDs[AIndex], TabNums[AIndex],
     RecrutDates[AIndex], DismissDates[AIndex],
     MonthCalendar, Holidays, PostScheduleInfos[AIndex], False{fact vacations},
     STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY);

  //график и норма часов учетного периода до текущего месяца
  AccountingPeriodWithMonth(MonthDropDown.Month, YearSpinEdit.Value,
                            ParamList.Selected['PeriodType'], PeriodBD, PeriodED);
  NormHoursAndWorkDaysCounInPeriod(TabNumIDs[AIndex], PeriodBD, PeriodED, YearCalendar, d, h);
  NormHours[AIndex]:= h;
  if Length(BeforeSchedules)=0 then Exit;
  BeforeSchedules[AIndex]:= SchedulePersonalByCalendar(TabNumIDs[AIndex], TabNums[AIndex],
     RecrutDates[AIndex], DismissDates[AIndex], BeforeCalendar, Holidays, False{fact vacations},
     STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY);
end;

procedure TSchedulePersonalMonthForm.ScheduleLoad;
var
  i: Integer;
  S: String;
  MonthBD, MonthED, BD, ED: TDate;
  Y, M: Word;
  PostScheduleInfo: TPostScheduleInfo;
  IsBeforePeriodExists: Boolean;
  Progress: TProgress;

  procedure GetCategoryValues;
  var
    Flags: TBoolMatrix;
  begin
    Flags:= MStaffList.Selected;
    TabNumIDs:= MToVector(MTabNumIDs, Flags);
    StaffNames:= MToVector(MStaffNames, Flags);
    PostNames:= MToVector(MPostNames, Flags);
    TabNums:= MToVector(MTabNums, Flags);
    RecrutDates:= MToVector(MRecrutDates, Flags);
    DismissDates:= MToVector(MDismissDates, Flags);
    PostBDs:= MToVector(MPostBDs, Flags);
    PostEDs:= MToVector(MPostEDs, Flags);
    ScheduleBDs:= MToVector(MScheduleBDs, Flags);
    ScheduleEDs:= MToVector(MScheduleEDs, Flags);
  end;

  procedure GetSimpleValues;
  var
    Flags: TBoolVector;
  begin
    Flags:= VStaffList.Selected;
    TabNumIDs:= VCut(VTabNumIDs, Flags);
    StaffNames:= VCut(VStaffNames, Flags);
    PostNames:= VCut(VPostNames, Flags);
    TabNums:= VCut(VTabNums, Flags);
    RecrutDates:= VCut(VRecrutDates, Flags);
    DismissDates:= VCut(VDismissDates, Flags);
    PostBDs:= VCut(VPostBDs, Flags);
    PostEDs:= VCut(VPostEDs, Flags);
    ScheduleBDs:= VCut(VScheduleBDs, Flags);
    ScheduleEDs:= VCut(VScheduleEDs, Flags);
  end;

begin
  if not CanLoadAndDraw then Exit;

  VSDel(Schedules);
  VSDel(BeforeSchedules);
  NormHours:= nil;
  PostScheduleInfos:= nil;

  if OrderType<=1 then
    GetCategoryValues
  else
    GetSimpleValues;

  if VIsNil(TabNumIDs) then Exit;

  Y:= YearSpinEdit.Value;
  M:= MonthDropDown.Month;
  Holidays:= DataBase.HolidaysLoad(Y);
  CalendarForYear(Y, YearCalendar);

  SetLength(PostScheduleInfos, Length(TabNumIDs));
  SetLength(Schedules, Length(TabNumIDs));
  SetLength(NormHours, Length(TabNumIDs));
  IsBeforePeriodExists:= (ParamList.Selected['PeriodType']<2 {учетный период<>месяц}) and
               AccountingPeriodBeforeMonth(M, Y, ParamList.Selected['PeriodType'], BD, ED);
  if IsBeforePeriodExists then
  begin
    SetLength(BeforeSchedules, Length(TabNumIDs));
    YearCalendar.Cut(BD, ED, BeforeCalendar);
  end;

  FirstLastDayInMonth(M, Y, MonthBD, MonthED);
  YearCalendar.Cut(MonthBD, MonthED, MonthCalendar);

  Progress:= TProgress.Create(nil);
  try
    Progress.WriteLine1('Расчет графиков');
    Progress.WriteLine2(EmptyStr);
    Progress.Show;
    for i:= 0 to High(TabNumIDs) do
    begin
      //обновляем строку в окне прогресса
      S:= StaffNames[i] + ' [таб.№ ' + TabNums[i] + '] - ' + PostNames[i];
      Progress.WriteLine2(S);
      //определяем период для загрузки инфо о должностях и графиках
      IsPeriodIntersect(ScheduleBDs[i], ScheduleEDs[i], PostBDs[i], PostEDs[i], BD, ED);
      IsPeriodIntersect(MonthBD, MonthED, BD, ED, BD, ED);
      IsPeriodIntersect(RecrutDates[i], DismissDates[i], BD, ED, BD, ED);
      //загружаем инфо о должностях и графиках
      DataBase.StaffPostScheduleInfoLoad(TabNumIDs[i], PostScheduleInfo, BD, ED);
      PostScheduleInfos[i]:= PostScheduleInfo;
      //рассчитываем графики
      ScheduleCreate(i);
    end;
  finally
    FreeAndNil(Progress);
  end;
end;

procedure TSchedulePersonalMonthForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  if not CanLoadAndDraw then Exit;

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(MonthCalendar,
               StaffNames, TabNums, PostNames, NormHours,
               ParamList.Checkeds['ViewParams'],
               ParamList.Checkeds['ExportParams'],
               Schedules, BeforeSchedules);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSchedulePersonalMonthForm.ScheduleRedraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TSchedulePersonalMonthForm.ScheduleSheetRecreate;
begin
  if not CanLoadAndDraw then Exit;

  if Assigned(Sheet) then FreeAndNil(Sheet);
  Sheet:= TPersonalMonthScheduleSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont,
     ParamList.Selected['CountType'],
     ParamList.Selected['PeriodType'],
     ParamList.Selected['SignType'],
     ParamList.Checkeds['ExtraColumns']);
  Sheet.CanSelect:= EditingButton.Down;
  Sheet.OnSelect:= @ScheduleSelect;

  ScheduleRedraw;
end;

procedure TSchedulePersonalMonthForm.ScheduleSelect;
begin
  EditButtonsEnabled;
end;

procedure TSchedulePersonalMonthForm.ScheduleExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TPersonalMonthScheduleSheet;
begin
  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet(SUpper(MonthDropDown.Text) + ' ' + YearSpinEdit.Text);
    ExpSheet:= TPersonalMonthScheduleSheet.Create(Worksheet, nil, GridFont,
                               ParamList.Selected['CountType'],
                               ParamList.Selected['PeriodType'],
                               ParamList.Selected['SignType'],
                               ParamList.Checkeds['ExtraColumns']);
    try
      ExpSheet.Draw(MonthCalendar,
               StaffNames, TabNums, PostNames, NormHours,
               ParamList.Checkeds['ViewParams'],
               ParamList.Checkeds['ExportParams'],
               Schedules, BeforeSchedules);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSchedulePersonalMonthForm.ScheduleCorrectionFormOpen;
var
  i, TabNumID: Integer;
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;
  CorrectIDs: TIntVector;
  Corrections: TScheduleCorrections;
begin
  if not Sheet.IsDateSelected then Exit;

  TabNumID:= TabNumIDs[Sheet.SelectedIndex];

  ScheduleCorrectionEditForm:= TScheduleCorrectionEditForm.Create(nil);
  try
    ScheduleCorrectionEditForm.TabNumID:= TabNumID;
    ScheduleCorrectionEditForm.FirstDatePicker.Date:= Sheet.SelectedDate;
    if DataBase.SchedulePersonalCorrectionsLoad(TabNumID, CorrectIDs, Corrections,
                                Sheet.SelectedDate, Sheet.SelectedDate) then
    begin
      ScheduleCorrectionEditForm.DigMark:= Corrections.DigMarks[0];
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursTotal[0]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursNight[0]);
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Corrections.ShiftNums[0];
    end
    else begin
      i:= DayOf(Sheet.SelectedDate)-1;
      ScheduleCorrectionEditForm.DigMark:= Schedules[Sheet.SelectedIndex].MarkDIGDefault[i];
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Schedules[Sheet.SelectedIndex].HoursDefault.Totals[i]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Schedules[Sheet.SelectedIndex].HoursDefault.Nights[i]);
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Schedules[Sheet.SelectedIndex].ShiftNumbersDefault[i];
    end;

    if ScheduleCorrectionEditForm.ShowModal=mrOK then
      ScheduleUpdate(TabNumID);
  finally
    FreeAndNil(ScheduleCorrectionEditForm);
  end;
end;

procedure TSchedulePersonalMonthForm.ScheduleUpdate(const ATabNumID: Integer);
var
  i: Integer;
begin
  for i:= 0 to High(TabNumIDs) do
  begin
    if TabNumIDs[i]<>ATabNumID then continue;
    FreeAndNil(Schedules[i]);
    if Length(BeforeSchedules)>0 then
       FreeAndNil(BeforeSchedules[i]);
    ScheduleCreate(i);
    Sheet.LineDraw(i);
  end;
end;

procedure TSchedulePersonalMonthForm.SelectionMove(const ADirection: TMoveDirection);
var
  OldSelectedIndex, NewSelectedIndex: Integer;
begin
  if ADirection=mdUp then
    NewSelectedIndex:= Sheet.SelectedIndex - 1
  else if ADirection=mdDown then
    NewSelectedIndex:= Sheet.SelectedIndex + 1
  else Exit;
  OldSelectedIndex:= Sheet.SelectedIndex;

  VSwap(StaffNames, OldSelectedIndex, NewSelectedIndex);
  VSwap(PostNames, OldSelectedIndex, NewSelectedIndex);
  VSwap(TabNumIDs, OldSelectedIndex, NewSelectedIndex);
  VSwap(TabNums, OldSelectedIndex, NewSelectedIndex);
  VSwap(NormHours, OldSelectedIndex, NewSelectedIndex);
  VSwap(PostBDs, OldSelectedIndex, NewSelectedIndex);
  VSwap(PostEDs, OldSelectedIndex, NewSelectedIndex);
  VSwap(ScheduleBDs, OldSelectedIndex, NewSelectedIndex);
  VSwap(ScheduleEDs, OldSelectedIndex, NewSelectedIndex);
  VSwap(RecrutDates, OldSelectedIndex, NewSelectedIndex);
  VSwap(DismissDates, OldSelectedIndex, NewSelectedIndex);
  VSSwap(BeforeSchedules, OldSelectedIndex, NewSelectedIndex);
  VSSwap(Schedules, OldSelectedIndex, NewSelectedIndex);
  VISwap(PostScheduleInfos, OldSelectedIndex, NewSelectedIndex);

  Sheet.SelectionMove(NewSelectedIndex);
end;

procedure TSchedulePersonalMonthForm.RowsMerge;
var
  ChooseIndex1, ChooseIndex2: Integer;
  V1, V2: TStrVector;
  RowIndexes: TIntVector;
  PostName: String;

  procedure Merge(const AResultIndex, ADeleteIndex: Integer; const APostName: String);
  begin
    //должность, в результирующей строке
    PostNames[AResultIndex]:= APostName;
    //Info графика в результирующей строке
    PostScheduleInfoAdd(PostScheduleInfos[AResultIndex], PostScheduleInfos[ADeleteIndex]);
    //график в результирующей строке
    FreeAndNil(Schedules[AResultIndex]);
    Schedules[AResultIndex]:= SchedulePersonalByInfo(TabNumIDs[AResultIndex],
      TabNums[AResultIndex], RecrutDates[AResultIndex], DismissDates[AResultIndex],
      MonthCalendar, Holidays, PostScheduleInfos[AResultIndex], False{fact vacations},
      STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY);

    //удаляем элементы векторов
    VDel(StaffNames, ADeleteIndex);
    VDel(PostNames, ADeleteIndex);
    VDel(TabNumIDs, ADeleteIndex);
    VDel(TabNums, ADeleteIndex);
    VDel(NormHours, ADeleteIndex);
    VDel(PostBDs, ADeleteIndex);
    VDel(PostEDs, ADeleteIndex);
    VDel(ScheduleBDs, ADeleteIndex);
    VDel(ScheduleEDs, ADeleteIndex);
    VDel(RecrutDates, ADeleteIndex);
    VDel(DismissDates, ADeleteIndex);
    VSDel(BeforeSchedules, ADeleteIndex);
    VSDel(Schedules, ADeleteIndex);
    VIDel(PostScheduleInfos, ADeleteIndex);
    ScheduleRedraw;
    Sheet.Select(AResultIndex - Ord(AResultIndex>ADeleteIndex));
  end;

begin
  RowIndexes:= VCreateInt([
    Min(Sheet.SelectedIndex, Sheet.SelectedIndex2),
    Max(Sheet.SelectedIndex, Sheet.SelectedIndex2)
  ]);

  V1:= VCreateStr(['№ ' + IntToStr(RowIndexes[0]+1), '№ ' + IntToStr(RowIndexes[1]+1)]);
  V2:= nil;
  if not SSame(PostNames[RowIndexes[0]], PostNames[RowIndexes[1]]) then
    V2:= VCreateStr([PostNames[RowIndexes[0]], PostNames[RowIndexes[1]]]);
  if not Choose('Записать объединенные данные в строку:',
                'Записать в результирующую строку должность (профессию):',
                V1, V2, ChooseIndex1, ChooseIndex2) then Exit;

  if ChooseIndex2>=0 then
    PostName:= PostNames[RowIndexes[ChooseIndex2]]
  else
    PostName:= PostNames[RowIndexes[0]];

  if ChooseIndex1=1 then
    VSwap(RowIndexes, 0, 1);

  Screen.Cursor:= crHourGlass;
  try
    Merge(RowIndexes[0], RowIndexes[1], PostName);
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSchedulePersonalMonthForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= nil;
  VAppend(SettingValues, ZoomPercent);
  SettingValues:= VAdd(SettingValues, ParamList.Params);
  DataBase.SettingsUpdate(SETTING_NAMES_SCHEDULEPERSONALMONTHFORM, SettingValues);
end;

procedure TSchedulePersonalMonthForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SCHEDULEPERSONALMONTHFORM);
  ZoomPercent:= SettingValues[0];
  ParamList.Params:= VCut(SettingValues, 1);
end;

procedure TSchedulePersonalMonthForm.ViewUpdate;
begin
  EditPanel.Visible:= EditingButton.Down;
  Sheet.CanSelect:= EditingButton.Down;

  if SettingButton.Down then
  begin
    SettingPanel.Visible:= True;
    SettingSplitter.Visible:= True;
    SheetPanel.AnchorToNeighbour(akLeft, 0, SettingSplitter);
    ScheduleCaptionPanel.AnchorToNeighbour(akLeft, 0, SettingSplitter);
  end
  else begin
    SettingSplitter.Visible:= False;
    SettingPanel.Visible:= False;
    SheetPanel.AnchorToNeighbour(akLeft, 2, Self);
    ScheduleCaptionPanel.AnchorToNeighbour(akLeft, 2, Self);
  end;
end;

end.

