unit UTimetableMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  Buttons, Spin, StdCtrls, DividerBevel, VirtualTrees,
  fpspreadsheetgrid, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Math, DK_Fonts, DK_Const, DK_DateUtils,
  DK_StrUtils, DK_VSTTables, DK_VSTParamList, DK_Zoom, DK_SheetExporter,
  DK_Progress, DK_CtrlUtils,
  //Project utils
  UDataBase, UConst, UTimingUtils, UImages, UCalendar, USchedule, UTimetable,
  UTimetableSheet, UTimingSheet, UTypes,
  //Forms
  UTimetableEditForm, UChooseForm;

type

  { TTimetableMonthForm }

  TTimetableMonthForm = class(TForm)
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
    ListButton: TSpeedButton;
    ListCaptionPanel: TPanel;
    TimetableButton: TSpeedButton;
    SettingCaptionPanel: TPanel;
    RowDownButton: TSpeedButton;
    RowMergeButton: TSpeedButton;
    RowUpButton: TSpeedButton;
    SettingButton: TSpeedButton;
    TimetableCaptionPanel: TPanel;
    SettingSplitter: TSplitter;
    RightPanel: TPanel;
    TimetableToolPanel: TPanel;
    FIORadioButton: TRadioButton;
    ListToolPanel: TPanel;
    MonthBCButton: TBCButton;
    TimetablePanel: TPanel;
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
    procedure TimetableButtonClick(Sender: TObject);
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

    Holidays: TDateVector;
    BeforeTotalHours, BeforeNightHours: TIntVector;
    MonthCalendar, YearCalendar: TCalendar;
    Timetables: TTimetableVector;
    PostScheduleInfos: TPostScheduleInfoVector;

    Sheet: TMonthTimetableSheet;
    SheetT12: TTimetableSheetT12;
    SheetT13: TTimetableSheetT13;

    procedure ParamListCreate;
    procedure ParamListVisibles;

    procedure TimetableTypeChange;
    procedure PeriodTypeSelect;
    function OrderTypeChange: Boolean;
    procedure EditButtonsEnabled;

    procedure StaffListCreate;
    procedure StaffListLoad;
    procedure VStaffListSelect;
    procedure MStaffListSelect;

    procedure TimetableUpdate(const ATabNumID: Integer);
    procedure TimetableLoad;
    procedure TimetableSheetCreate;
    procedure TimetableSheetRecreate;
    procedure TimetableDraw(const AZoomPercent: Integer);
    procedure TimetableRedraw;
    procedure TimetableSelect;
    procedure TimetableExport;

    procedure TimetableEditFormOpen;

    function ActiveSheet: TMonthCustomSheet;
    procedure SheetsFree;

    procedure SelectionMove(const ADirection: TMoveDirection);
    procedure RowsMerge;

    procedure SettingsSave;
    procedure SettingsLoad;

    procedure ViewUpdate;
  public

  end;

var
  TimetableMonthForm: TTimetableMonthForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TTimetableMonthForm }

procedure TTimetableMonthForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[5];

  SetToolPanels([
    ToolPanel, ListOrderToolPanel
  ]);
  SetCaptionPanels([
    ListCaptionPanel, SettingCaptionPanel, TimetableCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, CheckAllButton, UncheckAllButton, EditingButton, SettingButton,
    DayEditButton, RowUpButton, RowDownButton, RowMergeButton
  ]);

  Images.ToButtons([
    ExportButton, TimetableButton, ListButton,
    CloseButton, CheckAllButton, UncheckAllButton, EditingButton, SettingButton,
    DayEditButton, RowUpButton, RowDownButton, RowMergeButton
  ]);

  CanLoadAndDraw:= False;

  MonthCalendar:= TCalendar.Create;
  YearCalendar:= TCalendar.Create;

  ParamListCreate;

  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, @StaffListLoad);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @TimetableDraw, True);

  StaffListCreate;

  CanLoadAndDraw:= True;
end;

procedure TTimetableMonthForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;

  FreeAndNil(ParamList);

  FreeAndNil(MonthCalendar);
  FreeAndNil(YearCalendar);
  FreeAndNil(MonthDropDown);
  FreeAndNil(VStaffList);
  FreeAndNil(MStaffList);

  SheetsFree;

  VTDel(Timetables);
end;

procedure TTimetableMonthForm.FormShow(Sender: TObject);
begin
  ParamListVisibles; //ParamList.Show;
  MonthDropDown.AutoWidth;
  OrderType:= 0;
  StaffListLoad;
end;

procedure TTimetableMonthForm.CheckAllButtonClick(Sender: TObject);
begin
  if OrderType<=1 then
    MStaffList.CheckAll(True)
  else
    VStaffList.CheckAll(True);
end;

procedure TTimetableMonthForm.UncheckAllButtonClick(Sender: TObject);
begin
  if OrderType<=1 then
    MStaffList.CheckAll(False)
  else
    VStaffList.CheckAll(False);
end;

procedure TTimetableMonthForm.ViewGridDblClick(Sender: TObject);
begin
  TimetableEditFormOpen;
end;

procedure TTimetableMonthForm.YearSpinEditChange(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TTimetableMonthForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= 'Форма табеля:';
  V:= VCreateStr([
    'график',
    'Т-12',
    'Т-13'
  ]);
  ParamList.AddStringList('TimetableType', S, V, @TimetableTypeChange);

  S:= 'Вид табеля:';
  V:= VCreateStr([
    'форма',
    'таблица'
  ]);
  ParamList.AddStringList('ViewType', S, V, @TimetableRedraw);

  S:= 'Отображать табель за:';
  V:= VCreateStr([
    'половину месяца',
    'весь месяц'
  ]);
  ParamList.AddStringList('MonthType', S, V, @TimetableRedraw);

  S:= VIEW_PARAMS_CAPTION;
  V:= VCreateStr([
    'отображать строку ночных часов',
    'коды табеля для нерабочих дней'
  ]);
  ParamList.AddCheckList('ViewParams', S, V, @TimetableRedraw);

  S:= 'Учетный период:';
  V:= VCreateStr([
    'год',
    'квартал',
    'месяц'
  ]);
  ParamList.AddStringList('PeriodType', S, V, @TimetableSheetRecreate);

  S:= 'Отображать в итогах количество:';
  V:= VCreateStr([
    'дней',
    'смен',
    'дней и смен'
  ]);
  ParamList.AddStringList('CountType', S, V, @TimetableSheetRecreate);

  S:= 'Дополнительные столбцы:';
  V:= VCreateStr([
    'Порядковый номер',
    'Должность (профессия)',
    'Табельный номер',
    'Количество дней/часов за месяц',
    'Сумма часов за учетный период',
    'Норма часов за учетный период',
    'Отклонение от нормы часов',
    'Ночные часы'
  ]);
  ParamList.AddCheckList('ExtraColumns', S, V, @TimetableSheetRecreate);

  S:= 'Параметры экспорта:';
  V:= VCreateStr([
    'заголовок таблицы на каждой странице',
    'номера страниц в нижнем колонтитуле'
  ]);
  ParamList.AddCheckList('ExportParams', S, V, nil);

  S:= 'Вид обрамляющих линий таблицы:';
  V:= VCreateStr([
    'простые',
    'утолщенные',
    'двойные'
  ]);
  ParamList.AddStringList('LineType', S, V, nil);
end;

procedure TTimetableMonthForm.ParamListVisibles;
begin
  SettingClientPanel.Visible:= False;
  if ParamList.Selected['TimetableType']=0 then
  begin
    ParamList.Visibles['ViewType']:= False;
    ParamList.Visibles['ViewParams']:= True;
    ParamList.Visibles['PeriodType']:= True;
    ParamList.Visibles['CountType']:= True;
    ParamList.Visibles['ExtraColumns']:= True;
    ParamList.Visibles['LineType']:= False;
  end else
  begin
    ParamList.Visibles['ViewType']:= True;
    ParamList.Visibles['ViewParams']:= False;
    ParamList.Visibles['PeriodType']:= False;
    ParamList.Visibles['CountType']:= False;
    ParamList.Visibles['ExtraColumns']:= False;
    ParamList.Visibles['LineType']:= True;
  end;
  SettingClientPanel.Visible:= True;
end;

procedure TTimetableMonthForm.TimetableTypeChange;
begin
  if not CanLoadAndDraw then Exit;
  ParamListVisibles;
  TimetableSheetRecreate;
end;

procedure TTimetableMonthForm.PeriodTypeSelect;
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

    if Length(BeforeTotalHours)>0 then
    begin
      AccountingPeriodBeforeMonth(MonthDropDown.Month, YearSpinEdit.Value,
                                  ParamList.Selected['PeriodType'], BD, ED);
      for i:=0 to High(TabNumIDs) do
      begin
        BeforeTotalHours[i]:= DataBase.TimetableSumTotalHoursInPeriodLoad(TabNumIDs[i], BD, ED);
        BeforeNightHours[i]:= DataBase.TimetableSumNightHoursInPeriodLoad(TabNumIDs[i], BD, ED);
      end;
    end;

    TimetableSheetRecreate;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TTimetableMonthForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTimetableMonthForm.DayEditButtonClick(Sender: TObject);
begin
  TimetableEditFormOpen;
end;

procedure TTimetableMonthForm.EditingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TTimetableMonthForm.ExportButtonClick(Sender: TObject);
begin
  TimetableExport;
end;

procedure TTimetableMonthForm.ListButtonClick(Sender: TObject);
begin
  TimetableToolPanel.Visible:= False;
  ListToolPanel.Visible:= True;
  TimetablePanel.Visible:= False;
  TimetablePanel.Align:= alBottom;
  ListPanel.Align:= alClient;
  ListPanel.Visible:= True;
  EditingButton.Down:= False;
  SettingButton.Down:= False;
  EditingButton.Visible:= False;
  SettingButton.Visible:= False;
  ViewUpdate;
end;

procedure TTimetableMonthForm.TimetableButtonClick(Sender: TObject);
begin
  ListToolPanel.Visible:= False;
  TimetableToolPanel.Visible:= True;
  ListPanel.Visible:= False;
  ListPanel.Align:= alBottom;
  TimetablePanel.Align:= alClient;
  TimetablePanel.Visible:= True;
  SettingButton.Visible:= True;
  EditingButton.Visible:= True;

  TimetableLoad;
  TimetableSheetRecreate;
  ViewUpdate;
  EditButtonsEnabled;
end;

procedure TTimetableMonthForm.FIORadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TTimetableMonthForm.PostRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TTimetableMonthForm.RowDownButtonClick(Sender: TObject);
begin
  SelectionMove(mdDown);
end;

procedure TTimetableMonthForm.RowMergeButtonClick(Sender: TObject);
begin
  RowsMerge;
end;

procedure TTimetableMonthForm.RowUpButtonClick(Sender: TObject);
begin
  SelectionMove(mdUp);
end;

procedure TTimetableMonthForm.ScheduleRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TTimetableMonthForm.SettingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TTimetableMonthForm.TabNumRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
end;

procedure TTimetableMonthForm.StaffListCreate;
begin
  VStaffList:= TVSTCheckTable.Create(VStaffListVT);
  VStaffList.OnSelect:= @VStaffListSelect;
  VStaffList.StopSelectEventWhileCheckAll:= True;
  VStaffList.SetSingleFont(MainForm.GridFont);
  VStaffList.SelectedBGColor:= VStaffListVT.Color;
  VStaffList.HeaderFont.Style:= [fsBold];
  VStaffList.AddColumn('Фамилия И.О.', 200);
  VStaffList.AddColumn('Табельный номер', 150);
  VStaffList.AddColumn('Должность', 300);
  VStaffList.AddColumn('В должности', 150);
  VStaffList.AddColumn('График', 200);
  VStaffList.AddColumn('В графике', 150);
  VStaffList.AutosizeColumnDisable;
  VStaffList.Draw;

  MStaffList:= TVSTCategoryCheckTable.Create(MStaffListVT);
  MStaffList.OnSelect:= @MStaffListSelect;
  MStaffList.TreeLinesVisible:= False;
  MStaffList.StopSelectEventWhileCheckAll:= True;
  MStaffList.SetSingleFont(MainForm.GridFont);
  MStaffList.HeaderFont.Style:= [fsBold];
  MStaffList.AddColumn('Фамилия И.О.', 200);
  MStaffList.AddColumn('Табельный номер', 150);
  MStaffList.AddColumn('Должность', 300);
  MStaffList.AddColumn('В должности', 150);
  MStaffList.AddColumn('График', 200);
  MStaffList.AddColumn('В графике', 150);
  MStaffList.AutosizeColumnDisable;
  MStaffList.Draw;
end;

function TTimetableMonthForm.OrderTypeChange: Boolean;
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

procedure TTimetableMonthForm.EditButtonsEnabled;
var
  S: TMonthCustomSheet;
begin
  S:= ActiveSheet;
  DayEditButton.Enabled:= S.IsDateSelected;
  RowUpButton.Enabled:= S.IsRowSelected and (S.SelectedIndex>0);
  RowDownButton.Enabled:= S.IsRowSelected and (S.SelectedIndex<High(Timetables));
  RowMergeButton.Enabled:= S.IsDoubleRowSelected;
end;

procedure TTimetableMonthForm.StaffListLoad;

  procedure CategoryListLoad;
  begin
    GetStaffListForCommonTiming(YearSpinEdit.Value, MonthDropDown.Month,
                     OrderType, CategoryNames, MTabNumIDs,
                     MStaffNames, MTabNums, MPostNames, MScheduleNames,
                     MRecrutDates, MDismissDates, MPostBDs, MPostEDs, MScheduleBDs, MScheduleEDs,
                     False{short names});
    MStaffList.SetCategories(CategoryNames);
    MStaffList.SetColumn('Фамилия И.О.', MStaffNames, taLeftJustify);
    MStaffList.SetColumn('Табельный номер', MTabNums);
    MStaffList.SetColumn('Должность', MPostNames, taLeftJustify);
    MStaffList.SetColumn('В должности', MPeriodToStr(MPostBDs, MPostEDs), taLeftJustify);
    MStaffList.SetColumn('График', MScheduleNames, taLeftJustify);
    MStaffList.SetColumn('В графике', MPeriodToStr(MScheduleBDs, MScheduleEDs), taLeftJustify);
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
    VStaffList.SetColumn('Фамилия И.О.', VStaffNames, taLeftJustify);
    VStaffList.SetColumn('Табельный номер', VTabNums);
    VStaffList.SetColumn('Должность', VPostNames, taLeftJustify);
    VStaffList.SetColumn('В должности', VPeriodToStr(VPostBDs, VPostEDs), taLeftJustify);
    VStaffList.SetColumn('График', VScheduleNames, taLeftJustify);
    VStaffList.SetColumn('В графике', VPeriodToStr(VScheduleBDs, VScheduleEDs), taLeftJustify);
    VStaffList.Draw;
    VStaffList.CheckAll(True);
  end;

begin
  if not CanLoadAndDraw then Exit;

  TimetableCaptionPanel.Caption:= '  Табель учета рабочего времени за ' + MonthDropDown.Text + ' ' +
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

procedure TTimetableMonthForm.VStaffListSelect;
begin
  TimetableButton.Enabled:= VStaffList.IsSelected;
end;

procedure TTimetableMonthForm.MStaffListSelect;
begin
  TimetableButton.Enabled:= MStaffList.IsSelected;
end;

procedure TTimetableMonthForm.TimetableUpdate(const ATabNumID: Integer);
var
  i: Integer;
  S: TMonthCustomSheet;
begin
  S:= ActiveSheet;
  for i:= 0 to High(TabNumIDs) do
  begin
    if TabNumIDs[i]<>ATabNumID then continue;
    Timetables[i].Calc(TabNumIDs[i], TabNums[i], RecrutDates[i], DismissDates[i],
                     MonthCalendar, PostScheduleInfos[i]);
    S.LineDraw(i);
  end;
end;

procedure TTimetableMonthForm.TimetableLoad;
var
  Y, M: Word;
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

  procedure TimetablesCalc;
  var
    i: Integer;
    S: String;
    BD, ED, MonthBD, MonthED: TDate;
    Timetable: TTimetable;
    PostScheduleInfo: TPostScheduleInfo;
  begin
    FirstLastDayInMonth(M, Y, MonthBD, MonthED);
    YearCalendar.Cut(MonthBD, MonthED, MonthCalendar);
    Progress.WriteLine1('Расчет табелей');
    for i:=0 to High(TabNumIDs) do
    begin
      S:= StaffNames[i] + ' [таб.№ ' + TabNums[i] + '] - ' + PostNames[i];
      Progress.WriteLine2(S);
      //определяем период работы в этом месяце
      IsPeriodIntersect(RecrutDates[i], DismissDates[i], MonthBD, MonthED, BD, ED);
      //обновляем данные о табеле в базе за этот период
      TimetableForPeriodUpdate(TabNumIDs[i], RecrutDates[i], DismissDates[i],
                               BD, ED, Holidays, False);
      //определяем период для загрузки инфо о должностях и графиках
      IsPeriodIntersect(ScheduleBDs[i], ScheduleEDs[i], BD, ED, BD, ED);
      IsPeriodIntersect(PostBDs[i], PostEDs[i], BD, ED, BD, ED);
      //загружаем инфо о должностях и графиках
      DataBase.StaffPostScheduleInfoLoad(TabNumIDs[i], PostScheduleInfo, BD, ED);
      VIAppend(PostScheduleInfos, PostScheduleInfo);
      //создаем и записываем табель в вектор
      Timetable:= TTimetable.Create;
      Timetable.Calc(TabNumIDs[i], TabNums[i], RecrutDates[i], DismissDates[i],
                     MonthCalendar, PostScheduleInfo);
      VTAppend(Timetables, Timetable);
    end;
  end;

  procedure NormHoursCalc;
  var
    i, Days, Hours: Integer;
    BD, ED: TDate;
  begin
    Progress.WriteLine1('Расчет нормы часов');
    Progress.WriteLine2(EmptyStr);
    VDim(NormHours, Length(TabNumIDs));
    AccountingPeriodWithMonth(M, Y, ParamList.Selected['PeriodType'], BD, ED);
    for i:=0 to High(TabNumIDs) do
    begin
      NormHoursAndWorkDaysCounInPeriod(TabNumIDs[i], BD, ED, YearCalendar, Days, Hours);
      NormHours[i]:= Hours;
    end;
  end;

  procedure BeforeHoursCalc;
  var
    IsBeforePeriodExists: Boolean;
    i: Integer;
    BD, ED: TDate;
  begin
    Progress.WriteLine1('Расчет отработанных часов');
    Progress.WriteLine2(EmptyStr);

    BeforeTotalHours:= nil;
    BeforeNightHours:= nil;
    IsBeforePeriodExists:= (ParamList.Selected['PeriodType']<2 {учетный период<>месяц}) and
                 AccountingPeriodBeforeMonth(M, Y, ParamList.Selected['PeriodType'], BD, ED);
    if not IsBeforePeriodExists then Exit;

    VDim(BeforeTotalHours, Length(TabNumIDs));
    VDim(BeforeNightHours, Length(TabNumIDs));
    for i:=0 to High(TabNumIDs) do
    begin
      BeforeTotalHours[i]:= DataBase.TimetableSumTotalHoursInPeriodLoad(TabNumIDs[i], BD, ED);
      BeforeNightHours[i]:= DataBase.TimetableSumNightHoursInPeriodLoad(TabNumIDs[i], BD, ED);
    end;
  end;

begin
  if not CanLoadAndDraw then Exit;

  VTDel(Timetables);
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

  Progress:= TProgress.Create(nil);
  try
    Progress.WriteLine1(EmptyStr);
    Progress.WriteLine2(EmptyStr);
    Progress.Show;

    TimetablesCalc;
    NormHoursCalc;
    BeforeHoursCalc;
  finally
    FreeAndNil(Progress);
  end;
end;

procedure TTimetableMonthForm.TimetableSheetCreate;
begin
  SheetsFree;

  case ParamList.Selected['TimetableType'] of
    0: //форма графика
      begin
        Sheet:= TMonthTimetableSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont,
                                            ParamList.Selected['CountType'],
                                            ParamList.Selected['PeriodType'],
                                            ParamList.Checkeds['ExtraColumns']);
        Sheet.CanSelect:= EditingButton.Down;
        Sheet.OnSelect:= @TimetableSelect;
      end;
    1: //форма T-12
      begin
        SheetT12:= TTimetableSheetT12.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
        SheetT12.CanSelect:= EditingButton.Down;
        SheetT12.OnSelect:= @TimetableSelect;
      end;
    2: //форма T-13
      begin
        SheetT13:= TTimetableSheetT13.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
        SheetT13.CanSelect:= EditingButton.Down;
        SheetT13.OnSelect:= @TimetableSelect;
      end;
  end;
end;

procedure TTimetableMonthForm.TimetableSheetRecreate;
begin
  if not CanLoadAndDraw then Exit;
  TimetableSheetCreate;
  TimetableRedraw;
end;

procedure TTimetableMonthForm.TimetableDraw(const AZoomPercent: Integer);
begin
  if not CanLoadAndDraw then Exit;
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    case ParamList.Selected['TimetableType'] of
      0: //форма графика
        begin
          Sheet.Zoom(ZoomPercent);
          Sheet.Draw(MonthCalendar,
                 StaffNames, TabNums, PostNames, NormHours,
                 ParamList.Checkeds['ViewParams'],
                 ParamList.Checkeds['ExportParams'],
                 Timetables, BeforeTotalHours, BeforeNightHours,
                 ParamList.Selected['MonthType']=0);
        end;
      1: //форма T-12
        begin
          SheetT12.Zoom(ZoomPercent);
          SheetT12.Draw(MonthCalendar, Timetables,
                 StaffNames, TabNums, PostNames,
                 ParamList.Selected['ViewType']=0,
                 ParamList.Selected['MonthType']=0,
                 ParamList.Checkeds['ExportParams'],
                 1{simple border line in grid});
        end;
      2: //форма T-13
        begin
          SheetT13.Zoom(ZoomPercent);
          SheetT13.Draw(MonthCalendar, Timetables,
                 StaffNames, TabNums, PostNames,
                 ParamList.Selected['ViewType']=0,
                 ParamList.Selected['MonthType']=0,
                 ParamList.Checkeds['ExportParams'],
                 1{simple border line in grid});
        end;
    end;
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TTimetableMonthForm.TimetableRedraw;
begin
  TimetableDraw(ZoomPercent);
end;

procedure TTimetableMonthForm.TimetableSelect;
begin
  EditButtonsEnabled;
end;

procedure TTimetableMonthForm.TimetableExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;

  procedure SheetExport;
  var
    ExpSheet: TMonthTimetableSheet;
  begin
    ExpSheet:= TMonthTimetableSheet.Create(Worksheet, nil, MainForm.GridFont,
                                              ParamList.Selected['CountType'],
                                              ParamList.Selected['PeriodType'],
                                              ParamList.Checkeds['ExtraColumns']);
    try
      ExpSheet.Draw(MonthCalendar, StaffNames, TabNums, PostNames, NormHours,
                   ParamList.Checkeds['ViewParams'],
                   ParamList.Checkeds['ExportParams'],
                   Timetables, BeforeTotalHours, BeforeNightHours,
                   ParamList.Selected['MonthType']=0)
    finally
      FreeAndNil(ExpSheet)
    end;
  end;

  procedure SheetT12Export;
  var
    ExpSheetT12: TTimetableSheetT12;
  begin
    ExpSheetT12:= TTimetableSheetT12.Create(Worksheet, nil, MainForm.GridFont);
    try
      ExpSheetT12.Draw(MonthCalendar, Timetables, StaffNames, TabNums, PostNames,
                   ParamList.Selected['ViewType']=0,
                   ParamList.Selected['MonthType']=0,
                   ParamList.Checkeds['ExportParams'],
                   ParamList.Selected['LineType']+1);
    finally
      FreeAndNil(ExpSheetT12)
    end;
  end;

  procedure SheetT13Export;
  var
    ExpSheetT13: TTimetableSheetT13;
  begin
    ExpSheetT13:= TTimetableSheetT13.Create(Worksheet, nil, MainForm.GridFont);
    try
      ExpSheetT13.Draw(MonthCalendar, Timetables, StaffNames, TabNums, PostNames,
                   ParamList.Selected['ViewType']=0,
                   ParamList.Selected['MonthType']=0,
                   ParamList.Checkeds['ExportParams'],
                   ParamList.Selected['LineType']+1);
    finally
      FreeAndNil(ExpSheetT13)
    end;
  end;

begin
  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet(SUpper(MonthDropDown.Text) + ' ' + YearSpinEdit.Text);
    case ParamList.Selected['TimetableType'] of
      0: SheetExport;
      1: SheetT12Export;
      2: SheetT13Export;
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TTimetableMonthForm.TimetableEditFormOpen;
var
  TimetableEditForm: TTimetableEditForm;
  TabNumID: Integer;
  S: TMonthCustomSheet;
begin
  S:= ActiveSheet;
  if not S.IsDateSelected then Exit;

  TabNumID:= TabNumIDs[S.SelectedIndex];

  TimetableEditForm:= TTimetableEditForm.Create(nil);
  try
    TimetableEditForm.TabNumID:= TabNumID;
    TimetableEditForm.RecrutDate:= RecrutDates[S.SelectedIndex];
    TimetableEditForm.DismissDate:= DismissDates[S.SelectedIndex];
    TimetableEditForm.FirstDate:= S.SelectedDate;
    if TimetableEditForm.ShowModal=mrOK then
      TimetableUpdate(TabNumID);
  finally
    FreeAndNil(TimetableEditForm);
  end
end;

function TTimetableMonthForm.ActiveSheet: TMonthCustomSheet;
begin
  case ParamList.Selected['TimetableType'] of
    0: Result:= Sheet;
    1: Result:= SheetT12;
    2: Result:= SheetT13;
  end;
end;

procedure TTimetableMonthForm.SheetsFree;
begin
  if Assigned(Sheet) then FreeAndNil(Sheet);
  if Assigned(SheetT12) then FreeAndNil(SheetT12);
  if Assigned(SheetT13) then FreeAndNil(SheetT13);
end;

procedure TTimetableMonthForm.SelectionMove(const ADirection: TMoveDirection);
var
  OldSelectedIndex, NewSelectedIndex: Integer;
  S: TMonthCustomSheet;
begin
  S:= ActiveSheet;

  if ADirection=mdUp then
    NewSelectedIndex:= S.SelectedIndex - 1
  else if ADirection=mdDown then
    NewSelectedIndex:= S.SelectedIndex + 1
  else Exit;
  OldSelectedIndex:= S.SelectedIndex;

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
  VSwap(BeforeTotalHours, OldSelectedIndex, NewSelectedIndex);
  VSwap(BeforeNightHours, OldSelectedIndex, NewSelectedIndex);
  VTSwap(Timetables, OldSelectedIndex, NewSelectedIndex);
  VISwap(PostScheduleInfos, OldSelectedIndex, NewSelectedIndex);

  S.SelectionMove(NewSelectedIndex);
end;

procedure TTimetableMonthForm.RowsMerge;
var
  ChooseIndex1, ChooseIndex2: Integer;
  V1, V2: TStrVector;
  RowIndexes: TIntVector;
  PostName: String;
  S: TMonthCustomSheet;

  procedure Merge(const AResultIndex, ADeleteIndex: Integer; const APostName: String);
  begin
    //должность, в результирующей строке
    PostNames[AResultIndex]:= APostName;
    //Info графика в результирующей строке
    PostScheduleInfoAdd(PostScheduleInfos[AResultIndex], PostScheduleInfos[ADeleteIndex]);
    //табель в результирующей строке
    Timetables[AResultIndex].Calc(TabNumIDs[AResultIndex], TabNums[AResultIndex],
                                  RecrutDates[AResultIndex], DismissDates[AResultIndex],
                                  MonthCalendar, PostScheduleInfos[AResultIndex]);

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
    VDel(BeforeTotalHours, ADeleteIndex);
    VDel(BeforeNightHours, ADeleteIndex);
    VTDel(Timetables, ADeleteIndex);
    VIDel(PostScheduleInfos, ADeleteIndex);
    TimetableRedraw;
    S.Select(AResultIndex - Ord(AResultIndex>ADeleteIndex));
  end;

begin
  S:= ActiveSheet;

  RowIndexes:= VCreateInt([
    Min(S.SelectedIndex, S.SelectedIndex2),
    Max(S.SelectedIndex, S.SelectedIndex2)
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

procedure TTimetableMonthForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= nil;
  VAppend(SettingValues, ZoomPercent);
  SettingValues:= VAdd(SettingValues, ParamList.Params);
  DataBase.SettingsUpdate(SETTING_NAMES_TIMETABLEMONTHFORM, SettingValues);
end;

procedure TTimetableMonthForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_TIMETABLEMONTHFORM);
  ZoomPercent:= SettingValues[0];
  ParamList.Params:= VCut(SettingValues, 1);
end;

procedure TTimetableMonthForm.ViewUpdate;
begin
  EditPanel.Visible:= EditingButton.Down;

  ActiveSheet.CanSelect:= EditingButton.Down;

  if SettingButton.Down then
  begin
    SettingPanel.Visible:= True;
    SettingSplitter.Visible:= True;
    SheetPanel.AnchorToNeighbour(akLeft, 0, SettingSplitter);
    TimetableCaptionPanel.AnchorToNeighbour(akLeft, 0, SettingSplitter);
  end
  else begin
    SettingSplitter.Visible:= False;
    SettingPanel.Visible:= False;
    SheetPanel.AnchorToNeighbour(akLeft, 2, Self);
    TimetableCaptionPanel.AnchorToNeighbour(akLeft, 2, Self);
  end;
end;

end.

