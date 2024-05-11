unit USchedulePersonalMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  BCPanel, Buttons, Spin, StdCtrls, VirtualTrees, fpspreadsheetgrid, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Math, DK_Fonts, DK_Const, DK_DateUtils,
  DK_StrUtils, DK_VSTTables, DK_VSTTableTools, DK_Zoom, DK_SheetExporter, DK_Progress,
  //Project utils
  UDataBase, UConst, UUtils, UCalendar, USchedule, UScheduleSheet, UWorkHours,
  UTypes,
  //Forms
  UScheduleCorrectionEditForm, UChooseForm;

type

  { TSchedulePersonalMonthForm }

  TSchedulePersonalMonthForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    CheckAllButton: TSpeedButton;
    CountTypeVT: TVirtualStringTree;
    DayEditButton: TSpeedButton;
    EditingButton: TSpeedButton;
    EditPanel: TPanel;
    ExtraColumnListVT: TVirtualStringTree;
    RowDownButton: TSpeedButton;
    RowSplitButton: TSpeedButton;
    RowUpButton: TSpeedButton;
    SettingButton: TSpeedButton;
    SignTypeVT: TVirtualStringTree;
    ExportParamListVT: TVirtualStringTree;
    ViewParamListVT: TVirtualStringTree;
    PeriodTypeVT: TVirtualStringTree;
    SettingSplitter: TSplitter;
    RightPanel: TPanel;
    CloseButton: TSpeedButton;
    ExportButton: TBCButton;
    ScheduleCaptionPanel: TBCPanel;
    ScheduleToolPanel: TPanel;
    ScheduleButton: TBCButton;
    FIORadioButton: TRadioButton;
    ListToolPanel: TPanel;
    MonthBCButton: TBCButton;
    ListButton: TBCButton;
    SchedulePanel: TPanel;
    ScheduleRadioButton: TRadioButton;
    ListCaptionPanel: TBCPanel;
    ListOrderToolPanel: TPanel;
    ListPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    SettingCaptionPanel: TBCPanel;
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
    procedure RowSplitButtonClick(Sender: TObject);
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
    Calendar, BeforeCalendar, YearCalendar: TCalendar;
    Schedules: TPersonalScheduleVector;
    BeforeSchedules: TPersonalScheduleVector; //для расчета часов за учетный период
    PostSchedules: TPostScheduleMatrix;
    Sheet: TPersonalMonthScheduleSheet;

    ViewParamList: TVSTCheckList;
    CountType: TVSTStringList;
    PeriodType: TVSTStringList;
    ExtraColumnList: TVSTCheckList;
    SignType: TVSTStringList;
    ExportParamList: TVSTCheckList;

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

    procedure ViewParamListCreate;
    procedure CountTypeCreate;
    procedure PeriodTypeCreate;
    procedure ExtraColumnListCreate;
    procedure SignTypeCreate;
    procedure ExportParamListCreate;

    procedure PeriodTypeSelect;

    function OrderTypeChange: Boolean;
    procedure EditButtonsEnabled;

    procedure StaffListCreate;
    procedure StaffListLoad;

    procedure ScheduleCreate(const AIndex: Integer);
    procedure ScheduleLoad;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleRecreate;
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

procedure SchedulePersonalMonthFormOpen(const AYear: Integer);

implementation

uses UMainForm;

{$R *.lfm}

procedure SchedulePersonalMonthFormOpen(const AYear: Integer);
var
  Form: TSchedulePersonalMonthForm;
begin
  Form:= TSchedulePersonalMonthForm.Create(nil);
  try
    Form.YearSpinEdit.Value:= AYear;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{ TSchedulePersonalMonthForm }

procedure TSchedulePersonalMonthForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[13];
  //Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel, ListOrderToolPanel
  ]);

  SetCaptionPanels([
    ListCaptionPanel, SettingCaptionPanel, ScheduleCaptionPanel
  ]);

  SetToolButtons([
    CloseButton, CheckAllButton, UncheckAllButton,
    RowUpButton, RowDownButton, RowSplitButton
  ]);

  SetCategoryButtons([
    ExportButton
  ]);

  CanLoadAndDraw:= False;

  Calendar:= TCalendar.Create;
  BeforeCalendar:= TCalendar.Create;
  YearCalendar:= TCalendar.Create;

  ViewParamListCreate;
  CountTypeCreate;
  PeriodTypeCreate;
  ExtraColumnListCreate;
  SignTypeCreate;
  ExportParamListCreate;

  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, @StaffListLoad);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  StaffListCreate;

  CanLoadAndDraw:= True;
end;

procedure TSchedulePersonalMonthForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;

  FreeAndNil(ViewParamList);
  FreeAndNil(CountType);
  FreeAndNil(PeriodType);
  FreeAndNil(ExtraColumnList);
  FreeAndNil(SignType);
  FreeAndNil(ExportParamList);

  FreeAndNil(Calendar);
  FreeAndNil(BeforeCalendar);
  FreeAndNil(YearCalendar);
  FreeAndNil(MonthDropDown);
  FreeAndNil(VStaffList);
  FreeAndNil(MStaffList);
  FreeAndNil(Sheet);

  VSDel(Schedules);
  VSDel(BeforeSchedules);
  MSDel(PostSchedules);
end;

procedure TSchedulePersonalMonthForm.FormShow(Sender: TObject);
begin
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

procedure TSchedulePersonalMonthForm.ViewParamListCreate;
var
  V: TStrVector;
begin
  V:= VCreateStr([
    'отображать строку ночных часов',
    'учитывать корректировки графика',
    'коды табеля для нерабочих дней',
    'учитывать отпуск'
  ]);
  ViewParamList:= TVSTCheckList.Create(ViewParamListVT, VIEW_PARAMS_CAPTION, V, @ScheduleRedraw);
end;

procedure TSchedulePersonalMonthForm.CountTypeCreate;
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
  CountType:= TVSTStringList.Create(CountTypeVT, S, @ScheduleRecreate);
  CountType.Update(V);
end;

procedure TSchedulePersonalMonthForm.PeriodTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Учетный период:';
  V:= VCreateStr([
    'год',
    'квартал',
    'месяц'
  ]);
  PeriodType:= TVSTStringList.Create(PeriodTypeVT, S, @PeriodTypeSelect);
  PeriodType.Update(V);
end;

procedure TSchedulePersonalMonthForm.ExtraColumnListCreate;
var
  S: String;
  V: TStrVector;
begin
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
  ExtraColumnList:= TVSTCheckList.Create(ExtraColumnListVT, S, V, @ScheduleRecreate);
end;

procedure TSchedulePersonalMonthForm.SignTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Ознакомление с графиком:';
  V:= VCreateStr([
    'не выводить',
    'столбцы Дата/Подпись',
    'список под таблицей'
  ]);
  SignType:= TVSTStringList.Create(SignTypeVT, S, @ScheduleRecreate);
  SignType.Update(V);
end;

procedure TSchedulePersonalMonthForm.ExportParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Параметры экспорта:';
  V:= VCreateStr([
    'заголовок таблицы на каждой странице',
    'номера страниц в нижнем колонтитуле'
  ]);
  ExportParamList:= TVSTCheckList.Create(ExportParamListVT, S, V, nil);
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
                              PeriodType.ItemIndex, BD, ED);
    for i:= 0 to High(TabNumIDs) do
    begin
      NormHoursAndWorkDaysCounInPeriod(TabNumIDs[i], BD, ED, YearCalendar, d, h);
      NormHours[i]:= h;
    end;

    if Length(BeforeSchedules)>0 then
    begin
      AccountingPeriodBeforeMonth(MonthDropDown.Month, YearSpinEdit.Value,
                              PeriodType.ItemIndex, BD, ED);
      YearCalendar.Cut(BD, ED, BeforeCalendar);
      for i:= 0 to High(TabNumIDs) do
        BeforeSchedules[i]:= SchedulePersonalByCalendar(TabNumIDs[i], TabNums[i],
          RecrutDates[i], DismissDates[i], BeforeCalendar, Holidays, False{fact vacations},
          STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY,
          ScheduleBDs[i], ScheduleEDs[i], PostBDs[i], PostEDs[i]);
    end;

    ScheduleRecreate;
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
  ScheduleRecreate;
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

procedure TSchedulePersonalMonthForm.RowSplitButtonClick(Sender: TObject);
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
begin
  VStaffList:= TVSTCheckTable.Create(VStaffListVT);
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
  RowSplitButton.Enabled:= Sheet.IsDoubleRowSelected;
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

  ScheduleCaptionPanel.Caption:= 'График работы на ' + MonthDropDown.Text + ' ' +
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

procedure TSchedulePersonalMonthForm.ScheduleCreate(const AIndex: Integer);
var
  d, h: Integer;
  PeriodBD, PeriodED: TDate;
begin
  PostSchedules[AIndex]:= PostSchedulesByCalendar(TabNumIDs[AIndex], Calendar,
     ScheduleBDs[AIndex], ScheduleEDs[AIndex], PostBDs[AIndex], PostEDs[AIndex]);

  Schedules[AIndex]:= PersonalScheduleByPostSchedules(TabNumIDs[AIndex], TabNums[AIndex],
     RecrutDates[AIndex], DismissDates[AIndex],
     Calendar, Holidays, PostSchedules[AIndex], False{fact vacations},
     STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY);

  AccountingPeriodWithMonth(MonthDropDown.Month, YearSpinEdit.Value,
                            PeriodType.ItemIndex, PeriodBD, PeriodED);
  NormHoursAndWorkDaysCounInPeriod(TabNumIDs[AIndex], PeriodBD, PeriodED, YearCalendar, d, h);
  NormHours[AIndex]:= h;

  if Length(BeforeSchedules)=0 then Exit;

  BeforeSchedules[AIndex]:= SchedulePersonalByCalendar(TabNumIDs[AIndex], TabNums[AIndex],
     RecrutDates[AIndex], DismissDates[AIndex], BeforeCalendar, Holidays, False{fact vacations},
     STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY,
     ScheduleBDs[AIndex], ScheduleEDs[AIndex], PostBDs[AIndex], PostEDs[AIndex]);
end;

procedure TSchedulePersonalMonthForm.ScheduleLoad;
var
  i: Integer;
  S: String;
  BD, ED: TDate;
  Y, M: Word;
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
  MSDel(PostSchedules);
  NormHours:= nil;

  if OrderType<=1 then
    GetCategoryValues
  else
    GetSimpleValues;

  if VIsNil(TabNumIDs) then Exit;

  Y:= YearSpinEdit.Value;
  M:= MonthDropDown.Month;
  Holidays:= DataBase.HolidaysLoad(Y);

  CalendarForYear(Y, YearCalendar);

  SetLength(PostSchedules, Length(TabNumIDs));
  SetLength(Schedules, Length(TabNumIDs));
  SetLength(NormHours, Length(TabNumIDs));
  IsBeforePeriodExists:= (PeriodType.ItemIndex<2 {учетный период<>месяц}) and
               AccountingPeriodBeforeMonth(M, Y, PeriodType.ItemIndex, BD, ED);
  if IsBeforePeriodExists then
  begin
    SetLength(BeforeSchedules, Length(TabNumIDs));
    YearCalendar.Cut(BD, ED, BeforeCalendar);
  end;

  FirstLastDayInMonth(M, Y, BD, ED);
  YearCalendar.Cut(BD, ED, Calendar);

  Progress:= TProgress.Create(nil);
  try
    Progress.WriteLine1('Расчет графиков');
    Progress.WriteLine2(EmptyStr);
    Progress.Show;
    for i:= 0 to High(TabNumIDs) do
    begin
      S:= StaffNames[i] + ' [таб.№ ' + TabNums[i] + '] - ' + PostNames[i];
      Progress.WriteLine2(S);
      ScheduleCreate(i);
    end;
  finally
    FreeAndNil(Progress);
  end;
end;

procedure TSchedulePersonalMonthForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(Calendar, Schedules, BeforeSchedules,
               StaffNames, TabNums, PostNames, NormHours,
               ViewParamList.Selected, ExportParamList.Selected);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSchedulePersonalMonthForm.ScheduleRedraw;
begin
  if not CanLoadAndDraw then Exit;
  ScheduleDraw(ZoomPercent);
end;

procedure TSchedulePersonalMonthForm.ScheduleRecreate;
begin
  if not CanLoadAndDraw then Exit;

  if Assigned(Sheet) then FreeAndNil(Sheet);
  Sheet:= TPersonalMonthScheduleSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont,
     CountType.ItemIndex, PeriodType.ItemIndex, SignType.ItemIndex, ExtraColumnList.Selected);
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
    ExpSheet:= TPersonalMonthScheduleSheet.Create(Worksheet, nil, MainForm.GridFont,
                               CountType.ItemIndex, PeriodType.ItemIndex,
                               SignType.ItemIndex, ExtraColumnList.Selected);
    try
      ExpSheet.Draw(Calendar, Schedules, BeforeSchedules,
               StaffNames, TabNums, PostNames, NormHours,
               ViewParamList.Selected, ExportParamList.Selected);
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
  i: Integer;
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;
  CorrectIDs: TIntVector;
  Corrections: TScheduleCorrections;
begin
  if not Sheet.IsDateSelected then Exit;

  ScheduleCorrectionEditForm:= TScheduleCorrectionEditForm.Create(nil);
  try
    ScheduleCorrectionEditForm.TabNumID:= TabNumIDs[Sheet.SelectedIndex];
    ScheduleCorrectionEditForm.FirstDatePicker.Date:= Sheet.SelectedDate;
    if DataBase.SchedulePersonalCorrectionsLoad(TabNumIDs[Sheet.SelectedIndex],
            CorrectIDs, Corrections, Sheet.SelectedDate, Sheet.SelectedDate) then
    begin
      ScheduleCorrectionEditForm.DigMark:= Corrections.DigMarks[0];
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursTotal[0]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Corrections.HoursNight[0]);
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Corrections.ShiftNums[0];
    end
    else begin
      i:= DayOf(Sheet.SelectedDate)-1;
      ScheduleCorrectionEditForm.DigMark:= Schedules[Sheet.SelectedIndex].MarkDIGDefault[i];
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Schedules[Sheet.SelectedIndex].HoursDefault.Total[i]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Schedules[Sheet.SelectedIndex].HoursDefault.Night[i]);
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Schedules[Sheet.SelectedIndex].ShiftNumbersDefault[i];
    end;

    if ScheduleCorrectionEditForm.ShowModal=mrOK then
    begin
      FreeAndNil(Schedules[Sheet.SelectedIndex]);
      if Length(BeforeSchedules)>0 then
         FreeAndNil(BeforeSchedules[Sheet.SelectedIndex]);
      ScheduleCreate(Sheet.SelectedIndex);
      Sheet.LineDraw(Sheet.SelectedIndex);
    end;
  finally
    FreeAndNil(ScheduleCorrectionEditForm);
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
  MSSwap(PostSchedules, OldSelectedIndex, NewSelectedIndex);

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
    //график в результирующей строке
    VSAppend(PostSchedules[AResultIndex], PostSchedules[ADeleteIndex]);
    FreeAndNil(Schedules[AResultIndex]);
    Schedules[AResultIndex]:= PersonalScheduleByPostSchedules(TabNumIDs[AResultIndex],
      TabNums[AResultIndex], RecrutDates[AResultIndex], DismissDates[AResultIndex],
      Calendar, Holidays, PostSchedules[AResultIndex], False{fact vacations},
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
    MSDel(PostSchedules,ADeleteIndex, -1, False);

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
  SettingValues:= VAdd(SettingValues, VBoolToInt(ViewParamList.Selected));
  VAppend(SettingValues, CountType.ItemIndex);
  VAppend(SettingValues, PeriodType.ItemIndex);
  SettingValues:= VAdd(SettingValues, VBoolToInt(ExtraColumnList.Selected));
  VAppend(SettingValues, SignType.ItemIndex);
  SettingValues:= VAdd(SettingValues, VBoolToInt(ExportParamList.Selected));
  DataBase.SettingsUpdate(SETTING_NAMES_SCHEDULEPERSONALMONTHFORM, SettingValues);
end;

procedure TSchedulePersonalMonthForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SCHEDULEPERSONALMONTHFORM);
  ZoomPercent:= SettingValues[0];
  ViewParamList.Selected:= VIntToBool(VCut(SettingValues, 1, 4));
  CountType.ItemIndex:= SettingValues[5];
  PeriodType.ItemIndex:= SettingValues[6];
  ExtraColumnList.Selected:= VIntToBool(VCut(SettingValues, 7, 13));
  SignType.ItemIndex:= SettingValues[14];
  ExportParamList.Selected:= VIntToBool(VCut(SettingValues, 15, 16));
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

