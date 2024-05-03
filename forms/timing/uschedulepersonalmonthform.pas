unit USchedulePersonalMonthForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  BCPanel, Buttons, Spin, StdCtrls, VirtualTrees, fpspreadsheetgrid,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Fonts, DK_Const, DK_VSTDropDown, DK_DateUtils,
  DK_StrUtils, DK_VSTTables, DK_Zoom, DK_SheetExporter,
  //Project utils
  UDataBase, UConst, UUtils, UCalendar, USchedule, UScheduleSheet;

type

  { TSchedulePersonalMonthForm }

  TSchedulePersonalMonthForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    CheckAllButton: TSpeedButton;
    RowUpButton: TSpeedButton;
    RowSplitButton: TSpeedButton;
    RowDownButton: TSpeedButton;
    CloseButton: TSpeedButton;
    ExportButton: TBCButton;
    ScheduleToolPanel: TPanel;
    ScheduleButton: TBCButton;
    FIORadioButton: TRadioButton;
    ListToolPanel: TPanel;
    MonthBCButton: TBCButton;
    ListButton: TBCButton;
    ScheduleCaptionPanel: TBCPanel;
    SchedulePanel: TPanel;
    ScheduleRadioButton: TRadioButton;
    ListCaptionPanel: TBCPanel;
    ListOrderToolPanel: TPanel;
    ListPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    SheetPanel: TPanel;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    ViewGrid: TsWorksheetGrid;
    MStaffListVT: TVirtualStringTree;
    VStaffListVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure ScheduleRadioButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanLoadStaffList: Boolean;
    ZoomPercent: Integer;
    MonthDropDown: TVSTDropDown;
    OrderType: Integer;

    Holidays: TDateVector;
    Calendar: TCalendar;
    Schedules: TPersonalScheduleVector;

    VStaffList: TVSTCheckTable;
    MStaffList: TVSTCategoryCheckTable;

    VTabNumIDs: TIntVector;
    VStaffNames, VTabNums, VPostNames, VScheduleNames: TStrVector;
    VRecrutDates, VDismissDates, VPostBDs, VPostEDs, VScheduleBDs, VScheduleEDs: TDateVector;

    CategoryNames: TStrVector;
    MTabNumIDs: TIntMatrix;
    MStaffNames, MTabNums, MPostNames, MScheduleNames: TStrMatrix;
    MRecrutDates, MDismissDates, MPostBDs, MPostEDs, MScheduleBDs, MScheduleEDs: TDateMatrix;

    TabNumIDs: TIntVector;
    StaffNames, TabNums, PostNames, ScheduleNames: TStrVector;
    RecrutDates, DismissDates, PostBDs, PostEDs, ScheduleBDs, ScheduleEDs: TDateVector;

    function OrderTypeChange: Boolean;

    procedure StaffListCreate;
    procedure StaffListLoad;

    procedure ScheduleLoad;

    procedure ScheduleDraw(const AZoomPercent: Integer);
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
  Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel, ListOrderToolPanel
  ]);

  SetCaptionPanels([
    ListCaptionPanel, ScheduleCaptionPanel
  ]);

  SetToolButtons([
    CloseButton, CheckAllButton, UncheckAllButton,
    RowUpButton, RowDownButton, RowSplitButton
  ]);

  SetCategoryButtons([
    ExportButton
  ]);

  CanLoadStaffList:= False;

  Calendar:= TCalendar.Create;

  MonthDropDown:= TVSTDropDown.Create(MonthBCButton);
  MonthDropDown.OnChange:= @StaffListLoad;
  MonthDropDown.Items:= VCreateStr(MONTHSNOM);
  MonthDropDown.ItemIndex:= MonthOfDate(Date) - 1;
  MonthDropDown.DropDownCount:= 12;

  ZoomPercent:= 100;//SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  StaffListCreate;

  CanLoadStaffList:= True;
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

procedure TSchedulePersonalMonthForm.YearSpinEditChange(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSchedulePersonalMonthForm.FormDestroy(Sender: TObject);
begin
  //SettingsSave;
  FreeAndNil(Calendar);
  FreeAndNil(MonthDropDown);
  FreeAndNil(VStaffList);
  FreeAndNil(MStaffList);
  //FreeAndNil(Sheet);
  VSDel(Schedules);
end;

procedure TSchedulePersonalMonthForm.FormShow(Sender: TObject);
begin
  MonthBCButton.Width:= GetBCButtonWidth(MonthBCButton, MONTHSNOM[9]);
  OrderType:= 0;
  StaffListLoad;
end;

procedure TSchedulePersonalMonthForm.ListButtonClick(Sender: TObject);
begin
  ScheduleToolPanel.Visible:= False;
  ListToolPanel.Visible:= True;
  SchedulePanel.Visible:= False;
  SchedulePanel.Align:= alBottom;
  ListPanel.Align:= alClient;
  ListPanel.Visible:= True;
end;

procedure TSchedulePersonalMonthForm.ScheduleButtonClick(Sender: TObject);
begin
  ListToolPanel.Visible:= False;
  ScheduleToolPanel.Visible:= True;
  ListPanel.Visible:= False;
  ListPanel.Align:= alBottom;
  SchedulePanel.Align:= alClient;
  SchedulePanel.Visible:= True;

  ScheduleLoad;
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

procedure TSchedulePersonalMonthForm.ScheduleRadioButtonClick(Sender: TObject);
begin
  if not OrderTypeChange then Exit;
  StaffListLoad;
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

procedure TSchedulePersonalMonthForm.StaffListLoad;

  procedure CategoryListLoad;
  begin
    GetStaffListForCommonTiming(YearSpinEdit.Value, MonthDropDown.ItemIndex+1,
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
    GetStaffListForCommonTiming(YearSpinEdit.Value, MonthDropDown.ItemIndex+1,
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
  if not CanLoadStaffList then Exit;

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

procedure TSchedulePersonalMonthForm.ScheduleLoad;

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
  CalendarForMonth(MonthDropDown.ItemIndex+1, YearSpinEdit.Value, Calendar);
  Holidays:= DataBase.HolidaysLoad(YearSpinEdit.Value);

  if OrderType<=1 then
    GetCategoryValues
  else
    GetSimpleValues;

  VSDel(Schedules);
  Schedules:= SchedulesPersonalByCalendar(TabNumIDs, TabNums,
              RecrutDates, DismissDates, ScheduleBDs, ScheduleEDs, PostBDs, PostEDs,
              Calendar, Holidays, False{fact vacations},
              STRMARK_VACATIONMAIN, STRMARK_VACATIONADDITION, STRMARK_VACATIONHOLIDAY);

end;

procedure TSchedulePersonalMonthForm.ScheduleDraw(const AZoomPercent: Integer);
begin

end;

end.

