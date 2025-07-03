unit UVacationPlanForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel, Spin, StdCtrls, VirtualTrees, DateUtils,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_VSTTypes, DK_Vector, DK_Filter, DK_StrUtils,
  DK_CtrlUtils,
  //Project utils
  UVars, UTimingUtils, UTypes, UConst,
  //Forms
  UVacationScheduleForm, UVacationPlanEditForm, UVacationPlanningForm;

type

  { TVacationPlanForm }

  TVacationPlanForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel5: TDividerBevel;
    EditButton: TSpeedButton;
    ExportButton: TSpeedButton;
    FilterPanel: TPanel;
    PlanButton: TSpeedButton;
    SettingSplitter: TSplitter;
    MainPanel: TPanel;
    SettingCaptionPanel: TPanel;
    ListCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    ToolPanel: TPanel;
    ScheduleButton: TSpeedButton;
    VT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PlanButtonClick(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure VTDblClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanLoadStaffList: Boolean;
    FilterString: String;
    ModeType: TModeType;

    StaffList: TVSTTable;
    ParamList: TVSTParamList;

    TabNumIDs: TIntVector;
    FullNames, Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    RecrutDates, Part1FirstDates, Part2FirstDates: TDateVector;
    Part1Counts, Part1AddCounts, Part2Counts, Part2AddCounts: TIntVector;

    procedure ParamListCreate;
    procedure OrderTypeSelect;
    procedure RecrutTypeSelect;
    procedure ColumnsListSelect;
    procedure NameTypeSelect;

    procedure StaffListCreate;
    procedure StaffListLoad(const SelectedID: Integer = -1);
    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListItemSelect;
    procedure StaffListItemEdit;


    procedure VacationPlanEditFormOpen;
    procedure VacationPlanningFormOpen;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  VacationPlanForm: TVacationPlanForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TVacationPlanForm }

procedure TVacationPlanForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  CanLoadStaffList:= False;
  StaffListCreate;
  ParamListCreate;
  SettingsLoad;
  YearSpinEdit.Value:= YearOf(Date);
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter);
  CanLoadStaffList:= True;
end;

procedure TVacationPlanForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StaffList);
  FreeAndNil(ParamList);
end;

procedure TVacationPlanForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    SettingCaptionPanel, ListCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, EditButton
  ]);

  Images.ToButtons([
    ExportButton, PlanButton, ScheduleButton,
    CloseButton, EditButton
  ]);

  DataUpdate;
end;

procedure TVacationPlanForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TVacationPlanForm.EditButtonClick(Sender: TObject);
begin
  StaffListItemEdit;
end;

procedure TVacationPlanForm.ExportButtonClick(Sender: TObject);
begin
  StaffList.Save([ctInteger, //№ п/п
                  ctString,  //Ф.И.О
                  ctString,  //табельный номер
                  ctString,  //должность
                  ctDate,    //дата приема
                  ctString,  //отпуск 1 часть
                  ctString   //отпуск 2 часть
  ]);
end;

procedure TVacationPlanForm.VacationPlanningFormOpen;
var
  VacationPlanningForm: TVacationPlanningForm;
begin
  VacationPlanningForm:= TVacationPlanningForm.Create(nil);
  try
    VacationPlanningForm.YearSpinEdit.Value:= YearSpinEdit.Value;
    VacationPlanningForm.ShowModal;
    StaffListLoad;
  finally
    FreeAndNil(VacationPlanningForm);
  end;
end;

procedure TVacationPlanForm.PlanButtonClick(Sender: TObject);
begin
  VacationPlanningFormOpen;
end;

procedure TVacationPlanForm.ScheduleButtonClick(Sender: TObject);
begin
  VacationScheduleFormShow(YearSpinEdit.Value);
end;

procedure TVacationPlanForm.VTDblClick(Sender: TObject);
begin
  StaffListItemEdit;
end;

procedure TVacationPlanForm.YearSpinEditChange(Sender: TObject);
begin
  ListCaptionPanel.Caption:= '  Планируемые отпуска в ' + YearSpinEdit.Text + ' году';
  StaffListLoad;
end;

procedure TVacationPlanForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= 'Сортировать список по:';
  V:= VCreateStr([
    'Ф.И.О.',
    'табельному номеру',
    'должности (профессии)',
    'дате 1 части отпуска',
    'дате 2 части отпуска'
  ]);
  ParamList.AddStringList('OrderType', S, V, @OrderTypeSelect);

  S:= 'Включать в список сотрудников:';
  V:= VCreateStr([
    'принятых до планируемого года',
    'принятых в планируемый год',
    'всех'
  ]);
  ParamList.AddStringList('RecrutType', S, V, @RecrutTypeSelect);

  S:= 'Отображать столбцы:';
  V:= VCreateStr([
    '№ п/п',
    'Ф.И.О',
    'табельный номер',
    'должность',
    'дата приема',
    'отпуск (1 часть)',
    'отпуск (2 часть)'
  ]);
  ParamList.AddCheckList('ColumnsList', S, V, @ColumnsListSelect);

  S:= 'Формат имени:';
  V:= VCreateStr([
    'Фамилия Имя Отчество',
    'Фамилия И.О.'
  ]);
  ParamList.AddStringList('NameType', S, V, @NameTypeSelect);

end;

procedure TVacationPlanForm.OrderTypeSelect;
begin
  StaffListLoad;
end;

procedure TVacationPlanForm.RecrutTypeSelect;
begin
  StaffListLoad;
end;

procedure TVacationPlanForm.ColumnsListSelect;
begin
  StaffList.ColumnVisibles:= ParamList.Checkeds['ColumnsList'];
end;

procedure TVacationPlanForm.NameTypeSelect;
begin
  if not ParamList.IsSelected['NameType'] then Exit;
  if (ParamList.Selected['NameType']=1)  then
    FullNames:= VNameShort(Families, Names, Patronymics)
  else
    FullNames:= VNameLong(Families, Names, Patronymics);
  StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[1], FullNames, taLeftJustify);
  StaffList.Refresh;
end;

procedure TVacationPlanForm.StaffListCreate;
var
  i: Integer;
begin
  StaffList:= TVSTTable.Create(VT);
  StaffList.OnSelect:= @StaffListItemSelect;
  StaffList.OnReturnKeyDown:= @StaffListItemEdit;
  StaffList.SetSingleFont(GridFont);
  StaffList.HeaderFont.Style:= [fsBold];
  for i:= 0 to High(VACATION_PLAN_STAFFLIST_COLUMN_NAMES) do
    StaffList.AddColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[i],
                        VACATION_PLAN_STAFFLIST_COLUMN_WIDTHS[i]);
  StaffList.AutosizeColumnDisable;
  StaffList.Draw;
end;

procedure TVacationPlanForm.StaffListLoad(const SelectedID: Integer);
var
  V: TStrVector;
  SelectedTabNumID: Integer;
begin
  if not CanLoadStaffList then Exit;

  if ModeType=mtEditing then
    SelectedTabNumID:= GetSelectedID(StaffList, TabNumIDs, SelectedID);

  DataBase.VacationPlanListForYearLoad(STrimLeft(FilterString),
                                      ParamList.Selected['OrderType'],
                                      ParamList.Selected['RecrutType'],
                                      YearSpinEdit.Value, TabNumIDs,
                                      Families, Names, Patronymics, TabNums, PostNames,
                                      RecrutDates, Part1FirstDates, Part2FirstDates,
                                      Part1Counts, Part1AddCounts, Part2Counts, Part2AddCounts);

  ExportButton.Enabled:= not VIsNil(TabNumIDs);
  PlanButton.Enabled:= ExportButton.Enabled;
  ScheduleButton.Enabled:= ExportButton.Enabled;

  StaffList.Visible:= False;
  try
    StaffList.ValuesClear;
    V:= VIntToStr(VOrder(Length(TabNumIDs)));
    StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[0], V);
    NameTypeSelect;
    StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[2], TabNums);
    StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[3], PostNames, taLeftJustify);
    StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[4], VFormatDateTime('dd.mm.yyyy', RecrutDates));
    V:= VVacationPart(Part1FirstDates, Part1Counts, Part1AddCounts);
    StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[5], V);
    V:= VVacationPart(Part2FirstDates, Part2Counts, Part2AddCounts);
    StaffList.SetColumn(VACATION_PLAN_STAFFLIST_COLUMN_NAMES[6], V);
    StaffList.Draw;
    if ModeType=mtEditing then
      StaffList.ReSelect(TabNumIDs, SelectedTabNumID, True);
  finally
    StaffList.Visible:= True;
  end;
end;

procedure TVacationPlanForm.StaffListItemSelect;
begin
  EditButton.Enabled:= StaffList.IsSelected;
end;

procedure TVacationPlanForm.StaffListItemEdit;
begin
  if not STaffList.IsSelected then Exit;
  VacationPlanEditFormOpen;
end;

procedure TVacationPlanForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TVacationPlanForm.VacationPlanEditFormOpen;
var
  i, TabNumID: Integer;
  S: String;
begin
  i:= StaffList.SelectedIndex;
  TabNumID:= TabNumIDs[i];

  S:= StaffNameForVacationPlanning(SNameShort(Families[i], Names[i], Patronymics[i]), TabNums[i]);
  if VacationPlanEditFormShow(S, YearSpinEdit.Value, TabNumID,
                          Part1FirstDates[i], Part2FirstDates[i],
                          Part1Counts[i], Part1AddCounts[i],
                          Part2Counts[i], Part2AddCounts[i]) <> mrOK then Exit;
  StaffListLoad(TabNumID);
end;

procedure TVacationPlanForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_VACATIONPLANFORM);
end;

procedure TVacationPlanForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_VACATIONPLANFORM, ParamList.Params);
end;

procedure TVacationPlanForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
  try
    ModeType:= AModeType;

    if ModeType=mtSetting then
    begin
      SettingPanel.Visible:= True;
      SettingSplitter.Visible:= True;
    end
    else begin
      SettingSplitter.Visible:= False;
      SettingPanel.Visible:= False;
    end;

    StaffList.CanUnselect:= ModeType<>mtEditing;
    StaffList.CanSelect:= ModeType=mtEditing;
    EditButton.Visible:= ModeType=mtEditing;
    ExportButton.Visible:= ModeType<>mtEditing;

    MainPanel.BorderSpacing.Left:= 2*Ord(ModeType<>mtSetting);

    if ModeType=mtEditing then
    begin
      if not VIsNil(TabNumIDs) then
        StaffList.Select(0);
    end
    else begin
      if StaffList.IsSelected then
        StaffList.UnSelect;
    end;

  finally
    MainPanel.Visible:= True;
  end;
end;

procedure TVacationPlanForm.DataUpdate;
begin
  StaffListLoad;
end;

end.

