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
  UDataBase, UTimingUtils, UImages, UTypes, UConst,
  //Forms
  UVacationScheduleForm, UVacationPlanEditForm, UVacationPlanningForm;

type

  { TVacationPlanForm }

  TVacationPlanForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    ExportButton: TSpeedButton;
    PlanButton: TSpeedButton;
    FilterPanel: TPanel;
    LeftSplitter: TSplitter;
    EditButton: TSpeedButton;
    ListToolPanel: TPanel;
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
    procedure StaffListSelect;
    procedure StaffListFilter(const AFilterString: String);

    procedure VacationPlanEditFormOpen;
    procedure VacationPlanningFormOpen;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  VacationPlanForm: TVacationPlanForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TVacationPlanForm }

procedure TVacationPlanForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TVacationPlanForm.EditButtonClick(Sender: TObject);
begin
  VacationPlanEditFormOpen;
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

procedure TVacationPlanForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  SetToolPanels([
    ToolPanel, ListToolPanel
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

  CanLoadStaffList:= False;
  StaffListCreate;
  ParamListCreate;
  SettingsLoad;
  YearSpinEdit.Value:= YearOf(Date);
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter);
  CanLoadStaffList:= True;

  StaffListLoad;
end;

procedure TVacationPlanForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StaffList);
  FreeAndNil(ParamList);
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
  if not STaffList.IsSelected then Exit;
  VacationPlanEditFormOpen;
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
  StaffList.SetColumn('Ф.И.О', FullNames, taLeftJustify);
  StaffList.Refresh;
end;

procedure TVacationPlanForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(VT);
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(MainForm.GridFont);
  StaffList.HeaderFont.Style:= [fsBold];

  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Ф.И.О', 250);
  StaffList.AddColumn('Табельный номер', 120);
  StaffList.AddColumn('Должность на начало года', 300);
  StaffList.AddColumn('Дата приема', 100);
  StaffList.AddColumn('Отпуск (1 часть)', 150);
  StaffList.AddColumn('Отпуск (2 часть)', 150);
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


  StaffList.Visible:= False;
  try
    StaffList.ValuesClear;
    V:= VIntToStr(VOrder(Length(TabNumIDs)));
    StaffList.SetColumn('№ п/п', V);
    NameTypeSelect;
    StaffList.SetColumn('Табельный номер', TabNums);
    StaffList.SetColumn('Должность на начало года', PostNames, taLeftJustify);
    StaffList.SetColumn('Дата приема', VFormatDateTime('dd.mm.yyyy', RecrutDates));
    V:= VVacationPart(Part1FirstDates, Part1Counts, Part1AddCounts);
    StaffList.SetColumn('Отпуск (1 часть)', V);
    V:= VVacationPart(Part2FirstDates, Part2Counts, Part2AddCounts);
    StaffList.SetColumn('Отпуск (2 часть)', V);
    StaffList.Draw;
    if ModeType=mtEditing then
      StaffList.ReSelect(TabNumIDs, SelectedTabNumID, True);
  finally
    StaffList.Visible:= True;
  end;
end;

procedure TVacationPlanForm.StaffListSelect;
begin
  EditButton.Enabled:= StaffList.IsSelected;
end;

procedure TVacationPlanForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TVacationPlanForm.VacationPlanEditFormOpen;
var
  i, TabNumID: Integer;
begin
  i:= StaffList.SelectedIndex;
  TabNumID:= TabNumIDs[i];

  if VacationPlanEditFormShow(YearSpinEdit.Value, TabNumID,
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
      LeftSplitter.Visible:= True;
    end
    else begin
      LeftSplitter.Visible:= False;
      SettingPanel.Visible:= False;
    end;

    StaffList.CanUnselect:= ModeType<>mtEditing;
    StaffList.CanSelect:= ModeType=mtEditing;
    ListToolPanel.Visible:= ModeType=mtEditing;

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

end.

