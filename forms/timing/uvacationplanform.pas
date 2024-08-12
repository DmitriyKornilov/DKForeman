unit UVacationPlanForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel, BCPanel, BCButton, Spin, StdCtrls, VirtualTrees, DateUtils,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_VSTTypes, DK_Vector, DK_Filter, DK_StrUtils,
  //Project utils
  UDataBase, UUtils, UUIUtils, UTypes,
  //Forms
  UVacationScheduleForm, UVacationPlanEditForm;

type

  { TVacationPlanForm }

  TVacationPlanForm = class(TForm)
    ListCaptionPanel: TBCPanel;
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    ExportButton: TBCButton;
    FilterPanel: TPanel;
    LeftSplitter: TSplitter;
    EditButton: TSpeedButton;
    ListToolPanel: TPanel;
    MainPanel: TPanel;
    PlanButton: TBCButton;
    SettingCaptionPanel: TBCPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    ToolPanel: TPanel;
    ScheduleButton: TBCButton;
    VT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure NameTypeSelect;

    procedure StaffListCreate;
    procedure StaffListLoad(const SelectedID: Integer = -1);
    procedure StaffListSelect;
    procedure StaffListFilter(const AFilterString: String);

    procedure VacationPlanEditFormOpen;

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
  SetCategoryButtons([
    ExportButton, PlanButton, ScheduleButton
  ]);

  CanLoadStaffList:= False;
  StaffListCreate;
  ParamListCreate;
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

procedure TVacationPlanForm.FormShow(Sender: TObject);
begin


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
  ListCaptionPanel.Caption:= 'Планируемые отпуска в ' + YearSpinEdit.Text + ' году';
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
  StaffList.AddColumn('Должность', 300);
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

  function VacationPart(const AFirstDate: TDate; const ACount, AAddCount: Integer): String;
  begin
    Result:= EmptyStr;
    if Acount=0 then Exit;
    Result:= FormatDateTime('с dd.mm.yyyy на ', AFirstDate) +
             IntToStr(ACount+AAddCount) + ' дней';
  end;

  function VVacationPart(const AFirstDates: TDateVector; const ACounts, AAddCounts: TIntVector): TStrVector;
  var
    i: Integer;
  begin
    Result:= nil;
    if VIsNil(AFirstDates) then Exit;
    VDim(Result, Length(AFirstDates));
    for i:= 0 to High(AFirstDates) do
      Result[i]:= VacationPart(AFirstDates[i], ACounts[i], AAddCounts[i]);
  end;

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
    StaffList.SetColumn('Должность', PostNames, taLeftJustify);
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
  //TabNumID: Integer;
  VacationPlanEditForm: TVacationPlanEditForm;
begin
  //TabNumID:= TabNumIDs[StaffList.SelectedIndex];
  VacationPlanEditForm:= TVacationPlanEditForm.Create(nil);
  try
    VacationPlanEditForm.YearNum:= YearSpinEdit.Value;
    VacationPlanEditForm.TabNumID:= TabNumIDs[StaffList.SelectedIndex];

    if Part1FirstDates[StaffList.SelectedIndex]>0 then
      VacationPlanEditForm.Plan1DatePicker.Date:= Part1FirstDates[StaffList.SelectedIndex];
    if Part1Counts[StaffList.SelectedIndex]>0 then
      VacationPlanEditForm.Plan1CountSpinEdit.Value:= Part1Counts[StaffList.SelectedIndex];
    if Part1AddCounts[StaffList.SelectedIndex]>0 then
      VacationPlanEditForm.Plan1CountAddSpinEdit.Value:= Part1AddCounts[StaffList.SelectedIndex];

    if (Part2FirstDates[StaffList.SelectedIndex]>0) and
       (Part2Counts[StaffList.SelectedIndex]>0) then
    begin
      VacationPlanEditForm.Plan2CheckBox.Checked:= True;
      VacationPlanEditForm.Plan2DatePicker.Date:= Part2FirstDates[StaffList.SelectedIndex];
      VacationPlanEditForm.Plan2CountSpinEdit.Value:= Part2Counts[StaffList.SelectedIndex];
      if Part2AddCounts[StaffList.SelectedIndex]>0 then
       VacationPlanEditForm.Plan2CountAddSpinEdit.Value:= Part2AddCounts[StaffList.SelectedIndex];
    end;

    if VacationPlanEditForm.ShowModal=mrOK then
      StaffListLoad(VacationPlanEditForm.TabNumID);
  finally
    FreeAndNil(VacationPlanEditForm);
  end;

end;

procedure TVacationPlanForm.SettingsLoad;
begin

end;

procedure TVacationPlanForm.SettingsSave;
begin

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

    //StaffListUpdate;

  finally
    MainPanel.Visible:= True;
  end;
end;

end.

