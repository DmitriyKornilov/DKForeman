unit UStaffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, EditBtn, VirtualTrees,
  //Project utils
  UDBUtils, UConst, UTypes, UUtils,
  //DK packages utils
  DK_VSTTables, DK_VSTTools, DK_Vector, DK_CtrlUtils, DK_StrUtils,
  //Forms
  UStaffMainEditForm;


type

  { TStaffForm }

  TStaffForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CloseButton: TSpeedButton;
    ExportButton: TSpeedButton;
    FilterEdit: TEditButton;
    FilterLabel: TLabel;
    FilterPanel: TPanel;
    ListAddButton: TSpeedButton;
    TabNumVT: TVirtualStringTree;
    TabNumAddButton: TSpeedButton;
    PostLogAddButton: TSpeedButton;
    TabNumDelButton: TSpeedButton;
    ListEditButton: TSpeedButton;
    PostLogDelButton: TSpeedButton;
    TabNumEditButton: TSpeedButton;
    TabNumDismissButton: TSpeedButton;
    PostLogEditButton: TSpeedButton;
    TabNumToolPanel: TPanel;
    MainPanel: TPanel;
    ListPanel: TPanel;
    ListDelButton: TSpeedButton;
    EditingPanel: TPanel;
    HorizEditingSplitter: TSplitter;
    PostLogEditingPanel: TPanel;
    TabNumEditingPanel: TPanel;
    SettingPanel: TPanel;
    PostButton: TSpeedButton;
    SettingSplitter: TSplitter;
    PostLogToolPanel: TPanel;
    PostLogVT: TVirtualStringTree;
    EditingSplitter: TSplitter;
    ToolPanel: TPanel;
    ListToolPanel: TPanel;
    StaffVT: TVirtualStringTree;
    ListTypeVT: TVirtualStringTree;
    OrderTypeVT: TVirtualStringTree;
    ColumnsListVT: TVirtualStringTree;
    NameTypeVT: TVirtualStringTree;
    procedure CloseButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FilterEditButtonClick(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListAddButtonClick(Sender: TObject);
    procedure ListEditButtonClick(Sender: TObject);
    procedure StaffVTNodeDblClick(Sender: TBaseVirtualTree;
      const {%H-}HitInfo: THitInfo);
    procedure PostButtonClick(Sender: TObject);
  private
    ModeType: TModeType;

    OrderType: TVSTStringList;
    ListType: TVSTStringList;
    ColumnsList: TVSTCheckTable;
    NameType: TVSTStringList;

    StaffList: TVSTTable;
    StaffIDs, TabNumIDs, Genders: TIntVector;
    BornDates, RecrutDates, DismissDates: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames, Ranks, FullNames, StrGenders: TStrVector;

    TabNumList: TVSTTable;
    TabNumListTabNumIDs: TIntVector;
    TabNumListRecrutDates, TabNumListDismissDates: TDateVector;
    TabNumListTabNums, TabNumListPostNames, TabNumListRanks: TStrVector;

    PostLog: TVSTTable;
    PostLogIDs, PostLogPostTemps: TIntVector;
    PostLogFirstDates, PostLogLastDates: TDateVector;
    PostLogPostNames, PostLogRanks: TStrVector;

    procedure OrderTypeCreate;
    procedure OrderTypeSelect;
    procedure ListTypeCreate;
    procedure ListTypeSelect;
    procedure ColumnsListCreate;
    procedure ColumnsListSelect;
    procedure NameTypeCreate;
    procedure NameTypeSelect;

    procedure StaffListUpdate;
    procedure StaffListCreate;
    procedure StaffListColumnSet;
    procedure StaffListLoad(const AStaffID: Integer = 0);
    procedure StaffListSelect;

    procedure TabNumListCreate;
    procedure TabNumListLoad(const ATabNumID: Integer = 0);
    procedure TabNumListSelect;

    procedure PostLogCreate;
    procedure PostLogLoad(const APostLogID: Integer = 0);
    procedure PostLogSelect;

    procedure StaffMainEditFormOpen(const EditingType: TEditingType);
  public
    procedure ChangeMode(const AModeType: TModeType);
  end;

var
  StaffForm: TStaffForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TStaffForm }

procedure TStaffForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TStaffForm.ExportButtonClick(Sender: TObject);
begin
  StaffList.Save([ctInteger, //№ п/п
                  ctString,  //Ф.И.О
                  ctDate,    //дата рождения
                  ctString,  //пол
                  ctString,  //табельный номер
                  ctDate,    //дата приема
                  ctDate,    //дата увольнения
                  ctString,  //разряд
                  ctString   //должность

  ]);
end;

procedure TStaffForm.FilterEditButtonClick(Sender: TObject);
begin
  FilterEdit.Text:= EmptyStr;
end;

procedure TStaffForm.FilterEditChange(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TStaffForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  ControlWidth(CloseButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(ExportButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(PostButton, TOOL_BUTTON_WIDTH_DEFAULT);

  ControlHeight(ListToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  ControlWidth(ListAddButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(ListDelButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(ListEditButton, TOOL_BUTTON_WIDTH_DEFAULT);

  ControlHeight(TabNumToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  ControlWidth(TabNumAddButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(TabNumDelButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(TabNumEditButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(TabNumDismissButton, TOOL_BUTTON_WIDTH_DEFAULT);

  ControlHeight(PostLogToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  ControlWidth(PostLogAddButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(PostLogDelButton, TOOL_BUTTON_WIDTH_DEFAULT);
  ControlWidth(PostLogEditButton, TOOL_BUTTON_WIDTH_DEFAULT);

  StaffListCreate;
  ListTypeCreate;
  OrderTypeCreate;
  ColumnsListCreate;
  NameTypeCreate;

  TabNumListCreate;
  PostLogCreate;

  ChangeMode(mtView);
end;

procedure TStaffForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StaffList);
  FreeAndNil(OrderType);
  FreeAndNil(ListType);
  FreeAndNil(ColumnsList);
  FreeAndNil(NameType);
  FreeAndNil(TabNumList);
  FreeAndNil(PostLog);
end;

procedure TStaffForm.ListAddButtonClick(Sender: TObject);
begin
  StaffMainEditFormOpen(etAdd);
end;

procedure TStaffForm.ListEditButtonClick(Sender: TObject);
begin
  StaffMainEditFormOpen(etEdit);
end;

procedure TStaffForm.StaffVTNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if not StaffList.IsSelected then Exit;
  StaffMainEditFormOpen(etEdit);
end;

procedure TStaffForm.PostButtonClick(Sender: TObject);
begin
  MainForm.DictionarySelect(1);
end;

procedure TStaffForm.OrderTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Сортировать список по:';
  V:= VCreateStr([
    'Ф.И.О.',
    'табельному номеру',
    'должности (профессии)',
    'дате рождения',
    'дате приема',
    'дате увольнения',
    'разряду'
  ]);
  OrderType:= TVSTStringList.Create(OrderTypeVT, S, @OrderTypeSelect);
  OrderType.Update(V);
end;

procedure TStaffForm.OrderTypeSelect;
begin
  StaffListLoad;
end;

procedure TStaffForm.ListTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Включать в список:';
  V:= VCreateStr([
    'всех',
    'работающих на текущую дату',
    'уволенных на текущую дату',
    'без табельного номера'
  ]);
  ListType:= TVSTStringList.Create(ListTypeVT, S, @ListTypeSelect);
  ListType.Update(V);
  ListType.Select(1);
end;

procedure TStaffForm.ListTypeSelect;
begin
  StaffListLoad;
end;

procedure TStaffForm.ColumnsListCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Отображать столбцы:';
  V:= VCreateStr([
    '№ п/п',
    'Ф.И.О',
    'дата рождения',
    'пол',
    'табельный номер',
    'дата приема',
    'дата увольнения',
    'разряд',
    'должность'
  ]);
  ColumnsList:= TVSTCheckList.Create(ColumnsListVT, S, V, @ColumnsListSelect);
end;

procedure TStaffForm.ColumnsListSelect;
begin
  StaffList.ColumnVisibles:= ColumnsList.Selected;
end;

procedure TStaffForm.NameTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Формат имени:';
  V:= VCreateStr([
    'Фамилия Имя Отчество',
    'Фамилия И.О.'
  ]);
  NameType:= TVSTStringList.Create(NameTypeVT, S, @NameTypeSelect);
  NameType.Update(V);
end;

procedure TStaffForm.NameTypeSelect;
begin
  if not NameType.IsSelected then Exit;
  if ModeType=mtEditing then
    FullNames:= VNameLong(Families, Names, Patronymics)
  else if (NameType.SelectedIndex=1)  then
    FullNames:= VNameShort(Families, Names, Patronymics)
  else
    FullNames:= VNameLong(Families, Names, Patronymics);
  StaffList.SetColumn('Ф.И.О', FullNames, taLeftJustify);
  StaffList.Refresh;
end;

procedure TStaffForm.StaffListUpdate;
begin
  StaffListColumnSet;
  StaffListLoad;
end;

procedure TStaffForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffVT);
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(MainForm.GridFont);
  StaffList.HeaderFont.Style:= [fsBold];
end;

procedure TStaffForm.StaffListColumnSet;
begin
  StaffList.Visible:= False;

  StaffList.Clear;
  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Ф.И.О', 220);
  StaffList.AddColumn('Дата рождения', 120);
  StaffList.AddColumn('Пол', 50);
  if ModeType<>mtEditing then
  begin
    StaffList.AddColumn('Табельный номер', 120);
    StaffList.AddColumn('Дата приема', 120);
    StaffList.AddColumn('Дата увольнения', 120);
    StaffList.AddColumn('Разряд', 60);
    StaffList.AddColumn('Должность', 200);
    StaffList.AutosizeColumnEnable('Должность');
  end
  else
    StaffList.AutosizeColumnEnable('Ф.И.О');
  StaffList.Draw;

  StaffList.Visible:= True;
end;

procedure TStaffForm.StaffListLoad(const AStaffID: Integer = 0);
begin
  if (not Assigned(OrderType)) or (not Assigned(ListType)) then Exit;
  if ModeType=mtEditing then
    DataBase.StaffMainListLoad(STrimLeft(FilterEdit.Text), StaffIDs, Genders,
                               Families, Names, Patronymics, BornDates)
  else
    DataBase.StaffListLoad(OrderType.SelectedIndex, ListType.SelectedIndex,
                           StaffIDs, TabNumIDs, Genders,
                           BornDates, RecrutDates, DismissDates,
                           Families, Names, Patronymics, TabNums, PostNames, Ranks);

  StrGenders:= VPickFromKey(Genders, GENDER_KEYS, GENDER_PICKS);
  StaffList.ValuesClear;
  StaffList.SetColumn('№ п/п', VIntToStr(VOrder(Length(StaffIDs))));
  NameTypeSelect;
  StaffList.SetColumn('Дата рождения', VDateToStr(BornDates, True));
  StaffList.SetColumn('Пол', StrGenders);
  if ModeType<>mtEditing then
  begin
    StaffList.SetColumn('Табельный номер', TabNums);
    StaffList.SetColumn('Дата приема', VDateToStr(RecrutDates, True));
    StaffList.SetColumn('Дата увольнения', VDateToStr(DismissDates, True));
    StaffList.SetColumn('Разряд', Ranks);
    StaffList.SetColumn('Должность', PostNames, taLeftJustify);
  end;
  StaffList.Draw;

  //возвращаем выделение строки
  if ModeType=mtEditing then
    ReSelectTableRow(StaffList, StaffIDs, AStaffID);
end;

procedure TStaffForm.StaffListSelect;
begin
  ListDelButton.Enabled:= StaffList.IsSelected;
  ListEditButton.Enabled:= StaffList.IsSelected;
  TabNumAddButton.Enabled:= StaffList.IsSelected;
  TabNumListLoad;
end;

procedure TStaffForm.TabNumListCreate;
begin
  TabNumList:= TVSTTable.Create(TabNumVT);
  TabNumList.CanSelect:= True;
  TabNumList.CanUnselect:= False;
  TabNumList.OnSelect:= @TabNumListSelect;
  TabNumList.SetSingleFont(MainForm.GridFont);
  TabNumList.HeaderFont.Style:= [fsBold];

  TabNumList.AddColumn('Табельный номер', 120);
  TabNumList.AddColumn('Дата приема', 120);
  TabNumList.AddColumn('Дата увольнения', 120);
  TabNumList.AddColumn('Разряд', 60);
  TabNumList.AddColumn('Последняя (текущая) должность', 200);
  TabNumList.Draw;
end;

procedure TStaffForm.TabNumListLoad(const ATabNumID: Integer);
var
  StaffID: Integer;
begin
  if not Assigned(TabNumList) then Exit;
  if ModeType<>mtEditing then Exit;
  TabNumList.ValuesClear;
  StaffID:= 0;
  if StaffList.IsSelected then
    StaffID:= StaffIDs[StaffList.SelectedIndex];
  DataBase.StaffTabNumListLoad(StaffID, TabNumListTabNumIDs, TabNumListTabNums,
                               TabNumListPostNames, TabNumListRanks,
                               TabNumListRecrutDates, TabNumListDismissDates);

  TabNumList.SetColumn('Табельный номер', TabNumListTabNums);
  TabNumList.SetColumn('Дата приема', VDateToStr(TabNumListRecrutDates, True));
  TabNumList.SetColumn('Дата увольнения', VDateToStr(TabNumListDismissDates, True));
  TabNumList.SetColumn('Разряд', TabNumListRanks);
  TabNumList.SetColumn('Последняя (текущая) должность', TabNumListPostNames, taLeftJustify);

  TabNumList.Draw;

  //возвращаем выделение строки
  ReSelectTableRow(TabNumList, TabNumListTabNumIDs, ATabNumID);
end;

procedure TStaffForm.TabNumListSelect;
begin
  TabNumDelButton.Enabled:= TabNumList.IsSelected;
  TabNumEditButton.Enabled:= TabNumList.IsSelected;
  TabNumDismissButton.Enabled:= TabNumList.IsSelected;
  PostLogLoad;
end;

procedure TStaffForm.PostLogCreate;
begin
  PostLog:= TVSTTable.Create(PostLogVT);
  PostLog.CanSelect:= True;
  PostLog.CanUnselect:= False;
  PostLog.OnSelect:= @PostLogSelect;
  PostLog.SetSingleFont(MainForm.GridFont);
  PostLog.HeaderFont.Style:= [fsBold];

  PostLog.AddColumn('Статус должности', 120);
  PostLog.AddColumn('Дата начала', 120);
  PostLog.AddColumn('Дата окончания', 120);
  PostLog.AddColumn('Разряд', 60);
  PostLog.AddColumn('Должность', 200);
  PostLog.Draw;
end;

procedure TStaffForm.PostLogLoad(const APostLogID: Integer);
var
  TabNumID: Integer;
begin
  if not Assigned(PostLog) then Exit;
  if ModeType<>mtEditing then Exit;
  PostLog.ValuesClear;
  TabNumID:= 0;
  if TabNumList.IsSelected then
    TabNumID:= TabNumListTabNumIDs[TabNumList.SelectedIndex];

  DataBase.StaffPostLogListLoad(TabNumID, PostLogIDs, PostLogPostTemps,
                            PostLogPostNames, PostLogRanks,
                            PostLogFirstDates, PostLogLastDates);

  PostLog.SetColumn('Статус должности', VPickFromKey(PostLogPostTemps, POST_TEMP_KEYS, POST_TEMP_PICKS));
  PostLog.SetColumn('Дата начала', VDateToStr(PostLogFirstDates, True));
  PostLog.SetColumn('Дата окончания', VDateToStr(PostLogLastDates, True));
  PostLog.SetColumn('Разряд', PostLogRanks);
  PostLog.SetColumn('Должность', PostLogPostNames, taLeftJustify);

  PostLog.Draw;

  //возвращаем выделение строки
  ReSelectTableRow(PostLog, PostLogIDs, APostLogID);
end;

procedure TStaffForm.PostLogSelect;
begin
  PostLogAddButton.Enabled:= PostLog.SelectedIndex=0;
  PostLogDelButton.Enabled:= PostLog.IsSelected;
  PostLogEditButton.Enabled:= PostLog.IsSelected;
end;

procedure TStaffForm.StaffMainEditFormOpen(const EditingType: TEditingType);
var
  StaffMainEditForm: TStaffMainEditForm;
begin
  StaffMainEditForm:= TStaffMainEditForm.Create(nil);
  try
    StaffMainEditForm.StaffID:= 0;
    if EditingType=etEdit then
    begin
      StaffMainEditForm.StaffID:= StaffIDs[StaffList.SelectedIndex];
      StaffMainEditForm.FamilyEdit.Text:= Families[StaffList.SelectedIndex];
      StaffMainEditForm.NameEdit.Text:= Names[StaffList.SelectedIndex];
      StaffMainEditForm.PatronymicEdit.Text:= Patronymics[StaffList.SelectedIndex];
      StaffMainEditForm.BornDateTimePicker.Date:= BornDates[StaffList.SelectedIndex];
      StaffMainEditForm.GenderComboBox.ItemIndex:= Genders[StaffList.SelectedIndex];
    end;
    if StaffMainEditForm.ShowModal=mrOK then
      StaffListLoad(StaffMainEditForm.StaffID);
  finally
    FreeAndNil(StaffMainEditForm);
  end;
end;

procedure TStaffForm.ChangeMode(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;


  ModeType:= AModeType;
  ExportButton.Enabled:= ModeType<>mtEditing;

  StaffListUpdate;

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
  ListToolPanel.Visible:= ModeType=mtEditing;
  if ModeType=mtEditing then
  begin
    EditingPanel.Width:= Round(ClientWidth*2/3);
    EditingPanel.Visible:= True;
    EditingSplitter.Visible:= True;
    if not VIsNil(StaffIDs) then
      StaffList.Select(0);
  end
  else begin
    EditingSplitter.Visible:= False;
    EditingPanel.Visible:= False;

    if StaffList.IsSelected then
      StaffList.UnSelect;
  end;

  MainPanel.Visible:= True;
end;

end.

