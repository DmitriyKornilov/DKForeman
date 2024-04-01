unit UStaffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, EditBtn, VirtualTrees, BCPanel, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils,
  //DK packages utils
  DK_VSTTables, DK_VSTTools, DK_Vector, DK_StrUtils, DK_Const, DK_Dialogs,
  //Forms
  UStaffMainEditForm, UStaffTabNumEditForm, UStaffPostlogEditForm;


type

  { TStaffForm }

  TStaffForm = class(TForm)
    ListCaptionPanel: TBCPanel;
    TabNumCaptionPanel: TBCPanel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CloseButton: TSpeedButton;
    ExportButton: TSpeedButton;
    FilterEdit: TEditButton;
    FilterLabel: TLabel;
    FilterPanel: TPanel;
    ListAddButton: TSpeedButton;
    PostLogCaptionPanel: TBCPanel;
    TabNumDismissCancelButton: TSpeedButton;
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
    LeftSplitter: TSplitter;
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
    procedure ListDelButtonClick(Sender: TObject);
    procedure ListEditButtonClick(Sender: TObject);
    procedure PostLogAddButtonClick(Sender: TObject);
    procedure PostLogDelButtonClick(Sender: TObject);
    procedure PostLogEditButtonClick(Sender: TObject);
    procedure PostLogVTNodeDblClick(Sender: TBaseVirtualTree; const {%H-}HitInfo: THitInfo);
    procedure StaffVTNodeDblClick(Sender: TBaseVirtualTree; const {%H-}HitInfo: THitInfo);
    procedure PostButtonClick(Sender: TObject);
    procedure TabNumAddButtonClick(Sender: TObject);
    procedure TabNumDelButtonClick(Sender: TObject);
    procedure TabNumDismissButtonClick(Sender: TObject);
    procedure TabNumDismissCancelButtonClick(Sender: TObject);
    procedure TabNumEditButtonClick(Sender: TObject);
    procedure TabNumVTNodeDblClick(Sender: TBaseVirtualTree; const {%H-}HitInfo: THitInfo);
  private
    CanLoadStaffList: Boolean;
    Percents: Integer;

    ModeType: TModeType;

    SettingValues: TIntVector;

    OrderType: TVSTStringList;
    ListType: TVSTStringList;
    ColumnsList: TVSTCheckTable;
    NameType: TVSTStringList;

    StaffList: TVSTTable;
    StaffIDs, TabNumIDs, Genders: TIntVector;
    BornDates, RecrutDates, DismissDates: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames, Ranks, FullNames, StrGenders: TStrVector;

    TabNumList: TVSTTable;
    TabNumListTabNumIDs, TabNumListPostIDs: TIntVector;
    TabNumListRecrutDates, TabNumListDismissDates: TDateVector;
    TabNumListTabNums, TabNumListPostNames, TabNumListRanks: TStrVector;

    PostLog: TVSTTable;
    PostLogIDs, PostLogPostIDs, PostLogPostTemps: TIntVector;
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

    procedure StaffMainEditFormOpen(const AEditingType: TEditingType);
    procedure StaffTabNumEditFormOpen(const AEditingType: TEditingType);
    procedure StaffTabNumDismissCancel;
    procedure StaffPostLogEditFormOpen(const AEditingType: TEditingType);

    procedure SettingsLoad;
  public
    procedure SettingsSave;
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
  Percents:= 100;
  ModeType:= mtView;

  SetToolPanels([
    ToolPanel, ListToolPanel, TabNumToolPanel, PostLogToolPanel
  ]);
  SetCaptionPanels([
    ListCaptionPanel, TabNumCaptionPanel, PostLogCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, ExportButton, PostButton,
    ListAddButton, ListDelButton, ListEditButton,
    TabNumAddButton, TabNumDelButton, TabNumEditButton, TabNumDismissButton, TabNumDismissCancelButton,
    PostLogAddButton, PostLogDelButton, PostLogEditButton
  ]);

  CanLoadStaffList:= False;
  StaffListCreate;
  ListTypeCreate;
  OrderTypeCreate;
  ColumnsListCreate;
  NameTypeCreate;
  SettingsLoad;
  TabNumListCreate;
  PostLogCreate;
  CanLoadStaffList:= True;

  ChangeMode(mtView);
end;

procedure TStaffForm.SettingsLoad;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_STAFFORM);
  ListType.Select(SettingValues[0]);  //STAFFORM.LISTTYPE
  OrderType.Select(SettingValues[1]); //STAFFORM.ORDERTYPE
  ColumnsList.Selected:= VIntToBool(VCut(SettingValues, 2, 10)); //columns
  NameType.Select(SettingValues[11]); //STAFFORM.NAMETYPE
end;

procedure TStaffForm.SettingsSave;
begin
  SettingValues[0]:= ListType.SelectedIndex;  //STAFFORM.LISTTYPE
  SettingValues[1]:= OrderType.SelectedIndex; //STAFFORM.ORDERTYPE
  VChangeIn(SettingValues, VBoolToInt(ColumnsList.Selected), 2, 10); //columns
  SettingValues[11]:= NameType.SelectedIndex; //STAFFORM.NAMETYPE
  DataBase.SettingsUpdate(SETTING_NAMES_STAFFORM, SettingValues);
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

procedure TStaffForm.ListDelButtonClick(Sender: TObject);
var
  S: String;
begin
  S:= SNameLong(Families[StaffList.SelectedIndex], Names[StaffList.SelectedIndex],
                Patronymics[StaffList.SelectedIndex]) +
      FormatDateTime(' dd.mm.yyyy г.р.', BornDates[StaffList.SelectedIndex]);
  if not Confirm('Удалить всю информацию по "' + S + '"?') then Exit;
  DataBase.StaffMainDelete(StaffIDs[StaffList.SelectedIndex]);
  StaffListLoad;
end;

procedure TStaffForm.ListEditButtonClick(Sender: TObject);
begin
  StaffMainEditFormOpen(etEdit);
end;

procedure TStaffForm.PostLogAddButtonClick(Sender: TObject);
begin
  StaffPostLogEditFormOpen(etAdd);
end;

procedure TStaffForm.PostLogDelButtonClick(Sender: TObject);
begin
  if not Confirm('Удалить информацию о выбранном периоде работы?') then Exit;
  if DataBase.StaffPostLogDelete(PostLogIDs[PostLog.SelectedIndex + 1],
                              PostLogIDs[PostLog.SelectedIndex],
                              PostLogLastDates[PostLog.SelectedIndex]) then
    TabNumListLoad(TabNumListTabNumIDs[TabNumList.SelectedIndex])
end;

procedure TStaffForm.PostLogEditButtonClick(Sender: TObject);
begin
  StaffPostLogEditFormOpen(etEdit);
end;

procedure TStaffForm.PostLogVTNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if not PostLog.IsSelected then Exit;
  StaffPostLogEditFormOpen(etEdit);
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

procedure TStaffForm.TabNumDelButtonClick(Sender: TObject);
var
  S: String;
begin
  S:= TabNumListTabNums[TabNumList.SelectedIndex];
  if not Confirm('Удалить всю информацию по табельному номеру "' + S + '"?') then Exit;
  DataBase.StaffTabNumDelete(TabNumListTabNumIDs[TabNumList.SelectedIndex]);
  TabNumListLoad;
end;

procedure TStaffForm.TabNumAddButtonClick(Sender: TObject);
begin
  StaffTabNumEditFormOpen(etAdd);
end;

procedure TStaffForm.TabNumEditButtonClick(Sender: TObject);
begin
  StaffTabNumEditFormOpen(etEdit);
end;

procedure TStaffForm.TabNumVTNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if (not TabNumList.IsSelected) then Exit;
  if SameDate(INFDATE, TabNumListDismissDates[TabNumList.SelectedIndex]) then
    StaffTabNumEditFormOpen(etEdit)
  else
    StaffTabNumDismissCancel;
end;

procedure TStaffForm.TabNumDismissButtonClick(Sender: TObject);
begin
  StaffTabNumEditFormOpen(etCustom);
end;

procedure TStaffForm.TabNumDismissCancelButtonClick(Sender: TObject);
begin
  StaffTabNumDismissCancel;
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
    'дате увольнения'
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
  try
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
  finally
    StaffList.Visible:= True;
  end;
end;

procedure TStaffForm.StaffListLoad(const AStaffID: Integer = 0);
var
  StrDismissDates: TStrVector;
begin
  if not CanLoadStaffList then Exit;

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
  StrDismissDates:= VDateToStr(DismissDates, True);
  VChangeIf(StrDismissDates, EmptyStr, EMPTY_MARK);

  StaffList.Visible:= False;
  try
    StaffList.ValuesClear;
    StaffList.SetColumn('№ п/п', VIntToStr(VOrder(Length(StaffIDs))));
    NameTypeSelect;
    StaffList.SetColumn('Дата рождения', VDateToStr(BornDates, True));
    StaffList.SetColumn('Пол', StrGenders);
    if ModeType<>mtEditing then
    begin
      StaffList.SetColumn('Табельный номер', TabNums);
      StaffList.SetColumn('Дата приема', VDateToStr(RecrutDates, True));
      StaffList.SetColumn('Дата увольнения', StrDismissDates);
      StaffList.SetColumn('Разряд', Ranks);
      StaffList.SetColumn('Должность', PostNames, taLeftJustify);
    end;
    StaffList.Draw;
    if ModeType=mtEditing then
      StaffList.ReSelect(StaffIDs, AStaffID)  //возвращаем выделение строки
    else
      StaffList.ColumnVisibles:= ColumnsList.Selected;
  finally
    StaffList.Visible:= True;
  end;
end;

procedure TStaffForm.StaffListSelect;
begin
  ListDelButton.Enabled:= StaffList.IsSelected;
  ListEditButton.Enabled:= StaffList.IsSelected;
  TabNumAddButton.Enabled:= StaffList.IsSelected;

  TabNumCaptionPanel.Caption:= 'Табельные номера';
  if StaffList.IsSelected then
    TabNumCaptionPanel.Caption:= TabNumCaptionPanel.Caption + ': ' +
                                 SNameLong(Families[StaffList.SelectedIndex],
                                           Names[StaffList.SelectedIndex],
                                           Patronymics[StaffList.SelectedIndex]);

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
  StrDismissDates: TStrVector;
begin
  if not Assigned(TabNumList) then Exit;
  if ModeType<>mtEditing then Exit;

  StaffID:= 0;
  if StaffList.IsSelected then
    StaffID:= StaffIDs[StaffList.SelectedIndex];
  DataBase.StaffTabNumListLoad(StaffID, TabNumListTabNumIDs, TabNumListPostIDs,
                               TabNumListTabNums, TabNumListPostNames, TabNumListRanks,
                               TabNumListRecrutDates, TabNumListDismissDates);
  StrDismissDates:= VDateToStr(TabNumListDismissDates, True);
  VChangeIf(StrDismissDates, EmptyStr, EMPTY_MARK);

  TabNumList.Visible:= False;
  try
    TabNumList.ValuesClear;
    TabNumList.SetColumn('Табельный номер', TabNumListTabNums);
    TabNumList.SetColumn('Дата приема', VDateToStr(TabNumListRecrutDates, True));
    TabNumList.SetColumn('Дата увольнения', StrDismissDates);
    TabNumList.SetColumn('Разряд', TabNumListRanks);
    TabNumList.SetColumn('Последняя (текущая) должность', TabNumListPostNames, taLeftJustify);
    TabNumList.Draw;
    TabNumList.ReSelect(TabNumListTabNumIDs, ATabNumID); //возвращаем выделение строки
  finally
    TabNumList.Visible:= True;
  end;
end;

procedure TStaffForm.TabNumListSelect;
begin
  TabNumDismissButton.Visible:= TabNumList.IsSelected and
             SameDate(INFDATE, TabNumListDismissDates[TabNumList.SelectedIndex]);
  TabNumDismissCancelButton.Visible:= not TabNumDismissButton.Visible;
  TabNumDelButton.Enabled:= TabNumDismissButton.Visible;
  TabNumEditButton.Enabled:= TabNumDismissButton.Visible;
  TabNumDismissButton.Enabled:= TabNumList.IsSelected;

  PostLogCaptionPanel.Caption:= 'История переводов';
  if TabNumList.IsSelected then
    PostLogCaptionPanel.Caption:= PostLogCaptionPanel.Caption + ' по табельному номеру ' +
                                  TabNumListTabNums[TabNumList.SelectedIndex];
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
  StrLastDates: TStrVector;
begin
  if not Assigned(PostLog) then Exit;
  if ModeType<>mtEditing then Exit;

  TabNumID:= 0;
  if TabNumList.IsSelected then
    TabNumID:= TabNumListTabNumIDs[TabNumList.SelectedIndex];
  DataBase.StaffPostLogListLoad(TabNumID, PostLogIDs, PostLogPostIDs, PostLogPostTemps,
                            PostLogPostNames, PostLogRanks,
                            PostLogFirstDates, PostLogLastDates);
  StrLastDates:= VDateToStr(PostLogLastDates, True);
  VChangeIf(StrLastDates, EmptyStr, EMPTY_MARK);

  PostLog.Visible:= False;
  try
    PostLog.ValuesClear;
    PostLog.SetColumn('Статус должности', VPickFromKey(PostLogPostTemps, POST_TEMP_KEYS, POST_TEMP_PICKS));
    PostLog.SetColumn('Дата начала', VDateToStr(PostLogFirstDates, True));
    PostLog.SetColumn('Дата окончания', StrLastDates);
    PostLog.SetColumn('Разряд', PostLogRanks);
    PostLog.SetColumn('Должность', PostLogPostNames, taLeftJustify);
    PostLog.Draw;
    PostLog.ReSelect(PostLogIDs, APostLogID);  //возвращаем выделение строки
  finally
    PostLog.Visible:= True;
  end;
end;

procedure TStaffForm.PostLogSelect;
var
  IsOK: Boolean;
begin
  IsOK:= TabNumDismissButton.Visible {не уволен} and PostLog.IsSelected;
  PostLogAddButton.Enabled:= IsOK and (PostLog.SelectedIndex=0 {последняя запись});
  PostLogDelButton.Enabled:= IsOK and (PostLog.SelectedIndex<High(PostLogIDs) {не самая первая должность});
  PostLogEditButton.Enabled:= IsOK;
end;

procedure TStaffForm.StaffMainEditFormOpen(const AEditingType: TEditingType);
var
  StaffMainEditForm: TStaffMainEditForm;
begin
  StaffMainEditForm:= TStaffMainEditForm.Create(nil);
  try
    StaffMainEditForm.EditingType:= AEditingType;
    StaffMainEditForm.BornDatePicker.Date:= Date;
    if AEditingType=etEdit then
    begin
      StaffMainEditForm.StaffID:= StaffIDs[StaffList.SelectedIndex];
      StaffMainEditForm.FamilyEdit.Text:= Families[StaffList.SelectedIndex];
      StaffMainEditForm.NameEdit.Text:= Names[StaffList.SelectedIndex];
      StaffMainEditForm.PatronymicEdit.Text:= Patronymics[StaffList.SelectedIndex];
      StaffMainEditForm.BornDatePicker.Date:= BornDates[StaffList.SelectedIndex];
      StaffMainEditForm.GenderComboBox.ItemIndex:= Genders[StaffList.SelectedIndex];
    end;
    if StaffMainEditForm.ShowModal=mrOK then
      StaffListLoad(StaffMainEditForm.StaffID);
  finally
    FreeAndNil(StaffMainEditForm);
  end;
end;

procedure TStaffForm.StaffTabNumEditFormOpen(const AEditingType: TEditingType);
var
  StaffTabNumEditForm: TStaffTabNumEditForm;
begin
  StaffTabNumEditForm:= TStaffTabNumEditForm.Create(nil);
  try
    StaffTabNumEditForm.EditingType:= AEditingType;
    StaffTabNumEditForm.StaffID:= StaffIDs[StaffList.SelectedIndex];
    case AEditingType of
      etAdd:
        begin
          StaffTabNumEditForm.RecrutDatePicker.Date:= Date;
          StaffTabNumEditForm.DismissDatePicker.Date:= INFDATE;
          StaffTabNumEditForm.DismissDatePicker.Enabled:= False;
        end;
      etEdit:
        begin
          StaffTabNumEditForm.TabNumID:= TabNumListTabNumIDs[TabNumList.SelectedIndex];
          StaffTabNumEditForm.TabNumEdit.Text:= TabNumListTabNums[TabNumList.SelectedIndex];
          StaffTabNumEditForm.RecrutDatePicker.Date:= TabNumListRecrutDates[TabNumList.SelectedIndex];
          StaffTabNumEditForm.DismissDatePicker.Date:= INFDATE;
          StaffTabNumEditForm.DismissDatePicker.Enabled:= False;
        end;
      etCustom:
        begin //Dismiss
          StaffTabNumEditForm.TabNumID:= TabNumListTabNumIDs[TabNumList.SelectedIndex];
          StaffTabNumEditForm.TabNumEdit.Text:= TabNumListTabNums[TabNumList.SelectedIndex];
          StaffTabNumEditForm.TabNumEdit.Enabled:= False;
          StaffTabNumEditForm.RecrutDatePicker.Date:= TabNumListRecrutDates[TabNumList.SelectedIndex];
          StaffTabNumEditForm.RecrutDatePicker.Enabled:= False;
          StaffTabNumEditForm.DismissDatePicker.Date:= Date;
          StaffTabNumEditForm.DismissDatePicker.MinDate:= StaffTabNumEditForm.RecrutDatePicker.Date;
        end;
    end;

    if StaffTabNumEditForm.ShowModal=mrOK then
      TabNumListLoad(StaffTabNumEditForm.TabNumID);
  finally
    FreeAndNil(StaffTabNumEditForm);
  end;
end;

procedure TStaffForm.StaffTabNumDismissCancel;
var
  S: String;
begin
  S:= TabNumListTabNums[TabNumList.SelectedIndex];
  if not Confirm('Отменить увольнение по табельному номеру "' + S + '"?') then Exit;
  DataBase.StaffTabNumDismissCancel(TabNumListTabNumIDs[TabNumList.SelectedIndex]);
  TabNumListLoad(TabNumListTabNumIDs[TabNumList.SelectedIndex]);
end;

procedure TStaffForm.StaffPostLogEditFormOpen(const AEditingType: TEditingType);
var
  StaffPostLogEditForm: TStaffPostLogEditForm;
  ThisFirstDate, PrevFirstDate, NextFirstDate: TDate;
begin
  StaffPostLogEditForm:= TStaffPostLogEditForm.Create(nil);
  try
    ThisFirstDate:= PostLogFirstDates[PostLog.SelectedIndex];
    StaffPostLogEditForm.EditingType:= AEditingType;
    StaffPostLogEditForm.TabNumID:= TabNumListTabNumIDs[TabNumList.SelectedIndex];
    StaffPostLogEditForm.PostID:= PostLogPostIDs[PostLog.SelectedIndex];
    StaffPostLogEditForm.PostLogID:= PostLogIDs[PostLog.SelectedIndex];
    StaffPostLogEditForm.FirstDatePicker.Date:= ThisFirstDate;
    case AEditingType of
      etAdd: //перевод с последней должности
        begin
          StaffPostLogEditForm.FirstDatePicker.Date:= IncDay(ThisFirstDate, 1);
          StaffPostLogEditForm.FirstDatePicker.MinDate:= StaffPostLogEditForm.FirstDatePicker.Date;
          StaffPostLogEditForm.FirstDatePicker.MaxDate:= IncDay(INFDATE, -1);
        end;
      etEdit: //редактирование должности
        begin
          StaffPostLogEditForm.FirstDatePicker.Date:= ThisFirstDate;
          if SameDate(ThisFirstDate, TabNumListRecrutDates[TabNumList.SelectedIndex]) then
          begin //первая запись
            StaffPostLogEditForm.StatusComboBox.Enabled:= False; //нельзя менять постоянный статус должности
            StaffPostLogEditForm.FirstDatePicker.Enabled:= False;//нельзя менять дату начала работы (приема)
            StaffPostLogEditForm.PrevPostLogID:= 0;
          end
          else begin //последующие записи
            StaffPostLogEditForm.StatusComboBox.ItemIndex:= PostLogPostTemps[PostLog.SelectedIndex];
            PrevFirstDate:= PostLogFirstDates[PostLog.SelectedIndex + 1];
            StaffPostLogEditForm.PrevPostLogID:= PostLogIDs[PostLog.SelectedIndex + 1];
            StaffPostLogEditForm.FirstDatePicker.MinDate:= IncDay(PrevFirstDate, 1);
            if PostLog.SelectedIndex=0 then //последняя запись
              NextFirstDate:= IncDay(INFDATE, 1)
            else //промежуточная запись
              NextFirstDate:= IncDay(PostLogFirstDates[PostLog.SelectedIndex - 1]);
            StaffPostLogEditForm.FirstDatePicker.MaxDate:= IncDay(NextFirstDate, -1);
          end;
        end;
    end;
    if StaffPostLogEditForm.ShowModal=mrOK then
      TabNumListLoad(TabNumListTabNumIDs[TabNumList.SelectedIndex]);
      //PostLogLoad(StaffPostLogEditForm.PostLogID);
  finally
    FreeAndNil(StaffPostLogEditForm);
  end;

  {DataBase.StaffPostLogListLoad(TabNumID, PostLogIDs, PostLogPostIDs, PostLogPostTemps,
                            PostLogPostNames, PostLogRanks,
                            PostLogFirstDates, PostLogLastDates);}
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
    LeftSplitter.Visible:= True;
  end
  else begin
    LeftSplitter.Visible:= False;
    SettingPanel.Visible:= False;
  end;

  StaffList.CanUnselect:= ModeType<>mtEditing;
  StaffList.CanSelect:= ModeType=mtEditing;
  ListToolPanel.Visible:= ModeType=mtEditing;
  ListCaptionPanel.Visible:= ModeType=mtEditing;
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

