unit UStaffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, DividerBevel, VirtualTrees, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UImages,
  //DK packages utils
  DK_VSTTypes, DK_VSTTables, DK_VSTParamList, DK_Vector, DK_StrUtils, DK_Const,
  DK_Dialogs, DK_DateUtils, DK_Filter, DK_CtrlUtils,
  //Forms
  UStaffMainEditForm, UStaffTabNumEditForm, UStaffPostlogEditForm;

type

  { TStaffForm }

  TStaffForm = class(TForm)
    AscendingButton: TSpeedButton;
    DescendingButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    ExportButton: TSpeedButton;
    FIORadioButton: TRadioButton;
    ListOrderToolPanel: TPanel;
    SettingCaptionPanel: TPanel;
    OrderButtonPanel: TPanel;
    OrderLabel: TLabel;
    BornDateRadioButton: TRadioButton;
    ListCaptionPanel: TPanel;
    EditingSplitter: TSplitter;
    TabNumCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    CloseButton: TSpeedButton;
    FilterPanel: TPanel;
    ListAddButton: TSpeedButton;
    PostLogCaptionPanel: TPanel;
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
    LeftSplitter: TSplitter;
    PostLogToolPanel: TPanel;
    PostLogVT: TVirtualStringTree;
    ToolPanel: TPanel;
    ListToolPanel: TPanel;
    StaffVT: TVirtualStringTree;
    procedure AscendingButtonClick(Sender: TObject);
    procedure BornDateRadioButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DescendingButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListAddButtonClick(Sender: TObject);
    procedure ListDelButtonClick(Sender: TObject);
    procedure ListEditButtonClick(Sender: TObject);
    procedure PostLogAddButtonClick(Sender: TObject);
    procedure PostLogDelButtonClick(Sender: TObject);
    procedure PostLogEditButtonClick(Sender: TObject);
    procedure PostLogVTDblClick(Sender: TObject);
    procedure StaffVTDblClick(Sender: TObject);
    procedure TabNumAddButtonClick(Sender: TObject);
    procedure TabNumDelButtonClick(Sender: TObject);
    procedure TabNumDismissButtonClick(Sender: TObject);
    procedure TabNumDismissCancelButtonClick(Sender: TObject);
    procedure TabNumEditButtonClick(Sender: TObject);
    procedure TabNumVTDblClick(Sender: TObject);
  private
    CanLoadStaffList: Boolean;
    //ZoomPercent: Integer;
    FilterString: String;
    ModeType: TModeType;

    ParamList: TVSTParamList;

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

    procedure ParamListCreate;
    procedure OrderTypeSelect;
    procedure ListTypeSelect;
    procedure ColumnsListSelect;
    procedure NameTypeSelect;

    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListCreate;
    procedure StaffListColumnSet;
    procedure StaffListLoad(const SelectedID: Integer = -1);
    procedure StaffListUpdate;
    procedure StaffListSelect;
    procedure StaffListDelItem;
    procedure StaffListEditItem;

    procedure TabNumListCreate;
    procedure TabNumListLoad(const SelectedID: Integer = -1);
    procedure TabNumListSelect;
    procedure TabNumListDelItem;
    procedure TabNumListEditItem;

    procedure PostLogCreate;
    procedure PostLogLoad(const SelectedID: Integer = -1);
    procedure PostLogSelect;
    procedure PostLogDelItem;
    procedure PostLogEditItem;

    procedure StaffMainEditFormOpen(const AEditingType: TEditingType);
    procedure StaffTabNumEditFormOpen(const AEditingType: TEditingType);
    procedure StaffTabNumDismissCancel;
    procedure StaffPostLogEditFormOpen(const AEditingType: TEditingType);

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
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

procedure TStaffForm.DescendingButtonClick(Sender: TObject);
begin
  DescendingButton.Visible:= False;
  AscendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TStaffForm.AscendingButtonClick(Sender: TObject);
begin
  AscendingButton.Visible:= False;
  DescendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TStaffForm.BornDateRadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
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

procedure TStaffForm.FIORadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TStaffForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  //ZoomPercent:= 100;

  SetToolPanels([
    ToolPanel, ListToolPanel, ListOrderToolPanel, TabNumToolPanel, PostLogToolPanel
  ]);
  SetCaptionPanels([
    SettingCaptionPanel, ListCaptionPanel, TabNumCaptionPanel, PostLogCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton,
    ListAddButton, ListDelButton, ListEditButton,
    TabNumAddButton, TabNumDelButton, TabNumEditButton, TabNumDismissButton, TabNumDismissCancelButton,
    PostLogAddButton, PostLogDelButton, PostLogEditButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton, AscendingButton, DescendingButton,
    ListAddButton, ListDelButton, ListEditButton,
    TabNumAddButton, TabNumDelButton, TabNumEditButton, TabNumDismissButton, TabNumDismissCancelButton,
    PostLogAddButton, PostLogDelButton, PostLogEditButton
  ]);

  CanLoadStaffList:= False;
  StaffListCreate;
  ParamListCreate;
  SettingsLoad;
  TabNumListCreate;
  PostLogCreate;
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter);
  CanLoadStaffList:= True;
end;

procedure TStaffForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_STAFFORM);
end;

procedure TStaffForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_STAFFORM, ParamList.Params);
end;

procedure TStaffForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StaffList);
  FreeAndNil(ParamList);
  FreeAndNil(TabNumList);
  FreeAndNil(PostLog);
end;

procedure TStaffForm.FormShow(Sender: TObject);
begin
  EditingPanel.Width:= Round(ClientWidth*2/3);
end;

procedure TStaffForm.ListAddButtonClick(Sender: TObject);
begin
  StaffMainEditFormOpen(etAdd);
end;

procedure TStaffForm.StaffListDelItem;
var
  S: String;
begin
  if not ListDelButton.Enabled then Exit;
  S:= SNameLong(Families[StaffList.SelectedIndex], Names[StaffList.SelectedIndex],
                Patronymics[StaffList.SelectedIndex]) +
      FormatDateTime(' dd.mm.yyyy г.р.', BornDates[StaffList.SelectedIndex]);
  if not Confirm('Удалить всю информацию по "' + S + '"?') then Exit;
  DataBase.StaffMainDelete(StaffIDs[StaffList.SelectedIndex]);
  StaffListLoad;
end;

procedure TStaffForm.StaffListEditItem;
begin
  if not StaffList.IsSelected then Exit;
  StaffMainEditFormOpen(etEdit);
end;

procedure TStaffForm.ListDelButtonClick(Sender: TObject);
begin
 StaffListDelItem;
end;

procedure TStaffForm.ListEditButtonClick(Sender: TObject);
begin
  StaffListEditItem;
end;

procedure TStaffForm.PostLogAddButtonClick(Sender: TObject);
begin
  StaffPostLogEditFormOpen(etAdd);
end;

procedure TStaffForm.PostLogDelItem;
begin
  if not PostLogDelButton.Enabled then Exit;
  if not Confirm('Удалить информацию о выбранном периоде работы в должности?') then Exit;
  if DataBase.StaffPostLogDelete(PostLogIDs[PostLog.SelectedIndex + 1],
                              PostLogIDs[PostLog.SelectedIndex],
                              PostLogLastDates[PostLog.SelectedIndex]) then
    TabNumListLoad;
end;

procedure TStaffForm.PostLogEditItem;
begin
  if not PostLog.IsSelected then Exit;
  StaffPostLogEditFormOpen(etEdit);
end;

procedure TStaffForm.PostLogDelButtonClick(Sender: TObject);
begin
  PostLogDelItem;
end;

procedure TStaffForm.PostLogEditButtonClick(Sender: TObject);
begin
  PostLogEditItem;
end;

procedure TStaffForm.PostLogVTDblClick(Sender: TObject);
begin
  PostLogEditItem;
end;

procedure TStaffForm.StaffVTDblClick(Sender: TObject);
begin
  StaffListEditItem;
end;

procedure TStaffForm.TabNumListDelItem;
var
  S: String;
begin
  if not TabNumDelButton.Enabled then Exit;
  S:= TabNumListTabNums[TabNumList.SelectedIndex];
  if not Confirm('Удалить всю информацию по табельному номеру "' + S + '"?') then Exit;
  DataBase.StaffTabNumDelete(TabNumListTabNumIDs[TabNumList.SelectedIndex]);
  TabNumListLoad;
end;

procedure TStaffForm.TabNumListEditItem;
begin
  if not TabNumList.IsSelected then Exit;
  StaffTabNumEditFormOpen(etEdit);
end;

procedure TStaffForm.TabNumDelButtonClick(Sender: TObject);
begin
  TabNumListDelItem;
end;

procedure TStaffForm.TabNumAddButtonClick(Sender: TObject);
begin
  StaffTabNumEditFormOpen(etAdd);
end;

procedure TStaffForm.TabNumEditButtonClick(Sender: TObject);
begin
  TabNumListEditItem;
end;

procedure TStaffForm.TabNumVTDblClick(Sender: TObject);
begin
  if not TabNumList.IsSelected then Exit;
  if SameDate(INFDATE, TabNumListDismissDates[TabNumList.SelectedIndex]) then
    TabNumListEditItem
  else
    StaffTabNumDismissCancel;
end;

procedure TStaffForm.ParamListCreate;
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
    'дате рождения',
    'дате приема',
    'дате увольнения'
  ]);
  ParamList.AddStringList('OrderType', S, V, @OrderTypeSelect);

  S:= 'Включать в список:';
  V:= VCreateStr([
    'всех',
    'работающих на текущую дату',
    'уволенных на текущую дату',
    'без табельного номера'
  ]);
  ParamList.AddStringList('ListType', S, V, @ListTypeSelect, 1);

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
  ParamList.AddCheckList('ColumnsList', S, V, @ColumnsListSelect);

  S:= 'Формат имени:';
  V:= VCreateStr([
    'Фамилия Имя Отчество',
    'Фамилия И.О.'
  ]);
  ParamList.AddStringList('NameType', S, V, @NameTypeSelect);
end;

procedure TStaffForm.TabNumDismissButtonClick(Sender: TObject);
begin
  StaffTabNumEditFormOpen(etCustom);
end;

procedure TStaffForm.TabNumDismissCancelButtonClick(Sender: TObject);
begin
  StaffTabNumDismissCancel;
end;

procedure TStaffForm.OrderTypeSelect;
begin
  StaffListLoad;
end;

procedure TStaffForm.ListTypeSelect;
begin
  StaffListLoad;
end;

procedure TStaffForm.ColumnsListSelect;
begin
  StaffList.ColumnVisibles:= ParamList.Checkeds['ColumnsList'];
end;

procedure TStaffForm.NameTypeSelect;
begin
  if not ParamList.IsSelected['NameType'] then Exit;
  if ModeType=mtEditing then
    FullNames:= VNameLong(Families, Names, Patronymics)
  else if (ParamList.Selected['NameType']=1)  then
    FullNames:= VNameShort(Families, Names, Patronymics)
  else
    FullNames:= VNameLong(Families, Names, Patronymics);
  StaffList.SetColumn('Ф.И.О', FullNames, taLeftJustify);
  StaffList.Refresh;
end;

procedure TStaffForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TStaffForm.StaffListUpdate;
var
  SelectedID: Integer;
begin
  SelectedID:= GetSelectedID(StaffList, StaffIDs);
  StaffListColumnSet;
  StaffListLoad(SelectedID);
end;

procedure TStaffForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffVT);
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.OnDelKeyDown:= @StaffListDelItem;
  StaffList.OnReturnKeyDown:= @StaffListEditItem;
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

procedure TStaffForm.StaffListLoad(const SelectedID: Integer = -1);
var
  StrDismissDates: TStrVector;
  SelectedStaffID: Integer;
  IsDescOrder: Boolean;
  ListOrderType: Byte;
begin
  if not CanLoadStaffList then Exit;

  SelectedStaffID:= GetSelectedID(StaffList, StaffIDs, SelectedID);

  if ModeType=mtEditing then
  begin
    if FIORadioButton.Checked then
      ListOrderType:= 0
    else
      ListOrderType:= 1;
    IsDescOrder:= not DescendingButton.Visible;
    DataBase.StaffMainListLoad(STrimLeft(FilterString), ListOrderType, IsDescOrder,
                       StaffIDs, Genders, Families, Names, Patronymics, BornDates);
  end
  else
    DataBase.StaffListLoad(ParamList.Selected['OrderType'],
                           ParamList.Selected['ListType'],
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
      StaffList.ReSelect(StaffIDs, SelectedStaffID, True)  //возвращаем выделение строки
    else
      StaffList.ColumnVisibles:= ParamList.Checkeds['ColumnsList'];
  finally
    StaffList.Visible:= True;
  end;
end;

procedure TStaffForm.StaffListSelect;
begin
  ListDelButton.Enabled:= StaffList.IsSelected;
  ListEditButton.Enabled:= StaffList.IsSelected;
  TabNumAddButton.Enabled:= StaffList.IsSelected;

  TabNumCaptionPanel.Caption:= '  Табельные номера';
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
  TabNumList.OnDelKeyDown:= @TabNumListDelItem;
  TabNumList.OnReturnKeyDown:= @TabNumListEditItem;
  TabNumList.SetSingleFont(MainForm.GridFont);
  TabNumList.HeaderFont.Style:= [fsBold];

  TabNumList.AddColumn('Табельный номер', 120);
  TabNumList.AddColumn('Дата приема', 120);
  TabNumList.AddColumn('Дата увольнения', 120);
  TabNumList.AddColumn('Разряд', 60);
  TabNumList.AddColumn('Последняя (текущая) должность', 200);
  TabNumList.Draw;
end;

procedure TStaffForm.TabNumListLoad(const SelectedID: Integer = -1);
var
  StaffID: Integer;
  StrDismissDates: TStrVector;
  SelectedTabNumID: Integer;
begin
  if not Assigned(TabNumList) then Exit;
  if ModeType<>mtEditing then Exit;

  SelectedTabNumID:= GetSelectedID(TabNumList, TabNumListTabNumIDs, SelectedID);

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
    TabNumList.ReSelect(TabNumListTabNumIDs, SelectedTabNumID, True); //возвращаем выделение строки
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

  PostLogCaptionPanel.Caption:= '  История переводов';
  if TabNumList.IsSelected then
    PostLogCaptionPanel.Caption:= PostLogCaptionPanel.Caption + ' по табельному номеру: ' +
                                  TabNumListTabNums[TabNumList.SelectedIndex];
  PostLogLoad;
end;

procedure TStaffForm.PostLogCreate;
begin
  PostLog:= TVSTTable.Create(PostLogVT);
  PostLog.CanSelect:= True;
  PostLog.CanUnselect:= False;
  PostLog.OnSelect:= @PostLogSelect;
  PostLog.OnDelKeyDown:= @PostLogDelItem;
  PostLog.OnReturnKeyDown:= @PostLogEditItem;
  PostLog.SetSingleFont(MainForm.GridFont);
  PostLog.HeaderFont.Style:= [fsBold];

  PostLog.AddColumn('Статус должности', 120);
  PostLog.AddColumn('Дата начала', 120);
  PostLog.AddColumn('Дата окончания', 120);
  PostLog.AddColumn('Разряд', 60);
  PostLog.AddColumn('Должность', 200);
  PostLog.Draw;
end;

procedure TStaffForm.PostLogLoad(const SelectedID: Integer = -1);
var
  TabNumID: Integer;
  StrLastDates: TStrVector;
  SelectedPostLogID: Integer;
begin
  if not Assigned(PostLog) then Exit;
  if ModeType<>mtEditing then Exit;

  SelectedPostLogID:= GetSelectedID(PostLog, PostLogIDs, SelectedID);

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
    PostLog.ReSelect(PostLogIDs, SelectedPostLogID, True);  //возвращаем выделение строки
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
      StaffMainEditForm.GenderDropDown.ItemIndex:= Genders[StaffList.SelectedIndex];
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
  ThisFirstDate, PrevFirstDate, NextFirstDate, D: TDate;
begin
  StaffPostLogEditForm:= TStaffPostLogEditForm.Create(nil);
  try
    ThisFirstDate:= PostLogFirstDates[PostLog.SelectedIndex];
    StaffPostLogEditForm.EditingType:= AEditingType;
    StaffPostLogEditForm.TabNumID:= TabNumListTabNumIDs[TabNumList.SelectedIndex];
    StaffPostLogEditForm.PostID:= PostLogPostIDs[PostLog.SelectedIndex];
    StaffPostLogEditForm.PostLogID:= PostLogIDs[PostLog.SelectedIndex];
    case AEditingType of
      etAdd: //перевод с последней должности
        begin
          D:= IncDay(ThisFirstDate, 1);
          StaffPostLogEditForm.FirstDatePicker.Date:= MaxDate(Date, D);
          StaffPostLogEditForm.FirstDatePicker.MinDate:= D;
          StaffPostLogEditForm.FirstDatePicker.MaxDate:= IncDay(INFDATE, -1);
        end;
      etEdit: //редактирование должности
        begin
          StaffPostLogEditForm.FirstDatePicker.Date:= ThisFirstDate;
          if SameDate(ThisFirstDate, TabNumListRecrutDates[TabNumList.SelectedIndex]) then
          begin //первая запись
            StaffPostLogEditForm.StatusDropDown.Enabled:= False; //нельзя менять постоянный статус должности
            StaffPostLogEditForm.FirstDatePicker.Enabled:= False;//нельзя менять дату начала работы (приема)
            StaffPostLogEditForm.PrevPostLogID:= 0;
          end
          else begin //последующие записи
            StaffPostLogEditForm.StatusDropDown.ItemIndex:= PostLogPostTemps[PostLog.SelectedIndex];
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
  finally
    FreeAndNil(StaffPostLogEditForm);
  end;

end;

procedure TStaffForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
  try
    ModeType:= AModeType;
    ExportButton.Visible:= ModeType<>mtEditing;

    if ModeType=mtSetting then
    begin
      SettingPanel.Visible:= True;
      LeftSplitter.Visible:= True;
    end
    else begin
      LeftSplitter.Visible:= False;
      SettingPanel.Visible:= False;
    end;

    if ModeType=mtEditing then
    begin
      EditingPanel.Visible:= True;
      EditingSplitter.Visible:= True;
    end
    else begin
      EditingSplitter.Visible:= False;
      EditingPanel.Visible:= False;
    end;

    ListPanel.BorderSpacing.Right:= 2*Ord(ModeType<>mtEditing);

    StaffList.CanUnselect:= ModeType<>mtEditing;
    StaffList.CanSelect:= ModeType=mtEditing;
    ListToolPanel.Visible:= ModeType=mtEditing;
    ListOrderToolPanel.Visible:= ModeType=mtEditing;

    StaffListUpdate;

  finally
    MainPanel.Visible:= True;
  end;
end;

end.

