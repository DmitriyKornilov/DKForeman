unit USIZCardForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel, StdCtrls, ComCtrls, fpspreadsheetgrid, ColorSpeedButton,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZSizes, USIZNormTypes, USIZCardTypes,
  USIZCardSheet,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_Vector, DK_Filter, DK_CtrlUtils, DK_Color,
  DK_StrUtils, DK_SheetExporter, DK_Progress, DK_Dialogs, DK_Matrix, DK_Inputs,
  //Forms
  USIZCardFrontForm, USIZCardBackForm, USIZCardStatusForm, USIZCardEditForm,
  USIZStaffHistoryForm;

type

  { TSIZCardForm }

  TSIZCardForm = class(TForm)
    AscendingButton: TSpeedButton;
    DividerBevel2: TDividerBevel;
    FrontEditButton: TSpeedButton;
    HistoryButton: TSpeedButton;
    FormPanel: TPanel;
    StatusTabButton: TColorSpeedButton;
    FrontTabButton: TColorSpeedButton;
    BackTabButton: TColorSpeedButton;
    ViewToolPanel: TPanel;
    ViewButtonPanel: TPanel;
    ViewCaptionPanel: TPanel;
    CloseButton: TSpeedButton;
    DescendingButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel4: TDividerBevel;
    ExportButton: TSpeedButton;
    FilterPanel: TPanel;
    FIORadioButton: TRadioButton;
    ViewPanel: TPanel;
    SettingSplitter: TSplitter;
    CardSplitter: TSplitter;
    CardListCaptionPanel: TPanel;
    CardListVT: TVirtualStringTree;
    StaffSplitter: TSplitter;
    CardListPanel: TPanel;
    CardPanel: TPanel;
    StaffCaptionPanel: TPanel;
    StaffFilterToolPanel: TPanel;
    StaffOrderToolPanel: TPanel;
    StaffPanel: TPanel;
    OrderButtonPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    MainPanel: TPanel;
    SettingCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    StaffListVT: TVirtualStringTree;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    procedure AscendingButtonClick(Sender: TObject);
    procedure BackTabButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DescendingButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FrontEditButtonClick(Sender: TObject);
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrontTabButtonClick(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure StatusTabButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
  private
    CanDataUpdate: Boolean;
    FilterString: String;
    ModeType: TModeType;

    Category: Byte;
    CategoryForm: TForm;

    ParamList: TVSTParamList;
    StaffList: TVSTTable;
    CardList: TVSTTable;

    StaffIDs, TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames: TStrVector;
    //RecrutDates, DismissDates: TDateVector;
    Families, Names, Patronymics, Genders, TabNums, PostNames: TStrVector;
    PersonSizes: TSIZStaffSizeIndexes;

    CardIDs, CardItemIDs, CardItemPostIDs: TIntVector;
    CardPostNames, CardNums, ViewCardNums, CardNormNames: TStrVector;
    CardBDs, CardEDs: TDateVector;

    NormSubItems: TNormSubItems;
    StatusSubItems: TStatusSubItems;

    procedure ParamListCreate;
    procedure WriteoffTypeChange;

    procedure StaffListCreate;
    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListLoad;
    procedure StaffListSelect;

    procedure CardListCreate;
    procedure CardListSelect;

    procedure CardLoad;
    procedure StatusLoad;

    procedure CategorySelect(const ACategory: Byte);
    procedure CardSettingsSave;

    procedure CardFrontUpdate;
    procedure CardBackUpdate;
    procedure CardStatusUpdate;
    procedure CardDataUpdate;
    procedure CardViewUpdate;

    procedure CardFrontExport(const ACardNum, AF, AN, AP, AGender,
                                    ATabNum, APostName: String;
                        const ACardBD, ACardED: TDate;
                        const APersonSizes: TSIZStaffSizeIndexes;
                        const ASubItems: TNormSubItems;
                        const AWorksheet: TsWorksheet;
                        out ACardFileName: String);
    procedure CardBackExport(const ACardID: Integer;
                        const AWorksheet: TsWorksheet);

    procedure CardExport(const ASelected: Boolean); //true-выбранная, false - актуальная
    procedure StatusExport;
    procedure PersonCardsExport;
    procedure AllCardsExport;
    procedure AllStatusesExport;
    procedure DataExport;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
    procedure CardCaptionUpdate;
    procedure CardListLoad(const ASaveSelection: Boolean = False);
  end;

var
  SIZCardForm: TSIZCardForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZCardForm }

procedure TSIZCardForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  FrontTabButton.StateActive.Color:= DefaultSelectionBGColor;
  BackTabButton.StateActive.Color:= DefaultSelectionBGColor;
  StatusTabButton.StateActive.Color:= DefaultSelectionBGColor;

  CanDataUpdate:= False;

  StaffListCreate;
  CardListCreate;
  ParamListCreate;
  SettingsLoad;
  DKFilterCreate('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter, 300);

  CanDataUpdate:= True;
end;

procedure TSIZCardForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(StaffList);
  FreeAndNil(CardList);
end;

procedure TSIZCardForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, StaffFilterToolPanel, StaffOrderToolPanel, ViewToolPanel
  ]);
  SetCaptionPanels([
    StaffCaptionPanel, SettingCaptionPanel, CardListCaptionPanel,
    ViewCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton,
    FrontEditButton
  ]);

  Images.ToButtons([
    ExportButton, HistoryButton,
    CloseButton, AscendingButton, DescendingButton,
    FrontEditButton
  ]);

  ParamList.AutoHeight;

  SetControlHeightScaleToForm(ViewButtonPanel, Round(TOOL_PANEL_HEIGHT_DEFAULT*0.65));

  FrontTabButton.Width:= BackTabButton.Width;
  StatusTabButton.Width:= BackTabButton.Width;

  StaffListLoad;
  CategorySelect(1);
end;

procedure TSIZCardForm.FrontTabButtonClick(Sender: TObject);
begin
  CategorySelect(1);
end;

procedure TSIZCardForm.BackTabButtonClick(Sender: TObject);
begin
  CategorySelect(2);
end;

procedure TSIZCardForm.StatusTabButtonClick(Sender: TObject);
begin
  CategorySelect(3);
end;

procedure TSIZCardForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZCardForm.HistoryButtonClick(Sender: TObject);
var
  i: Integer;
  StaffName: String;
begin
  i:= StaffList.SelectedIndex;
  StaffName:= SNameLong(Families[i], Names[i], Patronymics[i]);
  SIZStaffHistoryFormOpen(TabNumIDs[i], StaffName, TabNums[i]);
end;

procedure TSIZCardForm.ExportButtonClick(Sender: TObject);
begin
  DataExport;
end;

procedure TSIZCardForm.FIORadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSIZCardForm.PostRadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSIZCardForm.TabNumRadioButtonClick(Sender: TObject);
begin
  StaffListLoad;
end;

procedure TSIZCardForm.AscendingButtonClick(Sender: TObject);
begin
  AscendingButton.Visible:= False;
  DescendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TSIZCardForm.DescendingButtonClick(Sender: TObject);
begin
  DescendingButton.Visible:= False;
  AscendingButton.Visible:= True;
  StaffListLoad;
end;

procedure TSIZCardForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= 'Включать в список:';
  V:= VCreateStr([
    'всех',
    'работающих на текущую дату',
    'уволенных на текущую дату'
  ]);
  ParamList.AddStringList('ListType', S, V, @StaffListLoad, 1);

  S:= 'Рассчитывать даты списания:';
  V:= VCreateStr([
    'по нормам на момент выдачи',
    'по текущим нормам'
  ]);
  ParamList.AddStringList('WriteoffType', S, V, @WriteoffTypeChange, 1);
end;

procedure TSIZCardForm.WriteoffTypeChange;
begin
  StatusLoad;
  if Category=3 then
    CardStatusUpdate;
end;

procedure TSIZCardForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffListVT);
  StaffList.CanSelect:= True;
  StaffList.CanUnselect:= False;
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(GridFont);
  StaffList.HeaderFont.Style:= [fsBold];

  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Сотрудник', 300);
  StaffList.AutosizeColumnEnable('Сотрудник');
  StaffList.Draw;
end;

procedure TSIZCardForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TSIZCardForm.StaffListLoad;
var
  SelectedID: Integer;
  OrderType: Byte;
  IsDescOrder: Boolean;
begin
  if not CanDataUpdate then Exit;

  SelectedID:= GetSelectedID(StaffList, TabNumIDs, -1);

  if FIORadioButton.Checked then
    OrderType:= 0
  else if TabNumRadioButton.Checked then
    OrderType:= 1
  else if PostRadioButton.Checked then
    OrderType:= 2;

  IsDescOrder:= not DescendingButton.Visible;

  DataBase.SIZStaffListForPersonalCardsLoad(STrimLeft(FilterString),
                             ParamList.Selected['ListType'],
                             OrderType, IsDescOrder, StaffIDs, TabNumIDs,
                             Families, Names, Patronymics, Genders, TabNums, PostNames);
  StaffLongNames:= StaffFullName(Families, Names, Patronymics, TabNums, PostNames, False{long});
  StaffShortNames:= StaffFullName(Families, Names, Patronymics, TabNums, PostNames, True{short});

  StaffList.Visible:= False;
  try
    StaffList.ValuesClear;
    StaffList.SetColumn('№ п/п', VIntToStr(VOrder(Length(TabNumIDs))));
    StaffList.SetColumn('Сотрудник', StaffShortNames, taLeftJustify);
    StaffList.Draw;
    StaffList.ReSelect(TabNumIDs, SelectedID, True);  //возвращаем выделение строки
  finally
    StaffList.Visible:= True;
  end;

  ExportButton.Enabled:= not VIsNil(StaffIDs);
end;

procedure TSIZCardForm.StaffListSelect;
begin
  CardListCaptionPanel.Caption:= '  Личные карточки учета выдачи СИЗ';
  HistoryButton.Enabled:= StaffList.IsSelected;
  if not StaffList.IsSelected then Exit;

  SIZStaffSizeIndexesClear(PersonSizes);
  DataBase.SIZStaffSizeLoad(StaffIDs[StaffList.SelectedIndex], PersonSizes);

  CardListCaptionPanel.Caption:= CardListCaptionPanel.Caption + ': ' +
                                StaffLongNames[StaffList.SelectedIndex];

  CardListLoad;
end;

procedure TSIZCardForm.CardListCreate;
begin
  CardList:= TVSTTable.Create(CardListVT);
  CardList.CanSelect:= True;
  CardList.CanUnselect:= False;
  CardList.OnSelect:= @CardListSelect;
  CardList.SetSingleFont(GridFont);
  CardList.HeaderFont.Style:= [fsBold];

  CardList.AddColumn('Номер', 80);
  CardList.AddColumn('Период действия', 150);
  CardList.AddColumn('Должность (профессия)', 300);
  CardList.AddColumn('Нормы выдачи СИЗ', 300);
  CardList.AutosizeColumnEnable('Нормы выдачи СИЗ');
  CardList.Draw;
end;

procedure TSIZCardForm.CardListLoad(const ASaveSelection: Boolean = False);
var
  SelectedIndex: Integer;
begin
  if not StaffList.IsSelected then Exit;

  SelectedIndex:= -1;
  if ASaveSelection and CardList.IsSelected then
    SelectedIndex:= CardList.SelectedIndex;

  DataBase.SIZPersonalCardListLoad(TabNumIDs[StaffList.SelectedIndex], CardIDs,
                                   CardItemIDs, CardItemPostIDs, CardNums, CardPostNames,
                                   CardNormNames,CardBDs, CardEDs);
  ViewCardNums:= VCut(CardNums);
  VChangeIf(ViewCardNums, EmptyStr, 'б/н');

  CardList.Visible:= False;
  try
    CardList.ValuesClear;
    CardList.SetColumn('Номер', ViewCardNums);
    CardList.SetColumn('Период действия', PeriodToStr(CardBDs, CardEDs){, taLeftJustify});
    CardList.SetColumn('Должность (профессия)', CardPostNames, taLeftJustify);
    CardList.SetColumn('Нормы выдачи СИЗ', CardNormNames, taLeftJustify);
    CardList.Draw;
    if ASaveSelection and (SelectedIndex>=0) then
      CardList.Select(SelectedIndex)
    else
      CardList.Select(0);
  finally
    CardList.Visible:= True;
  end;
end;

procedure TSIZCardForm.CardLoad;
begin
  NormSubItemsClear(NormSubItems);
  if CardList.IsSelected then
    DataBase.SIZNormSubItemsLoad(CardItemIDs[CardList.SelectedIndex], NormSubItems);
end;

procedure TSIZCardForm.StatusLoad;
begin
  StatusSubItemsClear(StatusSubItems);
  if Length(NormSubItems)>0 then
    DataBase.SIZStatusLoad(TabNumIDs[StaffList.SelectedIndex],
                           CardIDs[CardList.SelectedIndex],
                           ParamList.Selected['WriteoffType'],
                           Date, NormSubItems, StatusSubItems);
end;

procedure TSIZCardForm.CardListSelect;
begin
  FrontEditButton.Enabled:= CardList.IsSelected;
  CardLoad;
  StatusLoad;
  CardCaptionUpdate;
  CardDataUpdate;
end;

procedure TSIZCardForm.CategorySelect(const ACategory: Byte);
begin
  if ACategory=Category then Exit;

  Screen.Cursor:= crHourGlass;
  FormPanel.Visible:= False;
  try
    CardSettingsSave;
    Category:= ACategory;

    if Assigned(CategoryForm) then FreeAndNil(CategoryForm);
    case Category of
      1: CategoryForm:= FormOnPanelCreate(TSIZCardFrontForm, FormPanel);
      2: CategoryForm:= FormOnPanelCreate(TSIZCardBackForm, FormPanel);
      3: CategoryForm:= FormOnPanelCreate(TSIZCardStatusForm, FormPanel);
    end;

    if Assigned(CategoryForm) then
    begin
      CategoryForm.Show;
      CardDataUpdate;
      CardViewUpdate;
    end;

  finally
    Screen.Cursor:= crDefault;
    FormPanel.Visible:= True;
  end;
end;

procedure TSIZCardForm.CardSettingsSave;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    1: (CategoryForm as TSIZCardFrontForm).SettingsSave;
    2: (CategoryForm as TSIZCardBackForm).SettingsSave;
    3: (CategoryForm as TSIZCardStatusForm).SettingsSave;
  end;
end;

procedure TSIZCardForm.FrontEditButtonClick(Sender: TObject);
var
  StaffID, TabNumID, CardID, ItemPostID, CardIndex: Integer;
  CardNum: String;
begin
  if not CardList.IsSelected then Exit;

  StaffID:= StaffIDs[StaffList.SelectedIndex];
  TabNumID:= TabNumIDs[StaffList.SelectedIndex];

  CardIndex:= CardList.SelectedIndex;
  CardID:= CardIDs[CardIndex];
  ItemPostID:= CardItemPostIDs[CardIndex];
  CardNum:= CardNums[CardIndex];

  if not SIZCardEditFormOpen(StaffID, PersonSizes, False, CardID, CardNum,
                         TabNumID, ItemPostID) then Exit;

  CardNums[CardIndex]:= CardNum;
  if SEmpty(CardNum) then
    ViewCardNums[CardIndex]:= 'б/н'
  else
    ViewCardNums[CardIndex]:= CardNum;

  CardList.SetColumn('Номер', ViewCardNums);
  CardList.Draw;
  CardList.Select(CardIndex);
end;

procedure TSIZCardForm.CardFrontUpdate;
var
  CardNum, Family, PersonName, Patronymic, Gender, TabNum, PostName: String;
  CardBD, CardED: TDate;
begin
  if CardList.IsSelected then
  begin
    CardNum:= CardNums[CardList.SelectedIndex];
    CardBD:= CardBDs[CardList.SelectedIndex];
    CardED:= CardEDs[CardList.SelectedIndex];
    PostName:= CardPostNames[CardList.SelectedIndex];

    Family:= Families[StaffList.SelectedIndex];
    PersonName:= Names[StaffList.SelectedIndex];
    Patronymic:= Patronymics[StaffList.SelectedIndex];
    Gender:= Genders[StaffList.SelectedIndex];
    TabNum:= TabNums[StaffList.SelectedIndex];
  end
  else begin
    CardNum:= EmptyStr;
    CardBD:= 0;
    CardED:= 0;
    PostName:= EmptyStr;

    Family:= EmptyStr;
    PersonName:= EmptyStr;
    Patronymic:= EmptyStr;
    Gender:= EmptyStr;
    TabNum:= EmptyStr;
  end;

  (CategoryForm as TSIZCardFrontForm).DataUpdate(CardNum,
        Family, PersonName, Patronymic, Gender, TabNum, PostName,
        CardBD, CardED, PersonSizes, NormSubItems);
end;

procedure TSIZCardForm.CardBackUpdate;
var
  CardID: Integer;
begin
  if CardList.IsSelected then
    CardID:= CardIDs[CardList.SelectedIndex]
  else
    CardID:= 0;

  (CategoryForm as TSIZCardBackForm).DataUpdate(CardList.IsSelected, CardID);
end;

procedure TSIZCardForm.CardStatusUpdate;
var
  CardID, CardItemPostID: Integer;
  CardBD: TDate;
begin
  if CardList.IsSelected then
  begin
    CardID:= CardIDs[CardList.SelectedIndex];
    CardItemPostID:= CardItemPostIDs[CardList.SelectedIndex];
    CardBD:= CardBDs[CardList.SelectedIndex];
  end
  else begin
    CardID:= 0;
    CardItemPostID:= 0;
    CardBD:= 0;
  end;

  (CategoryForm as TSIZCardStatusForm).DataUpdate(TabNumIDs[StaffList.SelectedIndex],
                                     CardID, CardItemPostID, CardBD, NormSubItems, StatusSubItems);
end;

procedure TSIZCardForm.CardDataUpdate;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    1: CardFrontUpdate;
    2: CardBackUpdate;
    3: CardStatusUpdate;
  end;
end;

procedure TSIZCardForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_SIZCARDFORM);
end;

procedure TSIZCardForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_SIZCARDFORM, ParamList.Params);
  CardSettingsSave;
end;

procedure TSIZCardForm.ViewUpdate(const AModeType: TModeType);
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

    MainPanel.BorderSpacing.Left:= 2*Ord(ModeType<>mtSetting);
    CardViewUpdate;

  finally
    MainPanel.Visible:= True;
  end;
end;

procedure TSIZCardForm.CardViewUpdate;
begin
  ViewToolPanel.Visible:= (ModeType=mtEditing) and (Category=1);
  if Category=2 then
    (CategoryForm as TSIZCardBackForm).ViewUpdate(ModeType)
  else if Category=3 then
    (CategoryForm as TSIZCardStatusForm).ViewUpdate(ModeType);
end;

procedure TSIZCardForm.CardFrontExport(const ACardNum, AF, AN, AP, AGender,
                                             ATabNum, APostName: String;
                        const ACardBD, ACardED: TDate;
                        const APersonSizes: TSIZStaffSizeIndexes;
                        const ASubItems: TNormSubItems;
                        const AWorksheet: TsWorksheet;
                        out ACardFileName: String);
var
  ExpSheet: TSIZCardFrontSheet;
begin
  ExpSheet:= TSIZCardFrontSheet.Create(AWorksheet, nil, GridFont);
  try
    ExpSheet.Draw(ACardNum, AF, AN, AP, AGender, ATabNum, APostName,
                  Department, ACardBD, ACardED, APersonSizes, ASubItems);
  finally
    FreeAndNil(ExpSheet);
  end;

  ACardFileName:= StaffFullName(AF, AN, AP, ATabNum, True{short}) +
                  ' (' + PeriodToStr(ACardBD, ACardED) + ')';
end;

procedure TSIZCardForm.CardBackExport(const ACardID: Integer; const AWorksheet: TsWorksheet);
var
  ExpSheet: TSIZCardBackSheet;
  ExpLogIDs: TInt64Vector;
  ExpReceivingDates, ExpReturningDates: TDateVector;
  ExpNormSIZTypes: TIntVector;
  ExpNormSizNames: TStrVector;
  ExpReceivingDocNames, ExpReturningDocNames: TStrVector;
  ExpReceivingSizNames, ExpWriteoffDocNames: TStrMatrix;
  ExpSizCounts, ExpSizeTypes: TIntMatrix;
begin
  ExpSheet:= TSIZCardBackSheet.Create(AWorksheet, nil, GridFont);
  try
    //при ACardID=0 - очищение векторов, вывод пустой таблицы (ничего не выдавалось)
    DataBase.SIZPersonalCardSIZLoad(0, ACardID,
                              ExpLogIDs, ExpReceivingDates, ExpReturningDates,
                              ExpNormSIZTypes, ExpNormSizNames, ExpReceivingDocNames,
                              ExpReturningDocNames, ExpReceivingSizNames,
                              ExpWriteoffDocNames, ExpSizCounts, ExpSizeTypes);
    ExpSheet.Draw(True{NeedDraw}, ExpReceivingDates, ExpReturningDates, ExpNormSizNames,
             ExpReceivingDocNames, ExpReturningDocNames,
             ExpReceivingSizNames, ExpWriteoffDocNames, ExpSizCounts, ExpSizeTypes);
  finally
    FreeAndNil(ExpSheet);
  end;
end;

procedure TSIZCardForm.CardExport(const ASelected: Boolean); //true-выбранная, false - актуальная;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  FileName: String;

  ExpSubItems: TNormSubItems;
  ExpSizes: TSIZStaffSizeIndexes;

  ExpDate: TDate;
  ExpCardID, ExpItemID, ExpItemPostID: Integer;
  ExpCardNum, ExpPostName, ExpNormName: String;
  ExpCardBD, ExpCardED: TDate;
begin
  if not CardList.IsSelected  then
  begin
    Inform('Нет данных для экспорта!');
    Exit;
  end;

  if ASelected then
    ExpDate:= CardBDs[CardList.SelectedIndex]
  else
    ExpDate:= Date;
  if not DataBase.SIZPersonalCardForDateLoad(TabNumIDs[StaffList.SelectedIndex],
                  ExpDate, ExpCardID, ExpItemID, ExpItemPostID,
                  ExpCardNum, ExpPostName, ExpNormName,
                  ExpCardBD, ExpCardED) then
  begin
    Inform('Нет личной карточки на ' + DateToStr(ExpDate) + '!');
    Exit;
  end;

  //размеры
  SIZStaffSizeIndexesClear(ExpSizes{%H-});
  DataBase.SIZStaffSizeLoad(StaffIDs[StaffList.SelectedIndex], ExpSizes);
  //нормы
  NormSubItemsClear(ExpSubItems{%H-});
  DataBase.SIZNormSubItemsLoad(ExpItemID, ExpSubItems);

  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лицевая сторона');
    CardFrontExport(ExpCardNum,
                    Families[StaffList.SelectedIndex],
                    Names[StaffList.SelectedIndex],
                    Patronymics[StaffList.SelectedIndex],
                    Genders[StaffList.SelectedIndex],
                    TabNums[StaffList.SelectedIndex],
                    ExpPostName, ExpCardBD, ExpCardED, ExpSizes, ExpSubItems,
                    Worksheet, FileName);
    Exporter.PageSettings(spoPortrait, pfWidth, False, False);

    Worksheet:= Exporter.AddWorksheet('Оборотная сторона');
    CardBackExport(ExpCardID, Worksheet);
    Exporter.PageSettings(spoLandscape, pfWidth, False, False);

    Exporter.Save('Выполнено!', FileName);
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZCardForm.StatusExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TSIZCardStatusSheet;
  StatusTitle, FileName: String;
  R, WarnDaysCount: Integer;

  ExpNormSubItems: TNormSubItems;
  ExpStatusSubItems: TStatusSubItems;

  ExpCardID, ExpItemID, ExpItemPostID: Integer;
  ExpCardNum, ExpPostName, ExpNormName: String;
  ExpCardBD, ExpCardED: TDate;
begin
  if (not StaffList.IsSelected) or
     (not DataBase.SIZPersonalCardForDateLoad(TabNumIDs[StaffList.SelectedIndex],
                  Date, ExpCardID, ExpItemID, ExpItemPostID,
                  ExpCardNum, ExpPostName, ExpNormName,
                  ExpCardBD, ExpCardED)) then
  begin
    Inform('Нет данных для экспорта!');
    Exit;
  end;

  WarnDaysCount:= DataBase.SettingLoad('SIZCARDSTATUSFORM.WARNDAYSCOUNT');

  NormSubItemsClear(ExpNormSubItems{%H-});
  DataBase.SIZNormSubItemsLoad(ExpItemID, ExpNormSubItems);

  StatusSubItemsClear(ExpStatusSubItems{%H-});
  if Length(ExpNormSubItems)>0 then
    DataBase.SIZStatusLoad(TabNumIDs[StaffList.SelectedIndex],
                           ExpCardID,
                           ParamList.Selected['WriteoffType'],
                           Date, ExpNormSubItems, ExpStatusSubItems);

  FileName:= StaffFullName(Families[StaffList.SelectedIndex],
                           Names[StaffList.SelectedIndex],
                           Patronymics[StaffList.SelectedIndex],
                           TabNums[StaffList.SelectedIndex], True{short}) +
           ' (' + PeriodToStr(ExpCardBD, ExpCardED) + ')';

  StatusTitle:= StaffFullName(Families[StaffList.SelectedIndex],
                           Names[StaffList.SelectedIndex],
                           Patronymics[StaffList.SelectedIndex],
                           TabNums[StaffList.SelectedIndex],
                           ExpPostName, False{long});

  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лист1');
    ExpSheet:= TSIZCardStatusSheet.Create(Worksheet, nil, GridFont);
    try
      R:= 1;
      ExpSheet.Draw(R, ExpNormSubItems, ExpStatusSubItems, WarnDaysCount, StatusTitle);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!', FileName);
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZCardForm.PersonCardsExport;
var
  i: Integer;
  Exporter: TBooksExporter;
  Worksheet: TsWorksheet;
  Progress: TProgress;

  ExpSubItems: TNormSubItems;
  ExpSizes: TSIZStaffSizeIndexes;
  S, FileName: String;
begin
  if not CardList.IsSelected  then
  begin
    Inform('Нет данных для экспорта!');
    Exit;
  end;

  Exporter:= TBooksExporter.Create;
  if not Exporter.BeginExport then
  begin
    FreeAndNil(Exporter);
    Exit;
  end;

  try
    Progress:= TProgress.Create(nil);
    try
      Progress.WriteLine1('Экспорт личных карточек учета выдачи СИЗ');
      Progress.WriteLine2(EmptyStr);
      Progress.Show;
      for i:=0 to High(CardIDs) do
      begin
        S:= PeriodToStr(CardBDs[i], CardEDs[i]);
        Progress.WriteLine2(S);
        //размеры
        SIZStaffSizeIndexesClear(ExpSizes{%H-});
        DataBase.SIZStaffSizeLoad(StaffIDs[StaffList.SelectedIndex], ExpSizes);
        //нормы
        NormSubItemsClear(ExpSubItems{%H-});
        DataBase.SIZNormSubItemsLoad(CardItemIDs[i], ExpSubItems);

        Worksheet:= Exporter.AddWorksheet('Лицевая сторона', True {новая книга});
        CardFrontExport(CardNums[i],
                    Families[StaffList.SelectedIndex],
                    Names[StaffList.SelectedIndex],
                    Patronymics[StaffList.SelectedIndex],
                    Genders[StaffList.SelectedIndex],
                    TabNums[StaffList.SelectedIndex],
                    CardPostNames[i], CardBDs[i], CardEDs[i],
                    ExpSizes, ExpSubItems,
                    Worksheet, FileName);
        Exporter.PageSettings(spoPortrait);

        Worksheet:= Exporter.AddWorksheet('Оборотная сторона', False{старая книга});
        CardBackExport(CardIDs[i], Worksheet);
        Exporter.PageSettings(spoLandscape);

        Exporter.Save(FileName);
      end;

    finally
      FreeAndNil(Progress);
    end;
    Exporter.EndExport('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZCardForm.AllCardsExport;
var
  i: Integer;
  Exporter: TBooksExporter;
  Worksheet: TsWorksheet;
  Progress: TProgress;

  S, FileName: String;

  ExpSubItems: TNormSubItems;
  ExpSizes: TSIZStaffSizeIndexes;
  ExpCardID, ExpItemID, ExpItemPostID: Integer;
  ExpCardNum, ExpPostName, ExpNormName: String;
  ExpCardBD, ExpCardED: TDate;
begin
  if VIsNil(TabNumIDs) then
  begin
    Inform('Нет данных для экспорта!');
    Exit;
  end;

  Exporter:= TBooksExporter.Create;
  if not Exporter.BeginExport then
  begin
    FreeAndNil(Exporter);
    Exit;
  end;

  try
    Progress:= TProgress.Create(nil);
    try
      Progress.WriteLine1('Экспорт личных карточек учета СИЗ');
      Progress.WriteLine2(EmptyStr);
      Progress.Show;
      for i:=0 to High(TabNumIDs) do
      begin
        S:= StaffFullName(Families[i], Names[i], Patronymics[i],
                               TabNums[i], False{long});
        Progress.WriteLine2(S);
        //данные
        if not DataBase.SIZPersonalCardForDateLoad(TabNumIDs[i], Date,
                  ExpCardID, ExpItemID, ExpItemPostID,
                  ExpCardNum, ExpPostName, ExpNormName,
                  ExpCardBD, ExpCardED) then continue;
        //размеры
        SIZStaffSizeIndexesClear(ExpSizes{%H-});
        DataBase.SIZStaffSizeLoad(StaffIDs[i], ExpSizes);
        //нормы
        NormSubItemsClear(ExpSubItems{%H-});
        DataBase.SIZNormSubItemsLoad(ExpItemID, ExpSubItems);

        Worksheet:= Exporter.AddWorksheet('Лицевая сторона', True {новая книга});
        CardFrontExport(ExpCardNum, Families[i], Names[i], Patronymics[i],
                    Genders[i], TabNums[i],
                    ExpPostName, ExpCardBD, ExpCardED, ExpSizes, ExpSubItems,
                    Worksheet, FileName);
        Exporter.PageSettings(spoPortrait);

        Worksheet:= Exporter.AddWorksheet('Оборотная сторона', False{старая книга});
        CardBackExport(ExpCardID, Worksheet);
        Exporter.PageSettings(spoLandscape);

        Exporter.Save(FileName);
      end;

    finally
      FreeAndNil(Progress);
    end;
    Exporter.EndExport('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZCardForm.AllStatusesExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TSIZCardStatusSheet;
  Progress: TProgress;
  i, N, R, WarnDaysCount: Integer;
  S: String;

  ExpNormSubItems: TNormSubItems;
  ExpStatusSubItems: TStatusSubItems;

  ExpCardID, ExpItemID, ExpItemPostID: Integer;
  ExpCardNum, ExpPostName, ExpNormName: String;
  ExpCardBD, ExpCardED: TDate;
begin
  if VIsNil(TabNumIDs) then
  begin
    Inform('Нет данных для экспорта!');
    Exit;
  end;

  WarnDaysCount:= DataBase.SettingLoad('SIZCARDSTATUSFORM.WARNDAYSCOUNT');

  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лист1');
    ExpSheet:= TSIZCardStatusSheet.Create(Worksheet, nil, GridFont);
    Progress:= TProgress.Create(nil);
    try
      Progress.WriteLine1('Экспорт статусов выдачи СИЗ');
      Progress.WriteLine2(EmptyStr);
      Progress.Show;

      R:= 1;
      N:= 0;
      ExpSheet.DrawBegin;
      for i:=0 to High(TabNumIDs) do
      begin
        S:= StaffFullName(Families[i], Names[i], Patronymics[i],
                               TabNums[i], False{long});
        Progress.WriteLine2(S);
        if not DataBase.SIZPersonalCardForDateLoad(TabNumIDs[i],
                  Date, ExpCardID, ExpItemID, ExpItemPostID,
                  ExpCardNum, ExpPostName, ExpNormName,
                  ExpCardBD, ExpCardED) then continue;

        NormSubItemsClear(ExpNormSubItems{%H-});
        DataBase.SIZNormSubItemsLoad(ExpItemID, ExpNormSubItems);

        StatusSubItemsClear(ExpStatusSubItems{%H-});
        if Length(ExpNormSubItems)>0 then
          DataBase.SIZStatusLoad(TabNumIDs[i], ExpCardID,
                                 ParamList.Selected['WriteoffType'],
                                 Date, ExpNormSubItems, ExpStatusSubItems);

        N:= N + 1;
        S:= IntToStr(N) + '. ' +
            StaffFullName(Families[i], Names[i], Patronymics[i],
                          TabNums[i], ExpPostName, False{long});

        ExpSheet.DrawNext(R, ExpNormSubItems, ExpStatusSubItems, WarnDaysCount, S);
        R:= R + 1;
      end;
      ExpSheet.DrawEnd;

    finally
      FreeAndNil(ExpSheet);
      FreeAndNil(Progress);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!', Department);
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZCardForm.DataExport;
var
  V: TStrVector;
  S, PersonName: String;
  ChooseIndex: Integer;
begin
  if not StaffList.IsSelected then Exit;

  PersonName:= StaffFullName(Families[StaffList.SelectedIndex],
                                  Names[StaffList.SelectedIndex],
                                  Patronymics[StaffList.SelectedIndex],
                                  TabNums[StaffList.SelectedIndex],
                                  False{long});

  S:= 'Сохранить в файл:';
  V:= VCreateStr([
    'Выбранную личную карточку сотрудника: ' + PersonName,
    'Актуальную личную карточку сотрудника: ' + PersonName,
    'Статус выдачи СИЗ сотруднику: ' + PersonName,
    'Все личные карточки сотрудника: ' + PersonName,
    'Актуальные личные карточки всех сотрудников',
    'Статус выдачи СИЗ всем сотрудникам'
  ]);
  if not Choose(S, V, ChooseIndex, 'Выбор', 800) then Exit;

  case ChooseIndex of
    0: CardExport(True);
    1: CardExport(False);
    2: StatusExport;
    3: PersonCardsExport;
    4: AllCardsExport;
    5: AllStatusesExport;
  end;
end;

procedure TSIZCardForm.DataUpdate;
var
  SelectedCardIndex: Integer;
begin
  SelectedCardIndex:= CardList.SelectedIndex;

  StaffListLoad;

  if SelectedCardIndex>=0 then
    CardList.Select(SelectedCardIndex);

  CardDataUpdate;
end;

procedure TSIZCardForm.CardCaptionUpdate;
begin
  ViewCaptionPanel.Caption:= '  Личная карточка: ';
  if not CardList.IsSelected then Exit;

  ViewCaptionPanel.Caption:= ViewCaptionPanel.Caption +
      ViewCardNums[CardList.SelectedIndex] +
      ' (' +
      PeriodToStr(CardBDs[CardList.SelectedIndex], CardEDs[CardList.SelectedIndex]) +
      ')';
end;

end.

