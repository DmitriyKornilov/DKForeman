unit USIZCardForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel, StdCtrls, ComCtrls, fpspreadsheetgrid,
  //Project utils
  UDataBase, UConst, UTypes, UTimingUtils, UImages, ColorSpeedButton,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_Vector, DK_Filter, DK_CtrlUtils, DK_Color,
  DK_StrUtils,
  //Forms
  USIZCardFrontForm, USIZCardBackForm, USIZCardStatusForm;

type

  { TSIZCardForm }

  TSIZCardForm = class(TForm)
    AscendingButton: TSpeedButton;
    FormPanel: TPanel;
    StatusTabButton: TColorSpeedButton;
    FrontTabButton: TColorSpeedButton;
    BackTabButton: TColorSpeedButton;
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
    procedure FIORadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FrontTabButtonClick(Sender: TObject);
    procedure PostRadioButtonClick(Sender: TObject);
    procedure StatusTabButtonClick(Sender: TObject);
    procedure TabNumRadioButtonClick(Sender: TObject);
  private
    CanDraw: Boolean;
    FilterString: String;
    ModeType: TModeType;

    Category: Byte;
    CategoryForm: TForm;

    ParamList: TVSTParamList;
    StaffList: TVSTTable;
    CardList: TVSTTable;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames: TStrVector;
    //RecrutDates, DismissDates: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    CardIDs, CardItemIDs: TIntVector;
    CardPostNames, CardNums, CardNormNames: TStrVector;
    CardBDs, CardEDs: TDateVector;

    procedure ParamListCreate;

    procedure StaffListCreate;
    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListLoad;
    procedure StaffListSelect;

    procedure CardListCreate;
    procedure CardListLoad;
    procedure CardListSelect;

    procedure CategorySelect(const ACategory: Byte);
    procedure CardSettingsSave;
    procedure CardViewUpdate;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
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

  SetToolPanels([
    ToolPanel, StaffFilterToolPanel, StaffOrderToolPanel
  ]);
  SetCaptionPanels([
    StaffCaptionPanel, SettingCaptionPanel, CardListCaptionPanel,
    ViewCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton, AscendingButton, DescendingButton
  ]);

  ControlHeight(ViewButtonPanel, Round(TOOL_PANEL_HEIGHT_DEFAULT*0.65));
  FrontTabButton.StateActive.Color:= DefaultSelectionBGColor;
  BackTabButton.StateActive.Color:= DefaultSelectionBGColor;
  StatusTabButton.StateActive.Color:= DefaultSelectionBGColor;

  CanDraw:= False;

  StaffListCreate;
  CardListCreate;
  ParamListCreate;
  SettingsLoad;
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter, 300);

  CanDraw:= True;
end;

procedure TSIZCardForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(StaffList);
  FreeAndNil(CardList);
end;

procedure TSIZCardForm.FormShow(Sender: TObject);
begin
  FrontTabButton.Width:= BackTabButton.Width;
  StatusTabButton.Width:= BackTabButton.Width;
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

procedure TSIZCardForm.DescendingButtonClick(Sender: TObject);
begin
  DescendingButton.Visible:= False;
  AscendingButton.Visible:= True;
  StaffListLoad;
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
end;

procedure TSIZCardForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffListVT);
  StaffList.CanSelect:= True;
  StaffList.CanUnselect:= False;
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(MainForm.GridFont);
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
                             OrderType, IsDescOrder, TabNumIDs,
                             Families, Names, Patronymics, TabNums, PostNames);
  StaffLongNames:= StaffNamesForPersonalTiming(Families, Names, Patronymics, TabNums, PostNames, True);
  StaffShortNames:= StaffNamesForPersonalTiming(Families, Names, Patronymics, TabNums, PostNames, False);

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
end;

procedure TSIZCardForm.StaffListSelect;
begin
  CardListCaptionPanel.Caption:= '  Личные карточки учета выдачи СИЗ';
  if not StaffList.IsSelected then Exit;

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
  CardList.SetSingleFont(MainForm.GridFont);
  CardList.HeaderFont.Style:= [fsBold];

  CardList.AddColumn('Номер', 80);
  CardList.AddColumn('Период действия', 150);
  CardList.AddColumn('Должность (профессия)', 300);
  CardList.AddColumn('Нормы выдачи СИЗ', 300);
  CardList.AutosizeColumnEnable('Нормы выдачи СИЗ');
  CardList.Draw;
end;

procedure TSIZCardForm.CardListLoad;
begin
  if not StaffList.IsSelected then Exit;

  DataBase.SIZPersonalCardListLoad(TabNumIDs[StaffList.SelectedIndex], CardIDs,
                                   CardItemIDs, CardNums, CardPostNames,
                                   CardNormNames,CardBDs, CardEDs);
  VChangeIf(CardNums, EmptyStr, 'б/н');

  CardList.Visible:= False;
  try
    CardList.ValuesClear;
    CardList.SetColumn('Номер', CardNums);
    CardList.SetColumn('Период действия', VPeriodToStr(CardBDs, CardEDs){, taLeftJustify});
    CardList.SetColumn('Должность (профессия)', CardPostNames, taLeftJustify);
    CardList.SetColumn('Нормы выдачи СИЗ', CardNormNames, taLeftJustify);
    CardList.Draw;
    CardList.Select(0);
  finally
    CardList.Visible:= True;
  end;
end;

procedure TSIZCardForm.CardListSelect;
begin
  ViewCaptionPanel.Caption:= '  Личная карточка: ';
  if CardList.IsSelected then
    ViewCaptionPanel.Caption:= ViewCaptionPanel.Caption +
      CardNums[CardList.SelectedIndex] +
      ' (' +
      PeriodToStr(CardBDs[CardList.SelectedIndex], CardEDs[CardList.SelectedIndex]) +
      ')';
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

procedure TSIZCardForm.CardViewUpdate;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    1: (CategoryForm as TSIZCardFrontForm).ViewUpdate(ModeType);
    2: (CategoryForm as TSIZCardBackForm).ViewUpdate(ModeType);
    3: (CategoryForm as TSIZCardStatusForm).ViewUpdate(ModeType);
  end;
end;

procedure TSIZCardForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_SIZSTAFFORM);
end;

procedure TSIZCardForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_SIZSTAFFORM, ParamList.Params);
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

    StaffListLoad;

    CardViewUpdate;

  finally
    MainPanel.Visible:= True;
  end;
end;

end.

