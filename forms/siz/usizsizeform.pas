unit USIZSizeForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel, VirtualTrees,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZSizes,
  //DK packages utils
  DK_VSTParamList, DK_Vector, DK_VSTTables, DK_VSTTypes, DK_Filter, DK_CtrlUtils,
  DK_StrUtils,
  //Forms
  USIZSizeEditForm;

type

  { TSIZSizeForm }

  TSIZSizeForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    ExportButton: TSpeedButton;
    FilterPanel: TPanel;
    LeftSplitter: TSplitter;
    MainPanel: TPanel;
    EditButton: TSpeedButton;
    SettingCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    ListVT: TVirtualStringTree;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListVTDblClick(Sender: TObject);
  private
    FilterString: String;
    ModeType: TModeType;
    CanLoadSizeList: Boolean;

    SizeList: TVSTTable;
    ParamList: TVSTParamList;

    StaffIDs, Clothes, Heights, Shoes, Heads,
    Hands, Gasmasks, Respirators: TIntVector;
    BornDates: TDateVector;
    Families, Names, Patronymics, FullNames: TStrVector;

    procedure ParamListCreate;
    procedure ColumnsListSelect;
    procedure NameTypeSelect;

    procedure SizeListCreate;
    procedure SizeListFilter(const AFilterString: String);
    procedure SizeListLoad;
    procedure SizeListEdit;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  SIZSizeForm: TSIZSizeForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZSizeForm }

procedure TSIZSizeForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  CanLoadSizeList:= False;
  SizeListCreate;
  ParamListCreate;
  SettingsLoad;
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @SizeListFilter);
  CanLoadSizeList:= True;
end;

procedure TSIZSizeForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SizeList);
  FreeAndNil(ParamList);
end;

procedure TSIZSizeForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    SettingCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, EditButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton, EditButton
  ]);

  DataUpdate;
end;

procedure TSIZSizeForm.EditButtonClick(Sender: TObject);
begin
  SizeListEdit;
end;

procedure TSIZSizeForm.ExportButtonClick(Sender: TObject);
begin
  SizeList.Save([ ctInteger, //№ п/п
                  ctString,  //Ф.И.О
                  ctString,  //Одежда
                  ctString,  //Обувь
                  ctString,  //Головной убор
                  ctString,  //Рукавицы
                  ctString,  //Перчатки
                  ctString,  //Противогаз
                  ctString   //Респиратор
  ]);
end;

procedure TSIZSizeForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZSizeForm.ListVTDblClick(Sender: TObject);
begin
  SizeListEdit;
end;

procedure TSIZSizeForm.ParamListCreate;
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
  ParamList.AddStringList('ListType', S, V, @SizeListLoad, 1);

  S:= 'Сортировать список по:';
  V:= VCreateStr([
    'Ф.И.О.',
    'размеру одежды/росту',
    'размеру обуви',
    'размеру головного убора',
    'размеру рукавиц',
    'размеру перчаток',
    'размеру противогаза',
    'размеру респиратора'
  ]);
  ParamList.AddStringList('OrderType', S, V, @SizeListLoad);

  S:= 'Отображать столбцы:';
  V:= VCreateStr([
    '№ п/п',
    'Ф.И.О',
    'Одежда',
    'Обувь',
    'Головной убор',
    'Рукавицы',
    'Перчатки',
    'Противогаз',
    'Респиратор'
  ]);
  ParamList.AddCheckList('ColumnsList', S, V, @ColumnsListSelect);

  S:= 'Формат имени:';
  V:= VCreateStr([
    'Фамилия Имя Отчество',
    'Фамилия И.О.'
  ]);
  ParamList.AddStringList('NameType', S, V, @NameTypeSelect);
end;

procedure TSIZSizeForm.NameTypeSelect;
begin
  if not ParamList.IsSelected['NameType'] then Exit;
  if ModeType=mtEditing then
    FullNames:= VNameLong(Families, Names, Patronymics)
  else if (ParamList.Selected['NameType']=1)  then
    FullNames:= VNameShort(Families, Names, Patronymics)
  else
    FullNames:= VNameLong(Families, Names, Patronymics);
  FullNames:= VSum(FullNames, ', ');
  FullNames:= VSum(FullNames, VDateToStr(BornDates));
  FullNames:= VSum(FullNames, ' г.р.');
  SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[1], FullNames, taLeftJustify);
  SizeList.Refresh;
end;

procedure TSIZSizeForm.SizeListCreate;
var
  i: Integer;
begin
  SizeList:= TVSTTable.Create(ListVT);
  SizeList.OnReturnKeyDown:= @SizeListEdit;
  SizeList.SetSingleFont(GridFont);
  SizeList.HeaderFont.Style:= [fsBold];
  for i:= 0 to High(SIZSIZE_STAFFLIST_COLUMN_NAMES) do
    SizeList.AddColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[i],
                       SIZSIZE_STAFFLIST_COLUMN_WIDTHS[i]);
  SizeList.AutosizeColumnDisable;
  SizeList.Draw;
end;

procedure TSIZSizeForm.ColumnsListSelect;
begin
  SizeList.ColumnVisibles:= ParamList.Checkeds['ColumnsList'];
end;

procedure TSIZSizeForm.SizeListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  SizeListLoad;
end;

procedure TSIZSizeForm.SizeListLoad;
var
  SelectedID: Integer;
  V: TStrVector;
begin
  if not CanLoadSizeList then Exit;

  SelectedID:= GetSelectedID(SizeList, StaffIDs);

  DataBase.SIZStaffSizeLoad(STrimLeft(FilterString),
                            ParamList.Selected['OrderType'],
                            ParamList.Selected['ListType'],
                            StaffIDs, Clothes, Heights, Shoes, Heads,
                            Hands, Gasmasks, Respirators,
                            Families, Names, Patronymics, BornDates);

  ExportButton.Enabled:= not VIsNil(StaffIDs);
  EditButton.Enabled:= ExportButton.Enabled;

  SizeList.Visible:= False;
  try
    SizeList.ValuesClear;
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[0], VIntToStr(VOrder(Length(StaffIDs))));
    NameTypeSelect;
    V:= VReplace(USIZSizes.CLOTHES, Clothes);
    V:= VSum(V, '/');
    V:= VSum(V, VReplace(USIZSizes.PERSONHEIGHTS, Heights));
    VChangeIf(V, '/', EmptyStr);
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[2], V);
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[3], VReplace(USIZSizes.SHOES, Shoes));
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[4], VReplace(USIZSizes.HEADDRESS, Heads));
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[5], VReplace(USIZSizes.HANDS, Hands));
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[6], VReplace(USIZSizes.GASMASKS, Gasmasks));
    SizeList.SetColumn(SIZSIZE_STAFFLIST_COLUMN_NAMES[7], VReplace(USIZSizes.RESPIRATORS, Respirators));

    SizeList.Draw;
    if ModeType=mtEditing then
      SizeList.ReSelect(StaffIDs, SelectedID, True)  //возвращаем выделение строки

  finally
    SizeList.Visible:= True;
  end;
end;

procedure TSIZSizeForm.SizeListEdit;
var
  PersonSizes: TSIZStaffSizeIndexes;
begin
  if not SizeList.IsSelected then Exit;

  SIZStaffSizeIndexesSet(PersonSizes, Clothes[SizeList.SelectedIndex],
             Heights[SizeList.SelectedIndex], Shoes[SizeList.SelectedIndex],
             Heads[SizeList.SelectedIndex], Hands[SizeList.SelectedIndex],
             Gasmasks[SizeList.SelectedIndex], Respirators[SizeList.SelectedIndex]);

  if SizeEditFormShowModal(StaffIDs[SizeList.SelectedIndex], PersonSizes)=mrOK then
    SizeListLoad;
end;

procedure TSIZSizeForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_SIZSIZEFORM);
end;

procedure TSIZSizeForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_SIZSIZEFORM, ParamList.Params);
end;

procedure TSIZSizeForm.ViewUpdate(const AModeType: TModeType);
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

    SizeList.CanUnselect:= ModeType<>mtEditing;
    SizeList.CanSelect:= ModeType=mtEditing;
    ExportButton.Visible:= ModeType<>mtEditing;
    EditButton.Visible:= ModeType=mtEditing;

    MainPanel.BorderSpacing.Left:= 2*Ord(ModeType<>mtSetting);



  finally
    MainPanel.Visible:= True;
  end;
end;

procedure TSIZSizeForm.DataUpdate;
begin
  SizeListLoad;
end;

end.

