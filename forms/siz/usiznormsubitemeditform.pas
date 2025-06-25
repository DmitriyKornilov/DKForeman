unit USIZNormSubItemEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, BCButton, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_Dialogs, DK_VSTTables, DK_VSTTableTools,
  DK_Vector, DK_Matrix, DK_VSTDropDown, DK_StrUtils,
  //Project utils
  UDataBase, UTypes, UConst, UUtils, USIZUtils, USIZNormTypes, UImages,
  //Forms
  USearchForm;

type

  { TSIZNormSubItemEditForm }

  TSIZNormSubItemEditForm = class(TForm)
    ClauseNameEdit: TEdit;
    ClauseNameLabel: TLabel;
    SearchButton: TSpeedButton;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    LifeBCButton: TBCButton;
    InfoPanel: TPanel;
    InfoVT: TVirtualStringTree;
    ReasonBCButton: TBCButton;
    TypeBCButton: TBCButton;
    AddButton: TSpeedButton;
    DelButton: TSpeedButton;
    DownButton: TSpeedButton;
    EditButton: TSpeedButton;
    ToolPanel: TPanel;
    UpButton: TSpeedButton;
    YearsLabel: TLabel;
    NumLabel: TLabel;
    LifeLabel: TLabel;
    TypeLabel: TLabel;
    ReasonLabel: TLabel;
    NameLabel: TLabel;
    NamePanel: TPanel;
    NameVT: TVirtualStringTree;
    SaveButton: TSpeedButton;
    NumSpinEdit: TSpinEdit;
    LifeSpinEdit: TSpinEdit;
    procedure AddButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LifeSpinEditChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure UpButtonClick(Sender: TObject);
  private
    ReasonDropDown: TVSTDropDown;
    TypeDropDown: TVSTDropDown;
    LifeDropDown: TVSTDropDown;
    NameList: TVSTStringList;
    InfoTable: TVSTTable;

    OldSubItem: TNormSubItem;

    ReasonIDs: TIntVector;
    ReasonNames: TStrVector;

    SIZTypes: TIntVector;
    Names, Units: TStrMatrix;
    NameIDs, SizeTypes: TIntMatrix;

    procedure TypeChange;
    procedure LifeChange;

    procedure InfoTableCreate;

    procedure InfoShow(const ASelectedNameID: Integer=-1);
    procedure InfoRowSelect;
    procedure InfoRowAdd;
    procedure InfoRowEditBegin;
    procedure InfoRowEditEnd;
    procedure InfoRowDelete;
    procedure InfoRowMove(const ADirection: Integer);
    procedure InfoRowMoveUp;
    procedure InfoRowMoveDown;

    function ValuesGetAndVerify(out AClauseName: String;
                            out ATypeIndex, ANameIndex, ALife: Integer;
                            const ANameSkipIndex: Integer = -1): Boolean;

  public
    ItemID: Integer;
    SubItem: TNormSubItem;
    EditingType: TEditingType;
  end;

var
  SIZNormSubItemEditForm: TSIZNormSubItemEditForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZNormSubItemEditForm }

procedure TSIZNormSubItemEditForm.FormCreate(Sender: TObject);
begin
  ReasonDropDown:= TVSTDropDown.Create(ReasonBCButton);
  ReasonDropDown.DropDownCount:= 20;

  TypeDropDown:= TVSTDropDown.Create(TypeBCButton);
  TypeDropDown.DropDownCount:= 20;
  TypeDropDown.OnChange:= @TypeChange;

  LifeDropDown:= TVSTDropDown.Create(LifeBCButton);
  LifeDropDown.DropDownCount:= 20;
  LifeDropDown.OnChange:= @LifeChange;

  NameList:= TVSTStringList.Create(NameVT, EmptyStr, nil);

  InfoTableCreate;

  NormSubItemClear(OldSubItem);
end;

procedure TSIZNormSubItemEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZNormSubItemEditForm.AddButtonClick(Sender: TObject);
begin
  if InfoVT.Enabled then
    InfoRowAdd
  else
    InfoRowEditEnd;
end;

procedure TSIZNormSubItemEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ReasonDropDown);
  FreeAndNil(TypeDropDown);
  FreeAndNil(LifeDropDown);
  FreeAndNil(NameList);
  FreeAndNil(InfoTable);
end;

procedure TSIZNormSubItemEditForm.FormShow(Sender: TObject);
var
  IsSIZExists: Boolean;
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);

  ControlHeight(SearchButton, TOOL_PANEL_HEIGHT_DEFAULT-2);
  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT-2);

  SetToolButtons([
    SearchButton,
    AddButton, DelButton, EditButton, UpButton, DownButton
  ]);

  DataBase.KeyPickList('SIZREASON', 'ReasonID', 'ReasonName',
                       ReasonIDs, ReasonNames, False {with zero ID}, 'ReasonID');
  ReasonNames[0]:= 'НЕТ';
  ReasonDropDown.Items:= ReasonNames;
  ReasonDropDown.ItemIndex:= VIndexOf(ReasonIDs, SubItem.ReasonID);

  LifeDropDown.Items:= VAdd(VCreateStr(['в месяцах']), SIZ_LIFE_PICKS);
  LifeDropDown.ItemIndex:= 0;

  IsSIZExists:= DataBase.SIZAssortmentLoad(SIZTypes, Names, Units, NameIDs, SizeTypes);
  TypeDropDown.Items:= VPickFromKey(SIZTypes, SIZ_TYPE_KEYS, SIZ_TYPE_PICKS);
  if IsSIZExists then
    TypeDropDown.ItemIndex:= 0;
  AddButton.Enabled:= IsSIZExists;

  if EditingType=etEdit then
    NormSubItemCopy(SubItem, OldSubItem);
  InfoShow;
end;

procedure TSIZNormSubItemEditForm.LifeSpinEditChange(Sender: TObject);
begin
  if LifeSpinEdit.Value<12 then
    YearsLabel.Caption:= EmptyStr
  else
    YearsLabel.Caption:= '(' + SIZLifeInYearsStr(LifeSpinEdit.Value) + ')';
end;

procedure TSIZNormSubItemEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
begin
  if VIsNil(SubItem.Info.InfoIDs) then
  begin
    Inform('Не указано ни одного наименования!');
    Exit;
  end;

  SubItem.ReasonID:= ReasonIDs[ReasonDropDown.ItemIndex];
  SubItem.Reason:= ReasonNames[ReasonDropDown.ItemIndex];
  //получаем свободный порядковый номер строки пункта нормы,
  //если редактируемый SubItem "пустой" или изменилось доп. условие выдачи
  if (SubItem.OrderNum<0) or (SubItem.ReasonID<>OldSubItem.ReasonID) then
    SubItem.OrderNum:= DataBase.SIZNormSubItemOrderNumFreeLoad(ItemID, SubItem.ReasonID);

  case EditingType of
    etAdd:
      IsOK:= DataBase.SIZNormSubItemAdd(ItemID, SubItem);
    etEdit:
      IsOK:= DataBase.SIZNormSubItemUpdate(ItemID, SubItem, OldSubItem);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSIZNormSubItemEditForm.SearchButtonClick(Sender: TObject);
var
  NameID, i, j: Integer;
begin
  if not Search('Фильтр по наименованию СИЗ:',
                'SIZNAME', 'NameID', 'SizName', NameID) then Exit;
  if not MIndexOf(NameIDs, NameID, i, j) then Exit;
  TypeDropDown.ItemIndex:= i;
  NameList.ItemIndex:= j;
end;

procedure TSIZNormSubItemEditForm.DelButtonClick(Sender: TObject);
begin
  InfoRowDelete;
end;

procedure TSIZNormSubItemEditForm.EditButtonClick(Sender: TObject);
begin
  InfoRowEditBegin;
end;

procedure TSIZNormSubItemEditForm.DownButtonClick(Sender: TObject);
begin
  InfoRowMoveDown;
end;

procedure TSIZNormSubItemEditForm.UpButtonClick(Sender: TObject);
begin
  InfoRowMoveUp;
end;

procedure TSIZNormSubItemEditForm.TypeChange;
begin
  NameList.Update(Names[TypeDropDown.ItemIndex]);
end;

procedure TSIZNormSubItemEditForm.LifeChange;
begin
  LifeSpinEdit.Visible:= LifeDropDown.ItemIndex=0;
  YearsLabel.Visible:= LifeSpinEdit.Visible;
end;

procedure TSIZNormSubItemEditForm.InfoTableCreate;
begin
  InfoTable:= TVSTTable.Create(InfoVT);
  InfoTable.SetSingleFont(MainForm.GridFont);
  InfoTable.HeaderFont.Bold:= True;
  InfoTable.AddColumn('Наименование СИЗ', 200);
  InfoTable.AddColumn('Нормы выдачи', 150);
  InfoTable.AddColumn('Основание выдачи',300);
  InfoTable.AutosizeColumnEnable('Наименование СИЗ');
  InfoTable.CanSelect:= True;
  InfoTable.CanUnselect:= False;
  InfoTable.OnSelect:= @InfoRowSelect;
  InfoTable.OnDelKeyDown:= @InfoRowDelete;
  InfoTable.OnReturnKeyDown:= @InfoRowEditBegin;
end;

procedure TSIZNormSubItemEditForm.InfoShow(const ASelectedNameID: Integer = -1);
var
  i, SelectedID: Integer;
  V: TStrVector;
begin
  SelectedID:= GetSelectedID(InfoTable, SubItem.Info.NameIDs, ASelectedNameID);


  VDim(V{%H-}, Length(SubItem.Info.Lifes));
  for i:= 0 to High(SubItem.Info.Lifes) do
   V[i]:= SubItem.Info.Units[i] + ', ' +
          SIZNumLifeStr(SubItem.Info.Nums[i], SubItem.Info.Lifes[i]);

  InfoTable.ValuesClear;
  InfoTable.SetColumn('Наименование СИЗ', SubItem.Info.Names, taLeftJustify);
  InfoTable.SetColumn('Нормы выдачи', V);
  InfoTable.SetColumn('Основание выдачи', SubItem.Info.ClauseNames, taLeftJustify);
  InfoTable.Draw;
  InfoTable.ReSelect(SubItem.Info.NameIDs, SelectedID, True);
end;

procedure TSIZNormSubItemEditForm.InfoRowSelect;
begin
  DelButton.Enabled:= InfoTable.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;
  UpButton.Enabled:= InfoTable.SelectedIndex>0;
  DownButton.Enabled:= InfoTable.SelectedIndex<High(SubItem.Info.InfoIDs);
end;

procedure TSIZNormSubItemEditForm.InfoRowAdd;
var
  i, j, OrderNum, Life: Integer;
  ClauseName: String;
begin
  if not ValuesGetAndVerify(ClauseName, i, j, Life) then Exit;

  OrderNum:= Length(SubItem.Info.InfoIDs);

  NormSubItemInfoAdd(SubItem.Info, -1{need new ID}, OrderNum, SIZTypes[i],
                     NameIDs[i, j], SizeTypes[i, j], NumSpinEdit.Value,
                     Life, Names[i, j], Units[i, j], ClauseName);

  InfoShow(SubItem.Info.NameIDs[OrderNum]);
end;

procedure TSIZNormSubItemEditForm.InfoRowEditBegin;
var
  Index: Integer;
begin
  if not InfoTable.IsSelected then Exit;
  InfoVT.Enabled:= False;

  DelButton.Enabled:= False;
  EditButton.Enabled:= False;
  UpButton.Enabled:= False;
  DownButton.Enabled:= False;
  AddButton.ImageIndex:= 14;
  AddButton.Hint:= 'Сохранить изменения';

  Index:= VIndexOf(SIZTypes, SubItem.Info.SIZTypes[InfoTable.SelectedIndex]);
  if Index>=0 then
    TypeDropDown.ItemIndex:= Index;

  Index:= VIndexOf(NameIDs[Index], SubItem.Info.NameIDs[InfoTable.SelectedIndex]);
  if Index>=0 then
    NameList.ItemIndex:= Index;

  Index:= SubItem.Info.Lifes[InfoTable.SelectedIndex];
  if Index>0 then
    Index:= 0
  else
    Index:= VIndexOf(SIZ_LIFE_KEYS, Index) + 1;
  LifeDropDown.ItemIndex:= Index;

  NumSpinEdit.Value:= SubItem.Info.Nums[InfoTable.SelectedIndex];
  LifeSpinEdit.Value:= SubItem.Info.Lifes[InfoTable.SelectedIndex];
  ClauseNameEdit.Text:= SubItem.Info.ClauseNames[InfoTable.SelectedIndex];
end;

function TSIZNormSubItemEditForm.ValuesGetAndVerify(out AClauseName: String;
                            out ATypeIndex, ANameIndex, ALife: Integer;
                            const ANameSkipIndex: Integer = -1): Boolean;
begin
  Result:= False;

  ATypeIndex:= TypeDropDown.ItemIndex;
  ANameIndex:= NameList.ItemIndex;

  if VIndexOf(SubItem.Info.NameIDs, NameIDs[ATypeIndex, ANameIndex], ANameSkipIndex)>=0 then
  begin
    Inform('"' + Names[ATypeIndex, ANameIndex] + '" уже есть в списке!');
    Exit;
  end;

  AClauseName:= STrim(ClauseNameEdit.Text);
  if SEmpty(AClauseName) then
  begin
    Inform('Не указан пункт Норм!');
    Exit;
  end;

  if LifeDropDown.ItemIndex=0 then
    ALife:= LifeSpinEdit.Value
  else
    ALife:= SIZ_LIFE_KEYS[LifeDropDown.ItemIndex-1];

  Result:= True;
end;

procedure TSIZNormSubItemEditForm.InfoRowEditEnd;
var
  i, j, k, Life: Integer;
  ClauseName: String;
begin
  if not ValuesGetAndVerify(ClauseName, i, j, Life, InfoTable.SelectedIndex) then Exit;

  k:= InfoTable.SelectedIndex;
  SubItem.Info.SIZTypes[k]:= SIZTypes[i];
  SubItem.Info.NameIDs[k]:= NameIDs[i, j];
  SubItem.Info.Names[k]:= Names[i, j];
  SubItem.Info.Units[k]:= Units[i, j];
  SubItem.Info.SizeTypes[k]:= SizeTypes[i, j];
  SubItem.Info.Nums[k]:= NumSpinEdit.Value;
  SubItem.Info.ClauseNames[k]:= ClauseName;
  SubItem.Info.Lifes[k]:= Life;

  InfoShow(SubItem.Info.NameIDs[k]);

  InfoVT.Enabled:= True;
  InfoRowSelect;
  AddButton.ImageIndex:= 4;
  AddButton.Hint:= 'Добавить';
end;

procedure TSIZNormSubItemEditForm.InfoRowDelete;
var
  i, NameID: Integer;
begin
  if not InfoTable.IsSelected then Exit;
  if not Confirm('Удалить "' + SubItem.Info.Names[InfoTable.SelectedIndex] + '"?') then Exit;

  if InfoTable.SelectedIndex<High(SubItem.Info.NameIDs) then
    NameID:= SubItem.Info.NameIDs[InfoTable.SelectedIndex+1]
  else if InfoTable.SelectedIndex>0 then
    NameID:= SubItem.Info.NameIDs[InfoTable.SelectedIndex-1]
  else
    NameID:= -1;

  //удаляем выделенную строку из Info
  NormSubItemInfoDel(SubItem.Info, InfoTable.SelectedIndex);
  //сдвигаем порядковые номера
  for i:= InfoTable.SelectedIndex to High(SubItem.Info.OrderNums) do
    SubItem.Info.OrderNums[i]:= i;

  InfoShow(NameID);
end;

procedure TSIZNormSubItemEditForm.InfoRowMove(const ADirection: Integer);
var
  NameID, Index: Integer;
begin
  Index:= InfoTable.SelectedIndex;
  NameID:= SubItem.Info.NameIDs[Index];
  NormSubItemInfoSwap(SubItem.Info, Index, Index + ADirection);
  InfoShow(NameID);
end;

procedure TSIZNormSubItemEditForm.InfoRowMoveUp;
begin
  InfoRowMove(-1);
end;

procedure TSIZNormSubItemEditForm.InfoRowMoveDown;
begin
  InfoRowMove(1);
end;

end.

