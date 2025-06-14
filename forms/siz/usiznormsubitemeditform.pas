unit USIZNormSubItemEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, BCButton, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_Dialogs, DK_VSTTables, DK_VSTTableTools,
  DK_Vector, DK_Matrix, DK_VSTDropDown,
  //Project utils
  UDataBase, UTypes, UUtils, USIZUtils, USIZNormTypes, UImages,
  //Forms
  USearchForm;

type

  { TSIZNormSubItemEditForm }

  TSIZNormSubItemEditForm = class(TForm)
    SearchButton: TSpeedButton;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    LifeBCButton: TBCButton;
    InfoPanel: TPanel;
    InfoVT: TVirtualStringTree;
    ReasonBCButton: TBCButton;
    ClassBCButton: TBCButton;
    AddButton: TSpeedButton;
    DelButton: TSpeedButton;
    DownButton: TSpeedButton;
    EditButton: TSpeedButton;
    ToolPanel: TPanel;
    UpButton: TSpeedButton;
    YearsLabel: TLabel;
    NumLabel: TLabel;
    LifeLabel: TLabel;
    ClassLabel: TLabel;
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
    ClassDropDown: TVSTDropDown;
    LifeDropDown: TVSTDropDown;
    NameList: TVSTStringList;
    InfoTable: TVSTTable;

    OldSubItem: TNormSubItem;

    ReasonIDs, LifeIDs: TIntVector;
    ReasonNames, LifeNames: TStrVector;

    ClassIDs: TIntVector;
    ClassNames:TStrVector;
    Names, Units: TStrMatrix;
    NameIDs, SizeTypes: TIntMatrix;

    procedure ClassChange;
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
  Images.ToButtons([SaveButton, CancelButton]);

  ControlHeight(SearchButton, TOOL_PANEL_HEIGHT_DEFAULT-2);
  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT-2);

  SetToolButtons([
    SearchButton,
    AddButton, DelButton, EditButton, UpButton, DownButton
  ]);

  ReasonDropDown:= TVSTDropDown.Create(ReasonBCButton);
  ReasonDropDown.DropDownCount:= 20;

  ClassDropDown:= TVSTDropDown.Create(ClassBCButton);
  ClassDropDown.DropDownCount:= 20;
  ClassDropDown.OnChange:= @ClassChange;

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
  FreeAndNil(ClassDropDown);
  FreeAndNil(LifeDropDown);
  FreeAndNil(NameList);
  FreeAndNil(InfoTable);
end;

procedure TSIZNormSubItemEditForm.FormShow(Sender: TObject);
var
  IsSIZExists: Boolean;
begin
  SetEventButtons([SaveButton, CancelButton]);

  DataBase.KeyPickList('SIZREASON', 'ReasonID', 'ReasonName',
                       ReasonIDs, ReasonNames, False {with zero ID}, 'ReasonID');
  ReasonNames[0]:= 'НЕТ';
  ReasonDropDown.Items:= ReasonNames;
  ReasonDropDown.ItemIndex:= VIndexOf(ReasonIDs, SubItem.ReasonID);

  DataBase.KeyPickList('SIZSPECLIFE', 'SpecLifeID', 'SpecLifeName',
                       LifeIDs, LifeNames, False {with zero ID}, 'SpecLifeID');
  LifeNames[0]:= 'в месяцах';
  LifeDropDown.Items:= LifeNames;
  LifeDropDown.ItemIndex:= 0;

  IsSIZExists:= DataBase.SIZAssortmentLoad(ClassIDs, ClassNames, Names, Units, NameIDs, SizeTypes);
  ClassDropDown.Items:= ClassNames;
  if IsSIZExists then
    ClassDropDown.ItemIndex:= 0;
  AddButton.Enabled:= IsSIZExists;

  if EditingType=etEdit then
    NormSubItemCopy(SubItem, OldSubItem);
  InfoShow;
end;

procedure TSIZNormSubItemEditForm.LifeSpinEditChange(Sender: TObject);
begin
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
  ClassDropDown.ItemIndex:= i;
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

procedure TSIZNormSubItemEditForm.ClassChange;
begin
  NameList.Update(Names[ClassDropDown.ItemIndex]);
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
  InfoTable.AddColumn('Наименование', 200);
  InfoTable.AddColumn('Количество', 100);
  InfoTable.AddColumn('Срок службы',150);
  InfoTable.AutosizeColumnEnable('Наименование');
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

  InfoTable.ValuesClear;
  VDim(V{%H-}, Length(SubItem.Info.Lifes));
  for i:= 0 to High(SubItem.Info.Lifes) do
   V[i]:= SIZLifeStr(SubItem.Info.Lifes[i]);
  InfoTable.SetColumn('Наименование', SubItem.Info.Names, taLeftJustify);
  InfoTable.SetColumn('Количество', VIntToStr(SubItem.Info.Nums));
  InfoTable.SetColumn('Срок службы', V);
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
  i, j, k, Life: Integer;
  LifeName: String;
begin
  i:= ClassDropDown.ItemIndex;
  j:= NameList.ItemIndex;

  k:= VIndexOf(SubItem.Info.NameIDs, NameIDs[i, j]);
  if k>=0 then
  begin
    Inform('"' + Names[i, j] + '" уже есть в списке!');
    Exit;
  end;

  k:= Length(SubItem.Info.InfoIDs);
  if LifeDropDown.ItemIndex=0 then
  begin
    LifeName:= '<не указан>';
    Life:= LifeSpinEdit.Value;
  end
  else begin
    Life:= 0;
    LifeName:= LifeNames[LifeDropDown.ItemIndex];
  end;

  //!!!!!!!!!!!!!!!!!!!!!
  //NormSubItemInfoAdd(SubItem.Info, -1{need new ID}, k, ClassIDs[i], NameIDs[i, j],
  //                   SizeTypes[i, j], NumSpinEdit.Value,
  //                   LifeIDs[LifeDropDown.ItemIndex], Life,
  //                   Names[i, j], Units[i, j], LifeName);

  InfoShow(SubItem.Info.NameIDs[k]);
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

  Index:= VIndexOf(ClassIDs, SubItem.Info.SIZTypes[InfoTable.SelectedIndex]);
  if Index>=0 then
    ClassDropDown.ItemIndex:= Index;

  Index:= VIndexOf(NameIDs[Index], SubItem.Info.NameIDs[InfoTable.SelectedIndex]);
  if Index>=0 then
    NameList.ItemIndex:= Index;

  //!!!!!!!!!!!!!!!!!
  //Index:= VIndexOf(LifeIDs, SubItem.Info.LifeIDs[InfoTable.SelectedIndex]);
  //if Index>=0 then
  //  LifeDropDown.ItemIndex:= Index;

  NumSpinEdit.Value:= SubItem.Info.Nums[InfoTable.SelectedIndex];
  LifeSpinEdit.Value:= SubItem.Info.Lifes[InfoTable.SelectedIndex];
end;

procedure TSIZNormSubItemEditForm.InfoRowEditEnd;
var
  i, j, k: Integer;
begin
  i:= ClassDropDown.ItemIndex;
  j:= NameList.ItemIndex;

  k:= VIndexOf(SubItem.Info.NameIDs, NameIDs[i, j], InfoTable.SelectedIndex);
  if k>=0 then
  begin
    Inform('"' + Names[i, j] + '" уже есть в списке!');
    Exit;
  end;

  k:= InfoTable.SelectedIndex;
  SubItem.Info.SIZTypes[k]:= ClassIDs[i];
  SubItem.Info.NameIDs[k]:= NameIDs[i, j];
  SubItem.Info.Names[k]:= Names[i, j];
  SubItem.Info.Units[k]:= Units[i, j];
  SubItem.Info.SizeTypes[k]:= SizeTypes[i, j];
  SubItem.Info.Nums[k]:= NumSpinEdit.Value;
  //SubItem.Info.LifeIDs[k]:= LifeIDs[LifeDropDown.ItemIndex];

  if LifeDropDown.ItemIndex=0 then
  begin
    SubItem.Info.Lifes[k]:= LifeSpinEdit.Value;
    //SubItem.Info.LifeNames[k]:= '<не указан>'
  end
  else begin
    SubItem.Info.Lifes[k]:= 0;
    //SubItem.Info.LifeNames[k]:= LifeNames[LifeDropDown.ItemIndex];
  end;

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

