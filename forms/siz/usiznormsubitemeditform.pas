unit USIZNormSubItemEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, BCButton, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_StrUtils, DK_Dialogs, DK_VSTTables, DK_VSTTableTools,
  DK_Vector, DK_Matrix, DK_VSTDropDown,
  //Project utils
  UDataBase, UTypes, UUtils, USIZUtils, USIZTypes, UImages;

type

  { TSIZNormSubItemEditForm }

  TSIZNormSubItemEditForm = class(TForm)
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
    procedure UpButtonClick(Sender: TObject);
  private
    ReasonDropDown: TVSTDropDown;
    ClassDropDown: TVSTDropDown;
    LifeDropDown: TVSTDropDown;
    NameList: TVSTStringList;

    ReasonIDs, LifeIDs: TIntVector;
    ReasonNames, LifeNames: TStrVector;

    ClassIDs: TIntVector;
    ClassNames:TStrVector;
    Names, Units: TStrMatrix;
    NameIDs, SizeTypes: TIntMatrix;

    InfoTable: TVSTTable;
    Info: TNormSubItemInfo;

    procedure ClassChange;
    procedure LifeChange;

    procedure InfoTableCreate;

    procedure InfoShow(const ASelectedID: Integer=-1);
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
  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT-2);

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

  NormSubItemInfoClear(Info);
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

  DataBase.SIZAssortmentLoad(ClassIDs, ClassNames, Names, Units, NameIDs, SizeTypes);
  ClassDropDown.Items:= ClassNames;
  ClassDropDown.ItemIndex:= 0;

  if EditingType=etEdit then
    NormSubItemInfoCopy(SubItem.Info, Info);
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


  case EditingType of
    etAdd:
      IsOK:= True;
    etEdit:
      IsOK:= True;
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
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

procedure TSIZNormSubItemEditForm.InfoShow(const ASelectedID: Integer = -1);
var
  i, SelectedID: Integer;
  V: TStrVector;
begin
  SelectedID:= GetSelectedID(InfoTable, Info.InfoIDs, ASelectedID);

  InfoTable.ValuesClear;
  VDim(V{%H-}, Length(Info.Lifes));
  for i:= 0 to High(Info.Lifes) do
   V[i]:= SIZLifeStr(Info.Lifes[i], Info.LifeNames[i]);
  InfoTable.SetColumn('Наименование', Info.Names, taLeftJustify);
  InfoTable.SetColumn('Количество', VIntToStr(Info.Nums));
  InfoTable.SetColumn('Срок службы', V);
  InfoTable.Draw;
  InfoTable.ReSelect(Info.InfoIDs, SelectedID, True);
end;

procedure TSIZNormSubItemEditForm.InfoRowSelect;
begin
  DelButton.Enabled:= InfoTable.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;
  UpButton.Enabled:= InfoTable.SelectedIndex>0;
  DownButton.Enabled:= InfoTable.SelectedIndex<High(Info.InfoIDs);
end;

procedure TSIZNormSubItemEditForm.InfoRowAdd;
begin

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

  Index:= VIndexOf(ClassIDs, Info.ClassIDs[InfoTable.SelectedIndex]);
  if Index>=0 then
    ClassDropDown.ItemIndex:= Index;

  Index:= VIndexOf(NameIDs[Index], Info.NameIDs[InfoTable.SelectedIndex]);
  if Index>=0 then
    NameList.ItemIndex:= Index;

  Index:= VIndexOf(LifeIDs, Info.LifeIDs[InfoTable.SelectedIndex]);
  if Index>=0 then
    LifeDropDown.ItemIndex:= Index;

  NumSpinEdit.Value:= Info.Nums[InfoTable.SelectedIndex];
  LifeSpinEdit.Value:= Info.Lifes[InfoTable.SelectedIndex];
end;

procedure TSIZNormSubItemEditForm.InfoRowEditEnd;
var
  i, j, k: Integer;
begin
  i:= ClassDropDown.ItemIndex;
  j:= NameList.ItemIndex;

  k:= VIndexOf(Info.NameIDs, NameIDs[i, j], InfoTable.SelectedIndex);
  if k>=0 then
  begin
    Inform('"' + Names[i, j] + '" уже есть в списке!');
    Exit;
  end;

  k:= InfoTable.SelectedIndex;
  Info.ClassIDs[k]:= ClassIDs[i];
  Info.NameIDs[k]:= NameIDs[i, j];
  Info.Names[k]:= Names[i, j];
  Info.Units[k]:= Units[i, j];
  Info.SizeTypes[k]:= SizeTypes[i, j];
  Info.Nums[k]:= NumSpinEdit.Value;
  Info.LifeIDs[k]:= LifeIDs[LifeDropDown.ItemIndex];
  Info.Lifes[k]:= LifeSpinEdit.Value;
  if LifeDropDown.ItemIndex=0 then
    Info.LifeNames[k]:= '<не указан>'
  else
    Info.LifeNames[k]:= LifeNames[LifeDropDown.ItemIndex];
  Info.YearNums[k]:= SIZNumInLifeStr(Info.Nums[k], Info.Lifes[k], Info.LifeNames[k]);

  InfoShow(Info.InfoIDs[k]);

  InfoVT.Enabled:= True;
  InfoRowSelect;
  AddButton.ImageIndex:= 4;
  AddButton.Hint:= 'Добавить';
end;

procedure TSIZNormSubItemEditForm.InfoRowDelete;
var
  i, ID: Integer;
begin
  if not InfoTable.IsSelected then Exit;
  if not Confirm('Удалить "' + Info.Names[InfoTable.SelectedIndex] + '"?') then Exit;

  if InfoTable.SelectedIndex<High(Info.InfoIDs) then
    ID:= Info.InfoIDs[InfoTable.SelectedIndex+1]
  else if InfoTable.SelectedIndex>0 then
    ID:= Info.InfoIDs[InfoTable.SelectedIndex-1]
  else
    ID:= -1;

  //удаляем выделенную строку из Info
  NormSubItemInfoDel(Info, InfoTable.SelectedIndex);
  //сдвигаем порядковые номера
  for i:= InfoTable.SelectedIndex to High(Info.OrderNums) do
    Info.OrderNums[i]:= i;

  InfoShow(ID);
end;

procedure TSIZNormSubItemEditForm.InfoRowMove(const ADirection: Integer);
var
  ID, Index: Integer;
begin
  Index:= InfoTable.SelectedIndex;
  ID:= Info.InfoIDs[Index];
  NormSubItemInfoSwap(Info, Index, Index + ADirection);
  InfoShow(ID);
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

