unit USIZStoreEntryEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, Spin, BCButton, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_Dialogs, DK_VSTTables, DK_VSTTableTools,
  DK_Vector, DK_Matrix, DK_VSTDropDown, DK_StrUtils,
  //Project utils
  UVars, UTypes, UConst, UUtils, USIZUtils, USIZSizes,
  //Forms
  USearchForm;

type

  { TSIZStoreEntryEditForm }

  TSIZStoreEntryEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    VolumeSpinEdit: TSpinEdit;
    VolumeLabel: TLabel;
    SizeBCButton: TBCButton;
    HeightBCButton: TBCButton;
    SizeLabel: TLabel;
    NamePanel: TPanel;
    NameVT: TVirtualStringTree;
    NomNumEdit: TEdit;
    NameLabel: TLabel;
    NoteEdit: TEdit;
    NomNumLabel: TLabel;
    NoteLabel: TLabel;
    NumLabel: TLabel;
    NumSpinEdit: TSpinEdit;
    SaveButton: TSpeedButton;
    SearchButton: TSpeedButton;
    HeightLabel: TLabel;
    TypeBCButton: TBCButton;
    TypeLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
  private
    NameList: TVSTStringList;
    TypeDropDown: TVSTDropDown;
    SizeDropDown: TVSTDropDown;
    HeightDropDown: TVSTDropDown;

    SIZTypes: TIntVector;
    Names, Units: TStrMatrix;
    NameIDs, SizeTypes: TIntMatrix;

    procedure TypeChange;

    procedure NameListSelect;
  public
    DocID, NameID, SizeID, HeightID: Integer;
    EntryID: Int64;
    EditingType: TEditingType;
  end;

var
  SIZStoreEntryEditForm: TSIZStoreEntryEditForm;

implementation

{$R *.lfm}

{ TSIZStoreEntryEditForm }

procedure TSIZStoreEntryEditForm.FormCreate(Sender: TObject);
var
  IsSIZExists: Boolean;
begin
  EntryID:= 0;
  DocID:= 0;

  TypeDropDown:= TVSTDropDown.Create(TypeBCButton);
  TypeDropDown.DropDownCount:= 20;
  TypeDropDown.OnChange:= @TypeChange;

  NameList:= TVSTStringList.Create(NameVT, EmptyStr, @NameListSelect);

  IsSIZExists:= DataBase.SIZAssortmentLoad(SIZTypes, Names, Units, NameIDs, SizeTypes);
  TypeDropDown.Items:= VPickFromKey(SIZTypes, SIZ_TYPE_KEYS, SIZ_TYPE_PICKS);
  if IsSIZExists then
    TypeDropDown.ItemIndex:= 0;
end;

procedure TSIZStoreEntryEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(NameList);
  FreeAndNil(TypeDropDown);
  if Assigned(SizeDropDown) then FreeAndNil(SizeDropDown);
  if Assigned(HeightDropDown) then FreeAndNil(HeightDropDown);
end;

procedure TSIZStoreEntryEditForm.FormShow(Sender: TObject);
//var
//  IsSIZExists: Boolean;
begin
  Images.ToButtons([SaveButton, CancelButton, SearchButton]);
  SetEventButtons([SaveButton, CancelButton]);
  ControlHeight(SearchButton, TOOL_PANEL_HEIGHT_DEFAULT-2);
  SetToolButtons([SearchButton]);
  FormKeepMinSize(Self, False);

  //IsSIZExists:= DataBase.SIZAssortmentLoad(SIZTypes, Names, Units, NameIDs, SizeTypes);
  //TypeDropDown.Items:= VPickFromKey(SIZTypes, SIZ_TYPE_KEYS, SIZ_TYPE_PICKS);
  //if IsSIZExists then
  //  TypeDropDown.ItemIndex:= 0;

  NomNumEdit.SetFocus;
end;

procedure TSIZStoreEntryEditForm.TypeChange;
begin
  NameList.Update(Names[TypeDropDown.ItemIndex]);
end;

procedure TSIZStoreEntryEditForm.NameListSelect;
var
  SizeType: Integer;

  procedure DropDownCreate(var ADropDown: TVSTDropDown;
                           const AButton: TBCButton;
                           const AValues: TStrVector);
  var
    V: TStrVector;
  begin
    ADropDown:= TVSTDropDown.Create(AButton);
    V:= VCreateStr(AValues);
    V[0]:= '<нет>';
    ADropDown.Items:= V;
  end;

begin
  SizeLabel.Visible:= False;
  SizeBCButton.Visible:= False;
  HeightLabel.Visible:= False;
  HeightBCButton.Visible:= False;
  VolumeLabel.Visible:= False;
  VolumeSpinEdit.Visible:= False;

  if Assigned(SizeDropDown) then FreeAndNil(SizeDropDown);
  if Assigned(HeightDropDown) then FreeAndNil(HeightDropDown);

  if not NameList.IsSelected then Exit;

  SizeType:= SizeTypes[TypeDropDown.ItemIndex, NameList.SelectedIndex];
  if SizeType=0 then Exit;

  if SizeType>6 then
  begin
    VolumeLabel.Visible:= True;
    VolumeSpinEdit.Visible:= True;
  end
  else begin
    SizeLabel.Visible:= True;
    SizeBCButton.Visible:= True;
    case SizeType of
      1:
        begin
          DropDownCreate(SizeDropDown, SizeBCButton, CLOTHES);
          HeightLabel.Visible:= True;
          HeightBCButton.Visible:= True;
          DropDownCreate(HeightDropDown, HeightBCButton, PERSONHEIGHTS);
          HeightDropDown.ItemIndex:= 0;
        end;
      2: DropDownCreate(SizeDropDown, SizeBCButton, SHOES);
      3: DropDownCreate(SizeDropDown, SizeBCButton, HEADDRESS);
      4: DropDownCreate(SizeDropDown, SizeBCButton, HANDS);
      5: DropDownCreate(SizeDropDown, SizeBCButton, GASMASKS);
      6: DropDownCreate(SizeDropDown, SizeBCButton, RESPIRATORS);
    end;
    SizeDropDown.ItemIndex:= 0;
  end;


end;

procedure TSIZStoreEntryEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZStoreEntryEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  NomNum: String;

  procedure SizeAndHeight;
  var
    SizeType: Integer;
  begin
    SizeID:= 0;
    HeightID:= 0;
    SizeType:= SizeTypes[TypeDropDown.ItemIndex, NameList.SelectedIndex];
    if SizeType=0 then Exit;

    if SizeType>6 then
      SizeID:= VolumeSpinEdit.Value
    else begin
      SizeID:= SizeDropDown.ItemIndex;
      if SizeType=1 then
        HeightID:= HeightDropDown.ItemIndex;
    end;
  end;

begin
  NomNum:= STrim(NomNumEdit.Text);
  if SEmpty(NomNum) then
  begin
    Inform('Не указан номенклатурный номер!');
    Exit;
  end;

  if not NameList.IsSelected then
  begin
    Inform('Не указано ни одного наименования СИЗ!');
    Exit;
  end;

  NameID:= NameIDs[TypeDropDown.ItemIndex, NameList.SelectedIndex];
  SizeAndHeight;

  if DataBase.SIZStoreEntryExists(EntryID, NomNum, NameID, SizeID, HeightID, DocID) then
  begin
    Inform('СИЗ уже есть в этом документе!');
    Exit;
  end;

  case EditingType of
    etAdd:
      IsOK:= DataBase.SIZStoreEntryWrite(EntryID, NomNum, STrim(NoteEdit.Text),
                         NameID, SizeID, HeightID, NumSpinEdit.Value, DocID);
    etEdit: ;
      //IsOK:= DataBase.SIZNormSubItemUpdate(ItemID, SubItem, OldSubItem);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSIZStoreEntryEditForm.SearchButtonClick(Sender: TObject);
var
  ID, i, j: Integer;
begin
  if not Search('Фильтр по наименованию СИЗ:',
                'SIZNAME', 'ID', 'SizName', ID) then Exit;
  if not MIndexOf(NameIDs, ID, i, j) then Exit;
  TypeDropDown.ItemIndex:= i;
  NameList.ItemIndex:= j;
end;

end.

