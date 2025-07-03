unit USIZCardEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, Buttons,
  //DK packages utils
  DK_VSTDropDown, DK_CtrlUtils, DK_StrUtils, DK_Vector,
  //Project utils
  UVars, UConst, USIZSizes;

type

  { TSIZCardEditForm }

  TSIZCardEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    CardNumEdit: TEdit;
    CardNumLabel: TLabel;
    CardNumLabel1: TLabel;
    CardNumPanel: TPanel;
    ClothesBCButton: TBCButton;
    ClothesLabel: TLabel;
    GasmaskBCButton: TBCButton;
    GasmaskLabel: TLabel;
    HandBCButton: TBCButton;
    HandLabel: TLabel;
    HeadBCButton: TBCButton;
    HeadLabel: TLabel;
    HeightBCButton: TBCButton;
    HeightLabel: TLabel;
    SizePanel: TPanel;
    RespiratorBCButton: TBCButton;
    RespiratorLabel: TLabel;
    SaveButton: TSpeedButton;
    ShoesBCButton: TBCButton;
    ShoesLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    HeightDD: TVSTDropDown;
    ClothesDD: TVSTDropDown;
    ShoesDD: TVSTDropDown;
    HeadDD: TVSTDropDown;
    HandDD: TVSTDropDown;
    GasmaskDD: TVSTDropDown;
    RespiratorDD: TVSTDropDown;
  public
    CardNum: String;
    CardID, StaffID, TabNumID, ItemPostID: Integer;
    SizeIndexes: TSIZStaffSizeIndexes;
    SizeOnly: Boolean;
  end;

var
  SIZCardEditForm: TSIZCardEditForm;

  function SIZCardEditFormOpen(const AStaffID: Integer;
                               var ASizes: TSIZStaffSizeIndexes;
                               const ASizeOnly: Boolean;
                               var ACardID: Integer;
                               var ACardNum: String;
                               const ATabNumID: Integer = 0;
                               const AItemPostID: Integer = 0): Boolean;

implementation

function SIZCardEditFormOpen(const AStaffID: Integer;
                             var ASizes: TSIZStaffSizeIndexes;
                             const ASizeOnly: Boolean;
                             var ACardID: Integer;
                             var ACardNum: String;
                             const ATabNumID: Integer = 0;
                             const AItemPostID: Integer = 0): Boolean;
var
  Form: TSIZCardEditForm;
begin
  Form:= TSIZCardEditForm.Create(nil);
  try
    Form.StaffID:= AStaffID;
    Form.SizeIndexes:= ASizes;
    Form.SizeOnly:= ASizeOnly;
    Form.CardID:= ACardID;
    Form.CardNum:= ACardNum;
    Form.TabNumID:= ATabNumID;
    Form.ItemPostID:= AItemPostID;

    Result:= Form.ShowModal=mrOK;
    if Result then
    begin
      ASizes:= Form.SizeIndexes;
      ACardID:= Form.CardID;
      ACardNum:= Form.CardNum;
    end;

  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZCardEditForm }

procedure TSIZCardEditForm.FormCreate(Sender: TObject);

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
  SizeOnly:= True;
  CardID:= 0;
  CardNum:= EmptyStr;
  TabNumID:= 0;
  ItemPostID:= 0;

  SIZStaffSizeIndexesClear(SizeIndexes);

  DropDownCreate(HeightDD, HeightBCButton, PERSONHEIGHTS);
  DropDownCreate(ClothesDD, ClothesBCButton, CLOTHES);
  DropDownCreate(ShoesDD, ShoesBCButton, SHOES);
  DropDownCreate(HeadDD, HeadBCButton, HEADDRESS);
  DropDownCreate(HandDD, HandBCButton, HANDS);
  DropDownCreate(GasmaskDD, GasmaskBCButton, GASMASKS);
  DropDownCreate(RespiratorDD, RespiratorBCButton, RESPIRATORS);
end;

procedure TSIZCardEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(HeightDD);
  FreeAndNil(ClothesDD);
  FreeAndNil(ShoesDD);
  FreeAndNil(HeadDD);
  FreeAndNil(HandDD);
  FreeAndNil(GasmaskDD);
  FreeAndNil(RespiratorDD);
end;

procedure TSIZCardEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);

  HeightDD.ItemIndex:= SizeIndexes.Height;
  ClothesDD.ItemIndex:= SizeIndexes.Clothes;
  ShoesDD.ItemIndex:= SizeIndexes.Shoes;
  HeadDD.ItemIndex:= SizeIndexes.Head;
  HandDD.ItemIndex:= SizeIndexes.Hand;
  GasmaskDD.ItemIndex:= SizeIndexes.Gasmask;
  RespiratorDD.ItemIndex:= SizeIndexes.Respirator;

  CardNumPanel.Visible:= not SizeOnly;
  CardNumEdit.Text:= CardNum;

  if SizeOnly then
    Caption:= 'Размеры'
  else
    Caption:= 'Личная карточка';

  FormKeepMinSize(Self);
end;

procedure TSIZCardEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  S: String;
begin
  SizeIndexes.Height:= HeightDD.ItemIndex;
  SizeIndexes.Clothes:= ClothesDD.ItemIndex;
  SizeIndexes.Shoes:= ShoesDD.ItemIndex;
  SizeIndexes.Head:= HeadDD.ItemIndex;
  SizeIndexes.Hand:= HandDD.ItemIndex;
  SizeIndexes.Gasmask:= GasmaskDD.ItemIndex;
  SizeIndexes.Respirator:= RespiratorDD.ItemIndex;

  S:= STrim(CardNumEdit.Text);

  IsOK:= DataBase.SIZStaffSizeUpdate(StaffID, SizeIndexes);

  if IsOK and (not SizeOnly) then
  begin
    if CardID=0 then
      IsOK:= DataBase.SIZPersonalCardAdd(CardID, S, TabNumID, ItemPostID)
    else
      IsOK:= DataBase.SIZPersonalCardUpdate(CardID, S);
  end;

  if not IsOK then Exit;

  CardNum:= S;
  ModalResult:= mrOK;
end;

procedure TSIZCardEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

