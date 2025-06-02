unit USIZSizeEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, Buttons,
  //DK packages utils
  DK_VSTDropDown, DK_CtrlUtils, DK_Vector,
  //Project utils
  UDataBase, UConst, UImages, USIZSizes;

type

  { TSIZSizeEditForm }

  TSIZSizeEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    RespiratorLabel: TLabel;
    GlovesLabel: TLabel;
    GasmaskLabel: TLabel;
    MittensDressLabel: TLabel;
    ShoesLabel: TLabel;
    GasmaskBCButton: TBCButton;
    HeightLabel: TLabel;
    ClothesLabel: TLabel;
    RespiratorBCButton: TBCButton;
    MittensBCButton: TBCButton;
    GlovesBCButton: TBCButton;
    ShoesBCButton: TBCButton;
    HeightBCButton: TBCButton;
    ClothesBCButton: TBCButton;
    SaveButton: TSpeedButton;
    HeadDressBCButton: TBCButton;
    HeadDressLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    HeightDD: TVSTDropDown;
    ClothesDD: TVSTDropDown;
    ShoesDD: TVSTDropDown;
    HeadDressDD: TVSTDropDown;
    MittensDD: TVSTDropDown;
    GlovesDD: TVSTDropDown;
    GasmaskDD: TVSTDropDown;
    RespiratorDD: TVSTDropDown;
  public
    StaffID: Integer;
    SizeIndexes: TSIZStaffSizeIndexes;
  end;

var
  SIZSizeEditForm: TSIZSizeEditForm;

implementation

{$R *.lfm}

{ TSIZSizeEditForm }

procedure TSIZSizeEditForm.FormCreate(Sender: TObject);

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
  Images.ToButtons([SaveButton, CancelButton]);

  SIZStaffSizeIndexesClear(SizeIndexes);

  DropDownCreate(HeightDD, HeightBCButton, MANHEIGHTS);
  DropDownCreate(ClothesDD, ClothesBCButton, CLOTHES);
  DropDownCreate(ShoesDD, ShoesBCButton, SHOES);
  DropDownCreate(HeadDressDD, HeadDressBCButton, HEADDRESS);
  DropDownCreate(MittensDD, MittensBCButton, MITTENS);
  DropDownCreate(GlovesDD, GlovesBCButton, MITTENS);
  DropDownCreate(GasmaskDD, GasmaskBCButton, GASMASK);
  DropDownCreate(RespiratorDD, RespiratorBCButton, RESPIRATOR);
end;

procedure TSIZSizeEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(HeightDD);
  FreeAndNil(ClothesDD);
  FreeAndNil(ShoesDD);
  FreeAndNil(HeadDressDD);
  FreeAndNil(MittensDD);
  FreeAndNil(GlovesDD);
  FreeAndNil(GasmaskDD);
  FreeAndNil(RespiratorDD);
end;

procedure TSIZSizeEditForm.FormShow(Sender: TObject);
begin
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self);

  HeightDD.ItemIndex:= SizeIndexes.Height;
  ClothesDD.ItemIndex:= SizeIndexes.Clothes;
  ShoesDD.ItemIndex:= SizeIndexes.Shoes;
  HeadDressDD.ItemIndex:= SizeIndexes.HeadDress;
  MittensDD.ItemIndex:= SizeIndexes.Mittens;
  GlovesDD.ItemIndex:= SizeIndexes.Gloves;
  GasmaskDD.ItemIndex:= SizeIndexes.Gasmask;
  RespiratorDD.ItemIndex:= SizeIndexes.Respirator;
end;

procedure TSIZSizeEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
begin
  SizeIndexes.Height:= HeightDD.ItemIndex;
  SizeIndexes.Clothes:= ClothesDD.ItemIndex;
  SizeIndexes.Shoes:= ShoesDD.ItemIndex;
  SizeIndexes.HeadDress:= HeadDressDD.ItemIndex;
  SizeIndexes.Mittens:= MittensDD.ItemIndex;
  SizeIndexes.Gloves:= GlovesDD.ItemIndex;
  SizeIndexes.Gasmask:= GasmaskDD.ItemIndex;
  SizeIndexes.Respirator:= RespiratorDD.ItemIndex;

  IsOK:= DataBase.SIZStaffSizeUpdate(StaffID, SizeIndexes);

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSIZSizeEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

