unit USIZSizeSpecEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, Buttons,
  //DK packages utils
  DK_VSTDropDown, DK_CtrlUtils, DK_Vector,
  //Project utils
  UVars, UConst, USIZSizes;

type

  { TSIZSizeSpecEditForm }

  TSIZSizeSpecEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SizeBCButton: TBCButton;
    SizeLabel: TLabel;
    HeightBCButton: TBCButton;
    HeightLabel: TLabel;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    SizeDropDown: TVSTDropDown;
    HeightDropDown: TVSTDropDown;

    procedure DropDownCreate;
  public
    SizeID, HeightID, SizeType: Integer;
    TabNumID, InfoID: Integer;
  end;

var
  SIZSizeSpecEditForm: TSIZSizeSpecEditForm;

  function SIZSizeSpecEditFormOpen(const ATabNumID, AInfoID, ASizeType: Integer;
                                   var ASizeID, AHeightID: Integer): Boolean;

implementation

function SIZSizeSpecEditFormOpen(const ATabNumID, AInfoID, ASizeType: Integer;
                                 var ASizeID, AHeightID: Integer): Boolean;
var
  Form: TSIZSizeSpecEditForm;
begin
  Form:= TSIZSizeSpecEditForm.Create(nil);
  try
    Form.TabNumID:= ATabNumID;
    Form.InfoID:= AInfoID;
    Form.SizeType:= ASizeType;
    Form.SizeID:= ASizeID;
    Form.HeightID:= AHeightID;

    Result:= Form.ShowModal=mrOK;
    if Result then
    begin
      ASizeID:= Form.SizeID;
      AHeightID:= Form.HeightID;
    end;

  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZSizeSpecEditForm }

procedure TSIZSizeSpecEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SizeDropDown);
  FreeAndNil(HeightDropDown);
end;

procedure TSIZSizeSpecEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZSizeSpecEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);

  DropDownCreate;

  HeightLabel.Visible:= SizeType=1;
  HeightBCButton.Visible:= SizeType=1;

  FormKeepMinSize(Self);
end;

procedure TSIZSizeSpecEditForm.SaveButtonClick(Sender: TObject);
begin
  if not DataBase.SIZSpecSizeUpdate(InfoID, TabNumID, SizeDropDown.ItemIndex,
                                    HeightDropDown.ItemIndex) then Exit;

  SizeID:= SizeDropDown.ItemIndex;
  HeightID:= HeightDropDown.ItemIndex;

  ModalResult:= mrOK;
end;

procedure TSIZSizeSpecEditForm.DropDownCreate;
var
  V: TStrVector;
begin
  SizeDropDown:= TVSTDropDown.Create(SizeBCButton);
  case SizeType of
    1: V:= VCreateStr(CLOTHES);
    2: V:= VCreateStr(SHOES);
    3: V:= VCreateStr(HEADDRESS);
    4: V:= VCreateStr(HANDS);
    5: V:= VCreateStr(GASMASKS);
    6: V:= VCreateStr(RESPIRATORS);
  end;
  V[0]:= '<нет>';
  SizeDropDown.Items:= V;
  SizeDropDown.ItemIndex:= SizeID;

  HeightDropDown:= TVSTDropDown.Create(HeightBCButton);
  V:= VCreateStr(PERSONHEIGHTS);
  V[0]:= '<нет>';
  HeightDropDown.Items:= V;
  HeightDropDown.ItemIndex:= HeightID;
end;

end.

