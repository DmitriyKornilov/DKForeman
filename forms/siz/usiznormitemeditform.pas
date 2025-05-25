unit USIZNormItemEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_StrUtils, DK_Dialogs,
  //Project utils
  UDataBase, UTypes, UImages, VirtualTrees;

type

  { TSIZNormItemEditForm }

  TSIZNormItemEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    PostLabel: TLabel;
    NormNameComboBox: TComboBox;
    ItemNameEdit: TEdit;
    ItemNameLabel: TLabel;
    NormNameLabel: TLabel;
    PostVT: TVirtualStringTree;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    ItemID: Integer;
    EditingType: TEditingType;
  end;

var
  SIZNormItemEditForm: TSIZNormItemEditForm;

implementation

{$R *.lfm}

{ TSIZNormItemEditForm }

procedure TSIZNormItemEditForm.FormCreate(Sender: TObject);
begin
  ItemID:= -1;
  Images.ToButtons([SaveButton, CancelButton]);
end;

procedure TSIZNormItemEditForm.FormShow(Sender: TObject);
begin
  SetEventButtons([SaveButton, CancelButton]);
  //FormKeepMinSize(Self);
  ItemNameEdit.SetFocus;
end;

procedure TSIZNormItemEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZNormItemEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  ItemName: String;
begin
  ItemName:= STrim(ItemNameEdit.Text);
  if ItemName=EmptyStr then
  begin
    Inform('Не указано наименование пункта!');
    Exit;
  end;


  case EditingType of
    etAdd:
      IsOK:= True;
    etEdit:
      IsOK:= True;
    UTypes.etCustom: //копирование в другие типовые нормы
      IsOK:= True;
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

