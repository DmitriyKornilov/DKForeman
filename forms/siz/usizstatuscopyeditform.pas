unit USIZStatusCopyEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, Buttons,
  //Project utils
  UVars, UConst, UTypes, USIZUtils,
  //DK packages utils
  DK_CtrlUtils, DK_Vector, DK_Matrix, DK_VSTDropDown, DK_DateUtils,
  DK_Dialogs;

type

  { TSIZStatusCopyEditForm }

  TSIZStatusCopyEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    CardBCButton: TBCButton;
    CardLabel: TLabel;
    SaveButton: TSpeedButton;
    SIZListLabel: TLabel;
    SIZNeedCountLabel: TLabel;
    SIZNeedCountNameLabel: TLabel;
    SIZNeedLabel: TLabel;
    SIZNeedNameLabel: TLabel;
    SIZNeedSizeLabel: TLabel;
    SIZNeedSizeNameLabel: TLabel;
    SIZPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    TabNumID, CardID: Integer;
  end;

var
  SIZStatusCopyEditForm: TSIZStatusCopyEditForm;

implementation

{$R *.lfm}

{ TSIZStatusCopyEditForm }

procedure TSIZStatusCopyEditForm.FormCreate(Sender: TObject);
begin

end;

procedure TSIZStatusCopyEditForm.FormDestroy(Sender: TObject);
begin

end;

procedure TSIZStatusCopyEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);
end;

end.

