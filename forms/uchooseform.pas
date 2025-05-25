unit UChooseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees,
  //DK packages utils
  DK_Vector, DK_VSTTableTools, DK_CtrlUtils,
  //Project utils
  UImages;

type

  { TChooseForm }

  TChooseForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    MainPanel: TPanel;
    SaveButton: TSpeedButton;
    VT1: TVirtualStringTree;
    VT2: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private

  public

  end;

var
  ChooseForm: TChooseForm;

  function Choose(const ATitle: String; const AItems: TStrVector;
                  out AChooseIndex: Integer;
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

  function Choose(const ATitle1, ATitle2: String;
                  const AItems1, AItems2: TStrVector;
                  out AChooseIndex1, AChooseIndex2: Integer;
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

{$R *.lfm}

const
  MIN_FORM_WIDTH = 500;
  MIN_FORM_HEIGHT = 250;

function Choose(const ATitle: String; const AItems: TStrVector;
                  out AChooseIndex: Integer;
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;
var
  i: Integer;
begin
  Result:= Choose(ATitle, EmptyStr, AItems, nil, AChooseIndex, i, AWidth, AHeight);
end;

function Choose(const ATitle1, ATitle2: String;
                const AItems1, AItems2: TStrVector;
                out AChooseIndex1, AChooseIndex2: Integer;
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  Form: TChooseForm;
  List1, List2: TVSTStringList;
begin
  Result:= False;
  AChooseIndex1:= -1;
  AChooseIndex1:= -2;

  Form:= TChooseForm.Create(nil);
  try
    List1:= TVSTStringList.Create(Form.VT1, ATitle1, nil);
    List2:= TVSTStringList.Create(Form.VT2, ATitle2, nil);
    try
      List1.AutoHeight:= True;
      List1.Update(AItems1);
      if VIsNil(AItems2) then
        Form.VT2.Visible:= False
      else begin
        List2.AutoHeight:= True;
        List2.Update(AItems2);
      end;

      if AWidth>MIN_FORM_WIDTH then
        Form.Width:= AWidth;
      if AHeight>MIN_FORM_HEIGHT then
        Form.Height:= AHeight;

      if Form.ShowModal=mrOK then
      begin
        AChooseIndex1:= List1.SelectedIndex;
        AChooseIndex2:= List2.SelectedIndex;
        Result:= True;
      end;

    finally
      FreeAndNil(List1);
      FreeAndNil(List2);
    end;
  finally
    FreeAndNil(Form);
  end;
end;

{ TChooseForm }

procedure TChooseForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  H:= ButtonPanel.Height + VT1.Height + 50;
  if VT2.Visible then
    H:= H + VT2.Height;

  if H>MIN_FORM_HEIGHT then
    ClientHeight:= H;

  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);

  FormToScreenCenter(Self);
end;

procedure TChooseForm.FormCreate(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
end;

procedure TChooseForm.SaveButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TChooseForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

