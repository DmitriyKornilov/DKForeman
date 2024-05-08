unit UChooseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees,
  //DK packages utils
  DK_Vector, DK_VSTTableTools, DK_CtrlUtils;

type

  { TChooseForm }

  TChooseForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    VT: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public

  end;

var
  ChooseForm: TChooseForm;

  function Choose(const ATitle: String; const AItems: TStrVector;
                  out AChooseIndex: Integer;
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

function Choose(const ATitle: String; const AItems: TStrVector;
                out AChooseIndex: Integer;
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  Form: TChooseForm;
  List: TVSTStringList;
begin
  Result:= False;
  AChooseIndex:= -1;
  Form:= TChooseForm.Create(nil);
  try
    List:= TVSTStringList.Create(Form.VT, ATitle, nil);
    try
      List.Update(AItems);
      Form.VT.BorderStyle:= bsSingle;

      if AWidth>0 then
        Form.Width:= AWidth;
      if AHeight>0 then
        Form.Height:= AHeight;

      if Form.ShowModal=mrOK then
      begin
        AChooseIndex:= List.SelectedIndex;
        Result:= True;
      end;

    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TChooseForm }

procedure TChooseForm.FormShow(Sender: TObject);
begin
  FormToScreenCenter(Self);
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

