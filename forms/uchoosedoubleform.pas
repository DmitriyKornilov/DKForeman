unit UChooseDoubleForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees,
  //DK packages utils
  DK_Vector, DK_VSTTableTools, DK_CtrlUtils;

type

  { TChooseDoubleForm }

  TChooseDoubleForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    MainPanel: TPanel;
    SaveButton: TSpeedButton;
    VT1: TVirtualStringTree;
    VT2: TVirtualStringTree;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private

  public

  end;

var
  ChooseDoubleForm: TChooseDoubleForm;

  function Choose(const ATitle1, ATitle2: String;
                  const AItems1, AItems2: TStrVector;
                  out AChooseIndex1, AChooseIndex2: Integer;
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Boolean;

implementation

{$R *.lfm}

function Choose(const ATitle1, ATitle2: String;
                const AItems1, AItems2: TStrVector;
                out AChooseIndex1, AChooseIndex2: Integer;
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Boolean;
var
  Form: TChooseDoubleForm;
  List1, List2: TVSTStringList;
begin
  Result:= False;
  AChooseIndex1:= -1;
  AChooseIndex1:= -2;
  Form:= TChooseDoubleForm.Create(nil);
  try
    List1:= TVSTStringList.Create(Form.VT1, ATitle1, nil);
    List1.AutoHeight:= True;
    List2:= TVSTStringList.Create(Form.VT2, ATitle2, nil);
    try
      List1.Update(AItems1);
      List2.Update(AItems2);

      if AWidth>0 then
        Form.Width:= AWidth;
      if AHeight>0 then
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

{ TChooseDoubleForm }

procedure TChooseDoubleForm.FormShow(Sender: TObject);
begin
  FormToScreenCenter(Self);
end;

procedure TChooseDoubleForm.SaveButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TChooseDoubleForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

