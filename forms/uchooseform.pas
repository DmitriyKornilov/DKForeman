unit UChooseForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, VirtualTrees,
  //DK packages utils
  DK_Vector, DK_VSTTools, DK_CtrlUtils;

type

  { TChooseForm }

  TChooseForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    TitleLabel: TLabel;
    SaveButton: TSpeedButton;
    VT: TVirtualStringTree;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    ChooseList: TVSTStringList;
  public
    ChooseItems: TStrVector;
    ChooseResult: Integer;
  end;

var
  ChooseForm: TChooseForm;

  function Choose(const ATitle: String; const AItems: TStrVector;
                  const AWidth: Integer = 0;
                  const AHeight: Integer = 0): Integer;

implementation

function Choose(const ATitle: String; const AItems: TStrVector;
                const AWidth: Integer = 0;
                const AHeight: Integer = 0): Integer;
var
  CF: TChooseForm;
begin
  CF:= TChooseForm.Create(nil);
  try
    CF.ChooseItems:= AItems;
    CF.TitleLabel.Caption:= ATitle;
    if AWidth>0 then
      CF.Width:= AWidth;
    if AHeight>0 then
      CF.Height:= AHeight;
    Result:= 0;
    if CF.ShowModal=mrOK then
      Result:= CF.ChooseResult;
  finally
    FreeAndNil(CF)
  end;
end;

{$R *.lfm}

{ TChooseForm }

procedure TChooseForm.FormShow(Sender: TObject);
begin
  FormToScreenCenter(Self);
  ChooseList.Update(ChooseItems);
  VT.BorderStyle:= bsSingle;
end;

procedure TChooseForm.SaveButtonClick(Sender: TObject);
begin
  ChooseResult:= ChooseList.SelectedIndex + 1;
  ModalResult:= mrOK;
end;

procedure TChooseForm.FormCreate(Sender: TObject);
begin
  ChooseList:= TVSTStringList.Create(VT, EmptyStr, nil);
end;

procedure TChooseForm.CancelButtonClick(Sender: TObject);
begin
  ChooseResult:= 0;
  ModalResult:= mrCancel;
end;

procedure TChooseForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ChooseList);
end;

end.

