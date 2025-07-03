unit UParamForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons,
  //DK packages utils
  DK_CtrlUtils, DK_StrUtils, DK_Dialogs,
  //Project utils
  UVars;

type

  { TParamForm }

  TParamForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    DepartmentEdit: TEdit;
    CompanyLabel: TLabel;
    CompanyEdit: TEdit;
    DepartmentLabel: TLabel;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    Company, Department: String;
  end;

var
  ParamForm: TParamForm;

implementation

{$R *.lfm}

{ TParamForm }

procedure TParamForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);

  CompanyEdit.Text:= Company;
  DepartmentEdit.Text:= Department;
end;

procedure TParamForm.SaveButtonClick(Sender: TObject);
var
  IsChanged: Boolean;

  procedure ValueChange(const AParamName, ANewValue: String;
                        var AOldValue: String;
                        var AIsChanged: Boolean);
  var
    S: String;
  begin
    S:= STrim(ANewValue);
    if SSame(S, AOldValue) then Exit;

    DataBase.TextParamUpdate(AParamName, S);
    AOldValue:= S;
    AIsChanged:= True;
  end;

begin
  IsChanged:= False;
  ValueChange('COMPANY', CompanyEdit.Text, Company, IsChanged);
  ValueChange('DEPARTMENT', DepartmentEdit.Text, Department, IsChanged);

  if not IsChanged then
  begin
    Inform('Не внеcено никаких изменений!');
    Exit;
  end;

  ModalResult:= mrOK;
end;

procedure TParamForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

