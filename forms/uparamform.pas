unit UParamForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, LCLType,
  //DK packages utils
  DK_CtrlUtils, DK_StrUtils, DK_Dialogs, DK_Const,
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
    Label1: TLabel;
    Label2: TLabel;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure CompanyEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DepartmentEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure QuoteInsert(const Sender: TObject; const Key: Word; const Shift: TShiftState);
  public
    Company, Department: String;
  end;

var
  ParamForm: TParamForm;

  function ParamFormOpen(var ACompany, ADepartment: String): Boolean;

implementation

function ParamFormOpen(var ACompany, ADepartment: String): Boolean;
var
  Form: TParamForm;
begin
  Result:= False;

  Form:= TParamForm.Create(nil);
  try
    Form.Company:= ACompany;
    Form.Department:= ADepartment;

    if Form.ShowModal=mrOK then
    begin
      ACompany:= Form.Company;
      ADepartment:= Form.Department;
      Result:= True;
    end;

  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TParamForm }

procedure TParamForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, True);

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

procedure TParamForm.QuoteInsert(const Sender: TObject; const Key: Word;
  const Shift: TShiftState);
begin
  if not (ssCtrl in Shift) then Exit;
  if Key=VK_OEM_COMMA then
    (Sender as TEdit).SelText:= SYMBOL_QUOTELEFT
  else if Key=VK_OEM_PERIOD then
    (Sender as TEdit).SelText:= SYMBOL_QUOTERIGHT;
end;

procedure TParamForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TParamForm.CompanyEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  QuoteInsert(Sender, Key, Shift);
end;

procedure TParamForm.DepartmentEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  QuoteInsert(Sender, Key, Shift);
end;

end.

