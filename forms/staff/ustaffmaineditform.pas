unit UStaffMainEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, LCLType, Buttons,
  //DK packages utils
  DK_StrUtils, DK_Dialogs,
  //Project utils
  UDBUtils;

type

  { TStaffMainEditForm }

  TStaffMainEditForm = class(TForm)
    ButtonPanelBevel: TBevel;
    ButtonPanel: TPanel;
    CancelButton: TSpeedButton;
    BornDateTimePicker: TDateTimePicker;
    GenderComboBox: TComboBox;
    FamilyEdit: TEdit;
    NameEdit: TEdit;
    PatronymicEdit: TEdit;
    FamilyLabel: TLabel;
    NameLabel: TLabel;
    PatronymicLabel: TLabel;
    BornDateLabel: TLabel;
    GenderLabel: TLabel;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FamilyEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure NameEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure PatronymicEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SaveButtonClick(Sender: TObject);
  private
    CanFormClose: Boolean;
  public
    StaffID: Integer;
  end;

var
  StaffMainEditForm: TStaffMainEditForm;

implementation

{$R *.lfm}

{ TStaffMainEditForm }

procedure TStaffMainEditForm.FormShow(Sender: TObject);
begin
  CanFormClose:= True;
  if StaffID=0 then BornDateTimePicker.Date:= Date;
  FamilyEdit.SetFocus;
end;

procedure TStaffMainEditForm.CancelButtonClick(Sender: TObject);
begin
  CanFormClose:= True;
  ModalResult:= mrCancel;
end;

procedure TStaffMainEditForm.FamilyEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then NameEdit.SetFocus;
end;

procedure TStaffMainEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= CanFormClose;
end;

procedure TStaffMainEditForm.NameEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then PatronymicEdit.SetFocus;
end;

procedure TStaffMainEditForm.PatronymicEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then BornDateTimePicker.SetFocus;
end;

procedure TStaffMainEditForm.SaveButtonClick(Sender: TObject);
var
  FamilyValue, NameValue, PatronymicValue: String;
begin
  CanFormClose:= False;

  FamilyValue:= STrim(FamilyEdit.Text);
  if FamilyValue=EmptyStr then
  begin
    ShowInfo('Не указана фамилия!');
    Exit;
  end;

  NameValue:= STrim(NameEdit.Text);
  if NameValue=EmptyStr then
  begin
    ShowInfo('Не указано имя!');
    Exit;
  end;

  PatronymicValue:= STrim(PatronymicEdit.Text);

  if StaffID=0 then //add
    CanFormClose:= DataBase.StaffMainAdd(StaffID, FamilyValue, NameValue, PatronymicValue,
                                         BornDateTimePicker.Date, GenderComboBox.ItemIndex)
  else //edit
    CanFormClose:= DataBase.StaffMainUpdate(StaffID, FamilyValue, NameValue, PatronymicValue,
                                         BornDateTimePicker.Date, GenderComboBox.ItemIndex);
  if CanFormClose then
    ModalResult:= mrOK;
end;

end.

