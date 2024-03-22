unit UStaffMainEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, LCLType, Buttons,
  //DK packages utils
  DK_StrUtils, DK_Dialogs,
  //Project utils
  UDataBase, UTypes;

type

  { TStaffMainEditForm }

  TStaffMainEditForm = class(TForm)
    ButtonPanelBevel: TBevel;
    ButtonPanel: TPanel;
    CancelButton: TSpeedButton;
    BornDatePicker: TDateTimePicker;
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure PatronymicEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    StaffID: Integer;
    EditingType: TEditingType;
  end;

var
  StaffMainEditForm: TStaffMainEditForm;

implementation

{$R *.lfm}

{ TStaffMainEditForm }

procedure TStaffMainEditForm.FormCreate(Sender: TObject);
begin
  StaffID:= -1;
end;

procedure TStaffMainEditForm.FormShow(Sender: TObject);
begin
  FamilyEdit.SetFocus;
end;

procedure TStaffMainEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TStaffMainEditForm.FamilyEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then NameEdit.SetFocus;
end;

procedure TStaffMainEditForm.NameEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then PatronymicEdit.SetFocus;
end;

procedure TStaffMainEditForm.PatronymicEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then BornDatePicker.SetFocus;
end;

procedure TStaffMainEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  FamilyValue, NameValue, PatronymicValue: String;
begin
  IsOK:= False;

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

  case EditingType of
    etAdd:
      IsOK:= DataBase.StaffMainAdd(StaffID, FamilyValue, NameValue, PatronymicValue,
                                         BornDatePicker.Date, GenderComboBox.ItemIndex);
    etEdit:
      IsOK:= DataBase.StaffMainUpdate(StaffID, FamilyValue, NameValue, PatronymicValue,
                                         BornDatePicker.Date, GenderComboBox.ItemIndex);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

