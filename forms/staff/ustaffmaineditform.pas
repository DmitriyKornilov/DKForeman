unit UStaffMainEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, LCLType, Buttons, BCButton,
  //DK packages utils
  DK_StrUtils, DK_Dialogs, DK_VSTDropDown, DK_CtrlUtils,
  //Project utils
  UDataBase, UTypes, UConst, UImages;

type

  { TStaffMainEditForm }

  TStaffMainEditForm = class(TForm)
    BornDatePicker: TDateTimePicker;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FamilyEdit: TEdit;
    NameEdit: TEdit;
    PatronymicEdit: TEdit;
    FamilyLabel: TLabel;
    NameLabel: TLabel;
    PatronymicLabel: TLabel;
    BornDateLabel: TLabel;
    GenderLabel: TLabel;
    GenderBCButton: TBCButton;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FamilyEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure PatronymicEditKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    StaffID: Integer;
    EditingType: TEditingType;
    GenderDropDown: TVSTDropDown;
  end;

var
  StaffMainEditForm: TStaffMainEditForm;

implementation

{$R *.lfm}

{ TStaffMainEditForm }

procedure TStaffMainEditForm.FormCreate(Sender: TObject);
begin
  StaffID:= -1;
  GenderDropDown:= TVSTDropDown.Create(GenderBCButton);
  GenderDropDown.Items:= GENDER_PICKS;
  GenderDropDown.ItemIndex:= 1;
end;

procedure TStaffMainEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(GenderDropDown);
end;

procedure TStaffMainEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self);
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
    Inform('Не указана фамилия!');
    Exit;
  end;

  NameValue:= STrim(NameEdit.Text);
  if NameValue=EmptyStr then
  begin
    Inform('Не указано имя!');
    Exit;
  end;

  PatronymicValue:= STrim(PatronymicEdit.Text);

  case EditingType of
    etAdd:
      IsOK:= DataBase.StaffMainAdd(StaffID, FamilyValue, NameValue, PatronymicValue,
                                         BornDatePicker.Date, GenderDropDown.ItemIndex);
    etEdit:
      IsOK:= DataBase.StaffMainUpdate(StaffID, FamilyValue, NameValue, PatronymicValue,
                                         BornDatePicker.Date, GenderDropDown.ItemIndex);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

