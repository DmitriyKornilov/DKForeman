unit USIZNormEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, Buttons, LCLType,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_StrUtils, DK_Dialogs,
  //Project utils
  UDataBase, UTypes, UImages;

type

  { TSIZNormEditForm }

  TSIZNormEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    InfEndDateCheckBox: TCheckBox;
    BeginDatePicker: TDateTimePicker;
    EndDatePicker: TDateTimePicker;
    NormNameEdit: TEdit;
    NoteEdit: TEdit;
    NormNameLabel: TLabel;
    BeginDateLabel: TLabel;
    NoteLabel: TLabel;
    EndDateLabel: TLabel;
    SaveButton: TSpeedButton;
    procedure BeginDatePickerChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InfEndDateCheckBoxChange(Sender: TObject);
    procedure NormNameEditKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure SaveButtonClick(Sender: TObject);
    procedure NoteEditKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
  private

  public
    NormID: Integer;
    EditingType: TEditingType;
  end;

var
  SIZNormEditForm: TSIZNormEditForm;

implementation

{$R *.lfm}

{ TSIZNormEditForm }

procedure TSIZNormEditForm.FormCreate(Sender: TObject);
begin
  NormID:= -1;
  Images.ToButtons([SaveButton, CancelButton]);
  BeginDatePicker.Date:= Date;
  EndDatePicker.Date:= Date;
  EndDatePicker.MinDate:= BeginDatePicker.Date;
end;

procedure TSIZNormEditForm.BeginDatePickerChange(Sender: TObject);
begin
  EndDatePicker.MinDate:= BeginDatePicker.Date;
end;

procedure TSIZNormEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZNormEditForm.FormShow(Sender: TObject);
begin
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self);
  NormNameEdit.SetFocus;
end;

procedure TSIZNormEditForm.InfEndDateCheckBoxChange(Sender: TObject);
begin
  if InfEndDateCheckBox.Checked then
  begin
    EndDatePicker.Date:= INFDATE;
    EndDatePicker.Enabled:= False;
  end
  else begin
    EndDatePicker.Enabled:= True;
    EndDatePicker.MinDate:= BeginDatePicker.Date;
    EndDatePicker.Date:= BeginDatePicker.Date;
  end;
end;

procedure TSIZNormEditForm.NormNameEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then NoteEdit.SetFocus;
end;

procedure TSIZNormEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  NormName, Note: String;
begin
  IsOK:= False;

  NormName:= STrim(NormNameEdit.Text);
  if NormName=EmptyStr then
  begin
    Inform('Не указано наименование норм выдачи СИЗ!');
    Exit;
  end;

  Note:= STrim(NoteEdit.Text);

  case EditingType of
    etAdd:
      IsOK:= DataBase.SIZNormAdd(NormID, NormName, Note,
                                 BeginDatePicker.Date, EndDatePicker.Date);
    etEdit:
      IsOK:= DataBase.SIZNormUpdate(NormID, NormName, Note,
                                 BeginDatePicker.Date, EndDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSIZNormEditForm.NoteEditKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_RETURN then BeginDatePicker.SetFocus;
end;

end.

