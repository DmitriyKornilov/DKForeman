unit UStaffTabNumEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons,
  //DK packages utils
  DK_StrUtils, DK_Dialogs, DK_Const,
  //Project utils
  UDataBase, UTypes, UUtils, UUIUtils;

type

  { TStaffTabNumEditForm }

  TStaffTabNumEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    TabNumEdit: TEdit;
    RecrutDatePicker: TDateTimePicker;
    DismissDatePicker: TDateTimePicker;
    TabNumLabel: TLabel;
    RecrutLabel: TLabel;
    DismissLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    EditingType: TEditingType; //etCustom - уволить (изменить дату увольнения)
    StaffID, TabNumID: Integer;
  end;

var
  StaffTabNumEditForm: TStaffTabNumEditForm;

implementation

{$R *.lfm}

{ TStaffTabNumEditForm }

procedure TStaffTabNumEditForm.FormCreate(Sender: TObject);
begin
  StaffID:= -1;
  TabNumID:= -1;
  SaveButton.Images:= ImageListForScreen;
  CancelButton.Images:= SaveButton.Images;
  RecrutDatePicker.MinDate:= NUlDATE;
  DismissDatePicker.MinDate:= NUlDATE;
end;

procedure TStaffTabNumEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TStaffTabNumEditForm.FormShow(Sender: TObject);
begin
  SetEditButtons([SaveButton, CancelButton]);
  Constraints.MinHeight:= Height;
  Constraints.MinWidth:= Width;
  if EditingType=etCustom then
    DismissDatePicker.SetFocus
  else
    TabNumEdit.SetFocus;
end;

procedure TStaffTabNumEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  TabNum: String;
begin
  IsOK:= False;

  if EditingType<>etCustom then //not Dismiss
  begin
    TabNum:= STrim(TabNumEdit.Text);
    if SEmpty(TabNum) then
    begin
      ShowInfo('Не указан табельный номер!');
      Exit;
    end;
    if DataBase.StaffTabNumIsExists(TabNumID, TabNum) then
      if not Confirm('Табельный номер "' + TabNum + '" уже был назначен! Всё равно сохранить?') then Exit;
  end;

  case EditingType of
    etAdd:
      IsOK:= DataBase.StaffTabNumAdd(TabNumID, StaffID, TabNum, RecrutDatePicker.Date);
    etEdit:
      IsOK:= DataBase.StaffTabNumUpdate(TabNumID, TabNum, RecrutDatePicker.Date);
    etCustom: //Dismiss
      IsOK:= DataBase.StaffTabNumDismiss(TabNumID, DismissDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

