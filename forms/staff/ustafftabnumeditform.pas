unit UStaffTabNumEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_Dialogs, DK_Const,
  //Project utils
  UDBUtils, UTypes;

type

  { TStaffTabNumEditForm }

  TStaffTabNumEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    PostComboBox: TComboBox;
    SaveButton: TSpeedButton;
    TabNumEdit: TEdit;
    RecrutDatePicker: TDateTimePicker;
    DismissDatePicker: TDateTimePicker;
    RankEdit: TEdit;
    TabNumLabel: TLabel;
    RecrutLabel: TLabel;
    DismissLabel: TLabel;
    PostLabel: TLabel;
    RankLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    PostIDs: TIntVector;
  public
    EditingType: TEditingType; //etCustom - уволить (изменить дату увольнения)
    StaffID, TabNumID, PostID: Integer;
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
  PostID:= -1;

  RecrutDatePicker.MinDate:= NUlDATE;
  DismissDatePicker.MinDate:= NUlDATE;
end;

procedure TStaffTabNumEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TStaffTabNumEditForm.FormShow(Sender: TObject);
begin
  DataBase.PostDictionaryLoad(PostComboBox, PostIDs, PostID);

  if EditingType=etCustom then
  begin
    PostComboBox.Enabled:= False;
    DismissDatePicker.SetFocus;
  end
  else
    PostComboBox.SetFocus;
end;

procedure TStaffTabNumEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  TabNum, Rank: String;
begin
  IsOK:= False;

  if EditingType<>etCustom then //not Dismiss
  begin
    if SEmpty(PostComboBox.Text) then
    begin
      ShowInfo('Не указана должность!');
      Exit;
    end;

    TabNum:= STrim(TabNumEdit.Text);
    if SEmpty(TabNum) then
    begin
      ShowInfo('Не указан табельный номер!');
      Exit;
    end;
    if DataBase.StaffTabNumIsExists(TabNumID, TabNum) then
      if not Confirm('Табельный номер "' + TabNum + '" уже был назначен! Всё равно сохранить?') then Exit;

    PostID:= PostIDs[PostComboBox.ItemIndex];
    Rank:= STrim(RankEdit.Text);
  end;

  case EditingType of
    etAdd:
      IsOK:= DataBase.StaffTabNumAdd(TabNumID, StaffID, PostID,
                                             TabNum, Rank, RecrutDatePicker.Date);
    etEdit:
      IsOK:= DataBase.StaffTabNumUpdate(TabNumID, PostID,
                                             TabNum, Rank, RecrutDatePicker.Date);

    etCustom: //Dismiss
      IsOK:= DataBase.StaffTabNumDismiss(TabNumID, DismissDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

