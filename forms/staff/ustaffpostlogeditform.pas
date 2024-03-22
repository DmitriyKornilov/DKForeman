unit UStaffPostLogEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons, DateUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_Dialogs, DK_Const,
  //Project utils
  UDataBase, UTypes;

type

  { TStaffPostlogEditForm }

  TStaffPostlogEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    StatusComboBox: TComboBox;
    PostComboBox: TComboBox;
    PostLabel: TLabel;
    RankEdit: TEdit;
    RankLabel: TLabel;
    FirstDatePicker: TDateTimePicker;
    FirstDateLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    PostIDs: TIntVector;
  public
    EditingType: TEditingType;
    TabNumID, PostID, PostLogID, PrevPostLogID: Integer;
  end;

var
  StaffPostLogEditForm: TStaffPostLogEditForm;

implementation

{$R *.lfm}

{ TStaffPostlogEditForm }

procedure TStaffPostlogEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TStaffPostlogEditForm.FormShow(Sender: TObject);
begin
  DataBase.PostDictionaryLoad(PostComboBox, PostIDs, PostID);
end;

procedure TStaffPostlogEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  Rank: String;
begin
  IsOK:= False;

  if SEmpty(PostComboBox.Text) then
  begin
    ShowInfo('Не указана должность!');
    Exit;
  end;

  Rank:= STrim(RankEdit.Text);
  PostID:= PostIDs[PostComboBox.ItemIndex];

  case EditingType of
    etAdd:  //превод
      IsOK:= DataBase.StaffPostLogAdd(PostLogID, TabNumID, PostID,
                         StatusComboBox.ItemIndex, Rank, FirstDatePicker.Date);
    etEdit:
      IsOK:= DataBase.StaffPostLogUpdate(PrevPostLogID, PostLogID, PostID,
                         StatusComboBox.ItemIndex, Rank, FirstDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

