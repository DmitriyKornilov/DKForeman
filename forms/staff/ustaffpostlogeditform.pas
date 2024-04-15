unit UStaffPostLogEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons, BCButton, DateUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_Dialogs, DK_Const, DK_VSTDropDown,
  //Project utils
  UDataBase, UTypes, UConst;

type

  { TStaffPostlogEditForm }

  TStaffPostlogEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    PostBCButton: TBCButton;
    StatusBCButton: TBCButton;
    SaveButton: TSpeedButton;
    PostLabel: TLabel;
    RankEdit: TEdit;
    RankLabel: TLabel;
    FirstDatePicker: TDateTimePicker;
    FirstDateLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    PostIDs: TIntVector;
    PostDropDown: TVSTDropDown;

  public
    EditingType: TEditingType;
    TabNumID, PostID, PostLogID, PrevPostLogID: Integer;
    StatusDropDown: TVSTDropDown;
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

procedure TStaffPostlogEditForm.FormCreate(Sender: TObject);
begin
  StatusDropDown:= TVSTDropDown.Create(StatusBCButton);
  StatusDropDown.Items:= POST_STATUS_PICKS;
  StatusDropDown.ItemIndex:= 0;
  PostDropDown:= TVSTDropDown.Create(PostBCButton);
end;

procedure TStaffPostlogEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PostDropDown);
  FreeAndNil(StatusDropDown);
end;

procedure TStaffPostlogEditForm.FormShow(Sender: TObject);
begin
  DataBase.PostDictionaryLoad(PostDropDown, PostIDs, PostID);
end;

procedure TStaffPostlogEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  Rank: String;
begin
  IsOK:= False;

  if PostDropDown.ItemIndex<0 then
  begin
    ShowInfo('Не указана должность!');
    Exit;
  end;

  Rank:= STrim(RankEdit.Text);
  PostID:= PostIDs[PostDropDown.ItemIndex];

  case EditingType of
    etAdd:  //превод
      IsOK:= DataBase.StaffPostLogAdd(PostLogID, TabNumID, PostID,
                         StatusDropDown.ItemIndex, Rank, FirstDatePicker.Date);
    etEdit:
      IsOK:= DataBase.StaffPostLogUpdate(PrevPostLogID, PostLogID, PostID,
                         StatusDropDown.ItemIndex, Rank, FirstDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

