unit UBriefingEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, DateTimePicker, Buttons, Spin, VirtualTrees,
  //DK packages utils
  DK_StrUtils, DK_Dialogs, DK_VSTDropDown, DK_CtrlUtils, DK_Const, DK_Vector,
  DK_VSTTableTools,
  //Project utils
  UTypes, UConst, UVars;

type

  { TBriefingEditForm }

  TBriefingEditForm = class(TForm)
    BeginDateLabel: TLabel;
    BeginDatePicker: TDateTimePicker;
    ObjectBCButton: TBCButton;
    ObjectLabel: TLabel;
    LastDatePicker: TDateTimePicker;
    LastDateCheckBox: TCheckBox;
    PeriodBCButton: TBCButton;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    EndDateLabel: TLabel;
    EndDatePicker: TDateTimePicker;
    BriefTypeBCButton: TBCButton;
    InfEndDateCheckBox: TCheckBox;
    BriefNameEdit: TEdit;
    BriefNameLabel: TLabel;
    PeriodLabel: TLabel;
    NoteEdit: TEdit;
    NoteLabel: TLabel;
    BriefTypeLabel: TLabel;
    SaveButton: TSpeedButton;
    NumSpinEdit: TSpinEdit;
    VTPanel: TPanel;
    VT: TVirtualStringTree;
    procedure BeginDatePickerChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InfEndDateCheckBoxChange(Sender: TObject);
    procedure LastDateCheckBoxChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    BriefTypeDropDown: TVSTDropDown;
    PeriodDropDown: TVSTDropDown;
    ObjectDropDown: TVSTDropDown;

    ObjectList: TVSTCheckList;
    ObjectItems: TStrVector;
    ObjectIDs: TIntVector;

    procedure ObjectListLoad;
    procedure ObjectListCheck;

    procedure BriefTypeChange;
    procedure BriefObjectChange;

  public
    EditingType: TEditingType;
    BriefID: Integer;
  end;

var
  BriefingEditForm: TBriefingEditForm;

implementation

{$R *.lfm}

{ TBriefingEditForm }

procedure TBriefingEditForm.FormCreate(Sender: TObject);
begin
  BriefID:= -1;

  ObjectList:= TVSTCheckList.Create(VT, EmptyStr, @ObjectListCheck);


  BriefTypeDropDown:= TVSTDropDown.Create(BriefTypeBCButton);
  BriefTypeDropDown.OnChange:= @BriefTypeChange;
  BriefTypeDropDown.Items:= BRIEFTYPE_PICKS;
  BriefTypeDropDown.ItemIndex:= 1;

  PeriodDropDown:= TVSTDropDown.Create(PeriodBCButton);
  PeriodDropDown.Items:= BRIEFPERIOD_PICKS;
  PeriodDropDown.ItemIndex:= 0;

  ObjectDropDown:= TVSTDropDown.Create(ObjectBCButton);
  ObjectDropDown.OnChange:= @BriefObjectChange;
  ObjectDropDown.Items:= BRIEFOBJECT_PICKS;
  ObjectDropDown.ItemIndex:= 1;

  LastDatePicker.Date:= Date;
  BeginDatePicker.Date:= Date;
  EndDatePicker.Date:= Date;
  LastDatePicker.MinDate:= BeginDatePicker.Date;
  EndDatePicker.MinDate:= BeginDatePicker.Date;
end;

procedure TBriefingEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BriefTypeDropDown);
  FreeAndNil(PeriodDropDown);
  FreeAndNil(ObjectDropDown);
  FreeAndNil(ObjectList);
end;

procedure TBriefingEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);

  BriefTypeDropDown.AutoWidth;
  PeriodDropDown.AutoWidth;

  FormKeepMinSize(Self);
  ObjectListLoad;
  BriefNameEdit.SetFocus;
end;

procedure TBriefingEditForm.InfEndDateCheckBoxChange(Sender: TObject);
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

procedure TBriefingEditForm.LastDateCheckBoxChange(Sender: TObject);
begin
  LastDatePicker.Enabled:= LastDateCheckBox.Checked;
end;

procedure TBriefingEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  BriefName, Note: String;
begin
  IsOK:= False;

  BriefName:= STrim(BriefNameEdit.Text);
  if SEmpty(BriefName) then
  begin
    Inform('Не указано наименование!');
    Exit;
  end;

  Note:= STrim(NoteEdit.Text);

  case EditingType of
    etAdd:
      IsOK:= False;
        //DataBase.SIZNormAdd(NormID, NormName, Note,
        //                         BeginDatePicker.Date, EndDatePicker.Date);
    etEdit:
      IsOK:= False;
        //DataBase.SIZNormUpdate(NormID, NormName, Note,
        //                         BeginDatePicker.Date, EndDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TBriefingEditForm.ObjectListLoad;
begin
  case ObjectDropDown.ItemIndex of
    0: ObjectItems:= nil;//ObjectList.ValuesClear;
    1:
      begin
        DataBase.KeyPickList('STAFFPOST', 'PostID', 'PostName', ObjectIDs,
                             ObjectItems, True{ID>0}, 'PostName');

      end;
    2:
      begin

      end;
  end;
  ObjectList.Update(ObjectItems, 0);
end;

procedure TBriefingEditForm.BriefTypeChange;
begin
  PeriodLabel.Visible:= BriefTypeDropDown.ItemIndex=1;
  NumSpinEdit.Visible:= PeriodLabel.Visible;
  PeriodBCButton.Visible:= PeriodLabel.Visible;
end;

procedure TBriefingEditForm.BriefObjectChange;
begin
  ObjectListLoad;
end;

procedure TBriefingEditForm.ObjectListCheck;
begin

end;

procedure TBriefingEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TBriefingEditForm.BeginDatePickerChange(Sender: TObject);
begin
  LastDatePicker.MinDate:= BeginDatePicker.Date;
  EndDatePicker.MinDate:= BeginDatePicker.Date;
end;

end.

