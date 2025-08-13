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
  UTypes, UConst, UVars, UUtils;

type

  { TBriefingEditForm }

  TBriefingEditForm = class(TForm)
    PeriodDatesLabel: TLabel;
    BeginDatePicker: TDateTimePicker;
    EndDateLabel: TLabel;
    EndDatePicker: TDateTimePicker;
    InfEndDateCheckBox: TCheckBox;
    LastDateLabel: TLabel;
    LastDateCheckBox: TCheckBox;
    ObjectBCButton: TBCButton;
    ObjectLabel: TLabel;
    LastDatePicker: TDateTimePicker;
    LastDatePanel: TPanel;
    PeriodPanel: TPanel;
    PeriodBCButton: TBCButton;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    BriefTypeBCButton: TBCButton;
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
    ObjectNames: TStrVector;
    ObjectIDs: TIntVector;

    procedure ObjectListLoad;

    procedure BriefTypeChange;
    procedure BriefObjectChange;

    procedure OldDataLoad;
  public
    EditingType: TEditingType;
    BriefID, OldObject, OldPeriod: Integer;
    OldObjectIDs: TIntVector;
    OldObjectNames: TStrVector;
  end;

var
  BriefingEditForm: TBriefingEditForm;

implementation

{$R *.lfm}

{ TBriefingEditForm }

procedure TBriefingEditForm.FormCreate(Sender: TObject);
begin
  BriefID:= -1;
  OldPeriod:= 1;
  OldObject:= 1;
  OldObjectIDs:=nil;

  ObjectList:= TVSTCheckList.Create(VT, EmptyStr, nil);

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
  OldDataLoad;
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
  Period, Num: Integer;
  SelectedObjectIDs: TIntVector;
  BriefName, Note: String;
  BeginDate, EndDate, LastDate: TDate;
begin
  IsOK:= False;

  BriefName:= STrim(BriefNameEdit.Text);
  if SEmpty(BriefName) then
  begin
    Inform('Не указано наименование!');
    Exit;
  end;

  Note:= STrim(NoteEdit.Text);

  SelectedObjectIDs:= nil;
  if ObjectDropDown.ItemIndex>0 then
    SelectedObjectIDs:= VCut(ObjectIDs, ObjectList.Checkeds);

  if BriefTypeDropDown.ItemIndex=0 then //разово
  begin
    LastDate:= LastDatePicker.Date;
    BeginDate:= LastDate;
    EndDate:= LastDate;
    Period:= 0;
    Num:= 0;
  end
  else begin //периодически
    BeginDate:= BeginDatePicker.Date;
    EndDate:= EndDatePicker.Date;
    LastDate:= 0;
    if LastDateCheckBox.Checked then
      LastDate:= LastDatePicker.Date;
    Period:= PeriodDropDown.ItemIndex + 1;
    Num:= NumSpinEdit.Value;
  end;

  case EditingType of
    etAdd, etCustom:
      IsOK:= DataBase.BriefingAdd(BriefID, BriefName, Note,
                                  BeginDate, EndDate, LastDate,
                                  ObjectDropDown.ItemIndex, Period, Num,
                                  SelectedObjectIDs);

    etEdit:
      IsOK:= DataBase.BriefingUpdate(BriefID, BriefName, Note,
                                  BeginDate, EndDate, LastDate,
                                  OldObject, ObjectDropDown.ItemIndex, Period, Num,
                                  SelectedObjectIDs);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TBriefingEditForm.ObjectListLoad;
var
  Fs, Ns, Ps, TabNums: TStrVector;
begin
  case ObjectDropDown.ItemIndex of
    0: ObjectNames:= nil;
    1:
      begin
        DataBase.KeyPickList('STAFFPOST', 'PostID', 'PostName', ObjectIDs,
                             ObjectNames, True{ID>0}, 'PostName');

      end;
    2:
      begin
        DataBase.StaffListLoad(BeginDatePicker.Date, 0{сорт по ФИО}, 1{работающие}, ObjectIDs,
                               Fs, Ns, Ps, TabNums);
        ObjectNames:= StaffFullName(Fs, Ns, Ps, TabNums, False{long});
      end;
  end;
  ObjectList.Update(ObjectNames);
end;

procedure TBriefingEditForm.BriefTypeChange;
begin
  PeriodLabel.Visible:= BriefTypeDropDown.ItemIndex=1;
  NumSpinEdit.Visible:= PeriodLabel.Visible;
  PeriodBCButton.Visible:= PeriodLabel.Visible;
  LastDateCheckBox.Visible:= BriefTypeDropDown.ItemIndex=1;
  LastDateLabel.Visible:= BriefTypeDropDown.ItemIndex=0;
  PeriodPanel.Visible:= BriefTypeDropDown.ItemIndex=1;
  LastDatePicker.Enabled:= (BriefTypeDropDown.ItemIndex=0) or
                           ((BriefTypeDropDown.ItemIndex=1) and LastDateCheckBox.Checked);
end;

procedure TBriefingEditForm.BriefObjectChange;
begin
  VTPanel.Visible:= ObjectDropDown.ItemIndex>0;
  ObjectListLoad;
end;

procedure TBriefingEditForm.OldDataLoad;
var
  i, n: Integer;
begin
  if EditingType=etAdd then Exit;

  BriefTypeDropDown.ItemIndex:= Ord(OldPeriod>0);
  if OldPeriod>0 then
    PeriodDropDown.ItemIndex:= OldPeriod - 1;
  ObjectDropDown.ItemIndex:= OldObject;

  if (OldObject=0) or VIsNil(OldObjectIDs) then Exit;

  for i:= 0 to High(OldObjectIDs) do
  begin
    n:= VIndexOf(ObjectIDs, OldObjectIDs[i]);
    if n<0 then
    begin
      n:= VIndexOfAsc(ObjectNames, OldObjectNames[i]);
      VIns(ObjectNames, n, OldObjectNames[i]);
      VIns(ObjectIDs, n, OldObjectIDs[i]);
    end;

    ObjectList.Checked[n]:= True;
  end;
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

