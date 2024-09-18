unit UVacationPlanEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, DateTimePicker, Buttons, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_Dialogs, DK_CtrlUtils,
  //Project utils
  UDataBase, UImages;

type

  { TVacationPlanEditForm }

  TVacationPlanEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    Plan2CountAddSpinEdit: TSpinEdit;
    Plan2CountAddLabel: TLabel;
    Plan1CountLabel: TLabel;
    Plan1CountAddLabel: TLabel;
    Plan2CountSpinEdit: TSpinEdit;
    Plan2CountLabel: TLabel;
    Plan1CountAddSpinEdit: TSpinEdit;
    Plan2CheckBox: TCheckBox;
    Plan1Label: TLabel;
    Plan2Label: TLabel;
    Plan1DateLabel: TLabel;
    Plan2DatePicker: TDateTimePicker;
    Plan1DatePicker: TDateTimePicker;
    Plan1CountSpinEdit: TSpinEdit;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Plan2CheckBoxChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public
    TabNumID, YearNum: Integer;
  end;

var
  VacationPlanEditForm: TVacationPlanEditForm;

  function VacationPlanEditFormShow(const AYear, ATabNumID: Integer;
              const APlan1Date, APlan2Date: TDate;
              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Integer;

implementation

function VacationPlanEditFormShow(const AYear, ATabNumID: Integer;
            const APlan1Date, APlan2Date: TDate;
            const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Integer;
var
  Form: TVacationPlanEditForm;
begin
  Form:= TVacationPlanEditForm.Create(nil);
  try
    Form.YearNum:= AYear;
    Form.TabNumID:= ATabNumID;

    if APlan1Date>0 then
      Form.Plan1DatePicker.Date:= APlan1Date;
    if APlan1Count>0 then
      Form.Plan1CountSpinEdit.Value:= APlan1Count;
    if APlan1AddCount>0 then
      Form.Plan1CountAddSpinEdit.Value:= APlan1AddCount;

    if (APlan2Date>0) and
       (APlan2Count+APlan2AddCount>0) then
    begin
      Form.Plan2CheckBox.Checked:= True;
      Form.Plan2DatePicker.Date:= APlan2Date;
      if APlan2Count>0 then
        Form.Plan2CountSpinEdit.Value:= APlan2Count;
      if APlan2AddCount>0 then
       Form.Plan2CountAddSpinEdit.Value:= APlan2AddCount;
    end;

    Result:= Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TVacationPlanEditForm }

procedure TVacationPlanEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  Plan1FirstDate, Plan2FirstDate, ED1, ED2: TDate;
  Plan1Count, Plan1AddCount, Plan2Count, Plan2AddCount: Integer;

  function EndDate(const ABeginDate: TDate; const ADaysCount: Integer): TDate;
  var
    HolidaysCount: Integer;
  begin
    Result:= IncDay(ABeginDate, ADaysCount-1);
    HolidaysCount:= DataBase.HolidaysCount(ABeginDate, Result);
    if HolidaysCount>0 then
      Result:= IncDay(Result, HolidaysCount);
  end;

begin
  IsOK:= False;

  Plan1FirstDate:= Plan1DatePicker.Date;
  Plan1Count:= Plan1CountSpinEdit.Value;
  Plan1AddCount:= Plan1CountAddSpinEdit.Value;
  ED1:= EndDate(Plan1FirstDate, Plan1Count+Plan1AddCount);

  Plan2FirstDate:= 0;
  Plan2Count:= 0;
  Plan2AddCount:= 0;
  if Plan2CheckBox.Checked then
  begin
    Plan2FirstDate:= Plan2DatePicker.Date;
    Plan2Count:= Plan2CountSpinEdit.Value;
    Plan2AddCount:= Plan2CountAddSpinEdit.Value;
    ED2:= EndDate(Plan2FirstDate, Plan2Count+Plan2AddCount);
    if IsPeriodIntersect(Plan1FirstDate, ED1, Plan2FirstDate, ED2) then
    begin
      ShowInfo('Периоды частей отпуска не должны пересекаться!');
      Exit;
    end;
  end;

  IsOK:= DataBase.VacationPlanEdit(YearNum, TabNumID, Plan1FirstDate, Plan2FirstDate,
                            Plan1Count, Plan1AddCount, Plan2Count, Plan2AddCount);

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TVacationPlanEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TVacationPlanEditForm.FormCreate(Sender: TObject);
begin
  TabNumID:= -1;
  Images.ToButtons([SaveButton, CancelButton]);
end;

procedure TVacationPlanEditForm.FormShow(Sender: TObject);
var
  BD, ED: TDate;
begin
  FormKeepMinSize(Self);

  FirstLastDayInYear(YearNum, BD, ED);
  Plan1DatePicker.MinDate:= BD;
  Plan1DatePicker.MaxDate:= ED;
  Plan2DatePicker.MinDate:= BD;
  Plan2DatePicker.MaxDate:= ED;
end;

procedure TVacationPlanEditForm.Plan2CheckBoxChange(Sender: TObject);
begin
  Plan2DatePicker.Enabled:= Plan2CheckBox.Checked;
  Plan2CountSpinEdit.Enabled:= Plan2CheckBox.Checked;
  Plan2CountAddSpinEdit.Enabled:= Plan2CheckBox.Checked;
  Plan2Label.Enabled:= Plan2CheckBox.Checked;
  Plan2CountLabel.Enabled:= Plan2CheckBox.Checked;
  Plan2CountAddLabel.Enabled:= Plan2CheckBox.Checked;
end;

end.

