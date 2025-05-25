unit UScheduleCorrectionEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, VirtualTrees, DateTimePicker, Buttons, BCButton, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils, DK_VSTDropDown, DK_Dialogs, DK_Const, DK_CtrlUtils,
  //Project utils
  UDataBase, UImages, UWorkHours, USchedule;

type

  { TScheduleCorrectionEditForm }

  TScheduleCorrectionEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    MarkBCButton: TBCButton;
    LastDateCheckBox: TCheckBox;
    FirstDatePicker: TDateTimePicker;
    LastDatePicker: TDateTimePicker;
    SaveButton: TSpeedButton;
    TotalHoursSpinEdit: TFloatSpinEdit;
    NightHoursSpinEdit: TFloatSpinEdit;
    FirstDateLabel: TLabel;
    TotalHoursLabel: TLabel;
    NightHoursLabel: TLabel;
    MarkLabel: TLabel;
    ShiftNumLabel: TLabel;
    ShiftNumSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastDateCheckBoxChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    DigMarks: TIntVector;
    MarkDropDown: TVSTDropDown;
  public
    Year: Integer;
    DigMark, ScheduleID, TabNumID: Integer;
  end;

var
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;

implementation

{$R *.lfm}

{ TScheduleCorrectionEditForm }

procedure TScheduleCorrectionEditForm.FormCreate(Sender: TObject);
begin
  DigMark:= -1;
  ScheduleID:= -1;
  TabNumID:= -1;

  Images.ToButtons([SaveButton, CancelButton]);

  MarkDropDown:= TVSTDropDown.Create(MarkBCButton);
  ShiftNumSpinEdit.MaxValue:= 365;
end;

procedure TScheduleCorrectionEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MarkDropDown);
end;

procedure TScheduleCorrectionEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TScheduleCorrectionEditForm.FirstDatePickerChange(Sender: TObject);
begin
  LastDatePicker.MinDate:= FirstDatePicker.Date;
end;

procedure TScheduleCorrectionEditForm.FormShow(Sender: TObject);
begin
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self);

  if SameDate(FirstDatePicker.Date, NULDATE) then
    FirstDatePicker.Date:= FirstDayInYear(Year);
  LastDatePicker.Date:= FirstDatePicker.Date;
  FirstDatePicker.MinDate:= FirstDayInYear(FirstDatePicker.Date);
  FirstDatePicker.MaxDate:= LastDayInYear(FirstDatePicker.Date);
  LastDatePicker.MinDate:= FirstDatePicker.Date;
  LastDatePicker.MaxDate:= FirstDatePicker.MaxDate;
  DataBase.TimetableMarkDictionaryLoad(MarkDropDown, DigMarks, DigMark);
end;

procedure TScheduleCorrectionEditForm.LastDateCheckBoxChange(Sender: TObject);
begin
  if not LastDateCheckBox.Checked then
    LastDatePicker.Date:= FirstDatePicker.Date;
  LastDatePicker.Enabled:= LastDateCheckBox.Checked;
end;

procedure TScheduleCorrectionEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  Corrections: TScheduleCorrections;
  HoursTotal, HoursNight, ShiftNum: Integer;
begin
  IsOK:= False;

  if MarkDropDown.ItemIndex<0 then
  begin
    Inform('Не указан основной код табеля!');
    Exit;
  end;

  HoursTotal:= WorkHoursFracToInt(TotalHoursSpinEdit.Value);
  HoursNight:= WorkHoursFracToInt(NightHoursSpinEdit.Value);
  ShiftNum:= ShiftNumSpinEdit.Value;

  if (HoursTotal>0) and (ShiftNum=0) then
    if not Confirm('Указано количество рабочих часов, но не указан номер смены (=0)! ' +
             'Всё равно записать изменения?') then Exit;

  Corrections:= ScheduleCorrectionsCreate(FirstDatePicker.Date, LastDatePicker.Date,
                  HoursTotal, HoursNight, DigMarks[MarkDropDown.ItemIndex], ShiftNum);

  if TabNumID>0 then //pesonal schedule corrections
    IsOK:= DataBase.SchedulePersonalCorrectionsUpdate(TabNumID, Corrections)
  else if ScheduleID>0 then //shift schedule corrections
    IsOK:= DataBase.ScheduleShiftCorrectionsUpdate(ScheduleID, Corrections);

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

