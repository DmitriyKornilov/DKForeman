unit UScheduleShiftEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, BCButton, VirtualTrees, DateTimePicker, Buttons,
  //DK packages utils
  DK_Vector, DK_VSTDropDown, DK_VSTEdit, DK_Const, DK_StrUtils, DK_Dialogs,
  DK_CtrlUtils,
  //Project utils
  UVars, UWorkHours, USchedule, UTimingUtils, UConst;

type

  { TScheduleShiftEditForm }

  TScheduleShiftEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FirstDatePicker: TDateTimePicker;
    CycleCountLabel: TLabel;
    SaveButton: TSpeedButton;
    StrucutureLabel: TLabel;
    TypeBCButton: TBCButton;
    VT: TVirtualStringTree;
    WeekHoursLabel: TLabel;
    FirstDateLabel: TLabel;
    TypeLabel: TLabel;
    NameEdit: TEdit;
    NameLabel: TLabel;
    WeekHoursSpinEdit: TSpinEdit;
    CycleCountSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure CycleCountSpinEditChange(Sender: TObject);
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    TypeDropDown: TVSTDropDown;
    Structure: TVSTEdit;
    KeyMarks: TIntVector;
    PickMarks: TStrVector;
    procedure TypeDropDownChange;
  public
    Cycle: TScheduleCycle;
    FirstDate: TDate;
  end;

var
  ScheduleShiftEditForm: TScheduleShiftEditForm;

implementation

{$R *.lfm}

{ TScheduleShiftEditForm }

procedure TScheduleShiftEditForm.FormCreate(Sender: TObject);
begin
  Cycle:= EmptyScheduleCycle;

  DataBase.TimetableMarkListLoad(KeyMarks, PickMarks, True{ DigMark>0});

  Structure:= TVSTEdit.Create(VT);
  Structure.SetSingleFont(GridFont);
  Structure.IsShowZeros:= True;
  Structure.HeaderFont.Style:= [fsBold];
  Structure.AddColumnRowTitles(SCHEDULE_CORRECTION_COLUMN_NAMES[0],
                               SCHEDULE_CORRECTION_COLUMN_WIDTHS[0]);

  Structure.AddColumnIntegerRange(SCHEDULE_CORRECTION_COLUMN_NAMES[1], 0, 366,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[1]);
  Structure.AddColumnDoubleRange(SCHEDULE_CORRECTION_COLUMN_NAMES[2], 0, 24,
                            FRACTION_DIGITS_IN_WORKHOURS,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[2]);
  Structure.AddColumnDoubleRange(SCHEDULE_CORRECTION_COLUMN_NAMES[3], 0, 24,
                            FRACTION_DIGITS_IN_WORKHOURS,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[3]);
  Structure.AddColumnKeyPick(SCHEDULE_CORRECTION_COLUMN_NAMES[4], KeyMarks, PickMarks,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[4],
                             taCenter, taLeftJustify);
  Structure.Draw;

  TypeDropDown:= TVSTDropDown.Create(TypeBCButton);
  TypeDropDown.OnChange:= @TypeDropDownChange;
  TypeDropDown.Items:= VCreateStr(['недельный', 'цикловой']);
end;

procedure TScheduleShiftEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TypeDropDown);
  FreeAndNil(Structure);
end;

procedure TScheduleShiftEditForm.FormShow(Sender: TObject);
begin
  Width:= Width + 5; //fix datetimepicker button size bug

  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self, False);

  FirstDatePicker.Date:= FirstDate;
  if not Cycle.IsWeek then
    CycleCountSpinEdit.Value:= Cycle.Count;
  TypeDropDown.ItemIndex:= Ord(not Cycle.IsWeek);
end;

procedure TScheduleShiftEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TScheduleShiftEditForm.CycleCountSpinEditChange(Sender: TObject);
begin
  ScheduleCycleToCount(Cycle, CycleCountSpinEdit.Value, FirstDatePicker.Date);
  ScheduleCycleDraw(Structure, Cycle);
end;

procedure TScheduleShiftEditForm.FirstDatePickerChange(Sender: TObject);
begin
  ScheduleCycleToCount(Cycle, CycleCountSpinEdit.Value, FirstDatePicker.Date);
  ScheduleCycleDraw(Structure, Cycle);
end;

procedure TScheduleShiftEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  V: TDblVector;
  ScheduleName: String;
begin
  IsOK:= False;

  ScheduleName:= STrim(NameEdit.Text);
  if SEmpty(ScheduleName) then
  begin
    Inform('Не указано наименование графика!');
    Exit;
  end;

  if DataBase.ScheduleShiftIsExists(Cycle.ScheduleID, ScheduleName) then
  begin
    Inform('График с наименованием "' + ScheduleName + '" уже существует!');
    Exit;
  end;

  if Structure.IsEditing then
    Structure.UnSelect(True);

  Structure.ColumnAsInteger(Cycle.ShiftNums, SCHEDULE_CORRECTION_COLUMN_NAMES[1]);
  Structure.ColumnAsDouble(V, SCHEDULE_CORRECTION_COLUMN_NAMES[2]);
  Cycle.HoursTotal:= VWorkHoursFracToInt(V);
  Structure.ColumnAsDouble(V, SCHEDULE_CORRECTION_COLUMN_NAMES[3]);
  Cycle.HoursNight:= VWorkHoursFracToInt(V);
  Structure.ColumnAsInteger(Cycle.DigMarks, SCHEDULE_CORRECTION_COLUMN_NAMES[4]);

  if Cycle.ScheduleID=0 then
    IsOK:= DataBase.ScheduleShiftAdd(ScheduleName, WeekHoursSpinEdit.Value, Cycle)
  else
    IsOK:= DataBase.ScheduleShiftUpdate(ScheduleName, WeekHoursSpinEdit.Value, Cycle);

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TScheduleShiftEditForm.TypeDropDownChange;
begin
  CycleCountLabel.Visible:= TypeDropDown.ItemIndex=1;
  CycleCountSpinEdit.Visible:= TypeDropDown.ItemIndex=1;
  FirstDateLabel.Visible:= TypeDropDown.ItemIndex=1;
  FirstDatePicker.Visible:= TypeDropDown.ItemIndex=1;

  if TypeDropDown.ItemIndex=0 then  //недельный
  begin
    FirstDatePicker.Date:= MONDAY_DATE;
    ScheduleCycleToWeek(Cycle);
  end
  else begin//цикловой
    FirstDatePicker.Date:= FirstDate;
    ScheduleCycleToCount(Cycle, CycleCountSpinEdit.Value, FirstDatePicker.Date);
  end;
  ScheduleCycleDraw(Structure, Cycle);
end;

end.

