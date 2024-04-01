unit UCalendarEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_Vector, DK_Const,
  //Project utils
  UDataBase;

type

  { TCalendarEditForm }

  TCalendarEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    StatusComboBox: TComboBox;
    SwapDayComboBox: TComboBox;
    FirstDatePicker: TDateTimePicker;
    SwapDayLabel: TLabel;
    LastDatePicker: TDateTimePicker;
    PeriodLabel: TLabel;
    Label2: TLabel;
    StatusLabel: TLabel;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure StatusComboBoxChange(Sender: TObject);
  private

  public
    Year: Word;
    DayDate: TDate;
  end;

var
  CalendarEditForm: TCalendarEditForm;

implementation

{$R *.lfm}

{ TCalendarEditForm }

procedure TCalendarEditForm.StatusComboBoxChange(Sender: TObject);
begin
  if (SwapDayComboBox.ItemIndex>0) and (StatusComboBox.ItemIndex<>3) then
    SwapDayComboBox.ItemIndex:=0;
  SwapDayComboBox.Enabled:= StatusComboBox.ItemIndex=3;
end;

procedure TCalendarEditForm.FormShow(Sender: TObject);
begin
  FirstDatePicker.MinDate:= FirstDayInYear(Year);
  FirstDatePicker.MaxDate:= LastDayInYear(Year);
  if SameDate(DayDate, NULDATE) then //новый
    FirstDatePicker.Date:= FirstDatePicker.MinDate
  else begin //редактирование
    FirstDatePicker.Date:= DayDate;
    FirstDatePicker.Enabled:= False;
  end;
  LastDatePicker.Date:= FirstDatePicker.Date;
  LastDatePicker.MinDate:= FirstDatePicker.Date;
  LastDatePicker.MaxDate:= FirstDatePicker.MaxDate;
end;

procedure TCalendarEditForm.SaveButtonClick(Sender: TObject);
var
  i, n, Status, SwapDay: Integer;
  Dates: TDateVector;
begin
  Status:= StatusComboBox.ItemIndex+1;
  SwapDay:= SwapDayComboBox.ItemIndex; //Ord(SwapDayComboBox.Enabled)*(SwapDayComboBox.ItemIndex);
  n:= DaysBetweenDates(FirstDatePicker.Date, LastDatePicker.Date);
  Dates:= nil;
  for i:= 0 to n do
    VAppend(Dates, IncDay(FirstDatePicker.Date, i));
  if DataBase.CalendarCorrectionsUpdate(Dates, Status, SwapDay) then
    ModalResult:= mrOK;
end;

procedure TCalendarEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TCalendarEditForm.FirstDatePickerChange(Sender: TObject);
begin
  LastDatePicker.MinDate:= FirstDatePicker.Date;
  LastDatePicker.Date:= FirstDatePicker.Date;
end;

end.

