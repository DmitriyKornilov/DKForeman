unit UCalendarEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons, BCButton, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_Vector, DK_Const, DK_DropDown,
  //Project utils
  UDataBase, UConst;

type

  { TCalendarEditForm }

  TCalendarEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FirstDatePicker: TDateTimePicker;
    StatusBCButton: TBCButton;
    SwapDayBCButton: TBCButton;
    SwapDayLabel: TLabel;
    LastDatePicker: TDateTimePicker;
    PeriodLabel: TLabel;
    Label2: TLabel;
    StatusLabel: TLabel;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    StatusDropDown: TDropDown;
    SwapDayDropDown: TDropDown;
    procedure StatusDropDownChange;
  public
    Year: Word;
    DayDate: TDate;
  end;

var
  CalendarEditForm: TCalendarEditForm;

implementation

{$R *.lfm}

{ TCalendarEditForm }

procedure TCalendarEditForm.StatusDropDownChange;
begin
  if (SwapDayDropDown.ItemIndex>0) and (StatusDropDown.ItemIndex<>3) then
    SwapDayDropDown.ItemIndex:=0;
  SwapDayDropDown.Enabled:= StatusDropDown.ItemIndex=3;
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
  Status:= StatusDropDown.ItemIndex+1;
  SwapDay:= SwapDayDropDown.ItemIndex; //Ord(SwapDayComboBox.Enabled)*(SwapDayComboBox.ItemIndex);
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

procedure TCalendarEditForm.FormCreate(Sender: TObject);
begin
  SwapDayDropDown:= TDropDown.Create(SwapDayBCButton);
  SwapDayDropDown.Items:= DAY_NAME_PICKS;
  SwapDayDropDown.ItemIndex:= 0;

  StatusDropDown:= TDropDown.Create(StatusBCButton);
  StatusDropDown.OnChange:= @StatusDropDownChange;
  StatusDropDown.Items:= VCut(DAY_STATUS_PICKS, 1);
  StatusDropDown.ItemIndex:= 0;
end;

procedure TCalendarEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StatusDropDown);
  FreeAndNil(SwapDayDropDown);
end;

end.

