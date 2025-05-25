unit UCalendarEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons, BCButton, DateUtils,
  //DK packages utils
  DK_DateUtils, DK_Vector, DK_Const, DK_VSTDropDown, DK_CtrlUtils,
  //Project utils
  UDataBase, UConst, UCalendar, UTimingUtils, UImages;

type

  { TCalendarEditForm }

  TCalendarEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FirstDatePicker: TDateTimePicker;
    SaveButton: TSpeedButton;
    StatusBCButton: TBCButton;
    SwapDayBCButton: TBCButton;
    SwapDayLabel: TLabel;
    LastDatePicker: TDateTimePicker;
    PeriodLabel: TLabel;
    Label2: TLabel;
    StatusLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    procedure StatusDropDownChange;
  public
    Year: Integer;
    DayDate: TDate;
    StatusDropDown: TVSTDropDown;
    SwapDayDropDown: TVSTDropDown;
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
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self);

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
  Corrections: TCalendarCorrections;
begin
  Corrections:= GetCalendarCorrections(FirstDatePicker.Date, LastDatePicker.Date,
                        StatusDropDown.ItemIndex+1, SwapDayDropDown.ItemIndex);
  if DataBase.CalendarCorrectionsUpdate(Corrections) then
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
  Images.ToButtons([SaveButton, CancelButton]);

  SwapDayDropDown:= TVSTDropDown.Create(SwapDayBCButton);
  SwapDayDropDown.Items:= DAY_NAME_PICKS;
  SwapDayDropDown.ItemIndex:= 0;

  StatusDropDown:= TVSTDropDown.Create(StatusBCButton);
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

