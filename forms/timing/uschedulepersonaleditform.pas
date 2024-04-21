unit USchedulePersonalEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fpspreadsheetgrid, DateTimePicker, DateUtils, Buttons,
  //Project utils
  UDataBase, UUtils, UCalendar, USchedule,  UScheduleShiftSheet,
  //DK packages utils
  DK_Vector;

type

  { TSchedulePersonalEditForm }

  TSchedulePersonalEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FirstDatePicker: TDateTimePicker;
    FirstDateLabel: TLabel;
    SaveButton: TSpeedButton;
    SheetPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ScheduleIDs: TIntVector;
    ScheduleNames: TStrVector;
    Calendar: TCalendar;
    Schedules: TShiftScheduleVector;
    Sheet: TShiftScheduleSimpleSheet;

    procedure FirstDateChange;
  public

  end;

var
  SchedulePersonalEditForm: TSchedulePersonalEditForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSchedulePersonalEditForm }

procedure TSchedulePersonalEditForm.FormDestroy(Sender: TObject);
begin
  VSDel(Schedules);
  FreeAndNil(Calendar);
  FreeAndNil(Sheet);
end;

procedure TSchedulePersonalEditForm.FormShow(Sender: TObject);
begin
  FirstDatePicker.Date:= Date;
  FirstDateChange;
end;

procedure TSchedulePersonalEditForm.FirstDatePickerChange(Sender: TObject);
begin
  FirstDateChange;
end;

procedure TSchedulePersonalEditForm.FormCreate(Sender: TObject);
var
  V: TIntVector;
begin
  Calendar:= TCalendar.Create;
  DataBase.ScheduleMainListLoad(ScheduleIDs, V, V, ScheduleNames);
  Sheet:= TShiftScheduleSimpleSheet.Create(ViewGrid, MainForm.GridFont, ScheduleNames);
end;

procedure TSchedulePersonalEditForm.FirstDateChange;
begin
  CalendarForPeriod(FirstDatePicker.Date, IncDay(FirstDatePicker.Date, 9), Calendar);
  ScheduleShiftVectorByCalendar(ScheduleIDs, Calendar, Schedules);
  Sheet.Draw(Schedules);
end;

end.

