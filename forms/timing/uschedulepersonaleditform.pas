unit USchedulePersonalEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fpspreadsheetgrid, DateTimePicker, DateUtils, Buttons,
  //Project utils
  UDataBase, UTimingUtils, UImages, UTypes, UCalendar, USchedule,  UScheduleSheet,
  //DK packages utils
  DK_Vector, DK_Dialogs, DK_CtrlUtils;

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
    procedure CancelButtonClick(Sender: TObject);
    procedure FirstDatePickerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    ScheduleIDs: TIntVector;
    ScheduleNames: TStrVector;
    Calendar: TCalendar;
    Schedules: TShiftScheduleVector;
    Sheet: TShiftSimpleScheduleSheet;

    procedure SchedulesUpdate;
  public
    EditingType: TEditingType;
    TabNumID, ScheduleID, HistoryID, PrevHistoryID: Integer;
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
  FormKeepMinSize(Self, False);

  SchedulesUpdate;
  Sheet.SelectedIndex:= VIndexOf(ScheduleIDs, ScheduleID);
end;

procedure TSchedulePersonalEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
begin
  IsOK:= False;

  if not Sheet.IsSelected then
  begin
    ShowInfo('Не указан график сменности!');
    Exit;
  end;

  case EditingType of
    etAdd:  //превод
      IsOK:= DataBase.StaffScheduleHistoryAdd(HistoryID, TabNumID,
                 ScheduleIDs[Sheet.SelectedIndex], FirstDatePicker.Date);
    etEdit:
      IsOK:= DataBase.StaffScheduleHistoryUpdate(PrevHistoryID, HistoryID,
                 ScheduleIDs[Sheet.SelectedIndex], FirstDatePicker.Date);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSchedulePersonalEditForm.FirstDatePickerChange(Sender: TObject);
begin
  SchedulesUpdate;
end;

procedure TSchedulePersonalEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSchedulePersonalEditForm.FormCreate(Sender: TObject);
var
  V: TIntVector;
begin
  PrevHistoryID:= -1;
  Calendar:= TCalendar.Create;

  Images.ToButtons([SaveButton, CancelButton]);

  DataBase.ScheduleMainListLoad(ScheduleIDs, V, V, ScheduleNames);
  Sheet:= TShiftSimpleScheduleSheet.Create(ViewGrid, MainForm.GridFont, ScheduleNames);
end;

procedure TSchedulePersonalEditForm.SchedulesUpdate;
begin
  CalendarForPeriod(FirstDatePicker.Date, IncDay(FirstDatePicker.Date, 9), Calendar);
  ScheduleShiftVectorByCalendar(ScheduleIDs, Calendar, Schedules);
  Sheet.Draw(Schedules);
end;

end.

