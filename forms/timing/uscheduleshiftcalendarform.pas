unit UScheduleShiftCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BCButton, Spin, StdCtrls, VirtualTrees, fpspreadsheetgrid, DateUtils,

  //DK packages utils
  DK_Vector, DK_Fonts, DK_Const,
  DK_VSTEditTools, DK_VSTTableTools, DK_Zoom, DK_SheetExporter,
  //Project utils
  UConst, UUtils, UCalendar, USchedule, UScheduleShiftSheet;

type

  { TScheduleShiftCalendarForm }

  TScheduleShiftCalendarForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CloseButton: TSpeedButton;
    Label1: TLabel;
    ParamVT: TVirtualStringTree;
    ExportButton: TBCButton;
    LeftPanel: TPanel;
    LeftSplitter: TSplitter;
    SettingButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ColorVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
  private
    ZoomPercent: Integer;

    ColorNames: TStrVector;
    ColorValues: TColorVector;
    ColorList: TVSTColorList;
    ParamList: TVSTCheckList;

    PrevShiftNumber: Integer;
    Calendar: TCalendar;
    Schedule: TShiftSchedule;
    Sheet: TShiftScheduleCalendarSheet;

    procedure PrevShiftNumberLoad;
    procedure ScheduleLoad;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleRefresh;
  public
    ScheduleID: Integer;
    ScheduleName: String;
  end;

var
  ScheduleShiftCalendarForm: TScheduleShiftCalendarForm;

  procedure ScheduleShiftCalendarFormCreate(const AYear, AScheduleID: Integer;
                                            const AScheduleName: String);

implementation

uses UMainForm;

{$R *.lfm}

procedure ScheduleShiftCalendarFormCreate(const AYear, AScheduleID: Integer; const AScheduleName: String);
var
  Frm: TScheduleShiftCalendarForm;
begin
  Frm:= TScheduleShiftCalendarForm.Create(nil);
  try
    Frm.Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[10] + ' - [' + AScheduleName + ']';
    Frm.YearSpinEdit.Value:= AYear;
    Frm.ScheduleID:= AScheduleID;
    Frm.ScheduleName:= AScheduleName;
    Frm.ShowModal;
  finally
    FreeAndNil(Frm);
  end;
end;

{ TScheduleShiftCalendarForm }

procedure TScheduleShiftCalendarForm.FormCreate(Sender: TObject);
var
  V: TStrVector;
begin
  Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CloseButton, SettingButton
  ]);

  SetCategoryButtons([
    ExportButton
  ]);

  ColorList:= TVSTColorList.Create(ColorVT);

  V:= VCreateStr([
    'Учитывать корректировки графика',
    'Выделять цветом второй день смены'
  ]);
  ParamList:= TVSTCheckList.Create(ParamVT, VIEW_PARAMS_CAPTION, V, @ScheduleRedraw);

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  Calendar:= TCalendar.Create;
  Schedule:= TShiftSchedule.Create;
  Sheet:= TShiftScheduleCalendarSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
end;

procedure TScheduleShiftCalendarForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ColorList);
  FreeAndNil(ParamList);
  FreeAndNil(Calendar);
  FreeAndNil(Schedule);
  FreeAndNil(Sheet);
end;

procedure TScheduleShiftCalendarForm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  ColorValues:= VCreateColor(COLORS_SHIFT);
  VDim(ColorNames, Length(ColorValues));
  ColorNames[0]:= 'Нерабочий день';
  for i:= 1 to High(ColorNames) do
    ColorNames[i]:= 'Смена №' + IntToStr(i);
  ColorList.Update(ColorNames, ColorValues);
  ScheduleRefresh;
end;

procedure TScheduleShiftCalendarForm.SettingButtonClick(Sender: TObject);
begin
  if SettingButton.Down then
  begin
    LeftPanel.Visible:= True;
    LeftSplitter.Visible:= True;
    SheetPanel.AnchorToNeighbour(akLeft, 0, LeftSplitter);
  end
  else begin
    LeftSplitter.Visible:= False;
    LeftPanel.Visible:= False;
    SheetPanel.AnchorToNeighbour(akLeft, 2, Self);
  end;
end;

procedure TScheduleShiftCalendarForm.PrevShiftNumberLoad;
var
  PrevSched: TShiftSchedule;
  PrevCal: TCalendar;
  PrevDate: TDate;
begin
  //определяем номер смены в последний день предыдущего года
  PrevShiftNumber:= 0;
  PrevDate:= IncDay(Calendar.BeginDate, -1);
  PrevCal:= TCalendar.Create;
  try
    CalendarForPeriod(PrevDate, PrevDate, PrevCal);
    PrevSched:= TShiftSchedule.Create;
    try
      ScheduleShiftByCalendar(ScheduleID, PrevCal, PrevSched);
      if ParamList.Checked[0] then
        PrevShiftNumber:= PrevSched.ShiftNumbersCorrect[0]
      else
        PrevShiftNumber:= PrevSched.ShiftNumbersDefault[0];
    finally
      FreeAndNil(PrevSched);
    end;
  finally
    FreeAndNil(PrevCal);
  end;
end;

procedure TScheduleShiftCalendarForm.ScheduleLoad;
begin
  CalendarForYear(YearSpinEdit.Value, Calendar);
  PrevShiftNumberLoad;
  ScheduleShiftByCalendar(ScheduleID, Calendar, Schedule);
end;

procedure TScheduleShiftCalendarForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(Calendar, Schedule, PrevShiftNumber, ScheduleName,
               True{NeedCorrect}, True{FirstShiftDayColorOnly});
    //if not VIsNil(Colors) then
    //  Sheet.ColorsUpdate(Colors);
  finally
    ViewGrid.Visible:= True;
  end;
end;

procedure TScheduleShiftCalendarForm.ScheduleRedraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TScheduleShiftCalendarForm.ScheduleRefresh;
begin
  ScheduleLoad;
  ScheduleRedraw
end;

procedure TScheduleShiftCalendarForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

