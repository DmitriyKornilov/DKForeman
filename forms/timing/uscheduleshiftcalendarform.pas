unit UScheduleShiftCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BCButton, BCPanel, Spin, StdCtrls, VirtualTrees, fpspreadsheetgrid, DateUtils,

  //DK packages utils
  DK_Vector, DK_Fonts, DK_Const, DK_Graph,
  DK_VSTTypes, DK_VSTEditTools, DK_Zoom, DK_SheetExporter,
  //Project utils
  UConst, UUtils, UCalendar, USchedule, UScheduleShiftSheet;

type

  { TScheduleShiftCalendarForm }

  TScheduleShiftCalendarForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ColorCaptionPanel: TBCPanel;
    ListToolPanel: TPanel;
    NeedCorrectionsCheckBox: TCheckBox;
    FirstShiftDayColorOnlyCheckBox: TCheckBox;
    CloseButton: TSpeedButton;
    ExportButton: TBCButton;
    LeftPanel: TPanel;
    LeftSplitter: TSplitter;
    AddButton: TSpeedButton;
    DelButton: TSpeedButton;
    EditButton: TSpeedButton;
    SettingButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ColorVT: TVirtualStringTree;
    ViewGrid: TsWorksheetGrid;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FirstShiftDayColorOnlyCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NeedCorrectionsCheckBoxChange(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
  private
    ZoomPercent: Integer;

    ColorNames: TStrVector;
    ColorValues: TColorVector;
    ColorList: TVSTColorList;

    PrevShiftNumber: Integer;
    Calendar: TCalendar;
    Schedule: TShiftSchedule;
    Sheet: TShiftScheduleCalendarSheet;

    procedure PrevShiftNumberLoad;
    procedure ScheduleLoad;
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleRefresh;

    procedure ColorsLoad;
    function Colors: TColorVector;
    procedure ColorSelect;
    procedure ColorChange(const {%H-}ARowIndex, {%H-}AColIndex: Integer;
                           const {%H-}ANewText: String;
                           const {%H-}AColumnType: TVSTColumnType;
                           const {%H-}ASaveChanges: Boolean);
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
begin
  Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[10];
  Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel, ListToolPanel
  ]);
  SetCaptionPanels([
    ColorCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, SettingButton, AddButton, DelButton, EditButton
  ]);
  SetCategoryButtons([
    ExportButton
  ]);

  ColorList:= TVSTColorList.Create(ColorVT);
  ColorVT.BorderStyle:= bsSingle;
  ColorList.OnEdititingDone:= @ColorChange;
  ColorList.OnSelect:= @ColorSelect;

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  Calendar:= TCalendar.Create;
  Schedule:= TShiftSchedule.Create;
  Sheet:= TShiftScheduleCalendarSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
end;

procedure TScheduleShiftCalendarForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ColorList);
  FreeAndNil(Calendar);
  FreeAndNil(Schedule);
  FreeAndNil(Sheet);
end;

procedure TScheduleShiftCalendarForm.FormShow(Sender: TObject);
begin
  ColorsLoad;
  ColorList.Update(ColorNames, ColorValues);
  ScheduleRefresh;
end;

procedure TScheduleShiftCalendarForm.NeedCorrectionsCheckBoxChange(Sender: TObject);
begin
  ScheduleRedraw;
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
      if NeedCorrectionsCheckBox.Checked then
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
               NeedCorrectionsCheckBox.Checked, FirstShiftDayColorOnlyCheckBox.Checked);
    Sheet.ColorsUpdate(Colors);
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
  ScheduleRedraw;
end;

procedure TScheduleShiftCalendarForm.ColorsLoad;
var
  i: Integer;
begin
  ColorValues:= VCreateColor(COLORS_SHIFT);
  VDim(ColorNames, Length(ColorValues));
  ColorNames[0]:= 'Нерабочий день';
  for i:= 1 to High(ColorNames) do
    ColorNames[i]:= 'Смена №' + IntToStr(i);
  VAppend(ColorValues, COLOR_SHIFT_UNDEFINED_VALUE);
  VAppend(ColorNames, COLOR_SHIFT_UNDEFINED_NAME);
end;

function TScheduleShiftCalendarForm.Colors: TColorVector;
var
  TmpColors: TColorVector;
  i: Integer;
begin
  Result:= nil;
  //0 - not work color, 1..365 - shift colors
  //366 - month name color (MONTHNAME_COLOR_INDEX);
  //367 - day name color (DAYNAME_COLOR_INDEX);
  TmpColors:= ColorList.Colors;
  if VIsNil(TmpColors) then Exit;

  VDim(Result, DAYNAME_COLOR_INDEX+1, VLast(TmpColors));
  Result[0]:= VFirst(TmpColors);
  Result[MONTHNAME_COLOR_INDEX]:= COLOR_CALENDAR_MONTHNAME;
  Result[DAYNAME_COLOR_INDEX]:= COLOR_CALENDAR_DAYNAME;

  TmpColors:= VCut(TmpColors, 1, High(TmpColors));
  for i:= 0 to MONTHNAME_COLOR_INDEX-2 do
    Result[i+1]:= ColorFromVector(TmpColors, i);
end;

procedure TScheduleShiftCalendarForm.ColorSelect;
begin
  DelButton.Enabled:= ColorList.IsSelected and
                      (ColorList.SelectedRowIndex<>ColorList.Count-1) and //нельзя удалять цвет undefined
                      (ColorList.SelectedRowIndex<>0);    //нельзя удалять цвет нерабочего дня

  EditButton.Enabled:= ColorList.IsSelected;
end;

procedure TScheduleShiftCalendarForm.ColorChange(const ARowIndex, AColIndex: Integer;
                           const ANewText: String;
                           const AColumnType: TVSTColumnType;
                           const ASaveChanges: Boolean);
begin
  ScheduleRedraw;
end;

procedure TScheduleShiftCalendarForm.FirstShiftDayColorOnlyCheckBoxChange(Sender: TObject);
begin
  ScheduleRedraw;
end;

procedure TScheduleShiftCalendarForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TScheduleShiftCalendarForm.DelButtonClick(Sender: TObject);
begin
  if not ColorList.IsSelected then Exit;
  VDel(ColorNames, ColorList.SelectedRowIndex);
  VDel(ColorValues, ColorList.SelectedRowIndex);
  ColorList.Update(ColorNames, ColorValues);
  ScheduleRedraw;
end;

procedure TScheduleShiftCalendarForm.AddButtonClick(Sender: TObject);
begin
  if ColorList.Count>=367 then Exit;  //0 - not work color, 1..365 - shift colors, 366 - undefined color

  VIns(ColorNames, High(ColorNames), 'Смена №' + IntToStr(High(ColorNames)));
  VIns(ColorValues, High(ColorValues), COLOR_SHIFT_UNDEFINED_VALUE);
  ColorList.Update(ColorNames, ColorValues);
  ScheduleRedraw;
end;

procedure TScheduleShiftCalendarForm.EditButtonClick(Sender: TObject);
begin
  ColorList.Select(ColorList.SelectedRowIndex, 0);
end;

procedure TScheduleShiftCalendarForm.ExportButtonClick(Sender: TObject);
begin
  Sheet.Save(YearSpinEdit.Text);
end;

end.

