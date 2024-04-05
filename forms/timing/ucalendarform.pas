unit UCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Spin, StdCtrls, fpspreadsheetgrid, VirtualTrees, BCPanel, BCButton, DateUtils,
  //Project utils
  UConst, UTypes, UUtils, UCalendar, UCalendarSheet,
  //DK packages utils
  DK_DateUtils, {DK_VSTTools,} DK_VSTTables, DK_Vector, DK_Zoom, DK_Color,
  DK_Const,
  //Forms
  UCalendarEditForm;

type

  { TCalendarForm }

  TCalendarForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    ExportButton: TBCButton;
    ZoomBevel: TBevel;
    ViewGrid: TsWorksheetGrid;
    CloseButton: TSpeedButton;
    CopySaveButton: TSpeedButton;
    CopyDelButton: TSpeedButton;
    CopyCancelButton: TSpeedButton;
    CopyPanel: TPanel;
    CopyToolPanel: TPanel;
    CaptionPanel: TBCPanel;
    DayPanel: TPanel;
    SheetPanel: TPanel;
    EditingPanel: TPanel;
    LeftSplitter: TSplitter;
    DayAddButton: TSpeedButton;
    DayDelButton: TSpeedButton;
    DayCopyButton: TSpeedButton;
    DayEditButton: TSpeedButton;
    DayToolPanel: TPanel;
    DayVT: TVirtualStringTree;
    CopyVT: TVirtualStringTree;
    YearPanel: TPanel;
    ToolPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomPanel: TPanel;
    procedure ViewGridDblClick(Sender: TObject);
    procedure ViewGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure CloseButtonClick(Sender: TObject);
    procedure CopyCancelButtonClick(Sender: TObject);
    procedure CopyDelButtonClick(Sender: TObject);
    procedure CopySaveButtonClick(Sender: TObject);
    procedure DayAddButtonClick(Sender: TObject);
    procedure DayCopyButtonClick(Sender: TObject);
    procedure DayDelButtonClick(Sender: TObject);
    procedure DayEditButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DayVTDblClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    ModeType: TModeType;
    ZoomPercent: Integer;

    //Items: TStrVector;
    Colors: TColorVector;
    //ColorList: TVSTColorList;

    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;
    CalendarSheet: TCalendarSheet;
    Calendar: TCalendar;
    Corrections: TCalendarCorrections;

    SelectedDates: TDateVector;
    SelectedStatus: Integer;
    SelectedSwapDay: Integer;
    IsCopyDates: Boolean;

    procedure TablesCreate;
    procedure ColorsLoad;
    procedure CalendarLoad(const ASelectedDate: TDate = 0);
    procedure CalendarDraw(const AZoomPercent: Integer);
    procedure CalendarRefresh;
    procedure CopyListLoad(const ASelectedDate: TDate=0);

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);

    procedure CorrectionSelect;
    procedure CopySelect;

    procedure DayInGridSelect(const ADate: TDate);
    procedure DayInListSelect(const ADate: TDate);

    procedure CalendarEditFormOpen(const ADate: TDate);
  public
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  CalendarForm: TCalendarForm;

implementation

uses UMainForm, UDataBase;

{$R *.lfm}

{ TCalendarForm }

procedure TCalendarForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TCalendarForm.ViewGridDblClick(Sender: TObject);
var
  DayDate: TDate;
begin
  if not CalendarSheet.GridToDate(ViewGrid.Row, ViewGrid.Col, DayDate) then Exit;
  VSTDays.ReSelect(Corrections.Dates, DayDate, False);
  CalendarEditFormOpen(DayDate);
end;

procedure TCalendarForm.ViewGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R,C: Integer;
  DayDate: TDate;
begin
  R:= 0;
  C:= 0;
  if Button=mbLeft then
    (Sender as TsWorksheetGrid).MouseToCell(X,Y,C,R);

  CalendarSheet.GridToDate(R, C, DayDate);
  DayInGridSelect(DayDate);
  DayInListSelect(DayDate);
end;

procedure TCalendarForm.CopyCancelButtonClick(Sender: TObject);
begin
  CopyEnd(False);
end;

procedure TCalendarForm.CopyDelButtonClick(Sender: TObject);
begin
  VDel(SelectedDates, VSTCopy.SelectedIndex);
  CopyListLoad;
  CalendarDraw(ZoomPercent);
end;

procedure TCalendarForm.CopySaveButtonClick(Sender: TObject);
begin
  CopyEnd(True);
end;

procedure TCalendarForm.DayAddButtonClick(Sender: TObject);
begin
  CalendarEditFormOpen(NULDATE);
end;

procedure TCalendarForm.DayCopyButtonClick(Sender: TObject);
begin
  CopyBegin;
end;

procedure TCalendarForm.DayDelButtonClick(Sender: TObject);
begin
  DataBase.CalendarCorrectionDelete(Corrections.Dates[VSTDays.SelectedIndex]);
  CalendarRefresh;
end;

procedure TCalendarForm.DayEditButtonClick(Sender: TObject);
begin
  CalendarEditFormOpen(Corrections.Dates[VSTDays.SelectedIndex]);
end;

procedure TCalendarForm.ExportButtonClick(Sender: TObject);
begin
  CalendarSheet.Save(YearSpinEdit.Text, 'Выполнено!', True);
end;

procedure TCalendarForm.FormCreate(Sender: TObject);

begin
  SetToolPanels([
    ToolPanel, DayToolPanel, CopyToolPanel
  ]);
  SetCaptionPanels([
    CaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton,CopyCancelButton
  ]);

  ColorsLoad;

  //Items:= VCreateStr([
  //  'Нерабочий праздничный день',
  //  'Нерабочий выходной день',
  //  'Рабочий предпраздничный (сокращенный) день',
  //  'Рабочий день'
  //]);
  //Colors:= VCreateColor([
  //  $0097CBFF,
  //  $00CCE3CC,
  //  $00FFCACA,
  //  $00FFFFFF]);
  //ColorList:= TVSTColorList.Create(DayVT);

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @CalendarDraw, True);

  CalendarSheet:= TCalendarSheet.Create(MainForm.GridFont, ViewGrid.Worksheet, ViewGrid);
  TablesCreate;
  Calendar:= TCalendar.Create;
  YearSpinEdit.Value:= YearOfDate(Date);
end;

procedure TCalendarForm.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(ColorList);
  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);
  FreeAndNil(CalendarSheet);
  FreeAndNil(Calendar);
end;

procedure TCalendarForm.FormShow(Sender: TObject);
begin
  //ColorList.Update(Items, Colors);
end;

procedure TCalendarForm.DayVTDblClick(Sender: TObject);
var
  DayDate: TDate;
begin
  if CalendarSheet.GridToDate(ViewGrid.Row, ViewGrid.Col, DayDate) then
    CalendarEditFormOpen(DayDate);
end;

procedure TCalendarForm.YearSpinEditChange(Sender: TObject);
begin
  CalendarRefresh;
end;

procedure TCalendarForm.TablesCreate;
var
  i: Integer;
  W: TIntVector;
begin
  W:= VCreateInt([80, 110, 150]);

  VSTDays:= TVSTTable.Create(DayVT);
  VSTDays.OnSelect:= @CorrectionSelect;
  VSTDays.SetSingleFont(MainForm.GridFont);
  VSTDays.HeaderFont.Style:= [fsBold];
  VSTDays.CanSelect:= True;
  for i:= 0 to High(W) do
    VSTDays.AddColumn(CALENDAR_CORRECTION_COLUMN_NAMES[i], W[i]);
  VSTDays.Draw;

  VSTCopy:= TVSTTable.Create(CopyVT);
  VSTCopy.OnSelect:= @CopySelect;
  VSTCopy.SetSingleFont(MainForm.GridFont);
  VSTCopy.HeaderFont.Style:= [fsBold];
  VSTCopy.CanSelect:= True;
  for i:= 0 to High(W) do
    VSTCopy.AddColumn(CALENDAR_CORRECTION_COLUMN_NAMES[i], W[i]);
  VSTCopy.Draw;
end;

procedure TCalendarForm.ColorsLoad;
begin
  Colors:= nil;
  VDim(Colors, 11);
  Colors[HOLIDEY_COLOR_INDEX]:= COLORS_CALENDAR[HOLIDEY_COLOR_INDEX];
  Colors[OFFDAY_COLOR_INDEX]:= COLORS_CALENDAR[OFFDAY_COLOR_INDEX];
  Colors[BEFORE_COLOR_INDEX]:= COLORS_CALENDAR[BEFORE_COLOR_INDEX];
  Colors[WEEKDAY_COLOR_INDEX]:= COLORS_CALENDAR[WEEKDAY_COLOR_INDEX];
  Colors[MONTHNAME_COLOR_INDEX]:= COLOR_CALENDAR_MONTHNAME;
  Colors[DAYNAME_COLOR_INDEX]:= COLOR_CALENDAR_DAYNAME;
  Colors[HIGHLIGHT_COLOR_INDEX]:= DefaultSelectionBGColor;

  Colors[QUARTER_COLOR_INDEX]:= COLOR_CALENDAR_QUARTER;
  Colors[HALFYEAR_COLOR_INDEX]:= COLOR_CALENDAR_HALFYEAR;
  Colors[YEAR_COLOR_INDEX]:= COLOR_CALENDAR_YEAR;
end;

procedure TCalendarForm.CalendarLoad(const ASelectedDate: TDate = 0);
var
  BD, ED: TDate;
  Dates, Statuses, SwapDays: TStrVector;
begin
  BD:= FirstDayInYear(YearSpinEdit.Value);
  ED:= LastDayInYear(YearSpinEdit.Value);
  DataBase.CalendarCorrectionsLoad(BD, ED, Corrections);

  Dates:= VDateToStr(Corrections.Dates);
  Statuses:= VPickFromKey(Corrections.Statuses, DAY_STATUS_KEYS, DAY_STATUS_PICKS);
  SwapDays:= VPickFromKey(Corrections.SwapDays, DAY_NAME_KEYS, DAY_NAME_PICKS);

  VSTDays.ValuesClear;
  VSTDays.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[0], Dates);
  VSTDays.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[1], Statuses);
  VSTDays.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[2], SwapDays);
  VSTDays.Draw;
  VSTDays.ReSelect(Corrections.Dates, ASelectedDate);

  Calendar.Calc(BD, ED, Corrections);
end;

procedure TCalendarForm.CopyListLoad(const ASelectedDate: TDate = 0);
var
  Dates, Statuses, SwapDays: TStrVector;
begin
  Dates:= VDateToStr(SelectedDates);
  VDim(Statuses{%H-}, Length(SelectedDates), DAY_STATUS_PICKS[SelectedStatus]);
  VDim(SwapDays{%H-}, Length(SelectedDates), DAY_NAME_PICKS[SelectedSwapDay]);

  VSTCopy.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[0], Dates);
  VSTCopy.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[1], Statuses);
  VSTCopy.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[2], SwapDays);
  VSTCopy.Draw;
  VSTCopy.ReSelect(SelectedDates, ASelectedDate);

  CopySaveButton.Enabled:= not VIsNil(SelectedDates);
end;

procedure TCalendarForm.CopyBegin;
begin
  IsCopyDates:= True;
  DayPanel.Visible:= False;
  DayPanel.Align:= alBottom;
  CopyPanel.Align:= alClient;
  CopyPanel.Visible:= True;

  SelectedDates:= nil;
  SelectedStatus:= Corrections.Statuses[VSTDays.SelectedIndex];
  SelectedSwapDay:= Corrections.SwapDays[VSTDays.SelectedIndex];
  CalendarSheet.SelectionClear;
end;

procedure TCalendarForm.CopyEnd(const ANeedSave: Boolean);
begin
  if not VIsNil(SelectedDates) then
  begin
    if ANeedSave then //apply copies
      DataBase.CalendarCorrectionsUpdate(SelectedDates, SelectedStatus, SelectedSwapDay);
    VSTCopy.ValuesClear;
    CalendarRefresh;
  end
  else begin //cancel copies
    CalendarSheet.SelectionClear;
    VSTDays.Unselect;
  end;
  IsCopyDates:= False;
  CopyPanel.Visible:= False;
  CopyPanel.Align:= alBottom;
  DayPanel.Align:= alClient;
  DayPanel.Visible:= True;
end;

procedure TCalendarForm.CorrectionSelect;
var
  DayDate: TDate;
begin
  DayDate:= NULDATE;
  if VSTDays.IsSelected then
    DayDate:= Corrections.Dates[VSTDays.SelectedIndex];

  DayInGridSelect(DayDate);
  DayDelButton.Enabled:= VSTDays.IsSelected;
  DayEditButton.Enabled:= DayDelButton.Enabled;
  DayCopyButton.Enabled:= DayDelButton.Enabled;
end;

procedure TCalendarForm.CopySelect;
begin
  CopyDelButton.Enabled:= VSTCopy.IsSelected;
end;

procedure TCalendarForm.DayInGridSelect(const ADate: TDate);
var
  Ind, R, C, ROld, COld: Integer;
begin
  CalendarSheet.DateToGrid(ADate, R, C);
  if IsCopyDates then
  begin
    Ind:= VIndexOfDate(SelectedDates, ADate);
    if Ind>=0 then
    begin
      VDel(SelectedDates, Ind);
      CalendarSheet.SelectionDelCell(R, C);
    end
    else begin
      VInsAscDate(SelectedDates, ADate);
      CalendarSheet.SelectionAddCell(R, C);
    end;
    CopyListLoad;
  end
  else begin
    if not VIsNil(SelectedDates) then
    begin
      CalendarSheet.DateToGrid(SelectedDates[0], ROld, COld);
      CalendarSheet.SelectionDelCell(ROld, COld);
    end;
    if not SameDate(ADate, NULDATE) then
    begin
      VDim(SelectedDates, 1, ADate);
      CalendarSheet.SelectionAddCell(R, C);
    end
    else begin
      SelectedDates:= nil;
    end;
  end;
end;

procedure TCalendarForm.DayInListSelect(const ADate: TDate);
var
  Ind: Integer;
begin
  if IsCopyDates then Exit;

  Ind:= VIndexOfDate(Corrections.Dates, ADate);
  if Ind>=0 then
    VSTDays.Select(Ind)
  else
    VSTDays.UnSelect;
end;

procedure TCalendarForm.CalendarEditFormOpen(const ADate: TDate);
var
  CalendarEditForm: TCalendarEditForm;
begin
  CalendarEditForm:= TCalendarEditForm.Create(CalendarForm);
  try
    CalendarEditForm.DayDate:= ADate;
    CalendarEditForm.Year:= YearSpinEdit.Value;
    if CalendarEditForm.ShowModal=mrOK then
      CalendarRefresh
    else begin
      SelectedDates:= nil;
      CalendarDraw(ZoomPercent);
      VSTDays.UnSelect;
    end;
  finally
    FreeAndNil(CalendarEditForm);
  end;
end;

procedure TCalendarForm.CalendarDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  try
    ZoomPercent:= AZoomPercent;
    CalendarSheet.Zoom(ZoomPercent);
    CalendarSheet.Draw(Calendar, SelectedDates);
    CalendarSheet.ColorsUpdate(Colors);
  finally
    ViewGrid.Visible:= True;
  end;
end;

procedure TCalendarForm.CalendarRefresh;
begin
  SelectedDates:= nil;
  CalendarLoad;
  CalendarDraw(ZoomPercent);
end;

procedure TCalendarForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;

  if ModeType=mtEditing then
  begin
    EditingPanel.Visible:= True;
    LeftSplitter.Visible:= True;
    SheetPanel.AnchorToNeighbour(akLeft, 0, LeftSplitter);
  end
  else begin
    LeftSplitter.Visible:= False;
    EditingPanel.Visible:= False;
    SheetPanel.AnchorToNeighbour(akLeft, 2, Self);
  end;
end;

end.

