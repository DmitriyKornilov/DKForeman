unit UCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Spin, StdCtrls, DividerBevel, fpspreadsheetgrid, VirtualTrees, BCPanel,
  BCButton, DateUtils,
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
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    ExportButton: TBCButton;
    ZoomBevel: TBevel;
    ViewGrid: TsWorksheetGrid;
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
    procedure DayVTNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
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

    SelectedStatus, SelectedSwapDay: Integer;
    IsCopyDates: Boolean;

    procedure TablesCreate;
    procedure ColorsLoad;
    procedure CalendarLoad(const ASelectedDate: TDate = 0);
    procedure CalendarDraw(const AZoomPercent: Integer);
    procedure CalendarRefresh;
    procedure CopyListLoad(const ASelectedDate: TDate=0);

    procedure CopyBegin;
    procedure CopyEnd(const ANeedSave: Boolean);

    procedure CorrectionDelete;
    procedure CorrectionSelect;
    procedure CopySelect;

    function DayInListSelect(const ADate: TDate): Boolean;

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
  D: TDate;
begin
  if ModeType<>mtEditing then Exit;
  if not CalendarSheet.GridToDate(ViewGrid.Row, ViewGrid.Col, D) then Exit;
  VSTDays.ReSelect(Corrections.Dates, D, False);
  CalendarEditFormOpen(D);
end;

procedure TCalendarForm.ViewGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R,C: Integer;
  D: TDate;
begin
  if ModeType<>mtEditing then Exit;
  if Button=mbLeft then
  begin
    (Sender as TsWorksheetGrid).MouseToCell(X,Y,C,R);
    if not CalendarSheet.GridToDate(R, C, D) then Exit;
    if DayInListSelect(D) then Exit;
    CalendarSheet.DayInGridSelect(D);
    if IsCopyDates then CopyListLoad;
  end
  else if Button=mbRight then
  begin
    if IsCopyDates then
    begin
      VSTCopy.ValuesClear;
      CalendarSheet.SelectionClear;
    end
    else
      VSTDays.UnSelect;
  end;
end;

procedure TCalendarForm.CopyCancelButtonClick(Sender: TObject);
begin
  CopyEnd(False);
end;

procedure TCalendarForm.CopyDelButtonClick(Sender: TObject);
begin
  CalendarSheet.Unselect(CalendarSheet.SelectedDates[VSTCopy.SelectedIndex]);
  CopyListLoad;
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
  CorrectionDelete;
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
  ModeType:= mtView;

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
  SetCategoryButtons([
    ExportButton
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

  CalendarSheet:= TCalendarSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
  TablesCreate;
  Calendar:= TCalendar.Create;
  YearSpinEdit.Value:= YearOfDate(Date);
  IsCopyDates:= False;
end;

procedure TCalendarForm.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(ColorList);
  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);
  FreeAndNil(CalendarSheet);
  FreeAndNil(Calendar);
end;

procedure TCalendarForm.DayVTNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
var
  D: TDate;
begin
  if not VSTDays.IsSelected then Exit;
  D:= Corrections.Dates[HitInfo.HitNode^.Index];
  CalendarEditFormOpen(D);
end;

procedure TCalendarForm.YearSpinEditChange(Sender: TObject);
begin
  CalendarRefresh;
end;

procedure TCalendarForm.TablesCreate;
var
  i: Integer;
begin
  VSTDays:= TVSTTable.Create(DayVT);
  VSTDays.OnSelect:= @CorrectionSelect;
  VSTDays.OnDelKeyDown:= @CorrectionDelete;
  VSTDays.SetSingleFont(MainForm.GridFont);
  VSTDays.HeaderFont.Style:= [fsBold];
  VSTDays.CanSelect:= True;
  for i:= 0 to High(CALENDAR_CORRECTION_COLUMN_WIDTHS) do
    VSTDays.AddColumn(CALENDAR_CORRECTION_COLUMN_NAMES[i],
                      CALENDAR_CORRECTION_COLUMN_WIDTHS[i]);
  VSTDays.Draw;

  VSTCopy:= TVSTTable.Create(CopyVT);
  VSTCopy.OnSelect:= @CopySelect;
  VSTCopy.SetSingleFont(MainForm.GridFont);
  VSTCopy.HeaderFont.Style:= [fsBold];
  VSTCopy.CanSelect:= True;
  for i:= 0 to High(CALENDAR_CORRECTION_COLUMN_WIDTHS) do
    VSTCopy.AddColumn(CALENDAR_CORRECTION_COLUMN_NAMES[i],
                      CALENDAR_CORRECTION_COLUMN_WIDTHS[i]);
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
  FirstLastDayInYear(YearSpinEdit.Value, BD, ED);
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
  Dates:= VDateToStr(CalendarSheet.SelectedDates);
  VDim(Statuses{%H-}, Length(Dates), DAY_STATUS_PICKS[SelectedStatus]);
  VDim(SwapDays{%H-}, Length(Dates), DAY_NAME_PICKS[SelectedSwapDay]);

  VSTCopy.Visible:= False;
  try
    VSTCopy.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[0], Dates);
    VSTCopy.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[1], Statuses);
    VSTCopy.SetColumn(CALENDAR_CORRECTION_COLUMN_NAMES[2], SwapDays);
    VSTCopy.Draw;
    VSTCopy.ReSelect(CalendarSheet.SelectedDates, ASelectedDate);
  finally
    VSTCopy.Visible:= True;
  end;

  CopySaveButton.Enabled:= CalendarSheet.IsSelected;
end;

procedure TCalendarForm.CopyBegin;
begin
  IsCopyDates:= True;
  CalendarSheet.MultiSelect:= True;
  DayPanel.Visible:= False;
  DayPanel.Align:= alBottom;
  CopyPanel.Align:= alClient;
  CopyPanel.Visible:= True;

  SelectedStatus:= Corrections.Statuses[VSTDays.SelectedIndex];
  SelectedSwapDay:= Corrections.SwapDays[VSTDays.SelectedIndex];
  VSTDays.UnSelect;
end;

procedure TCalendarForm.CopyEnd(const ANeedSave: Boolean);
var
  C: TCalendarCorrections;
begin
  if CalendarSheet.IsSelected then
  begin
    if ANeedSave then //apply copies
    begin
      C:= GetCalendarCorrections(CalendarSheet.SelectedDates, SelectedStatus, SelectedSwapDay);
      DataBase.CalendarCorrectionsUpdate(C);
      CalendarRefresh;
    end
    else //cancel copies
      CalendarSheet.SelectionClear;
    VSTCopy.ValuesClear;
  end;

  IsCopyDates:= False;
  CalendarSheet.MultiSelect:= False;
  CopyPanel.Visible:= False;
  CopyPanel.Align:= alBottom;
  DayPanel.Align:= alClient;
  DayPanel.Visible:= True;
end;

procedure TCalendarForm.CorrectionDelete;
var
  Ind: Integer;
begin
  if not VSTDays.IsSelected then Exit;
  Ind:= VSTDays.SelectedIndex;
  DataBase.CalendarCorrectionDelete(Corrections.Dates[Ind]);
  CalendarRefresh;
  if VIsNil(Corrections.Dates) then Exit;
  if Ind>High(Corrections.Dates) then Dec(Ind);
  VSTDays.Select(Ind);
  VSTDays.SetFocus;
end;

procedure TCalendarForm.CorrectionSelect;
begin
  if VSTDays.IsSelected then
    CalendarSheet.DayInGridSelect(Corrections.Dates[VSTDays.SelectedIndex])
  else
    CalendarSheet.SelectionClear;

  DayDelButton.Enabled:= VSTDays.IsSelected;
  DayEditButton.Enabled:= DayDelButton.Enabled;
  DayCopyButton.Enabled:= DayDelButton.Enabled;
end;

procedure TCalendarForm.CopySelect;
begin
  CopyDelButton.Enabled:= VSTCopy.IsSelected;
end;

function TCalendarForm.DayInListSelect(const ADate: TDate): Boolean;
var
  Ind: Integer;
begin
  Result:= False;
  if IsCopyDates then Exit;

  Ind:= VIndexOfDate(Corrections.Dates, ADate);
  if Ind>=0 then
    VSTDays.Select(Ind)
  else if VSTDays.IsSelected then
    VSTDays.UnSelect;
  Result:= Ind>=0;
end;

procedure TCalendarForm.CalendarEditFormOpen(const ADate: TDate);
var
  CalendarEditForm: TCalendarEditForm;
  Ind: Integer;
begin
  CalendarEditForm:= TCalendarEditForm.Create(CalendarForm);
  try
    CalendarEditForm.DayDate:= ADate;
    CalendarEditForm.Year:= YearSpinEdit.Value;
    Ind:= VIndexOfDate(Corrections.Dates, ADate);
    if Ind>=0 then
    begin
      CalendarEditForm.SwapDayDropDown.ItemIndex:= Corrections.SwapDays[Ind];
      CalendarEditForm.StatusDropDown.ItemIndex:= Corrections.Statuses[Ind]-1;
    end;
    if CalendarEditForm.ShowModal=mrOK then
      CalendarRefresh
    else begin
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
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    CalendarSheet.Zoom(ZoomPercent);
    CalendarSheet.Draw(Calendar);
    CalendarSheet.ColorsUpdate(Colors);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TCalendarForm.CalendarRefresh;
begin
  CalendarLoad;
  CalendarDraw(ZoomPercent);
end;

procedure TCalendarForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;

  if IsCopyDates then CopyEnd(False);

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

  ExportButton.Enabled:= ModeType<>mtEditing;
end;

end.

