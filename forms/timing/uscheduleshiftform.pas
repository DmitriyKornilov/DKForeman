unit UScheduleShiftForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, BCPanel, BCButton, VirtualTrees, Spin, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UWorkHours, Ucalendar, USchedule,
  UScheduleShiftSheet,
  //DK packages utils
  DK_VSTTables, DK_VSTTools, DK_Vector, DK_StrUtils, DK_Const, DK_Dialogs,
  DK_Zoom, DK_DateUtils, DK_Color, DK_SheetExporter,
  //Forms
  UChooseForm, UScheduleCorrectionEditForm;

type

  { TScheduleShiftForm }

  TScheduleShiftForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    ColorTypeVT: TVirtualStringTree;
    CountTypeVT: TVirtualStringTree;
    ExportButton: TBCButton;
    CorrectionsCaptionPanel: TBCPanel;
    CloseButton: TSpeedButton;
    CopyCancelButton: TSpeedButton;
    CopyDelButton: TSpeedButton;
    CopyPanel: TPanel;
    CopySaveButton: TSpeedButton;
    CopyToolPanel: TPanel;
    ParamListVT: TVirtualStringTree;
    SettingClientPanel: TPanel;
    SettingCaptionPanel: TBCPanel;
    SheetCaptionPanel: TBCPanel;
    MainPanel: TPanel;
    ScheduleAddButton: TSpeedButton;
    DayAddButton: TSpeedButton;
    DayCopyButton: TSpeedButton;
    ScheduleDelButton: TSpeedButton;
    DayDelButton: TSpeedButton;
    ScheduleEditButton: TSpeedButton;
    DayEditButton: TSpeedButton;
    DayPanel: TPanel;
    DayToolPanel: TPanel;
    CorrectionsPanel: TPanel;
    SheetPanel: TPanel;
    ListToolPanel: TPanel;
    LeftSplitter: TSplitter;
    ListCaptionPanel: TBCPanel;
    ListPanel: TPanel;
    EditingPanel: TPanel;
    ScheduleListVT: TVirtualStringTree;
    ScheduleListPanel: TPanel;
    SettingPanel: TPanel;
    EditingSplitter: TSplitter;
    Splitter2: TSplitter;
    StructureCaptionPanel: TBCPanel;
    StructurePanel: TPanel;
    StructureVT: TVirtualStringTree;
    ToolPanel: TPanel;
    DayVT: TVirtualStringTree;
    CopyVT: TVirtualStringTree;
    ViewGrid: TsWorksheetGrid;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure DayAddButtonClick(Sender: TObject);
    procedure DayEditButtonClick(Sender: TObject);
    procedure DayVTNodeDblClick(Sender: TBaseVirtualTree; const {%H-}HitInfo: THitInfo);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    CanDrawSchedule: Boolean;
    ZoomPercent: Integer;
    ModeType: TModeType;

    Colors: TColorVector;

    ParamList: TVSTCheckTable;
    CountType: TVSTStringList;
    ColorType: TVSTStringList;

    ScheduleList: TVSTTable;
    //ScheduleCheck: TVSTCheckTable;
    Structure: TVSTTable;
    VSTDays: TVSTTable;
    VSTCopy: TVSTTable;

    ScheduleIDs, WeekHours, CycleCounts: TIntVector;
    ScheduleNames: TStrVector;

    Calendar: TCalendar;
    CorrectIDs: TIntVector;
    Correct: TScheduleCorrect;
    Cycle: TScheduleCycle;
    Schedule: TShiftSchedule;
    ScheduleSheetYear: TShiftScheduleTableSheet;

    procedure ColorsLoad;

    procedure ParamListCreate;
    procedure CountTypeCreate;
    procedure ColorTypeCreate;

    procedure EditingTablesCreate;
    procedure CorrectionSelect;

    procedure ScheduleListCreate;
    procedure ScheduleListSelect;
    procedure ScheduleListLoad(const SelectedID: Integer = -1);
    procedure ScheduleListDelItem;

    procedure CycleLoad;
    procedure CorrectionsLoad(const SelectedID: Integer = -1);
    procedure ScheduleLoad;
    procedure YearChange;
    procedure ScheduleChange;
    procedure ScheduleToSheet(var ASheet: TShiftScheduleTableSheet;
                              const AWorksheet: TsWorksheet;
                              const AGrid: TsWorksheetGrid;
                              const ACalendar: TCalendar;
                              const ASchedule: TShiftSchedule;
                              const AScheduleName: String = '');
    procedure ScheduleDraw(const AZoomPercent: Integer);
    procedure ScheduleRedraw;
    procedure ScheduleExport;

    procedure ScheduleCorrectionEditFormOpen(const AEditingType: TEditingType);

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  ScheduleShiftForm: TScheduleShiftForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TScheduleShiftForm }

procedure TScheduleShiftForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TScheduleShiftForm.DayAddButtonClick(Sender: TObject);
begin
  ScheduleCorrectionEditFormOpen(etAdd);
end;

procedure TScheduleShiftForm.DayEditButtonClick(Sender: TObject);
begin
  ScheduleCorrectionEditFormOpen(etEdit);
end;

procedure TScheduleShiftForm.DayVTNodeDblClick(Sender: TBaseVirtualTree; const HitInfo: THitInfo);
begin
  if not VSTDays.IsSelected then Exit;
  ScheduleCorrectionEditFormOpen(etEdit);
end;

procedure TScheduleShiftForm.ExportButtonClick(Sender: TObject);
begin
  ScheduleExport;
end;

procedure TScheduleShiftForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  SetToolPanels([
    ToolPanel, ListToolPanel, DayToolPanel, CopyToolPanel
  ]);
  SetCaptionPanels([
    SettingCaptionPanel, ListCaptionPanel, StructureCaptionPanel, CorrectionsCaptionPanel,
    SheetCaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    ScheduleAddButton, ScheduleDelButton, ScheduleEditButton,
    DayAddButton, DayDelButton, DayEditButton, DayCopyButton,
    CopySaveButton, CopyDelButton,CopyCancelButton
  ]);

  ColorsLoad;

  CanDrawSchedule:= False;
  Calendar:= TCalendar.Create;
  Schedule:= TShiftSchedule.Create;
  ScheduleListCreate;
  ParamListCreate;
  CountTypeCreate;
  ColorTypeCreate;
  EditingTablesCreate;
  SettingsLoad;
  YearSpinEdit.Value:= YearOfDate(Date);
  CanDrawSchedule:= True;

  ViewUpdate(mtView);
end;

procedure TScheduleShiftForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(CountType);
  FreeAndNil(ColorType);

  FreeAndNil(ScheduleList);

  FreeAndNil(Structure);
  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);

  FreeAndNil(Calendar);
  FreeAndNil(Schedule);
  if Assigned(ScheduleSheetYear) then FreeAndNil(ScheduleSheetYear);
end;

procedure TScheduleShiftForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  H:= MainPanel.Height div 3;
  EditingPanel.Height:= 2*H;
  CorrectionsPanel.Height:= H;
end;

procedure TScheduleShiftForm.YearSpinEditChange(Sender: TObject);
begin
  YearChange;
end;

procedure TScheduleShiftForm.ColorsLoad;
begin
  Colors:= nil;
  VDim(Colors, 6);
  Colors[CORRECT_COLOR_INDEX]:= COLOR_SCHEDULE_CORRECTION;
  Colors[NOTWORK_COLOR_INDEX]:= COLOR_SCHEDULE_NOTWORK;
  Colors[TITLE_COLOR_INDEX]:= COLOR_SCHEDULE_TITLE;
  Colors[OUTSIDEMONTH_COLOR_INDEX]:= COLOR_SCHEDULE_OUTSIDEMONTH;
  Colors[HIGHLIGHT_COLOR_INDEX]:= DefaultSelectionBGColor;
end;

procedure TScheduleShiftForm.ScheduleLoad;
begin
  if ScheduleList.IsSelected then
    Schedule.Calc(Calendar, WeekHours[ScheduleList.SelectedIndex], Cycle, Correct)
  else
    Schedule.Clear;
end;

procedure TScheduleShiftForm.EditingTablesCreate;
var
  i: Integer;
  W: TIntVector;
begin
  W:= VCreateInt([70, 60, 80, 90, 70]);

  Structure:= TVSTTable.Create(StructureVT);
  Structure.SetSingleFont(MainForm.GridFont);
  Structure.HeaderFont.Style:= [fsBold];
  Structure.CanSelect:= False;
  for i:= 0 to High(W) do
    Structure.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i], W[i]);
  Structure.Draw;

  VSTDays:= TVSTTable.Create(DayVT);
  VSTDays.OnSelect:= @CorrectionSelect;
  VSTDays.SetSingleFont(MainForm.GridFont);
  VSTDays.HeaderFont.Style:= [fsBold];
  VSTDays.CanSelect:= True;
  for i:= 0 to High(W) do
    VSTDays.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i], W[i]);
  VSTDays.Draw;

  VSTCopy:= TVSTTable.Create(CopyVT);
  //VSTCopy.OnSelect:= @CopySelect;
  VSTCopy.SetSingleFont(MainForm.GridFont);
  VSTCopy.HeaderFont.Style:= [fsBold];
  VSTCopy.CanSelect:= True;
  for i:= 0 to High(W) do
    VSTCopy.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i], W[i]);
  VSTCopy.Draw;
end;

procedure TScheduleShiftForm.CycleLoad;
var
  CycleIDs: TIntVector;
  StrDates, StrShiftNums: TStrVector;
begin
  Structure.ValuesClear;
  StructureCaptionPanel.Caption:= 'Структура: ';

  if not ScheduleList.IsSelected then Exit;

  StructureCaptionPanel.Caption:= StructureCaptionPanel.Caption +
                                  ScheduleNames[ScheduleList.SelectedIndex];

  DataBase.ScheduleCycleLoad(ScheduleIDs[ScheduleList.SelectedIndex], CycleIDs, Cycle);

  StrShiftNums:= VIntToStr(Cycle.ShiftNums);
  VChangeIf(StrShiftNums, '0', EMPTY_MARK);

  if Cycle.IsWeek then
  begin
    Structure.RenameColumn(0, 'День');
    StrDates:= VCreateStr(WEEKDAYSSHORT);
  end
  else begin
    Structure.RenameColumn(0, 'Дата');
    StrDates:= VDateToStr(Cycle.Dates);
  end;

  Structure.Visible:= False;
  try
    Structure.ValuesClear;
    Structure.SetColumn({SCHEDULE_CORRECTION_COLUMN_NAMES[0]}0, StrDates);
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], StrShiftNums);
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursToStr(Cycle.HoursTotal));
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursToStr(Cycle.HoursNight));
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], Cycle.StrMarks);
    Structure.Draw;
  finally
    Structure.Visible:= True;
  end;
end;

procedure TScheduleShiftForm.CorrectionsLoad(const SelectedID: Integer = -1);
var
  SelectedCorrectionID: Integer;
  StrDates, StrShiftNums: TStrVector;
  BD, ED: TDate;
begin
  SelectedCorrectionID:= GetSelectedID(VSTDays, CorrectIDs, SelectedID);

  VSTDays.ValuesClear;
  CorrectionsCaptionPanel.Caption:= 'Корректировки: ';

  if not ScheduleList.IsSelected then Exit;

  CorrectionsCaptionPanel.Caption:= CorrectionsCaptionPanel.Caption +
                                  ScheduleNames[ScheduleList.SelectedIndex];

  BD:= FirstDayInYear(YearSpinEdit.Value);
  ED:= LastDayInYear(YearSpinEdit.Value);
  DataBase.ScheduleShiftCorrectionsLoad(ScheduleIDs[ScheduleList.SelectedIndex],
                                        CorrectIDs, Correct, BD, ED);

  StrShiftNums:= VIntToStr(Correct.ShiftNums);
  VChangeIf(StrShiftNums, '0', EMPTY_MARK);
  StrDates:= VDateToStr(Correct.Dates);

  VSTDays.Visible:= False;
  try
    VSTDays.ValuesClear;
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[0], StrDates);
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], StrShiftNums);
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursToStr(Correct.HoursTotal));
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursToStr(Correct.HoursNight));
    VSTDays.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], Correct.StrMarks);
    VSTDays.Draw;
  finally
    VSTDays.Visible:= True;
  end;
end;

procedure TScheduleShiftForm.CorrectionSelect;
begin
  DayDelButton.Enabled:= VSTDays.IsSelected;
  DayEditButton.Enabled:= DayDelButton.Enabled;
  DayCopyButton.Enabled:= DayDelButton.Enabled;
end;

procedure TScheduleShiftForm.ScheduleListCreate;
begin
  ScheduleList:= TVSTTable.Create(ScheduleListVT);
  ScheduleList.CanSelect:= True;
  ScheduleList.CanUnselect:= False;
  ScheduleList.OnSelect:= @ScheduleListSelect;
  ScheduleList.OnDelKeyDown:= @ScheduleListDelItem;
  ScheduleList.SetSingleFont(MainForm.GridFont);
  ScheduleList.HeaderFont.Style:= [fsBold];

  ScheduleList.AddColumn('№ п/п', 50);
  ScheduleList.AddColumn('Наименование графика', 300);
  ScheduleList.AddColumn('Часов в неделю', 120);
  ScheduleList.AutosizeColumnEnable('Наименование графика');
  ScheduleList.Draw;
end;

procedure TScheduleShiftForm.ScheduleListSelect;
begin
  DayAddButton.Enabled:= ScheduleList.IsSelected;
  ScheduleChange;
end;

procedure TScheduleShiftForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Параметры отображения:';
  V:= VCreateStr([
    'Отображать строку ночных часов',
    'Учитывать корректировки графика',
    'Коды табеля для нерабочих дней',
    'Использовать цвета'
  ]);
  ParamList:= TVSTCheckList.Create(ParamListVT, S, V, @ScheduleRedraw);
end;

procedure TScheduleShiftForm.CountTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Отображать в итогах количество:';
  V:= VCreateStr([
    'дней',
    'смен',
    'дней и смен'
  ]);
  CountType:= TVSTStringList.Create(CountTypeVT, S, @ScheduleRedraw);
  CountType.Update(V);
end;

procedure TScheduleShiftForm.ColorTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Выделять цветом нерабочие дни:';
  V:= VCreateStr([
    'по графику сменности',
    'по производственному календарю'
  ]);
  ColorType:= TVSTStringList.Create(ColorTypeVT, S, @ScheduleRedraw);
  ColorType.Update(V);
end;

procedure TScheduleShiftForm.YearChange;
begin
  CalendarForYear(YearSpinEdit.Value, Calendar);
  ScheduleChange;
end;

procedure TScheduleShiftForm.ScheduleChange;
begin
  CycleLoad;
  CorrectionsLoad;
  ScheduleLoad;
  ScheduleRedraw;
end;

procedure TScheduleShiftForm.ScheduleToSheet(var ASheet: TShiftScheduleTableSheet;
  const AWorksheet: TsWorksheet; const AGrid: TsWorksheetGrid;
  const ACalendar: TCalendar; const ASchedule: TShiftSchedule;
  const AScheduleName: String = '');
var
  SelDates: TDateVector;
begin
  if Assigned(ASheet) then FreeAndNil(ASheet);
    ASheet:= TShiftScheduleTableSheet.Create(MainForm.GridFont,
                              AWorksheet, AGrid, CountType.SelectedIndex);
  SelDates:= nil;
  if Assigned(AGrid) then
  begin
    ASheet.Zoom(ZoomPercent);
    //SelDates:= SelectedDates;  !!!!
  end;
  ASheet.Draw(ACalendar, ASchedule, AScheduleName,
              ParamList.Checked[0], ParamList.Checked[1], ParamList.Checked[2],
              ColorType.SelectedIndex=0, SelDates);
  if ParamList.Checked[3] then
    ASheet.ColorsUpdate(Colors)
  else
    ASheet.ColorsClear;
end;

procedure TScheduleShiftForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  if not CanDrawSchedule then Exit;
  if not Calendar.Calculated then Exit;
  if not Schedule.Calculated then Exit;

  SheetCaptionPanel.Caption:= 'График: ';
  if not ScheduleList.IsSelected then Exit;
  SheetCaptionPanel.Caption:= SheetCaptionPanel.Caption +
                              ScheduleNames[ScheduleList.SelectedIndex] +
                              ' на ' + YearSpinEdit.Text + ' год';

  ViewGrid.Visible:= False;
  try
    ZoomPercent:= AZoomPercent;
    ScheduleToSheet(ScheduleSheetYear, ViewGrid.Worksheet, ViewGrid, Calendar, Schedule);
    //if Assigned(ScheduleSheetYear) then FreeAndNil(ScheduleSheetYear);
    //ScheduleSheetYear:= TShiftScheduleTableSheet.Create(MainForm.GridFont,
    //                          ViewGrid.Worksheet, ViewGrid, CountType.SelectedIndex);
    //
    //ScheduleSheetYear.Zoom(ZoomPercent);
    //ScheduleSheetYear.Draw(Calendar, Schedule, EmptyStr,
    //                       ParamList.Checked[0], ParamList.Checked[1],
    //                       ParamList.Checked[2], ColorType.SelectedIndex=0,
    //                       {SelectedDates}nil);
    //if ParamList.Checked[3] then
    //  ScheduleSheetYear.ColorsUpdate(Colors)
    //else
    //  ScheduleSheetYear.ColorsClear;
  finally
    ViewGrid.Visible:= True;
  end;
end;

procedure TScheduleShiftForm.ScheduleRedraw;
begin
  ScheduleDraw(ZoomPercent);
end;

procedure TScheduleShiftForm.ScheduleExport;
var
  V: TStrVector;
  S: String;
  i: Integer;
  TmpSchedule: TShiftSchedule;
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ScheduleSheet: TShiftScheduleTableSheet;
begin
  if not ScheduleList.IsSelected then Exit;

  S:= 'Сохранить в файл:';
  V:= VCreateStr([
    'График "' + ScheduleNames[ScheduleList.SelectedIndex] + '" на ' + YearSpinEdit.Text + ' год',
    'Все графики на ' + YearSpinEdit.Text + ' год'
  ]);
  i:= Choose(S, V);
  if i=0 then Exit;

  Exporter:= TSheetsExporter.Create;
  try
    ScheduleSheet:= nil;
    if i=1 then //выбранный график
    begin
      Worksheet:= Exporter.AddWorksheet(ScheduleNames[ScheduleList.SelectedIndex]);
      ScheduleToSheet(ScheduleSheet, Worksheet, nil, Calendar, Schedule,
                      ScheduleNames[ScheduleList.SelectedIndex]);
      Exporter.PageSettings(spoLandscape);
    end
    else begin  //все графики
      TmpSchedule:= TShiftSchedule.Create;
      try
        for i:=0 to High(ScheduleIDs) do
        begin
          ScheduleShiftByCalendar(ScheduleIDs[i], Calendar, TmpSchedule);
          Worksheet:= Exporter.AddWorksheet(ScheduleNames[i]);
          ScheduleToSheet(ScheduleSheet, Worksheet, nil, Calendar, TmpSchedule,
                          ScheduleNames[i]);
          Exporter.PageSettings(spoLandscape);
        end;
      finally
        FreeAndNil(TmpSchedule);
      end;
    end;
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(ScheduleSheet);
    FreeAndNil(Exporter);
  end;
end;

procedure TScheduleShiftForm.ScheduleCorrectionEditFormOpen(const AEditingType: TEditingType);
var
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;
begin
  ScheduleCorrectionEditForm:= TScheduleCorrectionEditForm.Create(nil);
  try
    if CycleCounts[ScheduleList.SelectedIndex]>0 then
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.MaxValue:= CycleCounts[ScheduleList.SelectedIndex]
    else
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.MaxValue:= 7; //недельный график
    if AEditingType=etEdit then
    begin
      ScheduleCorrectionEditForm.DigMark:= Correct.DigMarks[VSTDays.SelectedIndex];
      ScheduleCorrectionEditForm.FirstDatePicker.Date:= Correct.Dates[VSTDays.SelectedIndex];
      ScheduleCorrectionEditForm.LastDatePicker.Date:= IncDay(Correct.Dates[VSTDays.SelectedIndex]);
      ScheduleCorrectionEditForm.TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(Correct.HoursTotal[VSTDays.SelectedIndex]);
      ScheduleCorrectionEditForm.NightHoursSpinEdit.Value:= WorkHoursIntToFrac(Correct.HoursNight[VSTDays.SelectedIndex]);;
      ScheduleCorrectionEditForm.ShiftNumSpinEdit.Value:= Correct.ShiftNums[VSTDays.SelectedIndex];
    end;
    if ScheduleCorrectionEditForm.ShowModal=mrOK then
      {StaffListLoad(ScheduleCorrectionEditForm.StaffID)};
  finally
    FreeAndNil(ScheduleCorrectionEditForm);
  end;
end;

procedure TScheduleShiftForm.ScheduleListLoad(const SelectedID: Integer = -1);
var
  SelectedScheduleID: Integer;
begin
  SelectedScheduleID:= GetSelectedID(ScheduleList, ScheduleIDs, SelectedID);

  DataBase.ScheduleMainListLoad(ScheduleIDs, WeekHours, CycleCounts, ScheduleNames);

  ScheduleList.Visible:= False;
  try
    ScheduleList.ValuesClear;
    ScheduleList.SetColumn('№ п/п', VIntToStr(VOrder(Length(ScheduleIDs))));
    ScheduleList.SetColumn('Наименование графика', ScheduleNames, taLeftJustify);
    ScheduleList.SetColumn('Часов в неделю', VIntToStr(WeekHours));
    ScheduleList.Draw;
    ScheduleList.ReSelect(ScheduleIDs, SelectedScheduleID, True);  //возвращаем выделение строки
  finally
    ScheduleList.Visible:= True;
  end;
end;

procedure TScheduleShiftForm.ScheduleListDelItem;
begin

end;

procedure TScheduleShiftForm.SettingsLoad;
begin

end;

procedure TScheduleShiftForm.SettingsSave;
begin

end;

procedure TScheduleShiftForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
  SettingPanel.Visible:= False;
  ListPanel.Visible:= False;
  try
    ModeType:= AModeType;
    ExportButton.Enabled:= ModeType<>mtEditing;

    LeftSplitter.Align:= alRight;
    if ModeType=mtSetting then
      SettingPanel.Visible:= True
    else
      ListPanel.Visible:= True;
    LeftSplitter.Align:= alLeft;

    //StaffList.CanUnselect:= ModeType<>mtEditing;
    //StaffList.CanSelect:= ModeType=mtEditing;
    ListToolPanel.Visible:= ModeType=mtEditing;

    if ModeType=mtEditing then
    begin
      EditingPanel.Visible:= True;
      EditingSplitter.Visible:= True;
    end
    else begin
      EditingSplitter.Visible:= False;
      EditingPanel.Visible:= False;
    end;

    ScheduleListLoad;

  finally
    MainPanel.Visible:= True;
  end;
end;

end.

