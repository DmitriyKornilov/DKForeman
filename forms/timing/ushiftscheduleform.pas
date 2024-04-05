unit UShiftScheduleForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, BCPanel, BCButton, VirtualTrees, Spin,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UWorkHours,
  //DK packages utils
  DK_VSTTables, DK_VSTTools, DK_Vector, DK_StrUtils, DK_Const, DK_Dialogs,
  DK_Zoom, DK_DateUtils;
  //Forms
  //;

type

  { TShiftScheduleForm }

  TShiftScheduleForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CanDrawSchedule: Boolean;
    ZoomPercent: Integer;
    ModeType: TModeType;

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

    procedure EditingTablesCreate;
    procedure StructureLoad;

    procedure ScheduleListCreate;
    procedure ScheduleListSelect;
    procedure ScheduleListLoad(const SelectedID: Integer = -1);
    procedure ScheduleListDelItem;

    procedure ParamListCreate;
    procedure ParamListSelect;

    procedure CountTypeCreate;
    procedure CountTypeSelect;

    procedure ColorTypeCreate;
    procedure ColorTypeSelect;

    procedure ScheduleDraw(const AZoomPercent: Integer);

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  ShiftScheduleForm: TShiftScheduleForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TShiftScheduleForm }

procedure TShiftScheduleForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TShiftScheduleForm.FormCreate(Sender: TObject);
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

  CanDrawSchedule:= False;
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

procedure TShiftScheduleForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(CountType);
  FreeAndNil(ColorType);

  FreeAndNil(ScheduleList);

  FreeAndNil(Structure);
  FreeAndNil(VSTDays);
  FreeAndNil(VSTCopy);
end;

procedure TShiftScheduleForm.FormShow(Sender: TObject);
var
  H: Integer;
begin
  H:= MainPanel.Height div 3;
  EditingPanel.Height:= H;
  StructurePanel.Height:= H;
end;

procedure TShiftScheduleForm.EditingTablesCreate;
var
  i: Integer;
  W: TIntVector;
begin
  W:= VCreateInt([70, 80, 90, 70, 60]);

  Structure:= TVSTTable.Create(StructureVT);
  Structure.SetSingleFont(MainForm.GridFont);
  Structure.HeaderFont.Style:= [fsBold];
  Structure.CanSelect:= True;
  for i:= 0 to High(W) do
    Structure.AddColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[i], W[i]);
  Structure.Draw;

  VSTDays:= TVSTTable.Create(DayVT);
  //VSTDays.OnSelect:= @CorrectionSelect;
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

procedure TShiftScheduleForm.StructureLoad;
var
  Dates: TDateVector;
  TotalHours, NightHours, ShiftNums: TIntVector;
  StrMarks, StrDates, StrShiftNums: TStrVector;
begin
  Structure.ValuesClear;
  StructureCaptionPanel.Caption:= 'Структура: ';

  if not ScheduleList.IsSelected then Exit;

  StructureCaptionPanel.Caption:= StructureCaptionPanel.Caption +
                                  ScheduleNames[ScheduleList.SelectedIndex];


  DataBase.ScheduleCycleLoad(ScheduleIDs[ScheduleList.SelectedIndex],
                             Dates, TotalHours, NightHours, ShiftNums, StrMarks);

  StrShiftNums:= VIntToStr(ShiftNums);
  VChangeIf(StrShiftNums, '0', EMPTY_MARK);
  if CycleCounts[ScheduleList.SelectedIndex]=0 then
    StrDates:= VCreateStr(WEEKDAYSSHORT)
  else
    StrDates:= VDateToStr(Dates);

  Structure.Visible:= False;
  try
    Structure.ValuesClear;
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[0], StrDates);
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], VWorkHoursToStr(TotalHours));
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursToStr(NightHours));
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], StrMarks);
    Structure.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], StrShiftNums);
    Structure.Draw;
  finally
    Structure.Visible:= True;
  end;
end;

procedure TShiftScheduleForm.ScheduleListCreate;
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

procedure TShiftScheduleForm.ScheduleListSelect;
begin
  StructureLoad;
end;

procedure TShiftScheduleForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Параметры отображения:';
  V:= VCreateStr([
    'Учитывать корректировки графика',
    'Отображать строку ночных часов',
    'Коды табеля для нерабочих дней',
    'Не использовать цвета'
  ]);
  ParamList:= TVSTCheckList.Create(ParamListVT, S, V, @ParamListSelect);
end;

procedure TShiftScheduleForm.ParamListSelect;
begin

end;

procedure TShiftScheduleForm.CountTypeCreate;
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
  CountType:= TVSTStringList.Create(CountTypeVT, S, @CountTypeSelect);
  CountType.Update(V);
end;

procedure TShiftScheduleForm.CountTypeSelect;
begin

end;

procedure TShiftScheduleForm.ColorTypeCreate;
var
  S: String;
  V: TStrVector;
begin
  S:= 'Выделять цветом нерабочие дни:';
  V:= VCreateStr([
    'по графику сменности',
    'по производственному календарю'
  ]);
  ColorType:= TVSTStringList.Create(ColorTypeVT, S, @ColorTypeSelect);
  ColorType.Update(V);
end;

procedure TShiftScheduleForm.ColorTypeSelect;
begin

end;

procedure TShiftScheduleForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  try
    ZoomPercent:= AZoomPercent;
    //CalendarSheet.Zoom(ZoomPercent);
    //CalendarSheet.Draw(Calendar, SelectedDates);
    //CalendarSheet.ColorsUpdate(Colors);
  finally
    ViewGrid.Visible:= True;
  end;
end;

procedure TShiftScheduleForm.ScheduleListLoad(const SelectedID: Integer = -1);
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

procedure TShiftScheduleForm.ScheduleListDelItem;
begin

end;

procedure TShiftScheduleForm.SettingsLoad;
begin

end;

procedure TShiftScheduleForm.SettingsSave;
begin

end;

procedure TShiftScheduleForm.ViewUpdate(const AModeType: TModeType);
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

