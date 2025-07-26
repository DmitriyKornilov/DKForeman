unit UScheduleSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet, fpstypes,
  LCLType, Controls, DateUtils, Grids,
  //Project utils
  UTimingUtils, UConst, UTypes, UCalendar, UWorkHours, USchedule, UTimingSheet,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, DK_DateUtils, DK_StrUtils, DK_SheetTypes,
  DK_SheetConst, DK_Math, DK_Color;

type

  { TVacationStatSheet }

  TVacationStatSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  public
    procedure Draw;
    procedure Update(const ACounts: TIntVector);
  end;

  { TVacationPlanSheet }

  TVacationPlanSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      NAME_COLUMN_WIDTH = 150;
      TABNUM_COLUMN_WIDTH = 100;
      DATE_COLUMN_WIDTH = 130;
      DAY_COLUMN_WIDTH = 35;
    var
      FFirstDateCol: Integer;
      FLastRow: Integer;
      FCalendar: TCalendar;
      FSelectedRowIndex, FSelectedCol: Integer;
      FOnSelect: TSheetEvent;
      FClickedRow: Integer;

      FPlan1Dates, FPlan2Dates: TDateVector;
      FPlan1Counts, FPlan1AddCounts, FPlan2Counts, FPlan2AddCounts: TIntVector;

    function DateToIndex(const ADate: TDate): Integer;
    function VacationEndDate(const ABeginDate: TDate; const ADaysCount: Integer): TDate;
    function IsVacationDayCell(const ARow, ACol: Integer; out APart: Integer): Boolean;

    function PartToDateCol(const APart: Integer): Integer;
    function DateColToPart(const ACol: Integer): Integer;

    procedure CaptionDraw;

    procedure StaffLineDraw(const AIndex: Integer;
                            const AStaffName, ATabNum: String);
    procedure ScheduleLineDraw(const AIndex: Integer;
                               const ASchedule: TPersonalSchedule);

    procedure MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; {%H-}aState: TGridDrawState);
  public
    constructor Create(const ACalendar: TCalendar;
                       const AWorksheet: TsWorksheet; const AGrid: TsWorksheetGrid;
                       const AFont: TFont);

    procedure Draw(const AStaffNames, ATabNums: TStrVector;
         const ASchedules: TPersonalScheduleVector;
         const APlan1Dates, APlan2Dates: TDateVector;
         const APlan1Counts, APlan1AddCounts, APlan2Counts, APlan2AddCounts: TIntVector);

    procedure BeginDraw;
    procedure EndDraw(const ARowCount: Integer);

    procedure LineDraw(const AIndex: Integer;
                 const AStaffName, ATabNum: String;
                 const ASchedule: TPersonalSchedule;
                 const APlan1Date, APlan2Date: TDate;
                 const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer);
    procedure VacationLineDraw(const AIndex: Integer;
                const APlan1Date, APlan2Date: TDate;
                const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer);

    function ColToIndex(const ACol: Integer): Integer;
    function IndexToCol(const AIndex: Integer): Integer;
    function IndexToRow(const AIndex: Integer): Integer;
    function RowToIndex(const ARow: Integer): Integer;

    procedure Select(const ARowIndex, ACol: Integer);
    procedure Unselect(const ANeedDoEvent: Boolean);
    function IsSelected: Boolean;
    function SelectedPart: Integer;
    property SelectedIndex: Integer read FSelectedRowIndex;
    property SelectedCol: Integer read FSelectedCol;
    property ClickedRow: Integer read FClickedRow;
    property OnSelect: TSheetEvent read FOnSelect write FOnSelect;
  end;

  { TVacationScheduleSheet }
  //График отпусков (Форма Т-7)
  TVacationScheduleSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    FCompany, FDepartment: String;
    FYear: Word;
    FStaffNames: TStrVector;
    FTabNums: TStrVector;
    FPostNames: TStrVector;
    FFirstDates: TDateVector;
    FTotalCounts: TIntVector;
    procedure CaptionDraw(out ARow: Integer);
    procedure LineDraw(var ARow: Integer; const AIndex: Integer);
    procedure BottomDraw(const ARow: Integer);
  public
    constructor Create(const AWorksheet: TsWorksheet; const AGrid: TsWorksheetGrid;
                       const AFont: TFont);
    procedure Draw(const ACompany, ADepartment: String;
                   const AYear: Word;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const AFirstDates: TDateVector;
                   const ATotalCounts: TIntVector);
  end;

  { TTableScheduleSheet }
  //базовый класс для графика в виде таблицы
  TTableScheduleSheet = class (TDateSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      FWriteTotalIfZero = False;
      FWriteNightIfZero = False;
      FWriteDaysCountIfZero = True;
      FWriteSumTotalIfZero = True;
      FWriteSumNightIfZero = True;
      COLUMNS_COUNT        = 36;
      DAY_COLUMN_WIDTH     = 33;    //ширина столбцов дней месяца
      PERIOD_COLUMN_WIDTH  = 70;    //ширина столбца "Месяц"
      SUMDAYS_COLUMN_WITH  = 35;    //--кол-ва дней
      SUMHOURS_COLUMN_WITH = 50;    //--кол-ва часов
      ROW_DEFAULT_HEIGHT   = 24;
      //FIRSTROW_TITLE_HEIGHT = 50; 3*SHeight
    var
      FCalendar: TCalendar;
      FNeedNight: Boolean;
      FNeedCorrect: Boolean;
      FScheduleNotWorkColor: Boolean;
      FNeedMarks: Boolean;
      FResumeType: Byte;

    function IsNeedCaption: Boolean;

    function GetCaption: String; virtual; abstract;
    function GetPeriodColumnName: String; virtual; abstract;
    procedure CaptionDraw;
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены});
  end;

  { TCustomYearScheduleSheet }
  // Годовой график в виде таблицы
  TCustomYearScheduleSheet = class (TTableScheduleSheet)
  private
    FYear: Word;
    FCaption: String;
    procedure BlankDraw;

    function GetPeriodColumnName: String; override;

    function RowToMonth(const ARow: Integer): Integer;
    function ColToDay(const ACol, AMonth: Integer): Integer;
    function DateToRow(const ADate: TDate): Integer;
    function DateToCol(const ADate: TDate): Integer;
  public
    function GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean; override;
    function DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean; override;

    procedure Select(const ADate: TDate); override;
    procedure Unselect(const ADate: TDate); override;
  end;

  { TShiftYearScheduleSheet }
  // Годовой график сменности в виде таблицы
  TShiftYearScheduleSheet = class (TCustomYearScheduleSheet)
  private
    FSchedule: TShiftSchedule;
    procedure ScheduleDraw;
    function GetCaption: String; override;
  public
    procedure Draw(const ACalendar: TCalendar;
                   const ASchedule: TShiftSchedule;
                   const AName: String;
                   const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean);
  end;

  { TPersonalYearScheduleSheet }
  // Годовой персональный график в виде таблицы
  TPersonalYearScheduleSheet = class (TCustomYearScheduleSheet)
  private
    FSchedule: TPersonalSchedule;
    FNeedVacation: Boolean;
    FUseWorkPeriodInLoadNormHoursAndDays: Boolean; //если True, то кол-во
                                                   //рабочих дней и часов по норме
                                                   //буду обрезаны периодом работы [RecrutDate, DismissDate]
    procedure ScheduleDraw;
    function GetCaption: String; override;
  public
    procedure Draw(const ACalendar: TCalendar;
      const ASchedule: TPersonalSchedule;
      const AName: String;
      const ANeedNight, ANeedCorrect, ANeedMarks, ANeedVacation, AScheduleNotWorkColor: Boolean;
      const AUseWorkPeriodInLoadNormHoursAndDays: Boolean = False);
  end;

  { TShiftMonthScheduleSheet }
  //Сводная таблица графиков сменности на месяц
  TShiftMonthScheduleSheet = class (TTableScheduleSheet)
  private
    FSchedules: TShiftScheduleVector;
    FVisible: TBoolVector;
    FNames: TStrVector;
    procedure BlankDraw;
    procedure ScheduleDraw;
    function GetCaption: String; override;
    function GetPeriodColumnName: String; override;
  public
    procedure Draw(const ACalendar: TCalendar;
                   const ASchedules: TShiftScheduleVector;
                   const ANames: TStrVector;
                   const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean;
                   const AVisible: TBoolVector = nil);
    //not used
    function GridToDate(const {%H-}ARow, {%H-}ACol: Integer; out ADate: TDate): Boolean; override;
    function DateToGrid(const {%H-}ADate: TDate; out ARow, ACol: Integer): Boolean; override;
  end;


  { TPersonalMonthScheduleSheet }
  //Сводная таблица персональных графиков на месяц
  TPersonalMonthScheduleSheet = class (TMonthSheet)
  protected
    function SetWidths: TIntVector; override;
    procedure CaptionDraw(var R, C: Integer); override;
    procedure SelectDate(const ADate: TDate); override;
  private
    const
      DATE_COLUMN_WIDTH = 70;
      SIGN_COLUMN_WIDTH = 70;
    var
      FSchedules: TPersonalScheduleVector;
      FBeforeSchedules: TPersonalScheduleVector;
      FSignatureType: Byte;

    procedure SignatureListDraw;
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены};
                       const APeriodType: Byte {0-год, 1-квартал, 2-месяц};
                       const ASignatureType: Byte {0-нет, 1-столбцы дата/подпись, 2-список под таблицей};
                       const AExtraColumns: TBoolVector
                        //AExtraColumns:
                        //0 - Порядковый номер,
                        //1 - Должность (профессия)
                        //2 - Табельный номер
                        //3 - Количество дней/часов за месяц
                        //4 - Сумма часов за учетный период
                        //5 - Норма часов за учетный период
                        //6 - Отклонение от нормы часов
                       );

    procedure Draw(const ACalendar: TCalendar;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                    //AViewParams:
                    //0 - Отображать строку ночных часов,
                    //1 - Учитывать корректировки графика
                    //2 - Коды табеля для нерабочих дней
                    //3 - Учитывать отпуск
                   const AExportParams: TBoolVector;
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   const ASchedules, ABeforeSchedules: TPersonalScheduleVector
                   );
    procedure LineDraw(const AIndex: Integer); override;
  end;

  { TShiftCalendarScheduleSheet }
  //Годовой график сменности в виде календаря
  TShiftCalendarScheduleSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      COL_WIDTH = 30;
      COLUMNS_COUNT = 23;
      MONTH_FIRST_ROWS:  array [1..12] of Byte = (3,3,3,12,12,12,21,21,21,30,30,30);
      MONTH_FIRST_COLS:  array [1..12] of Byte = (1,9,17,1,9,17,1,9,17,1,9,17);
    var
      FSchedule: TShiftSchedule;
      FCalendar: TCalendar;
      FNeedCorrect, FFirstShiftDayColorOnly: Boolean;
      FYear: Word;
      FPrevShiftNumber: Integer;
    procedure CaptionDraw(const AName: String);
    procedure MonthDraw(const AMonth: Byte);
  public
    procedure Draw(const ACalendar: TCalendar;
                   const ASchedule: TShiftSchedule;
                   const APrevShiftNumber: Integer;
                   const AName: String;
                   const ANeedCorrect,                            //учитывать корректировки
                         AFirstShiftDayColorOnly: Boolean);       //раскаршивать только первый день переходящей смены
  end;

  {Список графиков сменности для назначения работнику }
  { TShiftSimpleScheduleSheet }

  TShiftSimpleScheduleSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      NAME_COLUMN_WIDTH = 100;
      DAY_COLUMN_WIDTH = 60;
    var
      FSchedules: TShiftScheduleVector;
      FNames: TStrVector;
      FSelectedIndex: Integer;
    procedure CaptionDraw;
    procedure LineDraw(const AIndex: Integer; const ASelected: Boolean);
    function GetIsSelected: Boolean;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure KeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure SelectionMove(const ADirection: TMoveDirection);
    procedure SetSelectedIndex(const AValue: Integer);
  public
    constructor Create(const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const ANames: TStrVector);
    procedure Draw(const ASchedules: TShiftScheduleVector);
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property IsSelected: Boolean read GetIsSelected;
  end;


  procedure ChooseShiftScheduleData(const TSS: TShiftSchedule; const ANeedCorrect: Boolean;
                                    out WH: TWorkHours;
                                   out DC,SC: Integer; out Marks: TStrVector);

implementation

procedure ChooseShiftScheduleData(const TSS: TShiftSchedule; const ANeedCorrect: Boolean;
                                  out WH: TWorkHours; out DC, SC: Integer; out Marks: TStrVector);
begin
  if ANeedCorrect then
  begin
    WH:= TSS.HoursCorrect;
    DC:= TSS.DaysCountCorrect;
    SC:= TSS.ShiftCountCorrect;
    Marks:= VCut(TSS.MarkSTRCorrect);
  end
  else begin
    WH:= TSS.HoursDefault;
    DC:= TSS.DaysCountDefault;
    SC:= TSS.ShiftCountDefault;
    Marks:= VCut(TSS.MarkSTRDefault);
  end;
end;

{ TPersonalMonthScheduleSheet }

function TPersonalMonthScheduleSheet.SetWidths: TIntVector;
begin
  Result:= inherited SetWidths;
  if FSignatureType=1 then
  begin
    VAppend(Result, DATE_COLUMN_WIDTH);
    VAppend(Result, SIGN_COLUMN_WIDTH);
  end;
end;

procedure TPersonalMonthScheduleSheet.CaptionDraw(var R, C: Integer);
begin
  inherited CaptionDraw(R, C);
  if FSignatureType=1 then
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R, C+1, 'Ознакомлен', cbtOuter);
    Writer.WriteText(R+1, C, 'дата', cbtOuter);
    Writer.WriteText(R+1, C+1, 'подпись', cbtOuter);
  end;
end;

procedure TPersonalMonthScheduleSheet.SignatureListDraw;
var
  i, N, R, C: Integer;

  procedure SignatureLineDraw(const ARow, ACol: Integer; const AStaffName: String);
  begin
    Writer.SetAlignment(haLeft, vaBottom);
    Writer.WriteText(ARow, ACol+1, ARow, ACol+4, AStaffName);
    Writer.WriteText(ARow, ACol+5, ARow, ACol+6, EmptyStr, cbtBottom);
    Writer.SetAlignment(haRight, vaBottom);
    Writer.WriteText(ARow, ACol+7, '«');
    Writer.WriteText(ARow, ACol+8,  EmptyStr, cbtBottom);
    Writer.SetAlignment(haLeft, vaBottom);
    Writer.WriteText(ARow, ACol+9, '»');
    Writer.WriteText(ARow, ACol+10, ARow, ACol+12, EmptyStr, cbtBottom);
    Writer.SetAlignment(haRight, vaBottom);
    Writer.WriteText(ARow, ACol+13, '20');
    Writer.WriteText(ARow, ACol+14,  EmptyStr, cbtBottom);
    Writer.SetAlignment(haLeft, vaBottom);
    Writer.WriteText(ARow, ACol+15, 'г.');
  end;

begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  N:= Length(FStaffNames);
  R:= 3 + (1+Ord(FViewParams[0]))*N + 1;
  C:= 1 + Ord(FExtraColumns[0]) + Ord(FExtraColumns[1]) + Ord(FExtraColumns[2]);
  if (N mod 2)=0 then
    N:= N div 2
  else
    N:= (N div 2) + 1;
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteText(R, 1, R, C,  'Ознакомлен:');
  R:= R + 1;
  for i:= 0 to N-1 do
    SignatureLineDraw(R+i*2, C, FStaffNames[i]);
  C:= C+16;
  for i:= N to High(FStaffNames) do
    SignatureLineDraw(R+(i-N)*2, C, FStaffNames[i]);
end;

procedure TPersonalMonthScheduleSheet.SelectDate(const ADate: TDate);
begin
  if IsRowSelected and
     (FSchedules[FSelectedRowIndex1].IsExists[DayOf(ADate)-1]=EXISTS_NO) then Exit;
  inherited SelectDate(ADate);
end;

procedure TPersonalMonthScheduleSheet.LineDraw(const AIndex: Integer);
var
  R, C, i, n, d, s, SHTotal, SHNight: Integer;
  BeforeSchedule: TPersonalSchedule;
  WorkHours, WorkHoursBefore: TWorkHours;
  Marks: TStrVector;
begin
  UnSelectDate;

  if AIndex in [FSelectedRowIndex1, FSelectedRowIndex2] then
    Writer.SetBackground(DefaultSelectionBGColor)
  else
    Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  C:= 0;
  n:= Ord(FViewParams[0]); //0 - Отображать строку ночных часов
  R:= IndexToRow(AIndex);

  if FExtraColumns[0] then //0 - Порядковый номер
  begin
    Writer.SetAlignment(haCenter, vaCenter);
    C:= C + 1;
    Writer.WriteNumber(R, C, R+n, C, AIndex+1, cbtOuter);
  end;

  Writer.SetAlignment(haLeft, vaCenter);
  C:= C + 1;
  Writer.WriteText(R, C, R+n, C, FStaffNames[AIndex], cbtOuter);

  if FExtraColumns[1] then //1 - Должность (профессия)
  begin
    Writer.SetAlignment(haLeft, vaCenter);
    C:= C + 1;
    Writer.WriteText(R, C, R+n, C, FPostNames[AIndex], cbtOuter);
  end;

  if FExtraColumns[2] then //2 - Табельный номер
  begin
    Writer.SetAlignment(haCenter, vaCenter);
    C:= C + 1;
    Writer.WriteText(R, C, R+n, C, FTabNums[AIndex], cbtOuter);
  end;

  ChoosePersonalScheduleData(FSchedules[AIndex],
                             FViewParams[1], //1 - Учитывать корректировки графика
                             FViewParams[3], //3 - Учитывать отпуск
                             WorkHours, d,s, Marks);
  Writer.SetAlignment(haCenter, vaCenter);
  for i:= 0 to FCalendar.DaysCount - 1 do
    DrawPersonalHoursOrMarks(Writer, R, C+i+1,
                FSchedules[AIndex].IsExists[i], FSchedules[AIndex].IsDefines[i],
                FSchedules[AIndex].IsVacations[i],
                WorkHours.Totals[i], WorkHours.Nights[i], Marks[i], EmptyStr,
                FWriteTotalIfZero, FWriteNightIfZero,
                FViewParams[0], //0 - Отображать строку ночных часов
                FViewParams[2], //2 - Коды табеля для нерабочих дней
                FViewParams[3], //3 - Учитывать отпуск
                FSchedules[AIndex].StrMarkVacationMain,
                FSchedules[AIndex].StrMarkVacationAddition,
                FSchedules[AIndex].StrMarkVacationHoliday);
  for i:= FCalendar.DaysCount to 30 do
    Writer.WriteText(R, C+i+1, STRMARK_NONEXISTENT, cbtOuter);
  if FViewParams[0] then //0 - Отображать строку ночных часов
    for i:= FCalendar.DaysCount to 30 do
      Writer.WriteText(R+1, C+i+1, STRMARK_NONEXISTENT, cbtOuter);

  C:= C+31;
  if FExtraColumns[3] then  //3 - Количество дней/часов за месяц
  begin
    C:= C + 1;
    case FResumeType of
    0: if (d>0) or FWriteDaysCountIfZero then
         Writer.WriteNumber(R, C, R+n, C, d, cbtOuter)
       else
         {%H-}Writer.WriteText(R, C, R+n, C, EmptyStr, cbtOuter);
    1: if (s>0) or FWriteDaysCountIfZero then
         Writer.WriteNumber(R, C, R+n, C, s, cbtOuter)
       else
         {%H-}Writer.WriteText(R, C, R+n, C, EmptyStr, cbtOuter);
    2: begin
         if (d>0) or FWriteDaysCountIfZero then
           Writer.WriteNumber(R, C, R+n, C, d, cbtOuter)
         else
           {%H-}Writer.WriteText(R, C, R+n, C, EmptyStr, cbtOuter);
         C:= C + 1;
         if (s>0) or FWriteDaysCountIfZero then
           Writer.WriteNumber(R, C, R+n, C, s, cbtOuter)
         else
           {%H-}Writer.WriteText(R, C, R+n, C, EmptyStr, cbtOuter);
       end;
    end;
    C:= C + 1;
    DrawHours(Writer, R, C, WorkHours.SumTotal, WorkHours.SumNight,
              FWriteSumTotalIfZero, FWriteSumNightIfZero,
              FViewParams[0]); //0 - Отображать строку ночных часов

  end;

  SHTotal:= WorkHours.SumTotal;
  SHNight:= WorkHours.SumNight;
  BeforeSchedule:= nil;
  if Assigned(FBeforeSchedules) then
    BeforeSchedule:= FBeforeSchedules[AIndex];
  if (FPeriodType<2) and Assigned(BeforeSchedule) then
  begin
    ChoosePersonalScheduleData(BeforeSchedule,
                               FViewParams[1], //1 - Учитывать корректировки графика
                               FViewParams[3], //3 - Учитывать отпуск
                               WorkHoursBefore, d,s, Marks);
    SHTotal:= WorkHours.SumTotal + WorkHoursBefore.SumTotal;
    SHNight:= WorkHours.SumNight + WorkHoursBefore.SumNight;
  end;

  if (FPeriodType<2) and FExtraColumns[4] then  //4 - Сумма часов за учетный период
  begin
    C:= C + 1;
    DrawHours(Writer, R, C, SHTotal, SHNight,
              FWriteSumTotalIfZero, FWriteSumNightIfZero,
              FViewParams[0]); //0 - Отображать строку ночных часов
  end;

  if FExtraColumns[5] then  //5 - Норма часов за учетный период
  begin
    C:= C + 1;
    WriteCellHours(Writer, R, C, R+n, C, FNormHours[AIndex]);
  end;

  if FExtraColumns[6] then //6 - Отклонение от нормы часов
  begin
    C:= C + 1;
    WriteCellHours(Writer, R, C, R+n, C, SHTotal-FNormHours[AIndex]);
  end;

  if FSignatureType=1 then
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R+n, C, EmptyStr, cbtOuter);
    C:= C + 1;
    Writer.WriteText(R, C, R+n, C, EmptyStr, cbtOuter);
  end;

  if not FViewParams[0] then //0 - Отображать строку ночных часов
    Writer.SetRowHeight(R, Trunc(1.6*Writer.RowHeightDefault));
end;

constructor TPersonalMonthScheduleSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены};
                       const APeriodType: Byte {0-год, 1-квартал, 2-месяц};
                       const ASignatureType: Byte {0-нет, 1-столбцы дата/подпись, 2-список под таблицей};
                       const AExtraColumns: TBoolVector
                        //AExtraColumns:
                        //0 - Порядковый номер,
                        //1 - Должность (профессия)
                        //2 - Табельный номер
                        //3 - Количество дней/часов за месяц
                        //4 - Сумма часов за учетный период
                        //5 - Норма часов за учетный период
                        //6 - Отклонение от нормы часов
                       );
begin
  FSignatureType:= ASignatureType;
  inherited Create(AWorksheet, AGrid, AFont, AResumeType, APeriodType, AExtraColumns);
  FResumeCaption:= 'По графику';
end;

procedure TPersonalMonthScheduleSheet.Draw(
                       const ACalendar: TCalendar;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                    //AViewParams:
                    //0 - Отображать строку ночных часов
                    //1 - Учитывать корректировки графика
                    //2 - Коды табеля для нерабочих дней
                    //3 - Учитывать отпуск
                   const AExportParams: TBoolVector;
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   const ASchedules, ABeforeSchedules: TPersonalScheduleVector
                   );
begin
  SelectionClear;

  FSchedules:= ASchedules;
  FBeforeSchedules:= ABeforeSchedules;

  DrawCustom(ACalendar, AStaffNames, ATabNums, APostNames,
                 ANormHours, AViewParams, AExportParams);

  if FSignatureType=2 then //2-список ознакомления под таблицей
    SignatureListDraw;

  Writer.EndEdit;
end;

{ TPersonalYearScheduleSheet }

procedure TPersonalYearScheduleSheet.ScheduleDraw;
var
  R, C, DeltaR, m: Integer;
  AYear: Word;
  BD, ED: TDate;
  CutCalendar: TCalendar;
  CutSchedule: TPersonalSchedule;
  WorkHours: TWorkHours;
  Marks: TStrVector;

  procedure DrawMonth(ARow, ACol, AMonth: Word);
  var
    i, d, h, s: Integer;
  begin
    FirstLastDayInMonth(AMonth,AYear,BD,ED);
    FCalendar.Cut(BD, ED, CutCalendar);
    FSchedule.Cut(BD, ED, CutSchedule);
    ChoosePersonalScheduleData(CutSchedule, FNeedCorrect, FNeedVacation, WorkHours, d,s, Marks);
    for i:= 0 to CutCalendar.DaysCount - 1 do
    begin
      if CutSchedule.IsExists[i]=EXISTS_NO then
         AddScheduleColorIndex(Writer, ARow, ACol+i, OUTSIDEMONTH_COLOR_INDEX, FNeedNight)
      else begin
        if CutSchedule.IsDefines[i]=DEFINE_NO then
          AddScheduleColorIndex(Writer, ARow, ACol+i, NOTDEFINE_COLOR_INDEX, FNeedNight)
        else begin
          if FNeedCorrect and (CutSchedule.IsCorrections[i]=CORRECTION_YES) and
             (CutSchedule.IsVacations[i]=VACATION_NO) then
            AddScheduleColorIndex(Writer, ARow, ACol+i, CORRECT_COLOR_INDEX, FNeedNight)
          else begin
            if FScheduleNotWorkColor then
            begin
              if WorkHours.Totals[i]=0 then
                AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
            end
            else begin
              if (CutCalendar.DayStatuses[i]=DAY_STATUS_HOLIDAY) or
                 (CutCalendar.DayStatuses[i]=DAY_STATUS_OFFDAY) then
                 AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
            end;
          end;
        end;
      end;

      DrawPersonalHoursOrMarks(Writer, ARow, ACol+i,
                CutSchedule.IsExists[i], CutSchedule.IsDefines[i], CutSchedule.IsVacations[i],
                WorkHours.Totals[i], WorkHours.Nights[i], Marks[i], EmptyStr,
                FWriteTotalIfZero, FWriteNightIfZero, FNeedNight, FNeedMarks, FNeedVacation,
                CutSchedule.StrMarkVacationMain, CutSchedule.StrMarkVacationAddition, CutSchedule.StrMarkVacationHoliday);

    end;
    for i:= CutCalendar.DaysCount to 30 do
    begin
      DrawMonthOutside(Writer, ARow, ACol+i, FNeedNight);
      AddScheduleColorIndex(Writer, ARow, ACol+i, OUTSIDEMONTH_COLOR_INDEX, FNeedNight);
    end;
    DrawDaysOrShiftCount(Writer,  ARow, ACol+31, d, s, FWriteDaysCountIfZero, FNeedNight, FResumeType, i);
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    if FUseWorkPeriodInLoadNormHoursAndDays then
      NormHoursAndWorkDaysCounInPeriod(FSchedule.TabNumID, BD, ED,
                CutCalendar, d, h, FSchedule.RecrutDate, FSchedule.DismissDate)
    else
      NormHoursAndWorkDaysCounInPeriod(FSchedule.TabNumID, BD, ED, CutCalendar, d, h);
    DrawDaysCount(Writer, ARow, ACol+33+i, d, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, h, 0, FWriteSumTotalIfZero, False, FNeedNight);
  end;

  procedure DrawQuarter(ARow, ACol, AQuart: Word);
  var
    d,i,s,h: Integer;
  begin
    FirstLastDayInQuarter(AQuart,AYear,BD,ED);
    FCalendar.Cut(BD, ED, CutCalendar);
    FSchedule.Cut(BD, ED, CutSchedule);
    ChoosePersonalScheduleData(CutSchedule, FNeedCorrect, FNeedVacation, WorkHours, d,s, Marks);
    DrawDaysOrShiftCount(Writer, ARow, ACol+31, d, s, FWriteDaysCountIfZero, FNeedNight, FResumeType, i);
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    if FUseWorkPeriodInLoadNormHoursAndDays then
      NormHoursAndWorkDaysCounInPeriod(FSchedule.TabNumID, BD, ED,
                CutCalendar, d, h, FSchedule.RecrutDate, FSchedule.DismissDate)
    else
      NormHoursAndWorkDaysCounInPeriod(FSchedule.TabNumID, BD, ED, CutCalendar, d, h);
    DrawDaysCount(Writer, ARow, ACol+33+i, d, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, h, 0, FWriteSumTotalIfZero, False, FNeedNight);
  end;

  procedure DrawYear(ARow, ACol: Word);
  var
    d,i,s,h: Integer;
  begin
    ChoosePersonalScheduleData(FSchedule, FNeedCorrect, FNeedVacation, WorkHours, d,s, Marks);
    DrawDaysOrShiftCount(Writer, ARow, ACol+31, d, s, FWriteDaysCountIfZero, FNeedNight, FResumeType, i);
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    FirstLastDayInYear(AYear, BD, ED);
    if FUseWorkPeriodInLoadNormHoursAndDays then
      NormHoursAndWorkDaysCounInPeriod(FSchedule.TabNumID, BD, ED, FCalendar,
                             d, h, FSchedule.RecrutDate, FSchedule.DismissDate)
    else
      NormHoursAndWorkDaysCounInPeriod(FSchedule.TabNumID, BD, ED, FCalendar, d, h);
    DrawDaysCount(Writer, ARow, ACol+33+i, d, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, h, 0, FWriteSumTotalIfZero, False, FNeedNight);
  end;

begin
  CutCalendar:= TCalendar.Create;
  try
    CutSchedule:= TPersonalSchedule.Create;
    try
      Writer.SetFont(Font.Name, Font.Size, [], clBlack);
      AYear:= YearOfDate(FSchedule.BeginDate);
      DeltaR:= 1 + Ord(FNeedNight);
      R:= 3 + Ord(not Writer.HasGrid);
      C:= 2;
      for m:= 1 to 3 do
      begin
        DrawMonth(R,C,m);
        R:= R + DeltaR;
      end;
      DrawQuarter(R,C,1);
      R:= R + DeltaR;
      for m:= 4 to 6 do
      begin
        DrawMonth(R,C,m);
        R:= R + DeltaR;
      end;
      DrawQuarter(R,C,2);
      R:= R + DeltaR;
      for m:= 7 to 9 do
      begin
        DrawMonth(R,C,m);
        R:= R + DeltaR;
      end;
      DrawQuarter(R,C,3);
      R:= R + DeltaR;
      for m:= 10 to 12 do
      begin
        DrawMonth(R,C,m);
        R:= R + DeltaR;
      end;
      DrawQuarter(R,C,4);
      R:= R + DeltaR;
      DrawYear(R,C);
    finally
      FreeAndNil(CutSchedule);
    end;
  finally
    FreeAndNil(CutCalendar);
  end;
end;

function TPersonalYearScheduleSheet.GetCaption: String;
begin
  Result:= EmptyStr;
  if SEmpty(FCaption) then Exit;
  Result:= FCaption + ': график работы на ' + IntToStr(FYear) + ' год';
end;

procedure TPersonalYearScheduleSheet.Draw(const ACalendar: TCalendar;
  const ASchedule: TPersonalSchedule;
  const AName: String;
  const ANeedNight, ANeedCorrect, ANeedMarks, ANeedVacation, AScheduleNotWorkColor: Boolean;
  const AUseWorkPeriodInLoadNormHoursAndDays: Boolean = False);
var
  i: Integer;
begin
  FCaption:= AName;
  FCalendar:= ACalendar;
  FSchedule:= ASchedule;
  FYear:= YearOfDate(FSchedule.BeginDate);
  FNeedNight:= ANeedNight;
  FNeedCorrect:= ANeedCorrect;
  FNeedMarks:= ANeedMarks;
  FNeedVacation:= ANeedVacation;
  FScheduleNotWorkColor:= AScheduleNotWorkColor;
  FUseWorkPeriodInLoadNormHoursAndDays:= AUseWorkPeriodInLoadNormHoursAndDays;

  Writer.BeginEdit;
  CaptionDraw;
  BlankDraw;
  ScheduleDraw;
  if Writer.HasGrid then
    Writer.SetFrozenRows(2);
  Writer.EndEdit;

  BordersDraw(1 + Ord(IsNeedCaption));
  for i:= 2+Ord(IsNeedCaption) to Writer.RowCount do
    Writer.SetRowHeight(i, ROW_DEFAULT_HEIGHT);
end;

{ TCustomYearScheduleSheet }

procedure TCustomYearScheduleSheet.BlankDraw;
const
  C1= 2;
var
  DeltaR, R, C, i, j, C2: Integer;
begin
  C2:= COLUMNS_COUNT + Ord(FResumeType=2);
  DeltaR:= 1 + Ord(FNeedNight);
  R:= 3 + Ord(IsNeedCaption);
  C:= 1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  for i:= R to R+3*DeltaR-1 do
    for j:= C1 to C2 do
      Writer.WriteText(i, j, EmptyStr, cbtOuter);
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Январь', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Февраль', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Март', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'I квартал', cbtOuter);
  Writer.WriteText(R, C+1, R+DeltaR-1, C+31, EmptyStr, cbtOuter);
  for i:= R to R+DeltaR-1 do
    for j:= C to C2 do
      Writer.AddCellBGColorIndex(i, j, TITLE_COLOR_INDEX);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Апрель', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Май', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Июнь', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'II квартал', cbtOuter);
  Writer.WriteText(R, C+1, R+DeltaR-1, C+31, EmptyStr, cbtOuter);
  for i:= R to R+DeltaR-1 do
    for j:= C to C2 do
      Writer.AddCellBGColorIndex(i, j, TITLE_COLOR_INDEX);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Июль', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Август', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Сентябрь', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'III квартал', cbtOuter);
  Writer.WriteText(R, C+1, R+DeltaR-1, C+31, EmptyStr, cbtOuter);
  for i:= R to R+DeltaR-1 do
    for j:= C to C2 do
      Writer.AddCellBGColorIndex(i, j, TITLE_COLOR_INDEX);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Октябрь', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Ноябрь', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Декабрь', cbtOuter);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'IV квартал', cbtOuter);
  Writer.WriteText(R, C+1, R+DeltaR-1, C+31, EmptyStr, cbtOuter);
  for i:= R to R+DeltaR-1 do
    for j:= C to C2 do
      Writer.AddCellBGColorIndex(i, j, TITLE_COLOR_INDEX);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, IntToStr(FYear) + ' год', cbtOuter);
  Writer.WriteText(R, C+1, R+DeltaR-1, C+31, EmptyStr, cbtOuter);
end;

function TCustomYearScheduleSheet.GetPeriodColumnName: String;
begin
  Result:= 'Месяц';
end;

function TCustomYearScheduleSheet.RowToMonth(const ARow: Integer): Integer;
begin
  Result:= 0;
  if FNeedNight then
  begin
    case ARow of
    3,4: Result:= 1;
    5,6: Result:= 2;
    7,8: Result:= 3;
    11,12: Result:= 4;
    13,14: Result:= 5;
    15,16: Result:= 6;
    19,20: Result:= 7;
    21,22: Result:= 8;
    23,24: Result:= 9;
    27,28: Result:= 10;
    29,30: Result:= 11;
    31,32: Result:= 12;
    end;
  end else
  begin
    case ARow of
    3: Result:= 1;
    4: Result:= 2;
    5: Result:= 3;
    7: Result:= 4;
    8: Result:= 5;
    9: Result:= 6;
    11: Result:= 7;
    12: Result:= 8;
    13: Result:= 9;
    15: Result:= 10;
    16: Result:= 11;
    17: Result:= 12;
   end;
  end;
end;

function TCustomYearScheduleSheet.ColToDay(const ACol, AMonth: Integer): Integer;
begin
  Result:= 0;
  if (not Assigned(FCalendar)) or (not FCalendar.IsCalculated) then Exit;
  if (ACol>=2) and (ACol<=32) then
  begin
    if (ACol-1)<=DaysInPeriod(AMonth, YearOfDate(FCalendar.BeginDate)) then
      Result:= ACol-1;
  end;
end;

function TCustomYearScheduleSheet.DateToRow(const ADate: TDate): Integer;
var
  M: Integer;
begin
  M:= MonthOfDate(ADate);
  if FNeedNight then
  begin
    case M of
    1: Result:= 3;
    2: Result:= 5;
    3: Result:= 7;
    4: Result:= 11;
    5: Result:= 13;
    6: Result:= 15;
    7: Result:= 19;
    8: Result:= 21;
    9: Result:= 23;
    10: Result:= 27;
    11: Result:= 29;
    12: Result:= 31;
    end;
  end else
  begin
    case M of
    1: Result:= 3;
    2: Result:= 4;
    3: Result:= 5;
    4: Result:= 7;
    5: Result:= 8;
    6: Result:= 9;
    7: Result:= 11;
    8: Result:= 12;
    9: Result:= 13;
    10: Result:= 15;
    11: Result:= 16;
    12: Result:= 17;
   end;
  end;
end;

function TCustomYearScheduleSheet.DateToCol(const ADate: TDate): Integer;
begin
  Result:= DayOfDate(ADate) + 1;
end;

function TCustomYearScheduleSheet.GridToDate(const ARow, ACol: Integer;
  out ADate: TDate): Boolean;
var
  M, D: Integer;
begin
  Result:= False;
  ADate:= NULDATE;
  if (not Assigned(FCalendar)) or (not FCalendar.IsCalculated) then Exit;
  M:= RowToMonth(ARow);
  D:= 0;
  if M>0 then D:= ColToDay(ACol, M);
  if D>0 then
  begin
    ADate:= EncodeDate(YearOfDate(FCalendar.BeginDate), M, D);
    Result:= True;
  end;
end;

function TCustomYearScheduleSheet.DateToGrid(const ADate: TDate;
  out ARow, ACol: Integer): Boolean;
begin
  Result:= False;
  ARow:= 0;
  ACol:= 0;
  if (not Assigned(FCalendar)) or (not FCalendar.IsCalculated) then Exit;
  if YearOfDate(ADate)<>YearOfDate(FCalendar.BeginDate) then Exit;
  ARow:= DateToRow(ADate);
  ACol:= DateToCol(ADate);
  Result:= True;
end;

procedure TCustomYearScheduleSheet.Select(const ADate: TDate);
var
  R, C: Integer;
begin
  inherited Select(ADate);
  if not FNeedNight then Exit;
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionAddCell(R+1, C);
end;

procedure TCustomYearScheduleSheet.Unselect(const ADate: TDate);
var
  R, C: Integer;
begin
  inherited Unselect(ADate);
  if not FNeedNight then Exit;
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionDelCell(R+1, C);
end;

{ TVacationStatSheet }

function TVacationStatSheet.SetWidths: TIntVector;
begin
  VDim(Result{%H-}, 12, 100);
end;

procedure TVacationStatSheet.Draw;
var
  i: Integer;
begin
  Writer.BeginEdit;

  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetBackgroundDefault;

  for i:= 1 to 12 do
  begin
    Writer.WriteText(1, i, SUpper(MONTHSNOM[i]), cbtOuter);
    Writer.WriteNumber(2, i, 0, cbtOuter);
  end;

  Writer.EndEdit;
end;

procedure TVacationStatSheet.Update(const ACounts: TIntVector);
var
  i: Integer;
begin
  for i:= 1 to 12 do
    Writer.WriteNumber(2, i, ACounts[i-1], cbtOuter);
end;

{ TVacationPlanSheet }

function TVacationPlanSheet.SetWidths: TIntVector;
begin
  FFirstDateCol:= 5;
  Result:= nil;
  VDim(Result, FCalendar.DaysCount + FFirstDateCol - 1, DAY_COLUMN_WIDTH);
  Result[0]:= NAME_COLUMN_WIDTH;
  Result[1]:= TABNUM_COLUMN_WIDTH;
  Result[2]:= DATE_COLUMN_WIDTH;
  Result[3]:= DATE_COLUMN_WIDTH;
end;

function TVacationPlanSheet.DateToIndex(const ADate: TDate): Integer;
begin
  Result:= DayNumberInYear(ADate) - 1;
end;

function TVacationPlanSheet.VacationEndDate(const ABeginDate: TDate;
                                            const ADaysCount: Integer): TDate;
var
  HolidaysCount: Integer;
begin
  Result:= IncDay(ABeginDate, ADaysCount-1);
  HolidaysCount:= FCalendar.HoliDaysCount(ABeginDate, Result);
  if HolidaysCount>0 then
    Result:= IncDay(Result, HolidaysCount);
  if CompareDate(Result, FCalendar.EndDate)>0 then
    Result:= FCalendar.EndDate;
end;

function TVacationPlanSheet.IsVacationDayCell(const ARow, ACol: Integer;
                                              out APart: Integer): Boolean;
var
  RowIndex: Integer;

  function VacationCheck(const AFirstDate: TDate; const ADaysCount: Integer): Boolean;
  var
    FirstCol, LastCol: Integer;
    LastDate: TDate;
  begin
    Result:= False;
    if AFirstDate>0 then
    begin
      FirstCol:= IndexToCol(DateToIndex(AFirstDate));
      LastDate:= VacationEndDate(AFirstDate, ADaysCount);
      LastCol:= IndexToCol(DateToIndex(LastDate));
      Result:= (ACol>=FirstCol) and (ACol<=LastCol);
    end;
  end;

begin
  Result:= False;
  APart:= 0;

  RowIndex:= RowToIndex(ARow);

  Result:= VacationCheck(FPlan1Dates[RowIndex],
                         FPlan1Counts[RowIndex] + FPlan1AddCounts[RowIndex]);
  if Result then
    APart:= 1
  else begin
    Result:= VacationCheck(FPlan2Dates[RowIndex],
                         FPlan2Counts[RowIndex] + FPlan2AddCounts[RowIndex]);
    if Result then
      APart:= 2;
  end;
end;

function TVacationPlanSheet.ColToIndex(const ACol: Integer): Integer;
begin
  Result:= ACol - FFirstDateCol;
end;

function TVacationPlanSheet.IndexToCol(const AIndex: Integer): Integer;
begin
  Result:= AIndex + FFirstDateCol;
end;

function TVacationPlanSheet.IndexToRow(const AIndex: Integer): Integer;
begin
  Result:= AIndex + 3;
end;

function TVacationPlanSheet.RowToIndex(const ARow: Integer): Integer;
begin
 Result:= ARow - 3;
end;

procedure TVacationPlanSheet.CaptionDraw;
var
  R, C1, C2, i, j, YearNum: Integer;
begin
  YearNum:= YearOf(FCalendar.BeginDate);

  R:= 1;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetBackgroundDefault;

  Writer.WriteText(R, 1, R+1, 1, 'Фамилия И.О.', cbtOuter);
  Writer.WriteText(R, 2, R+1, 2, 'Табельный номер', cbtOuter);
  Writer.WriteText(R, 3, R+1, 3, 'Отпуск (1 часть)', cbtOuter);
  Writer.WriteText(R, 4, R+1, 4, 'Отпуск (2 часть)', cbtOuter);

  C2:= FFirstDateCol - 1;
  for i:= 1 to 12 do
  begin
    C1:= C2 + 1;
    C2:= C2 + DaysInAMonth(YearNum, i);
    Writer.WriteText(R, C1, R, C2, SUpper(MONTHSNOM[i]), cbtOuter);
    for j:= C1 to C2 do
    begin
      if FCalendar.DayStatuses[ColToIndex(j)]=DAY_STATUS_HOLIDAY then
        Writer.SetBackground(COLORS_CALENDAR[DAY_STATUS_HOLIDAY])
      else
        Writer.SetBackgroundDefault;
      Writer.WriteNumber(R+1, j, j-C1+1, cbtOuter);
    end;
  end;

  Writer.DrawBorders(1, 1, 2, Writer.ColCount, cbtAll);
end;

procedure TVacationPlanSheet.StaffLineDraw(const AIndex: Integer;
                                           const AStaffName, ATabNum: String);
var
  R: Integer;
begin
  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  Writer.SetBackgroundDefault;
  R:= IndexToRow(AIndex);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, 1, AStaffName, cbtOuter);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 2, ATabNum, cbtOuter);
end;

procedure TVacationPlanSheet.ScheduleLineDraw(const AIndex: Integer;
                                              const ASchedule: TPersonalSchedule);
var
  R, C, H, i: Integer;
begin
  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  R:= IndexToRow(AIndex);
  for i:= 0 to FCalendar.DaysCount-1 do
  begin
    C:= IndexToCol(i);
    H:= ASchedule.HoursCorrect.Totals[i];
    if H>0 then
      WriteCellHours(Writer, R, C, H)
    else
      Writer.WriteText(R, C, EmptyStr, cbtOuter);
  end;
end;

procedure TVacationPlanSheet.Select(const ARowIndex, ACol: Integer);
begin
  FSelectedRowIndex:= ARowIndex;
  FSelectedCol:= ACol;

  if IsSelected then
  begin
    Writer.SetBackground(DefaultSelectionBGColor);
    Writer.WriteBackground(IndexToRow(ARowIndex), ACol);
    Writer.SetBackgroundDefault;

    VacationLineDraw(ARowIndex, FPlan1Dates[ARowIndex], FPlan2Dates[ARowIndex],
                     FPlan1Counts[ARowIndex], FPlan1AddCounts[ARowIndex],
                     FPlan2Counts[ARowIndex], FPlan2AddCounts[ARowIndex]);
  end;

  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TVacationPlanSheet.Unselect(const ANeedDoEvent: Boolean);
var
  RowIndex: Integer;
begin
  if IsSelected then
  begin
    Writer.SetBackgroundDefault;
    Writer.WriteBackground(IndexToRow(FSelectedRowIndex), FSelectedCol);
  end;

  RowIndex:= FSelectedRowIndex;
  FSelectedCol:= 0;
  FSelectedRowIndex:= -1;

  VacationLineDraw(RowIndex, FPlan1Dates[RowIndex], FPlan2Dates[RowIndex],
                     FPlan1Counts[RowIndex], FPlan1AddCounts[RowIndex],
                     FPlan2Counts[RowIndex], FPlan2AddCounts[RowIndex]);

  if ANeedDoEvent and Assigned(FOnSelect) then FOnSelect;
end;

procedure TVacationPlanSheet.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R, C, P: Integer;
begin
  if FLastRow<=2 then Exit;
  if Button=mbLeft then
  begin
     (Sender as TsWorksheetGrid).MouseToCell(X, Y, C, R);
     FClickedRow:= R;
     if not (
       ((C in [3, 4]) or IsVacationDayCell(R, C, P)) and
       ((R>2) and (R<=FLastRow))
     ) then Exit;
     Unselect(False);
     if P>0 then
       C:= PartToDateCol(P);
     Select(RowToIndex(R), C);
  end
  else if Button=mbRight then
    Unselect(True);
end;

procedure TVacationPlanSheet.DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Grid: TsWorksheetGrid;
begin
  //fix frozen pane border drawing
  if not ((ACol=FFirstDateCol-1) and ((ARow>=1) and (ARow<=FLastRow))) then Exit;
  Grid:= Sender as TsWorksheetGrid;
  Grid.Canvas.Pen.Color:= clBlack;
  Grid.Canvas.Pen.Style:= psSolid;
  Grid.Canvas.Line(aRect.Right-1, aRect.Top, aRect.Right-1, aRect.Bottom);
end;

procedure TVacationPlanSheet.VacationLineDraw(const AIndex: Integer;
   const APlan1Date, APlan2Date: TDate;
   const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer);
var
  R, C: Integer;
  S: String;

  procedure ColorClear;
  var
    i: Integer;
  begin
    Writer.SetBackgroundDefault;
    for i:= 0 to FCalendar.DaysCount-1 do
    begin
      C:= IndexToCol(i);
      Writer.WriteBackground(R, C);
    end;
  end;

  procedure ColorDraw(const AFirstIndex, ALastIndex: Integer; const ABGColor: TColor);
  var
    i: Integer;
  begin
    for i:= AFirstIndex to ALastIndex do
    begin
      C:= IndexToCol(i);
      if FCalendar.DayStatuses[i]=DAY_STATUS_HOLIDAY then
        Writer.SetBackground(COLORS_CALENDAR[DAY_STATUS_HOLIDAY])
      else
        Writer.SetBackground(ABGColor);
      Writer.WriteBackground(R, C);
    end;
  end;

  procedure VacationDraw(const ABeginDate: TDate; const ACount, AAddCount: Integer;
                         const AIsSelected: Boolean);
  var
    FirstDate, EndDate: TDate;
    FirstIndex, LastIndex: Integer;
    DayColor: TColor;
  begin
    FirstDate:= ABeginDate;
    EndDate:= IncDay(FirstDate, -1);
    if ACount>0 then
    begin
      FirstDate:= IncDay(EndDate);
      FirstIndex:= DateToIndex(FirstDate);
      EndDate:= VacationEndDate(FirstDate, ACount);
      LastIndex:= DateToIndex(EndDate);
      DayColor:= COLORS_CALENDAR[DAY_STATUS_OFFDAY];
      if AIsSelected then
        DayColor:= ColorIncLightness(DayColor, -50);
      ColorDraw(FirstIndex, LastIndex, DayColor);
    end;
    if AAddCount>0 then
    begin
      FirstDate:= IncDay(EndDate);
      FirstIndex:= DateToIndex(FirstDate);
      EndDate:= VacationEndDate(FirstDate, AAddCount);
      LastIndex:= DateToIndex(EndDate);
      DayColor:= COLORS_CALENDAR[DAY_STATUS_BEFORE];
      if AIsSelected then
        DayColor:= ColorIncLightness(DayColor, -50);
      ColorDraw(FirstIndex, LastIndex, DayColor);
    end;
    Writer.SetBackgroundDefault;
  end;

begin
  FPlan1Dates[AIndex]:= APlan1Date;
  FPlan2Dates[AIndex]:= APlan2Date;
  FPlan1Counts[AIndex]:= APlan1Count;
  FPlan2Counts[AIndex]:= APlan2Count;
  FPlan1AddCounts[AIndex]:= APlan1AddCount;
  FPlan2AddCounts[AIndex]:= APlan2AddCount;

  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);

  R:= IndexToRow(AIndex);
  S:= VacationPart(APlan1Date, APlan1Count, APlan1AddCount);
  if (FSelectedCol=3) and (SelectedIndex=AIndex) then
    Writer.SetBackground(DefaultSelectionBGColor)
  else
    Writer.SetBackgroundDefault;
  Writer.WriteText(R, 3, S, cbtOuter);
  if (FSelectedCol=4) and (SelectedIndex=AIndex) then
    Writer.SetBackground(DefaultSelectionBGColor)
  else
    Writer.SetBackgroundDefault;
  S:= VacationPart(APlan2Date, APlan2Count, APlan2AddCount);
  Writer.WriteText(R, 4, S, cbtOuter);
  Writer.SetBackgroundDefault;

  ColorClear;
  VacationDraw(APlan1Date, APlan1Count, APlan1AddCount,
               (SelectedIndex=AIndex) and (SelectedPart=1));
  VacationDraw(APlan2Date, APlan2Count, APlan2AddCount,
               (SelectedIndex=AIndex) and (SelectedPart=2));
end;

function TVacationPlanSheet.IsSelected: Boolean;
begin
  Result:= (FSelectedRowIndex>=0) and (FSelectedCol>0);
end;

function TVacationPlanSheet.PartToDateCol(const APart: Integer): Integer;
begin
  Result:= APart + 2;
end;

function TVacationPlanSheet.DateColToPart(const ACol: Integer): Integer;
begin
  Result:= 0;
  if ACol>0 then
    Result:= ACol - 2;
end;

function TVacationPlanSheet.SelectedPart: Integer;
begin
  Result:= DateColToPart(FSelectedCol);
end;

constructor TVacationPlanSheet.Create(const ACalendar: TCalendar;
                       const AWorksheet: TsWorksheet; const AGrid: TsWorksheetGrid;
                       const AFont: TFont);
begin
  FCalendar:= ACalendar;
  inherited Create(AWorksheet, AGrid, AFont);
  if not Writer.HasGrid then Exit;
  Writer.Grid.Options:= Writer.Grid.Options - [goRangeSelect] + [goThumbTracking];
  Writer.Grid.OnDrawCell:= @DrawCell;
  Writer.Grid.OnMouseDown:= @MouseDown;
end;

procedure TVacationPlanSheet.Draw(const AStaffNames, ATabNums: TStrVector;
          const ASchedules: TPersonalScheduleVector;
          const APlan1Dates, APlan2Dates: TDateVector;
          const APlan1Counts, APlan1AddCounts, APlan2Counts, APlan2AddCounts: TIntVector);
var
  i: Integer;
begin
  BeginDraw;
  for i:=0 to High(AStaffNames) do
    LineDraw(i, AStaffNames[i], ATabNums[i], ASchedules[i], APlan1Dates[i], APlan2Dates[i],
             APlan1Counts[i], APlan1AddCounts[i], APlan2Counts[i], APlan2AddCounts[i]);
  EndDraw(Length(AStaffNames));
end;

procedure TVacationPlanSheet.BeginDraw;
begin
  Writer.BeginEdit;
  CaptionDraw;
end;

procedure TVacationPlanSheet.EndDraw(const ARowCount: Integer);
var
  i, X: Integer;
begin
  X:= IndexToRow(ARowCount);
  for i:= 1 to Writer.ColCount do
    Writer.WriteText(X, i, EmptyStr, cbtTop);
  FLastRow:= 2;
  if ARowCount>0 then
  begin
    Writer.SetFrozenRows(2);
    FLastRow:= FLastRow + ARowCount;
  end;
  Writer.SetFrozenCols(IndexToCol(-1));
  Writer.EndEdit;
end;

procedure TVacationPlanSheet.LineDraw(const AIndex: Integer;
                const AStaffName, ATabNum: String;
                const ASchedule: TPersonalSchedule;
                const APlan1Date, APlan2Date: TDate;
                const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer);
begin
  StaffLineDraw(AIndex, AStaffName, ATabNum);
  ScheduleLineDraw(AIndex, ASchedule);

  VRedim(FPlan1Dates, AIndex+1);
  VRedim(FPlan2Dates, AIndex+1);
  VRedim(FPlan1Counts, AIndex+1);
  VRedim(FPlan2Counts, AIndex+1);
  VRedim(FPlan1AddCounts, AIndex+1);
  VRedim(FPlan2AddCounts, AIndex+1);
  VacationLineDraw(AIndex, APlan1Date, APlan2Date,
                   APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount);
end;

{ TVacationScheduleSheet }

function TVacationScheduleSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    {01} 25, //Структурное подразделение
    {02} 30,
    {03} 15,
    {04} 90,
    {05} 25, //должность
    {06} 25,
    {07} 15,
    {08} 25,
    {09} 70,
    {10} 175, //ФИО
    {11} 15,
    {12} 60,
    {13} 50, //Табельный номер
    {14} 45,
    {15} 80, //количество календарных дней
    {16} 55, //запланированная дата
    {17} 15,
    {18} 45, //фактическая дата
    {19} 25,
    {20} 30, //основание (документ)
    {21} 15,
    {22} 15,
    {23} 35,
    {24} 40, //дата предполагаемого отпуска
    {25} 25,
    {26} 25,
    {27} 15, //примечание
    {28} 80
  ]);
end;

procedure TVacationScheduleSheet.CaptionDraw(out ARow: Integer);
var
  R: Integer;
begin
  R:= 1;
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, 24, R, 28, 'Унифицированная форма № Т-7');
  Writer.SetRowHeight(R, 12);
  R:= R + 1;
  Writer.WriteText(R, 24, R, 28, 'Утверждена Постановлением Госкомстата');
  Writer.SetRowHeight(R, 12);
  R:= R + 1;
  Writer.WriteText(R, 24, R, 28, 'России от 05.01.2004 № 1');
  Writer.SetRowHeight(R, 12);

  R:= R + 2;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 26, R, 28, 'Код', cbtOuter);
  R:= R + 1;
  Writer.SetAlignment(haRight, vaCenter);
  Writer.WriteText(R, 23, R, 25, 'Форма по ОКУД');
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 26, R, 28, '0301020', cbtOuter);
  R:= R + 1;
  Writer.SetAlignment(haRight, vaCenter);
  Writer.WriteText(R, 23, R, 25, 'по ОКПО');
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 26, R, 28, '', cbtOuter);
  Writer.WriteText(R,  1, R, 22, FCompany, cbtBottom, True, True);
  if not Writer.HasGrid then
  begin
    Writer.SetBorders(lsMedium, clBlack, lsThin, clBlack);
    Writer.DrawBorders(R-1, 26, R, 28, cbtAll);
    Writer.SetBordersDefault;
  end;

  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(R,  1, R, 22, '(наименование организации)', cbtTop);

  R:= R + 2;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R,  1, R, 16, 'Мнение выборного профсоюзного органа');
  Writer.WriteText(R, 18, R, 28, 'УТВЕРЖДАЮ');
  R:= R + 1;
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteText(R, 1, 'от «');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 2, '', cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 3, '»');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 4, '', cbtBottom);
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteNumber(R, 5, 20);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 6, '', cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 7, 'г.');
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteText(R, 8, '№');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 9, '', cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 10, 'учтено');
  Writer.WriteText(R, 18, R, 20, 'Руководитель');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 21, R, 28, '', cbtBottom);

  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(R, 21, R, 28, '(должность)', cbtTop);

  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 12, R, 13, 'Номер документа', cbtOuter);
  Writer.WriteText(R, 14, R, 15, 'Дата составления', cbtOuter);
  Writer.WriteText(R, 16, R, 16, 'На год', cbtOuter);
  Writer.WriteText(R, 18, R, 21, '', cbtBottom);
  Writer.WriteText(R, 23, R, 28, '', cbtBottom);

  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
  Writer.SetAlignment(haRight, vaCenter);
  Writer.WriteText(R, 1, R, 10, 'ГРАФИК ОТПУСКОВ');
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.WriteText(R, 12, R, 13, '', cbtOuter);
  Writer.WriteText(R, 14, R, 15, '', cbtOuter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteNumber(R, 16, R, 16, FYear, cbtOuter);
  if not Writer.HasGrid then
  begin
    Writer.SetBorders(lsMedium, clBlack, lsThin, clBlack);
    Writer.DrawBorders(R, 12, R, 16, cbtAll);
    Writer.SetBordersDefault;
  end;

  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(R, 18, R, 21, '(личная подпись)', cbtTop);
  Writer.WriteText(R, 23, R, 28, '(расшифровка подписи)', cbtTop);

  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteText(R, 19, 'от «');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 20, '', cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 21, '»');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 22, R, 24, '', cbtBottom);
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteNumber(R, 25, 20);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 26, '', cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 27, 'г.');

  R:= R + 2;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 1, R+2, 4, 'Структурное'+SYMBOL_BREAK+'подразделение', cbtOuter);
  Writer.WriteText(R, 5, R+2, 9, 'Должность'+SYMBOL_BREAK+'(специальность, профессия)'+
                                  SYMBOL_BREAK+'по штатному расписанию', cbtOuter);
  Writer.WriteText(R, 10, R+2, 12, 'Фамилия, имя, отчество', cbtOuter);
  Writer.WriteText(R, 13, R+2, 14, 'Табельный'+SYMBOL_BREAK+'номер', cbtOuter);
  Writer.WriteText(R, 15, R, 26, 'ОТПУСК', cbtOuter);
  Writer.WriteText(R+1, 15, R+2, 15, 'количество'+SYMBOL_BREAK+'календарных'+
                                     SYMBOL_BREAK+'дней', cbtOuter);
  Writer.WriteText(R+1, 16, R+1, 19, 'дата', cbtOuter);
  Writer.WriteText(R+2, 16, R+2, 17, 'заплани-'+SYMBOL_BREAK+'рованная', cbtOuter);
  Writer.WriteText(R+2, 18, R+2, 19, 'факти-'+SYMBOL_BREAK+'ческая', cbtOuter);
  Writer.WriteText(R+1, 20, R+1, 26, 'перенесение отпуска', cbtOuter);
  Writer.WriteText(R+2, 20, R+2, 23, 'основание'+SYMBOL_BREAK+'(документ)', cbtOuter);
  Writer.WriteText(R+2, 24, R+2, 26, 'дата предпо-'+SYMBOL_BREAK+'лагаемого'+
                                     SYMBOL_BREAK+'отпуска', cbtOuter);
  Writer.WriteText(R, 27, R+2, 28, 'Примечание', cbtOuter);
  Writer.SetRowHeight(R+2, 50);

  R:= R + 3;
  Writer.WriteNumber(R, 1,  R, 4,  1, cbtOuter);
  Writer.WriteNumber(R, 5,  R, 9,  2, cbtOuter);
  Writer.WriteNumber(R, 10, R, 12, 3, cbtOuter);
  Writer.WriteNumber(R, 13, R, 14, 4, cbtOuter);
  Writer.WriteNumber(R, 15, R, 15, 5, cbtOuter);
  Writer.WriteNumber(R, 16, R, 17, 6, cbtOuter);
  Writer.WriteNumber(R, 18, R, 19, 7, cbtOuter);
  Writer.WriteNumber(R, 20, R, 23, 8, cbtOuter);
  Writer.WriteNumber(R, 24, R, 26, 9, cbtOuter);
  Writer.WriteNumber(R, 27, R, 28, 10, cbtOuter);

  Writer.DrawBorders(R-3, 1, R, 28, cbtAll);

  ARow:= R + 1;
end;

procedure TVacationScheduleSheet.LineDraw(var ARow: Integer; const AIndex: Integer);
var
  R: Integer;
begin
  R:= ARow;

  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, 1,  R, 4,  FDepartment, cbtOuter, True, True);
  Writer.WriteText(R, 5,  R, 9,  FPostNames[AIndex], cbtOuter, True, True);
  Writer.WriteText(R, 10, R, 12, FStaffNames[AIndex], cbtOuter, True, True);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, 13, R, 14, FTabNums[AIndex], cbtOuter);
  Writer.WriteNumber(R, 15, R, 15, FTotalCounts[AIndex], cbtOuter);
  Writer.WriteDate(R, 16, R, 17, FFirstDates[AIndex], cbtOuter);
  Writer.WriteText(R, 18, R, 19, '', cbtOuter);
  Writer.WriteText(R, 20, R, 23, '', cbtOuter);
  Writer.WriteText(R, 24, R, 26, '', cbtOuter);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, 27, R, 28, '', cbtOuter);

  ARow:= R + 1;
end;

procedure TVacationScheduleSheet.BottomDraw(const ARow: Integer);
var
  R: Integer;
begin
  R:= ARow + 1;

  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 1, R, 6, 'Руководитель кадровой службы');
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.WriteText(R, 7, R, 13, '', cbtBottom);
  Writer.WriteText(R, 15, R, 17, '', cbtBottom);
  Writer.WriteText(R, 19, R, 28, '', cbtBottom);

  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(R, 7, R, 13, '(должность)', cbtTop);
  Writer.WriteText(R, 15, R, 17, '(личная подпись)', cbtTop);
  Writer.WriteText(R, 19, R, 28, '(расшифровка подписи)', cbtTop);
end;

constructor TVacationScheduleSheet.Create(const AWorksheet: TsWorksheet;
  const AGrid: TsWorksheetGrid; const AFont: TFont);
begin
  inherited Create(AWorksheet, AGrid, AFont, 20);
end;

procedure TVacationScheduleSheet.Draw(const ACompany, ADepartment: String;
                   const AYear: Word;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const AFirstDates: TDateVector;
                   const ATotalCounts: TIntVector);
var
  i, R, CaptionRowCount: Integer;
begin
  FCompany:= ACompany;
  FDepartment:= ADepartment;
  FYear:= AYear;
  FStaffNames:= AStaffNames;
  FTabNums:= ATabNums;
  FPostNames:= APostNames;
  FFirstDates:= AFirstDates;
  FTotalCounts:= ATotalCounts;

  Writer.BeginEdit;
  CaptionDraw(R);
  CaptionRowCount:= R - 1;
  for i:= 0 to High(FStaffNames) do
    LineDraw(R, i);
  BottomDraw(R);

  Writer.SetFrozenRows(CaptionRowCount);
  Writer.SetRepeatedRows(CaptionRowCount-3, CaptionRowCount);
  Writer.WorkSheet.PageLayout.Footers[HEADER_FOOTER_INDEX_ALL] := '&R страница &P (из &N)';

  Writer.EndEdit;
end;

{ TTableScheduleSheet }

function TTableScheduleSheet.SetWidths: TIntVector;
var
  i, W: Integer;
begin
  Result:=  nil;
  VDim(Result, COLUMNS_COUNT + Ord(FResumeType=2));

  W:= Max(PERIOD_COLUMN_WIDTH, SWidth('IV квартал', Font.Name, Font.Size, [fsBold]));
  Result[0]:= W;

  W:= Max(DAY_COLUMN_WIDTH, SWidth('00,00', Font.Name, Font.Size, [fsBold]));
  for i:= 1 to 31 do
    Result[i]:= W;

  W:= Max(SUMDAYS_COLUMN_WITH, SWidth('000', Font.Name, Font.Size, [fsBold]));
  W:= W+10;
  Result[32]:= W;
  if FResumeType=2 then
  begin
    Result[33]:= W;
    Result[35]:= W;
  end
  else
    Result[34]:= W;

  W:= Max(SUMHOURS_COLUMN_WITH, SWidth('0000,00', Font.Name, Font.Size, [fsBold]));
  W:= W+10;
  if FResumeType=2 then
  begin
    Result[34]:= W;
    Result[36]:= W;
  end
  else begin
    Result[33]:= W;
    Result[35]:= W;
  end;
end;

function TTableScheduleSheet.IsNeedCaption: Boolean;
begin
  Result:= (not Writer.HasGrid) and (not SEmpty(GetCaption));
end;

procedure TTableScheduleSheet.CaptionDraw;
var
  R, C, i,n: Integer;
  S: String;
begin
  C:= 1;
  R:= 1;
  if IsNeedCaption then
  begin
    Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.WriteText(R, C, R, C+35, GetCaption);
    R:= R + 1;
  end;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C, R+1, C, GetPeriodColumnName, cbtOuter);
  Writer.AddCellBGColorIndex(R, C, TITLE_COLOR_INDEX);
  Writer.AddCellBGColorIndex(R+1, C, TITLE_COLOR_INDEX);
  S:= 'Часов за день';
  if FNeedNight then
    S:= S + ', в том числе ночных';
  Writer.WriteText(R, C+1, R, C+31, S, cbtOuter);
  for i:= C+1 to C+31 do
  begin
    Writer.WriteNumber(R+1, i, i-C, cbtOuter);
    Writer.AddCellBGColorIndex(R, i, TITLE_COLOR_INDEX);
    Writer.AddCellBGColorIndex(R+1, i, TITLE_COLOR_INDEX);
  end;
  C:= C + 32;
  n:= 0;
  if FResumeType=2 then n:= 1;
  Writer.WriteText(R, C, R, C+1+n, 'По графику', cbtOuter);
  if FResumeType=0 then
    Writer.WriteText(R+1,C, 'дней', cbtOuter)
  else if FResumeType=1 then
    Writer.WriteText(R+1,C, 'смен', cbtOuter)
  else begin
    Writer.WriteText(R+1,C, 'дней', cbtOuter);
    Writer.WriteText(R+1,C+1, 'смен', cbtOuter)
  end;
  Writer.WriteText(R+1,C+1+n, 'часов', cbtOuter);
  Writer.WriteText(R,C+2+n,R,C+3+n, 'По произ-' + SYMBOL_BREAK+ 'водственному' +
                            SYMBOL_BREAK + 'календарю' , cbtOuter);
  Writer.WriteText(R+1,C+2+n, 'дней', cbtOuter);
  Writer.WriteText(R+1,C+3+n, 'часов', cbtOuter);
  for i:= C to C+3+n do
  begin
    Writer.AddCellBGColorIndex(R, i, TITLE_COLOR_INDEX);
    Writer.AddCellBGColorIndex(R+1, i, TITLE_COLOR_INDEX);
  end;
  i:= 3*SHeight(Font.Name, Font.Size, [fsBold]);
  Writer.SetRowHeight(R, i);
end;

constructor TTableScheduleSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены});
begin
  FResumeType:= AResumeType;
  inherited Create(AWorksheet, AGrid, AFont);
end;

{ TShiftYearScheduleSheet }

procedure TShiftYearScheduleSheet.ScheduleDraw;
var
  R,C,DeltaR,m: Integer;
  AYear: Word;
  BD, ED: TDate;
  CutCalendar: TCalendar;
  CutSchedule: TShiftSchedule;
  WorkHours: TWorkHours;
  Marks: TStrVector;

  procedure DrawMonth(ARow, ACol, AMonth: Word);
  var
    i, d, s: Integer;
  begin
    FirstLastDayInMonth(AMonth,AYear,BD,ED);
    FCalendar.Cut(BD, ED, CutCalendar);
    FSchedule.Cut(BD, ED, CutSchedule);
    ChooseShiftScheduleData(CutSchedule, FNeedCorrect, WorkHours, d, s, Marks);
    for i:= 0 to CutCalendar.DaysCount - 1 do
    begin
      if FNeedCorrect and (CutSchedule.IsCorrections[i]=CORRECTION_YES) then
        AddScheduleColorIndex(Writer, ARow, ACol+i, CORRECT_COLOR_INDEX, FNeedNight)
      else begin
        if FScheduleNotWorkColor then
        begin
          if WorkHours.Totals[i]=0 then
            AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end
        else begin
          if (CutCalendar.DayStatuses[i]=DAY_STATUS_HOLIDAY) or
             (CutCalendar.DayStatuses[i]=DAY_STATUS_OFFDAY) then
             AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end;
      end;
      DrawHoursOrMarks(Writer, ARow, ACol+i, WorkHours.Totals[i], WorkHours.Nights[i],
                       Marks[i], EmptyStr, FWriteTotalIfZero, FWriteNightIfZero, FNeedNight, FNeedMarks);
    end;
    for i:= CutCalendar.DaysCount to 30 do
    begin
      DrawMonthOutside(Writer, ARow, ACol+i, FNeedNight);
      AddScheduleColorIndex(Writer, ARow, ACol+i, OUTSIDEMONTH_COLOR_INDEX, FNeedNight);
    end;
    DrawDaysOrShiftCount(Writer,  ARow, ACol+31, d, s, FWriteDaysCountIfZero, FNeedNight, FResumeType, i);
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    DrawDaysCount(Writer, ARow, ACol+33+i, CutCalendar.WorkDaysCount, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, CutCalendar.SumWorkHoursInt(CutSchedule.HoursInWeek), 0,
               FWriteSumTotalIfZero, False, FNeedNight);
  end;

  procedure DrawQuarter(ARow, ACol, AQuart: Word);
  var
    d,s,i: Integer;
  begin
    FirstLastDayInQuarter(AQuart,AYear,BD,ED);
    FCalendar.Cut(BD, ED, CutCalendar);
    FSchedule.Cut(BD, ED, CutSchedule);
    ChooseShiftScheduleData(CutSchedule, FNeedCorrect, WorkHours, d,s, Marks);
    DrawDaysOrShiftCount(Writer,  ARow, ACol+31, d, s, FWriteDaysCountIfZero, FNeedNight, FResumeType, i);
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    DrawDaysCount(Writer, ARow, ACol+33+i, CutCalendar.WorkDaysCount, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, CutCalendar.SumWorkHoursInt(CutSchedule.HoursInWeek), 0,
               FWriteSumTotalIfZero, False, FNeedNight);
  end;

  procedure DrawYear(ARow, ACol: Word);
  var
    d,s,i: Integer;
  begin
    ChooseShiftScheduleData(FSchedule, FNeedCorrect, WorkHours, d,s, Marks);
    DrawDaysOrShiftCount(Writer,  ARow, ACol+31, d, s, FWriteDaysCountIfZero, FNeedNight, FResumeType, i);
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    DrawDaysCount(Writer, ARow, ACol+33+i, FCalendar.WorkDaysCount, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, FCalendar.SumWorkHoursInt(FSchedule.HoursInWeek), 0,
                 FWriteSumTotalIfZero, False, FNeedNight);
  end;
begin
  CutSchedule:= TShiftSchedule.Create;
  CutCalendar:= TCalendar.Create;
  try
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    AYear:= YearOfDate(FSchedule.BeginDate);
    DeltaR:= 1 + Ord(FNeedNight);
    R:= 3 + Ord(IsNeedCaption);
    C:= 2;
    for m:= 1 to 3 do
    begin
      DrawMonth(R, C, m);
      R:= R + DeltaR;
    end;
    DrawQuarter(R, C, 1);
    R:= R + DeltaR;
    for m:= 4 to 6 do
    begin
      DrawMonth(R, C, m);
      R:= R + DeltaR;
    end;
    DrawQuarter(R, C, 2);
    R:= R + DeltaR;
    for m:= 7 to 9 do
    begin
      DrawMonth(R, C, m);
      R:= R + DeltaR;
    end;
    DrawQuarter(R, C, 3);
    R:= R + DeltaR;
    for m:= 10 to 12 do
    begin
      DrawMonth(R, C, m);
      R:= R + DeltaR;
    end;
    DrawQuarter(R, C, 4);
    R:= R + DeltaR;
    DrawYear(R, C);
  finally
    FreeAndNil(CutSchedule);
    FreeAndNil(CutCalendar);
  end;
end;

procedure TShiftYearScheduleSheet.Draw(const ACalendar: TCalendar;
                   const ASchedule: TShiftSchedule;
                   const AName: String;
                   const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean);
var
  i: Integer;
begin
  FCaption:= AName;
  FCalendar:= ACalendar;
  FSchedule:= ASchedule;
  FYear:= YearOfDate(FSchedule.BeginDate);
  FNeedNight:= ANeedNight;
  FNeedCorrect:= ANeedCorrect;
  FNeedMarks:= ANeedMarks;
  FScheduleNotWorkColor:= AScheduleNotWorkColor;

  Writer.BeginEdit;
  CaptionDraw;
  BlankDraw;
  ScheduleDraw;
  if Writer.HasGrid then
    Writer.SetFrozenRows(2);
  Writer.EndEdit;

  BordersDraw(1 + Ord(IsNeedCaption));
  for i:= 2+Ord(IsNeedCaption) to Writer.RowCount do
    Writer.SetRowHeight(i, ROW_DEFAULT_HEIGHT);
end;

function TShiftYearScheduleSheet.GetCaption: String;
begin
  Result:= EmptyStr;
  if SEmpty(FCaption) then Exit;
  Result:= 'График сменности "' + FCaption + '" на ' + IntToStr(FYear) + ' год';
end;

{ TShiftMonthScheduleSheet }

procedure TShiftMonthScheduleSheet.BlankDraw;
const
  C1= 2;
var
  DeltaR, R, RR, C,C2, i,j,k,m: Integer;
begin
  C2:= COLUMNS_COUNT + Ord(FResumeType=2);
  DeltaR:= 1 + Ord(FNeedNight);
  RR:= 3 + Ord(IsNeedCaption);
  C:= 1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  k:= -1;
  for i:= 0 to High(FSchedules) do
  begin
    if FVisible[i] then
    begin
      Inc(k);
      R:= RR + k*DeltaR;
      Writer.SetAlignment(haLeft, vaCenter);
      Writer.WriteText(R, C, R + DeltaR - 1, C, FNames[i], cbtOuter);
      Writer.SetAlignment(haCenter, vaCenter);
      for m:= R to R+DeltaR-1 do
        for j:= C1 to C2 do
          Writer.WriteText(m,j,EmptyStr, cbtOuter);
    end;
  end;
end;

procedure TShiftMonthScheduleSheet.ScheduleDraw;
var
  R,RR,C,DeltaR,j,k: Integer;
  Marks: TStrVector;
  WorkHours: TWorkHours;

  procedure DrawSingle(ARow, ACol, AInd: Word);
  var
    i,d,s: Integer;
  begin
    ChooseShiftScheduleData(FSchedules[AInd], FNeedCorrect, WorkHours, d,s, Marks);
    for i:= 0 to FCalendar.DaysCount - 1 do
    begin
      if FNeedCorrect and (FSchedules[AInd].IsCorrections[i]=CORRECTION_YES) then
        AddScheduleColorIndex(Writer, ARow, ACol+i, CORRECT_COLOR_INDEX, FNeedNight)
      else begin
        if FScheduleNotWorkColor then
        begin
          if WorkHours.Totals[i]=0 then
            AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end
        else begin
          if (FCalendar.DayStatuses[i]=DAY_STATUS_HOLIDAY) or
             (FCalendar.DayStatuses[i]=DAY_STATUS_OFFDAY) then
             AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end;
      end;
      DrawHoursOrMarks(Writer, ARow, ACol+i, WorkHours.Totals[i], WorkHours.Nights[i],
                       Marks[i], EmptyStr, FWriteTotalIfZero, FWriteNightIfZero, FNeedNight, FNeedMarks);
    end;
    for i:= FCalendar.DaysCount to 30 do
    begin
      DrawMonthOutside(Writer, ARow, ACol+i, FNeedNight);
      AddScheduleColorIndex(Writer, ARow, ACol+i, OUTSIDEMONTH_COLOR_INDEX, FNeedNight);
    end;
    if FResumeType<2 then
    begin
      if FResumeType=0 then
        DrawDaysCount(Writer, ARow, ACol+31, d, FWriteDaysCountIfZero, FNeedNight)
      else
        DrawDaysCount(Writer, ARow, ACol+31, s, FWriteDaysCountIfZero, FNeedNight);
      i:=0;
    end
    else begin
      DrawDaysCount(Writer, ARow, ACol+31, d, FWriteDaysCountIfZero, FNeedNight);
      DrawDaysCount(Writer, ARow, ACol+32, s, FWriteDaysCountIfZero, FNeedNight);
      i:= 1;
    end;
    DrawHours(Writer, ARow, ACol+32+i, WorkHours.SumTotal, WorkHours.SumNight,
                 FWriteSumTotalIfZero, FWriteSumNightIfZero, FNeedNight);
    DrawDaysCount(Writer, ARow, ACol+33+i, FCalendar.WorkDaysCount, FWriteDaysCountIfZero, FNeedNight);
    DrawHours(Writer, ARow, ACol+34+i, FCalendar.SumWorkHoursInt(FSchedules[AInd].HoursInWeek), 0,
               FWriteSumTotalIfZero, False, FNeedNight);
  end;

begin
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  DeltaR:= 1 + Ord(FNeedNight);
  RR:= 3 + Ord(not Writer.HasGrid);
  C:= 2;
  k:= -1;
  for j:= 0 to High(FSchedules) do
  begin
    if FVisible[j] then
    begin
      Inc(k);
      R:= RR + k*DeltaR;
      DrawSingle(R,C,j);
    end;
  end;
end;

function TShiftMonthScheduleSheet.GetCaption: String;
begin
  Result:= 'Графики сменности на ' +
           MONTHSNOM[MonthOfDate(FCalendar.BeginDate)] +
           FormatDateTime(' yyyy года', FCalendar.BeginDate);
end;

function TShiftMonthScheduleSheet.GetPeriodColumnName: String;
begin
  Result:= 'График';
end;

procedure TShiftMonthScheduleSheet.Draw(const ACalendar: TCalendar;
  const ASchedules: TShiftScheduleVector; const ANames: TStrVector;
  const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean;
  const AVisible: TBoolVector = nil);
var
  W,i: Integer;
begin
  FCalendar:= ACalendar;
  FSchedules:= ASchedules;
  FNeedNight:= ANeedNight;
  FNeedCorrect:= ANeedCorrect;
  FNeedMarks:= ANeedMarks;
  FScheduleNotWorkColor:= AScheduleNotWorkColor;
  FVisible:= AVisible;
  FNames:= ANames;

  Writer.BeginEdit;
  W:= PERIOD_COLUMN_WIDTH;
  for i:= 0 to High(FSchedules) do
    if FVisible[i] then
      W:= Max(W, SWidth(FNames[i], Font.Name, Font.Size+1));
  Writer.SetColWidth(1, W);
  CaptionDraw;
  BlankDraw;
  ScheduleDraw;
  if Writer.HasGrid then
    Writer.SetFrozenRows(2);
  Writer.EndEdit;

  BordersDraw(1 + Ord(IsNeedCaption));
  for i:= 2+Ord(IsNeedCaption) to Writer.RowCount do
    Writer.SetRowHeight(i, ROW_DEFAULT_HEIGHT);
end;

function TShiftMonthScheduleSheet.GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean;
begin
  ADate:= 0;
  Result:= False;
end;

function TShiftMonthScheduleSheet.DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean;
begin
  ARow:= 0;
  ACol:= 0;
  Result:= False;
end;

{ TShiftCalendarScheduleSheet }

function TShiftCalendarScheduleSheet.SetWidths: TIntVector;
begin
  VDim(Result{%H-}, COLUMNS_COUNT, COL_WIDTH);
end;

procedure TShiftCalendarScheduleSheet.CaptionDraw(const AName: String);
begin
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
  Writer.WriteText(1, 1, 1, Writer.ColCount, SUpper('График "' + AName + '"' +
                           FormatDateTime(' на yyyy год', FSchedule.BeginDate)));
end;

procedure TShiftCalendarScheduleSheet.MonthDraw(const AMonth: Byte);
var
  R,C, i,j, Ind, PrevInd: Integer;
  MonthCalendar: TCalendar;
  BD, ED: TDate;
begin
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  R:= MONTH_FIRST_ROWS[AMonth];
  C:= MONTH_FIRST_COLS[AMonth];
  Writer.WriteText(R, C, R, C+6, SUpper(MONTHSNOM[AMonth]), cbtOuter);
  Writer.AddCellBGColorIndex(R, C, MONTHNAME_COLOR_INDEX);
  if Writer.HasGrid then
    Writer.DrawBorders(R, C+7, cbtLeft);
  R:= R+1;
  for i:= 0 to 6 do
  begin
    C:= MONTH_FIRST_COLS[AMonth] + i;
    Writer.WriteText(R, C, WEEKDAYSSHORT[i+1], cbtOuter);
    Writer.AddCellBGColorIndex(R, C, DAYNAME_COLOR_INDEX);
  end;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  for i:= MONTH_FIRST_ROWS[AMonth] + 2 to MONTH_FIRST_ROWS[AMonth] + 7 do
    for j:= MONTH_FIRST_COLS[AMonth] to MONTH_FIRST_COLS[AMonth] + 6 do
      Writer.WriteText(i, j, EmptyStr, cbtOuter);
  FirstLastDayinMonth(AMonth, FYear, BD, ED);
  MonthCalendar:= TCalendar.Create;
  try
    FCalendar.Cut(BD, ED, MonthCalendar);
    for i:=0 to MonthCalendar.DaysCount - 1 do
    begin
      R:= MONTH_FIRST_ROWS[AMonth] + MonthCalendar.WeekNumsInMonth[i] + 1;
      C:= MONTH_FIRST_COLS[AMonth] + MonthCalendar.DayNumsInWeek[i] - 1;
      Writer.WriteNumber(R,C,i+1,cbtOuter);
      j:= DayNumberInYear(MonthCalendar.Dates[i])-1; //индекс дня в году
      PrevInd:= -1;
      //определяем номер смены этой и предыдущей
      if FNeedCorrect then
      begin
        Ind:= FSchedule.ShiftNumbersCorrect[j];
        if j>0 then
          PrevInd:= FSchedule.ShiftNumbersCorrect[j-1]
        else
          PrevInd:= FPrevShiftNumber;
      end
      else begin
        Ind:= FSchedule.ShiftNumbersDefault[j];
        if j>0 then
          PrevInd:= FSchedule.ShiftNumbersDefault[j-1]
        else
          PrevInd:= FPrevShiftNumber;
      end;
      //если нужно красить только первый день переходящей смены и это второй день, то обнуляем номер смены
      if FFirstShiftDayColorOnly then
        if Ind=PrevInd then
          Ind:= 0;
      Writer.AddCellBGColorIndex(R, C, Ind);
    end;
  finally
    FreeAndNil(MonthCalendar);
  end;
end;

procedure TShiftCalendarScheduleSheet.Draw(const ACalendar: TCalendar;
  const ASchedule: TShiftSchedule; const APrevShiftNumber: Integer; const AName: String;
  const ANeedCorrect, AFirstShiftDayColorOnly: Boolean);
var
  i: Integer;
begin
  FCalendar:= ACalendar;
  FSchedule:= ASchedule;
  FPrevShiftNumber:= APrevShiftNumber;
  FNeedCorrect:= ANeedCorrect;
  FFirstShiftDayColorOnly:= AFirstShiftDayColorOnly;
  FYear:= YearOfDate(FSchedule.Dates[0]);
  Writer.BeginEdit;
  CaptionDraw(AName);
  for i:=1 to 12 do MonthDraw(i);
  Writer.EndEdit;
end;

{ TShiftSimpleScheduleSheet }

function TShiftSimpleScheduleSheet.SetWidths: TIntVector;
var
  W: Integer;
begin
  W:= Max(DAY_COLUMN_WIDTH, SWidth('00.00.00', Font.Name, Font.Size, [fsBold]));
  VDim(Result{%H-}, 11, W);
  W:= VMaxWidth(FNames, Font.Name, Font.Size, [fsBold], NAME_COLUMN_WIDTH);
  Result[0]:= W;
end;

procedure TShiftSimpleScheduleSheet.CaptionDraw;
var
  i: Integer;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(1, 1, 'График', cbtOuter);
  for i:=0 to 9 do
    Writer.WriteText(1, i+2, FormatDateTime('dd.mm.yy', IncDay(FSchedules[0].BeginDate, i)), cbtOuter);
end;

procedure TShiftSimpleScheduleSheet.LineDraw(const AIndex: Integer; const ASelected: Boolean);
var
  R,i: Integer;
  WorkHours: TWorkHours;
  Marks: TStrVector;
begin
  R:= AIndex*2 + 2;
  if ASelected then
    Writer.SetBackground(DefaultSelectionBGColor)
  else
    Writer.SetBackgroundDefault;

  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, 1, R+1, 1, FNames[AIndex], cbtOuter);
  Writer.SetAlignment(haCenter, vaCenter);
  ChooseShiftScheduleData(FSchedules[AIndex], False, WorkHours, i, i, Marks);
  for i:=0 to 9 do
    DrawHoursOrMarks(Writer, R, i+2, WorkHours.Totals[i], WorkHours.Nights[i], Marks[i],
                     EmptyStr, False, False, True, True);
end;

function TShiftSimpleScheduleSheet.GetIsSelected: Boolean;
begin
  Result:= FSelectedIndex>=0;
end;

procedure TShiftSimpleScheduleSheet.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R, C: Integer;
begin
  if VIsNil(FNames) then Exit;
  if Button<>mbLeft then Exit;
  (Sender as TsWorksheetGrid).MouseToCell(X, Y, C, R);
  C:= Trunc((R - 2)/2); //new selected index
  if (C<0) or (C>High(FNames)) then Exit;
  if IsSelected then
    LineDraw(FSelectedIndex, False);
  FSelectedIndex:= C;
  LineDraw(FSelectedIndex, True);
end;

procedure TShiftSimpleScheduleSheet.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_UP then
    SelectionMove(mdUp)
  else if Key=VK_DOWN then
    SelectionMove(mdDown)
end;

procedure TShiftSimpleScheduleSheet.SelectionMove(const ADirection: TMoveDirection);
begin
  if not IsSelected then Exit;
  if ADirection=mdUp then
    SelectedIndex:= SelectedIndex - 1
  else if ADirection=mdDown then
    SelectedIndex:= SelectedIndex + 1;
end;

procedure TShiftSimpleScheduleSheet.SetSelectedIndex(const AValue: Integer);
begin
  if FSelectedIndex=AValue then Exit;
  if (AValue<0) or (AValue>High(FNames)) then Exit;
  if IsSelected then
    LineDraw(FSelectedIndex, False);
  FSelectedIndex:= AValue;
  LineDraw(FSelectedIndex, True);
  //move grid
  Writer.Grid.Row:= AValue*2 + 2;
  Writer.Grid.Row:= AValue*2 + 3;
end;

constructor TShiftSimpleScheduleSheet.Create(const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const ANames: TStrVector);
begin
  FNames:= ANames;
  inherited Create(AGrid.Worksheet, AGrid, AFont);
  FSelectedIndex:= -1;
  Writer.Grid.OnMouseDown:= @MouseDown;
  Writer.Grid.OnKeyDown:= @KeyDown;
  Writer.SetBordersColor(clBlack);
end;

procedure TShiftSimpleScheduleSheet.Draw(const ASchedules: TShiftScheduleVector);
var
  i: Integer;
begin
  Writer.Clear;
  if Length(ASchedules)=0 then Exit;

  FSchedules:= ASchedules;
  if (not IsSelected) and (Length(FSchedules)>0) then
    FSelectedIndex:= 0;

  Writer.BeginEdit;
  CaptionDraw;
  for i:=0 to High(FSchedules) do
    LineDraw(i, i=FSelectedIndex);
  if Length(FSchedules)>0 then
    Writer.SetFrozenRows(1);

  Writer.EndEdit;
  Writer.DrawBorders(Length(FSchedules)*2 + 2, 1, cbtTop);
end;

end.

