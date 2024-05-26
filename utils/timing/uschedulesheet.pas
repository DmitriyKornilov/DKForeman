unit UScheduleSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet, fpstypes,
  LCLType, Controls, DateUtils,
  //Project utils
  UUtils, UConst, UTypes, UCalendar, UWorkHours, USchedule, UDateSheet,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, DK_DateUtils, DK_StrUtils, DK_SheetTypes,
  DK_Math, DK_Color;

type

  { TVacationScheduleSheet }
  //График отпусков (Форма Т-7)
  TVacationScheduleSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
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
    procedure Draw(const AYear: Word;
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
  TPersonalMonthScheduleSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      FWriteTotalIfZero = False;
      FWriteNightIfZero = False;
      FWriteDaysCountIfZero = True;
      FWriteSumTotalIfZero = True;
      FWriteSumNightIfZero = True;
      ORDERNUM_COLUMN_WIDTH = 30;    //№п/п
      DAY_COLUMN_WIDTH = 33;         //ширина столбцов дней месяца
      STAFFNAME_COLUMN_WIDTH = 110;  //ширина столбца "ФИО"
      POSTNAME_COLUMN_WIDTH = 200;
      TABNUM_COLUMN_WIDTH = 70;
      SUMDAYS_COLUMN_WITH = 35;      //--кол-ва дней
      SUMHOURS_COLUMN_WITH = 50;     //--кол-ва часов
      DATE_COLUMN_WIDTH = 70;
      SIGN_COLUMN_WIDTH = 70;
      HOURS_COLUMN_WIDTH1 = 90;
      HOURS_COLUMN_WIDTH2 = 70;
      HOURS_COLUMN_WIDTH3 = 80;
      //FIRSTROW_TITLE_HEIGHT = 45; 3*SHeight
    var
      FYear, FMonth: Word;
      FCalendar: TCalendar;
      FSchedules: TPersonalScheduleVector;
      FBeforeSchedules: TPersonalScheduleVector;
      FViewParams: TBoolVector;
      FExtraColumns: TBoolVector;
      FExportParams: TBoolVector;
      FResumeType: Byte;
      FPeriodType: Byte;
      FSignatureType: Byte;
      FStaffNames, FTabNums, FPostNames: TStrVector;
      FNormHours: TIntVector;

      FSelectedRowIndex1: Integer;
      FSelectedRowIndex2: Integer;
      FSelectedDate: TDate;

      FCanSelect: Boolean;
      FOnSelect: TSheetEvent;

    procedure CaptionDraw;
    procedure SignatureListDraw;

    function GetIsDateSelected: Boolean;
    function GetIsDoubleRowSelected: Boolean;
    function GetIsRowSelected: Boolean;

    procedure SetCanSelect(const AValue: Boolean);

    function IndexToRow(const AIndex: Integer): Integer;
    function RowToIndex(const ARow: Integer): Integer;

    function FirstDateCol: Integer;
    function ColToDate(const ACol: Integer): TDate;
    function DateToCol(const ADate: TDate): Integer;

    procedure UnSelectDate;
    procedure SelectDate(const ADate: TDate);

    procedure UnSelectRow;
    procedure SelectRow(const AIndex: Integer);
    procedure SelectRow1(const AIndex: Integer);
    procedure SelectRow2(const AIndex: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
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
                   const ASchedules, ABeforeSchedules: TPersonalScheduleVector;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                    //AViewParams:
                    //0 - Отображать строку ночных часов,
                    //1 - Учитывать корректировки графика
                    //2 - Коды табеля для нерабочих дней
                    //3 - Учитывать отпуск
                   const AExportParams: TBoolVector
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   );
    procedure LineDraw(const AIndex: Integer);
    procedure Select(const AIndex: Integer);
    procedure SelectionClear; override;
    procedure SelectionMove(const ANewIndex: Integer);

    property IsDateSelected: Boolean read GetIsDateSelected;
    property IsRowSelected: Boolean read GetIsRowSelected;
    property IsDoubleRowSelected: Boolean read GetIsDoubleRowSelected;
    property SelectedIndex: Integer read FSelectedRowIndex1;
    property SelectedIndex2: Integer read FSelectedRowIndex2;
    property SelectedDate: TDate read FSelectedDate;
    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property OnSelect: TSheetEvent read FOnSelect write FOnSelect;
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
var
  i: Integer;
begin
  Result:=  nil;
  if FExtraColumns[0] then //0 - Порядковый номер
    VAppend(Result, ORDERNUM_COLUMN_WIDTH);
  VAppend(Result, STAFFNAME_COLUMN_WIDTH);
  if FExtraColumns[1] then //1 - Должность (профессия)
    VAppend(Result, POSTNAME_COLUMN_WIDTH);
  if FExtraColumns[2] then //2 - Табельный номер
    VAppend(Result, TABNUM_COLUMN_WIDTH);
  for i:= 1 to 31 do
    VAppend(Result, DAY_COLUMN_WIDTH);
  if FExtraColumns[3] then //3 - Количество дней/часов за месяц
  begin
    VAppend(Result, SUMDAYS_COLUMN_WITH);
    if FResumeType=2 then
      VAppend(Result, SUMDAYS_COLUMN_WITH);
    VAppend(Result, SUMHOURS_COLUMN_WITH);
  end;
  if FExtraColumns[4] then //4 - Сумма часов за учетный период
  begin
    if FPeriodType<2 then
      VAppend(Result, HOURS_COLUMN_WIDTH1);
  end;
  if FExtraColumns[5] then //5 - Норма часов за учетный период
    VAppend(Result, HOURS_COLUMN_WIDTH2);
  if FExtraColumns[6] then //6 - Отклонение от нормы часов
    VAppend(Result, HOURS_COLUMN_WIDTH3);
  if FSignatureType=1 then
  begin
    VAppend(Result, DATE_COLUMN_WIDTH);
    VAppend(Result, SIGN_COLUMN_WIDTH);
  end;
end;

procedure TPersonalMonthScheduleSheet.CaptionDraw;
var
  R, C, i: Integer;
  S: String;
begin
  C:= 0;
  R:= 1;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  if FExtraColumns[0] then //0 - Порядковый номер
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R+1, C, '№ п/п', cbtOuter);
  end;
  C:= C + 1;
  Writer.WriteText(R, C, R+1, C, 'Ф.И.О.', cbtOuter);
  if FExtraColumns[1] then //1 - Должность (профессия)
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R+1, C, 'Должность (профессия)', cbtOuter);
  end;
  if FExtraColumns[2] then //2 - Табельный номер
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R+1, C, 'Табельный номер', cbtOuter);
  end;
  S:= 'Часов за день';
  if FViewParams[0] then //0 - Отображать строку ночных часов
    S:= S + ', в том числе ночных';
  Writer.WriteText(R, C+1, R, C+31, S, cbtOuter);
  for i:= C+1 to C+FCalendar.DaysCount do
    Writer.WriteNumber(R+1, i, i-C, cbtOuter);
  for i:= C+FCalendar.DaysCount+1 to C+31 do
    Writer.WriteText(R+1, i, STRMARK_NONEXISTENT, cbtOuter);
  C:= C+31;
  if FExtraColumns[3] then //3 - Количество дней/часов за месяц
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R, C+1+Ord(FResumeType=2), 'По графику', cbtOuter);
    Writer.WriteText(R+1, C, 'дней', cbtOuter);
    if FResumeType=1 then
      Writer.WriteText(R+1, C, 'смен', cbtOuter);
    if FResumeType=2 then
    begin
      C:= C + 1;
      Writer.WriteText(R+1, C, 'смен', cbtOuter);
    end;
    C:= C + 1;
    Writer.WriteText(R+1, C, 'часов', cbtOuter);
  end;
  if FExtraColumns[4] then  //4 - Сумма часов за учетный период
  begin
    if FPeriodType<2 then
    begin
      C:= C + 1;
      if FPeriodType=0 then
        Writer.WriteText(R, C, R+1, C, 'Часы, отработанные с нарастанием за ' +
                         FormatDateTime('yyyy год', FCalendar.BeginDate), cbtOuter)
      else if FPeriodType=1 then
        Writer.WriteText(R, C, R+1, C, 'Часы, отработанные с нарастанием за ' +
                         QuarterNumberToRome(QuarterNumber(FMonth)) + ' квартал', cbtOuter);
    end;
  end;
  if FExtraColumns[5] then //5 - Норма часов за учетный период
  begin
    C:= C + 1;
    S:= 'Норма часов на ';
    case FPeriodType of
      2: S:= S + MONTHSNOM[FMonth] + ' ' + IntToStr(FYear) + ' года';
      1: S:= S + SYMBOL_BREAK + QuarterNumberToRome(QuarterNumber(FMonth)) + ' квартал ' + IntToStr(FYear) + ' года';
      0: S:= S + IntToStr(FYear) + ' год';
    end;
    Writer.WriteText(R, C, R+1, C, S, cbtOuter);
  end;
  if FExtraColumns[6] then  //6 - Отклонение от нормы часов
  begin
    C:= C + 1;
    S:= 'Отклонение от ';
    case FPeriodType of
      2: S:= S + ' месячной ';
      1: S:= S + ' квартальной ';
      0: S:= S + ' годовой ';
    end;
    S:= S + 'нормы';
    Writer.WriteText(R, C, R+1, C, S, cbtOuter);
  end;
  if FSignatureType=1 then
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R, C+1, 'Ознакомлен', cbtOuter);
    Writer.WriteText(R+1, C, 'дата', cbtOuter);
    Writer.WriteText(R+1, C+1, 'подпись', cbtOuter);
  end;
  Writer.SetRowHeight(R, 3*SHeight(Font));
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

function TPersonalMonthScheduleSheet.GetIsDateSelected: Boolean;
begin
  Result:= FSelectedDate>0;
end;

function TPersonalMonthScheduleSheet.GetIsDoubleRowSelected: Boolean;
begin
  Result:= (FSelectedRowIndex1>=0) and (FSelectedRowIndex2>=0);
end;

function TPersonalMonthScheduleSheet.GetIsRowSelected: Boolean;
begin
  Result:= (FSelectedRowIndex1>=0) and (FSelectedRowIndex2<0);
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
                FSchedules[AIndex].IsExists[i], FSchedules[AIndex].IsDefine[i],
                FSchedules[AIndex].IsVacation[i],
                WorkHours.Total[i], WorkHours.Night[i], Marks[i], EmptyStr,
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
    SHTotal:= WorkHours.SumTotal+WorkHoursBefore.SumTotal;
    SHNight:= WorkHours.SumNight+WorkHoursBefore.SumNight;
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

procedure TPersonalMonthScheduleSheet.Select(const AIndex: Integer);
begin
  SelectRow1(AIndex);
end;

procedure TPersonalMonthScheduleSheet.SetCanSelect(const AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  FCanSelect:= AValue;
  if not AValue then SelectionClear;
end;

function TPersonalMonthScheduleSheet.IndexToRow(const AIndex: Integer): Integer;
begin
  Result:= 3 + (1 + Ord(FViewParams[0])) * AIndex;
end;

function TPersonalMonthScheduleSheet.RowToIndex(const ARow: Integer): Integer;
begin
  Result:= (ARow - 3) div (1 + Ord(FViewParams[0])); //FViewParams: 0 - Отображать строку ночных часов
end;

function TPersonalMonthScheduleSheet.FirstDateCol: Integer;
begin
  //FExtraColumns: 0 - Порядковый номер, 1 - Должность (профессия), 2 - Табельный номер
  Result:= 2+Ord(FExtraColumns[0])+ Ord(FExtraColumns[1]) + Ord(FExtraColumns[2]);
end;

function TPersonalMonthScheduleSheet.ColToDate(const ACol: Integer): TDate;
var
  C1, C2: Integer;
begin
  Result:= 0;
  C1:= FirstDateCol;
  C2:= C1 + DaysInAMonth(FYear, FMonth) - 1;
  if (ACol<C1) or (ACol>C2) then Exit;
  Result:= EncodeDate(FYear, FMonth, ACol - C1 + 1);
end;

function TPersonalMonthScheduleSheet.DateToCol(const ADate: TDate): Integer;
begin
  Result:= FirstDateCol + DayOf(ADate) - 1;
end;

procedure TPersonalMonthScheduleSheet.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R, C, Index: Integer;
  D: TDate;
begin
  if not CanSelect then Exit;
  if Length(FSchedules)=0 then Exit;

  if Button=mbLeft then
  begin
    (Sender as TsWorksheetGrid).MouseToCell(X, Y, C, R);
    if (R <= 2) or (R >= IndexToRow(Length(FSchedules))) then Exit;
    D:= ColToDate(C);
    Index:= RowToIndex(R);
    if IsDoubleRowSelected then
    begin
      if (Index<>FSelectedRowIndex1) and (Index<>FSelectedRowIndex2) then
        SelectRow1(Index);
      SelectDate(D);
    end
    else begin
      if Index=FSelectedRowIndex1 then
        SelectDate(D)
      else begin
        if IsRowSelected and SSame(FTabNums[Index], FTabNums[FSelectedRowIndex1]) then
        begin
          UnselectDate;
          SelectRow2(Index);
        end
        else begin
          SelectRow1(Index);
          SelectDate(D);
        end;
      end;
    end;
  end
  else if Button=mbRight then
    UnSelectRow;
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
  FResumeType:= AResumeType;
  FPeriodType:= APeriodType;
  FSignatureType:= ASignatureType;
  FExtraColumns:= AExtraColumns;
  inherited Create(AWorksheet, AGrid, AFont);
  FSelectedRowIndex1:= -1;
  FSelectedRowIndex2:= -1;
  FSelectedDate:= 0;
  FCanSelect:= True;
  if Writer.HasGrid then
    Writer.Grid.OnMouseDown:= @MouseDown;
end;

procedure TPersonalMonthScheduleSheet.Draw(const ACalendar: TCalendar;
                   const ASchedules, ABeforeSchedules: TPersonalScheduleVector;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                    //AViewParams:
                    //0 - Отображать строку ночных часов
                    //1 - Учитывать корректировки графика
                    //2 - Коды табеля для нерабочих дней
                    //3 - Учитывать отпуск
                   const AExportParams: TBoolVector
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   );
var
  X, W, i: Integer;
begin
  FCalendar:= ACalendar;
  FYear:= YearOfDate(FCalendar.BeginDate);
  FMonth:= MonthOfDate(FCalendar.BeginDate);
  FSchedules:= ASchedules;
  FBeforeSchedules:= ABeforeSchedules;
  FViewParams:= AViewParams;
  FExportParams:= AExportParams;
  FStaffNames:= AStaffNames;
  FTabNums:= ATabNums;
  FPostNames:= APostNames;
  FNormHours:= ANormHours;

  Writer.BeginEdit;
  W:= VMaxWidth(FStaffNames, Font.Name, Font.Size+1, [], STAFFNAME_COLUMN_WIDTH);

  X:= 1+Ord(FExtraColumns[0]); //FExtraColumns: 0 - Порядковый номер
  Writer.SetColWidth(X, W);
  if FExtraColumns[2] then //FExtraColumns: 2 - Табельный номер
  begin
    W:= VMaxWidth(FTabNums, Font.Name, Font.Size+1, [], TABNUM_COLUMN_WIDTH);
    X:= 2+Ord(FExtraColumns[0])+Ord(FExtraColumns[1]); //FExtraColumns: 0 - Порядковый номер, 1 - Должность (профессия)
    Writer.SetColWidth(X, W);
  end;

  CaptionDraw;
  for i:=0 to High(FSchedules) do
    LineDraw(i);

  if FSignatureType=2 then //2-список ознакомления под таблицей
    SignatureListDraw;

  Writer.SetFrozenRows(2);
  if FExportParams[0] then //FExportParams: 0 - заголовок таблицы на каждой странице
    Writer.SetRepeatedRows(1, 2);
  if FExportParams[1] then //FExportParams: 1 - номера страниц в нижнем колонтитуле
    Writer.WorkSheet.PageLayout.Footers[HEADER_FOOTER_INDEX_ALL] := '&R страница &P (из &N)';

  X:= IndexToRow(Length(FSchedules));
  for i:= 1 to Writer.ColCount do
    Writer.WriteText(X, i, EmptyStr, cbtTop);

  Writer.EndEdit;
end;

procedure TPersonalMonthScheduleSheet.SelectDate(const ADate: TDate);
var
  R, C: Integer;
begin
  if (ADate=0) or SameDate(ADate, FSelectedDate) then Exit;
  if IsDoubleRowSelected then Exit;
  if IsRowSelected and
     (FSchedules[FSelectedRowIndex1].IsExists[DayOf(ADate)-1]=EXISTS_NO) then Exit;

  SelectionExtraClear;
  FSelectedDate:= ADate;
  R:= IndexToRow(FSelectedRowIndex1);
  C:= DateToCol(FSelectedDate);
  SelectionExtraAddCell(R, C);
  if FViewParams[0] then //строка ночных часов
    SelectionExtraAddCell(R+1, C);

  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TPersonalMonthScheduleSheet.UnSelectDate;
begin
  if not IsDateSelected then Exit;
  SelectionExtraClear;
  FSelectedDate:= 0;
  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TPersonalMonthScheduleSheet.SelectionClear;
begin
  inherited SelectionClear;
  FSelectedRowIndex1:= -1;
  FSelectedRowIndex2:= -1;
  FSelectedDate:= 0;
end;

procedure TPersonalMonthScheduleSheet.SelectionMove(const ANewIndex: Integer);
var
  OldIndex: Integer;
begin
  OldIndex:= FSelectedRowIndex1;
  SelectionClear;
  LineDraw(OldIndex);
  LineDraw(ANewIndex);
  SelectRow1(ANewIndex);
end;

procedure TPersonalMonthScheduleSheet.UnSelectRow;
begin
  SelectionClear;
  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TPersonalMonthScheduleSheet.SelectRow(const AIndex: Integer);
var
  R, i, j: Integer;
begin
  if Assigned(FOnSelect) then FOnSelect;
  R:= IndexToRow(AIndex);
  for i:= R to R + Ord(FViewParams[0]) do  //FViewParams: 0 - Отображать строку ночных часов
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(i, j);
end;

procedure TPersonalMonthScheduleSheet.SelectRow1(const AIndex: Integer);
begin
  SelectionClear;
  FSelectedRowIndex1:= AIndex;
  SelectRow(AIndex);
end;

procedure TPersonalMonthScheduleSheet.SelectRow2(const AIndex: Integer);
begin
  FSelectedRowIndex2:= AIndex;
  SelectRow(AIndex);
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
        if CutSchedule.IsDefine[i]=DEFINE_NO then
          AddScheduleColorIndex(Writer, ARow, ACol+i, NOTDEFINE_COLOR_INDEX, FNeedNight)
        else begin
          if FNeedCorrect and (CutSchedule.IsCorrection[i]=CORRECTION_YES) and
             (CutSchedule.IsVacation[i]=VACATION_NO) then
            AddScheduleColorIndex(Writer, ARow, ACol+i, CORRECT_COLOR_INDEX, FNeedNight)
          else begin
            if FScheduleNotWorkColor then
            begin
              if WorkHours.Total[i]=0 then
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
                CutSchedule.IsExists[i], CutSchedule.IsDefine[i], CutSchedule.IsVacation[i],
                WorkHours.Total[i], WorkHours.Night[i], Marks[i], EmptyStr,
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
    CutSchedule:= TPersonalSchedule.Create(FSchedule.TabNumID, FSchedule.TabNum,
                     FSchedule.RecrutDate, FSchedule.DismissDate, FSchedule.IsVacation,
                     FSchedule.PersonalCorrect,
                     FSchedule.StrMarkVacationMain, FSchedule.StrMarkVacationAddition,
                     FSchedule.StrMarkVacationHoliday);
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
  Writer.SetRowHeight(R, 15);
  R:= R + 1;
  Writer.WriteText(R, 24, R, 28, 'Утверждена Постановлением Госкомстата');
  Writer.SetRowHeight(R, 15);
  R:= R + 1;
  Writer.WriteText(R, 24, R, 28, 'России от 05.01.2004 № 1');
  Writer.SetRowHeight(R, 15);

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
  Writer.WriteText(R,  1, R, 22, '', cbtBottom);

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
  Writer.WriteText(R, 1,  R, 4,  '', cbtOuter, True, True);
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

procedure TVacationScheduleSheet.Draw(const AYear: Word;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const AFirstDates: TDateVector;
                   const ATotalCounts: TIntVector);
var
  i, R, CaptionRowCount: Integer;
begin
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
  //if Writer.HasGrid then
    Writer.SetFrozenRows(CaptionRowCount);
  //else begin
    Writer.SetRepeatedRows(CaptionRowCount-3, CaptionRowCount);
    Writer.WorkSheet.PageLayout.Footers[HEADER_FOOTER_INDEX_ALL] := '&R страница &P (из &N)';
  //end;
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
      if FNeedCorrect and (CutSchedule.IsCorrection[i]=CORRECTION_YES) then
        AddScheduleColorIndex(Writer, ARow, ACol+i, CORRECT_COLOR_INDEX, FNeedNight)
      else begin
        if FScheduleNotWorkColor then
        begin
          if WorkHours.Total[i]=0 then
            AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end
        else begin
          if (CutCalendar.DayStatuses[i]=DAY_STATUS_HOLIDAY) or
             (CutCalendar.DayStatuses[i]=DAY_STATUS_OFFDAY) then
             AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end;
      end;
      DrawHoursOrMarks(Writer, ARow, ACol+i, WorkHours.Total[i], WorkHours.Night[i],
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
      if FNeedCorrect and (FSchedules[AInd].IsCorrection[i]=CORRECTION_YES) then
        AddScheduleColorIndex(Writer, ARow, ACol+i, CORRECT_COLOR_INDEX, FNeedNight)
      else begin
        if FScheduleNotWorkColor then
        begin
          if WorkHours.Total[i]=0 then
            AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end
        else begin
          if (FCalendar.DayStatuses[i]=DAY_STATUS_HOLIDAY) or
             (FCalendar.DayStatuses[i]=DAY_STATUS_OFFDAY) then
             AddScheduleColorIndex(Writer, ARow, ACol+i, NOTWORK_COLOR_INDEX, FNeedNight);
        end;
      end;
      DrawHoursOrMarks(Writer, ARow, ACol+i, WorkHours.Total[i], WorkHours.Night[i],
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
    DrawHoursOrMarks(Writer, R, i+2, WorkHours.Total[i], WorkHours.Night[i], Marks[i],
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

