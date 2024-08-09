unit UTimetableSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet, fpstypes,
  DateUtils,
  //Project utils
  {UUtils,  UTypes,  UWorkHours,} UConst, UCalendar, UTimetable, UTimingSheet,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, DK_DateUtils, DK_StrUtils, DK_SheetTypes,
  DK_Math, DK_Color;

const
  BOLD_LINE_STYLE = lsMedium;
  DOUBLE_LINE_STYLE = lsDouble;
  BORDER_STYLE_DEFAULT = lsThin;

type

  { TYearTimetableSheet }
  //годовой табель
  TYearTimetableSheet = class (TDateSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      DAY_COLUMN_WIDTH = 40;       //ширина столбцов дней месяца
      PERIOD_COLUMN_WIDTH = 70;    //ширина столбца "Месяц"
      DAYSCOUNT_COLUMN_WIDTH = 40;
      TOTALHOURS_COLUMN_WIDTH = 40;
      OVERHOURS_COLUMN_WIDTH = 60;
      NIGHTHOURS_COLUMN_WIDTH = 50;
      HOLIDAYHOURS_COLUMN_WIDTH = 80;
      SKIP_COLUMN_WIDTH1 = 50;
      SKIP_COLUMN_WIDTH2 = 40;
      SKIP_COLUMN_WIDTH3 = 85;
      TITLE_HEIGHT1 = 25;
      TITLE_HEIGHT2 = 20;
    var
      FTimetables: TTimetableVector;
      FTimetableTotals: TTimetableTotals;
      FRecrutDate, FDismissDate: TDate;
      FYear: Word;
      FName: String;
      FResumeType: Byte;

    function IsNeedCaption: Boolean;
    procedure CaptionDraw;
    procedure BlankDraw;
    procedure TotalsDraw(const ARow, ACol: Integer;
                         const AShiftCount, AWorkDaysCount, ASumHoursTotal,
                               ASumHoursOver, ASumHoursNight, ASumHoursHoliday,
                               ASkipDaysCount, ASumHoursSkip: Integer;
                         const AMarksSkipStr, ADaysHoursSkipStr: String;
                         const AIsMonth: Boolean;
                         const AFontBold: Boolean=False);
    procedure TotalsQuartAndYearDraw;
    procedure MonthDraw(const AMonth: Byte);

    function MonthToFirstRow(const AMonth: Byte): Integer;
    function RowToMonth(const ARow: Integer): Integer;
  public
    procedure Draw(const AMonthTimetables: TTimetableVector;
                   const ATimetableTotals: TTimetableTotals;
                   const AYear: Word; const AName: String;
                   const ARecrutDate, ADismissDate: TDate;
                   const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены});
    procedure MonthRedraw(const AMonth: Integer;
                        const AMonthTimetable: TTimetable;
                        const ATimetableTotals: TTimetableTotals);

    function GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean; override;
    function DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean; override;

    procedure Select(const ADate: TDate); override;
    procedure Unselect(const ADate: TDate); override;
  end;

  { TMonthTimetableSheet }
  //Табель за месяц (форма графика)
  TMonthTimetableSheet = class (TMonthSheet)
  protected
    function SetWidths: TIntVector; override;
    procedure CaptionDraw(var R, C: Integer); override;
    procedure SelectDate(const ADate: TDate); override;
  private
    const
      NIGHTHOURS_COLUMN_WIDTH = 60;
    var
      FTimetables: TTimetableVector;
      FBeforeTotalHours: TIntVector;
      FBeforeNightHours: TIntVector;
      FHalfMonth: Boolean; //табель за половину месяца
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены};
                       const APeriodType: Byte {0-год, 1-квартал, 2-месяц};
                       const AExtraColumns: TBoolVector
                        //AExtraColumns:
                        //0 - Порядковый номер,
                        //1 - Должность (профессия)
                        //2 - Табельный номер
                        //3 - Количество дней/часов за месяц
                        //4 - Сумма часов за учетный период
                        //5 - Норма часов за учетный период
                        //6 - Отклонение от нормы часов
                        //7 - Сумма ночных часов за учетный период
                       );

    procedure Draw(const ACalendar: TCalendar;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                    //AViewParams:
                    //0 - Отображать строку ночных часов
                    //1 - Коды табеля для нерабочих дней
                   const AExportParams: TBoolVector;
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   const ATimetables: TTimetableVector;
                   const ABeforeTotalHours, ABeforeNightHours: TIntVector;
                   const AHalfMonth: Boolean
                   );
    procedure LineDraw(const AIndex: Integer); override;
  end;

  { TTimetableSheetT12 }
  //Табель за месяц (форма T-12)
  TTimetableSheetT12 = class (TMonthCustomSheet)
  protected
    function SetWidths: TIntVector; override;
    procedure SelectDate(const ADate: TDate); override;
    procedure SelectRow(const AIndex: Integer); override;
    function DateAndIndexFromCell(const ARow, ACol: Integer;
                                  out ADate: TDate;
                                  out AIndex: Integer): Boolean; override;
  private
    const
      FWriteTotalHoursIfZero = False;
      FWriteDaysCountIfZero = True;
      COL1_WIDTH = 55;
      COL2_WIDTH = 140;
      COL3_WIDTH = 70;
      COL_DAYS_WIDTH = 32;
      COL_HALF_WIDTH = 50;
      RESUME1_COLUMN_WITH = 40;
      RESUME2_COLUMN_WITH = 50;
      RESUME3_COLUMN_WITH = 50;
      RESUME4_COLUMN_WITH = 50;
      RESUME5_COLUMN_WITH = 50;
      RESUME6_COLUMN_WITH = 30;
      RESUME7_1_COLUMN_WITH = 20;
      RESUME7_2_COLUMN_WITH = 30;
      RESUME8_COLUMN_WITH = 40;
      RESUME9_COLUMN_WITH = 50;
      RESUME10_COLUMN_WITH = 50;
      ROW1_HEIGHT = 35;
      ROW2_HEIGHT = 20;
      ROW3_HEIGHT = 20;
      ROW4_HEIGHT = 65;
      ROW5_HEIGHT = 20;
      LINE_ROW_HEIGHT = 30;
    var
      FTimetables: TTimetableVector;
      FCalendar: TCalendar;
      FYear, FMonth: Word;
      FHalfMonth: Boolean;
      FNeedTopBottom: Boolean;
      FBorderStyle: Byte;

    procedure TopDraw;
    procedure BottomDraw;
    procedure CaptionDraw(const ANeedRepeatTitle: Boolean);
    procedure CaptionBordersDraw(const AFirstRow: Integer);
    procedure LineBordersDraw(const AFirstRow: Integer);

    function IndexToRow(const AIndex: Integer): Integer;
    function RowToIndex(const ARow: Integer): Integer;
    function ColToDate(const ACol: Integer): TDate;
    function DateToCol(const ADate: TDate): Integer;
  public
    procedure Draw(const ACalendar: TCalendar;
                   const ATimetables: TTimetableVector;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANeedTopBottom, AHalfMonth, ANeedRepeatTitle, ANeedPageNumber: Boolean;
                   const ABorderStyle: Byte = 1);
    procedure LineDraw(const AIndex: Integer); override;
  end;

implementation

{ TYearTimetableSheet }

function TYearTimetableSheet.SetWidths: TIntVector;
var
  i, W: Integer;
begin
  VDim(Result{%H-}, 25);
  W:= Max(PERIOD_COLUMN_WIDTH, SWidth('II полугодие', Font.Name, Font.Size, [fsBold]));
  Result[0]:= W;
  W:= Max(DAY_COLUMN_WIDTH, SWidth('XX/X/X', Font.Name, Font.Size, [{fsBold}]));
  for i:= 1 to 16 do Result[i]:= W;
  Result[17]:= Max(DAYSCOUNT_COLUMN_WIDTH, SWidth('(смен)', Font.Name, Font.Size, [fsBold]));
  Result[18]:= Max(TOTALHOURS_COLUMN_WIDTH, Result[17]);
  Result[19]:= Max(OVERHOURS_COLUMN_WIDTH, SWidth('урочных', Font.Name, Font.Size, [fsBold]));
  Result[20]:= Max(NIGHTHOURS_COLUMN_WIDTH, SWidth('ночных', Font.Name, Font.Size, [fsBold]));
  Result[21]:= Max(HOLIDAYHOURS_COLUMN_WIDTH, SWidth('праздничных', Font.Name, Font.Size, [fsBold]));
  Result[22]:= Max(SKIP_COLUMN_WIDTH1, SWidth('неявок', Font.Name, Font.Size, [fsBold]));
  Result[23]:= Max(SKIP_COLUMN_WIDTH2, SWidth(' код ', Font.Name, Font.Size, [fsBold]));
  Result[24]:= Max(SKIP_COLUMN_WIDTH3, SWidth('количество', Font.Name, Font.Size, [fsBold]));
end;

function TYearTimetableSheet.IsNeedCaption: Boolean;
begin
  Result:= (not Writer.HasGrid) and (not SEmpty(FName));
end;

procedure TYearTimetableSheet.CaptionDraw;
var
  R, C, i: Integer;
  S: String;
begin
  C:= 1;
  R:= 1;
  if IsNeedCaption then
  begin
    Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
    Writer.SetAlignment(haLeft, vaCenter);
    S:=  FName + '. Табель учета рабочего времени за ' + IntToStr(FYear) + ' год';
    Writer.WriteText(R, C, R, C+24, S);
    R:= R+1;
  end;
  Writer.AddCellBGColorIndex(R, C, R+3, C+24, TITLE_COLOR_INDEX);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C, R+3, C, 'Месяц', cbtOuter);
  C:= C+1;
  Writer.WriteText(R, C, R, C+15, 'Отметки о явках и неявках на работу по числам месяца' , cbtOuter);
  C:= C+16;
  Writer.WriteText(R, C, R, C+4, 'Итого отработано', cbtOuter);
  Writer.SetAlignment(haCenter, vaTop);
  case FResumeType of
    0: S:= 'дней';
    1: S:= 'смен';
    2: S:= 'дней' + SYMBOL_BREAK + '(смен)';
  end;
  Writer.WriteText(R+1, C, R+3, C, S, cbtOuter);
  Writer.WriteText(R+1, C+1, R+1, C+4,  'часов', cbtOuter);
  Writer.WriteText(R+2, C+1, R+3, C+1,  'всего', cbtOuter);
  Writer.WriteText(R+2, C+2, R+2, C+4,  'из них', cbtOuter);
  Writer.WriteText(R+3, C+2, 'сверх-' + SYMBOL_BREAK + 'урочных', cbtOuter);
  Writer.WriteText(R+3, C+3, 'ночных', cbtOuter);
  Writer.WriteText(R+3, C+4, 'выходных,' + SYMBOL_BREAK + 'праздничных', cbtOuter);
  Writer.SetAlignment(haCenter, vaCenter);
  C:= C+5;
  S:= 'Коли-' + SYMBOL_BREAK + 'чество' + SYMBOL_BREAK +
      'неявок'+ SYMBOL_BREAK + SYMBOL_BREAK +'дней'+ SYMBOL_BREAK +
      EMPTY_MARK + SYMBOL_BREAK +'часов';
  Writer.WriteText(R, C, R+3, C, S, cbtOuter);
  C:= C+1;
  Writer.WriteText(R, C, R, C+1, 'Из них по причинам', cbtOuter);
  Writer.WriteText(R+1, C, R+3, C, 'код', cbtOuter);
  Writer.WriteText(R+1, C+1, R+3, C+1, 'количество' + SYMBOL_BREAK +'дней (часов)', cbtOuter);
  Writer.SetRowHeight(R, TITLE_HEIGHT1);
  Writer.SetRowHeight(R+1,TITLE_HEIGHT2);
  Writer.SetRowHeight(R+2,TITLE_HEIGHT2);
  Writer.SetRowHeight(R+3,2*TITLE_HEIGHT2);
  R:= 2 + Ord(IsNeedCaption);
  C:= 1;
  for i:= 1 to 15 do
  begin
    C:= C + 1;
    Writer.WriteNumber(R, C, R+1, C, i, cbtOuter);
    Writer.WriteNumber(R+2, C, i+15, cbtOuter);
  end;
  C:= 17;
  Writer.WriteText(R, C, R+1, C, OUTSIDEMONTHSTR, cbtOuter);
  Writer.WriteNumber(R+2, C, 31, cbtOuter);
end;

procedure TYearTimetableSheet.BlankDraw;
var
  R,C,Y1,Y2, DeltaR, DeltaR2: Integer;

  procedure ClearDays(const AFirstRow: Integer);
  var
    i, j: Integer;
  begin
    for i:= AFirstRow to AFirstRow+3*DeltaR-1 do
      for j:= Y1 to Y2 do
        Writer.WriteText(i, j, EmptyStr, cbtOuter);
  end;

  procedure ClearResume(const AFirstRow: Integer; const AIs30Days: Boolean = False);
  var
    i: Integer;
  begin
    for i:= Y2+1 to Y2+5 do
      Writer.WriteText(AFirstRow, i, AFirstRow+DeltaR-1, i, EmptyStr, cbtOuter);
    Writer.WriteText(AFirstRow, Y2+6, AFirstRow+1, Y2+6, EmptyStr, cbtOuter);
    Writer.WriteText(AFirstRow+2, Y2+6, AFirstRow+3, Y2+6, EmptyStr, cbtOuter);
    for i:= Y2+7 to Y2+8 do
      Writer.WriteText(AFirstRow, i, AFirstRow+DeltaR-1, i, EmptyStr, cbtOuter);
    for i:= 0 to 1 do
      Writer.WriteText(AFirstRow+i, Y2, OUTSIDEMONTHSTR, cbtOuter);
    Writer.AddCellBGColorIndex(AFirstRow, Y2, AFirstRow+1, Y2, OUTSIDEMONTH_COLOR_INDEX);
    if AIs30Days then
    begin
      for i:= 2 to 3 do
        Writer.WriteText(AFirstRow+i, Y2, OUTSIDEMONTHSTR, cbtOuter);
      Writer.AddCellBGColorIndex(AFirstRow+2, Y2,AFirstRow+3, Y2, OUTSIDEMONTH_COLOR_INDEX);
    end;
  end;

  procedure FebOutside(const AFirstRow: Integer);
  var
    i, j, n: Integer;
  begin
    n:= 30-DaysInAMonth(FYear, 2){DaysInPeriod(2, FYear)};
    for i:= 2 to 3 do
      for j:= Y2 downto Y2-n do
      begin
        Writer.WriteText(AFirstRow+i, j, OUTSIDEMONTHSTR, cbtOuter);
        Writer.AddCellBGColorIndex(AFirstRow+i, j, OUTSIDEMONTH_COLOR_INDEX);
      end;
  end;

  procedure ClearLine(const AFirstRow: Integer);
  var
    i: Integer;
  begin
    Writer.WriteText(AFirstRow, C+1, AFirstRow+1, Y2, EmptyStr, cbtOuter);
    for i:= Y2+1 to Y2+5 do
      Writer.WriteText(AFirstRow, i, AFirstRow+1, i, EmptyStr, cbtOuter);
    for i:= 0 to 1 do
      Writer.WriteText(AFirstRow+i, Y2+6, EmptyStr, cbtOuter);
    for i:= Y2+7 to Y2+8 do
      Writer.WriteText(AFirstRow, i, AFirstRow+1, i, EmptyStr, cbtOuter);
    Writer.AddCellBGColorIndex(AFirstRow, 1, AFirstRow+1, 25, TITLE_COLOR_INDEX);
  end;

begin
  C:= 1;
  R:= 5 + Ord(IsNeedCaption);
  Y1:= C + 1;
  Y2:= C + 16;
  DeltaR:= 4;
  DeltaR2:= 2;
  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  ClearDays(R);
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Январь', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Февраль', cbtOuter);
  ClearResume(R);
  FebOutside(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Март', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR2-1, C, 'I квартал', cbtOuter);
  ClearLine(R);
  R:= R + DeltaR2;
  ClearDays(R);
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Апрель', cbtOuter);
  ClearResume(R, True);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Май', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Июнь', cbtOuter);
  ClearResume(R, True);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR2-1, C, 'II квартал', cbtOuter);
  ClearLine(R);
  R:= R + DeltaR2;
  Writer.WriteText(R, C, R+DeltaR2-1, C, 'I полугодие', cbtOuter);
  ClearLine(R);
  R:= R + DeltaR2;
  ClearDays(R);
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Июль', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Август', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Сентябрь', cbtOuter);
  ClearResume(R, True);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR2-1, C, 'III квартал', cbtOuter);
  ClearLine(R);
  R:= R + DeltaR2;
  ClearDays(R);
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Октябрь', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Ноябрь', cbtOuter);
  ClearResume(R, True);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR-1, C, 'Декабрь', cbtOuter);
  ClearResume(R);
  R:= R + DeltaR;
  Writer.WriteText(R, C, R+DeltaR2-1, C, 'IV квартал', cbtOuter);
  ClearLine(R);
  R:= R + DeltaR2;
  Writer.WriteText(R, C, R+DeltaR2-1, C, 'II полугодие', cbtOuter);
  ClearLine(R);
  R:= R + DeltaR2;
  Writer.WriteText(R, C, R+DeltaR2-1, C, IntToStr(FYear) + ' ГОД', cbtOuter);
  ClearLine(R);
end;

procedure TYearTimetableSheet.TotalsDraw(const ARow, ACol: Integer;
                         const AShiftCount, AWorkDaysCount, ASumHoursTotal,
                               ASumHoursOver, ASumHoursNight, ASumHoursHoliday,
                               ASkipDaysCount, ASumHoursSkip: Integer;
                         const AMarksSkipStr, ADaysHoursSkipStr: String;
                         const AIsMonth: Boolean;
                         const AFontBold: Boolean=False);
var
  R, C: Integer;

  procedure DrawSumHours(const H: Integer);
  begin
    if H>0 then
      WriteCellHours(Writer, R, C, R+1 + 2*Ord(AIsMonth), C, H)
    else
      Writer.WriteText(R, C, R+1 + 2*Ord(AIsMonth), C, EmptyStr, cbtOuter);
  end;

begin
  R:= ARow;
  C:= ACol;

  if AFontBold then
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack)
  else
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  case FResumeType of
    0: Writer.WriteNumber(R, C, R+1 + 2*Ord(AIsMonth), C, AWorkDaysCount, cbtOuter);
    1: Writer.WriteNumber(R, C, R+1 + 2*Ord(AIsMonth), C, AShiftCount, cbtOuter);
    2: Writer.WriteText(R, C, R+1 + 2*Ord(AIsMonth), C, IntToStr(AWorkDaysCount)+
                  SYMBOL_BREAK + '('+ IntToStr(AShiftCount) + ')', cbtOuter);
  end;
  C:= C+1;
  WriteCellHours(Writer, R, C, R+1 + 2*Ord(AIsMonth), C, ASumHoursTotal);
  C:= C+1;
  DrawSumHours(ASumHoursOver);
  C:= C+1;
  DrawSumHours(ASumHoursNight);
  C:= C+1;
  DrawSumHours(ASumHoursHoliday);
  C:= C+1;
  if (ASkipDaysCount>0) or (ASumHoursSkip>0) then
  begin
    Writer.WriteNumber(R, C, R+Ord(AIsMonth), C, ASkipDaysCount, cbtOuter);
    WriteCellHours(Writer, R+1 + Ord(AIsMonth), C, R+1+2*Ord(AIsMonth), C, ASumHoursSkip);
  end
  else begin
    Writer.WriteText(R, C, R+Ord(AIsMonth), C, EmptyStr, cbtOuter);
    Writer.WriteText(R+1 + Ord(AIsMonth), C, R+1+2*Ord(AIsMonth), C, EmptyStr, cbtOuter);
  end;
  if AIsMonth then
  begin
    Writer.SetAlignment(haCenter, vaTop);
    C:= C+1;
    Writer.WriteText(R, C, R+1 + 2*Ord(AIsMonth), C, AMarksSkipStr, cbtOuter);
    C:= C+1;
    Writer.WriteText(R, C, R+1 + 2*Ord(AIsMonth), C, ADaysHoursSkipStr, cbtOuter);
    Writer.SetAlignment(haCenter, vaCenter);
  end;
end;

procedure TYearTimetableSheet.TotalsQuartAndYearDraw;
var
  R, C, i: Integer;

  function TotalsFirstRow(const AInd: Integer): Integer;
  begin
    case AInd of
    1: Result:= 17;
    2: Result:= 31;
    3: Result:= 47;
    4: Result:= 61;
    5: Result:= 33;
    6: Result:= 63;
    7: Result:= 65;
    end;
    Result:= Result + Ord(IsNeedCaption);
  end;

begin
  C:= 18;
  for i:= 1 to 7 do
  begin
    R:= TotalsFirstRow(i);
    TotalsDraw(R,C, FTimetableTotals.ShiftCount[i],
               FTimetableTotals.WorkDaysCount[i], FTimetableTotals.TotalHours[i],
               FTimetableTotals.OverHours[i], FTimetableTotals.NightHours[i],
               FTimetableTotals.HolidayHours[i], FTimetableTotals.SkipDaysCount[i],
               FTimetableTotals.SkipHours[i], FTimetableTotals.SkipMarksStr[i],
               FTimetableTotals.SkipDaysHoursStr[i], False);
  end;
end;

procedure TYearTimetableSheet.MonthDraw(const AMonth: Byte);
var
  R, C, FirstCol, i: Integer;
  Timetable: TTimetable;

  procedure SetColor(const AInd, ARow, ACol: Integer; const AFirstLine: Boolean);
  var
    X, Y: Integer;
  begin
    X:= Timetable.IsManualChanged[AInd];
    Y:= Timetable.Calendar.DayStatuses[AInd];
    if (X=MANUAL_YES) and (not AFirstLine) then
      Writer.AddCellBGColorIndex(ARow, ACol, MANUAL_COLOR_INDEX)
    else if Y=DAY_STATUS_HOLIDAY then
      Writer.AddCellBGColorIndex(ARow, ACol, HOLIDAY_COLOR_INDEX)
    else if Y=DAY_STATUS_BEFORE then
      Writer.AddCellBGColorIndex(ARow, ACol, BEFORE_COLOR_INDEX)
    else if Timetable.HoursTotal[AInd]=0 then
      Writer.AddCellBGColorIndex(ARow, ACol, NOTWORK_COLOR_INDEX)
    else if X=MANUAL_YES then
      Writer.AddCellBGColorIndex(ARow, ACol, MANUAL_COLOR_INDEX);
  end;

  procedure DrawNonExistent(const ARow, ACol: Integer);
  var
    j: Integer;
  begin
    for j:=0 to 1 do
    begin
      Writer.AddCellBGColorIndex(ARow+j, ACol, OUTSIDEMONTH_COLOR_INDEX);
      Writer.WriteText(ARow+j,ACol, STRMARK_NONEXISTENT, cbtOuter);
    end;
  end;

  procedure DrawEmpty(const ARow, ACol: Integer);
  begin
    Writer.WriteText(ARow,ACol, EmptyStr, cbtOuter);
    Writer.WriteText(ARow+1,ACol, EmptyStr, cbtOuter);
  end;

  procedure DrawDay(const AInd, ARow, ACol: Integer);
  begin
    Writer.DelCellBGColorIndex(ARow, ACol);
    Writer.DelCellBGColorIndex(ARow+1, ACol);
    if Timetable.IsExists[AInd]=EXISTS_NO then
      DrawNonExistent(ARow, ACol)
    else if Timetable.IsDayInBase[AInd]=INBASE_NO then
      DrawEmpty(ARow, ACol)
    else begin
      SetColor(AInd,ARow,ACol, True);
      Writer.WriteText(ARow,ACol, Timetable.MarksStr[AInd], cbtOuter);
      SetColor(AInd,ARow+1,ACol, False);
      Writer.WriteText(ARow+1,ACol, Timetable.HoursStr[AInd], cbtOuter);
    end;
  end;

begin
  Timetable:= FTimetables[AMonth-1];

  FirstCol:= 2;
  R:= MonthToFirstRow(AMonth);

  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  for i:=0 to 14 do
  begin
    C:= FirstCol + i;
    DrawDay(i, R, C);
  end;
  for i:=15 to Timetable.Calendar.DaysCount-1 do
  begin
    C:= FirstCol + i - 15;
    DrawDay(i, R+2, C);
  end;
  C:= FirstCol + 16;
  TotalsDraw(R, C, Timetable.ShiftCountMonth, Timetable.WorkDaysCountMonth,
             Timetable.SumHoursTotalMonth, Timetable.SumHoursOverMonth,
             Timetable.SumHoursNightMonth, Timetable.SumHoursHolidayMonth,
             Timetable.SkipDaysCountMonth, Timetable.SumHoursSkipMonth,
             Timetable.MarksSkipStrMonth, Timetable.DaysHoursSkipStrMonth, True);
end;

procedure TYearTimetableSheet.Draw(const AMonthTimetables: TTimetableVector;
                   const ATimetableTotals: TTimetableTotals;
                   const AYear: Word; const AName: String;
                   const ARecrutDate, ADismissDate: TDate;
                   const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены});
var
  i: Integer;
begin
  FTimetables:=  AMonthTimetables;
  FTimetableTotals:= ATimetableTotals;
  FRecrutDate:= ARecrutDate;
  FDismissDate:= ADismissDate;
  FYear:= AYear;
  FName:= AName;
  FResumeType:= AResumeType;

  Writer.BeginEdit;
  CaptionDraw;
  BlankDraw;
  if Length(FTimetables)>0 then
  begin
    for i:= 1 to 12 do
      MonthDraw(i);
      TotalsQuartAndYearDraw;
  end;
  if Writer.HasGrid then
    Writer.SetFrozenRows(4);
  Writer.EndEdit;
  BordersDraw(1 + Ord(IsNeedCaption));
end;

procedure TYearTimetableSheet.MonthRedraw(const AMonth: Integer;
                        const AMonthTimetable: TTimetable;
                        const ATimetableTotals: TTimetableTotals);
begin
  FTimetables[AMonth-1]:=  AMonthTimetable;
  FTimetableTotals:= ATimetableTotals;
  MonthDraw(AMonth);
  TotalsQuartAndYearDraw;
end;

function TYearTimetableSheet.MonthToFirstRow(const AMonth: Byte): Integer;
begin
  case AMonth of
    1: Result:= 5;
    2: Result:= 9;
    3: Result:= 13;
    4: Result:= 19;
    5: Result:= 23;
    6: Result:= 27;
    7: Result:= 35;
    8: Result:= 39;
    9: Result:= 43;
    10: Result:= 49;
    11: Result:= 53;
    12: Result:= 57;
  end;
  Result:= Result + Ord(IsNeedCaption);
end;

function TYearTimetableSheet.RowToMonth(const ARow: Integer): Integer;
begin
  Result:= 0;
  case ARow of
    5..8:   Result:=1;
    9..12:  Result:=2;
    13..16: Result:=3;
    19..22: Result:=4;
    23..26: Result:=5;
    27..30: Result:=6;
    35..38: Result:=7;
    39..42: Result:=8;
    43..46: Result:=9;
    49..52: Result:=10;
    53..56: Result:=11;
    57..60: Result:=12;
  end;
end;

function TYearTimetableSheet.GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean;
var
  M, D: Integer;

  function ColToDay(AFirstRow: Integer; out ADay: Integer): Boolean;
  var
    NHalf: Integer;
  begin
    Result:= False;
    ADay:= 0;
    NHalf:= (ARow-AFirstRow) div 2;
    if NHalf=0 then //первая половина
    begin
      if (ACol>=2) and (ACol<=16) then
      begin
        ADay:= ACol-1;
        Result:= True;
      end;
    end else  //вторая половина
    begin
      if (ACol>=2) and (ACol<=DaysInAMonth(FYear, M)-14) then
      begin
        ADay:= ACol+14;
        Result:= True;
      end;
    end;
  end;

begin
  Result:= False;
  ADate:= 0;
  M:= RowToMonth(ARow);
  case ARow of
    5..8:   Result:= ColToDay(5, D);
    9..12:  Result:= ColToDay(9, D);
    13..16: Result:= ColToDay(13, D);
    19..22: Result:= ColToDay(19, D);
    23..26: Result:= ColToDay(23, D);
    27..30: Result:= ColToDay(27, D);
    35..38: Result:= ColToDay(35, D);
    39..42: Result:= ColToDay(39, D);
    43..46: Result:= ColToDay(43, D);
    49..52: Result:= ColToDay(49, D);
    53..56: Result:= ColToDay(53, D);
    57..60: Result:= ColToDay(57, D);
  end;
  if Result then
  begin
    ADate:= EncodeDate(FYear, M, D);
    Result:= IsDateInPeriod(ADate, FRecrutDate, FDismissDate);
  end;
end;

function TYearTimetableSheet.DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean;
var
  D, M, Y: Word;
begin
  Result:= False;
  ARow:= 0;
  ACol:= 0;

  DecodeDate(ADate, Y, M, D);
  if Y<>FYear then Exit;

  ARow:= MonthToFirstRow(M);
  if D>15 then
  begin
    ARow:= ARow + 2;
    ACol:= D - 14;
  end
  else
    ACol:= D + 1;

  Result:= True;
end;

procedure TYearTimetableSheet.Select(const ADate: TDate);
var
  R, C: Integer;
begin
  inherited Select(ADate);
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionAddCell(R+1, C);
end;

procedure TYearTimetableSheet.Unselect(const ADate: TDate);
var
  R, C: Integer;
begin
  inherited Unselect(ADate);
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionDelCell(R+1, C);
end;

{ TMonthTimetableSheet }

function TMonthTimetableSheet.SetWidths: TIntVector;
begin
  Result:= inherited SetWidths;
  if FExtraColumns[7] then //7 - Сумма ночных часов за период
    VAppend(Result, NIGHTHOURS_COLUMN_WIDTH);
end;

procedure TMonthTimetableSheet.CaptionDraw(var R, C: Integer);
begin
  inherited CaptionDraw(R, C);

  if FExtraColumns[7] then  //7 - Сумма ночных часов за период
  begin
    C:= C + 1;
    Writer.WriteText(R, C, R+1, C, 'Ночные часы', cbtOuter);
  end;
end;

procedure TMonthTimetableSheet.SelectDate(const ADate: TDate);
begin
  if IsRowSelected and
     (FTimetables[FSelectedRowIndex1].IsExists[DayOf(ADate)-1]=EXISTS_NO) then Exit;
  inherited SelectDate(ADate);
end;

constructor TMonthTimetableSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены};
                       const APeriodType: Byte {0-год, 1-квартал, 2-месяц};
                       const AExtraColumns: TBoolVector
                        //AExtraColumns:
                        //0 - Порядковый номер,
                        //1 - Должность (профессия)
                        //2 - Табельный номер
                        //3 - Количество дней/часов за месяц
                        //4 - Сумма часов за учетный период
                        //5 - Норма часов за учетный период
                        //6 - Отклонение от нормы часов
                        //7 - Сумма ночных часов за учетный период
                       );
begin
  inherited Create(AWorksheet, AGrid, AFont, AResumeType, APeriodType, AExtraColumns);
  FResumeCaption:= 'Количество за текущий месяц';
end;

procedure TMonthTimetableSheet.Draw(const ACalendar: TCalendar;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                    //AViewParams:
                    //0 - Отображать строку ночных часов
                    //1 - Коды табеля для нерабочих дней
                   const AExportParams: TBoolVector;
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   const ATimetables: TTimetableVector;
                   const ABeforeTotalHours, ABeforeNightHours: TIntVector;
                   const AHalfMonth: Boolean);
begin
  SelectionClear;

  FTimetables:= ATimetables;
  FBeforeTotalHours:= ABeforeTotalHours;
  FBeforeNightHours:= ABeforeNightHours;
  FHalfMonth:= AHalfMonth;
  DrawCustom(ACalendar, AStaffNames, ATabNums, APostNames,
                 ANormHours, AViewParams, AExportParams);
end;

procedure TMonthTimetableSheet.LineDraw(const AIndex: Integer);
var
  R, C, i, n, d, s, SHTotal, SHNight: Integer;

  procedure DrawDay(const ARow, ACol, ADayIndex: Integer; const AEmptyDay: Boolean = False);
  begin
    if AEmptyDay then
    begin
      Writer.WriteText(ARow, ACol, EmptyStr, cbtOuter);
      if FViewParams[0] then //0 - Отображать строку ночных часов
        Writer.WriteText(ARow+1, ACol, EmptyStr, cbtOuter);
      Exit;
    end;
    if FTimetables[AIndex].IsExists[ADayIndex]=EXISTS_YES then
    begin
      if (FTimetables[AIndex].IsAbsence[ADayIndex]=ABSENCE_YES) or
         (FTimetables[AIndex].MarksMainDig[ADayIndex]=DEFINE_NO) then
      begin
        Writer.WriteText(ARow, ACol, FTimetables[AIndex].MarksMainStr[ADayIndex], cbtOuter);
        if FViewParams[0] then //0 - Отображать строку ночных часов
          Writer.WriteText(ARow+1, ACol, EmptyStr, cbtOuter);
      end
      else begin
        if FTimetables[AIndex].HoursTotal[ADayIndex]=0 then
        begin
          if FViewParams[1] then  //1 - Коды табеля для нерабочих дней
            Writer.WriteText(ARow, ACol, FTimetables[AIndex].MarksMainStr[ADayIndex], cbtOuter)
          else
            Writer.WriteText(ARow, ACol, EmptyStr, cbtOuter);
          if FViewParams[0] then //0 - Отображать строку ночных часов
            Writer.WriteText(ARow+1, ACol, EmptyStr, cbtOuter);
        end
        else begin
          DrawHours(Writer, ARow, ACol,
                    FTimetables[AIndex].HoursTotal[ADayIndex],
                    FTimetables[AIndex].HoursNight[ADayIndex],
                    FWriteTotalIfZero, FWriteNightIfZero,
                    FViewParams[0] {0 - Отображать строку ночных часов});
        end;
      end;
    end
    else begin
      Writer.WriteText(ARow, ACol, STRMARK_NONEXISTENT, cbtOuter);
      if FViewParams[0] then //0 - Отображать строку ночных часов
        Writer.WriteText(ARow+1,ACol, STRMARK_NONEXISTENT, cbtOuter);
    end;

  end;

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

  Writer.SetAlignment(haCenter, vaCenter);
  for i:= 0 to 14 do
    DrawDay(R, C+i+1, i);
  for i:= 15 to FCalendar.DaysCount-1 do
    DrawDay(R, C+i+1, i, FHalfMonth);
  for i:= FCalendar.DaysCount to 30 do
    Writer.WriteText(R, C+i+1, STRMARK_NONEXISTENT, cbtOuter);
  if FViewParams[0] then //0 - Отображать строку ночных часов
    for i:= FCalendar.DaysCount to 30 do
      Writer.WriteText(R+1, C+i+1, STRMARK_NONEXISTENT, cbtOuter);
  if FHalfMonth then
  begin
    d:= FTimetables[AIndex].WorkDaysCountHalf1;
    s:= FTimetables[AIndex].ShiftCountHalf1;
    SHTotal:= FTimetables[AIndex].SumHoursTotalHalf1;
    SHNight:= FTimetables[AIndex].SumHoursNightHalf1;
  end
  else begin
    d:= FTimetables[AIndex].WorkDaysCountMonth;
    s:= FTimetables[AIndex].ShiftCountMonth;
    SHTotal:= FTimetables[AIndex].SumHoursTotalMonth;
    SHNight:= FTimetables[AIndex].SumHoursNightMonth;
  end;
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
    DrawHours(Writer, R, C, SHTotal, SHNight,
              FWriteSumTotalIfZero, FWriteSumNightIfZero,
              FViewParams[0] {0 - Отображать строку ночных часов});
  end;

  if FPeriodType<2 then
  begin
    SHTotal:= SHTotal + FBeforeTotalHours[AIndex];
    SHNight:= SHNight + FBeforeNightHours[AIndex];
  end;
  if (FPeriodType<2) and FExtraColumns[4] then  //4 - Сумма часов за учетный период
  begin
    C:= C + 1;
    DrawHours(Writer, R, C, SHTotal, SHNight,
              FWriteSumTotalIfZero, FWriteSumNightIfZero,
              FViewParams[0] {0 - Отображать строку ночных часов});
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

  if FExtraColumns[7] then //7 - Сумма ночных часов
  begin
    C:= C + 1;
    if FHalfMonth then
      d:= FTimetables[AIndex].SumHoursNightHalf1
    else
      d:= FTimetables[AIndex].SumHoursNightMonth;
    WriteCellHours(Writer, R, C, R+n, C, d);
  end;

  if not FViewParams[0] then //0 - Отображать строку ночных часов
    Writer.SetRowHeight(R, Trunc(1.6*Writer.RowHeightDefault));
end;

{ TTimetableSheetT12 }

function TTimetableSheetT12.SetWidths: TIntVector;
var
  i: Integer;
begin
  Result:=  nil;
  VAppend(Result, COL1_WIDTH);
  VAppend(Result, COL2_WIDTH);
  VAppend(Result, COL3_WIDTH);
  for i:= 1 to 15 do
    VAppend(Result, COL_DAYS_WIDTH);
  VAppend(Result, COL_HALF_WIDTH);
  for i:= 1 to 16 do
    VAppend(Result, COL_DAYS_WIDTH);
  VAppend(Result, COL_HALF_WIDTH);
  VAppend(Result, RESUME1_COLUMN_WITH);
  VAppend(Result, RESUME2_COLUMN_WITH);
  VAppend(Result, RESUME3_COLUMN_WITH);
  VAppend(Result, RESUME4_COLUMN_WITH);
  VAppend(Result, RESUME5_COLUMN_WITH);
  VAppend(Result, RESUME6_COLUMN_WITH);
  VAppend(Result, RESUME7_1_COLUMN_WITH);
  VAppend(Result, RESUME7_2_COLUMN_WITH);
  VAppend(Result, RESUME8_COLUMN_WITH);
  VAppend(Result, RESUME9_COLUMN_WITH);
  VAppend(Result, RESUME10_COLUMN_WITH);
end;

procedure TTimetableSheetT12.SelectDate(const ADate: TDate);
var
  R, C: Integer;
begin
  if (ADate=0) or SameDate(ADate, FSelectedDate) then Exit;
  if IsDoubleRowSelected then Exit;

  SelectionExtraClear;
  FSelectedDate:= ADate;
  R:= IndexToRow(FSelectedRowIndex1);
  C:= DateToCol(FSelectedDate);
  SelectionExtraAddCell(R, C);
  SelectionExtraAddCell(R+1, C);

  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TTimetableSheetT12.SelectRow(const AIndex: Integer);
var
  R, i, j: Integer;
begin
  inherited SelectRow(AIndex);
  if AIndex<0 then Exit;
  R:= IndexToRow(AIndex);
  for i:= R to R + 1 do
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(i, j);
end;

function TTimetableSheetT12.DateAndIndexFromCell(const ARow, ACol: Integer;
                                  out ADate: TDate;
                                  out AIndex: Integer): Boolean;
var
  Index: Integer;
begin
  Result:= False;
  ADate:= 0;
  AIndex:= -1;
  if ARow>=IndexToRow(Length(FTabNums)) then Exit;
  Index:= RowToIndex(ARow);
  if Index<0 then Exit;
  ADate:= ColToDate(ACol);
  AIndex:= Index;
  Result:= True;
end;

procedure TTimetableSheetT12.TopDraw;
var
  R, C, i: Integer;
  EndDateStr, BeginDateStr: String;
const
  BIGROWHEIGHT = 25;
  SMALLROWHEIGHT = 15;
begin
  if not FNeedTopBottom then Exit;
  if FHalfMonth then
    EndDateStr:= FormatDateTime('dd.mm.yyyy', FCalendar.Dates[14])
  else
    EndDateStr:= FormatDateTime('dd.mm.yyyy', FCalendar.EndDate);
  BeginDateStr:= FormatDateTime('dd.mm.yyyy', FCalendar.BeginDate);

  Writer.SetBackgroundDefault;

  C:= 1;
  R:= 1;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.SetFont(Font.Name, Font.Size-1, [{fsBold}], clBlack);
  Writer.WriteText(R, Writer.ColCount-5, R, Writer.ColCount, 'Унифицированная форма № Т-12');
  Writer.WriteText(R+1, Writer.ColCount-5, R+1, Writer.ColCount, 'Утверждена Постановлением Госкомстата');
  Writer.WriteText(R+2, Writer.ColCount-5, R+2, Writer.ColCount, 'России от 5 января 2004 г. № 1');
  for i:= 0 to 3 do Writer.SetRowHeight(R+i, SMALLROWHEIGHT);
  R:= R + 4;
  Writer.SetFont(Font.Name, Font.Size+1, [{fsBold}], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, Writer.ColCount-1, R, Writer.ColCount, 'Код', cbtOuter);
  R:= R + 1;
  Writer.SetAlignment(haRight, vaCenter);
  Writer.WriteText(R, Writer.ColCount-5, R, Writer.ColCount-2, 'Форма по ОКУД');
  Writer.SetAlignment(haCenter, vaCenter);
  if not Writer.HasGrid then Writer.SetBorders(BOLD_LINE_STYLE, clBlack);
  Writer.WriteText(R, Writer.ColCount-1, R, Writer.ColCount, '0301007', cbtOuter);
  R:= R + 1;
  Writer.SetAlignment(haRight, vaCenter);
  Writer.WriteText(R, Writer.ColCount-4, R, Writer.ColCount-2, 'по ОКПО');
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R, Writer.ColCount-1, R, Writer.ColCount, '', cbtOuter);
  if not Writer.HasGrid then Writer.SetBordersDefault;
  Writer.SetFont(Font.Name, Font.Size+4, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, C, R, Writer.ColCount-5, EmptyStr, cbtBottom);
  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size+1, [{fsBold}], clBlack);
  if not Writer.HasGrid then Writer.SetBorders(BOLD_LINE_STYLE, clBlack);
  Writer.WriteText(R, Writer.ColCount-1, R+1, Writer.ColCount, EmptyStr, cbtOuter);
  if Writer.HasGrid then Writer.WriteText(R+2, Writer.ColCount-1, R+2, Writer.ColCount, EmptyStr, cbtTop);
  if not Writer.HasGrid then Writer.SetBordersDefault;
  Writer.SetFont(Font.Name, Font.Size-1, [{fsBold}], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(R, C, R, Writer.ColCount-5, '(наименование организации)');
  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size+4, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, C, R, Writer.ColCount-5, EmptyStr  {ADepartment}, cbtBottom);
  R:= R + 1;
  Writer.SetFont(Font.Name, Font.Size-1, [{fsBold}], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(R, C, R, Writer.ColCount-5, '(структурное подразделение)');
  R:= R + 1;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size+1, [{fsBold}], clBlack);
  Writer.WriteText(R, Writer.ColCount-17, R+1, Writer.ColCount-14, 'Номер документа', cbtOuter);
  Writer.WriteText(R, Writer.ColCount-13, R+1, Writer.ColCount-11, 'Дата составления', cbtOuter);
  Writer.WriteText(R, Writer.ColCount-9, R, Writer.ColCount-6, 'Отчетный период', cbtOuter);
  Writer.WriteText(R+1, Writer.ColCount-9, R+1, Writer.ColCount-8, 'с', cbtOuter);
  Writer.WriteText(R+1, Writer.ColCount-7, R+1, Writer.ColCount-6, 'по', cbtOuter);
  if Writer.HasGrid then
  begin
    for i:= 0 to 2 do
    begin
      Writer.WriteText(R+i, Writer.ColCount-10, EmptyStr, cbtLeft);
      Writer.WriteText(R+i, Writer.ColCount-5, EmptyStr, cbtLeft);
    end;
  end;
  R:= R + 2;
  Writer.SetFont(Font.Name, Font.Size+1, [fsBold], clBlack);
  if not Writer.HasGrid then Writer.SetBorders(BOLD_LINE_STYLE, clBlack);
  Writer.WriteText(R, Writer.ColCount-17, R, Writer.ColCount-14, EmptyStr, cbtOuter);
  Writer.WriteText(R, Writer.ColCount-13, R, Writer.ColCount-11, EndDateStr, cbtOuter);
  Writer.WriteText(R, Writer.ColCount-9, R, Writer.ColCount-8, BeginDateStr, cbtOuter);
  Writer.WriteText(R, Writer.ColCount-7, R, Writer.ColCount-6, EndDateStr , cbtOuter);
  if not Writer.HasGrid then Writer.SetBordersDefault;
  Writer.SetFont(Font.Name, Font.Size+6, [fsBold], clBlack);
  Writer.WriteText(R, C+19, R, C+24, 'ТАБЕЛЬ');
  Writer.SetRowHeight(R, BIGROWHEIGHT);
  R:= R + 1;
  Writer.WriteText(R, C, R, Writer.ColCount, 'учета рабочего времени');
  Writer.SetRowHeight(R, BIGROWHEIGHT);
end;

procedure TTimetableSheetT12.BottomDraw;
var
  R, C: Integer;
begin
  if not FNeedTopBottom then Exit;

  Writer.SetBackgroundDefault;

  R:= IndexToRow(Length(FTimetables)) + 1;
  C:= 1;
  Writer.SetFont(Font.Name, Font.Size+1, [fsBold], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, C+22, R, C+27, 'Руководитель');
  R:= R + 1;
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, C+1, 'Ответственное лицо');
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.SetFont(Font.Name, Font.Size+1, [], clBlack);
  Writer.WriteText(R, C+2, R, C+6, EmptyStr, cbtBottom);
  Writer.WriteText(R, C+8, R, C+11, EmptyStr, cbtBottom);
  Writer.WriteText(R, C+13, R, C+17, EmptyStr, cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.SetFont(Font.Name, Font.Size+1, [fsBold], clBlack);
  Writer.WriteText(R, C+22, R, C+27, 'структурного подразделения');
  Writer.SetFont(Font.Name, Font.Size+1, [], clBlack);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, C+28, R, C+31, EmptyStr, cbtBottom);  //должность
  Writer.WriteText(R, C+33, R, C+35, EmptyStr, cbtBottom);  //подпись
  Writer.WriteText(R, C+37, R, C+39, EmptyStr, cbtBottom);  //расшифровка
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteText(R, C+40, '«');
  Writer.WriteText(R, C+41, EmptyStr, cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, C+42, '»');
  Writer.WriteText(R, C+43, R, C+45, EmptyStr, cbtBottom);  //месяц
  Writer.WriteText(R, C+46, '20      г.');
  R:= R + 1;
  Writer.SetAlignment(haCenter, vaTop);
  Writer.SetFont(Font.Name, Font.Size-1, [], clBlack);
  Writer.WriteText(R, C+2, R, C+6, '(должность)');
  Writer.WriteText(R, C+8, R, C+11, '(личная подпись)');
  Writer.WriteText(R, C+13, R, C+17, '(расшифровка подписи)');
  Writer.WriteText(R, C+28, R, C+31, '(должность)');
  Writer.WriteText(R, C+33, R, C+35, '(личная подпись)');
  Writer.WriteText(R, C+37, R, C+39, '(расшифровка подписи)');
  R:= R + 1;
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.SetFont(Font.Name, Font.Size+1, [fsBold], clBlack);
  Writer.WriteText(R, C+22, R, C+27, 'Работник');
  R:= R + 1;
  Writer.WriteText(R, C+22, R, C+27, 'кадровой службы');
  Writer.SetFont(Font.Name, Font.Size+1, [], clBlack);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, C+28, R, C+31, EmptyStr, cbtBottom);  //должность
  Writer.WriteText(R, C+33, R, C+35, EmptyStr, cbtBottom);  //подпись
  Writer.WriteText(R, C+37, R, C+39, EmptyStr, cbtBottom);  //расшифровка
  Writer.SetAlignment(haRight, vaBottom);
  Writer.WriteText(R, C+40, '«');
  Writer.WriteText(R, C+41, EmptyStr, cbtBottom);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, C+42, '»');
  Writer.WriteText(R, C+43, R, C+45, EmptyStr, cbtBottom);  //месяц
  Writer.WriteText(R, C+46, '20      г.');
  R:= R + 1;
  Writer.SetAlignment(haCenter, vaTop);
  Writer.SetFont(Font.Name, Font.Size-1, [], clBlack);
  Writer.WriteText(R, C+28, R, C+31, '(должность)');
  Writer.WriteText(R, C+33, R, C+35, '(личная подпись)');
  Writer.WriteText(R, C+37, R, C+39, '(расшифровка подписи)');
end;

procedure TTimetableSheetT12.CaptionDraw(const ANeedRepeatTitle: Boolean);
var
  R, C, FirstRow, i: Integer;
begin
  Writer.SetBackgroundDefault;

  FirstRow:= 15*Ord(FNeedTopBottom) + 1;
  C:= 1;
  R:= FirstRow;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C, R+3, C, 'Номер по порядку', cbtOuter);
  Writer.WriteNumber(R+4, C, 1, cbtOuter);
  C:= C+1;
  Writer.WriteText(R, C, R+3, C, 'Фамилия, инициалы, должность (специальность, профессия)', cbtOuter);
  Writer.WriteNumber(R+4, C, 2, cbtOuter);
  C:= C+1;
  Writer.WriteText(R, C, R+3, C, 'Табельный номер', cbtOuter);
  Writer.WriteNumber(R+4, C, 3, cbtOuter);
  C:= C+1;
  Writer.WriteText(R, C, R, C+32, 'Отметки о явках и неявках на работу по числам месяца', cbtOuter);
  for i:= 0 to 14 do  Writer.WriteNumber(R+1, C+i, R+3, C+i, i+1, cbtOuter);
  Writer.WriteNumber(R+4, C, R+4, C+14, 4, cbtOuter);
  Writer.WriteText(R+1, C+15, R+3, C+15,   'Итого' + SYMBOL_BREAK + 'отрабо-'+ SYMBOL_BREAK +
                                       'тано за'+ SYMBOL_BREAK + 'I поло-'+ SYMBOL_BREAK +
                                       'вину' + SYMBOL_BREAK + 'месяца', cbtOuter);
  Writer.WriteNumber(R+4, C+15, 5, cbtOuter);
  for i:= 16 to 31 do Writer.WriteNumber(R+1, C+i, R+3, C+i, i, cbtOuter);
  Writer.WriteNumber(R+4, C+16, R+4, C+31, 6, cbtOuter);
  Writer.WriteText(R+1, C+32, R+3, C+32, 'Итого' + SYMBOL_BREAK + 'отрабо-'+ SYMBOL_BREAK +
                                       'тано за'+ SYMBOL_BREAK + 'II поло-'+ SYMBOL_BREAK +
                                       'вину' + SYMBOL_BREAK + 'месяца', cbtOuter);
  Writer.WriteNumber(R+4, C+32, 7, cbtOuter);
  for i:= 1 to 6 do Writer.WriteNumber(R+4, C+32+i, i+7, cbtOuter);
  Writer.WriteNumber(R+4, C+39, R+4, C+40, 14, cbtOuter);
  for i:= 8 to 10 do Writer.WriteNumber(R+4, C+32+i+1, i+7, cbtOuter);
  C:= C+33;
  Writer.WriteText(R,C,R,C+5, 'Итого отработано', cbtOuter);
  Writer.WriteText(R+1,C,R+3,C, 'дней', cbtOuter);
  Writer.WriteText(R+1,C+1, R+1,C+5,  'часов', cbtOuter);
  Writer.WriteText(R+2,C+1, R+3,C+1,  'всего', cbtOuter);
  Writer.WriteText(R+2,C+2, R+2,C+5,  'из них', cbtOuter);
  Writer.WriteText(R+3,C+2, 'сверх-' + SYMBOL_BREAK +
                        'уроч-' + SYMBOL_BREAK + 'ных', cbtOuter);
  Writer.WriteText(R+3,C+3, 'ночных', cbtOuter);
  Writer.WriteText(R+3,C+4, 'выход-' + SYMBOL_BREAK + 'ных,' + SYMBOL_BREAK +
                        'празд-'+ SYMBOL_BREAK + 'ничных', cbtOuter);
  Writer.WriteText(R+3,C+5, '', cbtOuter);
  C:= C+6;
  Writer.WriteText(R,C,R+3,C+1, 'Коли-' + SYMBOL_BREAK + 'чество' + SYMBOL_BREAK +
                            'неявок'+ SYMBOL_BREAK +'дней'+ SYMBOL_BREAK +'часов', cbtOuter);
  C:= C+2;
  Writer.WriteText(R,C,R,C+1, 'Из них по причинам', cbtOuter);
  Writer.WriteText(R+1,C,R+3,C, 'код', cbtOuter);
  Writer.WriteText(R+1,C+1,R+3,C+1, 'коли-'+ SYMBOL_BREAK +'чество' + SYMBOL_BREAK +
                                'дней'+ SYMBOL_BREAK +'(часов)', cbtOuter);
  C:= C+2;
  Writer.WriteText(R,C,R+3,C, 'Коли-' + SYMBOL_BREAK + 'чество' + SYMBOL_BREAK +
                          'выход-'+ SYMBOL_BREAK +'ных и'+ SYMBOL_BREAK +
                          'празд-'+ SYMBOL_BREAK +'ничных'+ SYMBOL_BREAK +'дней', cbtOuter);
  Writer.SetRowHeight(FirstRow, ROW1_HEIGHT);
  Writer.SetRowHeight(FirstRow+1, ROW2_HEIGHT);
  Writer.SetRowHeight(FirstRow+2, ROW3_HEIGHT);
  Writer.SetRowHeight(FirstRow+3, ROW4_HEIGHT);
  Writer.SetRowHeight(FirstRow+4, ROW5_HEIGHT);
  if (not Writer.HasGrid) then
  begin
    if (FBorderStyle>1) then CaptionBordersDraw(FirstRow);
    if ANeedRepeatTitle then Writer.SetRepeatedRows(FirstRow, FirstRow+4);
  end;
end;

procedure TTimetableSheetT12.CaptionBordersDraw(const AFirstRow: Integer);
var
  R, C: Integer;
begin
  C:= 1;
  R:= AFirstRow;
  case FBorderStyle of
  2: Writer.SetBorders(BOLD_LINE_STYLE, clBlack, BORDER_STYLE_DEFAULT, clBlack);
  3: Writer.SetBorders(DOUBLE_LINE_STYLE, clBlack, BORDER_STYLE_DEFAULT, clBlack);
  end;
  Writer.DrawBorders(R, C,   R+3, C, cbtAll);
  Writer.DrawBorders(R, C+1, R+3, C+1, cbtAll);
  Writer.DrawBorders(R, C+2, R+3, C+2, cbtAll);
  Writer.DrawBorders(R, C+3, R, C+35, cbtAll);
  Writer.DrawBorders(R+1, C+3, R+3, C+17, cbtAll);
  Writer.DrawBorders(R+1, C+18, R+3, C+18, cbtAll);
  Writer.DrawBorders(R+1, C+19, R+3, C+34, cbtAll);
  Writer.DrawBorders(R+1, C+35, R+3, C+35, cbtAll);
  Writer.DrawBorders(R, C+36, R+3, C+41, cbtAll);
  Writer.DrawBorders(R, C+42, R+3, C+45, cbtAll);
  Writer.DrawBorders(R, C+46, R+3, C+46, cbtAll);
  R:= R+4;
  Writer.DrawBorders(R, C,   R, C, cbtAll);
  Writer.DrawBorders(R, C+1, R, C+1, cbtAll);
  Writer.DrawBorders(R, C+2, R, C+2, cbtAll);
  Writer.DrawBorders(R, C+3, R, C+17, cbtAll);
  Writer.DrawBorders(R, C+18, R, C+18, cbtAll);
  Writer.DrawBorders(R, C+19, R, C+34, cbtAll);
  Writer.DrawBorders(R, C+35, R, C+35, cbtAll);
  Writer.DrawBorders(R, C+36, R, C+41, cbtAll);
  Writer.DrawBorders(R, C+42, R, C+45, cbtAll);
  Writer.DrawBorders(R, C+46, R, C+46, cbtAll);
  Writer.SetBordersDefault;
end;

procedure TTimetableSheetT12.LineBordersDraw(const AFirstRow: Integer);
var
  R, C: Integer;
begin
  C:= 1;
  R:= AFirstRow;
  case FBorderStyle of
  2: Writer.SetBorders(BOLD_LINE_STYLE, clBlack, BORDER_STYLE_DEFAULT, clBlack);
  3: Writer.SetBorders(DOUBLE_LINE_STYLE, clBlack, BORDER_STYLE_DEFAULT, clBlack);
  end;
  Writer.DrawBorders(R, C,    R+1, C,    cbtAll);
  Writer.DrawBorders(R, C+1,  R+1, C+1,  cbtAll);
  Writer.DrawBorders(R, C+2,  R+1, C+2,  cbtAll);
  Writer.DrawBorders(R, C+3,  R+1, C+17, cbtAll);
  Writer.DrawBorders(R, C+18, R+1, C+18, cbtAll);
  Writer.DrawBorders(R, C+19, R+1, C+34, cbtAll);
  Writer.DrawBorders(R, C+35, R+1, C+35, cbtAll);
  Writer.DrawBorders(R, C+36, R+1, C+41, cbtAll);
  Writer.DrawBorders(R, C+42, R+1, C+45, cbtAll);
  Writer.DrawBorders(R, C+46, R+1, C+46, cbtAll);
  Writer.SetBordersDefault;
end;

function TTimetableSheetT12.IndexToRow(const AIndex: Integer): Integer;
begin
  Result:= 15*Ord(FNeedTopBottom) + 6 + AIndex*2;
end;

function TTimetableSheetT12.RowToIndex(const ARow: Integer): Integer;
begin
  Result:= (ARow - 15*Ord(FNeedTopBottom) - 6) div 2;
end;

function TTimetableSheetT12.ColToDate(const ACol: Integer): TDate;
var
  d: Integer;
begin
  Result:= 0;
  if (ACol>=4) and (ACol<=18) then
    d:= ACol - 3
  else if (ACol>=20) and (ACol<=DaysInAMonth(FYear, FMonth)+4) then
    d:= ACol - 4
  else
    Exit;
  Result:= EncodeDate(FYear, FMonth, d);
end;

function TTimetableSheetT12.DateToCol(const ADate: TDate): Integer;
var
  d: Integer;
begin
  d:= DayOfDate(ADate);
  Result:= d + 3;
  if d>15 then
    Result:= Result + 1;
end;

procedure TTimetableSheetT12.Draw(const ACalendar: TCalendar;
                   const ATimetables: TTimetableVector;
                   const AStaffNames, ATabNums, APostNames: TStrVector;
                   const ANeedTopBottom, AHalfMonth, ANeedRepeatTitle, ANeedPageNumber: Boolean;
                   const ABorderStyle: Byte = 1);
var
  i, R: Integer;
begin
  SelectionClear;

  FTimetables:= ATimetables;
  FCalendar:= ACalendar;
  FYear:= YearOfDate(ACalendar.BeginDate);
  FMonth:= MonthOfDate(ACalendar.BeginDate);
  FNeedTopBottom:= ANeedTopBottom;
  FHalfMonth:= AHalfMonth;
  FStaffNames:= AStaffNames;
  FTabNums:= ATabNums;
  FPostNames:= APostNames;
  FBorderStyle:= ABorderStyle;

  Writer.BeginEdit;

  TopDraw;
  CaptionDraw(ANeedRepeatTitle);
  for i:=0 to High(FTimetables) do
    LineDraw(i);
  R:= IndexToRow(Length(FTimetables));
  if Writer.HasGrid then
    for i:= 1 to Writer.ColCount do
      Writer.WriteText(R, i, EmptyStr, cbtTop);
  BottomDraw;

  if not FNeedTopBottom then
    Writer.SetFrozenRows(IndexToRow(0)-1);
  if ANeedPageNumber then
    Writer.WorkSheet.PageLayout.Footers[HEADER_FOOTER_INDEX_ALL] := '&R страница &P (из &N)';

  Writer.EndEdit;
end;

procedure TTimetableSheetT12.LineDraw(const AIndex: Integer);
var
  R, C, i: Integer;
begin
  UnSelectDate;

  if AIndex in [FSelectedRowIndex1, FSelectedRowIndex2] then
    Writer.SetBackground(DefaultSelectionBGColor)
  else
    Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  C:= 1;
  R:= IndexToRow(AIndex);
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteNumber(R, C, R+1, C, AIndex+1, cbtOuter);
  C:= C+1;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.WriteText(R, C, R+1, C, FStaffNames[AIndex] + ',' + SYMBOL_BREAK +
                              FPostNames[AIndex], cbtOuter);
  C:= C+1;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.WriteText(R,C,R+1,C, FTabNums[AIndex], cbtOuter);
  //1 половина
  C:= C+1;
  for i:= 0 to 14 do
    DrawTimetableDay(Writer, R, C+i, i, FTimetables[AIndex]);
  DrawDaysCount(Writer, R, C+15, FTimetables[AIndex].WorkDaysCountHalf1, FWriteDaysCountIfZero, False);
  DrawTotalHours(Writer, R+1, C+15, FTimetables[AIndex].SumHoursTotalHalf1, FWriteTotalHoursIfZero);
  //2половина
  for i:= 15 to FCalendar.DaysCount-1 do
    DrawTimetableDay(Writer, R, C+i+1, i, FTimetables[AIndex], FHalfMonth);
  for i:= FCalendar.DaysCount to 30 do
  begin
    Writer.WriteText(R,C+i+1, STRMARK_NONEXISTENT, cbtOuter);   //Х в несуществующем дне
    Writer.WriteText(R+1,C+i+1, STRMARK_NONEXISTENT, cbtOuter); //Х в несуществующем дне
  end;
  Writer.WriteText(R,C+38, R+1,C+38, EmptyStr, cbtOuter);
  if not FHalfMonth then
  begin
    DrawDaysCount( Writer, R, C+32, FTimetables[AIndex].WorkDaysCountHalf2, FWriteDaysCountIfZero, False);
    DrawTotalHours(Writer, R+1, C+32, FTimetables[AIndex].SumHoursTotalHalf2, FWriteTotalHoursIfZero);
    DrawDaysCount( Writer, R, C+33, R+1,C+33, FTimetables[AIndex].WorkDaysCountMonth, FWriteDaysCountIfZero);
    DrawTotalHours(Writer, R, C+34, R+1,C+34, FTimetables[AIndex].SumHoursTotalMonth, FWriteTotalHoursIfZero);
    DrawTotalHours(Writer, R, C+35, R+1,C+35, FTimetables[AIndex].SumHoursOverMonth, False);
    DrawTotalHours(Writer, R, C+36, R+1,C+36, FTimetables[AIndex].SumHoursNightMonth, False);
    DrawTotalHours(Writer, R, C+37, R+1,C+37, FTimetables[AIndex].SumHoursHolidayMonth, False);
    if (FTimetables[AIndex].SkipDaysCountMonth>0) or (FTimetables[AIndex].SumHoursSkipMonth>0) then
    begin
      Writer.WriteNumber(R,C+39,R,C+40, FTimetables[AIndex].SkipDaysCountMonth, cbtOuter);
      WriteCellHours(Writer, R+1, C+39, R+1, C+40, FTimetables[AIndex].SumHoursSkipMonth);
    end
    else begin
      Writer.WriteText(R,C+39,R,C+40, EmptyStr, cbtOuter);
      Writer.WriteText(R+1,C+39, R+1,C+40, EmptyStr, cbtOuter);
    end;
    Writer.SetAlignment(haCenter, vaTop);
    Writer.WriteText(R,C+41, R+1,C+41, FTimetables[AIndex].MarksSkipStrMonth, cbtOuter);
    Writer.WriteText(R,C+42, R+1,C+42, FTimetables[AIndex].DaysHoursSkipStrMonth, cbtOuter);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R,C+43, R+1,C+43, FTimetables[AIndex].NotWorkDaysCountMonth, cbtOuter);
  end
  else begin
    Writer.WriteText(R,C+32, EmptyStr, cbtOuter);
    Writer.WriteText(R+1,C+32, EmptyStr, cbtOuter);
    DrawDaysCount( Writer, R, C+33, R+1, C+33, FTimetables[AIndex].WorkDaysCountHalf1, FWriteDaysCountIfZero);
    DrawTotalHours(Writer, R, C+34, R+1, C+34, FTimetables[AIndex].SumHoursTotalHalf1, FWriteTotalHoursIfZero);
    DrawTotalHours(Writer, R, C+35, R+1, C+35, FTimetables[AIndex].SumHoursOverHalf1, False);
    DrawTotalHours(Writer, R, C+36, R+1, C+36, FTimetables[AIndex].SumHoursNightHalf1, False);
    DrawTotalHours(Writer, R, C+37, R+1, C+37, FTimetables[AIndex].SumHoursHolidayHalf1, False);
    if (FTimetables[AIndex].SkipDaysCountHalf1>0) or (FTimetables[AIndex].SumHoursSkipHalf1>0) then
    begin
      Writer.WriteNumber(R,C+39,R,C+40, FTimetables[AIndex].SkipDaysCountHalf1, cbtOuter);
      WriteCellHours(Writer, R+1, C+39, R+1, C+40, FTimetables[AIndex].SumHoursSkipHalf1);
    end
    else begin
      Writer.WriteText(R,C+39,R,C+40, EmptyStr, cbtOuter);
      Writer.WriteText(R+1,C+39, R+1,C+40, EmptyStr, cbtOuter);
    end;
    Writer.SetAlignment(haCenter, vaTop);
    Writer.WriteText(R,C+41, R+1,C+41, FTimetables[AIndex].MarksSkipStrHalf1, cbtOuter);
    Writer.WriteText(R,C+42, R+1,C+42, FTimetables[AIndex].DaysHoursSkipStrHalf1, cbtOuter);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R,C+43, R+1,C+43, FTimetables[AIndex].NotWorkDaysCountHalf1, cbtOuter);
  end;
  Writer.SetRowHeight(R, LINE_ROW_HEIGHT);
  Writer.SetRowHeight(R+1, LINE_ROW_HEIGHT);
  if (not Writer.HasGrid) and (FBorderStyle>1) then
    LineBordersDraw(R);
end;

end.

