unit UCalendarSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet,
  //Project utils
  UConst, UCalendar, UWorkHours,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, DK_DateUtils, DK_StrUtils, DK_SheetTypes;

type

  { TCalendarSheet }

  TCalendarSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
  const
    COLUMNS_COUNT    = 32;
    RESUME_FIRST_ROW = 15;
    RESUME_FIRST_COL = 25;
    LEGEND_FIRST_ROW = 3;
    LEGEND_FIRST_COL = RESUME_FIRST_COL;
    MONTH_FIRST_ROWS:  array [1..12] of Byte = (3,3,3,12,12,12,21,21,21,30,30,30);
    MONTH_FIRST_COLS:  array [1..12] of Byte = (1,9,17,1,9,17,1,9,17,1,9,17);
    MONTH_RESUME_ROWS: array [1..12] of Byte = (RESUME_FIRST_ROW+4, RESUME_FIRST_ROW+5, RESUME_FIRST_ROW+6,
                                                RESUME_FIRST_ROW+8, RESUME_FIRST_ROW+9, RESUME_FIRST_ROW+10,
                                                RESUME_FIRST_ROW+13, RESUME_FIRST_ROW+14, RESUME_FIRST_ROW+15,
                                                RESUME_FIRST_ROW+17, RESUME_FIRST_ROW+18, RESUME_FIRST_ROW+19);
    QUARTER_RESUME_ROWS: array [1..4] of Byte = (RESUME_FIRST_ROW+7,
                                                 RESUME_FIRST_ROW+11,
                                                 RESUME_FIRST_ROW+16,
                                                 RESUME_FIRST_ROW+20);
    HALF_RESUME_ROWS: array [1..2] of Byte = (RESUME_FIRST_ROW+12,
                                              RESUME_FIRST_ROW+21);
    YEAR_RESUME_ROW = RESUME_FIRST_ROW+22;
  var
    FCalendar: TCalendar;
    FYear: Word;
    FRowHeight: Integer;

    procedure CaptionDraw;
    procedure LegendDraw;
    procedure ResumeLineDraw(const R, C, AColorIndex: Integer; const ACalendar: TCalendar);
    procedure ResumeTableCaptionDraw;
    procedure MonthDraw(const AMonth: Byte);
    procedure QuarterDraw(const AQuarter: Byte);
    procedure HalfDraw(const AHalf: Byte);
    procedure YearDraw;
    function GridToMonth(const ARow, ACol: Integer; out ADayInWeek, AWeekInMonth, AMonth: Integer): Boolean;
  public
    constructor Create(const AFont: TFont; const AWorksheet: TsWorksheet; const AGrid: TsWorksheetGrid = nil);

    procedure Draw(const AYearCalendar: TCalendar);

    function GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean;
    function DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean;

    procedure Select(const ADate: TDate);
    procedure Select(const ARow, ACol: Integer);
    procedure Unselect(const ADate: TDate);
    procedure Unselect(const ARow, ACol: Integer);
 end;

implementation

procedure TCalendarSheet.CaptionDraw;
begin
  Writer.SetFont(Font.Name, Font.Size+3, [fsBold], clBlack);
  Writer.WriteText(1, 1, 1, Writer.ColCount, 'ПРОИЗВОДСТВЕННЫЙ КАЛЕНДАРЬ НА ' +
               IntToStr(FYear) + ' ГОД');
end;

procedure TCalendarSheet.LegendDraw;
var
  R,C: Integer;

  procedure DrawLegendLine(const ARow, ACol, AColorIndex: Integer;
                           const ALegendValue: String);
  begin
    Writer.WriteText(ARow, ACol, EmptyStr, cbtOuter);
    Writer.AddCellBGColorIndex(ARow, ACol, AColorIndex);
    Writer.WriteText(ARow, ACol+1, ARow, Writer.ColCount, ALegendValue, cbtOuter);
    Writer.DrawBorders(ARow, ACol, ARow, ACol+1, cbtAll);
  end;

begin
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);
  R:= LEGEND_FIRST_ROW;
  C:= LEGEND_FIRST_COL;
  DrawLegendLine(R, C, HOLIDEY_COLOR_INDEX, 'Нерабочий праздничный день');
  R:= R+1;
  DrawLegendLine(R, C, OFFDAY_COLOR_INDEX, 'Нерабочий выходной день');
  R:= R+1;
  DrawLegendLine(R, C, BEFORE_COLOR_INDEX, 'Рабочий предпраздничный (сокращенный) день');
  R:= R+1;
  DrawLegendLine(R, C, WEEKDAY_COLOR_INDEX, 'Рабочий день');
  Writer.SetAlignmentDefault;
end;

procedure TCalendarSheet.ResumeTableCaptionDraw;
var
  R,C: Integer;
begin
  R:= RESUME_FIRST_ROW;
  C:= RESUME_FIRST_COL;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C, R+3, C, 'Период', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R, C, R, C+3, 'Количество дней', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, MONTHNAME_COLOR_INDEX);
  Writer.WriteText(R+1, C, R+3, C, 'Кален-'+ SYMBOL_BREAK + 'дарных', cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R+1, C, R+3, C, 'Рабочих', cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R+1, C, R+3, C, 'Выход-'+ SYMBOL_BREAK + 'ных' , cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R+1, C, R+3, C, 'Празд-'+ SYMBOL_BREAK + 'ничных', cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R, C, R, C+2, 'Рабочее время (часов)', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, MONTHNAME_COLOR_INDEX);
  Writer.WriteText(R+1, C, R+3, C, '40-часовая' + SYMBOL_BREAK + 'рабочая' + SYMBOL_BREAK + 'неделя', cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R+1, C, R+3, C, '36-часовая' + SYMBOL_BREAK + 'рабочая' + SYMBOL_BREAK + 'неделя', cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  C:= C+1;
  Writer.WriteText(R+1, C, R+3, C, '24-часовая' + SYMBOL_BREAK + 'рабочая' + SYMBOL_BREAK + 'неделя', cbtOuter);
  Writer.AddCellBGColorIndex(R+1, C, MONTHNAME_COLOR_INDEX);
  R:= RESUME_FIRST_ROW+4;
  C:= RESUME_FIRST_COL;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.WriteText(R, C,   'Январь', cbtOuter);
  Writer.WriteText(R+1, C, 'Февраль', cbtOuter);
  Writer.WriteText(R+2, C, 'Март', cbtOuter);
  R:= R+3;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C,   'I КВАРТАЛ', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, QUARTER_COLOR_INDEX);
  R:= R+1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.WriteText(R, C,   'Апрель', cbtOuter);
  Writer.WriteText(R+1, C, 'Май', cbtOuter);
  Writer.WriteText(R+2, C, 'Июнь', cbtOuter);
  R:= R+3;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C,   'II КВАРТАЛ', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, QUARTER_COLOR_INDEX);
  R:= R+1;
  Writer.WriteText(R, C,   'I ПОЛУГОДИЕ', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, HALFYEAR_COLOR_INDEX);
  R:= R+1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.WriteText(R, C,   'Июль', cbtOuter);
  Writer.WriteText(R+1, C, 'Август', cbtOuter);
  Writer.WriteText(R+2, C, 'Сентябрь', cbtOuter);
  R:= R+3;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C,   'III КВАРТАЛ', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, QUARTER_COLOR_INDEX);
  R:= R+1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.WriteText(R, C,   'Октябрь', cbtOuter);
  Writer.WriteText(R+1, C, 'Ноябрь', cbtOuter);
  Writer.WriteText(R+2, C, 'Декабрь', cbtOuter);
  R:= R+3;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(R, C,   'IV КВАРТАЛ', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, QUARTER_COLOR_INDEX);
  R:= R+1;
  Writer.WriteText(R, C,   'II ПОЛУГОДИЕ', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, HALFYEAR_COLOR_INDEX);
  R:= R+1;
  Writer.WriteText(R, C,   IntToStr(FYear) + ' ГОД', cbtOuter);
  Writer.AddCellBGColorIndex(R, C, YEAR_COLOR_INDEX);

  R:= RESUME_FIRST_ROW;
  C:= RESUME_FIRST_COL;
  Writer.DrawBorders(R, C, R+3, C+7, cbtAll);
  R:= RESUME_FIRST_ROW+4;
  C:= RESUME_FIRST_COL;
  Writer.DrawBorders(R, C, R+18, C, cbtAll);
end;

procedure TCalendarSheet.ResumeLineDraw(const R,C, AColorIndex: Integer; const ACalendar: TCalendar);
var
  i: Integer;
begin
  Writer.WriteNumber(R,C,   ACalendar.DaysCount, cbtOuter);
  Writer.WriteNumber(R,C+1, ACalendar.WorkDaysCount, cbtOuter);
  Writer.WriteNumber(R,C+2, ACalendar.OffDaysCount, cbtOuter);
  Writer.WriteNumber(R,C+3, ACalendar.HoliDaysCount, cbtOuter);

  Writer.WriteNumber(R,C+4, ACalendar.SumWorkHoursFrac(40), FRACTION_DIGITS_IN_WORKHOURS, cbtOuter);
  Writer.WriteNumber(R,C+5, ACalendar.SumWorkHoursFrac(36), FRACTION_DIGITS_IN_WORKHOURS, cbtOuter);
  Writer.WriteNumber(R,C+6, ACalendar.SumWorkHoursFrac(24), FRACTION_DIGITS_IN_WORKHOURS, cbtOuter);

  if AColorIndex> 0 then
    for i:=0 to 6 do
      Writer.AddCellBGColorIndex(R, C+i, AColorIndex);

  Writer.DrawBorders(R, C, R, C+6, cbtAll);
end;

procedure TCalendarSheet.YearDraw;
var
  R,C: Integer;
begin
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  R:= YEAR_RESUME_ROW;
  C:= RESUME_FIRST_COL+1;
  ResumeLineDraw(R,C, YEAR_COLOR_INDEX, FCalendar);
end;

procedure TCalendarSheet.HalfDraw(const AHalf: Byte);
var
  R,C: Integer;
  HalfCalendar: TCalendar;
  BD,ED: TDate;
begin
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  R:= HALF_RESUME_ROWS[AHalf];
  C:= RESUME_FIRST_COL+1;
  FirstLastDayInHalfYear(AHalf, FYear, BD, ED);
  HalfCalendar:= TCalendar.Create;
  try
    FCalendar.Cut(BD, ED, HalfCalendar);
    ResumeLineDraw(R,C, HALFYEAR_COLOR_INDEX, HalfCalendar);
  finally
    FreeAndNil(HalfCalendar);
  end;
end;

procedure TCalendarSheet.QuarterDraw(const AQuarter: Byte);
var
  R,C: Integer;
  QuarterCalendar: TCalendar;
  BD,ED: TDate;
begin
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  R:= QUARTER_RESUME_ROWS[AQuarter];
  C:= RESUME_FIRST_COL+1;
  FirstLastDayInQuarter(AQuarter, FYear, BD, ED);
  QuarterCalendar:= TCalendar.Create;
  try
    FCalendar.Cut(BD, ED, QuarterCalendar);
    ResumeLineDraw(R,C, QUARTER_COLOR_INDEX, QuarterCalendar);
  finally
    FreeAndNil(QuarterCalendar);
  end;
end;

procedure TCalendarSheet.MonthDraw(const AMonth: Byte);
var
  R,C, i,j: Integer;
  MonthCalendar: TCalendar;
begin
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  R:= MONTH_FIRST_ROWS[AMonth];
  C:= MONTH_FIRST_COLS[AMonth];
  Writer.WriteText(R,C,R,C+6, SUpper(MONTHSNOM[AMonth]), cbtOuter);
  Writer.AddCellBGColorIndex(R, C, MONTHNAME_COLOR_INDEX);

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
  MonthCalendar:= TCalendar.Create;
  try
    FCalendar.Cut(FirstDayInMonth(AMonth, FYear), LastDayInMonth(AMonth, FYear), MonthCalendar);
    for i:=0 to MonthCalendar.DaysCount - 1 do
    begin
      R:= MONTH_FIRST_ROWS[AMonth] + MonthCalendar.WeekNumsInMonth[i] + 1;
      C:= MONTH_FIRST_COLS[AMonth] + MonthCalendar.DayNumsInWeek[i] - 1;
      Writer.WriteNumber(R,C,i+1,cbtOuter);
      Writer.AddCellBGColorIndex(R, C, MonthCalendar.DayStatuses[i]);
    end;
    R:= MONTH_RESUME_ROWS[AMonth];
    C:= RESUME_FIRST_COL+1;
    ResumeLineDraw(R,C, 0, MonthCalendar);
  finally
    FreeAndNil(MonthCalendar);
  end;

  //устранение проблем с отображением границ
  R:= MONTH_FIRST_ROWS[AMonth];
  C:= MONTH_FIRST_COLS[AMonth];
  Writer.DrawBorders(R, C, R+7, C+6, cbtAll);
end;

function TCalendarSheet.SetWidths: TIntVector;
var
  i, W: Integer;
begin
  Result:= nil;
  VDim(Result, COLUMNS_COUNT);

  W:= 30;
  for i:= 0 to 23 do Result[i]:= W;

  W:= 110;
  Result[24]:= W;

  W:= 60;
  for i:= 25 to 28 do Result[i]:= W;

  W:= 80;
  for i:= 29 to 31 do Result[i]:= W;
end;

constructor TCalendarSheet.Create(const AFont: TFont; const AWorksheet: TsWorksheet;
  const AGrid: TsWorksheetGrid);
begin
  inherited Create(AWorksheet, AGrid, AFont);
  FRowHeight:= 24;
  Writer.SetBordersColor(clBlack);
end;

procedure TCalendarSheet.Draw(const AYearCalendar: TCalendar);
var
  i: Integer;
begin
  FCalendar:= AYearCalendar;
  FYear:= YearOfDate(FCalendar.BeginDate);
  Writer.BeginEdit;
  CaptionDraw;
  LegendDraw;
  ResumeTableCaptionDraw;
  for i:=1 to 12 do MonthDraw(i);
  for i:=1 to 4  do QuarterDraw(i);
  for i:=1 to 2  do HalfDraw(i);
  YearDraw;
  Writer.WriteText(13,25, EmptyStr);

  Writer.SetRowHeight(2, FRowHeight-4);
  for i:= 3 to Writer.RowCount-1 do
    Writer.SetRowHeight(i, FRowHeight);

  Writer.EndEdit;
end;

function TCalendarSheet.GridToMonth(const ARow, ACol: Integer;
                    out ADayInWeek, AWeekInMonth, AMonth: Integer): Boolean;

  function GetMonth(const AQuart: Byte): Boolean;
  var
    m: Integer;
  begin
    GetMonth:= True;
    m:= (AQuart-1)*3 + 1;
    if (ACol>=1) and (ACol<=7) then
    begin
      ADayInWeek:= ACol;
      AMonth:= m;
    end
    else if (ACol>=9) and (ACol<=15) then
    begin
      ADayInWeek:= ACol-8;
      AMonth:= m+1;
    end
    else if (ACol>=17) and (ACol<=23) then
    begin
      ADayInWeek:= ACol-16;
      AMonth:= m+2
    end
    else begin
      GetMonth:= False;
      Exit;
    end;
    case AQuart of
    1: AWeekInMonth:= ARow-4;
    2: AWeekInMonth:= ARow-13;
    3: AWeekInMonth:= ARow-22;
    4: AWeekInMonth:= ARow-31;
    end;
  end;

begin
  Result:= False;
  ADayInWeek:= 0;
  AWeekInMonth:= 0;
  AMonth:= 0;
  if (ARow>=5) and (ARow<=10) then Result:= GetMonth(1)
  else if (ARow>=14) and (ARow<=19) then Result:= GetMonth(2)
    else if (ARow>=23) and (ARow<=28) then Result:= GetMonth(3)
      else if (ARow>=32) and (ARow<=37) then Result:= GetMonth(4);
end;

function TCalendarSheet.GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean;
var
  D,W,M: Integer;
begin
  Result:= False;
  ADate:= NULDATE;
  if GridToMonth(ARow, ACol, D,W,M) then
    Result:= MonthGridToDate(W,D,M,FYear, ADate);
end;

function TCalendarSheet.DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean;
var
  X: Integer;
begin
  ARow:= -1;
  ACol:= -1;
  Result:= False;
  if YearOfDate(ADate)<>FYear then Exit;

  X:= QuarterNumber(ADate);
  case X of
  1: ARow:= WeekNumberInMonth(ADate) + 4;
  2: ARow:= WeekNumberInMonth(ADate) + 13;
  3: ARow:= WeekNumberInMonth(ADate) + 22;
  4: ARow:= WeekNumberInMonth(ADate) + 31;
  end;

  X:= MonthNumberInQuarter(ADate);
  case X of
  1: ACol:= DayNumberInWeek(ADate);
  2: ACol:= DayNumberInWeek(ADate) + 8;
  3: ACol:= DayNumberInWeek(ADate) + 16;
  end;
  Result:= True;
end;

procedure TCalendarSheet.Select(const ADate: TDate);
var
  R, C: Integer;
begin
  DateToGrid(ADate, R, C);
  Select(R, C);
end;

procedure TCalendarSheet.Select(const ARow, ACol: Integer);
begin
  SelectionAddCell(ARow, ACol);
end;

procedure TCalendarSheet.Unselect(const ADate: TDate);
var
  R, C: Integer;
begin
  DateToGrid(ADate, R, C);
  Unselect(R, C);
end;

procedure TCalendarSheet.Unselect(const ARow, ACol: Integer);
begin
  SelectionDelCell(ARow, ACol);
end;

end.

