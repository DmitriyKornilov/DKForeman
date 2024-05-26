unit UTimetableSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet, fpstypes,
  DateUtils,
  //Project utils
  {UUtils,  UTypes,  UWorkHours,} UConst, UCalendar, UTimetable, UDateSheet,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, {DK_DateUtils,} DK_StrUtils, DK_SheetTypes,
  DK_Math;

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

    function MonthFirstRow(const AMonth: Byte): Integer;
  public
    procedure Draw(const AMonthTimetables: TTimetableVector;
                   const ATimetableTotals: TTimetableTotals;
                   const AYear: Word; const AName: String;
                   const ARecrutDate, ADismissDate: TDate;
                   const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены});
    procedure MonthRedraw(const AMonth: Integer;
                        const AMonthTimetable: TTimetable;
                        const ATimetableTotals: TTimetableTotals);
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
  R:= MonthFirstRow(AMonth);

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

function TYearTimetableSheet.MonthFirstRow(const AMonth: Byte): Integer;
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

end.

