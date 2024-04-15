unit UScheduleShiftSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet,
  //Project utils
  UConst, UCalendar, UWorkHours, USchedule, UTimingSheet,
  //DK packages utils
  DK_SheetWriter, DK_Vector, DK_Const, DK_DateUtils, DK_StrUtils, DK_SheetTypes,
  DK_Math;

type

  { TShiftScheduleTableSheet }

  TShiftScheduleTableSheet = class (TDateSheet)
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

  { TShiftScheduleYearSheet }
  // Годовой график сменности в виде таблицы
  TShiftScheduleYearSheet = class (TShiftScheduleTableSheet)
  private
    FSchedule: TShiftSchedule;
    FCaption: String;
    procedure BlankDraw;
    procedure ScheduleDraw;

    function RowToMonth(const ARow: Integer): Integer;
    function ColToDay(const ACol, AMonth: Integer): Integer;
    function DateToRow(const ADate: TDate): Integer;
    function DateToCol(const ADate: TDate): Integer;

    function GetCaption: String; override;
    function GetPeriodColumnName: String; override;
  public
    procedure Draw(const ACalendar: TCalendar;
                   const ASchedule: TShiftSchedule;
                   const AName: String;
                   const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean);

    function GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean; override;
    function DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean; override;

    procedure Select(const ADate: TDate); override;
    procedure Unselect(const ADate: TDate); override;
  end;

  { TShiftScheduleMonthSheet }
  //Сводная таблица графиков сменности на месяц
  TShiftScheduleMonthSheet = class (TShiftScheduleTableSheet)
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

{ TShiftScheduleTableSheet }

function TShiftScheduleTableSheet.SetWidths: TIntVector;
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

function TShiftScheduleTableSheet.IsNeedCaption: Boolean;
begin
  Result:= (not Writer.HasGrid) and (not SEmpty(GetCaption));
end;

procedure TShiftScheduleTableSheet.CaptionDraw;
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

constructor TShiftScheduleTableSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены});
begin
  FResumeType:= AResumeType;
  inherited Create(AWorksheet, AGrid, AFont);
  FResumeType:= AResumeType;
  FCalendar:= nil;
end;

{ TShiftScheduleYearSheet }



procedure TShiftScheduleYearSheet.BlankDraw;
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
  Writer.WriteText(R, C, R+DeltaR-1, C, FormatDateTime('yyyy год', FSchedule.BeginDate), cbtOuter);
  Writer.WriteText(R, C+1, R+DeltaR-1, C+31, EmptyStr, cbtOuter);
end;

procedure TShiftScheduleYearSheet.ScheduleDraw;
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

procedure TShiftScheduleYearSheet.Draw(const ACalendar: TCalendar;
                   const ASchedule: TShiftSchedule;
                   const AName: String;
                   const ANeedNight, ANeedCorrect, ANeedMarks, AScheduleNotWorkColor: Boolean);
var
  i: Integer;
begin
  FCaption:= AName;
  FCalendar:= ACalendar;
  FSchedule:= ASchedule;
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

function TShiftScheduleYearSheet.RowToMonth(const ARow: Integer): Integer;
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

function TShiftScheduleYearSheet.ColToDay(const ACol, AMonth: Integer): Integer;
begin
  Result:= 0;
  if (not Assigned(FCalendar)) or (not FCalendar.Calculated) then Exit;
  if (ACol>=2) and (ACol<=32) then
  begin
    if (ACol-1)<=DaysInPeriod(AMonth, YearOfDate(FCalendar.BeginDate)) then
      Result:= ACol-1;
  end;
end;

function TShiftScheduleYearSheet.DateToRow(const ADate: TDate): Integer;
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

function TShiftScheduleYearSheet.DateToCol(const ADate: TDate): Integer;
begin
  Result:= DayOfDate(ADate) + 1;
end;

function TShiftScheduleYearSheet.GetCaption: String;
begin
  Result:= EmptyStr;
  if SEmpty(FCaption) then Exit;
  Result:= 'График сменности "' + FCaption + '"' +
           FormatDateTime(' на yyyy год', FSchedule.BeginDate);
end;

function TShiftScheduleYearSheet.GetPeriodColumnName: String;
begin
  Result:= 'Месяц';
end;

function TShiftScheduleYearSheet.GridToDate(const ARow, ACol: Integer;
                                             out ADate: TDate): Boolean;
var
  M, D: Integer;
begin
  Result:= False;
  ADate:= NULDATE;
  if (not Assigned(FCalendar)) or (not FCalendar.Calculated) then Exit;
  M:= RowToMonth(ARow);
  D:= 0;
  if M>0 then D:= ColToDay(ACol, M);
  if D>0 then
  begin
    ADate:= EncodeDate(YearOfDate(FCalendar.BeginDate), M, D);
    Result:= True;
  end;
end;

function TShiftScheduleYearSheet.DateToGrid(const ADate: TDate;
                                             out ARow, ACol: Integer): Boolean;
begin
  Result:= False;
  ARow:= 0;
  ACol:= 0;
  if (not Assigned(FCalendar)) or (not FCalendar.Calculated) then Exit;
  if YearOfDate(ADate)<>YearOfDate(FCalendar.BeginDate) then Exit;
  ARow:= DateToRow(ADate);
  ACol:= DateToCol(ADate);
  Result:= True;
end;

procedure TShiftScheduleYearSheet.Select(const ADate: TDate);
var
  R, C: Integer;
begin
  inherited Select(ADate);
  if not FNeedNight then Exit;
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionAddCell(R+1, C);
end;

procedure TShiftScheduleYearSheet.Unselect(const ADate: TDate);
var
  R, C: Integer;
begin
  inherited Unselect(ADate);
  if not FNeedNight then Exit;
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionDelCell(R+1, C);
end;

{ TShiftScheduleMonthSheet }

procedure TShiftScheduleMonthSheet.BlankDraw;
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

procedure TShiftScheduleMonthSheet.ScheduleDraw;
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

function TShiftScheduleMonthSheet.GetCaption: String;
begin
  Result:= 'Графики сменности на ' +
           SUpper(MONTHSNOM[MonthOfDate(FCalendar.BeginDate)]) +
           FormatDateTime(' yyyy года', FCalendar.BeginDate);
end;

function TShiftScheduleMonthSheet.GetPeriodColumnName: String;
begin
  Result:= 'График';
end;

procedure TShiftScheduleMonthSheet.Draw(const ACalendar: TCalendar;
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
      W:= Max(W, SWidth(FNames[i], Font.Name, Font.Size));
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

end.

