unit UTimingSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet, DateUtils,
  //DK packages utils
  DK_SheetTypes, DK_SheetWriter, DK_Vector,
  //Project utils
  USchedule, UWorkHours;

type

  { TDateSheet }

  TDateSheet = class(TCustomSheet)
  private
    FSelectedDates: TDateVector;
    FMultiSelect: Boolean;
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont);

    function GridToDate(const ARow, ACol: Integer; out ADate: TDate): Boolean; virtual; abstract;
    function DateToGrid(const ADate: TDate; out ARow, ACol: Integer): Boolean; virtual; abstract;

    procedure Select(const ADate: TDate); virtual;
    procedure Unselect(const ADate: TDate); virtual;
    procedure SelectionClear; override;
    function IsSelected: Boolean;
    function IsDateSelected(const ADate: TDate): Boolean;

    procedure DayInGridSelect(const ADate: TDate);

    property SelectedDates: TDateVector read FSelectedDates write FSelectedDates;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect;
  end;

  procedure AddScheduleColorIndex(const AWriter: TSheetWriter;
                                  const ARow, ACol, AInd: Integer;
                                  const ANeedNight: Boolean);
  procedure DrawDaysCount(const AWriter: TSheetWriter;
                        const ARow, ACol, ADaysCount: Integer;
                        const AWriteDaysCountIfZero, ANeedNight: Boolean);
  procedure DrawDaysOrShiftCount(const AWriter: TSheetWriter;
                                const ARow,ACol, ADaysCount, AShiftCount: Integer;
                                const AWriteDaysCountIfZero, ANeedNight: Boolean;
                                const AResumeType: Byte;
                                out AInd: Integer);
  procedure DrawMonthOutside(const AWriter: TSheetWriter;
                           const ARow, ACol: Integer;
                           const ANeedNight: Boolean;
                           const AOutsideMonthStr: String = OUTSIDEMONTHSTR);
  function LoadNumFormatString(const AValue: Integer;
                             const ADigits: Integer =  FRACTION_DIGITS_IN_WORKHOURS;
                             const ADenominator: Integer = WORKHOURS_DENOMINATOR): String;
  procedure WriteCellHours(const AWriter: TSheetWriter;
                           const ARow1, ACol1, ARow2, ACol2, AValue: Integer);
  procedure WriteCellHours(const AWriter: TSheetWriter;
                           const ARow, ACol, AValue: Integer);
  procedure DrawHours(const AWriter: TSheetWriter;
                      const ARow, ACol, ATotalHours, ANightHours: Integer;
                      const AWriteTotalIfZero, AWriteNightIfZero, ANeedNight: Boolean;
                      const AIsDefine: Integer = DEFINE_YES);
  procedure DrawMarks(const AWriter: TSheetWriter;
                    const ARow, ACol: Integer; ATotalMark, ANightMark: String;
                    const ANeedNight: Boolean;
                    const AIsDefine: Integer = DEFINE_YES);
  procedure DrawHoursOrMarks(const AWriter: TSheetWriter;
                           const ARow, ACol, ATotalHours, ANightHours: Integer;
                           const ATotalMark, ANightMark: String;
                           const AWriteTotalIfZero, AWriteNightIfZero, ANeedNight, ANeedMarks: Boolean;
                           const AIsVacation: Integer = VACATION_NO;
                           const AIsDefine: Integer = DEFINE_YES);
  procedure DrawPersonalHoursOrMarks(const AWriter: TSheetWriter;
                           const ARow, ACol, AIsExists, AIsDefine, AIsVacation, ATotalHours, ANightHours: Integer;
                           const ATotalMark, ANightMark: String;
                           const AWriteTotalIfZero, AWriteNightIfZero, ANeedNight, ANeedMarks, ANeedVacation: Boolean;
                           const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                           const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                           const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY);
  procedure ChoosePersonalScheduleData(const PS: TPersonalSchedule;
                                       const ANeedCorrect, ANeedVacation: Boolean;
                                       out WH: TWorkHours; out DC,SC: Integer; out Marks: TStrVector);

implementation

procedure ChoosePersonalScheduleData(const PS: TPersonalSchedule;
                                      const ANeedCorrect, ANeedVacation: Boolean;
                                      out WH: TWorkHours; out DC,SC: Integer; out Marks: TStrVector);
begin
  if ANeedCorrect and ANeedVacation then
  begin
    WH:= PS.HoursCorrectVacation;
    DC:= PS.DaysCountCorrectVacation;
    SC:= PS.ShiftCountCorrectVacation;
    Marks:= VCut(PS.MarkSTRCorrectVacation);
  end
  else begin
    if ANeedCorrect then
    begin
      WH:= PS.HoursCorrect;
      DC:= PS.DaysCountCorrect;
      SC:= PS.ShiftCountCorrect;
      Marks:= VCut(PS.MarkSTRCorrect);
    end
    else begin
      if ANeedVacation then
      begin
        WH:= PS.HoursDefaultVacation;
        DC:= PS.DaysCountDefaultVacation;
        SC:= PS.ShiftCountDefaultVacation;
        Marks:= VCut(PS.MarkSTRDefaultVacation);
      end
      else begin
        WH:= PS.HoursDefault;
        DC:= PS.DaysCountDefault;
        SC:= PS.ShiftCountDefault;
        Marks:= VCut(PS.MarkSTRDefault);
      end;
    end;
  end;
end;

procedure AddScheduleColorIndex(const AWriter: TSheetWriter;
                                const ARow, ACol, AInd: Integer;
                                const ANeedNight: Boolean);
begin
  AWriter.AddCellBGColorIndex(ARow, ACol, AInd);
  if ANeedNight then
    AWriter.AddCellBGColorIndex(ARow+1, ACol, AInd);
end;

procedure DrawDaysCount(const AWriter: TSheetWriter;
                        const ARow, ACol, ADaysCount: Integer;
                        const AWriteDaysCountIfZero, ANeedNight: Boolean);
begin
  if (ADaysCount>0) or AWriteDaysCountIfZero then
    AWriter.WriteNumber(ARow, ACol, ADaysCount, cbtOuter)
  else
    AWriter.WriteText(ARow, ACol, EmptyStr, cbtOuter);
  if ANeedNight then
    AWriter.WriteText(ARow+1, ACol, EmptyStr, cbtOuter);
end;

procedure DrawDaysOrShiftCount(const AWriter: TSheetWriter;
                                const ARow,ACol, ADaysCount, AShiftCount: Integer;
                                const AWriteDaysCountIfZero, ANeedNight: Boolean;
                                const AResumeType: Byte;
                                out AInd: Integer);
begin
  AInd:= Ord(AResumeType=2);
  if AResumeType=0 then
    DrawDaysCount(AWriter, ARow, ACol, ADaysCount, AWriteDaysCountIfZero, ANeedNight)
  else if AResumeType=1 then
    DrawDaysCount(AWriter, ARow, ACol, AShiftCount, AWriteDaysCountIfZero, ANeedNight)
  else begin
    DrawDaysCount(AWriter, ARow, ACol, ADaysCount, AWriteDaysCountIfZero, ANeedNight);
    DrawDaysCount(AWriter, ARow, ACol+1, AShiftCount, AWriteDaysCountIfZero, ANeedNight);
  end;
end;

procedure DrawMonthOutside(const AWriter: TSheetWriter;
                           const ARow, ACol: Integer;
                           const ANeedNight: Boolean;
                           const AOutsideMonthStr: String = OUTSIDEMONTHSTR);
begin
  AWriter.WriteText(ARow, ACol, AOutsideMonthStr, cbtOuter);
  if ANeedNight then
    AWriter.WriteText(ARow+1, ACol, AOutsideMonthStr, cbtOuter);
end;

function LoadNumFormatString(const AValue: Integer;
                             const ADigits: Integer =  FRACTION_DIGITS_IN_WORKHOURS;
                             const ADenominator: Integer = WORKHOURS_DENOMINATOR): String;
var
  i: Integer;
begin
  Result:= EmptyStr;
  if Trunc(AValue/ADenominator)*ADenominator <> AValue then
  begin
    Result:= '0.0';
    for i:= 1 to ADigits-1 do
      Result:= Result + '?';
  end;
end;

procedure WriteCellHours(const AWriter: TSheetWriter;
                         const ARow1, ACol1, ARow2, ACol2, AValue: Integer);
var
  NumFormatStr: String;
begin
  NumFormatStr:= LoadNumFormatString(AValue);
  AWriter.WriteNumber(ARow1, ACol1, ARow2, ACol2,
                 WorkHoursIntToFrac(AValue), cbtOuter, NumFormatStr);
end;

procedure WriteCellHours(const AWriter: TSheetWriter;
                         const ARow, ACol, AValue: Integer);
begin
  WriteCellHours(AWriter, ARow, ACol, ARow, ACol, AValue);
end;

procedure DrawHours(const AWriter: TSheetWriter;
                    const ARow, ACol, ATotalHours, ANightHours: Integer;
                    const AWriteTotalIfZero, AWriteNightIfZero, ANeedNight: Boolean;
                    const AIsDefine: Integer = DEFINE_YES);
begin
  if AIsDefine = DEFINE_NO then
  begin
    AWriter.WriteText(ARow, ACol, STRMARK_UNKNOWN, cbtOuter);
    if ANeedNight then
      AWriter.WriteText(ARow+1, ACol, STRMARK_UNKNOWN, cbtOuter);
    Exit;
  end;
  if (ATotalHours<>0) or AWriteTotalIfZero then
    WriteCellHours(AWriter, ARow, ACol, ATotalHours)
  else
    AWriter.WriteText(ARow, ACol, EmptyStr, cbtOuter);
  if ANeedNight then
  begin
    if (ANightHours>0) or AWriteNightIfZero then
      WriteCellHours(AWriter, ARow+1, ACol, ANightHours)
    else
      AWriter.WriteText(ARow+1, ACol, EmptyStr, cbtOuter);
  end;
end;

procedure DrawMarks(const AWriter: TSheetWriter;
                    const ARow, ACol: Integer; ATotalMark, ANightMark: String;
                    const ANeedNight: Boolean;
                    const AIsDefine: Integer = DEFINE_YES);
begin
  if AIsDefine = DEFINE_NO then
  begin
    AWriter.WriteText(ARow, ACol, STRMARK_UNKNOWN, cbtOuter);
    if ANeedNight then
      AWriter.WriteText(ARow+1, ACol, STRMARK_UNKNOWN, cbtOuter);
    Exit;
  end;
  AWriter.WriteText(ARow, ACol, ATotalMark, cbtOuter);
  if ANeedNight then
    AWriter.WriteText(ARow+1, ACol, ANightMark, cbtOuter);
end;

procedure DrawHoursOrMarks(const AWriter: TSheetWriter;
                           const ARow, ACol, ATotalHours, ANightHours: Integer;
                           const ATotalMark, ANightMark: String;
                           const AWriteTotalIfZero, AWriteNightIfZero, ANeedNight, ANeedMarks: Boolean;
                           const AIsVacation: Integer = VACATION_NO;
                           const AIsDefine: Integer = DEFINE_YES);
begin
  if (AIsVacation=VACATION_NO) and (ATotalHours=0) and ANeedMarks then
    DrawMarks(AWriter, ARow, ACol, ATotalMark, ANightMark, ANeedNight, AIsDefine)
  else
    DrawHours(AWriter, ARow, ACol, ATotalHours, ANightHours,
              AWriteTotalIfZero, AWriteNightIfZero, ANeedNight, AIsDefine);
end;

procedure DrawPersonalHoursOrMarks(const AWriter: TSheetWriter;
                                   const ARow, ACol, AIsExists, AIsDefine, AIsVacation, ATotalHours, ANightHours: Integer;
                                   const ATotalMark, ANightMark: String;
                                   const AWriteTotalIfZero,AWriteNightIfZero, ANeedNight, ANeedMarks, ANeedVacation: Boolean;
                                   const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                                   const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                                   const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY);
var
  S: String;
begin
  if AIsExists=EXISTS_NO then
  begin
    AWriter.WriteText(ARow, ACol, STRMARK_NONEXISTENT, cbtOuter);
    if ANeedNight then
      AWriter.WriteText(ARow+1, ACol, STRMARK_NONEXISTENT, cbtOuter);
    Exit;
  end;
  if (AIsVacation<>VACATION_NO) and ANeedVacation then
  begin
    case AIsVacation of
    VACATION_MAIN    : S:= AStrMarkVacationMain;
    VACATION_ADDITION: S:= AStrMarkVacationAddition;
    VACATION_HOLIDAY : S:= AStrMarkVacationHoliday;
    end;
    AWriter.WriteText(ARow, ACol, S, cbtOuter);
    if ANeedNight then
      AWriter.WriteText(ARow+1, ACol, EmptyStr, cbtOuter);
    Exit;
  end;
  DrawHoursOrMarks(AWriter, ARow, ACol, ATotalHours, ANightHours, ATotalMark, ANightMark,
                   AWriteTotalIfZero, AWriteNightIfZero, ANeedNight, ANeedMarks,
                   AIsVacation, AIsDefine);
end;

{ TDateSheet }


constructor TDateSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont);
begin
  inherited Create(AWorksheet, AGrid, AFont);
  Writer.SetBordersColor(clBlack);
  FSelectedDates:= nil;
  FMultiSelect:= False;
end;

procedure TDateSheet.Select(const ADate: TDate);
var
  R, C: Integer;
begin
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionAddCell(R, C);
  VInsAscDate(FSelectedDates, ADate);
end;

procedure TDateSheet.Unselect(const ADate: TDate);
var
  R, C: Integer;
begin
  if not IsSelected then Exit;
  if not DateToGrid(ADate, R, C) then Exit;
  SelectionDelCell(R, C);
  VDel(FSelectedDates, ADate);
end;

procedure TDateSheet.SelectionClear;
begin
  inherited SelectionClear;
  FSelectedDates:= nil;
end;

function TDateSheet.IsSelected: Boolean;
begin
  Result:= not VIsNil(FSelectedDates);
end;

function TDateSheet.IsDateSelected(const ADate: TDate): Boolean;
begin
  Result:= VIndexOfDate(FSelectedDates, ADate)>=0;
end;

procedure TDateSheet.DayInGridSelect(const ADate: TDate);
begin
  if MultiSelect then //копирование корректировки (multiselect)
  begin
    if IsDateSelected(ADate) then //повторный клик на дату - убираем выделение
      Unselect(ADate)
    else //клик по новой дате - добавляем к выделению
      Select(ADate);
  end
  else begin //корректировка (single select)
    if IsSelected then  //уже есть выделение
    begin
      //клик по новой дате - убираем старое выделение
      if not SameDate(FSelectedDates[0], ADate) then
        Unselect(FSelectedDates[0]);
    end;
    //новое выделение
    Select(ADate);
  end;
end;



end.

