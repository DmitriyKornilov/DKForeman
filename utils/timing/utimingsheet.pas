unit UTimingSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpspreadsheetgrid, fpspreadsheet, fpstypes,
  DateUtils, Controls,
  //DK packages utils
  DK_SheetTypes, DK_SheetWriter, DK_Vector, DK_StrUtils, DK_DateUtils,
  DK_Const,
  //Project utils
  UCalendar, USchedule, UTimetable, UWorkHours;

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

  { TMonthCustomSheet }

  TMonthCustomSheet = class(TCustomSheet)
  protected
    FStaffNames, FTabNums, FPostNames, FRanks: TStrVector;

    FSelectedRowIndex1: Integer;
    FSelectedRowIndex2: Integer;
    FSelectedDate: TDate;

    FCanSelect: Boolean;
    FOnSelect: TSheetEvent;

    function GetIsDateSelected: Boolean;
    function GetIsDoubleRowSelected: Boolean;
    function GetIsRowSelected: Boolean;

    procedure SetCanSelect(AValue: Boolean);

    procedure UnSelectDate;
    procedure SelectDate(const ADate: TDate); virtual; abstract;
    function DateAndIndexFromCell(const ARow, ACol: Integer;
                                  out ADate: TDate;
                                  out AIndex: Integer): Boolean; virtual; abstract;


    procedure UnSelectRow;
    procedure SelectRow(const AIndex: Integer); virtual;
    procedure SelectRow1(const AIndex: Integer);
    procedure SelectRow2(const AIndex: Integer);

    procedure MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont);

    procedure LineDraw(const AIndex: Integer); virtual; abstract;

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

  { TMonthSheet }
  //для месячных графиков и табелей в форме графика
  TMonthSheet = class(TMonthCustomSheet)
  protected
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
      TABNUM_COLUMN_WIDTH = 80;
      SUMDAYS_COLUMN_WITH = 40;      //--кол-ва дней
      SUMHOURS_COLUMN_WITH = 60;     //--кол-ва часов
      HOURS_COLUMN_WIDTH1 = 90;
      HOURS_COLUMN_WIDTH2 = 80;
      HOURS_COLUMN_WIDTH3 = 90;
      //FIRSTROW_TITLE_HEIGHT = 45; 3*SHeight
    var
      FYear, FMonth: Word;
      FCalendar: TCalendar;
      FViewParams: TBoolVector;
      FExtraColumns: TBoolVector;
      FExportParams: TBoolVector;
      FResumeType: Byte;
      FPeriodType: Byte;
      FNormHours: TIntVector;
      FResumeCaption: String;

    function SetWidths: TIntVector; override;
    procedure SelectDate(const ADate: TDate); override;
    procedure SelectRow(const AIndex: Integer); override;
    function DateAndIndexFromCell(const ARow, ACol: Integer;
                                  out ADate: TDate;
                                  out AIndex: Integer): Boolean; override;

    procedure CaptionDraw(var R, C: Integer); virtual;

    function IndexToRow(const AIndex: Integer): Integer;
    function RowToIndex(const ARow: Integer): Integer;

    function FirstDateCol: Integer;
    function ColToDate(const ACol: Integer): TDate;
    function DateToCol(const ADate: TDate): Integer;
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены};
                       const APeriodType: Byte {0-год, 1-квартал, 2-месяц};
                       const AExtraColumns: TBoolVector);

    procedure DrawCustom(const ACalendar: TCalendar;
                   const AStaffNames, ATabNums, APostNames, ARanks: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                   const AExportParams: TBoolVector
                   );
  end;

  //для табелей форм Т-12 и Т-13

  { TMonthFormSheet }

  TMonthFormSheet = class(TMonthCustomSheet)
  protected
    FTimetables: TTimetableVector;
    FCompany, FDepartment: String;
    FCalendar: TCalendar;
    FYear, FMonth: Word;
    FHalfMonth: Boolean;
    FNeedTopBottom: Boolean;
    FNeedRanks: Boolean;
    FBorderStyle: Byte;

    procedure TopDraw; virtual; abstract;
    procedure BottomDraw; virtual; abstract;
    procedure CaptionDraw(const ANeedRepeatTitle: Boolean); virtual; abstract;
    procedure CaptionBordersDraw(const AFirstRow: Integer); virtual; abstract;
    procedure LineBordersDraw(const AFirstRow: Integer); virtual; abstract;

    function IndexToRow(const AIndex: Integer): Integer; virtual; abstract;
  public
    procedure Draw(const ACompany, ADepartment: String;
                   const ACalendar: TCalendar;
                   const ATimetables: TTimetableVector;
                   const AStaffNames, ATabNums, APostNames, ARanks: TStrVector;
                   const ANeedRanks, ANeedTopBottom, AHalfMonth: Boolean;
                   const AExportParams: TBoolVector;
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   const ABorderStyle: Byte = 1);
  end;

  procedure AddScheduleColorIndex(const AWriter: TSheetWriter;
                                  const ARow, ACol, AInd: Integer;
                                  const ANeedNight: Boolean);
  procedure DrawDaysCount(const AWriter: TSheetWriter;
                        const ARow, ACol, ADaysCount: Integer;
                        const AWriteDaysCountIfZero, ANeedNight: Boolean);
  procedure DrawDaysCount(const AWriter: TSheetWriter;
                        const ARow1, ACol1, ARow2, ACol2, ADaysCount: Integer;
                        const AWriteDaysCountIfZero: Boolean);
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

  procedure DrawTimetableDay(const AWriter: TSheetWriter;
                       const ARow, ACol, ADayIndex: Integer;
                       const ATimetable: TTimetable;
                       const AEmptyDay: Boolean = False);
  procedure DrawTotalHours(const AWriter: TSheetWriter;
                        const ARow1, ACol1, ARow2, ACol2, AValue: Integer;
                        const ADrawIfZero: Boolean);
  procedure DrawTotalHours(const AWriter: TSheetWriter;
                        const ARow, ACol, AValue: Integer;
                        const ADrawIfZero: Boolean);


implementation

procedure DrawTotalHours(const AWriter: TSheetWriter;
                        const ARow1, ACol1, ARow2, ACol2, AValue: Integer;
                        const ADrawIfZero: Boolean);
begin
  if ADrawIfZero or (AValue<>0) then
    WriteCellHours(AWriter, ARow1, ACol1, ARow2, ACol2, AValue)
  else
    AWriter.WriteText(ARow1, ACol1, ARow2, ACol2, EmptyStr, cbtOuter);
end;

procedure DrawTotalHours(const AWriter: TSheetWriter;
                        const ARow, ACol, AValue: Integer;
                        const ADrawIfZero: Boolean);
begin
  DrawTotalHours(AWriter, ARow, ACol, ARow, ACol, AValue, ADrawIfZero);
end;

procedure DrawTimetableDay(const AWriter: TSheetWriter;
                       const ARow, ACol, ADayIndex: Integer;
                       const ATimetable: TTimetable;
                       const AEmptyDay: Boolean = False);
begin
  if AEmptyDay then
  begin
    AWriter.WriteText(ARow,ACol, EmptyStr, cbtOuter);
    AWriter.WriteText(ARow+1,ACol, EmptyStr, cbtOuter);
    Exit;
  end;
  if ATimetable.IsExists[ADayIndex]=EXISTS_YES then
  begin
    AWriter.WriteText(ARow,ACol, ATimetable.MarksStr[ADayIndex], cbtOuter);
    AWriter.WriteText(ARow+1,ACol, ATimetable.HoursStr[ADayIndex], cbtOuter);
  end
  else begin
    AWriter.WriteText(ARow,ACol, STRMARK_NONEXISTENT, cbtOuter);
    AWriter.WriteText(ARow+1,ACol, STRMARK_NONEXISTENT, cbtOuter);
  end;
end;

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

{procedure DrawDaysCount(const AFX: TXLSX;
                        const ARow1,ACol1,ARow2,ACol2, AValue: Integer;
                        const ADrawIfZero: Boolean;
                        const ABordersType: TCellBorderType);
begin
  if ADrawIfZero or (AValue<>0) then
    AFX.WriteNumber(ARow1,ACol1,ARow2,ACol2, AValue, ABordersType)
  else
    AFX.WriteText(ARow1,ACol1,ARow2,ACol2, EmptyStr, ABordersType);
end;

   }

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

procedure DrawDaysCount(const AWriter: TSheetWriter;
                        const ARow1, ACol1, ARow2, ACol2, ADaysCount: Integer;
                        const AWriteDaysCountIfZero: Boolean);
begin
  if (ADaysCount>0) or AWriteDaysCountIfZero then
    AWriter.WriteNumber(ARow1, ACol1, ARow2, ACol2, ADaysCount, cbtOuter)
  else
    AWriter.WriteText(ARow1, ACol1, ARow2, ACol2, EmptyStr, cbtOuter);
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

{ TMonthCustomSheet }

function TMonthCustomSheet.GetIsDateSelected: Boolean;
begin
  Result:= FSelectedDate>0;
end;

function TMonthCustomSheet.GetIsDoubleRowSelected: Boolean;
begin
  Result:= (FSelectedRowIndex1>=0) and (FSelectedRowIndex2>=0);
end;

function TMonthCustomSheet.GetIsRowSelected: Boolean;
begin
  Result:= (FSelectedRowIndex1>=0) and (FSelectedRowIndex2<0);
end;

procedure TMonthCustomSheet.SetCanSelect(AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  FCanSelect:= AValue;
  if not AValue then SelectionClear;
end;

procedure TMonthCustomSheet.UnSelectDate;
begin
  if not IsDateSelected then Exit;
  SelectionExtraClear;
  FSelectedDate:= 0;
  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TMonthCustomSheet.UnSelectRow;
begin
  SelectionClear;
  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TMonthCustomSheet.SelectRow(const AIndex: Integer);
begin
  if AIndex<0 then Exit;
  if Assigned(FOnSelect) then
    FOnSelect;
end;

procedure TMonthCustomSheet.SelectRow1(const AIndex: Integer);
begin
  SelectionClear;
  FSelectedRowIndex1:= AIndex;
  SelectRow(AIndex);
end;

procedure TMonthCustomSheet.SelectRow2(const AIndex: Integer);
begin
  FSelectedRowIndex2:= AIndex;
  SelectRow(AIndex);
end;

procedure TMonthCustomSheet.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R, C, Index: Integer;
  D: TDate;
begin
  if not CanSelect then Exit;
  if Length(FTabNums)=0 then Exit;

  if Button=mbLeft then
  begin
    (Sender as TsWorksheetGrid).MouseToCell(X, Y, C, R);
    if not DateAndIndexFromCell(R, C, D, Index) then Exit;
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

constructor TMonthCustomSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont);
begin
  inherited Create(AWorksheet, AGrid, AFont);
  FSelectedRowIndex1:= -1;
  FSelectedRowIndex2:= -1;
  FSelectedDate:= 0;
  FCanSelect:= True;
  if Writer.HasGrid then
    Writer.Grid.OnMouseDown:= @MouseDown;
end;

procedure TMonthCustomSheet.Select(const AIndex: Integer);
begin
  SelectRow1(AIndex);
end;

procedure TMonthCustomSheet.SelectionClear;
begin
  inherited SelectionClear;
  FSelectedRowIndex1:= -1;
  FSelectedRowIndex2:= -1;
  FSelectedDate:= 0;
end;

procedure TMonthCustomSheet.SelectionMove(const ANewIndex: Integer);
var
  OldIndex: Integer;
begin
  OldIndex:= FSelectedRowIndex1;
  SelectionClear;
  LineDraw(OldIndex);
  LineDraw(ANewIndex);
  SelectRow1(ANewIndex);
end;

{ TMonthSheet }

function TMonthSheet.IndexToRow(const AIndex: Integer): Integer;
begin
  //FViewParams: 0 - Отображать строку ночных часов
  Result:= 3 + (1 + Ord(FViewParams[0])) * AIndex;
end;

function TMonthSheet.RowToIndex(const ARow: Integer): Integer;
begin
  //FViewParams: 0 - Отображать строку ночных часов
  Result:= (ARow - 3) div (1 + Ord(FViewParams[0]));
end;

function TMonthSheet.FirstDateCol: Integer;
begin
  //FExtraColumns: 0 - Порядковый номер, 1 - Должность (профессия), 2 - Табельный номер
  Result:= 2+Ord(FExtraColumns[0])+ Ord(FExtraColumns[1]) + Ord(FExtraColumns[2]);
end;

function TMonthSheet.ColToDate(const ACol: Integer): TDate;
var
  C1, C2: Integer;
begin
  Result:= 0;
  C1:= FirstDateCol;
  C2:= C1 + DaysInAMonth(FYear, FMonth) - 1;
  if (ACol<C1) or (ACol>C2) then Exit;
  Result:= EncodeDate(FYear, FMonth, ACol - C1 + 1);
end;

function TMonthSheet.DateToCol(const ADate: TDate): Integer;
begin
  Result:= FirstDateCol + DayOf(ADate) - 1;
end;

procedure TMonthSheet.SelectDate(const ADate: TDate);
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
  if FViewParams[0] then //строка ночных часов
    SelectionExtraAddCell(R+1, C);

  if Assigned(FOnSelect) then FOnSelect;
end;

procedure TMonthSheet.SelectRow(const AIndex: Integer);
var
  R, i, j: Integer;
begin
  inherited SelectRow(AIndex);
  if AIndex<0 then Exit;
  R:= IndexToRow(AIndex);
  for i:= R to R + Ord(FViewParams[0]) do  //FViewParams: 0 - Отображать строку ночных часов
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(i, j);
end;

function TMonthSheet.DateAndIndexFromCell(const ARow, ACol: Integer;
                                  out ADate: TDate;
                                  out AIndex: Integer): Boolean;
begin
  ADate:= 0;
  AIndex:= -1;
  Result:= (ARow > 2) and (ARow < IndexToRow(Length(FTabNums)));
  if not Result then Exit;

  ADate:= ColToDate(ACol);
  AIndex:= RowToIndex(ARow);
end;

constructor TMonthSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const AResumeType: Byte {0-дни, 1-смены, 2-дни и смены};
                       const APeriodType: Byte {0-год, 1-квартал, 2-месяц};
                       const AExtraColumns: TBoolVector);
begin
  FResumeType:= AResumeType;
  FPeriodType:= APeriodType;
  FExtraColumns:= AExtraColumns;
  inherited Create(AWorksheet, AGrid, AFont);
end;

procedure TMonthSheet.DrawCustom(const ACalendar: TCalendar;
                   const AStaffNames, ATabNums, APostNames, ARanks: TStrVector;
                   const ANormHours: TIntVector;
                   const AViewParams: TBoolVector;
                   const AExportParams: TBoolVector
                   );
var
  X, W, R, C, i: Integer;
begin
  FCalendar:= ACalendar;
  FYear:= YearOf(FCalendar.BeginDate);
  FMonth:= MonthOf(FCalendar.BeginDate);

  FViewParams:= AViewParams;
  FExportParams:= AExportParams;
  FStaffNames:= AStaffNames;
  FTabNums:= ATabNums;
  FPostNames:= APostNames;
  FRanks:= ARanks;
  FNormHours:= ANormHours;

  Writer.BeginEdit;
  W:= VMaxWidth(FStaffNames, Font.Name, Font.Size+1, [],
                STAFFNAME_COLUMN_WIDTH, False{не менее STAFFNAME_COLUMN_WIDTH});

  X:= 1+Ord(FExtraColumns[0]); //FExtraColumns: 0 - Порядковый номер
  Writer.SetColWidth(X, W);
  if FExtraColumns[2] then //FExtraColumns: 2 - Табельный номер
  begin
    W:= VMaxWidth(FTabNums, Font.Name, Font.Size+1, [],
                  TABNUM_COLUMN_WIDTH, False{не менее TABNUM_COLUMN_WIDTH});

    X:= 2+Ord(FExtraColumns[0])+Ord(FExtraColumns[1]); //FExtraColumns: 0 - Порядковый номер, 1 - Должность (профессия)
    Writer.SetColWidth(X, W);
  end;

  Writer.SetBackgroundDefault;
  CaptionDraw(R{%H-}, C{%H-});
  for i:=0 to High(FTabNums) do
    LineDraw(i);

  Writer.SetFrozenRows(2);
  if FExportParams[0] then //FExportParams: 0 - заголовок таблицы на каждой странице
    Writer.SetRepeatedRows(1, 2);
  if FExportParams[1] then //FExportParams: 1 - номера страниц в нижнем колонтитуле
    Writer.WorkSheet.PageLayout.Footers[HEADER_FOOTER_INDEX_ALL] := '&R страница &P (из &N)';

  X:= IndexToRow(Length(FTabNums));
  for i:= 1 to Writer.ColCount do
    Writer.WriteText(X, i, EmptyStr, cbtTop);

  Writer.EndEdit;
end;

function TMonthSheet.SetWidths: TIntVector;
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
end;

procedure TMonthSheet.CaptionDraw(var R, C: Integer);
var
  i: Integer;
  S: String;
begin
  C:= 0;
  R:= 1;
  Writer.SetBackgroundDefault;
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
    Writer.WriteText(R, C, R, C+1+Ord(FResumeType=2), FResumeCaption, cbtOuter);
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
    S:= 'Норма часов ' + SYMBOL_BREAK + 'на ';
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
    S:= 'Отклонение' + SYMBOL_BREAK + 'от ';
    case FPeriodType of
      2: S:= S + 'месячной ';
      1: S:= S + 'квартальной ';
      0: S:= S + 'годовой ';
    end;
    S:= S + SYMBOL_BREAK + 'нормы';
    Writer.WriteText(R, C, R+1, C, S, cbtOuter);
  end;
  Writer.SetRowHeight(R, 3*SHeight(Font));
end;

{ TMonthFormSheet }

procedure TMonthFormSheet.Draw(const ACompany, ADepartment: String;
                   const ACalendar: TCalendar;
                   const ATimetables: TTimetableVector;
                   const AStaffNames, ATabNums, APostNames, ARanks: TStrVector;
                   const ANeedRanks, ANeedTopBottom, AHalfMonth: Boolean;
                   const AExportParams: TBoolVector;
                    //AExportParams:
                    //0 - заголовок таблицы на каждой странице
                    //1 - номера страниц в нижнем колонтитуле
                   const ABorderStyle: Byte = 1);
var
  i, R: Integer;
begin
  SelectionClear;

  FTimetables:= ATimetables;
  FCompany:= ACompany;
  FDepartment:= ADepartment;
  FCalendar:= ACalendar;
  FYear:= YearOfDate(ACalendar.BeginDate);
  FMonth:= MonthOfDate(ACalendar.BeginDate);
  FNeedRanks:= ANeedRanks;
  FNeedTopBottom:= ANeedTopBottom;
  FHalfMonth:= AHalfMonth;
  FStaffNames:= AStaffNames;
  FTabNums:= ATabNums;
  FPostNames:= APostNames;
  FRanks:= ARanks;
  FBorderStyle:= ABorderStyle;

  Writer.BeginEdit;

  TopDraw;
  CaptionDraw(AExportParams[0]); //ExportParams: 0 - заголовок таблицы на каждой странице
  for i:=0 to High(FTimetables) do
    LineDraw(i);
  R:= IndexToRow(Length(FTimetables));
  if Writer.HasGrid then
    for i:= 1 to Writer.ColCount do
      Writer.WriteText(R, i, EmptyStr, cbtTop);
  BottomDraw;

  if not FNeedTopBottom then
    Writer.SetFrozenRows(IndexToRow(0)-1);
  if AExportParams[1] then  //ExportParams: 1 - номера страниц в нижнем колонтитуле
    Writer.WorkSheet.PageLayout.Footers[HEADER_FOOTER_INDEX_ALL] := '&R страница &P (из &N)';

  Writer.EndEdit;

end;

end.

