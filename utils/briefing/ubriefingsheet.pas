unit UBriefingSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  fpspreadsheetgrid, fpstypes,
  //Project utils
  UConst, UUtils,
  //DK packages utils
  DK_SheetTypes, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const, DK_SheetWriter,
  DK_SheetConst, DK_DateUtils, DK_Color;

type

  { TBriefingMainListSheet }

  TBriefingMainListSheet = class(TSingleSelectableSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //1100
      COLWIDTHS: array of Integer = (
        {01} 150, //период действия
        {02} 350, //наименование
        {03} 350, //кому проводится
        {04} 150, //периодичность проведения
        {05} 100, //провести до даты
        {06} 300  //примечание
      );
    var
      FBriefNames, FNotes: TStrVector;
      FObjects, FPeriods, FNums: TIntVector;
      FBeginDates, FEndDates, FLastDates: TDateVector;
      FObjectNames: TStrMatrix;

    procedure CaptionDraw(var ARow: Integer);
    procedure ItemDraw(var ARow: Integer; const AIndex: Integer);
  public
    procedure Draw(const ABriefNames, ANotes: TStrVector;
                   const AObjects, APeriods, ANums: TIntVector;
                   const ABeginDates, AEndDates, ALastDates: TDateVector;
                   const AObjectNames: TStrMatrix);
  end;

  { TBriefingListSheet }

  TBriefingListSheet = class(TSingleSelectableSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    FBriefNames, FNotes: TStrVector;
  public
    procedure Draw(const ABriefNames, ANotes: TStrVector);
  end;

implementation

{ TBriefingMainListSheet }

function TBriefingMainListSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TBriefingMainListSheet.CaptionDraw(var ARow: Integer);
var
  R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);

  Writer.WriteText(R, 1, 'Период действия', cbtOuter);
  Writer.WriteText(R, 2, 'Наименование мероприятия', cbtOuter);
  Writer.WriteText(R, 3, 'Кому проводится', cbtOuter);
  Writer.WriteText(R, 4, 'Периодичность', cbtOuter);
  Writer.WriteText(R, 5, 'Провести до', cbtOuter);
  Writer.WriteText(R, 6, 'Примечание', cbtOuter);

end;

procedure TBriefingMainListSheet.ItemDraw(var ARow: Integer; const AIndex: Integer);
var
  i, R1, R2: Integer;
  S: String;
begin
  R1:= ARow;
  R2:= R1 + High(FObjectNames[AIndex]);
  VAppend(FFirstRows, R1);
  VAppend(FLastRows, R2);

  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  if R1=R2 then
    Writer.SetAlignment(haCenter, vaCenter)
  else
    Writer.SetAlignment(haCenter, vaTop);

  if FPeriods[AIndex]=0 then //разово
    S:= EMPTY_MARK
  else //периодически
    S:= PeriodToStr(FBeginDates[AIndex], FEndDates[AIndex]);
  Writer.WriteText(R1, 1, R2, 1, S, cbtOuter, True, True);

  S:= '1 раз';
  case FPeriods[AIndex] of
    1: S:= S + ' в ' + SDays(IntToStr(FNums[AIndex]), True, False);
    2: S:= S + ' в ' + SMonths(IntToStr(FNums[AIndex]), True, False);
    3: S:= S + ' в ' + SYears(IntToStr(FNums[AIndex]), True, False);
  end;
  Writer.WriteText(R1, 4, R2, 4, S, cbtOuter, True, True);

  if FLastDates[AIndex]=0 then
    S:= EMPTY_MARK
  else
    S:= FormatDateTime('dd.mm.yyyy', FLastDates[AIndex]);
  Writer.WriteText(R1, 5, R2, 5, S, cbtOuter, True, True);


  if R1=R2 then
    Writer.SetAlignment(haLeft, vaCenter)
  else
    Writer.SetAlignment(haLeft, vaTop);
  Writer.WriteText(R1, 2, R2, 2, FBriefNames[AIndex], cbtOuter, True, True);
  Writer.WriteText(R1, 6, R2, 6, FNotes[AIndex], cbtOuter, True, True);
  for i:= 0 to High(FObjectNames[AIndex]) do
  begin
    S:= FObjectNames[AIndex, i];
    if i<High(FObjectNames[AIndex]) then
      S:= S + ';';
    Writer.WriteText(R1+i, 3, S, cbtNone, True, True);
  end;

  Writer.DrawBorders(R1, 3, R2, 3, cbtOuter);

  ARow:= R2;
end;

procedure TBriefingMainListSheet.Draw(const ABriefNames, ANotes: TStrVector;
                   const AObjects, APeriods, ANums: TIntVector;
                   const ABeginDates, AEndDates, ALastDates: TDateVector;
                   const AObjectNames: TStrMatrix);
var
  i, R, CaptionRowCount: Integer;
begin
  FBriefNames:= ABriefNames;
  FNotes:= ANotes;
  FObjects:= AObjects;
  FPeriods:= APeriods;
  FNums:= ANums;
  FBeginDates:= ABeginDates;
  FEndDates:= AEndDates;
  FLastDates:= ALastDates;
  FObjectNames:= AObjectNames;


  Unselect;

  FFirstRows:= nil;
  FLastRows:= nil;

  Writer.BeginEdit;

  R:= 1;
  CaptionDraw(R);
  CaptionRowCount:= R;

  for i:= 0 to High(FBriefNames) do
  begin
    R:= R + 1;
    ItemDraw(R, i);
  end;

  Writer.SetFrozenRows(CaptionRowCount);
  //Writer.SetRepeatedRows(CaptionRowCount-2, CaptionRowCount);

  R:= R + 1;
  for i:= 1 to Writer.ColCount do
    Writer.DrawBorders(R, i, cbtTop);

  Writer.EndEdit;
end;

{ TBriefingListSheet }

function TBriefingListSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([100]);
end;

procedure TBriefingListSheet.Draw(const ABriefNames, ANotes: TStrVector);
var
  i, R: Integer;
  S: String;
begin
  Unselect;

  FFirstRows:= nil;
  FLastRows:= nil;

  Writer.BeginEdit;

  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);

  R:= 0;
  for i:= 0 to High(ABriefNames) do
  begin
    R:= R + 1;
    VAppend(FFirstRows, R);
    VAppend(FLastRows, R);
    S:= ABriefNames[i];
    if not SEmpty(ANotes[i]) then
      S:= S + ' (' + ANotes[i] + ')';
    Writer.WriteText(R, 1,  S, cbtOuter, True, True);
  end;


  Writer.EndEdit;
end;

end.

