unit USIZNormSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, //fpsTypes,
  //DK packages utils
  DK_SheetTables, DK_SheetTypes, DK_Vector, DK_StrUtils, DK_Const,
  //Project utils
  USIZTypes, UConst;

type

  { TSIZNormItemSheet }

  TSIZNormItemSheet = class (TCustomSheetTable)
  protected
    function SetWidths: TIntVector; override;
    function FirstDataRow: Integer; override;
    function LastDataRow: Integer; override;
    procedure Select(const ARow, {%H-}ACol: Integer); override;
    procedure Unselect; override;
    procedure SelectionMove(const AVertDelta: Integer); override;
  private
    const
      COLUMN1_WIDTH = 100;
      TITLE_HEIGHT = 35;
    var
      FItemNames, FPostNames: TStrVector;
      //начальный и конечный индексы пункта в входящих списках
      FFirstItemIndexes, FLastItemIndexes: TIntVector;

    //расчет начальных и конечных индексов пунктов норм по входящему списку
    procedure ItemIndexesCalc;
    procedure CaptionDraw;
  public
    procedure Draw(const AItemNames, APostNames: TStrVector; const ASelectedIndex: Integer);
    procedure LineDraw(const AIndex: Integer);
  end;

  { TSIZNormSubItemsSheet }

  TSIZNormSubItemsSheet = class (TCustomSheetTable)
  protected
    function SetWidths: TIntVector; override;
    function FirstDataRow: Integer; override;
    function LastDataRow: Integer; override;

    function IsSelectedCellCorrect(const ARow, {%H-}ACol: Integer): Boolean; //override;
    procedure Select(const ARow, {%H-}ACol: Integer); override;
    procedure Unselect; override;
    procedure SelectionMove(const AVertDelta: Integer); override;



    function RowToIndex(const ARow: Integer): Integer; override;
    function IndexToRow(const AIndex: Integer): Integer; override;



  private
    const
      COLUMN1_WIDTH = 300;
      COLUMN2_WIDTH = 100;
      COLUMN3_WIDTH = 100;
      COLUMN4_WIDTH = 100;
      TITLE_HEIGHT = 35;
    var
      FSubItems: TNormSubItems;
      FItemName: String;
      FFirstRows, FLastRows: TIntVector;

    function IndexToSubItemIndex(const AIndex: Integer): Integer;
    function IndexToSubItemInfoIndex(const AIndex: Integer): Integer;
    function RowToSubItemIndex(const ARow: Integer): Integer;

    procedure CaptionDraw;
    procedure ReasonDraw(var ARow: Integer; const AIndex: Integer);
    procedure LineDraw(var ARow: Integer; const AIndex: Integer);
  public
    procedure Draw(const ASubItems: TNormSubItems; const AItemName: String;
                   const ASelectedIndex: Integer);
    function SelectedSubItemIndex: Integer;
    function CanUpSelection: Boolean;
    function CanDownSelection: Boolean;
  end;

implementation

{ TSIZNormItemSheet }

function TSIZNormItemSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([COLUMN1_WIDTH, 300]);
end;

function TSIZNormItemSheet.FirstDataRow: Integer;
begin
  Result:= 2;
end;

function TSIZNormItemSheet.LastDataRow: Integer;
begin
  Result:= FirstDataRow + High(FItemNames);
end;

procedure TSIZNormItemSheet.Select(const ARow, ACol: Integer);
var
  i, j, n: Integer;
begin
  n:= VIndexOf(FFirstItemIndexes, FLastItemIndexes, RowToIndex(ARow));
  if n<0 then Exit;
  FSelectedIndex:= n;
  for i:= FFirstItemIndexes[n] to FLastItemIndexes[n] do
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(IndexToRow(i), j);
end;

procedure TSIZNormItemSheet.Unselect;
begin
  FSelectedIndex:= -1;
  SelectionClear;
end;

procedure TSIZNormItemSheet.SelectionMove(const AVertDelta: Integer);
var
  n: Integer;
begin
  if not IsSelected then Exit;
  n:= VIndexOf(FFirstItemIndexes, FLastItemIndexes, FSelectedIndex);
  if n<0 then Exit;
  n:= n + AVertDelta;
  if not CheckIndex(High(FFirstItemIndexes), n) then Exit;
  SetSelection(IndexToRow(FFirstItemIndexes[n]), 1);
end;

procedure TSIZNormItemSheet.ItemIndexesCalc;
var
  S: String;
  i: Integer;
begin
  FFirstItemIndexes:= nil;
  FLastItemIndexes:= nil;
  if VIsNil(FItemNames) then Exit;

  S:= FItemNames[0];
  VAppend(FFirstItemIndexes, 0);
  for i:= 1 to High(FItemNames) do
  begin
    if not SSame(FItemNames[i], S) then
    begin
      VAppend(FLastItemIndexes, i-1);
      VAppend(FFirstItemIndexes, i);
      S:= FItemNames[i];
    end;
  end;
  VAppend(FLastItemIndexes, High(FItemNames));
end;

procedure TSIZNormItemSheet.CaptionDraw;
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(1, 1, 'Пункт типовых норм', cbtOuter);
  Writer.WriteText(1, 2, 'Должность (профессия)', cbtOuter);
  Writer.SetRowHeight(1, TITLE_HEIGHT);
end;

procedure TSIZNormItemSheet.Draw(const AItemNames, APostNames: TStrVector;
  const ASelectedIndex: Integer);
var
  i: Integer;
begin
  FItemNames:= AItemNames;
  FPostNames:= APostNames;

  DrawingBegin;

  CaptionDraw;
  ItemIndexesCalc;
  for i:= 0 to High(FFirstItemIndexes) do
    LineDraw(i);

  DrawingEnd;

  SetSelection(IndexToRow(ASelectedIndex), 1);
end;

procedure TSIZNormItemSheet.LineDraw(const AIndex: Integer);
var
  i, R, RR: Integer;
  S: String;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  i:= FFirstItemIndexes[AIndex];
  RR:= IndexToRow(i);
  R:= RR - 1;
  Writer.SetAlignment(haLeft, vaCenter);
  for i:= FFirstItemIndexes[AIndex] to FLastItemIndexes[AIndex] do
  begin
     R:= R + 1;
     S:= FPostNames[i];
     if i<FLastItemIndexes[AIndex] then S:= S + ',';
     Writer.WriteText(R, 2, S);
  end;
  Writer.SetAlignment(haCenter, vaTop);
  Writer.WriteText(RR, 1, R, 1, FItemNames[FFirstItemIndexes[AIndex]]);
  Writer.SetBackgroundDefault;
  Writer.DrawBorders(RR, 1, R, 1, cbtOuter);
  Writer.DrawBorders(RR, 2, R, 2, cbtOuter);
end;

{ TSIZNormSubItemsSheet }

function TSIZNormSubItemsSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    COLUMN1_WIDTH,
    COLUMN2_WIDTH,
    COLUMN3_WIDTH,
    COLUMN4_WIDTH
  ]);
end;

function TSIZNormSubItemsSheet.FirstDataRow: Integer;
begin
  Result:= 2;
end;

function TSIZNormSubItemsSheet.LastDataRow: Integer;
begin
  Result:= VLast(FLastRows);
end;

function TSIZNormSubItemsSheet.IsSelectedCellCorrect(const ARow, ACol: Integer): Boolean;
begin
  Result:= CheckIndex(High(FFirstRows), RowToSubItemIndex(ARow));
end;

function TSIZNormSubItemsSheet.RowToSubItemIndex(const ARow: Integer): Integer;
begin
  Result:= IndexToSubItemIndex(RowToIndex(ARow));
end;

procedure TSIZNormSubItemsSheet.Select(const ARow, ACol: Integer);
var
  i, j, n: Integer;
begin
  FSelectedIndex:= RowToIndex(ARow);
  n:= IndexToSubItemIndex(FSelectedIndex);
  for i:= FFirstRows[n] to FLastRows[n] do
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(i, j);
end;

procedure TSIZNormSubItemsSheet.Unselect;
begin
  FSelectedIndex:= -1;
  SelectionClear;
end;

procedure TSIZNormSubItemsSheet.SelectionMove(const AVertDelta: Integer);
var
  n: Integer;
begin
  if not IsSelected then Exit;
  n:= IndexToSubItemIndex(FSelectedIndex);
  if n<0 then Exit;
  n:= n + AVertDelta;
  if not CheckIndex(High(FFirstRows), n) then Exit;
  SetSelection(FFirstRows[n], 1);
end;

function TSIZNormSubItemsSheet.RowToIndex(const ARow: Integer): Integer;
begin
  Result:= VIndexOf(FFirstRows, FLastRows, ARow);
  if Result<0 then Exit;
  Result:= Result*1000 + (ARow-FFirstRows[Result]);
end;

function TSIZNormSubItemsSheet.IndexToRow(const AIndex: Integer): Integer;
var
  i, j: Integer;
begin
  Result:= -1;
  if AIndex<0 then Exit;
  i:= IndexToSubItemIndex(AIndex);
  j:= IndexToSubItemInfoIndex(AIndex);
  Result:= FFirstRows[i] + j;
end;

function TSIZNormSubItemsSheet.IndexToSubItemIndex(const AIndex: Integer): Integer;
begin
  Result:= -1;
  if AIndex<0 then Exit;
  Result:= AIndex div 1000;
end;

function TSIZNormSubItemsSheet.IndexToSubItemInfoIndex(const AIndex: Integer): Integer;
begin
  Result:= -1;
  if AIndex<0 then Exit;
  Result:= AIndex - 1000;
end;

procedure TSIZNormSubItemsSheet.CaptionDraw;
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(1, 1, 'Наименование средств индивидуальной защиты', cbtOuter);
  Writer.WriteText(1, 2, 'Пункт типовых норм', cbtOuter);
  Writer.WriteText(1, 3, 'Единица измерения', cbtOuter);
  Writer.WriteText(1, 4, 'Количество' + SYMBOL_BREAK + 'на год', cbtOuter);
  Writer.SetRowHeight(1, TITLE_HEIGHT);
end;

procedure TSIZNormSubItemsSheet.ReasonDraw(var ARow: Integer; const AIndex: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold, fsItalic], clBlack);
  Writer.WriteText(ARow, 1, ARow, 4, FSubItems[AIndex].Reason + ':', cbtOuter, True, True);
  //if Writer.HasGrid then
  //begin
  //  Writer.SetFont(Font.Name, Font.Size, [], Writer.Grid.Color); //!!!!!
  //  Writer.WriteNumber(ARow, 5, -1, cbtLeft); //!!!!!
  //end;
  ARow:= ARow + 1;
end;

procedure TSIZNormSubItemsSheet.LineDraw(var ARow: Integer; const AIndex: Integer);
var
  R,i,N: Integer;
  S: String;
  //C: Integer;
  //FS: TsFontStyles;
begin
  //if ASelected then
  //begin
  //  FX.SetBackground(HighlightColor);
  //  C:= COLOR_WHITE;
  //  FS:= [{fssBold}];
  //end
  //else begin
  //  FX.SetBackground(COLOR_WHITE);
  //  C:= COLOR_BLACK;
  //  FS:= [{fssBold}];
  //end;

  Writer.SetBackgroundDefault;

  N:= High(FSubItems[AIndex].Names);
  for i:=0 to N do
  begin
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    R:= ARow + i;
    Writer.SetAlignment(haLeft, vaCenter);
    S:= FSubItems[AIndex].Names[i];
    if (N>0) and (i<N) then S:= S + ' или';
    Writer.WriteText(R, 1, S, cbtNone, True, True);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, 2, FItemName);
    Writer.WriteText(R, 3, FSubItems[AIndex].Units[i]);
    Writer.WriteText(R, 4, FSubItems[AIndex].Lifes[i]);
    //if Writer.HasGrid then
    //begin
    //  Writer.SetFont(Font.Name, Font.Size, [], Writer.Grid.Color);   //!!!!!!
    //  Writer.WriteNumber(R, 5, FSubItems[AIndex].SubItemID, cbtLeft); //!!!!!
    //end;
  end;

  for i:= 1 to 4 do
    Writer.DrawBorders(ARow, i, ARow+N, i, cbtOuter);

  ARow:= ARow + N + 1;

end;

procedure TSIZNormSubItemsSheet.Draw(const ASubItems: TNormSubItems;
                                     const AItemName: String;
                                     const ASelectedIndex: Integer);
var
  i, R: Integer;
  S: String;
begin
  FSubItems:= ASubItems;
  FItemName:= AItemName;

  FFirstRows:= nil;
  FLastRows:= nil;

  DrawingBegin;

  CaptionDraw;

  S:= MAIN_REASON;
  R:= FirstDataRow;
  for i:=0 to High(FSubItems) do
  begin
    if FSubItems[i].Reason<>S then
    begin
      ReasonDraw(R, i);
      VAppend(FFirstRows, R);
      LineDraw(R, i);
      VAppend(FLastRows, R-1);
      S:= FSubItems[i].Reason;
    end
    else begin
      VAppend(FFirstRows, R);
      LineDraw(R, i);
      VAppend(FLastRows, R-1);
    end;
  end;

  DrawingEnd;

  SetSelection(IndexToRow(ASelectedIndex), 1);

  //if CanSelect and (ASelectedIndex>=0) then
  //  Select(IndexToRow(ASelectedIndex), 1);
end;

function TSIZNormSubItemsSheet.SelectedSubItemIndex: Integer;
begin
  Result:= IndexToSubItemIndex(FSelectedIndex);
end;

function TSIZNormSubItemsSheet.CanUpSelection: Boolean;
begin
  Result:= IsSelected and (SelectedSubItemIndex>0);
end;

function TSIZNormSubItemsSheet.CanDownSelection: Boolean;
begin
  Result:= IsSelected and (SelectedSubItemIndex<High(FSubItems));
end;

end.

