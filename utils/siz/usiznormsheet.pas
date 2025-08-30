unit USIZNormSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpstypes,
  //DK packages utils
  DK_SheetTables, DK_SheetTypes, DK_Vector, DK_Matrix, DK_Const, DK_StrUtils,
  DK_SheetWriter,
  //Project utils
  USIZNormTypes, USIZUtils, UConst;

type

  { TSIZNormItemSheet }

  TSIZNormItemSheet = class (TCustomSheetTable)
  protected
    function SetWidths: TIntVector; override;
    function FirstDataRow: Integer; override;
    function LastDataRow: Integer; override;

    function GetSelectedIndex: Integer; override;
    procedure SetSelection(const ARow, {%H-}ACol: Integer); override;
    procedure DelSelection; override;
    procedure SelectionMove(const AVertDelta: Integer); override;
  private
    const
      COLUMN1_WIDTH = 30; //№п/п
      COLUMN2_WIDTH = 300; //должность (профессия)
      TITLE_HEIGHT = 70;//35;
    var
      FPostNames: TStrMatrix;
      FOrderNums: TIntVector;
      FOrderNames: TStrVector;
      //начальный и конечный индексы пункта в входящих списках
      FFirstItemIndexes, FLastItemIndexes: TIntVector;

    //расчет начальных и конечных индексов пунктов норм по входящему списку
    procedure ItemIndexesCalc;

    procedure CaptionDraw;
    procedure LineDraw(const AIndex: Integer);
  public
    procedure Draw(const AOrderNums: TIntVector;
                   const AOrderNames: TStrVector;
                   const APostNames: TStrMatrix;
                   const ASelectedIndex: Integer);
    procedure SelectByIndex(const ASelectedIndex: Integer);
    procedure Swap(const AIndex1, AIndex2: Integer);
    function IndexToItemIndex(const AIndex: Integer): Integer;
    function ItemIndexToIndex(const AIndex: Integer): Integer;
    function CanUp: Boolean;
    function CanDown: Boolean;
  end;

  { TSIZNormSubItemsSheet }

  TSIZNormSubItemsSheet = class (TCustomSheetTable)
  protected
    function SetWidths: TIntVector; override;
    function FirstDataRow: Integer; override;
    function LastDataRow: Integer; override;

    function GetSelectedIndex: Integer; override;

    procedure SetSelection(const ARow, {%H-}ACol: Integer); override;
    procedure DelSelection; override;
    procedure SelectionMove(const AVertDelta: Integer); override;
    function IsCellSelectable(const ARow, ACol: Integer): Boolean; override;

    function RowToIndex(const ARow: Integer): Integer; override;
    function IndexToRow(const AIndex: Integer): Integer; override;
  private
    const
      COLUMN1_WIDTH = 130; //Тип СИЗ
      COLUMN2_WIDTH = 300; //Наименование СИЗ
      COLUMN3_WIDTH = 260; //Нормы выдачи
      COLUMN4_WIDTH = 260; //Основание выдачи (пункты норм)
      TITLE_HEIGHT = 70;//35;
    var
      FSubItems: TNormSubItems;
      FFirstRows, FLastRows: TIntVector;

    function IndexToSubItemIndex(const AIndex: Integer): Integer;
    function IndexToSubItemInfoIndex(const AIndex: Integer): Integer;
    function RowToSubItemIndex(const ARow: Integer): Integer;
    function SubItemIndexToRow(const ASubItemIndex: Integer): Integer;

    procedure CaptionDraw;
    procedure ReasonDraw(var ARow: Integer; const AIndex: Integer);
    procedure LineDraw(var ARow: Integer; const AIndex: Integer);
  public
    procedure Draw(const ASubItems: TNormSubItems;
                   const ASelectedIndex: Integer);
    procedure SelectByIndex(const ASelectedIndex: Integer);
    function CanUp: Boolean;
    function CanDown: Boolean;
  end;

  { TSIZNormSheet }

  TSIZNormSheet = class (TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      COLUMN1_WIDTH = 40;  //№п/п
      COLUMN2_WIDTH = 120; //должность (профессия)
      COLUMN3_WIDTH = 120; //Тип СИЗ
      COLUMN4_WIDTH = 130; //Наименование СИЗ
      COLUMN5_WIDTH = 130; //Нормы выдачи
      COLUMN6_WIDTH = 120; //Основание выдачи (пункты норм)
    var
      FNorm: TNorm;

    procedure TitleDraw(var ARow: Integer);
    procedure CaptionDraw(var ARow: Integer);
    procedure ItemDraw(var ARow: Integer; const AItem: TNormItem);
  public
    procedure Draw(const ANorm: TNorm);
  end;

  procedure VectorDraw(const AWriter: TSheetWriter;
                       const AVector: TStrVector;
                       const ARow, ACol: Integer;
                       const AMiddleStr: String;
                       const ASameCheck: Boolean;
                       const AHorAlignment: TsHorAlignment;
                       const AVertAlignment: TsVertAlignment);
  procedure VectorDraw(const AWriter: TSheetWriter;
                       const AVector: TStrVector;
                       const ARow, ACol1, ACol2: Integer;
                       const AMiddleStr: String;
                       const ASameCheck: Boolean;
                       const AHorAlignment: TsHorAlignment;
                       const AVertAlignment: TsVertAlignment);

implementation

{ TSIZNormItemSheet }

function TSIZNormItemSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    COLUMN1_WIDTH,
    COLUMN2_WIDTH
  ]);
end;

function TSIZNormItemSheet.FirstDataRow: Integer;
begin
  Result:= 2;
end;

function TSIZNormItemSheet.LastDataRow: Integer;
begin
  Result:= FirstDataRow + VLast(FLastItemIndexes);
end;

function TSIZNormItemSheet.GetSelectedIndex: Integer;
begin
  Result:= IndexToItemIndex(inherited GetSelectedIndex);
end;

procedure TSIZNormItemSheet.SetSelection(const ARow, ACol: Integer);
var
  i, j, ItemIndex: Integer;
begin
  i:= RowToIndex(ARow);
  ItemIndex:= IndexToItemIndex(i);
  if ItemIndex<0 then Exit;
  FSelectedIndex:= i;
  for i:= FFirstItemIndexes[ItemIndex] to FLastItemIndexes[ItemIndex] do
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(IndexToRow(i), j);
end;

procedure TSIZNormItemSheet.DelSelection;
begin
  FSelectedIndex:= -1;
  SelectionClear;
end;

procedure TSIZNormItemSheet.SelectionMove(const AVertDelta: Integer);
var
  ItemIndex: Integer;
begin
  if not IsSelected then Exit;
  ItemIndex:= IndexToItemIndex(FSelectedIndex);
  if ItemIndex<0 then Exit;
  ItemIndex:= ItemIndex + AVertDelta;
  if not CheckIndex(High(FFirstItemIndexes), ItemIndex) then Exit;
  Select(IndexToRow(FFirstItemIndexes[ItemIndex]), 1);
end;

procedure TSIZNormItemSheet.ItemIndexesCalc;
var
  i, n: Integer;
begin
  FFirstItemIndexes:= nil;
  FLastItemIndexes:= nil;
  if MIsNil(FPostNames) then Exit;

  n:= -1;
  for i:= 0 to High(FPostNames) do
  begin
    n:= n + 1;
    VAppend(FFirstItemIndexes, n);
    n:= n + High(FPostNames[i]);
    VAppend(FLastItemIndexes, n);
  end;
end;

procedure TSIZNormItemSheet.CaptionDraw;
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaTop);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(1, 1, '№ п/п', cbtOuter);
  Writer.WriteText(1, 2, 'Наименование профессии' + SYMBOL_BREAK + '(должности)', cbtOuter);
  Writer.SetRowHeight(1, TITLE_HEIGHT);
end;

procedure TSIZNormItemSheet.Draw(const AOrderNums: TIntVector;
                   const AOrderNames: TStrVector;
                   const APostNames: TStrMatrix;
                   const ASelectedIndex: Integer);
var
  i: Integer;
begin
  FOrderNums:= AOrderNums;
  FOrderNames:= AOrderNames;
  FPostNames:= APostNames;

  if VIsNil(AOrderNums) then
  begin
    Writer.BeginEdit;
    CaptionDraw;
    Writer.EndEdit;
    Exit;
  end;

  DrawingBegin;
  CaptionDraw;
  ItemIndexesCalc;
  for i:= 0 to High(FFirstItemIndexes) do
    LineDraw(i);
  DrawingEnd;

  SelectByIndex(ASelectedIndex);
end;

procedure TSIZNormItemSheet.SelectByIndex(const ASelectedIndex: Integer);
begin
  Select(IndexToRow(ItemIndexToIndex(ASelectedIndex)), 1);
end;

procedure TSIZNormItemSheet.Swap(const AIndex1, AIndex2: Integer);
begin
  VSwap(FFirstItemIndexes, AIndex1, AIndex2);
  VSwap(FLastItemIndexes, AIndex1, AIndex2);
end;

function TSIZNormItemSheet.IndexToItemIndex(const AIndex: Integer): Integer;
begin
  Result:= VIndexOf(FFirstItemIndexes, FLastItemIndexes, AIndex);
end;

function TSIZNormItemSheet.ItemIndexToIndex(const AIndex: Integer): Integer;
begin
  Result:= -1;
  if VIsNil(FFirstItemIndexes) then Exit;
  Result:= FFirstItemIndexes[AIndex];
end;

function TSIZNormItemSheet.CanUp: Boolean;
begin
  Result:= IsSelected and (SelectedIndex>0);
end;

function TSIZNormItemSheet.CanDown: Boolean;
begin
  Result:= IsSelected and (SelectedIndex<High(FOrderNums));
end;

procedure TSIZNormItemSheet.LineDraw(const AIndex: Integer);
var
  i, R, RR: Integer;
  S: String;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  RR:= IndexToRow(FFirstItemIndexes[AIndex]);
  R:= RR - 1;

  if Length(FPostNames[AIndex])=1 then
    Writer.SetAlignment(haLeft, vaCenter)
  else
    Writer.SetAlignment(haLeft, vaTop);
  for i:= FFirstItemIndexes[AIndex] to FLastItemIndexes[AIndex] do
  begin
     R:= R + 1;
     S:= FPostNames[AIndex, i-FFirstItemIndexes[AIndex]];
     if i<FLastItemIndexes[AIndex] then S:= S + ',';
     Writer.WriteText(R, 2, S, cbtNone, True, True);
  end;

  if Length(FPostNames[AIndex])=1 then
    Writer.SetAlignment(haCenter, vaCenter)
  else
    Writer.SetAlignment(haCenter, vaTop);
  if SEmpty(FOrderNames[AIndex]) then
    Writer.WriteNumber(RR, 1, R, 1, FOrderNums[AIndex]+1)
  else
    Writer.WriteText(RR, 1, R, 1, FOrderNames[AIndex]);

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

function TSIZNormSubItemsSheet.GetSelectedIndex: Integer;
begin
  Result:= IndexToSubItemIndex(inherited GetSelectedIndex);
end;

function TSIZNormSubItemsSheet.RowToSubItemIndex(const ARow: Integer): Integer;
begin
  Result:= IndexToSubItemIndex(RowToIndex(ARow));
end;

function TSIZNormSubItemsSheet.SubItemIndexToRow(const ASubItemIndex: Integer): Integer;
begin
  Result:= -1;
  if ASubItemIndex<0 then Exit;
  Result:= FFirstRows[ASubItemIndex];
end;

procedure TSIZNormSubItemsSheet.SetSelection(const ARow, ACol: Integer);
var
  i, j, n: Integer;
begin
  FSelectedIndex:= RowToIndex(ARow);
  n:= IndexToSubItemIndex(FSelectedIndex);
  for i:= FFirstRows[n] to FLastRows[n] do
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(i, j);
end;

procedure TSIZNormSubItemsSheet.DelSelection;
begin
  FSelectedIndex:= -1;
  SelectionClear;
end;

procedure TSIZNormSubItemsSheet.SelectionMove(const AVertDelta: Integer);
var
  SubItemIndex: Integer;
begin
  if not IsSelected then Exit;
  SubItemIndex:= IndexToSubItemIndex(FSelectedIndex);
  if SubItemIndex<0 then Exit;
  SubItemIndex:= SubItemIndex + AVertDelta;
  if not CheckIndex(High(FFirstRows), SubItemIndex) then Exit;
  Select(FFirstRows[SubItemIndex], 1);
end;

function TSIZNormSubItemsSheet.IsCellSelectable(const ARow, ACol: Integer): Boolean;
begin
  Result:= (inherited IsCellSelectable(ARow, ACol)) and (RowToIndex(ARow)>=0);
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
  if AIndex<1000 then
    Result:= 0
  else
    Result:= AIndex - 1000;
end;

procedure TSIZNormSubItemsSheet.CaptionDraw;
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaTop);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(1, 1, 'Тип СИЗ', cbtOuter);
  Writer.WriteText(1, 2, 'Наименование СИЗ ' + SYMBOL_BREAK +
                         '(с указанием конкретных данных о ' +
                         'конструкции, классе защиты, категориях эффективности ' +
                         'и/или эксплуатационных уровнях)', cbtOuter);
  Writer.WriteText(1, 3, 'Нормы выдачи' + SYMBOL_BREAK +
                         'с указанием периодичности выдачи, ' +
                         'количества на период, единицы измерения (штуки, ' +
                         'пары, комплекты, г, мл)', cbtOuter);
  Writer.WriteText(1, 4, 'Основание выдачи СИЗ' + SYMBOL_BREAK +
                         '(пункты Единых типовых норм, ' +
                         'правил по охране труда и иных документов)', cbtOuter);
  Writer.SetRowHeight(1, TITLE_HEIGHT);
end;

procedure TSIZNormSubItemsSheet.ReasonDraw(var ARow: Integer; const AIndex: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold, fsItalic], clBlack);
  Writer.WriteText(ARow, 1, ARow, 4, FSubItems[AIndex].Reason + ':', cbtOuter, True, True);
  ARow:= ARow + 1;
end;

procedure TSIZNormSubItemsSheet.LineDraw(var ARow: Integer; const AIndex: Integer);
var
  i, N: Integer;
  V: TStrVector;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  V:= nil;
  N:= High(FSubItems[AIndex].Info.Names);

  //Тип СИЗ
  VDim(V, N+1);
  for i:=0 to N do
    V[i]:= SIZ_TYPE_PICKS[FSubItems[AIndex].Info.SIZTypes[i]];
  VectorDraw(Writer, V, ARow, 1, EmptyStr, True, haCenter, vaTop);


  //Наименование СИЗ
  V:= FSubItems[AIndex].Info.Names;
  VectorDraw(Writer, V, ARow, 2, 'или', False, haLeft, vaTop);
  //Нормы выдачи
  VDim(V, N+1);
  for i:=0 to N do
    V[i]:= SIZUnitCommaNumForPeriod(FSubItems[AIndex].Info.Units[i],
                                    FSubItems[AIndex].Info.Nums[i],
                                    FSubItems[AIndex].Info.Lifes[i]);
  VectorDraw(Writer, V, ARow, 3, EmptyStr, False, haCenter, vaTop);
  //Основание выдачи (пункты ЕТН)
  V:= FSubItems[AIndex].Info.ClauseNames;
  VectorDraw(Writer, V, ARow, 4, EmptyStr, True, haCenter, vaTop);
  //Границы ячеек
  for i:= 1 to Writer.ColCount do
    Writer.DrawBorders(ARow, i, ARow+2*N, i, cbtOuter);

  ARow:= ARow + 2*N + 1;
end;

procedure TSIZNormSubItemsSheet.Draw(const ASubItems: TNormSubItems;
                                     const ASelectedIndex: Integer);
var
  i, R: Integer;
  S: String;
begin
  FSubItems:= ASubItems;

  if Length(FSubItems)=0 then
  begin
    Writer.BeginEdit;
    CaptionDraw;
    Writer.EndEdit;
    Exit;
  end;

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

  SelectByIndex(ASelectedIndex);
end;

procedure TSIZNormSubItemsSheet.SelectByIndex(const ASelectedIndex: Integer);
begin
  Select(SubItemIndexToRow(ASelectedIndex), 1);
end;

function TSIZNormSubItemsSheet.CanUp: Boolean;
var
  SubItemIndex: Integer;
begin
  SubItemIndex:= IndexToSubItemIndex(FSelectedIndex);
  Result:= IsSelected and (SubItemIndex>0) and
           (FSubItems[SubItemIndex].ReasonID = FSubItems[SubItemIndex-1].ReasonID);
end;

function TSIZNormSubItemsSheet.CanDown: Boolean;
var
  SubItemIndex: Integer;
begin
  SubItemIndex:= IndexToSubItemIndex(FSelectedIndex);
  Result:= IsSelected and (SubItemIndex<High(FSubItems)) and
           (FSubItems[SubItemIndex].ReasonID = FSubItems[SubItemIndex+1].ReasonID);
end;

{ TSIZNormSheet }

function TSIZNormSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    COLUMN1_WIDTH,
    COLUMN2_WIDTH,
    COLUMN3_WIDTH,
    COLUMN4_WIDTH,
    COLUMN5_WIDTH,
    COLUMN6_WIDTH
  ]);
end;

procedure TSIZNormSheet.TitleDraw(var ARow: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
  Writer.WriteText(ARow, 1, ARow, Writer.ColCount, 'Нормы выдачи СИЗ', cbtNone, True, True);
  ARow:= ARow+1;
  Writer.WriteText(ARow, 1, ARow, Writer.ColCount, FNorm.NormName, cbtNone, True, True);
  ARow:= ARow+1;
  Writer.WriteText(ARow, 1, ARow, Writer.ColCount, '('+FNorm.Note+')', cbtNone, True, True);
end;

procedure TSIZNormSheet.CaptionDraw(var ARow: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaTop);
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.WriteText(ARow, 1, '№ п/п', cbtOuter);
  Writer.WriteText(ARow, 2, 'Наименование профессии' + SYMBOL_BREAK +
                            '(должности)', cbtOuter, True, True);
  Writer.WriteText(ARow, 3, 'Тип СИЗ', cbtOuter);
  Writer.WriteText(ARow, 4, 'Наименование СИЗ ' + SYMBOL_BREAK +
                         '(с указанием конкретных данных о ' +
                         'конструкции, классе защиты, категориях эффективности ' +
                         'и/или эксплуатационных уровнях)', cbtOuter, True, True);
  Writer.WriteText(ARow, 5, 'Нормы выдачи' + SYMBOL_BREAK +
                         'с указанием периодичности выдачи, ' +
                         'количества на период, единицы измерения (штуки, ' +
                         'пары, комплекты, г, мл)', cbtOuter, True, True);
  Writer.WriteText(ARow, 6, 'Основание' + SYMBOL_BREAK +
                         'выдачи СИЗ' + SYMBOL_BREAK +
                         '(пункты Единых типовых норм, ' +
                         'правил по охране труда и иных документов)', cbtOuter, True, True);

  Writer.SetRepeatedRows(ARow, ARow);
end;

procedure VectorDraw(const AWriter: TSheetWriter;
                       const AVector: TStrVector;
                       const ARow, ACol: Integer;
                       const AMiddleStr: String;
                       const ASameCheck: Boolean;
                       const AHorAlignment: TsHorAlignment;
                       const AVertAlignment: TsVertAlignment);
begin
  VectorDraw(AWriter, AVector, ARow, ACol, ACol, AMiddleStr, ASameCheck,
             AHorAlignment, AVertAlignment);
end;

procedure VectorDraw(const AWriter: TSheetWriter;
                       const AVector: TStrVector;
                       const ARow, ACol1, ACol2: Integer;
                       const AMiddleStr: String;
                       const ASameCheck: Boolean;
                       const AHorAlignment: TsHorAlignment;
                       const AVertAlignment: TsVertAlignment);
var
  i, N, R: Integer;
begin
  if VIsNil(AVector) then Exit;

  N:= High(AVector);

  if ASameCheck and VSame(AVector) then
  begin
    AWriter.SetAlignment(AHorAlignment, AVertAlignment);
    AWriter.WriteText(ARow, ACol1, ARow+2*N, ACol2, AVector[0], cbtNone, True, True);
    Exit;
  end;

  R:= ARow - 1;
  for i:=0 to N do
  begin
    R:= R + 1;
    AWriter.SetAlignment(AHorAlignment, AVertAlignment);
    AWriter.WriteText(R, ACol1, R, ACol2, AVector[i], cbtNone, True, True);
    if i<N then
    begin
      R:= R + 1;
      AWriter.SetAlignment(haLeft, AVertAlignment);
      AWriter.WriteText(R, ACol1, R, ACol2, AMiddleStr, cbtNone, True, True);
    end;
  end;
end;

procedure TSIZNormSheet.ItemDraw(var ARow: Integer; const AItem: TNormItem);
var
  i, R, R1, R2: Integer;
  S: String;

  procedure ReasonDraw(var AR: Integer; const AIndex: Integer);
  begin
    Writer.SetBackgroundDefault;
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.SetFont(Font.Name, Font.Size, [fsBold, fsItalic], clBlack);
    Writer.WriteText(AR, 3, AR, Writer.ColCount,
                     AItem.SubItems[AIndex].Reason + ':', cbtOuter, True, True);
    AR:= AR + 1;
  end;

  procedure LineDraw(var AR: Integer; const AIndex: Integer);
  var
    j, N: Integer;
    V: TStrVector;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);

    V:= nil;
    N:= High(AItem.SubItems[AIndex].Info.Names);

    //Тип СИЗ
    VDim(V, N+1);
    for j:=0 to N do
      V[j]:= SIZ_TYPE_PICKS[AItem.SubItems[AIndex].Info.SIZTypes[j]];
    VectorDraw(Writer, V, AR, 3, EmptyStr, True, haCenter, vaTop);
    //Наименование СИЗ
    V:= AItem.SubItems[AIndex].Info.Names;
    VectorDraw(Writer, V, AR, 4, 'или', False, haLeft, vaTop);
    //Нормы выдачи
    VDim(V, N+1);
    for j:=0 to N do
      V[j]:= SIZUnitCommaNumForPeriod(AItem.SubItems[AIndex].Info.Units[j],
                                      AItem.SubItems[AIndex].Info.Nums[j],
                                      AItem.SubItems[AIndex].Info.Lifes[j]);
    VectorDraw(Writer, V, AR, 5, EmptyStr, False, haCenter, vaTop);
    //Основание выдачи (пункты ЕТН)
    V:= AItem.SubItems[AIndex].Info.ClauseNames;
    VectorDraw(Writer, V, AR, 6, EmptyStr, True, haCenter, vaTop);
    //Границы ячеек
    for j:= 3 to Writer.ColCount do
      Writer.DrawBorders(AR, j, AR+2*N, j, cbtOuter);

    AR:= AR + 2*N;
  end;

begin
  S:= MAIN_REASON;
  R1:= ARow;
  R:= ARow-1;
  if Length(AItem.SubItems)=0 then
  begin
    R:= R + 1;
    for i:= 3 to Writer.ColCount do
      Writer.WriteText(R, i, EmptyStr, cbtOuter);
  end
  else begin
    for i:=0 to High(AItem.SubItems) do
    begin
      R:= R + 1;
      if AItem.SubItems[i].Reason<>S then
      begin
        ReasonDraw(R, i);
        S:= AItem.SubItems[i].Reason;
      end;
      LineDraw(R, i);
    end;
  end;
  R2:= R;

  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [{fsBold}], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  if SEmpty(AItem.OrderName) then
    Writer.WriteNumber(R1, 1, R2, 1, AItem.OrderNum+1, cbtOuter)
  else
    Writer.WriteText(R1, 1, R2, 1, AItem.OrderName, cbtOuter);
  Writer.SetAlignment(haLeft, vaTop);
  S:= EmptyStr;
  if not VIsNil(AItem.PostNames) then
    S:= VVectorToStr(AItem.PostNames, ',' + SYMBOL_BREAK);
  Writer.WriteText(R1, 2, R2, 2, S, cbtOuter, True, True);

  ARow:= R2;
end;

procedure TSIZNormSheet.Draw(const ANorm: TNorm);
var
  i, R: Integer;
begin
  FNorm:= ANorm;

  Writer.BeginEdit;

  R:= 1;
  TitleDraw(R);
  R:= R + 2;
  CaptionDraw(R);
  for i:=0 to High(ANorm.Items) do
  begin
    R:= R + 1;
    ItemDraw(R, ANorm.Items[i]);
  end;

  R:= R + 1;
  for i:=1 to Writer.ColCount do
    Writer.DrawBorders(R, i, cbtTop);

  Writer.EndEdit;
end;

end.

