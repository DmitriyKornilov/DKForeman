unit USIZStoreSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  //Project utils
  USIZUtils,
  //DK packages utils
  DK_SheetTypes, DK_Vector, DK_Matrix, DK_StrUtils;

type

  { TSIZStoreSheet }

  TSIZStoreSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //1200
      COLWIDTHS: array of Integer = (
        {01} 40,  //№п/п
        {02} 200, //номенкл. номер
        {03} 280, //наименование
        {04} 150, //единица измерения
        {05} 150, //количество
        {06} 180, //размер, объем, вес
        {07} 200  //документ поступления
      );
  public
    procedure Draw(const ANomNums, ASizNames, ASizUnits, ASizSizes, ADocNames: TStrMatrix;
                   const ASizCounts: TIntMatrix);
  end;

  { TSIZStoreEntrySheet }

  TSIZStoreEntrySheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //1200
      COLWIDTHS: array of Integer = (
        {01} 40,  //№п/п
        {02} 200, //номенкл. номер
        {03} 280, //наименование
        {04} 150, //единица измерения
        {05} 150, //количество
        {06} 180, //размер, объем, вес
        {07} 200  //примечание
      );
  public
    procedure Draw(const ANomNums, ASizNames, ASizUnits, ANotes: TStrMatrix;
                   const ASizCounts, ASizeIDs, AHeightIDs, ASizeTypes: TIntMatrix);
  end;

  { TSIZStoreWriteoffSheet }

  TSIZStoreWriteoffSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //1300
      COLWIDTHS: array of Integer = (
        {01} 40,  //№п/п
        {02} 200, //номенкл. номер
        {03} 280, //наименование
        {04} 150, //единица измерения
        {05} 150, //количество
        {06} 180, //размер, объем, вес
        {07} 150, //документ поступления
        {08} 150  //примечание
      );
  public
    procedure Draw(const ANomNums, ASizNames, ASizUnits, ASizSizes, AEntryDocNames, ANotes: TStrMatrix;
                   const ASizCounts: TIntMatrix);
  end;

  { TSIZStoreReceivingSheet }

  TSIZStoreReceivingSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //1300
      COLWIDTHS: array of Integer = (
        {01} 40,  //№п/п
        {02} 150, //ФИО
        {03} 150, //табельный номер
        {04} 200, //Должность (профессия)
        {05} 280, //наименование СИЗ
        {06} 200, //номенкл. номер
        {07} 150, //единица измерения
        {08} 150, //количество
        {09} 100, //дата выдачи
        {10} 150  //срок службы
      );
  public
    procedure Draw(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                   const ASizCounts: TIntMatrix;
                   const ANomNums, ASizNames, ASizStrUnits, ASizLifes: TStrMatrix;
                   const AReceivingDates: TDateMatrix);
  end;

  { TSIZStoreReturningSheet }

  TSIZStoreReturningSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //1400
      COLWIDTHS: array of Integer = (
        {01} 40,  //№п/п
        {02} 150, //ФИО
        {03} 150, //табельный номер
        {04} 200, //Должность (профессия)
        {05} 280, //наименование СИЗ
        {06} 200, //номенкл. номер
        {07} 150, //единица измерения
        {08} 150, //количество
        {09} 150, //документ выдачи
        {10} 150  //примечание
      );
  public
    procedure Draw(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                   const ASizCounts: TIntMatrix;
                   const ANomNums, ASizNames, ASizStrUnits,
                         AReceivingDocNames, AReceivingDocNums, ANotes: TStrMatrix;
                   const AReceivingDates: TDateMatrix);
  end;


implementation

{ TSIZStoreSheet }

function TSIZStoreSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreSheet.Draw(const ANomNums, ASizNames, ASizUnits, ASizSizes, ADocNames: TStrMatrix;
                   const ASizCounts: TIntMatrix);
var
  i, j, R: Integer;

  procedure CaptionDraw;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, 1, '№ п/п', cbtOuter, True, True);
    Writer.WriteText(R, 2, 'Номенклатурный номер', cbtOuter, True, True);
    Writer.WriteText(R, 3, 'Наименование СИЗ', cbtOuter, True, True);
    Writer.WriteText(R, 4, 'Единица измерения', cbtOuter, True, True);
    Writer.WriteText(R, 5, 'Количество', cbtOuter, True, True);
    Writer.WriteText(R, 6, 'Размер/объём/вес', cbtOuter, True, True);
    Writer.WriteText(R, 7, 'Документ поступления', cbtOuter, True, True);
  end;

  procedure LineDraw(const AInd1, AInd2: Integer);
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    R:= R + 1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R, 1, R-1, cbtOuter);
    Writer.WriteText(R, 2, ANomNums[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 4, ASizUnits[AInd1, AInd2], cbtOuter);
    Writer.WriteNumber(R, 5, ASizCounts[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 6, ASizSizes[AInd1, AInd2], cbtOuter);
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.WriteText(R, 3, ASizNames[AInd1, AInd2], cbtOuter, True, True);
    Writer.WriteText(R, 7, ADocNames[AInd1, AInd2], cbtOuter, True, True);
  end;

begin
  Writer.BeginEdit;

  R:= 1;
  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    for j:= 0 to High(ANomNums[i]) do
      LineDraw(i, j);

  Writer.EndEdit;
end;

{ TSIZStoreEntrySheet }

function TSIZStoreEntrySheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreEntrySheet.Draw(const ANomNums, ASizNames, ASizUnits, ANotes: TStrMatrix;
                     const ASizCounts, ASizeIDs, AHeightIDs, ASizeTypes: TIntMatrix);
var
  i, j, R: Integer;

  procedure CaptionDraw;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, 1, '№ п/п', cbtOuter, True, True);
    Writer.WriteText(R, 2, 'Номенклатурный номер', cbtOuter, True, True);
    Writer.WriteText(R, 3, 'Наименование СИЗ', cbtOuter, True, True);
    Writer.WriteText(R, 4, 'Единица измерения', cbtOuter, True, True);
    Writer.WriteText(R, 5, 'Количество', cbtOuter, True, True);
    Writer.WriteText(R, 6, 'Размер/объём/вес', cbtOuter, True, True);
    Writer.WriteText(R, 7, 'Примечание', cbtOuter, True, True);
  end;

  procedure LineDraw(const AInd1, AInd2: Integer);
  var
    S: String;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    R:= R + 1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R, 1, R-1, cbtOuter);
    Writer.WriteText(R, 2, ANomNums[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 4, ASizUnits[AInd1, AInd2], cbtOuter);
    Writer.WriteNumber(R, 5, ASizCounts[AInd1, AInd2], cbtOuter);
    S:= SIZFullSize(ASizeTypes[AInd1, AInd2], ASizeIDs[AInd1, AInd2], AHeightIDs[AInd1, AInd2]);
    Writer.WriteText(R, 6, S, cbtOuter);
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.WriteText(R, 3, ASizNames[AInd1, AInd2], cbtOuter, True, True);
    Writer.WriteText(R, 7, ANotes[AInd1, AInd2], cbtOuter, True, True);
  end;

begin
  Writer.BeginEdit;

  R:= 1;
  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    for j:= 0 to High(ANomNums[i]) do
      LineDraw(i, j);

  Writer.EndEdit;
end;

{ TSIZStoreWriteoffSheet }

function TSIZStoreWriteoffSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreWriteoffSheet.Draw(const ANomNums, ASizNames, ASizUnits,
                                  ASizSizes, AEntryDocNames, ANotes: TStrMatrix;
                                  const ASizCounts: TIntMatrix);
var
  i, j, R: Integer;

  procedure CaptionDraw;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, 1, '№ п/п', cbtOuter, True, True);
    Writer.WriteText(R, 2, 'Номенклатурный номер', cbtOuter, True, True);
    Writer.WriteText(R, 3, 'Наименование СИЗ', cbtOuter, True, True);
    Writer.WriteText(R, 4, 'Единица измерения', cbtOuter, True, True);
    Writer.WriteText(R, 5, 'Количество', cbtOuter, True, True);
    Writer.WriteText(R, 6, 'Размер/объём/вес', cbtOuter, True, True);
    Writer.WriteText(R, 7, 'Документ поступления', cbtOuter, True, True);
    Writer.WriteText(R, 8, 'Примечание', cbtOuter, True, True);
  end;

  procedure LineDraw(const AInd1, AInd2: Integer);
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    R:= R + 1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R, 1, R-1, cbtOuter);
    Writer.WriteText(R, 2, ANomNums[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 4, ASizUnits[AInd1, AInd2], cbtOuter);
    Writer.WriteNumber(R, 5, ASizCounts[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 6, ASizSizes[AInd1, AInd2], cbtOuter);
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.WriteText(R, 3, ASizNames[AInd1, AInd2], cbtOuter, True, True);
    Writer.WriteText(R, 7, AEntryDocNames[AInd1, AInd2], cbtOuter, True, True);
    Writer.WriteText(R, 8, ANotes[AInd1, AInd2], cbtOuter, True, True);
  end;

begin
  Writer.BeginEdit;

  R:= 1;
  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    for j:= 0 to High(ANomNums[i]) do
      LineDraw(i, j);

  Writer.EndEdit;
end;

{ TSIZStoreReceivingSheet }

function TSIZStoreReceivingSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreReceivingSheet.Draw(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                   const ASizCounts: TIntMatrix;
                   const ANomNums, ASizNames, ASizStrUnits, ASizLifes: TStrMatrix;
                   const AReceivingDates: TDateMatrix);
var
  i, j, R: Integer;

  procedure CaptionDraw;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, 1, '№ п/п', cbtOuter, True, True);
    Writer.WriteText(R, 2, 'Фамилия, имя, отчество', cbtOuter, True, True);
    Writer.WriteText(R, 3, 'Табельный номер', cbtOuter, True, True);
    Writer.WriteText(R, 4, 'Должность (профессия)', cbtOuter, True, True);
    Writer.WriteText(R, 5, 'Наименование СИЗ', cbtOuter, True, True);
    Writer.WriteText(R, 6, 'Номенклатурный номер', cbtOuter, True, True);
    Writer.WriteText(R, 7, 'Единица измерения', cbtOuter, True, True);
    Writer.WriteText(R, 8, 'Количество', cbtOuter, True, True);
    Writer.WriteText(R, 9, 'Дата выдачи', cbtOuter, True, True);
    Writer.WriteText(R, 10, 'Срок службы', cbtOuter, True, True);
  end;

  procedure LineDraw(const AInd1, AInd2: Integer);
  var
    S: String;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    R:= R + 1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R, 1, R-1, cbtOuter);
    Writer.WriteText(R, 3, ATabNums[AInd1], cbtOuter);
    Writer.WriteText(R, 6, ANomNums[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 7, ASizStrUnits[AInd1, AInd2], cbtOuter);
    Writer.WriteNumber(R, 8, ASizCounts[AInd1, AInd2], cbtOuter);
    Writer.WriteDate(R, 9, AReceivingDates[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 10, ASizLifes[AInd1, AInd2], cbtOuter, True, True);
    Writer.SetAlignment(haLeft, vaCenter);
    S:= SNameLong(AFs[AInd1], ANs[AInd1], APs[AInd1]);
    Writer.WriteText(R, 2, S, cbtOuter, True, True);
    Writer.WriteText(R, 4, APostNames[AInd1], cbtOuter, True, True);
    Writer.WriteText(R, 5, ASizNames[AInd1, AInd2], cbtOuter, True, True);
  end;

begin
  Writer.BeginEdit;

  R:= 1;
  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    for j:= 0 to High(ANomNums[i]) do
      LineDraw(i, j);

  Writer.EndEdit;
end;

{ TSIZStoreReturningSheet }

function TSIZStoreReturningSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreReturningSheet.Draw(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                   const ASizCounts: TIntMatrix;
                   const ANomNums, ASizNames, ASizStrUnits,
                         AReceivingDocNames, AReceivingDocNums, ANotes: TStrMatrix;
                   const AReceivingDates: TDateMatrix);
var
  i, j, R: Integer;

  procedure CaptionDraw;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, 1, '№ п/п', cbtOuter, True, True);
    Writer.WriteText(R, 2, 'Фамилия, имя, отчество', cbtOuter, True, True);
    Writer.WriteText(R, 3, 'Табельный номер', cbtOuter, True, True);
    Writer.WriteText(R, 4, 'Должность (профессия)', cbtOuter, True, True);
    Writer.WriteText(R, 5, 'Наименование СИЗ', cbtOuter, True, True);
    Writer.WriteText(R, 6, 'Номенклатурный номер', cbtOuter, True, True);
    Writer.WriteText(R, 7, 'Единица измерения', cbtOuter, True, True);
    Writer.WriteText(R, 8, 'Количество', cbtOuter, True, True);
    Writer.WriteText(R, 9, 'Документ выдачи', cbtOuter, True, True);
    Writer.WriteText(R, 10, 'Примечание', cbtOuter, True, True);
  end;

  procedure LineDraw(const AInd1, AInd2: Integer);
  var
    S: String;
  begin
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);
    R:= R + 1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R, 1, R-1, cbtOuter);
    Writer.WriteText(R, 3, ATabNums[AInd1], cbtOuter);
    Writer.WriteText(R, 6, ANomNums[AInd1, AInd2], cbtOuter);
    Writer.WriteText(R, 7, ASizStrUnits[AInd1, AInd2], cbtOuter);
    Writer.WriteNumber(R, 8, ASizCounts[AInd1, AInd2], cbtOuter);

    Writer.SetAlignment(haLeft, vaCenter);
    S:= SNameLong(AFs[AInd1], ANs[AInd1], APs[AInd1]);
    Writer.WriteText(R, 2, S, cbtOuter, True, True);
    Writer.WriteText(R, 4, APostNames[AInd1], cbtOuter, True, True);
    Writer.WriteText(R, 5, ASizNames[AInd1, AInd2], cbtOuter, True, True);
    S:= SIZDocFullName(AReceivingDocNames[AInd1, AInd2],
                       AReceivingDocNums[AInd1, AInd2],
                       AReceivingDates[AInd1, AInd2]);
    Writer.WriteText(R, 9, S, cbtOuter, True, True);
    Writer.WriteText(R, 10, ANotes[AInd1, AInd2], cbtOuter, True, True);
  end;

begin
  Writer.BeginEdit;

  R:= 1;
  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    for j:= 0 to High(ANomNums[i]) do
      LineDraw(i, j);

  Writer.EndEdit;
end;

end.

