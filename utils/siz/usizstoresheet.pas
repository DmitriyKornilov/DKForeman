unit USIZStoreSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  //Project utils
  UConst, UUtils, USIZUtils,
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

  { TSIZStoreHistorySheet }

  TSIZStoreHistorySheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //710
      COLWIDTHS: array of Integer = (
        {01} 30,  //наименование/инфо 1
        {02} 30,  //наименование/инфо 2
        {03} 550, //наименование/инфо 3
        {04} 100  //количество
      );
  public
    procedure Draw(const ANomNums, ASizNames, AHistoryItems: TStrVector;
                   const AInfos: TStrMatrix;
                   const ASizCounts: TIntMatrix);
  end;

  { TSIZStoreRequestSheet }

  TSIZStoreRequestSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      COLWIDTHS: array of Integer = (
        {01} 400,  //наименование сиз
        {02} 50,   //пол
        {03} 100,  //размер
        {04} 100,  //количество
        {05} 600   //кому выдавать
      );
  public
    procedure Draw(const ATitle: String;
                   const ASizNames: TStrVector;
                   const AGenders: TIntVector;
                   const ASizSizes: TStrMatrix;
                   const AFamilies, ANames, APatronymics, ATabNums: TStrMatrix3D;
                   const ASizCounts: TIntMatrix3D);
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
    R:= 1;
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
    R:= 1;
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
    R:= 1;
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
    R:= 1;
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
    R:= 1;
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

  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    for j:= 0 to High(ANomNums[i]) do
      LineDraw(i, j);

  Writer.EndEdit;
end;

{ TSIZStoreHistorySheet }

function TSIZStoreHistorySheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreHistorySheet.Draw(const ANomNums, ASizNames, AHistoryItems: TStrVector;
                   const AInfos: TStrMatrix;
                   const ASizCounts: TIntMatrix);
var
  i, R: Integer;

  procedure CaptionDraw;
  var
    S: String;
  begin
    R:= 1;
    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haCenter, vaCenter);
    S:= 'Номенклатурный номер и наименование СИЗ / информация о движении СИЗ';
    Writer.WriteText(R, 1, R, Writer.ColCount-1, S, cbtOuter, True, True);
    Writer.WriteText(R, Writer.ColCount, 'Количество', cbtOuter);
  end;

  procedure LineDraw(const AInd: Integer);
  var
    S: String;
    k: Integer;
  begin
    R:= R + 1;

    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.SetAlignment(haLeft, vaCenter);
    S:= ANomNums[AInd] + ' - ' + ASizNames[AInd];
    Writer.WriteText(R, 1, R, Writer.ColCount, S, cbtOuter, True, True);

    for k:= 0 to High(AInfos[AInd]) do
    begin
      R:= R + 1;
      if VIndexOf(AHistoryItems, AInfos[AInd, k])>=0 then
      begin
        Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
        Writer.SetAlignment(haLeft, vaCenter);
        Writer.WriteText(R, 2, R, 3, AInfos[AInd, k], cbtNone, True, True);
      end
      else begin
        Writer.SetFont(Font.Name, Font.Size, [], clBlack);
        Writer.SetAlignment(haLeft, vaCenter);
        Writer.WriteText(R, 3, AInfos[AInd, k], cbtNone, True, True);
      end;
      Writer.SetAlignment(haCenter, vaCenter);
      Writer.WriteNumber(R, 4, ASizCounts[AInd, k], cbtOuter);
      Writer.DrawBorders(R, 1, R, 3, cbtOuter);
    end;
  end;

begin
  Writer.BeginEdit;

  CaptionDraw;
  for i:= 0 to High(ANomNums) do
    LineDraw(i);

  Writer.EndEdit;
end;

{ TSIZStoreRequestSheet }

function TSIZStoreRequestSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZStoreRequestSheet.Draw(const ATitle: String;
                   const ASizNames: TStrVector;
                   const AGenders: TIntVector;
                   const ASizSizes: TStrMatrix;
                   const AFamilies, ANames, APatronymics, ATabNums: TStrMatrix3D;
                   const ASizCounts: TIntMatrix3D);
var
  i, R: Integer;

  procedure CaptionDraw;
  begin
    R:= 1;
    Writer.SetBackgroundDefault;
    Writer.SetAlignment(haCenter, vaCenter);

    if not SEmpty(ATitle) then
    begin
      Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
      Writer.WriteText(R, 1, R, Writer.ColCount, ATitle, cbtNone, True, True);
      R:= R + 1;
    end;

    Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
    Writer.WriteText(R, 1, 'Наименование СИЗ', cbtOuter, True, True);
    Writer.WriteText(R, 2, 'Пол', cbtOuter);
    Writer.WriteText(R, 3, 'Размер', cbtOuter);
    Writer.WriteText(R, 4, 'Количество', cbtOuter);
    Writer.WriteText(R, 5, 'Примечание', cbtOuter);
  end;

  function InfoLoad(const AInd1, AInd2, AInd3: Integer): String;
  begin
    Result:= StaffFullName(AFamilies[AInd1, AInd2, AInd3],
                           ANames[AInd1, AInd2, AInd3],
                           APatronymics[AInd1, AInd2, AInd3],
                           ATabNums[AInd1, AInd2, AInd3],
                           True{short)}) +
            ' = ' + IntToStr(ASizCounts[AInd1, AInd2, AInd3]);
  end;

  procedure LineDraw(const AInd: Integer);
  var
    m, n, R1, R2: Integer;
    S: String;
  begin
    R1:= R + 1;
    R2:= R1 + High(ASizSizes[AInd]);

    Writer.SetBackgroundDefault;
    Writer.SetFont(Font.Name, Font.Size, [], clBlack);

    Writer.SetAlignment(haLeft, vaTop);
    Writer.WriteText(R1, 1, R2, 1, ASizNames[AInd], cbtOuter, True, True);
    Writer.SetAlignment(haCenter, vaTop);
    Writer.WriteText(R1, 2, R2, 2, GENDER_PICKS[AGenders[AInd]], cbtOuter);

    for m:= 0 to High(ASizSizes[AInd]) do
    begin
      R:= R + 1;
      Writer.SetAlignment(haCenter, vaTop);
      Writer.WriteText(R, 3, ASizSizes[AInd, m], cbtOuter);
      Writer.WriteNumber(R, 4, VSum(ASizCounts[AInd, m]), cbtOuter);
      S:= InfoLoad(AInd, m, 0);
      for n:= 1 to High(AFamilies[AInd, m]) do
        S:= S + '; ' + InfoLoad(AInd, m, n);
      Writer.SetAlignment(haLeft, vaTop);
      Writer.WriteText(R, 5, S, cbtOuter, True, True);
    end;

    Writer.DrawBorders(R1, 1, R2, Writer.ColCount, cbtAll);
  end;

begin
  Writer.BeginEdit;

  CaptionDraw;
  for i:= 0 to High(ASizNames) do
    LineDraw(i);

  Writer.EndEdit;
end;

end.

