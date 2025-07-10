unit USIZCardSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpstypes, DateUtils,
  //DK packages utils
  DK_SheetTypes, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const,
  DK_SheetWriter,
  //Project utils
  USIZNormTypes, USIZUtils, USIZSizes, UConst, USIZNormSheet, USIZCardTypes;

type

  { TSIZCardFrontSheet }

  TSIZCardFrontSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //700
      COLUMN1_WIDTH = 40;
      COLUMN2_WIDTH = 25;
      COLUMN3_WIDTH = 100;
      COLUMN4_WIDTH = 120;
      COLUMN5_WIDTH = 25;
      COLUMN6_WIDTH = 170;
      COLUMN7_WIDTH = 20;
      COLUMN8_WIDTH = 100;
      COLUMN9_WIDTH = 100;
    var
      FCardNum, FFamily, FPersonName, FPatronymic, FGender,
      FTabNum, FPostName, FDepartment: String;
      FCardBD, FCardED: TDate;
      FPersonSizes: TSIZStaffSizeIndexes;
      FSubItems: TNormSubItems;

    procedure AttachmentDraw(var ARow: Integer);
    procedure TitleDraw(var ARow: Integer);
    procedure MainInfoDraw(var ARow: Integer);
    procedure MainDataDraw(const ARow: Integer);
    procedure CaptionDraw(var ARow: Integer);

    procedure ReasonDraw(var ARow: Integer; const AIndex: Integer);
    procedure LineDraw(var ARow: Integer; const AIndex: Integer);
    procedure GridDraw(var ARow: Integer);

    procedure SignatureDraw(var ARow: Integer);
  public
    procedure Draw(const ACardNum, AFamily, APersonName, APatronymic,
                         AGender, ATabNum, APostName, ADepartment: String;
                   const ACardBD, ACardED: TDate;
                   const APersonSizes: TSIZStaffSizeIndexes;
                   const ASubItems: TNormSubItems);

  end;

  { TSIZCardBackSheet }

  TSIZCardBackSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //980
      COLUMN1_WIDTH = 200;
      COLUMN2_WIDTH = 200;
      COLUMN3_WIDTH = 60;
      COLUMN4_WIDTH = 50;
      COLUMN5_WIDTH = 60;
      COLUMN6_WIDTH = 100;
      COLUMN7_WIDTH = 60;
      COLUMN8_WIDTH = 50;
      COLUMN9_WIDTH = 100;
      COLUMN10_WIDTH = 100;
    var
      FItemSIZNames: TStrVector;

    procedure CaptionDraw(var ARow: Integer);
    procedure GridDraw(var ARow: Integer);
    procedure NoteDraw(var ARow: Integer);
  public
    procedure Draw(const ANeedDraw: Boolean; const AItemSIZNames: TStrVector);
  end;

  { TSIZCardStatusSheet }

  TSIZCardStatusSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const //980
      COLUMN1_WIDTH = 230; //перечень СИЗ по нормам
      COLUMN2_WIDTH = 80;  //единица измерения
      COLUMN3_WIDTH = 100; //количество на период
      COLUMN4_WIDTH = 100; //размер
      COLUMN5_WIDTH = 230; //перечень выданных СИЗ
      COLUMN6_WIDTH = 80;  //количество
      COLUMN7_WIDTH = 80;  //дата выдачи
      COLUMN8_WIDTH = 80;  //дата следующей выдачи
    var
      FSubItems: TNormSubItems;
      FStatusItems: TStatusItems;

    procedure CaptionDraw(var ARow: Integer);

    procedure ReasonDraw(var ARow: Integer; const AIndex: Integer);
    procedure NormLineDraw(var ARow: Integer; const AIndex: Integer);
    procedure ReceivingLineDraw(var ARow: Integer; const AIndex: Integer);
    procedure NormDraw(var ARow: Integer);

  public
    procedure Draw(const ASubItems: TNormSubItems;
                   const AStatusItems: TStatusItems);
  end;

implementation

{ TSIZCardFrontSheet }

function TSIZCardFrontSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    COLUMN1_WIDTH,
    COLUMN2_WIDTH,
    COLUMN3_WIDTH,
    COLUMN4_WIDTH,
    COLUMN5_WIDTH,
    COLUMN6_WIDTH,
    COLUMN7_WIDTH,
    COLUMN8_WIDTH,
    COLUMN9_WIDTH
  ]);
end;

procedure TSIZCardFrontSheet.AttachmentDraw(var ARow: Integer);
var
  i, R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.WriteText(R, 8, R, Writer.ColCount, 'Приложение № 2', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 8, R, Writer.ColCount, 'к Правилам обеспечения работников', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 8, R, Writer.ColCount, 'средствами индивидуальной защиты', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 8, R, Writer.ColCount, 'и смывающими средствами, ', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 8, R, Writer.ColCount, 'утвержденным', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 8, R, Writer.ColCount, 'приказом Минтруда России', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 8, R, Writer.ColCount, 'от 29 октября 2021 г. № 766н', cbtNone);

  for i:= ARow to R do
    Writer.SetRowHeight(i, 12);

  ARow:= R;
end;

procedure TSIZCardFrontSheet.TitleDraw(var ARow: Integer);
var
  i, R: Integer;
  S: String;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size+1, [fsBold], clBlack);

  S:= 'ЛИЧНАЯ КАРТОЧКА № ';
  if SEmpty(FCardNum) then
    S:= S + '___'
  else
    S:= S + FCardNum;

  Writer.WriteText(R, 1, R, Writer.ColCount, S, cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 1, R, Writer.ColCount, 'учета выдачи СИЗ', cbtNone);

  for i:= ARow to R do
    Writer.SetRowHeight(i, 16);

  ARow:= R;
end;

procedure TSIZCardFrontSheet.MainInfoDraw(var ARow: Integer);
var
  i, R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 1, R, 2, 'Фамилия', cbtNone);
  Writer.WriteText(R, 3, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'Пол', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 1, 'Имя', cbtNone);
  Writer.WriteText(R, 2, R, 3, '', cbtBottom);
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, 4, R, 5, 'Отчество (при наличии)', cbtNone);
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, 6, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'Рост', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 3, 'Табельный номер', cbtNone);
  Writer.WriteText(R, 4, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'Размер:', cbtNone);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 3, 'Структурное подразделение', cbtNone);
  Writer.WriteText(R, 4, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'одежды', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 3, 'Профессия (должность)', cbtNone);
  Writer.WriteText(R, 4, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'обуви', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 3, 'Дата поступления на работу', cbtNone);
  Writer.WriteText(R, 4, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'головного убора', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 6, 'Дата изменения профессии (должности) или', cbtNone);
  Writer.WriteText(R, 8, R, 8, 'СИЗОД', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 6, 'перевода в другое структурное подразделение', cbtNone);

  R:= R + 1;
  Writer.WriteText(R, 1, R, 3, '', cbtBottom);
  Writer.WriteText(R, 4, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'СИЗ рук', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);


  for i:= ARow to R do
    Writer.SetRowHeight(i, 20);

  ARow:= R;
end;

procedure TSIZCardFrontSheet.MainDataDraw(const ARow: Integer);
var
  R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaBottom);

  Writer.WriteText(R, 3, R, 6, SUpper(FFamily), cbtBottom, True, True);
  Writer.WriteText(R, 9, R, 9, SUpper(FGender), cbtBottom, True, True);

  R:= R + 1;
  Writer.WriteText(R, 2, R, 3, SUpper(FPersonName), cbtBottom, True, True);
  Writer.WriteText(R, 6, R, 6, SUpper(FPatronymic), cbtBottom, True, True);
  Writer.WriteText(R, 9, R, 9, PERSONHEIGHTS[FPersonSizes.Height], cbtBottom, True, True);

  R:= R + 1;
  Writer.WriteText(R, 4, R, 6, FTabNum, cbtBottom, True, True);

  R:= R + 1;
  Writer.WriteText(R, 4, R, 6, SUpper(FDepartment), cbtBottom, True, True);
  Writer.WriteText(R, 9, R, 9, CLOTHES[FPersonSizes.Clothes], cbtBottom, True, True);

  R:= R + 1;
  Writer.WriteText(R, 4, R, 6, SUpper(FPostName), cbtBottom, True, True);
  Writer.WriteText(R, 9, R, 9, SHOES[FPersonSizes.Shoes], cbtBottom);

  R:= R + 1;
  Writer.WriteDate(R, 4, R, 6, FCardBD, cbtBottom);
  Writer.WriteText(R, 9, R, 9, HEADDRESS[FPersonSizes.Head], cbtBottom);

  R:= R + 1;
  if (FPersonSizes.Respirator>0) and (FPersonSizes.Gasmask>0) then
  begin
    Writer.WriteText(R, 9, R, 9, RESPIRATORS[FPersonSizes.Respirator] +
                                ' (респиратор)', cbtBottom, True, True);
    R:= R + 1;
    Writer.WriteText(R, 9, R, 9, GASMASKS[FPersonSizes.Gasmask] +
                                ' (противогаз)', cbtBottom, True, True);
  end
  else begin
    if FPersonSizes.Respirator>0 then
      Writer.WriteText(R, 9, R, 9, RESPIRATORS[FPersonSizes.Respirator] +
                                ' (респиратор)', cbtBottom, True, True)
    else if FPersonSizes.Gasmask>0 then
      Writer.WriteText(R, 9, R, 9, GASMASKS[FPersonSizes.Gasmask] +
                                ' (противогаз)', cbtBottom, True, True);
    R:= R + 1;
  end;

  R:= R + 1;
  if not SameDate(FCardED, INFDATE) then
    Writer.WriteDate(R, 4, R, 6, FCardED, cbtBottom);
  Writer.WriteText(R, 9, R, 9, HANDS[FPersonSizes.Hand], cbtBottom);
end;

procedure TSIZCardFrontSheet.CaptionDraw(var ARow: Integer);
var
  R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);

  Writer.WriteText(R, 1, R, 4, 'Наименование СИЗ', cbtOuter, True, True);
  Writer.WriteText(R, 5, R, 7, 'Пункт Норм', cbtOuter, True, True);
  Writer.WriteText(R, 8, R, 8, 'Единица' + SYMBOL_BREAK +
                               'измерения,' + SYMBOL_BREAK +
                               'периодичность' + SYMBOL_BREAK +
                               'выдачи', cbtOuter, True, True);
  Writer.WriteText(R, 9, R, 9, 'Количество на' + SYMBOL_BREAK + 'период', cbtOuter, True, True);

  ARow:= R;
end;

procedure TSIZCardFrontSheet.ReasonDraw(var ARow: Integer; const AIndex: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold, fsItalic], clBlack);
  Writer.WriteText(ARow, 1, ARow, 9, FSubItems[AIndex].Reason + ':', cbtOuter, True, True);
  ARow:= ARow + 1;
end;

procedure TSIZCardFrontSheet.LineDraw(var ARow: Integer; const AIndex: Integer);
var
  i, N: Integer;
  V: TStrVector;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  V:= nil;
  N:= High(FSubItems[AIndex].Info.Names);

  //Наименование СИЗ
  V:= VCut(FSubItems[AIndex].Info.Names);
  VectorDraw(Writer, V, ARow, 1, 4, 'или', False, haLeft, vaTop);
  //Пункт Норм
  V:= VCut(FSubItems[AIndex].Info.ClauseNames);
  VectorDraw(Writer, V, ARow, 5, 7, EmptyStr, True, haCenter, vaTop);
  //Единица измерения, периодичность выдачи
  VDim(V, N+1);
  for i:=0 to N do
    V[i]:= FSubItems[AIndex].Info.Units[i] + ', ' + SYMBOL_BREAK +
           '1 раз в ' + SIZLifePeriod(FSubItems[AIndex].Info.Lifes[i]);
  VectorDraw(Writer, V, ARow, 8, EmptyStr, False, haCenter, vaTop);
  //Количество на период
  V:= VIntToStr(FSubItems[AIndex].Info.Nums);
  VectorDraw(Writer, V, ARow, 9, EmptyStr, False, haCenter, vaTop);

  //Границы ячеек
  for i:= 1 to Writer.ColCount do
    Writer.DrawBorders(ARow, i, ARow+2*N, i, cbtOuter);

  ARow:= ARow + 2*N + 1;
end;

procedure TSIZCardFrontSheet.GridDraw(var ARow: Integer);
var
  i, R: Integer;
  S: String;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  if Length(FSubItems)=0 then
  begin
    Writer.WriteText(R, 1, R, 4, '', cbtOuter);
    Writer.WriteText(R, 5, R, 7, '', cbtOuter);
    Writer.WriteText(R, 8, R, 8, '', cbtOuter);
    Writer.WriteText(R, 9, R, 9, '', cbtOuter);
    Exit;
  end;

  //FFirstRows:= nil;
  //FLastRows:= nil;

  S:= MAIN_REASON;
  for i:=0 to High(FSubItems) do
  begin
    if FSubItems[i].Reason<>S then
    begin
      ReasonDraw(R, i);
      //VAppend(FFirstRows, R);
      LineDraw(R, i);
      //VAppend(FLastRows, R-1);
      S:= FSubItems[i].Reason;
    end
    else begin
      //VAppend(FFirstRows, R);
      LineDraw(R, i);
      //VAppend(FLastRows, R-1);
    end;
  end;


  Writer.DrawBorders(R, 1, R, 9, cbtTop);

  ARow:= R - 1;
end;

procedure TSIZCardFrontSheet.SignatureDraw(var ARow: Integer);
var
  i, R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaBottom);

  Writer.WriteText(R, 1, R, 4, 'Ответственное лицо за ведение карточек', cbtNone);
  R:= R + 1;
  Writer.WriteText(R, 1, R, 4, 'учета выдачи СИЗ', cbtNone);
  Writer.WriteText(R, 5, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 9, '', cbtBottom);

  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  R:= R + 1;
  Writer.WriteText(R, 5, R, 6, '(подпись)', cbtNone);
  Writer.WriteText(R, 8, R, 9, '(фамилия, инициалы)', cbtNone);

  for i:= ARow to R do
    Writer.SetRowHeight(i, 16);

  ARow:= R;
end;

procedure TSIZCardFrontSheet.Draw(const ACardNum, AFamily, APersonName, APatronymic,
                                        AGender, ATabNum, APostName, ADepartment: String;
                                  const ACardBD, ACardED: TDate;
                                  const APersonSizes: TSIZStaffSizeIndexes;
                                  const ASubItems: TNormSubItems);
var
  R, KeepRow: Integer;
begin
  if SEmpty(AFamily) then
  begin
    Writer.Clear;
    Exit;
  end;

  FCardNum:= ACardNum;
  FCardBD:= ACardBD;
  FCardED:= ACardED;

  FFamily:= AFamily;
  FPersonName:= APersonName;
  FPatronymic:= APatronymic;
  FGender:= AGender;
  FTabNum:= ATabNum;
  FPostName:= APostName;
  FDepartment:= ADepartment;

  FPersonSizes:= APersonSizes;
  FSubItems:= ASubItems;

  Writer.BeginEdit;

  R:= 1;
  AttachmentDraw(R);
  R:= R + 2;
  TitleDraw(R);
  R:= R + 2;
  KeepRow:= R;
  MainInfoDraw(R);
  MainDataDraw(KeepRow);
  R:= R + 2;
  CaptionDraw(R);
  R:= R + 1;
  GridDraw(R);
  R:= R + 2;
  SignatureDraw(R);


  Writer.EndEdit;
end;

{ TSIZCardBackSheet }

function TSIZCardBackSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    COLUMN1_WIDTH,
    COLUMN2_WIDTH,
    COLUMN3_WIDTH,
    COLUMN4_WIDTH,
    COLUMN5_WIDTH,
    COLUMN6_WIDTH,
    COLUMN7_WIDTH,
    COLUMN8_WIDTH,
    COLUMN9_WIDTH,
    COLUMN10_WIDTH
  ]);
end;

procedure TSIZCardBackSheet.CaptionDraw(var ARow: Integer);
var
  i, R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);

  Writer.WriteText(R, 1, R+1, 1, 'Наименование СИЗ', cbtOuter, True, True);
  Writer.WriteText(R, 2, R+1, 2, 'Модель, марка, артикул,' + SYMBOL_BREAK +
                                 'класс защиты СИЗ,' + SYMBOL_BREAK +
                                 'дерматологических СИЗ', cbtOuter, True, True);

  Writer.WriteText(R, 3, R, 6, 'Выдано', cbtOuter, True, True);
  Writer.WriteText(R, 7, R, 10, 'Возвращено**', cbtOuter, True, True);

  R:= R + 1;
  Writer.WriteText(R, 3, 'дата', cbtOuter, True, True);
  Writer.WriteText(R, 4, 'коли-'+ SYMBOL_BREAK + 'чество', cbtOuter, True, True);
  Writer.WriteText(R, 5, 'лично/'+ SYMBOL_BREAK + 'дозатор*', cbtOuter, True, True);
  Writer.WriteText(R, 6, 'подпись' + SYMBOL_BREAK +
                         'получившего' + SYMBOL_BREAK +
                         'СИЗ', cbtOuter, True, True);

  Writer.WriteText(R, 7, 'дата', cbtOuter, True, True);
  Writer.WriteText(R, 8, 'коли-'+ SYMBOL_BREAK + 'чество', cbtOuter, True, True);
  Writer.WriteText(R, 9, 'подпись' + SYMBOL_BREAK +
                         'сдавшего' + SYMBOL_BREAK +
                         'СИЗ', cbtOuter, True, True);
  Writer.WriteText(R, 10, 'Акт списания' + SYMBOL_BREAK +
                          '(дата, номер)' , cbtOuter, True, True);

  R:= R + 1;
  for i:=1 to 10 do
    Writer.WriteNumber(R, i, i, cbtOuter);

  ARow:= R;
end;

procedure TSIZCardBackSheet.GridDraw(var ARow: Integer);
var
  i, R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  if VIsNil(FItemSIZNames) then
  begin
    for i:=1 to 10 do
      Writer.WriteText(R, i, EmptyStr, cbtOuter);
    Exit;
  end;

  ARow:= R;
end;

procedure TSIZCardBackSheet.NoteDraw(var ARow: Integer);
var
  i, R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaCenter);

  Writer.WriteText(R, 1, R, Writer.ColCount,
                   '* - информация указывается только для ' +
                   'дерматологических СИЗ', cbtRight);
  R:= R + 1;
  Writer.WriteText(R, 1, R, Writer.ColCount,
                   '** - информация указывается для всех СИЗ, кроме ' +
                   'дерматологических СИЗ и СИЗ однократного применения', cbtRight);

  Writer.DrawBorders(ARow, 1, R, Writer.ColCount, cbtOuter);

  for i:= ARow to R do
    Writer.SetRowHeight(i, 16);

  ARow:= R;
end;

procedure TSIZCardBackSheet.Draw(const ANeedDraw: Boolean;
  const AItemSIZNames: TStrVector);
var
  R: Integer;
begin
  if not ANeedDraw then
  begin
    Writer.Clear;
    Exit;
  end;

  FItemSIZNames:= AItemSIZNames;

  Writer.BeginEdit;

  R:= 1;
  CaptionDraw(R);
  R:= R + 1;
  GridDraw(R);
  R:= R + 1;
  NoteDraw(R);

  Writer.EndEdit;
end;

{ TSIZCardStatusSheet }

function TSIZCardStatusSheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt([
    COLUMN1_WIDTH,
    COLUMN2_WIDTH,
    COLUMN3_WIDTH,
    COLUMN4_WIDTH,
    COLUMN5_WIDTH,
    COLUMN6_WIDTH,
    COLUMN7_WIDTH,
    COLUMN8_WIDTH
  ]);
end;

procedure TSIZCardStatusSheet.CaptionDraw(var ARow: Integer);
var
  R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);

  Writer.WriteText(R, 1, 'Перечень СИЗ по нормам', cbtOuter, True, True);
  Writer.WriteText(R, 2, 'Единица' + SYMBOL_BREAK + 'измерения', cbtOuter, True, True);
  Writer.WriteText(R, 3, 'Количество' + SYMBOL_BREAK + 'на период', cbtOuter, True, True);
  Writer.WriteText(R, 4, 'Размер', cbtOuter, True, True);
  Writer.WriteText(R, 5, 'Перечень выданных СИЗ', cbtOuter, True, True);
  Writer.WriteText(R, 6, 'Количество', cbtOuter, True, True);
  Writer.WriteText(R, 7, 'Дата' + SYMBOL_BREAK + 'выдачи', cbtOuter, True, True);
  Writer.WriteText(R, 8, 'Дата' + SYMBOL_BREAK + 'следующей' + SYMBOL_BREAK + 'выдачи', cbtOuter, True, True);

  ARow:= R;
end;

procedure TSIZCardStatusSheet.ReasonDraw(var ARow: Integer;
  const AIndex: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold, fsItalic], clBlack);
  Writer.WriteText(ARow, 1, ARow, Writer.ColCount, FSubItems[AIndex].Reason + ':', cbtOuter, True, True);
  ARow:= ARow + 1;
end;

procedure TSIZCardStatusSheet.NormLineDraw(var ARow: Integer;
  const AIndex: Integer);
var
  i, N: Integer;
  V: TStrVector;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  V:= nil;
  N:= High(FSubItems[AIndex].Info.Names);

  //Наименование СИЗ по нормам
  V:= VCut(FSubItems[AIndex].Info.Names);
  VectorDraw(Writer, V, ARow, 1, 'или', False, haLeft, vaTop);

  //Единица измерения
  V:= VCut(FSubItems[AIndex].Info.Units);
  VectorDraw(Writer, V, ARow, 2, EmptyStr, False, haCenter, vaTop);

  //Количество на период
  VDim(V, N+1);
  for i:=0 to N do
    V[i]:= SIZNumLifeStr(FSubItems[AIndex].Info.Nums[i],
                         FSubItems[AIndex].Info.Lifes[i]);
  VectorDraw(Writer, V, ARow, 3, EmptyStr, False, haCenter, vaTop);

  //Размер
  V:= SIZFullSize(FSubItems[AIndex].Info.SizeTypes,
                  FStatusItems[AIndex].SizeIDs, FStatusItems[AIndex].HeightIDs);
  VectorDraw(Writer, V, ARow, 4, EmptyStr, False, haCenter, vaTop);

  //Границы ячеек
  for i:= 1 to Writer.ColCount do
    Writer.DrawBorders(ARow, i, ARow+2*N, i, cbtOuter);

  ARow:= ARow + 2*N + 1;
end;

procedure TSIZCardStatusSheet.ReceivingLineDraw(var ARow: Integer;
  const AIndex: Integer);
begin

end;

procedure TSIZCardStatusSheet.NormDraw(var ARow: Integer);
var
  i, R: Integer;
  S: String;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  //FFirstRows:= nil;
  //FLastRows:= nil;

  S:= MAIN_REASON;
  for i:=0 to High(FSubItems) do
  begin
    if FSubItems[i].Reason<>S then
    begin
      ReasonDraw(R, i);
      //VAppend(FFirstRows, R);
      NormLineDraw(R, i);
      //VAppend(FLastRows, R-1);
      S:= FSubItems[i].Reason;
    end
    else begin
      //VAppend(FFirstRows, R);
      NormLineDraw(R, i);
      //VAppend(FLastRows, R-1);
    end;
  end;


  Writer.DrawBorders(R, 1, R, Writer.ColCount, cbtTop);

  ARow:= R - 1;

end;

procedure TSIZCardStatusSheet.Draw(const ASubItems: TNormSubItems;
                                   const AStatusItems: TStatusItems);
var
  R: Integer;
begin
  if Length(ASubItems)=0 then
  begin
    Writer.Clear;
    Exit;
  end;

  FSubItems:= ASubItems;
  FStatusItems:= AStatusItems;

  Writer.BeginEdit;

  R:= 1;
  CaptionDraw(R);
  R:= R + 1;
  NormDraw(R);

  Writer.EndEdit;
end;

end.

