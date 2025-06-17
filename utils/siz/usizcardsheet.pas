unit USIZCardSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpstypes,
  //DK packages utils
  DK_SheetTables, DK_SheetTypes, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const,
  DK_SheetWriter,
  //Project utils
  USIZNormTypes, USIZUtils, UConst;

type

  { TSIZCardFrontSheet }

  TSIZCardFrontSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
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
      FSubItems: TNormSubItems;

    procedure AttachmentDraw(var ARow: Integer);
    procedure TitleDraw(var ARow: Integer);
    procedure MainInfoDraw(var ARow: Integer);
    procedure MainDataDraw(const ARow: Integer);
    procedure CaptionDraw(var ARow: Integer);
    procedure GridDraw(var ARow: Integer);
    procedure SignatureDraw(var ARow: Integer);
  public
    procedure Draw(const ASubItems: TNormSubItems);
  end;

  { TSIZCardBackSheet }

  TSIZCardBackSheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
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
    procedure Draw(const AItemSIZNames: TStrVector);
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
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size+1, [], clBlack);

  Writer.WriteText(R, 1, R, Writer.ColCount, 'ЛИЧНАЯ КАРТОЧКА № ___', cbtNone);
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
  Writer.WriteText(R, 1, R, 6, '', cbtBottom);
  Writer.WriteText(R, 8, R, 8, 'СИЗ рук', cbtNone);
  Writer.WriteText(R, 9, R, 9, '', cbtBottom);


  for i:= ARow to R do
    Writer.SetRowHeight(i, 20);

  ARow:= R;
end;

procedure TSIZCardFrontSheet.MainDataDraw(const ARow: Integer);
begin

end;

procedure TSIZCardFrontSheet.CaptionDraw(var ARow: Integer);
var
  R: Integer;
begin
  R:= ARow;
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
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

procedure TSIZCardFrontSheet.GridDraw(var ARow: Integer);
var
  R: Integer;
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

  ARow:= R;
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

procedure TSIZCardFrontSheet.Draw(const ASubItems: TNormSubItems);
var
  R, KeepRow: Integer;
begin
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

procedure TSIZCardBackSheet.Draw(const AItemSIZNames: TStrVector);
var
  R: Integer;
begin
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

end.

