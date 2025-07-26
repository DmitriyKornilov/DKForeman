unit USIZDocSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  fpspreadsheet, fpspreadsheetgrid, fpstypes,
  //DK packages utils
  DK_SheetTypes, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const, DK_SheetWriter,
  DK_SheetConst, DK_Math;

type

  { TSIZDocMB7Sheet }

  TSIZDocMB7Sheet = class(TCustomSheet)
  protected
    function SetWidths: TIntVector; override;
  private
    const
      MINROWS = 1;
      COLWIDTHS: array of Integer = (
        55,  //номер п/п
        130, //ФИО 1
        20,  //ФИО 2
        75,  //таб номер
        15,  //наименование СИЗ 1
        70,  //наименование СИЗ 2
        15,  //наименование СИЗ 3
        125, //наименование СИЗ 4
        30,  //номенкл номер 1
        80,  //номенкл номер 2
        40,  //код ед. изм
        60,  //наимен ед изм 1
        35,  //наимен ед изм 2
        40,  //кол-во 1
        15,  //кол-во 2
        75,  //дата поступления в эксплуатацию 1
        15,  //дата поступления в эксплуатацию 2
        50,  //срок службы 1
        30,  //срок службы 2
        70   //подпись
      );
    var
      FDocNum: String;
      FDocDate: TDate;

      FCompany: String;
      FDepartment: String;

      FIsReturn: Boolean;

      FFIOs, FTabNums, FSIZNames, FNomNums, FSTRUnits, FSIZLifes: TStrVector;
      FDIGUnits, FSIZCounts: TIntVector;

    procedure HeaderDraw(var ARow: Integer);
    procedure CaptionDraw(var ARow: Integer);
    procedure DataDraw(var ARow: Integer);
    procedure FooterDraw(var ARow: Integer);
  public
    procedure Draw(const ACompany, ADepartment, ADocNum: String;
                   const ADocDate: TDate;
                   const AIsReturn: Boolean;
                   const AFIOs, ATabNums, ASIZNames, ANomNums,
                         ASTRUnits, ASIZLifes: TStrVector;
                   const ADIGUnits, ASIZCounts: TIntVector);
  end;

implementation

{ TSIZDocMB7Sheet }

function TSIZDocMB7Sheet.SetWidths: TIntVector;
begin
  Result:= VCreateInt(COLWIDTHS);
end;

procedure TSIZDocMB7Sheet.HeaderDraw(var ARow: Integer);
var
  R, C1, C2: Integer;
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haRight, vaCenter);
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);

  C1:= 1;
  C2:= Writer.ColCount;

  R:= ARow;
  Writer.WriteText(R, C1, R, C2, 'Типовая межотраслевая форма № МБ-7');
  Writer.SetRowHeight(R, 15);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, 'Утверждена постановлением Госкомстата');
  Writer.SetRowHeight(R, 15);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, 'России от 30.10.97 № 71а');
  Writer.SetRowHeight(R, 15);

  Writer.SetFont(Font.Name, Font.Size+2, [fsBold], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, 'ВЕДОМОСТЬ № ' + FDocNum);
  Writer.SetRowHeight(R, 18);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, 'учета выдачи спецодежды, спецобуви и');
  Writer.SetRowHeight(R, 18);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, 'предохранительных приспособлений');
  Writer.SetRowHeight(R, 18);

  C1:= Writer.ColCount-1;
  C2:= Writer.ColCount;
  Writer.SetAlignment(haCenter, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, 'Коды', cbtOuter);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, '0320003', cbtOuter);
  R:= R + 1;
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
  {if not Writer.HasGrid then
  begin
    Writer.SetBorders(lsMedium, scBlack);
    Writer.DrawBorders(R-1, C1, R, C2, cbtAll);
    Writer.SetBordersDefault;
  end; }

  R:= R - 1;
  C1:= 1;
  C2:= Writer.ColCount-2;
  Writer.SetAlignment(haRight, vaCenter);
  Writer.WriteText(R, C1, R, C2, 'Форма по ОКУД');
  R:= R + 1;
  C1:= Writer.ColCount-3;
  C2:= Writer.ColCount-2;
  Writer.WriteText(R, C1, R, C2, 'по ОКПО');

  Writer.SetAlignment(haLeft, vaBottom);
  C1:= 1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R, C2, 'Организация:');
  C1:= C2+1;
  C2:= Writer.ColCount-4;
  Writer.WriteText(R, C1, R, C2, FCompany, cbtBottom, True, True);
  R:= R + 1;
  C1:= 1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R, C2, 'Структурное подразделение:');
  C1:= C2+1;
  C2:= Writer.ColCount-4;
  Writer.WriteText(R, C1, R, C2, FDepartment, cbtBottom, True, True);

  Writer.SetAlignment(haCenter, vaCenter);
  R:= R + 2;
  C1:= Writer.ColCount-9;
  C2:= C1+1;
  Writer.WriteText(R, C1, R, C2, 'Дата составления', cbtOuter, True, True);
  C1:= C2+1;
  C2:= C1+2;
  Writer.WriteText(R, C1, R, C2, 'Код вида операции', cbtOuter, True, True);
  C1:= C2+1;
  C2:= C1+2;
  Writer.WriteText(R, C1, R, C2, 'Структурное подразделение', cbtOuter, True, True);
  C1:= C2+1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R, C2, 'Вид деятельности', cbtOuter, True, True);
  R:= R + 1;
  C1:= Writer.ColCount-9;
  C2:= C1+1;
  Writer.WriteDate(R, C1, R, C2, FDocDate, cbtOuter);
  C1:= C2+1;
  C2:= C1+2;
  Writer.WriteText(R, C1, R, C2, EmptyStr{код вида операции}, cbtOuter, True, True);
  C1:= C2+1;
  C2:= C1+2;
  Writer.WriteText(R, C1, R, C2, FDepartment, cbtOuter, True, True);
  C1:= C2+1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R, C2, EmptyStr{вид деятельности}, cbtOuter, True, True);
  {if not Writer.HasGrid then
  begin
    C1:= Writer.ColCount-9;
    C2:= Writer.ColCount;
    Writer.SetBorders(lsMedium, scBlack, lsThin, scBlack);
    Writer.DrawBorders(R, C1, R, C2, cbtAll);
    Writer.SetBordersDefault;
  end;}

  ARow:= R;
end;

procedure TSIZDocMB7Sheet.CaptionDraw(var ARow: Integer);
var
  R, C1, C2: Integer;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haCenter, vaCenter);

  R:= ARow;
  C1:= 1;
  C2:= 1;
  Writer.WriteText(R, C1, R+1, C2, 'Номер по порядку', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 1, cbtOuter);
  C1:= C2+1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R+1, C2, 'Фамилия, имя, отчество', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 2, cbtOuter);
  C1:= C2+1;
  C2:= C1;
  Writer.WriteText(R, C1, R+1, C2, 'Табельный номер', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 3, cbtOuter);
  C1:= C2+1;
  C2:= C1+5;
  Writer.WriteText(R, C1, R, C2, 'Спецодежда, спецобувь и предохранительные приспособления', cbtOuter, True, True);
  Writer.WriteText(R+1, C1, R+1, C1+3, 'наименование', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C1+3, 4, cbtOuter);
  Writer.WriteText(R+1, C1+4, R+1, C2, 'номенклатурный номер', cbtOuter, True, True);
  Writer.WriteNumber(R+2, C1+4, R+2, C2, 5, cbtOuter);
  C1:= C2+1;
  C2:= C1+2;
  Writer.WriteText(R, C1, R, C2, 'Единица измерения', cbtOuter);
  Writer.WriteText(R+1, C1, R+1, C1, 'код', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C1, 6, cbtOuter);
  Writer.WriteText(R+1, C1+1, R+1, C2, 'наименование', cbtOuter, True, True);
  Writer.WriteNumber(R+2, C1+1, R+2, C2, 7, cbtOuter);
  C1:= C2+1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R+1, C2, 'Коли- чество', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 8, cbtOuter);
  C1:= C2+1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R+1, C2, 'Дата поступления в эксплуатацию', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 9, cbtOuter);
  C1:= C2+1;
  C2:= C1+1;
  Writer.WriteText(R, C1, R+1, C2, 'Срок службы', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 10, cbtOuter);
  C1:= C2+1;
  C2:= C1;
  if FIsReturn then
    Writer.WriteText(R, C1, R+1, C2, 'Подпись в сдаче', cbtOuter)
  else
    Writer.WriteText(R, C1, R+1, C2, 'Подпись в получении', cbtOuter);
  Writer.WriteNumber(R+2, C1, R+2, C2, 11, cbtOuter);

  Writer.SetRepeatedRows(R, R+2);

  Writer.DrawBorders(ARow, 1, R+2, Writer.ColCount, cbtAll);

  ARow:= R + 2;
end;

procedure TSIZDocMB7Sheet.DataDraw(var ARow: Integer);
var
  i, RR: Integer;

  procedure EmptyLine(const AInd: Integer);
  var
    R, C1, C2: Integer;
  begin
    R:= ARow + AInd;
    C1:= 1; C2:= 1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1+5;
    Writer.WriteText(R, C1, R, C1+3, EmptyStr, cbtOuter);
    Writer.WriteText(R, C1+4, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1+2;
    Writer.WriteText(R, C1, R, C1, EmptyStr, cbtOuter);
    Writer.WriteText(R, C1+1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
    C1:= C2+1; C2:= C1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
  end;

  procedure DataLine(AInd: Integer);
  var
    R, C1, C2: Integer;
  begin
    R:= ARow + AInd;
    C1:= 1; C2:= 1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteNumber(R, C1, R, C2, AInd+1, cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.WriteText(R, C1, R, C2, FFIOs[AInd], cbtOuter, True, True);
    C1:= C2+1; C2:= C1;
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, C1, R, C2, FTabNums[AInd], cbtOuter);
    C1:= C2+1; C2:= C1+5;
    Writer.SetAlignment(haLeft, vaCenter);
    Writer.WriteText(R, C1, R, C1+3, FSIZNames[AInd], cbtOuter, True, True);
    Writer.SetAlignment(haCenter, vaCenter);
    Writer.WriteText(R, C1+4, R, C2, FNomNums[AInd], cbtOuter);
    C1:= C2+1; C2:= C1+2;
    Writer.WriteNumber(R, C1, R, C1, FDIGUnits[AInd], cbtOuter);
    Writer.WriteText(R, C1+1, R, C2, FSTRUnits[AInd], cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.WriteNumber(R, C1, R, C2, FSIZCounts[AInd], cbtOuter);
    C1:= C2+1; C2:= C1+1;
    if FIsReturn then
      Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter)
    else
      Writer.WriteDate(R, C1, R, C2, FDocDate, cbtOuter);
    C1:= C2+1; C2:= C1+1;
    Writer.WriteText(R, C1, R, C2, FSIZLifes[AInd], cbtOuter);
    C1:= C2+1; C2:= C1;
    Writer.WriteText(R, C1, R, C2, EmptyStr, cbtOuter);
  end;

begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  for i:= 0 to High(FFIOs) do
    DataLine(i);
  for i:=Length(FFIOs) to MINROWS-1 do
    EmptyLine(i);

  RR:= ARow + Max(Length(FFIOs), MINROWS) - 1;
  {if not Writer.HasGrid then
  begin
    Writer.SetBorders(lsMedium, scBlack, lsThin, scBlack);
    Writer.DrawBorders(ARow, 4, RR, 4, cbtAll);
    Writer.DrawBorders(ARow, 9, RR, 11, cbtAll);
    Writer.DrawBorders(ARow, 14, RR, 19, cbtAll);
    Writer.SetBordersDefault;
  end;  }

  ARow:= RR;
end;

procedure TSIZDocMB7Sheet.FooterDraw(var ARow: Integer);
var
  R, C1, C2: Integer;
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  R:= ARow;

  C1:= 1;
  C2:= 3;
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, C1, R, C2, 'Материально ответственное лицо', cbtNone, True, True);
  C1:= 4;
  C2:= 4;
  Writer.SetAlignment(haCenter, vaBottom);
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtBottom);
  C1:= 6;
  C2:= 6;
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtBottom);
  C1:= 8;
  C2:= 8;
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtBottom);
  C1:= 10;
  C2:= 12;
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, C1, R, C2, 'Руководитель подразделения', cbtNone, True, True);
  C1:= 13;
  C2:= 14;
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtBottom);
  C1:= 16;
  C2:= 16;
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtBottom);
  C1:= 18;
  C2:= 20;
  Writer.WriteText(R, C1, R, C2, EmptyStr, cbtBottom);

  R:= R+1;
  Writer.SetFont(Font.Name, Font.Size-2, [], clBlack);
  Writer.SetAlignment(haCenter, vaTop);
  C1:= 4;
  C2:= 4;
  Writer.WriteText(R, C1, R, C2, '(должность)');
  C1:= 6;
  C2:= 6;
  Writer.WriteText(R, C1, R, C2, '(подпись)');
  C1:= 8;
  C2:= 8;
  Writer.WriteText(R, C1, R, C2, '(расшифровка подписи)');
  C1:= 13;
  C2:= 14;
  Writer.WriteText(R, C1, R, C2, '(должность)');
  C1:= 16;
  C2:= 16;
  Writer.WriteText(R, C1, R, C2, '(подпись)');
  C1:= 18;
  C2:= 20;
  Writer.WriteText(R, C1, R, C2, '(расшифровка подписи)');

  R:= R+1;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  C1:= 10;
  C2:= 20;
  Writer.SetAlignment(haLeft, vaBottom);
  Writer.WriteText(R, C1, R, C2, '"_____" ____________________ 20____ г.');

  ARow:= R;
end;

procedure TSIZDocMB7Sheet.Draw(const ACompany, ADepartment, ADocNum: String;
                   const ADocDate: TDate;
                   const AIsReturn: Boolean;
                   const AFIOs, ATabNums, ASIZNames, ANomNums,
                         ASTRUnits, ASIZLifes: TStrVector;
                   const ADIGUnits, ASIZCounts: TIntVector);
var
  R: Integer;
begin
  FCompany:= ACompany;
  FDepartment:= ADepartment;
  FDocNum:= ADocNum;
  FDocDate:= ADocDate;
  FIsReturn:= AIsReturn;

  FFIOs:= AFIOs;
  FTabNums:= ATabNums;
  FSIZNames:= ASIZNames;
  FNomNums:= ANomNums;
  FSTRUnits:= ASTRUnits;
  FSIZLifes:= ASIZLifes;
  FDIGUnits:= ADIGUnits;
  FSIZCounts:= ASIZCounts;

  Writer.BeginEdit;

  R:= 1;
  HeaderDraw(R);
  R:= R + 2;
  CaptionDraw(R);
  R:= R + 1;
  DataDraw(R);
  R:= R + 2;
  FooterDraw(R);

  Writer.EndEdit;
end;

end.

