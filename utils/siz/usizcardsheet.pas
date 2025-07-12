unit USIZCardSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, fpstypes, DateUtils, Controls,
  fpspreadsheet, fpspreadsheetgrid,
  //DK packages utils
  DK_SheetTypes, DK_Vector, DK_StrUtils, DK_Const, DK_SheetWriter,
  DK_SheetConst, DK_DateUtils,
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
      FOnSelect: TSheetEvent;
      FCanSelect: Boolean;
      FSelectedSubItemIndex: Integer;
      FSelectedInfoIndex: Integer;
      FSelectedStatusInfoIndex: Integer;

      FSubItems: TNormSubItems;
      FStatusItems: TStatusItems;
      FWarnDaysCount: Integer;

      FInfoFirstRows: TIntVector; //первая строка начала NormSubItem.Info.InfoID
      FInfoLastRows: TIntVector;  //последняя строка NormSubItem.Info.InfoID
      FSubItemIndexes: TIntVector;
      FInfoIndexes: TIntVector;
      FIsReceivingExists: TBoolVector;

    procedure CaptionDraw(var ARow: Integer);
    procedure OrDraw(const ARow: Integer;const ASubItemIndex: Integer);
    procedure InfoDraw(var ARow: Integer; const ASubItemIndex, AInfoIndex: Integer);
    procedure ReasonDraw(const ARow: Integer; const ASubItemIndex: Integer);
    procedure SubItemDraw(var ARow: Integer; const ASubItemIndex: Integer);

    procedure MouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);

    function IsCellSelectable(const ARow, ACol: Integer): Boolean;
    procedure SetSelection(const ARow, ACol: Integer; const ADoEvent: Boolean = True);
    procedure DelSelection(const ADoEvent: Boolean = True);
    procedure Select(const ARow, ACol: Integer);
    procedure Unselect;

    procedure SetCanSelect(const AValue: Boolean);
    function GetIsSelected: Boolean;
    function GetIsNormInfoSelected: Boolean;
    function GetIsStatusInfoSelected: Boolean;
  public
    constructor Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const ARowHeightDefault: Integer = ROW_HEIGHT_DEFAULT);
    procedure Draw(const ASubItems: TNormSubItems;
                   const AStatusItems: TStatusItems;
                   const AWarnDaysCount: Integer);

    property CanSelect: Boolean read FCanSelect write SetCanSelect;
    property IsSelected: Boolean read GetIsSelected;
    property IsNormInfoSelected: Boolean read GetIsNormInfoSelected;
    property IsStatusInfoSelected: Boolean read GetIsStatusInfoSelected;
    property SelectedSubItemIndex: Integer read FSelectedSubItemIndex;
    property SelectedInfoIndex: Integer read FSelectedInfoIndex;
    property SelectedStatusInfoIndex: Integer read FSelectedStatusInfoIndex;
    property OnSelect: TSheetEvent read FOnSelect write FOnSelect;
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

function TSIZCardStatusSheet.GetIsSelected: Boolean;
begin
  Result:= IsNormInfoSelected;
end;

function TSIZCardStatusSheet.GetIsStatusInfoSelected: Boolean;
begin
  Result:= IsNormInfoSelected and (FSelectedStatusInfoIndex>=0);
end;

function TSIZCardStatusSheet.GetIsNormInfoSelected: Boolean;
begin
  Result:= (FSelectedSubItemIndex>=0) and (FSelectedInfoIndex>=0);
end;

procedure TSIZCardStatusSheet.OrDraw(const ARow: Integer; const ASubItemIndex: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);
  Writer.SetAlignment(haLeft, vaTop);
  Writer.WriteText(ARow, 1, ARow, 1, 'или', cbtNone);

  if not FStatusItems[ASubItemIndex].Info.IsFreshExists then
    Writer.AddCellBGColorIndex(ARow, 8, COLOR_INDEX_SIZSTATUS_ERROR);
end;

procedure TSIZCardStatusSheet.InfoDraw(var ARow: Integer;
                                       const ASubItemIndex, AInfoIndex: Integer);
var
  i, n, k, R1, R2: Integer;
  S: String;
  D: TDate;
begin
  R1:= ARow;
  R2:= R1;
  n:= Length(FStatusItems[ASubItemIndex].Info.LogIDs[AInfoIndex]);
  if n>0 then
    R2:= ARow + n - 1;
  VAppend(FInfoFirstRows, R1);
  VAppend(FInfoLastRows, R2);
  VAppend(FSubItemIndexes, ASubItemIndex);
  VAppend(FInfoIndexes, AInfoIndex);
  VAppend(FIsReceivingExists, n>0);
  ARow:= R2;

  Writer.SetBackgroundDefault;
  Writer.SetFont(Font.Name, Font.Size, [], clBlack);

  Writer.SetAlignment(haLeft, vaTop);
  S:= FSubItems[ASubItemIndex].Info.Names[AInfoIndex];
  Writer.WriteText(R1, 1, R2, 1, S, cbtNone, True, True);

  Writer.SetAlignment(haCenter, vaTop);
  S:= FSubItems[ASubItemIndex].Info.Units[AInfoIndex];
  Writer.WriteText(R1, 2, R2, 2, S, cbtNone);

  S:= SIZNumLifeStr(FSubItems[ASubItemIndex].Info.Nums[AInfoIndex],
                    FSubItems[ASubItemIndex].Info.Lifes[AInfoIndex]);
  Writer.WriteText(R1, 3, R2, 3, S, cbtNone);

  if (FSubItems[ASubItemIndex].Info.SIZTypes[AInfoIndex]=SIZ_TYPE_KEYS[0]) or
     (FSubItems[ASubItemIndex].Info.SizeTypes[AInfoIndex]=SIZ_SIZETYPE_KEYS[0]) then
    S:= EMPTY_MARK
  else
    S:= SIZFullSize(FSubItems[ASubItemIndex].Info.SizeTypes[AInfoIndex],
                    FStatusItems[ASubItemIndex].SizeIDs[AInfoIndex],
                    FStatusItems[ASubItemIndex].HeightIDs[AInfoIndex]);
  Writer.WriteText(R1, 4, R2, 4, S, cbtNone);

  if n=0 then
  begin
    Writer.AddCellBGColorIndex(R1, 8, COLOR_INDEX_SIZSTATUS_ERROR);
    Exit;
  end;

  for i:=0 to n-1 do
  begin
    Writer.SetAlignment(haLeft, vaTop);
    S:= FStatusItems[ASubItemIndex].Info.SizNames[AInfoIndex, i];
    Writer.WriteText(R1+i, 5, S, cbtNone, True, True);

    Writer.SetAlignment(haCenter, vaTop);
    k:= FStatusItems[ASubItemIndex].Info.SizCounts[AInfoIndex, i];
    Writer.WriteNumber(R1+i, 6, k, cbtNone);

    D:= FStatusItems[ASubItemIndex].Info.ReceivingDates[AInfoIndex, i];
    Writer.WriteDate(R1+i, 7, D, cbtNone);

    D:= FStatusItems[ASubItemIndex].Info.WriteoffDates[AInfoIndex, i];
    if SameDate(D, INFDATE) then
      Writer.WriteText(R1+i, 8, EMPTY_MARK, cbtNone)
    else begin
      Writer.WriteDate(R1+i, 8, D, cbtNone);

      if not FStatusItems[ASubItemIndex].Info.IsFreshExists then
        Writer.AddCellBGColorIndex(R1+i, 8, COLOR_INDEX_SIZSTATUS_ERROR)
      else if DaysBetweenDates(Date, D)<=FWarnDaysCount then
        Writer.AddCellBGColorIndex(R1+i, 8, COLOR_INDEX_SIZSTATUS_WARN);
    end;
  end;

end;

procedure TSIZCardStatusSheet.ReasonDraw(const ARow: Integer; const ASubItemIndex: Integer);
begin
  Writer.SetBackgroundDefault;
  Writer.SetAlignment(haLeft, vaCenter);
  Writer.SetFont(Font.Name, Font.Size, [fsBold, fsItalic], clBlack);
  Writer.WriteText(ARow, 1, ARow, Writer.ColCount,
                   FSubItems[ASubItemIndex].Reason + ':', cbtOuter, True, True);
end;

procedure TSIZCardStatusSheet.SetCanSelect(const AValue: Boolean);
begin
  if FCanSelect=AValue then Exit;
  if not AValue then Unselect;
  FCanSelect:=AValue;
end;

procedure TSIZCardStatusSheet.SubItemDraw(var ARow: Integer; const ASubItemIndex: Integer);
var
  i, R: Integer;
begin
  R:= ARow - 1;
  for i:=0 to High(FSubItems[ASubItemIndex].Info.InfoIDs) do
  begin
    R:= R + 1;
    if i>0 then
    begin
      OrDraw(R, ASubItemIndex);
      R:= R + 1;
    end;
    InfoDraw(R, ASubItemIndex, i);
  end;

  for i:= 1 to  Writer.ColCount do
    Writer.DrawBorders(ARow, i, R, i, cbtOuter);

  ARow:= R;
end;

procedure TSIZCardStatusSheet.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R, C: Integer;
begin
  if Button=mbLeft then
  begin
    (Sender as TsWorksheetGrid).MouseToCell(X, Y, C, R);
    SetSelection(R, C);
  end
  else if Button=mbRight then
  begin
    DelSelection;
  end;
end;

function TSIZCardStatusSheet.IsCellSelectable(const ARow, ACol: Integer): Boolean;
begin
  Result:= ((ACol>=1) and (ACol<=Writer.ColCount)) and
           (VIndexOf(FInfoFirstRows, FInfoLastRows, ARow)>=0);
end;

procedure TSIZCardStatusSheet.SetSelection(const ARow, ACol: Integer;
  const ADoEvent: Boolean);
begin
  if not CanSelect then Exit;
  if not IsCellSelectable(ARow, ACol) then Exit;

  if IsSelected then Unselect;
  Select(ARow, ACol);
  if ADoEvent and Assigned(FOnSelect) then FOnSelect;
end;

procedure TSIZCardStatusSheet.DelSelection(const ADoEvent: Boolean);
begin
  if not IsSelected then Exit;
  Unselect;
  if ADoEvent and Assigned(FOnSelect) then FOnSelect;
end;

procedure TSIZCardStatusSheet.Select(const ARow, ACol: Integer);
var
  i, j, k: Integer;
begin
  k:= VIndexOf(FInfoFirstRows, FInfoLastRows, ARow);
  FSelectedSubItemIndex:= FSubItemIndexes[k];
  FSelectedInfoIndex:= FInfoIndexes[k];
  for i:= FInfoFirstRows[k] to FInfoLastRows[k] do
    for j:= 1 to Writer.ColCount do
      SelectionAddCell(i, j);

  if FIsReceivingExists[k] and (ACol>=5) then
  begin
    FSelectedStatusInfoIndex:= ARow - FInfoFirstRows[k];
    for i:= FInfoFirstRows[k] to FInfoLastRows[k] do
      for j:= 5 to Writer.ColCount do
        SelectionExtraAddCell(i, j);
  end;
end;

procedure TSIZCardStatusSheet.Unselect;
begin
  FSelectedSubItemIndex:= -1;
  FSelectedInfoIndex:= -1;
  FSelectedStatusInfoIndex:= -1;
  SelectionExtraClear;
  SelectionClear;
end;

constructor TSIZCardStatusSheet.Create(const AWorksheet: TsWorksheet;
                       const AGrid: TsWorksheetGrid;
                       const AFont: TFont;
                       const ARowHeightDefault: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(AWorksheet, AGrid, AFont, ARowHeightDefault);
  if Assigned(AGrid) then
    Writer.Grid.OnMouseDown:= @MouseDown;
  FCanSelect:= False;
end;

procedure TSIZCardStatusSheet.Draw(const ASubItems: TNormSubItems;
                                   const AStatusItems: TStatusItems;
                                   const AWarnDaysCount: Integer);
var
  i, R: Integer;
  S: String;
begin
  if Length(ASubItems)=0 then
  begin
    Writer.Clear;
    Exit;
  end;

  FSubItems:= ASubItems;
  FStatusItems:= AStatusItems;
  FWarnDaysCount:= AWarnDaysCount;

  FInfoFirstRows:= nil;
  FInfoLastRows:= nil;
  FSubItemIndexes:= nil;
  FInfoIndexes:= nil;
  FIsReceivingExists:= nil;

  Writer.BeginEdit;

  R:= 1;
  CaptionDraw(R);

  S:= MAIN_REASON;
  for i:=0 to High(FSubItems) do
  begin
    R:= R + 1;
    if FSubItems[i].Reason<>S then
    begin
      ReasonDraw(R, i);
      S:= FSubItems[i].Reason;
      R:= R + 1;
    end;
    SubItemDraw(R, i);
  end;

  R:= R + 1;
  Writer.DrawBorders(R, 1, R, Writer.ColCount, cbtTop);

  Writer.EndEdit;

  ColorsUpdate(COLORS_SIZSTATUS);
end;

end.

