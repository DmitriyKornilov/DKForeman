unit UBriefingSheet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  fpspreadsheet, fpspreadsheetgrid, fpstypes,
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
    procedure CaptionDraw(var ARow: Integer);
    procedure ItemDraw(var ARow: Integer; const AIndex: Integer);
  public
    procedure Draw();
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
  Writer.WriteText(R, 2, 'Наименование', cbtOuter);
  Writer.WriteText(R, 3, 'Кому проводится', cbtOuter);
  Writer.WriteText(R, 4, 'Периодичность', cbtOuter);
  Writer.WriteText(R, 5, 'Провести до', cbtOuter);
  Writer.WriteText(R, 6, 'Примечание', cbtOuter);

end;

procedure TBriefingMainListSheet.ItemDraw(var ARow: Integer; const AIndex: Integer);
begin

end;

procedure TBriefingMainListSheet.Draw();
var
  i, R, CaptionRowCount: Integer;
begin
  Unselect;

  FFirstRows:= nil;
  FLastRows:= nil;

  Writer.BeginEdit;

  R:= 1;
  CaptionDraw(R);
  CaptionRowCount:= R;

  //for i:= 0 to High() do
  //begin
  //  R:= R + 1;
  //  ItemDraw(R, i);
  //end;

  Writer.SetFrozenRows(CaptionRowCount);
  //Writer.SetRepeatedRows(CaptionRowCount-2, CaptionRowCount);

  Writer.EndEdit;
end;

end.

