unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector, DK_VSTTables;

  procedure ReSelectTableRow(const AVSTTable: TVSTTable;
                            const AIDVector: TIntVector; const AIDValue: Integer);

implementation

procedure ReSelectTableRow(const AVSTTable: TVSTTable;
                          const AIDVector: TIntVector; const AIDValue: Integer);
var
  Index: Integer;
begin
  if VIsNil(AIDVector) then Exit;
  if AIDValue<=0 then
    Index:= 0
  else begin
    Index:= VIndexOf(AIDVector, AIDValue);
    if Index<0 then Index:= 0;
  end;
  AVSTTable.Select(Index);
end;

end.

