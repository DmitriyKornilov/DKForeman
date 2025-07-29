unit USIZStoreTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, VirtualTrees,
  //DK packages utils
  DK_Vector, DK_VSTCategoryTables, DK_VSTTypes;

type

  { TSIZStoreHistoryTable }

  TSIZStoreHistoryTable = class (TVSTCategoryCoreTable)
  protected
    FHistoryItemFont: TFont;
    FHistoryItems: TStrVector;
    procedure CellFont(Node: PVirtualNode; {%H-}Column: TColumnIndex); override;
    procedure SetHistoryItemFont(AValue: TFont);
    procedure SetHistoryItems(const AValues: TStrVector);
  public
    constructor Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
    destructor Destroy; override;

    procedure SetSingleFont(const AFont: TFont); override;
    property HistoryItemFont: TFont read FHistoryItemFont write SetHistoryItemFont;
    property HistoryItems: TStrVector read FHistoryItems write SetHistoryItems;
  end;

implementation

{ TSIZStoreHistoryTable }

procedure TSIZStoreHistoryTable.CellFont(Node: PVirtualNode; Column: TColumnIndex);
var
  i, j: Integer;
begin
  inherited CellFont(Node, Column);
  if FTree.GetNodeLevel(Node)=1 then
  begin
    i:= (Node^.Parent)^.Index;
    j:= Node^.Index;
    if VIndexOf(FHistoryItems, FDataValues[0, i, j])>=0 then
      FCellFont.Assign(FHistoryItemFont);
  end;
end;

procedure TSIZStoreHistoryTable.SetHistoryItemFont(AValue: TFont);
begin
  FHistoryItemFont.Assign(AValue);
  FTree.Refresh;
end;

procedure TSIZStoreHistoryTable.SetHistoryItems(const AValues: TStrVector);
begin
  FHistoryItems:= VCut(AValues);
end;

constructor TSIZStoreHistoryTable.Create(const ATree: TVirtualStringTree;
                       const AHeaderHeight: Integer = ROW_HEIGHT_DEFAULT;
                       const ARowHeight: Integer = ROW_HEIGHT_DEFAULT);
begin
  inherited Create(ATree, AHeaderHeight, ARowHeight);

  FHistoryItemFont:= TFont.Create;
  FHistoryItemFont.Assign(FTree.Font);
end;

destructor TSIZStoreHistoryTable.Destroy;
begin
  FreeAndNil(FHistoryItemFont);
  inherited Destroy;
end;

procedure TSIZStoreHistoryTable.SetSingleFont(const AFont: TFont);
begin
  inherited SetSingleFont(AFont);
  HistoryItemFont:= AFont;
end;

end.

