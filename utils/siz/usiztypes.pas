unit USIZTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector;

type
  TNormSubItem = record
    SubItemID: Integer;
    OrderNum : Integer;
    ReasonID : Integer;
    Reason   : String;
    InfoIDs  : TIntVector;
    SizeTypes: TIntVector;
    Names    : TStrVector;
    Units    : TStrVector;
    Lifes    : TStrVector;
  end;
  procedure NormSubItemNew(var ASubItem: TNormSubItem;
                           const ASubItemID, AOrderNum, AReasonID: Integer; const AReason: String);
  procedure NormSubItemClear(var ASubItem: TNormSubItem);
  procedure NormSubItemAdd(var ASubItem: TNormSubItem; const AInfoID, ASizeType: Integer;
                           const AName, AUnit, ALife: String);
  procedure NormSubItemDel(var ASubItem: TNormSubItem; const AIndex: Integer);
  procedure NormSubItemSwap(var ASubItem: TNormSubItem; const AIndex1, AIndex2: Integer);
  procedure NormSubItemCopy(const ASourceSubItem: TNormSubItem; var ADestinationSubItem: TNormSubItem);

type
  TNormSubItems = array of TNormSubItem;
  procedure NormSubItemsClear(var ASubItems: TNormSubItems);
  procedure NormSubItemsAdd(var ASubItems: TNormSubItems; const ASubItem: TNormSubItem);
  procedure NormSubItemsDel(var ASubItems: TNormSubItems; const AIndex1: Integer; AIndex2: Integer=-1);
  procedure NormSubItemsSwap(var ASubItems: TNormSubItems; const AIndex1, AIndex2: Integer);
  function  NormSubItemsIndexOf(const ASubItems: TNormSubItems; const ASubItemID: Integer): Integer;

type
  TNormSubItemInfo = record
    InfoIDs    : TIntVector;
    NameIDs    : TIntVector;
    Nums       : TIntVector;
    Lifes      : TIntVector;
    SpecLifeIDs: TIntVector;
    OrderNums  : TIntVector;
  end;
  procedure NormSubItemInfoClear(var ASubItemInfo: TNormSubItemInfo);
  procedure NormSubItemInfoAdd(var ASubItemInfo: TNormSubItemInfo;
                           const AInfoID, ANameID, ANum, ALife, ASpecLifeID, AOrderNum: Integer);
  procedure NormSubItemInfoCut(var ASubItemInfo: TNormSubItemInfo;
                               const AInfoIDs, ANameIDs, ANums, ALifes, ASpecLifeIDs, AOrderNums: TIntVector);
  procedure NormSubItemInfoIns(var ASubItemInfo: TNormSubItemInfo;
                           const AInfoID, ANameID, ANum, ALife, ASpecLifeID, AOrderNum, AIndex: Integer);
  procedure NormSubItemInfoSwap(var ASubItemInfo: TNormSubItemInfo; const AIndex1, AIndex2: Integer);
  procedure NormSubItemInfoDel(var ASubItemInfo: TNormSubItemInfo; const AIndex: Integer);


implementation

//--- TNormSubItem------------------------------------------------------------

procedure NormSubItemNew(var ASubItem: TNormSubItem;
                         const ASubItemID, AOrderNum, AReasonID: Integer; const AReason: String);
begin
  ASubItem.SubItemID:= ASubItemID;
  ASubItem.OrderNum:= AOrderNum;
  ASubItem.ReasonID:= AReasonID;
  ASubItem.Reason:= AReason;
  ASubItem.InfoIDs:= nil;
  ASubItem.SizeTypes:= nil;
  ASubItem.Names:= nil;
  ASubItem.Units:= nil;
  ASubItem.Lifes:= nil;
end;

procedure NormSubItemClear(var ASubItem: TNormSubItem);
begin
  NormSubItemNew(ASubItem, -1, -1, 0, EmptyStr);
end;

procedure NormSubItemAdd(var ASubItem: TNormSubItem; const AInfoID, ASizeType: Integer;
                         const AName, AUnit, ALife: String);
begin
  VAppend(ASubItem.InfoIDs, AInfoID);
  VAppend(ASubItem.SizeTypes, ASizeType);
  VAppend(ASubItem.Names, AName);
  VAppend(ASubItem.Units, AUnit);
  VAppend(ASubItem.Lifes, ALife);
end;

procedure NormSubItemDel(var ASubItem: TNormSubItem; const AIndex: Integer);
begin
  VDel(ASubItem.InfoIDs, AIndex);
  VDel(ASubItem.SizeTypes, AIndex);
  VDel(ASubItem.Names, AIndex);
  VDel(ASubItem.Units, AIndex);
  VDel(ASubItem.Lifes, AIndex);
end;

procedure NormSubItemSwap(var ASubItem: TNormSubItem; const AIndex1, AIndex2: Integer);
begin
  VSwap(ASubItem.InfoIDs, AIndex1, AIndex2);
  VSwap(ASubItem.SizeTypes, AIndex1, AIndex2);
  VSwap(ASubItem.Names, AIndex1, AIndex2);
  VSwap(ASubItem.Units, AIndex1, AIndex2);
  VSwap(ASubItem.Lifes, AIndex1, AIndex2);
end;

procedure NormSubItemCopy(const ASourceSubItem: TNormSubItem; var ADestinationSubItem: TNormSubItem);
begin
  ADestinationSubItem.SubItemID:= ASourceSubItem.SubItemID;
  ADestinationSubItem.OrderNum:= ASourceSubItem.OrderNum;
  ADestinationSubItem.ReasonID:= ASourceSubItem.ReasonID;
  ADestinationSubItem.Reason:= ASourceSubItem.Reason;
  ADestinationSubItem.InfoIDs:= VCut(ASourceSubItem.InfoIDs);
  ADestinationSubItem.SizeTypes:= VCut(ASourceSubItem.SizeTypes);
  ADestinationSubItem.Names:= VCut(ASourceSubItem.Names);
  ADestinationSubItem.Units:= VCut(ASourceSubItem.Units);
  ADestinationSubItem.Lifes:= VCut(ASourceSubItem.Lifes);
end;

//--- TNormSubItemS------------------------------------------------------------

procedure NormSubItemsClear(var ASubItems: TNormSubItems);
var
  i: Integer;
begin
  for i:= 0 to High(ASubItems) do
    NormSubItemClear(ASubItems[i]);
  ASubItems:= nil;
end;

procedure NormSubItemsAdd(var ASubItems: TNormSubItems; const ASubItem: TNormSubItem);
var
  N: Integer;
begin
  N:= Length(ASubItems);
  SetLength(ASubItems, N+1);
  NormSubItemCopy(ASubItem, ASubItems[N]);
end;

procedure NormSubItemsDel(var ASubItems: TNormSubItems; const AIndex1: Integer; AIndex2: Integer=-1);
var
  i, OldSize, DelLength: Integer;
begin
  OldSize:= Length(ASubItems);
  if OldSize=0 then Exit;
  if AIndex2<AIndex1 then AIndex2:= AIndex1;
  DelLength:= AIndex2 - AIndex1 + 1;
  if OldSize=DelLength then
  begin
    NormSubItemsClear(ASubItems);
    Exit;
  end;
  for i := AIndex2+1 to OldSize-1 do
    ASubItems[i-DelLength]:= ASubItems[i];
  SetLength(ASubItems, OldSize - DelLength);
end;

procedure NormSubItemsSwap(var ASubItems: TNormSubItems; const AIndex1, AIndex2: Integer);
var
  TmpValue: TNormSubItem;
  OrderNum1, OrderNum2: Integer;
begin
  OrderNum1:= ASubItems[AIndex1].OrderNum;
  OrderNum2:= ASubItems[AIndex2].OrderNum;
  //все значения меняем
  TmpValue:= ASubItems[AIndex1];
  ASubItems[AIndex1]:= ASubItems[AIndex2];
  ASubItems[AIndex2]:= TmpValue;
  //кроме №п/п
  ASubItems[AIndex1].OrderNum:= OrderNum1;
  ASubItems[AIndex2].OrderNum:= OrderNum2;
end;

function  NormSubItemsIndexOf(const ASubItems: TNormSubItems; const ASubItemID: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to High(ASubItems) do
  begin
    if ASubItems[i].SubItemID=ASubItemID then
    begin
      Result:= i;
      break;
    end;
  end;
end;

//--- TNormSubItemInfo ------------------------------------------------------------

procedure NormSubItemInfoClear(var ASubItemInfo: TNormSubItemInfo);
begin
  ASubItemInfo.InfoIDs:= nil;
  ASubItemInfo.NameIDs:= nil;
  ASubItemInfo.Nums:= nil;
  ASubItemInfo.Lifes:= nil;
  ASubItemInfo.SpecLifeIDs:= nil;
  ASubItemInfo.OrderNums:= nil;
end;

procedure NormSubItemInfoAdd(var ASubItemInfo: TNormSubItemInfo;
                        const AInfoID, ANameID, ANum, ALife, ASpecLifeID, AOrderNum: Integer);
begin
  VAppend(ASubItemInfo.InfoIDs, AInfoID);
  VAppend(ASubItemInfo.NameIDs, ANameID);
  VAppend(ASubItemInfo.Nums, ANum);
  VAppend(ASubItemInfo.Lifes, ALife);
  VAppend(ASubItemInfo.SpecLifeIDs, ASpecLifeID);
  VAppend(ASubItemInfo.OrderNums, AOrderNum);
end;

procedure NormSubItemInfoCut(var ASubItemInfo: TNormSubItemInfo;
                               const AInfoIDs, ANameIDs, ANums, ALifes, ASpecLifeIDs, AOrderNums: TIntVector);
begin
  ASubItemInfo.InfoIDs:= VCut(AInfoIDs);
  ASubItemInfo.NameIDs:= VCut(ANameIDs);
  ASubItemInfo.Nums:= VCut(ANums);
  ASubItemInfo.Lifes:= VCut(ALifes);
  ASubItemInfo.SpecLifeIDs:= VCut(ASpecLifeIDs);
  ASubItemInfo.OrderNums:= VCut(AOrderNums);
end;

procedure NormSubItemInfoIns(var ASubItemInfo: TNormSubItemInfo;
                           const AInfoID, ANameID, ANum, ALife, ASpecLifeID, AOrderNum, AIndex: Integer);
begin
  VIns(ASubItemInfo.InfoIDs, AIndex, AInfoID);
  VIns(ASubItemInfo.NameIDs, AIndex, ANameID);
  VIns(ASubItemInfo.Nums, AIndex, ANum);
  VIns(ASubItemInfo.Lifes, AIndex, ALife);
  VIns(ASubItemInfo.SpecLifeIDs, AIndex, ASpecLifeID);
  VIns(ASubItemInfo.OrderNums, AIndex, AOrderNum);
end;

procedure NormSubItemInfoSwap(var ASubItemInfo: TNormSubItemInfo; const AIndex1, AIndex2: Integer);
begin
  VSwap(ASubItemInfo.InfoIDs, AIndex1, AIndex2);
  VSwap(ASubItemInfo.NameIDs, AIndex1, AIndex2);
  VSwap(ASubItemInfo.Nums, AIndex1, AIndex2);
  VSwap(ASubItemInfo.Lifes, AIndex1, AIndex2);
  VSwap(ASubItemInfo.SpecLifeIDs, AIndex1, AIndex2);
  VSwap(ASubItemInfo.OrderNums, AIndex1, AIndex2);
end;

procedure NormSubItemInfoDel(var ASubItemInfo: TNormSubItemInfo; const AIndex: Integer);
begin
  VDel(ASubItemInfo.InfoIDs, AIndex);
  VDel(ASubItemInfo.NameIDs, AIndex);
  VDel(ASubItemInfo.Nums, AIndex);
  VDel(ASubItemInfo.Lifes, AIndex);
  VDel(ASubItemInfo.SpecLifeIDs, AIndex);
  VDel(ASubItemInfo.OrderNums, AIndex);
end;

end.

