unit USIZTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector;

type
  TNormSubItemInfo = record
    InfoIDs    : TIntVector;
    OrderNums  : TIntVector;
    //СИЗ
    ClassIDs   : TIntVector;
    NameIDs    : TIntVector;
    Names      : TStrVector;
    Units      : TStrVector;   //str code
    SizeTypes  : TIntVector;
    //норма
    Nums       : TIntVector;   //кол-во
    LifeIDs    : TIntVector;   //ID особых сроков (=0 if Life>0)
    Lifes      : TIntVector;   //месяцы (=0 if LifeID>0)
    LifeNames  : TStrVector;   //особый срок службы
  end;

  procedure NormSubItemInfoClear(var AInfo: TNormSubItemInfo);
  procedure NormSubItemInfoAdd(var AInfo: TNormSubItemInfo;
                               const AInfoID, AOrderNum,
                                     AClassID, ANameID, ASizeType,
                                     ANum, ALifeID, ALife : Integer;
                               const AName, AUnit, ALifeName: String);
  procedure NormSubItemInfoSwap(var AInfo: TNormSubItemInfo;
                               const AIndex1, AIndex2: Integer);
  procedure NormSubItemInfoDel(var AInfo: TNormSubItemInfo;
                               const AIndex: Integer);
  procedure NormSubItemInfoCopy(const ASourceInfo: TNormSubItemInfo;
                                var ADestInfo: TNormSubItemInfo);

type
  TNormSubItem = record
    SubItemID: Integer;
    OrderNum : Integer;
    ReasonID : Integer;
    Reason   : String;
    Info     : TNormSubItemInfo;
  end;

  procedure NormSubItemNew(var ASubItem: TNormSubItem;
                           const ASubItemID, AOrderNum, AReasonID: Integer;
                           const AReason: String);
  procedure NormSubItemClear(var ASubItem: TNormSubItem);
  procedure NormSubItemCopy(const ASourceSubItem: TNormSubItem;
                            var ADestSubItem: TNormSubItem);

type
  TNormSubItems = array of TNormSubItem;

  procedure NormSubItemsClear(var ASubItems: TNormSubItems);
  procedure NormSubItemsAdd(var ASubItems: TNormSubItems;
                            const ASubItem: TNormSubItem);
  procedure NormSubItemsDel(var ASubItems: TNormSubItems;
                            const AIndex1: Integer; AIndex2: Integer=-1);
  procedure NormSubItemsSwap(var ASubItems: TNormSubItems;
                            const AIndex1, AIndex2: Integer);
  function  NormSubItemsIndexOf(const ASubItems: TNormSubItems;
                            const ASubItemID: Integer): Integer;
  procedure NormSubItemsCopy(const ASourceSubItems: TNormSubItems;
                            var ADestSubItems: TNormSubItems);

type
  TNormItem = record
    ItemID: Integer;
    ItemName: String;
    PostIDs: TIntVector;
    PostNames: TStrVector;
    SubItems: TNormSubItems;
  end;

  procedure NormItemClear(var AItem: TNormItem);
  procedure NormItemCopy(const ASourceItem: TNormItem; var ADestItem: TNormItem);

type
  TNormItems = array of TNormItem;

  procedure NormItemsClear(var AItems: TNormItems);
  procedure NormItemsAdd(var AItems: TNormItems; const AItem: TNormItem);

type
  TNorm = record
    NormID: Integer;
    NormName: String;
    TypicalName: String;
    BeginDate: TDate;
    EndDate: TDate;
    Items: TNormItems;
  end;

  procedure NormClear(var ANorm: TNorm);

implementation

procedure NormSubItemInfoClear(var AInfo: TNormSubItemInfo);
begin
  AInfo.InfoIDs:= nil;
  AInfo.OrderNums:= nil;
  AInfo.ClassIDs:= nil;
  AInfo.NameIDs:= nil;
  AInfo.Names:= nil;
  AInfo.Units:= nil;
  AInfo.SizeTypes:= nil;
  AInfo.Nums:= nil;
  AInfo.LifeIDs:= nil;
  AInfo.Lifes:= nil;
  AInfo.LifeNames:= nil;
end;

procedure NormSubItemInfoAdd(var AInfo: TNormSubItemInfo;
                             const AInfoID, AOrderNum,
                                   AClassID, ANameID, ASizeType,
                                   ANum, ALifeID, ALife : Integer;
                             const AName, AUnit, ALifeName: String);
begin
  VAppend(AInfo.InfoIDs, AInfoID);
  VAppend(AInfo.OrderNums, AOrderNum);
  VAppend(AInfo.ClassIDs, AClassID);
  VAppend(AInfo.NameIDs, ANameID);
  VAppend(AInfo.Names, AName);
  VAppend(AInfo.Units, AUnit);
  VAppend(AInfo.SizeTypes, ASizeType);
  VAppend(AInfo.Nums, ANum);
  VAppend(AInfo.LifeIDs, ALifeID);
  VAppend(AInfo.Lifes, ALife);
  VAppend(AInfo.LifeNames, ALifeName);
end;

procedure NormSubItemInfoSwap(var AInfo: TNormSubItemInfo;
                              const AIndex1, AIndex2: Integer);
begin
  VSwap(AInfo.InfoIDs, AIndex1, AIndex2);
  //AInfo.OrderNums no swap
  VSwap(AInfo.ClassIDs, AIndex1, AIndex2);
  VSwap(AInfo.NameIDs, AIndex1, AIndex2);
  VSwap(AInfo.Names, AIndex1, AIndex2);
  VSwap(AInfo.Units, AIndex1, AIndex2);
  VSwap(AInfo.SizeTypes, AIndex1, AIndex2);
  VSwap(AInfo.Nums, AIndex1, AIndex2);
  VSwap(AInfo.LifeIDs, AIndex1, AIndex2);
  VSwap(AInfo.Lifes, AIndex1, AIndex2);
  VSwap(AInfo.LifeNames, AIndex1, AIndex2);
end;

procedure NormSubItemInfoDel(var AInfo: TNormSubItemInfo;
                             const AIndex: Integer);
begin
  VDel(AInfo.InfoIDs, AIndex);
  VDel(AInfo.OrderNums, AIndex);
  VDel(AInfo.ClassIDs, AIndex);
  VDel(AInfo.NameIDs, AIndex);
  VDel(AInfo.Names, AIndex);
  VDel(AInfo.Units, AIndex);
  VDel(AInfo.SizeTypes, AIndex);
  VDel(AInfo.Nums, AIndex);
  VDel(AInfo.LifeIDs, AIndex);
  VDel(AInfo.Lifes, AIndex);
  VDel(AInfo.LifeNames, AIndex);
end;

procedure NormSubItemInfoCopy(const ASourceInfo: TNormSubItemInfo;
                              var ADestInfo: TNormSubItemInfo);
begin
  ADestInfo.InfoIDs:= VCut(ASourceInfo.InfoIDs);
  ADestInfo.OrderNums:= VCut(ASourceInfo.OrderNums);
  ADestInfo.ClassIDs:= VCut(ASourceInfo.ClassIDs);
  ADestInfo.NameIDs:= VCut(ASourceInfo.NameIDs);
  ADestInfo.Names:= VCut(ASourceInfo.Names);
  ADestInfo.Units:= VCut(ASourceInfo.Units);
  ADestInfo.SizeTypes:= VCut(ASourceInfo.SizeTypes);
  ADestInfo.Nums:= VCut(ASourceInfo.Nums);
  ADestInfo.LifeIDs:= VCut(ASourceInfo.LifeIDs);
  ADestInfo.Lifes:= VCut(ASourceInfo.Lifes);
  ADestInfo.LifeNames:= VCut(ASourceInfo.LifeNames);
end;

procedure NormSubItemNew(var ASubItem: TNormSubItem;
                           const ASubItemID, AOrderNum, AReasonID: Integer;
                           const AReason: String);
begin
  ASubItem.SubItemID:= ASubItemID;
  ASubItem.OrderNum:= AOrderNum;
  ASubItem.ReasonID:= AReasonID;
  ASubItem.Reason:= AReason;
  NormSubItemInfoClear(ASubItem.Info);
end;

procedure NormSubItemClear(var ASubItem: TNormSubItem);
begin
  NormSubItemNew(ASubItem, -1, -1, 0, EmptyStr);
end;

procedure NormSubItemCopy(const ASourceSubItem: TNormSubItem;
                          var ADestSubItem: TNormSubItem);
begin
  ADestSubItem.SubItemID:= ASourceSubItem.SubItemID;
  ADestSubItem.OrderNum:= ASourceSubItem.OrderNum;
  ADestSubItem.ReasonID:= ASourceSubItem.ReasonID;
  ADestSubItem.Reason:= ASourceSubItem.Reason;
  NormSubItemInfoCopy(ASourceSubItem.Info, ADestSubItem.Info);
end;

procedure NormSubItemsClear(var ASubItems: TNormSubItems);
var
  i: Integer;
begin
  for i:= 0 to High(ASubItems) do
    NormSubItemClear(ASubItems[i]);
  ASubItems:= nil;
end;

procedure NormSubItemsAdd(var ASubItems: TNormSubItems;
                          const ASubItem: TNormSubItem);
var
  N: Integer;
begin
  N:= Length(ASubItems);
  SetLength(ASubItems, N+1);
  NormSubItemCopy(ASubItem, ASubItems[N]);
end;

procedure NormSubItemsDel(var ASubItems: TNormSubItems;
                          const AIndex1: Integer; AIndex2: Integer);
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

procedure NormSubItemsSwap(var ASubItems: TNormSubItems;
                           const AIndex1, AIndex2: Integer);
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

function NormSubItemsIndexOf(const ASubItems: TNormSubItems;
                             const ASubItemID: Integer): Integer;
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

procedure NormSubItemsCopy(const ASourceSubItems: TNormSubItems;
                           var ADestSubItems: TNormSubItems);
var
  i: Integer;
begin
  NormSubItemsClear(ADestSubItems);
  if Length(ASourceSubItems)=0 then Exit;
  SetLength(ADestSubItems, Length(ASourceSubItems));
  for i:= 0 to High(ASourceSubItems) do
    NormSubItemCopy(ASourceSubItems[i], ADestSubItems[i]);
end;

procedure NormItemClear(var AItem: TNormItem);
begin
  AItem.ItemID:= -1;
  AItem.ItemName:= EmptyStr;
  AItem.PostIDs:= nil;
  AItem.PostNames:= nil;
  NormSubItemsClear(AItem.SubItems);
end;

procedure NormItemCopy(const ASourceItem: TNormItem; var ADestItem: TNormItem);
begin
  ADestItem.ItemName:= ASourceItem.ItemName;
  ADestItem.PostIDs:= VCut(ASourceItem.PostIDs);
  ADestItem.PostNames:= VCut(ASourceItem.PostNames);
  NormSubItemsCopy(ASourceItem.SubItems, ADestItem.SubItems);
end;

procedure NormItemsClear(var AItems: TNormItems);
var
  i: Integer;
begin
  for i:= 0 to High(AItems) do
    NormItemClear(AItems[i]);
  AItems:= nil;
end;

procedure NormItemsAdd(var AItems: TNormItems; const AItem: TNormItem);
var
  N: Integer;
begin
  N:= Length(AItems);
  SetLength(AItems, N+1);
  NormItemCopy(AItem, AItems[N]);
end;

procedure NormClear(var ANorm: TNorm);
begin
  ANorm.NormID:= -1;
  ANorm.NormName:= EmptyStr;
  ANorm.TypicalName:= EmptyStr;
  ANorm.BeginDate:= 0;
  ANorm.EndDate:= 0;
  NormItemsClear(ANorm.Items);
end;

end.

