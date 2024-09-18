unit USIZTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector,
  //Project utils
  UDataBase, USIZUtils;

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
  procedure NormSubItemsLoad(const AItemID: Integer; var ASubItems: TNormSubItems);
  function  NormSubItemsIndexOf(const ASubItems: TNormSubItems; const ASubItemID: Integer): Integer;


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
  NormSubItemNew(ASubItem, 0, -1, -1, EmptyStr);
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

procedure NormSubItemsLoad(const AItemID: Integer; var ASubItems: TNormSubItems);
var
  SubItemIDs, Nums, Lifes, SubItemOrderNums, ReasonIDs, InfoIDs, SizeTypes: TIntVector;
  SizNames, Units, SpecLifeNames, ReasonNames: TStrVector;
  SubItem: TNormSubItem;
  Life: String;
  i, SubItemID: Integer;
begin
  NormSubItemsClear(ASubItems);
  DataBase.SizNormItemDataLoad(AItemID, SubItemIDs, Nums, Lifes,
                               SubItemOrderNums, ReasonIDs, InfoIDs, SizeTypes,
                               SizNames, Units, SpecLifeNames, ReasonNames);
  if Length(SubItemIDs)=0 then Exit;
  SubItemID:= -1;
  for i:=0 to High(SubItemIDs) do
  begin
    //новая строка нормы
    if SubItemIDs[i]<>SubItemID then
    begin
      //записываем сформированную строку в вектор
      if i>0 then NormSubItemsAdd(ASubItems, SubItem{%H-});
      //задаем новую строку нормы
      NormSubItemNew(SubItem, SubItemIDs[i], SubItemOrderNums[i], ReasonIDs[i], ReasonNames[i]);
      //запоминаем её ID
      SubItemID:= SubItemIDs[i];
    end;
    //определяем срок службы
    Life:= GetSizNumInLifeStr(Nums[i], Lifes[i], SpecLifeNames[i]);
    //добавляем данные в строку
    NormSubItemAdd(SubItem, InfoIDs[i], SizeTypes[i], SizNames[i], Units[i], Life);
  end;
  //записываем последнюю сформированную строку в вектор
  NormSubItemsAdd(ASubItems, SubItem);
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

end.

