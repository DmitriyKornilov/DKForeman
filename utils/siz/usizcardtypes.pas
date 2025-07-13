unit USIZCardTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector, DK_Matrix;

type
  //соотв TNormSubItemInfo
  //Index1 - соотв индексу InfoIDs
  //Index2 - группировка в один комплект по одному LogID
  //Index3 - индекс СИЗ в комплекте
  TStatusSubItemInfo = record
    LogIDs        : TInt64Matrix;
    ReceivingDates: TDateMatrix;
    WriteoffDates : TDateMatrix;
    SizNames      : TStrMatrix3D;
    SizCounts     : TIntMatrix3D;
  end;
  procedure StatusSubItemInfoClear(var AInfo: TStatusSubItemInfo);
  procedure StatusSubItemInfoSetLength(var AInfo: TStatusSubItemInfo;
                              const ALength: Integer);
  procedure StatusSubItemInfoCopy(const ASourceInfo: TStatusSubItemInfo;
                           var ADestInfo: TStatusSubItemInfo);

type
  //соотв TNormSubItem
  TStatusSubItem = record
    IsFreshExists : Boolean;
    SizeIDs       : TIntVector;
    HeightIDs     : TIntVector;
    Info          : TStatusSubItemInfo;
  end;
  procedure StatusSubItemClear(var ASubItem: TStatusSubItem);
  procedure StatusSubItemNew(var ASubItem: TStatusSubItem;
                           const ASizeIDs, AHeightIDs: TIntVector);
  procedure StatusSubItemCopy(const ASourceSubItem: TStatusSubItem;
                              var ADestSubItem: TStatusSubItem);

type
  //соотв TNormSubItems
  TStatusSubItems = array of TStatusSubItem;
  procedure StatusSubItemsClear(var ASubItems: TStatusSubItems);
  procedure StatusSubItemsAdd(var ASubItems: TStatusSubItems;
                          const ASubItem: TStatusSubItem);

implementation

procedure StatusSubItemInfoClear(var AInfo: TStatusSubItemInfo);
begin
  AInfo.LogIDs:= nil;
  AInfo.SizNames:= nil;
  AInfo.SizCounts:= nil;
  AInfo.ReceivingDates:= nil;
  AInfo.WriteoffDates:= nil;
end;

procedure StatusSubItemInfoSetLength(var AInfo: TStatusSubItemInfo;
  const ALength: Integer);
begin
  SetLength(AInfo.LogIDs, ALength);
  SetLength(AInfo.SizNames, ALength);
  SetLength(AInfo.SizCounts, ALength);
  SetLength(AInfo.ReceivingDates, ALength);
  SetLength(AInfo.WriteoffDates, ALength);
end;

procedure StatusSubItemInfoCopy(const ASourceInfo: TStatusSubItemInfo;
                           var ADestInfo: TStatusSubItemInfo);
begin
  ADestInfo.LogIDs:= MCut(ASourceInfo.LogIDs);
  ADestInfo.SizNames:= MCut(ASourceInfo.SizNames);
  ADestInfo.SizCounts:= MCut(ASourceInfo.SizCounts);
  ADestInfo.ReceivingDates:= MCut(ASourceInfo.ReceivingDates);
  ADestInfo.WriteoffDates:= MCut(ASourceInfo.WriteoffDates);
end;

procedure StatusSubItemClear(var ASubItem: TStatusSubItem);
begin
  ASubItem.IsFreshExists:= False;
  ASubItem.SizeIDs:= nil;
  ASubItem.HeightIDs:= nil;
  StatusSubItemInfoClear(ASubItem.Info);
end;

procedure StatusSubItemNew(var ASubItem: TStatusSubItem;
                           const ASizeIDs, AHeightIDs: TIntVector);
begin
  StatusSubItemInfoClear(ASubItem.Info);
  StatusSubItemInfoSetLength(ASubItem.Info, Length(ASizeIDs));
  ASubItem.SizeIDs:= VCut(ASizeIDs);
  ASubItem.HeightIDs:= VCut(AHeightIDs);
end;

procedure StatusSubItemCopy(const ASourceSubItem: TStatusSubItem;
                            var ADestSubItem: TStatusSubItem);
begin
  ADestSubItem.IsFreshExists:= ASourceSubItem.IsFreshExists;
  ADestSubItem.SizeIDs:= VCut(ASourceSubItem.SizeIDs);
  ADestSubItem.HeightIDs:= VCut(ASourceSubItem.HeightIDs);
  StatusSubItemInfoCopy(ASourceSubItem.Info, ADestSubItem.Info);
end;

procedure StatusSubItemsClear(var ASubItems: TStatusSubItems);
var
  i: Integer;
begin
  for i:= 0 to High(ASubItems) do
    StatusSubItemClear(ASubItems[i]);
  ASubItems:= nil;
end;

procedure StatusSubItemsAdd(var ASubItems: TStatusSubItems; const ASubItem: TStatusSubItem);
var
  N: Integer;
begin
  N:= Length(ASubItems);
  SetLength(ASubItems, N+1);
  StatusSubItemCopy(ASubItem, ASubItems[N]);
end;

end.

