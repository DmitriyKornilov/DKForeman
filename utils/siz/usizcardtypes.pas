unit USIZCardTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  //DK packages utils
  DK_Vector, DK_Matrix;

type
  TStatusItemInfo = record
    IsFreshExists : Boolean;
    LogIDs        : TInt64Matrix;
    SizNames      : TStrMatrix;
    SizCounts     : TIntMatrix;
    ReceivingDates: TDateMatrix;
    WriteoffDates : TDateMatrix;
  end;
  procedure StatusItemInfoClear(var AStatusItemInfo: TStatusItemInfo);
  procedure StatusItemInfoSetLength(var AStatusItemInfo: TStatusItemInfo;
                                 const ALength: Integer);
  procedure StatusItemInfoAdd(var AStatusItemInfo: TStatusItemInfo;
                           const ALogIDs: TInt64Vector;
                           const ASizNames: TStrVector;
                           const ASizCounts: TIntVector;
                           const AReceivingDates, AWriteoffDates: TDateVector);
  procedure StatusItemInfoAdd(var AStatusItemInfo: TStatusItemInfo;
                           const AIndex: Integer;
                           const ALogID: Int64;
                           const ASizName: String;
                           const ASizCount: Integer;
                           const AReceivingDate, AWriteoffDate: TDate);
  procedure StatusItemInfoCopy(const ASourceStatusItemInfo: TStatusItemInfo;
                           var ADestStatusItemInfo: TStatusItemInfo);

type
  TStatusItem = record
    SizeIDs  : TIntVector;
    HeightIDs: TIntVector;
    Info     : TStatusItemInfo;
  end;
  procedure StatusItemClear(var AStatusItem: TStatusItem);
  procedure StatusItemNew(var AStatusItem: TStatusItem;
                          const ASizeIDs, AHeightIDs: TIntVector);
  procedure StatusItemAdd(var AStatusItem: TStatusItem;
                        const ALogIDs: TInt64Vector;
                        const ASizNames: TStrVector;
                        const ASizCounts: TIntVector;
                        const AReceivingDates, AWriteoffDates: TDateVector);
  procedure StatusItemCopy(const ASourceStatusItem: TStatusItem;
                        var ADestStatusItem: TStatusItem);

type
  TStatusItems = array of TStatusItem;
  procedure StatusItemsClear(var AStatusItems: TStatusItems);
  procedure StatusItemsAdd(var AStatusItems: TStatusItems; const AStatusItem: TStatusItem);

implementation

procedure StatusItemInfoClear(var AStatusItemInfo: TStatusItemInfo);
begin
  AStatusItemInfo.LogIDs:= nil;
  AStatusItemInfo.SizNames:= nil;
  AStatusItemInfo.SizCounts:= nil;
  AStatusItemInfo.ReceivingDates:= nil;
  AStatusItemInfo.WriteoffDates:= nil;
  AStatusItemInfo.IsFreshExists:= False;
end;

procedure StatusItemInfoSetLength(var AStatusItemInfo: TStatusItemInfo;
                                 const ALength: Integer);
begin
  SetLength(AStatusItemInfo.LogIDs, ALength);
  SetLength(AStatusItemInfo.SizNames, ALength);
  SetLength(AStatusItemInfo.SizCounts, ALength);
  SetLength(AStatusItemInfo.ReceivingDates, ALength);
  SetLength(AStatusItemInfo.WriteoffDates, ALength);
end;

procedure StatusItemInfoAdd(var AStatusItemInfo: TStatusItemInfo;
                           const ALogIDs: TInt64Vector;
                           const ASizNames: TStrVector; const ASizCounts: TIntVector;
                           const AReceivingDates, AWriteoffDates: TDateVector);
begin
  MAppend(AStatusItemInfo.LogIDs, ALogIDs);
  MAppend(AStatusItemInfo.SizNames, ASizNames);
  MAppend(AStatusItemInfo.SizCounts, ASizCounts);
  MAppend(AStatusItemInfo.ReceivingDates, AReceivingDates);
  MAppend(AStatusItemInfo.WriteoffDates, AWriteoffDates);
end;

procedure StatusItemInfoAdd(var AStatusItemInfo: TStatusItemInfo;
                          const AIndex: Integer;
                          const ALogID: Int64;
                          const ASizName: String;
                          const ASizCount: Integer;
                          const AReceivingDate, AWriteoffDate: TDate);
begin
  VAppend(AStatusItemInfo.LogIDs[AIndex], ALogID);
  VAppend(AStatusItemInfo.SizNames[AIndex], ASizName);
  VAppend(AStatusItemInfo.SizCounts[AIndex], ASizCount);
  VAppend(AStatusItemInfo.ReceivingDates[AIndex], AReceivingDate);
  VAppend(AStatusItemInfo.WriteoffDates[AIndex], AWriteoffDate);
end;

procedure StatusItemInfoCopy(const ASourceStatusItemInfo: TStatusItemInfo;
                           var ADestStatusItemInfo: TStatusItemInfo);
begin
  ADestStatusItemInfo.IsFreshExists:= ASourceStatusItemInfo.IsFreshExists;
  ADestStatusItemInfo.LogIDs:= MCut(ASourceStatusItemInfo.LogIDs);
  ADestStatusItemInfo.SizNames:= MCut(ASourceStatusItemInfo.SizNames);
  ADestStatusItemInfo.SizCounts:= MCut(ASourceStatusItemInfo.SizCounts);
  ADestStatusItemInfo.ReceivingDates:= MCut(ASourceStatusItemInfo.ReceivingDates);
  ADestStatusItemInfo.WriteoffDates:= MCut(ASourceStatusItemInfo.WriteoffDates);
end;

procedure StatusItemClear(var AStatusItem: TStatusItem);
begin
  AStatusItem.SizeIDs:= nil;
  AStatusItem.HeightIDs:= nil;
  StatusItemInfoClear(AStatusItem.Info);
end;

procedure StatusItemNew(var AStatusItem: TStatusItem;
                        const ASizeIDs, AHeightIDs: TIntVector);
begin
  StatusItemClear(AStatusItem);
  AStatusItem.SizeIDs:= VCut(ASizeIDs);
  AStatusItem.HeightIDs:= VCut(AHeightIDs);
  StatusItemInfoSetLength(AStatusItem.Info, Length(ASizeIDs));
end;

procedure StatusItemAdd(var AStatusItem: TStatusItem;
                        const ALogIDs: TInt64Vector;
                        const ASizNames: TStrVector;
                        const ASizCounts: TIntVector;
                        const AReceivingDates, AWriteoffDates: TDateVector);
begin
  if Length(ALogIDs)>0 then
    StatusItemInfoAdd(AStatusItem.Info, ALogIDs, ASizNames, ASizCounts,
                      AReceivingDates, AWriteoffDates);
end;

procedure StatusItemCopy(const ASourceStatusItem: TStatusItem; var ADestStatusItem: TStatusItem);
begin
  ADestStatusItem.SizeIDs:= VCut(ASourceStatusItem.SizeIDs);
  ADestStatusItem.HeightIDs:= VCut(ASourceStatusItem.HeightIDs);
  StatusItemInfoCopy(ASourceStatusItem.Info, ADestStatusItem.Info);
end;

procedure StatusItemsClear(var AStatusItems: TStatusItems);
var
  i: Integer;
begin
  for i:= 0 to High(AStatusItems) do
    StatusItemClear(AStatusItems[i]);
  AStatusItems:= nil;
end;

procedure StatusItemsAdd(var AStatusItems: TStatusItems; const AStatusItem: TStatusItem);
var
  N: Integer;
begin
  N:= Length(AStatusItems);
  SetLength(AStatusItems, N+1);
  StatusItemCopy(AStatusItem, AStatusItems[N]);
end;

end.

