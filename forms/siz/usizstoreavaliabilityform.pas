unit USIZStoreAvaliabilityForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  DividerBevel, Buttons,
  //Project utils
  UVars, UConst, UTypes, USIZStoreSheet,
  //DK packages utils
  DK_CtrlUtils, DK_VSTCategoryTables, DK_Vector, DK_Matrix, DK_StrUtils,
  DK_DropFilter, DK_SheetExporter,
  //Forms
  USIZStoreWriteoffEditForm;

type

  { TSIZStoreAvaliabilityForm }

  TSIZStoreAvaliabilityForm = class(TForm)
    CaptionPanel: TPanel;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    FilterPanel: TPanel;
    CheckAllButton: TSpeedButton;
    CollapseAllButton: TSpeedButton;
    EditButtonPanel: TPanel;
    ExpandAllButton: TSpeedButton;
    ToolPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    ViewButtonPanel: TPanel;
    WriteoffButton: TSpeedButton;
    ToolButtonPanel: TPanel;
    ToolFilterPanel: TPanel;
    VT: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
    procedure WriteoffButtonClick(Sender: TObject);
  private
    ModeType: TModeType;

    DropFilter: TDKDropFilter;
    SIZList: TVSTCategoryCheckTable;

    CategoryNames: TStrMatrix;
    StoreIDs: TInt64Matrix;
    SizCounts: TIntMatrix;
    NomNums, SizNames, SizUnits, SizSizes, DocNames: TStrMatrix;

    ShowCategoryNames: TStrMatrix;
    ShowStoreIDs: TInt64Matrix;
    ShowNomNums, ShowSizNames, ShowSizUnits, ShowSizSizes, ShowDocNames: TStrMatrix;
    ShowSizCounts: TIntMatrix;

    FilterDocNames: TStrVector;
    FilterIndex: Integer;

    procedure FilterChange(const AFilterIndex: Integer);

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListShow;
    procedure SIZListCalc;
    procedure SIZListSelect;
  public
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
    procedure DataExport;
  end;

var
  SIZStoreAvaliabilityForm: TSIZStoreAvaliabilityForm;

implementation

{$R *.lfm}

{ TSIZStoreAvaliabilityForm }

procedure TSIZStoreAvaliabilityForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  SIZListCreate;

  FilterIndex:= -1;
  DropFilter:= DKDropFilterCreate('Фильтр по документу:', ToolFilterPanel, @FilterChange);
end;

procedure TSIZStoreAvaliabilityForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZList);
end;

procedure TSIZStoreAvaliabilityForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    CaptionPanel
  ]);
  SetToolButtons([
    ExpandAllButton, CollapseAllButton,
    CheckAllButton, UncheckAllButton, WriteoffButton
  ]);

  Images.ToButtons([
    ExpandAllButton, CollapseAllButton,
    CheckAllButton, UncheckAllButton, WriteoffButton
  ]);

  DataUpdate;
end;

procedure TSIZStoreAvaliabilityForm.FilterChange(const AFilterIndex: Integer);
begin
  FilterIndex:= AFilterIndex;
  SIZListShow;
end;

procedure TSIZStoreAvaliabilityForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckKind:= chkNone;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 500);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Документ поступления', 300);
  SIZList.Draw;
end;

procedure TSIZStoreAvaliabilityForm.SIZListLoad;
begin
  DataBase.SIZStoreLoad(-1{все типы}, CategoryNames, StoreIDs, SizCounts, NomNums,
                        SizNames, SizUnits, SizSizes, DocNames);

  FilterDocNames:= VAdd(['ВСЕ ДОКУМЕНТЫ'], VUnique(MToVector(DocNames)));
  DropFilter.SetItems(FilterDocNames);

  ExpandAllButton.Enabled:= not MIsNil(StoreIDs);
  CollapseAllButton.Enabled:= ExpandAllButton.Enabled;
  CheckAllButton.Enabled:= ExpandAllButton.Enabled;
  UncheckAllButton.Enabled:= ExpandAllButton.Enabled;
end;

procedure TSIZStoreAvaliabilityForm.SIZListShow;
begin
  SIZListCalc;

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(ShowCategoryNames);
    SIZList.SetColumn('Номенклатурный номер', ShowNomNums, taLeftJustify);
    SIZList.SetColumn('Наименование', ShowSizNames, taLeftJustify);
    SIZList.SetColumn('Единица измерения', ShowSizUnits);
    SIZList.SetColumn('Количество', MIntToStr(ShowSizCounts));
    SIZList.SetColumn('Размер/объём/вес', ShowSizSizes);
    SIZList.SetColumn('Документ поступления', ShowDocNames, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZStoreAvaliabilityForm.SIZListCalc;
var
  TmpNomNums, TmpSizNames, TmpSizUnits, TmpSizSizes, TmpDocNames: TStrMatrix;
  TmpSizCounts: TIntMatrix;

  procedure FilterVectorsLoad;
  var
    i, j, N: Integer;
    Indexes: TIntVector;
  begin
    if FilterIndex=0 then
    begin //все документы - нет фильтра - исходные вектора
      ShowStoreIDs:= StoreIDs;
      ShowCategoryNames:= CategoryNames;
      TmpNomNums:= NomNums;
      TmpSizNames:= SizNames;
      TmpSizUnits:= SizUnits;
      TmpSizSizes:= SizSizes;
      TmpDocNames:= DocNames;
      TmpSizCounts:= SizCounts;
    end
    else begin  //есть фильтр по документу
      //отбираем категории, где встречается этот документ и запоминаем индексы
      ShowCategoryNames:= nil;
      Indexes:= nil;
      for i:= 0 to High(CategoryNames) do
      begin
        if VIndexOf(DocNames[i], FilterDocNames[FilterIndex])>=0 then
        begin
          MAppend(ShowCategoryNames, CategoryNames[i]);
          VAppend(Indexes, i);
        end;
      end;
      //отбираем данные
      N:= Length(ShowCategoryNames);
      MDim(ShowStoreIDs, N);
      MDim(TmpNomNums, N);
      MDim(TmpSizNames, N);
      MDim(TmpSizUnits, N);
      MDim(TmpSizSizes, N);
      MDim(TmpDocNames, N);
      MDim(TmpSizCounts, N);
      for i:= 0 to High(Indexes) do
      begin
        for j:= 0 to High(NomNums[Indexes[i]]) do
        begin
          if DocNames[Indexes[i], j]<>FilterDocNames[FilterIndex] then continue;
          VAppend(ShowStoreIDs[i], StoreIDs[Indexes[i], j]);
          VAppend(TmpNomNums[i], NomNums[Indexes[i], j]);
          VAppend(TmpSizNames[i], SizNames[Indexes[i], j]);
          VAppend(TmpSizUnits[i], SizUnits[Indexes[i], j]);
          VAppend(TmpSizSizes[i], SizSizes[Indexes[i], j]);
          VAppend(TmpDocNames[i], DocNames[Indexes[i], j]);
          VAppend(TmpSizCounts[i], SizCounts[Indexes[i], j]);
        end;
        //кол-во отфильтрованных СИЗ в категории
        ShowCategoryNames[i, 3]:= IntToStr(VSum(TmpSizCounts[i]));
      end;
    end;
  end;

  procedure AddToVector(const AInd, AInd1, AInd2: Integer);
  begin
    VAppend(ShowNomNums[AInd], TmpNomNums[AInd, 0]);
    VAppend(ShowSizNames[AInd], TmpSizNames[AInd, 0]);
    VAppend(ShowSizUnits[AInd], TmpSizUnits[AInd, 0]);
    VAppend(ShowSizSizes[AInd], TmpSizSizes[AInd, AInd1]);
    VAppend(ShowDocNames[AInd], TmpDocNames[AInd, AInd1]);
    VAppend(ShowSizCounts[AInd], VSum(TmpSizCounts[AInd], AInd1, AInd2));
  end;

  procedure ShowVectorsLoad;
  var
    i, j, N, N1, N2: Integer;
    SizSize, DocName: String;
  begin
    //если рекдактирование - оставляем записи СИЗ по 1 экземпляру
    if ModeType=mtEditing then
    begin
      ShowNomNums:= TmpNomNums;
      ShowSizNames:= TmpSizNames;
      ShowSizUnits:= TmpSizUnits;
      ShowSizSizes:= TmpSizSizes;
      ShowDocNames:= TmpDocNames;
      ShowSizCounts:= TmpSizCounts;
      Exit;
    end;
    //для простого отображения - группируем внутри категории СИЗ по размеру
    //и документу прихода
    N:= Length(ShowCategoryNames);
    MDim(ShowNomNums, N);
    MDim(ShowSizNames, N);
    MDim(ShowSizUnits, N);
    MDim(ShowSizSizes, N);
    MDim(ShowDocNames, N);
    MDim(ShowSizCounts, N);
    for i:= 0 to N-1 do
    begin
      SizSize:= TmpSizSizes[i, 0];
      DocName:= TmpDocNames[i, 0];
      N1:= 0;
      for j:= 1 to High(TmpDocNames[i]) do
      begin
        if not (SSame(TmpDocNames[i, j], DocName) and SSame(TmpSizSizes[i, j], SizSize)) then
        begin
          N2:= j - 1;
          AddToVector(i, N1, N2);
          N1:= j;
          SizSize:= TmpSizSizes[i, j];
          DocName:= TmpDocNames[i, j];
        end;
      end;
      N2:= High(TmpDocNames[i]);
      AddToVector(i, N1, N2);
    end;
  end;

begin
  FilterVectorsLoad;
  ShowVectorsLoad;
end;

procedure TSIZStoreAvaliabilityForm.SIZListSelect;
begin
  WriteoffButton.Enabled:= SIZList.IsSelected;
end;

procedure TSIZStoreAvaliabilityForm.DataExport;
var
  Exporter: TSheetsExporter;
  ExpSheet: TSIZStoreSheet;
  Worksheet: TsWorksheet;
begin
  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лист1');
    ExpSheet:= TSIZStoreSheet.Create(Worksheet, nil, GridFont);
    try
      ExpSheet.Draw(ShowNomNums, ShowSizNames, ShowSizUnits, ShowSizSizes,
                    ShowDocNames, ShowSizCounts);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!', FormatDateTime('Склад на dd.mm.yyyy', Date));
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZStoreAvaliabilityForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;
  EditButtonPanel.Visible:= ModeType=mtEditing;
  if ModeType=mtEditing then
    SIZList.CheckKind:= chkAll
  else
    SIZList.CheckKind:= chkNone;
  SIZListShow;
end;

procedure TSIZStoreAvaliabilityForm.DataUpdate;
begin
  SIZListLoad;
end;

procedure TSIZStoreAvaliabilityForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
  SIZList.ShowFirst;
end;

procedure TSIZStoreAvaliabilityForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZStoreAvaliabilityForm.CheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZStoreAvaliabilityForm.UncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZStoreAvaliabilityForm.WriteoffButtonClick(Sender: TObject);
var
  SIZStoreWriteoffEditForm: TSIZStoreWriteoffEditForm;
  MUsed: TBoolMatrix;
begin
  MUsed:= SIZList.Selected;

  SIZStoreWriteoffEditForm:= TSIZStoreWriteoffEditForm.Create(nil);
  try
    SIZStoreWriteoffEditForm.StoreIDs:= MToVector(ShowStoreIDs, MUsed);
    SIZStoreWriteoffEditForm.NomNums:= MToVector(ShowNomNums, MUsed);
    SIZStoreWriteoffEditForm.SizNames:= MToVector(ShowSizNames, MUsed);
    SIZStoreWriteoffEditForm.SizUnits:= MToVector(ShowSizUnits, MUsed);
    SIZStoreWriteoffEditForm.SizSizes:= MToVector(ShowSizSizes, MUsed);
    SIZStoreWriteoffEditForm.EntryDocNames:= MToVector(ShowDocNames, MUsed);

    if SIZStoreWriteoffEditForm.ShowModal=mrOK then
      SIZListLoad;
  finally
    FreeAndNil(SIZStoreWriteoffEditForm);
  end;
end;

end.

