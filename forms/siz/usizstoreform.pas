unit USIZStoreForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, VirtualTrees, BCButton, DividerBevel,
  //Project utils
  UVars, UConst, UTypes,
  //DK packages utils
  DK_CtrlUtils, DK_VSTTables, DK_Vector, DK_Matrix, DK_StrUtils, DK_VSTDropDown,
  //Forms
  USIZDocForm, USIZStoreHistoryForm, USIZStoreWriteoffForm;

type

  { TSIZStoreForm }

  TSIZStoreForm = class(TForm)
    DividerBevel4: TDividerBevel;
    FilterDocBCButton: TBCButton;
    FilterPanel: TPanel;
    FilterLabel: TLabel;
    SIZCheckAllButton: TSpeedButton;
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    CaptionPanel: TPanel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    HistoryButton: TSpeedButton;
    OutButton: TSpeedButton;
    ExportButton: TSpeedButton;
    EntryButton: TSpeedButton;
    SIZWriteoffButton: TSpeedButton;
    SIZCollapseAllButton: TSpeedButton;
    SIZEditButtonPanel: TPanel;
    SIZExpandAllButton: TSpeedButton;
    SIZToolPanel: TPanel;
    SIZViewButtonPanel: TPanel;
    SIZUncheckAllButton: TSpeedButton;
    WriteoffButton: TSpeedButton;
    ToolPanel: TPanel;
    VT: TVirtualStringTree;
    BackButton: TSpeedButton;
    procedure BackButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure EntryButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
    procedure OutButtonClick(Sender: TObject);
    procedure SIZCheckAllButtonClick(Sender: TObject);
    procedure SIZCollapseAllButtonClick(Sender: TObject);
    procedure SIZExpandAllButtonClick(Sender: TObject);
    procedure SIZUncheckAllButtonClick(Sender: TObject);
    procedure SIZWriteoffButtonClick(Sender: TObject);
    procedure WriteoffButtonClick(Sender: TObject);
  private
    ModeType: TModeType;

    SIZList: TVSTCategoryCheckTable;
    FilterDocDropDown: TVSTDropDown;

    CategoryNames: TStrMatrix;
    StoreIDs: TInt64Matrix;
    SizCounts: TIntMatrix;
    NomNums, SizNames, SizUnits, SizSizes, DocNames: TStrMatrix;

    ShowCategoryNames: TStrMatrix;
    ShowNomNums, ShowSizNames, ShowSizUnits, ShowSizSizes, ShowDocNames: TStrMatrix;
    ShowSizCounts: TIntMatrix;

    FilterDocNames: TStrVector;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListShow;
    procedure SIZListCalc;
    procedure SIZListSelect;
  public
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  SIZStoreForm: TSIZStoreForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZStoreForm }

procedure TSIZStoreForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZStoreForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  FilterDocDropDown:= TVSTDropDown.Create(FilterDocBCButton);
  FilterDocDropDown.DropDownCount:= 20;
  FilterDocDropDown.OnChange:= @SIZListShow;

  SIZListCreate;
end;

procedure TSIZStoreForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FilterDocDropDown);
  FreeAndNil(SIZList);
end;

procedure TSIZStoreForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    CaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    SIZExpandAllButton, SIZCollapseAllButton,
    SIZCheckAllButton, SIZUncheckAllButton, SIZWriteoffButton
  ]);

  Images.ToButtons([
    ExportButton, EntryButton, OutButton, BackButton, WriteoffButton, HistoryButton,
    CloseButton,
    SIZExpandAllButton, SIZCollapseAllButton,
    SIZCheckAllButton, SIZUncheckAllButton, SIZWriteoffButton
  ]);

  DataUpdate;
end;

procedure TSIZStoreForm.HistoryButtonClick(Sender: TObject);
begin
  FormModalShow(TSIZStoreHistoryForm);
end;

procedure TSIZStoreForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckVisible:= True;
  SIZList.CheckEnable:= False;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Документ', 300);
  SIZList.Draw;
end;

procedure TSIZStoreForm.SIZListLoad;
begin
  DataBase.SIZStoreLoad(CategoryNames, StoreIDs, SizCounts, NomNums,
                        SizNames, SizUnits, SizSizes, DocNames);

  FilterDocNames:= VAdd(['ВСЕ ДОКУМЕНТЫ'], VUnique(MToVector(DocNames)));
  FilterDocDropDown.Items:= FilterDocNames;
  FilterDocDropDown.ItemIndex:= 0;

  //SIZListShow;
end;

procedure TSIZStoreForm.SIZListShow;
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
    SIZList.SetColumn('Документ', ShowDocNames, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZStoreForm.SIZListCalc;
var
  i, j, k, N, N1, N2: Integer;
  SizSize, DocName: String;
  Indexes: TIntVector;

  TmpNomNums, TmpSizNames, TmpSizUnits, TmpSizSizes, TmpDocNames: TStrMatrix;
  TmpSizCounts: TIntMatrix;

  procedure AddToVector(const AInd, AInd1, AInd2: Integer);
  begin
    VAppend(ShowNomNums[AInd], NomNums[AInd, 0]);
    VAppend(ShowSizNames[AInd], SizNames[AInd, 0]);
    VAppend(ShowSizUnits[AInd], SizUnits[AInd, 0]);
    VAppend(ShowSizSizes[AInd], SizSizes[AInd, AInd1]);
    VAppend(ShowDocNames[AInd], DocNames[AInd, AInd1]);
    VAppend(ShowSizCounts[AInd], VSum(SizCounts[AInd], AInd1, AInd2));
  end;

begin
  if FilterDocDropDown.ItemIndex=0 then
  begin
    ShowCategoryNames:= CategoryNames;
    TmpNomNums:= NomNums;
    TmpSizNames:= SizNames;
    TmpSizUnits:= SizUnits;
    TmpSizSizes:= SizSizes;
    TmpDocNames:= DocNames;
    TmpSizCounts:= SizCounts;
  end
  else begin
    ShowCategoryNames:= nil;
    Indexes:= nil;
    for i:= 0 to High(CategoryNames) do
    begin
      if VIndexOf(DocNames[i], FilterDocNames[FilterDocDropDown.ItemIndex])>=0 then
      begin
        MAppend(ShowCategoryNames, CategoryNames[i]);
        VAppend(Indexes, i);
      end;
    end;

    N:= Length(ShowCategoryNames);
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
        if DocNames[i, j]<>FilterDocNames[FilterDocDropDown.ItemIndex] then continue;
        VAppend(TmpNomNums[i], NomNums[i, j]);
        VAppend(TmpSizNames[i], SizNames[i, j]);
        VAppend(TmpSizUnits[i], SizUnits[i, j]);
        VAppend(TmpSizSizes[i], SizSizes[i, j]);
        VAppend(TmpDocNames[i], DocNames[i, j]);
        VAppend(TmpSizCounts[i], SizCounts[i, j]);
      end;
    end;
  end;

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

  N:= Length(ShowCategoryNames);
  MDim(ShowNomNums, N);
  MDim(ShowSizNames, N);
  MDim(ShowSizUnits, N);
  MDim(ShowSizSizes, N);
  MDim(ShowDocNames, N);
  MDim(ShowSizCounts, N);

  for i:= 0 to N-1 do
  begin
    SizSize:= SizSizes[i, 0];
    DocName:= DocNames[i, 0];
    N1:= 0;
    for j:= 1 to High(DocNames[i]) do
    begin
      if not (SSame(DocNames[i, j], DocName) and SSame(SizSizes[i, j], SizSize)) then
      begin
        N2:= j - 1;
        AddToVector(i, N1, N2);
        N1:= j;
        SizSize:= SizSizes[i, j];
        DocName:= DocNames[i, j];
      end;
    end;
    N2:= High(DocNames[i]);
    AddToVector(i, N1, N2);
  end;
end;

//procedure TSIZStoreForm.SIZListCalc;
//var
//  i, j, N, N1, N2: Integer;
//  SizSize, DocName: String;
//
//  procedure AddToVector(const AInd, AInd1, AInd2: Integer);
//  begin
//    VAppend(ShowNomNums[AInd], NomNums[AInd, 0]);
//    VAppend(ShowSizNames[AInd], SizNames[AInd, 0]);
//    VAppend(ShowSizUnits[AInd], SizUnits[AInd, 0]);
//    VAppend(ShowSizSizes[AInd], SizSizes[AInd, AInd1]);
//    VAppend(ShowDocNames[AInd], DocNames[AInd, AInd1]);
//    VAppend(ShowSizCounts[AInd], VSum(SizCounts[AInd], AInd1, AInd2));
//  end;
//
//begin
//  ShowCategoryNames:= CategoryNames;
//
//  if ModeType=mtEditing then
//  begin
//    ShowNomNums:= NomNums;
//    ShowSizNames:= SizNames;
//    ShowSizUnits:= SizUnits;
//    ShowSizSizes:= SizSizes;
//    ShowDocNames:= DocNames;
//    ShowSizCounts:= SizCounts;
//    Exit;
//  end;
//
//  N:= Length(CategoryNames);
//  MDim(ShowNomNums, N);
//  MDim(ShowSizNames, N);
//  MDim(ShowSizUnits, N);
//  MDim(ShowSizSizes, N);
//  MDim(ShowDocNames, N);
//  MDim(ShowSizCounts, N);
//
//  for i:= 0 to N-1 do
//  begin
//    SizSize:= SizSizes[i, 0];
//    DocName:= DocNames[i, 0];
//    N1:= 0;
//    for j:= 1 to High(DocNames[i]) do
//    begin
//      if not (SSame(DocNames[i, j], DocName) and SSame(SizSizes[i, j], SizSize)) then
//      begin
//        N2:= j - 1;
//        AddToVector(i, N1, N2);
//        N1:= j;
//        SizSize:= SizSizes[i, j];
//        DocName:= DocNames[i, j];
//      end;
//    end;
//    N2:= High(DocNames[i]);
//    AddToVector(i, N1, N2);
//  end;
//end;

procedure TSIZStoreForm.SIZListSelect;
begin
  SIZWriteoffButton.Enabled:= SIZList.IsSelected;
end;

procedure TSIZStoreForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;
  SIZEditButtonPanel.Visible:= ModeType=mtEditing;
  SIZList.CheckEnable:= ModeType=mtEditing;
  SIZListShow;
end;

procedure TSIZStoreForm.DataUpdate;
begin
  SIZListLoad;
end;

procedure TSIZStoreForm.EntryButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(1);
end;

procedure TSIZStoreForm.OutButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(2);
end;

procedure TSIZStoreForm.WriteoffButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(3);
end;

procedure TSIZStoreForm.BackButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(4);
end;

procedure TSIZStoreForm.SIZExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
  SIZList.ShowFirst;
end;

procedure TSIZStoreForm.SIZCollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZStoreForm.SIZCheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZStoreForm.SIZUncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZStoreForm.SIZWriteoffButtonClick(Sender: TObject);
var
  SIZStoreWriteoffForm: TSIZStoreWriteoffForm;
  MUsed: TBoolMatrix;
begin
  MUsed:= SIZList.Selected;

  SIZStoreWriteoffForm:= TSIZStoreWriteoffForm.Create(nil);
  try
    SIZStoreWriteoffForm.StoreIDs:= MToVector(StoreIDs, MUsed);
    SIZStoreWriteoffForm.NomNums:= MToVector(NomNums, MUsed);
    SIZStoreWriteoffForm.SizNames:= MToVector(SizNames, MUsed);
    SIZStoreWriteoffForm.SizUnits:= MToVector(SizUnits, MUsed);
    SIZStoreWriteoffForm.SizSizes:= MToVector(SizSizes, MUsed);
    SIZStoreWriteoffForm.EntryDocNames:= MToVector(DocNames, MUsed);

    if SIZStoreWriteoffForm.ShowModal=mrOK then
      SIZListLoad;
  finally
    FreeAndNil(SIZStoreWriteoffForm);
  end;
end;

end.

