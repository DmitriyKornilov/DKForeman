unit USIZDocStoreWriteoffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  DividerBevel, Buttons,
  //Project utils
  UVars, UConst,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs, DK_StrUtils;

type

  { TSIZDocStoreWriteoffForm }

  TSIZDocStoreWriteoffForm = class(TForm)
    CollapseAllButton: TSpeedButton;
    DelButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    EditButtonPanel: TPanel;
    ExpandAllButton: TSpeedButton;
    CheckAllButton: TSpeedButton;
    CheckButtonPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    ToolButtonPanel: TPanel;
    ToolPanel: TPanel;
    ViewButtonPanel: TPanel;
    VT: TVirtualStringTree;
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
  private
    DocID: Integer;
    IsEditing: Boolean;
    SIZList: TVSTCategoryCheckTable;

    CategoryNames: TStrMatrix;
    StoreIDs: TInt64Matrix;
    SizCounts: TIntMatrix;
    NomNums, SizNames, SizUnits, SizSizes, EntryDocNames, Notes: TStrMatrix;

    ShowSizCounts: TIntMatrix;
    ShowNomNums, ShowSizNames, ShowSizUnits,
    ShowSizSizes, ShowEntryDocNames, ShowNotes: TStrMatrix;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListShow;
    procedure SIZListCalc;
    procedure SIZListSelect;
  public
    procedure ViewUpdate(const AIsEditing: Boolean);
    procedure DocChange(const ADocID: Integer);
    procedure DocExport;
  end;

var
  SIZDocStoreWriteoffForm: TSIZDocStoreWriteoffForm;

implementation

{$R *.lfm}

{ TSIZDocStoreWriteoffForm }

procedure TSIZDocStoreWriteoffForm.FormCreate(Sender: TObject);
begin
  DocID:= 0;
  IsEditing:= False;
  SIZListCreate;
end;

procedure TSIZDocStoreWriteoffForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZList);
end;

procedure TSIZDocStoreWriteoffForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton, DelButton
  ]);

  Images.ToButtons([
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton, DelButton
  ]);
end;

procedure TSIZDocStoreWriteoffForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckKind:= chkNone;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Документ поступления', 200);
  SIZList.AddColumn('Примечание', 150);
  SIZList.AutosizeColumnEnable('Наименование');
  SIZList.Draw;
end;

procedure TSIZDocStoreWriteoffForm.SIZListLoad;
begin
  if DocID<=0 then
  begin
    SIZList.ValuesClear;
    Exit;
  end;
  DataBase.SIZStoreWriteOffLoad(DocID, CategoryNames, StoreIDs, SizCounts,
                                NomNums, SizNames, SizUnits, SizSizes,
                                EntryDocNames, Notes);

  SIZListShow;

  ExpandAllButton.Enabled:= not MIsNil(StoreIDs);
  CollapseAllButton.Enabled:= ExpandAllButton.Enabled;
  CheckAllButton.Enabled:= ExpandAllButton.Enabled;
  UncheckAllButton.Enabled:= ExpandAllButton.Enabled;
end;

procedure TSIZDocStoreWriteoffForm.SIZListShow;
begin
  SIZListCalc;

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(CategoryNames);
    SIZList.SetColumn('Номенклатурный номер', ShowNomNums, taLeftJustify);
    SIZList.SetColumn('Наименование', ShowSizNames, taLeftJustify);
    SIZList.SetColumn('Единица измерения', ShowSizUnits);
    SIZList.SetColumn('Количество', MIntToStr(ShowSizCounts));
    SIZList.SetColumn('Размер/объём/вес', ShowSizSizes);
    SIZList.SetColumn('Документ поступления', ShowEntryDocNames, taLeftJustify);
    SIZList.SetColumn('Документ', ShowNotes, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZDocStoreWriteoffForm.SIZListCalc;
var
  i, j, N, N1, N2: Integer;
  SizSize, EntryDocName: String;

  procedure AddToVector(const AInd, AInd1, AInd2: Integer);
  begin
    VAppend(ShowNomNums[AInd], NomNums[AInd, 0]);
    VAppend(ShowSizNames[AInd], SizNames[AInd, 0]);
    VAppend(ShowSizUnits[AInd], SizUnits[AInd, 0]);
    VAppend(ShowSizSizes[AInd], SizSizes[AInd, AInd1]);
    VAppend(ShowEntryDocNames[AInd], EntryDocNames[AInd, AInd1]);
    VAppend(ShowSizCounts[AInd], VSum(SizCounts[AInd], AInd1, AInd2));
    VAppend(ShowNotes[AInd], Notes[AInd, AInd1]);
  end;

begin
  //если рекдактирование - оставляем записи СИЗ по 1 экземпляру
  if IsEditing then
  begin
    ShowNomNums:= NomNums;
    ShowSizNames:= SizNames;
    ShowSizUnits:= SizUnits;
    ShowSizSizes:= SizSizes;
    ShowEntryDocNames:= EntryDocNames;
    ShowSizCounts:= SizCounts;
    ShowNotes:= Notes;
    Exit;
  end;
  //для простого отображения - группируем внутри категории СИЗ по размеру
  //и документу прихода
  N:= Length(CategoryNames);
  MDim(ShowNomNums, N);
  MDim(ShowSizNames, N);
  MDim(ShowSizUnits, N);
  MDim(ShowSizSizes, N);
  MDim(ShowEntryDocNames, N);
  MDim(ShowSizCounts, N);
  MDim(ShowNotes, N);
  for i:= 0 to N-1 do
  begin
    SizSize:= SizSizes[i, 0];
    EntryDocName:= EntryDocNames[i, 0];
    N1:= 0;
    for j:= 1 to High(EntryDocNames[i]) do
    begin
      if not (SSame(EntryDocNames[i, j], EntryDocName) and SSame(SizSizes[i, j], SizSize)) then
      begin
        N2:= j - 1;
        AddToVector(i, N1, N2);
        N1:= j;
        SizSize:= SizSizes[i, j];
        EntryDocName:= EntryDocNames[i, j];
      end;
    end;
    N2:= High(EntryDocNames[i]);
    AddToVector(i, N1, N2);
  end;
end;

procedure TSIZDocStoreWriteoffForm.SIZListSelect;
begin
  DelButton.Enabled:= SIZList.IsSelected;
end;

procedure TSIZDocStoreWriteoffForm.ViewUpdate(const AIsEditing: Boolean);
begin
  IsEditing:= AIsEditing;
  EditButtonPanel.Visible:= AIsEditing;
  CheckButtonPanel.Visible:= AIsEditing;
  if AIsEditing then
    SIZList.CheckKind:= chkAll
  else
    SIZList.CheckKind:= chkNone;
  SIZListShow;
end;

procedure TSIZDocStoreWriteoffForm.DocChange(const ADocID: Integer);
begin
  DocID:= ADocID;
  SIZListLoad;
end;

procedure TSIZDocStoreWriteoffForm.DocExport;
begin
  //!!!
end;

procedure TSIZDocStoreWriteoffForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

procedure TSIZDocStoreWriteoffForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZDocStoreWriteoffForm.CheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZDocStoreWriteoffForm.UncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZDocStoreWriteoffForm.DelButtonClick(Sender: TObject);
var
  DelStoreIDs: TInt64Vector;
begin
  if not Confirm('Отменить списание (передачу) выбранных СИЗ?') then Exit;
  DelStoreIDs:= MToVector(StoreIDs, SIZList.Selected);
  if DataBase.SIZStoreWriteoffCancel(DelStoreIDs) then
    SIZListLoad;
end;

end.

