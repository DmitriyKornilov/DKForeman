unit USIZDocStoreWriteoffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  DividerBevel, Buttons,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZUtils,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_DateUtils, DK_Dialogs;

type

  { TSIZDocStoreWriteoffForm }

  TSIZDocStoreWriteoffForm = class(TForm)
    CollapseAllButton: TSpeedButton;
    DelButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    EditButtonPanel: TPanel;
    ExpandAllButton: TSpeedButton;
    ToolPanel: TPanel;
    ViewButtonPanel: TPanel;
    VT: TVirtualStringTree;
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    DocID: Integer;

    SIZList: TVSTCategoryCheckTable;

    CategoryNames: TStrMatrix;
    EntryIDs: TInt64Matrix;
    NomNums, SizNames, SizUnits, EntryDocNames, Notes: TStrMatrix;
    SizCounts, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes: TIntMatrix;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListSelect;
  public
    procedure ViewUpdate(const AIsEditing: Boolean);
    procedure DocChange(const ADocID: Integer);
  end;

var
  SIZDocStoreWriteoffForm: TSIZDocStoreWriteoffForm;

implementation

{$R *.lfm}

{ TSIZDocStoreWriteoffForm }

procedure TSIZDocStoreWriteoffForm.FormCreate(Sender: TObject);
begin
  DocID:= 0;
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
    ExpandAllButton, CollapseAllButton, DelButton
  ]);

  Images.ToButtons([
    ExpandAllButton, CollapseAllButton, DelButton
  ]);
end;

procedure TSIZDocStoreWriteoffForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckEnable:= False;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Документ поступления', 200);
  SIZList.AddColumn('Примечание', 300);
  SIZList.Draw;
end;

procedure TSIZDocStoreWriteoffForm.SIZListLoad;
begin

end;

procedure TSIZDocStoreWriteoffForm.SIZListSelect;
begin
  DelButton.Enabled:= SIZList.IsSelected;
end;

procedure TSIZDocStoreWriteoffForm.ViewUpdate(const AIsEditing: Boolean);
begin
  EditButtonPanel.Visible:= AIsEditing;
  SIZList.CheckEnable:= AIsEditing;
end;

procedure TSIZDocStoreWriteoffForm.DocChange(const ADocID: Integer);
begin
  DocID:= ADocID;
  SIZListLoad;
end;

procedure TSIZDocStoreWriteoffForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

procedure TSIZDocStoreWriteoffForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

end.

