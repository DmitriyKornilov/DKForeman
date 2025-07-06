unit USIZDocForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel, Spin,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZUtils,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_DateUtils,
  //Forms
  USIZDocEditForm;

type

  { TSIZDocForm }

  TSIZDocForm = class(TForm)
    DividerBevel4: TDividerBevel;
    DocCheckAllButton: TSpeedButton;
    SIZCheckAllButton: TSpeedButton;
    DocCollapseAllButton: TSpeedButton;
    SIZCollapseAllButton: TSpeedButton;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DocAddButton: TSpeedButton;
    DocDelButton: TSpeedButton;
    DocEditButton: TSpeedButton;
    DocEraseButton: TSpeedButton;
    DocExpandAllButton: TSpeedButton;
    SIZExpandAllButton: TSpeedButton;
    DocViewButtonPanel: TPanel;
    SIZViewButtonPanel: TPanel;
    DocEditButtonPanel: TPanel;
    SIZEditButtonPanel: TPanel;
    SIZAddButton: TSpeedButton;
    SIZCopyButton: TSpeedButton;
    SIZDelButton: TSpeedButton;
    DocCaptionPanel: TPanel;
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    EditingButton: TSpeedButton;
    SIZEditButton: TSpeedButton;
    SIZVT: TVirtualStringTree;
    SIZToolPanel: TPanel;
    ExportButton: TSpeedButton;
    SIZCaptionPanel: TPanel;
    DocPanel: TPanel;
    Splitter: TSplitter;
    DocToolPanel: TPanel;
    SIZPanel: TPanel;
    ToolPanel: TPanel;
    DocVT: TVirtualStringTree;
    DocUncheckAllButton: TSpeedButton;
    SIZUncheckAllButton: TSpeedButton;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure DocAddButtonClick(Sender: TObject);
    procedure DocEditButtonClick(Sender: TObject);
    procedure DocVTDblClick(Sender: TObject);
    procedure EditingButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    DocList: TVSTTable;
    SIZList: TVSTCustomCategoryTable;

    DocIDs, DocForms: TIntVector;
    DocNames, DocNums: TStrVector;
    DocDates: TDateVector;

    CategoryNames: TStrVector;
    EntryIDs: TInt64Matrix;
    NomNums, SizNames, SizUnits, Notes: TStrMatrix;
    SizCounts, NameIDs, SizeIDs, HeightIDs, SizeTypes: TIntMatrix;

    procedure DocListCreate;
    procedure DocListSelect;
    procedure DocListLoad(const ASelectedID: Integer = -1);

    procedure SIZListCreate;
    procedure SIZListLoad;

    procedure DocEdit(const AEditingType: TEditingType);

    procedure ViewUpdate;
  public
    DocType: Integer;
  end;

var
  SIZDocForm: TSIZDocForm;

  procedure SIZDocFormOpen(const ADocType: Integer);

implementation

procedure SIZDocFormOpen(const ADocType: Integer);
var
  Form: TSIZDocForm;
begin
  Form:= TSIZDocForm.Create(nil);
  try
    Form.DocType:= ADocType;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZDocForm }

procedure TSIZDocForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION;
  DocType:= 0;

  SIZListCreate;
  DocListCreate;
end;

procedure TSIZDocForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocList);
  FreeAndNil(SIZList);
end;

procedure TSIZDocForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, DocToolPanel, SIZToolPanel
  ]);
  SetCaptionPanels([
    DocCaptionPanel, SIZCaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    EditingButton,
    DocExpandAllButton, DocCollapseAllButton, DocCheckAllButton, DocUncheckAllButton,
    DocAddButton, DocDelButton, DocEditButton, DocEraseButton,
    SIZExpandAllButton, SIZCollapseAllButton, SIZCheckAllButton, SIZUncheckAllButton,
    SIZAddButton, SIZDelButton, SIZEditButton, SIZCopyButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton,
    EditingButton,
    DocExpandAllButton, DocCollapseAllButton, DocCheckAllButton, DocUncheckAllButton,
    DocAddButton, DocDelButton, DocEditButton, DocEraseButton,
    SIZExpandAllButton, SIZCollapseAllButton, SIZCheckAllButton, SIZUncheckAllButton,
    SIZAddButton, SIZDelButton, SIZEditButton, SIZCopyButton
  ]);

  if DocType>0 then
    Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[DocType+6];

  YearSpinEdit.Value:= YearOfDate(Date);
end;

procedure TSIZDocForm.YearSpinEditChange(Sender: TObject);
begin
  DocListLoad;
end;

procedure TSIZDocForm.DocListCreate;
begin
  DocList:= TVSTTable.Create(DocVT);
  DocList.CanSelect:= True;
  DocList.CanUnselect:= False;
  DocList.OnSelect:= @DocListSelect;
  DocList.SetSingleFont(GridFont);
  DocList.HeaderFont.Style:= [fsBold];

  DocList.AddColumn('№ п/п', 50);
  DocList.AddColumn('Документ', 300);
  DocList.AutosizeColumnEnable('Документ');
  DocList.Draw;
end;

procedure TSIZDocForm.DocListSelect;
begin
  DocDelButton.Enabled:= DocList.IsSelected;
  DocEditButton.Enabled:= DocDelButton.Enabled;

  SIZListLoad;
end;

procedure TSIZDocForm.DocListLoad(const ASelectedID: Integer);
var
  SelectedID: Integer;
  V: TStrVector;
begin
  SelectedID:= GetSelectedID(DocList, DocIDs, ASelectedID);

  DataBase.SIZDocListLoad(YearSpinEdit.Value, DocType, DocIDs, DocForms,
                          DocNames, DocNums, DocDates);

  DocList.Visible:= False;
  try
    DocList.ValuesClear;
    DocList.SetColumn('№ п/п', VIntToStr(VOrder(Length(DocIDs))));
    V:= SIZDocFullName(DocNames, DocNums, DocDates);
    DocList.SetColumn('Документ', V, taLeftJustify);
    DocList.Draw;
    DocList.ReSelect(DocIDs, SelectedID, True);  //возвращаем выделение строки
  finally
    DocList.Visible:= True;
  end;

  ExportButton.Enabled:= not VIsNil(DocIDs);
end;

procedure TSIZDocForm.SIZListCreate;
begin
  SIZList:= TVSTCustomCategoryTable.Create(SIZVT);
  SIZList.TreeLinesVisible:= False;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Примечание', 300);
  SIZList.Draw;
end;

procedure TSIZDocForm.SIZListLoad;
var
  i: Integer;
  M: TStrMatrix;
begin
  if not DocList.IsSelected then Exit;

  DataBase.SIZDocEntryLoad(DocIDs[DocList.SelectedIndex],
                         EntryIDs, NomNums, SizNames, SizUnits, Notes,
                         SizCounts, NameIDs, SizeIDs, HeightIDs, SizeTypes);

  VDim(CategoryNames, Length(EntryIDs));
  for i:= 0 to High(EntryIDs) do
  begin
    CategoryNames[i]:= NomNums[i, 0] + ' - ' + SizNames[i, 0] +
                    ' (' + SizUnits[i, 0] + ') - ' +
                    IntToStr(VSum(SizCounts[i]));
  end;

  SIZList.SetCategories(CategoryNames);
  SIZList.SetColumn('Номенклатурный номер', NomNums);
  SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
  SIZList.SetColumn('Единица измерения', SizUnits);
  SIZList.SetColumn('Количество', MIntToStr(SizCounts));
  M:= SIZFullSize(SizeTypes, SizeIDs, HeightIDs);
  SIZList.SetColumn('Размер/объём/вес', M);
  SIZList.SetColumn('Примечание', Notes, taLeftJustify);
  SIZList.Draw;
  SIZList.ExpandAll(True);
  //SIZList.CheckAll(True);
  SIZList.ShowFirst;
end;

procedure TSIZDocForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSIZDocForm.DocEdit(const AEditingType: TEditingType);
var
  DocID, DocForm: Integer;
  DocName, DocNum: String;
  DocDate: TDate;
begin
  if (AEditingType=etEdit) and (not DocList.IsSelected) then Exit;

  if DocList.IsSelected then
  begin
    DocID:= DocIDs[DocList.SelectedIndex];
    DocForm:= DocForms[DocList.SelectedIndex];
    DocName:= DocNames[DocList.SelectedIndex];
    DocNum:= DocNums[DocList.SelectedIndex];
    DocDate:= DocDates[DocList.SelectedIndex];
  end
  else begin
    DocID:= 0;
    DocForm:= 0;
    DocName:= EmptyStr;
    DocNum:= EmptyStr;
    DocDate:= 0;
  end;

  if not SIZDocEditFormOpen(AEditingType, DocType, DocID, DocName, DocNum,
                            DocDate, DocForm) then Exit;
  DocListLoad(DocID);
end;

procedure TSIZDocForm.DocAddButtonClick(Sender: TObject);
begin
  DocEdit(etAdd);
end;

procedure TSIZDocForm.DocEditButtonClick(Sender: TObject);
begin
  DocEdit(etEdit);
end;

procedure TSIZDocForm.DocVTDblClick(Sender: TObject);
begin
  DocEdit(etEdit);
end;

procedure TSIZDocForm.ViewUpdate;
begin
  DocEditButtonPanel.Visible:= EditingButton.Down;
  SIZEditButtonPanel.Visible:= EditingButton.Down;


end;

procedure TSIZDocForm.EditingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

end.

