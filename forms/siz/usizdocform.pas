unit USIZDocForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel, Spin,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZUtils,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_DateUtils, DK_Dialogs,
  //Forms
  USIZDocEditForm, USIZStoreEntryEditForm;

type

  { TSIZDocForm }

  TSIZDocForm = class(TForm)
    DividerBevel4: TDividerBevel;
    DocAddButton: TSpeedButton;
    DocDelButton: TSpeedButton;
    DocEditButton: TSpeedButton;
    DocEraseButton: TSpeedButton;
    SIZCollapseAllButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    SIZExpandAllButton: TSpeedButton;
    SIZViewButtonPanel: TPanel;
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
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure DocAddButtonClick(Sender: TObject);
    procedure DocEditButtonClick(Sender: TObject);
    procedure DocEraseButtonClick(Sender: TObject);
    procedure DocVTDblClick(Sender: TObject);
    procedure EditingButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SIZAddButtonClick(Sender: TObject);
    procedure SIZCollapseAllButtonClick(Sender: TObject);
    procedure SIZCopyButtonClick(Sender: TObject);
    procedure SIZEditButtonClick(Sender: TObject);
    procedure SIZExpandAllButtonClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    DocList: TVSTTable;
    SIZList: TVSTCategoryRadioTable;

    DocIDs, DocForms: TIntVector;
    DocNames, DocNums, DocFullNames: TStrVector;
    DocDates: TDateVector;

    CategoryNames: TStrMatrix;
    EntryIDs: TInt64Matrix;
    NomNums, SizNames, SizUnits, Notes: TStrMatrix;
    SizCounts, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes: TIntMatrix;

    procedure DocListCreate;
    procedure DocListSelect;
    procedure DocListLoad(const ASelectedID: Integer = -1);

    procedure SIZListCreate;
    procedure SIZListSelect;
    procedure SIZListLoad;

    procedure DocEdit(const AEditingType: TEditingType);

    procedure SIZStoreEntryEditFormOpen(const AEditingType: TEditingType);

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
    DocAddButton, DocDelButton, DocEditButton, DocEraseButton,
    SIZExpandAllButton, SIZCollapseAllButton,
    SIZAddButton, SIZDelButton, SIZEditButton, SIZCopyButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton,
    EditingButton,
    DocAddButton, DocDelButton, DocEditButton, DocEraseButton,
    SIZExpandAllButton, SIZCollapseAllButton,
    SIZAddButton, SIZDelButton, SIZEditButton, SIZCopyButton
  ]);

  if DocType>0 then
    Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[DocType+6];

  YearSpinEdit.Value:= YearOfDate(Date);


end;

procedure TSIZDocForm.SIZAddButtonClick(Sender: TObject);
begin
  case DocType of
    1: SIZStoreEntryEditFormOpen(etAdd);
    2: ; //!!!!
    3: ; //!!!!
    4: ; //!!!!
  end;
end;

procedure TSIZDocForm.SIZEditButtonClick(Sender: TObject);
begin
  case DocType of
    1: SIZStoreEntryEditFormOpen(etEdit);
    2: ; //!!!!
    3: ; //!!!!
    4: ; //!!!!
  end;
end;

procedure TSIZDocForm.SIZCopyButtonClick(Sender: TObject);
begin
  case DocType of
    1: SIZStoreEntryEditFormOpen(etCustom);
    2: ; //!!!!
    3: ; //!!!!
    4: ; //!!!!
  end;
end;

procedure TSIZDocForm.SIZCollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZDocForm.SIZExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
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
  SIZAddButton.Enabled:= DocDelButton.Enabled;

  SIZCaptionPanel.Caption:= '  Документ: ';
  if DocList.IsSelected then
    SIZCaptionPanel.Caption:= SIZCaptionPanel.Caption +
                              DocFullNames[DocList.SelectedIndex];

  SIZListLoad;
end;

procedure TSIZDocForm.DocListLoad(const ASelectedID: Integer);
var
  SelectedID: Integer;
begin
  SelectedID:= GetSelectedID(DocList, DocIDs, ASelectedID);

  DataBase.SIZDocListLoad(YearSpinEdit.Value, DocType, DocIDs, DocForms,
                          DocNames, DocNums, DocDates);

  DocList.Visible:= False;
  try
    DocList.ValuesClear;
    DocList.SetColumn('№ п/п', VIntToStr(VOrder(Length(DocIDs))));
    DocFullNames:= SIZDocFullName(DocNames, DocNums, DocDates);
    DocList.SetColumn('Документ', DocFullNames, taLeftJustify);
    DocList.Draw;
    DocList.ReSelect(DocIDs, SelectedID, True);  //возвращаем выделение строки
  finally
    DocList.Visible:= True;
  end;

  ExportButton.Enabled:= not VIsNil(DocIDs);
end;

procedure TSIZDocForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryRadioTable.Create(SIZVT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.RadioVisible:= False;
  SIZList.RadioEnable:= False;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Примечание', 300);
  SIZList.Draw;
end;

procedure TSIZDocForm.SIZListSelect;
begin
  SIZDelButton.Enabled:= SIZList.IsSelected;
  SIZEditButton.Enabled:= SIZDelButton.Enabled;
  SIZCopyButton.Enabled:= SIZDelButton.Enabled;
end;

procedure TSIZDocForm.SIZListLoad;
var
  i: Integer;
  M: TStrMatrix;
begin
  if not DocList.IsSelected then Exit;

  DataBase.SIZStoreEntryLoad(DocIDs[DocList.SelectedIndex],
                         EntryIDs, NomNums, SizNames, SizUnits, Notes,
                         SizCounts, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes);

  MDim(CategoryNames, Length(EntryIDs), 6);
  for i:= 0 to High(EntryIDs) do
  begin
    CategoryNames[i, 0]:= NomNums[i, 0];
    CategoryNames[i, 1]:= SizNames[i, 0];
    CategoryNames[i, 2]:= SizUnits[i, 0];
    CategoryNames[i, 3]:= IntToStr(VSum(SizCounts[i]));
  end;

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(CategoryNames);
    SIZList.SetColumn('Номенклатурный номер', NomNums, taLeftJustify);
    SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
    SIZList.SetColumn('Единица измерения', SizUnits);
    SIZList.SetColumn('Количество', MIntToStr(SizCounts));
    M:= SIZFullSize(SizeTypes, SizeIDs, HeightIDs);
    SIZList.SetColumn('Размер/объём/вес', M);
    SIZList.SetColumn('Примечание', Notes, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
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

procedure TSIZDocForm.SIZStoreEntryEditFormOpen(const AEditingType: TEditingType);
var
  SIZStoreEntryEditForm: TSIZStoreEntryEditForm;
  i, j: Integer;
begin
  if not DocList.IsSelected then Exit;
  if (AEditingType<>etAdd) and (not SIZList.IsSelected) then Exit;

  SIZStoreEntryEditForm:= TSIZStoreEntryEditForm.Create(nil);
  try
    SIZStoreEntryEditForm.DocID:= DocIDs[DocList.SelectedIndex];

    if AEditingType=etCustom then
      SIZStoreEntryEditForm.EditingType:= etAdd
    else
      SIZStoreEntryEditForm.EditingType:= AEditingType;

    if AEditingType<>etAdd then
    begin
      i:= SIZList.SelectedIndex1;
      j:= SIZList.SelectedIndex2;
      SIZStoreEntryEditForm.NomNumEdit.Text:= NomNums[i,j];
      SIZStoreEntryEditForm.NoteEdit.Text:= Notes[i,j];
      SIZStoreEntryEditForm.SizCount:= SizCounts[i,j];
      //SIZStoreEntryEditForm.CountSpinEdit.Value:= SizCounts[i,j];
      if AEditingType=etEdit then
        SIZStoreEntryEditForm.EntryID:= EntryIDs[i,j];
      SIZStoreEntryEditForm.NameID:= NameIDs[i,j];
      SIZStoreEntryEditForm.SizeID:= SizeIDs[i,j];
      SIZStoreEntryEditForm.HeightID:= HeightIDs[i,j];
    end;

    if SIZStoreEntryEditForm.ShowModal=mrOK then
      SIZListLoad;
  finally
    FreeAndNil(SIZStoreEntryEditForm);
  end;

  {EntryIDs: TInt64Matrix;
    NomNums, SizNames, SizUnits, Notes: TStrMatrix;
    SizCounts, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes: TIntMatrix;}
end;

procedure TSIZDocForm.DocAddButtonClick(Sender: TObject);
begin
  DocEdit(etAdd);
end;

procedure TSIZDocForm.DocEditButtonClick(Sender: TObject);
begin
  DocEdit(etEdit);
end;

procedure TSIZDocForm.DocEraseButtonClick(Sender: TObject);
var
  NeedReload: Boolean;
begin
  if not Confirm('Удалить все пустые документы?') then Exit;
  case DocType of
    1: NeedReload:= DataBase.SIZEmptyDocStoreEntryDelete(YearSpinEdit.Value);
    2: ; //!!!!
    3: ; //!!!!
    4: ; //!!!!
  end;

  if NeedReload then DocListLoad;
end;

procedure TSIZDocForm.DocVTDblClick(Sender: TObject);
begin
  DocEdit(etEdit);
end;

procedure TSIZDocForm.ViewUpdate;
begin
  DocToolPanel.Visible:= EditingButton.Down;
  SIZEditButtonPanel.Visible:= EditingButton.Down;
  SIZList.RadioEnable:= EditingButton.Down;
end;

procedure TSIZDocForm.EditingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

end.

