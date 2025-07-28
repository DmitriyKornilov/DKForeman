unit USIZDocStoreEntryForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  DividerBevel, Buttons,
  //Project utils
  UVars, UConst, UTypes, USIZUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs, DK_VSTCategoryTables,
  //Forms
  USIZStoreEntryEditForm;

type

  { TSIZDocStoreEntryForm }

  TSIZDocStoreEntryForm = class(TForm)
    AddButton: TSpeedButton;
    CollapseAllButton: TSpeedButton;
    CopyButton: TSpeedButton;
    DelButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    EditButton: TSpeedButton;
    EditButtonPanel: TPanel;
    ExpandAllButton: TSpeedButton;
    ToolPanel: TPanel;
    ViewButtonPanel: TPanel;
    VT: TVirtualStringTree;
    procedure AddButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VTDblClick(Sender: TObject);
  private
    DocID: Integer;
    IsEditing: Boolean;

    SIZList: TVSTCategoryRadioTable;

    CategoryNames: TStrMatrix;
    EntryIDs: TInt64Matrix;
    NomNums, SizNames, SizUnits, Notes: TStrMatrix;
    SizCounts, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes: TIntMatrix;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListSelect;
    procedure SIZListEdit;

    procedure SIZStoreEntryEditFormOpen(const AEditingType: TEditingType);
  public
    procedure ViewUpdate(const AIsEditing: Boolean);
    procedure DocChange(const ADocID: Integer);
  end;

var
  SIZDocStoreEntryForm: TSIZDocStoreEntryForm;

implementation

{$R *.lfm}

{ TSIZDocStoreEntryForm }

procedure TSIZDocStoreEntryForm.FormCreate(Sender: TObject);
begin
  DocID:= 0;
  SIZListCreate;
end;

procedure TSIZDocStoreEntryForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZList);
end;

procedure TSIZDocStoreEntryForm.FormShow(Sender: TObject);
begin
   SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    ExpandAllButton, CollapseAllButton,
    AddButton, DelButton, EditButton, CopyButton
  ]);

  Images.ToButtons([
    ExpandAllButton, CollapseAllButton,
    AddButton, DelButton, EditButton, CopyButton
  ]);
end;

procedure TSIZDocStoreEntryForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryRadioTable.Create(VT);
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
  SIZList.AddColumn('Примечание', 150);
  SIZList.AutosizeColumnEnable('Наименование');
  SIZList.Draw;
end;

procedure TSIZDocStoreEntryForm.SIZListLoad;
var
  i: Integer;
  M: TStrMatrix;
begin
  if DocID<=0 then
  begin
    SIZList.ValuesClear;
    ExpandAllButton.Enabled:= False;
    CollapseAllButton.Enabled:= False;
    Exit;
  end;

  DataBase.SIZStoreEntryLoad(DocID, EntryIDs, NomNums, SizNames, SizUnits,
                             Notes, SizCounts, SizTypes, NameIDs, SizeIDs,
                             HeightIDs, SizeTypes);

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

  if IsEditing and (not MIsNil(EntryIDs)) then
    SIZList.Select(0, 0);

  ExpandAllButton.Enabled:= not MIsNil(EntryIDs);
  CollapseAllButton.Enabled:= ExpandAllButton.Enabled;
end;

procedure TSIZDocStoreEntryForm.SIZListSelect;
begin
  DelButton.Enabled:= SIZList.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;
  CopyButton.Enabled:= DelButton.Enabled;
end;

procedure TSIZDocStoreEntryForm.SIZListEdit;
begin
  SIZStoreEntryEditFormOpen(etEdit);
end;

procedure TSIZDocStoreEntryForm.SIZStoreEntryEditFormOpen(const AEditingType: TEditingType);
var
  SIZStoreEntryEditForm: TSIZStoreEntryEditForm;
  i, j: Integer;
begin
  if DocID<=0 then Exit;
  if (AEditingType<>etAdd) and (not SIZList.IsSelected) then Exit;

  SIZStoreEntryEditForm:= TSIZStoreEntryEditForm.Create(nil);
  try
    SIZStoreEntryEditForm.DocID:= DocID;

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
end;

procedure TSIZDocStoreEntryForm.ViewUpdate(const AIsEditing: Boolean);
begin
  IsEditing:= AIsEditing;

  EditButtonPanel.Visible:= IsEditing;
  SIZList.RadioEnable:= IsEditing;

  if IsEditing and (not MIsNil(EntryIDs)) then
    SIZList.Select(0, 0);
end;

procedure TSIZDocStoreEntryForm.DocChange(const ADocID: Integer);
begin
  DocID:= ADocID;
  AddButton.Enabled:= DocID>0;
  SIZListLoad;
end;

procedure TSIZDocStoreEntryForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

procedure TSIZDocStoreEntryForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZDocStoreEntryForm.AddButtonClick(Sender: TObject);
begin
  SIZStoreEntryEditFormOpen(etAdd);
end;

procedure TSIZDocStoreEntryForm.EditButtonClick(Sender: TObject);
begin
  SIZListEdit;
end;

procedure TSIZDocStoreEntryForm.VTDblClick(Sender: TObject);
begin
  SIZListEdit;
end;

procedure TSIZDocStoreEntryForm.CopyButtonClick(Sender: TObject);
begin
  SIZStoreEntryEditFormOpen(etCustom);
end;

procedure TSIZDocStoreEntryForm.DelButtonClick(Sender: TObject);
var
  DelEntryID: Int64;
begin
  if not Confirm('Удалить СИЗ из документа?') then Exit;
  DelEntryID:= EntryIDs[SIZList.SelectedIndex1, SIZList.SelectedIndex2];
  if DataBase.SIZStoreEntryDelete(DelEntryID) then
    SIZListLoad;
end;

end.

