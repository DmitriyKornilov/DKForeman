unit USIZNormForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, VirtualTrees, DividerBevel, DateUtils,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UImages, USIZNormSheet, USIZTypes,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Dialogs, DK_CtrlUtils, DK_Const,
  //Forms
  USIZNormEditForm, USIZNormItemEditForm, USIZNormSubItemEditForm;

type

  { TSIZNormForm }

  TSIZNormForm = class(TForm)
    CloseButton: TSpeedButton;
    ItemCaptionPanel: TPanel;
    NormCaptionPanel: TPanel;
    ExportButton: TSpeedButton;
    ItemAddButton: TSpeedButton;
    SubItemAddButton: TSpeedButton;
    ItemCopyButton: TSpeedButton;
    ItemDelButton: TSpeedButton;
    SubItemDelButton: TSpeedButton;
    ItemEditButton: TSpeedButton;
    SubItemEditButton: TSpeedButton;
    SubItemGrid: TsWorksheetGrid;
    SubItemSheetPanel: TPanel;
    ItemToolPanel: TPanel;
    DividerBevel1: TDividerBevel;
    ItemSheetPanel: TPanel;
    SubItemDownButton: TSpeedButton;
    SubItemUpButton: TSpeedButton;
    SubItemToolPanel: TPanel;
    SubItemPanel: TPanel;
    NormAddButton: TSpeedButton;
    NormDelButton: TSpeedButton;
    NormEditButton: TSpeedButton;
    NormToolPanel: TPanel;
    NormPanel: TPanel;
    MainPanel: TPanel;
    ItemPanel: TPanel;
    Splitter1: TSplitter;
    NormVT: TVirtualStringTree;
    Splitter2: TSplitter;
    ToolPanel: TPanel;
    ItemGrid: TsWorksheetGrid;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ItemAddButtonClick(Sender: TObject);
    procedure ItemCopyButtonClick(Sender: TObject);
    procedure ItemDelButtonClick(Sender: TObject);
    procedure ItemEditButtonClick(Sender: TObject);
    procedure ItemGridDblClick(Sender: TObject);
    procedure NormAddButtonClick(Sender: TObject);
    procedure NormDelButtonClick(Sender: TObject);
    procedure NormEditButtonClick(Sender: TObject);
    procedure NormVTDblClick(Sender: TObject);
    procedure SubItemAddButtonClick(Sender: TObject);
    procedure SubItemDelButtonClick(Sender: TObject);
    procedure SubItemDownButtonClick(Sender: TObject);
    procedure SubItemEditButtonClick(Sender: TObject);
    procedure SubItemGridDblClick(Sender: TObject);
    procedure SubItemUpButtonClick(Sender: TObject);
  private
    ModeType: TModeType;

    NormList: TVSTTable;
    NormIDs: TIntVector;
    BeginDates, EndDates: TDateVector;
    NormNames, TypicalNames: TStrVector;

    NormItemSheet: TSIZNormItemSheet;
    ItemIDs, PostIDs: TIntVector;
    ItemNames, PostNames: TStrVector;

    NormSubItems: TNormSubItems;
    NormSubItemSheet: TSIZNormSubItemsSheet;

    procedure NormListCreate;
    procedure NormListLoad(const ASelectedID: Integer = -1);
    procedure NormListSelect;
    procedure NormListDelItem;
    procedure NormListEditItem;

    procedure NormItemListLoad(const ASelectedID: Integer = -1);
    procedure NormItemSelect;
    procedure NormItemDelete;
    procedure NormItemEdit;

    procedure NormSubItemListLoad(const ASelectedID: Integer = -1);
    procedure NormSubItemSelect;
    procedure NormSubItemDelete;
    procedure NormSubItemEdit;
    procedure NormSubItemSwap(const AIndex1, AIndex2: Integer);

    procedure NormButtonsEnabled;
    procedure NormItemButtonsEnabled;
    procedure NormSubItemButtonsEnabled;

    procedure SIZNormEditFormOpen(const AEditingType: TEditingType);
    procedure SIZNormItemEditFormOpen(const AEditingType: TEditingType);
    procedure SIZNormSubItemEditFormOpen(const AEditingType: TEditingType);
  public
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  SIZNormForm: TSIZNormForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZNormForm }

procedure TSIZNormForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZNormForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  SetToolPanels([
    ToolPanel, NormToolPanel, ItemToolPanel, SubItemToolPanel
  ]);
  SetCaptionPanels([
    NormCaptionPanel, ItemCaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    NormAddButton, NormDelButton, NormEditButton,
    ItemAddButton, ItemDelButton, ItemEditButton, ItemCopyButton,
    SubItemAddButton, SubItemDelButton, SubItemEditButton, SubItemUpButton, SubItemDownButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton,
    NormAddButton, NormDelButton, NormEditButton,
    ItemAddButton, ItemDelButton, ItemEditButton, ItemCopyButton,
    SubItemAddButton, SubItemDelButton, SubItemEditButton, SubItemUpButton, SubItemDownButton
  ]);

  NormListCreate;

  NormItemSheet:= TSIZNormItemSheet.Create(ItemGrid.Worksheet, ItemGrid, MainForm.GridFont);
  NormItemSheet.OnSelect:= @NormItemSelect;
  NormItemSheet.OnDelKeyDown:= @NormItemDelete;
  NormItemSheet.OnReturnKeyDown:= @NormItemEdit;
  NormItemSheet.CanUnselect:= False;

  NormSubItemSheet:= TSIZNormSubItemsSheet.Create(SubItemGrid.Worksheet, SubItemGrid, MainForm.GridFont);
  NormSubItemSheet.OnSelect:= @NormSubItemSelect;
  NormSubItemSheet.OnDelKeyDown:= @NormSubItemDelete;
  NormSubItemSheet.OnReturnKeyDown:= @NormSubItemEdit;
  NormSubItemSheet.CanUnselect:= False;
  NormSubItemSheet.AutosizeColumnDisable;
end;

procedure TSIZNormForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(NormList);
  FreeAndNil(NormItemSheet);
  FreeAndNil(NormSubItemSheet);
end;

procedure TSIZNormForm.FormShow(Sender: TObject);
begin
  NormListLoad;
end;

procedure TSIZNormForm.ItemAddButtonClick(Sender: TObject);
begin
  SIZNormItemEditFormOpen(etAdd);
end;

procedure TSIZNormForm.ItemCopyButtonClick(Sender: TObject);
begin
  SIZNormItemEditFormOpen(etCustom);
end;

procedure TSIZNormForm.ItemDelButtonClick(Sender: TObject);
begin
  NormItemDelete;
end;

procedure TSIZNormForm.ItemEditButtonClick(Sender: TObject);
begin
  NormItemEdit;
end;

procedure TSIZNormForm.ItemGridDblClick(Sender: TObject);
begin
  NormItemEdit;
end;

procedure TSIZNormForm.NormAddButtonClick(Sender: TObject);
begin
  SIZNormEditFormOpen(etAdd);
end;

procedure TSIZNormForm.NormDelButtonClick(Sender: TObject);
begin
  NormListDelItem;
end;

procedure TSIZNormForm.NormEditButtonClick(Sender: TObject);
begin
  NormListEditItem;
end;

procedure TSIZNormForm.NormVTDblClick(Sender: TObject);
begin
  NormListEditItem;
end;

procedure TSIZNormForm.SubItemAddButtonClick(Sender: TObject);
begin
  SIZNormSubItemEditFormOpen(etAdd);
end;

procedure TSIZNormForm.SubItemDelButtonClick(Sender: TObject);
begin
  NormSubItemDelete;
end;

procedure TSIZNormForm.SubItemEditButtonClick(Sender: TObject);
begin
  NormSubItemEdit;
end;

procedure TSIZNormForm.SubItemGridDblClick(Sender: TObject);
begin
  NormSubItemEdit;
end;

procedure TSIZNormForm.SubItemUpButtonClick(Sender: TObject);
begin
  NormSubItemSwap(NormSubItemSheet.SelectedIndex, NormSubItemSheet.SelectedIndex - 1);
end;

procedure TSIZNormForm.SubItemDownButtonClick(Sender: TObject);
begin
  NormSubItemSwap(NormSubItemSheet.SelectedIndex, NormSubItemSheet.SelectedIndex + 1);
end;

procedure TSIZNormForm.NormListCreate;
begin
  NormList:= TVSTTable.Create(NormVT);
  NormList.CanSelect:= True;
  NormList.CanUnselect:= False;
  NormList.OnSelect:= @NormListSelect;
  NormList.OnDelKeyDown:= @NormListDelItem;
  NormList.OnReturnKeyDown:= @NormListEditItem;
  NormList.SetSingleFont(MainForm.GridFont);
  NormList.HeaderFont.Style:= [fsBold];

  NormList.AddColumn('Период действия', 150);
  NormList.AddColumn('Нормы предприятия', 500);
  NormList.AddColumn('Типовые (отраслевые) нормы', 500);
  NormList.Draw;
end;

procedure TSIZNormForm.NormListLoad(const ASelectedID: Integer);
var
  SelectedID: Integer;
begin
  NormSubItemSheet.Clear;
  NormItemSheet.Clear;
  NormList.ValuesClear;

  SelectedID:= GetSelectedID(NormList, NormIDs, ASelectedID);

  DataBase.SIZNormsLoad(NormIDs, NormNames, TypicalNames, BeginDates, EndDates);

  NormList.Visible:= False;
  try
    NormList.SetColumn('Период действия', VPeriodToStr(BeginDates, EndDates), taLeftJustify);
    NormList.SetColumn('Нормы предприятия', NormNames, taLeftJustify);
    NormList.SetColumn('Типовые (отраслевые) нормы', TypicalNames, taLeftJustify);
    NormList.Draw;
    NormList.ReSelect(NormIDs, SelectedID, True);
  finally
    NormList.Visible:= True;
  end;

  NormItemButtonsEnabled;
  NormSubItemButtonsEnabled;

  if not VIsNil(NormIDs) then Exit;
  NormItemSheet.Draw(nil, nil, 0);
  NormSubItemSheet.Draw(nil, EmptyStr, 0);
end;

procedure TSIZNormForm.NormListSelect;
begin
  NormButtonsEnabled;
  NormItemListLoad;
end;

procedure TSIZNormForm.NormListDelItem;
begin
  if ModeType<>mtEditing then Exit;
  if not NormList.IsSelected then Exit;
  if not Confirm('Удалить всю информацию по "' +
                 NormNames[NormList.SelectedIndex] + '"?') then Exit;
  DataBase.SIZNormDelete(NormIDs[NormList.SelectedIndex]);
  NormListLoad;
end;

procedure TSIZNormForm.NormListEditItem;
begin
  if ModeType<>mtEditing then Exit;
  if not NormList.IsSelected then Exit;
  SIZNormEditFormOpen(etEdit);
end;

procedure TSIZNormForm.NormItemListLoad(const ASelectedID: Integer);
var
  SelectedID, SelectedIndex: Integer;
begin
  NormSubItemSheet.Clear;
  NormItemSheet.Clear;
  if not NormList.IsSelected then Exit;

  SelectedID:= GetSelectedID(NormItemSheet, ItemIDs, ASelectedID);

  DataBase.SIZNormItemsLoad(NormIDs[NormList.SelectedIndex],
                              ItemIDs, PostIDs, ItemNames, PostNames);

  SelectedIndex:= VIndexOf(ItemIDs, SelectedID);
  if SelectedIndex<0 then SelectedIndex:= 0;
  NormItemSheet.Draw(ItemNames, PostNames, SelectedIndex);

  NormItemButtonsEnabled;
  NormSubItemButtonsEnabled;

  if not VIsNil(ItemNames) then Exit;
  NormSubItemSheet.Draw(nil, EmptyStr, 0);
end;

procedure TSIZNormForm.NormItemSelect;
begin
  NormItemButtonsEnabled;
  NormSubItemListLoad;
end;

procedure TSIZNormForm.NormItemDelete;
begin
  if not NormItemSheet.IsSelected then Exit;
  if not Confirm('Удалить всю информацию по пункту "' +
                 ItemNames[NormItemSheet.SelectedIndex] + '"?') then Exit;
  DataBase.SIZNormItemDelete(ItemIDs[NormItemSheet.SelectedIndex]);
  NormItemListLoad;
end;

procedure TSIZNormForm.NormItemEdit;
begin
  if not NormItemSheet.IsSelected then Exit;
  SIZNormItemEditFormOpen(etEdit);
end;

procedure TSIZNormForm.NormSubItemListLoad(const ASelectedID: Integer);
var
  SelectedIndex: Integer;
begin
  NormSubItemSheet.Clear;
  if not NormItemSheet.IsSelected then Exit;

  NormSubItemsDel(NormSubItems, 0, High(NormSubItems));
  DataBase.SIZNormSubItemsLoad(ItemIDs[NormItemSheet.SelectedIndex], NormSubItems);

  if ASelectedID>0 then
    SelectedIndex:= NormSubItemsIndexOf(NormSubItems, ASelectedID)
  else
    SelectedIndex:= NormSubItemSheet.SelectedIndex;
  if SelectedIndex<0 then SelectedIndex:= 0;

  NormSubItemSheet.Draw(NormSubItems, ItemNames[NormItemSheet.SelectedIndex], SelectedIndex);
end;

procedure TSIZNormForm.NormSubItemSelect;
begin
  NormSubItemButtonsEnabled;
end;

procedure TSIZNormForm.NormSubItemDelete;
var
  SubItem: TNormSubItem;
begin
  if not NormSubItemSheet.IsSelected then Exit;
  if not Confirm('Удалить всю информацию по строке?') then Exit;

  SubItem:= NormSubItems[NormSubItemSheet.SelectedIndex];
  DataBase.SIZNormSubItemDelete(ItemIDs[NormItemSheet.SelectedIndex],
                     SubItem.SubItemID, SubItem.ReasonID, SubItem.OrderNum );
  NormSubItemListLoad;
end;

procedure TSIZNormForm.NormSubItemEdit;
begin
  if not NormSubItemSheet.IsSelected then Exit;
  SIZNormSubItemEditFormOpen(etEdit);
end;

procedure TSIZNormForm.NormSubItemSwap(const AIndex1, AIndex2: Integer);
begin
  if not DataBase.SIZNormSubItemSwap(NormSubItems[AIndex1].SubItemID,
                                     NormSubItems[AIndex1].OrderNum,
                                     NormSubItems[AIndex2].SubItemID,
                                     NormSubItems[AIndex2].OrderNum) then Exit;
  NormSubItemsSwap(NormSubItems, AIndex1, AIndex2);
  NormSubItemSheet.Draw(NormSubItems, ItemNames[NormItemSheet.SelectedIndex], AIndex2);
end;

procedure TSIZNormForm.NormButtonsEnabled;
begin
  NormDelButton.Enabled:= NormList.IsSelected;
  NormEditButton.Enabled:= NormList.IsSelected;
  ItemAddButton.Enabled:= NormList.IsSelected;
end;

procedure TSIZNormForm.NormItemButtonsEnabled;
begin
  ItemDelButton.Enabled:= NormItemSheet.IsSelected;
  ItemEditButton.Enabled:= NormItemSheet.IsSelected;
  ItemCopyButton.Enabled:= NormItemSheet.IsSelected;
  SubItemAddButton.Enabled:= NormItemSheet.IsSelected;
end;

procedure TSIZNormForm.NormSubItemButtonsEnabled;
begin
  SubItemDelButton.Enabled:= NormSubItemSheet.IsSelected;
  SubItemEditButton.Enabled:= NormSubItemSheet.IsSelected;
  SubItemUpButton.Enabled:= NormSubItemSheet.CanUp;
  SubItemDownButton.Enabled:= NormSubItemSheet.CanDown;
end;

procedure TSIZNormForm.SIZNormEditFormOpen(const AEditingType: TEditingType);
var
  SIZNormEditForm: TSIZNormEditForm;
begin
  SIZNormEditForm:= TSIZNormEditForm.Create(nil);
  try

    SIZNormEditForm.EditingType:= AEditingType;
    if AEditingType=etEdit then
    begin
      SIZNormEditForm.NormID:= NormIDs[NormList.SelectedIndex];
      SIZNormEditForm.NormNameEdit.Text:= NormNames[NormList.SelectedIndex];
      SIZNormEditForm.TypicalNameEdit.Text:= TypicalNames[NormList.SelectedIndex];
      SIZNormEditForm.BeginDatePicker.Date:= BeginDates[NormList.SelectedIndex];
      SIZNormEditForm.EndDatePicker.Date:= EndDates[NormList.SelectedIndex];
      SIZNormEditForm.InfEndDateCheckBox.Checked:=
                            SameDate(EndDates[NormList.SelectedIndex], INFDATE);
    end;

    if SIZNormEditForm.ShowModal=mrOK then
      NormListLoad(SIZNormEditForm.NormID);

  finally
    FreeAndNil(SIZNormEditForm);
  end;
end;

procedure TSIZNormForm.SIZNormItemEditFormOpen(const AEditingType: TEditingType);
var
  SIZNormItemEditForm: TSIZNormItemEditForm;
begin
  SIZNormItemEditForm:= TSIZNormItemEditForm.Create(nil);
  try

    SIZNormItemEditForm.EditingType:= AEditingType;
    SIZNormItemEditForm.NormID:= NormIDs[NormList.SelectedIndex];

    if AEditingType<>etAdd then
    begin
      SIZNormItemEditForm.ItemID:= ItemIDs[NormItemSheet.SelectedIndex];
      SIZNormItemEditForm.ItemNameEdit.Text:= ItemNames[NormItemSheet.SelectedIndex];
    end;

    if SIZNormItemEditForm.ShowModal=mrOK then
      NormItemListLoad(SIZNormItemEditForm.ItemID);

  finally
    FreeAndNil(SIZNormItemEditForm);
  end;
end;

procedure TSIZNormForm.SIZNormSubItemEditFormOpen(const AEditingType: TEditingType);
var
  SIZNormSubItemEditForm: TSIZNormSubItemEditForm;
begin
  SIZNormSubItemEditForm:= TSIZNormSubItemEditForm.Create(nil);
  try
    SIZNormSubItemEditForm.EditingType:= AEditingType;
    SIZNormSubItemEditForm.ItemID:= ItemIDs[NormItemSheet.SelectedIndex];

    if AEditingType=etAdd then
      NormSubItemClear(SIZNormSubItemEditForm.SubItem)
    else
      SIZNormSubItemEditForm.SubItem:= NormSubItems[NormSubItemSheet.SelectedIndex];

    if SIZNormSubItemEditForm.ShowModal=mrOK then
      NormSubItemListLoad(SIZNormSubItemEditForm.SubItem.SubItemID);

  finally
    FreeAndNil(SIZNormSubItemEditForm);
  end;
end;

procedure TSIZNormForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;

  NormToolPanel.Visible:= ModeType=mtEditing;
  ItemToolPanel.Visible:= ModeType=mtEditing;
  SubItemToolPanel.Visible:= ModeType=mtEditing;

  NormSubItemSheet.CanSelect:= ModeType=mtEditing;
  if ModeType=mtEditing then
  begin
    if Length(NormSubItems)>0 then
      NormSubItemSheet.SetSelection(2, 1);
  end
  else
    NormSubItemSheet.DelSelection;
end;

end.

