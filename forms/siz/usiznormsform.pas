unit USIZNormsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, VirtualTrees, DividerBevel,
  //Project utils
  UDataBase, UConst, UTypes, UUtils, UImages, USIZNormSheet, USIZTypes,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Dialogs, DK_CtrlUtils;

type

  { TSIZNormsForm }

  TSIZNormsForm = class(TForm)
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
    procedure NormDelButtonClick(Sender: TObject);
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
    procedure NormListLoad(const SelectedID: Integer = -1);
    procedure NormListSelect;
    procedure NormListDelItem;

    procedure NormItemLoad;
    procedure NormItemSelect;

    procedure NormSubItemLoad;
    procedure NormSubItemSelect;
  public
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  SIZNormsForm: TSIZNormsForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZNormsForm }

procedure TSIZNormsForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZNormsForm.FormCreate(Sender: TObject);
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
  NormItemSheet.CanUnselect:= False;

  NormSubItemSheet:= TSIZNormSubItemsSheet.Create(SubItemGrid.Worksheet, SubItemGrid, MainForm.GridFont);
  NormSubItemSheet.OnSelect:=@NormSubItemSelect;
  NormSubItemSheet.AutosizeColumnDisable;
end;

procedure TSIZNormsForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(NormList);
  FreeAndNil(NormItemSheet);
  FreeAndNil(NormSubItemSheet);
end;

procedure TSIZNormsForm.FormShow(Sender: TObject);
begin
  NormListLoad;
end;

procedure TSIZNormsForm.NormDelButtonClick(Sender: TObject);
begin
  NormListDelItem;
end;

procedure TSIZNormsForm.NormListCreate;
begin
  NormList:= TVSTTable.Create(NormVT);
  NormList.CanSelect:= True;
  NormList.CanUnselect:= False;
  NormList.OnSelect:= @NormListSelect;
  NormList.OnDelKeyDown:= @NormListDelItem;
  NormList.SetSingleFont(MainForm.GridFont);
  NormList.HeaderFont.Style:= [fsBold];

  NormList.AddColumn('Период действия', 150);
  NormList.AddColumn('Нормы предприятия', 500);
  NormList.AddColumn('Типовые (отраслевые) нормы', 500);
  NormList.Draw;
end;

procedure TSIZNormsForm.NormListLoad(const SelectedID: Integer);
var
  SelectedNormID: Integer;
begin
  SelectedNormID:= GetSelectedID(NormList, NormIDs, SelectedID);

  DataBase.SIZNormsLoad(NormIDs, NormNames, TypicalNames, BeginDates, EndDates);

  NormList.Visible:= False;
  try
    NormList.ValuesClear;
    NormList.SetColumn('Период действия', VPeriodToStr(BeginDates, EndDates), taLeftJustify);
    NormList.SetColumn('Нормы предприятия', NormNames, taLeftJustify);
    NormList.SetColumn('Типовые (отраслевые) нормы', TypicalNames, taLeftJustify);
    NormList.Draw;
    NormList.ReSelect(NormIDs, SelectedNormID, True);
  finally
    NormList.Visible:= True;
  end;
end;

procedure TSIZNormsForm.NormListSelect;
begin
  NormDelButton.Enabled:= NormList.IsSelected;
  NormEditButton.Enabled:= NormList.IsSelected;
  ItemAddButton.Enabled:= NormList.IsSelected;
  NormItemLoad;
end;

procedure TSIZNormsForm.NormListDelItem;
begin
  if not NormList.IsSelected then Exit;
  if not Confirm('Удалить всю информацию по "' +
                 NormNames[NormList.SelectedIndex] + '"?') then Exit;
  DataBase.SIZNormDelete(NormIDs[NormList.SelectedIndex]);
  NormListLoad;
end;

procedure TSIZNormsForm.NormItemLoad;
begin
  if not NormList.IsSelected then Exit;

  DataBase.SIZNormItemsLoad(NormIDs[NormList.SelectedIndex],
                            ItemIDs, PostIDs, ItemNames, PostNames);
  NormItemSheet.Draw(ItemNames, PostNames, 0);
end;

procedure TSIZNormsForm.NormItemSelect;
begin
  ItemDelButton.Enabled:= NormItemSheet.IsSelected;
  ItemEditButton.Enabled:= NormItemSheet.IsSelected;
  ItemCopyButton.Enabled:= NormItemSheet.IsSelected;
  SubItemAddButton.Enabled:= NormItemSheet.IsSelected;
  NormSubItemLoad;
end;

procedure TSIZNormsForm.NormSubItemLoad;
begin
  if not NormItemSheet.IsSelected then Exit;

  NormSubItemsDel(NormSubItems, 0, High(NormSubItems));
  if NormItemSheet.IsSelected then
    NormSubItemsLoad(ItemIDs[NormItemSheet.SelectedIndex], NormSubItems);
  NormSubItemSheet.Draw(NormSubItems, ItemNames[NormItemSheet.SelectedIndex], 0);
end;

procedure TSIZNormsForm.NormSubItemSelect;
begin
  SubItemDelButton.Enabled:= NormSubItemSheet.IsSelected;
  SubItemEditButton.Enabled:= NormSubItemSheet.IsSelected;
  SubItemUpButton.Enabled:= NormSubItemSheet.CanUpSelection;
  SubItemDownButton.Enabled:= NormSubItemSheet.CanDownSelection;
end;

procedure TSIZNormsForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;

  NormToolPanel.Visible:= ModeType=mtEditing;
  ItemToolPanel.Visible:= ModeType=mtEditing;
  SubItemToolPanel.Visible:= ModeType=mtEditing;

  NormSubItemSheet.CanSelect:= ModeType=mtEditing;
  NormSubItemSheet.CanUnselect:= ModeType<>mtEditing;
  if ModeType=mtEditing then
    NormSubItemSheet.SetSelection(2, 1) //!!!
  else
    NormSubItemSheet.DelSelection;
end;

end.

