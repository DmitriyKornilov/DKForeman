unit USIZDocReturningForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel,
  //Project utils
  UVars, UConst, UTimingUtils, USIZUtils,
  //DK packages utils
  DK_VSTCategoryTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs;

type

  { TSIZDocReturningForm }

  TSIZDocReturningForm = class(TForm)
    CheckAllButton: TSpeedButton;
    CheckButtonPanel: TPanel;
    CollapseAllButton: TSpeedButton;
    DelButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    EditButtonPanel: TPanel;
    ExpandAllButton: TSpeedButton;
    ToolButtonPanel: TPanel;
    ToolPanel: TPanel;
    UncheckAllButton: TSpeedButton;
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

    Fs, Ns, Ps, TabNums, PostNames: TStrVector;
    StoreIDs: TInt64Matrix3D;
    SizCounts, SizDigUnits: TIntMatrix;
    NomNums, SizNames, SizStrUnits, SizLifes: TStrMatrix;
    ReceivingDocNames, ReceivingDocNums, Notes: TStrMatrix;
    ReceivingDates: TDateMatrix;

    procedure SIZListCreate;
    procedure SIZListShow;
    procedure SIZListLoad;
    procedure SIZListSelect;

  public
    procedure ViewUpdate(const AIsEditing: Boolean);
    procedure DocChange(const ADocID: Integer);
  end;

var
  SIZDocReturningForm: TSIZDocReturningForm;

implementation

{$R *.lfm}

{ TSIZDocReturningForm }

procedure TSIZDocReturningForm.FormCreate(Sender: TObject);
begin
  DocID:= 0;
  IsEditing:= False;
  SIZListCreate;
end;

procedure TSIZDocReturningForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZList);
end;

procedure TSIZDocReturningForm.FormShow(Sender: TObject);
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

procedure TSIZDocReturningForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.Span:= True;
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckKind:= chkNone;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Наименование', 200);
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Документ выдачи', 200);
  SIZList.AddColumn('Примечание', 100);
  SIZList.AutosizeColumnEnable('Наименование');
  SIZList.Draw;
end;

procedure TSIZDocReturningForm.SIZListShow;
var
  StaffNames: TStrVector;
  DocNames: TStrMatrix;
begin
  StaffNames:= StaffNamesForPersonalTiming(Fs, Ns, Ps, TabNums, PostNames);
  DocNames:= SIZDocFullName(ReceivingDocNames, ReceivingDocNums, ReceivingDates);

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(StaffNames);
    SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
    SIZList.SetColumn('Номенклатурный номер', NomNums);
    SIZList.SetColumn('Единица измерения', SizStrUnits);
    SIZList.SetColumn('Количество', MIntToStr(SizCounts));
    SIZList.SetColumn('Дата выдачи', MFormatDateTime('dd.mm.yyyy', ReceivingDates));
    SIZList.SetColumn('Документ выдачи', DocNames, taLeftJustify);
    MChangeIf(Notes, EmptyStr, ' ');
    SIZList.SetColumn('Примечание', Notes, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZDocReturningForm.SIZListLoad;
begin
  if DocID<=0 then
  begin
    SIZList.ValuesClear;
    ExpandAllButton.Enabled:= False;
    CollapseAllButton.Enabled:= False;
    CheckAllButton.Enabled:= False;
    UncheckAllButton.Enabled:= False;
    Exit;
  end;

  DataBase.SIZStoreReturningLoad(DocID, Fs, Ns, Ps, TabNums, PostNames,
                                 StoreIDs, SizCounts, SizDigUnits,
                                 NomNums, SizNames, SizStrUnits, SizLifes,
                                 ReceivingDocNames, ReceivingDocNums, Notes,
                                 ReceivingDates);

  SIZListShow;

  ExpandAllButton.Enabled:= not MIsNil(StoreIDs);
  CollapseAllButton.Enabled:= ExpandAllButton.Enabled;
  CheckAllButton.Enabled:= ExpandAllButton.Enabled;
  UncheckAllButton.Enabled:= ExpandAllButton.Enabled;
end;

procedure TSIZDocReturningForm.SIZListSelect;
begin
  DelButton.Enabled:= SIZList.IsSelected;
end;

procedure TSIZDocReturningForm.ViewUpdate(const AIsEditing: Boolean);
begin
  IsEditing:= AIsEditing;
  EditButtonPanel.Visible:= AIsEditing;
  CheckButtonPanel.Visible:= AIsEditing;
  if AIsEditing then
    SIZList.CheckKind:= chkCategory
  else
    SIZList.CheckKind:= chkNone;
end;

procedure TSIZDocReturningForm.DocChange(const ADocID: Integer);
begin
  DocID:= ADocID;
  SIZListLoad;
end;

procedure TSIZDocReturningForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

procedure TSIZDocReturningForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZDocReturningForm.CheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZDocReturningForm.UncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZDocReturningForm.DelButtonClick(Sender: TObject);
var
  DelStoreIDs: TInt64Vector;
begin
  if not Confirm('Отменить возврат выбранных СИЗ?') then Exit;
  DelStoreIDs:= MToVector(StoreIDs, SIZList.Selected);
  if DataBase.SIZStoreReturningCancel(DelStoreIDs) then
    SIZListLoad;
end;

end.

