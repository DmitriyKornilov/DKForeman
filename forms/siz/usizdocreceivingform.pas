unit USIZDocReceivingForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel,
  //Project utils
  UVars, UConst, UTimingUtils,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs, DK_StrUtils;

type

  { TSIZDocReceivingForm }

  TSIZDocReceivingForm = class(TForm)
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
    ReceivingDates: TDateMatrix;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListShow;
    procedure SIZListSelect;

  public
    procedure ViewUpdate(const AIsEditing: Boolean);
    procedure DocChange(const ADocID: Integer);
  end;

var
  SIZDocReceivingForm: TSIZDocReceivingForm;

implementation

{$R *.lfm}

{ TSIZDocReceivingForm }

procedure TSIZDocReceivingForm.FormCreate(Sender: TObject);
begin
  DocID:= 0;
  IsEditing:= False;
  SIZListCreate;
end;

procedure TSIZDocReceivingForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZList);
end;

procedure TSIZDocReceivingForm.FormShow(Sender: TObject);
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

procedure TSIZDocReceivingForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.Span:= True;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckKind:= chkNone;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Наименование', 500);
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 120);
  SIZList.AddColumn('Дата выдачи', 120);
  SIZList.AddColumn('Срок службы', 150);
  //SIZList.AutosizeColumnEnable('Наименование');
  SIZList.AutosizeColumnDisable;
  SIZList.Draw;
end;

procedure TSIZDocReceivingForm.SIZListLoad;
begin
  if DocID<=0 then
  begin
    SIZList.ValuesClear;
    Exit;
  end;
  DataBase.SIZStoreReceivingLoad(DocID, Fs, Ns, Ps, TabNums, PostNames,
                                 StoreIDs, SizCounts, SizDigUnits,
                                 NomNums, SizNames, SizStrUnits, SizLifes,
                                 ReceivingDates);

  SIZListShow;

  ExpandAllButton.Enabled:= not MIsNil(StoreIDs);
  CollapseAllButton.Enabled:= ExpandAllButton.Enabled;
end;

procedure TSIZDocReceivingForm.SIZListShow;
var
  StaffNames: TStrVector;
begin
  StaffNames:= StaffNamesForPersonalTiming(Fs, Ns, Ps, TabNums, PostNames);

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(StaffNames);
    SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
    SIZList.SetColumn('Номенклатурный номер', NomNums);
    SIZList.SetColumn('Единица измерения', SizStrUnits);
    SIZList.SetColumn('Количество', MIntToStr(SizCounts));
    SIZList.SetColumn('Дата выдачи', MFormatDateTime('dd.mm.yyyy', ReceivingDates));
    SIZList.SetColumn('Срок службы', SizLifes);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZDocReceivingForm.SIZListSelect;
begin
  DelButton.Enabled:= SIZList.IsSelected;
end;

procedure TSIZDocReceivingForm.ViewUpdate(const AIsEditing: Boolean);
begin
  IsEditing:= AIsEditing;
  EditButtonPanel.Visible:= AIsEditing;
  CheckButtonPanel.Visible:= AIsEditing;
  if AIsEditing then
    SIZList.CheckKind:= chkAll
  else
    SIZList.CheckKind:= chkNone;
  //SIZListShow;
end;

procedure TSIZDocReceivingForm.DocChange(const ADocID: Integer);
begin
  DocID:= ADocID;
  SIZListLoad;
end;

procedure TSIZDocReceivingForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

procedure TSIZDocReceivingForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZDocReceivingForm.CheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZDocReceivingForm.UncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZDocReceivingForm.DelButtonClick(Sender: TObject);
var
  DelStoreIDs: TInt64Vector;
begin
  if not Confirm('Отменить выдачу выбранных СИЗ?') then Exit;
  DelStoreIDs:= MToVector(StoreIDs, SIZList.Selected);
  if DataBase.SIZStoreReceivingCancel(DelStoreIDs) then
    SIZListLoad;
end;

end.

