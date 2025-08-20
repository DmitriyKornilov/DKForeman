unit USIZDocReceivingForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel,
  //Project utils
  UVars, UConst, UUtils,
  //DK packages utils
  DK_VSTCategoryTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs,
  //Forms
  USIZDocMB7Form;

type

  { TSIZDocReceivingForm }

  TSIZDocReceivingForm = class(TForm)
    CheckAllButton: TSpeedButton;
    CheckButtonPanel: TPanel;
    CollapseAllButton: TSpeedButton;
    DelButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    MB7Button: TSpeedButton;
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
    procedure MB7ButtonClick(Sender: TObject);
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

    procedure SetButtonsEnabled(const AEnabled: Boolean);

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
    MB7Button,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton, DelButton
  ]);
end;

procedure TSIZDocReceivingForm.MB7ButtonClick(Sender: TObject);
begin
  SIZDocMB7FormOpen(DocID, False{выдача});
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
  SIZList.AutosizeColumnEnable('Наименование');
  SIZList.Draw;
end;

procedure TSIZDocReceivingForm.SIZListLoad;
begin
  if DocID<=0 then
  begin
    SIZList.ValuesClear;
    SetButtonsEnabled(False);
    Exit;
  end;

  DataBase.SIZStoreReceivingLoad(DocID, Fs, Ns, Ps, TabNums, PostNames,
                                 StoreIDs, SizCounts, SizDigUnits,
                                 NomNums, SizNames, SizStrUnits, SizLifes,
                                 ReceivingDates);
  SIZListShow;
  SetButtonsEnabled(not MIsNil(StoreIDs));
end;

procedure TSIZDocReceivingForm.SIZListShow;
var
  StaffNames: TStrVector;
begin
  StaffNames:= StaffFullName(Fs, Ns, Ps, TabNums, PostNames, True{short});

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

procedure TSIZDocReceivingForm.SetButtonsEnabled(const AEnabled: Boolean);
begin
  ExpandAllButton.Enabled:= AEnabled;
  CollapseAllButton.Enabled:= AEnabled;
  CheckAllButton.Enabled:= AEnabled;
  UncheckAllButton.Enabled:= AEnabled;
  MB7Button.Enabled:= AEnabled;
end;

procedure TSIZDocReceivingForm.ViewUpdate(const AIsEditing: Boolean);
begin
  IsEditing:= AIsEditing;
  EditButtonPanel.Visible:= AIsEditing;
  CheckButtonPanel.Visible:= AIsEditing;
  if AIsEditing then
    SIZList.CheckKind:= chkCategory
  else
    SIZList.CheckKind:= chkNone;
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

