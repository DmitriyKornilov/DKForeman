unit USIZDocReturningForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel,
  //Project utils
  UVars, UConst,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs, DK_StrUtils;

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
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
  private
    DocID: Integer;
    IsEditing: Boolean;
    SIZList: TVSTCategoryCheckTable;

    procedure SIZListCreate;
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
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckKind:= chkNone;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Ф.И.О.', 150);
  SIZList.AddColumn('Табельный номер', 150);
  SIZList.AddColumn('Наименование', 200);
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Срок службы', 150);
  SIZList.AddColumn('Документ выдачи', 200);
  SIZList.Draw;
end;

procedure TSIZDocReturningForm.SIZListLoad;
begin

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
    SIZList.CheckKind:= chkAll
  else
    SIZList.CheckKind:= chkNone;
  //SIZListShow;
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

end.

