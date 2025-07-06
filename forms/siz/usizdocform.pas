unit USIZDocForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel, Spin,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZUtils,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_CtrlUtils, DK_DateUtils,
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
    procedure EditingButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    DocList: TVSTTable;

    DocIDs, DocForms: TIntVector;
    DocNames, DocNums: TStrVector;
    DocDates: TDateVector;

    procedure DocListCreate;
    procedure DocListSelect;
    procedure DocListLoad(const ASelectedID: Integer = -1);

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
  DocListCreate;
end;

procedure TSIZDocForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocList);


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

procedure TSIZDocForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSIZDocForm.DocAddButtonClick(Sender: TObject);
var
  DocID: Integer;
begin
  DocID:= 0; //!!!!!!!!!!!!
  if DocList.IsSelected then
    DocID:= DocIDs[DocList.SelectedIndex];

  if not SIZDocEditFormOpen(etAdd, DocType, DocID{, ...!!!!}) then Exit;
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

