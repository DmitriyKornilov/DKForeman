unit USIZDocForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel,
  //Project utils
  UVars, UConst, UTypes,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_CtrlUtils;

type

  { TSIZDocForm }

  TSIZDocForm = class(TForm)
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
    procedure CloseButtonClick(Sender: TObject);
    procedure EditingButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

    procedure ViewUpdate;
  public
    DocType: Byte;
  end;

var
  SIZDocForm: TSIZDocForm;

  procedure SIZDocFormOpen(const ADocType: Byte);

implementation

procedure SIZDocFormOpen(const ADocType: Byte);
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
end;

procedure TSIZDocForm.FormDestroy(Sender: TObject);
begin



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
end;

procedure TSIZDocForm.CloseButtonClick(Sender: TObject);
begin
  Close;
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

