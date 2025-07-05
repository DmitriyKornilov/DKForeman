unit USIZStoreHistoryForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel,
  //Project utils
  UVars, UConst, UTypes,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_CtrlUtils;

type

  { TSIZStoreHistoryForm }

  TSIZStoreHistoryForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    CheckAllButton: TSpeedButton;
    CollapseAllButton: TSpeedButton;
    ExpandAllButton: TSpeedButton;
    UncheckAllButton: TSpeedButton;
    DocViewButtonPanel: TPanel;
    VT: TVirtualStringTree;
    ExportButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  SIZStoreHistoryForm: TSIZStoreHistoryForm;

implementation

{$R *.lfm}

{ TSIZStoreHistoryForm }

procedure TSIZStoreHistoryForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[10];
end;

procedure TSIZStoreHistoryForm.FormDestroy(Sender: TObject);
begin

end;

procedure TSIZStoreHistoryForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CloseButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton
  ]);
end;

procedure TSIZStoreHistoryForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

