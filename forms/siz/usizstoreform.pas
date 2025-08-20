unit USIZStoreForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, ColorSpeedButton, DividerBevel,
  //Project utils
  UVars, UConst, UTypes,
  //DK packages utils
  DK_CtrlUtils,
  //Forms
  USIZDocForm, USIZStoreHistoryForm, USIZStoreAvaliabilityForm;

type

  { TSIZStoreForm }

  TSIZStoreForm = class(TForm)
    EntryTabButton: TColorSpeedButton;
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    AvaliabilityTabButton: TColorSpeedButton;
    HistoryButton: TSpeedButton;
    ExportButton: TSpeedButton;
    FormPanel: TPanel;
    ReturningTabButton: TColorSpeedButton;
    WriteoffTabButton: TColorSpeedButton;
    ViewButtonPanel: TPanel;
    ToolPanel: TPanel;
    ReceivingTabButton: TColorSpeedButton;

    procedure AvaliabilityTabButtonClick(Sender: TObject);
    procedure EntryTabButtonClick(Sender: TObject);
    procedure ReceivingTabButtonClick(Sender: TObject);
    procedure ReturningTabButtonClick(Sender: TObject);
    procedure WriteoffTabButtonClick(Sender: TObject);

    procedure CloseButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
  private
    ModeType: TModeType;
    Category: Integer;
    CategoryForm: TForm;
    procedure CategorySelect(const ACategory: Integer);
    procedure DataExport;
  public
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  SIZStoreForm: TSIZStoreForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZStoreForm }

procedure TSIZStoreForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZStoreForm.FormCreate(Sender: TObject);
begin
  Category:= -1;
  ModeType:= mtView;
end;

procedure TSIZStoreForm.FormDestroy(Sender: TObject);
begin
  if Assigned(CategoryForm) then FreeAndNil(CategoryForm);
end;

procedure TSIZStoreForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CloseButton
  ]);

  Images.ToButtons([
    ExportButton, HistoryButton,
    CloseButton
  ]);

  AvaliabilityTabButton.Width:= EntryTabButton.Width;
  WriteoffTabButton.Width:= EntryTabButton.Width;
  ReceivingTabButton.Width:= EntryTabButton.Width;
  ReturningTabButton.Width:= EntryTabButton.Width;

  CategorySelect(0);
end;

procedure TSIZStoreForm.HistoryButtonClick(Sender: TObject);
begin
  FormModalShow(TSIZStoreHistoryForm);
end;

procedure TSIZStoreForm.DataExport;
begin
  if Category=0 then
    (CategoryForm as TSIZStoreAvaliabilityForm).DataExport
  else if Category in [1..4] then
    (CategoryForm as TSIZDocForm).DataExport;
end;

procedure TSIZStoreForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;
  if Category=0 then
    (CategoryForm as TSIZStoreAvaliabilityForm).ViewUpdate(AModeType)
  else if Category in [1..4] then
    (CategoryForm as TSIZDocForm).ViewUpdate(AModeType);
end;

procedure TSIZStoreForm.DataUpdate;
begin
  if Category=0 then
    (CategoryForm as TSIZStoreAvaliabilityForm).DataUpdate
  else if Category in [1..4] then
    (CategoryForm as TSIZDocForm).DataUpdate;
end;

procedure TSIZStoreForm.AvaliabilityTabButtonClick(Sender: TObject);
begin
  CategorySelect(0);
end;

procedure TSIZStoreForm.EntryTabButtonClick(Sender: TObject);
begin
  CategorySelect(1);
end;

procedure TSIZStoreForm.ReceivingTabButtonClick(Sender: TObject);
begin
  CategorySelect(2);
end;

procedure TSIZStoreForm.WriteoffTabButtonClick(Sender: TObject);
begin
  CategorySelect(3);
end;

procedure TSIZStoreForm.ReturningTabButtonClick(Sender: TObject);
begin
  CategorySelect(4);
end;

procedure TSIZStoreForm.CategorySelect(const ACategory: Integer);
begin
  if ACategory=Category then Exit;

  Screen.Cursor:= crHourGlass;
  FormPanel.Visible:= False;
  try
    Category:= ACategory;

    if Assigned(CategoryForm) then FreeAndNil(CategoryForm);
    if Category=0 then
    begin
      CategoryForm:= FormOnPanelCreate(TSIZStoreAvaliabilityForm, FormPanel);
      MainForm.Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[8] + OTHER_DESCRIPTION[14];
    end
    else if Category in [1..4] then
    begin
      CategoryForm:= SIZDocFormCreate(Category, FormPanel);
      MainForm.Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[8] + OTHER_DESCRIPTION[Category+6];
    end;

    if Assigned(CategoryForm) then
    begin
      CategoryForm.Show;
      ViewUpdate(ModeType);
    end;

  finally
    Screen.Cursor:= crDefault;
    FormPanel.Visible:= True;
  end;
end;

procedure TSIZStoreForm.ExportButtonClick(Sender: TObject);
begin
  DataExport;
end;

end.

