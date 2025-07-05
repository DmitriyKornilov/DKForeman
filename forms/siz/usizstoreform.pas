unit USIZStoreForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel,
  //Project utils
  UVars, UConst, UTypes,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_CtrlUtils,
  //Forms
  USIZDocForm, USIZStoreHistoryForm;

type

  { TSIZStoreForm }

  TSIZStoreForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    CaptionPanel: TPanel;
    DividerBevel2: TDividerBevel;
    EditVT: TVirtualStringTree;
    HistoryButton: TSpeedButton;
    OutButton: TSpeedButton;
    ExportButton: TSpeedButton;
    EntryButton: TSpeedButton;
    WriteoffButton: TSpeedButton;
    ToolPanel: TPanel;
    ViewVT: TVirtualStringTree;
    procedure CloseButtonClick(Sender: TObject);
    procedure EntryButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HistoryButtonClick(Sender: TObject);
    procedure OutButtonClick(Sender: TObject);
    procedure WriteoffButtonClick(Sender: TObject);
  private
    ModeType: TModeType;

    SIZViewList: TVSTCustomCategoryTable;
    SIZEditList: TVSTCategoryCheckTable;

    procedure SIZListCreate;
    procedure SIZListLoad;
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
  ModeType:= mtView;
  EditVT.Align:= alClient;
  SIZListCreate;
end;

procedure TSIZStoreForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZViewList);
  FreeAndNil(SIZEditList);
end;

procedure TSIZStoreForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    CaptionPanel
  ]);
  SetToolButtons([
    CloseButton
  ]);

  Images.ToButtons([
    ExportButton, EntryButton, OutButton, WriteoffButton, HistoryButton,
    CloseButton
  ]);

  DataUpdate;
end;

procedure TSIZStoreForm.HistoryButtonClick(Sender: TObject);
begin
  FormModalShow(TSIZStoreHistoryForm);
end;

procedure TSIZStoreForm.SIZListCreate;
begin
  SIZViewList:= TVSTCustomCategoryTable.Create(ViewVT);
  SIZViewList.TreeLinesVisible:= False;
  SIZViewList.SetSingleFont(GridFont);
  SIZViewList.HeaderFont.Style:= [fsBold];
  SIZViewList.AddColumn('Номенклатурный номер', 200);
  SIZViewList.AddColumn('Наименование', 300);
  SIZViewList.AddColumn('Единица измерения', 150);
  SIZViewList.AddColumn('Размер/объём/вес', 130);
  SIZViewList.AddColumn('Количество', 100);
  SIZViewList.AddColumn('Документ', 300);
  SIZViewList.Draw;

  SIZEditList:= TVSTCategoryCheckTable.Create(EditVT);
  SIZEditList.CanSelect:= False;
  //SIZEditList.OnSelect:= @MStaffListSelect;
  SIZEditList.TreeLinesVisible:= False;
  SIZEditList.StopSelectEventWhileCheckAll:= True;
  SIZEditList.SetSingleFont(GridFont);
  SIZEditList.HeaderFont.Style:= [fsBold];
  SIZEditList.AddColumn('Номенклатурный номер', 200);
  SIZEditList.AddColumn('Наименование', 300);
  SIZEditList.AddColumn('Единица измерения', 150);
  SIZEditList.AddColumn('Размер/объём/вес', 130);
  SIZEditList.AddColumn('Количество', 100);
  SIZEditList.AddColumn('Документ', 300);
  SIZEditList.Draw;
end;

procedure TSIZStoreForm.SIZListLoad;
begin

end;

procedure TSIZStoreForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;

  ViewVT.Visible:= ModeType=mtView;
  EditVT.Visible:= ModeType=mtEditing;
end;

procedure TSIZStoreForm.DataUpdate;
begin

end;

procedure TSIZStoreForm.EntryButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(1);
end;

procedure TSIZStoreForm.OutButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(2);
end;

procedure TSIZStoreForm.WriteoffButtonClick(Sender: TObject);
begin
  SIZDocFormOpen(3);
end;

end.

