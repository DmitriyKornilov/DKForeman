unit USIZStoreForm;

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

  { TSIZStoreForm }

  TSIZStoreForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    CaptionPanel: TPanel;
    VT: TVirtualStringTree;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ModeType: TModeType;

    SIZList: TVSTTable;

    procedure SIZListCreate;
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

  SIZListCreate;
end;

procedure TSIZStoreForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZList);
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
    //ExportButton,
    CloseButton
  ]);

  DataUpdate;
end;

procedure TSIZStoreForm.SIZListCreate;
begin
  SIZList:= TVSTTable.Create(VT);
  SIZList.CanSelect:= False;
  //SIZList.CanUnselect:= False;
  //SIZList.OnSelect:= @NormListSelect;
  //SIZList.OnDelKeyDown:= @NormListDelItem;
  //SIZList.OnReturnKeyDown:= @NormListEditItem;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];

  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Документ', 300);
  SIZList.Draw;
end;

procedure TSIZStoreForm.ViewUpdate(const AModeType: TModeType);
begin
  ModeType:= AModeType;

end;

procedure TSIZStoreForm.DataUpdate;
begin

end;

end.

