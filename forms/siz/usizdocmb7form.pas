unit USIZDocMB7Form;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,Buttons,
  DividerBevel, fpspreadsheetgrid,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Zoom, DK_CtrlUtils, DK_SheetTypes, DK_StrUtils,
  //Project utils
  UVars, UConst, USIZDocSheet;

type

  { TSIZDocMB7Form }

  TSIZDocMB7Form = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ExportButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TSIZDocMB7Sheet;

    DocName, DocNum: String;
    DocDate: TDate;
    DocType, DocForm: Integer;

    Fs, Ns, Ps, TabNums, PostNames: TStrVector;
    StoreIDs: TInt64Matrix3D;
    SizCounts, SizDigUnits: TIntMatrix;
    NomNums, SizNames, SizStrUnits, SizLifes: TStrMatrix;
    ReceivingDocNames, ReceivingDocNums, Notes: TStrMatrix;
    ReceivingDates: TDateMatrix;

    procedure DocLoad;
    procedure DocDraw(const AZoomPercent: Integer);

    procedure SettingsLoad;
    procedure SettingsSave;
  public
    DocID: Integer;
    IsReturn: Boolean;
  end;

var
  SIZDocMB7Form: TSIZDocMB7Form;

  procedure SIZDocMB7FormOpen(const ADocID: Integer; const AIsReturn: Boolean);

implementation

procedure SIZDocMB7FormOpen(const ADocID: Integer; const AIsReturn: Boolean);
var
  Form: TSIZDocMB7Form;
begin
  Form:= TSIZDocMB7Form.Create(nil);
  try
    Form.DocID:= ADocID;
    Form.IsReturn:= AIsReturn;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZDocMB7Form }

procedure TSIZDocMB7Form.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[12];
  DocID:= 0;

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @DocDraw, True);

  Sheet:= TSIZDocMB7Sheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
end;

procedure TSIZDocMB7Form.FormDestroy(Sender: TObject);
begin
  SettingsSave;
  FreeAndNil(Sheet);
end;

procedure TSIZDocMB7Form.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    CloseButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton
  ]);

  DocLoad;
  DocDraw(ZoomPercent);
end;

procedure TSIZDocMB7Form.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSIZDocMB7Form.ExportButtonClick(Sender: TObject);
begin
  SheetFromGridSave(Sheet, ZoomPercent, @DocDraw, 'МБ-7', 'Выполнено!', True);
end;

procedure TSIZDocMB7Form.DocLoad;
begin
  if DocID=0 then Exit;
  DataBase.SIZDocLoad(DocID, DocName, DocNum, DocDate, DocType, DocForm);

  if IsReturn then
    DataBase.SIZStoreReturningLoad(DocID, Fs, Ns, Ps, TabNums, PostNames,
                                 StoreIDs, SizCounts, SizDigUnits,
                                 NomNums, SizNames, SizStrUnits, SizLifes,
                                 ReceivingDocNames, ReceivingDocNums, Notes,
                                 ReceivingDates)
  else
    DataBase.SIZStoreReceivingLoad(DocID, Fs, Ns, Ps, TabNums, PostNames,
                                   StoreIDs, SizCounts, SizDigUnits,
                                   NomNums, SizNames, SizStrUnits, SizLifes,
                                   ReceivingDates);
end;

procedure TSIZDocMB7Form.DocDraw(const AZoomPercent: Integer);
var
  i: Integer;
  VFIOs, VTabNums, VSIZNames, VNomNums, VSTRUnits, VSIZLifes, V: TStrVector;
  VDIGUnits, VSIZCounts: TIntVector;
  VReceivingDates: TDateVector;
begin
  VFIOs:= nil;
  VTabNums:= nil;
  for i:= 0 to High(NomNums) do
  begin
    VDim(V{%H-}, Length(NomNums[i]), SNameLong(Fs[i], Ns[i], Ps[i]));
    VFIOs:= VAdd(VFIOs, V);
    VDim(V, Length(NomNums[i]), TabNums[i]);
    VTabNums:= VAdd(VTabNums, V);
  end;
  VSIZNames:= MToVector(SizNames);
  VNomNums:= MToVector(NomNums);
  VSTRUnits:= MToVector(SizStrUnits);
  VDIGUnits:= MToVector(SizDigUnits);
  VSIZLifes:= MToVector(SizLifes);
  VSIZCounts:= MToVector(SizCounts);
  VReceivingDates:= MToVector(ReceivingDates);

  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(Company, Department, DocNum, DocDate, IsReturn,
               VFIOs, VTabNums, VSIZNames, VNomNums, VSTRUnits, VSIZLifes,
               VDIGUnits, VSIZCounts, VReceivingDates);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZDocMB7Form.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SIZDOCMB7FORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TSIZDocMB7Form.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SIZDOCMB7FORM, SettingValues);
end;

end.

