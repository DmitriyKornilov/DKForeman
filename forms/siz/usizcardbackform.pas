unit USIZCardBackForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid,
  //Project utils
  UTypes, UConst, UVars, USIZCardSheet,
  //DK packages utils
  DK_Zoom, DK_CtrlUtils, DK_Vector;

type

  { TSIZCardBackForm }

  TSIZCardBackForm = class(TForm)
    DelButton: TSpeedButton;
    WriteoffButton: TSpeedButton;
    CancelButton: TSpeedButton;
    SheetBottomPanel: TPanel;
    SheetPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ToolPanel: TPanel;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TSIZCardBackSheet;

    NeedDraw: Boolean;

    procedure DataDraw(const AZoomPercent: Integer);
    procedure DataReDraw;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure DataUpdate(const ANeedDraw: Boolean);
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  SIZCardBackForm: TSIZCardBackForm;

implementation

{$R *.lfm}

{ TSIZCardBackForm }

procedure TSIZCardBackForm.FormCreate(Sender: TObject);
begin
  Sheet:= TSIZCardBackSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @DataDraw, True);
end;

procedure TSIZCardBackForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
end;

procedure TSIZCardBackForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    DelButton, WriteoffButton, CancelButton
  ]);
  Images.ToButtons([
    DelButton, WriteoffButton, CancelButton
  ]);
end;

procedure TSIZCardBackForm.DataDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(NeedDraw, nil);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZCardBackForm.DataReDraw;
begin
  DataDraw(ZoomPercent);
end;

procedure TSIZCardBackForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SIZCARDBACKFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TSIZCardBackForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SIZCARDBACKFORM, SettingValues);
end;

procedure TSIZCardBackForm.DataUpdate(const ANeedDraw: Boolean);
begin
  NeedDraw:= ANeedDraw;

  DataReDraw;
end;

procedure TSIZCardBackForm.ViewUpdate(const AModeType: TModeType);
begin
  ToolPanel.Visible:= AModeType=mtEditing;
  //Sheet.CanSelect:= AModeType=mtEditing;
end;

end.

