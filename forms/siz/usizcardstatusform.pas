unit USIZCardStatusForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid,
  //Project utils
  UTypes, UConst, UVars, USIZCardSheet, USIZNormTypes, USIZCardTypes,
  //DK packages utils
  DK_Zoom, DK_CtrlUtils, DK_Vector;

type

  { TSIZCardStatusForm }

  TSIZCardStatusForm = class(TForm)
    SheetBottomPanel: TPanel;
    SheetPanel: TPanel;
    AddButton: TSpeedButton;
    DelButton: TSpeedButton;
    SizeButton: TSpeedButton;
    ViewGrid: TsWorksheetGrid;
    ToolPanel: TPanel;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ZoomPercent: Integer;

    Sheet: TSIZCardStatusSheet;

    SubItems: TNormSubItems;
    StatusItems: TStatusItems;

    procedure DataDraw(const AZoomPercent: Integer);
    procedure DataReDraw;

    procedure OnStatusSelect;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure DataUpdate(const ASubItems: TNormSubItems;
                         const AStatusItems: TStatusItems);
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  SIZCardStatusForm: TSIZCardStatusForm;

implementation

{$R *.lfm}

{ TSIZCardStatusForm }

procedure TSIZCardStatusForm.FormCreate(Sender: TObject);
begin
  Sheet:= TSIZCardStatusSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
  Sheet.OnSelect:= @OnStatusSelect;

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @DataDraw, True);
end;

procedure TSIZCardStatusForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
end;

procedure TSIZCardStatusForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    SizeButton, AddButton, DelButton
  ]);
  Images.ToButtons([
    SizeButton, AddButton, DelButton
  ]);
end;

procedure TSIZCardStatusForm.DataDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(SubItems, StatusItems);

  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZCardStatusForm.DataReDraw;
begin
  DataDraw(ZoomPercent);
end;

procedure TSIZCardStatusForm.OnStatusSelect;
begin
  AddButton.Enabled:= Sheet.IsNormInfoSelected;
  SizeButton.Enabled:= AddButton.Enabled;
  DelButton.Enabled:= Sheet.IsStatusInfoSelected;
end;

procedure TSIZCardStatusForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SIZCARDSTATUSFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TSIZCardStatusForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SIZCARDSTATUSFORM, SettingValues);
end;

procedure TSIZCardStatusForm.DataUpdate(const ASubItems: TNormSubItems;
                                        const AStatusItems: TStatusItems);
begin
  SubItems:= ASubItems;
  StatusItems:= AStatusItems;

  DataReDraw;
end;

procedure TSIZCardStatusForm.ViewUpdate(const AModeType: TModeType);
begin
  ToolPanel.Visible:= AModeType=mtEditing;
  Sheet.CanSelect:= AModeType=mtEditing;
end;

end.

