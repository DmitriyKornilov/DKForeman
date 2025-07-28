unit USIZStaffHistoryForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid, DividerBevel,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Zoom, DK_CtrlUtils, DK_SheetTypes, DK_StrUtils,
  DK_SheetExporter,
  //Project utils
  UVars, UConst, USIZCardSheet;

type

  { TSIZStaffHistoryForm }

  TSIZStaffHistoryForm = class(TForm)
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
    Sheet: TSIZCardBackSheet;
    SheetTitle: String;

    LogIDs: TInt64Vector;
    ReceivingDates, ReturningDates: TDateVector;
    NormSIZTypes: TIntVector;
    NormSizNames: TStrVector;
    ReceivingDocNames, ReturningDocNames: TStrVector;
    ReceivingSizNames, WriteoffDocNames: TStrMatrix;
    SizCounts, SizeTypes: TIntMatrix;

    procedure DataLoad;
    procedure DataDraw(const AZoomPercent: Integer);
    procedure DataExport;

    procedure SettingsLoad;
    procedure SettingsSave;
  public
    TabNumID: Integer;
    StaffName, TabNum: String;
  end;

var
  SIZStaffHistoryForm: TSIZStaffHistoryForm;

  procedure SIZStaffHistoryFormOpen(const ATabNumID: Integer;
                                    const AStaffName, ATabNum: String);

implementation

procedure SIZStaffHistoryFormOpen(const ATabNumID: Integer;
                                  const AStaffName, ATabNum: String);
var
  Form: TSIZStaffHistoryForm;
begin
  Form:= TSIZStaffHistoryForm.Create(nil);
  try
    Form.TabNumID:= ATabNumID;
    Form.StaffName:= AStaffName;
    Form.TabNum:= ATabNum;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZStaffHistoryForm }

procedure TSIZStaffHistoryForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[13];
  TabNumID:= 0;

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @DataDraw, True);

  Sheet:= TSIZCardBackSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
end;

procedure TSIZStaffHistoryForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;
  FreeAndNil(Sheet);
end;

procedure TSIZStaffHistoryForm.FormShow(Sender: TObject);
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

  SheetTitle:= StaffName + ' [таб.№ ' + TabNum +
               '] - история выдачи средств индивидуальной защиты';
  DataLoad;
  DataDraw(ZoomPercent);
end;

procedure TSIZStaffHistoryForm.DataLoad;
begin
  DataBase.SIZPersonalCardSIZLoad(TabNumID, 0, LogIDs, ReceivingDates, ReturningDates,
                                NormSIZTypes, NormSizNames, ReceivingDocNames,
                                ReturningDocNames, ReceivingSizNames,
                                WriteoffDocNames, SizCounts, SizeTypes);
end;

procedure TSIZStaffHistoryForm.DataDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(True{NeedDraw}, ReceivingDates, ReturningDates, NormSizNames,
               ReceivingDocNames, ReturningDocNames,
               ReceivingSizNames, WriteoffDocNames, SizCounts, SizeTypes,
               True{IsHistory}, SheetTitle);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZStaffHistoryForm.DataExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TSIZCardBackSheet;
begin
  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лист1');
    ExpSheet:= TSIZCardBackSheet.Create(Worksheet, nil, GridFont);
    try
      ExpSheet.Draw(True{NeedDraw}, ReceivingDates, ReturningDates, NormSizNames,
                    ReceivingDocNames, ReturningDocNames,
                    ReceivingSizNames, WriteoffDocNames, SizCounts, SizeTypes,
                    True{IsHistory}, SheetTitle);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZStaffHistoryForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SIZSTAFFHISTORYFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TSIZStaffHistoryForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SIZSTAFFHISTORYFORM, SettingValues);
end;

procedure TSIZStaffHistoryForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSIZStaffHistoryForm.ExportButtonClick(Sender: TObject);
begin
  DataExport;
end;

end.

