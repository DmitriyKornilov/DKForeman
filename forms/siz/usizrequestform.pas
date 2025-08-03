unit USIZRequestForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  DateTimePicker, DividerBevel, Buttons, fpspreadsheetgrid,
  //Project utils
  UVars, UConst, USIZStoreSheet,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Zoom, DK_SheetExporter;

type

  { TSIZRequestForm }

  TSIZRequestForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    ExportButton: TSpeedButton;
    DatePanel: TPanel;
    DatePicker: TDateTimePicker;
    DateLabel: TLabel;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure DatePickerChange(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TSIZStoreRequestSheet;

    SIZNames: TStrVector;
    Genders: TIntVector;
    SIZSizes: TStrMatrix;
    Families, Names, Patronymics, TabNums: TStrMatrix3D;
    SIZCounts: TIntMatrix3D;

    procedure RequestLoad;
    procedure RequestDraw(const AZoomPercent: Integer);
    procedure RequestExport;

    procedure SettingsLoad;
    procedure SettingsSave;
  public

    procedure DataUpdate;
  end;

var
  SIZRequestForm: TSIZRequestForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZRequestForm }

procedure TSIZRequestForm.FormCreate(Sender: TObject);
begin
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @RequestDraw, True);

  Sheet:= TSIZStoreRequestSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
  DatePicker.Date:= Date;
end;

procedure TSIZRequestForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
end;

procedure TSIZRequestForm.FormShow(Sender: TObject);
begin
  SetToolPanels([ToolPanel]);
  SetToolButtons([CloseButton]);
  Images.ToButtons([ExportButton, CloseButton]);

  DataUpdate;
end;

procedure TSIZRequestForm.RequestLoad;
begin
  DataBase.SIZStoreRequestLoad(DatePicker.Date, 0 {WriteoffType},
                          SIZNames, Genders, SIZSizes,
                          Families, Names, Patronymics, TabNums, SIZCounts);
end;

procedure TSIZRequestForm.RequestDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(EmptyStr, SIZNames, Genders, SIZSizes, Families, Names, Patronymics,
               TabNums, SIZCounts);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZRequestForm.RequestExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TSIZStoreRequestSheet;
  S: String;
begin
  S:= DateToStr(DatePicker.Date);

  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet(S);
    ExpSheet:= TSIZStoreRequestSheet.Create(Worksheet, nil, GridFont);
    try
      S:= 'Список средств индивидуальной защиты, требующихся на ' + S;
      ExpSheet.Draw(S, SIZNames, Genders, SIZSizes, Families, Names, Patronymics,
                    TabNums, SIZCounts);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZRequestForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SIZREQUESTFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TSIZRequestForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SIZREQUESTFORM, SettingValues);
end;

procedure TSIZRequestForm.DataUpdate;
begin
  RequestLoad;
  RequestDraw(ZoomPercent);
end;

procedure TSIZRequestForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZRequestForm.DatePickerChange(Sender: TObject);
begin
  DataUpdate;
end;

procedure TSIZRequestForm.ExportButtonClick(Sender: TObject);
begin
  RequestExport;
end;

end.

