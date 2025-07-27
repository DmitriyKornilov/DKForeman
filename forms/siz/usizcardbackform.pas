unit USIZCardBackForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid,
  //Project utils
  UTypes, UConst, UVars, USIZCardSheet,
  //DK packages utils
  DK_Zoom, DK_CtrlUtils, DK_Vector, DK_Matrix, DK_Dialogs,
  //Forms
  USIZCardReturnEditForm;

type

  { TSIZCardBackForm }

  TSIZCardBackForm = class(TForm)
    DelButton: TSpeedButton;
    ReturnButton: TSpeedButton;
    CancelButton: TSpeedButton;
    SheetBottomPanel: TPanel;
    SheetPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ToolPanel: TPanel;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure DelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReturnButtonClick(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TSIZCardBackSheet;

    NeedDraw: Boolean;

    CardID: Integer;

    LogIDs: TInt64Vector;
    ReceivingDates, ReturningDates: TDateVector;
    NormSIZTypes: TIntVector;
    NormSizNames: TStrVector;
    ReceivingDocNames, ReturningDocNames: TStrVector;
    ReceivingSizNames, WriteoffDocNames: TStrMatrix;
    SizCounts, SizeTypes: TIntMatrix;

    procedure DataLoad;
    procedure DataDraw(const AZoomPercent: Integer);
    procedure DataReDraw;
    procedure DataSelect;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure DataUpdate(const ANeedDraw: Boolean; const ACardID: Integer);
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  SIZCardBackForm: TSIZCardBackForm;

implementation

uses UMainForm, USIZCardForm;

{$R *.lfm}

{ TSIZCardBackForm }

procedure TSIZCardBackForm.FormCreate(Sender: TObject);
begin
  Sheet:= TSIZCardBackSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
  Sheet.OnSelect:= @DataSelect;

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
    DelButton, ReturnButton, CancelButton
  ]);
  Images.ToButtons([
    DelButton, ReturnButton, CancelButton
  ]);
end;

procedure TSIZCardBackForm.DataLoad;
begin
  DataBase.SIZPersonalCardSIZLoad(0, CardID, LogIDs, ReceivingDates, ReturningDates,
                                NormSIZTypes, NormSizNames, ReceivingDocNames,
                                ReturningDocNames, ReceivingSizNames,
                                WriteoffDocNames, SizCounts, SizeTypes);
end;

procedure TSIZCardBackForm.DataDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(NeedDraw, ReceivingDates, ReturningDates, NormSizNames,
               ReceivingDocNames, ReturningDocNames,
               ReceivingSizNames, WriteoffDocNames, SizCounts, SizeTypes);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZCardBackForm.DataReDraw;
begin
  DataDraw(ZoomPercent);
end;

procedure TSIZCardBackForm.DataSelect;
begin
  DelButton.Enabled:= Sheet.IsSelected;
  ReturnButton.Enabled:= DelButton.Enabled and
                         (NormSIZTypes[Sheet.SelectedIndex]>0{не дерматологические});
  CancelButton.Enabled:= DelButton.Enabled;

  CancelButton.Visible:= Sheet.IsSelected and
                         (ReturningDates[Sheet.SelectedIndex]>0);
  ReturnButton.Visible:= not CancelButton.Visible;
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

procedure TSIZCardBackForm.DataUpdate(const ANeedDraw: Boolean; const ACardID: Integer);
begin
  NeedDraw:= ANeedDraw;
  CardID:= ACardID;
  DataLoad;
  DataReDraw;
end;

procedure TSIZCardBackForm.ViewUpdate(const AModeType: TModeType);
begin
  ToolPanel.Visible:= AModeType=mtEditing;
  Sheet.CanSelect:= AModeType=mtEditing;
end;

procedure TSIZCardBackForm.ReturnButtonClick(Sender: TObject);
var
  Form: TSIZCardReturnEditForm;
begin
  Form:= TSIZCardReturnEditForm.Create(nil);
  try
    Form.LogID:= LogIDs[Sheet.SelectedIndex];
    Form.ReceivingDocName:= ReceivingDocNames[Sheet.SelectedIndex];
    Form.ReceivingDate:= ReturningDates[Sheet.SelectedIndex];
    Form.SizNames:= ReceivingSizNames[Sheet.SelectedIndex];
    Form.SizCounts:= SizCounts[Sheet.SelectedIndex];
    if Form.ShowModal=mrOK then
      (MainForm.CategoryForm as TSIZCardForm).CardListLoad(True);
  finally
    FreeAndNil(Form);
  end;
end;

procedure TSIZCardBackForm.DelButtonClick(Sender: TObject);
begin
  if not Confirm('Отменить выдачу?') then Exit;
  if DataBase.SIZReceivingCancel(LogIDs[Sheet.SelectedIndex]) then
    (MainForm.CategoryForm as TSIZCardForm).CardListLoad(True);
end;

procedure TSIZCardBackForm.CancelButtonClick(Sender: TObject);
begin
  if not Confirm('Отменить возврат на склад?') then Exit;
  if DataBase.SIZReturningCancel(LogIDs[Sheet.SelectedIndex]) then
    (MainForm.CategoryForm as TSIZCardForm).CardListLoad(True);
end;

end.

