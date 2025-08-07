unit UBriefingForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel, fpspreadsheetgrid,
  //Project utils
  UVars, UConst, UTypes, UBriefingSheet,
  //DK packages utils
  DK_Vector, DK_CtrlUtils, DK_Dialogs, DK_Zoom,
  //Forms
  UBriefingEditForm;

type

  { TBriefingForm }

  TBriefingForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    AddButton: TSpeedButton;
    CopyButton: TSpeedButton;
    DelButton: TSpeedButton;
    DividerBevel2: TDividerBevel;
    EditButton: TSpeedButton;
    EditButtonPanel: TPanel;
    ExportButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure AddButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TBriefingMainListSheet;

    procedure BriefingListLoad(const ASelectedID: Integer = -1);
    procedure BriefingListDraw(const AZoomPercent: Integer);

    procedure BriefingEditFormOpen(const AEdititingType: TEditingType);

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  BriefingForm: TBriefingForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TBriefingForm }

procedure TBriefingForm.FormCreate(Sender: TObject);
begin
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @BriefingListDraw, True);
  Sheet:= TBriefingMainListSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
end;

procedure TBriefingForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
end;

procedure TBriefingForm.FormShow(Sender: TObject);
begin
  SetToolPanels([ToolPanel]);
  SetToolButtons([
    CloseButton, AddButton, DelButton, EditButton, CopyButton
  ]);

  Images.ToButtons([
    ExportButton, CloseButton, AddButton, DelButton, EditButton, CopyButton
  ]);

  DataUpdate;
end;

procedure TBriefingForm.BriefingListLoad(const ASelectedID: Integer = -1);
begin

end;

procedure TBriefingForm.BriefingListDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw();
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TBriefingForm.BriefingEditFormOpen(const AEdititingType: TEditingType);
var
  BriefingEditForm: TBriefingEditForm;
  BriefID: Integer;
begin
  BriefingEditForm:= TBriefingEditForm.Create(nil);
  try
    BriefID:= 0;
    if AEdititingType=etEdit then
    begin

    end;
    BriefingEditForm.BriefID:= BriefID;
    BriefingEditForm.EditingType:= AEdititingType;
    if BriefingEditForm.ShowModal=mrOk then
    begin
      BriefID:= BriefingEditForm.BriefID;
      BriefingListLoad(BriefID);
      BriefingListDraw(ZoomPercent);
    end;
  finally
    FreeAndNil(BriefingEditForm);
  end;
end;

procedure TBriefingForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_BRIEFINGFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TBriefingForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_BRIEFINGFORM, SettingValues);
end;

procedure TBriefingForm.ViewUpdate(const AModeType: TModeType);
begin
  EditButtonPanel.Visible:= AModeType=mtEditing;
end;

procedure TBriefingForm.DataUpdate;
begin
  BriefingListLoad;
  BriefingListDraw(ZoomPercent);
end;

procedure TBriefingForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TBriefingForm.CopyButtonClick(Sender: TObject);
begin
  BriefingEditFormOpen(etCustom);
end;

procedure TBriefingForm.EditButtonClick(Sender: TObject);
begin
  BriefingEditFormOpen(etEdit);
end;

procedure TBriefingForm.AddButtonClick(Sender: TObject);
begin
  BriefingEditFormOpen(etAdd);
end;

end.

