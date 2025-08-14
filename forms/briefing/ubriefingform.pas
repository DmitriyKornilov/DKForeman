unit UBriefingForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel, fpspreadsheetgrid, DateUtils,
  //Project utils
  UVars, UConst, UTypes, UUtils, UBriefingSheet,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Dialogs, DK_Zoom, DK_Const, DK_SheetExporter,
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
    procedure DelButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ViewGridDblClick(Sender: TObject);
  private
    ModeType: TModeType;
    ZoomPercent: Integer;
    BriefingList: TBriefingMainListSheet;

    BriefIDs, Objects, Periods, Nums: TIntVector;
    BriefNames, Notes: TStrVector;
    BeginDates, EndDates, LastDates: TDateVector;
    ObjectNames: TStrMatrix;
    ObjectIDs: TIntMatrix;

    procedure BriefingListCreate;
    procedure BriefingListLoad(const ASelectedID: Integer = -1);
    procedure BriefingListDraw(const AZoomPercent: Integer);
    procedure BriefingListSelect;
    procedure BriefingListEditItem;
    procedure BriefingListDelItem;

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
  ModeType:= mtView;
  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @BriefingListDraw, True);
  BriefingListCreate;
end;

procedure TBriefingForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(BriefingList);
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

procedure TBriefingForm.ViewGridDblClick(Sender: TObject);
begin
  BriefingListEditItem;
end;

procedure TBriefingForm.BriefingListCreate;
begin
  BriefingList:= TBriefingMainListSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
  BriefingList.CanUnselect:= False;
  BriefingList.OnSelect:= @BriefingListSelect;
  BriefingList.OnReturnKeyDown:= @BriefingListEditItem;
  BriefingList.OnDelKeyDown:= @BriefingListDelItem;
end;

procedure TBriefingForm.BriefingListLoad(const ASelectedID: Integer = -1);
var
  SelectedID: Integer;
begin
  if ModeType=mtEditing then
    SelectedID:= GetSelectedID(BriefingList, BriefIDs, ASelectedID);

  DataBase.BriefingListLoad(0{все}, BriefIDs, Objects, Periods, Nums, BriefNames, Notes,
                            BeginDates, EndDates, LastDates);
  DataBase.BriefingListObjectNamesLoad(BriefIDs, Objects, ObjectIDs, ObjectNames);

  ExportButton.Enabled:= not VIsNil(BriefIDs);

  BriefingListDraw(ZoomPercent);
  if ModeType=mtEditing then
    BriefingList.ReSelect(BriefIDs, SelectedID, True);
end;

procedure TBriefingForm.BriefingListDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    BriefingList.Zoom(ZoomPercent);
    BriefingList.Draw(BriefNames, Notes, Objects, Periods, Nums,
               BeginDates, EndDates, LastDates, ObjectNames);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TBriefingForm.BriefingListSelect;
begin
  DelButton.Enabled:= BriefingList.IsSelected;
  EditButton.Enabled:= DelButton.Enabled;
  CopyButton.Enabled:= DelButton.Enabled;
end;

procedure TBriefingForm.BriefingListEditItem;
begin
  if ModeType<>mtEditing then Exit;
  if not BriefingList.IsSelected then Exit;
  BriefingEditFormOpen(etEdit);
end;

procedure TBriefingForm.BriefingListDelItem;
begin
  if ModeType<>mtEditing then Exit;
  if not BriefingList.IsSelected then Exit;
  if not Confirm('Удалить "' +
                 BriefNames[BriefingList.SelectedIndex] + '"?') then Exit;
  DataBase.Delete('BRIEFINGMAIN', 'BriefID', BriefIDs[BriefingList.SelectedIndex]);
  BriefingListLoad;
end;

procedure TBriefingForm.BriefingEditFormOpen(const AEdititingType: TEditingType);
var
  BriefingEditForm: TBriefingEditForm;
  i, BriefID: Integer;
begin
  BriefingEditForm:= TBriefingEditForm.Create(nil);
  try
    BriefID:= 0;
    if AEdititingType<>etAdd then
    begin
      i:= BriefingList.SelectedIndex;
      BriefingEditForm.BriefNameEdit.Text:= BriefNames[i];
      BriefingEditForm.NoteEdit.Text:= Notes[i];
      BriefingEditForm.OldPeriod:= Periods[i];
      BriefingEditForm.OldObject:= Objects[i];
      BriefingEditForm.OldObjectIDs:= ObjectIDs[i];
      BriefingEditForm.OldObjectNames:= ObjectNames[i];
      BriefingEditForm.BeginDatePicker.Date:= BeginDates[i];
      BriefingEditForm.EndDatePicker.Date:= EndDates[i];
      BriefingEditForm.InfEndDateCheckBox.Checked:= SameDate(EndDates[i], INFDATE);
      BriefingEditForm.LastDateCheckBox.Checked:= LastDates[i]>0;
      if LastDates[i]>0 then
        BriefingEditForm.LastDatePicker.Date:= LastDates[i]
      else
        BriefingEditForm.LastDatePicker.Date:= BeginDates[i];
      if Nums[i]>0 then
        BriefingEditForm.NumSpinEdit.Value:= Nums[i];
    end;
    BriefingEditForm.BriefID:= BriefID;
    BriefingEditForm.EditingType:= AEdititingType;
    if BriefingEditForm.ShowModal=mrOk then
    begin
      BriefID:= BriefingEditForm.BriefID;
      BriefingListLoad(BriefID);
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
  ModeType:= AModeType;
  EditButtonPanel.Visible:= ModeType=mtEditing;

  BriefingList.CanSelect:= ModeType=mtEditing;
  if ModeType=mtEditing then
    BriefingList.ReSelect(BriefIDs, -1, True);
end;

procedure TBriefingForm.DataUpdate;
begin
  BriefingListLoad;
end;

procedure TBriefingForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TBriefingForm.CopyButtonClick(Sender: TObject);
begin
  BriefingEditFormOpen(etCustom);
end;

procedure TBriefingForm.DelButtonClick(Sender: TObject);
begin
  BriefingListDelItem;
end;

procedure TBriefingForm.EditButtonClick(Sender: TObject);
begin
  BriefingEditFormOpen(etEdit);
end;

procedure TBriefingForm.AddButtonClick(Sender: TObject);
begin
  BriefingEditFormOpen(etAdd);
end;

procedure TBriefingForm.ExportButtonClick(Sender: TObject);
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TBriefingMainListSheet;
begin
  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лист1');
    ExpSheet:= TBriefingMainListSheet.Create(Worksheet, nil, GridFont);
    try
      ExpSheet.Draw(BriefNames, Notes, Objects, Periods, Nums,
                    BeginDates, EndDates, LastDates, ObjectNames);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.PageSettings(spoLandscape);
    Exporter.Save('Выполнено!');
  finally
    FreeAndNil(Exporter);
  end;
end;

end.

