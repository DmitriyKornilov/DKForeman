unit UBriefingLogForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,  Buttons,
  DividerBevel, VirtualTrees, fpspreadsheetgrid,
  //Project utils
  UVars, UConst, UTypes, UUtils, UBriefingSheet,
  //DK packages utils
  DK_Vector, DK_CtrlUtils, DK_Dialogs, DK_VSTParamList;

type

  { TBriefingLogForm }

  TBriefingLogForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ListCaptionPanel: TPanel;
    ListPanel: TPanel;
    MainPanel: TPanel;
    SettingCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    SettingSplitter: TSplitter;
    ListSplitter: TSplitter;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewCaptionPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ViewPanel: TPanel;
    VT: TVirtualStringTree;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    CanDataUpdate: Boolean;
    ParamList: TVSTParamList;

    BriefingList: TBriefingListSheet;

    BriefIDs, Objects, Periods, Nums: TIntVector;
    BriefNames, Notes: TStrVector;
    BeginDates, EndDates, LastDates: TDateVector;

    procedure ParamListCreate;
    procedure ListTypeSelect;

    procedure BriefingListCreate;
    procedure BriefingListLoad(const ASelectedID: Integer = -1);
    procedure BriefingListSelect;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  BriefingLogForm: TBriefingLogForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TBriefingLogForm }

procedure TBriefingLogForm.FormCreate(Sender: TObject);
begin
  CanDataUpdate:= False;
  ParamListCreate;
  SettingsLoad;
  BriefingListCreate;
  CanDataUpdate:= True;
end;

procedure TBriefingLogForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(BriefingList);
end;

procedure TBriefingLogForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    SettingCaptionPanel, ListCaptionPanel, ViewCaptionPanel
  ]);
  SetToolButtons([
    CloseButton
  ]);

  Images.ToButtons([
    CloseButton
  ]);

  DataUpdate;
end;

procedure TBriefingLogForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= 'Включать в список мероприятия:';
  V:= VCreateStr([
    'все',
    'действующие на текущую дату',
    'завершенные на текущую дату'
  ]);
  ParamList.AddStringList('ListType', S, V, @ListTypeSelect, 1);
end;

procedure TBriefingLogForm.ListTypeSelect;
begin
  BriefingListLoad;
end;

procedure TBriefingLogForm.BriefingListCreate;
begin
  BriefingList:= TBriefingListSheet.Create(ViewGrid.Worksheet, ViewGrid, GridFont);
  BriefingList.AutosizeColumnEnableLast;
  BriefingList.CanSelect:= True;
  BriefingList.CanUnselect:= False;
  BriefingList.OnSelect:= @BriefingListSelect;
end;

procedure TBriefingLogForm.BriefingListLoad(const ASelectedID: Integer);
var
  SelectedID: Integer;
begin
  if not CanDataUpdate then Exit;

  SelectedID:= GetSelectedID(BriefingList, BriefIDs, ASelectedID);

  DataBase.BriefingListLoad(ParamList.Selected['ListType'],
                            BriefIDs, Objects, Periods, Nums, BriefNames, Notes,
                            BeginDates, EndDates, LastDates);

  //ExportButton.Enabled:= not VIsNil(BriefIDs);



  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    BriefingList.Draw(BriefNames, Notes);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
  BriefingList.ReSelect(BriefIDs, SelectedID, True);

end;

procedure TBriefingLogForm.BriefingListSelect;
begin

end;

procedure TBriefingLogForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_BRIEFINGLOGFORM);
end;

procedure TBriefingLogForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_BRIEFINGLOGFORM, ParamList.Params);
end;

procedure TBriefingLogForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
  try
    //ModeType:= AModeType;
    SettingSplitter.Align:= alRight;
    SettingPanel.Visible:= AModeType=mtSetting;
    SettingSplitter.Visible:= AModeType=mtSetting;
    SettingSplitter.Align:= alLeft;

    MainPanel.BorderSpacing.Left:= 2*Ord(AModeType<>mtSetting);
  finally
    MainPanel.Visible:= True;
  end;
end;

procedure TBriefingLogForm.DataUpdate;
begin
  BriefingListLoad;
end;

procedure TBriefingLogForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

