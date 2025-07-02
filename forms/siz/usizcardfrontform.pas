unit USIZCardFrontForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  fpspreadsheetgrid,
  //Project utils
  UTypes, UConst, UDataBase, USIZSizes, USIZCardSheet, USIZNormTypes, UImages,
  //DK packages utils
  DK_Zoom, DK_CtrlUtils, DK_Vector, DK_StrUtils,
  //Forms
  USIZSizeEditForm;

type

  { TSIZCardFrontForm }

  TSIZCardFrontForm = class(TForm)
    CardNumButton: TSpeedButton;
    PersonSizesButton: TSpeedButton;
    SheetBottomPanel: TPanel;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PersonSizesButtonClick(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TSIZCardFrontSheet;

    StaffID, TabNumID, CardID, ItemID: Integer;
    CardNum, Family, PersonName, Patronymic, Gender, TabNum, PostName: String;
    CardBD, CardED: TDate;
    PersonSizes: TSIZStaffSizeIndexes;
    SubItems: TNormSubItems;

    procedure DataDraw(const AZoomPercent: Integer);
    procedure DataReDraw;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate(const AStaffID, ATabNumID, ACardID, AItemID: Integer;
                         const ACardNum, AFamily, AName, APatronymic,
                               AGender, ATabNum, APostName: String;
                         const ACardBD, ACardED: TDate;
                         const APersonSizes: TSIZStaffSizeIndexes;
                         const ASubItems: TNormSubItems);
  end;

var
  SIZCardFrontForm: TSIZCardFrontForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZCardFrontForm }

procedure TSIZCardFrontForm.FormCreate(Sender: TObject);
begin
  Sheet:= TSIZCardFrontSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @DataDraw, True);
end;

procedure TSIZCardFrontForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
end;

procedure TSIZCardFrontForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CardNumButton, PersonSizesButton
  ]);

  Images.ToButtons([
    CardNumButton, PersonSizesButton
  ]);
end;

procedure TSIZCardFrontForm.PersonSizesButtonClick(Sender: TObject);
begin
  if SizeEditFormShowModal(StaffID, PersonSizes)=mrOK then
    DataReDraw;
end;

procedure TSIZCardFrontForm.DataDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  Screen.Cursor:= crHourGlass;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(CardNum, Family, PersonName, Patronymic, Gender, TabNum, PostName,
               CardBD, CardED, PersonSizes, SubItems);
  finally
    ViewGrid.Visible:= True;
    Screen.Cursor:= crDefault;
  end;
end;

procedure TSIZCardFrontForm.DataReDraw;
begin
  DataDraw(ZoomPercent);
end;

procedure TSIZCardFrontForm.SettingsLoad;
var
  SettingValues: TIntVector;
begin
  SettingValues:= DataBase.SettingsLoad(SETTING_NAMES_SIZCARDFRONTFORM);
  ZoomPercent:= SettingValues[0];
end;

procedure TSIZCardFrontForm.SettingsSave;
var
  SettingValues: TIntVector;
begin
  SettingValues:= VCreateInt([ZoomPercent]);
  DataBase.SettingsUpdate(SETTING_NAMES_SIZCARDFRONTFORM, SettingValues);
end;

procedure TSIZCardFrontForm.ViewUpdate(const AModeType: TModeType);
begin
  ToolPanel.Visible:= AModeType=mtEditing;
end;

procedure TSIZCardFrontForm.DataUpdate(const AStaffID, ATabNumID, ACardID, AItemID: Integer;
                                       const ACardNum, AFamily, AName, APatronymic,
                                             AGender, ATabNum, APostName: String;
                                       const ACardBD, ACardED: TDate;
                                       const APersonSizes: TSIZStaffSizeIndexes;
                                       const ASubItems: TNormSubItems);
begin
  StaffID:= AStaffID;
  TabNumID:= ATabNumID;
  CardID:= ACardID;
  ItemID:= AItemID;

  CardNum:= ACardNum;
  CardBD:= ACardBD;
  CardED:= ACardED;

  Family:= AFamily;
  PersonName:= AName;
  Patronymic:= APatronymic;
  Gender:= AGender;
  TabNum:= ATabNum;
  PostName:= APostName;

  PersonSizes:= APersonSizes;
  SubItems:= ASubItems;

  CardNumButton.Enabled:= StaffID>0;
  PersonSizesButton.Enabled:= CardNumButton.Enabled;

  DataReDraw;
end;

end.

