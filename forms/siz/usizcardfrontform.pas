unit USIZCardFrontForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  fpspreadsheetgrid,
  //Project utils
  UTypes, UConst, UDataBase, USIZSizes, USIZCardSheet, USIZNormTypes,
  //DK packages utils
  DK_Zoom, DK_CtrlUtils, DK_Vector, Types;

type

  { TSIZCardFrontForm }

  TSIZCardFrontForm = class(TForm)
    SheetBottomPanel: TPanel;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TSIZCardFrontSheet;

    CardNum, Family, PersonName, Patronymic, Gender, TabNum, PostName: String;
    CardBD, CardED: TDate;
    PersonSizes: TSIZStaffSizeIndexes;
    SubItems: TNormSubItems;

    procedure DataDraw(const AZoomPercent: Integer);

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate(const ACardNum, AFamily, AName, APatronymic,
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
  SetToolPanels([
    ToolPanel
  ]);

  //SetToolButtons([
  //
  //]);

  //Images.ToButtons([
  //
  //]);

  Sheet:= TSIZCardFrontSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);

  SettingsLoad; //load ZoomPercent
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @DataDraw, True);
end;

procedure TSIZCardFrontForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
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
  DataDraw(ZoomPercent);
end;

procedure TSIZCardFrontForm.DataUpdate(const ACardNum, AFamily, AName, APatronymic,
                                             AGender, ATabNum, APostName: String;
                                       const ACardBD, ACardED: TDate;
                                       const APersonSizes: TSIZStaffSizeIndexes;
                                       const ASubItems: TNormSubItems);
begin
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
end;

end.

