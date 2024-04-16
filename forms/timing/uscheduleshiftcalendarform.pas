unit UScheduleShiftCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BCButton, Spin, VirtualTrees, fpspreadsheetgrid,

  //DK packages utils
  DK_Vector, DK_Fonts, DK_Const, DK_VSTDropDown, DK_DateUtils, DK_StrUtils,
  DK_VSTEditTools, DK_Zoom, DK_SheetExporter,
  //Project utils
  UDataBase, UConst, UUtils, UCalendar, USchedule, UScheduleShiftSheet,
  //Forms
  UChooseForm;

type

  { TScheduleShiftCalendarForm }

  TScheduleShiftCalendarForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CloseButton: TSpeedButton;
    ExportButton: TBCButton;
    LeftPanel: TPanel;
    LeftSplitter: TSplitter;
    SettingButton: TSpeedButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    VT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
  private
    ZoomPercent: Integer;

    ColorNames: TStrVector;
    ColorValues: TColorVector;
    ColorList: TVSTColorList;
    procedure ScheduleDraw(const AZoomPercent: Integer);
  public
    ScheduleName: String;
  end;

var
  ScheduleShiftCalendarForm: TScheduleShiftCalendarForm;

  procedure ScheduleShiftCalendarFormCreate(const AYear: Integer; const AScheduleName: String);

implementation

uses UMainForm;

{$R *.lfm}

procedure ScheduleShiftCalendarFormCreate(const AYear: Integer; const AScheduleName: String);
var
  Frm: TScheduleShiftCalendarForm;
begin
  Frm:= TScheduleShiftCalendarForm.Create(nil);
  try
    Frm.Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[10] + ' - [' + AScheduleName + ']';
    Frm.YearSpinEdit.Value:= AYear;
    Frm.ScheduleName:= AScheduleName;
    Frm.ShowModal;
  finally
    FreeAndNil(Frm);
  end;
end;

{ TScheduleShiftCalendarForm }

procedure TScheduleShiftCalendarForm.FormCreate(Sender: TObject);
begin
  Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CloseButton, SettingButton
  ]);

  SetCategoryButtons([
    ExportButton
  ]);

  ColorList:= TVSTColorList.Create(VT);

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);
end;

procedure TScheduleShiftCalendarForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ColorList);
end;

procedure TScheduleShiftCalendarForm.FormShow(Sender: TObject);
var
  i: Integer;
begin
  ColorValues:= VCreateColor(COLORS_SHIFT);
  VDim(ColorNames, Length(ColorValues));
  ColorNames[0]:= 'Нерабочий день';
  for i:= 1 to High(ColorNames) do
    ColorNames[i]:= 'Смена №' + IntToStr(i);
  ColorList.Update(ColorNames, ColorValues);
end;

procedure TScheduleShiftCalendarForm.SettingButtonClick(Sender: TObject);
begin
  if SettingButton.Down then
  begin
    LeftPanel.Visible:= True;
    LeftSplitter.Visible:= True;
    SheetPanel.AnchorToNeighbour(akLeft, 0, LeftSplitter);
  end
  else begin
    LeftSplitter.Visible:= False;
    LeftPanel.Visible:= False;
    SheetPanel.AnchorToNeighbour(akLeft, 2, Self);
  end;
end;

procedure TScheduleShiftCalendarForm.ScheduleDraw(const AZoomPercent: Integer);
begin

end;

procedure TScheduleShiftCalendarForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.

