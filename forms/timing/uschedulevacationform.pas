unit UScheduleVacationForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, BCButton,
  Buttons, Spin, fpspreadsheetgrid,
  //DK packages utils
  DK_Vector, DK_Zoom,
  //Project utils
  UDataBase, UConst, UUtils, UScheduleSheet;

type

  { TScheduleVacationForm }

  TScheduleVacationForm = class(TForm)
    Bevel1: TBevel;
    Bevel3: TBevel;
    CloseButton: TSpeedButton;
    ExportButton: TBCButton;
    SheetPanel: TPanel;
    ToolPanel: TPanel;
    ViewGrid: TsWorksheetGrid;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    ZoomBevel: TBevel;
    ZoomPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    ZoomPercent: Integer;
    Sheet: TVacationScheduleSheet;
    StaffNames, TabNums, PostNames: TStrVector;
    FirstDates: TDateVector;
    TotalCounts: TIntVector;
    procedure ScheduleChange;
    procedure ScheduleDraw(const AZoomPercent: Integer);
  public

  end;

var
  ScheduleVacationForm: TScheduleVacationForm;

  procedure ScheduleVacationFormShow(const AYear: Word);

implementation

uses UMainForm;

{$R *.lfm}

procedure ScheduleVacationFormShow(const AYear: Word);
var
  Form: TScheduleVacationForm;
begin
  Form:= TScheduleVacationForm.Create(nil);
  try
    Form.YearSpinEdit.Value:= AYear;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{ TScheduleVacationForm }

procedure TScheduleVacationForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[12];
  Height:= 300; Width:= 500; //for normal form maximizing

  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    CloseButton
  ]);
  SetCategoryButtons([
    ExportButton
  ]);

  ZoomPercent:= 100;
  CreateZoomControls(50, 150, ZoomPercent, ZoomPanel, @ScheduleDraw, True);

  Sheet:= TVacationScheduleSheet.Create(ViewGrid.Worksheet, ViewGrid, MainForm.GridFont);
end;

procedure TScheduleVacationForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Sheet);
end;

procedure TScheduleVacationForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TScheduleVacationForm.ExportButtonClick(Sender: TObject);
begin
  Sheet.Save(YearSpinEdit.Text, 'Выполнено!', True);
end;

procedure TScheduleVacationForm.FormShow(Sender: TObject);
begin
  ScheduleChange;
end;

procedure TScheduleVacationForm.YearSpinEditChange(Sender: TObject);
begin
  ScheduleChange;
end;

procedure TScheduleVacationForm.ScheduleChange;
begin
  DataBase.VacationScheduleLoad(YearSpinEdit.Value,
                 StaffNames, TabNums, PostNames, FirstDates, TotalCounts);
  ScheduleDraw(ZoomPercent);
end;

procedure TScheduleVacationForm.ScheduleDraw(const AZoomPercent: Integer);
begin
  ViewGrid.Visible:= False;
  try
    ZoomPercent:= AZoomPercent;
    Sheet.Zoom(ZoomPercent);
    Sheet.Draw(YearSpinEdit.Value, StaffNames, TabNums, PostNames, FirstDates, TotalCounts);
  finally
    ViewGrid.Visible:= True;
  end;
end;

end.

