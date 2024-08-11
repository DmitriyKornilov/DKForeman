unit UVacationPlanForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel, BCPanel, BCButton, Spin, DateUtils,
  //DK packages utils

  //Project utils
  UUIUtils,
  //Forms
  UScheduleVacationForm;

type

  { TVacationPlanForm }

  TVacationPlanForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    ExportButton: TBCButton;
    PlanButton: TBCButton;
    CaptionPanel: TBCPanel;
    ToolPanel: TPanel;
    ScheduleButton: TBCButton;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScheduleButtonClick(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private

  public

  end;

var
  VacationPlanForm: TVacationPlanForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TVacationPlanForm }

procedure TVacationPlanForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TVacationPlanForm.FormCreate(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetCaptionPanels([
    CaptionPanel
  ]);
  SetToolButtons([
    CloseButton
  ]);
  SetCategoryButtons([
    ExportButton, PlanButton, ScheduleButton
  ]);

  YearSpinEdit.Value:= YearOf(Date);

end;

procedure TVacationPlanForm.ScheduleButtonClick(Sender: TObject);
begin
  ScheduleVacationFormShow(YearSpinEdit.Value);
end;

procedure TVacationPlanForm.YearSpinEditChange(Sender: TObject);
begin
  CaptionPanel.Caption:= 'Планируемые отпуска в ' + YearSpinEdit.Text + ' году';
end;

end.

