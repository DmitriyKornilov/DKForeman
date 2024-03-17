unit UCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,

  DK_CtrlUtils;

type

  { TCalendarForm }

  TCalendarForm = class(TForm)
    Bevel1: TBevel;
    CloseButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  CalendarForm: TCalendarForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TCalendarForm }

procedure TCalendarForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TCalendarForm.FormCreate(Sender: TObject);
begin
  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  ControlWidth(CloseButton, TOOL_BUTTON_WIDTH_DEFAULT);
end;

end.

