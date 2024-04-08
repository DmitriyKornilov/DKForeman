unit USchedulePersonalForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TSchedulePersonalForm }

  TSchedulePersonalForm = class(TForm)
    Bevel1: TBevel;
    CloseButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  SchedulePersonalForm: TSchedulePersonalForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSchedulePersonalForm }

procedure TSchedulePersonalForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

