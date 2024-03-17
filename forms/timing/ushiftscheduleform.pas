unit UShiftScheduleForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TShiftScheduleForm }

  TShiftScheduleForm = class(TForm)
    Bevel1: TBevel;
    CloseButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  ShiftScheduleForm: TShiftScheduleForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TShiftScheduleForm }

procedure TShiftScheduleForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

