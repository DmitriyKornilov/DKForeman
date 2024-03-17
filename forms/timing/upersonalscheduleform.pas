unit UPersonalScheduleForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TPersonalScheduleForm }

  TPersonalScheduleForm = class(TForm)
    Bevel1: TBevel;
    CloseButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  PersonalScheduleForm: TPersonalScheduleForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TPersonalScheduleForm }

procedure TPersonalScheduleForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

