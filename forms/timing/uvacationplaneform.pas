unit UVacationPlaneForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel;

type

  { TVacationPlaneForm }

  TVacationPlaneForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  VacationPlaneForm: TVacationPlaneForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TVacationPlaneForm }

procedure TVacationPlaneForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

