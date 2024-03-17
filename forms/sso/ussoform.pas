unit USSOForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TSSOForm }

  TSSOForm = class(TForm)
    Bevel1: TBevel;
    CloseButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  SSOForm: TSSOForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSSOForm }

procedure TSSOForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

