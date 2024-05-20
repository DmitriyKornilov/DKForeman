unit USSOForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel;

type

  { TSSOForm }

  TSSOForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
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

