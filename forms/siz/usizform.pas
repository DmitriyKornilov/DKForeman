unit USIZForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TSIZForm }

  TSIZForm = class(TForm)
    Bevel1: TBevel;
    CloseButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  SIZForm: TSIZForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZForm }

procedure TSIZForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

