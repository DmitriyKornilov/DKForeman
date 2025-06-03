unit USIZStorageForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel;

type

  { TSIZStorageForm }

  TSIZStorageForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  SIZStorageForm: TSIZStorageForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZStorageForm }

procedure TSIZStorageForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

