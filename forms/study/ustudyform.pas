unit UStudyForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  DividerBevel;

type

  { TStudyForm }

  TStudyForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  StudyForm: TStudyForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TStudyForm }

procedure TStudyForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

