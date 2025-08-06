unit UBriefingLogForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,  Buttons,
  DividerBevel,
  //Project utils
  UVars, UConst, UTypes,
  //DK packages utils
  DK_Vector, DK_CtrlUtils, DK_Dialogs;

type

  { TBriefingLogForm }

  TBriefingLogForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure ViewUpdate(const AModeType: TModeType);
    procedure DataUpdate;
  end;

var
  BriefingLogForm: TBriefingLogForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TBriefingLogForm }

procedure TBriefingLogForm.FormCreate(Sender: TObject);
begin

end;

procedure TBriefingLogForm.FormDestroy(Sender: TObject);
begin

end;

procedure TBriefingLogForm.FormShow(Sender: TObject);
begin
  SetToolPanels([ToolPanel]);
  Images.ToButtons([CloseButton]);
end;

procedure TBriefingLogForm.ViewUpdate(const AModeType: TModeType);
begin

end;

procedure TBriefingLogForm.DataUpdate;
begin

end;

procedure TBriefingLogForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

end.

