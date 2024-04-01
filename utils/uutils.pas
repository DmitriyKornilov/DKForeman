unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Buttons, BCPanel,
  //DK packages utils
  DK_CtrlUtils, DK_Color, DK_Vector;

  procedure SetToolPanels(const AControls: array of TControl);
  procedure SetCaptionPanels(const AControls: array of TBCPanel);
  procedure SetToolButtons(const AControls: array of TControl);

  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;


implementation

procedure SetToolPanels(const AControls: array of TControl);
var
  i: Integer;
begin
  for i:= 0 to High(AControls) do
    ControlHeight(AControls[i], TOOL_PANEL_HEIGHT_DEFAULT);
end;

procedure SetCaptionPanels(const AControls: array of TBCPanel);
var
  i: Integer;
begin
  for i:= 0 to High(AControls) do
  begin
    ControlHeight(AControls[i], Round(TOOL_PANEL_HEIGHT_DEFAULT*0.6));
    AControls[i].Background.Color:= ColorIncLightness(clBtnFace, -15);
    AControls[i].Border.Color:= clActiveBorder;
  end;
end;

procedure SetToolButtons(const AControls: array of TControl);
var
  i: Integer;
begin
  for i:= 0 to High(AControls) do
    ControlWidth(AControls[i], TOOL_BUTTON_WIDTH_DEFAULT);
end;

function SettingByName(const AName: String; const ANames: TStrVector;
  const AValues: TIntVector): Integer;
begin
  VSameIndexValue(AName, ANames, AValues, Result);
end;



end.

