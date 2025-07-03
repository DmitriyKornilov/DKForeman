unit UImages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Buttons,

  DK_CtrlUtils;

type

  { TImages }

  TImages = class(TDataModule)
    PX24: TImageList;
    PX42: TImageList;
    PX36: TImageList;
    PX30: TImageList;
  private

  public
    procedure ToButtons(const AButtons: array of TSpeedButton);
  end;

implementation

{$R *.lfm}

{ TImages }

procedure TImages.ToButtons(const AButtons: array of TSpeedButton);
var
  i: Integer;
  L: TImageList;
begin
  L:= ChooseImageListForScreenPPI(PX24, PX30, PX36, PX42);
  for i:= 0 to High(AButtons) do
    AButtons[i].Images:= L;
end;

end.

