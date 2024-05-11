unit UTypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BCButton,
  //DK packages utils
  DK_Vector, DK_Const, DK_DateUtils, DK_VSTDropDown, DK_VSTTypes;

type
  TModeType = (mtView, mtEditing, mtSetting);
  TEditingType = (etAdd, etEdit, etCustom);
  TMoveDirection = (mdUp, mdDown, mdLeft, mdRight);

  { TMonthDropDown }

  TMonthDropDown = class (TObject)
  private
    FDropDown: TVSTDropDown;
    function GetMonth: Byte;
    function GetText: String;
    procedure SetMonth(const AValue: Byte);
  public
    constructor Create(const AButton: TBCButton; const AOnChange: TVSTEvent = nil);
    destructor  Destroy; override;
    procedure AutoWidth;
    procedure MonthFromDate(const ADate: TDate);
    property Month: Byte read GetMonth write SetMonth;
    property Text: String read GetText;
  end;

implementation

{ TMonthDropDown }

function TMonthDropDown.GetMonth: Byte;
begin
  Result:= FDropDown.ItemIndex + 1;
end;

function TMonthDropDown.GetText: String;
begin
  Result:= FDropDown.Text;
end;

procedure TMonthDropDown.SetMonth(const AValue: Byte);
begin
  FDropDown.ItemIndex:= AValue - 1;
end;

constructor TMonthDropDown.Create(const AButton: TBCButton; const AOnChange: TVSTEvent = nil);
begin
  FDropDown:= TVSTDropDown.Create(AButton);
  if Assigned(AOnChange) then
    FDropDown.OnChange:= AOnChange;
  FDropDown.Items:= VCreateStr(MONTHSNOM);
  FDropDown.DropDownCount:= 12;
  MonthFromDate(Date);
end;

destructor TMonthDropDown.Destroy;
begin
  FreeAndNil(FDropDown);
  inherited Destroy;
end;

procedure TMonthDropDown.AutoWidth;
begin
  FDropDown.AutoWidth(MONTHSNOM[9]);
end;

procedure TMonthDropDown.MonthFromDate(const ADate: TDate);
begin
  Month:= MonthOfDate(ADate);
end;

end.

