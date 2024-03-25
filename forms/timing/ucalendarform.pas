unit UCalendarForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Spin, StdCtrls, VirtualTrees,

  DK_CtrlUtils, DK_VSTTools, DK_Vector;

type

  { TCalendarForm }

  TCalendarForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    CloseButton: TSpeedButton;
    ExportButton: TSpeedButton;
    SettingPanel: TPanel;
    Splitter1: TSplitter;
    VT1: TVirtualStringTree;
    YearPanel: TPanel;
    ToolPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Items: TStrVector;
    Colors: TColorVector;
    ColorList: TVSTColorList;

  public

  end;

var
  CalendarForm: TCalendarForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TCalendarForm }

procedure TCalendarForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TCalendarForm.FormCreate(Sender: TObject);
begin
  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT);
  ControlWidth(CloseButton, TOOL_BUTTON_WIDTH_DEFAULT);

  Items:= VCreateStr([
    'Нерабочий праздничный день',
    'Нерабочий выходной день',
    'Рабочий предпраздничный (сокращенный) день',
    'Рабочий день'
  ]);
  Colors:= VCreateColor([
    $0097CBFF,
    $00CCE3CC,
    $00FFCACA,
    $00FFFFFF]);

  ColorList:= TVSTColorList.Create(VT1);

end;

procedure TCalendarForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ColorList);
end;

procedure TCalendarForm.FormShow(Sender: TObject);
begin
  ColorList.Update(Items, Colors);
end;



end.

