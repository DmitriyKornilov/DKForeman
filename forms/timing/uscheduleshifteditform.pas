unit UScheduleShiftEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, BCButton, VirtualTrees, DateTimePicker, Buttons,
  //DK packages utils
  DK_Vector, DK_DropDown, DK_VSTTables,
  //Project utils
  UDataBase, UUtils, UConst;

type

  { TScheduleShiftEditForm }

  TScheduleShiftEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FirstDatePicker: TDateTimePicker;
    CycleCountLabel: TLabel;
    StrucutureLabel: TLabel;
    SaveButton: TSpeedButton;
    TypeBCButton: TBCButton;
    VT: TVirtualStringTree;
    WeekHoursLabel: TLabel;
    FirstDateLabel: TLabel;
    TypeLabel: TLabel;
    NameEdit: TEdit;
    NameLabel: TLabel;
    WeekHoursSpinEdit: TSpinEdit;
    CycleCountSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    TypeDropDown: TDropDown;
    Structure: TVSTEdit;
    procedure TypeDropDownChange;
  public
    ScheduleID: Integer;
  end;

var
  ScheduleShiftEditForm: TScheduleShiftEditForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TScheduleShiftEditForm }

procedure TScheduleShiftEditForm.FormCreate(Sender: TObject);
begin
  ScheduleID:= -1;
  FirstDatePicker.Date:= Date;

  Structure:= TVSTEdit.Create(VT);
  Structure.SetSingleFont(MainForm.GridFont);
  Structure.HeaderFont.Style:= [fsBold];
  Structure.AddColumnRowTitles(SCHEDULE_CORRECTION_COLUMN_NAMES[0], 70);
  Structure.AddColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[1], 70);
  Structure.AddColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[2], 80);
  Structure.AddColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[3], 90);
  Structure.AddColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[4], 70);
  Structure.Draw;

  TypeDropDown:= TDropDown.Create(TypeBCButton);
  TypeDropDown.OnChange:= @TypeDropDownChange;
  TypeDropDown.Items:= VCreateStr(['недельный', 'цикловой']);
  TypeDropDown.ItemIndex:= 0;
end;

procedure TScheduleShiftEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TScheduleShiftEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TypeDropDown);
  FreeAndNil(Structure);
end;

procedure TScheduleShiftEditForm.FormShow(Sender: TObject);
begin
  Width:= Width + 5; //fix datetimepicker button size bug
end;

procedure TScheduleShiftEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
begin
  IsOK:= False;


  IsOK:= True; //!!!
  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TScheduleShiftEditForm.TypeDropDownChange;
begin
  CycleCountSpinEdit.Enabled:= TypeDropDown.ItemIndex=1;
  FirstDatePicker.Enabled:= TypeDropDown.ItemIndex=1;
end;

end.

