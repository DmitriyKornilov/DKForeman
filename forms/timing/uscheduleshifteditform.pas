unit UScheduleShiftEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, BCButton, VirtualTrees, DateTimePicker, Buttons,
  //DK packages utils
  DK_Vector, DK_DropDown, DK_VSTTables, DK_Const,
  //Project utils
  UDataBase, UWorkHours, USchedule, UUtils, UConst;

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
    KeyMarks: TIntVector;
    PickMarks: TStrVector;
    procedure TypeDropDownChange;

  public
    Cycle: TScheduleCycle;
  end;

var
  ScheduleShiftEditForm: TScheduleShiftEditForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TScheduleShiftEditForm }

procedure TScheduleShiftEditForm.FormCreate(Sender: TObject);
begin
  Cycle:= ScheduleCycleWeek;

  DataBase.TimetableMarkListLoad(KeyMarks, PickMarks, True{ DigMark>0});

  Structure:= TVSTEdit.Create(VT);
  Structure.SetSingleFont(MainForm.GridFont);
  Structure.ShowZeros:= True;
  Structure.HeaderFont.Style:= [fsBold];
  Structure.AddColumnRowTitles(SCHEDULE_CORRECTION_COLUMN_NAMES[0],
                               SCHEDULE_CORRECTION_COLUMN_WIDTHS[0]);

  Structure.AddColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[1],
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[1]);
  Structure.AddColumnDouble(SCHEDULE_CORRECTION_COLUMN_NAMES[2],
                            FRACTION_DIGITS_IN_WORKHOURS,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[2]);
  Structure.AddColumnDouble(SCHEDULE_CORRECTION_COLUMN_NAMES[3],
                            FRACTION_DIGITS_IN_WORKHOURS,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[3]);
  Structure.AddColumnKeyPick(SCHEDULE_CORRECTION_COLUMN_NAMES[4], KeyMarks, PickMarks,
                             SCHEDULE_CORRECTION_COLUMN_WIDTHS[4],
                             taCenter, taLeftJustify);
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
  ScheduleCycleDraw(Structure, Cycle);
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
  if TypeDropDown.ItemIndex=0 then  //недельный
  begin
    FirstDatePicker.Date:= MONDAY_DATE;
    CycleCountSpinEdit.Value:= 7;
  end
  else begin//цикловой
    FirstDatePicker.Date:= Date;
    CycleCountSpinEdit.Value:= 1;
  end;

end;



end.

