unit UScheduleCorrectionEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, VirtualTrees, DateTimePicker, Buttons, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils,
  //Project utils
  UDataBase;

type

  { TScheduleCorrectionEditForm }

  TScheduleCorrectionEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    MarkComboBox: TComboBox;
    LastDateCheckBox: TCheckBox;
    FirstDatePicker: TDateTimePicker;
    LastDatePicker: TDateTimePicker;
    SaveButton: TSpeedButton;
    TotalHoursSpinEdit: TFloatSpinEdit;
    NightHoursSpinEdit: TFloatSpinEdit;
    FirstDateLabel: TLabel;
    TotalHoursLabel: TLabel;
    NightHoursLabel: TLabel;
    MarkLabel: TLabel;
    ShiftNumLabel: TLabel;
    ShiftNumSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);


  private
    DigMarks: TIntVector;
  public
    DigMark: Integer;
  end;

var
  ScheduleCorrectionEditForm: TScheduleCorrectionEditForm;

implementation

{$R *.lfm}

{ TScheduleCorrectionEditForm }

procedure TScheduleCorrectionEditForm.FormCreate(Sender: TObject);
begin
  DigMark:= -1;
  FirstDatePicker.Date:= Date;
  LastDatePicker.Date:= IncDay(Date);
end;

procedure TScheduleCorrectionEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TScheduleCorrectionEditForm.FormShow(Sender: TObject);
begin
  FirstDatePicker.MinDate:= FirstDayInYear(FirstDatePicker.Date);
  FirstDatePicker.MaxDate:= LastDayInYear(FirstDatePicker.Date);
  LastDatePicker.MinDate:= FirstDatePicker.MinDate;
  LastDatePicker.MaxDate:= FirstDatePicker.MaxDate;
  DataBase.TimetableMarkDictionaryLoad(MarkComboBox, DigMarks, DigMark);
end;





end.

