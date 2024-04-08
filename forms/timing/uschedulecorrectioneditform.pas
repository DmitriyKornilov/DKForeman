unit UScheduleCorrectionEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, VirtualTrees, DateTimePicker, Buttons, BCButton, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils, DK_DropDown,
  //Project utils
  UDataBase;

type

  { TScheduleCorrectionEditForm }

  TScheduleCorrectionEditForm = class(TForm)
    MarkBCButton: TBCButton;
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);


  private
    DigMarks: TIntVector;
    MarkDropDown: TDropDown;
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
  MarkDropDown:= TDropDown.Create(MarkBCButton);
end;

procedure TScheduleCorrectionEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MarkDropDown);
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
  DataBase.TimetableMarkDictionaryLoad(MarkDropDown, DigMarks, DigMark);
end;





end.

