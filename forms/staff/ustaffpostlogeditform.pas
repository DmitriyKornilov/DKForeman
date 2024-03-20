unit UStaffPostLogEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DateTimePicker, Buttons, DateUtils,
  //DK packages utils
  DK_Vector, DK_StrUtils, DK_Dialogs, DK_Const,
  //Project utils
  UDBUtils, UTypes;

type

  { TStaffPostlogEditForm }

  TStaffPostlogEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    StatusComboBox: TComboBox;
    PostComboBox: TComboBox;
    PostLabel: TLabel;
    RankEdit: TEdit;
    RankLabel: TLabel;
    FirstDatePicker: TDateTimePicker;
    FirstDateLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    PostIDs: TIntVector;

    PrevFirstDate, NextFirstDate: TDate;
    IsNextPeriodExists, IsPrevPeriodExists: Boolean;
  public
    EditingType: TEditingType;
    TabNumID, PostID, PostLogID: Integer;
  end;

var
  StaffPostLogEditForm: TStaffPostLogEditForm;

implementation

{$R *.lfm}

{ TStaffPostlogEditForm }

procedure TStaffPostlogEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TStaffPostlogEditForm.FormShow(Sender: TObject);
begin
  DataBase.PostDictionaryLoad(PostComboBox, PostIDs, PostID);
  IsPrevPeriodExists:= DataBase.StaffPostLogPrevPeriodFirstDate(TabNumID, FirstDatePicker.Date, PrevFirstDate);
  IsNextPeriodExists:= DataBase.StaffPostLogNextPeriodFirstDate(TabNumID, FirstDatePicker.Date, NextFirstDate);

  case EditingType of
    etAdd: //перевод
      begin
         FirstDatePicker.Date:= IncDay(FirstDatePicker.Date, 1);
         FirstDatePicker.MinDate:= FirstDatePicker.Date;
         FirstDatePicker.MaxDate:= IncDay(INFDATE, -1);
      end;
    etEdit: //изменение
      begin
         if (not IsPrevPeriodExists) then //первая запись
         begin
           FirstDatePicker.Enabled:= False; // менять первую дату(прием) нельзя
           StatusComboBox.Enabled:= False;  // менять статус должности на временную нельзя
         end
         else begin
           FirstDatePicker.MinDate:= IncDay(PrevFirstDate, 1);
           FirstDatePicker.MaxDate:= IncDay(NextFirstDate, -1);
         end;
         ////существование более поздних постоянных должностей
         //IsNextPeriodExists:= IsNextConstPostExists(TabNum, OldFirstDate);
         ////если должность постоянная
         //if (ComboBox2.ItemIndex=0) then
         //begin
         //  //невозможно изменить должность на временную, если других постоянных нет
         //  ComboBox2.Enabled:= IsOtherConstPostExists(TabNum, OldFirstDate);
         //end;
      end;
  end;
end;

procedure TStaffPostlogEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  Rank: String;
begin
  IsOK:= False;

  if SEmpty(PostComboBox.Text) then
  begin
    ShowInfo('Не указана должность!');
    Exit;
  end;

  Rank:= STrim(RankEdit.Text);

  if SEmpty(Rank) then
  begin
    ShowInfo('Не указан разряд!');
    Exit;
  end;

  PostID:= PostIDs[PostComboBox.ItemIndex];

  case EditingType of
    etAdd:  //превод
      IsOK:= True;
    etEdit:
      IsOK:= True;
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

end.

