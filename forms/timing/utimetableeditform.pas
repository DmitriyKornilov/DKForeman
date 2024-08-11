unit UTimetableEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, BCButton, VirtualTrees, DateTimePicker, Buttons, DateUtils,
  //DK packages utils
  DK_Vector, DK_DateUtils, DK_VSTDropDown, DK_Dialogs, DK_Const,
  //Project utils
  UDataBase, UUtils, UUIUtils, UWorkHours, UTimetable;

type

  { TTimetableEditForm }

  TTimetableEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    SaveButton: TSpeedButton;
    SkipMarkBCButton: TBCButton;
    SkipMarkLabel: TLabel;
    SkipCheckBox: TCheckBox;
    CorrectionRadioButton: TRadioButton;
    TotalHoursSpinEdit: TFloatSpinEdit;
    NightHoursSpinEdit: TFloatSpinEdit;
    OverHoursSpinEdit: TFloatSpinEdit;
    SkipHoursSpinEdit: TFloatSpinEdit;
    TotalHoursLabel: TLabel;
    ShiftNumLabel: TLabel;
    NightHoursLabel: TLabel;
    OverHoursLabel: TLabel;
    SkipHoursLabel: TLabel;
    LastDateTimePicker: TDateTimePicker;
    DauCountLabel: TLabel;
    LastDateLabel1: TLabel;
    LastDateLabel2: TLabel;
    MainMarkBCButton: TBCButton;
    MainMarkLabel: TLabel;
    MarkTypeLabel: TLabel;
    MainSettingsPanel: TPanel;
    PresenceRadioButton: TRadioButton;
    AbsenceRadioButton: TRadioButton;
    OffdayRadioButton: TRadioButton;
    ScheduleRadioButton: TRadioButton;
    DayCountSpinEdit: TSpinEdit;
    SettingsPanel1: TPanel;
    SettingsPanel2: TPanel;
    ShiftNumSpinEdit: TSpinEdit;
    procedure AbsenceRadioButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CorrectionRadioButtonClick(Sender: TObject);
    procedure DayCountSpinEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LastDateTimePickerChange(Sender: TObject);
    procedure OffdayRadioButtonClick(Sender: TObject);
    procedure PresenceRadioButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure ScheduleRadioButtonClick(Sender: TObject);
    procedure SkipCheckBoxChange(Sender: TObject);

  private
    MainMarkDropDown: TVSTDropDown;
    SkipMarkDropDown: TVSTDropDown;

    PresenceDigMarks, AbsenceDigMarks, OffDayDigMarks: TIntVector;
    PresenceItems, AbsenceItems, OffDayItems: TStrVector;

    TmpDayCount: Integer;
    TmpDate: TDate;

    OldMarkType: Integer;
    TimetableDay: TTimetableDay;

    procedure CorrectionTypeChange;
  public
    TabNumID: Integer;
    FirstDate, RecrutDate, DismissDate: TDate;
  end;

var
  TimetableEditForm: TTimetableEditForm;

implementation

{$R *.lfm}

{ TTimetableEditForm }


procedure TTimetableEditForm.FormShow(Sender: TObject);
begin
  SetEditButtons([SaveButton, CancelButton]);
  Constraints.MinWidth:= Width;
  Constraints.MinHeight:= Height;
  TotalHoursLabel.Width:= MainMarkLabel.Width;
  SkipHoursLabel.Width:= MainMarkLabel.Width;
  SkipMarkLabel.Width:= MainMarkLabel.Width;
  Caption:= Caption + FormatDateTime(' за dd.mm.yyyy', FirstDate);
  TmpDate:= FirstDate;
  TmpDayCount:= 1;
  LastDateTimePicker.Date:= FirstDate;
  DataBase.TimetableDayLoad(TabNumID, FirstDate, TimetableDay, OldMarkType);
  if OldMarkType=1 then
    PresenceRadioButton.Checked:= True
  else if OldMarkType=2 then
    AbsenceRadioButton.Checked:= True
  else if OldMarkType=4 then
    OffdayRadioButton.Checked:= True;
  CorrectionTypeChange;
end;

procedure TTimetableEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  Holidays: TDateVector;
begin
  IsOK:= False;

  if MainMarkDropDown.ItemIndex<0 then
  begin
    ShowInfo('Не указан основной код табеля!');
    Exit;
  end;

  if CorrectionRadioButton.Checked then //ручная корректировка
  begin
    if PresenceRadioButton.Checked then //явка
    begin
      if (TotalHoursSpinEdit.Value>0) and (ShiftNumSpinEdit.Value=0) then
        if not Confirm('Указано количество рабочих часов, но не указан номер смены (=0)! ' +
             'Всё равно записать изменения?') then Exit;
      if SkipCheckBox.Checked and (SkipHoursSpinEdit.Value=0) then //частичное отсутствие
      begin
        ShowInfo('Не указано количество пропущенных часов!');
        Exit;
      end;
    end;
  end;

  Holidays:= DataBase.HolidaysLoad(FirstDate, LastDateTimePicker.Date);

  if ScheduleRadioButton.Checked then
  begin
    IsOK:= DataBase.TimetableDaysDelete(TabNumID, FirstDate, LastDateTimePicker.Date) and
           TimetableForPeriodUpdate(TabNumID, RecrutDate, DismissDate,
                            FirstDate, LastDateTimePicker.Date, Holidays, False);
  end
  else begin //ручная корректировка
    TimetableDay:= TimetableDayEmpty;
    TimetableDay.ScheduleID:= MANUAL_SCHEDULEID; //ID графика для ручной корректировки табеля
    if PresenceRadioButton.Checked then //явка
    begin
      TimetableDay.TotalHours:= WorkHoursFracToInt(TotalHoursSpinEdit.Value);
      TimetableDay.NightHours:= WorkHoursFracToInt(NightHoursSpinEdit.Value);
      TimetableDay.OverHours:= WorkHoursFracToInt(OverHoursSpinEdit.Value);
      TimetableDay.ShiftNum:= ShiftNumSpinEdit.Value;
      TimetableDay.DigMark:=  PresenceDigMarks[MainMarkDropDown.ItemIndex];
      if SkipCheckBox.Checked then //частичное отсутствие
      begin
        TimetableDay.SkipHours:= WorkHoursFracToInt(SkipHoursSpinEdit.Value);
        TimetableDay.SkipMark:= AbsenceDigMarks[SkipMarkDropDown.ItemIndex];
      end;
    end
    else if AbsenceRadioButton.Checked then //отсутствие
    begin
      TimetableDay.DigMark:=  AbsenceDigMarks[MainMarkDropDown.ItemIndex];
      TimetableDay.SkipHours:= FULLSHIFT_SKIPHOURS; //вся смена
      TimetableDay.SkipMark:= TimetableDay.DigMark;
    end
    else //выходной
      TimetableDay.DigMark:= OffdayDigMarks[MainMarkDropDown.ItemIndex];

    IsOK:= TimetableDaysByCorrectionAdd(TabNumID, RecrutDate, DismissDate,
                  FirstDate, LastDateTimePicker.Date, Holidays, TimetableDay);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TTimetableEditForm.ScheduleRadioButtonClick(Sender: TObject);
begin
  MainSettingsPanel.Visible:= False;
end;

procedure TTimetableEditForm.SkipCheckBoxChange(Sender: TObject);
begin
  SettingsPanel2.Visible:= SkipCheckBox.Checked;
end;

procedure TTimetableEditForm.CorrectionTypeChange;
var
  DigMark: Integer;
begin
  SettingsPanel1.Visible:= PresenceRadioButton.Checked;
  NightHoursSpinEdit.Value:= 0;
  OverHoursSpinEdit.Value:= 0;
  SkipHoursSpinEdit.Value:= 0;

  if PresenceRadioButton.Checked then
  begin
    DigMark:= TimetableDay.DigMark;
    if (DigMark>0) and (OldMarkType=1) then
    begin
      TotalHoursSpinEdit.Value:= WorkHoursIntToFrac(TimetableDay.TotalHours);
      NightHoursSpinEdit.Value:= WorkHoursIntToFrac(TimetableDay.NightHours);
      OverHoursSpinEdit.Value:= WorkHoursIntToFrac(TimetableDay.OverHours);
      ShiftNumSpinEdit.Value:= TimetableDay.ShiftNum;
      MainMarkDropDown.KeyPick(PresenceItems, PresenceDigMarks, DigMark);

      DigMark:= TimetableDay.SkipMark;
      SkipCheckBox.Checked:= DigMark>0;
      if SkipCheckBox.Checked then
        SkipMarkDropDown.ItemIndex:= VIndexOf(AbsenceDigMarks, DigMark);
    end
    else begin
      TotalHoursSpinEdit.Value:= 8;
      ShiftNumSpinEdit.Value:= 1;
      DigMark:= 1 {Я};
      MainMarkDropDown.KeyPick(PresenceItems, PresenceDigMarks, DigMark);
    end;
  end
  else begin
    TotalHoursSpinEdit.Value:= 0;
    ShiftNumSpinEdit.Value:= 0;
    DigMark:= TimetableDay.DigMark;
    if AbsenceRadioButton.Checked then
    begin
      if (DigMark=0) or (OldMarkType<>2) then
        DigMark:= 19 {Б};
      MainMarkDropDown.KeyPick(AbsenceItems, AbsenceDigMarks, DigMark);
    end
    else begin
      if (DigMark=0) or (OldMarkType<>4)  then
        DigMark:= 26 {B};
      MainMarkDropDown.KeyPick(OffDayItems, OffDayDigMarks, DigMark);
    end;
  end;
end;

procedure TTimetableEditForm.FormCreate(Sender: TObject);
begin
  TabNumID:= -1;
  SaveButton.Images:= ImageListForScreen;
  CancelButton.Images:= SaveButton.Images;
  MainMarkDropDown:= TVSTDropDown.Create(MainMarkBCButton);
  SkipMarkDropDown:= TVSTDropDown.Create(SkipMarkBCButton);

  ShiftNumSpinEdit.MaxValue:= 365;

  DataBase.TimetableMarkListLoad(PresenceDigMarks, PresenceItems, True, 1);
  DataBase.TimetableMarkListLoad(AbsenceDigMarks, AbsenceItems, True, 2);
  DataBase.TimetableMarkListLoad(OffDayDigMarks, OffDayItems, True, 4);
  SkipMarkDropDown.KeyPick(AbsenceItems, AbsenceDigMarks, 16 {ДО});
end;

procedure TTimetableEditForm.CorrectionRadioButtonClick(Sender: TObject);
begin
  MainSettingsPanel.Visible:= True;
end;

procedure TTimetableEditForm.LastDateTimePickerChange(Sender: TObject);
begin
  if CompareDate(LastDateTimePicker.Date, FirstDate)<0 then
    LastDateTimePicker.Date:= FirstDate;
  //если дата изменилась
  if not SameDate(LastDateTimePicker.Date, TmpDate) then
  begin
    //запоминаем новую дату
    TmpDate:= LastDateTimePicker.Date;
    //запоминаем новое кол-во дней
    TmpDayCount:= DaysBetweenDates(FirstDate, TmpDate)+1;
    //изменяем кол-во дней
    DayCountSpinEdit.Value:= TmpDayCount;
  end;
end;

procedure TTimetableEditForm.DayCountSpinEditChange(Sender: TObject);
begin
  //если кол-во дней изменилось
  if DayCountSpinEdit.Value<>TmpDayCount then
  begin
    //запоминаем новое кол-во дней
    TmpDayCount:= DayCountSpinEdit.Value;
    //запоминаем новую дату
    TmpDate:= IncDay(FirstDate, TmpDayCount-1);
    //изменяем дату
    LastDateTimePicker.Date:= TmpDate;
  end;
end;

procedure TTimetableEditForm.PresenceRadioButtonClick(Sender: TObject);
begin
  CorrectionTypeChange;
end;

procedure TTimetableEditForm.AbsenceRadioButtonClick(Sender: TObject);
begin
  CorrectionTypeChange;
end;

procedure TTimetableEditForm.OffdayRadioButtonClick(Sender: TObject);
begin
  CorrectionTypeChange;
end;

procedure TTimetableEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TTimetableEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MainMarkDropDown);
  FreeAndNil(SkipMarkDropDown);
end;

end.

