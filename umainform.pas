unit UMainForm;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Menus, LCLType, DividerBevel, SQLDB,
  //DK packages utils
  {$IFDEF DEBUG}
  DK_HeapTrace,
  {$ENDIF}
  DK_Const, DK_LCLStrRus, DK_CtrlUtils, DK_VSTTypes,
  //Project utils
  UVars, UConst, UTypes,
  //Forms
  UAboutForm,
  UInfoForm,
  UParamForm,
  UStaffForm,
  UCalendarForm, UScheduleShiftForm, UVacationPlanForm,
  USchedulePersonalForm, UTimetableForm,
  USIZNameEditForm, USIZNormForm, USIZSizeForm, USIZCardForm, USIZStoreForm,
  USIZRequestForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TSpeedButton;
    CalendarMenuItem: TMenuItem;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DictionaryButton: TSpeedButton;
    DepartmentMenuItem: TMenuItem;
    SIZRequestMenuItem: TMenuItem;
    SIZStorageMenuItem: TMenuItem;
    SIZSizesMenuItem: TMenuItem;
    BaseDDLScript: TSQLScript;
    StaffButton: TSpeedButton;
    SIZNormsMenuItem: TMenuItem;
    TimingButton: TSpeedButton;
    TimetableMarkMenuItem: TMenuItem;
    SIZListMenuItem: TMenuItem;
    SIZUnitMenuItem: TMenuItem;
    SIZReasonMenuItem: TMenuItem;
    ShiftScheduleMenuItem: TMenuItem;
    SafetyButton: TSpeedButton;
    VacationPlaneMenuItem: TMenuItem;
    PersonalScheduleMenuItem: TMenuItem;
    TimetableMenuItem: TMenuItem;
    SIZStaffMenuItem: TMenuItem;
    DictionaryMenu: TPopupMenu;
    PostListMenuItem: TMenuItem;
    MainPanel: TPanel;
    SafetyMenu: TPopupMenu;
    Separator1: TMenuItem;
    TimingMenu: TPopupMenu;
    RefreshButton: TSpeedButton;
    SettingButton: TSpeedButton;
    ExitButton: TSpeedButton;
    EditingButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure AboutButtonClick(Sender: TObject);
    procedure CalendarMenuItemClick(Sender: TObject);
    procedure DepartmentMenuItemClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure EditingButtonClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PersonalScheduleMenuItemClick(Sender: TObject);
    procedure PostListMenuItemClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SafetyButtonClick(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
    procedure ShiftScheduleMenuItemClick(Sender: TObject);
    procedure SIZListMenuItemClick(Sender: TObject);
    procedure SIZRequestMenuItemClick(Sender: TObject);
    procedure SIZStaffMenuItemClick(Sender: TObject);
    procedure SIZNormsMenuItemClick(Sender: TObject);
    procedure SIZReasonMenuItemClick(Sender: TObject);
    procedure SIZSizesMenuItemClick(Sender: TObject);
    procedure SIZStorageMenuItemClick(Sender: TObject);
    procedure SIZUnitMenuItemClick(Sender: TObject);
    procedure StaffButtonClick(Sender: TObject);
    procedure TimetableMarkMenuItemClick(Sender: TObject);
    procedure TimetableMenuItemClick(Sender: TObject);
    procedure TimingButtonClick(Sender: TObject);
    procedure VacationPlaneMenuItemClick(Sender: TObject);
  private
    Category: Integer;

    procedure DBConnect;

    procedure ViewUpdate;
    procedure DataUpdate;
    procedure SettingsSave;

  public
    CategoryForm: TForm;
    procedure CategorySelect(const ACategory: Integer);
    procedure DictionarySelect(const ADictionary: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  HeapTraceOutputFile('trace.trc');
  {$ENDIF}
  Caption:= MAIN_CAPTION;
  Category:= -1;
  DBConnect;
  GlobalVarInit;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    EditingButton, SettingButton, RefreshButton, AboutButton, ExitButton
  ]);

  Images.ToButtons([
    StaffButton, TimingButton, SafetyButton, DictionaryButton,
    EditingButton, SettingButton, RefreshButton, AboutButton, ExitButton
  ]);

  TimingMenu.Images:= ChooseImageListForScreenPPI(Images.PX24, Images.PX30,
                                                  Images.PX36, Images.PX42);
  SafetyMenu.Images:= TimingMenu.Images;
  DictionaryMenu.Images:= TimingMenu.Images;

  CategorySelect(0);
end;

procedure TMainForm.DBConnect;
var
  DBPath, DBName{, DDLName}: String;
  IsDBFileExists: Boolean;
begin
  DBPath:= ExtractFilePath(Application.ExeName) + 'db' + DirectorySeparator;
  DBName:= DBPath + 'base.db';
  //DDLName:= DBPath + 'ddl.sql';
  IsDBFileExists:= FileExists(DBName);

  DataBase.Connect(DBName);
  //DataBase.ExecuteScript(DDLName);
  DataBase.ExecuteScript(BaseDDLScript);
  if not IsDBFileExists then
  begin
    DataBase.ColorsShiftUpdate;

  end;
end;

procedure TMainForm.ViewUpdate;
var
  ModeType: TModeType;
begin
  if not Assigned(CategoryForm) then Exit;

  if EditingButton.Down then
    ModeType:= mtEditing
  else if SettingButton.Down then
    ModeType:= mtSetting
  else
    ModeType:= mtView;

  case Category of
    //0: TInfoForm - no modes;
    1: (CategoryForm as TStaffForm).ViewUpdate(ModeType);
    2: (CategoryForm as TCalendarForm).ViewUpdate(ModeType);
    3: (CategoryForm as TScheduleShiftForm).ViewUpdate(ModeType);
    4: (CategoryForm as TVacationPlanForm).ViewUpdate(ModeType);
    5: (CategoryForm as TSchedulePersonalForm).ViewUpdate(ModeType);
    6: (CategoryForm as TTimetableForm).ViewUpdate(ModeType);
    7: (CategoryForm as TSIZNormForm).ViewUpdate(ModeType);
    8: (CategoryForm as TSIZStoreForm).ViewUpdate(ModeType);
    //9: TSIZRequestFrom - no modes
    10: (CategoryForm as TSIZSizeForm).ViewUpdate(ModeType);
    11: (CategoryForm as TSIZCardForm).ViewUpdate(ModeType);
  end;
end;

procedure TMainForm.DataUpdate;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    0: (CategoryForm as TInfoForm).DataUpdate;
    1: (CategoryForm as TStaffForm).DataUpdate;
    2: (CategoryForm as TCalendarForm).DataUpdate;
    3: (CategoryForm as TScheduleShiftForm).DataUpdate;
    4: (CategoryForm as TVacationPlanForm).DataUpdate;
    5: (CategoryForm as TSchedulePersonalForm).DataUpdate;
    6: (CategoryForm as TTimetableForm).DataUpdate;
    7: (CategoryForm as TSIZNormForm).DataUpdate;
    8: (CategoryForm as TSIZStoreForm).DataUpdate;
    9: (CategoryForm as TSIZRequestForm).DataUpdate;
    10: (CategoryForm as TSIZSizeForm).DataUpdate;
    11: (CategoryForm as TSIZCardForm).DataUpdate;
  end;
end;

procedure TMainForm.SettingsSave;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    //0: TInfoForm - no settings
    1: (CategoryForm as TStaffForm).SettingsSave;
    2: (CategoryForm as TCalendarForm).SettingsSave;
    3: (CategoryForm as TScheduleShiftForm).SettingsSave;
    4: (CategoryForm as TVacationPlanForm).SettingsSave;
    5: (CategoryForm as TSchedulePersonalForm).SettingsSave;
    6: (CategoryForm as TTimetableForm).SettingsSave;
    //7: TSIZNormForm   - no settings
    //8: TSIZStoreForm  - no settings
    9: (CategoryForm as TSIZRequestForm).SettingsSave;
    10: (CategoryForm as TSIZSizeForm).SettingsSave;
    11: (CategoryForm as TSIZCardForm).SettingsSave;
  end;
end;

procedure TMainForm.CategorySelect(const ACategory: Integer);
begin
  if ACategory=Category then Exit;

  Screen.Cursor:= crHourGlass;
  MainPanel.Visible:= False;
  try
    SettingsSave;
    Category:= ACategory;
    Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[ACategory];
    SettingButton.Enabled:= not (Category in [0, 2, 7, 8, 9]);
    EditingButton.Enabled:= not (Category in [0, 9]);

    if Assigned(CategoryForm) then FreeAndNil(CategoryForm);
    case Category of
      0: CategoryForm:= FormOnPanelCreate(TInfoForm, MainPanel);
      1: CategoryForm:= FormOnPanelCreate(TStaffForm, MainPanel);
      2: CategoryForm:= FormOnPanelCreate(TCalendarForm, MainPanel);
      3: CategoryForm:= FormOnPanelCreate(TScheduleShiftForm, MainPanel);
      4: CategoryForm:= FormOnPanelCreate(TVacationPlanForm, MainPanel);
      5: CategoryForm:= FormOnPanelCreate(TSchedulePersonalForm, MainPanel);
      6: CategoryForm:= FormOnPanelCreate(TTimetableForm, MainPanel);
      7: CategoryForm:= FormOnPanelCreate(TSIZNormForm, MainPanel);
      8: CategoryForm:= FormOnPanelCreate(TSIZStoreForm, MainPanel);
      9: CategoryForm:= FormOnPanelCreate(TSIZRequestForm, MainPanel);
      10: CategoryForm:= FormOnPanelCreate(TSIZSizeForm, MainPanel);
      11: CategoryForm:= FormOnPanelCreate(TSIZCardForm, MainPanel);
    end;
    if Assigned(CategoryForm) then
    begin
      CategoryForm.Show;
      ViewUpdate;
    end;

  finally
    Screen.Cursor:= crDefault;
    MainPanel.Visible:= True;
  end;
end;

procedure TMainForm.DictionarySelect(const ADictionary: Integer);
var
  IsOK: Boolean;
begin
  IsOK:= False;
  case ADictionary of
    1: IsOK:= DataBase.EditList('Перечень должностей (профессий)',
                         'STAFFPOST', 'PostID', 'PostName',
                         True, True, 400, GridFont,
                         True, 'Фильтр:');
    2: IsOK:= DataBase.EditTable('Коды табеля учета рабочего времени',
                          'TIMETABLEMARK', 'DigMark',
                          ['DigMark',      'StrMark',       'TypeMark', 'Note'        ],
                          ['Цифровой код', 'Буквенный код', 'Статус',   'Описание'    ],
                          [ ctInteger,      ctString,        ctKeyPick,  ctString     ],
                          [ True,           True,            True,       True         ],
                          [ 100,            100,             100,        500          ],
                          [ taCenter,       taCenter,        taCenter,   taLeftJustify],
                          True, ['DigMark'], 4,
                          [nil,             nil,             TIMETABLE_TYPEMARK_KEYS,  nil],
                          [nil,             nil,             TIMETABLE_TYPEMARK_PICKS, nil],
                          GridFont, True, 'Фильтр:');
    3: IsOK:= FormModalShow(TSIZNameEditForm)=mrOK;
    4: IsOK:= DataBase.EditTable('Единицы измерения средств индивидуальной защиты',
                          'SIZUNIT', 'UnitID',
                          ['UnitName',     'UnitDigitalCode', 'UnitStringCode'      ],
                          ['Наименование', 'Код ОКЕИ',        'Условное обозначение'],
                          [ ctString,       ctInteger,         ctString             ],
                          [ True,           True,              True                 ],
                          [ 300,            100,               200                  ],
                          [ taLeftJustify,  taCenter,          taCenter             ],
                          True, ['UnitName'], 1, nil, nil, GridFont,
                          True, 'Фильтр:');
    5: IsOK:= DataBase.EditList('Дополнительные условия выдачи СИЗ',
                         'SIZREASON', 'ReasonID', 'ReasonName',
                          True, True, 400, GridFont,
                          True, 'Фильтр:');
    6: IsOK:= ParamFormOpen(Company, Department);
  end;

  if IsOK then DataUpdate;
end;

procedure TMainForm.SafetyButtonClick(Sender: TObject);
begin
  ControlPopupMenuShow(Sender, SafetyMenu);
end;

procedure TMainForm.TimingButtonClick(Sender: TObject);
begin
  ControlPopupMenuShow(Sender, TimingMenu);
end;

procedure TMainForm.DictionaryButtonClick(Sender: TObject);
begin
  ControlPopupMenuShow(Sender, DictionaryMenu);
end;

procedure TMainForm.EditingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TMainForm.SettingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TMainForm.AboutButtonClick(Sender: TObject);
begin
  FormModalShow(TAboutForm);
end;

procedure TMainForm.StaffButtonClick(Sender: TObject);
begin
  CategorySelect(1);
end;

procedure TMainForm.CalendarMenuItemClick(Sender: TObject);
begin
  CategorySelect(2);
end;

procedure TMainForm.ShiftScheduleMenuItemClick(Sender: TObject);
begin
  CategorySelect(3);
end;

procedure TMainForm.VacationPlaneMenuItemClick(Sender: TObject);
begin
  CategorySelect(4);
end;

procedure TMainForm.PersonalScheduleMenuItemClick(Sender: TObject);
begin
  CategorySelect(5);
end;

procedure TMainForm.TimetableMenuItemClick(Sender: TObject);
begin
  CategorySelect(6);
end;

procedure TMainForm.SIZNormsMenuItemClick(Sender: TObject);
begin
  CategorySelect(7);
end;

procedure TMainForm.SIZStorageMenuItemClick(Sender: TObject);
begin
  CategorySelect(8);
end;

procedure TMainForm.SIZRequestMenuItemClick(Sender: TObject);
begin
  CategorySelect(9);
end;

procedure TMainForm.SIZSizesMenuItemClick(Sender: TObject);
begin
  CategorySelect(10);
end;

procedure TMainForm.SIZStaffMenuItemClick(Sender: TObject);
begin
  CategorySelect(11);
end;

procedure TMainForm.RefreshButtonClick(Sender: TObject);
begin
  DataBase.Reconnect;
  DataUpdate;
end;

procedure TMainForm.PostListMenuItemClick(Sender: TObject);
begin
  DictionarySelect(1);
end;

procedure TMainForm.TimetableMarkMenuItemClick(Sender: TObject);
begin
  DictionarySelect(2);
end;

procedure TMainForm.SIZListMenuItemClick(Sender: TObject);
begin
  DictionarySelect(3);
end;

procedure TMainForm.SIZUnitMenuItemClick(Sender: TObject);
begin
  DictionarySelect(4);
end;

procedure TMainForm.SIZReasonMenuItemClick(Sender: TObject);
begin
  DictionarySelect(5);
end;

procedure TMainForm.DepartmentMenuItemClick(Sender: TObject);
begin
  DictionarySelect(6);
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

end.

