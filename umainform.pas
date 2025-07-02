unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Menus, LCLType, DividerBevel,

  //DK packages utils
  DK_HeapTrace, DK_Const, DK_LCLStrRus, DK_CtrlUtils, DK_Fonts, DK_VSTTypes,
  //Project utils
  UDataBase, UImages, UConst, UTypes,
  //Forms
  UStaffForm,
  UCalendarForm, UScheduleShiftForm, UVacationPlanForm,
  USchedulePersonalForm, UTimetableForm,
  USIZNameEditForm, USIZNormForm, USIZSizeForm, USIZCardForm, USIZStorageForm,
  UStudyForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TSpeedButton;
    CalendarMenuItem: TMenuItem;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    DividerBevel3: TDividerBevel;
    DictionaryButton: TSpeedButton;
    SIZRequestMenuItem: TMenuItem;
    SIZStorageMenuItem: TMenuItem;
    SIZSizesMenuItem: TMenuItem;
    StaffButton: TSpeedButton;
    SIZNormsMenuItem: TMenuItem;
    Separator3: TMenuItem;
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
    StudyMenuItem: TMenuItem;
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
    procedure CalendarMenuItemClick(Sender: TObject);
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
    procedure SIZStaffMenuItemClick(Sender: TObject);
    procedure SIZNormsMenuItemClick(Sender: TObject);
    procedure SIZReasonMenuItemClick(Sender: TObject);
    procedure SIZSizesMenuItemClick(Sender: TObject);
    procedure SIZUnitMenuItemClick(Sender: TObject);
    procedure StaffButtonClick(Sender: TObject);
    procedure StudyMenuItemClick(Sender: TObject);
    procedure TimetableMarkMenuItemClick(Sender: TObject);
    procedure TimetableMenuItemClick(Sender: TObject);
    procedure TimingButtonClick(Sender: TObject);
    procedure VacationPlaneMenuItemClick(Sender: TObject);
  private
    Category: Byte;
    CategoryForm: TForm;

    procedure SetGridFont;
    procedure DBConnect;

    procedure ViewUpdate;
    procedure DataUpdate;
    procedure SettingsSave;
  public
    GridFont: TFont;
    procedure CategorySelect(const ACategory: Byte);
    procedure DictionarySelect(const ADictionary: Byte);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{TODO учесть замену в таблице SHIFTCOLORS индекса остальных смен с 101 на -1}
{TODO учесть замену в таблице STAFFTABNUM типа TABNUM с INTEGER на TEXT, введен PK TABNUMID INTEGER,
      проверка на незанятость табельного номера для периода}
{TODO учесть замену в таблице STAFFTABNUM типа RANK с INTEGER на TEXT}
{TODO учесть замену в таблице STAFFPOSTLOG TABNUM -> TABNUMID,
      проверка на отсутствие пересечений по периодам}
{TODO учесть замену в таблице STAFFPOSTLOG типа RANK с INTEGER на TEXT}
{TODO учесть замену в таблице TIMETABLELOG TABNUM -> TABNUMID}
{TODO учесть замену в таблице PERSONALCORRECT TABNUM -> TABNUMID}
{TODO учесть замену в таблице STAFFSCHEDULE TABNUM -> TABNUMID,
      проверка на отсутствие пересечений по периодам}
{TODO учесть замену в таблице STAFFVACATION TABNUM -> TABNUMID}
{TODO учесть в STAFFVACATION
      замена PLANDATE->PLAN1DATE, PLANCOUNT->PLAN1COUNT, PLANCOUNTADD->PLAN1COUNTADD,
      добавлено PLAN2DATE, PLAN2COUNT, PLAN2COUNTADD,
      добавлен YEARNUM}
{TODO учесть замену в таблице SIZENTRY типа NOMNUM с INTEGER на TEXT}
{TODO учесть в таблице SIZSTAFFLOGINFO поле SINGLECOMPLECT - пока удалено}

{TODO проверить TDataBase.SIZNormSubItemInfoDelete на соответствие изменениям в БД}

{SQLITE_CONSTRAINT_UNIQUE!!!!!!!!!!!!!!!}




{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  HeapTraceOutputFile('trace.trc');
  Caption:= MAIN_CAPTION;

  Images:= TImages.Create(Self);
  SetGridFont;
  DBConnect;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SettingsSave;
  FreeAndNil(DataBase);
  FreeAndNil(GridFont);
  FreeAndNil(Images);
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

  //CategorySelect(0);
end;

procedure TMainForm.DBConnect;
var
  DBPath, DBName, DDLName: String;
  IsDBFileExists: Boolean;
begin
  DBPath:= ExtractFilePath(Application.ExeName) + DirectorySeparator + 'db' + DirectorySeparator;
  DBName:= DBPath + 'base.db';
  DDLName:= DBPath + 'ddl.sql';
  IsDBFileExists:= FileExists(DBName);

  DataBase:= TDataBase.Create;
  DataBase.Connect(DBName);
  DataBase.ExecuteScript(DDLName);
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
    0: ;
    1: (CategoryForm as TStaffForm).ViewUpdate(ModeType);
    2: (CategoryForm as TCalendarForm).ViewUpdate(ModeType);
    3: (CategoryForm as TScheduleShiftForm).ViewUpdate(ModeType);
    4: (CategoryForm as TVacationPlanForm).ViewUpdate(ModeType);
    5: (CategoryForm as TSchedulePersonalForm).ViewUpdate(ModeType);
    6: (CategoryForm as TTimetableForm).ViewUpdate(ModeType);
    7: (CategoryForm as TSIZNormForm).ViewUpdate(ModeType);
    8: ; //SIZStorage
    9: ; //SIZRequest
    10: (CategoryForm as TSIZSizeForm).ViewUpdate(ModeType);
    11: (CategoryForm as TSIZCardForm).ViewUpdate(ModeType);
  end;

end;

procedure TMainForm.DataUpdate;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    //0: ;
    1: (CategoryForm as TStaffForm).DataUpdate;
    2: (CategoryForm as TCalendarForm).DataUpdate;
    3: (CategoryForm as TScheduleShiftForm).DataUpdate;
    4: (CategoryForm as TVacationPlanForm).DataUpdate;
    5: (CategoryForm as TSchedulePersonalForm).DataUpdate;
    6: (CategoryForm as TTimetableForm).DataUpdate;
    7: (CategoryForm as TSIZNormForm).DataUpdate;
    8: ; //SIZStorage
    9: ; //SIZRequest
    10: (CategoryForm as TSIZSizeForm).DataUpdate;
    11: (CategoryForm as TSIZCardForm).DataUpdate;
  end;
end;

procedure TMainForm.SettingsSave;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    0: ;
    1: (CategoryForm as TStaffForm).SettingsSave;
    2: (CategoryForm as TCalendarForm).SettingsSave;
    3: (CategoryForm as TScheduleShiftForm).SettingsSave;
    4: (CategoryForm as TVacationPlanForm).SettingsSave;
    5: (CategoryForm as TSchedulePersonalForm).SettingsSave;
    6: (CategoryForm as TTimetableForm).SettingsSave;
    //7: TSIZNormForm   - no settings
    8: ; //SIZStorage
    9: ; //SIZRequest
    10: (CategoryForm as TSIZSizeForm).SettingsSave;
    11: (CategoryForm as TSIZCardForm).SettingsSave;
  end;
end;

procedure TMainForm.CategorySelect(const ACategory: Byte);
begin
  if ACategory=Category then Exit;

  Screen.Cursor:= crHourGlass;
  MainPanel.Visible:= False;
  try
    SettingsSave;
    Category:= ACategory;
    Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[ACategory];
    SettingButton.Enabled:= not (Category in [2, 7]);

    if Assigned(CategoryForm) then FreeAndNil(CategoryForm);
    case Category of
      0: ;
      1: CategoryForm:= FormOnPanelCreate(TStaffForm, MainPanel);
      2: CategoryForm:= FormOnPanelCreate(TCalendarForm, MainPanel);
      3: CategoryForm:= FormOnPanelCreate(TScheduleShiftForm, MainPanel);
      4: CategoryForm:= FormOnPanelCreate(TVacationPlanForm, MainPanel);
      5: CategoryForm:= FormOnPanelCreate(TSchedulePersonalForm, MainPanel);
      6: CategoryForm:= FormOnPanelCreate(TTimetableForm, MainPanel);
      7: CategoryForm:= FormOnPanelCreate(TSIZNormForm, MainPanel);
      //8: CategoryForm:= FormOnPanelCreate(TSIZStorageForm, MainPanel);
      //9: CategoryForm:= FormOnPanelCreate(TSIZReaquestForm, MainPanel);
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

procedure TMainForm.DictionarySelect(const ADictionary: Byte);
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
                          ['Наименование', 'Код',             'Условное обозначение'],
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
  end;

  if IsOK then DataUpdate;//ViewUpdate;
end;

procedure TMainForm.SetGridFont;
begin
  GridFont:= TFont.Create;
  GridFont.Name:= FontLikeToName(flTimes{flArial});
  GridFont.Size:= 9{8};
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



procedure TMainForm.StudyMenuItemClick(Sender: TObject);
begin
  //CategorySelect(9);
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

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

end.

