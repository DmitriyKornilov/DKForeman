unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  Menus, BCButton, LCLType,

  //DK packages utils
  DK_Const, DK_LCLStrRus, DK_HeapTrace, DK_CtrlUtils, DK_Fonts, DK_VSTTypes,
  DK_Vector,
  //Project utils
  UDataBase, UUtils, UConst, UTypes,
  //Forms
  UStaffForm,
  UCalendarForm, UScheduleShiftForm, UVacationPlaneForm,
  USchedulePersonalForm, UTimetableForm,
  USIZForm,
  USSOForm,
  UStudyForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutButton: TSpeedButton;
    Bevel4: TBevel;
    CalendarMenuItem: TMenuItem;
    TimetableMarkMenuItem: TMenuItem;
    SIZListMenuItem: TMenuItem;
    SIZUnitMenuItem: TMenuItem;
    SIZReasonMenuItem: TMenuItem;
    SIZSpecLifeMenuItem: TMenuItem;
    SSOListMenuItem: TMenuItem;
    SSOUnitMenuItem: TMenuItem;
    ShiftScheduleMenuItem: TMenuItem;
    VacationPlaneMenuItem: TMenuItem;
    PersonalScheduleMenuItem: TMenuItem;
    TimetableMenuItem: TMenuItem;
    SIZMenuItem: TMenuItem;
    SSOMenuItem: TMenuItem;
    StudyMenuItem: TMenuItem;
    DictionaryButton: TBCButton;
    DictionaryMenu: TPopupMenu;
    PostListMenuItem: TMenuItem;
    MainPanel: TPanel;
    SafetyMenu: TPopupMenu;
    SafetyButton: TBCButton;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TimingMenu: TPopupMenu;
    TimingButton: TBCButton;
    StaffButton: TBCButton;
    RefreshButton: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
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
    procedure SafetyButtonClick(Sender: TObject);
    procedure SettingButtonClick(Sender: TObject);
    procedure ShiftScheduleMenuItemClick(Sender: TObject);
    procedure SIZListMenuItemClick(Sender: TObject);
    procedure SIZMenuItemClick(Sender: TObject);
    procedure SIZReasonMenuItemClick(Sender: TObject);
    procedure SIZSpecLifeMenuItemClick(Sender: TObject);
    procedure SIZUnitMenuItemClick(Sender: TObject);
    procedure SSOListMenuItemClick(Sender: TObject);
    procedure SSOMenuItemClick(Sender: TObject);
    procedure SSOUnitMenuItemClick(Sender: TObject);
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
    procedure SettingsSave;
  public
    GridFont: TFont;
    procedure CategorySelect(const ACategory: Byte);
    procedure DictionarySelect(const ADictionary: Byte);
  end;

var
  MainForm: TMainForm;

const
  MAIN_CAPTION =  'DKForeman v1.0.0';
  MAIN_DESCRIPTION: array of String = (
    {00} '',
    {01} ' - [ШТАТ]',
    {02} ' - [ПРОИЗВОДСТВЕННЫЙ КАЛЕНДАРЬ]',
    {03} ' - [ГРАФИКИ СМЕННОСТИ]',
    {04} ' - [ПЛАНИРОВАНИЕ ОТПУСКОВ]',
    {05} ' - [ГРАФИКИ РАБОТЫ]',
    {06} ' - [ТАБЕЛИ УЧЕТА РАБОЧЕГО ВРЕМЕНИ]',
    {07} ' - [СРЕДСТВА ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ]',
    {08} ' - [СМЫВАЮЩИЕ И ОБЕЗВРЕЖИВАЮЩИЕ СРЕДСТВА]',
    {09} ' - [ИНСТРУКТАЖИ, АТТЕСТАЦИЯ, ОБУЧЕНИЕ]',
    {10} ' - [ГОДОВОЙ ГРАФИК СМЕННОСТИ]',
    {11} ' - [СВОДНЫЙ ГРАФИК СМЕННОСТИ НА МЕСЯЦ]',
    {12} ' - [ГРАФИК ОТПУСКОВ]',
    {13} ' - [ГРАФИК РАБОТЫ НА МЕСЯЦ]'
  );

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

{SQLITE_CONSTRAINT_UNIQUE!!!!!!!!!!!!!!!}


{ TMainForm }

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
    4: ;
    5: (CategoryForm as TSchedulePersonalForm).ViewUpdate(ModeType);
    6: ;
    7: ;
    8: ;
    9: ;
  end;

end;

procedure TMainForm.SettingsSave;
begin
  if not Assigned(CategoryForm) then Exit;

  case Category of
    0: ;
    1: (CategoryForm as TStaffForm).SettingsSave;
    //2: (CategoryForm as TCalendarForm).SettingsSave;
    3: (CategoryForm as TScheduleShiftForm).SettingsSave;
    4: ;
    5: (CategoryForm as TSchedulePersonalForm).SettingsSave;
    6: ;
    7: ;
    8: ;
    9: ;
  end;
end;

procedure TMainForm.CategorySelect(const ACategory: Byte);
begin
  if ACategory=Category then Exit;

  Screen.Cursor:= crHourGlass;
  try
    SettingsSave;
    Category:= ACategory;
    Caption:= MAIN_CAPTION + MAIN_DESCRIPTION[ACategory];
    SettingButton.Enabled:= Category<>2;

    if Assigned(CategoryForm) then FreeAndNil(CategoryForm);
    case Category of
      0: ;
      1: CategoryForm:= FormOnPanelCreate(TStaffForm, MainPanel);
      2: CategoryForm:= FormOnPanelCreate(TCalendarForm, MainPanel);
      3: CategoryForm:= FormOnPanelCreate(TScheduleShiftForm, MainPanel);
      4: CategoryForm:= FormOnPanelCreate(TVacationPlaneForm, MainPanel);
      5: CategoryForm:= FormOnPanelCreate(TSchedulePersonalForm, MainPanel);
      6: CategoryForm:= FormOnPanelCreate(TTimetableForm, MainPanel);
      7: CategoryForm:= FormOnPanelCreate(TSIZForm, MainPanel);
      8: CategoryForm:= FormOnPanelCreate(TSSOForm, MainPanel);
      9: CategoryForm:= FormOnPanelCreate(TStudyForm, MainPanel);
    end;
    if Assigned(CategoryForm) then
    begin
      CategoryForm.Show;
      ViewUpdate;
    end;

  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TMainForm.DictionarySelect(const ADictionary: Byte);
var
  VKeys: TIntVector;
  VPicks: TStrVector;
  IsOK: Boolean;
begin
  IsOK:= False;
  case ADictionary of
    1: IsOK:= DataBase.EditList('Перечень должностей (профессий)',
                         'STAFFPOST', 'PostID', 'PostName', True, True, 400, GridFont);
    2: IsOK:= DataBase.EditTable('Коды табеля учета рабочего времени',
                          'TIMETABLEMARK', 'DigMark',
                          ['DigMark',      'StrMark',       'TypeMark', 'Note'        ],
                          ['Цифровой код', 'Буквенный код', 'Статус',   'Описание'    ],
                          [ ctInteger,      ctString,        ctKeyPick,  ctString     ],
                          [ True,           True,            True,       True         ],
                          [ 100,            100,             100,        400          ],
                          [ taCenter,       taCenter,        taCenter,   taLeftJustify],
                          True, ['DigMark'], 4,
                          [nil,             nil,             TIMETABLE_TYPEMARK_KEYS,  nil],
                          [nil,             nil,             TIMETABLE_TYPEMARK_PICKS, nil],
                          GridFont);
    3: begin
         DataBase.KeyPickList('SIZUNIT', 'UnitID', 'UnitName', VKeys, VPicks);
         IsOK:= DataBase.EditDoubleTable('Перечень средств индивидуальной защиты',
                         'SIZCLASSES', 'ClassID',
                         ['ClassName'],
                         ['Наименование класса'],
                         [ctString],
                         [True],
                         [200],
                         [taLeftJustify],
                         True, ['ClassName'], 1, nil, nil,
                         'SIZNAMES', 'NameID',
                         ['SIZName', 'UnitID', 'SizeType'],
                         ['Наименование',     'Единица измерения', 'Тип размера'],
                         [ctString,            ctKeyPick,           ctKeyPick   ],
                         [True,                True,                True        ],
                         [300,                 150,                 150         ],
                         [taLeftJustify,       taCenter,            taCenter    ],
                         True, ['SIZName'], 1,
                         [nil,                 VKeys,               SIZ_SIZETYPE_KEYS ],
                         [nil,                 VPicks,              SIZ_SIZETYPE_PICKS],
                         'ClassID', GridFont);

       end;
    4: IsOK:= DataBase.EditTable('Единицы измерения средств индивидуальной защиты',
                          'SIZUNIT', 'UnitID',
                          ['UnitName',     'UnitDigitalCode', 'UnitStringCode'      ],
                          ['Наименование', 'Код',             'Условное обозначение'],
                          [ ctString,       ctInteger,         ctString             ],
                          [ True,           True,              True                 ],
                          [ 400,            100,               200                  ],
                          [ taLeftJustify,  taCenter,          taCenter             ],
                          True, ['UnitName'], 1, nil, nil, GridFont);
    5: IsOK:= DataBase.EditList('Дополнительные условия выдачи СИЗ',
                         'SIZREASON', 'ReasonID', 'ReasonName', True, True, 400, GridFont);
    6: IsOK:= DataBase.EditList('Особые сроки службы СИЗ',
                         'SIZSPECLIFE', 'SpecLifeID', 'SpecLifeName', True, True, 400, GridFont);
    7: begin
         DataBase.KeyPickList('SSOUNIT', 'UnitID', 'UnitName', VKeys, VPicks);
         IsOK:= DataBase.EditDoubleTable('Перечень средств индивидуальной защиты',
                         'SSOCLASSES', 'ClassID',
                         ['ClassName'],
                         ['Наименование класса'],
                         [ctString],
                         [True],
                         [200],
                         [taLeftJustify],
                         True, ['ClassName'], 1, nil, nil,
                         'SSONAMES', 'NameID',
                         ['SSOName', 'UnitID', 'Method'],
                         ['Наименование', 'Единица измерения', 'Способ выдачи'],
                         [ctString,        ctKeyPick,           ctKeyPick   ],
                         [True,            True,                True        ],
                         [300,             150,                 150         ],
                         [taLeftJustify,   taCenter,            taCenter    ],
                         True, ['SSOName'], 1,
                         [nil,             VKeys,               SSO_SIZETYPE_KEYS ],
                         [nil,             VPicks,              SSO_SIZETYPE_PICKS],
                         'ClassID', GridFont);

       end;
    8: IsOK:= DataBase.EditTable('Единицы измерения смывающих и обезвреживающих средств',
                          'SSOUNIT', 'UnitID',
                          ['UnitName',     'UnitDigitalCode', 'UnitStringCode'      ],
                          ['Наименование', 'Код',             'Условное обозначение'],
                          [ ctString,       ctInteger,         ctString             ],
                          [ True,           True,              True                 ],
                          [ 400,            100,               200                  ],
                          [ taLeftJustify,  taCenter,          taCenter             ],
                          True, ['UnitName'], 1, nil, nil, GridFont);
  end;

  if IsOK then ViewUpdate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  HeapTraceOutputFile('trace.trc');
  Height:= 300; Width:= 500; //for normal form maximizing
  Caption:= MAIN_CAPTION;

  SetToolPanels([
    ToolPanel
  ]);
  SetToolButtons([
    EditingButton, SettingButton, RefreshButton, AboutButton, ExitButton
  ]);
  SetCategoryButtons([
    StaffButton, TimingButton, SafetyButton, DictionaryButton
  ]);

  SetGridFont;
  DBConnect;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DataBase);
  FreeAndNil(GridFont);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //MainForm.CategorySelect(0);
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

procedure TMainForm.SIZMenuItemClick(Sender: TObject);
begin
  CategorySelect(7);
end;

procedure TMainForm.SSOMenuItemClick(Sender: TObject);
begin
  CategorySelect(8);
end;

procedure TMainForm.StudyMenuItemClick(Sender: TObject);
begin
  CategorySelect(9);
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

procedure TMainForm.SIZSpecLifeMenuItemClick(Sender: TObject);
begin
  DictionarySelect(6);
end;

procedure TMainForm.SSOListMenuItemClick(Sender: TObject);
begin
  DictionarySelect(7);
end;

procedure TMainForm.SSOUnitMenuItemClick(Sender: TObject);
begin
  DictionarySelect(8);
end;

procedure TMainForm.ExitButtonClick(Sender: TObject);
begin
  SettingsSave;
  Close;
end;

end.

