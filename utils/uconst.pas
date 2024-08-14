unit UConst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,

  DK_Color;

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
    {13} ' - [ОБЩИЙ ГРАФИК РАБОТЫ НА МЕСЯЦ]',
    {14} ' - [ОБЩИЙ ТАБЕЛЬ УЧЕТА РАБОЧЕГО ВРЕМЕНИ ЗА МЕСЯЦ]',
    {15} ' - [ПЛАНИРОВАНИЕ ОТПУСКОВ НА ГОД]'
  );

  MONDAY_DATE = 45292; //01.01.2024 - ПН
  EMPTY_MARK  = '—';
  VIEW_PARAMS_CAPTION = 'Параметры отображения:';

  TIMETABLE_TYPEMARK_KEYS: array of Integer =
    (0,               1,      2,            3,              4        );
  TIMETABLE_TYPEMARK_PICKS: array of String  =
    ('<не указано>', 'явка', 'отсутствие', 'сверхурочные', 'выходной');

  DAY_STATUS_KEYS: array of Integer =
    (0,              1,             2,          3,                 4);
  DAY_STATUS_PICKS: array of String  =
    ('<не указано>', 'праздничный', 'выходной', 'предпраздничный', 'рабочий');
  DAY_NAME_KEYS: array of Integer =
    (0,           1,             2,        3,       4,         5,         6,         7);
  DAY_NAME_PICKS: array of String  =
    (EMPTY_MARK, 'понедельник', 'вторник', 'среда', 'четверг', 'пятница', 'суббота', 'воскресенье');

  GENDER_KEYS: array of Integer =
    (0,   1  );
  GENDER_PICKS: array of String  =
    ('Ж', 'М');

  POST_TEMP_KEYS: array of Integer =
    (0,             1         );
  POST_TEMP_PICKS: array of String  =
    ('Постоянная', 'Временная');

  SIZ_SIZETYPE_KEYS: array of Integer =
    (0,        1,        2,       3,               4,          5,          6,            7          );
  SIZ_SIZETYPE_PICKS: array of String  =
    ('<нет>', 'одежда', 'обувь', 'головной убор', 'рукавицы', 'перчатки', 'противогаз', 'респиратор');

  SSO_SIZETYPE_KEYS: array of Integer =
    (0,                1           );
  SSO_SIZETYPE_PICKS: array of String  =
    ('индивидуально', 'дозированно');

  POST_STATUS_KEYS: array of Integer =
    (0,            1          );
  POST_STATUS_PICKS: array of String  =
    ('Постоянная', 'Временная');

  CALENDAR_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', 'Статус', 'Заменяемый день');
  CALENDAR_CORRECTION_COLUMN_WIDTHS: array of Integer =
    (80,     110,      150              );
  SCHEDULE_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', '№ смены', 'Всего часов', 'Ночных часов', 'Код табеля');
  SCHEDULE_CORRECTION_COLUMN_WIDTHS: array of Integer =
    (70,     70,        80,            90,             70          );
  VACATION_EDIT_COLUMN_NAMES: array of String  =
    ('Отпуск', 'Дата начала', 'Основной (дней)', 'Дополнительный (дней)');
  VACATION_EDIT_COLUMN_WIDTHS: array of Integer =
    (150,       100,           120,               150                   );
  VACATION_EDIT_ROW_NAMES: array of String  =
    ('Планируемый (1 часть)', 'Фактический (1 часть)', 'Планируемый (2 часть)', 'Фактический (2 часть)');

  TIMETABLE_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', 'Табель', 'График сменности', '№ смены');
  TIMETABLE_CORRECTION_COLUMN_WIDTHS: array of Integer =
    (70,     110,       150,                70       );

  COLOR_CALENDAR_MONTHNAME = COLOR_WHITE; //цвет ячейки с названием месяца
  COLOR_CALENDAR_DAYNAME   = COLOR_WHITE; //цвет ячеек с названиями дней недели
  COLOR_CALENDAR_QUARTER   = COLOR_GREEN;
  COLOR_CALENDAR_HALFYEAR  = COLOR_PURPLE;
  COLOR_CALENDAR_YEAR      = COLOR_ORANGE;

  COLORS_CALENDAR: array of Integer = (
    COLOR_BLACK,   //неизвестный
    COLOR_ORANGE,  //праздничный
    COLOR_GREEN,   //выходной
    COLOR_PURPLE,  //предпраздничный
    COLOR_WHITE    //рабочий
  );

  COLOR_SCHEDULE_CORRECTION   = COLOR_YELLOW;
  COLOR_SCHEDULE_NOTWORK      = COLOR_GREEN;
  COLOR_SCHEDULE_TITLE        = COLOR_WHITE;
  COLOR_SCHEDULE_OUTSIDEMONTH = COLOR_GRAY;
  COLOR_SCHEDULE_NOTDEFINE    = COLOR_RED;

  COLOR_TIMETABLE_MANUAL       = COLOR_YELLOW;
  COLOR_TIMETABLE_HOLIDAY      = COLOR_ORANGE;
  COLOR_TIMETABLE_BEFORE       = COLOR_PURPLE;
  COLOR_TIMETABLE_TITLE        = COLOR_WHITE;
  COLOR_TIMETABLE_OUTSIDEMONTH = COLOR_GRAY;
  COLOR_TIMETABLE_NOTDEFINE    = COLOR_RED;
  COLOR_TIMETABLE_NOTWORK      = COLOR_GREEN;

  COLORS_SHIFT: array of Integer = (
    COLOR_WHITE,
    COLOR_YELLOW,
    COLOR_GREEN,
    COLOR_RED,
    COLOR_BLUE,
    COLOR_ORANGE,
    COLOR_AQUA,
    COLOR_PURPLE,
    COLOR_GRAY,
    COLOR_BEIGE,
    COLOR_BROWN
  );

  COLOR_SHIFT_UNDEFINED_VALUE = $007D7D00;
  COLOR_SHIFT_UNDEFINED_NAME  = 'Остальные смены';

  SETTING_NAMES_STAFFORM: array of String  =  (
    'STAFFORM.LISTTYPE',
    'STAFFORM.ORDERTYPE',
    'STAFFORM.NUMBER',
    'STAFFORM.STAFFNAME',
    'STAFFORM.BORNDATE',
    'STAFFORM.GENDER',
    'STAFFORM.TABNUM',
    'STAFFORM.RECRUTDATE',
    'STAFFORM.DISMISSDATE',
    'STAFFORM.RANK',
    'STAFFORM.POSTNAME',
    'STAFFORM.NAMETYPE'
  );

  SETTING_NAMES_SCHEDULESHIFTCALENDARFORM: array of String  =  (
    'SCHEDULESHIFTCALENDARFORM.ZOOM',
    'SCHEDULESHIFTCALENDARFORM.NEEDCORRECTIONS',
    'SCHEDULESHIFTCALENDARFORM.FIRSTSHIFTDAYCOLORONLY'
  );

  SETTING_NAMES_SCHEDULESHIFTMONTHFORM: array of String  =  (
    'SCHEDULESHIFTMONTHFORM.ZOOM'
  );

  SETTING_NAMES_SCHEDULESHIFTFORM: array of String  =  (
    'SCHEDULESHIFTFORM.ZOOM',
    'SCHEDULESHIFTFORM.NEEDNIGHTHOURS',
    'SCHEDULESHIFTFORM.NEEDCORRECTIONS',
    'SCHEDULESHIFTFORM.NEEDNOTWORKMARKS',
    'SCHEDULESHIFTFORM.NEEDCOLORS',
    'SCHEDULESHIFTFORM.RESUMETYPE',
    'SCHEDULESHIFTFORM.COLORTYPE'
  );

  SETTING_NAMES_SCHEDULEPERSONALFORM: array of String  =  (
    'SCHEDULEPERSONALFORM.ZOOM',
    'SCHEDULEPERSONALFORM.NEEDNIGHTHOURS',
    'SCHEDULEPERSONALFORM.NEEDCORRECTIONS',
    'SCHEDULEPERSONALFORM.NEEDNOTWORKMARKS',
    'SCHEDULEPERSONALFORM.NEEDVACATIONS',
    'SCHEDULEPERSONALFORM.NEEDCOLORS',
    'SCHEDULEPERSONALFORM.RESUMETYPE',
    'SCHEDULEPERSONALFORM.COLORTYPE'
  );

  SETTING_NAMES_SCHEDULEPERSONALMONTHFORM: array of String  =  (
    'SCHEDULEPERSONALMONTHFORM.ZOOM',
    'SCHEDULEPERSONALMONTHFORM.NEEDNIGHTHOURS',
    'SCHEDULEPERSONALMONTHFORM.NEEDCORRECTIONS',
    'SCHEDULEPERSONALMONTHFORM.NEEDNOTWORKMARKS',
    'SCHEDULEPERSONALMONTHFORM.NEEDVACATIONS',
    'SCHEDULEPERSONALMONTHFORM.RESUMETYPE',
    'SCHEDULEPERSONALMONTHFORM.PERIODTYPE',

    'SCHEDULEPERSONALMONTHFORM.ORDERNUM',
    'SCHEDULEPERSONALMONTHFORM.POSTNAME',
    'SCHEDULEPERSONALMONTHFORM.TABNUM',
    'SCHEDULEPERSONALMONTHFORM.DAYSCOUNT',
    'SCHEDULEPERSONALMONTHFORM.SUMHOURS',
    'SCHEDULEPERSONALMONTHFORM.NORMHOURS',
    'SCHEDULEPERSONALMONTHFORM.DELTAHOURS',

    'SCHEDULEPERSONALMONTHFORM.SIGNATURETYPE',
    'SCHEDULEPERSONALMONTHFORM.REPEATTITLE',
    'SCHEDULEPERSONALMONTHFORM.PAGENUM'
  );

  SETTING_NAMES_TIMETABLEFORM: array of String  =  (
    'TIMETABLEFORM.ZOOM',
    'TIMETABLEFORM.NEEDCOLORS',
    'TIMETABLEFORM.RESUMETYPE'
  );

  SETTING_NAMES_TIMETABLEMONTHFORM: array of String  =  (
    'TIMETABLEMONTHFORM.ZOOM',

    'TIMETABLEMONTHFORM.TIMETABLETYPE',
    'TIMETABLEMONTHFORM.VIEWTYPE',
    'TIMETABLEMONTHFORM.MONTHTYPE',

    'TIMETABLEMONTHFORM.NEEDNIGHTHOURS',
    'TIMETABLEMONTHFORM.NEEDNOTWORKMARKS',
    'TIMETABLEMONTHFORM.PERIODTYPE',
    'TIMETABLEMONTHFORM.COUNTTYPE',

    'TIMETABLEMONTHFORM.ORDERNUM',
    'TIMETABLEMONTHFORM.POSTNAME',
    'TIMETABLEMONTHFORM.TABNUM',
    'TIMETABLEMONTHFORM.DAYSCOUNT',
    'TIMETABLEMONTHFORM.SUMHOURS',
    'TIMETABLEMONTHFORM.NORMHOURS',
    'TIMETABLEMONTHFORM.DELTAHOURS',
    'TIMETABLEMONTHFORM.NIGHTHOURS',

    'TIMETABLEMONTHFORM.REPEATTITLE',
    'TIMETABLEMONTHFORM.PAGENUM',
    'TIMETABLEMONTHFORM.LINETYPE'
  );

  SETTING_NAMES_VACATIONPLANFORM: array of String  =  (
    'VACATIONPLANFORM.ORDERTYPE',
    'VACATIONPLANFORM.RECRUTTYPE',

    'VACATIONPLANFORM.NUMBER',
    'VACATIONPLANFORM.STAFFNAME',
    'VACATIONPLANFORM.TABNUM',
    'VACATIONPLANFORM.POSTNAME',
    'VACATIONPLANFORM.RECRUTDATE',
    'VACATIONPLANFORM.FIRSTPART',
    'VACATIONPLANFORM.SECONDPART',

    'VACATIONPLANFORM.NAMETYPE'
  );

  SETTING_NAMES_VACATIONPLANNINGFORM: array of String  =  (
    'VACATIONPLANNINGFORM.ZOOM'
  );

implementation

end.

