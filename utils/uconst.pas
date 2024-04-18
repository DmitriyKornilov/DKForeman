unit UConst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,

  DK_Color;

const
  MONDAY_DATE = 45292; //01.01.2024 - ПН
  EMPTY_MARK  = '—';
  VIEW_PARAMS_CAPTION = 'Параметры отображения';

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
    'SCHEDULESHIFTCALENDARFORM.NEEDCORRECTIONS',
    'SCHEDULESHIFTCALENDARFORM.FIRSTSHIFTDAYCOLORONLY'
  );

implementation

end.

