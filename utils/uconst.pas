unit UConst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  EMPTY_MARK = '—';

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

  CALENDAR_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', 'Статус', 'Заменяемый день');
  SCHEDULE_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', 'Всего часов', 'Ночных часов', 'Код табеля', '№ смены');

  COLOR_GRAY        = $00E0E0E0;
  COLOR_BLACK       = $00000000;
  COLOR_WHITE       = $00FFFFFF;
  COLOR_ORANGE      = $0097CBFF;
  COLOR_GREEN       = $00CCE3CC;

  COLOR_CALENDAR_MONTHNAME = COLOR_WHITE;//COLOR_BEIGE; //цвет ячейки с названием месяца
  COLOR_CALENDAR_DAYNAME   = COLOR_WHITE;//COLOR_GRAY;  //цвет ячеек с названиями дней недели
  COLOR_CALENDAR_QUARTER   = COLOR_GRAY;
  COLOR_CALENDAR_HALFYEAR  = COLOR_GREEN;
  COLOR_CALENDAR_YEAR      = COLOR_ORANGE;

  COLORS_CALENDAR: array [0..4] of Integer = (COLOR_BLACK,   //неизвестный
                                              COLOR_ORANGE,  //праздничный
                                              COLOR_GREEN,   //выходной
                                              COLOR_GRAY, //COLOR_VIOLET,  //предпраздничный
                                              COLOR_WHITE);  //рабочий

  MAIN_COLOR_INDEX      = 0;  //scTransparent
  HOLIDEY_COLOR_INDEX   = 1;
  OFFDAY_COLOR_INDEX    = 2;
  BEFORE_COLOR_INDEX    = 3;
  WEEKDAY_COLOR_INDEX   = 4;
  MONTHNAME_COLOR_INDEX = 5;
  DAYNAME_COLOR_INDEX   = 6;
  HIGHLIGHT_COLOR_INDEX = 7;
  QUARTER_COLOR_INDEX   = 8;
  HALFYEAR_COLOR_INDEX  = 9;
  YEAR_COLOR_INDEX      = 10;


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

implementation

end.

