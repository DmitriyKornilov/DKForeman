unit UConst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  MONDAY_DATE = 45292; //01.01.2024 - ПН
  EMPTY_MARK  = '—';

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

  COLOR_GRAY        = $00E0E0E0;
  COLOR_BLACK       = $00000000;
  COLOR_WHITE       = $00FFFFFF;
  COLOR_ORANGE      = $0097CBFF;
  COLOR_GREEN       = $00CCE3CC;
  COLOR_YELLOW      = $00B3FFFF;

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

  COLOR_SCHEDULE_CORRECTION   = COLOR_YELLOW;
  COLOR_SCHEDULE_NOTWORK      = COLOR_GREEN;
  COLOR_SCHEDULE_TITLE        = COLOR_WHITE;
  COLOR_SCHEDULE_OUTSIDEMONTH = COLOR_GRAY;

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

