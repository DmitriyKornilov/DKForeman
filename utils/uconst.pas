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
    {07} ' - [НОРМЫ ВЫДАЧИ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ]',
    {08} ' - [СКЛАД СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ]',
    {09} ' - [ЗАЯВКА НА ПОСТУПЛЕНИЕ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ]',
    {10} ' - [РАЗМЕРЫ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ СОТРУДНИКОВ]',
    {11} ' - [УЧЕТ ВЫДАЧИ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ]',
    {12} ' - [НОРМЫ ВЫДАЧИ СРЕДСТВ СМЫВАЮЩИХ И ОБЕЗВРЕЖИВАЮЩИХ]',
    {13} ' - [СКЛАД СРЕДСТВ СМЫВАЮЩИХ И ОБЕЗВРЕЖИВАЮЩИХ]',
    {14} ' - [ЗАЯВКА НА ПОСТУПЛЕНИЕ СРЕДСТВ СМЫВАЮЩИХ И ОБЕЗВРЕЖИВАЮЩИХ]',
    {15} ' - [УЧЕТ ВЫДАЧИ СРЕДСТВ СМЫВАЮЩИХ И ОБЕЗВРЕЖИВАЮЩИХ]'
  );

  OTHER_DESCRIPTION: array of String = (
    {00} '',
    {01} ' - [КАЛЕНДАРЬ ГРАФИКА СМЕННОСТИ]',
    {02} ' - [СВОДНЫЙ ГРАФИК СМЕННОСТИ НА МЕСЯЦ]',
    {03} ' - [ГРАФИК ОТПУСКОВ]',
    {04} ' - [ОБЩИЙ ГРАФИК РАБОТЫ НА МЕСЯЦ]',
    {05} ' - [ОБЩИЙ ТАБЕЛЬ УЧЕТА РАБОЧЕГО ВРЕМЕНИ ЗА МЕСЯЦ]',
    {06} ' - [ПЛАНИРОВАНИЕ ОТПУСКОВ НА ГОД]',
    {07} ' - [ДОКУМЕНТЫ ПОСТУПЛЕНИЯ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ НА СКЛАД]',
    {08} ' - [ДОКУМЕНТЫ ВЫДАЧИ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ СОТРУДНИКАМ]',
    {09} ' - [ДОКУМЕНТЫ СПИСАНИЯ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ СО СКЛАДА]',
    {10} ' - [ДОКУМЕНТЫ ВОЗВРАТА СОТРУДНИКАМИ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ НА СКЛАД]',
    {11} ' - [ИСТОРИЯ ДВИЖЕНИЯ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ ПО СКЛАДУ]',
    {12} ' - [ВЕДОМОСТЬ ФОРМЫ МБ-7]',
    {13} ' - [ИСТОРИЯ ВЫДАЧИ СРЕДСТВ ИНДИВИДУАЛЬНОЙ ЗАЩИТЫ]'
  );

  MONDAY_DATE = 45292; //01.01.2024 - ПН
  EMPTY_MARK  = '—';
  VIEW_PARAMS_CAPTION = 'Параметры отображения:';
  MAIN_REASON = '<не указано>';

  (**************************************************************************
                              СПРАВОЧНИКИ
  **************************************************************************)

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

  POST_STATUS_KEYS: array of Integer =
    (0,            1          );
  POST_STATUS_PICKS: array of String  =
    ('Постоянная', 'Временная');

  SIZ_TYPE_KEYS: array of Integer =
    (0,                              1,                              2,                     3,                         4,                     5,                      6,                               7,                               8,                      9,                                     10                  );
  SIZ_TYPE_PICKS: array of String  =
    ('Средства дерматологические', 'Одежда специальная защитная', 'Средства защиты ног', 'Средства защиты головы',  'Средства защиты рук', 'Средства защиты глаз', 'Средства защиты органов дыхания', 'Средства защиты органов слуха', 'Средства защиты лица', 'Средства защиты от падения с высоты', 'Средства защиты опорно-двигательного аппарата');

  SIZ_SIZETYPE_KEYS: array of Integer =
    (0,        1,        2,       3,               4,                   5,                  6          );
  SIZ_SIZETYPE_PICKS: array of String  =
    ('<нет>', 'одежда', 'обувь', 'головной убор', 'рукавицы/перчатки', 'противогаз/маска', 'респиратор/полумаска');

  SIZ_LIFE_KEYS: array of Integer =
    (0,           -1        );
  SIZ_LIFE_PICKS: array of String =
    ('до износа', 'дежурное');

  SSO_SIZETYPE_KEYS: array of Integer =
    (7,        8       );
  SSO_SIZETYPE_PICKS: array of String  =
    ('лично', 'дозатор');

  SIZ_DOCFORM_KEYS: array of Integer =
    (0      );
  SIZ_DOCFORM_PICKS: array of String =
    ('<нет>');



  (**************************************************************************
                   ЗАГОЛОВКИ И ШИРИНА СТОЛБЦОВ ТАБЛИЦ
  **************************************************************************)

  STAFF_STAFFLIST_COLUMN_NAMES: array of String  =
    ('№ п/п', 'Ф.И.О', 'Дата рождения', 'Пол',  'Табельный номер', 'Дата приема', 'Дата увольнения', 'Разряд', 'Должность');
  STAFF_STAFFLIST_COLUMN_WIDTHS: array of Integer =
    ( 50,      220,     120,             50,     120,               120,           120,               60,       200       );

  STAFF_TABNUMLIST_COLUMN_NAMES: array of String  =
    ('Табельный номер', 'Дата приема', 'Дата увольнения', 'Разряд', 'Последняя (текущая) должность');
  STAFF_TABNUMLIST_COLUMN_WIDTHS: array of Integer =
    ( 120,               120,           120,               60,        200          );

  STAFF_POSTLIST_COLUMN_NAMES: array of String  =
    ('Статус должности', 'Дата начала', 'Дата окончания', 'Разряд', 'Должность');
  STAFF_POSTLIST_COLUMN_WIDTHS: array of Integer =
    ( 120,               120,           120,               60,        200          );

  CALENDAR_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', 'Статус', 'Заменяемый день');
  CALENDAR_CORRECTION_COLUMN_WIDTHS: array of Integer =
    (80,     110,      150              );

  SCHEDULE_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', '№ смены', 'Всего часов', 'Ночных часов', 'Код табеля');
  SCHEDULE_CORRECTION_COLUMN_WIDTHS: array of Integer =
    ( 70,     70,        80,            90,             70          );

  SCHEDULESHIFT_LIST_COLUMN_NAMES: array of String  =
    ('№ п/п', 'Наименование графика', 'Часов в неделю');
  SCHEDULESHIFT_LIST_COLUMN_WIDTHS: array of Integer =
    (50,      300,                    120             );

  TIMING_MONTH_STAFFLIST_COLUMN_NAMES: array of String  =
    ('Фамилия И.О.', 'Табельный номер', 'Должность', 'В должности', 'График на начало месяца', 'В графике');
  TIMING_MONTH_STAFFLIST_COLUMN_WIDTHS: array of Integer =
    ( 200,            150,               300,         150,           200,                       150       );

  VACATION_EDIT_COLUMN_NAMES: array of String  =
    ('Отпуск', 'Дата начала', 'Основной (дней)', 'Дополнительный (дней)');
  VACATION_EDIT_COLUMN_WIDTHS: array of Integer =
    (150,       100,           120,               150                   );
  VACATION_EDIT_ROW_NAMES: array of String  =
    ('Планируемый (1 часть)', 'Фактический (1 часть)', 'Планируемый (2 часть)', 'Фактический (2 часть)');

  VACATION_PLAN_STAFFLIST_COLUMN_NAMES: array of String  =
    ('№ п/п', 'Ф.И.О', 'Табельный номер', 'Должность на начало года', 'Дата приема', 'Отпуск (1 часть)', 'Отпуск (2 часть)');
  VACATION_PLAN_STAFFLIST_COLUMN_WIDTHS: array of Integer =
    ( 50,      250,     120,               300,                        100,           150,                150              );

  VACATION_PLANNING_STAFFLIST_COLUMN_NAMES: array of String  =
    ('Фамилия И.О.', 'Табельный номер', 'Должность', 'График на начало года');
  VACATION_PLANNING_STAFFLIST_COLUMN_WIDTHS: array of Integer =
    ( 200,            150,               300,         200                   );

  TIMETABLE_CORRECTION_COLUMN_NAMES: array of String  =
    ('Дата', 'Табель', 'График сменности', '№ смены');
  TIMETABLE_CORRECTION_COLUMN_WIDTHS: array of Integer =
    (70,     110,       150,                70       );

  SIZSIZE_STAFFLIST_COLUMN_NAMES: array of String  =
    ('№ п/п', 'Ф.И.О', 'Одежда', 'Обувь', 'Головной убор', 'СИЗ рук', 'Противогаз', 'Респиратор');
  SIZSIZE_STAFFLIST_COLUMN_WIDTHS: array of Integer =
    ( 50,      300,     100,      100,     100,             100,       100,          100        );


  (**************************************************************************
                                ЦВЕТА
  **************************************************************************)

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

  COLOR_SIZSTATUS_WARN  = COLOR_YELLOW;
  COLOR_SIZSTATUS_ERROR = COLOR_RED;
  COLORS_SIZSTATUS: array of Integer = (
    0, //transparent
    COLOR_SIZSTATUS_WARN,
    COLOR_SIZSTATUS_ERROR
  );
  COLOR_INDEX_SIZSTATUS_WARN  = 1;
  COLOR_INDEX_SIZSTATUS_ERROR = 2;


  (**************************************************************************
                 НАИМЕНОВАНИЯ ПАРАМЕТРОВ ДЛЯ СОХРАНЕНИЯ
  **************************************************************************)
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

  SETTING_NAMES_CALENDARFORM: array of String  =  (
    'CALENDARFORM.ZOOM'
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

  SETTING_NAMES_VACATIONSCHEDULEFORM: array of String  =  (
    'VACATIONSCHEDULEFORM.ZOOM'
  );

  SETTING_NAMES_SIZSIZEFORM: array of String  =  (
    'SIZSIZEFORM.LISTTYPE',
    'SIZSIZEFORM.ORDERTYPE',
    'SIZSIZEFORM.NUMBER',
    'SIZSIZEFORM.STAFFNAME',
    'SIZSIZEFORM.CLOTHES',
    'SIZSIZEFORM.SHOES',
    'SIZSIZEFORM.HEADDRESS',
    'SIZSIZEFORM.MITTENS',
    'SIZSIZEFORM.GLOVES',
    'SIZSIZEFORM.GASMASK',
    'SIZSIZEFORM.RESPIRATOR',
    'SIZSIZEFORM.NAMETYPE'
  );

  SETTING_NAMES_SIZCARDFORM: array of String  =  (
    'SIZCARDFORM.LISTTYPE',
    'SIZCARDFORM.WRITEOFFTYPE'
  );

  SETTING_NAMES_SIZCARDFRONTFORM: array of String  =  (
    'SIZCARDFRONTFORM.ZOOM'
  );

  SETTING_NAMES_SIZCARDBACKFORM: array of String  =  (
    'SIZCARDBACKFORM.ZOOM'
  );

  SETTING_NAMES_SIZCARDSTATUSFORM: array of String  =  (
    'SIZCARDSTATUSFORM.ZOOM',
    'SIZCARDSTATUSFORM.WARNDAYSCOUNT'
  );

  SETTING_NAMES_SIZDOCMB7FORM: array of String  =  (
    'SIZDOCMB7FORM.ZOOM'
  );

  SETTING_NAMES_SIZSTAFFHISTORYFORM: array of String  =  (
    'SIZSTAFFHISTORYFORM.ZOOM'
  );

  SETTING_NAMES_SIZREQUESTFORM: array of String  =  (
    'SIZREQUESTFORM.ZOOM'
  );


implementation

end.

