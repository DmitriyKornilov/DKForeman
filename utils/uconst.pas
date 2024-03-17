unit UConst;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  TIMETABLE_TYPEMARK_KEYS: array of Integer =
    (0,               1,      2,            3,              4        );
  TIMETABLE_TYPEMARK_PICKS: array of String  =
    ('<не указано>', 'явка', 'отсутствие', 'сверхурочные', 'выходной');

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








implementation

end.

