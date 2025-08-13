unit UDataBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //Project utils
  UCalendar, UConst, USchedule, UTimetable, UWorkHours,
  USIZNormTypes, USIZSizes, USIZUtils, USIZCardTypes, UUtils,
  //DK packages utils
  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const,
  DK_DateUtils, DK_VSTDropDown, DK_Dialogs, DK_DBUtils;

type

  { TDataBase }

  TDataBase = class (TSQLite3)
  public

    (**************************************************************************
                                      ПАРАМЕТРЫ
    **************************************************************************)

    function ColorShiftUpdate(const AColorValue, AColorIndex: Integer): Boolean;
    function ColorsShiftUpdate(const AColorValues: TIntVector = nil;
                               const AColorIndexes: TIntVector = nil): Boolean;
    function ColorsShiftLoad(out AColorValues, AColorIndexes: TIntVector): Boolean;
    function SettingLoad(const ASettingName: String): Integer;
    function SettingsLoad(const ASettingNames: TStrVector): TIntVector;
    procedure SettingUpdate(const ASettingName: String; const ASettingValue: Integer);
    procedure SettingsUpdate(const ASettingNames: TStrVector; const ASettingValues: TIntVector);
    function TextParamLoad(const AParamName: String): String;
    procedure TextParamUpdate(const AParamName, AParamValue: String);

    (**************************************************************************
                                     СПРАВОЧНИКИ
    **************************************************************************)

    procedure PostDictionaryLoad(const ADropDown: TVSTDropDown;
                                 out APostIDs: TIntVector;
                                 const ASelectPostID: Integer = -1;
                                 const AIDNotZero: Boolean = True);
    procedure TimetableMarkDictionaryLoad(const ADropDown: TVSTDropDown;
                                 out ADigMarks: TIntVector;
                                 const ASelectDigMark: Integer = -1;
                                 const AIDNotZero: Boolean = True);

    (**************************************************************************
                                         ШТАТ
    **************************************************************************)

    {Cписок людей на дату ADate: True - ОК, False - список пуст;
     AOrderType - сортировка: 0-ФИО, 1-табельный номер, 2-должность, 3-дата рождения,
                              4-дата приема, 5-дата увольнения, 6-разряд;
     AListType - включить в список: 0-всех, 1-работающих, 2-уволенных, 3-без таб.№}
    function StaffListLoad(const ADate: TDate;
                           const AOrderType, AListType: Byte;
                           out AStaffIDs, ATabNumIDs, AGenders: TIntVector;
                           out ABornDates, ARecrutDates, ADismissDates: TDateVector;
                           out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;
    function StaffListLoad(const ADate: TDate;
                           const AOrderType, AListType: Byte;
                           out ATabNumIDs: TIntVector;
                           out AFs, ANs, APs, ATabNums: TStrVector): Boolean;

    {Cписок сотрудников для персональных графиков и табелей
     ABeginDate, AEndDate - отчетный период
     AOrderType - сортировка: 0-ФИО, 1-табельный номер, 2-должность
     AIsDescOrder=True - сортировка по убыванию, False - по возрастанию
     AFilterValue - фильтр по Ф.И.О.
     True - ОК, False - список пуст}
    function StaffListForPersonalTimingLoad(const AFilterValue: String;
                             const ABeginDate, AEndDate: TDate;
                             const AOrderType: Byte;
                             const AIsDescOrder: Boolean;
                             out ATabNumIDs: TIntVector;
                             out ARecrutDates, ADismissDates: TDateVector;
                             out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;

    {Cписок сотрудников для общих графиков и табелей
     ABeginDate, AEndDate - отчетный период
     AOrderType - сортировка: 0-график, 1-должность, 2-ФИО, 3-табельный номер
     True - ОК, False - список пуст}
    function StaffListForCommonTimingLoad(const ABeginDate, AEndDate: TDate;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateVector;
                   out AFs, ANs, APs, ATabNums, APostNames, ARanks, AScheduleNames: TStrVector): Boolean;

    {Cписок сотрудников для планирования отпусков на год
     AYear - отчетный период
     AOrderType - сортировка: 0-график, 1-должность, 2-ФИО, 3-табельный номер
     True - ОК, False - список пуст}
    function StaffListForVacationPlanningLoad(const AYear: Integer;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out AFamilies, ANames, APatronymics, ATabNums, APostNames, AScheduleNames: TStrVector): Boolean;

    {Получение списка не уволенных на дату AReportDate
    для заявки СИЗ (снчала М, потом Ж): True - ОК, False - пусто}
    function StaffListForSIZRequestLoad(const AReportDate: TDate;
                          out ATabNumIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics, ATabNums: TStrVector): Boolean;

    {Добавление данных нового человека: True - ОК, False - ошибка}
    function StaffMainAdd(out AStaffID: Integer;
                          const AFamily, AName, APatronymic: String;
                          const ABornDate: TDate; const AGender: Byte): Boolean;
    {Обновление данных человека: True - ОК, False - ошибка}
    function StaffMainUpdate(const AStaffID: Integer;
                          const AFamily, AName, APatronymic: String;
                          const ABornDate: TDate; const AGender: Byte): Boolean;
    {Получение данных человека: True - ОК, False - нет такого ID}
    function StaffMainLoad(const AStaffID: Integer;
                          out AFamily, AName, APatronymic: String;
                          out ABornDate: TDate; out AGender: Byte): Boolean;

    {Удаление данных человека: True - ОК, False - ошибка}
    function StaffMainDelete(const AStaffID: Integer): Boolean;
    {Получение списка людей: True - ОК, False - список пуст
     AOrderType - порядок сортировки 0 - по ФИО, 1 - по дате рождения
     AFilterValue - фильтр по Ф.И.О.}
    function StaffMainListLoad(const AFilterValue: String;
                          const AOrderType: Byte;
                          const AIsDescOrder: Boolean;
                          out AStaffIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;

    {StaffBirthdaysLoad - загрузка списка ДР на месяц AMonth текущего года,
     если AMonth=0 - весь год, AIncludeDismissed=True - включать уволенных,
     AIsDateOrder=True - сорировка по дате, иначе - по ФИО: True - ОК, False - пусто}
    function StaffBirthdaysLoad(const AMonth: Word;
                              const AIncludeDismissed, AIsDateOrder: Boolean;
                              out AFamilies, ANames, APatronymics: TStrVector;
                              out ABornDates: TDateVector): Boolean;
    {StaffVacationsLoad - загрузка списка отпуско на месяц AMonth года AYear,
     если AMonth=0 - весь год,
     AIsDateOrder=True - сорировка по дате, иначе - по ФИО: True - ОК, False - пусто}
    function StaffVacationsLoad(const AMonth, AYear: Word;
                              const AIsDateOrder: Boolean;
                              out AFamilies, ANames, APatronymics,
                                  ATabNums, APostNames: TStrVector;
                              out ACounts: TIntVector;
                              out ADates: TDateVector): Boolean;
    {Список табельных номеров по ID человека: True - ОК, False - список пуст}
    function StaffTabNumListLoad(const AStaffID: Integer;
                          out ATabNumIDs, APostIDs: TIntVector;
                          out ATabNums, APostNames, ARanks: TStrVector;
                          out ARecrutDates, ADismissDates: TDateVector): Boolean;
    {Добавление нового таб. номера: True - ОК, False - ошибка}
    function StaffTabNumAdd(out ATabNumID: Integer;
                          const AStaffID: Integer;
                          const ATabNum: String;
                          const ARecrutDate: TDate): Boolean;
    {Обновление данных о приеме нового таб. номера: True - ОК, False - ошибка}
    function StaffTabNumUpdate(const ATabNumID: Integer;
                          const ATabNum: String;
                          const ARecrutDate: TDate): Boolean;
    {Обновление даты увольнения таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDismiss(const ATabNumID: Integer; const ADismissDate: TDate): Boolean;
    {Отмена увольнения таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDismissCancel(const ATabNumID: Integer): Boolean;
    {Удаление таб. номера: True - ОК, False - ошибка}
    function StaffTabNumDelete(const ATabNumID: Integer): Boolean;
    {Проверка наличия таб. номера в записи с ID<>ATabNumID: True - да, False - нет}
    function StaffTabNumIsExists(const ATabNumID: Integer; const ATabNum: String): Boolean;
    {Получение периода работы по ID табельного номера: True - ОК, False - нет ID}
    function StaffTabNumWorkPeriodLoad(const ATabNumID: Integer;
                          out ARecrutDate, ADismissDate: TDate): Boolean;
    {Получение StaffID по TabNumID: True - ОК, False - нет ID}
    function StaffIDByTabNumID(const ATabNumID: Integer; out AStaffID: Integer): Boolean;

    {Список уникальных ID с матрицей периодов их действия за общий период по ID таб. номера: True - ОК, False - список пуст}
    function StaffParamIDsForPeriodLoad(const ATabNumID: Integer;
                                  const ABeginDate, AEndDate: TDate;
                                  const ATableName, AIDField, ABDField, AEDField: String;
                                  out AParamIDs: TIntVector;
                                  out AFirstDates, ALastDates: TDateMatrix): Boolean;
    {Список уникальных должностей с матрицей периодов их действия за общий период по ID таб. номера: True - ОК, False - список пуст}
    function StaffPostForPeriodLoad(const ATabNumID: Integer;
                                  const ABeginDate, AEndDate: TDate;
                                  out APostIDs: TIntVector;
                                  out AFirstDates, ALastDates: TDateMatrix): Boolean;
    {Список уникальных графиков с матрицей периодов их действия за общий период по ID таб. номера: True - ОК, False - список пуст}
    function StaffScheduleForPeriodLoad(const ATabNumID: Integer;
                                  const ABeginDate, AEndDate: TDate;
                                  out AScheduleIDs: TIntVector;
                                  out ABeginDates, AEndDates: TDateMatrix): Boolean;

    {Постоянная должность таб номера на дату
    (или первая после даты, если на эту дату еще не работал): True - ОК, False - пусто (уволен)}
    function StaffPostForDate(const ATabNumID: Integer;
                               const ADate: TDate;
                               out APostID: Integer;
                               out APostName, ARank: String): Boolean;
    {Последня постоянная должность}
    procedure StaffPostLast(const ATabNumID: Integer;
                               out APostID: Integer;
                               out APostName, ARank: String);

    {Список переводов по ID таб. номера: True - ОК, False - список пуст}
    function StaffPostLogListLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
    {Новая запись (перевод с последней должности) в таблице переводов по должностям: True - ОК, False - ошибка}
    function StaffPostLogAdd(const APostLogID, ATabNumID, APostID, APostTemp: Integer;
                          const ARank: String;
                          const AFirstDate: TDate): Boolean;
    {Обновление записи в таблице переводов по должностям: True - ОК, False - ошибка}
    function StaffPostLogUpdate(const APrevPostLogID, APostLogID, APostID, APostTemp: Integer;
                          const ARank: String;
                          const AFirstDate: TDate): Boolean;
    {Удаление записи из таблицы переводов по должностям: True - ОК, False - ошибка}
    function StaffPostLogDelete(const APrevPostLogID, APostLogID: Integer;
                          const ALastDate: TDate): Boolean;

    {Инфо по рабочим графикам: True - ОК, False - пусто}
    function StaffPostScheduleInfoLoad(const ATabNumID: Integer;
                          out AInfo: TPostScheduleInfo;
                          const AFromDate: TDate = NULDATE;
                          const AToDate: TDate = INFDATE): Boolean;
    function StaffScheduleInfoLoad(const ATabNumID: Integer;
                          out AInfo: TShiftScheduleInfo;
                          const AFromDate: TDate = NULDATE;
                          const AToDate: TDate = INFDATE): Boolean;
    {История переводов по рабочим графикам: True - ОК, False - пусто}
    function StaffScheduleHistoryLoad(const ATabNumID: Integer;
                          out AHistoryIDs, AScheduleIDs, AWeekHours: TIntVector;
                          out ABeginDates, AEndDates: TDateVector;
                          out AScheduleNames: TStrVector;
                          const AFromDate: TDate = NULDATE;
                          const AToDate: TDate = INFDATE;
                          const AIsDesc: Boolean = True): Boolean;
    {Новая запись (перевод с последнего графика) в таблице переводов по графикам: True - ОК, False - ошибка}
    function StaffScheduleHistoryAdd(const AHistoryID, ATabNumID, AScheduleID: Integer;
                          const ABeginDate: TDate): Boolean;
    {Обновление записи в таблице переводов по графикам: True - ОК, False - ошибка}
    function StaffScheduleHistoryUpdate(const APrevHistoryID, AHistoryID, AScheduleID: Integer;
                          const ABeginDate: TDate): Boolean;
    {Удаление записи из таблицы переводов по графикам: True - ОК, False - ошибка}
    function StaffScheduleHistoryDelete(const APrevHistoryID, AHistoryID: Integer;
                          const AEndDate: TDate): Boolean;

    (**************************************************************************
                                    КАЛЕНДАРЬ
    **************************************************************************)

    {Календарь за период с учетом корректировок}
    procedure CalendarLoad(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
    {Список корректировок календаря: True - ОК, False - список пуст}
    function CalendarCorrectionsLoad(const ABeginDate, AEndDate: TDate;
                                     out ACorrections: TCalendarCorrections): Boolean;
    {Запись или обновление корректировки календаря: True - ОК, False - ошибка}
    function CalendarCorrectionsUpdate(const ACorrections: TCalendarCorrections): Boolean;
    {Удаление корректировки дня календаря: True - ОК, False - ошибка}
    function CalendarCorrectionDelete(const ADate: TDate): Boolean;
    {Вектор дат праздничных дней с начала предыдщего года до конца текущего года}
    function HolidaysLoad(const AYear: Word): TDateVector;
    function HolidaysLoad(const ABeginDate, AEndDate: TDate): TDateVector;
    {Количество праздничных дней за период}
    function HolidaysCount(const ABeginDate, AEndDate: TDate): Integer;

    (**************************************************************************
                                     ГРАФИКИ
    **************************************************************************)

    {Список графиков сменности: True - ОК, False - список пуст}
    function ScheduleMainListLoad(out AScheduleIDs, AWeekHours, ACycleCounts: TIntVector;
                                  out AScheduleNames: TStrVector): Boolean;

    {Структура графика/список корректировок : True - ОК, False - пусто (ошибка)}
    function ScheduleParamsLoad(const ATableName, AFindFieldName, AIDFieldName: String;
                                const AFindValue: Integer;
                                out AParamIDs: TIntVector;
                                out ADates: TDateVector;
                                out ATotalHours, ANightHours, AShiftNums, ADigMarks: TIntVector;
                                out AStrMarks: TStrVector;
                                const ABeginDate: TDate = NULDATE;
                                const AEndDate: TDate = INFDATE): Boolean;
    {Запись или обновление корректировки графика: True - ОК, False - ошибка}
    function ScheduleCorrectionsUpdate(const ATableName, AIDFieldName: String;
                               const AIDValue: Integer;
                               const ACorrections: TScheduleCorrections): Boolean;

    {Цикл (структура) графика сменности: True - ОК, False - пусто (ошибка)}
    function ScheduleCycleLoad(const AScheduleID: Integer;
                               out ACycleIDs: TIntVector;
                               out ACycle: TScheduleCycle): Boolean;

    {Cписок корректировок графика сменности: True - ОК, False - пусто (ошибка)}
    function ScheduleShiftCorrectionsLoad(const AScheduleID: Integer;
                               out ACorrectIDs: TIntVector;
                               out ACorrections: TScheduleCorrections;
                               const ABeginDate: TDate = NULDATE;
                               const AEndDate: TDate = INFDATE): Boolean;
    {Запись или обновление корректировки графика сменности: True - ОК, False - ошибка}
    function ScheduleShiftCorrectionsUpdate(const AScheduleID: Integer;
                               const ACorrections: TScheduleCorrections): Boolean;
    {Удаление корректировки графика сменности: True - ОК, False - ошибка}
    function ScheduleShiftCorrectionDelete(const ACorrectID: Integer): Boolean;

    {Cписок корректировок персонального графика: True - ОК, False - пусто (ошибка)}
    function SchedulePersonalCorrectionsLoad(const ATabNumID: Integer;
                               out ACorrectIDs: TIntVector;
                               out ACorrections: TScheduleCorrections;
                               const ABeginDate: TDate = NULDATE;
                               const AEndDate: TDate = INFDATE): Boolean;
    {Запись или обновление корректировки персонального графика: True - ОК, False - ошибка}
    function SchedulePersonalCorrectionsUpdate(const ATabNumID: Integer;
                               const ACorrections: TScheduleCorrections): Boolean;
    {Удаление корректировки персонального графика: True - ОК, False - ошибка}
    function SchedulePersonalCorrectionDelete(const ACorrectID: Integer): Boolean;

    {Проверка наличия наименования графика в записи с ID<>AScheduleID: True - да, False - нет}
    function ScheduleShiftIsExists(const AScheduleID: Integer; const AScheduleName: String): Boolean;
    {Добавление нового графика сменности: True - ОК, False - ошибка}
    function ScheduleShiftAdd(const AScheduleName: String; const AWeekHours: Integer;
                              var ACycle: TScheduleCycle): Boolean;
    {Обновление графика сменности: True - ОК, False - ошибка}
    function ScheduleShiftUpdate(const AScheduleName: String; const AWeekHours: Integer;
                              const ACycle: TScheduleCycle): Boolean;
    {Удаление графика сменности: True - ОК, False - ошибка}
    function ScheduleShiftDelete(const AScheduleID: Integer): Boolean;

    (**************************************************************************
                                      ОТПУСКА
    **************************************************************************)

    {Информация по отпускам на год для редактирования: True - ОК, False - пусто}
    function VacationsEditingLoad(const ATabNumID, AYear: Integer;
                                  out ADates: TDateVector;
                                  out ACounts, AAddCounts: TIntVector): Boolean;
    {Обновление информации по отпускам на год при редактировании: True - ОК, False - пусто}
    function VacationsEditingUpdate(const ATabNumID, AYear: Integer;
                                  const ADates: TDateVector;
                                  const ACounts, AAddCounts: TIntVector): Boolean;
    {Информация по отпускам за период для графика/табеля : True - ОК, False - пусто
     AIsPlane=True - по плану, AIsPlane=False - по факту}
    function VacationLoad(const ATabNumID: Integer; const ABeginDate, AEndDate: TDate;
                          out AFirstDates: TDateVector; out ACounts, AAddCounts: TIntVector;
                          const AIsPlane: Boolean = False): Boolean;
    {Данные для графика отпусков (форма Т-7): True - ОК, False - пусто}
    function VacationScheduleLoad(const AYear: Integer;
                             out AStaffNames, ATabNums, APostNames: TStrVector;
                             out AFirstDates: TDateVector; out ATotalCounts: TIntVector): Boolean;
    {Список планируемых отпусков на год: True - ОК, False - ошибка}
    function VacationPlanListForYearLoad(const AFilterValue: String;
                             const AOrderType, ARecrutType: Byte;
                             const AYear: Integer;
                             out ATabNumIDs: TIntVector;
                             out AFamilies, ANames, APatronymics, ATabNums, APostNames: TStrVector;
                             out ARecrutDates, APlan1FirstDates, APlan2FirstDates: TDateVector;
                             out APlan1Counts, APlan1AddCounts, APlan2Counts, APlan2AddCounts: TIntVector): Boolean;

    {Проверка наличия записи об отпуске в базе}
    function VacationRecordIsExists(const AYear, ATabNumID: Integer): Boolean;
    {Получение данных о планируемом отпуске}
    function VacationPlanLoad(const AYear, ATabNumID: Integer;
                              out APlan1FirstDate, APlan2FirstDate: TDate;
                              out APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
    {Добавление записи о планируемом отпуске}
    function VacationPlanAdd(const AYear, ATabNumID: Integer;
                              const APlan1FirstDate, APlan2FirstDate: TDate;
                              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
    {Обновление записи планируемого отпуска}
    function VacationPlanUpdate(const AYear, ATabNumID: Integer;
                              const APlan1FirstDate, APlan2FirstDate: TDate;
                              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
    {Редактирование планируемго отпуска}
    function VacationPlanEdit(const AYear, ATabNumID: Integer;
                              const APlan1FirstDate, APlan2FirstDate: TDate;
                              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
    {Обновление даты начала планируемого отпуска}
    function VacationPlanDateUpdate(const AYear, ATabNumID, APart: Integer;
                                    const APlanFirstDate: TDate): Boolean;


    (**************************************************************************
                                      ТАБЕЛИ
    **************************************************************************)

    //коды табеля -------------------------------------------------------------
    {Список кодов табеля: True - ОК, False - пусто}
    function TimetableMarkListLoad(out ADigMarks: TIntVector;
                                   out AStrMarks, ANotes: TStrVector;
                                   const AIDNotZero: Boolean = True;
                                   const AMarkType: Integer = -1): Boolean;
    function TimetableMarkListLoad(out ADigMarks: TIntVector;
                                   out AItemMarks: TStrVector;
                                   const AIDNotZero: Boolean = True;
                                   const AMarkType: Integer = -1): Boolean;
    {Строковый код табеля по цифровому коду}
    function TimetableStrMarkLoad(const ADigMark: Integer): String;

    {Обновление часов по графику}
    procedure TimetableScheduleHoursUpdate(const ATabNumID: Integer;
                          const ADate: TDate;
                          const ASchedHours: Integer);
    {Актуализация записей табеля в соответствии с графиком: True - ОК, False - ошибка}
    function TimetableByScheduleUpdate(const ATabNumID: Integer;
                             const ACalendar: TCalendar;
                             const ASchedule: TPersonalSchedule;
                             const AUpdateWritedOnly: Boolean): Boolean;

    //редактирование табеля----------------------------------------------------
    {Удаление дня из табеля}
    procedure TimetableDayDelete(const ATabNumID: Integer; const ADate: TDate);
    {Удаление дней из табеля: True - ОК, False - ошибка}
    function TimetableDaysDelete(const ATabNumID: Integer;
                                 const ABeginDate, AEndDate: TDate;
                                 const ANeedCommit: Boolean = True): Boolean;
    {Запись дня табеля}
    procedure TimetableDayAdd(const ATabNumID: Integer; const ADate: TDate;
                              const ATimetableDay: TTimetableDay);
    {Замена дней табеля: True - ОК, False - ошибка}
    function TimetableDaysReplace(const ATabNumID: Integer;
                                  const ADates: TDateVector;
                                  const ATimetableDay: TTimetableDay): Boolean;

    {Запись корректировки в табель: True - ОК, False - ошибка}
    function TimetableDaysByCorrectionAdd(const ATabNumID: Integer;
                             ATimetableDay: TTimetableDay;
                             const ACalendar: TCalendar;
                             const ASchedule: TPersonalSchedule;
                             const ANeedDeleteOld: Boolean = True): Boolean;

    //расчет/загрузка табеля --------------------------------------------------
    {Первая и последняя записанные в базу даты табеля: True - ОК, False - пусто}
    function TimetableFirstWritedDateLoad(const ATabNumID: Integer; out ADate: TDate): Boolean;
    function TimetableLastWritedDateLoad(const ATabNumID: Integer; out ADate: TDate): Boolean;
    function TimetableFirstLastWritedDatesLoad(const ATabNumID: Integer; out AFirstDate, ALastDate: TDate): Boolean;

    {Cумма часов за период}
    function TimetableSumHoursInPeriodLoad(const AFieldName: String;
                                   const ATabNumID: Integer;
                                   const ABeginDate, AEndDate: TDate): Integer;
    {Cумма отработанных часов за период}
    function TimetableSumTotalHoursInPeriodLoad(const ATabNumID: Integer;
                                   const ABeginDate, AEndDate: TDate): Integer;
    {Cумма ночных часов за период}
    function TimetableSumNightHoursInPeriodLoad(const ATabNumID: Integer;
                                   const ABeginDate, AEndDate: TDate): Integer;
    {Данные дня табеля: True - ОК, False - пусто}
    function TimetableDayLoad(const TabNumID: Integer; const ADate: TDate;
                          out ATimetableDay: TTimetableDay; out AMarkType:Integer): Boolean;
    {Инфо дня табеля: True - ОК, False - пусто}
    function TimetableDayInfoLoad(const ATabNumID: Integer; const ADate: TDate;
                          out AWritedScheduleHours: Integer;
                          out AIsManualChangedDay: Boolean): Boolean;
    {Данные табеля за месяц для редактирования: True - ОК, False - пусто}
    function TimetableDataMonthForEditLoad(const ATabNumID, AMonth, AYear: Integer;
                       out ADates: TDateVector;
                       out ATimetableStrings, AScheduleNames: TStrVector;
                       out ATotalHours, ANightHours, AOverHours, ASkipHours, ASchedHours,
                           AMainMarks, ASkipMarks, ASchedIDs, AShiftNums: TIntVector): Boolean;
    {Загрузка из базы векторов данных табеля: True - ОК, False - пусто}
    function TimetableDataVectorsLoad(const ATabNumID: Integer; //таб номер
                               const ABeginDate, AEndDate: TDate; //период запроса
                               out ADates: TDateVector;
                               out ASheduleIDs, AShiftNums,
                                 ATotalHours, ANightHours, AOverHours,
                                 ASkipHours, ASchedHours, AMainMarkDig,
                                 ASkipMarkDig, AIsManualChanged, AIsAbsence: TIntVector;
                               out AMainMarkStr, ASkipMarkStr: TStrVector): Boolean;
    {Загрузка итоговых данных табеля за период}
    procedure TimetableDataTotalsLoad(const ATabNumID: Integer;
                               const ABeginDate, AEndDate: TDate; //период запроса
                               out AShiftCount,               //отработано смен
                                 AWorkDaysCount,              //отработано дней
                                 ANotWorkDaysCount,           //выходных, праздничных дней
                                 ATotalHours,                 //отработано часов всего
                                 ANightHours,                 //из них ночных
                                 AOverHours,                  //из них сверхурочных
                                 AHolidayHours,               //из них праздничных (выходных)
                                 ASkipDaysCount,              //пропущено дней
                                 ASkipHours: Integer;         //пропущено часов
                               out ASkipMarksStr,             //перечень кодов отсутствия
                                 ASkipDaysHoursStr:           //перечень дней (часов) отсутствия
                               String);

    (**************************************************************************
                                НОРМЫ ВЫДАЧИ СИЗ
    **************************************************************************)

    {Загрузка из базы списка типовых норм: True - ОК, False - пусто}
    function SIZNormsLoad(out ANormIDs: TIntVector;
                             out ANormNames, ANotes: TStrVector;
                             out ABeginDates, AEndDates: TDateVector): Boolean;

    {Добавление новой нормы: True - ОК, False - ошибка}
    function SIZNormAdd(out ANormID: Integer;
                          const ANormName, ANote: String;
                          const ABeginDate, AEndDate: TDate): Boolean;
    {Обновление нормы: True - ОК, False - ошибка}
    function SIZNormUpdate(const ANormID: Integer;
                          const ANormName, ANote: String;
                          const ABeginDate, AEndDate: TDate): Boolean;

    {Удаление норм с обработкой всех таблиц: True - ОК, False - ошибка}
    function SIZNormDelete(const ANormID: Integer): Boolean;
    function SIZNormItemDelete(const AItemID: Integer): Boolean;
    function SIZNormSubItemDelete(const AItemID, ASubItemID, AReasonID, AOrderNum: Integer): Boolean;
    function SIZNormSubItemInfoDelete(const AInfoIDs: TIntVector;
                                       const ACommit: Boolean = True): Boolean;

    {Загрузка из базы списка пунктов типовых норм: True - ОК, False - пусто}
    function SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs, APostIDs, AOrderNums: TIntVector;
                              out APostNames: TStrVector): Boolean;
    function SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs, AOrderNums: TIntVector;
                              out APostIDs: TIntMatrix;
                              out APostNames: TStrMatrix): Boolean;
    function SIZNormItemLoad(const AItemID: Integer; out AItem: TNormItem): Boolean;

    {Проверка пересечения периода действия нормы для указанной должности с другими
     пунктами этой и других норм}
    function SIZNormItemIntersectionExists(const APostID, AItemID: Integer;
                              const ABeginDate, AEndDate: TDate;
                              out ANormName, AOrderNum: String): Boolean;
    {Следующий свободный OrderNum для пункта}
    function SIZNormItemOrderNumFreeLoad(const ANormID: Integer): Integer;

    {Загрузка списка соответствия должностей и пунктов норм с ANormID: True - ОК, False - пусто}
    function SIZItemsAndPostsAccordanceLoad(const ANormID: Integer;
                                        out APostIDs, AItemIDs, AItemPostIDs: TIntVector): Boolean;
    {Запись соответствий ID должностей APostIDs пункту нормы с AItemID: True - ОК, False - ошибка}
    function SIZItemsAndPostsAccordanceAdd(const AItemID: Integer;
                                        const APostIDs: TIntVector;
                                        const ACommit: Boolean = True): Boolean;
    {Перенос соответствий AItemPostIDs в пункт с AItemID: True - ОК, False - ошибка}
    function SIZItemsAndPostsAccordanceMove(const AItemID: Integer;
                                        const AItemPostIDs: TIntVector;
                                        const ACommit: Boolean = True): Boolean;
    {Запись нового пункта нормы: True - ОК, False - ошибка}
    function SIZNormItemWrite(const ANormID: Integer; out AItemID: Integer;
                              const AOrderNum: Integer;
                              const ACommit: Boolean = True): Boolean;
    {Добавление полностью нового пункта нормы: True - ОК, False - ошибка}
    function SIZNormItemAdd(const ANormID: Integer; out AItemID: Integer;
                            const APostIDs: TIntVector): Boolean;
    {Обновление пункта нормы: True - ОК, False - ошибка}
    function SIZNormItemUpdate(const ANormID, AItemID: Integer;
                            const AAddPostIDs, ADelItemPostIDs: TIntVector): Boolean;
    {Копирование пункта в другие нормы: True - ОК, False - ошибка}
    function SIZNormItemDataCopy(const ANormID, ASourceItemID: Integer;
                            out ADestItemID: Integer;
                            const ACommit: Boolean = True): Boolean;
    function SIZNormItemPartCopy(const ANormID, ASourceItemID: Integer;
                            const AItemPostIDs: TIntVector;
                            const ACommit: Boolean = True): Boolean;
    function SIZNormItemCopy(const ANormID, AItemID: Integer;
                            const APostIDs: TIntVector): Boolean;

    {Перестановка местами пунктов: True - ОК, False - ошибка}
    function SIZNormItemSwap(const AItemID1, AOrderNum1,
                                   AItemID2, AOrderNum2: Integer): Boolean;

    {Загрузка полных данных по строкам пункта типовых норм: True - ОК, False - пусто}
    function SIZNormSubItemsDataLoad(const AItemID: Integer;
             out ASubItemIDs, ASubItemOrderNums, AReasonIDs,
                 AInfoIDs, AInfoOrderNums,
                 ASIZTypes, ANameIDs, ASizeTypes,
                 ANums, ALifes: TIntVector;
             out AReasonNames, ASizNames, AUnits, AClauseNames: TStrVector): Boolean;
    {Следующий свободный OrderNum для строки пункта}
    function SIZNormSubItemOrderNumFreeLoad(const AItemID, AReasonID: Integer): Integer;
    {Сдвиг вверх (уменьшение) OrderNum строки пункт с AItemID для AReasonID,
     начиная с ABeginOrderNum+1. No commit}
    function SIZNormSubItemOrderNumDecrement(const AItemID, AReasonID, ABeginOrderNum: Integer): Boolean;
    {Загрузка строк пункта: True - ОК, False - пусто}
    function SIZNormSubItemsLoad(const AItemID: Integer; var ASubItems: TNormSubItems): Boolean;

    {Запись новой строки пункта: True - ОК, False - ошибка}
    function SIZNormSubItemWrite(const AItemID: Integer;
                                 out ASubItemID: Integer;
                                 const AReasonID, AOrderNum: Integer;
                                 const ACommit: Boolean = True): Boolean;
    function SIZNormSubItemAdd(const AItemID: Integer; var ASubItem: TNormSubItem): Boolean;
    {Обновление строки пункта: True - ОК, False - ошибка}
    function SIZNormSubItemUpdate(const ASubItemID: Integer;
                                 const AReasonID, AOrderNum: Integer;
                                 const ACommit: Boolean = True): Boolean;
    function SIZNormSubItemUpdate(const AItemID: Integer;
                                  const ANewSubItem, AOldSubItem: TNormSubItem): Boolean;
    {Перестановка местами пунктов: True - ОК, False - ошибка}
    function SIZNormSubItemSwap(const ASubItemID1, AOrderNum1,
                                      ASubItemID2, AOrderNum2: Integer): Boolean;

    {Загрузка данных строки пункта типовых норм: True - ОК, False - пусто}
    function SIZNormSubItemInfoLoad(const ASubItemID: Integer;
                                    var AInfo: TNormSubItemInfo): Boolean;
    {Запись нового Info строки пункта: True - ОК, False - ошибка}
    function SIZNormSubItemInfoWrite(const ASubItemID, ANameID,
                                    ANum, ALife, AOrderNum: Integer;
                                    const AClauseName: String;
                                    out AInfoID: Integer;
                                    const ACommit: Boolean = True): Boolean;
    function SIZNormSubItemInfoWrite(const ASubItemID: Integer;
                                    const AInfo: TNormSubItemInfo;
                                    out AInfoIDs: TIntVector;
                                    const ACommit: Boolean = True): Boolean;
    {Обновление Info: True - ОК, False - ошибка}
    function SIZNormSubItemInfoUpdate(const AInfoID, ANameID,
                                    ANum, ALife, AOrderNum: Integer;
                                    const AClauseName: String;
                                    const ACommit: Boolean = True): Boolean;

    {Загрузка ассортимента СИЗ: True - ОК, False - ошибка}
    function SIZAssortmentLoad(out ASIZTypes: TIntVector;
                              //out ATypeNames: TStrVector;
                              out ASIZNames, AUnits: TStrMatrix;
                              out ANameIDs, ASizeTypes: TIntMatrix): Boolean;

    (**************************************************************************
                                РАЗМЕРЫ СИЗ
    **************************************************************************)

    function SIZStaffSizeLoad(const AFilterValue: String;
                          const AOrderType, AListType: Byte;
                          out AStaffIDs, AClothes, AHeights, AShoes, AHeads,
                              AHands, AGasmasks, ARespirators: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;
    function SIZStaffSizeLoad(const AStaffID: Integer;
                          var ASizeIndexes: TSIZStaffSizeIndexes): Boolean;
    function SIZStaffSizeUpdate(const AStaffID: Integer;
                          const ASizeIndexes: TSIZStaffSizeIndexes): Boolean;

    function SIZSpecSizeLoad(const ATabNumID, AInfoID: Integer;
                             out ASizeID, AHeightID: Integer): Boolean;
    function SIZSpecSizeLoad(const AInfoID: Integer;
                             out ATabNumIDs, ASizeIDs, AHeightIDs: TIntVector): Boolean;
    function SIZSpecSizeWrite(const AInfoID, ATabNumID, ASizeID, AHeightID: Integer;
                             const ACommit: Boolean = True): Boolean;
    function SIZSpecSizeWrite(const AInfoID: Integer;
                             const ATabNumIDs, ASizeIDs, AHeightIDs: TIntVector;
                             const ACommit: Boolean = True): Boolean;
    function SIZSpecSizeExists(const AInfoID, ATabNumID: Integer): Boolean;
    function SIZSpecSizeUpdate(const AInfoID, ATabNumID,
                                     ASizeID, AHeightID: Integer): Boolean;
    function SIZSpecSizeCopy(const ASourceInfoIDs, ADestInfoIDs: TIntVector;
                             const ACommit: Boolean = True): Boolean;

    procedure SizStatusSizeLoad(const ATabNumID, AInfoID, ASizeType: Integer;
                             const AStaffSizes: TSizStaffSizeIndexes;
                             out ASizeID, AHeightID: Integer);
    procedure SIZStatusSizeLoad(const ATabNumID: Integer;
                               const AInfoIDs, ASizeTypes: TIntVector;
                               const AStaffSizes: TSizStaffSizeIndexes;
                               out ASizeIDs, AHeightIDs: TIntVector);

    (**************************************************************************
                                ДОКУМЕНТЫ СИЗ
    **************************************************************************)

    {Проверка наличия документа: True - есть, False - нет}
    function SIZDocExists(const ADocID: Integer;
                          const ADocName, ADocNum: String;
                          const ADocDate: TDate): Boolean;
    {Загрузка списка документов: True - ОК, False - пусто,
     при AYear=0 - документы за все время,
     при ADocType=0 - документы всех типов}
    function SIZDocListLoad(const AYear, ADocType: Integer;
                          out ADocIDs, ADocForms: TIntVector;
                          out ADocNames, ADocNums: TStrVector;
                          out ADocDates: TDateVector): Boolean;
    {Запись нового документа: True - ОК, False - ошибка}
    function SIZDocWrite(out ADocID: Integer;
                         const ADocName, ADocNum: String;
                         const ADocDate: TDate;
                         const ADocType, ADocForm: Integer): Boolean;
    {Обновление документа: True - ОК, False - ошибка}
    function SIZDocLoad(const ADocID: Integer;
                         out ADocName, ADocNum: String;
                         out ADocDate: TDate;
                         out ADocType, ADocForm: Integer): Boolean;
    {Данные документа: True - ОК, False - пусто}
    function SIZDocUpdate(const ADocID: Integer;
                         const ADocName, ADocNum: String;
                         const ADocDate: TDate;
                         const ADocType, ADocForm: Integer): Boolean;

    {Удаление документа прихода СИЗ на склад: True - ОК, False - ошибка}
    function SIZDocStoreEntryDelete(const ADocID: Integer): Boolean;
    {Удаление документа списания (передачи) СИЗ со склада: True - ОК, False - ошибка}
    function SIZDocStoreWriteoffDelete(const ADocID: Integer): Boolean;
    {Удаление документа выдачи СИЗ: True - ОК, False - ошибка}
    function SIZDocReceivingDelete(const ADocID: Integer): Boolean;
    {Удаление документа возврата СИЗ: True - ОК, False - ошибка}
    function SIZDocReturningDelete(const ADocID: Integer): Boolean;

    {Удаление пустых документов за год: True - ОК, False - ошибка}
    function SIZDocStoreEmptyDelete(const ADocType, AYear: Integer): Boolean;


    (**************************************************************************
                                СКЛАД СИЗ
    **************************************************************************)

    {Загрузка документа поступления СИЗ на склад: True - ОК, False - пусто}
    function SIZStoreEntryLoad(const ADocID: Integer;
                         out AEntryIDs: TInt64Vector;
                         out ANomNums, ASizNames, ASizUnits, ANotes: TStrVector;
                         out ASizCounts, ASizTypes, ANameIDs, ASizeIDs,
                             AHeightIDs, ASizeTypes: TIntVector): Boolean;
    function SIZStoreEntryLoad(const ADocID: Integer;
                         out AEntryIDs: TInt64Matrix;
                         out ANomNums, ASizNames, ASizUnits, ANotes: TStrMatrix;
                         out ASizCounts, ASizTypes, ANameIDs, ASizeIDs,
                             AHeightIDs, ASizeTypes: TIntMatrix): Boolean;
    {Проверка наличия в документе прихода с ADocID записи, с другим AEntryID и
     указанными параметрами СИЗ: True - есть, False - нет  }
    function SIZStoreEntryExists(const ADocID: Integer;
                                 const AEntryID: Int64;
                                 const ANameID, ASizeID, AHeightID: Integer;
                                 const ANomNum: String): Boolean;

    {Запись СИЗ в лог склада: True - ОК, False - ошибка}
    procedure SIZStoreLogWrite(const AEntryID: Int64; const ACount: Integer);
    {Определение свободных СИЗ в логах склада: : True - ОК, False - ошибка}
    function SIZStoreLogFreeIDs(const AEntryID: Int64;
                                const ACount: Integer;
                                out AStoreIDs: TInt64Vector): Boolean;

    {Запись СИЗ в документ прихода на склад: True - ОК, False - ошибка}
    function SIZStoreEntryAdd(const ADocID: Integer;
                         out AEntryID: Int64;
                         const ANomNum, ANote: String;
                         const ANameID, ASizeID, AHeightID, ACount: Integer;
                         const ACommit: Boolean = True): Boolean;
    {Обновление СИЗ в документе прихода на склад: True - ОК, False - ошибка}
    function SIZStoreEntryUpdate(const ADocID: Integer;
                         const AEntryID: Int64;
                         const ANomNum, ANote: String;
                         const ANameID, ASizeID, AHeightID, ACount, AOldCount: Integer): Boolean;
    {Удаление информации о поступлении СИЗ: True - ОК, False - ошибка}
    function SIZStoreEntryDelete(const AEntryID: Int64; const ACommit: Boolean = True): Boolean;


    {Загрузка документа списания (передачи) СИЗ со склада: True - ОК, False - пусто}
    function SIZStoreWriteOffLoad(const ADocID: Integer;
                         out AStoreIDs: TInt64Vector;
                         out ANomNums, ASizNames, ASizUnits, AEntryDocNames,
                             AEntryDocNums, ANotes: TStrVector;
                         out ASizeIDs, AHeightIDs, ASizeTypes, ANameIDs: TIntVector;
                         out AEntryDocDates: TDateVector): Boolean;
    function SIZStoreWriteOffLoad(const ADocID: Integer;
                         out ACategoryNames: TStrMatrix;
                         out AStoreIDs: TInt64Matrix;
                         out ASizCounts: TIntMatrix;
                         out ANomNums, ASizNames, ASizUnits,
                             ASizSizes, AEntryDocNames, ANotes: TStrMatrix): Boolean;
    {Запись СИЗ в документ списания (передачи) со склада: True - ОК, False - ошибка}
    function SIZStoreWriteoffAdd(const ADocID: Integer;
                                 const AStoreIDs: TInt64Vector;
                                 const ANote: String): Boolean;


    {Загрузка списка СИЗ на складе: True - ОК, False - пусто}
    function SIZStoreLoad(const ASIZType: Integer; //-1 = все типы
                         out AStoreIDs: TInt64Vector;
                         out ANomNums, ASizNames, ASizUnits, ADocNames, ADocNums: TStrVector;
                         out ASizeIDs, AHeightIDs, ASizeTypes, ANameIDs: TIntVector;
                         out ADocDates: TDateVector): Boolean;
    function SIZStoreLoad(const ASIZType: Integer; //-1 = все типы
                         out ACategoryNames: TStrMatrix;
                         out AStoreIDs: TInt64Matrix;
                         out ASizCounts: TIntMatrix;
                         out ANomNums, ASizNames, ASizUnits,
                             ASizSizes, ADocNames: TStrMatrix;
                         const ANeedCountInCategory: Boolean = True): Boolean;

    {Загрузка документа выдачи СИЗ: True - ОК, False - пусто}
    function SIZStoreReceivingLoad(const ADocID: Integer;
                         out ALogIDs, AStoreIDs: TInt64Vector;
                         out AFs, ANs, APs, ATabNums, APostNames,
                             ANomNums, ASizNames, ASizSTRUnits: TStrVector;
                         out ATabNumIDs, ANums, ALifes, ANameIDs, ASizDIGUnits,
                             ASIZTypes, ASizeIDs: TIntVector;
                         out AReceivingDates: TDateVector): Boolean;
    function SIZStoreReceivingLoad(const ADocID: Integer;
                         out AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                         out AStoreIDs: TInt64Matrix3D;
                         out ASizCounts, ASizDigUnits: TIntMatrix;
                         out ANomNums, ASizNames, ASizStrUnits, ASizLifes: TStrMatrix;
                         out AReceivingDates: TDateMatrix): Boolean;

    {Загрузка документа возврата СИЗ: True - ОК, False - пусто}
    function SIZStoreReturningLoad(const ADocID: Integer;
                         out ALogIDs, AStoreIDs: TInt64Vector;
                         out AFs, ANs, APs, ATabNums, APostNames,
                             ANomNums, ASizNames, ASizSTRUnits,
                             AReceivingDocNames, AReceivingDocNums, ANotes: TStrVector;
                         out ATabNumIDs, ANums, ALifes, ANameIDs, ASizDIGUnits: TIntVector;
                         out AReceivingDates: TDateVector): Boolean;
    function SIZStoreReturningLoad(const ADocID: Integer;
                         out AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                         out AStoreIDs: TInt64Matrix3D;
                         out ASizCounts, ASizDigUnits: TIntMatrix;
                         out ANomNums, ASizNames, ASizStrUnits, ASizLifes,
                             AReceivingDocNames, AReceivingDocNums, ANotes: TStrMatrix;
                         out AReceivingDates: TDateMatrix): Boolean;

    {Отмена прихода на склад: True - ОК, False - пусто}
    function SIZStoreEntryCancel(const AStoreIDs: TInt64Vector;
                                 const ACommit: Boolean = True): Boolean;
    {Отмена выдачи: True - ОК, False - пусто}
    function SIZStoreReceivingCancel(const AStoreIDs: TInt64Vector;
                                 const ACommit: Boolean = True): Boolean;
    {Отмена возврата на склад: True - ОК, False - пусто}
    function SIZStoreReturningCancel(const AStoreIDs: TInt64Vector;
                                     const ACommit: Boolean = True): Boolean;
    {Отмена списания (передачи) со склада: True - ОК, False - пусто}
    function SIZStoreWriteoffCancel(const AStoreIDs: TInt64Vector;
                                 const ACommit: Boolean = True): Boolean;
    {История движения СИЗ по складу: True - ОК, False - пусто}
    function SIZStoreHistorySizListLoad(const AMatchSizName, AFilterNomNum: String;
                                 out ANomNums, ASizNames: TStrVector;
                                 out ANameIDs: TIntVector): Boolean;
    function SIZStoreHistoryEntryLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 const AIsNeedReturning: Boolean;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
    function SIZStoreHistoryReceivingLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 const AIsNeedReturning: Boolean;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
    function SIZStoreHistoryReturningLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
    function SIZStoreHistoryWriteoffLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 const AIsNeedReturning: Boolean;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
    function SIZStoreHistoryLoad(const AMatchSizName, AFilterNomNum: String;
                                 const AIsNeedReturning: Boolean;
                                 out ANomNums, ASizNames: TStrVector;
                                 out AInfos: TStrMatrix;
                                 out ACounts: TIntMatrix): Boolean;

    {Загрузка заявки СИЗ на дату AReportDate: True - OK, False - пусто
     (Без дерматологических СИЗ)
     AWriteoffType - расчет даты следующей выдачи:
     0 - по нормам на момент выдачи,
     1 - по текущим нормам}
    function SIZStoreRequestLoad(const AReportDate: TDate;
                          const AWriteoffType: Byte;
                          out ASizNames: TStrVector;
                          out AGenders: TIntVector;
                          out ASIZSizes: TStrMatrix;
                          out AFamilies, ANames, APatronymics, ATabNums: TStrMatrix3D;
                          out ASizCounts: TIntMatrix3D;
                          out AWriteoffDates: TDateMatrix3D): Boolean;

    (**************************************************************************
                                ЛИЧНЫЕ КАРТОЧКИ СИЗ
    **************************************************************************)

    {Cписок сотрудников для вывода личных карточек
     AListType - включать в список 0-всех, 1-работающих на текущую дату, 2-уволенных
     AOrderType - сортировка: 0-ФИО, 1-табельный номер, 2-должность
     AIsDescOrder=True - сортировка по убыванию, False - по возрастанию
     AFilterValue - фильтр по Ф.И.О.
     True - ОК, False - список пуст}
    function SIZStaffListForPersonalCardsLoad(const AFilterValue: String;
                             const AListType, AOrderType: Byte;
                             const AIsDescOrder: Boolean;
                             out AStaffIDs, ATabNumIDs: TIntVector;
                             out AFs, ANs, APs, AGenders, ATabNums, APostNames: TStrVector): Boolean;
    {Получение данных для формирования списка личных карточек
     для табельного номера ATabNumID: True - ОК, False - пусто}
    function SIZPersonalCardListDataLoad(const ATabNumID: Integer;
                 out ACardIDs, AItemIDs, AItemPostIDs: TIntVector;
                 out ACardNums, APostNames, ANormNames, ANormNotes: TStrVector;
                 out ATabNumBDs, ATabNumEDs, ANormBDs, ANormEDs: TDateVector): Boolean;
    {Получение списка личных карточек для табельного номера ATabNumID:
     True - ОК, False - пусто}
    function SIZPersonalCardListLoad(const ATabNumID: Integer;
                 out ACardIDs, AItemIDs, AItemPostIDs: TIntVector;
                 out ACardNums, APostNames, ANormNames: TStrVector;
                 out ACardBDs, ACardEDs: TDateVector): Boolean;
    {Получение данных личной карточки для табельного номера ATabNumID
     на дату ADate: True - ОК, False - пусто}
    function SIZPersonalCardForDateLoad(const ATabNumID: Integer;
                 const ADate: TDate;
                 out ACardID, AItemID, AItemPostID: Integer;
                 out ACardNum, APostName, ANormName: String;
                 out ACardBD, ACardED: TDate): Boolean;

    {Получение СИЗ из личной карточки: True - ОК, False - пусто
     Если ACardID>0 - СИЗ для конкретной карточки (ATabNumID может быть =0),
     Если ACardID=0 и ATabNumID>0 - вся история для этого таб. номера}
    function SIZPersonalCardDataLoad(const ATabNumID, ACardID: Integer;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANameIDs, ASizeTypes, ANormSIZTypes, ASizeIDs: TIntVector;
                                out ANormSizNames, AReceivingSizNames, AReceivingDocNames,
                                    AReturningDocNames, AWriteoffDocNames: TStrVector): Boolean;
    function SIZPersonalCardSIZLoad(const ATabNumID, ACardID: Integer;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANormSIZTypes: TIntVector;
                                out ANormSizNames, AReceivingDocNames, AReturningDocNames: TStrVector;
                                out AReceivingSizNames, AWriteoffDocNames: TStrMatrix;
                                out ASizCounts, ASizeTypes: TIntMatrix): Boolean;

    {Получение списка предыдущих личных карточек для табельного номера ATabNumID
     с картой CardID: True - ОК, False - пусто}
    function SIZPrevCardListLoad(const ATabNumID: Integer;
                 const ACardBD: TDate;
                 out ACardIDs: TIntVector;
                 out ACardNums, APostNames, ANormNames: TStrVector;
                 out ACardBDs, ACardEDs: TDateVector): Boolean;

    {Получение СИЗ из предыдущей личной карточки для учета в новой: True - ОК, False - пусто}
    function SIZPrevCardDataLoad(const ACardID, ASIZType: Integer;
                                out ALogIDs, AStoreIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANameIDs, ANums, ALifes, ANormSIZTypes, ASizeIDs,
                                    AReceivingDocIDs, AReceivingInfoIDs: TIntVector;
                                out AReceivingSizNames, AReceivingDocNames, ANormSizNames: TStrVector): Boolean;
    function SIZPrevCardSIZLoad(const ACardID, ASIZType: Integer;
                                const AReportDate: TDate;
                                out AReceivingDocIDs, AReceivingInfoIDs: TIntVector;
                                out ANormSizNames: TStrVector;
                                out AReceivingDates, AWriteoffDates: TDateMatrix;
                                out AStoreIDs: TInt64Matrix;
                                out AReceivingDocNames, AReceivingSizNames: TStrMatrix;
                                out ASizCounts: TIntMatrix): Boolean;

    {Запись личной карточки: True - ОК, False - ошибка}
    function SIZPersonalCardAdd(out ACardID: Integer;
                                const ACardNum: String;
                                const ATabNumID, AItemPostID: Integer;
                                const ACommit: Boolean = True): Boolean;
    {Обновление номера личной карточки: True - ОК, False - ошибка}
    function SIZPersonalCardUpdate(const ACardID: Integer;
                                const ACardNum: String): Boolean;

    {Запись СИЗ в таблицы логов личной карточки}
    procedure SIZPersonalCardLogWrite(out ALogID: Int64;
                                 const ACardID, ANowInfoID,
                                       AReceivingInfoID, ADocID: Integer;
                                 const AReceivingDate: TDate);
    procedure SIZPersonalCardLogInfoWrite(const ALogID: Int64;
                                 const AStoreIDs: TInt64Vector);

    {Получение подстроки статуса СИЗ для ACardID}
    function SIZStatusInfoDataLoad(const ACardID, AWriteoffType, AInfoID: Integer;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANameIDs, ANums, ALifes, ASizeIDs, ASizTypes: TIntVector;
                                out ASizNames: TStrVector): Boolean;
    procedure SIZStatusInfoLoad(const AReportDate: TDate;
                                const ACardID, AWriteoffType, AInfoID: Integer;
                                out AIsInfoFreshExists: Boolean;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AWriteoffDates: TDateVector;
                                out ASizNames: TStrMatrix;
                                out ASizCounts: TIntMatrix);
    {Получение строки статуса СИЗ для таб номера ATabNumID и соответствующих AInfoIDs}
    procedure SIZStatusSubItemLoad(const AReportDate: TDate;
                                const ACardID, AWriteoffType: Integer;
                                const AInfoIDs: TIntVector;
                                var AStatusSubItem: TStatusSubItem);
    {Получение статуса СИЗ для таб номера ATabNumID и соответствующих норм ANormSubItems}
    //AWriteoffType - расчет даты следующей выдачи:
    // 0 - по нормам на момент выдачи,
    // 1 - по текущим нормам}
    function SIZStatusLoad(const ATabNumID, ACardID, AWriteoffType: Integer;
                           const AReportDate: TDate;
                           const ANormSubItems: TNormSubItems;
                           var AStatusSubItems: TStatusSubItems): Boolean;


    {Запись информации о выдаче СИЗ: True - ОК, False - ошибка}
    function SIZReceivingWrite(const ACardID, ATabNumID, AItemPostID,
                                     AReceivingInfoID, ANowInfoID, ADocID: Integer;
                            const AStoreIDs: TInt64Vector;
                            const AReceivingDate: TDate): Boolean;
    {Удаление информации о выдаче СИЗ (отмена выдачи): True - ОК, False - ошибка}
    function SIZReceivingCancel(const ALogID: Int64; const ACommit: Boolean = True): Boolean;
    {Учет информации о выдаче СИЗ в следующей личной карточке: True - ОК, False - ошибка}
    function SIZReceivingNextWrite(const ACardID, ATabNumID, AItemPostID, ANowInfoID: Integer;
                            const AReceivingInfoIDs, ADocIDs: TIntVector;
                            const AReceivingDates: TDateVector;
                            const AStoreIDs: TInt64Matrix): Boolean;

    {Получение данных для записи возврата СИЗ на склад: True - ОК, False - пусто}
    function SIZReturningDataLoad(const ALogID: Int64;
                            out AStoreIDs: TInt64Matrix;
                            out ANomNums: TStrVector;
                            out ANameIDs, ASizeIDs, AHeightIDs: TIntVector): Boolean;
    {Возврат СИЗ на склад: True - ОК, False - ошибка}
    function SIZReturningWrite(const ADocID: Integer;
                               const ALogID: Int64;
                               const ANote: String): Boolean;
    {Удаление информации о возврате СИЗ (отмена возврата): True - ОК, False - ошибка}
    function SIZReturningCancel(const ALogID: Int64; const ACommit: Boolean = True): Boolean;

    (**************************************************************************
                                    ИНСТРУКТАЖИ
    **************************************************************************)

    {Получение списка инcтруктажей : True - ОК, False - пусто}
    function BriefingListLoad(out ABriefIDs, AObjects, APeriods, ANums: TIntVector;
                              out ABriefNames, ANotes: TStrVector;
                              out ABeginDates, AEndDates, ALastDates: TDateVector): Boolean;

    function BriefingPostObjectsLoad(const ABriefID: Integer;
                                   out APostIDs: TIntVector;
                                   out APostNames: TStrVector): Boolean;
    function BriefingTabNumObjectsLoad(const ABriefID: Integer;
                                   out ATabNumIDs: TIntVector;
                                   out AFs, ANs, APs, ATabNums: TStrVector): Boolean;

    procedure BriefingListObjectNamesLoad(const ABriefIDs, AObjects: TIntVector;
                                   out AObjectIDs: TIntMatrix;
                                   out AObjectNames: TStrMatrix);

    {Запись ID должностей или таб номеров для инструктажа: True - ОК, False - ошибка}
    function BriefingIDsWrite(const ABriefID, AObject: Integer;
                              const AObjectIDs: TIntVector;
                              const ACommit: Boolean = True): Boolean;
    {Добавление инструктажа: True - ОК, False - ошибка}
    function BriefingAdd(out ABriefID: Integer;
                         const ABriefName, ANote: String;
                         const ABeginDate, AEndDate, ALastDate: TDate;
                         const AObject, APeriod, ANum: Integer;
                         const AObjectIDs: TIntVector): Boolean;
    {Обновление инструктажа: True - ОК, False - ошибка}
    function BriefingUpdate(const ABriefID: Integer;
                         const ABriefName, ANote: String;
                         const ABeginDate, AEndDate, ALastDate: TDate;
                         const AOldObject, AObject, APeriod, ANum: Integer;
                         const AObjectIDs: TIntVector): Boolean;
  end;

implementation

{ TDataBase }

function TDataBase.ColorShiftUpdate(const AColorValue, AColorIndex: Integer): Boolean;
begin
  Result:= UpdateByInt32ID('SHIFTCOLORS', 'ColorValue', 'ColorIndex', AColorIndex, AColorValue);
end;

function TDataBase.ColorsShiftUpdate(const AColorValues: TIntVector = nil;
                                     const AColorIndexes: TIntVector = nil): Boolean;
var
  ColorIndexes, ColorValues: TIntVector;
  i: Integer;
begin
  if VIsNil(AColorValues) then
  begin
    ColorValues:= VCreateInt(COLORS_SHIFT);
    VAppend(ColorValues, COLOR_SHIFT_UNDEFINED_VALUE);
  end
  else
    ColorValues:= AColorValues;

  if VIsNil(AColorIndexes) then
    ColorIndexes:= VOrder(High(ColorValues), True)
  else
    ColorIndexes:= AColorIndexes;

  Result:= False;
  QSetQuery(FQuery);
  try
    //удаляем старые значения
    QSetSQL(
      'DELETE FROM SHIFTCOLORS'
    );
    QExec;
    //записываем новые значения
    QSetSQL(
      sqlINSERT('SHIFTCOLORS', ['ColorIndex', 'ColorValue'])
    );
    for i:= 0 to High(ColorValues) do
    begin
      QParamInt('ColorIndex', ColorIndexes[i]);
      QParamInt('ColorValue', ColorValues[i]);
      QExec;
    end;
    Result:= True;
    QCommit;
  except
    QRollback;
  end;
end;

function TDataBase.ColorsShiftLoad(out AColorValues, AColorIndexes: TIntVector): Boolean;
begin
  AColorValues:= nil;
  AColorIndexes:= nil;

  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('SHIFTCOLORS', ['ColorIndex', 'ColorValue']) +
    'ORDER BY ColorIndex'
  );
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AColorValues, QFieldInt('ColorValue'));
      VAppend(AColorIndexes, QFieldInt('ColorIndex'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SettingLoad(const ASettingName: String): Integer;
begin
  Result:= ValueIntByStrID('SETTINGS', 'Value', 'Name', ASettingName);
end;

function TDataBase.SettingsLoad(const ASettingNames: TStrVector): TIntVector;
var
  i: Integer;
begin
  VDim(Result{%H-}, Length(ASettingNames));
  for i:= 0 to High(Result) do
    Result[i]:= SettingLoad(ASettingNames[i]);
end;

procedure TDataBase.SettingUpdate(const ASettingName: String; const ASettingValue: Integer);
begin
  UpdateByStrID('SETTINGS', 'Value', 'Name', ASettingName, ASettingValue, True{commit});
end;

procedure TDataBase.SettingsUpdate(const ASettingNames: TStrVector;
  const ASettingValues: TIntVector);
var
  i: Integer;
begin
  try
    for i:= 0 to High(ASettingNames) do
      UpdateByStrID('SETTINGS', 'Value', 'Name', ASettingNames[i], ASettingValues[i], False{no commit});
    QCommit;
  finally
    QRollback;
  end;
end;

function TDataBase.TextParamLoad(const AParamName: String): String;
begin
  Result:= ValueStrByStrID('TEXTPARAMS', 'Value', 'Name', AParamName);
end;

procedure TDataBase.TextParamUpdate(const AParamName, AParamValue: String);
begin
  UpdateByStrID('TEXTPARAMS', 'Value', 'Name', AParamName, AParamValue, True{commit});
end;

procedure TDataBase.PostDictionaryLoad(const ADropDown: TVSTDropDown;
                                       out APostIDs: TIntVector;
                                       const ASelectPostID: Integer = -1;
                                       const AIDNotZero: Boolean = True);
var
  Items: TStrVector;
begin
  KeyPickList('STAFFPOST', 'PostID', 'PostName', APostIDs, Items, AIDNotZero, 'PostName');
  ADropDown.KeyPick(Items, APostIDs, ASelectPostID);
end;

procedure TDataBase.TimetableMarkDictionaryLoad(const ADropDown: TVSTDropDown;
                                 out ADigMarks: TIntVector;
                                 const ASelectDigMark: Integer = -1;
                                 const AIDNotZero: Boolean = True);
var
  Items: TStrVector;
begin
  TimetableMarkListLoad(ADigMarks, Items, AIDNotZero);
  ADropDown.KeyPick(Items, ADigMarks, ASelectDigMark);
end;

function TDataBase.StaffListLoad(const ADate: TDate;
                               const AOrderType, AListType: Byte;
                               out AStaffIDs, ATabNumIDs, AGenders: TIntVector;
                               out ABornDates, ARecrutDates, ADismissDates: TDateVector;
                               out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;
var
  SQLStr: String;
  i, PostID: Integer;
  PostName, Rank: String;
  Indexes: TIntVector;
begin
  Result:= False;

  AStaffIDs:= nil;
  ATabNumIDs:= nil;
  AGenders:= nil;
  ABornDates:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;
  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;

  //список людей на текущую дату
  SQLStr:=
    'SELECT t1.StaffID, t1.Name, t1.Patronymic, t1.Family, t1.BornDate, t1.Gender, '+
           'tt.TabNumID, tt.TabNum, tt.RecrutDate, tt.DismissDate ' +
    'FROM STAFFMAIN t1 ' +
    'LEFT OUTER JOIN ( ' +
           'SELECT t2.StaffID, t2.TabNumID, t2.TabNum, t2.RecrutDate, t2.DismissDate ' +
           'FROM STAFFTABNUM t2 ' +
           ') tt ON (tt.StaffID=t1.StaffID) ' ;

  if AListType>0 then
  begin
    SQLStr:= SQLStr + 'WHERE ';
    case AListType of
      1: SQLStr:= SQLStr + '(((tt.DismissDate >= :ADate) OR (tt.DismissDate IS NULL)) AND (tt.TabNum IS NOT NULL)) ';
      2: SQLStr:= SQLStr + '(((tt.DismissDate < :ADate) OR (tt.DismissDate IS NULL)) AND (tt.TabNum IS NOT NULL)) ';
      3: SQLStr:= SQLStr + '(tt.TabNum IS NULL) ';
    end;
  end;
  SQLStr:= SQLStr + 'ORDER BY ';
  case AOrderType of
    0,2: SQLStr:= SQLStr + 't1.Family, t1.Name, t1.Patronymic ';
    1: SQLStr:= SQLStr + 'tt.TabNum, t1.Family, t1.Name, t1.Patronymic ';
    3: SQLStr:= SQLStr + 't1.BornDate, t1.Family, t1.Name, t1.Patronymic ';
    4: SQLStr:= SQLStr + 'tt.RecrutDate, t1.Family, t1.Name, t1.Patronymic ';
    5: SQLStr:= SQLStr + 'tt.DismissDate, t1.Family, t1.Name, t1.Patronymic ';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('ADate', ADate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStaffIDs, QFieldInt('StaffID'));
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AGenders, QFieldInt('Gender'));
      VAppend(ABornDates, QFieldDT('BornDate'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));
      VAppend(ADismissDates, QFieldDT('DismissDate'));
      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на текущую дату
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], Date, PostID, PostName, Rank);
    VAppend(APostNames, PostName);
    VAppend(ARanks, Rank);
  end;

  //сортировка по наименованию должности
  if AOrderType<>2 then Exit;
  VSort(APostNames, Indexes);

  AStaffIDs:= VReplace(AStaffIDs, Indexes);
  ATabNumIDs:= VReplace(ATabNumIDs, Indexes);
  AGenders:= VReplace(AGenders, Indexes);
  ABornDates:= VReplace(ABornDates, Indexes);
  ARecrutDates:= VReplace(ARecrutDates, Indexes);
  ADismissDates:= VReplace(ADismissDates, Indexes);
  AFs:= VReplace(AFs, Indexes);
  ANs:= VReplace(ANs, Indexes);
  APs:= VReplace(APs, Indexes);
  ATabNums:= VReplace(ATabNums, Indexes);
  APostNames:= VReplace(APostNames, Indexes);
  ARanks:= VReplace(ARanks, Indexes);
end;

function TDataBase.StaffListLoad(const ADate: TDate;
                           const AOrderType, AListType: Byte;
                           out ATabNumIDs: TIntVector;
                           out AFs, ANs, APs, ATabNums: TStrVector): Boolean;
var
  StaffIDs, Genders: TIntVector;
  PostNames, Ranks: TStrVector;
  BornDates, RecrutDates, DismissDates: TDateVector;
begin
  Result:= StaffListLoad(ADate, AOrderType, AListType, StaffIDs, ATabNumIDs, Genders,
                         BornDates, RecrutDates, DismissDates,
                         AFs, ANs, APs, ATabNums, PostNames, Ranks);
end;

function TDataBase.StaffListForPersonalTimingLoad(const AFilterValue: String;
                             const ABeginDate, AEndDate: TDate;
                             const AOrderType: Byte;
                             const AIsDescOrder: Boolean;
                             out ATabNumIDs: TIntVector;
                             out ARecrutDates, ADismissDates: TDateVector;
                             out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;
var
  SQLStr: String;
  i, PostID: Integer;
  PostName, Rank: String;
  Indexes: TIntVector;
begin
  Result:= False;

  ATabNumIDs:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;
  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;

  SQLStr:=
    'SELECT t1.TabNumID, t1.TabNum, t1.RecrutDate, t1.DismissDate, ' +
           't2.Name, t2.Patronymic, t2.Family ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'WHERE ((t1.RecrutDate<= :ED) AND (t1.DismissDate >= :BD)) ';
  if not SEmpty(AFilterValue) then
    SQLStr:= SQLStr + 'AND (t2.FullName LIKE :FilterValue) ';
  case AOrderType of
  0: if AIsDescOrder then
       SQLStr:= SQLStr + 'ORDER BY t2.Family DESC, t2.Name DESC, t2.Patronymic DESC'
     else
       SQLStr:= SQLStr + 'ORDER BY t2.Family, t2.Name, t2.Patronymic';
  1: if AIsDescOrder then
       SQLStr:= SQLStr + 'ORDER BY t1.TabNum DESC'
     else
       SQLStr:= SQLStr + 'ORDER BY t1.TabNum';
  2: SQLStr:= SQLStr + 'ORDER BY t2.Family, t2.Name, t2.Patronymic';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QParamStr('FilterValue', '%'+AFilterValue+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));
      VAppend(ADismissDates, QFieldDT('DismissDate'));
      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на текущую дату
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], Date, PostID, PostName, Rank);
    VAppend(APostNames, PostName);
    VAppend(ARanks, Rank);
  end;

  //сортировка по наименованию должности
  if AOrderType<>2 then Exit;

  if AIsDescOrder then
    VSort(APostNames, Indexes, True)
  else
    VSort(APostNames, Indexes, False);

  ATabNumIDs:= VReplace(ATabNumIDs, Indexes);
  ARecrutDates:= VReplace(ARecrutDates, Indexes);
  ADismissDates:= VReplace(ADismissDates, Indexes);
  AFs:= VReplace(AFs, Indexes);
  ANs:= VReplace(ANs, Indexes);
  APs:= VReplace(APs, Indexes);
  ATabNums:= VReplace(ATabNums, Indexes);
  APostNames:= VReplace(APostNames, Indexes);
  ARanks:= VReplace(ARanks, Indexes);
end;

function TDataBase.StaffListForCommonTimingLoad(const ABeginDate, AEndDate: TDate;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateVector;
                   out AFs, ANs, APs, ATabNums, APostNames, ARanks, AScheduleNames: TStrVector): Boolean;
var
  SQLStr: String;
  BD, ED, RecrutDate, DismissDate: TDate;
begin
  Result:= False;

  ATabNumIDs:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;
  APostBDs:= nil;
  APostEDs:= nil;
  AScheduleBDs:= nil;
  AScheduleEDs:= nil;
  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;
  AScheduleNames:= nil;

  SQLStr:=
    'SELECT t1.Family, t1.Name, t1.Patronymic, '+
           't2.TabNumID, t2.TabNum, t2.RecrutDate, t2.DismissDate, ' +
           't3.ScheduleID, t3.BeginDate AS ScheduleBD, t3.EndDate AS ScheduleED, '+
           't4.Rank, t4.FirstDate AS PostBD, t4.LastDate AS PostED, '+
           't5.PostName, ' +
           't6.ScheduleName ' +
           //'t6.ScheduleName, t6.WeekHours, t6.CycleCount ' +
    'FROM STAFFMAIN t1 ' +
    'INNER JOIN STAFFTABNUM t2 ON (t1.StaffID=t2.StaffID) ' +
    'INNER JOIN STAFFSCHEDULE t3 ON (t2.TabNumID=t3.TabNumID) ' +
    'INNER JOIN STAFFPOSTLOG t4 ON (t2.TabNumID=t4.TabNumID) ' +
    'INNER JOIN STAFFPOST t5 ON (t4.PostID=t5.PostID) ' +
    'INNER JOIN SCHEDULEMAIN t6 ON (t6.ScheduleID=t3.ScheduleID) ' +
    'WHERE ((t2.RecrutDate<= :ED) AND (t2.DismissDate>= :BD)) AND ' +
          '((t3.BeginDate<= :ED) AND (t3.EndDate>= :BD)) AND ' +
          '((t4.FirstDate<= :ED) AND (t4.LastDate>= :BD)) AND (' +
          SqlCROSS('t3.BeginDate', 't3.EndDate', 't4.FirstDate', 't4.LastDate' ) + ') ';
  case AOrderType of
  0:  SQLStr:= SQLStr + 'ORDER BY t6.ScheduleName, t1.Family, t1.Name, t1.Patronymic';
  1:  SQLStr:= SQLStr + 'ORDER BY t5.PostName, t6.ScheduleName, t1.Family, t1.Name, t1.Patronymic';
  2:  SQLStr:= SQLStr + 'ORDER BY t1.Family, t1.Name, t1.Patronymic';
  3:  SQLStr:= SQLStr + 'ORDER BY t2.TabNum, t1.Family, t1.Name, t1.Patronymic';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(APostNames, QFieldStr('PostName'));
      VAppend(ARanks, QFieldStr('Rank'));
      VAppend(AScheduleNames, QFieldStr('ScheduleName'));

      RecrutDate:= QFieldDT('RecrutDate');
      DismissDate:= QFieldDT('DismissDate');
      VAppend(ARecrutDates, RecrutDate);
      VAppend(ADismissDates, DismissDate);
      IsPeriodIntersect(RecrutDate, DismissDate, QFieldDT('PostBD'), QFieldDT('PostED'), BD, ED);
      VAppend(APostBDs, BD);
      VAppend(APostEDs, ED);
      IsPeriodIntersect(RecrutDate, DismissDate, QFieldDT('ScheduleBD'), QFieldDT('ScheduleED'), BD, ED);
      VAppend(AScheduleBDs, BD);
      VAppend(AScheduleEDs, ED);

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffListForVacationPlanningLoad(const AYear: Integer;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out AFamilies, ANames, APatronymics, ATabNums, APostNames, AScheduleNames: TStrVector): Boolean;
var
  i, j: Integer;
  BD: TDate;
  S1, S2, SQLStr: String;
  Indexes: TIntVector;
begin
  Result:= False;
  ATabNumIDs:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  AScheduleNames:= nil;

  BD:= FirstDayInYear(AYear);

  SQLStr:=
    'SELECT t1.TabNum, t1.TabNumID,  ' +
           't2.Name, t2.Patronymic, t2.Family, ' +
           't4.ScheduleName ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'INNER JOIN STAFFSCHEDULE t3 ON (t1.TabNumID=t3.TabNumID) ' +
    'INNER JOIN SCHEDULEMAIN t4 ON (t3.ScheduleID=t4.ScheduleID) ' +
    'WHERE ((t1.RecrutDate < :BD) AND (t1.DismissDate >= :BD)) AND ' +
          '(:BD BETWEEN t3.BeginDate AND t3.EndDate)';

  //0-график, 1-должность, 2-ФИО, 3-табельный номер
  case AOrderType of
  0:    SQLStr:= SQLStr + 'ORDER BY t4.ScheduleName, t2.Family, t2.Name, t2.Patronymic';
  1,2:  SQLStr:= SQLStr + 'ORDER BY t2.Family, t2.Name, t2.Patronymic';
  3:    SQLStr:= SQLStr + 'ORDER BY t1.TabNum, t2.Family, t2.Name, t2.Patronymic';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('BD', BD);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(AScheduleNames, QFieldStr('ScheduleName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на начало года
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], BD, j, S1, S2);
    VAppend(APostNames, S1);
  end;

  //сортировка по наименованию должности
  if AOrderType<>1 then Exit;
  VSort(APostNames, Indexes);

  APostNames:= VReplace(APostNames, Indexes);
  ATabNumIDs:= VReplace(ATabNumIDs, Indexes);
  ATabNums:= VReplace(ATabNums, Indexes);
  AFamilies:= VReplace(AFamilies, Indexes);
  ANames:= VReplace(ANames, Indexes);
  APatronymics:= VReplace(APatronymics, Indexes);
  AScheduleNames:= VReplace(AScheduleNames, Indexes);
end;

function TDataBase.StaffListForSIZRequestLoad(const AReportDate: TDate;
                          out ATabNumIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics, ATabNums: TStrVector): Boolean;
begin
  Result:= False;
  ATabNumIDs:= nil;
  AGenders:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ATabNums:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.TabNum, t1.TabNumID, ' +
           't2.Family, t2.Name, t2.Patronymic, t2.Gender ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'WHERE (:ReportDate BETWEEN t1.RecrutDate AND t1.DismissDate) ' +
    'ORDER BY t2.Gender, t2.Family, t2.Name, t2.Patronymic'
  );
  QParamDT('ReportDate', AReportDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AGenders, QFieldInt('Gender'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffMainAdd(out AStaffID: Integer;
                                const AFamily, AName, APatronymic: String;
                                const ABornDate: TDate; const AGender: Byte): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись основных данных
    QSetSQL(
      sqlINSERT('STAFFMAIN', ['Family', 'Name', 'Patronymic', 'BornDate', 'Gender', 'FullName'])
    );
    QParamStr('Family', AFamily);
    QParamStr('Name', AName);
    QParamStr('Patronymic', APatronymic, not SEmpty(APatronymic));
    QParamDT('BornDate', ABornDate);
    QParamInt('Gender', AGender);
    QParamStr('FullName', SUpper(SNameLong(AFamily, AName, APatronymic)));
    QExec;
    //получение ID сделанной записи
    AStaffID:= LastWritedInt32ID('STAFFMAIN');
    //запись ID в таблицу размеров СИЗ
    QSetSQL(
      sqlINSERT('SIZSTAFFSIZE', ['StaffID'])
    );
    QParamInt('StaffID', AStaffID);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffMainUpdate(const AStaffID: Integer;
                                const AFamily, AName, APatronymic: String;
                                const ABornDate: TDate; const AGender: Byte): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('STAFFMAIN', ['Family', 'Name', 'Patronymic', 'BornDate', 'Gender', 'FullName']) +
      'WHERE StaffID = :StaffID'
    );
    QParamInt('StaffID', AStaffID);
    QParamStr('Family', AFamily);
    QParamStr('Name', AName);
    QParamStr('Patronymic', APatronymic, not SEmpty(APatronymic));
    QParamDT('BornDate', ABornDate);
    QParamInt('Gender', AGender);
    QParamStr('FullName', SUpper(SNameLong(AFamily, AName, APatronymic)));
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffMainLoad(const AStaffID: Integer;
                                 out AFamily, AName, APatronymic: String;
                                 out ABornDate: TDate; out AGender: Byte): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('STAFFMAIN', ['Family', 'Name', 'Patronymic', 'BornDate', 'Gender']) +
    'WHERE StaffID = :StaffID'
  );
  QParamInt('StaffID', AStaffID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    AFamily:=  QFieldStr('Family');
    AName:= QFieldStr('Name');
    APatronymic:= QFieldStr('Patronymic');
    ABornDate:= QFieldDT('BornDate');
    AGender:= QFieldInt('Gender');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffMainDelete(const AStaffID: Integer): Boolean;
begin
  Result:= Delete('STAFFMAIN', 'StaffID', AStaffID);
end;

function TDataBase.StaffMainListLoad(const AFilterValue: String;
                          const AOrderType: Byte;
                          const AIsDescOrder: Boolean;
                          out AStaffIDs, AGenders: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  AStaffIDs:= nil;
  AGenders:= nil;
  ABornDates:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;

  SQLStr:= sqlSELECT('STAFFMAIN', ['StaffID', 'Family', 'Name', 'Patronymic', 'BornDate', 'Gender']);
  if not SEmpty(AFilterValue) then
    SQLStr:= SQLStr + 'WHERE (FullName LIKE :FilterValue)';

  if AOrderType=0 then
  begin
    if AIsDescOrder then
      SQLStr:= SQLStr + 'ORDER BY Family DESC, Name DESC, Patronymic DESC, BornDate'
    else
      SQLStr:= SQLStr + 'ORDER BY Family, Name, Patronymic, BornDate' ;
  end
  else begin
    if AIsDescOrder then
      SQLStr:= SQLStr + 'ORDER BY BornDate DESC, Family, Name, Patronymic'
    else
      SQLStr:= SQLStr + 'ORDER BY BornDate, Family, Name, Patronymic';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamStr('FilterValue', '%'+AFilterValue+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStaffIDs, QFieldInt('StaffID'));
      VAppend(AGenders, QFieldInt('Gender'));
      VAppend(ABornDates, QFieldDT('BornDate'));
      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffBirthdaysLoad(const AMonth: Word;
                              const AIncludeDismissed, AIsDateOrder: Boolean;
                              out AFamilies, ANames, APatronymics: TStrVector;
                              out ABornDates: TDateVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ABornDates:= nil;

  if AIncludeDismissed then //весь список, вместе с уволенными
  begin
    SQLStr:=
      'SELECT Family, Name, Patronymic, BornDate, '+
             'STRFTIME(''%m'', BornDate) AS BornMonth, ' +
             'STRFTIME(''%d'', BornDate) AS BornDay ' +
      'FROM STAFFMAIN ';
    if (AMonth>=1) and (AMonth<=12) then
      SQLStr:= SQLStr + 'WHERE BornMonth = ' + QuotedStr(Format('%.2d', [AMonth])) + ' ';
  end
  else begin
    SQLStr:=
      'SELECT DISTINCT t1.StaffID, t2.Family, t2.Name, t2.Patronymic, t2.BornDate, '+
             'STRFTIME(''%m'', t2.BornDate) AS BornMonth, ' +
             'STRFTIME(''%d'', t2.BornDate) AS BornDay ' +
      'FROM STAFFTABNUM t1 ' +
      'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
      'WHERE (t1.DismissDate=:DismissDate) ';
    if (AMonth>=1) and (AMonth<=12) then
      SQLStr:= SQLStr + 'AND (BornMonth = ' + QuotedStr(Format('%.2d', [AMonth])) + ') ';
  end;

  if AIsDateOrder then
    SQLStr:= SQLStr + 'ORDER BY BornMonth, BornDay'
  else
    SQLStr:= SQLStr + 'ORDER BY Family, Name, Patronymic, BornMonth, BornDay';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('DismissDate', INFDATE);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));
      VAppend(ABornDates, QFieldDT('BornDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffVacationsLoad(const AMonth, AYear: Word;
                              const AIsDateOrder: Boolean;
                              out AFamilies, ANames, APatronymics,
                                  ATabNums, APostNames: TStrVector;
                              out ACounts: TIntVector;
                              out ADates: TDateVector): Boolean;
var
  PostName, Rank: String;
  i, PostID: Integer;
  BD, ED: TDate;
  TabNumIDs, Indexes: TIntVector;
begin
  Result:= False;

  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ACounts:= nil;
  ADates:= nil;

  TabNumIDs:= nil;

  if (AMonth>=1) and (AMonth<=12) then
    FirstLastDayInMonth(AMonth, AYear, BD, ED)
  else
    FirstLastDayInYear(AYear, BD, ED);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.TabNum, t1.TabNumID, ' +
           't2.Family, t2.Name, t2.Patronymic, ' +
           't3.Fact1Date, t3.Fact1Count, t3.Fact1CountAdd, ' +
           't3.Fact2Date, t3.Fact2Count, t3.Fact2CountAdd ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'INNER JOIN STAFFVACATION t3 ON (t1.TabNumID=t3.TabNumID) ' +
    'WHERE (t1.DismissDate>=:BD) AND (t1.RecrutDate<=:ED) AND (' +
          '(t3.Fact1Date BETWEEN :BD AND :ED) OR (t3.Fact2Date BETWEEN :BD AND :ED))' +
    'ORDER BY t2.Family, t2.Name, t2.Patronymic '
  );
  QParamDT('BD', BD);
  QParamDT('ED', ED);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      if IsDateInPeriod(QFieldDT('Fact1Date'), BD, ED) then
      begin
        VAppend(AFamilies, QFieldStr('Family'));
        VAppend(ANames, QFieldStr('Name'));
        VAppend(APatronymics, QFieldStr('Patronymic'));
        VAppend(ATabNums, QFieldStr('TabNum'));
        VAppend(ADates, QFieldDT('Fact1Date'));
        VAppend(ACounts, QFieldInt('Fact1Count')+QFieldInt('Fact1CountAdd'));
        VAppend(TabNumIDs, QFieldInt('TabNumID'));
      end;
      if IsDateInPeriod(QFieldDT('Fact2Date'), BD, ED) then
      begin
        VAppend(AFamilies, QFieldStr('Family'));
        VAppend(ANames, QFieldStr('Name'));
        VAppend(APatronymics, QFieldStr('Patronymic'));
        VAppend(ATabNums, QFieldStr('TabNum'));
        VAppend(ADates, QFieldDT('Fact2Date'));
        VAppend(ACounts, QFieldInt('Fact2Count')+QFieldInt('Fact2CountAdd'));
        VAppend(TabNumIDs, QFieldInt('TabNumID'));
      end;
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //сортировка по дате начала отпуска
  if AIsDateOrder then
  begin
    VSortDate(ADates, Indexes);
    AFamilies:= VReplace(AFamilies, Indexes);
    ANames:= VReplace(ANames, Indexes);
    APatronymics:= VReplace(APatronymics, Indexes);
    ATabNums:= VReplace(ATabNums, Indexes);
    ADates:= VReplace(ADates, Indexes);
    ACounts:= VReplace(ACounts, Indexes);
    TabNumIDs:= VReplace(TabNumIDs, Indexes);
  end;

  //актуальные должности на дату начала отпуска
  for i:= 0 to High(TabNumIDs) do
  begin
    StaffPostForDate(TabNumIDs[i], ADates[i], PostID, PostName, Rank);
    VAppend(APostNames, PostName);
  end;
end;

function TDataBase.StaffTabNumListLoad(const AStaffID: Integer;
                          out ATabNumIDs, APostIDs: TIntVector;
                          out ATabNums, APostNames, ARanks: TStrVector;
                          out ARecrutDates, ADismissDates: TDateVector): Boolean;
var
  i, PostID: Integer;
  PostName, Rank: String;
begin
  Result:= False;

  ATabNumIDs:= nil;
  APostIDs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARanks:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;

  if AStaffID<=0 then Exit;
  //табельные номера с периодами работы
  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('STAFFTABNUM', ['TabNumID', 'TabNum', 'RecrutDate', 'DismissDate']) +
    'WHERE (StaffID = :StaffID) ' +
    'ORDER BY TabNum, RecrutDate'
  );
  QParamInt('StaffID', AStaffID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));
      VAppend(ADismissDates, QFieldDT('DismissDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на текущую дату
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], Date, PostID, PostName, Rank);
    VAppend(APostIDs, PostID);
    VAppend(APostNames, PostName);
    VAppend(ARanks, Rank);
  end;
end;

function TDataBase.StaffTabNumAdd(out ATabNumID: Integer;
                          const AStaffID: Integer;
                          const ATabNum: String;
                          const ARecrutDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись данных в таблицу табельных номеров
    QSetSQL(
      sqlINSERT('STAFFTABNUM', ['StaffID', 'TabNum', 'RecrutDate', 'DismissDate'])
    );
    QParamInt('StaffID', AStaffID);
    QParamStr('TabNum', ATabNum);
    QParamDT('RecrutDate', ARecrutDate);
    QParamDT('DismissDate', INFDATE);
    QExec;
    //получение ID записанного табельного номера
    ATabNumID:= LastWritedInt32ID('STAFFTABNUM');
    //заносим первую запись в табицу переводов
    QSetSQL(
      sqlINSERT('STAFFPOSTLOG', ['TabNumID', 'FirstDate', 'LastDate'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('FirstDate', ARecrutDate);
    QParamDT('LastDate', INFDATE);
    QExec;
    //заносим первую запись в таблицу персональных графиков
    QSetSQL(
      sqlINSERT('STAFFSCHEDULE', ['TabNumID', 'BeginDate', 'EndDate'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('BeginDate', ARecrutDate);
    QParamDT('EndDate', INFDATE);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffTabNumUpdate(const ATabNumID: Integer;
                                     const ATabNum: String;
                                     const ARecrutDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //обновление данных в таблице таб. номеров
    QSetSQL(
      sqlUPDATE('STAFFTABNUM', ['TabNum', 'RecrutDate']) +
      'WHERE TabNumID = :TabNumID'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamStr('TabNum', ATabNum);
    QParamDT('RecrutDate', ARecrutDate);
    QExec;
    //удаление более ранних периодов из таблицы переводов
    QSetSQL(
      'DELETE FROM STAFFPOSTLOG ' +
      'WHERE (TabNumID = :TabNumID) AND (LastDate < :RecrutDate)'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('RecrutDate', ARecrutDate);
    QExec;
    //меняем данные в таблице переводов
    QSetSQL(
      sqlUPDATE('STAFFPOSTLOG', ['FirstDate']) +
      'WHERE (TabNumID = :TabNumID) AND ' +
                      '(FirstDate = (' +
                             'SELECT FirstDate FROM STAFFPOSTLOG ' +
                             'WHERE (TabNumID = :TabNumID) ' +
                             'ORDER BY FirstDate ' +
                             'LIMIT 1' +
                      ')) '
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('FirstDate', ARecrutDate);
    QExec;
    //удаляем более ранние периоды из таблицы графиков
    QSetSQL(
      'DELETE FROM STAFFSCHEDULE ' +
      'WHERE (TabNumID = :TabNumID) AND (EndDate < :RecrutDate)'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('RecrutDate', ARecrutDate);
    QExec;
    //меняем данные в таблице графиков
    QSetSQL(
      sqlUPDATE('STAFFSCHEDULE', ['BeginDate']) +
      'WHERE (TabNumID = :TabNumID) AND ' +
                      '(BeginDate = (' +
                             'SELECT BeginDate FROM STAFFSCHEDULE ' +
                             'WHERE (TabNumID = :TabNumID) ' +
                             'ORDER BY BeginDate ' +
                             'LIMIT 1' +
                      ')) '
    );
    QParamInt('TabNumID', ATabNumID);
    QParamDT('BeginDate', ARecrutDate);
    QExec;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffTabNumDismiss(const ATabNumID: Integer; const ADismissDate: TDate): Boolean;
begin
  Result:= UpdateByInt32ID('STAFFTABNUM', 'DismissDate', 'TabNumID', ATabNumID, ADismissDate);
end;

function TDataBase.StaffTabNumDismissCancel(const ATabNumID: Integer): Boolean;
begin
  Result:= StaffTabNumDismiss(ATabNumID, INFDATE);
end;

function TDataBase.StaffTabNumDelete(const ATabNumID: Integer): Boolean;
begin
  Result:= Delete('STAFFTABNUM', 'TabNumID', ATabNumID);
end;

function TDataBase.StaffTabNumIsExists(const ATabNumID: Integer; const ATabNum: String): Boolean;
begin
  Result:= IsValueInTableNotMatchInt32ID('STAFFTABNUM', 'TabNum', ATabNum,
                                         'TabNumID', ATabNumID);
end;

function TDataBase.StaffTabNumWorkPeriodLoad(const ATabNumID: Integer;
                                     out ARecrutDate, ADismissDate: TDate): Boolean;
begin
  Result:= False;
  ARecrutDate:= 0;
  ADismissDate:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT RecrutDate, DismissDate  FROM STAFFTABNUM ' +
    'WHERE TabNumID = :TabNumID');
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    ARecrutDate:= QFieldDT('RecrutDate');
    ADismissDate:= QFieldDT('DismissDate');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffIDByTabNumID(const ATabNumID: Integer;
                                     out AStaffID: Integer): Boolean;
begin
  AStaffID:= ValueInt32ByInt32ID('STAFFTABNUM', 'StaffID', 'TabNumID', ATabNumID);
  Result:= AStaffID>0;
end;

function TDataBase.StaffPostForDate(const ATabNumID: Integer;
                               const ADate: TDate;
                               out APostID: Integer;
                               out APostName, ARank: String): Boolean;
var
  S, SQLStr: String;

  function ValuesLoad(const ASQLStr: String): Boolean;
  begin
    Result:= False;
    QSetQuery(FQuery);
    QSetSQL(ASQLStr);
    QParamDT('DateValue', ADate);
    QParamInt('TabNumID', ATabNumID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      APostName:= QFieldStr('PostName');
      ARank:= QFieldStr('Rank');
      APostID:= QFieldInt('PostID');
      Result:= True;
    end;
    QClose;
  end;

begin
  Result:= False;

  APostName:= EmptyStr;
  ARank:= EmptyStr;
  APostID:= 0;

  S:= 'SELECT t1.Rank, t1.PostID, t2.PostName ' +
      'FROM STAFFPOSTLOG t1 ' +
      'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) '+
      'WHERE (t1.TabNumID = :TabNumID) AND (t1.PostTemp = 0) ';

  //постоянная должность на указанную дату
  SQLStr:= S + 'AND (:DateValue BETWEEN t1.FirstDate AND t1.LastDate) ';
  Result:= ValuesLoad(SQLStr);
  if Result then Exit;

  //последняя постоянная должность до указанной даты
  SQLStr:= S + 'AND (t1.LastDate < :DateValue) ' +
          'ORDER BY t1.LastDate DESC ' +
          'LIMIT 1';
  Result:= ValuesLoad(SQLStr);
  if Result then Exit;

  //постоянная должность после указанной даты
  SQLStr:= S + 'AND (t1.FirstDate > :DateValue) ' +
          'ORDER BY t1.FirstDate ' +
          'LIMIT 1';
  Result:= ValuesLoad(SQLStr);
end;

procedure TDataBase.StaffPostLast(const ATabNumID: Integer;
                               out APostID: Integer;
                               out APostName, ARank: String);
begin
  APostName:= EmptyStr;
  ARank:= EmptyStr;
  APostID:= 0;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.Rank, t1.PostID, t2.PostName ' +
    'FROM STAFFPOSTLOG t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'WHERE (t1.TabNumID = :TabNumID) AND (t1.PostTemp = 0) ' +
    'ORDER BY t1.FirstDate DESC ' +
    'LIMIT 1'
  );
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    APostName:= QFieldStr('PostName');
    ARank:= QFieldStr('Rank');
    APostID:= QFieldInt('PostID');
  end;
  QClose;
end;

function TDataBase.StaffParamIDsForPeriodLoad(const ATabNumID: Integer;
                                  const ABeginDate, AEndDate: TDate;
                                  const ATableName, AIDField, ABDField, AEDField: String;
                                  out AParamIDs: TIntVector;
                                  out AFirstDates, ALastDates: TDateMatrix): Boolean;
var
  i, ParamID: Integer;
  BD, ED: TDate;
begin
  Result:= False;

  AParamIDs:= nil;
  AFirstDates:= nil;
  ALastDates:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    SqlSELECT(ATableName, [AIDField, ABDField, AEDField]) +
    'WHERE (TabNumID = :TabNumID) AND (' +
           SqlCROSS(ABDField, AEDField, ':BD', ':ED') +
           ') ' +
    'ORDER BY ' + SqlEsc(ABDField)
  );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      ParamID:= QFieldInt(AIDField);
      BD:= MaxDate(QFieldDT(ABDField), ABeginDate);
      ED:= MinDate(QFieldDT(AEDField), AEndDate);
      i:= VIndexOf(AParamIDs, ParamID);
      if i<0 then
      begin
        VAppend(AParamIDs, ParamID);
        MAppend(AFirstDates, VCreateDate([BD]));
        MAppend(ALastDates, VCreateDate([ED]));
      end
      else begin
        VAppend(AFirstDates[i], BD);
        VAppend(ALastDates[i], ED);
      end;
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffPostForPeriodLoad(const ATabNumID: Integer;
                                  const ABeginDate, AEndDate: TDate;
                                  out APostIDs: TIntVector;
                                  out AFirstDates, ALastDates: TDateMatrix): Boolean;
begin
  Result:= StaffParamIDsForPeriodLoad(ATabNumID, ABeginDate, AEndDate,
                    'STAFFPOSTLOG', 'PostID', 'FirstDate', 'LastDate',
                    APostIDs, AFirstDates, ALastDates);
end;

function TDataBase.StaffScheduleForPeriodLoad(const ATabNumID: Integer;
                                  const ABeginDate, AEndDate: TDate;
                                  out AScheduleIDs: TIntVector;
                                  out ABeginDates, AEndDates: TDateMatrix): Boolean;
begin
  Result:= StaffParamIDsForPeriodLoad(ATabNumID, ABeginDate, AEndDate,
                    'STAFFSCHEDULE', 'ScheduleID', 'BeginDate', 'EndDate',
                    AScheduleIDs, ABeginDates, AEndDates);
end;

function TDataBase.StaffPostLogListLoad(const ATabNumID: Integer;
                          out APostLogIDs, APostIDs, APostTemps: TIntVector;
                          out APostNames, ARanks: TStrVector;
                          out AFirstDates, ALastDates: TDateVector): Boolean;
begin
  Result:= False;

  APostLogIDs:= nil;
  APostIDs:= nil;
  APostTemps:= nil;
  APostNames:= nil;
  ARanks:= nil;
  AFirstDates:= nil;
  ALastDates:= nil;

  if ATabNumID<=0 then Exit;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ID, t1.FirstDate, t1.LastDate, t1.PostTemp, t1.Rank, t1.PostID, t2.PostName ' +
    'FROM STAFFPOSTLOG t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'WHERE TabNumID = :TabNumID ' +
    'ORDER BY t1.FirstDate DESC'
  );
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(APostLogIDs, QFieldInt('ID'));
      VAppend(APostIDs, QFieldInt('PostID'));
      VAppend(APostTemps, QFieldInt('PostTemp'));
      VAppend(ARanks, QFieldStr('Rank'));
      VAppend(APostNames, QFieldStr('PostName'));
      VAppend(AFirstDates, QFieldDT('FirstDate'));
      VAppend(ALastDates, QFieldDT('LastDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffPostLogAdd(const APostLogID, ATabNumID, APostID, APostTemp: Integer;
                                  const ARank: String;
                                  const AFirstDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //меняем конечную дату текущего периода
    UpdateByInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APostLogID, IncDay(AFirstDate, -1), False{no commit});
    //записываем новые данные
    QSetSQL(
      sqlINSERT('STAFFPOSTLOG', ['TabNumID', 'PostID', 'FirstDate', 'LastDate', 'PostTemp', 'Rank'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('PostID', APostID);
    QParamDT('FirstDate', AFirstDate);
    QParamDT('LastDate', INFDATE);
    QParamInt('PostTemp', APostTemp);
    QParamStr('Rank', ARank, not SEmpty(ARank));
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffPostLogUpdate(const APrevPostLogID, APostLogID, APostID, APostTemp: Integer;
                                      const ARank: String;
                                      const AFirstDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //меняем конечную дату предыдущего периода
    if APrevPostLogID>0 then
      UpdateByInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APrevPostLogID, IncDay(AFirstDate, -1), False{no commit});
    //изменяем начальную дату и должность текущего периода
    QSetSQL(
      sqlUPDATE('STAFFPOSTLOG', [ 'PostID', 'FirstDate', 'PostTemp', 'Rank']) +
      'WHERE ID = :PostLogID'
    );
    QParamInt('PostLogID', APostLogID);
    QParamInt('PostID', APostID);
    QParamDT('FirstDate', AFirstDate);
    QParamInt('PostTemp', APostTemp);
    QParamStr('Rank', ARank, not SEmpty(ARank));
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffPostLogDelete(const APrevPostLogID, APostLogID: Integer;
                                      const ALastDate: TDate): Boolean;
begin
  Result:= False;
  try
    //удаление записи
    Delete('STAFFPOSTLOG', 'ID', APostLogID, False {no commit});
    //замена конечной даты предыдущего периода на конечную дату этого периода
    UpdateByInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APrevPostLogID, ALastDate, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffPostScheduleInfoLoad(const ATabNumID: Integer;
                          out AInfo: TPostScheduleInfo;
                          const AFromDate: TDate = NULDATE;
                          const AToDate: TDate = INFDATE): Boolean;
var
  i, j: Integer;
  PostIDs: TIntVector;
  FirstDates, LastDates: TDateMatrix;
  ShiftScheduleInfo: TShiftScheduleInfo;
begin
  Result:= False;
  AInfo:= EmptyPostScheduleInfo;

  Result:= StaffPostForPeriodLoad(ATabNumID, AFromDate, AToDate,
                                PostIDs, FirstDates, LastDates);

  if not Result then Exit;

  AInfo.PostIDs:= PostIDs;
  AInfo.FirstDates:= FirstDates;
  AInfo.LastDates:= LastDates;
  SetLength(AInfo.Infos, Length(PostIDs));
  for i:= 0 to High(PostIDs) do
  begin
    SetLength(AInfo.Infos[i], Length(FirstDates[i]));
    for j:=0 to High(FirstDates[i]) do
      AInfo.Infos[i, j]:= EmptyShiftScheduleInfo;
  end;

  for i:= 0 to High(PostIDs) do
  begin
    for j:=0 to High(FirstDates[i]) do
    begin
      if not StaffScheduleInfoLoad(ATabNumID, ShiftScheduleInfo,
                                   FirstDates[i, j], LastDates[i, j]) then continue;
      AInfo.Infos[i, j]:= ShiftScheduleInfo;
    end;
  end;
end;

function TDataBase.StaffScheduleInfoLoad(const ATabNumID: Integer;
                          out AInfo: TShiftScheduleInfo;
                          const AFromDate: TDate = NULDATE;
                          const AToDate: TDate = INFDATE): Boolean;
var
  i, j: Integer;
  ScheduleIDs, V: TIntVector;
  FirstDates, LastDates: TDateMatrix;
  Cycle: TScheduleCycle;
  Corrections: TScheduleCorrections;
begin
  Result:= False;
  AInfo:= EmptyShiftScheduleInfo;

  Result:= StaffScheduleForPeriodLoad(ATabNumID, AFromDate, AToDate,
                                      ScheduleIDs, FirstDates, LastDates);

  if not Result then Exit;

  AInfo.ScheduleIDs:= ScheduleIDs;
  AInfo.FirstDates:= FirstDates;
  AInfo.LastDates:= LastDates;
  SetLength(AInfo.Cycles, Length(ScheduleIDs));
  SetLength(AInfo.Corrections, Length(ScheduleIDs));
  for i:= 0 to High(ScheduleIDs) do
  begin
    AInfo.Cycles[i]:= EmptyScheduleCycle;
    SetLength(AInfo.Corrections[i], Length(FirstDates[i]));
    for j:=0 to High(FirstDates[i]) do
      AInfo.Corrections[i, j]:= EmptyScheduleCorrections;
  end;

  for i:= 0 to High(ScheduleIDs) do
  begin
    if ScheduleCycleLoad(ScheduleIDs[i], V, Cycle) then
      AInfo.Cycles[i]:= Cycle;

    for j:= 0 to High(FirstDates[i]) do
      if ScheduleShiftCorrectionsLoad(ScheduleIDs[i], V, Corrections,
                                      FirstDates[i, j], LastDates[i, j]) then
        AInfo.Corrections[i, j]:= Corrections;
  end;
end;

function TDataBase.StaffScheduleHistoryLoad(const ATabNumID: Integer;
                          out AHistoryIDs, AScheduleIDs, AWeekHours: TIntVector;
                          out ABeginDates, AEndDates: TDateVector;
                          out AScheduleNames: TStrVector;
                          const AFromDate: TDate = NULDATE;
                          const AToDate: TDate = INFDATE;
                          const AIsDesc: Boolean = True): Boolean;
var
  S: String;
begin
  AHistoryIDs:= nil;
  AScheduleIDs:= nil;
  AWeekHours:= nil;
  ABeginDates:= nil;
  AEndDates:= nil;
  AScheduleNames:= nil;

  S:= EmptyStr;
  if AIsDesc then
    S:= ' DESC';

  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ID, t1.BeginDate, t1.EndDate, t1.ScheduleID, ' +
           't2.ScheduleName, t2.WeekHours ' +
    'FROM STAFFSCHEDULE t1 ' +
    'INNER JOIN SCHEDULEMAIN t2 ON (t1.ScheduleID=t2.ScheduleID) ' +
    'WHERE (t1.TabNumID = :TabNumID) AND (' +
           SqlCROSS('t1.BeginDate', 't1.EndDate', ':BD', ':ED') +
           ') ' +
    'ORDER BY t1.BeginDate' + S
  );

  QParamInt('TabNumID', ATabNumID);
  QParamDT('BD', AFromDate);
  QParamDT('ED', AToDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AHistoryIDs, QFieldInt('ID'));
      VAppend(AScheduleIDs, QFieldInt('ScheduleID'));
      VAppend(AWeekHours, QFieldInt('WeekHours'));
      VAppend(ABeginDates, QFieldDT('BeginDate'));
      VAppend(AEndDates, QFieldDT('EndDate'));
      VAppend(AScheduleNames, QFieldStr('ScheduleName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.StaffScheduleHistoryAdd(const AHistoryID, ATabNumID, AScheduleID: Integer;
                                     const ABeginDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //меняем конечную дату текущего периода
    UpdateByInt32ID('STAFFSCHEDULE', 'EndDate', 'ID', AHistoryID, IncDay(ABeginDate, -1), False{no commit});
    //записываем новые данные
    QSetSQL(
      sqlINSERT('STAFFSCHEDULE', ['TabNumID', 'ScheduleID', 'BeginDate', 'EndDate'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('ScheduleID', AScheduleID);
    QParamDT('BeginDate', ABeginDate);
    QParamDT('EndDate', INFDATE);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffScheduleHistoryUpdate(const APrevHistoryID, AHistoryID, AScheduleID: Integer;
                                     const ABeginDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //меняем конечную дату предыдущего периода
    if APrevHistoryID>0 then
      UpdateByInt32ID('STAFFSCHEDULE', 'EndDate', 'ID', APrevHistoryID, IncDay(ABeginDate, -1), False{no commit});
    //изменяем начальную дату и график текущего периода
    QSetSQL(
      sqlUPDATE('STAFFSCHEDULE', [ 'ScheduleID', 'BeginDate']) +
      'WHERE ID = :HistoryID'
    );
    QParamInt('HistoryID', AHistoryID);
    QParamInt('ScheduleID', AScheduleID);
    QParamDT('BeginDate', ABeginDate);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.StaffScheduleHistoryDelete(const APrevHistoryID, AHistoryID: Integer;
                                     const AEndDate: TDate): Boolean;
begin
  Result:= False;
  try
    //удаление записи
    Delete('STAFFSCHEDULE', 'ID', AHistoryID, False {no commit});
    //замена конечной даты предыдущего периода на конечную дату этого периода
    UpdateByInt32ID('STAFFSCHEDULE', 'EndDate', 'ID', APrevHistoryID, AEndDate, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

procedure TDataBase.CalendarLoad(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
var
  Corrections: TCalendarCorrections;
begin
  CalendarCorrectionsLoad(ABeginDate, AEndDate, Corrections);
  ACalendar.Calc(ABeginDate, AEndDate, Corrections);
end;

function TDataBase.CalendarCorrectionsLoad(const ABeginDate, AEndDate: TDate;
                                     out ACorrections: TCalendarCorrections): Boolean;
begin
  Result:= False;
  ACorrections:= CalendarCorrectionsEmpty;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT * FROM CALENDAR ' +
    'WHERE DayDate BETWEEN :BD AND :ED ' +
    'ORDER BY DayDate');
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ACorrections.Dates, QFieldDT('DayDate'));
      VAppend(ACorrections.Statuses, QFieldInt('Status'));
      VAppend(ACorrections.SwapDays, QFieldInt('SwapDay'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.CalendarCorrectionsUpdate(const ACorrections: TCalendarCorrections): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if VIsNil(ACorrections.Dates) then Exit;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('CALENDAR', ['DayDate', 'Status', 'SwapDay'], 'REPLACE')
    );
    for i:= 0 to High(ACorrections.Dates) do
    begin
      QParamInt('Status', ACorrections.Statuses[i]);
      QParamInt('SwapDay', ACorrections.SwapDays[i]);
      QParamDT('DayDate', ACorrections.Dates[i]);
      QExec;
    end;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.CalendarCorrectionDelete(const ADate: TDate): Boolean;
begin
  Result:= Delete('CALENDAR', 'DayDate', ADate);
end;

function TDataBase.HolidaysLoad(const AYear: Word): TDateVector;
var
  D: TDate;
begin
  D:= FirstDayInYear(AYear);
  Result:= HolidaysLoad(D, D);
end;

function TDataBase.HolidaysLoad(const ABeginDate, AEndDate: TDate): TDateVector;
var
  BD, ED: TDate;
begin
  Result:= nil;
  //определяем дату начала предшествующего года
  BD:= FirstDayInYear(YearOfDate(ABeginDate)-1);
  //определяем дату конца года
  ED:= LastDayInYear(AEndDate);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DayDate FROM CALENDAR ' +
    'WHERE (DayDate BETWEEN :BD AND :ED) AND (Status = :Status) ' +
    'ORDER BY DayDate');
  QParamDT('BD', BD);
  QParamDT('ED', ED);
  QParamInt('Status', DAY_STATUS_HOLIDAY);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(Result, QFieldDT('DayDate'));
      QNext;
    end;
  end;
  QClose;
end;

function TDataBase.HolidaysCount(const ABeginDate, AEndDate: TDate): Integer;
begin
  Result:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT COUNT(DayDate) AS DaysCount FROM CALENDAR ' +
    'WHERE (DayDate BETWEEN :BD AND :ED) AND (Status = :Status)'
  );
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QParamInt('Status', DAY_STATUS_HOLIDAY);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    Result:= QFieldInt('DaysCount');
  end;
  QClose;
end;

function TDataBase.ScheduleMainListLoad(out AScheduleIDs, AWeekHours, ACycleCounts: TIntVector;
                                  out AScheduleNames: TStrVector): Boolean;
begin
  Result:= False;

  AScheduleIDs:= nil;
  AWeekHours:= nil;
  ACycleCounts:= nil;
  AScheduleNames:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('SCHEDULEMAIN', ['ScheduleID', 'ScheduleName', 'WeekHours', 'CycleCount']) +
    'WHERE ScheduleID>0 ' +
    'ORDER BY ScheduleName'
  );
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AScheduleIDs, QFieldInt('ScheduleID'));
      VAppend(AScheduleNames, QFieldStr('ScheduleName'));
      VAppend(AWeekHours, QFieldInt('WeekHours'));
      VAppend(ACycleCounts, QFieldInt('CycleCount'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.ScheduleParamsLoad(const ATableName, AFindFieldName, AIDFieldName: String;
                                const AFindValue: Integer;
                                out AParamIDs: TIntVector;
                                out ADates: TDateVector;
                                out ATotalHours, ANightHours, AShiftNums, ADigMarks: TIntVector;
                                out AStrMarks: TStrVector;
                                const ABeginDate: TDate = NULDATE;
                                const AEndDate: TDate = INFDATE): Boolean;
begin
  Result:= False;

  AParamIDs:= nil;
  ADates:= nil;
  ATotalHours:= nil;
  ANightHours:= nil;
  AShiftNums:= nil;
  ADigMarks:= nil;
  AStrMarks:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.DayDate, t1.HoursTotal, t1.HoursNight, t1.ShiftNum, t1.DigMark, t2.StrMark, ' +
           't1.' + SqlEsc(AIDFieldName, False) + ' ' +
    'FROM ' + SqlEsc(ATableName) + ' t1 ' +
    'INNER JOIN TIMETABLEMARK t2 ON (t1.DigMark=t2.DigMark) ' +
    'WHERE (t1.' + SqlEsc(AFindFieldName, False) + ' = :FindValue) AND ' +
          '(t1.DayDate BETWEEN :BD AND :ED) ' +
    'ORDER BY t1.DayDate'
  );
  QParamInt('FindValue', AFindValue);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AParamIDs, QFieldInt(AIDFieldName));
      VAppend(ADates, QFieldDT('DayDate'));
      VAppend(ATotalHours, QFieldInt('HoursTotal'));
      VAppend(ANightHours, QFieldInt('HoursNight'));
      VAppend(AShiftNums, QFieldInt('ShiftNum'));
      VAppend(ADigMarks, QFieldInt('DigMark'));
      VAppend(AStrMarks, QFieldStr('StrMark'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.ScheduleCycleLoad(const AScheduleID: Integer;
                               out ACycleIDs: TIntVector;
                               out ACycle: TScheduleCycle): Boolean;
begin
  ACycle:= EmptyScheduleCycle;
  ACycle.ScheduleID:= AScheduleID;
  ACycle.Count:= ValueInt32ByInt32ID('SCHEDULEMAIN', 'CycleCount', 'ScheduleID', AScheduleID);
  ACycle.IsWeek:= ACycle.Count=0;
  Result:= ScheduleParamsLoad('SCHEDULECYCLE', 'ScheduleID', 'CycleID', AScheduleID,
    ACycleIDs, ACycle.Dates, ACycle.HoursTotal, ACycle.HoursNight, ACycle.ShiftNums,
    ACycle.DigMarks, ACycle.StrMarks);
end;

function TDataBase.ScheduleShiftCorrectionsLoad(const AScheduleID: Integer;
                               out ACorrectIDs: TIntVector;
                               out ACorrections: TScheduleCorrections;
                               const ABeginDate: TDate = NULDATE;
                               const AEndDate: TDate = INFDATE): Boolean;
begin
  ACorrections:= EmptyScheduleCorrections;
  Result:= ScheduleParamsLoad('SCHEDULECORRECT', 'ScheduleID', 'CorrectID', AScheduleID,
    ACorrectIDs, ACorrections.Dates, ACorrections.HoursTotal, ACorrections.HoursNight, ACorrections.ShiftNums,
    ACorrections.DigMarks, ACorrections.StrMarks, ABeginDate, AEndDate);
end;

function TDataBase.ScheduleCorrectionsUpdate(const ATableName, AIDFieldName: String;
                               const AIDValue: Integer;
                               const ACorrections: TScheduleCorrections): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if VIsNil(ACorrections.Dates) then Exit;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT(ATableName,
        [AIDFieldName, 'DayDate', 'HoursTotal', 'HoursNight', 'DigMark', 'ShiftNum'], 'REPLACE')
    );
    QParamInt(AIDFieldName, AIDValue);
    for i:= 0 to High(ACorrections.Dates) do
    begin
      QParamDT('DayDate', ACorrections.Dates[i]);
      QParamInt('HoursTotal', ACorrections.HoursTotal[i]);
      QParamInt('HoursNight', ACorrections.HoursNight[i]);
      QParamInt('DigMark', ACorrections.DigMarks[i]);
      QParamInt('ShiftNum', ACorrections.ShiftNums[i]);
      QExec;
    end;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.ScheduleShiftCorrectionsUpdate(const AScheduleID: Integer;
  const ACorrections: TScheduleCorrections): Boolean;
begin
  Result:= ScheduleCorrectionsUpdate('SCHEDULECORRECT', 'ScheduleID', AScheduleID, ACorrections);
end;

function TDataBase.ScheduleShiftCorrectionDelete(const ACorrectID: Integer): Boolean;
begin
  Result:= Delete('SCHEDULECORRECT', 'CorrectID', ACorrectID);
end;

function TDataBase.SchedulePersonalCorrectionsLoad(const ATabNumID: Integer;
                               out ACorrectIDs: TIntVector;
                               out ACorrections: TScheduleCorrections;
                               const ABeginDate: TDate = NULDATE;
                               const AEndDate: TDate = INFDATE): Boolean;
begin
  ACorrections:= EmptyScheduleCorrections;
  Result:= ScheduleParamsLoad('PERSONALCORRECT', 'TabNumID', 'CorrectID', ATabNumID,
    ACorrectIDs, ACorrections.Dates, ACorrections.HoursTotal, ACorrections.HoursNight, ACorrections.ShiftNums,
    ACorrections.DigMarks, ACorrections.StrMarks, ABeginDate, AEndDate);
end;

function TDataBase.SchedulePersonalCorrectionsUpdate(const ATabNumID: Integer;
  const ACorrections: TScheduleCorrections): Boolean;
begin
  Result:= ScheduleCorrectionsUpdate('PERSONALCORRECT', 'TabNumID', ATabNumID, ACorrections);
end;

function TDataBase.SchedulePersonalCorrectionDelete(const ACorrectID: Integer): Boolean;
begin
  Result:= Delete('PERSONALCORRECT', 'CorrectID', ACorrectID);
end;

function TDataBase.ScheduleShiftIsExists(const AScheduleID: Integer;
  const AScheduleName: String): Boolean;
begin
  Result:= IsValueInTableNotMatchInt32ID('SCHEDULEMAIN', 'ScheduleName', AScheduleName,
                                         'ScheduleID', AScheduleID);
end;

procedure ScheduleCycleAdd(const ACycle: TScheduleCycle);
var
  i: Integer;
begin
  QSetSQL(
    sqlINSERT('SCHEDULECYCLE', ['ScheduleID', 'DayDate', 'HoursTotal', 'HoursNight',
                                'DigMark', 'ShiftNum'])
  );
  QParamInt('ScheduleID', ACycle.ScheduleID);
  for i:= 0 to High(ACycle.Dates) do
  begin
    QParamDT('DayDate', ACycle.Dates[i]);
    QParamInt('HoursTotal', ACycle.HoursTotal[i]);
    QParamInt('HoursNight', ACycle.HoursNight[i]);
    QParamInt('DigMark', ACycle.DigMarks[i]);
    QParamInt('ShiftNum', ACycle.ShiftNums[i]);
    QExec;
  end;
end;


function TDataBase.ScheduleShiftAdd(const AScheduleName: String;
  const AWeekHours: Integer; var ACycle: TScheduleCycle): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись графика
    QSetSQL(
      sqlINSERT('SCHEDULEMAIN', ['ScheduleName', 'WeekHours', 'CycleCount'])
    );
    QParamStr('ScheduleName', AScheduleName);
    QParamInt('WeekHours', AWeekHours);
    QParamInt('CycleCount', ACycle.Count);
    QExec;
    //получение ID записанного графика
    ACycle.ScheduleID:= LastWritedInt32ID('SCHEDULEMAIN');
    //запись цикла графика
    ScheduleCycleAdd(ACycle);
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.ScheduleShiftUpdate(const AScheduleName: String;
  const AWeekHours: Integer; const ACycle: TScheduleCycle): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //обновление данных графика
    QSetSQL(
      sqlUPDATE('SCHEDULEMAIN', ['ScheduleName', 'WeekHours', 'CycleCount']) +
      'WHERE ScheduleID = :ScheduleID'
    );
    QParamStr('ScheduleName', AScheduleName);
    QParamInt('WeekHours', AWeekHours);
    QParamInt('CycleCount', ACycle.Count);
    QExec;
    //удаление старого цикла графика
    Delete('SCHEDULECYCLE', 'ScheduleID', ACycle.ScheduleID, False {no commit});
    //запись цикла графика
    ScheduleCycleAdd(ACycle);
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.ScheduleShiftDelete(const AScheduleID: Integer): Boolean;
begin
  Result:= Delete('SCHEDULEMAIN', 'ScheduleID', AScheduleID);
end;

function TDataBase.VacationsEditingLoad(const ATabNumID, AYear: Integer;
                                out ADates: TDateVector;
                                out ACounts, AAddCounts: TIntVector): Boolean;
begin
  VDim(ADates{%H-}, 4);
  VDim(ACounts{%H-}, 4);
  VDim(AAddCounts{%H-}, 4);

  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT * FROM STAFFVACATION ' +
    'WHERE (TabNumID = :TabNumID) AND (YearNum = :YearNum)'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamInt('YearNum', AYear);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    ADates[0]:= QFieldDT('Plan1Date');
    ADates[1]:= QFieldDT('Fact1Date');
    ADates[2]:= QFieldDT('Plan2Date');
    ADates[3]:= QFieldDT('Fact2Date');
    ACounts[0]:= QFieldInt('Plan1Count');
    ACounts[1]:= QFieldInt('Fact1Count');
    ACounts[2]:= QFieldInt('Plan2Count');
    ACounts[3]:= QFieldInt('Fact2Count');
    AAddCounts[0]:= QFieldInt('Plan1CountAdd');
    AAddCounts[1]:= QFieldInt('Fact1CountAdd');
    AAddCounts[2]:= QFieldInt('Plan2CountAdd');
    AAddCounts[3]:= QFieldInt('Fact2CountAdd');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.VacationsEditingUpdate(const ATabNumID, AYear: Integer;
                                  const ADates: TDateVector;
                                  const ACounts, AAddCounts: TIntVector): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('STAFFVACATION', ['TabNumID', 'YearNum',
                                  'Plan1Date', 'Plan1Count', 'Plan1CountAdd',
                                  'Fact1Date', 'Fact1Count', 'Fact1CountAdd',
                                  'Plan2Date', 'Plan2Count', 'Plan2CountAdd',
                                  'Fact2Date', 'Fact2Count', 'Fact2CountAdd'], 'REPLACE')
    );

    QParamInt('TabNumID', ATabNumID);
    QParamInt('YearNum', AYear);

    QParamDT('Plan1Date', ADates[0]);
    QParamDT('Fact1Date', ADates[1]);
    QParamDT('Plan2Date', ADates[2]);
    QParamDT('Fact2Date', ADates[3]);
    QParamInt('Plan1Count', ACounts[0]);
    QParamInt('Fact1Count', ACounts[1]);
    QParamInt('Plan2Count', ACounts[2]);
    QParamInt('Fact2Count', ACounts[3]);
    QParamInt('Plan1CountAdd', AAddCounts[0]);
    QParamInt('Fact1CountAdd', AAddCounts[1]);
    QParamInt('Plan2CountAdd', AAddCounts[2]);
    QParamInt('Fact2CountAdd', AAddCounts[3]);

    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.VacationLoad(const ATabNumID: Integer; const ABeginDate, AEndDate: TDate;
                          out AFirstDates: TDateVector; out ACounts, AAddCounts: TIntVector;
                          const AIsPlane: Boolean = False): Boolean;
var
  BD, ED, D: TDate;
  S: String;
begin
  Result:= False;
  AFirstDates:= nil;
  ACounts:= nil;
  AAddCounts:= nil;

  if AIsPlane then
    S:= 'Plan'
  else
    S:= 'Fact';

  //определяем дату начала года, предшествующего году даты ABeginDate
  BD:= FirstDayInYear(YearOfDate(ABeginDate)-1);
  //определяем дату конца года даты AEndDate
  ED:= LastDayInYear(YearOfDate(AEndDate));
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT ' + S + '1Date, ' + S + '1Count, ' + S + '1CountAdd,  ' +
                S + '2Date, ' + S + '2Count, ' + S + '2CountAdd  ' +
    'FROM STAFFVACATION ' +
    'WHERE (TabNumID = :TabNumID) AND ' +
          '((' + S + '1Date BETWEEN :BD AND :ED) OR (' + S + '2Date BETWEEN :BD AND :ED))'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('BD', BD);
  QParamDT('ED', ED);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      D:= QFieldDT(S+'1Date');
      if IsDateInPeriod(D, BD, ED) then
      begin
        VAppend(AFirstDates, D);
        VAppend(ACounts, QFieldInt(S+'1Count'));
        VAppend(AAddCounts, QFieldInt(S+'1CountAdd'));
      end;
      D:= QFieldDT(S+'2Date');
      if IsDateInPeriod(D, BD, ED) then
      begin
        VAppend(AFirstDates, D);
        VAppend(ACounts, QFieldInt(S+'2Count'));
        VAppend(AAddCounts, QFieldInt(S+'2CountAdd'));
      end;
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.VacationScheduleLoad(const AYear: Integer;
                             out AStaffNames, ATabNums, APostNames: TStrVector;
                             out AFirstDates: TDateVector; out ATotalCounts: TIntVector): Boolean;
var
  BD, ED: TDate;
  Name, TabNum: String;
  i, TabNumID: Integer;
  TabNumIDs: TIntVector;

  procedure VacationAdd(const AVacationName: String);
  var
    D: TDate;
    N: Integer;
  begin
    D:= QFieldDT(AVacationName+'Date');
    N:= QFieldInt(AVacationName+'Count') + QFieldInt(AVacationName+'CountAdd');
    if (D>0) and (N>0) then
    begin
      VAppend(AStaffNames, Name);
      VAppend(TabNumIDs, TabNumID);
      VAppend(ATabNums, TabNum);
      VAppend(AFirstDates, D);
      VAppend(ATotalCounts, N);
    end;
  end;

begin
  Result:= False;
  AStaffNames:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  AFirstDates:= nil;
  ATotalCounts:= nil;
  TabNumIDs:= nil;

  FirstLastDayInYear(AYear, BD, ED);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.TabNum, t1.TabNumID, ' +
           't2.Name, t2.Patronymic, t2.Family, ' +
           't3.Plan1Date, t3.Plan1Count, t3.Plan1CountAdd, ' +
           't3.Plan2Date, t3.Plan2Count, t3.Plan2CountAdd ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'LEFT OUTER JOIN STAFFVACATION t3 ON (t1.TabNumID=t3.TabNumID) '  +
    'WHERE (t1.RecrutDate<= :ED) AND (t1.DismissDate >= :BD) AND (t3.YearNum=:YearNum) ' +
    'ORDER BY t2.Family, t2.Name, t2.Patronymic'
  );
  QParamDT('BD', BD);
  QParamDT('ED', ED);
  QParamInt('YearNum', AYear);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      Name:= SNameLong(QFieldStr('Family'), QFieldStr('Name'), QFieldStr('Patronymic'));
      TabNum:= QFieldStr('TabNum');
      TabNumID:= QFieldInt('TabNumID');
      VacationAdd('Plan1');
      VacationAdd('Plan2');
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  //актуальные должности и разряды на начало года
  for i:= 0 to High(TabNumIDs) do
  begin
    StaffPostForDate(TabNumIDs[i], BD, TabNumID{tmp}, Name, TabNum{tmp});
    VAppend(APostNames, Name);
  end;
end;

function TDataBase.VacationPlanListForYearLoad(const AFilterValue: String;
                             const AOrderType, ARecrutType: Byte;
                             const AYear: Integer;
                             out ATabNumIDs: TIntVector;
                             out AFamilies, ANames, APatronymics, ATabNums, APostNames: TStrVector;
                             out ARecrutDates, APlan1FirstDates, APlan2FirstDates: TDateVector;
                             out APlan1Counts, APlan1AddCounts, APlan2Counts, APlan2AddCounts: TIntVector): Boolean;
var
  i, j: Integer;
  BD, ED: TDate;
  S1, S2, SQLStr: String;
  Indexes: TIntVector;
begin
  Result:= False;
  ATabNumIDs:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ARecrutDates:= nil;
  APlan1FirstDates:= nil;
  APlan1Counts:= nil;
  APlan1AddCounts:= nil;
  APlan2FirstDates:= nil;
  APlan2Counts:= nil;
  APlan2AddCounts:= nil;

  FirstLastDayInYear(AYear, BD, ED);

  SQLStr:=
    'SELECT t1.TabNum, t1.TabNumID, t1.RecrutDate, ' +
           't2.Name, t2.Patronymic, t2.Family, ' +
           't3.Plan1Date, t3.Plan1Count, t3.Plan1CountAdd, ' +
           't3.Plan2Date, t3.Plan2Count, t3.Plan2CountAdd ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'LEFT OUTER JOIN (' +
       'SELECT TabNumID, YearNum, ' +
              'Plan1Date, Plan1Count, Plan1CountAdd, ' +
              'Plan2Date, Plan2Count, Plan2CountAdd ' +
       'FROM STAFFVACATION ' +
       'WHERE YearNum=:YearNum ' +
       ') t3 ON (t1.TabNumID=t3.TabNumID) ';

  case ARecrutType of
    0: SQLStr:= SQLStr + 'WHERE ((t1.RecrutDate < :BD) AND (t1.DismissDate >= :BD)) ';
    1: SQLStr:= SQLStr + 'WHERE (t1.RecrutDate BETWEEN :BD AND :ED) ';
    2: SQLStr:= SQLStr + 'WHERE ((t1.RecrutDate <= :ED) AND (t1.DismissDate >= :BD)) ';
  end;

  if not SEmpty(AFilterValue) then
    SQLStr:= SQLStr + 'AND (t2.FullName LIKE :FilterValue) ';

  case AOrderType of
    0,2: SQLStr:= SQLStr + 'ORDER BY t2.Family, t2.Name, t2.Patronymic';
    1: SQLStr:= SQLStr + 'ORDER BY t1.TabNum, t2.Family, t2.Name, t2.Patronymic';
    3: SQLStr:= SQLStr + 'ORDER BY t3.Plan1Date, t2.Family, t2.Name, t2.Patronymic';
    4: SQLStr:= SQLStr + 'ORDER BY t3.Plan2Date, t2.Family, t2.Name, t2.Patronymic';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('BD', BD);
  QParamDT('ED', ED);
  QParamInt('YearNum', AYear);
  QParamStr('FilterValue', '%'+AFilterValue+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(ARecrutDates, QFieldDT('RecrutDate'));

      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));

      VAppend(APlan1FirstDates, QFieldDT('Plan1Date'));
      VAppend(APlan2FirstDates, QFieldDT('Plan2Date'));
      VAppend(APlan1Counts, QFieldInt('Plan1Count'));
      VAppend(APlan2Counts, QFieldInt('Plan2Count'));
      VAppend(APlan1AddCounts, QFieldInt('Plan1CountAdd'));
      VAppend(APlan2AddCounts, QFieldInt('Plan2CountAdd'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на начало года
  for i:= 0 to High(ATabNumIDs) do
  begin
    StaffPostForDate(ATabNumIDs[i], BD, j, S1, S2);
    VAppend(APostNames, S1);
  end;

  //сортировка по наименованию должности
  if AOrderType<>2 then Exit;
  VSort(APostNames, Indexes);

  APostNames:= VReplace(APostNames, Indexes);
  ATabNumIDs:= VReplace(ATabNumIDs, Indexes);
  ATabNums:= VReplace(ATabNums, Indexes);
  AFamilies:= VReplace(AFamilies, Indexes);
  ANames:= VReplace(ANames, Indexes);
  APatronymics:= VReplace(APatronymics, Indexes);
  APlan1FirstDates:= VReplace(APlan1FirstDates, Indexes);
  APlan2FirstDates:= VReplace(APlan2FirstDates, Indexes);
  APlan1Counts:= VReplace(APlan1Counts, Indexes);
  APlan2Counts:= VReplace(APlan2Counts, Indexes);
  APlan1AddCounts:= VReplace(APlan1Counts, Indexes);
  APlan2AddCounts:= VReplace(APlan2Counts, Indexes);
end;

function TDataBase.VacationRecordIsExists(const AYear, ATabNumID: Integer): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT ID ' +
    'FROM STAFFVACATION ' +
    'WHERE (TabNumID = :TabNumID) AND (YearNum = :YearNum)'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamInt('YearNum', AYear);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TDataBase.VacationPlanLoad(const AYear, ATabNumID: Integer;
                              out APlan1FirstDate, APlan2FirstDate: TDate;
                              out APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
begin
  Result:= False;

  APlan1FirstDate:= 0;
  APlan2FirstDate:= 0;
  APlan1Count:= 0;
  APlan1AddCount:= 0;
  APlan2Count:= 0;
  APlan2AddCount:= 0;

  QSetQuery(FQuery);
  QSetSQL(
    SqlSELECT('STAFFVACATION', ['Plan1Date', 'Plan1Count', 'Plan1CountAdd',
                                'Plan2Date', 'Plan2Count', 'Plan2CountAdd']) +
    'WHERE (TabNumID = :TabNumID) AND (YearNum = :YearNum)'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamInt('YearNum', AYear);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    APlan1FirstDate:= QFieldDT('Plan1Date');
    APlan2FirstDate:= QFieldDT('Plan2Date');
    APlan1Count:= QFieldInt('Plan1Count');
    APlan1AddCount:= QFieldInt('Plan1CountAdd');
    APlan2Count:= QFieldInt('Plan2Count');
    APlan2AddCount:= QFieldInt('Plan2CountAdd');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.VacationPlanAdd(const AYear, ATabNumID: Integer;
                              const APlan1FirstDate, APlan2FirstDate: TDate;
                              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('STAFFVACATION', ['TabNumID', 'YearNum',
                                  'Plan1Date', 'Plan1Count', 'Plan1CountAdd',
                                  'Plan2Date', 'Plan2Count', 'Plan2CountAdd',
                                  'Fact1Date', 'Fact1Count', 'Fact1CountAdd',
                                  'Fact2Date', 'Fact2Count', 'Fact2CountAdd'])
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('YearNum', AYear);

    QParamDT('Plan1Date', APlan1FirstDate);
    QParamInt('Plan1Count', APlan1Count);
    QParamInt('Plan1CountAdd', APlan1AddCount);
    QParamDT('Plan2Date', APlan2FirstDate, APlan2FirstDate>0);
    QParamInt('Plan2Count', APlan2Count, APlan2Count>0);
    QParamInt('Plan2CountAdd', APlan2AddCount, APlan2AddCount>0);

    QParamDT('Fact1Date', APlan1FirstDate);
    QParamInt('Fact1Count', APlan1Count);
    QParamInt('Fact1CountAdd', APlan1AddCount);
    QParamDT('Fact2Date', APlan2FirstDate, APlan2FirstDate>0);
    QParamInt('Fact2Count', APlan2Count, APlan2Count>0);
    QParamInt('Fact2CountAdd', APlan2AddCount, APlan2AddCount>0);

    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.VacationPlanUpdate(const AYear, ATabNumID: Integer;
                              const APlan1FirstDate, APlan2FirstDate: TDate;
                              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('STAFFVACATION', ['Plan1Date', 'Plan1Count', 'Plan1CountAdd',
                                  'Plan2Date', 'Plan2Count', 'Plan2CountAdd']) +
      'WHERE (TabNumID = :TabNumID) AND (YearNum = :YearNum)'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('YearNum', AYear);

    QParamDT('Plan1Date', APlan1FirstDate);
    QParamInt('Plan1Count', APlan1Count);
    QParamInt('Plan1CountAdd', APlan1AddCount);
    QParamDT('Plan2Date', APlan2FirstDate, APlan2FirstDate>0);
    QParamInt('Plan2Count', APlan2Count, APlan2Count>0);
    QParamInt('Plan2CountAdd', APlan2AddCount, APlan2AddCount>0);

    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.VacationPlanEdit(const AYear, ATabNumID: Integer;
                              const APlan1FirstDate, APlan2FirstDate: TDate;
                              const APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount: Integer): Boolean;
begin
  if VacationRecordIsExists(AYear, ATabNumID) then
    Result:= VacationPlanUpdate(AYear, ATabNumID, APlan1FirstDate, APlan2FirstDate,
                                APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount)
  else
    Result:= VacationPlanAdd(AYear, ATabNumID, APlan1FirstDate, APlan2FirstDate,
                             APlan1Count, APlan1AddCount, APlan2Count, APlan2AddCount);
end;

function TDataBase.VacationPlanDateUpdate(const AYear, ATabNumID, APart: Integer;
                              const APlanFirstDate: TDate): Boolean;
var
  S: String;
begin
  Result:= False;

  if APart=1 then
    S:= 'Plan1Date'
  else
    S:= 'Plan2Date';

  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('STAFFVACATION', [S]) +
      'WHERE (TabNumID = :TabNumID) AND (YearNum = :YearNum)'
    );
    QParamInt('TabNumID', ATabNumID);
    QParamInt('YearNum', AYear);
    QParamDT(S, APlanFirstDate);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.TimetableMarkListLoad(out ADigMarks: TIntVector;
                                   out AStrMarks, ANotes: TStrVector;
                                   const AIDNotZero: Boolean = True;
                                   const AMarkType: Integer = -1): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  ADigMarks:= nil;
  AStrMarks:= nil;
  ANotes:= nil;

  SQLStr:=
    sqlSELECT('TIMETABLEMARK', ['DigMark', 'StrMark', 'Note']);
  if AIDNotZero then
    SQLStr:= SQLStr + 'WHERE (DigMark > 0) '
  else
    SQLStr:= SQLStr + 'WHERE (DigMark >= 0) ';
  if AMarkType>=0 then
    SQLStr:= SQLStr + 'AND (TypeMark = :TypeMark) ';
  SQLStr:= SQLStr +
    'ORDER BY DigMark';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamInt('TypeMark', AMarkType);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADigMarks, QFieldInt('DigMark'));
      VAppend(AStrMarks, QFieldStr('StrMark'));
      VAppend(ANotes, QFieldStr('Note'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.TimetableMarkListLoad(out ADigMarks: TIntVector;
                                   out AItemMarks: TStrVector;
                                   const AIDNotZero: Boolean = True;
                                   const AMarkType: Integer = -1): Boolean;
var
  i: Integer;
  StrMarks, Notes: TStrVector;
begin
  AItemMarks:= nil;
  Result:= TimetableMarkListLoad(ADigMarks, StrMarks, Notes, AIDNotZero, AMarkType);
  if not Result then Exit;
  VDim(AItemMarks, Length(ADigMarks));
  for i:= 0 to High(ADigMarks) do
    AItemMarks[i]:= StrMarks[i] +
                ' (' + SFillLeft(IntToStr(ADigMarks[i]), 2, '0') + ') ' +
                EMPTY_MARK + SYMBOL_SPACE + Notes[i];
end;

function TDataBase.TimetableStrMarkLoad(const ADigMark: Integer): String;
begin
  Result:= ValueStrByInt32ID('TIMETABLEMARK', 'StrMark', 'DigMark',  ADigMark);
end;

function TDataBase.TimetableFirstWritedDateLoad(const ATabNumID: Integer;
                                   out ADate: TDate): Boolean;
begin
  Result:= False;
  ADate:= NULDATE;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DayDate ' +
    'FROM TIMETABLELOG ' +
    'WHERE TabNumID = :TabNumID ' +
    'ORDER BY DayDate ' +
    'LIMIT 1');
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    ADate:= QFieldDT('DayDate');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.TimetableLastWritedDateLoad(const ATabNumID: Integer;
                                   out ADate: TDate): Boolean;
begin
  ADate:= NULDATE;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DayDate ' +
    'FROM TIMETABLELOG ' +
    'WHERE TabNumID = :TabNumID ' +
    'ORDER BY DayDate DESC ' +
    'LIMIT 1');
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    ADate:= QFieldDT('DayDate');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.TimetableFirstLastWritedDatesLoad(const ATabNumID: Integer;
                                   out AFirstDate, ALastDate: TDate): Boolean;
begin
  Result:= TimetableFirstWritedDateLoad(ATabNumID, AFirstDate) and
           TimetableLastWritedDateLoad(ATabNumID, ALastDate);
end;

function TDataBase.TimetableSumHoursInPeriodLoad(const AFieldName: String;
                                   const ATabNumID: Integer;
                                   const ABeginDate, AEndDate: TDate): Integer;
var
  FieldName: String;
begin
  Result:= 0;
  FieldName:= SQLEsc(AFieldName);
  QSetQuery(FQuery);
  QSetSQL(
   'SELECT SUM(' + FieldName + ') AS SumHours ' +
   'FROM TIMETABLELOG ' +
   'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND ('+FieldName+'>0)');
  QParamInt('TabNumID', ATabNumID);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
    Result:= QFieldInt('SumHours');
  QClose;
end;

function TDataBase.TimetableSumTotalHoursInPeriodLoad(const ATabNumID: Integer;
  const ABeginDate, AEndDate: TDate): Integer;
begin
  Result:= TimetableSumHoursInPeriodLoad('TotalHours', ATabNumID, ABeginDate, AEndDate);
end;

function TDataBase.TimetableSumNightHoursInPeriodLoad(const ATabNumID: Integer;
  const ABeginDate, AEndDate: TDate): Integer;
begin
  Result:= TimetableSumHoursInPeriodLoad('NightHours', ATabNumID, ABeginDate, AEndDate);
end;

function TDataBase.TimetableDayLoad(const TabNumID: Integer; const ADate: TDate;
  out ATimetableDay: TTimetableDay; out AMarkType: Integer): Boolean;
begin
  Result:= False;
  ATimetableDay:= TimetableDayEmpty;
  AMarkType:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.*, t2.TypeMark ' +
    'FROM TIMETABLELOG t1 ' +
    'INNER JOIN TIMETABLEMARK t2 ON (t1.DigMark=t2.DigMark) ' +
    'WHERE (t1.TabNumID = :TabNumID) AND (t1.DayDate = :ADate) AND (t1.DigMark>0)'
  );
  QParamInt('TabNumID', TabNumID);
  QParamDT('ADate', ADate);
  QOpen;
  if not QIsEmpty then
  begin
    ATimetableDay.ScheduleHours:= QFieldInt('SchedHours');
    ATimetableDay.TotalHours:= QFieldInt('TotalHours');
    ATimetableDay.NightHours:= QFieldInt('NightHours');
    ATimetableDay.OverHours:= QFieldInt('OverHours');
    ATimetableDay.SkipHours:= QFieldInt('SkipHours');
    ATimetableDay.DigMark:= QFieldInt('DigMark');
    ATimetableDay.SkipMark:= QFieldInt('SkipMark');
    ATimetableDay.ScheduleID:= QFieldInt('SchedID');
    ATimetableDay.ShiftNum:= QFieldInt('ShiftNum');
    AMarkType:= QFieldInt('TypeMark');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.TimetableDayInfoLoad(const ATabNumID: Integer; const ADate: TDate;
                          out AWritedScheduleHours: Integer;
                          out AIsManualChangedDay: Boolean): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT SchedHours, SchedID ' +
    'FROM TIMETABLELOG ' +
    'WHERE (TabNumID = :TabNumID) AND (DayDate = :DayDate)');
  QParamInt('TabNumID', ATabNumID);
  QParamDT('DayDate', ADate);
  QOpen;
  if not QIsEmpty then
  begin
    AIsManualChangedDay:= QFieldInt('SchedID')=MANUAL_SCHEDULEID;
    AWritedScheduleHours:= QFieldInt('SchedHours');
    Result:= True;
  end;
  QClose;
end;

procedure TDataBase.TimetableDayAdd(const ATabNumID: Integer; const ADate: TDate;
                                    const ATimetableDay: TTimetableDay);
begin
  QSetQuery(FQuery);
  QSetSQL(
    sqlINSERT('TIMETABLELOG', ['TabNumID', 'DayDate', 'SchedHours',
                               'TotalHours', 'NightHours', 'OverHours',
                               'SkipHours', 'DigMark', 'SkipMark',
                               'SchedID', 'ShiftNum'])
    );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('DayDate', ADate);
  QParamInt('SchedHours', ATimetableDay.ScheduleHours);
  QParamInt('TotalHours', ATimetableDay.TotalHours);
  QParamInt('NightHours', ATimetableDay.NightHours);
  QParamInt('OverHours', ATimetableDay.OverHours);
  QParamInt('SkipHours', ATimetableDay.SkipHours);
  QParamInt('DigMark', ATimetableDay.DigMark);
  QParamInt('SkipMark', ATimetableDay.SkipMark);
  QParamInt('SchedID', ATimetableDay.ScheduleID);
  QParamInt('ShiftNum', ATimetableDay.ShiftNum);
  QExec;
end;

procedure TDataBase.TimetableDayDelete(const ATabNumID: Integer; const ADate: TDate);
begin
  QSetQuery(FQuery);
  QSetSQL(
    'DELETE FROM TIMETABLELOG ' +
    'WHERE (TabNumID = :TabNumID) AND (DayDate = :DayDate)'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('DayDate', ADate);
  QExec;
end;

function TDataBase.TimetableDaysDelete(const ATabNumID: Integer;
                                       const ABeginDate, AEndDate: TDate;
                                       const ANeedCommit: Boolean = True): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  QSetSQL(
    'DELETE FROM TIMETABLELOG ' +
    'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BeginDate AND :EndDate)'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('BeginDate', ABeginDate);
  QParamDT('EndDate', AEndDate);
  try
    QExec;
    if ANeedCommit then QCommit;
    Result:= True;
  except
    if ANeedCommit then QRollback;
  end;
end;

function TDataBase.TimetableDaysReplace(const ATabNumID: Integer;
                                  const ADates: TDateVector;
                                  const ATimetableDay: TTimetableDay): Boolean;
var
  i: Integer;
begin
  Result:= False;
  if VIsNil(ADates) then Exit;
  QSetQuery(FQuery);
  try
    for i:= 0 to High(ADates) do
    begin
      TimetableDayDelete(ATabNumID, ADates[i]);
      TimetableDayAdd(ATabNumID, ADates[i], ATimetableDay);
    end;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.TimetableDaysByCorrectionAdd(const ATabNumID: Integer;
                             ATimetableDay: TTimetableDay;
                             const ACalendar: TCalendar;
                             const ASchedule: TPersonalSchedule;
                             const ANeedDeleteOld: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    if ANeedDeleteOld then
      TimetableDaysDelete(ATabNumID, ACalendar.BeginDate, ACalendar.EndDate, False{no commit});

    //пробегаем по датам периода
    for i:= 0 to ACalendar.DaysCount-1 do
    begin
      //если график не существует, переходим к следующему дню
      if ASchedule.IsExists[i] = EXISTS_NO then continue;
      //определяем данные за день (ATimetableDay содержит все, кроме часов по графику)
      ATimetableDay.ScheduleHours:= ASchedule.HoursCorrect.Totals[i];  {WorkHoursCorrect - потому что в графиковом времени нужно учитывать раб часы без учета отпуска, но  с корректировками}
      //записываем данные за день
      TimetableDayAdd(ATabNumID, ACalendar.Dates[i], ATimetableDay);
    end;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

procedure TDataBase.TimetableScheduleHoursUpdate(const ATabNumID: Integer;
                          const ADate: TDate;
                          const ASchedHours: Integer);
begin
  QSetQuery(FQuery);
  QSetSQL(
    'UPDATE TIMETABLELOG ' +
    'SET SchedHours = :SchedHours ' +
    'WHERE (TabNumID = :TabNumID) AND (DayDate = :DayDate)'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamInt('SchedHours', ASchedHours);
  QParamDT('DayDate', ADate);
  QExec;
end;

function TDataBase.TimetableByScheduleUpdate(const ATabNumID: Integer;
  const ACalendar: TCalendar; const ASchedule: TPersonalSchedule;
  const AUpdateWritedOnly: Boolean): Boolean;
var
  i, WritedScheduleHours: Integer;
  IsManualChangedDay: Boolean;
  TimetableDay: TTimetableDay;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //пробегаем по датам периода
    for i:= 0 to ACalendar.DaysCount-1 do
    begin
      //если табель на этот день уже записан
      if TimetableDayInfoLoad(ATabNumID, ACalendar.Dates[i], WritedScheduleHours, IsManualChangedDay) then
      begin
        if IsManualChangedDay then //если табель был изменен вручную
        begin
          //обновляем часы по графику
          if ASchedule.HoursCorrect.Totals[i]<>WritedScheduleHours then
            TimetableScheduleHoursUpdate(ATabNumID, ACalendar.Dates[i], ASchedule.HoursCorrect.Totals[i]); {HoursCorrect - потому что в графиковом времени нужно учитывать раб часы без учета отпуска, но  с корректировками}
        end
        else begin //табель был заполнен по графику
          //удаляем день
          TimetableDayDelete(ATabNumID, ACalendar.Dates[i]);
          //определяем данные за день
          TimetableDay:= TimetableDayDataFromSchedule(ASchedule, ACalendar.DayStatuses[i], i);
          //записываем данные за день
          TimetableDayAdd(ATabNumID, ACalendar.Dates[i], TimetableDay);
        end;
      end
      else begin //день не записан в базу
        if AUpdateWritedOnly then continue;
        //определяем данные за день
        TimetableDay:= TimetableDayDataFromSchedule(ASchedule, ACalendar.DayStatuses[i], i);
        //записываем данные за день
        TimetableDayAdd(ATabNumID, ACalendar.Dates[i], TimetableDay);
      end;
    end;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.TimetableDataMonthForEditLoad(const ATabNumID, AMonth, AYear: Integer;
             out ADates: TDateVector;
             out ATimetableStrings, AScheduleNames: TStrVector;
             out ATotalHours, ANightHours, AOverHours, ASkipHours, ASchedHours,
                 AMainMarks, ASkipMarks, ASchedIDs, AShiftNums: TIntVector): Boolean;
var
  BD, ED: TDate;
  X: Integer;
  S: String;
begin
  Result:= False;
  ADates:= nil;
  ATimetableStrings:= nil;
  AScheduleNames:= nil;
  ATotalHours:= nil;
  ANightHours:= nil;
  AOverHours:= nil;
  ASkipHours:= nil;
  ASchedHours:= nil;
  AMainMarks:= nil;
  ASkipMarks:= nil;
  ASchedIDs:= nil;
  AShiftNums:= nil;

  FirstLastDayInMonth(AMonth, AYear, BD, ED);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.DayDate, t1.TotalHours, t1.NightHours, t1.OverHours, t1.SkipHours, ' +
           't1.SchedHours, t1.SchedID, t1.ShiftNum, '+
           't1.DigMark AS MainMarkDig, t1.SkipMark AS SkipMarkDig, '+
           't2.StrMark AS MainMarkStr, t3.StrMark AS SkipMarkStr, t2.TypeMark, ' +
           't4.ScheduleName ' +
    'FROM TIMETABLELOG t1 ' +
    'INNER JOIN TIMETABLEMARK t2 ON (t1.DigMark=t2.DigMark) ' +
    'INNER JOIN TIMETABLEMARK t3 ON (t1.SkipMark=t3.DigMark) ' +
    'INNER JOIN SCHEDULEMAIN t4 ON (t1.SchedID=t4.ScheduleID) ' +
    'WHERE (t1.TabNumID = :TabNumID) AND (t1.DayDate BETWEEN :BD AND :ED) ' +
    'ORDER BY t1.DayDate'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('BD', BD);
  QParamDT('ED', ED);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADates, QFieldDT('DayDate'));
      VAppend(ATotalHours, QFieldInt('TotalHours'));
      VAppend(ANightHours, QFieldInt('NightHours'));
      VAppend(AOverHours, QFieldInt('OverHours'));
      VAppend(ASkipHours, QFieldInt('SkipHours'));
      VAppend(ASchedHours, QFieldInt('SchedHours'));
      VAppend(AMainMarks, QFieldInt('MainMarkDig'));
      VAppend(ASkipMarks, QFieldInt('SkipMarkDig'));
      VAppend(ASchedIDs, QFieldInt('SchedID'));
      VAppend(AShiftNums, QFieldInt('ShiftNum'));
      VAppend(AScheduleNames, QFieldStr('ScheduleName'));

      X:= VLast(ASkipHours);
      if X=FULLSHIFT_SKIPHOURS then
        X:= VLast(ASchedHours);
      S:= TimetableDataToDayStr(QFieldStr('MainMarkStr'), QFieldStr('SkipMarkStr'),
                                STRMARK_NIGHT, STRMARK_OVER,
                                VLast(ATotalHours), VLast(ANightHours),
                                VLast(AOverHours), X);
      VAppend(ATimetableStrings, S);

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.TimetableDataVectorsLoad(const ATabNumID: Integer;
                               const ABeginDate, AEndDate: TDate; //период запроса
                               out ADates: TDateVector;
                               out ASheduleIDs, AShiftNums,
                                 ATotalHours, ANightHours, AOverHours,
                                 ASkipHours, ASchedHours, AMainMarkDig,
                                 ASkipMarkDig, AIsManualChanged, AIsAbsence: TIntVector;
                               out AMainMarkStr, ASkipMarkStr: TStrVector): Boolean;
begin
  Result:= False;

  ADates:= nil;
  ASheduleIDs:= nil;
  AShiftNums:= nil;
  ATotalHours:= nil;
  ANightHours:= nil;
  AOverHours:= nil;
  ASkipHours:= nil;
  ASchedHours:= nil;
  AMainMarkDig:= nil;
  ASkipMarkDig:= nil;
  AMainMarkStr:= nil;
  ASkipMarkStr:= nil;
  AIsManualChanged:= nil;
  AIsAbsence:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.DayDate, t1.TotalHours, t1.NightHours, t1.OverHours, t1.SkipHours, ' +
           't1.SchedHours, t1.SchedID, t1.ShiftNum, '+
           't1.DigMark AS MainMarkDig, t1.SkipMark AS SkipMarkDig, '+
           't2.StrMark AS MainMarkStr, t3.StrMark AS SkipMarkStr, t2.TypeMark ' +
    'FROM TIMETABLELOG t1 ' +
    'INNER JOIN TIMETABLEMARK t2 ON (t1.DigMark=t2.DigMark) ' +
    'INNER JOIN TIMETABLEMARK t3 ON (t1.SkipMark=t3.DigMark) ' +
    'WHERE (t1.TabNumID = :TabNumID) AND (t1.DayDate BETWEEN :BD AND :ED) ' +
    'ORDER BY t1.DayDate'
  );
  QParamInt('TabNumID', ATabNumID);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADates, QFieldDT('DayDate'));
      VAppend(AIsAbsence, TimetableIsAbsence(QFieldInt('TypeMark')));
      VAppend(ASheduleIDs, QFieldInt('SchedID'));
      VAppend(AIsManualChanged, TimetableIsManualChanged(VLast(ASheduleIDs)));
      VAppend(AShiftNums, QFieldInt('ShiftNum'));
      VAppend(ATotalHours, QFieldInt('TotalHours'));
      VAppend(ANightHours, QFieldInt('NightHours'));
      VAppend(AOverHours, QFieldInt('OverHours'));
      VAppend(ASkipHours, QFieldInt('SkipHours'));
      VAppend(ASchedHours, QFieldInt('SchedHours'));
      VAppend(AMainMarkDig, QFieldInt('MainMarkDig'));
      VAppend(ASkipMarkDig, QFieldInt('SkipMarkDig'));
      VAppend(AMainMarkStr, QFieldStr('MainMarkStr'));
      VAppend(ASkipMarkStr, QFieldStr('SkipMarkStr'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

procedure TDataBase.TimetableDataTotalsLoad(const ATabNumID: Integer;
                               const ABeginDate, AEndDate: TDate; //период запроса
                               out AShiftCount,               //отработано смен
                                 AWorkDaysCount,              //отработано дней
                                 ANotWorkDaysCount,           //выходных, праздничных дней
                                 ATotalHours,                 //отработано часов всего
                                 ANightHours,                 //из них ночных
                                 AOverHours,                  //из них сверхурочных
                                 AHolidayHours,               //из них праздничных (выходных)
                                 ASkipDaysCount,              //пропущено дней
                                 ASkipHours: Integer;         //пропущено часов
                               out ASkipMarksStr,             //перечень кодов отсутствия
                                 ASkipDaysHoursStr:           //перечень дней (часов) отсутствия
                               String);

  procedure SetSQLParams;
  begin
    QParamInt('TabNumID', ATabNumID);
    QParamDT('BD', ABeginDate);
    QParamDT('ED', AEndDate);
  end;

  procedure GetShiftVectors(out AScheduleIDs, AShiftNums: TIntVector);
  begin
    AScheduleIDs:= nil;
    AShiftNums:= nil;
    QSetSQL(
      'SELECT SchedID, ShiftNum ' +
      'FROM TIMETABLELOG ' +
      'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) ' +
      'ORDER BY DayDate'
    );
    SetSQLParams;
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(AScheduleIDs, QFieldInt('SchedID'));
        VAppend(AShiftNums, QFieldInt('ShiftNum'));
        QNext;
      end;
    end;
    QClose;
  end;

  procedure GetShiftCount;
  var
    ScheduleIDs, ShiftNums: TIntVector;
  begin
    GetShiftVectors(ScheduleIDs, ShiftNums);
    if VIsNil(ScheduleIDs) then Exit;
    AShiftCount:= TTimetable.CalcShiftCount(ScheduleIDs, ShiftNums, 0, High(ScheduleIDs));
  end;

  procedure GetWorkDaysAndTotalNightOverHours;
  begin
    QSetSQL(
      'SELECT COUNT(TotalHours) AS AWorkDaysCount, SUM(TotalHours) AS ATotalHours, '+
             'SUM(NightHours) AS ANightHours, SUM(OverHours) AS AOverHours '+
      'FROM TIMETABLELOG ' +
      'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND (TotalHours>0)'
    );
    SetSQLParams;
    QOpen;
    if not QIsEmpty then
    begin
      AWorkDaysCount:= QFieldInt('AWorkDaysCount');
      ATotalHours:= QFieldInt('ATotalHours');
      ANightHours:= QFieldInt('ANightHours');
      AOverHours:= QFieldInt('AOverHours');
    end;
    QClose;
  end;

  procedure GetNotWorkDaysCount;
  begin
    QSetSQL(
      'SELECT COUNT(TotalHours) AS ANotWorkDaysCount ' +
      'FROM TIMETABLELOG '+
      'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND ((TotalHours=0) AND (SkipMark=0))'
    );
    SetSQLParams;
    QOpen;
    if not QIsEmpty then
      ANotWorkDaysCount:= QFieldInt('ANotWorkDaysCount');
    QClose;
  end;

  procedure GetHolidayHours;
  begin
    QSetSQL(
      'SELECT SUM(TotalHours) AS AHolidayHours ' +
      'FROM TIMETABLELOG '+
      'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND (DigMark=3)' {3=PB}
    );
    SetSQLParams;
    QOpen;
    if not QIsEmpty then
      AHolidayHours:= QFieldInt('AHolidayHours');
    QClose;
  end;

  procedure GetSkipDaysCount(out D: Integer; const ASkipMark: Integer = -1);
  var
    S: String;
  begin
    D:= 0;
    S:= 'SELECT COUNT(SkipMark) AS ASkipDaysCount ' +
        'FROM TIMETABLELOG '+
        'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND (SkipMark';
    if ASkipMark=-1 then
      S:= S+'<>0)'
    else
      S:= S+'= :SkipMark)';
    QSetSQL(S);
    SetSQLParams;
    QParamInt('SkipMark', ASkipMark);
    QOpen;
    if not QIsEmpty then
      D:= QFieldInt('ASkipDaysCount');
    QClose;
  end;

  procedure GetSkipHours(out H: Integer; const ASkipMark: Integer = -1);
  var
    S: String;
  begin
    H:= 0;
    S:= 'SELECT SUM(SkipHours) AS ASkipHours ' +
        'FROM TIMETABLELOG '+
        'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND (SkipHours>0)';
    if ASkipMark>=0 then
      S:= S + ' AND (SkipMark = :SkipMark)';
    QSetSQL(S);
    SetSQLParams;
    QParamInt('SkipMark', ASkipMark);
    QOpen;
    if not QIsEmpty then
      H:= H + QFieldInt('ASkipHours');
    QClose;
    S:= 'SELECT SUM(SchedHours) AS ASkipHours ' +
        'FROM TIMETABLELOG '+
        'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND (SkipHours= :SkipHours)';
    if ASkipMark>=0 then
      S:= S + ' AND (SkipMark = :SkipMark)';
    QSetSQL(S);
    SetSQLParams;
    QParamInt('SkipMark', ASkipMark);
    QParamInt('SkipHours', FULLSHIFT_SKIPHOURS);
    QOpen;
    if not QIsEmpty then
      H:= H + QFieldInt('ASkipHours');
    QClose;
  end;

  procedure GetSkipMarksDaysHoursStr;
  var
    SkipMarksDig: TIntVector;
    D,H,i: Integer;
  begin
    SkipMarksDig:= nil;
    //получаем список из уникальных кодов неявок
    QSetSQL(
      'SELECT DISTINCT t1.SkipMark, t2.StrMark ' +
      'FROM TIMETABLELOG t1 '+
      'INNER JOIN TIMETABLEMARK t2 ON (t1.SkipMark=t2.DigMark) '  +
      'WHERE (TabNumID = :TabNumID) AND (DayDate BETWEEN :BD AND :ED) AND (SkipMark>0)');
    SetSQLParams;
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        ASkipMarksStr:= ASkipMarksStr + SYMBOL_BREAK + QFieldStr('StrMark');
        VAppend(SkipMarksDig, QFieldInt('SkipMark'));
        QNext;
      end;
    end;
    QClose;
    ASkipMarksStr:= STrim(ASkipMarksStr);
    for i:=0 to High(SkipMarksDig) do
    begin
      GetSkipDaysCount(D, SkipMarksDig[i]);
      GetSkipHours(H, SkipMarksDig[i]);
      ASkipDaysHoursStr:= ASkipDaysHoursStr + SYMBOL_BREAK +
                            IntToStr(D) + ' (' + WorkHoursIntToFracStr(H) + ')';
    end;
    ASkipDaysHoursStr:= STrim(ASkipDaysHoursStr);
  end;

begin
  AShiftCount:= 0;
  AWorkDaysCount:= 0;
  ATotalHours:= 0;
  ANightHours:= 0;
  AOverHours:= 0;
  ANotWorkDaysCount:= 0;
  AHolidayHours:= 0;
  ASkipDaysCount:= 0;
  ASkipHours:= 0;
  ASkipMarksStr:= EmptyStr;
  ASkipDaysHoursStr:= EmptyStr;

  QSetQuery(FQuery);

  GetShiftCount;
  GetWorkDaysAndTotalNightOverHours;
  GetNotWorkDaysCount;
  GetHolidayHours;
  GetSkipDaysCount(ASkipDaysCount);
  GetSkipHours(ASkipHours);
  GetSkipMarksDaysHoursStr;
end;

function TDataBase.SIZNormsLoad(out ANormIDs: TIntVector;
                             out ANormNames, ANotes: TStrVector;
                             out ABeginDates, AEndDates: TDateVector): Boolean;
begin
  Result:= False;

  ANormIDs:= nil;
  ANormNames:= nil;
  ANotes:= nil;
  ABeginDates:= nil;
  AEndDates:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('SIZNORM', ['NormID', 'NormName', 'BeginDate', 'EndDate', 'Note']) +
    'WHERE NormID>0 ' +
    'ORDER BY BeginDate DESC'
  );
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ANormIDs, QFieldInt('NormID'));
      VAppend(ANormNames, QFieldStr('NormName'));
      VAppend(ANotes, QFieldStr('Note'));
      VAppend(ABeginDates, QFieldDT('BeginDate'));
      VAppend(AEndDates, QFieldDT('EndDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs, APostIDs, AOrderNums: TIntVector;
                              out APostNames: TStrVector): Boolean;
begin
  Result:= False;

  AItemIDs:= nil;
  APostIDs:= nil;
  AOrderNums:= nil;
  APostNames:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ItemID, t1.PostID, t2.OrderNum, t3.PostName ' +
    'FROM SIZNORMITEMPOST t1 ' +
    'INNER JOIN SIZNORMITEM t2 ON (t1.ItemID=t2.ItemID) ' +
    'INNER JOIN STAFFPOST t3 ON (t1.PostID=t3.PostID) ' +
    'WHERE t2.NormID = :NormID ' +
    'ORDER BY t2.OrderNum'
  );
  QParamInt('NormID', ANormID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AItemIDs, QFieldInt('ItemID'));
      VAppend(APostIDs, QFieldInt('PostID'));
      VAppend(AOrderNums, QFieldInt('OrderNum'));
      VAppend(APostNames, QFieldStr('PostName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs, AOrderNums: TIntVector;
                              out APostIDs: TIntMatrix;
                              out APostNames: TStrMatrix): Boolean;
var
  i, I1, I2, ItemID: Integer;
  ItemIDs, PostIDs, OrderNums: TIntVector;
  PostNames: TStrVector;
begin
  AItemIDs:= nil;
  AOrderNums:= nil;
  APostIDs:= nil;
  APostNames:= nil;

  Result:= SIZNormItemsLoad(ANormID, ItemIDs, PostIDs, OrderNums, PostNames);
  if not Result then Exit;

  I1:= 0;
  ItemID:= ItemIDs[0];
  for i:= 1 to High(ItemIDs) do
  begin
    if ItemIDs[i]=ItemID then continue;

    VAppend(AItemIDs, ItemIDs[i-1]);
    VAppend(AOrderNums, OrderNums[i-1]);

    I2:= i-1;
    MAppend(APostIDs, VCut(PostIDs, I1, I2));
    MAppend(APostNames, VCut(PostNames, I1, I2));

    ItemID:= ItemIDs[i];
    I1:= i;
  end;

  VAppend(AItemIDs, VLast(ItemIDs));
  VAppend(AOrderNums, VLast(OrderNums));
  I2:= High(ItemIDs);
  MAppend(APostIDs, VCut(PostIDs, I1, I2));
  MAppend(APostNames, VCut(PostNames, I1, I2));
end;

function TDataBase.SIZNormItemLoad(const AItemID: Integer; out AItem: TNormItem): Boolean;
begin
  Result:= False;
  NormItemClear(AItem{%H-});
  if AItemID<=0 then Exit;
  AItem.ItemID:= AItemID;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ItemID, t1.PostID, t2.OrderNum, t3.PostName ' +
    'FROM SIZNORMITEMPOST t1 ' +
    'INNER JOIN SIZNORMITEM t2 ON (t1.ItemID=t2.ItemID) ' +
    'INNER JOIN STAFFPOST t3 ON (t1.PostID=t3.PostID) ' +
    'WHERE t1.ItemID = :ItemID '
  );
  QParamInt('ItemID', AItemID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    AItem.OrderNum:= QFieldInt('OrderNum');
    while not QEOF do
    begin
      VAppend(AItem.PostIDs, QFieldInt('PostID'));
      VAppend(AItem.PostNames, QFieldStr('PostName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  SIZNormSubItemsLoad(AItemID, AItem.SubItems);
end;

function TDataBase.SIZItemsAndPostsAccordanceLoad(const ANormID: Integer;
                              out APostIDs, AItemIDs, AItemPostIDs: TIntVector): Boolean;
begin
  Result:= False;
  APostIDs:= nil;
  AItemIDs:= nil;
  AItemPostIDs:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ItemID, t1.PostID, t1.ItemPostID  ' +
    'FROM SIZNORMITEMPOST t1 ' +
    'INNER JOIN SIZNORMITEM t2 ON (t1.ItemID = t2.ItemID) ' +
    'WHERE t2.NormID = :NormID'
  );
  QParamInt('NormID', ANormID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AItemPostIDs, QFieldInt('ItemPostID'));
      VAppend(APostIDs, QFieldInt('PostID'));
      VAppend(AItemIDs, QFieldInt('ItemID'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZItemsAndPostsAccordanceAdd(const AItemID: Integer;
  const APostIDs: TIntVector; const ACommit: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись нормы
    QSetSQL(
      sqlINSERT('SIZNORMITEMPOST', ['ItemID', 'PostID'])
    );
    QParamInt('ItemID', AItemID);
    for i:= 0 to High(APostIDs) do
    begin
      QParamInt('PostID', APostIDs[i]);
      QExec;
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZItemsAndPostsAccordanceMove(const AItemID: Integer;
  const AItemPostIDs: TIntVector; const ACommit: Boolean): Boolean;
begin
  Result:= False;
  if VIsNil(AItemPostIDs) then Exit;
  Result:= UpdateByInt32ID('SIZNORMITEMPOST', 'ItemID', 'ItemPostID',
                         AItemPostIDs, AItemID, ACommit);
end;

function TDataBase.SIZNormItemWrite(const ANormID: Integer; out AItemID: Integer;
                              const AOrderNum: Integer;
                              const ACommit: Boolean = True): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись пункта нормы
    QSetSQL(
      sqlINSERT('SIZNORMITEM', ['OrderNum', 'NormID'])
    );
    QParamInt('NormID', ANormID);
    QParamInt('OrderNum', AOrderNum);
    QExec;
    //получение ID сделанной записи
    AItemID:= LastWritedInt32ID('SIZNORMITEM');
    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormItemAdd(const ANormID: Integer; out AItemID: Integer;
                                  const APostIDs: TIntVector): Boolean;
var
  OrderNum: Integer;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //Определяем свободный порядковый номер
    OrderNum:= SIZNormItemOrderNumFreeLoad(ANormID);
    //записываем пункт нормы в SIZNORMITEM
    SIZNormItemWrite(ANormID, AItemID, OrderNum, False{no commit});
    //записываем все выбранные должности для пункта в SIZNORMITEMPOST
    SIZItemsAndPostsAccordanceAdd(AItemID, APostIDs, False{no commit});
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemUpdate(const ANormID, AItemID: Integer;
                                     const AAddPostIDs, ADelItemPostIDs: TIntVector): Boolean;
begin
  Result:= False;
  try
    //переносим удалямые записи в новый пункт
    if not VIsNil(ADelItemPostIDs) then
      SIZNormItemPartCopy(ANormID, AItemID, ADelItemPostIDs, False{no commit});

    //записываем все выбранные должности для пункта в SIZNORMITEMPOST
    if not VIsNil(AAddPostIDs) then
      SIZItemsAndPostsAccordanceAdd(AItemID, AAddPostIDs, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemDataCopy(const ANormID, ASourceItemID: Integer;
                                       out ADestItemID: Integer;
                                       const ACommit: Boolean): Boolean;
var
  i, DestSubItemID, ItemOrderNum: Integer;
  SourceSubItemIDs, ReasonIDs, SubItemOrderNums, DestInfoIDs: TIntVector;
  Info: TNormSubItemInfo;

  procedure SourceItemLoad(const AItemID: Integer;
                               out ASubItemIDs, AReasonIDs, AOrderNums: TIntVector);
  begin
    ASubItemIDs:= nil;
    AReasonIDs:= nil;
    AOrderNums:= nil;
    QSetQuery(FQuery);
    QSetSQL(
      'SELECT SubItemID, ReasonID, OrderNum '+
      'FROM SIZNORMSUBITEM ' +
      'WHERE ItemID = :ItemID ' +
      'ORDER BY OrderNum');
    QParamInt('ItemID', AItemID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(ASubItemIDs, QFieldInt('SubItemID'));
        VAppend(AReasonIDs, QFieldInt('ReasonID'));
        VAppend(AOrderNums, QFieldInt('OrderNum'));
        QNext;
      end;
    end;
    QClose;
  end;

begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //Определяем свободный порядковый номер
    ItemOrderNum:= SIZNormItemOrderNumFreeLoad(ANormID);
    //записываем пункт
    SIZNormItemWrite(ANormID, ADestItemID, ItemOrderNum, False{no commit});

    //достаем список строк пункта-источника
    SourceItemLoad(ASourceItemID, SourceSubItemIDs, ReasonIDs, SubItemOrderNums);

    //пробегаем по всем строкам пункта-источника
    for i:=0 to High(SourceSubItemIDs) do
    begin
      //достаем спиcок подстрок для строки-источника
      SIZNormSubItemInfoLoad(SourceSubItemIDs[i], Info{%H-});
      //записываем строку пункта-цели в SIZNORMSUBITEM
      SIZNormSubItemWrite(ADestItemID, DestSubItemID, ReasonIDs[i],
                          SubItemOrderNums[i], False{no commit});
      //записываем инфо строки-цели SIZNORMSUBITEMINFO и получаем их ID
      SIZNormSubItemInfoWrite(DestSubItemID, Info, DestInfoIDs, False{no commit});
      //копируем спец размеры СИЗ
      SIZSpecSizeCopy(Info.InfoIDs, DestInfoIDs, False{no commit});
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormItemPartCopy(const ANormID, ASourceItemID: Integer;
  const AItemPostIDs: TIntVector; const ACommit: Boolean): Boolean;
var
  DestItemID: Integer;
begin
  Result:= False;
  try
    if SIZNormItemDataCopy(ANormID, ASourceItemID, DestItemID, False{no commit}) then
    begin
      //переносим все выбранные должности для пункта в SIZNORMITEMPOST
      SIZItemsAndPostsAccordanceMove(DestItemID, AItemPostIDs, False{no commit});
      if ACommit then QCommit;
      Result:= True;
    end;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormItemCopy(const ANormID, AItemID: Integer;
                                   const APostIDs: TIntVector): Boolean;
var
  DestItemID: Integer;
begin
  Result:= False;
  try
    if SIZNormItemDataCopy(ANormID, AItemID, DestItemID, False{no commit}) then
    begin
      //записываем все выбранные должности для пункта в SIZNORMITEMPOST
      SIZItemsAndPostsAccordanceAdd(DestItemID, APostIDs, False{no commit});
      QCommit;
      Result:= True;
    end;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemSwap(const AItemID1, AOrderNum1, AItemID2,
  AOrderNum2: Integer): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZNORMITEM', ['OrderNum']) +
      'WHERE ItemID = :ItemID'
    );
    QParamInt('ItemID', AItemID1);
    QParamInt('OrderNum', AOrderNum2);
    QExec;
    QParamInt('ItemID', AItemID2);
    QParamInt('OrderNum', AOrderNum1);
    QExec;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormSubItemInfoLoad(const ASubItemID: Integer;
                                          var AInfo: TNormSubItemInfo): Boolean;
begin
  Result:= False;
  NormSubItemInfoClear(AInfo);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.InfoID, t1.NameID, t1.Num, t1.Life, t1.ClauseName, t1.OrderNum, ' +
           't2.SIZName, t2.SizeType, t2.SIZType, ' +
           't3.UnitStringCode ' +
    'FROM SIZNORMSUBITEMINFO t1 ' +
    'INNER JOIN SIZNAME t2 ON (t1.NameID=t2.NameID) ' +
    'INNER JOIN SIZUNIT t3 ON (t2.UnitID=t3.UnitID) ' +
    'WHERE t1.SubItemID = :SubItemID ' +
    'ORDER BY t1.OrderNum');
  QParamInt('SubItemID', ASubItemID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      NormSubItemInfoAdd(AInfo, QFieldInt('InfoID'), QFieldInt('OrderNum'),
                                QFieldInt('SIZType'), QFieldInt('NameID'),
                                QFieldInt('SizeType'), QFieldInt('Num'),
                                QFieldInt('Life'), QFieldStr('SIZName'),
                                QFieldStr('UnitStringCode'), QFieldStr('ClauseName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormSubItemsDataLoad(const AItemID: Integer;
             out ASubItemIDs, ASubItemOrderNums, AReasonIDs,
                 AInfoIDs, AInfoOrderNums,
                 ASIZTypes, ANameIDs, ASizeTypes,
                 ANums, ALifes: TIntVector;
             out AReasonNames, ASizNames, AUnits, AClauseNames: TStrVector): Boolean;
begin
  Result:= False;

  ASubItemIDs:= nil;
  ASubItemOrderNums:= nil;
  AReasonIDs:= nil;
  AReasonNames:= nil;

  AInfoIDs:= nil;
  AInfoOrderNums:= nil;

  ASIZTypes:= nil;
  ANameIDs:= nil;
  ASizNames:= nil;
  AUnits:= nil;
  ASizeTypes:= nil;

  ANums:= nil;
  ALifes:= nil;
  AClauseNames:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.SubItemID, t1.Num, t1.Life, t1.InfoID, t1.ClauseName, ' +
           't1.OrderNum AS InfoOrderNum, t1.NameID, ' +
           't2.ReasonID, t2.OrderNum AS SubItemOrderNum, ' +
           't3.SizName, t3.SizeType, t3.SIZType, ' +
           't4.ReasonName, ' +
           't5.UnitStringCode ' +
    'FROM SIZNORMSUBITEMINFO t1 ' +
    'INNER JOIN SIZNORMSUBITEM t2 ON (t1.SubItemID=t2.SubItemID) ' +
    'INNER JOIN SIZNAME t3 ON (t1.NameID=t3.NameID) ' +
    'INNER JOIN SIZREASON t4 ON (t2.ReasonID=t4.ReasonID) ' +
    'INNER JOIN SIZUNIT t5 ON (t3.UnitID=t5.UnitID) ' +
    'WHERE t2.ItemID = :ItemID ' +
    'ORDER BY t2.ReasonID, t2.OrderNum, t1.OrderNum');
  QParamInt('ItemID', AItemID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ASubItemIDs, QFieldInt('SubItemID'));
      VAppend(ASubItemOrderNums, QFieldInt('SubItemOrderNum'));
      VAppend(AReasonIDs, QFieldInt('ReasonID'));
      VAppend(AReasonNames, QFieldStr('ReasonName'));

      VAppend(AInfoIDs, QFieldInt('InfoID'));
      VAppend(AInfoOrderNums, QFieldInt('InfoOrderNum'));

      VAppend(ASIZTypes, QFieldInt('SIZType'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(AUnits, QFieldStr('UnitStringCode'));
      VAppend(ASizeTypes, QFieldInt('SizeType'));

      VAppend(ANums, QFieldInt('Num'));
      VAppend(ALifes, QFieldInt('Life'));

      VAppend(AClauseNames, QFieldStr('ClauseName'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormSubItemOrderNumFreeLoad(const AItemID, AReasonID: Integer): Integer;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT COUNT(SubItemID) AS FreeOrderNum ' +
    'FROM SIZNORMSUBITEM ' +
    'WHERE (ItemID=:ItemID) AND (ReasonID=:ReasonID)'
  );
  QParamInt('ItemID', AItemID);
  QParamInt('ReasonID', AReasonID);
  QOpen;
  QFirst;
  Result:= QFieldInt('FreeOrderNum');
  QClose;
end;

function TDataBase.SIZNormSubItemOrderNumDecrement(const AItemID, AReasonID,
  ABeginOrderNum: Integer): Boolean;
var
  SubItemIDs, OrderNums: TIntvector;
  i: Integer;
begin
  Result:= False;
  SubItemIDs:= nil;
  OrderNums:= nil;

  //определяем строки, лежащие ниже по порядку, чем ABeginOrderNum
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT SubItemID, OrderNum ' +
    'FROM SIZNORMSUBITEM ' +
    'WHERE (ItemID=:ItemID) AND (ReasonID=:ReasonID) AND (OrderNum>:BeginOrderNum) ' +
    'ORDER BY OrderNum'
  );
  QParamInt('ItemID', AItemID);
  QParamInt('ReasonID', AReasonID);
  QParamInt('BeginOrderNum', ABeginOrderNum);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(SubItemIDs, QFieldInt('SubItemID'));
      VAppend(OrderNums, QFieldInt('OrderNum'));
      QNext;
    end;
  end;
  QClose;

  if VIsNil(SubItemIDs) then Exit;

  //сдвигаем порядковые номера
  QSetSQL(
    'UPDATE SIZNORMSUBITEM ' +
    'SET OrderNum=:OrderNum ' +
    'WHERE SubItemID=:SubItemID'
  );
  for i:=0 to High(SubItemIDs) do
  begin
    QParamInt('OrderNum', OrderNums[i] - 1);
    QParamInt('SubItemID', SubItemIDs[i]);
    QExec;
  end;

  Result:= True;
end;

function TDataBase.SIZNormSubItemsLoad(const AItemID: Integer;
                                       var ASubItems: TNormSubItems): Boolean;
var
  SubItemIDs, SubItemOrderNums, ReasonIDs: TIntVector;
  InfoIDs, InfoOrderNums: TIntVector;
  SIZTypes, NameIDs, SizeTypes: TIntVector;
  Nums, Lifes: TIntVector;
  ReasonNames, SizNames, Units, ClauseNames: TStrVector;

  i, SubItemID: Integer;

  SubItem: TNormSubItem;
  Info: TNormSubItemInfo;
begin
  NormSubItemsClear(ASubItems);
  Result:= SIZNormSubItemsDataLoad(AItemID, SubItemIDs, SubItemOrderNums, ReasonIDs,
                    InfoIDs, InfoOrderNums, SIZTypes, NameIDs, SizeTypes, Nums,
                    Lifes, ReasonNames, SizNames, Units, ClauseNames);
  if not Result then Exit;
  SubItemID:= -1;
  for i:=0 to High(SubItemIDs) do
  begin
    //новая строка нормы
    if SubItemIDs[i]<>SubItemID then
    begin
      if i>0 then
      begin
        //записываем Info в строку
        NormSubItemInfoCopy(Info{%H-}, {%H-}SubItem.Info);
        //записываем сформированную строку в вектор
        NormSubItemsAdd(ASubItems, SubItem{%H-});
      end;
      //задаем новую строку нормы
      NormSubItemNew(SubItem, SubItemIDs[i], SubItemOrderNums[i], ReasonIDs[i], ReasonNames[i]);
      //запоминаем её ID
      SubItemID:= SubItemIDs[i];
      //задаем новое Info для этой строки
      NormSubItemInfoClear(Info);
    end;
    //заполняем Info
    NormSubItemInfoAdd(Info, InfoIDs[i], InfoOrderNums[i],
                       SIZTypes[i], NameIDs[i], SizeTypes[i], Nums[i],
                       Lifes[i], SizNames[i], Units[i], ClauseNames[i]);

  end;
  //записываем последнюю сформированную строку в вектор
  NormSubItemInfoCopy(Info, SubItem.Info);
  NormSubItemsAdd(ASubItems, SubItem);
end;

function TDataBase.SIZNormSubItemWrite(const AItemID: Integer;
                                 out ASubItemID: Integer;
                                 const AReasonID, AOrderNum: Integer;
                                 const ACommit: Boolean = True): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись пункта нормы
    QSetSQL(
      sqlINSERT('SIZNORMSUBITEM', ['ItemID', 'ReasonID', 'OrderNum'])
    );
    QParamInt('ItemID', AItemID);
    QParamInt('ReasonID', AReasonID);
    QParamInt('OrderNum', AOrderNum);
    QExec;
    //получение ID сделанной записи
    ASubItemID:= LastWritedInt32ID('SIZNORMSUBITEM');
    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormSubItemAdd(const AItemID: Integer;
  var ASubItem: TNormSubItem): Boolean;
var
  V: TIntVector;
begin
  QSetQuery(FQuery);
  try
    //записываем данные строки в SIZNORMSUBITEM
    SIZNormSubItemWrite(AItemID, ASubItem.SubItemID, ASubItem.ReasonID,
                        ASubItem.OrderNum, False{no commit});

    //записываем инфо строки в SIZNORMSUBITEMINFO
    SIZNormSubItemInfoWrite(ASubItem.SubItemID, ASubItem.Info, V, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormSubItemSwap(const ASubItemID1, AOrderNum1,
                                  ASubItemID2, AOrderNum2: Integer): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZNORMSUBITEM', ['OrderNum']) +
      'WHERE SubItemID = :SubItemID'
    );
    QParamInt('SubItemID', ASubItemID1);
    QParamInt('OrderNum', AOrderNum2);
    QExec;
    QParamInt('SubItemID', ASubItemID2);
    QParamInt('OrderNum', AOrderNum1);
    QExec;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormSubItemUpdate(const ASubItemID: Integer;
                                 const AReasonID, AOrderNum: Integer;
                                 const ACommit: Boolean = True): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZNORMSUBITEM', ['ReasonID', 'OrderNum']) +
      'WHERE SubItemID = :SubItemID'
    );
    QParamInt('SubItemID', ASubItemID);
    QParamInt('ReasonID', AReasonID);
    QParamInt('OrderNum', AOrderNum);
    QExec;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormSubItemUpdate(const AItemID: Integer;
  const ANewSubItem, AOldSubItem: TNormSubItem): Boolean;
var
  i, n: Integer;
  DeleteIDs, Indexes: TIntVector;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //изменилось доп условие (и => порядковый номер)
    if ANewSubItem.ReasonID<>AOldSubItem.ReasonID then
    begin
      //записываем новые значения в SIZNORMSUBITEM
      SIZNormSubItemUpdate(ANewSubItem.SubItemID, ANewSubItem.ReasonID,
                           ANewSubItem.OrderNum, False{no commit});
      //сдвигаем вверх порядковые номера для старого доп условия
      SIZNormSubItemOrderNumDecrement(AItemID, AOldSubItem.ReasonID,
                           AOldSubItem.OrderNum);
    end;

    //определяем InfoID для удаления и индексы подстрок для обновления
    DeleteIDs:= nil;
    Indexes:= nil;
    for i:= 0 to High(AOldSubItem.Info.InfoIDs) do
    begin
      n:= VIndexOf(ANewSubItem.Info.InfoIDs, AOldSubItem.Info.InfoIDs[i]);
      if n<0 then //этой подстроки больше нет, запоминаем ID для удаления
        VAppend(DeleteIDs, AOldSubItem.Info.InfoIDs[i])
      else begin //эта подстрока осталась, тогда если изменились значения, запоминаем индекс
        if (ANewSubItem.Info.NameIDs[n]<>AOldSubItem.Info.NameIDs[i]) or
           (ANewSubItem.Info.Nums[n]<>AOldSubItem.Info.Nums[i]) or
           (ANewSubItem.Info.ClauseNames[n]<>AOldSubItem.Info.ClauseNames[i]) or
           (ANewSubItem.Info.Lifes[n]<>AOldSubItem.Info.Lifes[i]) or
           (ANewSubItem.Info.OrderNums[n]<>AOldSubItem.Info.OrderNums[i]) then
           VAppend(Indexes, n);
      end;
    end;

    //записываем освободившиеся InfoID в новые подстроки
    if not VIsNil(DeleteIDs) then
    begin
      for i:= 0 to High(ANewSubItem.Info.InfoIDs) do
      begin
        if ANewSubItem.Info.InfoIDs[i]=-1 then
        begin
          //запимываем InfoID
          ANewSubItem.Info.InfoIDs[i]:= VLast(DeleteIDs);
          //сохраняем индекс для обновления значений подстроки
          VAppend(Indexes, i);
          //удаляем вновь занятый InfoID
          VDel(DeleteIDs, High(DeleteIDs));
          //завершение, если незанятые InfoID закончились
          if VIsNil(DeleteIDs) then break;
        end;
      end;
    end;

    //удаляем отсутствующие InfoID
    SIZNormSubItemInfoDelete(DeleteIDs, False{no commit});
    //обновляем изменившиеся значения
    for i:= 0 to High(Indexes) do
      SIZNormSubItemInfoUpdate(ANewSubItem.Info.InfoIDs[Indexes[i]],
                               ANewSubItem.Info.NameIDs[Indexes[i]],
                               ANewSubItem.Info.Nums[Indexes[i]],
                               ANewSubItem.Info.Lifes[Indexes[i]],
                               ANewSubItem.Info.OrderNums[Indexes[i]],
                               ANewSubItem.Info.ClauseNames[Indexes[i]],
                               False{no commit});

    //записываем новые подстроки
    for i:= 0 to High(ANewSubItem.Info.InfoIDs) do
      if ANewSubItem.Info.InfoIDs[i]=-1 then
        SIZNormSubItemInfoWrite(ANewSubItem.SubItemID,
                               ANewSubItem.Info.NameIDs[i],
                               ANewSubItem.Info.Nums[i],
                               ANewSubItem.Info.Lifes[i],
                               ANewSubItem.Info.OrderNums[i],
                               ANewSubItem.Info.ClauseNames[i],
                               ANewSubItem.Info.InfoIDs[i],
                               False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemIntersectionExists(const APostID, AItemID: Integer;
              const ABeginDate, AEndDate: TDate;
              out ANormName, AOrderNum: String): Boolean;
begin
  Result:= False;
  ANormName:= EmptyStr;
  AOrderNum:= EmptyStr;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t3.NormName, t2.OrderNum ' +
    'FROM SIZNORMITEMPOST t1 ' +
    'INNER JOIN SIZNORMITEM t2 ON (t1.ItemID=t2.ItemID) ' +
    'INNER JOIN SIZNORM t3 ON (t2.NormID=t3.NormID) ' +
    'WHERE (t1.ItemID<>:ItemID) AND (t1.PostID=:PostID) AND (' +
            SqlCROSS('t3.BeginDate', 't3.EndDate', ':BD', ':ED') + ')'
  );
  QParamInt('ItemID', AItemID);
  QParamInt('PostID', APostID);
  QParamDT('BD', ABeginDate);
  QParamDT('ED', AEndDate);
  QOpen;
  if not QIsEmpty then
  begin
    Result:= True;
    ANormName:= QFieldStr('NormName');
    AOrderNum:= QFieldStr('OrderNum');
  end;
  QClose;
end;

function TDataBase.SIZNormItemOrderNumFreeLoad(const ANormID: Integer): Integer;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT COUNT(ItemID) AS FreeOrderNum ' +
    'FROM SIZNORMITEM ' +
    'WHERE NormID=:NormID'
  );
  QParamInt('NormID', ANormID);
  QOpen;
  QFirst;
  Result:= QFieldInt('FreeOrderNum');
  QClose;
end;

function TDataBase.SIZNormAdd(out ANormID: Integer;
                          const ANormName, ANote: String;
                          const ABeginDate, AEndDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись нормы
    QSetSQL(
      sqlINSERT('SIZNORM', ['NormName', 'Note', 'BeginDate', 'EndDate'])
    );
    QParamStr('NormName', ANormName);
    QParamStr('Note', ANote);
    QParamDT('BeginDate', ABeginDate);
    QParamDT('EndDate', AEndDate);
    QExec;
    //получение ID сделанной записи
    ANormID:= LastWritedInt32ID('SIZNORM');

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormUpdate(const ANormID: Integer;
                          const ANormName, ANote: String;
                          const ABeginDate, AEndDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZNORM', ['NormName', 'Note', 'BeginDate', 'EndDate']) +
      'WHERE NormID = :NormID'
    );
    QParamInt('NormID', ANormID);
    QParamStr('NormName', ANormName);
    QParamStr('Note', ANote);
    QParamDT('BeginDate', ABeginDate);
    QParamDT('EndDate', AEndDate);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormDelete(const ANormID: Integer): Boolean;
var
  InfoIDs: TIntVector;

  procedure GetInfoIDs;
  begin
    InfoIDs:= nil;
    QSetSQL(
      'SELECT t1.InfoID ' +
      'FROM SIZNORMSUBITEMINFO t1 ' +
      'INNER JOIN SIZNORMSUBITEM t2 ON (t1.SubItemID=t2.SubItemID) ' +
      'INNER JOIN SIZNORMITEM t3 ON (t2.ItemID=t3.ItemID) ' +
      'WHERE t3.NormID = :NormID'
    );
    QParamInt('NormID', ANormID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(InfoIDs, QFieldInt('InfoID'));
        QNext;
      end;
    end;
    QClose;
  end;

begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //определяем список InfoID для этой нормы
    GetInfoIDs;
    //удаляем эти InfoID (с обработкой)
    SIZNormSubItemInfoDelete(InfoIDs, False{no commit});
    //удаляем норму
    Delete('SIZNORM', 'NormID', ANormID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemDelete(const AItemID: Integer): Boolean;
var
  InfoIDs: TIntVector;

  procedure GetInfoIDs;
  begin
    InfoIDs:= nil;
    QSetSQL(
      'SELECT t1.InfoID '+
      'FROM SIZNORMSUBITEMINFO t1 ' +
      'INNER JOIN SIZNORMSUBITEM t2 ON (t1.SubItemID=t2.SubItemID) ' +
      'WHERE t2.ItemID = :ItemID'
    );
    QParamInt('ItemID', AItemID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(InfoIDs, QFieldInt('InfoID'));
        QNext;
      end;
    end;
    QClose;
  end;

begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //определяем список InfoID для этого пункта норм
    GetInfoIDs;
    //удаляем эти InfoID (с обработкой)
    SIZNormSubItemInfoDelete(InfoIDs, False{no commit});
    //удаляем пункт нормы
    Delete('SIZNORMITEM', 'ItemID', AItemID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormSubItemDelete(const AItemID, ASubItemID,
  AReasonID, AOrderNum: Integer): Boolean;
var
  SubItemIDs, InfoIDs, OrderNums: TIntVector;

  procedure GetSubItemsAfter;
  begin
    SubItemIDs:= nil;
    OrderNums:= nil;
    QSetSQL(
      'SELECT SubItemID, OrderNum '+
      'FROM SIZNORMSUBITEM ' +
      'WHERE (ItemID=:ItemID) AND (ReasonID=:ReasonID) AND (OrderNum>:BeginOrderNum) ' +
      'ORDER BY OrderNum'
    );
    QParamInt('ItemID', AItemID);
    QParamInt('ReasonID', AReasonID);
    QParamInt('BeginOrderNum', AOrderNum);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(SubItemIDs, QFieldInt('SubItemID'));
        VAppend(OrderNums, QFieldInt('OrderNum'));
        QNext;
      end;
    end;
    QClose;
  end;

  procedure MoveSubItemsUp;
  var
    i: Integer;
  begin
    if not VIsNil(SubItemIDs) then
    begin
      QSetSQL(
        'UPDATE SIZNORMSUBITEM ' +
        'SET OrderNum=:OrderNum ' +
        'WHERE SubItemID=:SubItemID'
      );
      for i:=0 to High(SubItemIDs) do
      begin
        QParamInt('OrderNum', OrderNums[i]-1);
        QParamInt('SubItemID', SubItemIDs[i]);
        QExec;
      end;
    end;
  end;

begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //определяем строки, лежащие ниже по порядку, чем AOrderNum
    GetSubItemsAfter;
    //сдвигаем вверх порядковые номера строк, лежащих ниже удаляемой
    MoveSubItemsUp;
    //определяем список InfoID для этой строки пункта норм
    InfoIDs:= ValuesInt32ByInt32ID('SIZNORMSUBITEMINFO', 'InfoID', 'SubItemID', ASubItemID);
    //удаляем эти InfoID (с обработкой)
    SIZNormSubItemInfoDelete(InfoIDs, False{no commit});
    //удаляем строку пункта нормы
    Delete('SIZNORMSUBITEM', 'SubItemID', ASubItemID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormSubItemInfoDelete(const AInfoIDs: TIntVector;
  const ACommit: Boolean): Boolean;
var
  i, j: Integer;
  LogIDs: TInt64Vector;
begin
  Result:= False;

  QSetQuery(FQuery);
  try
    for i:= 0 to High(AInfoIDs) do
    begin
      //получаем ID логов выдачи СИЗ по данному InfoID
      LogIDs:= ValuesInt64ByInt32ID('SIZCARDPERSONALLOG', 'LogID', 'ReceivingInfoID', AInfoIDs[i]);
      if VIsNil(LogIDs) then continue; //не было выдачи - идем дальше
      //удаляем информацию о выдаче, возврате и списаниях для этих LogID
      for j:= 0 to High(LogIDs) do
        SIZReceivingCancel(LogIDs[j], False{no commit});
    end;

    //удаляем сами InfoID
    Delete('SIZNORMSUBITEMINFO', 'InfoID', AInfoIDs, False{no commit});

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormSubItemInfoWrite(const ASubItemID, ANameID,
                                   ANum, ALife, AOrderNum: Integer;
                                   const AClauseName: String;
                                   out AInfoID: Integer;
                                   const ACommit: Boolean = True): Boolean;
begin
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('SIZNORMSUBITEMINFO',
               ['SubItemID', 'NameID', 'Num', 'Life', 'ClauseName', 'OrderNum'])
    );
    QParamInt('SubItemID', ASubItemID);
    QParamInt('NameID', ANameID);
    QParamInt('Num', ANum);
    QParamInt('Life', ALife);
    QParamStr('ClauseName', AClauseName);
    QParamInt('OrderNum', AOrderNum);
    QExec;
    //определяем ID записанной подстроки
    AInfoID:= LastWritedInt32ID('SIZNORMSUBITEMINFO');

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormSubItemInfoWrite(const ASubItemID: Integer;
                                    const AInfo: TNormSubItemInfo;
                                    out AInfoIDs: TIntVector;
                                    const ACommit: Boolean = True): Boolean;
var
  i, DestInfoID: Integer;
begin
  AInfoIDs:= nil;
  QSetQuery(FQuery);
  try
    for i:=0 to High(AInfo.NameIDs) do
    begin
      SIZNormSubItemInfoWrite(ASubItemID, AInfo.NameIDs[i],
                              AInfo.Nums[i], AInfo.Lifes[i],
                              AInfo.OrderNums[i], AInfo.ClauseNames[i],
                              DestInfoID, False{no commit});
      //добавляем ID в вектор
      VAppend(AInfoIDs, DestInfoID);
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZNormSubItemInfoUpdate(const AInfoID, ANameID,
                                    ANum, ALife, AOrderNum: Integer;
                                    const AClauseName: String;
                                    const ACommit: Boolean = True): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZNORMSUBITEMINFO',
                ['NameID', 'Num', 'Life', 'ClauseName', 'OrderNum']) +
      'WHERE InfoID = :InfoID'
    );
    QParamInt('InfoID', AInfoID);
    QParamInt('NameID', ANameID);
    QParamInt('Num', ANum);
    QParamInt('Life', ALife);
    QParamStr('ClauseName', AClauseName);
    QParamInt('OrderNum', AOrderNum);
    QExec;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZAssortmentLoad(out ASIZTypes: TIntVector;
                              //out ATypeNames: TStrVector;
                              out ASIZNames, AUnits: TStrMatrix;
                              out ANameIDs, ASizeTypes: TIntMatrix): Boolean;
var
  OldType, NewType: Integer;
  VNames, VUnits: TStrVector;
  VIDs, VSizeTypes: TIntVector;
begin
  Result:= False;

  ASIZTypes:= nil;
  //ATypeNames:= nil;
  ASIZNames:= nil;
  AUnits:= nil;
  ANameIDs:= nil;
  ASizeTypes:= nil;
  VNames:= nil;
  VUnits:= nil;
  VIDs:= nil;
  VSizeTypes:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.NameID, t1.SIZName, t1.SizeType, t1.SIZType, t2.UnitStringCode ' +
    'FROM SIZNAME t1 ' +
    'INNER JOIN SIZUNIT t2 ON (t1.UnitID=t2.UnitID) ' +
    'ORDER BY t1.SIZType, t1.SIZName'
  );
  QOpen;
  if not QIsEmpty then
  begin
    OldType:= -1;
    QFirst;
    while not QEOF do
    begin
      NewType:= QFieldInt('SIZType');
      if NewType<>OldType then //новый класс
      begin
        //записываем предыдущий тип в матрицы
        if not VIsNil(VNames) then
        begin
          MAppend(ASIZNames, VNames);
          MAppend(AUnits, VUnits);
          MAppend(ANameIDs, VIDs);
          MAppend(ASizeTypes, VSizeTypes);
        end;
        VNames:= nil;
        VUnits:= nil;
        VIDs:= nil;
        VSizeTypes:= nil;
        //сохраняем название нового класса
        OldType:= NewType;
        //записываем новый тип в вектор
       // VAppend(ATypeNames, SIZ_TYPE_PICKS[NewType]);
        VAppend(ASIZTypes, QFieldInt('SIZType'));
      end;
      VAppend(VNames, QFieldStr('SIZName'));
      VAppend(VUnits, QFieldStr('UnitStringCode'));
      VAppend(VIDs, QFieldInt('NameID'));
      VAppend(VSizeTypes, QFieldInt('SizeType'));
      QNext;
    end;
    //записываем последний класс
    MAppend(ASIZNames, VNames);
    MAppend(AUnits, VUnits);
    MAppend(ANameIDs, VIDs);
    MAppend(ASizeTypes, VSizeTypes);
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStaffSizeLoad(const AFilterValue: String;
                          const AOrderType, AListType: Byte;
                          out AStaffIDs, AClothes, AHeights, AShoes, AHeads,
                              AHands, AGasmasks, ARespirators: TIntVector;
                          out AFamilies, ANames, APatronymics: TStrVector;
                          out ABornDates: TDateVector): Boolean;
var
  SQLStr, S: String;
begin
  Result:= False;
  AStaffIDs:= nil;
  AClothes:= nil;
  AHeights:= nil;
  AShoes:= nil;
  AHeads:= nil;
  AHands:= nil;
  AGasmasks:= nil;
  ARespirators:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ABornDates:= nil;

  SQLStr:=
    'SELECT DISTINCT t1.*, t2.Family, t2.Name, t2.Patronymic, t2.BornDate ' +
    'FROM SIZSTAFFSIZE t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'INNER JOIN STAFFTABNUM t3 ON (t1.StaffID=t3.StaffID) ' +
    'WHERE (t1.StaffID>0) ';

  if AListType>0 then
  begin
    case AListType of
    1: SQLStr:= SQLStr + 'AND (t3.DismissDate>= :ADate) ';
    2: SQLStr:= SQLStr + 'AND (t3.DismissDate< :ADate) ';
    end;
  end;

  if not SEmpty(AFilterValue) then
    SQLStr:= SQLStr + 'AND (t2.FullName LIKE :FilterValue)';

  S:= 't2.Family, t2.Name, t2.Patronymic, t2.BornDate';
  SQLStr:= SQLStr + 'ORDER BY ';
  case AOrderType of
  0: SQLStr:= SQLStr + S;
  1: SQLStr:= SQLStr + 't1.Clothes, t1.Height, ' + S;
  2: SQLStr:= SQLStr + 't1.Shoes, ' + S;
  3: SQLStr:= SQLStr + 't1.Head, ' + S;
  4: SQLStr:= SQLStr + 't1.Hand, ' + S;
  5: SQLStr:= SQLStr + 't1.Gasmask, ' + S;
  6: SQLStr:= SQLStr + 't1.Respirator, ' + S;
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('ADate', Date);
  QParamStr('FilterValue', '%'+AFilterValue+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStaffIDs, QFieldInt('StaffID'));
      VAppend(AClothes, QFieldInt('Clothes'));
      VAppend(AHeights, QFieldInt('Height'));
      VAppend(AShoes, QFieldInt('Shoes'));
      VAppend(AHeads, QFieldInt('Head'));
      VAppend(AHands, QFieldInt('Hand'));
      VAppend(AGasmasks, QFieldInt('Gasmask'));
      VAppend(ARespirators, QFieldInt('Respirator'));
      VAppend(AFamilies, QFieldStr('Family'));
      VAppend(ANames, QFieldStr('Name'));
      VAppend(APatronymics, QFieldStr('Patronymic'));
      VAppend(ABornDates, QFieldDT('BornDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStaffSizeLoad(const AStaffID: Integer;
  var ASizeIndexes: TSIZStaffSizeIndexes): Boolean;
begin
  Result:= False;
  SIZStaffSizeIndexesClear(ASizeIndexes);

  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('SIZSTAFFSIZE', ['Clothes', 'Height', 'Shoes', 'Head',
                               'Hand', 'Gasmask', 'Respirator']) +
    'WHERE StaffID = :StaffID'
  );
  QParamInt('StaffID', AStaffID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    ASizeIndexes.Clothes:= QFieldInt('Clothes');
    ASizeIndexes.Height:= QFieldInt('Height');
    ASizeIndexes.Shoes:= QFieldInt('Shoes');
    ASizeIndexes.Head:= QFieldInt('Head');
    ASizeIndexes.Hand:= QFieldInt('Hand');
    ASizeIndexes.Gasmask:= QFieldInt('Gasmask');
    ASizeIndexes.Respirator:= QFieldInt('Respirator');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStaffSizeUpdate(const AStaffID: Integer;
                         const ASizeIndexes: TSIZStaffSizeIndexes): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZSTAFFSIZE', ['Clothes', 'Height', 'Shoes', 'Head',
                                 'Hand', 'Gasmask', 'Respirator']) +
      'WHERE StaffID = :StaffID'
    );
    QParamInt('StaffID', AStaffID);
    QParamInt('Clothes',  ASizeIndexes.Clothes);
    QParamInt('Height', ASizeIndexes.Height);
    QParamInt('Shoes', ASizeIndexes.Shoes);
    QParamInt('Head', ASizeIndexes.Head);
    QParamInt('Hand', ASizeIndexes.Hand);
    QParamInt('Gasmask', ASizeIndexes.Gasmask);
    QParamInt('Respirator', ASizeIndexes.Respirator);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZSpecSizeLoad(const ATabNumID, AInfoID: Integer;
                         out ASizeID, AHeightID: Integer): Boolean;
begin
  Result:= False;
  ASizeID:= 0;
  AHeightID:= 0;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT SizeID, HeightID ' +
    'FROM SIZSTAFFSPECSIZE ' +
    'WHERE (TabNumID = :TabNumID) AND (InfoID = :InfoID)');
  QParamInt('TabNumID', ATabNumID);
  QParamInt('InfoID', AInfoID);
  QOpen;
  if not QIsEmpty then
  begin
    ASizeID:= QFieldInt('SizeID');
    AHeightID:= QFieldInt('HeightID');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZSpecSizeLoad(const AInfoID: Integer;
   out ATabNumIDs, ASizeIDs, AHeightIDs: TIntVector): Boolean;
begin
  Result:= False;
  ATabNumIDs:= nil;
  ASizeIDs:= nil;
  AHeightIDs:= nil;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT TabNumID, SizeID, HeightID ' +
    'FROM SIZSTAFFSPECSIZE ' +
    'WHERE InfoID = :InfoID');
  QParamInt('InfoID', AInfoID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(AHeightIDs, QFieldInt('HeightID'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZSpecSizeWrite(const AInfoID, ATabNumID, ASizeID,
  AHeightID: Integer; const ACommit: Boolean): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('SIZSTAFFSPECSIZE', ['InfoID', 'TabNumID', 'SizeID', 'HeightID'])
    );

    QParamInt('InfoID', AInfoID);
    QParamInt('TabNumID', ATabNumID);
    QParamInt('SizeID', ASizeID);
    QParamInt('HeightID', AHeightID);
    QExec;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZSpecSizeWrite(const AInfoID: Integer;
                              const ATabNumIDs, ASizeIDs, AHeightIDs: TIntVector;
                              const ACommit: Boolean): Boolean;
var
  i: Integer;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('SIZSTAFFSPECSIZE', ['InfoID', 'TabNumID', 'SizeID', 'HeightID'])
    );
    QParamInt('InfoID', AInfoID);
    for i:=0 to High(ATabNumIDs) do
    begin
      QParamInt('TabNumID', ATabNumIDs[i]);
      QParamInt('SizeID', ASizeIDs[i]);
      QParamInt('HeightID', AHeightIDs[i]);
      QExec;
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZSpecSizeExists(const AInfoID, ATabNumID: Integer): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT InfoID ' +
    'FROM SIZSTAFFSPECSIZE ' +
    'WHERE (InfoID = :InfoID) AND (TabNumID = :TabNumID)'
  );
  QParamInt('InfoID', AInfoID);
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TDataBase.SIZSpecSizeUpdate(const AInfoID, ATabNumID, ASizeID,
  AHeightID: Integer): Boolean;
begin
  if not SIZSpecSizeExists(AInfoID, ATabNumID) then
  begin
    Result:= SIZSpecSizeWrite(AInfoID, ATabNumID, ASizeID, AHeightID, True{commit});
    Exit;
  end;

  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZSTAFFSPECSIZE', ['SizeID', 'HeightID']) +
      'WHERE (InfoID = :InfoID) AND (TabNumID = :TabNumID)'
    );

    QParamInt('InfoID', AInfoID);
    QParamInt('TabNumID', ATabNumID);
    QParamInt('SizeID', ASizeID);
    QParamInt('HeightID', AHeightID);
    QExec;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZSpecSizeCopy(const ASourceInfoIDs, ADestInfoIDs: TIntVector;
                                  const ACommit: Boolean = True): Boolean;
var
  i: Integer;
  TabNumIDs, SizeIDs, HeightIDs: TIntVector;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //пробегаем по всем подстрокам строк пункта типовых норм
    for i:= 0 to High(ASourceInfoIDs) do
    begin
      //получаем данные по особым размерам СИЗ для пункта-источника
      SIZSpecSizeLoad(ASourceInfoIDs[i], TabNumIDs, SizeIDs, HeightIDs);
      //пробегаем по всем полученным спецразмерам и записываем в инфо цели
      SIZSpecSizeWrite(ADestInfoIDs[i], TabNumIDs, SizeIDs, HeightIDs, False{no commit});
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

procedure TDataBase.SizStatusSizeLoad(const ATabNumID, AInfoID, ASizeType: Integer;
                             const AStaffSizes: TSizStaffSizeIndexes;
                             out ASizeID, AHeightID: Integer);
var
  SpecSizeID, SpecHeightID, StaffSizeID, StaffHeightID: Integer;
begin
  //определяем размеры из личной карточки
  SIZStaffSizesForSizeType(AStaffSizes, ASizeType, StaffSizeID, StaffHeightID);
  //загружаем особые размеры
  SIZSpecSizeLoad(ATabNumID, AInfoID, SpecSizeID, SpecHeightID);
  //выбираем spec size, если он есть, иначе простой размер
  if ({%H-}SpecSizeID>0) then
  begin
    ASizeID:= SpecSizeID;
    AHeightID:= {%H-}SpecHeightID;
  end
  else begin
    ASizeID:= StaffSizeID;
    AHeightID:= StaffHeightID;
  end;
end;

procedure TDataBase.SIZStatusSizeLoad(const ATabNumID: Integer;
                               const AInfoIDs, ASizeTypes: TIntVector;
                               const AStaffSizes: TSizStaffSizeIndexes;
                               out ASizeIDs, AHeightIDs: TIntVector);
var
  i, SizeID, HeightID: Integer;
begin
  ASizeIDs:= nil;
  AHeightIDs:= nil;
  for i:= 0 to High(AInfoIDs) do
  begin
    SIZStatusSizeLoad(ATabNumID, AInfoIDs[i], ASizeTypes[i], AStaffSizes, SizeID, HeightID);
    VAppend(ASizeIDs, SizeID);
    VAppend(AHeightIDs, HeightID);
  end;
end;

function TDataBase.SIZDocExists(const ADocID: Integer;
                          const ADocName, ADocNum: String;
                          const ADocDate: TDate): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT DocID FROM SIZDOC ' +
    'WHERE (DocID <> :DocID) AND (UPPER(DocName) = :DocName) AND ' +
          '(DocDate = :DocDate) AND (UPPER(DocNum) = :DocNum)'
  );
  QParamInt('DocID', ADocID);
  QParamStr('DocName', SUpper(ADocName));
  QParamStr('DocNum', SUpper(ADocNum));
  QParamDT('DocDate', ADocDate);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TDataBase.SIZDocListLoad(const AYear, ADocType: Integer;
                          out ADocIDs, ADocForms: TIntVector;
                          out ADocNames, ADocNums: TStrVector;
                          out ADocDates: TDateVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  ADocIDs:= nil;
  ADocForms:= nil;
  ADocNames:= nil;
  ADocNums:= nil;
  ADocDates:= nil;

  SQLStr:=
    'SELECT DocID, DocName, DocNum, DocDate, DocForm ' +
    'FROM SIZDOC ' +
    'WHERE (DocID>0) ';

  if AYear>0 then
    SQLStr:= SQLStr + 'AND (DocDate BETWEEN :BD AND :ED) ';

  if ADocType>0 then
    SQLStr:= SQLStr + 'AND (DocType = :DocType) ';

  SQLStr:= SQLStr + 'ORDER BY DocDate DESC, DocName DESC, DocNum DESC';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  if AYear>0 then
  begin
    QParamDT('BD', FirstDayInYear(AYear));
    QParamDT('ED', LastDayInYear(AYear));
  end;
  if ADocType>0 then
    QParamInt('DocType', ADocType);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ADocIDs, QFieldInt('DocID'));
      VAppend(ADocForms, QFieldInt('DocForm'));
      VAppend(ADocNames, QFieldStr('DocName'));
      VAppend(ADocNums, QFieldStr('DocNum'));
      VAppend(ADocDates, QFieldDT('DocDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZDocWrite(out ADocID: Integer;
                         const ADocName, ADocNum: String;
                         const ADocDate: TDate;
                         const ADocType, ADocForm: Integer): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('SIZDOC', ['DocName', 'DocNum', 'DocDate', 'DocType', 'DocForm'])
    );
    QParamStr('DocName', ADocName);
    QParamStr('DocNum', ADocNum);
    QParamDT('DocDate', ADocDate);
    QParamInt('DocType', ADocType);
    QParamInt('DocForm', ADocForm);
    QExec;
    //определяем ID записанного документа
    ADocID:= LastWritedInt32ID('SIZDOC');
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZDocLoad(const ADocID: Integer;
                         out ADocName, ADocNum: String;
                         out ADocDate: TDate;
                         out ADocType, ADocForm: Integer): Boolean;
begin
  Result:= False;

  ADocName:= EmptyStr;
  ADocNum:= EmptyStr;
  ADocDate:= 0;
  ADocType:= 0;
  ADocForm:= 0;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT * FROM SIZDOC ' +
    'WHERE DocID = :DocID'
  );
  QParamInt('DocID', ADocID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    ADocName:= QFieldStr('DocName');
    ADocNum:= QFieldStr('DocNum');
    ADocDate:= QFieldDT('DocDate');
    ADocType:= QFieldInt('DocType');
    ADocForm:= QFieldInt('DocForm');
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZDocUpdate(const ADocID: Integer;
                         const ADocName, ADocNum: String;
                         const ADocDate: TDate;
                         const ADocType, ADocForm: Integer): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZDOC', ['DocName', 'DocNum', 'DocDate', 'DocType', 'DocForm']) +
      'WHERE DocID = :DocID'
    );
    QParamInt('DocID', ADocID);
    QParamStr('DocName', ADocName);
    QParamStr('DocNum', ADocNum);
    QParamDT('DocDate', ADocDate);
    QParamInt('DocType', ADocType);
    QParamInt('DocForm', ADocForm);
    QExec;
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZDocStoreEntryDelete(const ADocID: Integer): Boolean;
var
  EntryIDs, StoreIDs: TInt64Vector;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //получаем ID записей прихода
    EntryIDs:= ValuesInt64ByInt32ID('SIZSTOREENTRY', 'EntryID', 'DocID', ADocID);
    if not VIsNil(EntryIDs) then
    begin
      //получаем ID склада
      StoreIDs:= ValuesInt64ByInt64ID('SIZSTORELOG', 'StoreID', 'EntryID', EntryIDs);
      //отменяем приход на склад
      if not VIsNil(StoreIDs) then
        SIZStoreEntryCancel(StoreIDs, False{no commit});
    end;

    //удалям документ прихода
    Delete('SIZDOC', 'DocID', ADocID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZDocStoreWriteoffDelete(const ADocID: Integer): Boolean;
var
  StoreIDs: TInt64Vector;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //получаем ID списанного со склада
    StoreIDs:= ValuesInt64ByInt32ID('SIZSTOREWRITEOFF', 'StoreID', 'DocID', ADocID);
    //отменяем списание
    if not VIsNil(StoreIDs) then
      SIZStoreWriteoffCancel(StoreIDs, False{no commit});
    //удаляем документ списания
    Delete('SIZDOC', 'DocID', ADocID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZDocReceivingDelete(const ADocID: Integer): Boolean;
var
  LogIDs, StoreIDs: TInt64Vector;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //получаем ID логов выданного со склада по этому документу
    LogIDs:= ValuesInt64ByInt32ID('SIZCARDPERSONALLOG', 'LogID', 'DocID', ADocID);
    if not VIsNil(LogIDs) then
    begin
      //получаем ID выданного со склада
      StoreIDs:= ValuesInt64ByInt64ID('SIZCARDPERSONALLOGINFO', 'StoreID', 'LogID', LogIDs);
      //отменяем выдачу
      if not VIsNil(StoreIDs) then
        SIZStoreReceivingCancel(StoreIDs, False{no commit});
    end;
    //удаляем документ выдачи
    Delete('SIZDOC', 'DocID', ADocID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZDocReturningDelete(const ADocID: Integer): Boolean;
var
  StoreIDs: TInt64Vector;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //получаем ID возвращенного
    StoreIDs:= ValuesInt64ByInt32ID('SIZSTAFFRETURN', 'StoreID', 'DocID', ADocID);
    //отменяем списание
    if not VIsNil(StoreIDs) then
      SIZStoreReturningCancel(StoreIDs, False{no commit});
    //удаляем документ возврата
    Delete('SIZDOC', 'DocID', ADocID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZDocStoreEmptyDelete(const ADocType, AYear: Integer): Boolean;
var
  DocIDs: TIntVector;
  FromStr: String;
begin
  Result:= False;

  case ADocType of
  1: FromStr:= 'FROM SIZSTOREENTRY ';
  2: FromStr:= 'FROM SIZCARDPERSONALLOG ';
  3: FromStr:= 'FROM SIZSTOREWRITEOFF ';
  4: FromStr:= 'FROM SIZSTAFFRETURN ';
  end;

  //определяем ID пустых документов
  DocIDs:= nil;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.DocID, t2.DocCount ' +
    'FROM SIZDOC t1 ' +
    'LEFT OUTER JOIN ( ' +
      'SELECT DocID, COUNT(DocID) as DocCount ' +
      FromStr  +
      'GROUP BY DocID ' +
      ') t2 ON (t1.DocID=t2.DocID) ' +
    'WHERE (t1.DocType = :DocType) AND (t1.DocDate BETWEEN :BD AND :ED)'
  );
  QParamDT('BD', FirstDayInYear(AYear));
  QParamDT('ED', LastDayInYear(AYear));
  QParamInt('DocType', ADocType);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      if QFieldInt('DocCount')=0 then
        VAppend(DocIDs, QFieldInt('DocID'));
      QNext;
    end;
  end;
  QClose;

  if VIsNil(DocIDs) then
  begin
    Inform('Пустых документов нет!');
    Exit;
  end;

  //удаляем пустые документы
  Result:= Delete('SIZDOC', 'DocID', DocIDs);
end;

function TDataBase.SIZStoreEntryLoad(const ADocID: Integer;
                         out AEntryIDs: TInt64Vector;
                         out ANomNums, ASizNames, ASizUnits, ANotes: TStrVector;
                         out ASizCounts, ASizTypes, ANameIDs, ASizeIDs,
                             AHeightIDs, ASizeTypes: TIntVector): Boolean;
begin
  Result:= False;

  AEntryIDs:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizUnits:= nil;
  ANotes:= nil;
  ASizCounts:= nil;
  ASizTypes:= nil;
  ANameIDs:= nil;
  ASizeIDs:= nil;
  AHeightIDs:= nil;
  ASizeTypes:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.EntryID, t1.NomNum, t1.NameID, t1.SizeID, t1.HeightID, ' +
           't1.EntryCount, t1.EntryNote, ' +
           't2.SizName, t2.SizType, t2.SizeType, ' +
           't3.UnitStringCode AS SizUnit ' +
    'FROM SIZSTOREENTRY t1 ' +
    'INNER JOIN SIZNAME t2 ON (t1.NameID=t2.NameID) ' +
    'INNER JOIN SIZUNIT t3 ON (t2.UnitID=t3.UnitID) ' +
    'WHERE t1.DocID = :DocID ' +
    'ORDER BY t2.SizName, t1.NomNum, t1.SizeID, t1.HeightID'
  );
  QParamInt('DocID', ADocID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AEntryIDs, QFieldInt64('EntryID'));

      VAppend(ANomNums, QFieldStr('NomNum'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(ASizUnits, QFieldStr('SizUnit'));
      VAppend(ANotes, QFieldStr('EntryNote'));

      VAppend(ASizCounts, QFieldInt('EntryCount'));
      VAppend(ASizTypes, QFieldInt('SizeType'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(AHeightIDs, QFieldInt('HeightID'));
      VAppend(ASizeTypes, QFieldInt('SizeType'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreEntryLoad(const ADocID: Integer;
                         out AEntryIDs: TInt64Matrix;
                         out ANomNums, ASizNames, ASizUnits, ANotes: TStrMatrix;
                         out ASizCounts, ASizTypes, ANameIDs, ASizeIDs,
                             AHeightIDs, ASizeTypes: TIntMatrix): Boolean;
var
  EntryIDs: TInt64Vector;
  NomNums, SizNames, SizUnits, Notes: TStrVector;
  SizNums, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes: TIntVector;
  i, N1, N2, NameID: Integer;
  NomNum: String;

  procedure AddToMatrix(const AInd1, AInd2: Integer);
  begin
    MAppend(AEntryIDs, VCut(EntryIDs, AInd1, AInd2));
    MAppend(ANomNums, VCut(NomNums, AInd1, AInd2));
    MAppend(ASizNames, VCut(SizNames, AInd1, AInd2));
    MAppend(ASizUnits, VCut(SizUnits, AInd1, AInd2));
    MAppend(ANotes, VCut(Notes, AInd1, AInd2));
    MAppend(ASizCounts, VCut(SizNums, AInd1, AInd2));
    MAppend(ANameIDs, VCut(NameIDs, AInd1, AInd2));
    MAppend(ASizeIDs, VCut(SizeIDs, AInd1, AInd2));
    MAppend(AHeightIDs, VCut(HeightIDs, AInd1, AInd2));
    MAppend(ASizeTypes, VCut(SizeTypes, AInd1, AInd2));
  end;

begin
  AEntryIDs:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizUnits:= nil;
  ANotes:= nil;
  ASizCounts:= nil;
  ASizTypes:= nil;
  ANameIDs:= nil;
  ASizeIDs:= nil;
  AHeightIDs:= nil;
  ASizeTypes:= nil;

  Result:= SIZStoreEntryLoad(ADocID, EntryIDs, NomNums, SizNames, SizUnits, Notes,
                           SizNums, SizTypes, NameIDs, SizeIDs, HeightIDs, SizeTypes);
  if not Result then Exit;

  NameID:= NameIDs[0];
  NomNum:= NomNums[0];
  N1:= 0;
  for i:= 1 to High(NomNums) do
  begin
    if not (SSame(NomNums[i], NomNum) and (NameIDs[i]=NameID)) then
    begin
      N2:= i - 1;
      AddToMatrix(N1, N2);
      N1:= i;
      NameID:= NameIDs[i];
      NomNum:= NomNums[i];
    end;
  end;
  N2:= High(NomNums);
  AddToMatrix(N1, N2);
end;

function TDataBase.SIZStoreEntryExists(const ADocID: Integer;
                                 const AEntryID: Int64;
                                 const ANameID, ASizeID, AHeightID: Integer;
                                 const ANomNum: String): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT EntryID FROM SIZSTOREENTRY ' +
    'WHERE (EntryID <> :EntryID) AND (NomNum = :NomNum) AND ' +
          '(NameID = :NameID) AND (SizeID = :SizeID)  AND ' +
          '(HeightID = :HeightID) AND (DocID = :DocID)'
  );
  QParamInt64('EntryID', AEntryID);
  QParamStr('NomNum', ANomNum);
  QParamInt('NameID', ANameID);
  QParamInt('SizeID', ASizeID);
  QParamInt('HeightID', AHeightID);
  QParamInt('DocID', ADocID);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

procedure TDataBase.SIZStoreLogWrite(const AEntryID: Int64; const ACount: Integer);
var
  i: Integer;
begin
  QSetQuery(FQuery);
  QSetSQL(
    sqlINSERT('SIZSTORELOG', ['EntryID'])
  );
  QParamInt64('EntryID', AEntryID);
  for i:= 1 to ACount do
    QExec;
end;

function TDataBase.SIZStoreLogFreeIDs(const AEntryID: Int64;
                                      const ACount: Integer;
                                      out AStoreIDs: TInt64Vector): Boolean;
var
  Delta: Integer;

  function FreeStoreIDs(const ANeedCount: Integer): Boolean;
  begin
    Result:= False;
    QSetQuery(FQuery);
    QSetSQL(
      'SELECT StoreID ' +
      'FROM SIZSTORELOG ' +
      'WHERE (EntryID=:EntryID) AND (IsBusy=0) ' +
      'ORDER BY StoreID DESC');
    QParamInt64('EntryID', AEntryID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(AStoreIDs, QFieldInt64('StoreID'));
        if Length(AStoreIDs)=ANeedCount then  //достаточно строк для удаления
        begin
          Result:= True;
          break;
        end;
        QNext;
      end;
    end;
    QClose;
  end;

  procedure BusyStoreIDs(const ANeedCount: Integer);
  begin
    QSetQuery(FQuery);
    QSetSQL(
      'SELECT StoreID ' +
      'FROM SIZSTORE ' +
      'WHERE (EntryID=:EntryID) AND (IsBusy=1) ' +
      'ORDER BY StoreID DESC ' +
      'LIMIT :Delta');
    QParamInt64('EntryID', AEntryID);
    QParamInt('Delta', ANeedCount);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(AStoreIDs, QFieldInt64('StoreID'));
        QNext;
      end;
    end;
    QClose;
  end;

begin
  Result:= False;
  AStoreIDs:= nil;

  //определяем StoreID невыданных СИЗ
  Delta:= ACount;  //сколько сиз удалить
  Result:= FreeStoreIDs(Delta);
  //свободных СИЗ достаточно
  if Result then Exit;
  //запрос на удаление занятых СИЗ
  if not Confirm('Уменьшение количества приведет к удалению информации ' +
                 'о выданных сотрудникам СИЗ! Все равно сохранить?') then Exit;
  //подтверждено сохранение - добираем StoreID в занятых СИЗ
  Delta:= Delta - Length(AStoreIDs); //столько нужно добрать
  BusyStoreIDs(Delta);
  Result:= Length(AStoreIDs)=ACount;
end;

function TDataBase.SIZStoreEntryAdd(const ADocID: Integer;
                   out AEntryID: Int64;
                   const ANomNum, ANote: String;
                   const ANameID, ASizeID, AHeightID, ACount: Integer;
                   const ACommit: Boolean = True): Boolean;

  procedure StoreEntryWrite;
  begin
    QSetSQL(
      sqlINSERT('SIZSTOREENTRY', ['NomNum', 'NameID', 'SizeID', 'HeightID',
                                  'EntryCount', 'DocID', 'EntryNote'])
    );
    QParamStr('NomNum', ANomNum);
    QParamInt('NameID', ANameID);
    QParamInt('SizeID', ASizeID);
    QParamInt('HeightID', AHeightID);
    QParamInt('EntryCount', ACount);
    QParamInt('DocID', ADocID);
    QParamStr('EntryNote', ANote);
    QExec;
  end;

begin
  Result:= False;
  QSetQuery(FQuery);
  try
    StoreEntryWrite;
    //определяем ID
    AEntryID:= LastWritedInt64ID('SIZSTOREENTRY');
    SIZStoreLogWrite(AEntryID, ACount);
    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZStoreEntryUpdate(const ADocID: Integer;
         const AEntryID: Int64;
         const ANomNum, ANote: String;
         const ANameID, ASizeID, AHeightID, ACount, AOldCount: Integer): Boolean;
var
  StoreIDs: TInt64Vector;

  procedure StoreEntryUpdate;
  begin
    QSetSQL(
      sqlUPDATE('SIZSTOREENTRY', ['NomNum', 'NameID', 'SizeID', 'HeightID',
                                  'EntryCount', 'DocID', 'EntryNote']) +
      'WHERE EntryID = :EntryID'
    );
    QParamInt64('EntryID', AEntryID);
    QParamStr('NomNum', ANomNum);
    QParamInt('NameID', ANameID);
    QParamInt('SizeID', ASizeID);
    QParamInt('HeightID', AHeightID);
    QParamInt('EntryCount', ACount);
    QParamInt('DocID', ADocID);
    QParamStr('EntryNote', ANote);
    QExec;
  end;

begin
  Result:= False;

  QSetQuery(FQuery);
  try
    if ACount<>AOldCount then  //изменилось количество СИЗ
    begin
      if ACount>AOldCount then //если стало больше - добавляем недостающее
        SIZStoreLogWrite(AEntryID, ACount - AOldCount)
      else begin //стало меньше
        if not SIZStoreLogFreeIDs(AEntryID, AOldCount - ACount, StoreIDs) then Exit;
          Delete('SIZSTORELOG', 'StoreID',  StoreIDs, False{no commit});
      end;
    end;
    StoreEntryUpdate;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZStoreEntryDelete(const AEntryID: Int64; const ACommit: Boolean): Boolean;
var
  StoreIDs: TInt64Vector;
begin
  Result:= False;
  StoreIDs:= ValuesInt64ByInt64ID('SIZSTORELOG', 'StoreID', 'EntryID', AEntryID, True{Unique});
  if VIsNil(StoreIDs) then Exit;
  Result:= SIZStoreEntryCancel(StoreIDs, ACommit);
end;

function TDataBase.SIZStoreWriteOffLoad(const ADocID: Integer;
                         out AStoreIDs: TInt64Vector;
                         out ANomNums, ASizNames, ASizUnits, AEntryDocNames, AEntryDocNums, ANotes: TStrVector;
                         out ASizeIDs, AHeightIDs, ASizeTypes, ANameIDs: TIntVector;
                         out AEntryDocDates: TDateVector): Boolean;
begin
  Result:= False;

  AStoreIDs:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizUnits:= nil;
  AEntryDocNames:= nil;
  AEntryDocNums:= nil;
  ANotes:= nil;
  ASizeIDs:= nil;
  AHeightIDs:= nil;
  ASizeTypes:= nil;
  ANameIDs:= nil;
  AEntryDocDates:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.StoreID, t1.Note, ' +
           't2.NomNum, t2.NameID, t2.SizeID, t2.HeightID, ' +
           't3.SizName, t3.SizeType, ' +
           't4.UnitStringCode AS SizUnit, ' +
           't5.DocName, t5.DocNum, t5.DocDate ' +
    'FROM SIZSTOREWRITEOFF t1 ' +
    'INNER JOIN SIZSTORELOG tt ON (t1.StoreID=tt.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t2 ON (tt.EntryID=t2.EntryID) ' +
    'INNER JOIN SIZNAME t3 ON (t2.NameID=t3.NameID) ' +
    'INNER JOIN SIZUNIT t4 ON (t3.UnitID=t4.UnitID) ' +
    'INNER JOIN SIZDOC t5 ON (t2.DocID=t5.DocID) ' +
    'WHERE (t1.DocID = :DocID) '  +
    'ORDER BY t3.SizName, t2.SizeID, t2.HeightID, t2.NomNum, ' +
             't5.DocDate, t5.DocName, t5.DocNum'
  );
  QParamInt('DocID', ADocID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStoreIDs, QFieldInt64('StoreID'));

      VAppend(ANomNums, QFieldStr('NomNum'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(ASizUnits, QFieldStr('SizUnit'));
      VAppend(AEntryDocNames, QFieldStr('DocName'));
      VAppend(AEntryDocNums, QFieldStr('DocNum'));
      VAppend(ANotes, QFieldStr('Note'));

      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(AHeightIDs, QFieldInt('HeightID'));
      VAppend(ASizeTypes, QFieldInt('SizeType'));
      VAppend(ANameIDs, QFieldInt('NameID'));

      VAppend(AEntryDocDates, QFieldDT('DocDate'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreWriteOffLoad(const ADocID: Integer;
                         out ACategoryNames: TStrMatrix;
                         out AStoreIDs: TInt64Matrix;
                         out ASizCounts: TIntMatrix;
                         out ANomNums, ASizNames, ASizUnits,
                             ASizSizes, AEntryDocNames, ANotes: TStrMatrix): Boolean;
var
  i, N1, N2, NameID: Integer;
  NomNum: String;
  StoreIDs: TInt64Vector;
  NomNums, SizNames, SizUnits, EntryDocNames, EntryDocNums, Notes: TStrVector;
  SizeIDs, HeightIDs, SizeTypes, NameIDs: TIntVector;
  EntryDocDates: TDateVector;
  SizSizes, DocFullNames: TStrVector;

  procedure AddToMatrix(const AInd1, AInd2: Integer);
  var
    V: TIntVector;
  begin
    MAppend(AStoreIDs, VCut(StoreIDs, AInd1, AInd2));
    MAppend(ANomNums, VCut(NomNums, AInd1, AInd2));
    MAppend(ASizNames, VCut(SizNames, AInd1, AInd2));
    MAppend(ASizUnits, VCut(SizUnits, AInd1, AInd2));
    MAppend(ASizSizes, VCut(SizSizes, AInd1, AInd2));
    MAppend(AEntryDocNames, VCut(DocFullNames, AInd1, AInd2));
    MAppend(ANotes, VCut(Notes, AInd1, AInd2));

    VDim(V{%H-}, AInd2-AInd1+1, 1);
    MAppend(ASizCounts, V);
  end;

begin
  Result:= False;

  ACategoryNames:= nil;
  AStoreIDs:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizUnits:= nil;
  ASizCounts:= nil;
  ASizSizes:= nil;
  AEntryDocNames:= nil;
  ANotes:= nil;

  if not SIZStoreWriteOffLoad(ADocID, StoreIDs, NomNums, SizNames, SizUnits,
                           EntryDocNames, EntryDocNums, Notes, SizeIDs, HeightIDs,
                           SizeTypes, NameIDs, EntryDocDates) then Exit;

  SizSizes:= SIZFullSize(SizeTypes, SizeIDs, HeightIDs);
  DocFullNames:= SIZDocFullName(EntryDocNames, EntryDocNums, EntryDocDates);

  NameID:= NameIDs[0];
  NomNum:= NomNums[0];
  N1:= 0;
  for i:= 1 to High(NomNums) do
  begin
    if not (SSame(NomNums[i], NomNum) and (NameIDs[i]=NameID)) then
    begin
      N2:= i - 1;
      AddToMatrix(N1, N2);
      N1:= i;
      NameID:= NameIDs[i];
      NomNum:= NomNums[i];
    end;
  end;
  N2:= High(NomNums);
  AddToMatrix(N1, N2);

  MDim(ACategoryNames, Length(AStoreIDs), 6);
  for i:= 0 to High(AStoreIDs) do
  begin
    ACategoryNames[i, 0]:= ANomNums[i, 0];
    ACategoryNames[i, 1]:= ASizNames[i, 0];
    ACategoryNames[i, 2]:= ASizUnits[i, 0];
    ACategoryNames[i, 3]:= IntToStr(VSum(ASizCounts[i]));
  end;

end;

function TDataBase.SIZStoreWriteoffAdd(const ADocID: Integer;
                                 const AStoreIDs: TInt64Vector;
                                 const ANote: String): Boolean;
var
  i: Integer;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //записываем данные в таблицу списания
    QSetSQL(
      sqlINSERT('SIZSTOREWRITEOFF', ['DocID', 'StoreID', 'Note'])
    );
    QParamInt('DocID', ADocID);
    QParamStr('Note', ANote);
    for i:= 0 to High(AStoreIDs) do
    begin
      QParamInt64('StoreID', AStoreIDs[i]);
      QExec;
    end;

    //отмечаем СИЗ на складе, как занятые
    UpdateByInt64ID('SIZSTORELOG', 'IsBusy', 'StoreID', AStoreIDs, 1{занято}, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZStoreLoad(const ASIZType: Integer;
                         out AStoreIDs: TInt64Vector;
                         out ANomNums, ASizNames, ASizUnits, ADocNames, ADocNums: TStrVector;
                         out ASizeIDs, AHeightIDs, ASizeTypes, ANameIDs: TIntVector;
                         out ADocDates: TDateVector): Boolean;
var
  WhereStr: String;
begin
  Result:= False;

  AStoreIDs:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizUnits:= nil;
  ADocNames:= nil;
  ADocNums:= nil;
  ASizeIDs:= nil;
  AHeightIDs:= nil;
  ASizeTypes:= nil;
  ANameIDs:= nil;
  ADocDates:= nil;

  WhereStr:= 'WHERE (t1.IsBusy=0) ';
  if ASIZType>=0 then
    WhereStr:= WhereStr + 'AND (t3.SIZType = :SIZType) ';

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.StoreID, ' +
           't2.NomNum, t2.NameID, t2.SizeID, t2.HeightID, ' +
           't3.SizName, t3.SizeType, ' +
           't4.UnitStringCode AS SizUnit, ' +
           't5.DocName, t5.DocNum, t5.DocDate ' +
    'FROM SIZSTORELOG t1 ' +
    'INNER JOIN SIZSTOREENTRY t2 ON (t1.EntryID=t2.EntryID) ' +
    'INNER JOIN SIZNAME t3 ON (t2.NameID=t3.NameID) ' +
    'INNER JOIN SIZUNIT t4 ON (t3.UnitID=t4.UnitID) ' +
    'INNER JOIN SIZDOC t5 ON (t2.DocID=t5.DocID) ' +
    WhereStr  +
    'ORDER BY t3.SizName, t2.SizeID, t2.HeightID, t2.NomNum, ' +
             't5.DocDate, t5.DocName, t5.DocNum'
  );
  QParamInt('SIZType', ASIZType);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStoreIDs, QFieldInt64('StoreID'));

      VAppend(ANomNums, QFieldStr('NomNum'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(ASizUnits, QFieldStr('SizUnit'));
      VAppend(ADocNames, QFieldStr('DocName'));
      VAppend(ADocNums, QFieldStr('DocNum'));

      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(AHeightIDs, QFieldInt('HeightID'));
      VAppend(ASizeTypes, QFieldInt('SizeType'));
      VAppend(ANameIDs, QFieldInt('NameID'));

      VAppend(ADocDates, QFieldDT('DocDate'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreLoad(const ASIZType: Integer;
                         out ACategoryNames: TStrMatrix;
                         out AStoreIDs: TInt64Matrix;
                         out ASizCounts: TIntMatrix;
                         out ANomNums, ASizNames, ASizUnits,
                             ASizSizes, ADocNames: TStrMatrix;
                         const ANeedCountInCategory: Boolean = True): Boolean;
var
  i, N1, N2, NameID: Integer;
  NomNum: String;
  StoreIDs: TInt64Vector;
  NomNums, SizNames, SizUnits, DocNames, DocNums: TStrVector;
  SizeIDs, HeightIDs, SizeTypes, NameIDs: TIntVector;
  DocDates: TDateVector;
  SizSizes, DocFullNames: TStrVector;

  procedure AddToMatrix(const AInd1, AInd2: Integer);
  var
    V: TIntVector;
  begin
    MAppend(AStoreIDs, VCut(StoreIDs, AInd1, AInd2));
    MAppend(ANomNums, VCut(NomNums, AInd1, AInd2));
    MAppend(ASizNames, VCut(SizNames, AInd1, AInd2));
    MAppend(ASizUnits, VCut(SizUnits, AInd1, AInd2));
    MAppend(ASizSizes, VCut(SizSizes, AInd1, AInd2));
    MAppend(ADocNames, VCut(DocFullNames, AInd1, AInd2));

    VDim(V{%H-}, AInd2-AInd1+1, 1);
    MAppend(ASizCounts, V);
  end;

begin
  Result:= False;

  ACategoryNames:= nil;
  AStoreIDs:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizUnits:= nil;
  ASizCounts:= nil;
  ASizSizes:= nil;
  ADocNames:= nil;

  if not SIZStoreLoad(ASIZType, StoreIDs, NomNums, SizNames, SizUnits,
                           DocNames, DocNums, SizeIDs, HeightIDs,
                           SizeTypes, NameIDs, DocDates) then Exit;

  SizSizes:= SIZFullSize(SizeTypes, SizeIDs, HeightIDs);
  DocFullNames:= SIZDocFullName(DocNames, DocNums, DocDates);

  NameID:= NameIDs[0];
  NomNum:= NomNums[0];
  N1:= 0;
  for i:= 1 to High(NomNums) do
  begin
    if not (SSame(NomNums[i], NomNum) and (NameIDs[i]=NameID)) then
    begin
      N2:= i - 1;
      AddToMatrix(N1, N2);
      N1:= i;
      NameID:= NameIDs[i];
      NomNum:= NomNums[i];
    end;
  end;
  N2:= High(NomNums);
  AddToMatrix(N1, N2);

  MDim(ACategoryNames, Length(AStoreIDs), 6);
  for i:= 0 to High(AStoreIDs) do
  begin
    ACategoryNames[i, 0]:= ANomNums[i, 0];
    ACategoryNames[i, 1]:= ASizNames[i, 0];
    ACategoryNames[i, 2]:= ASizUnits[i, 0];
    if ANeedCountInCategory then
      ACategoryNames[i, 3]:= IntToStr(VSum(ASizCounts[i]));
  end;

end;

function TDataBase.SIZStoreReceivingLoad(const ADocID: Integer;
                         out ALogIDs, AStoreIDs: TInt64Vector;
                         out AFs, ANs, APs, ATabNums, APostNames,
                             ANomNums, ASizNames, ASizSTRUnits: TStrVector;
                         out ATabNumIDs, ANums, ALifes, ANameIDs, ASizDIGUnits,
                             ASIZTypes, ASizeIDs: TIntVector;
                         out AReceivingDates: TDateVector): Boolean;
begin
  AStoreIDs:= nil;
  ALogIDs:= nil;

  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizSTRUnits:= nil;
  ASizDIGUnits:= nil;
  ASIZTypes:= nil;
  ASizeIDs:= nil;

  ATabNumIDs:= nil;
  ANums:= nil;
  ALifes:= nil;
  ANameIDs:= nil;

  AReceivingDates:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.StoreID, t1.LogID, ' +
           't4.TabNumID, t4.TabNum, '  +
           't5.Family, t5.Name, t5.Patronymic, ' +
           't7.PostName, ' +
           't9.NomNum, t9.SizeID, ' +
           't10.SizName, t10.NameID, t10.SIZType, ' +
           't11.UnitStringCode,  t11.UnitDigitalCode, ' +
           't12.Num, t12.Life, ' +
           't13.DocDate ' +
    'FROM SIZCARDPERSONALLOGINFO t1 ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZCARDPERSONAL t3 ON (t2.CardID=t3.CardID) ' +
    'INNER JOIN STAFFTABNUM t4 ON (t3.TabNumID=t4.TabNumID) ' +
    'INNER JOIN STAFFMAIN t5 ON (t4.StaffID=t5.StaffID) ' +
    'INNER JOIN SIZNORMITEMPOST t6 ON (t3.ItemPostID=t6.ItemPostID) ' +
    'INNER JOIN STAFFPOST t7 ON (t6.PostID=t7.PostID) ' +
    'INNER JOIN SIZSTORELOG t8 ON (t1.StoreID=t8.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t9 ON (t8.EntryID=t9.EntryID) ' +
    'INNER JOIN SIZNAME t10 ON (t9.NameID=t10.NameID) ' +
    'INNER JOIN SIZUNIT t11 ON (t10.UnitID=t11.UnitID) ' +
    'INNER JOIN SIZNORMSUBITEMINFO t12 ON (t2.NowInfoID=t12.InfoID) ' +
    'INNER JOIN SIZDOC t13 ON (t2.DocID=t13.DocID) ' +
    'WHERE (t2.DocID = :DocID) AND (t2.ReceivingInfoID=t2.NowInfoID) ' +
    'ORDER BY t5.Family, t5.Name, t5.Patronymic, t1.LogID, t10.SizName, t9.NomNum'
  );
  QParamInt('DocID', ADocID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ALogIDs, QFieldInt64('LogID'));
      VAppend(AStoreIDs, QFieldInt64('StoreID'));

      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(APostNames, QFieldStr('PostName'));

      VAppend(ANomNums, QFieldStr('NomNum'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(ASizSTRUnits, QFieldStr('UnitStringCode'));
      VAppend(ASizDIGUnits, QFieldInt('UnitDigitalCode'));

      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ANums, QFieldInt('Num'));
      VAppend(ALifes, QFieldInt('Life'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ASIZTypes, QFieldInt('SIZType'));
      VAppend(ASizeIDs, QFieldInt('SizeID'));

      VAppend(AReceivingDates, QFieldDT('DocDate'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreReceivingLoad(const ADocID: Integer;
                         out AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                         out AStoreIDs: TInt64Matrix3D;
                         out ASizCounts, ASizDigUnits: TIntMatrix;
                         out ANomNums, ASizNames, ASizStrUnits, ASizLifes: TStrMatrix;
                         out AReceivingDates: TDateMatrix): Boolean;
var
  LogIDs, StoreIDs: TInt64Vector;
  Fs, Ns, Ps, TabNums, PostNames, NomNums, SizNames, SizSTRUnits: TStrVector;
  TabNumIDs, Nums, Lifes, NameIDs, SizDIGUnits, SIZTypes, SizeIDs: TIntVector;
  ReceivingDates: TDateVector;

  VNomNums, VSizNames, VLifes, VSizSTRUnits: TStrVector;
  MStoreIDs: TInt64Matrix;
  VCounts, VSizCounts, VSizDIGUnits: TIntVector;
  VReceivingDates: TDateVector;

  procedure GroupByNomNumAndNameID(const AInd1, AInd2: Integer);
  var
    i, N1, N2, NameID: Integer;
    NomNum: String;
  begin
    VCounts:= nil;

    NameID:= NameIDs[AInd1];
    NomNum:= NomNums[AInd1];
    N1:= AInd1;
    for i:= AInd1+1 to AInd2 do
    begin
      if (not SSame(NomNums[i], NomNum)) or (NameIDs[i]<>NameID) then
      begin
        N2:= i - 1;
        VAppend(VNomNums, NomNums[N1]);
        VAppend(VSizNames, SizNames[N1]);
        if SIZTypes[N1]=0 then //дерматологические
          VAppend(VCounts, VSum(SizeIDs, N1, N2))
        else //прочие
          VAppend(VCounts, N2-N1+1);
        MAppend(MStoreIDs, VCut(StoreIDs, N1, N2));
        N1:= i;
        NameID:= NameIDs[i];
        NomNum:= NomNums[i];
      end;
    end;
    N2:= AInd2;
    VAppend(VNomNums, NomNums[N1]);
    VAppend(VSizNames, SizNames[N1]);
    if SIZTypes[N1]=0 then //дерматологические
      VAppend(VCounts, VSum(SizeIDs, N1, N2))
    else //прочие
      VAppend(VCounts, N2-N1+1);
    MAppend(MStoreIDs, VCut(StoreIDs, N1, N2));
  end;

  procedure AddLogData(const AInd1, AInd2: Integer);
  var
    ReceivingCount: Integer;
    Life: String;
    VStr: TStrVector;
    VInt: TIntVector;
    VDate: TDateVector;
  begin
    GroupByNomNumAndNameID(AInd1, AInd2);

    ReceivingCount:= VSum(VCounts);
    Life:= SIZLifeInMonthOrYears(ReceivingCount, Nums[AInd1], Lifes[AInd1]);
    VDim(VStr{%H-}, Length(VCounts), Life);
    VLifes:= VAdd(VLifes, VStr);

    VSizCounts:= VAdd(VSizCounts, VCounts);

    VDim(VStr, Length(VCounts), SizSTRUnits[AInd1]);
    VSizSTRUnits:= VAdd(VSizSTRUnits, VStr);

    VDim(VInt{%H-}, Length(VCounts), SizDIGUnits[AInd1]);
    VSizDIGUnits:= VAdd(VSizDIGUnits, VInt);

    VDim(VDate{%H-}, Length(VCounts), ReceivingDates[AInd1]);
    VReceivingDates:= VAdd(VReceivingDates, VDate);
  end;

  procedure GroupByLogID(const AInd1, AInd2: Integer);
  var
    i, N1, N2: Integer;
    LogID: Int64;
  begin
    MStoreIDs:= nil;
    VNomNums:= nil;
    VSizNames:= nil;
    VLifes:= nil;
    VSizCounts:= nil;
    VSizSTRUnits:= nil;
    VSizDIGUnits:= nil;

    LogID:= LogIDs[AInd1];
    N1:= AInd1;
    for i:= AInd1+1 to AInd2 do
    begin
      if LogIDs[i]<>LogID then
      begin
        N2:= i - 1;
        AddLogData(N1, N2);
        N1:= i;
        LogID:= LogIDs[i];
      end;
    end;
    N2:= AInd2;
    AddLogData(N1, N2);

    MAppend(ANomNums, VNomNums);
    MAppend(ASizNames, VSizNames);
    MAppend(AStoreIDs, MStoreIDs);
    MAppend(ASizLifes, VLifes);
    MAppend(ASizCounts, VSizCounts);
    MAppend(ASizSTRUnits, VSizSTRUnits);
    MAppend(ASizDIGUnits, VSizDIGUnits);
    MAppend(AReceivingDates, VReceivingDates);
  end;

  procedure AddStaffNameData(const AInd1, AInd2: Integer);
  begin
    VAppend(AFs, Fs[AInd1]);
    VAppend(ANs, Ns[AInd1]);
    VAppend(APs, Ps[AInd1]);
    VAppend(ATabNums, TabNums[AInd1]);
    VAppend(APostNames, PostNames[AInd1]);

    GroupByLogID(AInd1, AInd2);
  end;

  procedure GroupByStaffName;
  var
    i, N1, N2: Integer;
    TabNumID: Int64;
  begin
    TabNumID:= TabNumIDs[0];
    N1:= 0;
    for i:= 1 to High(TabNumIDs) do
    begin
      if TabNumIDs[i]<>TabNumID then
      begin
        N2:= i - 1;
        AddStaffNameData(N1, N2);
        N1:= i;
        TabNumID:= TabNumIDs[i];
      end;
    end;
    N2:= High(TabNumIDs);
    AddStaffNameData(N1, N2);
  end;

begin
  Result:= False;

  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;

  AStoreIDs:= nil;
  ASizCounts:= nil;
  ASizDigUnits:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizStrUnits:= nil;
  ASizLifes:= nil;

  AReceivingDates:= nil;

  if not SIZStoreReceivingLoad(ADocID, LogIDs, StoreIDs, Fs, Ns, Ps, TabNums,
                               PostNames, NomNums, SizNames, SizSTRUnits,
                               TabNumIDs, Nums, Lifes, NameIDs, SizDIGUnits,
                               SIZTypes, SizeIDs, ReceivingDates) then Exit;
  GroupByStaffName;

  Result:= True;
end;

function TDataBase.SIZStoreReturningLoad(const ADocID: Integer;
                         out ALogIDs, AStoreIDs: TInt64Vector;
                         out AFs, ANs, APs, ATabNums, APostNames,
                             ANomNums, ASizNames, ASizSTRUnits,
                             AReceivingDocNames, AReceivingDocNums, ANotes: TStrVector;
                         out ATabNumIDs, ANums, ALifes, ANameIDs, ASizDIGUnits: TIntVector;
                         out AReceivingDates: TDateVector): Boolean;
begin
  AStoreIDs:= nil;
  ALogIDs:= nil;

  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizSTRUnits:= nil;
  ASizDIGUnits:= nil;

  ATabNumIDs:= nil;
  ANums:= nil;
  ALifes:= nil;
  ANameIDs:= nil;

  AReceivingDocNames:= nil;
  AReceivingDocNums:= nil;
  AReceivingDates:= nil;
  ANotes:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t0.Note, ' +
           't1.StoreID, t1.LogID, ' +
           't4.TabNumID, t4.TabNum, '  +
           't5.Family, t5.Name, t5.Patronymic, ' +
           't7.PostName, ' +
           't9.NomNum, ' +
           't10.SizName, t10.NameID, ' +
           't11.UnitStringCode,  t11.UnitDigitalCode, ' +
           't12.Num, t12.Life, ' +
           't13.DocDate, t13.DocName, t13.DocNum ' +
    'FROM SIZSTAFFRETURN t0 ' +
    'INNER JOIN SIZCARDPERSONALLOGINFO t1 ON (t0.StoreID=t1.StoreID) ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZCARDPERSONAL t3 ON (t2.CardID=t3.CardID) ' +
    'INNER JOIN STAFFTABNUM t4 ON (t3.TabNumID=t4.TabNumID) ' +
    'INNER JOIN STAFFMAIN t5 ON (t4.StaffID=t5.StaffID) ' +
    'INNER JOIN SIZNORMITEMPOST t6 ON (t3.ItemPostID=t6.ItemPostID) ' +
    'INNER JOIN STAFFPOST t7 ON (t6.PostID=t7.PostID) ' +
    'INNER JOIN SIZSTORELOG t8 ON (t1.StoreID=t8.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t9 ON (t8.EntryID=t9.EntryID) ' +
    'INNER JOIN SIZNAME t10 ON (t9.NameID=t10.NameID) ' +
    'INNER JOIN SIZUNIT t11 ON (t10.UnitID=t11.UnitID) ' +
    'INNER JOIN SIZNORMSUBITEMINFO t12 ON (t2.NowInfoID=t12.InfoID) ' +
    'INNER JOIN SIZDOC t13 ON (t2.DocID=t13.DocID) ' +
    'WHERE (t0.DocID = :DocID) AND (t2.ReceivingInfoID=t2.NowInfoID) ' +
    'ORDER BY t5.Family, t5.Name, t5.Patronymic, t1.LogID, t10.SizName, t9.NomNum'
  );
  QParamInt('DocID', ADocID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ALogIDs, QFieldInt64('LogID'));
      VAppend(AStoreIDs, QFieldInt64('StoreID'));

      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      VAppend(APostNames, QFieldStr('PostName'));

      VAppend(ANomNums, QFieldStr('NomNum'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(ASizSTRUnits, QFieldStr('UnitStringCode'));
      VAppend(ASizDIGUnits, QFieldInt('UnitDigitalCode'));

      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(ANums, QFieldInt('Num'));
      VAppend(ALifes, QFieldInt('Life'));
      VAppend(ANameIDs, QFieldInt('NameID'));

      VAppend(AReceivingDocNames, QFieldStr('DocName'));
      VAppend(AReceivingDocNums, QFieldStr('DocNum'));
      VAppend(AReceivingDates, QFieldDT('DocDate'));
      VAppend(ANotes, QFieldStr('Note'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreReturningLoad(const ADocID: Integer;
                         out AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                         out AStoreIDs: TInt64Matrix3D;
                         out ASizCounts, ASizDigUnits: TIntMatrix;
                         out ANomNums, ASizNames, ASizStrUnits, ASizLifes,
                             AReceivingDocNames, AReceivingDocNums, ANotes: TStrMatrix;
                         out AReceivingDates: TDateMatrix): Boolean;
var
  LogIDs, StoreIDs: TInt64Vector;
  Fs, Ns, Ps, TabNums, PostNames, NomNums, SizNames, SizSTRUnits: TStrVector;
  ReceivingDocNames, ReceivingDocNums, Notes: TStrVector;
  TabNumIDs, Nums, Lifes, NameIDs, SizDIGUnits: TIntVector;
  ReceivingDates: TDateVector;

  VNomNums, VSizNames, VLifes, VSizSTRUnits, VDocNames, VDocNums, VNotes: TStrVector;
  MStoreIDs: TInt64Matrix;
  VCounts, VSizCounts, VSizDIGUnits: TIntVector;
  VReceivingDates: TDateVector;

  procedure GroupByNomNumAndNameID(const AInd1, AInd2: Integer);
  var
    i, N1, N2, NameID: Integer;
    NomNum: String;
  begin
    VCounts:= nil;

    NameID:= NameIDs[AInd1];
    NomNum:= NomNums[AInd1];
    N1:= AInd1;
    for i:= AInd1+1 to AInd2 do
    begin
      if (not SSame(NomNums[i], NomNum)) or (NameIDs[i]<>NameID) then
      begin
        N2:= i - 1;
        VAppend(VNomNums, NomNums[N1]);
        VAppend(VSizNames, SizNames[N1]);
        VAppend(VCounts, N2-N1+1);
        MAppend(MStoreIDs, VCut(StoreIDs, N1, N2));
        N1:= i;
        NameID:= NameIDs[i];
        NomNum:= NomNums[i];
      end;
    end;
    N2:= AInd2;
    VAppend(VNomNums, NomNums[N1]);
    VAppend(VSizNames, SizNames[N1]);
    VAppend(VCounts, N2-N1+1);
    MAppend(MStoreIDs, VCut(StoreIDs, N1, N2));
  end;

  procedure AddLogData(const AInd1, AInd2: Integer);
  var
    ReceivingCount: Integer;
    Life: String;
    VStr: TStrVector;
    VInt: TIntVector;
    VDate: TDateVector;
  begin
    GroupByNomNumAndNameID(AInd1, AInd2);

    ReceivingCount:= VSum(VCounts);
    Life:= SIZLifeInMonthOrYears(ReceivingCount, Nums[AInd1], Lifes[AInd1]);
    VDim(VStr{%H-}, Length(VCounts), Life);
    VLifes:= VAdd(VLifes, VStr);

    VSizCounts:= VAdd(VSizCounts, VCounts);

    VDim(VStr, Length(VCounts), SizSTRUnits[AInd1]);
    VSizSTRUnits:= VAdd(VSizSTRUnits, VStr);

    VDim(VInt{%H-}, Length(VCounts), SizDIGUnits[AInd1]);
    VSizDIGUnits:= VAdd(VSizDIGUnits, VInt);

    VDim(VDate{%H-}, Length(VCounts), ReceivingDates[AInd1]);
    VReceivingDates:= VAdd(VReceivingDates, VDate);

    VDim(VStr, Length(VCounts), ReceivingDocNames[AInd1]);
    VDocNames:= VAdd(VDocNames, VStr);

    VDim(VStr, Length(VCounts), ReceivingDocNums[AInd1]);
    VDocNums:= VAdd(VDocNums, VStr);

    VDim(VStr, Length(VCounts), Notes[AInd1]);
    VNotes:= VAdd(VNotes, VStr);
  end;

  procedure GroupByLogID(const AInd1, AInd2: Integer);
  var
    i, N1, N2: Integer;
    LogID: Int64;
  begin
    MStoreIDs:= nil;
    VNomNums:= nil;
    VSizNames:= nil;
    VLifes:= nil;
    VSizCounts:= nil;
    VSizSTRUnits:= nil;
    VSizDIGUnits:= nil;
    VReceivingDates:= nil;
    VDocNames:= nil;
    VDocNums:= nil;
    VNotes:= nil;

    LogID:= LogIDs[AInd1];
    N1:= AInd1;
    for i:= AInd1+1 to AInd2 do
    begin
      if LogIDs[i]<>LogID then
      begin
        N2:= i - 1;
        AddLogData(N1, N2);
        N1:= i;
        LogID:= LogIDs[i];
      end;
    end;
    N2:= AInd2;
    AddLogData(N1, N2);

    MAppend(ANomNums, VNomNums);
    MAppend(ASizNames, VSizNames);
    MAppend(AStoreIDs, MStoreIDs);
    MAppend(ASizLifes, VLifes);
    MAppend(ASizCounts, VSizCounts);
    MAppend(ASizSTRUnits, VSizSTRUnits);
    MAppend(ASizDIGUnits, VSizDIGUnits);
    MAppend(AReceivingDates, VReceivingDates);
    MAppend(AReceivingDocNames, VDocNames);
    MAppend(AReceivingDocNums, VDocNums);
    MAppend(ANotes, VNotes);
  end;

  procedure AddStaffNameData(const AInd1, AInd2: Integer);
  begin
    VAppend(AFs, Fs[AInd1]);
    VAppend(ANs, Ns[AInd1]);
    VAppend(APs, Ps[AInd1]);
    VAppend(ATabNums, TabNums[AInd1]);
    VAppend(APostNames, PostNames[AInd1]);

    GroupByLogID(AInd1, AInd2);
  end;

  procedure GroupByStaffName;
  var
    i, N1, N2: Integer;
    TabNumID: Int64;
  begin
    TabNumID:= TabNumIDs[0];
    N1:= 0;
    for i:= 1 to High(TabNumIDs) do
    begin
      if TabNumIDs[i]<>TabNumID then
      begin
        N2:= i - 1;
        AddStaffNameData(N1, N2);
        N1:= i;
        TabNumID:= TabNumIDs[i];
      end;
    end;
    N2:= High(TabNumIDs);
    AddStaffNameData(N1, N2);
  end;

begin
  Result:= False;

  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;
  APostNames:= nil;

  AStoreIDs:= nil;
  ASizCounts:= nil;
  ASizDigUnits:= nil;
  ANomNums:= nil;
  ASizNames:= nil;
  ASizStrUnits:= nil;
  ASizLifes:= nil;

  AReceivingDates:= nil;
  AReceivingDocNames:= nil;
  AReceivingDocNums:= nil;
  ANotes:= nil;

  if not SIZStoreReturningLoad(ADocID, LogIDs, StoreIDs, Fs, Ns, Ps, TabNums,
                               PostNames, NomNums, SizNames, SizSTRUnits,
                               ReceivingDocNames, ReceivingDocNums, Notes,
                               TabNumIDs, Nums, Lifes, NameIDs, SizDIGUnits,
                               ReceivingDates) then Exit;
  GroupByStaffName;

  Result:= True;
end;

function TDataBase.SIZStaffListForPersonalCardsLoad(const AFilterValue: String;
                             const AListType, AOrderType: Byte;
                             const AIsDescOrder: Boolean;
                             out AStaffIDs, ATabNumIDs: TIntVector;
                             out AFs, ANs, APs, AGenders, ATabNums, APostNames: TStrVector): Boolean;
var
  SQLStr: String;
  i, PostID: Integer;
  PostName, Rank: String;
  Indexes: TIntVector;
begin
  AStaffIDs:= nil;
  ATabNumIDs:= nil;
  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  AGenders:= nil;
  ATabNums:= nil;
  APostNames:= nil;

  SQLStr:=
    'SELECT t1.StaffID, t1.TabNumID, t1.TabNum, ' +
           't2.Name, t2.Patronymic, t2.Family, t2.Gender ' +
    'FROM STAFFTABNUM t1 ' +
    'INNER JOIN STAFFMAIN t2 ON (t1.StaffID=t2.StaffID) ' +
    'WHERE (t1.TabNumID>0) ';

  if not SEmpty(AFilterValue) then
    SQLStr:= SQLStr + 'AND (t2.FullName LIKE :FilterValue) ';

  if AListType>0 then
  begin
    case AListType of
    1: SQLStr:= SQLStr + 'AND (t1.DismissDate>= :ADate) ';
    2: SQLStr:= SQLStr + 'AND (t1.DismissDate< :ADate) ';
    end;
  end;

  case AOrderType of
  0: if AIsDescOrder then
       SQLStr:= SQLStr + 'ORDER BY t2.Family DESC, t2.Name DESC, t2.Patronymic DESC'
     else
       SQLStr:= SQLStr + 'ORDER BY t2.Family, t2.Name, t2.Patronymic';
  1: if AIsDescOrder then
       SQLStr:= SQLStr + 'ORDER BY t1.TabNum DESC'
     else
       SQLStr:= SQLStr + 'ORDER BY t1.TabNum';
  2: SQLStr:= SQLStr + 'ORDER BY t2.Family, t2.Name, t2.Patronymic';
  end;

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamDT('ADate', Date);
  QParamStr('FilterValue', '%'+AFilterValue+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AStaffIDs, QFieldInt('StaffID'));
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(AGenders, GENDER_PICKS[QFieldInt('Gender')]);
      VAppend(ATabNums, QFieldStr('TabNum'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  //актуальные должности и разряды на текущую дату (или последние, если уволен)
  for i:= 0 to High(ATabNumIDs) do
  begin
    if not StaffPostForDate(ATabNumIDs[i], Date, PostID, PostName, Rank) then
      StaffPostLast(ATabNumIDs[i], PostID, PostName, Rank);
    VAppend(APostNames, PostName);
  end;

  //сортировка по наименованию должности
  if AOrderType<>2 then Exit;

  if AIsDescOrder then
    VSort(APostNames, Indexes, True)
  else
    VSort(APostNames, Indexes, False);

  ATabNumIDs:= VReplace(ATabNumIDs, Indexes);
  AFs:= VReplace(AFs, Indexes);
  ANs:= VReplace(ANs, Indexes);
  APs:= VReplace(APs, Indexes);
  ATabNums:= VReplace(ATabNums, Indexes);
  APostNames:= VReplace(APostNames, Indexes);
end;

function TDataBase.SIZPersonalCardListDataLoad(const ATabNumID: Integer;
                 out ACardIDs, AItemIDs, AItemPostIDs: TIntVector;
                 out ACardNums, APostNames, ANormNames, ANormNotes: TStrVector;
                 out ATabNumBDs, ATabNumEDs, ANormBDs, ANormEDs: TDateVector): Boolean;
begin
  Result:= False;

  ACardIDs:= nil;
  AItemIDs:= nil;
  AItemPostIDs:= nil;

  ACardNums:= nil;
  APostNames:= nil;
  ANormNames:= nil;
  ANormNotes:= nil;

  ATabNumBDs:= nil;
  ATabNumEDs:= nil;
  ANormBDs:= nil;
  ANormEDs:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.FirstDate, t1.LastDate, t2.PostName, t3.ItemID, t3.ItemPostID, ' +
           't5.NormName, t5.Note, t5.BeginDate, t5.EndDate, ' +
           't6.CardID, t6.CardNum ' +
    'FROM STAFFPOSTLOG t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'INNER JOIN SIZNORMITEMPOST t3 ON (t1.PostID=t3.PostID) ' +
    'INNER JOIN SIZNORMITEM t4 ON (t3.ItemID=t4.ItemID) '  +
    'INNER JOIN SIZNORM t5 ON (t4.NormID=t5.NormID) ' +
    'LEFT OUTER JOIN SIZCARDPERSONAL t6 ON ((t1.TabNumID=t6.TabNumID) AND (t3.ItemPostID=t6.ItemPostID)) ' +
    'WHERE (t1.TabNumID=:TabNumID) AND (t1.PostTemp=0) AND (' +
            SqlCROSS('t1.FirstDate', 't1.LastDate', 't5.BeginDate', 't5.EndDate') + ') ' +
    'ORDER BY t1.FirstDate DESC, t5.BeginDate DESC');
  QParamInt('TabNumID', ATabNumID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ACardIDs, QFieldInt('CardID'));
      VAppend(AItemIDs, QFieldInt('ItemID'));
      VAppend(AItemPostIDs, QFieldInt('ItemPostID'));

      VAppend(ACardNums, QFieldStr('CardNum'));
      VAppend(APostNames, QFieldStr('PostName'));
      VAppend(ANormNames, QFieldStr('NormName'));
      VAppend(ANormNotes, QFieldStr('Note'));

      VAppend(ATabNumBDs, QFieldDT('FirstDate'));
      VAppend(ATabNumEDs, QFieldDT('LastDate'));
      VAppend(ANormBDs, QFieldDT('BeginDate'));
      VAppend(ANormEDs, QFieldDT('EndDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZPersonalCardListLoad(const ATabNumID: Integer;
                 out ACardIDs, AItemIDs, AItemPostIDs: TIntVector;
                 out ACardNums, APostNames, ANormNames: TStrVector;
                 out ACardBDs, ACardEDs: TDateVector): Boolean;
var
  CardIDs, ItemIDs, ItemPostIDs: TIntVector;
  CardNums, PostNames, NormNames, NormNotes: TStrVector;
  TabNumBDs, TabNumEDs, NormBDs, NormEDs: TDateVector;
  i: Integer;
begin
  Result:= False;

  ACardIDs:= nil;
  AItemIDs:= nil;
  AItemPostIDs:= nil;

  ACardNums:= nil;
  APostNames:= nil;
  ANormNames:= nil;

  ACardBDs:= nil;
  ACardEDs:= nil;

  if ATabNumID<=0 then Exit; //еще не назначен таб номер

  Result:= SIZPersonalCardListDataLoad(ATabNumID, CardIDs, ItemIDs, ItemPostIDs,
                                  CardNums, PostNames, NormNames, NormNotes,
                                  TabNumBDs, TabNumEDs, NormBDs, NormEDs);
  if not Result then Exit;

  for i:= 0 to High(ItemIDs) do
  begin
    VAppend(ACardIDs, CardIDs[i]);
    VAppend(AItemIDs, ItemIDs[i]);
    VAppend(AItemPostIDs, ItemPostIDs[i]);

    VAppend(ACardNums, CardNums[i]);
    VAppend(APostNames, PostNames[i]);
    VAppend(ANormNames, SIZNormFullName(NormNames[i], NormNotes[i]));

    VAppend(ACardBDs, MaxDate(TabNumBDs[i], NormBDs[i]));
    VAppend(ACardEDs, MinDate(TabNumEDs[i], NormEDs[i]));
  end;

  for i:= 1 to High(ItemIDs) do
    ACardEDs[i]:= IncDay(ACardBDs[i-1], -1);

  i:= High(AItemIDs)-1;
  while i>=0 do
  begin
    if (AItemIDs[i]=AItemIDs[i+1]) and (APostNames[i]=APostNames[i+1]) then
    begin
      ACardEDs[i+1]:= ACardEDs[i];

      VDel(ACardIDs, i);
      VDel(AItemIDs, i);
      VDel(AItemPostIDs, i);

      VDel(ACardNums, i);
      VDel(APostNames, i);
      VDel(ANormNames, i);

      VDel(ACardBDs, i);
      VDel(ACardEDs, i);
    end;
    i:= i-1;
  end;
end;

function TDataBase.SIZPersonalCardForDateLoad(const ATabNumID: Integer;
                 const ADate: TDate;
                 out ACardID, AItemID, AItemPostID: Integer;
                 out ACardNum, APostName, ANormName: String;
                 out ACardBD, ACardED: TDate): Boolean;
var
  N: Integer;
  CardIDs, ItemIDs, ItemPostIDs: TIntVector;
  CardNums, PostNames, NormNames: TStrVector;
  CardBDs, CardEDs: TDateVector;
begin
  Result:= False;

  ACardID:= 0;
  AItemID:= 0;
  AItemPostID:= 0;
  ACardNum:= EmptyStr;
  APostName:= EmptyStr;
  ANormName:= EmptyStr;
  ACardBD:= 0;
  ACardED:= 0;

  if not SIZPersonalCardListLoad(ATabNumID, CardIDs, ItemIDs, ItemPostIDs,
                                 CardNums, PostNames, NormNames,
                                 CardBDs, CardEDs) then Exit;

  N:= VIndexOfDate(CardBDs, CardEDs, ADate);
  if N<0 then Exit;

  ACardID:= CardIDs[N];
  AItemID:= ItemIDs[N];
  AItemPostID:= ItemPostIDs[N];
  ACardNum:= CardNums[N];
  APostName:= PostNames[N];
  ANormName:= NormNames[N];
  ACardBD:= CardBDs[N];
  ACardED:= CardEDs[N];

  Result:= True;
end;

function TDataBase.SIZPrevCardListLoad(const ATabNumID: Integer;
                 const ACardBD: TDate;
                 out ACardIDs: TIntVector;
                 out ACardNums, APostNames, ANormNames: TStrVector;
                 out ACardBDs, ACardEDs: TDateVector): Boolean;
var
  i: Integer;
  CardIDs, ItemIDs, ItemPostIDs: TIntVector;
  CardNums, PostNames, NormNames: TStrVector;
  CardBDs, CardEDs: TDateVector;

begin
  Result:= False;

  ACardIDs:= nil;
  ACardNums:= nil;
  APostNames:= nil;
  ANormNames:= nil;
  ACardBDs:= nil;
  ACardEDs:= nil;

  Result:= SIZPersonalCardListLoad(ATabNumID, CardIDs, ItemIDs, ItemPostIDs,
                                   CardNums, PostNames, NormNames, CardBDs, CardEDs);

  if not Result then Exit;

  for i:= 0 to High(CardIDs) do
  begin
    if (CardIDs[i]=0) or (CompareDate(CardBDs[i], ACardBD)>=0) then continue;
    VAppend(ACardIDs, CardIDs[i]);
    VAppend(ACardNums, CardNums[i]);
    VAppend(APostNames, PostNames[i]);
    VAppend(ANormNames, NormNames[i]);
    VAppend(ACardBDs, CardBDs[i]);
    VAppend(ACardEDs, CardEDs[i]);
  end;
end;

function TDataBase.SIZPersonalCardDataLoad(const ATabNumID, ACardID: Integer;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANameIDs, ASizeTypes, ANormSIZTypes, ASizeIDs: TIntVector;
                                out ANormSizNames, AReceivingSizNames, AReceivingDocNames,
                                    AReturningDocNames, AWriteoffDocNames: TStrVector): Boolean;
var
  WhereStr: String;
begin
  Result:= False;

  ALogIDs:= nil;
  AReceivingDates:= nil;
  AReturningDates:= nil;
  ANameIDs:= nil;
  ASizeTypes:= nil;
  ANormSIZTypes:= nil;
  ASizeIDs:= nil;

  ANormSizNames:= nil;
  AReceivingSizNames:= nil;
  AReceivingDocNames:= nil;
  AReturningDocNames:= nil;
  AWriteoffDocNames:= nil;

  if ACardID>0 then
    WhereStr:= 'WHERE (t2.CardID = :CardID) '
  else
    WhereStr:= 'WHERE (t0.TabNumID = :TabNumID) AND (t2.NowInfoID=t2.ReceivingInfoID) ';

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.LogID, ' +
           't2.ReceivingDate, ' +
           't5.SizeID, ' +
           't6.NameID, t6.SizeType, t6.SizName AS ReceivingSizName, ' +
           't7.DocName AS ReceivingDocName, t7.DocNum AS ReceivingDocNum, t7.DocDate AS ReceivingDocDate, ' +
           't8.SizName AS NormSizName, t8.SIZType AS NormSIZType, ' +
           't10.DocName AS ReturningDocName, t10.DocNum AS ReturningDocNum, t10.DocDate AS ReturningDocDate, ' +
           't12.DocName AS WriteoffDocName, t12.DocNum AS WriteoffDocNum, t12.DocDate AS WriteoffDocDate ' +
    'FROM SIZCARDPERSONALLOGINFO t1 ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZCARDPERSONAL t0 ON (t2.CardID=t0.CardID) ' +
    'INNER JOIN SIZNORMSUBITEMINFO t3 ON (t2.ReceivingInfoID=t3.InfoID) ' +
    'INNER JOIN SIZSTORELOG t4 ON (t1.StoreID=t4.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t5 ON (t4.EntryID=t5.EntryID) ' +
    'INNER JOIN SIZNAME t6 ON (t5.NameID=t6.NameID) ' +
    'INNER JOIN SIZDOC t7 ON (t2.DocID=t7.DocID) ' +
    'INNER JOIN SIZNAME t8 ON (t3.NameID=t8.NameID) ' +
    'LEFT OUTER JOIN SIZSTAFFRETURN t9 ON (t1.StoreID=t9.StoreID) ' +
    'LEFT OUTER JOIN SIZDOC t10 ON (t9.DocID=t10.DocID) ' +
    'LEFT OUTER JOIN SIZSTOREWRITEOFF t11 ON (t9.ReturnStoreID=t11.StoreID) ' +
    'LEFT OUTER JOIN SIZDOC t12 ON (t11.DocID=t12.DocID) ' +
    WhereStr +
    //'WHERE (t2.CardID = :CardID) ' +
    //'WHERE (t2.CardID = :CardID) AND (t2.NowInfoID=t2.ReceivingInfoID) ' +
    'ORDER BY t2.ReceivingDate, t1.LogID, t5.NameID '
   );
  QParamInt('TabNumID', ATabNumID);
  QParamInt('CardID', ACardID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ALogIDs, QFieldInt64('LogID'));
      VAppend(AReceivingDates, QFieldDT('ReceivingDate'));
      VAppend(AReturningDates, QFieldDT('ReturningDocDate'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ASizeTypes, QFieldInt('SizeType'));
      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(ANormSIZTypes, QFieldInt('NormSIZType'));
      VAppend(ANormSizNames, QFieldStr('NormSizName'));
      VAppend(AReceivingSizNames, QFieldStr('ReceivingSizName'));
      VAppend(AReceivingDocNames, SIZDocFullName(QFieldStr('ReceivingDocName'),
                                                 QFieldStr('ReceivingDocNum'),
                                                 QFieldDT('ReceivingDocDate')));
      VAppend(AReturningDocNames, SIZDocFullName(QFieldStr('ReturningDocName'),
                                                 QFieldStr('ReturningDocNum'),
                                                 QFieldDT('ReturningDocDate')));
      VAppend(AWriteoffDocNames, SIZDocFullName(QFieldStr('WriteoffDocName'),
                                                QFieldStr('WriteoffDocNum'),
                                                QFieldDT('WriteoffDocDate')));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZPersonalCardSIZLoad(const ATabNumID, ACardID: Integer;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANormSIZTypes: TIntVector;
                                out ANormSizNames, AReceivingDocNames, AReturningDocNames: TStrVector;
                                out AReceivingSizNames, AWriteoffDocNames: TStrMatrix;
                                out ASizCounts, ASizeTypes: TIntMatrix): Boolean;
var
  LogIDs: TInt64Vector;
  ReceivingDates, ReturningDates: TDateVector;
  NameIDs, SizeTypes, NormSIZTypes, SizeIDs: TIntVector;
  NormSizNames, ReceivingSizNames: TStrVector;
  ReceivingDocNames, ReturningDocNames, WriteoffDocNames: TStrVector;

  function UniqueWriteoffDocNames(const N1, N2: Integer): String;
  var
    i: Integer;
    V: TStrVector;
  begin
    Result:= EmptyStr;

    V:= nil;
    for i:= N1 to N2 do
      if not SEmpty(WriteoffDocNames[i]) then
        VAppend(V, WriteoffDocNames[i]);
    if VIsNil(V) then Exit;

    V:= VUnique(V);

    Result:= V[0];
    for i:= 1 to High(V) do
      Result:= Result + SYMBOL_BREAK + V[i];
  end;

  procedure GroupByNameID(const ALogN1, ALogN2: Integer);
  var
    i, N1, N2, NameID: Integer;
    VSizNames, VWriteoffDocNames: TStrVector;
    VCounts, VSizeTypes: TIntVector;
  begin
    VSizNames:= nil;
    VCounts:= nil;
    VSizeTypes:= nil;
    VWriteoffDocNames:= nil;

    NameID:= NameIDs[ALogN1];
    N1:= ALogN1;
    for i:= ALogN1+1 to ALogN2 do
    begin
      if NameIDs[i]<>NameID then
      begin
        N2:= i - 1;
        VAppend(VSizNames, ReceivingSizNames[N1]);
        if NormSIZTypes[N1]=0 then //дерматологические
          VAppend(VCounts, VSum(SizeIDs, N1, N2))
        else //прочие
          VAppend(VCounts, N2-N1+1);
        VAppend(VSizeTypes, SizeTypes[N1]);
        VAppend(VWriteoffDocNames, UniqueWriteoffDocNames(N1,N2));
        N1:= i;
        NameID:= NameIDs[i];
      end;
    end;
    N2:= ALogN2;
    VAppend(VSizNames, ReceivingSizNames[N1]);
    if NormSIZTypes[N1]=0 then //дерматологические
      VAppend(VCounts, VSum(SizeIDs, N1, N2))
    else //прочие
      VAppend(VCounts, N2-N1+1);
    VAppend(VSizeTypes, SizeTypes[N1]);
    VAppend(VWriteoffDocNames, UniqueWriteoffDocNames(N1,N2));

    MAppend(AReceivingSizNames, VSizNames);
    MAppend(ASizCounts, VCounts);
    MAppend(ASizeTypes, VSizeTypes);
    MAppend(AWriteoffDocNames, VWriteoffDocNames);
  end;

  procedure AddLogData(const AInd1, AInd2: Integer);
  begin
    VAppend(ALogIDs, LogIDs[AInd1]);
    VAppend(AReceivingDates, ReceivingDates[AInd1]);
    VAppend(AReturningDates, ReturningDates[AInd1]);
    VAppend(ANormSIZTypes, NormSIZTypes[AInd1]);
    VAppend(ANormSizNames, NormSizNames[AInd1]);
    VAppend(AReceivingDocNames, ReceivingDocNames[AInd1]);
    VAppend(AReturningDocNames, ReturningDocNames[AInd1]);

    //группировка по наименованию СИЗ
    GroupByNameID(AInd1, AInd2);
  end;

  procedure GroupByLogID;
  var
    i, N1, N2: Integer;
    LogID: Int64;
  begin
    LogID:= LogIDs[0];
    N1:= 0;
    for i:= 1 to High(LogIDs) do
    begin
      if LogIDs[i]<>LogID then
      begin
        N2:= i - 1;
        AddLogData(N1, N2);
        N1:= i;
        LogID:= LogIDs[i];
      end;
    end;
    N2:= High(LogIDs);
    AddLogData(N1, N2);
  end;

begin
  Result:= False;

  ALogIDs:= nil;
  AReceivingDates:= nil;
  AReturningDates:= nil;
  ASizeTypes:= nil;
  ANormSIZTypes:= nil;
  ANormSizNames:= nil;
  AReceivingDocNames:= nil;
  AReturningDocNames:= nil;
  AWriteoffDocNames:= nil;
  AReceivingSizNames:= nil;
  ASizCounts:= nil;

  if (ATabNumID=0) and (ACardID=0) then Exit;

  if not SIZPersonalCardDataLoad(ATabNumID, ACardID, LogIDs,
                                 ReceivingDates, ReturningDates,
                                 NameIDs, SizeTypes, NormSIZTypes, SizeIDs,
                                 NormSizNames, ReceivingSizNames,
                                 ReceivingDocNames, ReturningDocNames,
                                 WriteoffDocNames) then Exit;

  GroupByLogID;

  Result:= True;
end;

function TDataBase.SIZPrevCardDataLoad(const ACardID, ASIZType: Integer;
                                out ALogIDs, AStoreIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANameIDs, ANums, ALifes, ANormSIZTypes, ASizeIDs,
                                    AReceivingDocIDs, AReceivingInfoIDs: TIntVector;
                                out AReceivingSizNames, AReceivingDocNames, ANormSizNames: TStrVector): Boolean;
var
  WhereStr: String;
begin
  Result:= False;

  ALogIDs:= nil;
  AStoreIDs:= nil;
  AReceivingDates:= nil;
  AReturningDates:= nil;
  ANameIDs:= nil;
  ANums:= nil;
  ALifes:= nil;
  ANormSIZTypes:= nil;
  ASizeIDs:= nil;
  AReceivingDocIDs:= nil;
  AReceivingInfoIDs:= nil;
  AReceivingSizNames:= nil;
  AReceivingDocNames:= nil;
  ANormSizNames:= nil;

  WhereStr:= 'WHERE (t2.CardID = :CardID) AND (t2.NowInfoID=t2.ReceivingInfoID) ';
  if ASIZType>=0 then
    WhereStr:= WhereStr + 'AND (t6.SIZType = :SIZType) ';

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.LogID, t1.StoreID, ' +
           't2.ReceivingDate, t2.ReceivingInfoID, ' +
           't3.Num, t3.Life, ' +
           't5.SizeID, ' +
           't6.NameID, t6.SizName, ' +
           't7.DocID, t7.DocName, t7.DocNum, t7.DocDate, ' +
           't8.SizName as NormSizName, t8.SIZType AS NormSIZType, ' +
           't10.DocDate as ReturningDate ' +
    'FROM SIZCARDPERSONALLOGINFO t1 ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZNORMSUBITEMINFO t3 ON (t2.ReceivingInfoID=t3.InfoID) ' +
    'INNER JOIN SIZSTORELOG t4 ON (t1.StoreID=t4.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t5 ON (t4.EntryID=t5.EntryID) ' +
    'INNER JOIN SIZNAME t6 ON (t5.NameID=t6.NameID) ' +
    'INNER JOIN SIZDOC t7 ON (t2.DocID=t7.DocID) ' +
    'INNER JOIN SIZNAME t8 ON (t3.NameID=t8.NameID) ' +
    'LEFT OUTER JOIN SIZSTAFFRETURN t9 ON (t1.StoreID=t9.StoreID) ' +
    'LEFT OUTER JOIN SIZDOC t10 ON (t9.DocID=t10.DocID) ' +
    WhereStr +
    'ORDER BY t2.ReceivingDate DESC, t1.LogID, t5.NameID '
   );
  QParamInt('CardID', ACardID);
  QParamInt('SIZType', ASIZType);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ALogIDs, QFieldInt64('LogID'));
      VAppend(AStoreIDs, QFieldInt64('StoreID'));
      VAppend(AReceivingSizNames, QFieldStr('SizName'));
      VAppend(ANormSizNames, QFieldStr('NormSizName'));
      VAppend(AReceivingDates, QFieldDT('ReceivingDate'));
      VAppend(AReturningDates, QFieldDT('ReturningDate'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ANums, QFieldInt('Num'));
      VAppend(ALifes, QFieldInt('Life'));
      VAppend(ANormSIZTypes, QFieldInt('NormSIZType'));
      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(AReceivingDocIDs, QFieldInt('DocID'));
      VAppend(AReceivingInfoIDs, QFieldInt('ReceivingInfoID'));
      VAppend(AReceivingDocNames, SIZDocFullName(QFieldStr('DocName'), QFieldStr('DocNum'), QFieldDT('DocDate')));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZPrevCardSIZLoad(const ACardID, ASIZType: Integer;
                                const AReportDate: TDate;
                                out AReceivingDocIDs, AReceivingInfoIDs: TIntVector;
                                out ANormSizNames: TStrVector;
                                out AReceivingDates, AWriteoffDates: TDateMatrix;
                                out AStoreIDs: TInt64Matrix;
                                out AReceivingDocNames, AReceivingSizNames: TStrMatrix;
                                out ASizCounts: TIntMatrix): Boolean;
var
  LogIDs, StoreIDs: TInt64Vector;
  ReceivingDates, ReturnDates: TDateVector;
  NameIDs, Nums, Lifes, NormSIZTypes, SizeIDs: TIntVector;
  ReceivingDocIDs, ReceivingInfoIDs: TIntVector;
  SizNames, ReceivingDocNames, NormSizNames: TStrVector;

  IsInfoFreshExists: Boolean;

  OutReceivingDocIDs, OutReceivingInfoIDs: TIntVector;
  OutNormSizNames: TStrVector;
  OutReceivingDates, OutWriteoffDates: TDateMatrix;
  OutStoreIDs: TInt64Matrix;
  OutReceivingDocNames, OutSizNames: TStrMatrix;
  OutSizCounts: TIntMatrix;

  procedure GroupByNameID(const ALogN1, ALogN2: Integer);
  var
    i, N1, N2, NameID: Integer;
    VSizNames, VReceivingDocNames: TStrVector;
    VCounts: TIntVector;
    VReceivingDates: TDateVector;
  begin
    VSizNames:= nil;
    VReceivingDocNames:= nil;
    VCounts:= nil;
    VReceivingDates:= nil;

    NameID:= NameIDs[ALogN1];
    N1:= ALogN1;
    for i:= ALogN1+1 to ALogN2 do
    begin
      if NameIDs[i]<>NameID then
      begin
        N2:= i - 1;
        VAppend(VSizNames, SizNames[N1]);
        VAppend(VReceivingDocNames, ReceivingDocNames[N1]);
        VAppend(VReceivingDates, ReceivingDates[N1]);
        if NormSIZTypes[N1]=0 then //дерматологические
          VAppend(VCounts, VSum(SizeIDs, N1, N2))
        else //прочие
          VAppend(VCounts, N2-N1+1);
        N1:= i;
        NameID:= NameIDs[i];
      end;
    end;
    N2:= ALogN2;
    VAppend(VSizNames, SizNames[N1]);
    VAppend(VReceivingDocNames, ReceivingDocNames[N1]);
    VAppend(VReceivingDates, ReceivingDates[N1]);
    if NormSIZTypes[N1]=0 then //дерматологические
      VAppend(VCounts, VSum(SizeIDs, N1, N2))
    else //прочие
      VAppend(VCounts, N2-N1+1);

    MAppend(OutSizNames, VSizNames);
    MAppend(OutReceivingDocNames, VReceivingDocNames);
    MAppend(OutReceivingDates, VReceivingDates);
    MAppend(OutSizCounts, VCounts);
  end;

  procedure AddLogData(const AInd1, AInd2: Integer);
  var
    ReceivingCount: Integer;
    WriteoffDate: TDate;
    VWriteoffDates: TDateVector;
  begin
    VAppend(OutReceivingDocIDs, ReceivingDocIDs[AInd1]);
    VAppend(OutReceivingInfoIDs, ReceivingInfoIDs[AInd1]);
    VAppend(OutNormSizNames, NormSizNames[AInd1]);
    MAppend(OutStoreIDs, VCut(StoreIDs, AInd1, AInd2));
    //группировка по наименованию СИЗ и заполнение количества
    GroupByNameID(AInd1, AInd2);
    //определение даты списания
    ReceivingCount:= VSum(OutSizCounts[High(OutSizCounts)]);
    WriteoffDate:= SIZWriteoffDate(ReceivingDates[AInd1], ReturnDates[AInd1],
                                   ReceivingCount, Nums[AInd1], Lifes[AInd1]);
    VDim(VWriteoffDates{%H-}, Length(OutSizCounts[High(OutSizCounts)]), WriteoffDate);
    MAppend(OutWriteoffDates, VWriteoffDates);

    if CompareDate(WriteoffDate, AReportDate)>=0 then
      IsInfoFreshExists:= True;
  end;

  procedure GroupByLogID;
  var
    i, N1, N2: Integer;
    LogID: Int64;
  begin
    OutReceivingDocIDs:= nil;
    OutReceivingInfoIDs:= nil;
    OutReceivingDates:= nil;
    OutWriteoffDates:= nil;
    OutReceivingDocNames:= nil;
    OutStoreIDs:= nil;
    OutSizNames:= nil;
    OutSizCounts:= nil;

    LogID:= LogIDs[0];
    N1:= 0;
    for i:= 1 to High(LogIDs) do
    begin
      if LogIDs[i]<>LogID then
      begin
        N2:= i - 1;
        AddLogData(N1, N2);
        N1:= i;
        LogID:= LogIDs[i];
      end;
    end;
    N2:= High(LogIDs);
    AddLogData(N1, N2);
  end;

  procedure DataInclude(const AIndex: Integer);
  begin
    VAppend(AReceivingDocIDs, OutReceivingDocIDs[AIndex]);
    VAppend(AReceivingInfoIDs, OutReceivingInfoIDs[AIndex]);
    VAppend(ANormSizNames, OutNormSizNames[AIndex]);

    MAppend(AReceivingDates, OutReceivingDates[AIndex]);
    MAppend(AReceivingDocNames, OutReceivingDocNames[AIndex]);
    MAppend(AWriteoffDates, OutWriteoffDates[AIndex]);
    MAppend(AReceivingSizNames, OutSizNames[AIndex]);
    MAppend(ASizCounts, OutSizCounts[AIndex]);
    MAppend(AStoreIDs, OutStoreIDs[AIndex]);
  end;

  procedure FreshLoad; //отбираем только непросроченное
  var
    i: Integer;
  begin
    for i:= 0 to High(OutWriteoffDates) do
      if CompareDate(OutWriteoffDates[i, 0], AReportDate)>=0 then
        DataInclude(i);
  end;

  procedure LastWriteoffLoad; //отбираем только списанное последним
  var
    i: Integer;
    D: TDate;
  begin
    D:= MMaxDate(OutWriteoffDates);
    for i:= 0 to High(OutWriteoffDates) do
      if SameDate(OutWriteoffDates[i, 0], D) then
        DataInclude(i);
  end;

begin
  Result:= False;

  IsInfoFreshExists:= False;

  AReceivingDocIDs:= nil;
  AReceivingInfoIDs:= nil;
  AReceivingDates:= nil;
  AWriteoffDates:= nil;
  AReceivingDocNames:= nil;
  ANormSizNames:= nil;
  AStoreIDs:= nil;
  AReceivingSizNames:= nil;
  ASizCounts:= nil;

  if not SIZPrevCardDataLoad(ACardID, ASIZType,
                             LogIDs, StoreIDs, ReceivingDates, ReturnDates,
                             NameIDs, Nums, Lifes, NormSIZTypes, SizeIDs,
                             ReceivingDocIDs, ReceivingInfoIDs,
                             SizNames, ReceivingDocNames, NormSizNames) then Exit;

  GroupByLogID;

  if IsInfoFreshExists then
    FreshLoad
  else
    LastWriteoffLoad;

  Result:= True;
end;

function TDataBase.SIZPersonalCardAdd(out ACardID: Integer;
                                const ACardNum: String;
                                const ATabNumID, AItemPostID: Integer;
                                const ACommit: Boolean = True): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT('SIZCARDPERSONAL', ['CardNum', 'TabNumID', 'ItemPostID'])
    );

    QParamStr('CardNum', ACardNum);
    QParamInt('TabNumID', ATabNumID);
    QParamInt('ItemPostID', AItemPostID);
    QExec;

    //получение ID сделанной записи
    ACardID:= LastWritedInt32ID('SIZCARDPERSONAL');

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZPersonalCardUpdate(const ACardID: Integer;
                                         const ACardNum: String): Boolean;
begin
  Result:= UpdateByInt32ID('SIZCARDPERSONAL', 'CardNum', 'CardID', ACardID, ACardNum);
end;

procedure TDataBase.SIZStatusSubItemLoad(const AReportDate: TDate;
                                const ACardID, AWriteoffType: Integer;
                                const AInfoIDs: TIntVector;
                                var AStatusSubItem: TStatusSubItem);
var
  i, N: Integer;

  LogIDs: TInt64Vector;
  ReceivingDates, WriteoffDates: TDateVector;
  SizNames: TStrMatrix;
  SizCounts: TIntMatrix;
  IsFreshExists: Boolean;

  OutLogIDs: TInt64Matrix;
  OutReceivingDates, OutWriteoffDates: TDateMatrix;
  OutSizNames: TStrMatrix3D;
  OutSizCounts: TIntMatrix3D;
  OutIsFreshExists: TBoolVector;

  procedure DataInclude(const AIndex1, AIndex2: Integer);
  begin
    VAppend(AStatusSubItem.Info.LogIDs[AIndex1], OutLogIDs[AIndex1, AIndex2]);
    VAppend(AStatusSubItem.Info.ReceivingDates[AIndex1], OutReceivingDates[AIndex1, AIndex2]);
    VAppend(AStatusSubItem.Info.WriteoffDates[AIndex1], OutWriteoffDates[AIndex1, AIndex2]);
    MAppend(AStatusSubItem.Info.SizNames[AIndex1], OutSizNames[AIndex1, AIndex2]);
    MAppend(AStatusSubItem.Info.SizCounts[AIndex1], OutSizCounts[AIndex1, AIndex2]);
  end;

  procedure FreshLoad; //отбираем только непросроченное
  var
    i, j: Integer;
  begin
    for i:= 0 to High(OutLogIDs) do
    begin
      if not OutIsFreshExists[i] then continue;

      for j:= 0 to High(OutLogIDs[i]) do
        if CompareDate(OutWriteoffDates[i, j], AReportDate)>=0 then
          DataInclude(i, j);
    end;
  end;

  procedure LastWriteoffLoad; //отбираем только списанное последним
  var
    i, j: Integer;
    D: TDate;
  begin
    D:= MMaxDate(OutWriteoffDates);
    for i:= 0 to High(OutLogIDs) do
      for j:= 0 to High(OutLogIDs[i]) do
        if SameDate(OutWriteoffDates[i, j], D) then
          DataInclude(i, j);
  end;

begin
  N:= Length(AInfoIDs);

  VDim(OutIsFreshExists{%H-}, N);
  MDim(OutLogIDs{%H-}, N);
  MDim(OutReceivingDates{%H-}, N);
  MDim(OutWriteoffDates{%H-}, N);
  MDim(OutSizNames{%H-}, N);
  MDim(OutSizCounts{%H-}, N);

  for i:= 0 to N-1 do
  begin
    SIZStatusInfoLoad(AReportDate, ACardID, AWriteoffType, AInfoIDs[i],
                      IsFreshExists, LogIDs, ReceivingDates, WriteoffDates,
                      SizNames, SizCounts);

    OutIsFreshExists[i]:= IsFreshExists;
    OutLogIDs[i]:= VCut(LogIDs);
    OutReceivingDates[i]:= VCut(ReceivingDates);
    OutWriteoffDates[i]:= VCut(WriteoffDates);
    OutSizNames[i]:= MCut(SizNames);
    OutSizCounts[i]:= MCut(SizCounts);
  end;

  IsFreshExists:= VIsTrue(OutIsFreshExists);
  AStatusSubItem.IsFreshExists:= IsFreshExists;
  if IsFreshExists then
    FreshLoad
  else
    LastWriteoffLoad;
end;

function TDataBase.SIZStatusInfoDataLoad(const ACardID, AWriteoffType, AInfoID: Integer;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AReturningDates: TDateVector;
                                out ANameIDs, ANums, ALifes, ASizeIDs, ASizTypes: TIntVector;
                                out ASizNames: TStrVector): Boolean;
var
  InfoField: String;
begin
  Result:= False;

  ALogIDs:= nil;
  AReceivingDates:= nil;
  AReturningDates:= nil;
  ANameIDs:= nil;
  ANums:= nil;
  ALifes:= nil;
  ASizeIDs:= nil;
  ASizTypes:= nil;
  ASizNames:= nil;

  if AWriteoffType=0 then
    InfoField:= 'ReceivingInfoID'
  else
    InfoField:= 'NowInfoID';

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.LogID, t2.ReceivingDate,  ' +
           't3.Num, t3.Life, ' +
           't5.SizeID, ' +
           't6.NameID, t6.SizName, t6.SizType, ' +
           't8.DocDate as ReturnDate ' +
    'FROM SIZCARDPERSONALLOGINFO t1 ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZNORMSUBITEMINFO t3 ON (t2.' + InfoField + '=t3.InfoID) ' +
    'INNER JOIN SIZSTORELOG t4 ON (t1.StoreID=t4.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t5 ON (t4.EntryID=t5.EntryID) ' +
    'INNER JOIN SIZNAME t6 ON (t5.NameID=t6.NameID) ' +
    'LEFT OUTER JOIN SIZSTAFFRETURN t7 ON (t1.StoreID=t7.StoreID) ' +
    'LEFT OUTER JOIN SIZDOC t8 ON (t7.DocID=t8.DocID) ' +
    'WHERE (t2.CardID = :CardID) AND (t2.NowInfoID = :InfoID) ' +
    'ORDER BY t2.ReceivingDate DESC, t1.LogID, t5.NameID '
   );
  QParamInt('CardID', ACardID);
  QParamInt('InfoID', AInfoID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ALogIDs, QFieldInt64('LogID'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(AReceivingDates, QFieldDT('ReceivingDate'));
      VAppend(AReturningDates, QFieldDT('ReturnDate'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ANums, QFieldInt('Num'));
      VAppend(ALifes, QFieldInt('Life'));
      VAppend(ASizeIDs, QFieldInt('SizeID'));
      VAppend(ASizTypes, QFieldInt('SizType'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

procedure TDataBase.SIZStatusInfoLoad(const AReportDate: TDate;
                                const ACardID, AWriteoffType, AInfoID: Integer;
                                out AIsInfoFreshExists: Boolean;
                                out ALogIDs: TInt64Vector;
                                out AReceivingDates, AWriteoffDates: TDateVector;
                                out ASizNames: TStrMatrix;
                                out ASizCounts: TIntMatrix);
var
  LogIDs: TInt64Vector;
  ReceivingDates, ReturnDates: TDateVector;
  NameIDs, Nums, Lifes, SizeIDs, SIZTypes: TIntVector;
  SizNames: TStrVector;

  OutLogIDs: TInt64Vector;
  OutReceivingDates, OutWriteoffDates: TDateVector;
  OutSizNames: TStrMatrix;
  OutSizCounts: TIntMatrix;

  procedure GroupByNameID(const ALogN1, ALogN2: Integer);
  var
    i, N1, N2, NameID: Integer;
    VNames: TStrVector;
    VCounts: TIntVector;
  begin
    VNames:= nil;
    VCounts:= nil;

    NameID:= NameIDs[ALogN1];
    N1:= ALogN1;
    for i:= ALogN1+1 to ALogN2 do
    begin
      if NameIDs[i]<>NameID then
      begin
        N2:= i - 1;
        VAppend(VNames, SizNames[N1]);
        if SIZTypes[N1]=0 then //дерматологические
          VAppend(VCounts, VSum(SizeIDs, N1, N2))
        else //прочие
          VAppend(VCounts, N2-N1+1);
        N1:= i;
        NameID:= NameIDs[i];
      end;
    end;
    N2:= ALogN2;
    VAppend(VNames, SizNames[N1]);
    if SIZTypes[N1]=0 then //дерматологические
      VAppend(VCounts, VSum(SizeIDs, N1, N2))
    else //прочие
      VAppend(VCounts, N2-N1+1);

    MAppend(OutSizNames, VNames);
    MAppend(OutSizCounts, VCounts);
  end;

  procedure AddLogData(const AInd1, AInd2: Integer);
  var
    ReceivingCount: Integer;
    WriteoffDate: TDate;
  begin
    VAppend(OutLogIDs, LogIDs[AInd1]);
    VAppend(OutReceivingDates, ReceivingDates[AInd1]);
    //группировка по наименованию СИЗ и заполнение количества
    GroupByNameID(AInd1, AInd2);
    //определение даты списания
    ReceivingCount:= VSum(OutSizCounts[High(OutSizCounts)]);
    WriteoffDate:= SIZWriteoffDate(ReceivingDates[AInd1], ReturnDates[AInd1],
                                   ReceivingCount, Nums[AInd1], Lifes[AInd1]);
    VAppend(OutWriteoffDates, WriteoffDate);

    if CompareDate(WriteoffDate, AReportDate)>=0 then
      AIsInfoFreshExists:= True;
  end;

  procedure GroupByLogID;
  var
    i, N1, N2: Integer;
    LogID: Int64;
  begin
    OutLogIDs:= nil;
    OutReceivingDates:= nil;
    OutWriteoffDates:= nil;
    OutSizNames:= nil;
    OutSizCounts:= nil;

    LogID:= LogIDs[0];
    N1:= 0;
    for i:= 1 to High(LogIDs) do
    begin
      if LogIDs[i]<>LogID then
      begin
        N2:= i - 1;
        AddLogData(N1, N2);
        N1:= i;
        LogID:= LogIDs[i];
      end;
    end;
    N2:= High(LogIDs);
    AddLogData(N1, N2);
  end;

  procedure DataInclude(const AIndex: Integer);
  begin
    VAppend(ALogIDs, OutLogIDs[AIndex]);
    VAppend(AReceivingDates, OutReceivingDates[AIndex]);
    VAppend(AWriteoffDates, OutWriteoffDates[AIndex]);
    MAppend(ASizNames, OutSizNames[AIndex]);
    MAppend(ASizCounts, OutSizCounts[AIndex]);
  end;

  procedure FreshLoad; //отбираем только непросроченное
  var
    i: Integer;
  begin
    for i:= 0 to High(OutLogIDs) do
      if CompareDate(OutWriteoffDates[i], AReportDate)>=0 then
        DataInclude(i);
  end;

  procedure LastWriteoffLoad; //отбираем только списанное последним
  var
    i: Integer;
    D: TDate;
  begin
    D:= VMaxDate(OutWriteoffDates);
    for i:= 0 to High(OutLogIDs) do
      if SameDate(OutWriteoffDates[i], D) then
        DataInclude(i);
  end;

begin
  AIsInfoFreshExists:= False;

  ALogIDs:= nil;
  AReceivingDates:= nil;
  AWriteoffDates:= nil;
  ASizNames:= nil;
  ASizCounts:= nil;

  if not SIZStatusInfoDataLoad(ACardID, AWriteoffType, AInfoID,
                               LogIDs, ReceivingDates, ReturnDates,
                               NameIDs, Nums, Lifes, SizeIDs, SIZTypes,
                               SizNames) then Exit;

  GroupByLogID;

  if AIsInfoFreshExists then
    FreshLoad
  else
    LastWriteoffLoad;
end;

function TDataBase.SIZStatusLoad(const ATabNumID, ACardID, AWriteoffType: Integer;
                           const AReportDate: TDate;
                           const ANormSubItems: TNormSubItems;
                           var AStatusSubItems: TStatusSubItems): Boolean;
var
  i, StaffID: Integer;
  StaffSizes: TSIZStaffSizeIndexes;
  SizeIDs, HeightIDs: TIntVector;
  StatusSubItem: TStatusSubItem;
begin
  Result:= False;
  //очищаем
  StatusSubItemsClear(AStatusSubItems{%H-});

  //загружаем ID размеров сотрудника
  SIZStaffSizeIndexesClear(StaffSizes{%H-});
  if StaffIDByTabNumID(ATabNumID, StaffID) then
    SIZStaffSizeLoad(StaffID, StaffSizes);

  //пробегаем по всем строкам пункта типовой нормы
  for i:=0 to High(ANormSubItems) do
  begin
    //определяем размеры
    SIZStatusSizeLoad(ATabNumID, ANormSubItems[i].Info.InfoIDs,
                      ANormSubItems[i].Info.SizeTypes, StaffSizes, SizeIDs, HeightIDs);

    //заполняем размеры для этой строки StatusSubItem
    StatusSubItemNew(StatusSubItem{%H-}, SizeIDs, HeightIDs);
    //заполняем данные статуса
    SIZStatusSubItemLoad(AReportDate, ACardID, AWriteoffType,
                      ANormSubItems[i].Info.InfoIDs, StatusSubItem);
    //добавляем StatusSubItem в вектор
    StatusSubItemsAdd(AStatusSubItems, StatusSubItem);
  end;

  Result:= True;
end;

procedure TDataBase.SIZPersonalCardLogWrite(out ALogID: Int64;
                 const ACardID, ANowInfoID, AReceivingInfoID, ADocID: Integer;
                 const AReceivingDate: TDate);
begin
  QSetQuery(FQuery);
  QSetSQL(
    sqlINSERT('SIZCARDPERSONALLOG', ['CardID', 'ReceivingDate', 'DocID',
                                     'ReceivingInfoID', 'NowInfoID'])
  );
  QParamInt('CardID', ACardID);
  QParamDT('ReceivingDate', AReceivingDate);
  QParamInt('DocID', ADocID);
  QParamInt('ReceivingInfoID', AReceivingInfoID);
  QParamInt('NowInfoID', ANowInfoID);
  QExec;

  //получение ID сделанной записи
  ALogID:= LastWritedInt64ID('SIZCARDPERSONALLOG');
end;

procedure TDataBase.SIZPersonalCardLogInfoWrite(const ALogID: Int64;
                                      const AStoreIDs: TInt64Vector);
var
  i: Integer;
begin
  QSetQuery(FQuery);
  QSetSQL(
    sqlINSERT('SIZCARDPERSONALLOGINFO', ['LogID', 'StoreID'])
  );
  QParamInt64('LogID', ALogID);
  for i:= 0 to High(AStoreIDs) do
  begin
    QParamInt64('StoreID', AStoreIDs[i]);
    QExec;
  end;
end;

function TDataBase.SIZReceivingWrite(const ACardID, ATabNumID, AItemPostID,
                                     AReceivingInfoID, ANowInfoID, ADocID: Integer;
                            const AStoreIDs: TInt64Vector;
                            const AReceivingDate: TDate): Boolean;
var
  CardID: Integer;
  LogID: Int64;
begin
  Result:= False;

  QSetQuery(FQuery);
  try
    //если карточки еще нет, то записываем
    CardID:= ACardID;
    if CardID=0 then
      if not SIZPersonalCardAdd(CardID, EmptyStr, ATabNumID,
                                AItemPostID, False{no commit}) then Exit;

    //записываем выдачу в таблицы логов личной карточки
    SIZPersonalCardLogWrite(LogID, CardID, ANowInfoID, AReceivingInfoID,
                            ADocID, AReceivingDate);
    SIZPersonalCardLogInfoWrite(LogID, AStoreIDs);

    //меняем статус СИЗ на складе на "занято"
    UpdateByInt64ID('SIZSTORELOG', 'IsBusy', 'StoreID', AStoreIDs, 1{занято}, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZReceivingCancel(const ALogID: Int64; const ACommit: Boolean = True): Boolean;
var
  StoreIDs: TInt64Vector;

  function ItWasFreeSIZ: Boolean;
  begin
    QSetSQL(
      'SELECT ReceivingInfoID, NowInfoID ' +
      'FROM SIZCARDPERSONALLOG ' +
      'WHERE LogID=:LogID'
    );
    QParamInt64('LogID', ALogID);
    QOpen;
    Result:= QFieldInt('ReceivingInfoID')=QFieldInt('NowInfoID');
    QClose;
  end;

begin
  Result:= False;
  if ItWasFreeSIZ then  //если была выдача сиз со склада - полностью отменяем выдачу
  begin
    StoreIDs:= ValuesInt64ByInt64ID('SIZCARDPERSONALLOGINFO', 'StoreID', 'LogID', ALogID, True{Unique});
    if not VIsNil(StoreIDs) then
      Result:= SIZStoreReceivingCancel(StoreIDs, ACommit)
  end
  else //был учет ранее выданных сиз - просто удаляем строку из лога
    Result:= Delete('SIZCARDPERSONALLOG', 'LogID', ALogID, ACommit);
end;

function TDataBase.SIZReceivingNextWrite(const ACardID, ATabNumID, AItemPostID, ANowInfoID: Integer;
                            const AReceivingInfoIDs, ADocIDs: TIntVector;
                            const AReceivingDates: TDateVector;
                            const AStoreIDs: TInt64Matrix): Boolean;
var
  i, CardID: Integer;
  LogID: Int64;
begin
  Result:= False;

  QSetQuery(FQuery);
  try
    //если карточки еще нет, то записываем
    CardID:= ACardID;
    if CardID=0 then
      if not SIZPersonalCardAdd(CardID, EmptyStr, ATabNumID,
                                AItemPostID, False{no commit}) then Exit;

    //записываем выдачу в таблицы логов личной карточки
    for i:= 0 to High(ADocIDs) do
    begin
      SIZPersonalCardLogWrite(LogID, CardID, ANowInfoID, AReceivingInfoIDs[i],
                              ADocIDs[i], AReceivingDates[i]);
      SIZPersonalCardLogInfoWrite(LogID, AStoreIDs[i]);
    end;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZReturningDataLoad(const ALogID: Int64;
                            out AStoreIDs: TInt64Matrix;
                            out ANomNums: TStrVector;
                            out ANameIDs, ASizeIDs, AHeightIDs: TIntVector): Boolean;
var
  StoreIDs: TInt64Vector;
  NomNums: TStrVector;
  NameIDs, SizeIDs, HeightIDs: TIntVector;

  procedure AddData(const AInd1, AInd2: Integer);
  begin
    VAppend(ANomNums, NomNums[AInd1]);
    VAppend(ANameIDs, NameIDs[AInd1]);
    VAppend(ASizeIDs, SizeIDs[AInd1]);
    VAppend(AHeightIDs, HeightIDs[AInd1]);
    MAppend(AStoreIDs, VCut(StoreIDs, AInd1, AInd2));
  end;

  procedure GroupData;
  var
    i, N1, N2: Integer;
    NomNum: String;
    NameID, SizeID, HeightID: Integer;
  begin
    NomNum:= NomNums[0];
    NameID:= NameIDs[0];
    SizeID:= SizeIDs[0];
    HeightID:= HeightIDs[0];
    N1:= 0;
    for i:= 1 to High(NameIDs) do
    begin
      if (NameIDs[i]<>NameID) or (SizeIDs[i]<>SizeID) or
         (HeightIDs[i]<>HeightID) or (not SSame(NomNum, NomNums[i])) then
      begin
        N2:= i - 1;
        AddData(N1, N2);
        N1:= i;
        NomNum:= NomNums[i];
        NameID:= NameIDs[i];
        SizeID:= SizeIDs[i];
        HeightID:= HeightIDs[i];
      end;
    end;
    N2:= High(NameIDs);
    AddData(N1, N2);
  end;

begin
  Result:= False;

  AStoreIDs:= nil;
  ANomNums:= nil;
  ANameIDs:= nil;
  ASizeIDs:= nil;
  AHeightIDs:= nil;

  StoreIDs:= nil;
  NomNums:= nil;
  NameIDs:= nil;
  SizeIDs:= nil;
  HeightIDs:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.StoreID, t3.NomNum, t3.NameID, t3.SizeID, t3.HeightID ' +
    'FROM SIZCARDPERSONALLOGINFO t1 ' +
    'INNER JOIN SIZSTORELOG t2 ON (t1.StoreID=t2.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t3 ON (t2.EntryID=t3.EntryID) ' +
    'WHERE (t1.LogID = :LogID) ' +
    'ORDER BY t3.NomNum, t3.NameID, t3.SizeID, t3.HeightID '
   );
  QParamInt64('LogID', ALogID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(StoreIDs, QFieldInt64('StoreID'));
      VAppend(NomNums, QFieldStr('NomNum'));
      VAppend(NameIDs, QFieldInt('NameID'));
      VAppend(SizeIDs, QFieldInt('SizeID'));
      VAppend(HeightIDs, QFieldInt('HeightID'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not Result then Exit;

  GroupData;
end;



function TDataBase.SIZReturningWrite(const ADocID: Integer;
                                     const ALogID: Int64;
                                     const ANote: String): Boolean;
var
  i: Integer;
  EntryID: Int64;
  NewStoreIDs: TInt64Vector;

  StoreIDs: TInt64Matrix;
  NomNums: TStrVector;
  NameIDs, SizeIDs, HeightIDs: TIntVector;

  procedure ReturningWrite(const AOldIDs, ANewIDs: TInt64Vector);
  var
    k: Integer;
  begin
    QSetSQL(
      sqlINSERT('SIZSTAFFRETURN', ['DocID', 'StoreID', 'ReturnStoreID', 'Note'])
    );
    QParamStr('Note', ANote);
    QParamInt('DocID', ADocID);
    for k:= 0 to High(AOldIDs) do
    begin
      QParamInt64('StoreID', AOldIDs[k]);
      QParamInt64('ReturnStoreID', ANewIDs[k]);
      QExec;
    end;
  end;

begin
  Result:= False;

  if not SIZReturningDataLoad(ALogID, StoreIDs, NomNums, NameIDs,
                              SizeIDs, HeightIDs) then Exit;

  QSetQuery(FQuery);
  try
    for i:= 0 to High(StoreIDs) do
    begin
      SIZStoreEntryAdd(ADocID, EntryID, NomNums[i], ANote,
                       NameIDs[i], SizeIDs[i], HeightIDs[i], Length(StoreIDs[i]));

      NewStoreIDs:= ValuesInt64ByInt64ID('SIZSTORELOG', 'StoreID', 'EntryID', EntryID);
      ReturningWrite(StoreIDs[i], NewStoreIDs);
    end;

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZReturningCancel(const ALogID: Int64; const ACommit: Boolean = True): Boolean;
var
  StoreIDs: TInt64Vector;
begin
  Result:= False;
  StoreIDs:= ValuesInt64ByInt64ID('SIZCARDPERSONALLOGINFO', 'StoreID', 'LogID', ALogID, True{Unique});
  if VIsNil(StoreIDs) then Exit;
  Result:= SIZStoreReturningCancel(StoreIDs, ACommit);
end;

function TDataBase.SIZStoreEntryCancel(const AStoreIDs: TInt64Vector;
                                 const ACommit: Boolean = True): Boolean;
var
  EntryIDs: TInt64Vector;

  function EmptyEntryIDs(const AEntryIDs: TInt64Vector): TInt64Vector;
  var
    i: Integer;
    V: TInt64Vector;
  begin
    //оставшиеся LogID
    V:= ValuesInt64ByInt64ID('SIZSTORELOG', 'EntryID', 'EntryID', AEntryIDs, True{Unique});
    //отбираем LogID, инфо которых больше нет
    Result:= nil;
    for i:= 0 to High(AEntryIDs) do
      if VIndexOf(V, AEntryIDs[i])<0 then
        VAppend(Result, AEntryIDs[i]);
  end;

begin
  Result:= False;
  if VIsNil(AStoreIDs) then Exit;

  QSetQuery(FQuery);
  try
    //отменяем возможную выдачу (+возврат+списание)
    SIZStoreReceivingCancel(AStoreIDs, False{no commit});

    //получаем все EntryID прихода на склад
    EntryIDs:= ValuesInt64ByInt64ID('SIZSTORELOG', 'EntryID', 'StoreID', AStoreIDs, True{Unique});
    //удаляем записи из SIZSTORELOG
    Delete('SIZSTORELOG', 'StoreID', AStoreIDs, False{no commit});
    //получаем оставшиеся после удаления пустые EntryID
    EntryIDs:= EmptyEntryIDs(EntryIDs);
    //удаляем пустые EntryID
    if not VIsNil(EntryIDs) then
      Delete('SIZSTOREENTRY', 'EntryID', EntryIDs, False{no commit});

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZStoreReceivingCancel(const AStoreIDs: TInt64Vector;
                                 const ACommit: Boolean = True): Boolean;
var
  StoreIDs, LogIDs: TInt64Vector;

  function EmptyLogIDs(const ALogIDs: TInt64Vector): TInt64Vector;
  var
    i: Integer;
    V: TInt64Vector;
  begin
    //оставшиеся LogID
    V:= ValuesInt64ByInt64ID('SIZCARDPERSONALLOGINFO', 'LogID', 'LogID', ALogIDs, True{Unique});
    //отбираем LogID, инфо которых больше нет
    Result:= nil;
    for i:= 0 to High(ALogIDs) do
      if VIndexOf(V, ALogIDs[i])<0 then
        VAppend(Result, ALogIDs[i]);
  end;

begin
  Result:= False;
  if VIsNil(AStoreIDs) then Exit;

  QSetQuery(FQuery);
  try
    //определяем, была ли выдача
    StoreIDs:= ValuesInt64ByInt64ID('SIZCARDPERSONALLOGINFO', 'StoreID', 'StoreID', AStoreIDs, True{Unique});
    if VIsNil(StoreIDs) then Exit;

    //отменяем возможный возврат (+ списание)
    SIZStoreReturningCancel(StoreIDs, False{no commit});

    //меняем статус на "свободно" на складе
    UpdateByInt64ID('SIZSTORELOG', 'IsBusy', 'StoreID', StoreIDs, 0{свободно}, False{no commit});
    //получаем список всех LogID
    LogIDs:= ValuesInt64ByInt64ID('SIZCARDPERSONALLOGINFO', 'LogID', 'StoreID', StoreIDs, True{Unique});
    //удаляем строки из таблицы инфо лога
    Delete('SIZCARDPERSONALLOGINFO', 'StoreID', StoreIDs, False{no commit});
    //получаем оставшиеся после удаления пустые LogID
    LogIDs:= EmptyLogIDs(LogIDs);
    //удаляем пустые LogID
    if not VIsNil(LogIDs) then
      Delete('SIZCARDPERSONALLOG', 'LogID', LogIDs, False{no commit});

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZStoreReturningCancel(const AStoreIDs: TInt64Vector;
                                           const ACommit: Boolean = True): Boolean;
var
  StoreIDs: TInt64Vector;
begin
  Result:= False;
  if VIsNil(AStoreIDs) then Exit;

  QSetQuery(FQuery);
  try
    //проверяем, есть ли возвраты
    StoreIDs:= ValuesInt64ByInt64ID('SIZSTAFFRETURN', 'ReturnStoreID', 'StoreID', AStoreIDs, True{Unique});
    if VIsNil(StoreIDs) then Exit;

    //отменяем списание
    SIZStoreWriteoffCancel(StoreIDs, False{no commit});
    //отменяем приход на склад по этому возврату
    SIZStoreEntryCancel(StoreIDs, False{no commit});

    //удаляем записи из таблицы возврата
    Delete('SIZSTAFFRETURN', 'ReturnStoreID', StoreIDs, False{no commit});

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZStoreWriteoffCancel(const AStoreIDs: TInt64Vector;
                                          const ACommit: Boolean = True): Boolean;
var
  StoreIDs: TInt64Vector;
begin
  Result:= False;
  if VIsNil(AStoreIDs) then Exit;

  QSetQuery(FQuery);
  try
    //проверяем, есть ли списание
    StoreIDs:= ValuesInt64ByInt64ID('SIZSTOREWRITEOFF', 'StoreID', 'StoreID', AStoreIDs, True{Unique});
    if VIsNil(StoreIDs) then Exit;

    //отмечаем СИЗ на складе, как свободные
    UpdateByInt64ID('SIZSTORELOG', 'IsBusy', 'StoreID', StoreIDs, 0{свободно}, False{no commit});
    //удаляем записи из таблицы списания
    Delete('SIZSTOREWRITEOFF', 'StoreID', StoreIDs, False{no commit});

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZStoreHistorySizListLoad(const AMatchSizName, AFilterNomNum: String;
                                 out ANomNums, ASizNames: TStrVector;
                                 out ANameIDs: TIntVector): Boolean;
var
  WhereStr, MatchStr: String;
begin
  Result:= False;

  ANomNums:= nil;
  ASizNames:= nil;
  ANameIDs:= nil;

  MatchStr:= PrepareMatchStr(AMatchSizName);

  if not SEmpty(MatchStr) then
  begin
    ExecuteScript([
      'CREATE VIRTUAL TABLE STOREHISTORY_FTS USING FTS5(NomNum, NameID, SizName);',
      'INSERT INTO STOREHISTORY_FTS ' +
        'SELECT DISTINCT t1.NomNum, t1.NameID, t2.SizName ' +
        'FROM SIZSTOREENTRY t1 ' +
        'INNER JOIN SIZNAME t2 ON (t1.NameID=t2.NameID) ' +
        'ORDER BY t2.SizName;'
    ]);
  end;

  QSetQuery(FQuery);
  if SEmpty(MatchStr) then
  begin
    if SEmpty(AFilterNomNum) then
      WhereStr:= EmptyStr
    else
      WhereStr:= 'WHERE (t1.NomNum LIKE :FilterNomNum) ';
    QSetSQL(
      'SELECT DISTINCT t1.NomNum, t1.NameID, t2.SizName ' +
      'FROM SIZSTOREENTRY t1 ' +
      'INNER JOIN SIZNAME t2 ON (t1.NameID=t2.NameID) ' +
      WhereStr +
      'ORDER BY t2.SizName'
    );
  end
  else begin
    WhereStr:= 'WHERE (STOREHISTORY_FTS MATCH :MatchStr) ';
    if not SEmpty(AFilterNomNum) then
      WhereStr:= WhereStr + 'AND (NomNum LIKE :FilterNomNum) ';
    QSetSQL(
      'SELECT NomNum, NameID, SizName ' +
      'FROM STOREHISTORY_FTS ' +
      WhereStr
    );
    QParamStr('MatchStr', MatchStr + '*');
  end;
  QParamStr('FilterNomNum', '%'+AFilterNomNum+'%');
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ANomNums, QFieldStr('NomNum'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;

  if not SEmpty(MatchStr) then
    ExecuteScript([
      'DROP TABLE IF EXISTS STOREHISTORY_FTS;'
    ]);
end;

function TDataBase.SIZStoreHistoryEntryLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 const AIsNeedReturning: Boolean;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  AInfos:= nil;
  ACounts:= nil;

  SQLStr:=
    'SELECT COUNT(*) AS SizCount, t3.DocName, t3.DocDate, t3.DocNum ' +
    'FROM SIZSTORELOG t1 ' +
    'INNER JOIN SIZSTOREENTRY t2 ON (t1.EntryID=t2.EntryID) ' +
    'INNER JOIN SIZDOC t3 ON (t2.DocID=t3.DocID) ';
  if not AIsNeedReturning then
    SQLStr:= SQLStr +
      'LEFT OUTER JOIN SIZSTAFFRETURN t4 ON (t1.StoreID=t4.ReturnStoreID) ';

  SQLStr:= SQLStr +
    'WHERE (t2.NomNum = :NomNum) AND (t2.NameID = :NameID) ';
  if not AIsNeedReturning then
    SQLStr:= SQLStr +
    'AND (t4.ReturnStoreID IS NULL) ';

  SQLStr:= SQLStr +
    'GROUP BY t3.DocName, t3.DocDate, t3.DocNum ' +
    'ORDER BY t3.DocDate, t3.DocName, t3.DocNum ';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamStr('NomNum', ANomNum);
  QParamInt('NameID', ANameID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AInfos, SRepeat(ASpacesCount, SYMBOL_SPACE) +
                      SIZDocFullName(QFieldStr('DocName'),
                                     QFieldStr('DocNum'),
                                     QFieldDT('DocDate')));
      VAppend(ACounts, QFieldInt('SizCount'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreHistoryReceivingLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 const AIsNeedReturning: Boolean;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  AInfos:= nil;
  ACounts:= nil;

  SQLStr:=
    'SELECT COUNT(*) AS SizCount, t4.TabNum, ' +
           't5.Name, t5.Family, t5.Patronymic, ' +
           't6.DocName, t6.DocDate, t6.DocNum ' +
    'FROM SIZCARDPERSONALLOGINFO t1 ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZCARDPERSONAL t3 ON (t2.CardID=t3.CardID) ' +
    'INNER JOIN STAFFTABNUM t4 ON (t3.TabNumID=t4.TabNumID) ' +
    'INNER JOIN STAFFMAIN t5 ON (t4.StaffID=t5.StaffID) ' +
    'INNER JOIN SIZDOC t6 ON (t2.DocID=t6.DocID) ' +
    'INNER JOIN SIZSTORELOG t7 ON (t1.StoreID=t7.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t8 ON (t7.EntryID=t8.EntryID) ';
  if not AIsNeedReturning then
    SQLStr:= SQLStr +
      'LEFT OUTER JOIN SIZSTAFFRETURN t9 ON (t1.StoreID=t9.ReturnStoreID) ';
  SQLStr:= SQLStr +
    'WHERE (t8.NomNum = :NomNum) AND (t8.NameID = :NameID) AND (t2.ReceivingInfoID=t2.NowInfoID) ';
  if not AIsNeedReturning then
    SQLStr:= SQLStr +
      'AND (t9.ReturnStoreID IS NULL) ';
  SQLStr:= SQLStr +
    'GROUP BY t4.TabNum, t5.Name, t5.Family, t5.Patronymic, t6.DocName, t6.DocDate, t6.DocNum ' +
    'ORDER BY t6.DocDate, t6.DocName, t6.DocNum ';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamStr('NomNum', ANomNum);
  QParamInt('NameID', ANameID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AInfos, SRepeat(ASpacesCount, SYMBOL_SPACE) +
                      SIZDocFullName(QFieldStr('DocName'),
                                     QFieldStr('DocNum'),
                                     QFieldDT('DocDate')) + ' - ' +
                      StaffFullName(QFieldStr('Family'),
                                    QFieldStr('Name'),
                                    QFieldStr('Patronymic'),
                                    QFieldStr('TabNum'), True{short}));
      VAppend(ACounts, QFieldInt('SizCount'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreHistoryReturningLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
begin
  Result:= False;

  AInfos:= nil;
  ACounts:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT COUNT(*) AS SizCount, t4.TabNum, ' +
           't5.Name, t5.Family, t5.Patronymic, ' +
           't6.DocName, t6.DocDate, t6.DocNum ' +
    'FROM SIZSTAFFRETURN t0 ' +
    'INNER JOIN SIZCARDPERSONALLOGINFO t1 ON (t0.StoreID=t1.StoreID) ' +
    'INNER JOIN SIZCARDPERSONALLOG t2 ON (t1.LogID=t2.LogID) ' +
    'INNER JOIN SIZCARDPERSONAL t3 ON (t2.CardID=t3.CardID) ' +
    'INNER JOIN STAFFTABNUM t4 ON (t3.TabNumID=t4.TabNumID) ' +
    'INNER JOIN STAFFMAIN t5 ON (t4.StaffID=t5.StaffID) ' +
    'INNER JOIN SIZDOC t6 ON (t0.DocID=t6.DocID) ' +
    'INNER JOIN SIZSTORELOG t7 ON (t1.StoreID=t7.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t8 ON (t7.EntryID=t8.EntryID) ' +
    'WHERE (t8.NomNum = :NomNum) AND (t8.NameID = :NameID) AND (t2.ReceivingInfoID=t2.NowInfoID) ' +
    'GROUP BY t4.TabNum, t5.Name, t5.Family, t5.Patronymic, t6.DocName, t6.DocDate, t6.DocNum ' +
    'ORDER BY t6.DocDate, t6.DocName, t6.DocNum ');
  QParamStr('NomNum', ANomNum);
  QParamInt('NameID', ANameID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AInfos, SRepeat(ASpacesCount, SYMBOL_SPACE) +
                      SIZDocFullName(QFieldStr('DocName'),
                                     QFieldStr('DocNum'),
                                     QFieldDT('DocDate')) + ' - ' +
                      StaffFullName(QFieldStr('Family'),
                                    QFieldStr('Name'),
                                    QFieldStr('Patronymic'),
                                    QFieldStr('TabNum'), True{short}));
      VAppend(ACounts, QFieldInt('SizCount'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreHistoryWriteoffLoad(const ANomNum: String;
                                 const ANameID, ASpacesCount: Integer;
                                 const AIsNeedReturning: Boolean;
                                 out AInfos: TStrVector;
                                 out ACounts: TIntVector): Boolean;
var
  SQLStr: String;
begin
  Result:= False;

  AInfos:= nil;
  ACounts:= nil;

  SQLStr:=
    'SELECT COUNT(*) AS SizCount, t2.DocName, t2.DocDate, t2.DocNum ' +
    'FROM SIZSTOREWRITEOFF t1 ' +
    'INNER JOIN SIZDOC t2 ON (t1.DocID=t2.DocID) ' +
    'INNER JOIN SIZSTORELOG t3 ON (t1.StoreID=t3.StoreID) ' +
    'INNER JOIN SIZSTOREENTRY t4 ON (t3.EntryID=t4.EntryID) ';
  if not AIsNeedReturning then
    SQLStr:= SQLStr +
      'LEFT OUTER JOIN SIZSTAFFRETURN t5 ON (t1.StoreID=t5.ReturnStoreID) ';

  SQLStr:= SQLStr +
    'WHERE (t4.NomNum = :NomNum) AND (t4.NameID = :NameID) ';
  if not AIsNeedReturning then
    SQLStr:= SQLStr +
    'AND (t5.ReturnStoreID IS NULL) ';

  SQLStr:= SQLStr +
    'GROUP BY t2.DocName, t2.DocDate, t2.DocNum ' +
    'ORDER BY t2.DocDate, t2.DocName, t2.DocNum ';

  QSetQuery(FQuery);
  QSetSQL(SQLStr);
  QParamStr('NomNum', ANomNum);
  QParamInt('NameID', ANameID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(AInfos, SRepeat(ASpacesCount, SYMBOL_SPACE) +
                      SIZDocFullName(QFieldStr('DocName'),
                                     QFieldStr('DocNum'),
                                     QFieldDT('DocDate')));
      VAppend(ACounts, QFieldInt('SizCount'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStoreHistoryLoad(const AMatchSizName, AFilterNomNum: String;
                                  const AIsNeedReturning: Boolean;
                                  out ANomNums, ASizNames: TStrVector;
                                  out AInfos: TStrMatrix;
                                  out ACounts: TIntMatrix): Boolean;
const
  SPACES_COUNT = 10;
var
  i, EntryCount, ReceivingCount, ReturningCount, WriteoffCount: Integer;
  NameIDs, Counts, TmpCounts: TIntVector;
  Infos, TmpInfos: TStrVector;
begin
  AInfos:= nil;
  ACounts:= nil;

  //получаем список СИЗ
  Result:= SIZStoreHistorySizListLoad(AMatchSizName, AFilterNomNum,
                                      ANomNums, ASizNames, NameIDs);
  if not Result then Exit;

  //получаем данные для каждого СИЗ
  for i:= 0 to High(ANomNums) do
  begin
    ReceivingCount:= 0;
    ReturningCount:= 0;
    WriteoffCount:= 0;

    //приход на склад
    SIZStoreHistoryEntryLoad(ANomNums[i], NameIDs[i], SPACES_COUNT,
                             False{AIsNeedReturning - всегда без учета возврата,
                             т.к. возвраты записываются в отдельную строку}
                             , Infos, Counts);
    EntryCount:= VSum(Counts);
    VIns(Infos, 0, 'Получено');
    VIns(Counts, 0, EntryCount);

    //выдача
    if SIZStoreHistoryReceivingLoad(ANomNums[i], NameIDs[i], SPACES_COUNT,
                                    AIsNeedReturning, TmpInfos, TmpCounts) then
    begin
      ReceivingCount:= VSum(TmpCounts);
      VIns(TmpInfos, 0, 'Выдано');
      VIns(TmpCounts, 0, ReceivingCount);
      Infos:= VAdd(Infos, TmpInfos);
      Counts:= VAdd(Counts, TmpCounts);

      //возврат
      if AIsNeedReturning and
         SIZStoreHistoryReturningLoad(ANomNums[i], NameIDs[i], SPACES_COUNT,
                                     TmpInfos, TmpCounts) then
      begin
        ReturningCount:= VSum(TmpCounts);
        VIns(TmpInfos, 0, 'Возвращено');
        VIns(TmpCounts, 0, ReturningCount);
        Infos:= VAdd(Infos, TmpInfos);
        Counts:= VAdd(Counts, TmpCounts);
      end;
    end;

    //списание
    if SIZStoreHistoryWriteoffLoad(ANomNums[i], NameIDs[i], SPACES_COUNT,
                                   AIsNeedReturning, TmpInfos, TmpCounts) then
    begin
      WriteoffCount:= VSum(TmpCounts);
      VIns(TmpInfos, 0, 'Списано');
      VIns(TmpCounts, 0, WriteoffCount);
      Infos:= VAdd(Infos, TmpInfos);
      Counts:= VAdd(Counts, TmpCounts);
    end;

    //остаток
    VAppend(Infos, 'Остаток');
    VAppend(Counts, EntryCount - ReceivingCount + ReturningCount - WriteoffCount);

    MAppend(AInfos, Infos);
    MAppend(ACounts, Counts);
  end;

end;

function TDataBase.SIZStoreRequestLoad(const AReportDate: TDate;
                          const AWriteoffType: Byte;
                          out ASizNames: TStrVector;
                          out AGenders: TIntVector;
                          out ASIZSizes: TStrMatrix;
                          out AFamilies, ANames, APatronymics, ATabNums: TStrMatrix3D;
                          out ASizCounts: TIntMatrix3D;
                          out AWriteoffDates: TDateMatrix3D): Boolean;
var
  i, j: Integer;
  TabNumIDs, Genders: TIntVector;
  Families, Names, Patronymics, TabNums: TStrVector;

  CardID, ItemID, ItemPostID: Integer;
  CardNum, PostName, NormName: String;
  CardBD, CardED: TDate;

  NormSubItems: TNormSubItems;
  StatusSubItems: TStatusSubItems;

  function IndexOfName(const ASIZName: String; const AGender: Integer): Integer;
  var
    k: Integer;
  begin
    Result:= -1;
    for k:= 0 to High(ASizNames) do
      if SSame(ASizNames[k], ASIZName) and (AGenders[k]=AGender) then
      begin
        Result:= k;
        break;
      end;
  end;

  procedure AddData(const AInd1, AInd2: Integer);
  var
    S: String;
    m, n: Integer;
  begin
    //S:= VVectorToStr(NormSubItems[AInd2].Info.Names, ' или ');
    S:= VFirst(NormSubItems[AInd2].Info.Names);
    n:= IndexOfName(S, Genders[AInd1]);
    if n<0 then
    begin
      VAppend(ASizNames, S);
      VAppend(AGenders, Genders[AInd1]);
      n:= High(ASizNames);

      MAppend(ASIZSizes, nil);
      MAppend(AFamilies, nil);
      MAppend(ANames, nil);
      MAppend(APatronymics, nil);
      MAppend(ATabNums, nil);
      MAppend(ASizCounts, nil);
      MAppend(AWriteoffDates, nil);
    end;

    S:= SIZFullSize(VFirst(NormSubItems[AInd2].Info.SizeTypes),
                    VFirst(StatusSubItems[AInd2].SizeIDs),
                    VFirst(StatusSubItems[AInd2].HeightIDs));
    m:= VIndexOf(ASIZSizes[n], S);
    if m<0 then
    begin
      VAppend(ASIZSizes[n], S);
      m:= High(ASIZSizes[n]);

      MAppend(AFamilies[n], nil);
      MAppend(ANames[n], nil);
      MAppend(APatronymics[n], nil);
      MAppend(ATabNums[n], nil);
      MAppend(ASizCounts[n], nil);
      MAppend(AWriteoffDates[n], nil);
    end;

    VAppend(AFamilies[n, m], Families[AInd1]);
    VAppend(ANames[n, m], Names[AInd1]);
    VAppend(APatronymics[n, m], Patronymics[AInd1]);
    VAppend(ATabNums[n, m], TabNums[AInd1]);
    VAppend(ASizCounts[n, m], VFirst(NormSubItems[AInd2].Info.Nums));
    VAppend(AWriteoffDates[n, m], MMaxDate(StatusSubItems[AInd2].Info.WriteoffDates));
  end;

begin
  Result:= False;

  ASizNames:= nil;
  AGenders:= nil;
  ASIZSizes:= nil;
  AFamilies:= nil;
  ANames:= nil;
  APatronymics:= nil;
  ATabNums:= nil;
  ASizCounts:= nil;
  AWriteoffDates:= nil;

  //получаем список не уволенных на отчетную дату
  if not StaffListForSIZRequestLoad(AReportDate, TabNumIDs, Genders,
                              Families, Names, Patronymics, TabNums) then Exit;

  for i:=0 to High(TabNumIDs) do
  begin
    //актуальная личная карточка учета СИЗ на отчетную дату
    if not SIZPersonalCardForDateLoad(TabNumIDs[i], AReportDate,
                             CardID, ItemID, ItemPostID, CardNum,
                             PostName, NormName, CardBD, CardED) then continue;
    //нормы выдачи по актуальной личной карточке
    NormSubItemsClear(NormSubItems{%H-});
    SIZNormSubItemsLoad(ItemID, NormSubItems);
    if Length(NormSubItems)=0 then continue;
    //статус выданного
    StatusSubItemsClear(StatusSubItems{%H-});
    SIZStatusLoad(TabNumIDs[i], CardID, AWriteoffType, AReportDate,
                  NormSubItems, StatusSubItems);

    //пробегаем по строкам пункта норм
    for j:= 0 to High(StatusSubItems) do
    begin
      //пропускаем дерматологические СИЗ и непросроченные СИЗ
      if (VFirst(NormSubItems[j].Info.SIZTypes)=0) or
         StatusSubItems[j].IsFreshExists then continue;
      //заполняем список
      AddData(i, j);
    end;
  end;

  Result:= not VIsNil(ASizNames);
end;

function TDataBase.BriefingListLoad(out ABriefIDs, AObjects, APeriods, ANums: TIntVector;
                              out ABriefNames, ANotes: TStrVector;
                              out ABeginDates, AEndDates, ALastDates: TDateVector): Boolean;
begin
  Result:= False;

  ABriefIDs:= nil;
  AObjects:= nil;
  APeriods:= nil;
  ANums:= nil;
  ABriefNames:= nil;
  ANotes:= nil;
  ABeginDates:= nil;
  AEndDates:= nil;
  ALastDates:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT BriefID, Object, Period, Num, BriefName, Note, ' +
           'BeginDate, EndDate, LastDate ' +
    'FROM BRIEFINGMAIN ' +
    'ORDER BY BeginDate DESC'
   );
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ABriefIDs, QFieldInt('BriefID'));
      VAppend(AObjects, QFieldInt('Object'));
      VAppend(APeriods, QFieldInt('Period'));
      VAppend(ANums, QFieldInt('Num'));

      VAppend(ABriefNames, QFieldStr('BriefName'));
      VAppend(ANotes, QFieldStr('Note'));

      VAppend(ABeginDates, QFieldDT('BeginDate'));
      VAppend(AEndDates, QFieldDT('EndDate'));
      VAppend(ALastDates, QFieldDT('LastDate'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.BriefingPostObjectsLoad(const ABriefID: Integer;
                                           out APostIDs: TIntVector;
                                           out APostNames: TStrVector): Boolean;
begin
  Result:= False;

  APostIDs:= nil;
  APostNames:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.PostID, t2.PostName ' +
    'FROM BRIEFINGPOST t1 ' +
    'INNER JOIN STAFFPOST t2 ON (t1.PostID=t2.PostID) ' +
    'WHERE t1.BriefID = :BriefID ' +
    'ORDER BY t2.PostName'
   );
  QParamInt('BriefID', ABriefID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(APostIDs, QFieldInt('PostID'));
      VAppend(APostNames, QFieldStr('PostName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.BriefingTabNumObjectsLoad(const ABriefID: Integer;
                                   out ATabNumIDs: TIntVector;
                                   out AFs, ANs, APs, ATabNums: TStrVector): Boolean;
begin
  Result:= False;

  ATabNumIDs:= nil;
  AFs:= nil;
  ANs:= nil;
  APs:= nil;
  ATabNums:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.TabNumID, t2.TabNum, t3.Family, t3.Name, t3.Patronymic  ' +
    'FROM BRIEFINGTABNUM t1 ' +
    'INNER JOIN STAFFTABNUM t2 ON (t1.TabNumID=t2.TabNumID) ' +
    'INNER JOIN STAFFMAIN t3 ON (t2.StaffID=t3.StaffID) ' +
    'WHERE t1.BriefID = :BriefID ' +
    'ORDER BY t3.Family, t3.Name, t3.Patronymic'
   );
  QParamInt('BriefID', ABriefID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      VAppend(ATabNumIDs, QFieldInt('TabNumID'));
      VAppend(AFs, QFieldStr('Family'));
      VAppend(ANs, QFieldStr('Name'));
      VAppend(APs, QFieldStr('Patronymic'));
      VAppend(ATabNums, QFieldStr('TabNum'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

procedure TDataBase.BriefingListObjectNamesLoad(const ABriefIDs, AObjects: TIntVector;
                                   out AObjectIDs: TIntMatrix;
                                   out AObjectNames: TStrMatrix);
var
  i: Integer;

  procedure AllStaffObjectsLoad(const AIndex: Integer;
                            var AIDs: TIntVector;
                            var ANames: TStrVector);
  begin
    AIDs:= VCreateInt([0]);
    ANames:= VCreateStr([SUpper(BRIEFOBJECT_PICKS[AObjects[AIndex]])]);
  end;

  procedure PostObjectsLoad(const AIndex: Integer;
                            var AIDs: TIntVector;
                            var ANames: TStrVector);
  begin
    BriefingPostObjectsLoad(ABriefIDs[AIndex], AIDs, ANames);
  end;

  procedure TabNumObjectsLoad(const AIndex: Integer;
                            var AIDs: TIntVector;
                            var ANames: TStrVector);
  var
    Fs, Ns, Ps, TabNums: TStrVector;
  begin
    AIDs:= nil;
    ANames:= nil;
    if not BriefingTabNumObjectsLoad(ABriefIDs[AIndex], AIDs, Fs, Ns, Ps, TabNums) then Exit;
    ANames:= StaffFullName(Fs, Ns, Ps, TabNums, False{long});
  end;

begin
  AObjectNames:= nil;
  AObjectIDs:= nil;
  if VIsNil(ABriefIDs) then Exit;

  MDim(AObjectNames, Length(ABriefIDs));
  MDim(AObjectIDs, Length(ABriefIDs));
  for i:= 0 to High(ABriefIDs) do
  begin
    case AObjects[i] of
      0: AllStaffObjectsLoad(i, AObjectIDs[i], AObjectNames[i]);
      1: PostObjectsLoad(i, AObjectIDs[i], AObjectNames[i]);
      2: TabNumObjectsLoad(i, AObjectIDs[i], AObjectNames[i]);
    end;
  end;
end;

function TDataBase.BriefingIDsWrite(const ABriefID, AObject: Integer;
                                    const AObjectIDs: TIntVector;
                                    const ACommit: Boolean = True): Boolean;
var
  i: Integer;
  IDFieldName, TableName: String;
begin
  Result:= False;
  if (AObject=0) or VIsNil(AObjectIDs) then Exit;

  case AObject of
    1:
      begin
        IDFieldName:= 'PostID';
        TableName:= 'BRIEFINGPOST';
      end;
    2:
      begin
        IDFieldName:= 'TabNumID';
        TableName:= 'BRIEFINGTABNUM';
      end;
  end;

  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlINSERT(TableName, ['BriefID', IDFieldName])
    );
    QParamInt('BriefID', ABriefID);
    for i:= 0 to High(AObjectIDs) do
    begin
      QParamInt(IDFieldName, AObjectIDs[i]);
      QExec;
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.BriefingAdd(out ABriefID: Integer;
                         const ABriefName, ANote: String;
                         const ABeginDate, AEndDate, ALastDate: TDate;
                         const AObject, APeriod, ANum: Integer;
                         const AObjectIDs: TIntVector): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись основных данных
    QSetSQL(
      sqlINSERT('BRIEFINGMAIN', ['BriefName', 'Note', 'BeginDate', 'EndDate', 'LastDate',
                                 'Object', 'Period', 'Num'])
    );
    QParamStr('BriefName', ABriefName);
    QParamStr('Note', ANote);
    QParamDT('BeginDate', ABeginDate);
    QParamDT('EndDate', AEndDate);
    QParamDT('LastDate', ALastDate);
    QParamInt('Object', AObject);
    QParamInt('Period', APeriod);
    QParamInt('Num', ANum);
    QExec;

    //получение ID сделанной записи
    ABriefID:= LastWritedInt32ID('BRIEFINGMAIN');

    //запись ID должностей или таб номеров
    BriefingIDsWrite(ABriefID, AObject, AObjectIDs, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.BriefingUpdate(const ABriefID: Integer;
                         const ABriefName, ANote: String;
                         const ABeginDate, AEndDate, ALastDate: TDate;
                         const AOldObject, AObject, APeriod, ANum: Integer;
                         const AObjectIDs: TIntVector): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //обновление основных данных
    QSetSQL(
      sqlUPDATE('BRIEFINGMAIN', ['BriefName', 'Note', 'BeginDate', 'EndDate', 'LastDate',
                                 'Object', 'Period', 'Num']) +
      'WHERE BriefID = :BriefID'
    );
    QParamInt('BriefID', ABriefID);
    QParamStr('BriefName', ABriefName);
    QParamStr('Note', ANote);
    QParamDT('BeginDate', ABeginDate);
    QParamDT('EndDate', AEndDate);
    QParamDT('LastDate', ALastDate);
    QParamInt('Object', AObject);
    QParamInt('Period', APeriod);
    QParamInt('Num', ANum);
    QExec;

    //удаляем старые ID должностей или таб номеров
    if AOldObject>0 then
    begin
      case AObject of
        1: Delete('BRIEFINGPOST', 'BriefID', ABriefID, False{no commit});
        2: Delete('BRIEFINGTABNUM', 'BriefID', ABriefID, False{no commit});
      end;
    end;

    //записываем новые ID должностей или таб номеров
    BriefingIDsWrite(ABriefID, AObject, AObjectIDs, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

end.

