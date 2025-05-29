unit UDataBase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  //Project utils
  UCalendar, UConst, USchedule, UTimetable, UWorkHours, USIZTypes, USIZUtils,
  //DK packages utils
  DK_SQLite3, DK_SQLUtils, DK_Vector, DK_Matrix, DK_StrUtils, DK_Const,
  DK_DateUtils, DK_VSTDropDown;

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

    {Cписок людей: True - ОК, False - список пуст;
     AOrderType - сортировка: 0-ФИО, 1-табельный номер, 2-должность, 3-дата рождения,
                              4-дата приема, 5-дата увольнения, 6-разряд;
     AListType - включить в список: 0-всех, 1-работающих, 2-уволенных, 3-без таб.№}
    function StaffListLoad(const AOrderType, AListType: Byte;
                           out AStaffIDs, ATabNumIDs, AGenders: TIntVector;
                           out ABornDates, ARecrutDates, ADismissDates: TDateVector;
                           out AFs, ANs, APs, ATabNums, APostNames, ARanks: TStrVector): Boolean;

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
                             out AFs, ANs, APs, ATabNums, APostNames: TStrVector): Boolean;

    {Cписок сотрудников для общих графиков и табелей
     ABeginDate, AEndDate - отчетный период
     AOrderType - сортировка: 0-график, 1-должность, 2-ФИО, 3-табельный номер
     True - ОК, False - список пуст}
    function StaffListForCommonTimingLoad(const ABeginDate, AEndDate: TDate;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateVector;
                   out AFs, ANs, APs, ATabNums, APostNames, AScheduleNames: TStrVector): Boolean;

    {Cписок сотрудников для планирования отпусков на год
     AYear - отчетный период
     AOrderType - сортировка: 0-график, 1-должность, 2-ФИО, 3-табельный номер
     True - ОК, False - список пуст}
    function StaffListForVacationPlanningLoad(const AYear: Integer;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out AFamilies, ANames, APatronymics, ATabNums, APostNames, AScheduleNames: TStrVector): Boolean;



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
    (или первая после даты, если на эту дату еще не работал)}
    procedure StaffPostForDate(const ATabNumID: Integer;
                               const ADate: TDate;
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
                             out ANormNames, ATypicalNames: TStrVector;
                             out ABeginDates, AEndDates: TDateVector): Boolean;

    {Добавление новой нормы: True - ОК, False - ошибка}
    function SIZNormAdd(out ANormID: Integer;
                          const ANormName, ATypicalName: String;
                          const ABeginDate, AEndDate: TDate): Boolean;
    {Обновление нормы: True - ОК, False - ошибка}
    function SIZNormUpdate(const ANormID: Integer;
                          const ANormName, ATypicalName: String;
                          const ABeginDate, AEndDate: TDate): Boolean;

    {Удаление норм с обработкой всех таблиц: True - ОК, False - ошибка}
    function SIZNormDelete(const ANormID: Integer): Boolean;
    function SIZNormItemDelete(const AItemID: Integer): Boolean;
    function SIZNormSubItemDelete(const AItemID, ASubItemID, AReasonID, AOrderNum: Integer): Boolean;
    procedure SIZNormSubItemInfoDelete(const AInfoIDs: TIntVector); //no commit


    {Загрузка из базы списка пунктов типовых норм: True - ОК, False - пусто}
    function SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs, APostIDs: TIntVector;
                              out AItemNames, APostNames: TStrVector): Boolean;
    function SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs: TIntVector; out AItemNames: TStrVector;
                              out APostIDs: TIntMatrix; out APostNames: TStrMatrix): Boolean;



    {Проверка наличия в базе пункта нормы}
    function SIZIsNormItemExists(const ANormID, AItemID: Integer;
                                 const AItemName: String): Boolean;
    {Проверка пересечения периода действия нормы для указанной должностис другими
     пунктами и нормами}
    function SIZNormItemIntersectionExists(const APostID, AItemID: Integer;
                              const ABeginDate, AEndDate: TDate;
                              out ANormName, AItemName: String): Boolean;

    {Загрузка списка соответствия должностей и пунктов норм с ANormID: True - ОК, False - пусто}
    function SIZItemsAndPostsAccordanceLoad(const ANormID: Integer;
                                        out APostIDs, AItemIDs: TIntVector): Boolean;
    {Запись соответствий ID должностей APostIDs пункту нормы с AItemID: True - ОК, False - ошибка}
    function SIZItemsAndPostsAccordanceAdd(const AItemID: Integer;
                                        const APostIDs: TIntVector;
                                        const ACommit: Boolean = True): Boolean;
    {Запись нового пункта нормы: True - ОК, False - ошибка}
    function SIZNormItemWrite(const ANormID: Integer; out AItemID: Integer;
                              const AItemName: String;
                              const ACommit: Boolean = True): Boolean;
    {Добавление полностью нового пункта нормы: True - ОК, False - ошибка}
    function SIZNormItemAdd(const ANormID: Integer; out AItemID: Integer;
                            const AItemName: String; const APostIDs: TIntVector): Boolean;
    {Обновление пункта нормы: True - ОК, False - ошибка}
    function SIZNormItemUpdate(const AItemID: Integer;
                            const AItemName: String; const APostIDs: TIntVector): Boolean;
    {Копирование пункта в другие нормы: True - ОК, False - ошибка}
    function SIZNormItemCopy(const ANormID, AItemID: Integer;
                            const AItemName: String; const APostIDs: TIntVector): Boolean;






    {Загрузка полных данных по строкам пункта типовых норм: True - ОК, False - пусто}
    function SIZNormSubItemsDataLoad(const AItemID: Integer;
             out ASubItemIDs, ASubItemOrderNums, AReasonIDs,
                 AInfoIDs, AInfoOrderNums,
                 AClassIDs, ANameIDs, ASizeTypes,
                 ANums, ALifeIDs, ALifes: TIntVector;
             out AReasonNames, ASizNames, AUnits, ALifeNames: TStrVector): Boolean;
    {Загрузка строк пункта: True - ОК, False - пусто}
    function SIZNormSubItemsLoad(const AItemID: Integer; var ASubItems: TNormSubItems): Boolean;

    {Запись новой строки пункта: True - ОК, False - ошибка}
    function SIZNormSubItemWrite(const AItemID: Integer;
                                 out ASubItemID: Integer;
                                 const AReasonID, AOrderNum: Integer;
                                 const ACommit: Boolean = True): Boolean;




    {Загрузка данных строки пункта типовых норм: True - ОК, False - пусто}
    function SIZNormSubItemInfoLoad(const ASubItemID: Integer;
                                    var AInfo: TNormSubItemInfo): Boolean;
    {Запись нового Info строки пункта: True - ОК, False - ошибка}
    function SIZNormSubItemInfoWrite(const ASourceSubItemID: Integer;
                                    const ASourceInfo: TNormSubItemInfo;
                                    out ADestInfoIDs: TIntVector;
                                    const ACommit: Boolean = True): Boolean;





    {Загрузка ассортимента СИЗ: True - ОК, False - ошибка}
    function SIZAssortmentLoad(out AClassNames: TStrVector;
                              out ASizNames: TStrMatrix;
                              out ASizNameIDs, ASizSizeTypes: TIntMatrix): Boolean;





    function SIZStaffSpecSizeLoad(const AInfoID: Integer;
                               out ATabNumIDs, ASizeIDs, AHeightIDs: TIntVector): Boolean;
    function SIZStaffSpecSizeWrite(const AInfoID, ATabNumID, ASizeID, AHeightID: Integer;
                               const ACommit: Boolean = True): Boolean;
    function SIZStaffSpecSizeWrite(const AInfoID: Integer;
                               const ATabNumIDs, ASizeIDs, AHeightIDs: TIntVector;
                               const ACommit: Boolean = True): Boolean;
    function SIZStaffSpecSizeCopy(const ASourceInfoIDs, ADestInfoIDs: TIntVector;
                                  const ACommit: Boolean = True): Boolean;
  end;



var
  DataBase: TDataBase;

implementation



{ TDataBase }

function TDataBase.ColorShiftUpdate(const AColorValue, AColorIndex: Integer): Boolean;
begin
  Result:= UpdateInt32ID('SHIFTCOLORS', 'ColorValue', 'ColorIndex', AColorIndex, AColorValue);
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
  Result:= ValueIntStrID('SETTINGS', 'Value', 'Name', ASettingName);
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
  UpdateStrID('SETTINGS', 'Value', 'Name', ASettingName, ASettingValue, True {commit});
end;

procedure TDataBase.SettingsUpdate(const ASettingNames: TStrVector;
  const ASettingValues: TIntVector);
var
  i: Integer;
begin
  try
    for i:= 0 to High(ASettingNames) do
      UpdateStrID('SETTINGS', 'Value', 'Name', ASettingNames[i], ASettingValues[i], False {no commit});
    QCommit;
  finally
    QRollback;
  end;
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

function TDataBase.StaffListLoad(const AOrderType, AListType: Byte;
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
  QParamDT('ADate', Date);
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

function TDataBase.StaffListForPersonalTimingLoad(const AFilterValue: String;
                             const ABeginDate, AEndDate: TDate;
                             const AOrderType: Byte;
                             const AIsDescOrder: Boolean;
                             out ATabNumIDs: TIntVector;
                             out ARecrutDates, ADismissDates: TDateVector;
                             out AFs, ANs, APs, ATabNums, APostNames: TStrVector): Boolean;
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
    //VAppend(ARanks, Rank);
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
  //ARanks:= VReplace(ARanks, Indexes);
end;

function TDataBase.StaffListForCommonTimingLoad(const ABeginDate, AEndDate: TDate;
                   const AOrderType: Byte;
                   out ATabNumIDs: TIntVector;
                   out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateVector;
                   out AFs, ANs, APs, ATabNums, APostNames, AScheduleNames: TStrVector): Boolean;
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
  AScheduleNames:= nil;

  SQLStr:=
    'SELECT t1.Family, t1.Name, t1.Patronymic, '+
           't2.TabNumID, t2.TabNum, t2.RecrutDate, t2.DismissDate, ' +
           't3.ScheduleID, t3.BeginDate AS ScheduleBD, t3.EndDate AS ScheduleED, '+
           't4.FirstDate AS PostBD, t4.LastDate AS PostED, '+
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
  Result:= UpdateInt32ID('STAFFTABNUM', 'DismissDate', 'TabNumID', ATabNumID, ADismissDate);
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

procedure TDataBase.StaffPostForDate(const ATabNumID: Integer;
                                     const ADate: TDate;
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
    'WHERE (t1.TabNumID = :TabNumID) AND (t1.PostTemp = 0) AND (' +
          '(:DateValue BETWEEN t1.FirstDate AND t1.LastDate ) OR (:DateValue<t1.FirstDate) ' +
    ')' +
    'ORDER BY t1.FirstDate ' +
    'LIMIT 1'
  );
  QParamDT('DateValue', ADate);
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
    UpdateInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APostLogID, IncDay(AFirstDate, -1), False{no commit});
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
      UpdateInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APrevPostLogID, IncDay(AFirstDate, -1), False{no commit});
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
    UpdateInt32ID('STAFFPOSTLOG', 'LastDate', 'ID', APrevPostLogID, ALastDate, False{no commit});

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
  //TPostScheduleInfo = record
  //  ScheduleIDs: TIntVector; //список уникальных ID графиков
  //  Cycles: array of TScheduleCycle;
  //  Corrections: array of array of TScheduleCorrections;
  //  FirstDates, LastDates: TDateMatrix; //график с одним ID может попадаться в периоде несколько раз
  //end;
  //
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
    UpdateInt32ID('STAFFSCHEDULE', 'EndDate', 'ID', AHistoryID, IncDay(ABeginDate, -1), False{no commit});
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
      UpdateInt32ID('STAFFSCHEDULE', 'EndDate', 'ID', APrevHistoryID, IncDay(ABeginDate, -1), False{no commit});
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
    UpdateInt32ID('STAFFSCHEDULE', 'EndDate', 'ID', APrevHistoryID, AEndDate, False{no commit});

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
  DataBase.CalendarCorrectionsLoad(ABeginDate, AEndDate, Corrections);
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
  ACycle.Count:= ValueInt32Int32ID('SCHEDULEMAIN', 'CycleCount', 'ScheduleID', AScheduleID);
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
  Result:= ValueStrInt32ID('TIMETABLEMARK', 'StrMark', 'DigMark',  ADigMark);
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
                             out ANormNames, ATypicalNames: TStrVector;
                             out ABeginDates, AEndDates: TDateVector): Boolean;
begin
  Result:= False;

  ANormIDs:= nil;
  ANormNames:= nil;
  ATypicalNames:= nil;
  ABeginDates:= nil;
  AEndDates:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    sqlSELECT('SIZNORM', ['NormID', 'NormName', 'TypicalName', 'BeginDate', 'EndDate']) +
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
      VAppend(ATypicalNames, QFieldStr('TypicalName'));
      VAppend(ABeginDates, QFieldDT('BeginDate'));
      VAppend(AEndDates, QFieldDT('EndDate'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs, APostIDs: TIntVector;
                              out AItemNames, APostNames: TStrVector): Boolean;
begin
  Result:= False;

  AItemIDs:= nil;
  APostIDs:= nil;
  AItemNames:= nil;
  APostNames:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ItemID, t1.PostID, t2.ItemName, t3.PostName ' +
    'FROM SIZNORMITEMPOST t1 ' +
    'INNER JOIN SIZNORMITEM t2 ON (t1.ItemID=t2.ItemID) ' +
    'INNER JOIN STAFFPOST t3 ON (t1.PostID=t3.PostID) ' +
    'WHERE t2.NormID = :NormID ' +
    'ORDER BY t2.ItemName'
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
      VAppend(AItemNames, QFieldStr('ItemName'));
      VAppend(APostNames, QFieldStr('PostName'));
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormItemsLoad(const ANormID: Integer;
                              out AItemIDs: TIntVector; out AItemNames: TStrVector;
                              out APostIDs: TIntMatrix; out APostNames: TStrMatrix): Boolean;
var
  i, I1, I2, ItemID: Integer;
  ItemIDs, PostIDs: TIntVector;
  ItemNames, PostNames: TStrVector;
begin
  AItemIDs:= nil;
  AItemNames:= nil;
  APostIDs:= nil;
  APostNames:= nil;

  Result:= SIZNormItemsLoad(ANormID, ItemIDs, PostIDs, ItemNames, PostNames);
  if not Result then Exit;

  I1:= 0;
  ItemID:= ItemIDs[0];
  for i:= 1 to High(AItemIDs) do
  begin
    if ItemIDs[i]=ItemID then continue;

    VAppend(AItemIDs, ItemIDs[i-1]);
    VAppend(AItemNames, ItemNames[i-1]);

    I2:= i-1;
    MAppend(APostIDs, VCut(PostIDs, I1, I2));
    MAppend(APostNames, VCut(PostNames, I1, I2));

    ItemID:= ItemIDs[i];
    I1:= i;
  end;

  VAppend(AItemIDs, VLast(ItemIDs));
  VAppend(AItemNames, VLast(ItemNames));
  I2:= High(ItemIDs);
  MAppend(APostIDs, VCut(PostIDs, I1, I2));
  MAppend(APostNames, VCut(PostNames, I1, I2));
end;




function TDataBase.SIZItemsAndPostsAccordanceLoad(const ANormID: Integer;
                              out APostIDs, AItemIDs: TIntVector): Boolean;
begin
  Result:= False;
  APostIDs:= nil;
  AItemIDs:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.ItemID, t1.PostID ' +
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

function TDataBase.SIZNormItemWrite(const ANormID: Integer; out
  AItemID: Integer; const AItemName: String; const ACommit: Boolean): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись пункта нормы
    QSetSQL(
      sqlINSERT('SIZNORMITEM', ['ItemName', 'NormID'])
    );
    QParamInt('NormID', ANormID);
    QParamStr('ItemName', AItemName);
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
  const AItemName: String; const APostIDs: TIntVector): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //записываем пункт нормы в SIZNORMITEM
    SIZNormItemWrite(ANormID, AItemID, AItemName, False{no commit});
    //записываем все выбранные должности для пункта в SIZNORMITEMPOST
    SIZItemsAndPostsAccordanceAdd(AItemID, APostIDs, False{no commit});
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemUpdate(const AItemID: Integer;
  const AItemName: String; const APostIDs: TIntVector): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //обновляем наименование пункта
    UpdateInt32ID('SIZNORMITEM', 'ItemName', 'ItemID', AItemID, AItemName, False{no commit});
    //удаляем соответствие должностей этому пункту
    Delete('SIZNORMITEMPOST', 'ItemID', AItemID, False{no commit});
    //записываем все выбранные должности для пункта в SIZNORMITEMPOST
    SIZItemsAndPostsAccordanceAdd(AItemID, APostIDs, False{no commit});
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormItemCopy(const ANormID, AItemID: Integer;
  const AItemName: String; const APostIDs: TIntVector): Boolean;
var
  i, DestItemID, DestSubItemID: Integer;
  SourceSubItemIDs, ReasonIDs, SubItemOrderNums, DestInfoIDs: TIntVector;
  Info: TNormSubItemInfo;

  procedure SourceItemLoad(const ASourceItemID: Integer;
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
    QParamInt('ItemID', ASourceItemID);
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
    //записываем пункт
    SIZNormItemWrite(ANormID, DestItemID, AItemName, False{no commit});

    //достаем список строк пункта-источника
    SourceItemLoad(AItemID, SourceSubItemIDs, ReasonIDs, SubItemOrderNums);

    //пробегаем по всем строкам пункта-источника
    for i:=0 to High(SourceSubItemIDs) do
    begin
      //достаем спиcок подстрок для строки-источника
      SIZNormSubItemInfoLoad(SourceSubItemIDs[i], Info{%H-});
      //записываем строку пункта-цели в SIZNORMSUBITEM
      SIZNormSubItemWrite(DestItemID, DestSubItemID, ReasonIDs[i],
                          SubItemOrderNums[i], False{no commit});
      //записываем инфо строки-цели SIZNORMSUBITEMINFO и получаем их ID
      SIZNormSubItemInfoWrite(DestSubItemID, Info, DestInfoIDs, False{no commit});
      //копируем спец размеры СИЗ
      SIZStaffSpecSizeCopy(Info.InfoIDs, DestInfoIDs, False{no commit});
    end;

    //записываем все выбранные должности для пункта в SIZNORMITEMPOST
    SIZItemsAndPostsAccordanceAdd(DestItemID, APostIDs, False{no commit});
    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

function TDataBase.SIZNormSubItemInfoLoad(const ASubItemID: Integer;
                                          var AInfo: TNormSubItemInfo): Boolean;
var
  YearNum, SpecLifeName, UnitStr: String;
  Num, Life: Integer;
begin
  Result:= False;
  NormSubItemInfoClear(AInfo);

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.InfoID, t1.NameID, t1.Num, t1.Life, t1.SpecLifeID, t1.OrderNum, ' +
           't2.SizName, t2.SizeType, t2.ClassID, ' +
           't3.UnitStringCode, t4.SpecLifeName ' +
    'FROM SIZNORMSUBITEMINFO t1 ' +
    'INNER JOIN t2 SIZNAMES ON (t1.NameID=t2.NameID) ' +
    'INNER JOIN t3 SIZUNIT ON (t2.UnitID=t3.UnitID) ' +
    'INNER JOIN t4 SIZSPECLIFE ON (t1.SpecLifeID=t4.SpecLifeID) ' +
    'WHERE t1.SubItemID = :SubItemID ' +
    'ORDER BY t1.OrderNum');
  QParamInt('SubItemID', ASubItemID);
  QOpen;
  if not QIsEmpty then
  begin
    QFirst;
    while not QEOF do
    begin
      Num:= QFieldInt('Num');
      Life:= QFieldInt('Life');
      SpecLifeName:= QFieldStr('SpecLifeName');
      YearNum:= GetSizNumInLifeStr(Num, Life, SpecLifeName);
      UnitStr:= QFieldStr('UnitStringCode');
      NormSubItemInfoAdd(AInfo, QFieldInt('InfoID'), QFieldInt('OrderNum'),
                                QFieldInt('ClassID'), QFieldInt('NameID'),
                                QFieldInt('SizeType'), Num, QFieldInt('SpecLifeID'),
                                Life, QFieldStr('SizName'), UnitStr, YearNum);
      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormSubItemsDataLoad(const AItemID: Integer;
             out ASubItemIDs, ASubItemOrderNums, AReasonIDs,
                 AInfoIDs, AInfoOrderNums,
                 AClassIDs, ANameIDs, ASizeTypes,
                 ANums, ALifeIDs, ALifes: TIntVector;
             out AReasonNames, ASizNames, AUnits, ALifeNames: TStrVector): Boolean;
begin
  Result:= False;

  ASubItemIDs:= nil;
  ASubItemOrderNums:= nil;
  AReasonIDs:= nil;
  AReasonNames:= nil;

  AInfoIDs:= nil;
  AInfoOrderNums:= nil;

  AClassIDs:= nil;
  ANameIDs:= nil;
  ASizNames:= nil;
  AUnits:= nil;
  ASizeTypes:= nil;

  ANums:= nil;
  ALifeIDs:= nil;
  ALifes:= nil;
  ALifeNames:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.SubItemID, t1.Num, t1.Life, t1.InfoID, t1.SpecLifeID, ' +
           't1.OrderNum AS InfoOrderNum, t1.NameID, ' +
           't2.ReasonID, t2.OrderNum AS SubItemOrderNum, ' +
           't3.SizName, t3.SizeType, t3.ClassID, ' +
           't4.SpecLifeName, ' +
           't5.ReasonName, ' +
           't6.UnitStringCode ' +
    'FROM SIZNORMSUBITEMINFO t1 ' +
    'INNER JOIN SIZNORMSUBITEM t2 ON (t1.SubItemID=t2.SubItemID) ' +
    'INNER JOIN SIZNAMES t3 ON (t1.NameID=t3.NameID) ' +
    'INNER JOIN SIZSPECLIFE t4 ON (t1.SpecLifeID=t4.SpecLifeID) ' +
    'INNER JOIN SIZREASON t5 ON (t2.ReasonID=t5.ReasonID) ' +
    'INNER JOIN SIZUNIT t6 ON (t3.UnitID=t6.UnitID) ' +
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

      VAppend(AClassIDs, QFieldInt('ClassID'));
      VAppend(ANameIDs, QFieldInt('NameID'));
      VAppend(ASizNames, QFieldStr('SizName'));
      VAppend(AUnits, QFieldStr('UnitStringCode'));
      VAppend(ASizeTypes, QFieldInt('SizeType'));

      VAppend(ANums, QFieldInt('Num'));
      VAppend(ALifeIDs, QFieldInt('SpecLifeID'));
      VAppend(ALifes, QFieldInt('Life'));
      VAppend(ALifeNames, QFieldStr('SpecLifeName'));

      QNext;
    end;
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZNormSubItemsLoad(const AItemID: Integer;
                                       var ASubItems: TNormSubItems): Boolean;
var
  SubItemIDs, SubItemOrderNums, ReasonIDs: TIntVector;
  InfoIDs, InfoOrderNums: TIntVector;
  ClassIDs, NameIDs, SizeTypes: TIntVector;
  Nums, LifeIDs, Lifes: TIntVector;
  ReasonNames, SizNames, Units, LifeNames: TStrVector;

  YearNum: String;
  i, SubItemID: Integer;

  SubItem: TNormSubItem;
  Info: TNormSubItemInfo;
begin
  NormSubItemsClear(ASubItems);
  Result:= SIZNormSubItemsDataLoad(AItemID, SubItemIDs, SubItemOrderNums, ReasonIDs,
                    InfoIDs, InfoOrderNums, ClassIDs, NameIDs, SizeTypes, Nums,
                    LifeIDs, Lifes, ReasonNames, SizNames, Units, LifeNames);
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
    YearNum:= GetSizNumInLifeStr(Nums[i], Lifes[i], LifeNames[i]);
    NormSubItemInfoAdd(Info, InfoIDs[i], InfoOrderNums[i],
                       ClassIDs[i], NameIDs[i], SizeTypes[i], Nums[i],
                       LifeIDs[i], Lifes[i], SizNames[i], Units[i], YearNum);

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

function TDataBase.SIZIsNormItemExists(const ANormID, AItemID: Integer;
  const AItemName: String): Boolean;
begin
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT ItemID ' +
    'FROM SIZNORMITEM ' +
    'WHERE (ItemName= :ItemName) AND (ItemID<>:ItemID) AND (NormID=:NormID)'
  );
  QParamStr('ItemName', AItemName);
  QParamInt('ItemID', AItemID);
  QParamInt('NormID', ANormID);
  QOpen;
  Result:= not QIsEmpty;
  QClose;
end;

function TDataBase.SIZNormItemIntersectionExists(const APostID, AItemID: Integer;
              const ABeginDate, AEndDate: TDate;
              out ANormName, AItemName: String): Boolean;
begin
  Result:= False;
  ANormName:= EmptyStr;
  AItemName:= EmptyStr;
  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t3.NormName, t2.ItemName ' +
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
    AItemName:= QFieldStr('ItemName');
  end;
  QClose;
end;

function TDataBase.SIZNormAdd(out ANormID: Integer;
                          const ANormName, ATypicalName: String;
                          const ABeginDate, AEndDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    //запись нормы
    QSetSQL(
      sqlINSERT('SIZNORM', ['NormName', 'TypicalName', 'BeginDate', 'EndDate'])
    );
    QParamStr('NormName', ANormName);
    QParamStr('TypicalName', ATypicalName);
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
                          const ANormName, ATypicalName: String;
                          const ABeginDate, AEndDate: TDate): Boolean;
begin
  Result:= False;
  QSetQuery(FQuery);
  try
    QSetSQL(
      sqlUPDATE('SIZNORM', ['NormName', 'TypicalName', 'BeginDate', 'EndDate']) +
      'WHERE NormID = :NormID'
    );
    QParamInt('NormID', ANormID);
    QParamStr('NormName', ANormName);
    QParamStr('TypicalName', ATypicalName);
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
    SIZNormSubItemInfoDelete(InfoIDs);
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
    SIZNormSubItemInfoDelete(InfoIDs);
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

  procedure GetInfoIDs;
  begin
    InfoIDs:= nil;
    QSetSQL(
      'SELECT InfoID ' +
      'FROM SIZNORMSUBITEMINFO ' +
      'WHERE SubItemID = :SubItemID'
    );
    QParamInt('SubItemID', ASubItemID);
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
    //определяем строки, лежащие ниже по порядку, чем AOrderNum
    GetSubItemsAfter;
    //сдвигаем вверх порядковые номера строк, лежащих ниже удаляемой
    MoveSubItemsUp;
    //определяем список InfoID для этой строки пункта норм
    GetInfoIDs;
    //удаляем эти InfoID (с обработкой)
    SIZNormSubItemInfoDelete(InfoIDs);
    //удаляем строку пункта нормы
    Delete('SIZNORMSUBITEM', 'SubItemID', ASubItemID, False{no commit});

    QCommit;
    Result:= True;
  except
    QRollback;
  end;
end;

procedure TDataBase.SIZNormSubItemInfoDelete(const AInfoIDs: TIntVector);
var
  i: Integer;
  StoreIDs, EntryIDs: TInt64Vector;

  function GetStoreIDsFromInfoID(const AInfoID: Integer): TInt64Vector;
  begin
    Result:= nil;
    QSetSQL(
      'SELECT t1.StoreID ' +
      'FROM SIZSTAFFLOGINFO t1 ' +
      'INNER JOIN SIZSTAFFLOG t2 ON (t1.LogID=t2.LogID) ' +
      'INNER JOIN SIZNORMSUBITEMINFO t3 ON (t2.GettingInfoID=t3.InfoID) ' +
      'WHERE (t2.GettingInfoID=t2.NowInfoID) AND (t3.InfoID = :InfoID)'
    );
    QParamInt('InfoID', AInfoID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(Result, QFieldInt64('StoreID'));
        QNext;
      end;
    end;
    QClose;
  end;

  function GetReturnedSizEntryIDs(const AInfoID: Integer): TInt64Vector;
  begin
    Result:= nil;
    QSetSQL(
      'SELECT t4.EntryID ' +
      'FROM SIZSTAFFLOGINFO t1 ' +
      'INNER JOIN SIZSTAFFLOG t2 ON (t1.LogID=t2.LogID) ' +
      'INNER JOIN SIZNORMSUBITEMINFO t3 ON (t2.GettingInfoID=t3.InfoID) ' +
      'INNER JOIN SIZSTAFFBACK t4 ON (t1.StoreID=t4.StoreID) ' +
      'WHERE (t2.GettingInfoID=t2.NowInfoID) AND (t3.InfoID = :InfoID)'
    );
    QParamInt('InfoID', AInfoID);
    QOpen;
    if not QIsEmpty then
    begin
      QFirst;
      while not QEOF do
      begin
        VAppend(Result, QFieldInt64('EntryID'));
        QNext;
      end;
    end;
    QClose;
  end;

begin
  if VIsNil(AInfoIDs) then Exit;

  QSetQuery(FQuery);

  //получаем список StoreID, выданных СО СКЛАДА по этой строке подпункту норм
  StoreIDs:= nil;
  for i:=0 to High(AInfoIDs) do
    StoreIDs:= VAdd(StoreIDs, GetStoreIdsFromInfoID(AInfoIDs[i]));

  //получаем список EntryID тех сиз, что были возвращены на склад по этому подпункту норм
  EntryIDs:= nil;
  for i:=0 to High(AInfoIDs) do
    EntryIDs:= VAdd(EntryIDs, GetReturnedSizEntryIDs(AInfoIDs[i]));

  //удаляем эти возвращенные СИЗ из таблицы прихода на склад
  Delete('SIZENTRY', 'EntryID', EntryIDs, False{no commit});

  //удаляем эти StoreID из таблиц списания и возврата СИЗ
  Delete('SIZSTOREWRITEOFF', 'StoreID', StoreIDs, False{no commit});
  Delete('SIZSTAFFBACK', 'StoreID', StoreIDs, False{no commit});

  //отмечаем эти StoreID на складе, как свободные
  i:= 0;
  UpdateInt64ID('SIZSTORE', 'IsBusy', 'StoreID', StoreIDs, i, False{no commit});

  //удаляем сами InfoID
  Delete('SIZNORMSUBITEMINFO', 'InfoID', AInfoIDs, False{no commit});
end;

function TDataBase.SIZNormSubItemInfoWrite(const ASourceSubItemID: Integer;
                                    const ASourceInfo: TNormSubItemInfo;
                                    out ADestInfoIDs: TIntVector;
                                    const ACommit: Boolean = True): Boolean;
var
  i, DestInfoID: Integer;
begin
  ADestInfoIDs:= nil;
  QSetQuery(FQuery);
  try
    for i:=0 to High(ASourceInfo.NameIDs) do
    begin
      //QSetSQL внутри цикла для LastWritedInt32ID
      QSetSQL(
        sqlINSERT('SIZNORMSUBITEMINFO',
                 ['SubItemID', 'NameID', 'Num', 'Life', 'SpecLifeID', 'OrderNum'])
      );
      QParamInt('SubItemID', ASourceSubItemID);
      QParamInt('NameID', ASourceInfo.NameIDs[i]);
      QParamInt('Num', ASourceInfo.Nums[i]);
      QParamInt('Life', ASourceInfo.Lifes[i]);
      QParamInt('SpecLifeID', ASourceInfo.LifeIDs[i]);
      QParamInt('OrderNum', ASourceInfo.OrderNums[i]);
      QExec;
      //определяем ID записанной подстроки
      DestInfoID:= LastWritedInt32ID('SIZNORMSUBITEMINFO');
      //добавляем ID в вектор
      VAppend(ADestInfoIDs, DestInfoID);
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

function TDataBase.SIZAssortmentLoad(out AClassNames: TStrVector;
                              out ASizNames: TStrMatrix;
                              out ASizNameIDs, ASizSizeTypes: TIntMatrix): Boolean;
var
  OldClass, NewClass: String;
  VNames: TStrVector;
  VIDs, VSizeTypes: TIntVector;
begin
  Result:= False;

  AClassNames:= nil;
  ASizNames:= nil;
  ASizNameIDs:= nil;
  ASizSizeTypes:= nil;
  VNames:= nil;
  VIDs:= nil;
  VSizeTypes:= nil;

  QSetQuery(FQuery);
  QSetSQL(
    'SELECT t1.NameID, t1.SizName, t1.SizeType, t2.ClassName ' +
    'FROM SIZNAMES t1 ' +
    'INNER JOIN SIZCLASSES t2 ON (t1.ClassID=t2.ClassID) ' +
    'ORDER BY t2.ClassName, t1.SizName'
  );
  QOpen;
  if not QIsEmpty then
  begin
    OldClass:= EmptyStr;
    QFirst;
    while not QEOF do
    begin
      NewClass:= QFieldStr('ClassName');
      if NewClass<>OldClass then //новый класс
      begin
        //записываем предыдущий класс в матрицы
        if not VIsNil(VNames) then
        begin
          MAppend(ASizNames, VNames);
          MAppend(ASizNameIDs, VIDs);
          MAppend(ASizSizeTypes, VSizeTypes);
        end;
        VNames:= nil;
        VIDs:= nil;
        VSizeTypes:= nil;
        //сохраняем название нового класса
        OldClass:= NewClass;
        //записываем новый класс в вектор
        VAppend(AClassNames, NewClass);
      end;
      VAppend(VNames, QFieldStr('SizName'));
      VAppend(VIDs, QFieldInt('NameID'));
      VAppend(VSizeTypes, QFieldInt('SizeType'));
      QNext;
    end;
    //записываем последний класс
    MAppend(ASizNames, VNames);
    MAppend(ASizNameIDs, VIDs);
    MAppend(ASizSizeTypes, VSizeTypes);
    Result:= True;
  end;
  QClose;
end;

function TDataBase.SIZStaffSpecSizeLoad(const AInfoID: Integer;
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

function TDataBase.SIZStaffSpecSizeWrite(const AInfoID, ATabNumID, ASizeID,
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

function TDataBase.SIZStaffSpecSizeWrite(const AInfoID: Integer;
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

function TDataBase.SIZStaffSpecSizeCopy(const ASourceInfoIDs, ADestInfoIDs: TIntVector;
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
      SIZStaffSpecSizeLoad(ASourceInfoIDs[i], TabNumIDs, SizeIDs, HeightIDs);
      //пробегаем по всем полученным спецразмерам и записываем в инфо цели
      SIZStaffSpecSizeWrite(ADestInfoIDs[i], TabNumIDs, SizeIDs, HeightIDs, False{no commit});
    end;

    if ACommit then QCommit;
    Result:= True;
  except
    if ACommit then QRollback;
  end;
end;

end.

