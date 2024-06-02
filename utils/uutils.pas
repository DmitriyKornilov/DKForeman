unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BCPanel, BCButton, DateUtils, Forms,
  Buttons,
  //DK packages utils
  DK_CtrlUtils, DK_Color, DK_Vector, DK_Matrix, DK_DateUtils, DK_Const, DK_Fonts,
  DK_StrUtils, DK_VSTEdit, DK_VSTTables, DK_VSTCore,

  //Project utils
  UImages, UDataBase, UConst, UWorkHours, UCalendar, USchedule, UTimetable;

  //UI
  procedure SetToolPanels(const AControls: array of TControl);
  procedure SetCaptionPanels(const AControls: array of TBCPanel);
  procedure SetToolButtons(const AControls: array of TSpeedButton);
  procedure SetCategoryButtons(const AControls: array of TBCButton);
  function ImageListForScreen: TImageList;

  //ID for TVSTTable.ReSelect
  function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                         const ASelectedID: Integer = -1): Integer;
  //Settings
  function SettingByName(const AName: String; const ANames: TStrVector;
                         const AValues: TIntVector): Integer;

  function PeriodToStr(const ABeginDate, AEndDate: TDate): String;
  function VPeriodToStr(const ABeginDates, AEndDates: TDateVector): TStrVector;
  function MPeriodToStr(const ABeginDates, AEndDates: TDateMatrix): TStrMatrix;

  //Staff list
  function GetStaffListForCommonTiming(const AYear, AMonth, AOrderType: Word;
                         out ATabNumIDs: TIntVector;
                         out AStaffNames, ATabNums, APostNames, AScheduleNames: TStrVector;
                         out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateVector;
                         const ANeedLongName: Boolean = False): Boolean;
  function GetStaffListForCommonTiming(const AYear, AMonth, AOrderType: Word;
                         out ACategoryNames: TStrVector;
                         out ATabNumIDs: TIntMatrix;
                         out AStaffNames, ATabNums, APostNames, AScheduleNames: TStrMatrix;
                         out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateMatrix;
                         const ANeedLongName: Boolean = False): Boolean;
  function StaffNameForPersonalTiming(const AF, AN, AP, ATabNum, APostName: String;
                              const ANeedLongName: Boolean = False): String;
  function StaffNamesForPersonalTiming(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                              const ANeedLongName: Boolean = False): TStrVector;
  function StaffNameForScheduleName(const AF, AN, AP, ATabNum: String;
                              const ANeedLongName: Boolean = True): String;
  function StaffNamesForScheduleNames(const AFs, ANs, APs, ATabNums: TStrVector;
                              const ANeedLongName: Boolean = True): TStrVector;

  //Calendar
  procedure CalendarForPeriod(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
  procedure CalendarForYear(const AYear: Word; var ACalendar: TCalendar);
  procedure CalendarForMonth(const AMonth, AYear: Word; var ACalendar: TCalendar);
  //Calendar corrections
  function GetCalendarCorrections(const ADates: TDateVector;
                         const AStatus, ASwapDay: Integer): TCalendarCorrections;
  function GetCalendarCorrections(const ABeginDate, AEndDate: TDate;
                         const AStatus, ASwapDay: Integer): TCalendarCorrections;
  //Schedule corrections
  function GetScheduleCorrections(const ADates: TDateVector;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  function GetScheduleCorrections(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
  //ScheduleShift
  procedure ScheduleCycleToWeek(var ACycle: TScheduleCycle);
  procedure ScheduleCycleToCount(var ACycle: TScheduleCycle; const ACount: Integer; const AFirstDate: TDate);
  procedure ScheduleCycleDateColumnSet(const ATable: TVSTCustomSimpleTable; const ACycle: TScheduleCycle; out AStrDates: TStrVector);
  procedure ScheduleCycleDraw(const ATable: TVSTEdit; const ACycle: TScheduleCycle);
  procedure ScheduleCycleDraw(const ATable: TVSTTable; const ACycle: TScheduleCycle);
  procedure ScheduleShiftByCalendar(const AScheduleID: Integer;
                         const ACalendar: TCalendar; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftVectorByCalendar(const AScheduleIDs: TIntVector;
                         const ACalendar: TCalendar; var ASchedules: TShiftScheduleVector);
  procedure ScheduleShiftForPeriod(const AScheduleID: Integer;
                         const ABeginDate, AEndDate: TDate; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForYear(const AScheduleID: Integer;
                         const AYear: Word; var ASchedule: TShiftSchedule);
  procedure ScheduleShiftForMonth(const AScheduleID: Integer;
                         const AMonth, AYear: Word; var ASchedule: TShiftSchedule);

  //Vacations
  function VacationVector(const ABeginDate, AEndDate, AFirstDate: TDate;
                          const ACount, AAddCount: Integer;
                          const AHolidayDates: TDateVector;
                          out V: TIntVector): Boolean;
  function VacationForPeriod(const ATabNumID: Integer; const ABeginDate, AEndDate: TDate;
                          const AHolidayDates: TDateVector;const AIsPlane: Boolean = False): TIntVector;

  //SchedulePersonal

  { расчет нормы часов ANormHours и кол-ва рабочих дней AWorkDaysCount
    для таб номера c ATabNumID за период  ABeginDate, AEndDate
   (внутри периода могут быть графики разных рабочих недель (40ч, 36ч и т.д.))
   Если указать даты приема/увольнения ARecrutDate и ADismissDate, то будет
   расчет с их учетом (например на неполный месяц)}
  procedure NormHoursAndWorkDaysCounInPeriod(const ATabNumID: Integer;
                               const ABeginDate, AEndDate: TDate; //период
                               const ACalendar: TCalendar; //производственный календарь, перекрывающий период
                               out AWorkDaysCount, ANormHours: Integer;
                               const ARecrutDate: TDate = NULDATE;
                               const ADismissDate: TDate = INFDATE);

  function PostSchedulesByCalendar(const ATabNumID: Integer; const ACalendar: TCalendar;
                          const ASchedBD: TDate = NULDATE; ASchedED: TDate = INFDATE;
                          const APostBD: TDate = NULDATE; APostED: TDate = INFDATE
                          ): TPostScheduleVector;
  function PersonalScheduleByPostSchedules(const ATabNumID: Integer; const ATabNum: String;
                              const ARecrutDate, ADismissDate: TDate;
                              const ACalendar: TCalendar; const AHolidayDates: TDateVector;
                              const APostSchedules: TPostScheduleVector;
                              const AWithPlaneVacation: Boolean = False;
                              const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                              const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                              const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY
                              ): TPersonalSchedule;
  function SchedulePersonalByCalendar(const ATabNumID: Integer; const ATabNum: String;
                              const ARecrutDate, ADismissDate: TDate;
                              const ACalendar: TCalendar; const AHolidayDates: TDateVector;
                              const AWithPlaneVacation: Boolean = False;
                              const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                              const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                              const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY;
                              const ASchedBD: TDate = NULDATE; ASchedED: TDate = INFDATE;
                              const APostBD: TDate = NULDATE; APostED: TDate = INFDATE
                              ): TPersonalSchedule;

  //Учетный период, включающий в себя указанный отчетный месяц
  procedure AccountingPeriodWithMonth(const AMonth, AYear: Word;
                            const AccountingType: Byte; //0-год, 1-квартал, 2 - месяц
                            out ABeginDate, AEndDate: TDate);
  //Часть учетного периода до указанного отчетного месяца
  function AccountingPeriodBeforeMonth(const AMonth, AYear: Word;
                            const AccountingType: Byte; //0-год, 1-квартал, 2 - месяц
                            out ABeginDate, AEndDate: TDate): Boolean;

  //Timetable
  {Актуализация записей табеля за период}
  function TimetableForPeriodUpdate(const ATabNumID: Integer;
                                   const ABeginDate, AEndDate: TDate;
                                   const AHolidayDates: TDateVector): Boolean;
  {Сумма отработанных часов за период }
  function TimetableSumTotalHoursInPeriod(const ATabNumID: Integer;
                                   const ABeginDate, AEndDate: TDate): Integer;
  {Загрузка из базы векторов данных табеля: True - ОК, False - пусто}
  function TimetableDataVectorsLoad(const ATabNumID: Integer; //таб номер
                               const ABeginDate, AEndDate: TDate; //период запроса
                               out ASheduleIDs, AShiftNums,
                                 ATotalHours, ANightHours, AOverHours,
                                 ASkipHours, ASchedHours, AMainMarkDig,
                                 ASkipMarkDig, AIsManualChanged, AAbsence, AIsDayInBase: TIntVector;
                               out AMainMarkStr, ASkipMarkStr: TStrVector): Boolean;
  {Расчет итогов годового табеля}
  function TimetableYearTotalsLoad(const ATabNumID, AYear: Integer): TTimetableTotals;


  {Запись корректировки в табель: True - ОК, False - ошибка}
  function TimetableDaysByCorrectionAdd(const ATabNumID: Integer;
                           const ABeginDate, AEndDate: TDate;
                           const AHolidayDates: TDateVector;
                           ATimetableDay: TTimetableDay): Boolean;
  {Запись табеля по графику: True - ОК, False - ошибка}
  function TimetableDaysByScheduleAdd(const ATabNumID: Integer;
                           const ABeginDate, AEndDate: TDate;
                           const AHolidayDates: TDateVector): Boolean;


implementation

procedure SetToolPanels(const AControls: array of TControl);
var
  i: Integer;
begin
  for i:= 0 to High(AControls) do
    ControlHeight(AControls[i], TOOL_PANEL_HEIGHT_DEFAULT);
end;

procedure SetCaptionPanels(const AControls: array of TBCPanel);
var
  i, h: Integer;
  c: TColor;
begin
  h:= Round(TOOL_PANEL_HEIGHT_DEFAULT*0.65);
  c:= cl3DLight;
  //c:= ColorIncLightness(clBtnFace, -15);
  for i:= 0 to High(AControls) do
  begin
    ControlHeight(AControls[i], h);
    AControls[i].Background.Color:= c;
    AControls[i].Border.Color:= clActiveBorder;
    AControls[i].Rounding.RoundX:= 0;
    AControls[i].Rounding.RoundY:= 0;
  end;
end;

procedure SetToolButtons(const AControls: array of TSpeedButton);
var
  i: Integer;
  ImageList: TImageList;
begin
  ImageList:= ImageListForScreen;
  for i:= 0 to High(AControls) do
  begin
    ControlWidth(AControls[i], TOOL_BUTTON_WIDTH_DEFAULT);
    AControls[i].Images:= ImageList;
  end;
end;

procedure SetCategoryButtons(const AControls: array of TBCButton);
var
  i: Integer;
  c: TColor;
  ImageList: TImageList;
begin
  c:= cl3DLight;
  //c:= ColorIncLightness(clBtnFace, -15);
  ImageList:= ImageListForScreen;
  for i:= 0 to High(AControls) do
  begin
    AControls[i].Images:= ImageList;
    AControls[i].StateNormal.Background.Color:= c;
    AControls[i].StateNormal.Border.Color:= clActiveBorder;
  end;
end;

function ImageListForScreen: TImageList;
begin
  case Screen.PixelsPerInch of
    96 : Result:= Images.PX24;
    120: Result:= Images.PX30;
    144: Result:= Images.PX36;
    168: Result:= Images.PX42;
  end;
end;

function GetSelectedID(const ATable: TVSTTable; const AIDValues: TIntVector;
                       const ASelectedID: Integer = -1): Integer;
begin
  Result:= -1;
  if ASelectedID>0 then
    Result:= ASelectedID
  else if Assigned(ATable) and ATable.IsSelected then
    Result:= AIDValues[ATable.SelectedIndex];
end;

function SettingByName(const AName: String; const ANames: TStrVector;
  const AValues: TIntVector): Integer;
begin
  VSameIndexValue(AName, ANames, AValues, Result);
end;

function PeriodToStr(const ABeginDate, AEndDate: TDate): String;
begin
  Result:= FormatDateTime('dd.mm.yyyy - ', ABeginDate);
  if SameDate(AEndDate, INFDATE) then
    Result:= Result + 'наст. время'
  else
    Result:= Result + FormatDateTime('dd.mm.yyyy', AEndDate);
end;

function VPeriodToStr(const ABeginDates, AEndDates: TDateVector): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to High(ABeginDates) do
    VAppend(Result, PeriodToStr(ABeginDates[i], AEndDates[i]));
end;

function MPeriodToStr(const ABeginDates, AEndDates: TDateMatrix): TStrMatrix;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to High(ABeginDates) do
    MAppend(Result, VPeriodToStr(ABeginDates[i], AEndDates[i]));
end;

function GetStaffListForCommonTiming(const AYear, AMonth, AOrderType: Word;
           out ATabNumIDs: TIntVector;
           out AStaffNames, ATabNums, APostNames, AScheduleNames: TStrVector;
           out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateVector;
           const ANeedLongName: Boolean = False): Boolean;

var
  BD, ED: TDate;
  Fs, Ns, Ps: TStrVector;
begin
  FirstLastDayInMonth(AMonth, AYear, BD, ED);
  Result:= DataBase.StaffListForCommonTimingLoad(BD, ED, AOrderType, ATabNumIDs,
         ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs,
         Fs, Ns, Ps, ATabNums, APostNames, AScheduleNames);
  if ANeedLongName then
    AStaffNames:= VNameLong(Fs, Ns, Ps)
  else
    AStaffNames:= VNameShort(Fs, Ns, Ps);
end;

function GetStaffListForCommonTiming(const AYear, AMonth, AOrderType: Word;
                         out ACategoryNames: TStrVector;
                         out ATabNumIDs: TIntMatrix;
                         out AStaffNames, ATabNums, APostNames, AScheduleNames: TStrMatrix;
                         out ARecrutDates, ADismissDates, APostBDs, APostEDs, AScheduleBDs, AScheduleEDs: TDateMatrix;
                         const ANeedLongName: Boolean = False): Boolean;
var
  TabNumIDs: TIntVector;
  StaffNames, TabNums, PostNames, ScheduleNames: TStrVector;
  RecrutDates, DismissDates, PostBDs, PostEDs, ScheduleBDs, ScheduleEDs: TDateVector;
  i, N1, N2: Integer;
  S: String;
  V: TStrVector;

  procedure AddToMatrix(const AIndex1, AIndex2: Integer);
  begin
    MAppend(ATabNumIDs, VCut(TabNumIDs, AIndex1, AIndex2));
    MAppend(AStaffNames, VCut(StaffNames, AIndex1, AIndex2));
    MAppend(ATabNums, VCut(TabNums, AIndex1, AIndex2));
    MAppend(APostNames, VCut(PostNames, AIndex1, AIndex2));
    MAppend(AScheduleNames, VCut(ScheduleNames, AIndex1, AIndex2));

    MAppend(ARecrutDates, VCut(RecrutDates, AIndex1, AIndex2));
    MAppend(ADismissDates, VCut(DismissDates, AIndex1, AIndex2));
    MAppend(APostBDs, VCut(PostBDs, AIndex1, AIndex2));
    MAppend(APostEDs, VCut(PostEDs, AIndex1, AIndex2));
    MAppend(AScheduleBDs, VCut(ScheduleBDs, AIndex1, AIndex2));
    MAppend(AScheduleEDs, VCut(ScheduleEDs, AIndex1, AIndex2));
  end;

begin
  ACategoryNames:= nil;
  ATabNumIDs:= nil;
  AStaffNames:= nil;
  ATabNums:= nil;
  APostNames:= nil;
  AScheduleNames:= nil;
  ARecrutDates:= nil;
  ADismissDates:= nil;
  APostBDs:= nil;
  APostEDs:= nil;
  AScheduleBDs:= nil;
  AScheduleEDs:= nil;

  Result:= GetStaffListForCommonTiming(AYear, AMonth, AOrderType,
           TabNumIDs, StaffNames, TabNums, PostNames, ScheduleNames,
           RecrutDates, DismissDates, PostBDs, PostEDs, ScheduleBDs, ScheduleEDs,
           ANeedLongName);

  if not Result then Exit;

  if AOrderType=0 then
    V:= ScheduleNames
  else
    V:= PostNames;

  S:= V[0];
  N1:= 0;
  for i:= 1 to High(V) do
  begin
    if V[i]<>S then
    begin
      N2:= i-1;
      VAppend(ACategoryNames, S);
      AddToMatrix(N1, N2);
      N1:= i;
      S:= V[i];
    end;
  end;
  N2:= High(V);
  VAppend(ACategoryNames, S);
  AddToMatrix(N1, N2);
end;

function StaffNameForPersonalTiming(const AF, AN, AP, ATabNum, APostName: String;
                            const ANeedLongName: Boolean = False): String;
begin
  Result:= StaffNameForScheduleName(AF, AN, AP, ATabNum, ANeedLongName) + ' - ';
  if SSame(APostName, '<не указана>') then
     Result:= Result + '<должность не указана>'
  else
    Result:= Result + APostName;
end;

function StaffNamesForPersonalTiming(const AFs, ANs, APs, ATabNums, APostNames: TStrVector;
                            const ANeedLongName: Boolean = False): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ATabNums) then Exit;
  VDim(Result, Length(ATabNums));
  for i:= 0 to High(Result) do
    Result[i]:= StaffNameForPersonalTiming(AFs[i], ANs[i], APs[i], ATabNums[i], APostNames[i], ANeedLongName);
end;

function StaffNameForScheduleName(const AF, AN, AP, ATabNum: String;
                              const ANeedLongName: Boolean = True): String;
begin
  if ANeedLongName then
    Result:= SNameLong(AF, AN, AP)
  else
    Result:= SNameShort(AF, AN, AP);
  Result:= Result + ' [таб.№ ' + ATabNum + ']';
end;

function StaffNamesForScheduleNames(const AFs, ANs, APs, ATabNums: TStrVector;
                              const ANeedLongName: Boolean = True): TStrVector;
var
  i: Integer;
begin
  Result:= nil;
  if VIsNil(ATabNums) then Exit;
  VDim(Result, Length(ATabNums));
  for i:= 0 to High(Result) do
    Result[i]:= StaffNameForScheduleName(AFs[i], ANs[i], APs[i], ATabNums[i], ANeedLongName);
end;

procedure AccountingPeriodWithMonth(const AMonth, AYear: Word;
                          const AccountingType: Byte; //0-год, 1-квартал, 2 - месяц
                          out ABeginDate, AEndDate: TDate);
begin
  case AccountingType of
    0: FirstLastDayInYear(AYear, ABeginDate, AEndDate);  //год
    1: FirstLastDayInQuarter(QuarterNumber(AMonth), AYear, ABeginDate, AEndDate); //квартал
    2: FirstLastDayInMonth(AMonth, AYear, ABeginDate, AEndDate); //месяц
  end;
end;

function AccountingPeriodBeforeMonth(const AMonth, AYear: Word;
                            const AccountingType: Byte; //0-год, 1-квартал, 2 - месяц
                            out ABeginDate, AEndDate: TDate): Boolean;
begin
  Result:= False;
  ABeginDate:= NULDATE;
  AEndDate:= NULDATE;
  if AccountingType>1 then Exit;
  case AccountingType of
  0:
    begin //год
      if AMonth=1 then Exit; //1-ый месяц в году, до него ничего нет
      ABeginDate:= FirstDayInYear(AYear);
    end;
  1:
    begin //квартал
      if MonthNumberInQuarter(AMonth)=1 then Exit;  //1-ый месяц в квартале, до него ничего нет
      ABeginDate:= FirstDayInQuarter(QuarterNumber(AMonth), AYear);
    end;
  end;
  AEndDate:= LastDayInMonth(AMonth-1, AYear);
  Result:= True;
end;

function TimetableDaysByCorrectionAdd(const ATabNumID: Integer;
                           const ABeginDate, AEndDate: TDate;
                           const AHolidayDates: TDateVector;
                           ATimetableDay: TTimetableDay): Boolean;
var
  RecrutDate, DismissDate, BD, ED: TDate;
  Calendar: TCalendar;
  Schedule: TPersonalSchedule;
begin
  Result:= False;
  Schedule:= nil;
  Calendar:= nil;
  //получаем даты приема увольнения
  if not DataBase.StaffTabNumWorkPeriodLoad(ATabNumID, RecrutDate, DismissDate) then Exit;
  //ограничене периода на время работы
  if not IsPeriodIntersect(ABeginDate, AEndDate, RecrutDate, DismissDate, BD, ED) then Exit;
  //расчет календаря
  Calendar:= nil;
  CalendarForPeriod(BD, ED, Calendar);
  //расчет графика
  Schedule:= SchedulePersonalByCalendar(ATabNumID, EmptyStr,
                         RecrutDate, DismissDate, Calendar, AHolidayDates);

  try
    if Schedule.Calculated then
      Result:= DataBase.TimetableDaysByCorrectionAdd(ATabNumID, ATimetableDay,
                                                     Calendar, Schedule);
  finally
    FreeAndNil(Schedule);
    FreeAndNil(Calendar);
  end;
end;

function TimetableDaysByScheduleAdd(const ATabNumID: Integer;
                           const ABeginDate, AEndDate: TDate;
                           const AHolidayDates: TDateVector): Boolean;
var
  RecrutDate, DismissDate, BD, ED: TDate;
  Calendar: TCalendar;
  Schedule: TPersonalSchedule;
begin
  Result:= False;
  Schedule:= nil;
  Calendar:= nil;
  //получаем даты приема увольнения
  if not DataBase.StaffTabNumWorkPeriodLoad(ATabNumID, RecrutDate, DismissDate) then Exit;
  //ограничене периода на время работы
  if not IsPeriodIntersect(ABeginDate, AEndDate, RecrutDate, DismissDate, BD, ED) then Exit;
  //расчет календаря
  Calendar:= nil;
  CalendarForPeriod(BD, ED, Calendar);
  //расчет графика
  Schedule:= SchedulePersonalByCalendar(ATabNumID, EmptyStr,
                         RecrutDate, DismissDate, Calendar, AHolidayDates);

  try
    if Schedule.Calculated then
      Result:= DataBase.TimetableDaysByScheduleAdd(ATabNumID, Calendar, Schedule);
  finally
    FreeAndNil(Schedule);
    FreeAndNil(Calendar);
  end;
end;

function TimetableForPeriodUpdate(const ATabNumID: Integer;
                            const ABeginDate, AEndDate: TDate;
                            const AHolidayDates: TDateVector): Boolean;
var
  FirtsWritedDate, LastWritedDate, RecrutDate, DismissDate, BD, ED: TDate;
  Calendar: TCalendar;
  Schedule: TPersonalSchedule;
begin
  Result:= False;
  Schedule:= nil;
  Calendar:= nil;
  //получаем даты приема увольнения
  if not DataBase.StaffTabNumWorkPeriodLoad(ATabNumID, RecrutDate, DismissDate) then Exit;
  //ограничене периода на время работы
  if not IsPeriodIntersect(ABeginDate, AEndDate, RecrutDate, DismissDate, BD, ED) then Exit;
  //первая и последняя даты записанного  в базу табеля
  if not DataBase.TimetableFirstLastWritedDatesLoad(ATabNumID, FirtsWritedDate, LastWritedDate) then Exit;
  //ограничение периода
  if not IsPeriodIntersect(BD, ED, FirtsWritedDate, LastWritedDate, BD, ED) then Exit;
  //расчет календаря
  Calendar:= nil;
  CalendarForPeriod(BD, ED, Calendar);
  //расчет графика
  Schedule:= SchedulePersonalByCalendar(ATabNumID, EmptyStr,
                         RecrutDate, DismissDate, Calendar, AHolidayDates);

  try
    if Schedule.Calculated then
      Result:= DataBase.TimetableByScheduleUpdate(ATabNumID, Calendar, Schedule);
  finally
    FreeAndNil(Schedule);
    FreeAndNil(Calendar);
  end;
end;

function TimetableSumTotalHoursInPeriod(const ATabNumID: Integer;
  const ABeginDate, AEndDate: TDate): Integer;
begin
  Result:= DataBase.TimetableSumTotalHoursInPeriodLoad(ATabNumID, ABeginDate, AEndDate);
end;

function TimetableDataVectorsLoad(const ATabNumID: Integer; //таб номер
                               const ABeginDate, AEndDate: TDate; //период запроса
                               out ASheduleIDs, AShiftNums,
                                 ATotalHours, ANightHours, AOverHours,
                                 ASkipHours, ASchedHours, AMainMarkDig,
                                 ASkipMarkDig, AIsManualChanged, AAbsence, AIsDayInBase: TIntVector;
                               out AMainMarkStr, ASkipMarkStr: TStrVector): Boolean;
begin
  Result:= DataBase.TimetableDataVectorsLoad(ATabNumID, ABeginDate, AEndDate,
                  ASheduleIDs, AShiftNums, ATotalHours, ANightHours, AOverHours,
                  ASkipHours, ASchedHours, AMainMarkDig, ASkipMarkDig,
                  AIsManualChanged, AAbsence, AIsDayInBase, AMainMarkStr, ASkipMarkStr);
end;

function TimetableYearTotalsLoad(const ATabNumID, AYear: Integer): TTimetableTotals;
var
  i: Integer;
  BD, ED: TDate;
begin
  //данные за кварталы с 1 по 4; 5,6 - 1 и 2 полугодие; 7 - за год
  for i:= 1 to 7 do
  begin
    case i of
    1..4: FirstLastDayInQuarter(i, AYear, BD, ED);
    5,6:  FirstLastDayInHalfYear(i-4, AYear, BD, ED);
    7:    FirstLastDayInYear(AYear, BD, ED);
    end;
    DataBase.TimetableDataTotalsLoad(ATabNumID, BD, ED,
                          Result.ShiftCount[i], Result.WorkDaysCount[i],
                          Result.NotWorkDaysCount[i], Result.TotalHours[i],
                          Result.NightHours[i], Result.OverHours[i],
                          Result.HolidayHours[i], Result.SkipDaysCount[i],
                          Result.SkipHours[i], Result.SkipMarksStr[i],
                          Result.SkipDaysHoursStr[i]);
  end;
end;

procedure CalendarForPeriod(const ABeginDate, AEndDate: TDate; var ACalendar: TCalendar);
begin
  if not Assigned(ACalendar) then
    ACalendar:= TCalendar.Create;
  DataBase.CalendarLoad(ABeginDate, AEndDate, ACalendar);
end;

procedure CalendarForYear(const AYear: Word; var ACalendar: TCalendar);
var
  BD, ED: TDate;
begin
  FirstLastDayInYear(AYear, BD, ED);
  CalendarForPeriod(BD, ED, ACalendar);
end;

procedure CalendarForMonth(const AMonth, AYear: Word; var ACalendar: TCalendar);
var
  BD, ED: TDate;
begin
  FirstLastDayInMonth(AMonth, AYear, BD, ED);
  CalendarForPeriod(BD, ED, ACalendar);
end;

function GetCalendarCorrections(const ADates: TDateVector;
                       const AStatus, ASwapDay: Integer): TCalendarCorrections;
var
  n: Integer;
begin
  Result:= CalendarCorrectionsEmpty;
  Result.Dates:= VCut(ADates);
  n:= Length(ADates);
  VDim(Result.Statuses, n, AStatus);
  VDim(Result.SwapDays, n, ASwapDay);
end;

function GetCalendarCorrections(const ABeginDate, AEndDate: TDate;
  const AStatus, ASwapDay: Integer): TCalendarCorrections;
var
  Dates: TDateVector;
begin
  Dates:= VCreateDate(ABeginDate, AEndDate);
  Result:= GetCalendarCorrections(Dates, AStatus, ASwapDay);
end;

function GetScheduleCorrections(const ADates: TDateVector;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
var
  n: Integer;
begin
  Result:= EmptyScheduleCorrections;
  Result.Dates:= VCut(ADates);
  n:= Length(ADates);
  VDim(Result.HoursTotal, n, AHoursTotal);
  VDim(Result.HoursNight, n, AHoursNight);
  VDim(Result.DigMarks, n, ADigMark);
  VDim(Result.ShiftNums, n, AShiftNum);
  VDim(Result.StrMarks, n, AStrMark);
end;

function GetScheduleCorrections(const ABeginDate, AEndDate: TDate;
                         const AHoursTotal, AHoursNight, ADigMark, AShiftNum: Integer;
                         const AStrMark: String = ''): TScheduleCorrections;
var
  Dates: TDateVector;
begin
  Dates:= VCreateDate(ABeginDate, AEndDate);
  Result:= GetScheduleCorrections(Dates, AHoursTotal, AHoursNight, ADigMark,
                                  AShiftNum, AStrMark);
end;

procedure ScheduleCycleToWeek(var ACycle: TScheduleCycle);
var
  i, x, n: Integer;
  S: String;
begin
  ACycle.IsWeek:= True;
  ACycle.Count:= 0;
  n:= Length(ACycle.Dates);

  VRedim(ACycle.Dates, 7);
  for i:= 0 to 6 do
    ACycle.Dates[i]:= IncDay(MONDAY_DATE, i);
  if n=7 then Exit;
  S:= DataBase.TimetableStrMarkLoad(1);
  VRedim(ACycle.HoursTotal, 7, 0);
  VRedim(ACycle.HoursNight, 7, 0);
  VRedim(ACycle.DigMarks, 7, 1);
  VRedim(ACycle.StrMarks, 7, S);
  VRedim(ACycle.ShiftNums, 7, 0);
  if n>7 then Exit;

  S:= DataBase.TimetableStrMarkLoad(26);
  if n<=5 then
  begin
    x:= 8*WORKHOURS_DENOMINATOR;
    for i:= n to 4 do
    begin
      ACycle.HoursTotal[i]:= x;
      ACycle.ShiftNums[i]:= i+1;
    end;
    for i:= 5 to 6 do
    begin
      ACycle.DigMarks[i]:= 26 {B};
      ACycle.StrMarks[i]:= S;
    end;
  end
  else begin
    for i:= n-1 to 6 do
    begin
      ACycle.DigMarks[i]:= 26 {B};
      ACycle.StrMarks[i]:= S;
    end;
  end;
end;

procedure ScheduleCycleToCount(var ACycle: TScheduleCycle; const ACount: Integer;
                               const AFirstDate: TDate);
var
  i, n: Integer;
begin
  if ACycle.IsWeek then
    n:= 7
  else
    n:= ACycle.Count;
  ACycle.IsWeek:= False;
  ACycle.Count:= ACount;

  VRedim(ACycle.Dates, ACount);
  for i:= 0 to ACount-1 do
    ACycle.Dates[i]:= IncDay(AFirstDate, i);
  if n=ACount then Exit;

  VRedim(ACycle.HoursTotal, ACount, 0);
  VRedim(ACycle.HoursNight, ACount, 0);
  VRedim(ACycle.DigMarks, ACount, 26);
  VRedim(ACycle.StrMarks, ACount, DataBase.TimetableStrMarkLoad(26));
  VRedim(ACycle.ShiftNums, ACount, 0);
end;

procedure ScheduleCycleDateColumnSet(const ATable: TVSTCustomSimpleTable;
                                     const ACycle: TScheduleCycle;
                                     out AStrDates: TStrVector);
begin
  if ACycle.IsWeek then
  begin
    ATable.RenameColumn(0, 'День');
    AStrDates:= VCreateStr(WEEKDAYSSHORT);
  end
  else begin
    ATable.RenameColumn(0, SCHEDULE_CORRECTION_COLUMN_NAMES[0]);
    AStrDates:= VDateToStr(ACycle.Dates);
  end;
end;

procedure ScheduleCycleDraw(const ATable: TVSTEdit; const ACycle: TScheduleCycle);
var
  StrDates: TStrVector;
begin
  ScheduleCycleDateColumnSet(ATable, ACycle, StrDates);

  ATable.Visible:= False;
  try
    ATable.ValuesClear;
    ATable.SetColumnRowTitles(StrDates);
    ATable.SetColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[1], ACycle.ShiftNums);
    ATable.SetColumnDouble(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursIntToFrac(ACycle.HoursTotal));
    ATable.SetColumnDouble(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursIntToFrac(ACycle.HoursNight));
    ATable.SetColumnInteger(SCHEDULE_CORRECTION_COLUMN_NAMES[4], ACycle.DigMarks);
    ATable.Draw;
  finally
    ATable.Visible:= True;
  end;

end;

procedure ScheduleCycleDraw(const ATable: TVSTTable; const ACycle: TScheduleCycle);
var
  StrDates, StrShiftNums: TStrVector;
begin
  ScheduleCycleDateColumnSet(ATable, ACycle, StrDates);
  StrShiftNums:= VIntToStr(ACycle.ShiftNums);
  VChangeIf(StrShiftNums, '0', EMPTY_MARK);

  ATable.Visible:= False;
  try
    ATable.ValuesClear;
    ATable.SetColumn({SCHEDULE_CORRECTION_COLUMN_NAMES[0]}0, StrDates);
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[1], StrShiftNums);
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[2], VWorkHoursToStr(ACycle.HoursTotal));
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[3], VWorkHoursToStr(ACycle.HoursNight));
    ATable.SetColumn(SCHEDULE_CORRECTION_COLUMN_NAMES[4], ACycle.StrMarks);
    ATable.Draw;
  finally
    ATable.Visible:= True;
  end;
end;

procedure ScheduleShiftByCalendar(const AScheduleID: Integer;
  const ACalendar: TCalendar; var ASchedule: TShiftSchedule);
var
  Cycle: TScheduleCycle;
  V: TIntVector;
  WeekHours: Integer;
  Correct: TScheduleCorrections;
begin
  if not Assigned(ASchedule) then
    ASchedule:= TShiftSchedule.Create;
  WeekHours:= DataBase.ValueInt32Int32ID('SCHEDULEMAIN', 'WeekHours', 'ScheduleID', AScheduleID);
  DataBase.ScheduleCycleLoad(AScheduleID, V, Cycle);
  DataBase.ScheduleShiftCorrectionsLoad(AScheduleID, V, Correct, ACalendar.BeginDate, ACalendar.EndDate);
  ASchedule.Calc(ACalendar, WeekHours, Cycle, Correct);
end;

procedure ScheduleShiftVectorByCalendar(const AScheduleIDs: TIntVector;
  const ACalendar: TCalendar; var ASchedules: TShiftScheduleVector);
var
  i: Integer;
  Schedule: TShiftSchedule;
begin
  VSDel(ASchedules);
  for i:= 0 to High(AScheduleIDs) do
  begin
    Schedule:= TShiftSchedule.Create;
    ScheduleShiftByCalendar(AScheduleIDs[i], ACalendar, Schedule{%H-});
    VSAppend(ASchedules, Schedule);
  end;
end;

procedure ScheduleShiftForPeriod(const AScheduleID: Integer;
  const ABeginDate, AEndDate: TDate; var ASchedule: TShiftSchedule);
var
  Calendar: TCalendar;
begin
  Calendar:= TCalendar.Create;
  try
    CalendarForPeriod(ABeginDate, AEndDate, Calendar);
    ScheduleShiftByCalendar(AScheduleID, Calendar, ASchedule);
  finally
    FreeAndNil(Calendar);
  end;
end;

procedure ScheduleShiftForYear(const AScheduleID: Integer; const AYear: Word;
  var ASchedule: TShiftSchedule);
var
  BD, ED: TDate;
begin
  FirstLastDayInYear(AYear, BD, ED);
  ScheduleShiftForPeriod(AScheduleID, BD, ED, ASchedule);
end;

procedure ScheduleShiftForMonth(const AScheduleID: Integer;
  const AMonth, AYear: Word; var ASchedule: TShiftSchedule);
var
  BD, ED: TDate;
begin
  FirstLastDayInMonth(AMonth, AYear, BD, ED);
  ScheduleShiftForPeriod(AScheduleID, BD, ED, ASchedule);
end;

function VacationVector(const ABeginDate, AEndDate, AFirstDate: TDate;
                        const ACount, AAddCount: Integer;
                        const AHolidayDates: TDateVector;
                        out V: TIntVector): Boolean;
var
  HolidaysCount, I1, I2, i: Integer;
  LastDate, BD, ED: TDate;
  Holidays: TDateVector;
begin
  Result:= False;
  V:= nil;
  //последняя дата отпуска без учета попадания праздничных дней
  LastDate:= IncDay(AFirstDate, ACount + AAddCount - 1);
  //рассчитаем дату окончания отпуска с учетом попавших праздников
  HolidaysCount:= VCountIn(AHolidayDates, AFirstDate, LastDate); //кол-во праздничных дней, попавших в отпуск
  while HolidaysCount>0 do
  begin
    BD:= IncDay(LastDate,1); //начальная дата периода, на который увеличился отпуск из-за попавших праздников
    LastDate:= IncDay(LastDate, HolidaysCount); //конечная дата периода, на который увеличился отпуск из-за попавших праздников
    HolidaysCount:= VCountIn(AHolidayDates, BD, LastDate); // дополнительное кол-во праздничных дней, попавших в этот период
  end;
  //определяем, пересекается ли полученный период отпуска с запрашиваемым периодом
  //если да, получаем период пересечения, если нет, то выходим
  if not IsPeriodIntersect(ABeginDate, AEndDate, AFirstDate, LastDate, BD, ED) then Exit;

  Result:= True;
  //определяем вектор праздничных дней в полученном периоде отпуска
  Holidays:= VCut(AHolidayDates, AFirstDate, LastDate);
  //кол-во праздников, попавших в отпуск до начала периода запроса (для определения I1 - далее)
  HolidaysCount:= VCountBefore(Holidays, BD);
  //определяем вектор праздничных дней в периоде пересечения отпуска и запроса
  Holidays:= VCut(AHolidayDates, BD, ED);
  //создаем вектор длиной в запрашиваемый период с флагом отсутствия отпуска
  VDim(V, DaysInPeriod(ABeginDate, AEndDate), VACATION_NO);
  //заполняем период пересечения отпуска и запрашиваемого периода флагом основного отпуска
  I1:= DaysBetweenDates(ABeginDate, BD);
  I2:= DaysBetweenDates(ABeginDate, ED);
  VChangeIn(V, VACATION_MAIN, I1, I2);
  //изменяем флаг на VACATION_HOLIDAY в праздничные дни
  for i := 0 to High(Holidays) do
  begin
    I1:= DaysBetweenDates(ABeginDate, Holidays[i]);
    V[I1]:= VACATION_HOLIDAY;
  end;
  //если нет дополнительного отпуска - выходим
  if AAddCount=0 then Exit;

  I1:= 0;
  //если отпуск начался раньше периода запроса
  if CompareDate(AFirstDate, ABeginDate)<0 then
    //то на начало периода прошло уже I1 дней отпуска
      I1:= DaysBetweenDates(AFirstDate, ABeginDate) - HolidaysCount;
  //пробегаем по всем дням
  for i := 0 to High(V) do
  begin
    if V[i]=VACATION_MAIN then //если отмечен основной отпуск
    begin
      Inc(I1); //кол-во уже прошедших дней основного отпуска
      if I1> ACount then  //если основной отпуск кончился
        V[i]:=VACATION_ADDITION; //записываем дополнительный
    end;
  end;
end;

function VacationForPeriod(const ATabNumID: Integer;
                            const ABeginDate, AEndDate: TDate;
                            const AHolidayDates: TDateVector;
                            const AIsPlane: Boolean = False): TIntVector;
var
  FirstDates: TDateVector;
  Counts, AddCounts, V: TIntVector;
  i: Integer;
begin
  Result:= nil;
  //создаем вектор статусов отпуска на указанный период с флагом VACATION_NO
  VDim(Result, DaysInPeriod(ABeginDate, AEndDate), VACATION_NO);
  //достаем из базы данные по отпуску, если данных нет, то выход
  if not DataBase.VacationLoad(ATabNumID, ABeginDate, AEndDate, FirstDates,
                                   Counts, AddCounts, AIsPlane) then Exit;
  V:= nil;
  for i:= 0 to High(FirstDates) do
    if VacationVector(ABeginDate, AEndDate, FirstDates[i], Counts[i], AddCounts[i], AHolidayDates, V) then
      Result:= VSum(Result, V);
end;

procedure NormHoursAndWorkDaysCounInPeriod(const ATabNumID: Integer;  //таб номер
                               const ABeginDate, AEndDate: TDate; //период
                               const ACalendar: TCalendar; //производственный календарь, перекрывающий период
                               out AWorkDaysCount, ANormHours: Integer;
                               const ARecrutDate: TDate = NULDATE;
                               const ADismissDate: TDate = INFDATE);
var
  //CutCalendar: TCalendar;
  WeekHours, IDs, ScheduleIDs: TIntVector;
  BDs, EDs: TDateVector;
  ScheduleNames: TStrVector;
  i: Integer;
  BD, ED: TDate;
begin
  AWorkDaysCount:= 0;
  ANormHours:= 0;
  //определяем период пересечения запрашиваемого периода и периода работы
  if not IsPeriodIntersect(ABeginDate, AEndDate, ARecrutDate, ADismissDate, BD, ED) then Exit;
  //определяем кол-во дней
  AWorkDaysCount:= ACalendar.WorkDaysCount(BD, ED);
  //достаем из базы список рабочих часов в неделю с соответствующими периодами
  if not DataBase.StaffScheduleHistoryLoad(ATabNumID, IDs, ScheduleIDs, WeekHours,
    BDs, EDs, ScheduleNames, BD, ED) then Exit;
  //суммируем кол-во рабочих часов по полученным периодам
  for i:=0 to High(WeekHours) do
    ANormHours:= ANormHours +
      ACalendar.SumWorkHoursInt(WeekHours[i], MaxDate(BD, BDs[i]), MinDate(ED, EDs[i]));
end;

function PostSchedulesByCalendar(const ATabNumID: Integer; const ACalendar: TCalendar;
                          const ASchedBD: TDate = NULDATE; ASchedED: TDate = INFDATE;
                          const APostBD: TDate = NULDATE; APostED: TDate = INFDATE ): TPostScheduleVector;
var
  PostSchedule: TPostSchedule;
  ScheduleIDs, WeekHours, V: TIntVector;
  S: TStrVector;
  BDs, EDs: TDateVector;
  LimitBD, LimitED: TDate;
  i: Integer;

  function CreatePS(const AScheduleID, AHoursInWeek: Integer;
                    const AScheduleBD, AScheduleED, APstBD, APstED: TDate): TPostSchedule;
  var
    Cycle: TScheduleCycle;
    Correct: TScheduleCorrections;
  begin
    DataBase.ScheduleCycleLoad(AScheduleID, V, Cycle);
    DataBase.ScheduleShiftCorrectionsLoad(AScheduleID, V, Correct,
                                      ACalendar.BeginDate, ACalendar.EndDate);
    Result:= TPostSchedule.Create;
    Result.Calc(ACalendar, AHoursInWeek, Cycle, Correct,
                AScheduleBD, AScheduleED, APstBD, APstED);
  end;

begin
  Result:= nil;
  LimitBD:= MaxDate(APostBD, ASchedBD);
  LimitED:= MinDate(APostED, ASchedED);
  //загружаем графики сменности на период запроса, если они есть
  DataBase.StaffScheduleHistoryLoad(ATabNumID, V, ScheduleIDs, WeekHours,
                                    BDs, EDs, S, LimitBD, LimitED, False);
  for i:= 0 to High(ScheduleIDs) do
  begin
    //создаем сменный персональный график на подпериод
    PostSchedule:= CreatePS(ScheduleIDs[i], WeekHours[i], BDs[i], EDs[i], LimitBD, LimitED);
    //добавляем его к итоговому графику
    VSAppend(Result, PostSchedule);
  end;
end;

function PersonalScheduleByPostSchedules(const ATabNumID: Integer; const ATabNum: String;
                              const ARecrutDate, ADismissDate: TDate;
                              const ACalendar: TCalendar; const AHolidayDates: TDateVector;
                              const APostSchedules: TPostScheduleVector;
                              const AWithPlaneVacation: Boolean = False;
                              const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                              const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                              const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY ): TPersonalSchedule;
var
  i: Integer;
  Vac, V: TIntVector;
  PersonalCorrect: TScheduleCorrections;
begin
  Result:= nil;
  //загружаем персональные корректировки
  DataBase.SchedulePersonalCorrectionsLoad(ATabNumID, V, PersonalCorrect,
                                        ACalendar.BeginDate, ACalendar.EndDate);
  //загружаем вектор статусов отпуска
  Vac:= VacationForPeriod(ATabNumID, ACalendar.BeginDate, ACalendar.EndDate,
                          AHolidayDates, AWithPlaneVacation);
  //создаем персональный график
  Result:= TPersonalSchedule.Create(ATabNumID, ATabNum, ARecrutDate, ADismissDate,
    Vac, PersonalCorrect, AStrMarkVacationMain, AStrMarkVacationAddition, AStrMarkVacationHoliday);
  //заполняем персональный график
  for i:= 0 to High(APostSchedules) do
     Result.Add(APostSchedules[i], i=High(APostSchedules));
end;

function SchedulePersonalByCalendar(const ATabNumID: Integer; const ATabNum: String;
                const ARecrutDate, ADismissDate: TDate;
                const ACalendar: TCalendar; const AHolidayDates: TDateVector;
                const AWithPlaneVacation: Boolean = False;
                const AStrMarkVacationMain: String = STRMARK_VACATIONMAIN;
                const AStrMarkVacationAddition: String = STRMARK_VACATIONADDITION;
                const AStrMarkVacationHoliday: String = STRMARK_VACATIONHOLIDAY;
                const ASchedBD: TDate = NULDATE; ASchedED: TDate = INFDATE;
                const APostBD: TDate = NULDATE; APostED: TDate = INFDATE): TPersonalSchedule;
var
  PostSchedules: TPostScheduleVector;
begin
  Result:= nil;
  //загружаем графики работы в должности и "в графике"
  PostSchedules:= PostSchedulesByCalendar(ATabNumID, ACalendar, ASchedBD, ASchedED, APostBD, APostED);
  if Length(PostSchedules)=0 then Exit;
  try
    Result:= PersonalScheduleByPostSchedules(ATabNumID, ATabNum, ARecrutDate, ADismissDate,
                                ACalendar, AHolidayDates, PostSchedules, AWithPlaneVacation,
                               AStrMarkVacationMain, AStrMarkVacationAddition,
                               AStrMarkVacationHoliday);
  finally
    VSDel(PostSchedules);
  end;
end;

end.

