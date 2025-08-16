unit UInfoForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BCButton, DividerBevel, Spin, StdCtrls, VirtualTrees, DateUtils,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_Const, DK_CtrlUtils, DK_DateUtils, DK_VSTTables,
  //Project utils
  UVars, UConst, UTypes, UUtils;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    NameOrderCheckBox: TCheckBox;
    BirthdayVT: TVirtualStringTree;
    DismissCheckBox: TCheckBox;
    SIZCaptionPanel: TPanel;
    SIZVT: TVirtualStringTree;
    VacationCaptionPanel: TPanel;
    VacationVT: TVirtualStringTree;
    YearCheckBox: TCheckBox;
    CheckPanel: TPanel;
    DividerBevel2: TDividerBevel;
    LeftBottomPanel: TPanel;
    LeftTopPanel: TPanel;
    RightPanel: TPanel;
    BirthdayCaptionPanel: TPanel;
    VertSplitter: TSplitter;
    MonthBCButton: TBCButton;
    LeftPanel: TPanel;
    ToolPanel: TPanel;
    LeftHorSplitter: TSplitter;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure DismissCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameOrderCheckBoxChange(Sender: TObject);
    procedure YearCheckBoxChange(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    MonthDropDown: TMonthDropDown;
    CanUpdate: Boolean;

    BirthdayList: TVSTTable;
    VacationList: TVSTTable;
    SIZList: TVSTTable;

    procedure BirthdayListCreate;
    procedure BirthdayListLoad;

    procedure VacationListCreate;
    procedure VacationListLoad;

    procedure SIZListCreate;
    procedure SIZListLoad;
  public
    procedure DataUpdate;
  end;

var
  InfoForm: TInfoForm;

implementation

{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  CanUpdate:= False;
  YearSpinEdit.Value:= YearOfDate(Date);
  MonthDropDown:= TMonthDropDown.Create(MonthBCButton, @DataUpdate);
  BirthdayListCreate;
  VacationListCreate;
  SIZListCreate;
  CanUpdate:= True;
end;

procedure TInfoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MonthDropDown);
  FreeAndNil(BirthdayList);
  FreeAndNil(VacationList);
  FreeAndNil(SIZList);
end;

procedure TInfoForm.FormShow(Sender: TObject);
begin
  SetToolPanels([ToolPanel]);
  SetCaptionPanels([
    BirthdayCaptionPanel, VacationCaptionPanel, SIZCaptionPanel
  ]);

  MonthDropDown.AutoWidth;

  LeftPanel.Width:= (ClientWidth - VertSplitter.Width - 4) div 2;
  LeftTopPanel.Height:= (ClientHeight - ToolPanel.Height - 6) div 2;

  DataUpdate;
end;

procedure TInfoForm.NameOrderCheckBoxChange(Sender: TObject);
begin
  DataUpdate;
end;

procedure TInfoForm.YearCheckBoxChange(Sender: TObject);
begin
  MonthBCButton.Enabled:= not YearCheckBox.Checked;
  DataUpdate;
end;

procedure TInfoForm.DismissCheckBoxChange(Sender: TObject);
begin
  BirthdayListLoad;
end;

procedure TInfoForm.YearSpinEditChange(Sender: TObject);
begin
  DataUpdate;
end;

procedure TInfoForm.BirthdayListCreate;
begin
  BirthdayList:= TVSTTable.Create(BirthdayVT);
  BirthdayList.SetSingleFont(GridFont);
  BirthdayList.HeaderFont.Style:= [fsBold];
  BirthdayList.AddColumn('Ф.И.О.', 200);
  BirthdayList.AddColumn('День', 100);
  BirthdayList.AddColumn('Год', 100);
  BirthdayList.AddColumn('Исполняется лет', 150);
  BirthdayList.AutosizeColumnEnable(0);
  BirthdayList.Draw;
end;

procedure TInfoForm.BirthdayListLoad;
var
  M: Word;
  i: Integer;
  V, Families, Names, Patronymics: TStrVector;
  BornDates: TDateVector;
begin
  if YearCheckBox.Checked then
    M:= 0
  else
    M:= MonthDropDown.Month;

  BirthdayList.ValuesClear;
  if not DataBase.StaffBirthdaysLoad(M, DismissCheckBox.Checked,
                              not NameOrderCheckBox.Checked,
                              Families, Names, Patronymics, BornDates) then Exit;

  BirthdayList.Visible:= False;
  try
    V:= StaffFullName(Families, Names, Patronymics, False{long});
    BirthdayList.SetColumn('Ф.И.О.', V, taLeftJustify);

    for i:= 0 to High(V) do
      V[i]:= FormatDateTime('d', BornDates[i]) + ' ' +
             MONTHSGEN[MonthOf(BornDates[i])];
    BirthdayList.SetColumn('День', V);

    V:= VFormatDateTime('yyyy', BornDates);
    BirthdayList.SetColumn('Год', V);

    for i:= 0 to High(V) do
      V[i]:= IntToStr(YearSpinEdit.Value - YearOf(BornDates[i]));
    BirthdayList.SetColumn('Исполняется лет', V);

    BirthdayList.Draw;
  finally
    BirthdayList.Visible:= True;
  end;
end;

procedure TInfoForm.VacationListCreate;
begin
  VacationList:= TVSTTable.Create(VacationVT);
  VacationList.SetSingleFont(GridFont);
  VacationList.HeaderFont.Style:= [fsBold];
  VacationList.AddColumn('Ф.И.О.', 200);
  VacationList.AddColumn('Начало отпуска', 150);
  VacationList.AddColumn('Количество дней', 150);
  VacationList.AutosizeColumnEnable('Ф.И.О.');
  VacationList.Draw;
end;

procedure TInfoForm.VacationListLoad;
var
  M: Word;
  i: Integer;
  V, Families, Names, Patronymics, TabNums, PostNames: TStrVector;
  Counts: TIntVector;
  Dates: TDateVector;
begin
  if YearCheckBox.Checked then
    M:= 0
  else
    M:= MonthDropDown.Month;

  VacationList.ValuesClear;
  if not DataBase.StaffVacationsLoad(M, YearSpinEdit.Value,
                              not NameOrderCheckBox.Checked,
                              Families, Names, Patronymics, TabNums, PostNames,
                              Counts, Dates) then Exit;

  VacationList.Visible:= False;
  try
    V:= StaffFullName(Families, Names, Patronymics, TabNums, PostNames, True{short});
    VacationList.SetColumn('Ф.И.О.', V, taLeftJustify);

    for i:= 0 to High(V) do
      V[i]:= FormatDateTime('d', Dates[i]) + ' ' +
             MONTHSGEN[MonthOf(Dates[i])] + ' ' +
             FormatDateTime('yyyy', Dates[i]);
    VacationList.SetColumn('Начало отпуска', V);

    VacationList.SetColumn('Количество дней', VIntToStr(Counts));

    VacationList.Draw;
  finally
    VacationList.Visible:= True;
  end;
end;

procedure TInfoForm.SIZListCreate;
begin
  SIZList:= TVSTTable.Create(SIZVT);
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.AddColumn('Ф.И.О.', 200);
  SIZList.AddColumn('Дата', 100);
  SIZList.AddColumn('Наименование', 150);
  SIZList.AutosizeColumnEnable('Наименование');
  SIZList.Draw;
end;

procedure TInfoForm.SIZListLoad;
var
  i, j, k: Integer;
  D: TDate;
  S: String;
  V1, V2, V3: TStrVector;
  Indexes: TIntVector;
  WriteoffType: Byte;

  SIZNames: TStrVector;
  Genders: TIntVector;
  SIZSizes: TStrMatrix;
  Families, Names, Patronymics, TabNums: TStrMatrix3D;
  SIZCounts: TIntMatrix3D;
  WriteoffDates: TDateMatrix3D;
begin
  if YearCheckBox.Checked then
    D:= LastDayInYear(YearSpinEdit.Value)
  else
    D:= LastDayInMonth(MonthDropDown.Month, YearSpinEdit.Value);

  WriteoffType:= DataBase.SettingLoad('SIZCARDFORM.WRITEOFFTYPE');

  SIZList.ValuesClear;
  if not DataBase.SIZStoreRequestLoad(D, WriteoffType,
                                      SIZNames, Genders, SIZSizes,
                                      Families, Names, Patronymics, TabNums,
                                      SIZCounts, WriteoffDates) then Exit;

  V1:= nil;
  V2:= nil;
  V3:= nil;
  for i:= 0 to High(Families) do
    for j:= 0 to High(Families[i]) do
      for k:= 0 to High(Families[i, j]) do
        begin
          S:= StaffFullName(Families[i, j, k], Names[i, j, k],
                            Patronymics[i, j, k], TabNums[i, j, k], True{short});
          VAppend(V1, S);
          VAppend(V2, SIZNames[i]);
          if WriteoffDates[i, j, k]=0 then
            VAppend(V3, EMPTY_MARK)
          else
            VAppend(V3, FormatDateTime('dd.mm.yyyy', WriteoffDates[i, j, k]));
        end;

  if NameOrderCheckBox.Checked then
  begin
    VSort(V1, Indexes);
    V1:= VReplace(V1, Indexes);
    V2:= VReplace(V2, Indexes);
    V3:= VReplace(V3, Indexes);
  end;

  SIZList.Visible:= False;
  try
    SIZList.SetColumn('Ф.И.О.', V1, taLeftJustify);
    SIZList.SetColumn('Дата', V3);
    SIZList.SetColumn('Наименование', V2, taLeftJustify);
    SIZList.Draw;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TInfoForm.DataUpdate;
begin
  if not CanUpdate then Exit;

  BirthdayListLoad;
  VacationListLoad;
  SIZListLoad;
end;

end.

