unit UInfoForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  BCButton, DividerBevel, Spin, StdCtrls, VirtualTrees, DateUtils,
  //DK packages utils
  DK_Vector, DK_Const, DK_CtrlUtils, DK_DateUtils, DK_VSTTables,
  //Project utils
  UVars, UConst, UTypes, UUtils;

type

  { TInfoForm }

  TInfoForm = class(TForm)
    BirthdayVT: TVirtualStringTree;
    DismissCheckBox: TCheckBox;
    DividerBevel4: TDividerBevel;
    OrderLabel: TLabel;
    OrderPanel: TPanel;
    DateRadioButton: TRadioButton;
    NameRadioButton: TRadioButton;
    YearCheckBox: TCheckBox;
    CheckPanel: TPanel;
    DividerBevel3: TDividerBevel;
    SIZVT: TVirtualStringTree;
    StudyVT: TVirtualStringTree;
    VacationVT: TVirtualStringTree;
    SIZCaptionPanel: TPanel;
    StudyCaptionPanel: TPanel;
    VacationCaptionPanel: TPanel;
    DividerBevel2: TDividerBevel;
    ExportButton: TSpeedButton;
    LeftBottomPanel: TPanel;
    RightBottomPanel: TPanel;
    RightHorSplitter: TSplitter;
    LeftTopPanel: TPanel;
    RightTopPanel: TPanel;
    RightPanel: TPanel;
    BirthdayCaptionPanel: TPanel;
    VertSplitter: TSplitter;
    MonthBCButton: TBCButton;
    LeftPanel: TPanel;
    ToolPanel: TPanel;
    LeftHorSplitter: TSplitter;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure DateRadioButtonClick(Sender: TObject);
    procedure DismissCheckBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NameRadioButtonClick(Sender: TObject);
    procedure YearCheckBoxChange(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    MonthDropDown: TMonthDropDown;
    CanUpdate: Boolean;

    BirthdayList: TVSTTable;
    VacationList: TVSTTable;
    SIZList: TVSTTable;
    StudyList: TVSTTable;

    procedure BirthdayListCreate;
    procedure BirthdayListLoad;

    procedure VacationListCreate;
    procedure VacationListLoad;

    procedure SIZListCreate;
    procedure SIZListLoad;

    procedure StudyListCreate;
    procedure StudyListLoad;
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
  StudyListCreate;
  CanUpdate:= True;
end;

procedure TInfoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MonthDropDown);
  FreeAndNil(BirthdayList);
  FreeAndNil(VacationList);
  FreeAndNil(SIZList);
  FreeAndNil(StudyList);
end;

procedure TInfoForm.FormShow(Sender: TObject);
begin
  SetToolPanels([ToolPanel]);
  Images.ToButtons([ExportButton]);
  SetCaptionPanels([
    BirthdayCaptionPanel, VacationCaptionPanel, SIZCaptionPanel, StudyCaptionPanel
  ]);

  MonthDropDown.AutoWidth;

  LeftPanel.Width:= (ClientWidth - VertSplitter.Width - 4) div 2;
  LeftTopPanel.Height:= (ClientHeight - ToolPanel.Height - 6) div 2;
  RightTopPanel.Height:= LeftTopPanel.Height;

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

procedure TInfoForm.DateRadioButtonClick(Sender: TObject);
begin
  DataUpdate;
end;

procedure TInfoForm.NameRadioButtonClick(Sender: TObject);
begin
  DataUpdate;
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
  DataBase.StaffBirthdaysLoad(M, DismissCheckBox.Checked, DateRadioButton.Checked,
                              Families, Names, Patronymics, BornDates);

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
  DataBase.StaffVacationsLoad(M, YearSpinEdit.Value, DateRadioButton.Checked,
                              Families, Names, Patronymics, TabNums, PostNames,
                              Counts, Dates);

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

end;

procedure TInfoForm.SIZListLoad;
begin

end;

procedure TInfoForm.StudyListCreate;
begin

end;

procedure TInfoForm.StudyListLoad;
begin

end;

procedure TInfoForm.DataUpdate;
begin
  if not CanUpdate then Exit;

  BirthdayListLoad;
  VacationListLoad;
  SIZListLoad;
  StudyListLoad;
end;

end.

