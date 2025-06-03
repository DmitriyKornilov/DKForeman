unit USIZStaffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel, StdCtrls,
  //Project utils
  UDataBase, UConst, UTypes, UTimingUtils, UImages,
  //DK packages utils
  DK_VSTTables, DK_VSTParamList, DK_VSTEdit, DK_Vector, DK_Filter, DK_CtrlUtils;

type

  { TSIZStaffForm }

  TSIZStaffForm = class(TForm)
    AscendingButton: TSpeedButton;
    CloseButton: TSpeedButton;
    DescendingButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel4: TDividerBevel;
    ExportButton: TSpeedButton;
    FilterPanel: TPanel;
    FIORadioButton: TRadioButton;
    FormPanel: TPanel;
    SettingSplitter: TSplitter;
    CardSplitter: TSplitter;
    CardListCaptionPanel: TPanel;
    CardListVT: TVirtualStringTree;
    StaffSplitter: TSplitter;
    CardListPanel: TPanel;
    CardPanel: TPanel;
    StaffCaptionPanel: TPanel;
    StaffFilterToolPanel: TPanel;
    StaffOrderToolPanel: TPanel;
    StaffPanel: TPanel;
    OrderButtonPanel: TPanel;
    OrderLabel: TLabel;
    PostRadioButton: TRadioButton;
    MainPanel: TPanel;
    SettingCaptionPanel: TPanel;
    SettingClientPanel: TPanel;
    SettingPanel: TPanel;
    StaffListVT: TVirtualStringTree;
    TabNumRadioButton: TRadioButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CanDraw: Boolean;
    FilterString: String;
    ModeType: TModeType;

    ParamList: TVSTParamList;
    StaffList: TVSTTable;
    CardList: TVSTTable;

    TabNumIDs: TIntVector;
    StaffLongNames, StaffShortNames: TStrVector;
    RecrutDates, DismissDates: TDateVector;
    Families, Names, Patronymics, TabNums, PostNames: TStrVector;

    procedure ParamListCreate;

    procedure StaffListCreate;
    procedure StaffListFilter(const AFilterString: String);
    procedure StaffListLoad;
    procedure StaffListSelect;

    procedure CardListCreate;
    procedure CardListSelect;

    procedure SettingsLoad;
  public
    procedure SettingsSave;
    procedure ViewUpdate(const AModeType: TModeType);
  end;

var
  SIZStaffForm: TSIZStaffForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZStaffForm }

procedure TSIZStaffForm.FormCreate(Sender: TObject);
begin
  ModeType:= mtView;

  SetToolPanels([
    ToolPanel, StaffFilterToolPanel, StaffOrderToolPanel
  ]);
  SetCaptionPanels([
    StaffCaptionPanel, SettingCaptionPanel, CardListCaptionPanel
  ]);
  SetToolButtons([
    CloseButton, AscendingButton, DescendingButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton, AscendingButton, DescendingButton
  ]);

  CanDraw:= False;

  StaffListCreate;
  CardListCreate;
  ParamListCreate;
  SettingsLoad;
  CreateFilterControls('Фильтр по Ф.И.О.:', FilterPanel, @StaffListFilter, 300);

  CanDraw:= True;
end;

procedure TSIZStaffForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ParamList);
  FreeAndNil(StaffList);
  FreeAndNil(CardList);
end;

procedure TSIZStaffForm.CloseButtonClick(Sender: TObject);
begin
  MainForm.CategorySelect(0);
end;

procedure TSIZStaffForm.ParamListCreate;
var
  S: String;
  V: TStrVector;
begin
  ParamList:= TVSTParamList.Create(SettingClientPanel);

  S:= 'Включать в список:';
  V:= VCreateStr([
    'всех',
    'работающих на текущую дату',
    'уволенных на текущую дату'
  ]);
  ParamList.AddStringList('ListType', S, V, @StaffListLoad, 1);
end;

procedure TSIZStaffForm.StaffListCreate;
begin
  StaffList:= TVSTTable.Create(StaffListVT);
  StaffList.CanSelect:= True;
  StaffList.CanUnselect:= False;
  StaffList.OnSelect:= @StaffListSelect;
  StaffList.SetSingleFont(MainForm.GridFont);
  StaffList.HeaderFont.Style:= [fsBold];

  StaffList.AddColumn('№ п/п', 50);
  StaffList.AddColumn('Сотрудник', 300);
  StaffList.AutosizeColumnEnable('Сотрудник');
  StaffList.Draw;
end;

procedure TSIZStaffForm.StaffListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  StaffListLoad;
end;

procedure TSIZStaffForm.StaffListLoad;
begin

end;

procedure TSIZStaffForm.StaffListSelect;
begin

end;

procedure TSIZStaffForm.CardListCreate;
begin
  CardList:= TVSTTable.Create(CardListVT);
  CardList.CanSelect:= True;
  CardList.CanUnselect:= False;
  CardList.OnSelect:= @CardListSelect;
  CardList.SetSingleFont(MainForm.GridFont);
  CardList.HeaderFont.Style:= [fsBold];

  CardList.AddColumn('Период действия', 150);
  CardList.AddColumn('Должность (профессия)', 300);
  CardList.AddColumn('Пункт', 100);
  CardList.AddColumn('Типовые нормы', 300);
  CardList.AutosizeColumnEnable('Типовые нормы');
  CardList.Draw;
end;

procedure TSIZStaffForm.CardListSelect;
begin

end;

procedure TSIZStaffForm.SettingsLoad;
begin
  ParamList.Params:= DataBase.SettingsLoad(SETTING_NAMES_SIZSTAFFORM);
end;

procedure TSIZStaffForm.SettingsSave;
begin
  DataBase.SettingsUpdate(SETTING_NAMES_SIZSTAFFORM, ParamList.Params);
end;

procedure TSIZStaffForm.ViewUpdate(const AModeType: TModeType);
begin
  MainPanel.Visible:= False;
  try
    ModeType:= AModeType;

    if ModeType=mtSetting then
    begin
      SettingPanel.Visible:= True;
      SettingSplitter.Visible:= True;
    end
    else begin
      SettingSplitter.Visible:= False;
      SettingPanel.Visible:= False;
    end;

    StaffListLoad;

  finally
    MainPanel.Visible:= True;
  end;
end;

end.

