unit USIZStatusNewEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, DateTimePicker, BCButton, VirtualTrees,
  //Project utils
  UVars, UConst, UTypes, USIZUtils,
  //DK packages utils
  DK_CtrlUtils, DK_VSTTables, DK_Vector, DK_Matrix, DK_StrUtils, DK_VSTDropDown,
  //Forms
  USIZDocEditForm;

type

  { TSIZStatusNewEditForm }

  TSIZStatusNewEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    ResumeLabel: TLabel;
    SizLifeNameLabel: TLabel;
    ReceivingDatePicker: TDateTimePicker;
    SizCountLabel: TLabel;
    SizLifeLabel: TLabel;
    SIZPanel: TPanel;
    VT: TVirtualStringTree;
    WriteoffDatePicker: TDateTimePicker;
    DocBCButton: TBCButton;
    DocLabel: TLabel;
    ReceivingDateLabel: TLabel;
    WriteoffDateLabel: TLabel;
    SIZListLabel: TLabel;
    NewDocButton: TSpeedButton;
    SaveButton: TSpeedButton;
    SearchButton: TSpeedButton;
    TypeBCButton: TBCButton;
    TypeLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewDocButtonClick(Sender: TObject);
  private
    SIZList: TVSTCategoryCheckTable;
    DocDropDown: TVSTDropDown;
    TypeDropDown: TVSTDropDown;

    DocIDs: TIntVector;
    DocNames, DocNums: TStrVector;
    DocDates: TDateVector;

    procedure DocLoad(const ASelectedID: Integer = -1);
    procedure DocChange;

    procedure TypeChange;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListSelect;
  public

  end;

var
  SIZStatusNewEditForm: TSIZStatusNewEditForm;

implementation

{$R *.lfm}

{ TSIZStatusNewEditForm }

procedure TSIZStatusNewEditForm.FormCreate(Sender: TObject);
begin
  DocDropDown:= TVSTDropDown.Create(DocBCButton);
  DocDropDown.DropDownCount:= 20;
  DocDropDown.OnChange:= @DocChange;

  TypeDropDown:= TVSTDropDown.Create(TypeBCButton);
  TypeDropDown.DropDownCount:= 20;
  TypeDropDown.OnChange:= @TypeChange;

  ReceivingDatePicker.Date:= Date;

  SIZListCreate;
end;

procedure TSIZStatusNewEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocDropDown);
  FreeAndNil(TypeDropDown);
  FreeAndNil(SIZList);
end;

procedure TSIZStatusNewEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton, NewDocButton, SearchButton]);
  SetEventButtons([SaveButton, CancelButton]);
  ControlHeight(NewDocButton, TOOL_PANEL_HEIGHT_DEFAULT-2);
  ControlHeight(SearchButton, TOOL_PANEL_HEIGHT_DEFAULT-2);
  SetToolButtons([NewDocButton, SearchButton]);
  FormKeepMinSize(Self, False);

  DocLoad;
  SIZListLoad;
end;

procedure TSIZStatusNewEditForm.NewDocButtonClick(Sender: TObject);
var
  DocID: Integer;
begin
  DocID:= 0;
  if not SIZDocEditFormOpen(etAdd, 2{выдача СИЗ}, DocID) then Exit;
  DocLoad(DocID);
end;

procedure TSIZStatusNewEditForm.DocLoad(const ASelectedID: Integer = -1);
var
  SelectedIndex: Integer;
  V: TIntVector;
begin
  if not DataBase.SIZDocListLoad(0{все время}, 2{выдача СИЗ}, DocIDs,
                                 V, DocNames, DocNums, DocDates) then Exit;
  DocDropDown.Items:= SIZDocFullName(DocNames, DocNums, DocDates);

  if VIsNil(DocIDs) then Exit;
  DocDropDown.ItemIndex:= 0;

  if ASelectedID<0 then Exit;
  SelectedIndex:= VIndexOf(DocIDs, ASelectedID);
  if SelectedIndex>=0 then
    DocDropDown.ItemIndex:= SelectedIndex;
end;

procedure TSIZStatusNewEditForm.DocChange;
begin
  if DocDropDown.ItemIndex>=0 then
    ReceivingDatePicker.Date:= DocDates[DocDropDown.ItemIndex];


end;

procedure TSIZStatusNewEditForm.TypeChange;
begin

end;

procedure TSIZStatusNewEditForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  SIZList.OnSelect:= @SIZListSelect;
  SIZList.TreeLinesVisible:= False;
  SIZList.CheckVisible:= True;
  SIZList.CheckEnable:= True;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Документ поступления', 300);
  SIZList.Draw;
end;

procedure TSIZStatusNewEditForm.SIZListLoad;
begin

end;

procedure TSIZStatusNewEditForm.SIZListSelect;
begin

end;

end.

