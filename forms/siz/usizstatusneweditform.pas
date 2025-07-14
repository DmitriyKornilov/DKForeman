unit USIZStatusNewEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, BCButton, VirtualTrees,
  //Project utils
  UVars, UConst, UTypes, USIZUtils,
  //DK packages utils
  DK_CtrlUtils, DK_VSTTables, DK_Vector, DK_Matrix, DK_VSTDropDown, DK_DateUtils,
  DK_Dialogs,
  //Forms
  USIZDocEditForm;

type

  { TSIZStatusNewEditForm }

  TSIZStatusNewEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    ResumeLabel: TLabel;
    CheckAllButton: TSpeedButton;
    CollapseAllButton: TSpeedButton;
    ExpandAllButton: TSpeedButton;
    SIZNeedSizeLabel: TLabel;
    SIZNeedNameLabel: TLabel;
    SIZNeedLabel: TLabel;
    SIZNeedCountLabel: TLabel;
    SIZNeedSizeNameLabel: TLabel;
    SIZNeedCountNameLabel: TLabel;
    SIZPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    VT: TVirtualStringTree;
    DocBCButton: TBCButton;
    DocLabel: TLabel;
    SIZListLabel: TLabel;
    NewDocButton: TSpeedButton;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewDocButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
  private
    SIZList: TVSTCategoryCheckTable;
    DocDropDown: TVSTDropDown;

    DocIDs: TIntVector;
    DocNames, DocNums: TStrVector;
    DocDates: TDateVector;

    CategoryNames: TStrMatrix;
    StoreIDs: TInt64Matrix;
    SizCounts: TIntMatrix;
    NomNums, SizNames, SizUnits, SizSizes, EntryDocNames: TStrMatrix;

    ReceivingDate: TDate;

    procedure DocLoad(const ASelectedID: Integer = -1);
    procedure DocChange;

    procedure SIZListCreate;
    procedure SIZListLoad;
    procedure SIZListSelect;

    procedure NumAndLifeCalc;
  public
    TabNumID, CardID, InfoID, ItemPostID: Integer;
    SIZType, Num, Life: Integer;
  end;

var
  SIZStatusNewEditForm: TSIZStatusNewEditForm;

implementation

{$R *.lfm}

{ TSIZStatusNewEditForm }

procedure TSIZStatusNewEditForm.FormCreate(Sender: TObject);
begin
  SIZType:= -1;
  Num:= 0;
  Life:= 0;

  DocDropDown:= TVSTDropDown.Create(DocBCButton);
  DocDropDown.DropDownCount:= 20;
  DocDropDown.OnChange:= @DocChange;

  SIZListCreate;
end;

procedure TSIZStatusNewEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocDropDown);
  FreeAndNil(SIZList);
end;

procedure TSIZStatusNewEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([
    SaveButton, CancelButton, NewDocButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton
  ]);
  SetEventButtons([SaveButton, CancelButton]);
  SetSimpleButtons([
    NewDocButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton
  ]);

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
  NumAndLifeCalc;
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
  DataBase.SIZStoreLoad(SIZType, CategoryNames, StoreIDs, SizCounts, NomNums,
                        SizNames, SizUnits, SizSizes, EntryDocNames,
                        False{no counts in category});

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(CategoryNames);
    SIZList.SetColumn('Номенклатурный номер', NomNums, taLeftJustify);
    SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
    SIZList.SetColumn('Единица измерения', SizUnits);
    SIZList.SetColumn('Размер/объём/вес', SizSizes);
    SIZList.SetColumn('Документ поступления', EntryDocNames, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;

  ExpandAllButton.Enabled:= not MIsNil(StoreIDs);
  CollapseAllButton.Enabled:= ExpandAllButton.Enabled;
  CheckAllButton.Enabled:= ExpandAllButton.Enabled;
  UncheckAllButton.Enabled:= ExpandAllButton.Enabled;
end;

procedure TSIZStatusNewEditForm.SIZListSelect;
begin

  NumAndLifeCalc;
end;

procedure TSIZStatusNewEditForm.NumAndLifeCalc;
var
  Count: Integer;
  Months: Extended;
  WriteoffDate: TDate;
begin
  Count:= VSum(MToVector(SizCounts, SIZList.Selected));
  Months:= SIZLifeInMonths(Count, Num, Life);
  if DocDropDown.ItemIndex>=0 then
    ReceivingDate:= DocDates[DocDropDown.ItemIndex]
  else
    ReceivingDate:= Date;
  WriteoffDate:= IncMonthExt(ReceivingDate, Months);

  ResumeLabel.Caption:= 'ИТОГО:  ' +
               IntToStr(Count) + ' на ' + SIZLifeInMonthAndYears(Months) +
               ', дата выдачи ' + FormatDateTime('dd.mm.yyyy', ReceivingDate) +
               ', дата списания ' + FormatDateTime('dd.mm.yyyy', WriteoffDate);
end;

procedure TSIZStatusNewEditForm.SaveButtonClick(Sender: TObject);
var
  SelectedStoreIDs: TInt64Vector;
begin
  if DocDropDown.ItemIndex<0 then
  begin
    Inform('Не указан документ выдачи СИЗ!');
    Exit;
  end;

  if not SIZList.IsSelected then
  begin
    Inform('Не указано ни одного наименования СИЗ!');
    Exit;
  end;

  SelectedStoreIDs:= MToVector(StoreIDs, SIZList.Selected);
  if not DataBase.SIZReceivingWrite(CardID, TabNumID, ItemPostID, InfoID, InfoID,
                                    DocIDs[DocDropDown.ItemIndex], SelectedStoreIDs,
                                    ReceivingDate) then Exit;

  ModalResult:= mrOK;
end;

procedure TSIZStatusNewEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZStatusNewEditForm.UncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZStatusNewEditForm.CheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZStatusNewEditForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZStatusNewEditForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

end.

