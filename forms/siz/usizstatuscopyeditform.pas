unit USIZStatusCopyEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, Buttons, VirtualTrees,
  //Project utils
  UVars, UConst, UUtils,
  //DK packages utils
  DK_CtrlUtils, DK_Vector, DK_Matrix, DK_VSTDropDown, DK_Dialogs,
  DK_VSTCategoryTables;

type

  { TSIZStatusCopyEditForm }

  TSIZStatusCopyEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    CardBCButton: TBCButton;
    CardLabel: TLabel;
    CheckAllButton: TSpeedButton;
    CollapseAllButton: TSpeedButton;
    ExpandAllButton: TSpeedButton;
    SaveButton: TSpeedButton;
    SIZListLabel: TLabel;
    SIZNeedCountLabel: TLabel;
    SIZNeedCountNameLabel: TLabel;
    SIZNeedLabel: TLabel;
    SIZNeedNameLabel: TLabel;
    SIZNeedSizeLabel: TLabel;
    SIZNeedSizeNameLabel: TLabel;
    SIZPanel: TPanel;
    UncheckAllButton: TSpeedButton;
    VT: TVirtualStringTree;
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckAllButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure UncheckAllButtonClick(Sender: TObject);
  private
    SIZList: TVSTCategoryCheckTable;
    CardDropDown: TVSTDropDown;

    CardIDs: TIntVector;
    CardNums, PostNames, NormNames: TStrVector;
    CardBDs, CardEDs: TDateVector;

    ReceivingDocIDs, ReceivingInfoIDs: TIntVector;
    NeedSizNames: TStrVector;
    ReceivingDates, WriteoffDates: TDateMatrix;
    StoreIDs: TInt64Matrix;
    ReceivingDocNames, SizNames: TStrMatrix;
    SizCounts: TIntMatrix;

    procedure CardListLoad;
    procedure CardChange;

    procedure SIZListCreate;
    procedure SIZListLoad;
  public
    TabNumID, CardID, SIZType, InfoID, ItemPostID: Integer;
    CardBD: TDate;
  end;

var
  SIZStatusCopyEditForm: TSIZStatusCopyEditForm;

implementation

{$R *.lfm}

{ TSIZStatusCopyEditForm }

procedure TSIZStatusCopyEditForm.FormCreate(Sender: TObject);
begin
  CardDropDown:= TVSTDropDown.Create(CardBCButton);
  CardDropDown.DropDownCount:= 20;
  CardDropDown.OnChange:= @CardChange;

  SIZListCreate;
end;

procedure TSIZStatusCopyEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CardDropDown);
  FreeAndNil(SIZList);
end;

procedure TSIZStatusCopyEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([
    SaveButton, CancelButton,
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton
  ]);
  SetEventButtons([SaveButton, CancelButton]);
  SetSimpleButtons([
    ExpandAllButton, CollapseAllButton, CheckAllButton, UncheckAllButton
  ]);

  FormKeepMinSize(Self, False);

  CardListLoad;
end;

procedure TSIZStatusCopyEditForm.CardListLoad;
var
  i: Integer;
  CardFullNames, ViewCardNums: TStrVector;
begin
  CardDropDown.Clear;
  if not DataBase.SIZPrevCardListLoad(TabNumID, CardBD, CardIDs, CardNums,
                                 PostNames, NormNames, CardBDs, CardEDs) then Exit;

  ViewCardNums:= VCut(CardNums);
  VChangeIf(ViewCardNums, EmptyStr, 'б/н');
  VDim(CardFullNames{%H-}, Length(CardIDs));
  for i:= 0 to High(CardIDs) do
    CardFullNames[i]:= '№ ' + ViewCardNums[i] +
                       ' (' + PeriodToStr(CardBDs[i], CardEDs[i]) + ') - '  +
                       PostNames[i] + ' (' + NormNames[i] + ')';

  CardDropDown.Items:= CardFullNames;
  CardDropDown.ItemIndex:= 0;
end;

procedure TSIZStatusCopyEditForm.CardChange;
begin
  SIZListLoad;
end;

procedure TSIZStatusCopyEditForm.SIZListCreate;
begin
  SIZList:= TVSTCategoryCheckTable.Create(VT);
  //SIZList.OnSelect:= @SIZListSelect;
  SIZList.Span:= True;
  SIZList.CheckKind:= chkCategory;
  SIZList.TreeLinesVisible:= False;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.CategoryFont.Style:= [fsBold];
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Количество', 150);
  SIZList.AddColumn('Дата выдачи', 130);
  SIZList.AddColumn('Дата списания', 130);
  SIZList.AddColumn('Документ выдачи', 300);
  SIZList.Draw;
end;

procedure TSIZStatusCopyEditForm.SIZListLoad;
begin
  if CardDropDown.ItemIndex<0 then Exit;

  DataBase.SIZPrevCardSIZLoad(CardIDs[CardDropDown.ItemIndex], SIZType, CardBD,
                              ReceivingDocIDs, ReceivingInfoIDs, NeedSizNames,
                              ReceivingDates, WriteoffDates,
                              StoreIDs, ReceivingDocNames, SizNames, SizCounts);

  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetCategories(NeedSizNames);
    SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
    SIZList.SetColumn('Количество', MIntToStr(SizCounts));
    SIZList.SetColumn('Дата выдачи', MFormatDateTime('dd.mm.yyyy', ReceivingDates));
    SIZList.SetColumn('Дата списания', MFormatDateTime('dd.mm.yyyy', WriteoffDates));
    SIZList.SetColumn('Документ выдачи', ReceivingDocNames, taLeftJustify);
    SIZList.Draw;
    SIZList.ExpandAll(True);
    SIZList.ShowFirst;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZStatusCopyEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZStatusCopyEditForm.SaveButtonClick(Sender: TObject);
var
  SelectedStoreIDs: TInt64Matrix;
  SelectedReceivingDates: TDateVector;
  SelectedReceivingInfoIDs, SelectedReceivingDocIDs: TIntVector;
begin
  if not SIZList.IsSelected then
  begin
    Inform('Не указано ни одного наименования СИЗ!');
    Exit;
  end;

  SelectedReceivingDates:= MFirsts(ReceivingDates, SIZList.CategorySelected);
  SelectedReceivingInfoIDs:= VCut(ReceivingInfoIDs, SIZList.CategorySelected);
  SelectedReceivingDocIDs:= VCut(ReceivingDocIDs, SIZList.CategorySelected);
  SelectedStoreIDs:= MCut(StoreIDs, SIZList.CategorySelected);

  if not DataBase.SIZReceivingNextWrite(CardID, TabNumID, ItemPostID, InfoID,
                               SelectedReceivingInfoIDs, SelectedReceivingDocIDs,
                               SelectedReceivingDates, SelectedStoreIDs) then Exit;

  ModalResult:= mrOK;
end;

procedure TSIZStatusCopyEditForm.CheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(True);
end;

procedure TSIZStatusCopyEditForm.UncheckAllButtonClick(Sender: TObject);
begin
  SIZList.CheckAll(False);
end;

procedure TSIZStatusCopyEditForm.CollapseAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(False);
end;

procedure TSIZStatusCopyEditForm.ExpandAllButtonClick(Sender: TObject);
begin
  SIZList.ExpandAll(True);
end;

end.

