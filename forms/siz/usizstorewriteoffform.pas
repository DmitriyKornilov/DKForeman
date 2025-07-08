unit USIZStoreWriteoffForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, BCButton, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_Dialogs, DK_VSTTables, DK_Vector, DK_VSTDropDown,
  DK_StrUtils,
  //Project utils
  UVars, UTypes, USIZUtils,
  //Forms
  USIZDocEditForm;

type

  { TSIZStoreWriteoffForm }

  TSIZStoreWriteoffForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    DocLabel1: TLabel;
    SIZPanel: TPanel;
    VT: TVirtualStringTree;
    NoteEdit: TEdit;
    NoteLabel: TLabel;
    SaveButton: TSpeedButton;
    NewDocButton: TSpeedButton;
    DocBCButton: TBCButton;
    DocLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewDocButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    SIZList: TVSTTable;
    DocDropDown: TVSTDropDown;

    DocIDs: TIntVector;
    DocNames, DocNums: TStrVector;
    DocDates: TDateVector;

    procedure DocLoad(const ASelectedID: Integer = -1);

    procedure SIZListCreate;
    procedure SIZListLoad;
  public
    StoreIDs: TInt64Vector;
    NomNums, SizNames, SizUnits, SizSizes, EntryDocNames: TStrVector;
  end;

var
  SIZStoreWriteoffForm: TSIZStoreWriteoffForm;

implementation

{$R *.lfm}

{ TSIZStoreWriteoffForm }

procedure TSIZStoreWriteoffForm.FormCreate(Sender: TObject);
begin
  DocDropDown:= TVSTDropDown.Create(DocBCButton);
  DocDropDown.DropDownCount:= 20;

  SIZListCreate;
end;

procedure TSIZStoreWriteoffForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocDropDown);
  FreeAndNil(SIZList);
end;

procedure TSIZStoreWriteoffForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton, NewDocButton]);
  SetEventButtons([SaveButton, CancelButton]);
  ControlHeight(NewDocButton, TOOL_PANEL_HEIGHT_DEFAULT-2);
  SetToolButtons([NewDocButton]);
  FormKeepMinSize(Self, False);

  DocLoad;
  SIZListLoad;
end;

procedure TSIZStoreWriteoffForm.NewDocButtonClick(Sender: TObject);
var
  DocID: Integer;
begin
  DocID:= 0;
  if not SIZDocEditFormOpen(etAdd, 3{списание со склада}, DocID) then Exit;
  DocLoad(DocID);
end;

procedure TSIZStoreWriteoffForm.SIZListCreate;
begin
  SIZList:= TVSTTable.Create(VT);
  SIZList.CanSelect:= False;
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];

  SIZList.AddColumn('Номенклатурный номер', 200);
  SIZList.AddColumn('Наименование', 200);
  SIZList.AddColumn('Единица измерения', 150);
  SIZList.AddColumn('Количество', 100);
  SIZList.AddColumn('Размер/объём/вес', 130);
  SIZList.AddColumn('Документ', 200);
  SIZList.Draw;
end;

procedure TSIZStoreWriteoffForm.SIZListLoad;

var
  i, N1, N2: Integer;
  NomNum, SizName, SizSize, DocName: String;
  ShowNomNums, ShowSizNames, ShowSizUnits, ShowSizSizes, ShowDocNames: TStrVector;
  ShowSizCounts: TIntVector;

  procedure AddToVector(AInd1, AInd2: Integer);
  begin
    VAppend(ShowNomNums, NomNums[AInd1]);
    VAppend(ShowSizNames, SizNames[AInd1]);
    VAppend(ShowSizUnits, SizUnits[AInd1]);
    VAppend(ShowSizSizes, SizSizes[AInd1]);
    VAppend(ShowDocNames, EntryDocNames[AInd1]);
    VAppend(ShowSizCounts, AInd2-AInd1+1);
  end;

begin
  ShowNomNums:= nil;
  ShowSizNames:= nil;
  ShowSizUnits:= nil;
  ShowSizSizes:= nil;
  ShowDocNames:= nil;
  ShowSizCounts:= nil;

  NomNum:= NomNums[0];
  SizName:= SizNames[0];
  SizSize:= SizSizes[0];
  DocName:= EntryDocNames[0];
  N1:= 0;
  for i:= 1 to High(NomNums) do
  begin
    if not (SSame(NomNums[i], NomNum) and SSame(SizNames[i], SizName) and
            SSame(EntryDocNames[i], DocName) and SSame(SizSizes[i], SizSize) ) then
    begin
      N2:= i - 1;
      AddToVector(N1, N2);
      N1:= i;
      NomNum:= NomNums[i];
      SizName:= SizNames[i];
      SizSize:= SizSizes[i];
      DocName:= EntryDocNames[i];
    end;
  end;
  N2:= High(NomNums);
  AddToVector(N1, N2);

  SIZList.ValuesClear;
  SIZList.SetColumn('Номенклатурный номер', ShowNomNums, taLeftJustify);
  SIZList.SetColumn('Наименование', ShowSizNames, taLeftJustify);
  SIZList.SetColumn('Единица измерения', ShowSizUnits);
  SIZList.SetColumn('Количество', VIntToStr(ShowSizCounts));
  SIZList.SetColumn('Размер/объём/вес', ShowSizSizes);
  SIZList.SetColumn('Документ', ShowDocNames, taLeftJustify);
  SIZList.Draw;
end;

procedure TSIZStoreWriteoffForm.DocLoad(const ASelectedID: Integer);
var
  SelectedIndex: Integer;
  V: TIntVector;
begin
  if not DataBase.SIZDocListLoad(0{все время}, 3{списание со склада}, DocIDs,
                                 V, DocNames, DocNums, DocDates) then Exit;
  DocDropDown.Items:= SIZDocFullName(DocNames, DocNums, DocDates);

  if VIsNil(DocIDs) then Exit;
  DocDropDown.ItemIndex:= 0;

  if ASelectedID<0 then Exit;
  SelectedIndex:= VIndexOf(DocIDs, ASelectedID);
  if SelectedIndex>=0 then
    DocDropDown.ItemIndex:= SelectedIndex;
end;

procedure TSIZStoreWriteoffForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZStoreWriteoffForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
begin
  IsOK:= False;

  if DocDropDown.ItemIndex<0 then
  begin
    Inform('Не указан документ списания (передачи) СИЗ!');
    Exit;
  end;

  IsOK:= DataBase.SIZStoreWriteoffAdd(DocIDs[DocDropDown.ItemIndex],
                                      StoreIDs, STrim(NoteEdit.Text));

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;



end.

