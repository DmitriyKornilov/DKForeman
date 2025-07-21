unit USIZCardReturnEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, BCButton, VirtualTrees,
  //Project utils
  UVars, UConst, UTypes, USIZUtils,
  //DK packages utils
  DK_CtrlUtils, DK_VSTTables, DK_Vector, DK_VSTDropDown, DK_StrUtils, DK_Dialogs,
  //Forms
  USIZDocEditForm;

type

  { TSIZCardReturnEditForm }

  TSIZCardReturnEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    DocBCButton: TBCButton;
    DocLabel: TLabel;
    NewDocButton: TSpeedButton;
    NoteEdit: TEdit;
    NoteLabel: TLabel;
    SaveButton: TSpeedButton;
    SIZListLabel: TLabel;
    SIZPanel: TPanel;
    VT: TVirtualStringTree;
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

    procedure DocListLoad(const ASelectedID: Integer = -1);

    procedure SIZListCreate;
    procedure SIZListLoad;
  public
    LogID: Int64;
    ReceivingDocName: String;
    ReceivingDate: TDate;
    SizNames: TStrVector;
    SizCounts: TIntVector;
  end;

var
  SIZCardReturnEditForm: TSIZCardReturnEditForm;

implementation

{$R *.lfm}

{ TSIZCardReturnEditForm }

procedure TSIZCardReturnEditForm.FormCreate(Sender: TObject);
begin
  DocDropDown:= TVSTDropDown.Create(DocBCButton);
  DocDropDown.DropDownCount:= 20;

  SIZListCreate;
end;

procedure TSIZCardReturnEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocDropDown);
  FreeAndNil(SIZList);
end;

procedure TSIZCardReturnEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton, NewDocButton]);
  SetEventButtons([SaveButton, CancelButton]);
  SetSimpleButtons([NewDocButton]);

  FormKeepMinSize(Self, False);

  SIZListLoad;
end;

procedure TSIZCardReturnEditForm.NewDocButtonClick(Sender: TObject);
var
  DocID: Integer;
begin
  DocID:= 0;
  if not SIZDocEditFormOpen(etAdd, 4{возврат СИЗ на склад}, DocID) then Exit;
  DocListLoad(DocID);
end;

procedure TSIZCardReturnEditForm.DocListLoad(const ASelectedID: Integer);
var
  SelectedIndex: Integer;
  V: TIntVector;
begin
  if not DataBase.SIZDocListLoad(0{все время}, 4{возврат СИЗ на склад}, DocIDs,
                                 V, DocNames, DocNums, DocDates) then Exit;
  DocDropDown.Items:= SIZDocFullName(DocNames, DocNums, DocDates);

  if VIsNil(DocIDs) then Exit;
  DocDropDown.ItemIndex:= 0;

  if ASelectedID<0 then Exit;
  SelectedIndex:= VIndexOf(DocIDs, ASelectedID);
  if SelectedIndex>=0 then
    DocDropDown.ItemIndex:= SelectedIndex;
end;

procedure TSIZCardReturnEditForm.SIZListCreate;
begin
  SIZList:= TVSTTable.Create(VT);
  SIZList.SetSingleFont(GridFont);
  SIZList.HeaderFont.Style:= [fsBold];
  SIZList.AddColumn('Наименование', 300);
  SIZList.AddColumn('Дата выдачи', 150);
  SIZList.AddColumn('Количество', 130);
  SIZList.AddColumn('Документ выдачи', 300);
  SIZList.Draw;
end;

procedure TSIZCardReturnEditForm.SIZListLoad;
var
  V: TStrVector;
begin
  SIZList.Visible:= False;
  try
    SIZList.ValuesClear;
    SIZList.SetColumn('Наименование', SizNames, taLeftJustify);
    VDim(V, Length(SizNames), FormatDateTime('dd.mm.yyyy', ReceivingDate));
    SIZList.SetColumn('Дата выдачи', V);
    SIZList.SetColumn('Количество', VIntToStr(SizCounts));
    VDim(V, Length(SizNames), ReceivingDocName);
    SIZList.SetColumn('Документ выдачи', V, taLeftJustify);
    SIZList.Draw;
  finally
    SIZList.Visible:= True;
  end;
end;

procedure TSIZCardReturnEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZCardReturnEditForm.SaveButtonClick(Sender: TObject);
begin
  if DocDropDown.ItemIndex<0 then
  begin
    Inform('Не указан документ возврата СИЗ!');
    Exit;
  end;

  if VIsNil(SizNames) then
  begin
    Inform('Не указано ни одного наименования СИЗ!');
    Exit;
  end;

  if not DataBase.SIZReturningWrite(DocIDs[DocDropDown.ItemIndex],
                                    LogID, STrim(NoteEdit.Text)) then Exit;

  ModalResult:= mrOK;
end;

end.

