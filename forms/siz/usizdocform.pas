unit USIZDocForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees, DividerBevel, Spin,
  //Project utils
  UVars, UConst, UTypes, UUtils, USIZUtils,
  //DK packages utils
  DK_VSTTables, DK_Vector, DK_CtrlUtils, DK_DateUtils, DK_Dialogs,
  //Forms
  USIZDocEditForm, USIZDocStoreEntryForm, USIZDocStoreWriteoffForm,
  USIZDocReturningForm, USIZDocReceivingForm;

type

  { TSIZDocForm }

  TSIZDocForm = class(TForm)
    DividerBevel4: TDividerBevel;
    DocAddButton: TSpeedButton;
    DocDelButton: TSpeedButton;
    DocEditButton: TSpeedButton;
    DocEraseButton: TSpeedButton;
    DocCaptionPanel: TPanel;
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    EditingButton: TSpeedButton;
    ExportButton: TSpeedButton;
    SIZFormPanel: TPanel;
    SIZCaptionPanel: TPanel;
    DocPanel: TPanel;
    Splitter: TSplitter;
    DocToolPanel: TPanel;
    SIZPanel: TPanel;
    ToolPanel: TPanel;
    DocVT: TVirtualStringTree;
    YearPanel: TPanel;
    YearSpinEdit: TSpinEdit;
    procedure CloseButtonClick(Sender: TObject);
    procedure DocAddButtonClick(Sender: TObject);
    procedure DocDelButtonClick(Sender: TObject);
    procedure DocEditButtonClick(Sender: TObject);
    procedure DocEraseButtonClick(Sender: TObject);
    procedure DocVTDblClick(Sender: TObject);
    procedure EditingButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YearSpinEditChange(Sender: TObject);
  private
    SIZForm: TForm;
    DocList: TVSTTable;

    DocIDs, DocForms: TIntVector;
    DocNames, DocNums, DocFullNames: TStrVector;
    DocDates: TDateVector;

    procedure DocListCreate;
    procedure DocListSelect;
    procedure DocListLoad(const ASelectedID: Integer = -1);

    procedure DocEdit(const AEditingType: TEditingType);
    procedure DocChange;

    procedure SIZFormShow;
    procedure SIZFormViewUpdate;

    procedure ViewUpdate;
  public
    DocType: Integer;
  end;

var
  SIZDocForm: TSIZDocForm;

  procedure SIZDocFormOpen(const ADocType: Integer);

implementation

procedure SIZDocFormOpen(const ADocType: Integer);
var
  Form: TSIZDocForm;
begin
  Form:= TSIZDocForm.Create(nil);
  try
    Form.DocType:= ADocType;
    Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZDocForm }

procedure TSIZDocForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION;
  DocType:= 0;
  DocListCreate;
end;

procedure TSIZDocForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocList);
end;

procedure TSIZDocForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel, DocToolPanel
  ]);
  SetCaptionPanels([
    DocCaptionPanel, SIZCaptionPanel
  ]);
  SetToolButtons([
    CloseButton,
    EditingButton,
    DocAddButton, DocDelButton, DocEditButton, DocEraseButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton,
    EditingButton,
    DocAddButton, DocDelButton, DocEditButton, DocEraseButton
  ]);

  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[DocType+6];

  SIZFormShow;

  YearSpinEdit.Value:= YearOfDate(Date);
end;

procedure TSIZDocForm.YearSpinEditChange(Sender: TObject);
begin
  DocListLoad;
end;

procedure TSIZDocForm.DocListCreate;
begin
  DocList:= TVSTTable.Create(DocVT);
  DocList.CanSelect:= True;
  DocList.CanUnselect:= False;
  DocList.OnSelect:= @DocListSelect;
  DocList.SetSingleFont(GridFont);
  DocList.HeaderFont.Style:= [fsBold];

  DocList.AddColumn('№ п/п', 50);
  DocList.AddColumn('Документ', 300);
  DocList.AutosizeColumnEnable('Документ');
  DocList.Draw;
end;

procedure TSIZDocForm.DocListSelect;
begin
  DocDelButton.Enabled:= DocList.IsSelected;
  DocEditButton.Enabled:= DocDelButton.Enabled;

  SIZCaptionPanel.Caption:= '  Документ: ';
  if DocList.IsSelected then
    SIZCaptionPanel.Caption:= SIZCaptionPanel.Caption +
                              DocFullNames[DocList.SelectedIndex];

  DocChange;
end;

procedure TSIZDocForm.DocListLoad(const ASelectedID: Integer);
var
  SelectedID: Integer;
begin
  SelectedID:= GetSelectedID(DocList, DocIDs, ASelectedID);

  DataBase.SIZDocListLoad(YearSpinEdit.Value, DocType, DocIDs, DocForms,
                          DocNames, DocNums, DocDates);

  DocList.Visible:= False;
  try
    DocList.ValuesClear;
    DocList.SetColumn('№ п/п', VIntToStr(VOrder(Length(DocIDs))));
    DocFullNames:= SIZDocFullName(DocNames, DocNums, DocDates);
    DocList.SetColumn('Документ', DocFullNames, taLeftJustify);
    DocList.Draw;
    DocList.ReSelect(DocIDs, SelectedID, True);  //возвращаем выделение строки
  finally
    DocList.Visible:= True;
  end;

  ExportButton.Enabled:= not VIsNil(DocIDs);
  DocEraseButton.Enabled:= ExportButton.Enabled;
end;

procedure TSIZDocForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSIZDocForm.DocEdit(const AEditingType: TEditingType);
var
  DocID, DocForm: Integer;
  DocName, DocNum: String;
  DocDate: TDate;
begin
  if (AEditingType=etEdit) and (not DocList.IsSelected) then Exit;

  if DocList.IsSelected then
  begin
    DocID:= DocIDs[DocList.SelectedIndex];
    DocForm:= DocForms[DocList.SelectedIndex];
    DocName:= DocNames[DocList.SelectedIndex];
    DocNum:= DocNums[DocList.SelectedIndex];
    DocDate:= DocDates[DocList.SelectedIndex];
  end
  else begin
    DocID:= 0;
    DocForm:= 0;
    DocName:= EmptyStr;
    DocNum:= EmptyStr;
    DocDate:= 0;
  end;

  if not SIZDocEditFormOpen(AEditingType, DocType, DocID, DocName, DocNum,
                            DocDate, DocForm) then Exit;
  DocListLoad(DocID);
end;

procedure TSIZDocForm.DocAddButtonClick(Sender: TObject);
begin
  DocEdit(etAdd);
end;

procedure TSIZDocForm.DocEditButtonClick(Sender: TObject);
begin
  DocEdit(etEdit);
end;

procedure TSIZDocForm.DocEraseButtonClick(Sender: TObject);
begin
  if not Confirm('Удалить все пустые документы?') then Exit;
  if DataBase.SIZDocStoreEmptyDelete(DocType, YearSpinEdit.Value) then
    DocListLoad;
end;

procedure TSIZDocForm.DocDelButtonClick(Sender: TObject);
var
  NeedReload: Boolean;
begin
  if not Confirm('Удалить документ?') then Exit;
  case DocType of
    1: NeedReload:= DataBase.SIZDocStoreEntryDelete(DocIDs[DocList.SelectedIndex]);
    2: NeedReload:= DataBase.SIZDocReceivingDelete(DocIDs[DocList.SelectedIndex]);
    3: NeedReload:= DataBase.SIZDocStoreWriteoffDelete(DocIDs[DocList.SelectedIndex]);
    4: NeedReload:= DataBase.SIZDocReturningDelete(DocIDs[DocList.SelectedIndex]);
  end;

  if NeedReload then DocListLoad;
end;

procedure TSIZDocForm.SIZFormShow;
begin
  case DocType of
    1: SIZForm:= FormOnPanelCreate(TSIZDocStoreEntryForm, SIZFormPanel);
    2: SIZForm:= FormOnPanelCreate(TSIZDocReceivingForm, SIZFormPanel);
    3: SIZForm:= FormOnPanelCreate(TSIZDocStoreWriteoffForm, SIZFormPanel);
    4: SIZForm:= FormOnPanelCreate(TSIZDocReturningForm, SIZFormPanel);
  end;

  if Assigned(SIZForm) then
  begin
    SIZForm.Show;
    SIZFormViewUpdate;
  end;
end;

procedure TSIZDocForm.SIZFormViewUpdate;
begin
  case DocType of
    1: (SIZForm as TSIZDocStoreEntryForm).ViewUpdate(EditingButton.Down);
    2: (SIZForm as TSIZDocReceivingForm).ViewUpdate(EditingButton.Down);
    3: (SIZForm as TSIZDocStoreWriteoffForm).ViewUpdate(EditingButton.Down);
    4: (SIZForm as TSIZDocReturningForm).ViewUpdate(EditingButton.Down);
  end;
end;

procedure TSIZDocForm.DocChange;
var
  DocID: Integer;
begin
  DocID:= 0;
  if DocList.IsSelected then
    DocID:= DocIDs[DocList.SelectedIndex];
  case DocType of
    1: (SIZForm as TSIZDocStoreEntryForm).DocChange(DocID);
    2: (SIZForm as TSIZDocReceivingForm).DocChange(DocID);
    3: (SIZForm as TSIZDocStoreWriteoffForm).DocChange(DocID);
    4: (SIZForm as TSIZDocReturningForm).DocChange(DocID);
  end;
end;

procedure TSIZDocForm.ViewUpdate;
begin
  DocToolPanel.Visible:= EditingButton.Down;
  SIZFormViewUpdate;
end;

procedure TSIZDocForm.DocVTDblClick(Sender: TObject);
begin
  DocEdit(etEdit);
end;

procedure TSIZDocForm.EditingButtonClick(Sender: TObject);
begin
  ViewUpdate;
end;

procedure TSIZDocForm.ExportButtonClick(Sender: TObject);
begin
  //!!!!
end;

end.

