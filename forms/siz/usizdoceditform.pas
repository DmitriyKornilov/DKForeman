unit USIZDocEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCButton, DateTimePicker, Buttons,
  //Project utils
  UVars, UTypes, UConst, USIZUtils,
  //DK packages utils
  DK_CtrlUtils, DK_Vector, DK_VSTDropDown, DK_StrUtils, DK_Dialogs;

type

  { TSIZDocEditForm }

  TSIZDocEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    DocDateLabel: TLabel;
    DocFormLabel: TLabel;
    DocDatePicker: TDateTimePicker;
    DocNameEdit: TEdit;
    DocNameLabel: TLabel;
    DocNumEdit: TEdit;
    DocNumLabel: TLabel;
    DocFormBCButton: TBCButton;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    DocFormDropDown: TVSTDropDown;
  public
    DocID, DocType: Integer;
    EditingType: TEditingType;
  end;

var
  SIZDocEditForm: TSIZDocEditForm;

  function SIZDocEditFormOpen(const AEditingType: TEditingType;
                              const ADocType: Integer;
                              var ADocID: Integer;
                              const ADocName: String = '';
                              const ADocNum: String = '';
                              const ADocDate: TDate = 0;
                              const ADocForm: Integer = 0): Boolean;

implementation

function SIZDocEditFormOpen(const AEditingType: TEditingType;
                              const ADocType: Integer;
                              var ADocID: Integer;
                              const ADocName: String = '';
                              const ADocNum: String = '';
                              const ADocDate: TDate = 0;
                              const ADocForm: Integer = 0): Boolean;
var
  Form: TSIZDocEditForm;
  n: Integer;
begin
  Form:= TSIZDocEditForm.Create(nil);
  try
    Form.EditingType:= AEditingType;
    Form.DocType:= ADocType;

    if AEditingType<>etAdd then
    begin
      Form.DocID:= ADocID;
      Form.DocNameEdit.Text:= ADocName;
      Form.DocNumEdit.Text:= ADocNum;
      Form.DocDatePicker.Date:= ADocDate;
      n:= VIndexOf(SIZ_DOCFORM_KEYS, ADocForm);
      if n>=0 then
        Form.DocFormDropDown.ItemIndex:= n;
    end;

    Result:= Form.ShowModal=mrOK;

    if AEditingType=etAdd then
      ADocID:= Form.DocID;
  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSIZDocEditForm }

procedure TSIZDocEditForm.FormCreate(Sender: TObject);
begin
  DocID:= 0;
  DocDatePicker.Date:= Date;

  DocFormDropDown:= TVSTDropDown.Create(DocFormBCButton);
  DocFormDropDown.Items:= SIZ_DOCFORM_PICKS;
  DocFormDropDown.ItemIndex:= 0;

  //todo: add docforms
  DocFormLabel.Visible:= False;
  DocFormBCButton.Visible:= False;
end;

procedure TSIZDocEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(DocFormDropDown);
end;

procedure TSIZDocEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);
  FormKeepMinSize(Self);

  DocNameEdit.SetFocus;
end;

procedure TSIZDocEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  DocName, DocNum: String;
  DocForm: Integer;
  DocDate: TDate;
begin
  IsOK:= False;

  DocName:= STrim(DocNameEdit.Text);
  if SEmpty(DocName) then
  begin
    Inform('Не указано наименование документа!');
    Exit;
  end;

  DocNum:= STrim(DocNumEdit.Text);
  if SEmpty(DocNum) then
  begin
    Inform('Не указан номер документа!');
    Exit;
  end;

  DocDate:= DocDatePicker.Date;

  if DataBase.SIZDocExists(DocID, DocName, DocNum, DocDate) then
  begin
    Inform('Документ "' +
           SIZDocFullName(DocName, DocNum, DocDate) + '" уже существует!');
    Exit;
  end;

  DocForm:= SIZ_DOCFORM_KEYS[VIndexOf(SIZ_DOCFORM_PICKS, DocFormDropDown.Text)];

  case EditingType of
    etAdd:
      IsOK:= DataBase.SIZDocWrite(DocID, DocName, DocNum, DocDate, DocType, DocForm);
    etEdit:
      IsOK:= DataBase.SIZDocUpdate(DocID, DocName, DocNum, DocDate, DocType, DocForm);
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSIZDocEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

