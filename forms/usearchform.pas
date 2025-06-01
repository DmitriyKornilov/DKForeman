unit USearchForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  VirtualTrees,
  //Project utils
  UDataBase, UImages,
  //DK packages utils
  DK_Filter, DK_Dialogs, DK_Vector, DK_StrUtils, DK_CtrlUtils, DK_VSTTableTools;

type

  { TSearchForm }

  TSearchForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    FilterPanel: TPanel;
    ListPanel: TPanel;
    ListVT: TVirtualStringTree;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    List: TVSTStringList;

    FilterString: String;

    ListIDs: TIntVector;
    ListNames: TStrVector;

    procedure ListFilter(const AFilterString: String);
    procedure ListLoad;
  public
    FilterCaption, TableName, IDField, NameField: String;
    FoundID: Integer
  end;

var
  SearchForm: TSearchForm;

  function Search(const AFilterCaption, ATableName, AIDField, ANameField: String;
                  out AFoundID: Integer): Boolean;

implementation

function Search(const AFilterCaption, ATableName, AIDField, ANameField: String;
                out AFoundID: Integer): Boolean;
var
  Form: TSearchForm;
begin
  Result:= False;
  AFoundID:= -1;

  Form:= TSearchForm.Create(nil);
  try
    Form.FilterCaption:= AFilterCaption;
    Form.TableName:= ATableName;
    Form.IDField:= AIDField;
    Form.NameField:= ANameField;

    if Form.ShowModal=mrOK then
    begin
      AFoundID:= Form.FoundID;
      Result:= True;
    end;

  finally
    FreeAndNil(Form);
  end;
end;

{$R *.lfm}

{ TSearchForm }

procedure TSearchForm.FormShow(Sender: TObject);
begin
  SetEventButtons([SaveButton, CancelButton]);
  Images.ToButtons([SaveButton, CancelButton]);
  CreateFilterControls(FilterCaption, FilterPanel, @ListFilter);
  List:= TVSTStringList.Create(ListVT, EmptyStr, nil);
  ListLoad;
end;

procedure TSearchForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(List);
end;

procedure TSearchForm.ListFilter(const AFilterString: String);
begin
  FilterString:= AFilterString;
  ListLoad;
end;

procedure TSearchForm.ListLoad;
begin
  DataBase.KeyPickListMatch(STrimLeft(FilterString), TableName, IDField, NameField,
                            ListIDs, ListNames, True {without zero ID}, NameField);
  List.Update(ListNames);
end;

procedure TSearchForm.SaveButtonClick(Sender: TObject);
begin
  if not List.IsSelected then
  begin
    Inform('Ничего не выбрано!');
    Exit;
  end;

  FoundID:= ListIDs[List.SelectedIndex];

  ModalResult:= mrOK;
end;

procedure TSearchForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

