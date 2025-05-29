unit USIZNormSubItemEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, BCButton, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_StrUtils, DK_Dialogs, DK_VSTTableTools, DK_Vector,
  DK_Matrix, DK_VSTDropDown,
  //Project utils
  UDataBase, UTypes, USIZUtils, USIZTypes, UImages;

type

  { TSIZNormSubItemEditForm }

  TSIZNormSubItemEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    LifeBCButton: TBCButton;
    SubItemPanel: TPanel;
    SubItemVT: TVirtualStringTree;
    ReasonBCButton: TBCButton;
    ClassBCButton: TBCButton;
    SubItemAddButton: TSpeedButton;
    SubItemDelButton: TSpeedButton;
    SubItemDownButton: TSpeedButton;
    SubItemEditButton: TSpeedButton;
    ToolPanel: TPanel;
    SubItemUpButton: TSpeedButton;
    YearsLabel: TLabel;
    NumLabel: TLabel;
    LifeLabel: TLabel;
    ClassLabel: TLabel;
    ReasonLabel: TLabel;
    NameLabel: TLabel;
    NamePanel: TPanel;
    NameVT: TVirtualStringTree;
    SaveButton: TSpeedButton;
    NumSpinEdit: TSpinEdit;
    LifeSpinEdit: TSpinEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LifeSpinEditChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    ReasonDropDown: TVSTDropDown;
    ClassDropDown: TVSTDropDown;
    LifeDropDown: TVSTDropDown;
    NameList: TVSTStringList;

    ReasonIDs, LifeIDs: TIntVector;
    ReasonNames, LifeNames: TStrVector;

    ClassNames:TStrVector;
    Names: TStrMatrix;
    NameIDs, SizeTypes: TIntMatrix;

    procedure ClassChange;
    procedure LifeChange;
  public
    ItemID: Integer;
    SubItem: TNormSubItem;
    EditingType: TEditingType;
  end;

var
  SIZNormSubItemEditForm: TSIZNormSubItemEditForm;

implementation

{$R *.lfm}

{ TSIZNormSubItemEditForm }

procedure TSIZNormSubItemEditForm.FormCreate(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  ControlHeight(ToolPanel, TOOL_PANEL_HEIGHT_DEFAULT-2);

  ReasonDropDown:= TVSTDropDown.Create(ReasonBCButton);
  ReasonDropDown.DropDownCount:= 20;

  ClassDropDown:= TVSTDropDown.Create(ClassBCButton);
  ClassDropDown.DropDownCount:= 20;
  ClassDropDown.OnChange:= @ClassChange;

  LifeDropDown:= TVSTDropDown.Create(LifeBCButton);
  LifeDropDown.DropDownCount:= 20;
  LifeDropDown.OnChange:= @LifeChange;

  NameList:= TVSTStringList.Create(NameVT, EmptyStr, nil);

end;

procedure TSIZNormSubItemEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZNormSubItemEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ReasonDropDown);
  FreeAndNil(ClassDropDown);
  FreeAndNil(LifeDropDown);
  FreeAndNil(NameList);
end;

procedure TSIZNormSubItemEditForm.FormShow(Sender: TObject);
var
  n: Integer;
begin
  SetEventButtons([SaveButton, CancelButton]);

  DataBase.KeyPickList('SIZREASON', 'ReasonID', 'ReasonName',
                       ReasonIDs, ReasonNames, False {with zero ID}, 'ReasonID');
  ReasonNames[0]:= 'НЕТ';
  ReasonDropDown.Items:= ReasonNames;
  ReasonDropDown.ItemIndex:= VIndexOf(ReasonIDs, SubItem.ReasonID);

  DataBase.KeyPickList('SIZSPECLIFE', 'SpecLifeID', 'SpecLifeName',
                       LifeIDs, LifeNames, False {with zero ID}, 'SpecLifeID');
  LifeNames[0]:= 'в месяцах';
  LifeDropDown.Items:= LifeNames;
  n:= -1;
  if not VIsNil(SubItem.Info.LifeIDs) then
    n:= VIndexOf(LifeIDs, SubItem.Info.LifeIDs[0]);
  if n<0 then n:= 0;
  LifeDropDown.ItemIndex:= n;

  DataBase.SIZAssortmentLoad(ClassNames, Names, NameIDs, SizeTypes);
  ClassDropDown.Items:= ClassNames;
  ClassDropDown.ItemIndex:= 0;

end;

procedure TSIZNormSubItemEditForm.LifeSpinEditChange(Sender: TObject);
begin
  YearsLabel.Caption:= '(' + SIZLifeInYearsStr(LifeSpinEdit.Value) + ')';
end;

procedure TSIZNormSubItemEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
begin


  case EditingType of
    etAdd:
      IsOK:= True;
    etEdit:
      IsOK:= True;
  end;

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

procedure TSIZNormSubItemEditForm.ClassChange;
begin
  NameList.Update(Names[ClassDropDown.ItemIndex]);
end;

procedure TSIZNormSubItemEditForm.LifeChange;
begin
  LifeSpinEdit.Visible:= LifeDropDown.ItemIndex=0;
  YearsLabel.Visible:= LifeSpinEdit.Visible;
end;

end.

