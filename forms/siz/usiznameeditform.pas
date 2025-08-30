unit USIZNameEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VirtualTrees,
  //DK packages utils
  DK_VSTTables, DK_VSTTypes, DK_DBTable, DK_Vector,
  //Project utils
  UVars, UConst;

type

  { TSIZNameEditForm }

  TSIZNameEditForm = class(TForm)
    NamePanel: TPanel;
    Splitter: TSplitter;
    TypePanel: TPanel;
    TypeVT: TVirtualStringTree;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    SIZTypeTable: TVSTTable;
    SIZNameTable: TDBTable;

    UnitIDs: TIntVector;
    UnitNames: TStrVector;

    procedure TypeSelect;
    procedure SizeTypeChoose(out AColumnName: String;
                             out AKeys: TIntVector;
                             out APicks: TStrVector);
  public

  end;

var
  SIZNameEditForm: TSIZNameEditForm;

implementation

{$R *.lfm}

{ TSIZNameEditForm }

procedure TSIZNameEditForm.FormCreate(Sender: TObject);
begin
  DataBase.KeyPickList('SIZUNIT', 'UnitID', 'UnitName', UnitIDs, UnitNames);

  SIZNameTable:= TDBTable.Create(GridFont, NamePanel, DataBase, True, 'Фильтр:');
  SIZNameTable.Edit.HeaderFont.Style:= [fsBold];

  SIZTypeTable:= TVSTTable.Create(TypeVT);
  SIZTypeTable.SetSingleFont(GridFont);
  SIZTypeTable.CanSelect:= True;
  SIZTypeTable.CanUnselect:= True;
  SIZTypeTable.OnSelect:= @TypeSelect;
  SIZTypeTable.HeaderVisible:= False;
  SIZTypeTable.AddColumn(EmptyStr);
  SIZTypeTable.SetColumn(0, SIZ_TYPE_PICKS, taLeftJustify);
end;

procedure TSIZNameEditForm.FormShow(Sender: TObject);
begin
  SIZTypeTable.Draw;
  SIZTypeTable.Select(0);
end;

procedure TSIZNameEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
  ModalResult:= mrOK;
end;

procedure TSIZNameEditForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SIZNameTable);
  FreeAndNil(SIZTypeTable);
end;

procedure TSIZNameEditForm.SizeTypeChoose(out AColumnName: String;
                             out AKeys: TIntVector;
                             out APicks: TStrVector);
begin
  if SIZTypeTable.SelectedIndex=0 then
  begin
    AColumnName:= 'Способ выдачи';
    AKeys:= SSO_SIZETYPE_KEYS;
    APicks:= SSO_SIZETYPE_PICKS;
  end
  else begin
    AColumnName:= 'Тип размера';
    AKeys:= SIZ_SIZETYPE_KEYS;
    APicks:= SIZ_SIZETYPE_PICKS;
  end;
end;

procedure TSIZNameEditForm.TypeSelect;
var
  SizeColumn: String;
  SizeKeys: TIntVector;
  SizePicks: TStrVector;
begin
  SizeTypeChoose(SizeColumn, SizeKeys, SizePicks);

  NamePanel.Visible:= False;
  try
    SIZNameTable.Settings('SIZNAME', 'NameID',
      ['SIZName',      'UnitID',            'SizeType' ],
      ['Наименование', 'Единица измерения',  SizeColumn],
      [ctString,        ctKeyPick,           ctKeyPick ],
      [True,            True,                True      ],
      [300,             150,                 150       ],
      [taLeftJustify,   taCenter,            taCenter  ],
      True, True, ['SIZName'], 1,
      [nil,             UnitIDs,             SizeKeys  ],
      [nil,             UnitNames,           SizePicks ],
      'SizType'
    );
    SIZNameTable.MasterIDUpdate(IntToStr(SIZ_TYPE_KEYS[SIZTypeTable.SelectedIndex]));

  finally
    NamePanel.Visible:= True;
  end;
end;

end.

