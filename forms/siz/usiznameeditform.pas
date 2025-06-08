unit USIZNameEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VirtualTrees,
  //DK packages utils
  DK_VSTTableTools, DK_VSTTypes, DK_DBTable, DK_Vector,
  //Project utils
  UDataBase, UConst;

type

  { TSIZNameEditForm }

  TSIZNameEditForm = class(TForm)
    NamePanel: TPanel;
    Splitter: TSplitter;
    TypePanel: TPanel;
    TypeVT: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    TypeList: TVSTStringList;
    NameTable: TDBTable;

    UnitIDs: TIntVector;
    UnitNames: TStrVector;

    procedure TypeSelect;
  public

  end;

var
  SIZNameEditForm: TSIZNameEditForm;

implementation

uses UMainForm;

{$R *.lfm}

{ TSIZNameEditForm }

procedure TSIZNameEditForm.FormCreate(Sender: TObject);
begin
  DataBase.KeyPickList('SIZUNIT', 'UnitID', 'UnitName', UnitIDs, UnitNames);

  TypeList:= TVSTStringList.Create(TypeVT, EmptyStr, nil);
  TypeList.OnSelect:= @TypeSelect;
  TypeList.Update(SIZ_TYPE_PICKS);
end;

procedure TSIZNameEditForm.FormDestroy(Sender: TObject);
begin
  if Assigned(NameTable) then FreeAndNil(NameTable);
  FreeAndNil(TypeList);
end;

procedure TSIZNameEditForm.TypeSelect;
var
  SizeColumn: String;
  SizeKeys: TIntVector;
  SizePicks: TStrVector;
begin
  if TypeList.ItemIndex=High(SIZ_TYPE_PICKS) then
  begin
    SizeColumn:= 'Способ выдачи';
    SizeKeys:= SSO_SIZETYPE_KEYS;
    SizePicks:= SSO_SIZETYPE_PICKS;
  end
  else begin
    SizeColumn:= 'Тип размера';
    SizeKeys:= SIZ_SIZETYPE_KEYS;
    SizePicks:= SIZ_SIZETYPE_PICKS;
  end;

  if Assigned(NameTable) then FreeAndNil(NameTable);
  NameTable:= TDBTable.Create(NamePanel, DataBase.Query);
  NameTable.Edit.HeaderFont.Style:= [fsBold];
  NameTable.Settings(MainForm.GridFont, 'SIZNAME', 'NameID',
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
  NameTable.Update(IntToStr(SIZ_TYPE_KEYS[TypeList.ItemIndex]));
end;

end.

