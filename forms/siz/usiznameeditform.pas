unit USIZNameEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  VirtualTrees,
  //DK packages utils
  DK_VSTTables, DK_VSTTypes, DK_DBTable, DK_Vector,
  //Project utils
  UDataBase, UConst;

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
  private
    TypeList: TVSTTable;
    NameTable: TDBTable;

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

uses UMainForm;

{$R *.lfm}

{ TSIZNameEditForm }

procedure TSIZNameEditForm.FormCreate(Sender: TObject);
begin
  DataBase.KeyPickList('SIZUNIT', 'UnitID', 'UnitName', UnitIDs, UnitNames);

  TypeList:= TVSTTable.Create(TypeVT);
  TypeList.SetSingleFont(MainForm.GridFont);
  TypeList.CanSelect:= True;
  TypeList.CanUnselect:= True;
  TypeList.OnSelect:= @TypeSelect;
  TypeList.HeaderVisible:= False;
  TypeList.AddColumn(EmptyStr);
  TypeList.SetColumn(0, SIZ_TYPE_PICKS, taLeftJustify);
  TypeList.Draw;
  TypeList.Select(0);
end;

procedure TSIZNameEditForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
  ModalResult:= mrOK;
end;

procedure TSIZNameEditForm.FormDestroy(Sender: TObject);
begin
  if Assigned(NameTable) then FreeAndNil(NameTable);
  FreeAndNil(TypeList);
end;

procedure TSIZNameEditForm.SizeTypeChoose(out AColumnName: String;
                             out AKeys: TIntVector;
                             out APicks: TStrVector);
begin
  if TypeList.SelectedIndex=0 then
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

  //case TypeList.SelectedIndex of
  //0: //Средства дерматологические
  //  begin
  //    AKeys:= SSO_SIZETYPE_KEYS;
  //    APicks:= SSO_SIZETYPE_PICKS;
  //  end;
  //1: //Одежда специальная защитная
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[1]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[1]]);
  //  end;
  //2: //Средства защиты ног
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[2]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[2]]);
  //  end;
  //3: //Средства защиты головы
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[3]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[3]]);
  //  end;
  //4: //Средства защиты рук
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[4]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[4]]);
  //  end;
  //5: //Средства защиты глаз
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[3], SIZ_SIZETYPE_KEYS[5]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[3], SIZ_SIZETYPE_PICKS[5]]);
  //  end;
  //6: //Средства защиты органов дыхания
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[5], SIZ_SIZETYPE_KEYS[6]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[5], SIZ_SIZETYPE_PICKS[6]]);
  //  end;
  //7: //Средства защиты органов слуха
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[3]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[3]]);
  //  end;
  //8: //Средства защиты лица
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[3], SIZ_SIZETYPE_KEYS[5]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[3], SIZ_SIZETYPE_PICKS[5]]);
  //  end;
  //9: //Средства защиты от падения с высоты
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[1]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[1]]);
  //  end;
  //10: //Средства защиты опорно-двигательного аппарата
  //  begin
  //    AKeys:= VCreateInt([0, SIZ_SIZETYPE_KEYS[1]]);
  //    APicks:= VCreateStr(['<нет>', SIZ_SIZETYPE_PICKS[1]]);
  //  end;
  //end;
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
    if Assigned(NameTable) then FreeAndNil(NameTable);
    NameTable:= TDBTable.Create(MainForm.GridFont, NamePanel, DataBase, True, 'Фильтр:');
    NameTable.Edit.HeaderFont.Style:= [fsBold];
    NameTable.Settings('SIZNAME', 'NameID',
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
    NameTable.MasterIDUpdate(IntToStr(SIZ_TYPE_KEYS[TypeList.SelectedIndex]));

  finally
    NamePanel.Visible:= True;
  end;
end;

end.

