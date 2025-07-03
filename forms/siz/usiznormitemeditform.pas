unit USIZNormItemEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, BCButton, DateUtils, VirtualTrees,
  //DK packages utils
  DK_CtrlUtils, DK_Const, DK_StrUtils, DK_Dialogs, DK_VSTTableTools, DK_Vector,
  DK_VSTDropDown,
  //Project utils
  UTypes, UVars;

type

  { TSIZNormItemEditForm }

  TSIZNormItemEditForm = class(TForm)
    ButtonPanel: TPanel;
    ButtonPanelBevel: TBevel;
    CancelButton: TSpeedButton;
    NormNameBCButton: TBCButton;
    PostPanel: TPanel;
    PostLabel: TLabel;
    NormNameLabel: TLabel;
    PostVT: TVirtualStringTree;
    SaveButton: TSpeedButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    PostList: TVSTCheckList;
    NormNameDropDown: TVSTDropDown;

    PostIDs, EditablePostIDs: TIntVector;
    PostNames, EditablePostNames: TStrVector;
    EditablePostChecks: TBoolVector;

    OldPostItemIDs, OldPostIDs: TIntVector;

    NormIDs: TIntVector;
    NormNames: TStrVector;
    NormBDs, NormEDs: TDateVector;

    function NormsLoad: Boolean;
    procedure NormsItemSelect;

    procedure EditablePostListLoad(const ANormID: Integer);
  public
    NormID, ItemID: Integer;
    EditingType: TEditingType;
  end;

var
  SIZNormItemEditForm: TSIZNormItemEditForm;

implementation

{$R *.lfm}

{ TSIZNormItemEditForm }

procedure TSIZNormItemEditForm.FormCreate(Sender: TObject);
begin
  ItemID:= -1;
  NormNameDropDown:= TVSTDropDown.Create(NormNameBCButton);
  NormNameDropDown.OnChange:= @NormsItemSelect;
end;

procedure TSIZNormItemEditForm.FormDestroy(Sender: TObject);
begin
  if Assigned(PostList) then FreeAndNil(PostList);
  FreeAndNil(NormNameDropDown);
end;

procedure TSIZNormItemEditForm.FormShow(Sender: TObject);
begin
  Images.ToButtons([SaveButton, CancelButton]);
  SetEventButtons([SaveButton, CancelButton]);

  DataBase.KeyPickList('STAFFPOST', 'PostID', 'PostName',
                       PostIDs, PostNames, True, 'PostName');
  if VIsNil(PostIDs) then
  begin
    Inform('Нет ни одной должности в базе!');
    Exit;
  end;

  EditablePostListLoad(NormID);
  NormsLoad;
end;

procedure TSIZNormItemEditForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TSIZNormItemEditForm.SaveButtonClick(Sender: TObject);
var
  IsOK: Boolean;
  CheckedPostIDs, AddPostIDs, DelItemPostIDs: TIntVector;

  function IsCollision: Boolean;
  var
    i, ThisItemID: Integer;
    IntersectionNormName, IntersectionOrderNum: String;
  begin
    Result:= False;
    if EditingType=etEdit then
      //при редактировании нужно проверять на существование все пункты, кроме этого (редактируемого)
      ThisItemID:= ItemID
    else
      //при добавлении и копировании нужно проверять на существование все пункты
      ThisItemID:= 0;

    //ищем пересечения периодов действия с уже записанными нормами
    //период действия записываемой нормы
    for i:=0 to High(EditablePostIDs) do //пробегаем по всем выбранным должностям
    begin
      if not PostList.Checked[i] then continue;
      //если есть пересечение по периодам действия - выход
      if DataBase.SIZNormItemIntersectionExists(EditablePostIDs[i], ThisItemID,
                             NormBDs[NormNameDropDown.ItemIndex],
                             NormEDs[NormNameDropDown.ItemIndex],
                             IntersectionNormName, IntersectionOrderNum) then
      begin
        Inform('Период действия записываемого пункта норм для должности "' +
               EditablePostNames[i] +
               '" пересекается с периодом действия пункта № "' +
               IntersectionOrderNum +
               '" нормы "' + IntersectionNormName + '"!');
        Result:= True;
        Exit;
      end;
    end;
  end;

  function UpdateValuesLoad: Boolean;
  var
    i: Integer;
    S: String;
  begin
    //должности добавлены в этот пункт
    AddPostIDs:= nil;
    for i:=0 to High(CheckedPostIDs) do
      if VIndexOf(OldPostIDs, CheckedPostIDs[i])<0 then
        VAppend(AddPostIDs, CheckedPostIDs[i]);

    //ID записей соответствия удалены из пункта
    DelItemPostIDs:= nil;
    for i:=0 to High(OldPostIDs) do
      if VIndexOf(CheckedPostIDs, OldPostIDs[i])<0 then
        VAppend(DelItemPostIDs, OldPostItemIDs[i]);

    Result:= (not VIsNil(AddPostIDs)) or (not VIsNil(DelItemPostIDs));
    if not Result then
      Inform('Не внесено никаких изменений!');

    S:= EmptyStr;
    if Length(DelItemPostIDs)=1 then
      S:= 'Удаляемая должность (профессия) будет записана в отдельный пункт.'
    else if Length(DelItemPostIDs)>1 then
      S:= 'Удаляемые должности (профессии) будут записаны в отдельный пункт.';
    if not SEmpty(S) then
      Inform(S);
  end;

begin

  if PostList.IsAllUnchecked then
  begin
    Inform('Не выбрано ни одной должности (профессии)!');
    Exit;
  end;

  if IsCollision then Exit;

  CheckedPostIDs:= VCut(EditablePostIDs, PostList.Selected);
  if EditingType=etEdit then //редактирование
  begin
    if not UpdateValuesLoad then Exit;
    IsOK:= DataBase.SIZNormItemUpdate(NormID, ItemID, AddPostIDs, DelItemPostIDs);
  end
  else if EditingType=etAdd then //новый пункт
    IsOK:= DataBase.SIZNormItemAdd(NormID, ItemID, CheckedPostIDs)
  else //копирование в другие типовые нормы
    IsOK:= DataBase.SIZNormItemCopy(NormIDs[NormNameDropDown.ItemIndex],
                                      ItemID, CheckedPostIDs);

  if not IsOK then Exit;
  ModalResult:= mrOK;
end;

function TSIZNormItemEditForm.NormsLoad: Boolean;
var
  i: Integer;
  S: String;
  Notes: TStrVector;
begin
  NormNameDropDown.Clear;
  Result:= DataBase.SIZNormsLoad(NormIDs, NormNames, Notes, NormBDs, NormEDs);
  if not Result then Exit;

  for i:=0 to High(NormIDs) do
  begin
    if SameDate(NormEDs[i], INFDATE) then
      S:= 'настоящее время)'
    else
      S:= FormatDateTime('dd.mm.yyyy)', NormEDs[i]);
    S:= FormatDateTime(' (с dd.mm.yyyy по ', NormBDs[i]) + S;
    NormNames[i]:= NormNames[i] + S;
  end;

  NormNameDropDown.Items:= NormNames;
  NormNameDropDown.ItemIndex:= VIndexOf(NormIDs, NormID);
  NormNameDropDown.Enabled:= EditingType=UTypes.etCustom;
end;

procedure TSIZNormItemEditForm.NormsItemSelect;
begin
  EditablePostListLoad(NormIDs[NormNameDropDown.ItemIndex]);
end;

procedure TSIZNormItemEditForm.EditablePostListLoad(const ANormID: Integer);
var
  BusyPostIDs, BusyItemIDs, BusyPostItemIDs: TIntVector;

  //для редактирования: нужно отметить все должности, записанные в этом пункте,
  //а остальные занятые выкинуть
  procedure EditablePostListForEditLoad;
  var
    i, N: Integer;
  begin
    for i:= 0 to High(BusyPostIDs) do
    begin
      //индекс должности в векторах
      N:= VIndexOf(EditablePostIDs, BusyPostIDs[i]);
      //если должность приписана этому пункту, устанавливаем отметку
      if BusyItemIDs[i] = ItemID then
        EditablePostChecks[N]:= True
      else begin  //должность приписана другому пункту
        //удаляем её из списка, т.к. она занята
        VDel(EditablePostIDs, N);
        VDel(EditablePostChecks, N);
        VDel(EditablePostNames, N);
      end;
    end;
    //заполняем вектор ID записей соответствия пункта и должности
    OldPostItemIDs:= nil;
    OldPostIDs:= nil;
    for i:= 0 to High(EditablePostIDs) do
    begin
      if not EditablePostChecks[i] then continue;
      N:= VIndexOf(BusyPostIDs, EditablePostIDs[i]);
      VAppend(OldPostItemIDs, BusyPostItemIDs[N]);
      VAppend(OldPostIDs, BusyPostIDs[N]);
    end;
  end;

  //для добавления, копирования: нужно выкинуть все занятые должности
  procedure EditablePostListForAddAndCopyLoad;
  var
    i, N: Integer;
  begin
    for i:= 0 to High(BusyPostIDs) do
    begin
      //индекс должности в векторах
      N:= VIndexOf(EditablePostIDs, BusyPostIDs[i]);
      //удаляем её из списка, т.к. она занята
      VDel(EditablePostIDs, N);
      VDel(EditablePostChecks, N);
      VDel(EditablePostNames, N);
    end;
  end;

begin
  //ID и наименования всех должностей
  EditablePostIDs:= VCut(PostIDs);
  EditablePostNames:= VCut(PostNames);
  VDim(EditablePostChecks, Length(PostIDs), False);

  //достаем ID должностей уже приписанных к данной норме
  DataBase.SIZItemsAndPostsAccordanceLoad(ANormID, BusyPostIDs, BusyItemIDs, BusyPostItemIDs);

  if EditingType=etEdit then //редактирование
    EditablePostListForEditLoad
  else  //добавление, копирование
    EditablePostListForAddAndCopyLoad;

  if VIsNil(EditablePostIDs) then
  begin
    Inform('Все должности распределены по другим пунктам!');
    Exit;
  end;

  //отображаем список должностей
  if Assigned(PostList) then FreeAndNil(PostList);
  PostList:= TVSTCheckList.Create(PostVT, EmptyStr, EditablePostNames, nil);
  PostList.Selected:= EditablePostChecks;
end;

end.

