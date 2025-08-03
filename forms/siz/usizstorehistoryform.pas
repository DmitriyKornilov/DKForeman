unit USIZStoreHistoryForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, VirtualTrees,
  Buttons, DividerBevel,
  //Project utils
  UVars, UConst, USIZStoreTypes, USIZStoreSheet,
  //DK packages utils
  DK_Vector, DK_Matrix, DK_CtrlUtils, DK_Filter, DK_SheetExporter;

type

  { TSIZStoreHistoryForm }

  TSIZStoreHistoryForm = class(TForm)
    CloseButton: TSpeedButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    CollapseAllButton: TSpeedButton;
    DividerBevel3: TDividerBevel;
    DividerBevel4: TDividerBevel;
    ExpandAllButton: TSpeedButton;
    FilterSizNamePanel: TPanel;
    FilterNomNumPanel: TPanel;
    VT: TVirtualStringTree;
    DocViewButtonPanel: TPanel;
    ExportButton: TSpeedButton;
    ToolPanel: TPanel;
    procedure CloseButtonClick(Sender: TObject);
    procedure CollapseAllButtonClick(Sender: TObject);
    procedure ExpandAllButtonClick(Sender: TObject);
    procedure ExportButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FilterSizName, FilterNomNum: String;
    HistoryList: TSIZStoreHistoryTable;

    NomNums, SizNames: TStrVector;
    Infos: TStrMatrix;
    Counts: TIntMatrix;

    procedure HistoryCreate;
    procedure HistoryLoad;
    procedure HistorySizNameFilter(const AFilterString: String);
    procedure HistoryNomNumFilter(const AFilterString: String);
    procedure HistoryExport;
  public

  end;

var
  SIZStoreHistoryForm: TSIZStoreHistoryForm;

implementation

{$R *.lfm}

{ TSIZStoreHistoryForm }

procedure TSIZStoreHistoryForm.FormCreate(Sender: TObject);
begin
  Caption:= MAIN_CAPTION + OTHER_DESCRIPTION[11];
  HistoryCreate;
  DKFilterCreate('Поиск по наименованию СИЗ:', FilterSizNamePanel, @HistorySizNameFilter, 300);
  DKFilterCreate('Поиск по номенклатурному номеру:', FilterNomNumPanel, @HistoryNomNumFilter, 300);
end;

procedure TSIZStoreHistoryForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(HistoryList);
end;

procedure TSIZStoreHistoryForm.FormShow(Sender: TObject);
begin
  SetToolPanels([
    ToolPanel
  ]);

  SetToolButtons([
    CloseButton,
    ExpandAllButton, CollapseAllButton
  ]);

  Images.ToButtons([
    ExportButton,
    CloseButton,
    ExpandAllButton, CollapseAllButton
  ]);

  HistoryLoad;
end;

procedure TSIZStoreHistoryForm.HistoryCreate;
begin
  HistoryList:= TSIZStoreHistoryTable.Create(VT);
  HistoryList.TreeLinesVisible:= False;
  HistoryList.Span:= True;
  HistoryList.SetSingleFont(GridFont);
  HistoryList.HeaderFont.Style:= [fsBold];
  HistoryList.CategoryFont.Style:= [fsBold];
  HistoryList.HistoryItemFont.Style:= [fsBold];
  HistoryList.HistoryItems:= VCreateStr(['Получено', 'Выдано', 'Возвращено', 'Списано', 'Остаток']);
  HistoryList.AddColumn('Номенклатурный номер и наименование СИЗ / информация о движении СИЗ', 700);
  HistoryList.AddColumn('Количество', 100);
  HistoryList.AutosizeColumnDisable;
  HistoryList.Draw;
end;

procedure TSIZStoreHistoryForm.HistoryLoad;
var
  CategoryNames: TStrVector;
begin
  DataBase.SIZStoreHistoryLoad(FilterSizName, FilterNomNum,
                               NomNums, SizNames, Infos, Counts);

  CategoryNames:= VSum(VSum(NomNums, ' - '), SizNames);

  HistoryList.Visible:= False;
  try
    HistoryList.ValuesClear;
    HistoryList.SetCategories(CategoryNames);
    HistoryList.SetColumn(0, Infos, taLeftJustify);
    HistoryList.SetColumn(1, MIntToStr(Counts));
    HistoryList.Draw;
    HistoryList.ExpandAll(True);
    HistoryList.ShowFirst;
  finally
    HistoryList.Visible:= True;
  end;

  ExportButton.Enabled:= not VIsNil(NomNums);
end;

procedure TSIZStoreHistoryForm.HistorySizNameFilter(const AFilterString: String);
begin
  FilterSizName:= AFilterString;
  HistoryLoad;
end;

procedure TSIZStoreHistoryForm.HistoryNomNumFilter(const AFilterString: String);
begin
  FilterNomNum:= AFilterString;
  HistoryLoad;
end;

procedure TSIZStoreHistoryForm.HistoryExport;
var
  Exporter: TSheetsExporter;
  Worksheet: TsWorksheet;
  ExpSheet: TSIZStoreHistorySheet;
begin
  Exporter:= TSheetsExporter.Create;
  try
    Worksheet:= Exporter.AddWorksheet('Лист1');
    ExpSheet:= TSIZStoreHistorySheet.Create(Worksheet, nil, GridFont);
    try
      ExpSheet.Draw(NomNums, SizNames, HistoryList.HistoryItems, Infos, Counts);
    finally
      FreeAndNil(ExpSheet);
    end;
    Exporter.Save('Выполнено!', 'История движения СИЗ по складу на ' + DateToStr(Date));
  finally
    FreeAndNil(Exporter);
  end;
end;

procedure TSIZStoreHistoryForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TSIZStoreHistoryForm.CollapseAllButtonClick(Sender: TObject);
begin
  HistoryList.ExpandAll(False);
end;

procedure TSIZStoreHistoryForm.ExpandAllButtonClick(Sender: TObject);
begin
  HistoryList.ExpandAll(True);
end;

procedure TSIZStoreHistoryForm.ExportButtonClick(Sender: TObject);
begin
  HistoryExport;
end;

end.

