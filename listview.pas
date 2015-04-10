unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, ExtCtrls, StdCtrls, Grids, Menus, PairSplitter, sqldb, DB, MetaData,
  SQLRequest, FilterAndSort;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    ApplyFilterButton: TButton;
    AddFilterButton: TButton;
    DeleteFilterButton: TButton;
    DeleteAllFilterButton: TButton;
    FilterValueEdit: TEdit;
    ConditionComboBox: TComboBox;
    FieldComboBox: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    CreateFilterPanel: TPanel;
    AboutFieldComboBoxLabel: TLabel;
    AboutConditionComboBoxLabel: TLabel;
    AboutFilterValueEditLabel: TLabel;
    IsFilterApplyLabel: TLabel;
    MainMenu: TMainMenu;
    DataMenu: TMenuItem;
    FilterMenu: TMenuItem;
    AddedFilterPanel: TPanel;
    OperationComboBox: TComboBox;
    ShowFilterMenu: TMenuItem;
    TurnFilterMenu: TMenuItem;
    SQLQuery: TSQLQuery;
    FilterStringGrid: TStringGrid;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure ApplyFilterButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; CurrentTag: integer); static;
    procedure DBGridTitleClick(Column: TColumn);
    procedure DeleteAllFilterButtonClick(Sender: TObject);
    procedure FieldComboBoxChange(Sender: TObject);
    procedure ShowFilterMenuClick(Sender: TObject);
    procedure DeleteFilterButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TurnFilterMenuClick(Sender: TObject);
  private
    FinishedFilterArray: array of TFinishedFilter;
    SortArray: array of TSort;
    procedure AddFinishedFilter(CurrentField: TMyField;
      CurrentCondition: TCondition; CurrentValue, CurrentOperation: string;
      CurrentIsApply: boolean);
    procedure AddSort(CurrentFieldName, CurrentOrder: string);
    procedure FillInDBGrid(Table: TMyTable; Grid: TDBGrid);
    procedure FillInFieldComboBox(Table: TMyTable; ComboBox: TComboBox);
    procedure SelectTypeCondition;
    procedure RunSQL;
    procedure VisibleComponents(CurrentVisible: boolean);
  end;

var
  ListViewForm: TListViewForm;

implementation

{$R *.lfm}

{ TListViewForm }

class procedure TListViewForm.CreateNewForm(Table: TMyTable; CurrentTag: integer);
begin
  Application.CreateForm(TListViewForm, ListViewForm);
  with ListViewForm do
  begin
    with SQLQuery do
    begin
      Tag := CurrentTag;
      Close;
      SQL.Text := CreateSelect(Table);
      Open;
    end;
    FillInDBGrid(Table, DBGrid);
    FillInFieldComboBox(Table, FieldComboBox);
    SQLQuery.Tag := CurrentTag;
    Tag := CurrentTag;
    Caption := Table.Caption;
    Show;
  end;
end;

procedure TListViewForm.DBGridTitleClick(Column: TColumn);
var
  i, j: integer;
  IsFind: boolean;
begin
  IsFind := False;
  for i := 0 to High(SortArray) do
  begin
    if SortArray[i].FieldName = TableArray[Tag].FieldArray[Column.Index].Name then
    begin
      IsFind := True;
      if SortArray[i].Order = '' then
      begin
        SortArray[i].Order := ' desc ';
      end
      else
      begin
        for j := i to High(SortArray) - 1 do
        begin
          SortArray[j] := SortArray[j + 1];
        end;
        SetLength(SortArray, Length(SortArray) - 1);
        Break;
      end;
    end;
  end;
  if not IsFind then
    AddSort(TableArray[Tag].FieldArray[Column.Index].Name, '');
  RunSQL;
  FillInDBGrid(TableArray[Tag], DBGrid);
end;

procedure TListViewForm.DeleteAllFilterButtonClick(Sender: TObject);
begin
  SetLength(FinishedFilterArray, 0);
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  RunSQL;
  FillInDBGrid(TableArray[Tag], DBGrid);
  VisibleComponents(False);
end;

procedure TListViewForm.AddFinishedFilter(CurrentField: TMyField;
  CurrentCondition: TCondition; CurrentValue, CurrentOperation: string;
  CurrentIsApply: boolean);
begin
  SetLength(FinishedFilterArray, Length(FinishedFilterArray) + 1);
  if Length(FinishedFilterArray) = 1 then
    CurrentOperation := ' WHERE ';
  FinishedFilterArray[High(FinishedFilterArray)] := TFinishedFilter.Create;
  FinishedFilterArray[High(FinishedFilterArray)].Field := CurrentField;
  FinishedFilterArray[High(FinishedFilterArray)].Condition := CurrentCondition;
  FinishedFilterArray[High(FinishedFilterArray)].Value := CurrentValue;
  FinishedFilterArray[High(FinishedFilterArray)].Operation := CurrentOperation;
  FinishedFilterArray[High(FinishedFilterArray)].IsApply := CurrentIsApply;
end;

procedure TListViewForm.RunSQL;
var
  i: integer;
begin
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[Tag]) + CreateFilter(FinishedFilterArray) +
      CreateSort(SortArray);
    Open;
  end;
end;

procedure TListViewForm.AddSort(CurrentFieldName, CurrentOrder: string);
begin
  SetLength(SortArray, Length(SortArray) + 1);
  SortArray[High(SortArray)].FieldName := CurrentFieldName;
  SortArray[High(SortArray)].Order := CurrentOrder;
end;

procedure TListViewForm.SelectTypeCondition;
begin
  if TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).DataType =
    'Varchar' then
    FillInConditionComboBoxVarchar(ConditionComboBox)
  else
    FillInConditionComboBoxInteger(ConditionComboBox);
end;

procedure TListViewForm.VisibleComponents(CurrentVisible: boolean);
begin
  DeleteFilterButton.Visible := CurrentVisible;
  ApplyFilterButton.Visible := CurrentVisible;
  DeleteAllFilterButton.Visible := CurrentVisible;
  OperationComboBox.Visible := CurrentVisible;
  IsFilterApplyLabel.Visible := False;
end;

procedure TListViewForm.FillInDBGrid(Table: TMyTable; Grid: TDBGrid);
var
  i, j: integer;
begin
  for i := 0 to High(Table.FieldArray) do
  begin
    with Grid.Columns[i] do
    begin
      FieldName := Table.FieldArray[i].Name;
      Width := Table.FieldArray[i].Width;
      Title.Caption := Table.FieldArray[i].Caption;
      Visible := Table.FieldArray[i].Visible;
      for j := 0 to High(SortArray) do
      begin
        if FieldName = SortArray[j].FieldName then
          if SortArray[j].Order = '' then
          begin
            Title.Caption := Title.Caption + ' ▲' + IntToStr(j + 1);
          end
          else
          begin
            Title.Caption := Title.Caption + ' ▼' + IntToStr(j + 1);
          end;
      end;
    end;
  end;
end;

procedure TListViewForm.FillInFieldComboBox(Table: TMyTable; ComboBox: TComboBox);
var
  i: integer;
begin
  with ComboBox do
  begin
    for i := 0 to High(Table.FieldArray) do
    begin
      if Table.FieldArray[i].Visible then
        Items.AddObject(Table.FieldArray[i].Caption, Table.FieldArray[i]);
    end;
    Text := Items[0];
  end;
  SelectTypeCondition;
end;

procedure TListViewForm.ApplyFilterButtonClick(Sender: TObject);
var
  i: integer;
begin

  for i := 0 to High(FinishedFilterArray) do
    FinishedFilterArray[i].IsApply := True;
  RunSQL;
  FillInDBGrid(TableArray[Tag], DBGrid);
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  IsFilterApplyLabel.Visible := True;
end;

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
var
  x, ReturnCode: integer;
  OperationTmp: string;
begin
  if TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).DataType =
    'Integer' then
  begin
    Val(FilterValueEdit.Text, x, ReturnCode);
    if ReturnCode <> 0 then
    begin
      ShowMessage('Введите допустимое значение');
      exit;
    end;
  end;
  if OperationComboBox.Text = 'И' then
    OperationTmp := ' AND '
  else
    OperationTmp := ' OR ';
  if length(FinishedFilterArray) > 0 then
  begin
    with FinishedFilterArray[High(FinishedFilterArray)] do
    begin
      if (Field.Caption = FieldComboBox.Text) and
        (Condition.Caption = ConditionComboBox.Text) and
        (Value = FilterValueEdit.Text) and ((Operation = OperationTmp) or
        (Operation = ' WHERE ')) then
        exit;
    end;
  end;

  AddFinishedFilter(
    TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]),
    TCondition(ConditionComboBox.Items.Objects[ConditionComboBox.ItemIndex]),
    FilterValueEdit.Text, OperationTmp, False);
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  VisibleComponents(True);
end;

procedure TListViewForm.FieldComboBoxChange(Sender: TObject);
begin
  SelectTypeCondition;

end;

procedure TListViewForm.ShowFilterMenuClick(Sender: TObject);
begin
  CreateFilterPanel.Visible := True;
  AddedFilterPanel.Visible := True;
end;

procedure TListViewForm.DeleteFilterButtonClick(Sender: TObject);
var
  i: integer;
  IsApplyTmp: boolean;
begin
  if Length(FinishedFilterArray) = 0 then
    exit;
  IsApplyTmp := FinishedFilterArray[FilterStringGrid.Row - 1].IsApply;
  for i := FilterStringGrid.Row - 1 to High(FinishedFilterArray) - 1 do
  begin
    FinishedFilterArray[i] := FinishedFilterArray[i + 1];
  end;
  SetLength(FinishedFilterArray, Length(FinishedFilterArray) - 1);
  if Length(FinishedFilterArray) > 0 then
  begin
    FinishedFilterArray[0].Operation := ' WHERE ';
  end
  else
  begin
    VisibleComponents(False);
    OperationComboBox.Text := 'И';
    RunSQL;
  end;
  if IsApplyTmp then
    RunSQL;
  IsFilterApplyLabel.Visible := False;
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  FillInDBGrid(TableArray[Tag], DBGrid);
end;

procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TListViewForm.TurnFilterMenuClick(Sender: TObject);
begin
  CreateFilterPanel.Visible := False;
  AddedFilterPanel.Visible := False;
end;

end.
