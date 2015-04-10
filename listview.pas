unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, ExtCtrls, StdCtrls, Grids, sqldb, DB, MetaData, SQLRequest, Filters;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    ApplyFilterButton: TButton;
    AddFilterButton: TButton;
    FilterValueEdit: TEdit;
    ConditionComboBox: TComboBox;
    FieldComboBox: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    CreateFilterPanel: TPanel;
    ConditionsListBox: TListBox;
    SQLQuery: TSQLQuery;
    FiltersStringGrid: TStringGrid;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure ApplyFilterButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; CurrentTag: integer); static;
    procedure FieldComboBoxChange(Sender: TObject);
    procedure FiltersStringGridDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FinishedFilterArray: array of TFinishedFilter;
    procedure AddFinishedFilter(CurrentField: TMyField;
      CurrentCondition: TCondition; CurrentValue, CurrentOperation: string);
    class procedure FillInGrid(Table: TMyTable; Grid: TDBGrid); static;
    class procedure FillInFieldComboBox(Table: TMyTable;
      ComboBox: TComboBox); static;
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
      Close;
      SQL.Text := CreateSelect(Table);
      Open;
    end;
    FillInGrid(Table, DBGrid);
    FillInFieldComboBox(Table, FieldComboBox);
    Tag := CurrentTag;
    Caption := Table.Caption;
    Show;
  end;
end;

procedure TListViewForm.AddFinishedFilter(CurrentField: TMyField;
  CurrentCondition: TCondition; CurrentValue, CurrentOperation: string);
begin
  SetLength(FinishedFilterArray, Length(FinishedFilterArray) + 1);
  if Length(FinishedFilterArray) = 1 then CurrentOperation := ' WHERE ';
  FinishedFilterArray[High(FinishedFilterArray)] := TFinishedFilter.Create;
  FinishedFilterArray[High(FinishedFilterArray)].Field := CurrentField;
  FinishedFilterArray[High(FinishedFilterArray)].Condition := CurrentCondition;
  FinishedFilterArray[High(FinishedFilterArray)].Value := CurrentValue;
  FinishedFilterArray[High(FinishedFilterArray)].Operation := CurrentOperation;
end;

class procedure TListViewForm.FillInGrid(Table: TMyTable; Grid: TDBGrid);
var
  i: integer;
begin
  for i := 0 to High(Table.FieldArray) do
  begin
    with Grid.Columns[i] do
    begin
      FieldName := Table.FieldArray[i].Name;
      Width := Table.FieldArray[i].Width;
      Title.Caption := Table.FieldArray[i].Caption;
      Visible := Table.FieldArray[i].Visible;
    end;
  end;
end;

class procedure TListViewForm.FillInFieldComboBox(Table: TMyTable; ComboBox: TComboBox);
var
  i: integer;
begin
  for i := 0 to High(Table.FieldArray) do
  begin
    if Table.FieldArray[i].Visible then
    begin
      ComboBox.Items.AddObject(Table.FieldArray[i].Caption,
        Table.FieldArray[i]);
    end;
  end;
end;

procedure TListViewForm.ApplyFilterButtonClick(Sender: TObject);
begin
  AddFinishedFilter(
    TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]),
    TCondition(ConditionComboBox.Items.Objects[ConditionComboBox.ItemIndex]),
    FilterValueEdit.Text, 'AND ');

  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[TButton(Sender).Parent.Parent.Tag]) +
      CreateFilter(FinishedFilterArray);
    ShowMessage(SQL.Text);
    Open;
  end;
  FillInGrid(TableArray[TButton(Sender).Parent.Parent.Tag], DBGrid);

  FillInFiltersStringGrid(FiltersStringGrid, FinishedFilterArray);
end;

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
begin
  FillInFiltersStringGrid(FiltersStringGrid, FinishedFilterArray);
end;

procedure TListViewForm.FieldComboBoxChange(Sender: TObject);
var
  i: integer;
begin
  if TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).DataType =
    'Varchar' then
    FillInConditionComboBoxVarchar(ConditionComboBox)
  else
    FillInConditionComboBoxInteger(ConditionComboBox);

end;

procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
