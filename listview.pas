unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, ExtCtrls, StdCtrls, sqldb, DB, MetaData, SQLRequest, Filters;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    AddFilterButton: TButton;
    FilterValueEdit: TEdit;
    FilterComboBox: TComboBox;
    FieldNameComboBox: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    CreateFilterPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure AddFilterButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; CurrentTag: integer); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    class procedure FillInGrid(Table: TMyTable; Grid: TDBGrid); static;
    class procedure FillInFieldNameComboBox(Table: TMyTable; ComboBox: TComboBox); static;
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
    FillInFieldNameComboBox(Table, FieldNameComboBox);
    FillInFilterComboBox(FilterComboBox);
    Tag := CurrentTag;
    Caption := Table.Caption;
    Show;
  end;
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

class procedure TListViewForm.FillInFieldNameComboBox(Table: TMyTable;
  ComboBox: TComboBox);
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

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
begin
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[TButton(Sender).Parent.Parent.Tag]) +
      CreateFilter(TMyField(FieldNameComboBox.Items.Objects[
      FieldNameComboBox.ItemIndex]), FilterValueEdit.Text,
      TFilter(FilterComboBox.Items.Objects[FilterComboBox.ItemIndex]).Text);
    Prepare;
    ParamByName('param').AsString := FilterValueEdit.Text;

    Open;
  end;
  FillInGrid(TableArray[TButton(Sender).Parent.Parent.Tag], DBGrid);
end;


procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
