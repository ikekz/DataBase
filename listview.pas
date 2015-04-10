unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, ExtCtrls, StdCtrls, sqldb, DB, MetaData, SQLRequest, Filters;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    ApplyFilterButton: TButton;
    FilterValueEdit: TEdit;
    FilterComboBox: TComboBox;
    FieldNameComboBox: TComboBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    CreateFilterPanel: TPanel;
    SQLQuery: TSQLQuery;
    procedure ApplyFilterButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; CurrentTag: integer); static;
    procedure FieldNameComboBoxChange(Sender: TObject);
    procedure FilterComboBoxChange(Sender: TObject);
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

procedure TListViewForm.ApplyFilterButtonClick(Sender: TObject);
//var
//  Panel: TPanel;
begin
  //Panel := TPanel.Create(ListViewForm);
  //Panel.Show;
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[TButton(Sender).Parent.Parent.Tag]) +
      CreateFilter(TMyField(FieldNameComboBox.Items.Objects[
      FieldNameComboBox.ItemIndex]), FilterValueEdit.Text,
      TFilter(FilterComboBox.Items.Objects[FilterComboBox.ItemIndex]).Text);
    Prepare;
    //showmessage(SQL.Text);
    ParamByName('param').AsString := FilterValueEdit.Text;

    Open;
  end;
  FillInGrid(TableArray[TButton(Sender).Parent.Parent.Tag], DBGrid);

  //SQLQuery.SQL.Text := CreateSelect(TableArray[TButton(Sender).Parent.Parent.Tag]) +
  //  CreateFilter(TMyField(FieldNameComboBox.Items.Objects[FieldNameComboBox.ItemIndex]));
  //Showmessage(TMyField(FieldNameComboBox.Items.Objects[FieldNameComboBox.ItemIndex]).Caption);
end;

procedure TListViewForm.FieldNameComboBoxChange(Sender: TObject);
var
  //MenuItem: TMenuItem;
  i: integer;
begin
  //showmessage(TMyField(FieldNameComboBox.Items.Objects[FieldNameComboBox.ItemIndex]).DataType);
  if TMyField(FieldNameComboBox.Items.Objects[FieldNameComboBox.ItemIndex]).DataType = 'VARCHAR' then
    FillInFilterComboBoxVarchar(FilterComboBox)
  else
    FillInFilterComboBoxInteger(FilterComboBox);
    //MenuItem := TMenuItem.Create(DirectoryMenu);
    //FieldNameComboBox.Items[i] := DBGrid.Columns[i].FieldName;
    //DirectoryMenu.Add(MenuItem);
    //MenuItem.Tag := i;
    //MenuItem.Caption := TableArray[i].Caption;
    //MenuItem.OnClick := @DirectoryMenuItemClick;

end;


procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
