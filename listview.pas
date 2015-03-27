unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, sqldb, DB, MetaData;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    SQLQuery: TSQLQuery;
    class procedure CreateNewForm(Table: TMyTable; CurrentTag: integer); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  ListViewForm: TListViewForm;

implementation

{$R *.lfm}

{ TListViewForm }

class procedure TListViewForm.CreateNewForm(Table: TMyTable; CurrentTag: integer);
var
  i: integer;
begin
  Application.CreateForm(TListViewForm, ListViewForm);
  with ListViewForm do
  begin
    with SQLQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add(CreateSelect(Table));
      Open;
    end;
    for i := 0 to High(Table.FieldArray) do
    begin
      with DBGrid.Columns[i] do
      begin
        FieldName := Table.FieldArray[i].Name;
        Width := Table.FieldArray[i].Width;
        Title.Caption := Table.FieldArray[i].Caption;
        Visible := Table.FieldArray[i].Visible;
      end;
    end;
    Tag := CurrentTag;
    Caption := Table.Caption;
    Show;
  end;
end;

procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.


