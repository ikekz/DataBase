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
    class procedure CreateNewForm(Table: TMyTable); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
    { private declarations }
  public
    { public declarations }
  end;

var
  ListViewForm: TListViewForm;

implementation

{$R *.lfm}

{ TListViewForm }

class procedure TListViewForm.CreateNewForm(Table: TMyTable);
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
      SQL.Add('SELECT * FROM ' + Table.Name);
      Open;
    end;
    with DBGrid do
    begin
      for i := 0 to High(Table.FieldArray) do
      begin
        Columns[i].Width := Table.FieldArray[i].Width;
        Columns[i].Title.Caption := Table.FieldArray[i].Caption;
      end;
    end;
    Caption := Table.Caption;
    Show;
  end;
end;

procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.


