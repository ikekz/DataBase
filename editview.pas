unit EditView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, MetaData,
  sqldb, StdCtrls, DBGrids, SQLRequest;

type

  { TEditViewForm }

  TEditViewForm = class(TForm)

    ApplyButton: TButton;
    DynamicLabel: TLabel;
    DynamicEdit: TEdit;
    SQLQuery: TSQLQuery;
    procedure ApplyButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; DBGrid: TDBGrid;
      CurrentTag: integer); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    EditArray: array of TEdit;
    UpdateID: integer;
  public
    { public declarations }
  end;

var
  EditViewForm: TEditViewForm;

implementation

{$R *.lfm}

{ TEditViewForm }

class procedure TEditViewForm.CreateNewForm(Table: TMyTable; DBGrid: TDBGrid;
  CurrentTag: integer);
var
  i, j: integer;
begin
  Application.CreateForm(TEditViewForm, EditViewForm);
  j := 0;
  with EditViewForm do
  begin
    UpdateID := DBGrid.DataSource.DataSet.FieldByName(
      Table.FieldArray[0].Name).AsInteger;
    SQLQuery.Tag := CurrentTag;
    for i := 0 to High(Table.FieldArray) do
    begin
      Tag := CurrentTag;
      if Table.FieldArray[i].Visible then
      begin
        DynamicLabel := TLabel.Create(DynamicLabel);
        with DynamicLabel do
        begin
          Parent := EditViewForm;
          Left := 10;
          Top := 35 + 35 * j;
          Tag := i;
          Width := 140;
          Height := 21;
          Caption := Table.FieldArray[i].Caption;
        end;
        SetLength(EditArray, Length(EditArray) + 1);
        EditArray[High(EditArray)] := TEdit.Create(EditArray[High(EditArray)]);
        with EditArray[High(EditArray)] do
        begin
          Parent := EditViewForm;
          Left := 150;
          Top := 35 + 35 * j;
          Tag := i;
          Width := 250;
          Height := 21;
          //Caption := Table.FieldArray[i].Caption;
          Caption := DBGrid.DataSource.DataSet.FieldByName(
            Table.FieldArray[i].Name).AsString;
        end;
        Inc(j);
      end;
    end;
    ShowModal;
    //ModalResult := 1;
  end;

end;

procedure TEditViewForm.ApplyButtonClick(Sender: TObject);
var
  i: integer;
begin
  //SQLQuery := CreateUpdate(Table: TMyTable);
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateUpdate(TableArray[Tag]);
    if not Prepared then
      Prepare;
    for i := 0 to High(EditArray) do
      Params[i].AsString := EditArray[i].Text;
    ParamByName('p').AsInteger := UpdateID;
    ShowMessage(SQL.Text);
    Open;
  end;

end;

procedure TEditViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
