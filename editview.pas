unit EditView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, MetaData,
  sqldb, DB, StdCtrls, DBGrids, DBCtrls, SQLRequest;

type

  { TEditViewForm }

  TEditViewForm = class(TForm)
    ApplyButton: TButton;
    DataSource: TDataSource;
    DynamicLabel: TLabel;
    DynamicEdit: TEdit;
    SQLQuery: TSQLQuery;
    procedure ApplyButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; DBGrid: TDBGrid;
      CurrentTag: integer); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    ObjectArray: array of TObject;
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
  i, j, k: integer;
  CaptionTmp: string;
begin
  Application.CreateForm(TEditViewForm, EditViewForm);
  j := 0;
  with EditViewForm do
  begin
    UpdateID := DBGrid.DataSource.DataSet.FieldByName(
      Table.FieldArray[0].Name).AsInteger;
    SQLQuery.Tag := CurrentTag;
    Tag := CurrentTag;
    for i := 1 to High(Table.FieldArray) do
    begin
      //if Table.FieldArray[i].Visible then
      //begin
      if Table.FieldArray[i].TypeField > 0 then
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
          if Table.FieldArray[i].TypeField = 2 then
          begin
            for k := 1 to High(Table.FieldArray) do
            begin
              if Table.FieldArray[k].Name = TMyJoinField(
                Table.FieldArray[i]).JoinFieldShow then
                Caption := Table.FieldArray[k].Caption;
            end;

          end
          else
          begin
            Caption := Table.FieldArray[i].Caption;
          end;
        end;
        SetLength(ObjectArray, Length(ObjectArray) + 1);
        if Table.FieldArray[i].TypeField = 2 then
        begin
          //ObjectArray[High(ObjectArray)] :=
          //  TEdit.Create(TEdit(ObjectArray[High(ObjectArray)]));
          //with TEdit(ObjectArray[High(ObjectArray)]) do
          //begin
          //  Parent := EditViewForm;
          //  Left := 150;
          //  Top := 35 + 35 * j;
          //  Tag := i;
          //  Width := 250;
          //  Height := 21;
          //  //Caption := Table.FieldArray[i].Caption;
          //  Caption := DBGrid.DataSource.DataSet.FieldByName(
          //    Table.FieldArray[i].Name).AsString;
          //end;

          ObjectArray[High(ObjectArray)] :=
            TComboBox.Create(TComboBox(ObjectArray[High(ObjectArray)]));

          with SQLQuery do
          begin
            Close;
            SQL.Text := 'SELECT * FROM ' + TMyJoinField(
              Table.FieldArray[i]).JoinTableName;
            Open;

            while not EOF do
            begin
              TComboBox(ObjectArray[High(ObjectArray)]).Items.AddObject(
                FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldShow).AsString,
                TObject(FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldName).AsInteger));

              if DBGrid.DataSource.DataSet.FieldByName(
                Table.FieldArray[i].Name).AsString =
                FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldName).AsString
              then
                CaptionTmp :=
                  FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldShow).AsString;

              Next;
            end;
          end;
          with TComboBox(ObjectArray[High(ObjectArray)]) do
          begin
            Parent := EditViewForm;
            ReadOnly := True;
            Style := csOwnerDrawFixed;
            Left := 150;
            Top := 35 + 35 * j;
            Tag := i;
            Width := 250;
            Height := 21;
            Caption := CaptionTmp;
            //Items[1] := CaptionTmp;
          end;
        end
        else
        begin
          ObjectArray[High(ObjectArray)] :=
            TEdit.Create(TEdit(ObjectArray[High(ObjectArray)]));
          with TEdit(ObjectArray[High(ObjectArray)]) do
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


          //ObjectArray[High(ObjectArray)] :=
          //  TComboBox.Create(TComboBox(ObjectArray[High(ObjectArray)]));

          //with SQLQuery do
          //begin
          //  Close;
          //  SQL.Text := 'SELECT * FROM ' + TMyJoinField(
          //    Table.FieldArray[i]).JoinTableName;
          //  Open;

          //  while not EOF do
          //  begin
          //    TComboBox(ObjectArray[High(ObjectArray)]).Items.AddObject(
          //      FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldShow).AsString,
          //      TObject(FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldName).AsInteger));

          //    if DBGrid.DataSource.DataSet.FieldByName(
          //      Table.FieldArray[i].Name).AsString =
          //      FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldName).AsString then
          //      CaptionTmp := FieldByName(TMyJoinField(Table.FieldArray[i]).JoinFieldShow).AsString;

          //    Next;
          //  end;
          //end;
          //with TComboBox(ObjectArray[High(ObjectArray)]) do
          //begin
          //  Parent := EditViewForm;
          //  ReadOnly := True;
          //  Style := csOwnerDrawFixed;
          //  Left := 150;
          //  Top := 35 + 35 * j;
          //  Tag := i;
          //  Width := 250;
          //  Height := 21;
          //  Caption := CaptionTmp;
          //  //Items[1] := CaptionTmp;
          //end;
        end;

        Inc(j);
        //end;
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
    for i := 0 to High(ObjectArray) do
    begin
      if ObjectArray[i].ClassName = 'TComboBox' then
        Params[i].AsInteger :=
          integer(TComboBox(ObjectArray[i]).Items.Objects[TComboBox(
          ObjectArray[i]).ItemIndex])
      else
        Params[i].AsString := TEdit(ObjectArray[i]).Text;
      //case TableArray[Tag].FieldArray[i].TypeField of
      //  1: Params[i].AsString := TEdit(ObjectArray[i]).Text;
      //2: Params[i].AsInteger :=
      //    Integer(TComboBox(ObjectArray[i]).Items.Objects[TComboBox(
      //    ObjectArray[i]).ItemIndex]);

      //end;
      //showmessage(ObjectArray[i].ClassName);
      //Params[i].AsString := TEdit(ObjectArray[i]).Text;
    end;
    ParamByName('p').AsInteger := UpdateID;
    ShowMessage(SQL.Text);
    ExecSQL;
  end;

  //with SQLQuery do
  //begin
  //  Close;
  //  SQL.Text := CreateSelect(TableArray[Tag]);
  //  ShowMessage(SQL.Text);
  //  Open;

  //  while not EOF do
  //  begin
  //    //ComboBox1.Items.Add(Fields[9].AsString);
  //    Next;
  //  end;

  //end;
end;

procedure TEditViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
