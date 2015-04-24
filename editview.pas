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
    class procedure CreateNewForm(Table: TMyTable; DBGrid: TDBGrid;
      CurrentTag: integer; IsUpdate: boolean); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ApplyUpdate(Sender: TObject);
    procedure ApplyInsert(Sender: TObject);
  private
    ObjectArray: array of TObject;
    UpdateID: integer;
    procedure CreateNewLabel(NumberItem: integer; CurrentField: TMyField;
      Table: TMyTable; IsUpdate: boolean);
    procedure CreateNewComboBox(NumberItem: integer; CurrentField: TMyField;
      DBGrid: TDBGrid; IsUpdate: boolean);
    procedure CreateNewEdit(NumberItem: integer; CurrentField: TMyField;
      DBGrid: TDBGrid; IsUpdate: boolean);
  public
    { public declarations }
  end;

var
  EditViewForm: TEditViewForm;

implementation

{$R *.lfm}

{ TEditViewForm }

class procedure TEditViewForm.CreateNewForm(Table: TMyTable; DBGrid: TDBGrid;
  CurrentTag: integer; IsUpdate: boolean);
var
  i, NumberItem: integer;
begin
  Application.CreateForm(TEditViewForm, EditViewForm);
  NumberItem := 0;
  with EditViewForm do
  begin
    UpdateID := DBGrid.DataSource.DataSet.FieldByName(
      Table.FieldArray[0].Name).AsInteger;
    SQLQuery.Tag := CurrentTag;
    Tag := CurrentTag;
    for i := 1 to High(Table.FieldArray) do
    begin
      if Table.FieldArray[i].ClassName <> 'TMyShowField' then
      begin

        CreateNewLabel(NumberItem, Table.FieldArray[i], Table, IsUpdate);

        SetLength(ObjectArray, Length(ObjectArray) + 1);
        if Table.FieldArray[i].ClassName = 'TMyJoinField' then
          CreateNewComboBox(NumberItem, Table.FieldArray[i], DBGrid, IsUpdate)
        else
          CreateNewEdit(NumberItem, Table.FieldArray[i], DBGrid, IsUpdate);

        Inc(NumberItem);
        //end;
      end;

    end;
    if IsUpdate then
      ApplyButton.OnClick := @ApplyUpdate
    else
      ApplyButton.OnClick := @ApplyInsert;
    ShowModal;
    //ModalResult := 1;
  end;
end;

procedure TEditViewForm.CreateNewComboBox(NumberItem: integer;
  CurrentField: TMyField; DBGrid: TDBGrid; IsUpdate: boolean);
var
  CaptionTmp: string;
begin
  ObjectArray[High(ObjectArray)] :=
    TComboBox.Create(TComboBox(ObjectArray[High(ObjectArray)]));
  with SQLQuery do
  begin
    Close;
    SQL.Text := 'SELECT * FROM ' + TMyJoinField(CurrentField).JoinTableName;
    Open;

    while not EOF do
    begin
      TComboBox(ObjectArray[High(ObjectArray)]).Items.AddObject(
        FieldByName(TMyJoinField(CurrentField).JoinFieldShow).AsString,
        TObject(FieldByName(TMyJoinField(CurrentField).JoinFieldName).AsInteger));
      if IsUpdate then
      begin
        if DBGrid.DataSource.DataSet.FieldByName(CurrentField.Name).AsString =
          FieldByName(TMyJoinField(CurrentField).JoinFieldName).AsString then
        begin
          CaptionTmp :=
            FieldByName(TMyJoinField(CurrentField).JoinFieldShow).AsString;
        end;

      end;

      Next;
    end;
  end;
  with TComboBox(ObjectArray[High(ObjectArray)]) do
  begin
    Parent := EditViewForm;
    ReadOnly := True;
    Style := csOwnerDrawFixed;
    Left := 150;
    Top := 35 + 35 * NumberItem;
    Width := 250;
    Height := 21;
    if IsUpdate then
      Caption := CaptionTmp;
  end;
end;

procedure TEditViewForm.CreateNewEdit(NumberItem: integer; CurrentField: TMyField;
  DBGrid: TDBGrid; IsUpdate: boolean);
begin
  ObjectArray[High(ObjectArray)] :=
    TEdit.Create(TEdit(ObjectArray[High(ObjectArray)]));
  with TEdit(ObjectArray[High(ObjectArray)]) do
  begin
    Parent := EditViewForm;
    Left := 150;
    Top := 35 + 35 * NumberItem;
    Width := 250;
    Height := 21;
    if IsUpdate then
      Caption := DBGrid.DataSource.DataSet.FieldByName(CurrentField.Name).AsString;
  end;

end;

procedure TEditViewForm.CreateNewLabel(NumberItem: integer;
  CurrentField: TMyField; Table: TMyTable; IsUpdate: boolean);
var
  i: integer;
begin
  DynamicLabel := TLabel.Create(DynamicLabel);
  with DynamicLabel do
  begin
    Parent := EditViewForm;
    Left := 10;
    Top := 35 + 35 * NumberItem;
    Width := 140;
    Height := 21;
    if CurrentField.ClassName = 'TMyJoinField' then
    begin
      for i := 1 to High(Table.FieldArray) do
      begin
        if Table.FieldArray[i].Name = TMyJoinField(CurrentField).JoinFieldShow then
          Caption := Table.FieldArray[i].Caption;
      end;
    end
    else
    begin
      Caption := CurrentField.Caption;
    end;
  end;
end;

procedure TEditViewForm.ApplyUpdate(Sender: TObject);
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
    //ShowMessage(SQL.Text);
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

procedure TEditViewForm.ApplyInsert(Sender: TObject);
var
  i, MaxID: integer;
begin

  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateMaxID(TableArray[Tag]);
    Open;
    MaxID := FieldByName('MAX').AsInteger;
    //ShowMessage(SQL.Text);
    //ShowMessage(inttostr(MaxID));

    Close;
    SQL.Text := CreateInsert(TableArray[Tag]);
    if not Prepared then
      Prepare;
    Params[0].AsInteger := MaxID + 1;
    //ShowMessage(SQL.Text);
    for i := 0 to High(ObjectArray) do
    begin
      if ObjectArray[i].ClassName = 'TComboBox' then
        Params[i + 1].AsInteger :=
          integer(TComboBox(ObjectArray[i]).Items.Objects[TComboBox(
          ObjectArray[i]).ItemIndex])
      else
        Params[i + 1].AsString := TEdit(ObjectArray[i]).Text;

    end;
    //ParamByName('p').AsInteger := UpdateID;
    //ShowMessage(SQL.Text);
    ExecSQL;
  end;

end;

procedure TEditViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
