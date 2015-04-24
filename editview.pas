unit EditView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, MetaData,
  sqldb, DB, StdCtrls, DBGrids, DBCtrls, SQLRequest, Windows, DBConnection;

type

  TFieldEdit = class(TEdit)
  public
    Field: TMyField;
  end;

  { TEditViewForm }

  TEditViewForm = class(TForm)
    ApplyButton: TButton;
    CloseButton: TButton;
    DataSource: TDataSource;
    DynamicLabel: TLabel;
    SQLQuery: TSQLQuery;
    procedure CloseButtonClick(Sender: TObject);
    class procedure CreateNewForm(Table: TMyTable; DBGrid: TDBGrid;
      CurrentTag: integer; IsUpdate: boolean); static;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ApplyUpdate(Sender: TObject);
    procedure ApplyInsert(Sender: TObject);
  private
    ObjectArray: array of TObject;
    UpdateID: integer;
    procedure CreateNewLabel(NumberItem: integer; CurrentField: TMyField;
      Table: TMyTable);
    procedure CreateNewComboBox(NumberItem: integer; CurrentField: TMyField;
      DBGrid: TDBGrid; IsUpdate: boolean);
    procedure CreateNewFieldEdit(NumberItem: integer; CurrentField: TMyField;
      DBGrid: TDBGrid; IsUpdate: boolean);
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

        CreateNewLabel(NumberItem, Table.FieldArray[i], Table);

        SetLength(ObjectArray, Length(ObjectArray) + 1);
        if Table.FieldArray[i].ClassName = 'TMyJoinField' then
          CreateNewComboBox(NumberItem, Table.FieldArray[i], DBGrid, IsUpdate)
        else
          CreateNewFieldEdit(NumberItem, Table.FieldArray[i], DBGrid, IsUpdate);

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

procedure TEditViewForm.CloseButtonClick(Sender: TObject);
begin
  EditViewForm.Close;
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

procedure TEditViewForm.CreateNewFieldEdit(NumberItem: integer;
  CurrentField: TMyField; DBGrid: TDBGrid; IsUpdate: boolean);
begin
  ObjectArray[High(ObjectArray)] :=
    TFieldEdit.Create(TFieldEdit(ObjectArray[High(ObjectArray)]));
  with TFieldEdit(ObjectArray[High(ObjectArray)]) do
  begin
    Parent := EditViewForm;
    Left := 150;
    Top := 35 + 35 * NumberItem;
    Width := 250;
    Height := 21;
    Field := CurrentField;
    if IsUpdate then
      Caption := DBGrid.DataSource.DataSet.FieldByName(CurrentField.Name).AsString;
  end;

end;

procedure TEditViewForm.CreateNewLabel(NumberItem: integer;
  CurrentField: TMyField; Table: TMyTable);
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
        Params[i].AsString := TFieldEdit(ObjectArray[i]).Text;
      //case TableArray[Tag].FieldArray[i].TypeField of
      //  1: Params[i].AsString := TFieldEdit(ObjectArray[i]).Text;
      //2: Params[i].AsInteger :=
      //    Integer(TComboBox(ObjectArray[i]).Items.Objects[TComboBox(
      //    ObjectArray[i]).ItemIndex]);

      //end;
      //showmessage(ObjectArray[i].ClassName);
      //Params[i].AsString := TFieldEdit(ObjectArray[i]).Text;
    end;
    ParamByName('p').AsInteger := UpdateID;
    //ShowMessage(SQL.Text);
    ExecSQL;
    DBConnectionForm.SQLTransaction.Commit;
  end;
  EditViewForm.Close;
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
  i, MaxID, x, ReturnCode: integer;
begin
  //if MessageDlg('Добавить запись?', mtConfirmation, mbYesNo, 0) = mrYes then
  //if MessageBox(Handle, PChar('Добавить запись?'), PChar('Подтверждение'),
  //  MB_ICONQUESTION + MB_YESNO) = mrYes then
  //if Application.MessageBox(PChar('Добавить запись?'), PChar('Подтверждение'),
  //  MB_ICONQUESTION + MB_YESNO) = mrYes then
  //begin
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
      begin
        with TFieldEdit(ObjectArray[i]) do
        begin
          if Field.DataType = 'Integer' then
          begin
            Val(Text, x, ReturnCode);
            if ReturnCode <> 0 then
            begin
              ShowMessage('Введите допустимое значение в поле "' +
                Field.Caption + '"');
              //if MessageDlg('Добавить запись?', mtConfirmation, mbYesNo, 0) = mrYes then
              exit;
            end;
            Params[i + 1].AsInteger := StrToInt(Text);
          end
          else
          begin
            Params[i + 1].AsString := Text;
          end;
          //showmessage(TFieldEdit(ObjectArray[i]).Text);
          //try
          //  Params[i + 1].AsString := TFieldEdit(ObjectArray[i]).Text;
          //except
          //  Params[i + 1].AsInteger := TFieldEdit(ObjectArray[i]).Text;
          //end;
          //Params[i + 1].AsString := TFieldEdit(ObjectArray[i]).Text;
        end;

      end;

    end;
    //ParamByName('p').AsInteger := UpdateID;
    //ShowMessage(SQL.Text);
    ExecSQL;
    DBConnectionForm.SQLTransaction.Commit;
  end;
  EditViewForm.Close;
  //end;
end;

procedure TEditViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.
