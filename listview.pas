unit ListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  DBCtrls, ExtCtrls, StdCtrls, Grids, Menus, sqldb, DB, MetaData, SQLRequest,
  Filters;

type

  { TListViewForm }

  TListViewForm = class(TForm)
    ApplyFilterButton: TButton;
    AddFilterButton: TButton;
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
    MainMenu: TMainMenu;
    DataMenu: TMenuItem;
    FilterMenu: TMenuItem;
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
    procedure FilterStringGridDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure TurnFilterMenuClick(Sender: TObject);
  private
    FinishedFilterArray: array of TFinishedFilter;
    SortArray: array of TSort;
    procedure AddFinishedFilter(CurrentField: TMyField;
      CurrentCondition: TCondition; CurrentValue, CurrentOperation: string);
    procedure AddSort(CurrentFieldName, CurrentOrder: string);
    procedure FillInDBGrid(Table: TMyTable; Grid: TDBGrid);
    procedure FillInFieldComboBox(Table: TMyTable; ComboBox: TComboBox);
    procedure SelectTypeCondition;
    procedure AddFilter;
    //class var
    //FinishedFilterArray: array of TFinishedFilter;
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
    Tag := CurrentTag;
    Caption := Table.Caption;
    Show;
  end;
end;

procedure TListViewForm.DBGridTitleClick(Column: TColumn);
var
  i, j, Index: integer;
  IsFind: boolean;
begin
  //ShowMessage(inttostr(Column.Index));
  //ShowMessage(TableArray[Tag].Name);
  //ShowMessage(inttostr(Tag));
  //ShowMessage(TableArray[Tag].FieldArray[Column.Index].Name);
  IsFind := False;
  for i := 0 to High(SortArray) do
  begin
    if SortArray[i].FieldName = TableArray[Tag].FieldArray[Column.Index].Name then
    begin
      IsFind := True;
      if SortArray[i].Order = '' then
        SortArray[i].Order := ' desc '
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
  //Index := Column.Index;
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[Tag]) + CreateSort(SortArray);
    //ShowMessage(SQL.Text);
    //Params.Items[1].AsString :=
    //  TableArray[Tag].FieldArray[i].Name;
    //ShowMessage(inttostr(Tag));
    //ShowMessage(TableArray[Tag].FieldArray[Column.Index].Name);
    //ShowMessage(TableArray[Tag].FieldArray[i].Name);
    //ShowMessage(SQL.Text);
    //Prepare;
    //if not Prepared then
    //  Prepare;

    //ShowMessage(TableArray[Tag].FieldArray[i].Name);
    //ParamByName('param').AsString :=
    //  TableArray[Tag].FieldArray[i].Name;
    //Params.Items[1].AsString :=
    //  TableArray[Tag].FieldArray[i].Name;
    ShowMessage(SQL.Text);
    //Close;
    Open;
  end;
  FillInDBGrid(TableArray[Tag], DBGrid);
end;

procedure TListViewForm.DeleteAllFilterButtonClick(Sender: TObject);
begin
  SetLength(FinishedFilterArray, 0);
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[Tag]);
    Open;
  end;
  FillInDBGrid(TableArray[Tag], DBGrid);
end;

procedure TListViewForm.AddFinishedFilter(CurrentField: TMyField;
  CurrentCondition: TCondition; CurrentValue, CurrentOperation: string);
begin
  SetLength(FinishedFilterArray, Length(FinishedFilterArray) + 1);
  if Length(FinishedFilterArray) = 1 then
    CurrentOperation := ' WHERE ';
  FinishedFilterArray[High(FinishedFilterArray)] := TFinishedFilter.Create;
  FinishedFilterArray[High(FinishedFilterArray)].Field := CurrentField;
  FinishedFilterArray[High(FinishedFilterArray)].Condition := CurrentCondition;
  FinishedFilterArray[High(FinishedFilterArray)].Value := CurrentValue;
  FinishedFilterArray[High(FinishedFilterArray)].Operation := CurrentOperation;
  //Result := TableArray[High(TableArray)];
end;

procedure TListViewForm.AddSort(CurrentFieldName, CurrentOrder: string);
begin
  SetLength(SortArray, Length(SortArray) + 1);
  //if Length(FinishedFilterArray) = 1 then
  //  CurrentOperation := ' WHERE ';
  //SortArray[High(SortArray)] := TSort.Create;
  SortArray[High(SortArray)].FieldName := CurrentFieldName;
  SortArray[High(SortArray)].Order := CurrentOrder;
  //FinishedFilterArray[High(FinishedFilterArray)].Value := CurrentValue;
  //FinishedFilterArray[High(FinishedFilterArray)].Operation := CurrentOperation;
  //Result := TableArray[High(TableArray)];
end;

procedure TListViewForm.SelectTypeCondition;
begin
  if TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).DataType =
    'Varchar' then
    FillInConditionComboBoxVarchar(ConditionComboBox)
  else
    FillInConditionComboBoxInteger(ConditionComboBox);
end;

procedure TListViewForm.AddFilter;
var
  x, ReturnCode: integer;
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
  if length(FinishedFilterArray) > 0 then
  begin
    with FinishedFilterArray[High(FinishedFilterArray)] do
    begin
      if (Field.Caption = FieldComboBox.Text) and
        (Condition.Caption = ConditionComboBox.Text) and
        (Value = FilterValueEdit.Text) then
        exit;
    end;
  end;

  AddFinishedFilter(
    TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]),
    TCondition(ConditionComboBox.Items.Objects[ConditionComboBox.ItemIndex]),
    FilterValueEdit.Text, 'AND ');
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
end;


procedure TListViewForm.FillInDBGrid(Table: TMyTable; Grid: TDBGrid);
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
//var
//  Panel: TPanel;
begin
  //Panel := TPanel.Create(ListViewForm);
  //Panel.Show;

  AddFilter;
  //with FinishedFilterArray[High(FinishedFilterArray)] do
  //begin
  //  if (Field.Caption = FieldComboBox.Text) and
  //    (Condition.Caption = ConditionComboBox.Text) and
  //    (Value = FilterValueEdit.Text) and (Operation = 'AND ') then
  //    exit;
  //end;



  //AddFinishedFilter(
  //  TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]),
  //  TCondition(ConditionComboBox.Items.Objects[ConditionComboBox.ItemIndex]),
  //  FilterValueEdit.Text, 'AND ');
  //FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  //ShowMessage(TableArray[Tag].Name);
  with SQLQuery do
  begin
    Close;
    //ShowMessage(TableArray[Tag].Name);
    //SQL.Text := CreateSelect(TableArray[TButton(Sender).Parent.Parent.Tag]) +
    //  CreateFilter(TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]),
    //  FilterValueEdit.Text, TCondition(
    //  ConditionComboBox.Items.Objects[ConditionComboBox.ItemIndex]).Text);
    SQL.Text := CreateSelect(TableArray[Tag]) + CreateFilter(FinishedFilterArray);
    //Prepare;
    ////showmessage(SQL.Text);
    //ParamByName('param').AsString := FilterValueEdit.Text;
    //Params.Items[1].AsString := i;
    //ShowMessage(SQL.Text);
    Open;
  end;
  FillInDBGrid(TableArray[Tag], DBGrid);

  //FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  //SQLQuery.SQL.Text := CreateSelect(TableArray[TButton(Sender).Parent.Parent.Tag]) +
  //  CreateFilter(TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]));
  //Showmessage(TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).Caption);
end;

procedure TListViewForm.AddFilterButtonClick(Sender: TObject);
begin
  ////if length(FinishedFilterArray) > 0 then
  //begin
  //  with FinishedFilterArray[High(FinishedFilterArray)] do
  //  begin
  //    if (Field.Caption = FieldComboBox.Text) and
  //      (Condition.Caption = ConditionComboBox.Text) and
  //      (Value = FilterValueEdit.Text) and (Operation = 'AND ') then
  //      exit;
  //  end;
  //end;
  //AddFinishedFilter(
  //  TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]),
  //  TCondition(ConditionComboBox.Items.Objects[ConditionComboBox.ItemIndex]),
  //  FilterValueEdit.Text, 'AND ');
  //FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  AddFilter;
end;

procedure TListViewForm.FieldComboBoxChange(Sender: TObject);
begin
  //showmessage(TMyField(FieldComboBox.Items.Objects[FieldComboBox.ItemIndex]).DataType);
  SelectTypeCondition;
  //MenuItem := TMenuItem.Create(DirectoryMenu);
  //FieldComboBox.Items[i] := DBGrid.Columns[i].FieldName;
  //DirectoryMenu.Add(MenuItem);
  //MenuItem.Tag := i;
  //MenuItem.Caption := TableArray[i].Caption;
  //MenuItem.OnClick := @DirectoryMenuItemClick;

end;

procedure TListViewForm.ShowFilterMenuClick(Sender: TObject);
begin
  CreateFilterPanel.Visible := True;
  FilterStringGrid.Visible := True;
  //DBGrid.anchor;
end;

procedure TListViewForm.FilterStringGridDblClick(Sender: TObject);
var
  i: integer;
begin
  //showmessage(inttostr(FilterStringGrid.Row));
  //FilterStringGrid.Rows[FilterStringGrid.Row].Clear;
  //FilterStringGrid.sele;
  if Length(FinishedFilterArray) = 0 then
    exit;

  //showmessage(inttostr(FilterStringGrid.Row));
  //ShowMessage(FinishedFilterArray[FilterStringGrid.Row - 1].Condition.Caption);
  //ShowMessage(inttostr(FilterStringGrid.Row));
  for i := FilterStringGrid.Row - 1 to High(FinishedFilterArray) - 1 do
  begin
    FinishedFilterArray[i] := FinishedFilterArray[i + 1];
  end;
  SetLength(FinishedFilterArray, Length(FinishedFilterArray) - 1);
  if Length(FinishedFilterArray) > 0 then
  begin
    FinishedFilterArray[0].Operation := ' WHERE ';
  end;
  with SQLQuery do
  begin
    Close;
    SQL.Text := CreateSelect(TableArray[Tag]) + CreateFilter(FinishedFilterArray);
    //ShowMessage(SQL.Text);
    Open;
  end;
  FillInStringGrid(FilterStringGrid, FinishedFilterArray);
  //FilterStringGrid.DeleteRow(FilterStringGrid.Row);
  FillInDBGrid(TableArray[Tag], DBGrid);
end;

procedure TListViewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TListViewForm.TurnFilterMenuClick(Sender: TObject);
begin
  CreateFilterPanel.Visible := False;
  FilterStringGrid.Visible := False;
end;

end.
