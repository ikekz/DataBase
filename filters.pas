unit Filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBCtrls, ExtCtrls, StdCtrls, sqldb, MetaData, Grids;

type

  TSort = record
    FieldName: string;
    Order: string;
  end;

  TCondition = class
    Text, Caption: string;
    Tag: integer;
    constructor Create(CurrentText, CurrentCaption: string; CurrentTag: integer);
  end;

  TFinishedFilter = class
    Field: TMyField;
    Condition: TCondition;
    Value, Operation: string;
    function CreateWhereCondition: string;

  end;

procedure FillInConditionComboBoxVarchar(ComboBox: TComboBox);
procedure FillInConditionComboBoxInteger(ComboBox: TComboBox);
procedure FillInStringGrid(StringGrid: TStringGrid; Arr: array of TFinishedFilter);

implementation

procedure FillInConditionComboBoxVarchar(ComboBox: TComboBox);
begin
  with ComboBox.Items do
  begin
    with TCondition do
    begin
      Clear;
      AddObject('Равно', Create(' = ''%s'' ', 'Равно', 1));
      AddObject('Не равно', Create(' <> ''%s'' ', 'Не равно', 2));
      AddObject('Начинается с', Create(' like ''%s%%'' ', 'Начинается с', 3));
      AddObject('Не начинается с', Create(' not like ''%s%%'' ',
        'Не начинается с', 4));
      AddObject('Содержит', Create(' like ''%%%s%%'' ', 'Содержит', 5));
      AddObject('Не содержит', Create(' not like ''%%%s%%'' ', 'Не содержит', 6));
      AddObject('Заканчивается на', Create(' like ''%%s'' ',
        'Заканчивается на', 7));
      AddObject('Не заканчивается на', Create(' not like ''%%%s'' ',
        'Не заканчивается на', 8));
    end;
  end;
  ComboBox.Text := ComboBox.Items[0];
end;

procedure FillInConditionComboBoxInteger(ComboBox: TComboBox);
begin
  with ComboBox.Items do
  begin
    with TCondition do
    begin
      Clear;
      AddObject('Равно', Create(' = %s ', 'Равно', 9));
      AddObject('Не равно', Create(' <> %s ', 'Не равно', 10));
      AddObject('Меньше', Create(' < %s ', 'Меньше', 11));
      AddObject('Больше', Create(' > %s ', 'Больше', 12));
    end;
  end;
  ComboBox.Text := ComboBox.Items[0];
end;

procedure FillInStringGrid(StringGrid: TStringGrid; Arr: array of TFinishedFilter);
var
  i: integer;
  OperationTmp: string;
begin
  with StringGrid do
  begin
    for i := 1 to RowCount - 1 do
      Rows[i].Clear;

    for i := 1 to ColCount - 1 do
      Cols[i].Clear;
    RowCount := 1;
    for i := 0 to High(Arr) do
    begin
      case Arr[i].Operation of
        ' WHERE ': OperationTmp := '';
        ' AND ': OperationTmp := 'И';
        ' OR ': OperationTmp := 'Или';
      end;
      RowCount := i + 2;
      Cells[0, i + 1] := IntToStr(i + 1);
      Cells[1, i + 1] := OperationTmp;
      Cells[2, i + 1] := Arr[i].Field.Caption;
      Cells[3, i + 1] := Arr[i].Condition.Caption;
      Cells[4, i + 1] := Arr[i].Value;
    end;
  end;
end;

function TFinishedFilter.CreateWhereCondition: string;
begin
  Result := Operation + Field.Name + ' ' + Format(Condition.Text, [Value]);
end;

constructor TCondition.Create(CurrentText, CurrentCaption: string; CurrentTag: integer);
begin
  Text := CurrentText;
  Caption := CurrentCaption;
  Tag := CurrentTag;
end;

end.
