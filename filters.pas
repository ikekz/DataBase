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
    //class procedure AddFinishedFilter(CurrentField: TMyField;
    //  CurrentCondition: TCondition; CurrentValue: string); static;

  end;

procedure FillInConditionComboBoxVarchar(ComboBox: TComboBox);
procedure FillInConditionComboBoxInteger(ComboBox: TComboBox);
procedure FillInStringGrid(StringGrid: TStringGrid; Arr: array of TFinishedFilter);

//FinishedFilterArray: array of TFinishedFilter;

implementation

procedure FillInConditionComboBoxVarchar(ComboBox: TComboBox);
begin
  //with ComboBox.Items do
  //begin
  //  with TCondition do
  //  begin
  //    Clear;
  //    AddObject('Равно', Create(' = :param ', 'Равно', 1));
  //    AddObject('Не равно', Create(' <> :param ', 'Не равно', 2));
  //    AddObject('Начинается с', Create(' like :param || ''%'' ', 'Начинается с', 3));
  //    AddObject('Не начинается с', Create(' not like :param || ''%'' ',
  //      'Не начинается с', 4));
  //    AddObject('Содержит', Create(' like ''%'' || :param || ''%'' ', 'Содержит', 5));
  //    AddObject('Не содержит', Create(' not like ''%'' || :param || ''%'' ',
  //      'Не содержит', 6));
  //    AddObject('Заканчивается на', Create(' like ''%'' || :param ',
  //      'Заканчивается на', 7));
  //    AddObject('Не заканчивается на', Create(' not like ''%'' || :param ',
  //      'Не заканчивается на', 8));
  //  end;
  //end;
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
  //with ComboBox.Items do
  //begin
  //  with TCondition do
  //  begin
  //    Clear;
  //    AddObject('Равно', Create(' = :param ', 'Равно', 9));
  //    AddObject('Не равно', Create(' <> :param ', 'Не равно', 10));
  //    AddObject('Меньше', Create(' < :param ', 'Меньше', 11));
  //    AddObject('Больше', Create(' > :param ', 'Больше', 12));
  //  end;
  //end;
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
begin
  //ListBox.Items.Add('Первая колонка'^I'Вторая колонка'^I'Третья колонка') ;
  //ListBox.Items.Add('Строка№2-1'^I'Строка№2-2'^I'Строка№2-3') ;
  //ListBox.Items.Add('Строка№3-1'^I'Строка№3-2'^I'Строка№3-3') ;

  //for Col:=0 to StringGrid.ColCount-1 do
  // for Row:=0 to StringGrid.RowCount-1 do
  //    StringGrid.Cells[Col,Row] := '';

  //StringGrid.Columns.Add;
  //StringGrid.Columns.Items[1].Title := 'Поле';
  //StringGrid.Columns.Add;
  //StringGrid.Columns.Items[2].Title := 'Условие';
  //StringGrid.Columns.Add;
  //StringGrid.Columns.Items[3].Title := 'Значение';
  with StringGrid do
  begin
    for i := 1 to RowCount - 1 do
      Rows[i].Clear;

    for i := 1 to ColCount - 1 do
      Cols[i].Clear;
    RowCount := 1;
    for i := 0 to High(Arr) do
    begin
      RowCount := i + 2;
      Cells[1, i + 1] := IntToStr(i + 1);
      Cells[2, i + 1] := Arr[i].Field.Caption;
      Cells[3, i + 1] := Arr[i].Condition.Caption;
      Cells[4, i + 1] := Arr[i].Value;
    end;
  end;
end;

function TFinishedFilter.CreateWhereCondition: string;
begin
  //showmessage(Condition.Text);
  Result := Operation + Field.Name + ' ' + Format(Condition.Text, [Value]);
end;

//class procedure TFinishedFilter.AddFinishedFilter(CurrentField: TMyField;
//  CurrentCondition: TCondition; CurrentValue: string);
//begin
//  SetLength(FinishedFilterArray, Length(FinishedFilterArray) + 1);
//  FinishedFilterArray[High(FinishedFilterArray)] := TFinishedFilter.Create;
//  FinishedFilterArray[High(FinishedFilterArray)].Field := CurrentField;
//  FinishedFilterArray[High(FinishedFilterArray)].Condition := CurrentCondition;
//  FinishedFilterArray[High(FinishedFilterArray)].Value := CurrentValue;
//  //Result := TableArray[High(TableArray)];
//end;

constructor TCondition.Create(CurrentText, CurrentCaption: string; CurrentTag: integer);
begin
  Text := CurrentText;
  Caption := CurrentCaption;
  Tag := CurrentTag;
end;

end.
