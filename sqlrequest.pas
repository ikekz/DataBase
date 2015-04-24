unit SQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData, Dialogs, FilterAndSort;

function CreateSelect(Table: TMyTable): string;
function CreateFilter(Arr: array of TFinishedFilter): string;
function CreateSort(Arr: array of Sort): string;
function CreateUpdate(Table: TMyTable): string;

implementation

function CreateSelect(Table: TMyTable): string;
var
  i: integer;
begin
  Result := 'SELECT ' + Table.Name + '.* ';
  for i := 0 to High(Table.FieldArray) do
  begin
    with Table.FieldArray[i] do
    begin
      Result += MakeSelectField;
    end;
  end;
  Result += ' FROM ' + Table.Name;
  for i := 0 to High(Table.FieldArray) do
  begin
    with Table.FieldArray[i] do
    begin
      Result += MakeJoin(Table.Name);
    end;
  end;
end;

function CreateSort(Arr: array of Sort): string;
var
  i: integer;
begin
  if Length(Arr) > 0 then
  begin
    Result := ' ORDER BY ';
    for i := 0 to High(Arr) do
      Result += Arr[i].FieldName + ' ' + Arr[i].Order + ', ';
    Delete(Result, Length(Result) - 1, 2);

  end;
end;

function CreateFilter(Arr: array of TFinishedFilter): string;
var
  i: integer;
begin
  for i := 0 to High(Arr) do
  begin
    if Arr[i].IsApply then
      Result += Arr[i].Operation + Arr[i].Field.Name + ' ' +
        Format(Arr[i].Condition.Text, [i]);
  end;
end;

function CreateUpdate(Table: TMyTable): string;
var
  i: integer;
begin
  Result := 'UPDATE ' + Table.Name + ' SET ';
  for i := 1 to High(Table.FieldArray) do
  begin
    if Table.FieldArray[i].ClassName <> 'TMyShowField' then
      Result += Table.FieldArray[i].Name + ' = :param' + IntToStr(i) + ', ';
  end;
  Delete(Result, Length(Result) - 1, 1);
  Result += 'WHERE ' + Table.FieldArray[0].Name + ' = :p';
end;

end.


