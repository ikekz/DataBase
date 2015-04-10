unit SQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData, Dialogs, Filters;

function CreateSelect(Table: TMyTable): string;
function CreateFilter(Arr: array of TFinishedFilter): string;

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
      Result += OutSelectField;
    end;
  end;
  Result += ' FROM ' + Table.Name;
  for i := 0 to High(Table.FieldArray) do
  begin
    with Table.FieldArray[i] do
    begin
      Result += OutJoin(Table.Name);
    end;
  end;
end;

function CreateFilter(Arr: array of TFinishedFilter): string;
var
  i: integer;
begin
  for i := 0 to High(Arr) do
   Result += Arr[i].CreateWhereCondition;
end;


end.

