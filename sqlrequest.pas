unit SQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData;

function CreateSelect(Table: TMyTable): string;

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

end.

