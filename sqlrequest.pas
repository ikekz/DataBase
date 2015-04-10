unit SQLRequest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MetaData, Dialogs, Filters;

function CreateSelect(Table: TMyTable): string;
//function CreateSort(Table: TMyTable): string;
function CreateFilter(Arr: array of TFinishedFilter): string;
function CreateSort(Arr: array of TSort): string;

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

function CreateSort(Arr: array of TSort): string;
var
  i: integer;
begin
  //Result := ' ORDER BY :param ';
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
  //showmessage(inttostr(length(Arr)));
  for i := 0 to High(Arr) do
    Result += Arr[i].CreateWhereCondition;
  //Result += Arr[i + 1].CreateWhereCondition;
  //Result := Format(' WHERE %s %s ', [Field.Name, Filter]);
  //Result := 'SELECT * FROM country.db where capital like :param1';
  //Query1.Prepare;
  //Query1.ParamByName('param1').AsString:='M%';
  //Query1.Open;
end;


//function CreateSort(Table: TMyTable): string;
//begin

//end;

end.

