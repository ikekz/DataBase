unit Filters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBCtrls, ExtCtrls, StdCtrls, sqldb, MetaData;

type

  TFilter = class
    Text: string;
    Tag: integer;
    constructor Create(CurrentText: string; CurrentTag: integer);
  end;

procedure FillInFilterComboBoxVarchar(ComboBox: TComboBox);
procedure FillInFilterComboBoxInteger(ComboBox: TComboBox);

implementation

procedure FillInFilterComboBoxVarchar(ComboBox: TComboBox);
begin
  with ComboBox.Items do
  begin
    Clear;
    AddObject('Равно', TFilter.Create(' = :param ', 1));
    AddObject('Не равно', TFilter.Create(' <> :param ', 2));
    AddObject('Начинается с', TFilter.Create(' like :param || ''%'' ', 3));
    AddObject('Не начинается с', TFilter.Create(' not like :param || ''%'' ', 4));
    AddObject('Содержит', TFilter.Create(' like ''%'' || :param || ''%'' ', 5));
    AddObject('Не содержит', TFilter.Create(' not like ''%'' || :param || ''%'' ', 6));
    AddObject('Заканчивается на', TFilter.Create(' like ''%'' || :param ', 7));
    AddObject('Не заканчивается на', TFilter.Create(' not like ''%'' || :param ', 8));
  end;
end;

procedure FillInFilterComboBoxInteger(ComboBox: TComboBox);
begin
  with ComboBox.Items do
  begin
    Clear;
    AddObject('Равно', TFilter.Create(' = :param ', 1));
    AddObject('Не равно', TFilter.Create(' <> :param ', 2));
    AddObject('Меньше', TFilter.Create(' < :param ', 3));
    AddObject('Больше', TFilter.Create(' > :param ', 4));
  end;
end;

constructor TFilter.Create(CurrentText: string; CurrentTag: integer);
begin
  Text := CurrentText;
  Tag := CurrentTag;
end;

end.

