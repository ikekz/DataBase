unit MetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  TMyField = class
    Name, Caption: string;
    Width: integer;
  end;

  TMyTable = class
    Name, Caption: string;
    FieldArray: array of TMyField;
  end;


var
  TableArray: array of TMyTable;

implementation

procedure AddTable(CurrentName, CurrentCaption: string);
begin
  SetLength(TableArray, Length(TableArray) + 1);
  TableArray[High(TableArray)] := TMyTable.Create;
  TableArray[High(TableArray)].Name := CurrentName;
  TableArray[High(TableArray)].Caption := CurrentCaption;
end;

procedure AddField(CurrentName, CurrentCaption: string; CurrentWidth: integer);
begin
  SetLength(TableArray[High(TableArray)].FieldArray,
    Length(TableArray[High(TableArray)].FieldArray) + 1);
  with TableArray[High(TableArray)] do
  begin
    FieldArray[High(TableArray[High(TableArray)].FieldArray)] := TMyField.Create;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Name := CurrentName;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Caption := CurrentCaption;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Width := CurrentWidth;
  end;
end;

initialization

  AddTable('Teachers', 'Учителя');
  AddField('TeacherID', 'ID', 30);
  AddField('TeacherInitials', 'Фамилия имя отчество', 230);
  AddTable('Groups', 'Группы');
  AddField('GroupID', 'ID', 30);
  AddField('GroupNumber', 'Номер группы', 100);
  AddField('GroupName', 'Название группы', 250);
  AddTable('Students', 'Студенты');
  AddField('StudentID', 'ID', 30);
  AddField('StudentInitials', 'Фамилия имя отчество', 230);
  AddField('GroupID', 'ID группы', 70);
  AddTable('Subjects', 'Предметы');
  AddField('SubjectID', 'ID', 30);
  AddField('SubjectName', 'Название предмета', 230);
  AddTable('Audiences', 'Аудитории');
  AddField('AudienceID', 'ID', 30);
  AddField('AudienceNumber', 'Номер аудитории', 120);
  AddTable('Pairs', 'Занятия');
  AddField('PairID', 'ID', 30);
  AddField('PairBegin', 'Начало занятия', 100);
  AddField('PairEnd', 'Конец занятия', 100);
  AddField('PairNumber', 'Номер занятия', 100);
  AddTable('Schedules', 'Расписание');
  AddField('GroupID', 'ID группы', 80);
  AddField('WeekDayID', 'ID дня', 80);
  AddField('PairID', 'ID занятия', 80);
  AddField('SubjectID', 'ID предмета', 80);
  AddField('EducID', 'ID вида занятия', 80);
  AddField('TeacherID', 'ID учителя', 80);
  AddField('AudienceID', 'ID аудитории', 80);
  AddTable('Teachers_Subjects', 'Предметы учитиелей');
  AddField('TeacherID', 'ID учителя', 80);
  AddField('SubjectID', 'ID предмета', 80);
  AddTable('Group_Subjects', 'Предметы групп');
  AddField('GroupID', 'ID группы', 80);
  AddField('SubjectID', 'ID предмета', 80);
  AddTable('WeekDays', 'Дни недели');
  AddField('WeekDayID', 'ID', 80);
  AddField('WeekDayName', 'День недели', 80);
  AddField('WeekDayNumber', 'Номер дня недели', 120);
  AddTable('EducActivities', 'Вид занятия');
  AddField('EducID', 'ID', 30);
  AddField('EducName', 'Вид занятия', 80);

end.
