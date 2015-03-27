unit MetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  TMyField = class
    Name, Caption: string;
    Width: integer;
    Visible: boolean;
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

procedure AddField(CurrentName, CurrentCaption: string; CurrentWidth: integer;
  CurrentVisible: boolean);
begin
  with TableArray[High(TableArray)] do
  begin
    SetLength(FieldArray, Length(FieldArray) + 1);
    FieldArray[High(TableArray[High(TableArray)].FieldArray)] := TMyField.Create;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Name := CurrentName;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Caption := CurrentCaption;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Width := CurrentWidth;
    FieldArray[High(TableArray[High(TableArray)].FieldArray)].Visible := CurrentVisible;
  end;
end;

initialization

  AddTable('Teachers', 'Учителя');
  AddField('TeacherID', 'ID', 30, False);
  AddField('TeacherInitials', 'Фамилия имя отчество', 230, True);

  AddTable('Groups', 'Группы');
  AddField('GroupID', 'ID', 30, False);
  AddField('GroupNumber', 'Номер группы', 100, True);
  AddField('GroupName', 'Название группы', 250, True);

  AddTable('Students', 'Студенты');
  AddField('StudentID', 'ID', 30, False);
  AddField('StudentInitials', 'Фамилия имя отчество', 230, True);
  AddField('GroupID', 'ID группы', 70, True);

  AddTable('Subjects', 'Предметы');
  AddField('SubjectID', 'ID', 30, False);
  AddField('SubjectName', 'Название предмета', 230, True);

  AddTable('Audiences', 'Аудитории');
  AddField('AudienceID', 'ID', 30, False);
  AddField('AudienceNumber', 'Номер аудитории', 120, True);

  AddTable('Pairs', 'Занятия');
  AddField('PairID', 'ID', 30, False);
  AddField('PairBegin', 'Начало занятия', 100, True);
  AddField('PairEnd', 'Конец занятия', 100, True);
  AddField('PairNumber', 'Номер занятия', 100, True);

  AddTable('Schedules', 'Расписание');
  AddField('GroupID', 'ID группы', 80, True);
  AddField('WeekDayID', 'ID дня', 80, True);
  AddField('PairID', 'ID занятия', 80, True);
  AddField('SubjectID', 'ID предмета', 80, True);
  AddField('EducID', 'ID вида занятия', 90, True);
  AddField('TeacherID', 'ID учителя', 80, True);
  AddField('AudienceID', 'ID аудитории', 80, True);

  AddTable('Teachers_Subjects', 'Предметы учитиелей');
  AddField('TeacherID', 'ID учителя', 80, True);
  AddField('SubjectID', 'ID предмета', 80, True);

  AddTable('Group_Subjects', 'Предметы групп');
  AddField('GroupID', 'ID группы', 80, True);
  AddField('SubjectID', 'ID предмета', 80, True);

  AddTable('WeekDays', 'Дни недели');
  AddField('WeekDayID', 'ID', 80, False);
  AddField('WeekDayName', 'День недели', 80, True);
  AddField('WeekDayNumber', 'Номер дня недели', 120, True);

  AddTable('EducActivities', 'Вид занятия');
  AddField('EducID', 'ID', 30, False);
  AddField('EducName', 'Вид занятия', 80, True);

end.
