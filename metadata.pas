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
    constructor Create(CurrentName, CurrentCaption: string;
      CurrentWidth: integer; CurrentVisible: boolean);
    function OutJoin(TableName: string): string; virtual;
    function OutSelectField: string; virtual;
  end;

  TMyJoinField = class(TMyField)
    JoinTableName, JoinFieldName, JoinFieldShow: string;
    constructor Create(CurrentName, CurrentCaption: string;
      CurrentWidth: integer; CurrentVisible: boolean;
      CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
    function OutJoin(TableName: string): string; override;
    function OutSelectField: string; override;
  end;

  TMyTable = class
    Name, Caption: string;
    FieldArray: array of TMyField;
    class function AddTable(CurrentName, CurrentCaption: string): TMyTable; static;
    procedure AddField(CurrentName, CurrentCaption: string;
      CurrentWidth: integer; CurrentVisible: boolean);
    procedure AddJoinField(CurrentName, CurrentCaption: string;
      CurrentWidth: integer; CurrentVisible: boolean;
      CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
  end;

var
  TableArray: array of TMyTable;

implementation

class function TMyTable.AddTable(CurrentName, CurrentCaption: string): TMyTable;
begin
  SetLength(TableArray, Length(TableArray) + 1);
  TableArray[High(TableArray)] := TMyTable.Create;
  TableArray[High(TableArray)].Name := CurrentName;
  TableArray[High(TableArray)].Caption := CurrentCaption;
  Result := TableArray[High(TableArray)];
end;

constructor TMyField.Create(CurrentName, CurrentCaption: string;
  CurrentWidth: integer; CurrentVisible: boolean);
begin
  Name := CurrentName;
  Caption := CurrentCaption;
  Width := CurrentWidth;
  Visible := CurrentVisible;
end;

constructor TMyJoinField.Create(CurrentName, CurrentCaption: string;
  CurrentWidth: integer; CurrentVisible: boolean;
  CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
begin
  inherited Create(CurrentName, CurrentCaption, CurrentWidth, CurrentVisible);
  JoinTableName := CurrentJoinTableName;
  JoinFieldName := CurrentJoinFieldName;
  JoinFieldShow := CurrentJoinFieldShow;
end;

procedure TMyTable.AddField(CurrentName, CurrentCaption: string;
  CurrentWidth: integer; CurrentVisible: boolean);
begin
  SetLength(FieldArray, Length(FieldArray) + 1);
  FieldArray[High(FieldArray)] :=
    TMyField.Create(CurrentName, CurrentCaption, CurrentWidth, CurrentVisible);
end;

procedure TMyTable.AddJoinField(CurrentName, CurrentCaption: string;
  CurrentWidth: integer; CurrentVisible: boolean;
  CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
begin
  SetLength(FieldArray, Length(FieldArray) + 1);
  FieldArray[High(FieldArray)] :=
    TMyJoinField.Create(CurrentName, CurrentCaption, CurrentWidth,
    CurrentVisible, CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow);
end;

function TMyField.OutSelectField: string;
begin
  Result := '';
end;

function TMyJoinField.OutSelectField: string;
begin
  Result := Format(', %s.%s', [JoinTableName, JoinFieldShow]);
end;

function TMyField.OutJoin(TableName: string): string;
begin
  Result := '';
end;

function TMyJoinField.OutJoin(TableName: string): string;
begin
  Result := Format(' LEFT JOIN %s ON %s.%s = %s.%s ',
    [JoinTableName, TableName, Name, JoinTableName, JoinFieldName]);
end;

initialization

  with TMyTable.AddTable('Teachers', 'Учителя') do
  begin
    AddField('TeacherID', 'ID', 30, False);
    AddField('TeacherInitials', 'Фамилия имя отчество', 230, True);
  end;

  with TMyTable.AddTable('Groups', 'Группы') do
  begin
    AddField('GroupID', 'ID', 30, False);
    AddField('GroupNumber', 'Номер группы', 100, True);
    AddField('GroupName', 'Название группы', 250, True);
  end;

  with TMyTable.AddTable('Students', 'Студенты') do
  begin
    AddField('StudentID', 'ID', 30, False);
    AddField('StudentInitials', 'Фамилия имя отчество', 230, True);
    AddJoinField('GroupID', 'ID группы', 80, False, 'Groups', 'GroupID', 'GroupNumber');
    AddField('GroupNumber', 'Номер группы', 100, True);
  end;

  with TMyTable.AddTable('Subjects', 'Предметы') do
  begin
    AddField('SubjectID', 'ID', 30, False);
    AddField('SubjectName', 'Название предмета', 230, True);
  end;

  with TMyTable.AddTable('Audiences', 'Аудитории') do
  begin
    AddField('AudienceID', 'ID', 30, False);
    AddField('AudienceNumber', 'Номер аудитории', 120, True);
  end;

  with TMyTable.AddTable('Pairs', 'Занятия') do
  begin
    AddField('PairID', 'ID', 30, False);
    AddField('PairBegin', 'Начало занятия', 100, True);
    AddField('PairEnd', 'Конец занятия', 100, True);
    AddField('PairNumber', 'Номер занятия', 100, True);
  end;

  with TMyTable.AddTable('Schedules', 'Расписание') do
  begin
    AddJoinField('GroupID', 'ID группы', 80, False, 'Groups', 'GroupID', 'GroupNumber');
    AddJoinField('WeekDayID', 'ID дня', 80, False, 'WeekDays', 'WeekDayID', 'WeekDayName');
    AddJoinField('PairID', 'ID занятия', 80, False, 'Pairs', 'PairID', 'PairNumber');
    AddJoinField('SubjectID', 'ID предмета', 80, False, 'Subjects', 'SubjectID', 'SubjectName');
    AddJoinField('EducID', 'ID вида занятия', 90, False, 'EducActivities', 'EducID', 'EducName');
    AddJoinField('TeacherID', 'ID учителя', 80, False, 'Teachers', 'TeacherID', 'TeacherInitials');
    AddJoinField('AudienceID', 'ID аудитории', 80, False, 'Audiences', 'AudienceID', 'AudienceNumber');
    AddField('GroupNumber', 'Номер группы', 100, True);
    AddField('WeekDayName', 'День недели', 80, True);
    AddField('PairNumber', 'Номер занятия', 100, True);
    AddField('SubjectName', 'Название предмета', 230, True);
    AddField('EducName', 'Вид занятия', 80, True);
    AddField('TeacherInitials', 'Фамилия имя отчество', 230, True);
    AddField('AudienceNumber', 'Номер аудитории', 120, True);
  end;

  with TMyTable.AddTable('Teachers_Subjects', 'Предметы учитиелей') do
  begin
    AddJoinField('TeacherID', 'ID учителя', 80, False, 'Teachers', 'TeacherID', 'TeacherInitials');
    AddJoinField('SubjectID', 'ID предмета', 80, False, 'Subjects', 'SubjectID', 'SubjectName');
    AddField('TeacherInitials', 'Фамилия имя отчество', 230, True);
    AddField('SubjectName', 'Название предмета', 230, True);
  end;

  with TMyTable.AddTable('Group_Subjects', 'Предметы групп') do
  begin
    AddJoinField('GroupID', 'ID группы', 80, False, 'Groups', 'GroupID', 'GroupNumber');
    AddJoinField('SubjectID', 'ID предмета', 80, False, 'Subjects', 'SubjectID', 'SubjectName');
    AddField('GroupNumber', 'Номер группы', 100, True);
    AddField('SubjectName', 'Название предмета', 230, True);
  end;

  with TMyTable.AddTable('WeekDays', 'Дни недели') do
  begin
    AddField('WeekDayID', 'ID', 80, False);
    AddField('WeekDayName', 'День недели', 80, True);
    AddField('WeekDayNumber', 'Номер дня недели', 120, True);
  end;

  with TMyTable.AddTable('EducActivities', 'Вид занятия') do
  begin
    AddField('EducID', 'ID', 30, False);
    AddField('EducName', 'Вид занятия', 80, True);
  end;

end.
