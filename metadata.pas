unit MetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  TMyField = class
    Name, Caption, DataType: string;
    Width: integer;
    Visible: boolean;
    constructor Create(CurrentName, CurrentCaption, CurrentDataType: string;
      CurrentWidth: integer; CurrentVisible: boolean);
    function OutJoin(TableName: string): string; virtual;
    function OutSelectField: string; virtual;
  end;

  TMyJoinField = class(TMyField)
    JoinTableName, JoinFieldName, JoinFieldShow: string;
    constructor Create(CurrentName, CurrentCaption, CurrentDataType: string;
      CurrentWidth: integer; CurrentVisible: boolean;
      CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
    function OutJoin(TableName: string): string; override;
    function OutSelectField: string; override;
  end;

  TMyTable = class
    Name, Caption: string;
    FieldArray: array of TMyField;
    class function AddTable(CurrentName, CurrentCaption: string): TMyTable; static;
    procedure AddField(CurrentName, CurrentCaption, CurrentDataType: string;
      CurrentWidth: integer; CurrentVisible: boolean);
    procedure AddJoinField(CurrentName, CurrentCaption, CurrentDataType: string;
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

constructor TMyField.Create(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean);
begin
  Name := CurrentName;
  Caption := CurrentCaption;
  Width := CurrentWidth;
  Visible := CurrentVisible;
end;

constructor TMyJoinField.Create(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean;
  CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
begin
  inherited Create(CurrentName, CurrentCaption, CurrentDataType, CurrentWidth, CurrentVisible);
  JoinTableName := CurrentJoinTableName;
  JoinFieldName := CurrentJoinFieldName;
  JoinFieldShow := CurrentJoinFieldShow;
end;

procedure TMyTable.AddField(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean);
begin
  SetLength(FieldArray, Length(FieldArray) + 1);
  FieldArray[High(FieldArray)] :=
    TMyField.Create(CurrentName, CurrentCaption, CurrentDataType, CurrentWidth, CurrentVisible);
end;

procedure TMyTable.AddJoinField(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean;
  CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
begin
  SetLength(FieldArray, Length(FieldArray) + 1);
  FieldArray[High(FieldArray)] :=
    TMyJoinField.Create(CurrentName, CurrentCaption, CurrentDataType, CurrentWidth,
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
    AddField('TeacherID', 'ID', 'INTEGER', 30, False);
    AddField('TeacherInitials', 'Фамилия имя отчество', 'VARCHAR', 230, True);
  end;

  with TMyTable.AddTable('Groups', 'Группы') do
  begin
    AddField('GroupID', 'ID', 'INTEGER', 30, False);
    AddField('GroupNumber', 'Номер группы', 'VARCHAR', 100, True);
    AddField('GroupName', 'Название группы', 'VARCHAR', 250, True);
  end;

  with TMyTable.AddTable('Students', 'Студенты') do
  begin
    AddField('StudentID', 'ID', 'INTEGER', 30, False);
    AddField('StudentInitials', 'Фамилия имя отчество', 'VARCHAR', 230, True);
    AddJoinField('GroupID', 'ID группы', 'INTEGER', 80, False, 'Groups', 'GroupID', 'GroupNumber');
    AddField('GroupNumber', 'Номер группы', 'VARCHAR', 100, True);
  end;

  with TMyTable.AddTable('Subjects', 'Предметы') do
  begin
    AddField('SubjectID', 'ID', 'INTEGER', 30, False);
    AddField('SubjectName', 'Название предмета', 'VARCHAR', 230, True);
  end;

  with TMyTable.AddTable('Audiences', 'Аудитории') do
  begin
    AddField('AudienceID', 'ID', 'INTEGER', 30, False);
    AddField('AudienceNumber', 'Номер аудитории', 'VARCHAR', 120, True);
  end;

  with TMyTable.AddTable('Pairs', 'Занятия') do
  begin
    AddField('PairID', 'ID', 'INTEGER', 30, False);
    AddField('PairBegin', 'Начало занятия', 'VARCHAR', 100, True);
    AddField('PairEnd', 'Конец занятия', 'VARCHAR', 100, True);
    AddField('PairNumber', 'Номер занятия', 'VARCHAR', 100, True);
  end;

  with TMyTable.AddTable('Schedules', 'Расписание') do
  begin
    AddJoinField('GroupID', 'ID группы', 'INTEGER', 80, False, 'Groups', 'GroupID', 'GroupNumber');
    AddJoinField('WeekDayID', 'ID дня', 'INTEGER', 80, False, 'WeekDays', 'WeekDayID', 'WeekDayName');
    AddJoinField('PairID', 'ID занятия', 'INTEGER', 80, False, 'Pairs', 'PairID', 'PairNumber');
    AddJoinField('SubjectID', 'ID предмета', 'INTEGER', 80, False, 'Subjects', 'SubjectID', 'SubjectName');
    AddJoinField('EducID', 'ID вида занятия', 'INTEGER', 90, False, 'EducActivities', 'EducID', 'EducName');
    AddJoinField('TeacherID', 'ID учителя', 'INTEGER', 80, False, 'Teachers', 'TeacherID', 'TeacherInitials');
    AddJoinField('AudienceID', 'ID аудитории', 'INTEGER', 80, False, 'Audiences', 'AudienceID', 'AudienceNumber');
    AddField('GroupNumber', 'Номер группы', 'VARCHAR', 100, True);
    AddField('WeekDayName', 'День недели', 'VARCHAR', 80, True);
    AddField('PairNumber', 'Номер занятия', 'INTEGER', 100, True);
    AddField('SubjectName', 'Название предмета', 'VARCHAR', 230, True);
    AddField('EducName', 'Вид занятия', 'VARCHAR', 80, True);
    AddField('TeacherInitials', 'Фамилия имя отчество', 'VARCHAR', 230, True);
    AddField('AudienceNumber', 'Номер аудитории', 'VARCHAR', 120, True);
  end;

  with TMyTable.AddTable('Teachers_Subjects', 'Предметы учитиелей') do
  begin
    AddJoinField('TeacherID', 'ID учителя', 'INTEGER', 80, False, 'Teachers', 'TeacherID', 'TeacherInitials');
    AddJoinField('SubjectID', 'ID предмета', 'INTEGER', 80, False, 'Subjects', 'SubjectID', 'SubjectName');
    AddField('TeacherInitials', 'Фамилия имя отчество', 'VARCHAR', 230, True);
    AddField('SubjectName', 'Название предмета', 'VARCHAR', 230, True);
  end;

  with TMyTable.AddTable('Group_Subjects', 'Предметы групп') do
  begin
    AddJoinField('GroupID', 'ID группы', 'INTEGER', 80, False, 'Groups', 'GroupID', 'GroupNumber');
    AddJoinField('SubjectID', 'ID предмета', 'INTEGER', 80, False, 'Subjects', 'SubjectID', 'SubjectName');
    AddField('GroupNumber', 'Номер группы', 'VARCHAR', 100, True);
    AddField('SubjectName', 'Название предмета', 'VARCHAR', 230, True);
  end;

  with TMyTable.AddTable('WeekDays', 'Дни недели') do
  begin
    AddField('WeekDayID', 'ID', 'INTEGER', 80, False);
    AddField('WeekDayName', 'День недели', 'VARCHAR', 80, True);
    AddField('WeekDayNumber', 'Номер дня недели', 'INTEGER', 120, True);
  end;

  with TMyTable.AddTable('EducActivities', 'Вид занятия') do
  begin
    AddField('EducID', 'ID', 'INTEGER', 30, False);
    AddField('EducName', 'Вид занятия', 'VARCHAR', 80, True);
  end;

end.
