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
    function MakeJoin(TableName: string): string; virtual;
    function MakeSelectField: string; virtual;
  end;

  TMyJoinField = class(TMyField)
    JoinTableName, JoinFieldName, JoinFieldShow: string;
    constructor Create(CurrentName, CurrentCaption, CurrentDataType: string;
      CurrentWidth: integer; CurrentVisible: boolean;
      CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
    function MakeJoin(TableName: string): string; override;
    function MakeSelectField: string; override;
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
  DataType := CurrentDataType;
end;

constructor TMyJoinField.Create(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean;
  CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
begin
  inherited Create(CurrentName, CurrentCaption, CurrentDataType,
    CurrentWidth, CurrentVisible);
  JoinTableName := CurrentJoinTableName;
  JoinFieldName := CurrentJoinFieldName;
  JoinFieldShow := CurrentJoinFieldShow;
end;

procedure TMyTable.AddField(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean);
begin
  SetLength(FieldArray, Length(FieldArray) + 1);
  FieldArray[High(FieldArray)] :=
    TMyField.Create(CurrentName, CurrentCaption, CurrentDataType,
    CurrentWidth, CurrentVisible);
end;

procedure TMyTable.AddJoinField(CurrentName, CurrentCaption, CurrentDataType: string;
  CurrentWidth: integer; CurrentVisible: boolean;
  CurrentJoinTableName, CurrentJoinFieldName, CurrentJoinFieldShow: string);
begin
  SetLength(FieldArray, Length(FieldArray) + 1);
  FieldArray[High(FieldArray)] :=
    TMyJoinField.Create(CurrentName, CurrentCaption, CurrentDataType,
    CurrentWidth, CurrentVisible, CurrentJoinTableName, CurrentJoinFieldName,
    CurrentJoinFieldShow);
end;

function TMyField.MakeSelectField: string;
begin
  Result := '';
end;

function TMyJoinField.MakeSelectField: string;
begin
  Result := Format(', %s.%s', [JoinTableName, JoinFieldShow]);
end;

function TMyField.MakeJoin(TableName: string): string;
begin
  Result := '';
end;

function TMyJoinField.MakeJoin(TableName: string): string;
begin
  Result := Format(' LEFT JOIN %s ON %s.%s = %s.%s ',
    [JoinTableName, TableName, Name, JoinTableName, JoinFieldName]);
end;

initialization

  with TMyTable.AddTable('Teachers', 'Учителя') do
  begin
    AddField('TeacherID', 'ID', 'Integer', 40, False);
    AddField('TeacherInitials', 'Фамилия имя отчество', 'Varchar', 230, True);
  end;

  with TMyTable.AddTable('Groups', 'Группы') do
  begin
    AddField('GroupID', 'ID', 'Integer', 40, False);
    AddField('GroupNumber', 'Номер группы', 'Varchar', 110, True);
    AddField('GroupName', 'Название группы', 'Varchar', 250, True);
  end;

  with TMyTable.AddTable('Students', 'Студенты') do
  begin
    AddField('StudentID', 'ID', 'Integer', 40, False);
    AddField('StudentInitials', 'Фамилия имя отчество', 'Varchar', 230, True);
    AddJoinField('GroupID', 'ID группы', 'Integer', 90, False, 'Groups',
      'GroupID', 'GroupNumber');
    AddField('GroupNumber', 'Номер группы', 'Varchar', 110, True);
  end;

  with TMyTable.AddTable('Subjects', 'Предметы') do
  begin
    AddField('SubjectID', 'ID', 'Integer', 30, False);
    AddField('SubjectName', 'Название предмета', 'Varchar', 230, True);
  end;

  with TMyTable.AddTable('Audiences', 'Аудитории') do
  begin
    AddField('AudienceID', 'ID', 'Integer', 40, False);
    AddField('AudienceNumber', 'Номер аудитории', 'Varchar', 130, True);
  end;

  with TMyTable.AddTable('Pairs', 'Занятия') do
  begin
    AddField('PairID', 'ID', 'Integer', 40, False);
    AddField('PairBegin', 'Начало занятия', 'Varchar', 120, True);
    AddField('PairEnd', 'Конец занятия', 'Varchar', 110, True);
    AddField('PairNumber', 'Номер занятия', 'Varchar', 110, True);
  end;

  with TMyTable.AddTable('Schedules', 'Расписание') do
  begin
    AddJoinField('GroupID', 'ID группы', 'Integer', 90, False, 'Groups',
      'GroupID', 'GroupNumber');
    AddJoinField('WeekDayID', 'ID дня', 'Integer', 90, False, 'WeekDays',
      'WeekDayID', 'WeekDayName');
    AddJoinField('PairID', 'ID занятия', 'Integer', 90, False, 'Pairs',
      'PairID', 'PairNumber');
    AddJoinField('SubjectID', 'ID предмета', 'Integer', 90, False,
      'Subjects', 'SubjectID', 'SubjectName');
    AddJoinField('EducID', 'ID вида занятия', 'Integer', 100, False,
      'EducActivities', 'EducID', 'EducName');
    AddJoinField('TeacherID', 'ID учителя', 'Integer', 90, False,
      'Teachers', 'TeacherID', 'TeacherInitials');
    AddJoinField('AudienceID', 'ID аудитории', 'Integer', 90, False,
      'Audiences', 'AudienceID', 'AudienceNumber');
    AddField('GroupNumber', 'Номер группы', 'Varchar', 110, True);
    AddField('WeekDayName', 'День недели', 'Varchar', 100, True);
    AddField('PairNumber', 'Номер занятия', 'Integer', 110, True);
    AddField('SubjectName', 'Название предмета', 'Varchar', 230, True);
    AddField('EducName', 'Вид занятия', 'Varchar', 90, True);
    AddField('TeacherInitials', 'Фамилия имя отчество', 'Varchar', 230, True);
    AddField('AudienceNumber', 'Номер аудитории', 'Varchar', 130, True);
  end;

  with TMyTable.AddTable('Teachers_Subjects', 'Предметы учитиелей') do
  begin
    AddJoinField('TeacherID', 'ID учителя', 'Integer', 90, False,
      'Teachers', 'TeacherID', 'TeacherInitials');
    AddJoinField('SubjectID', 'ID предмета', 'Integer', 90, False,
      'Subjects', 'SubjectID', 'SubjectName');
    AddField('TeacherInitials', 'Фамилия имя отчество', 'Varchar', 230, True);
    AddField('SubjectName', 'Название предмета', 'Varchar', 230, True);
  end;

  with TMyTable.AddTable('Group_Subjects', 'Предметы групп') do
  begin
    AddJoinField('GroupID', 'ID группы', 'Integer', 90, False, 'Groups',
      'GroupID', 'GroupNumber');
    AddJoinField('SubjectID', 'ID предмета', 'Integer', 90, False,
      'Subjects', 'SubjectID', 'SubjectName');
    AddField('GroupNumber', 'Номер группы', 'Varchar', 110, True);
    AddField('SubjectName', 'Название предмета', 'Varchar', 230, True);
  end;

  with TMyTable.AddTable('WeekDays', 'Дни недели') do
  begin
    AddField('WeekDayID', 'ID', 'Integer', 90, False);
    AddField('WeekDayName', 'День недели', 'Varchar', 100, True);
    AddField('WeekDayNumber', 'Номер дня недели', 'Integer', 130, True);
  end;

  with TMyTable.AddTable('EducActivities', 'Вид занятия') do
  begin
    AddField('EducID', 'ID', 'Integer', 40, False);
    AddField('EducName', 'Вид занятия', 'Varchar', 90, True);
  end;

end.
