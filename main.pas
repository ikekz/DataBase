unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, Forms, Controls,
  Graphics, Dialogs, Menus, ListView, MetaData, AboutProgram;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExitMenu: TMenuItem;
    DirectoryMenu: TMenuItem;
    AboutProgramMenu: TMenuItem;
    ReferenceMenu: TMenuItem;
    procedure AboutProgramMenuClick(Sender: TObject);
    procedure DirectoryMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DirectoryMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  MenuItem: TMenuItem;
  i: integer;
begin
  for i := 0 to High(TableArray) do
  begin
    MenuItem := TMenuItem.Create(DirectoryMenu);
    DirectoryMenu.Add(MenuItem);
    MenuItem.Tag := i;
    MenuItem.Caption := TableArray[i].Caption;
    MenuItem.OnClick := @DirectoryMenuItemClick;
  end;
end;

procedure TMainForm.DirectoryMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  with Screen do
    for i := 0 to FormCount - 1 do
    begin
      if Forms[i].Caption = TMenuItem(Sender).Caption then
      begin
        Forms[i].ShowOnTop;
        exit;
      end;
    end;
  TListViewForm.CreateNewForm(TableArray[TMenuItem(Sender).Tag]);
end;

procedure TMainForm.ExitMenuClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.DirectoryMenuClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to MainForm.DirectoryMenu.Count - 1 do
  begin
    MainForm.DirectoryMenu.Items[i].Checked := False;
    with Screen do
      for j := 0 to FormCount - 1 do
      begin
        if Forms[j].Caption = MainForm.DirectoryMenu.Items[i].Caption then
          MainForm.DirectoryMenu.Items[i].Checked := True;
      end;
  end;
end;

procedure TMainForm.AboutProgramMenuClick(Sender: TObject);
begin
  AboutProgramForm.show;
end;

end.
