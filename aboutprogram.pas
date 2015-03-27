unit AboutProgram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TAboutProgramForm }

  TAboutProgramForm = class(TForm)
    AboutCreatorLabel: TLabel;
    CloseButton: TButton;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  AboutProgramForm: TAboutProgramForm;

implementation

{$R *.lfm}

{ TAboutProgramForm }

procedure TAboutProgramForm.CloseButtonClick(Sender: TObject);
begin
  AboutProgramForm.Close;
end;

procedure TAboutProgramForm.FormCreate(Sender: TObject);
begin
  AboutProgramForm.Tag := -1;
end;

end.

