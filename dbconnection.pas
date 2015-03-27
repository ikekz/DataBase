unit DBConnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil;

type

  { TDBConnectionForm }

  TDBConnectionForm = class(TDataModule)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DBConnectionForm: TDBConnectionForm;

implementation

{$R *.lfm}

end.

