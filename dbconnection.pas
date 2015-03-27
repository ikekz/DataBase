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
  end;

var
  DBConnectionForm: TDBConnectionForm;

implementation

{$R *.lfm}

{ TDBConnectionForm }

end.

