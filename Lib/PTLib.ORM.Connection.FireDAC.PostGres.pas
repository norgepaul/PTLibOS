unit PTLib.ORM.Connection.FireDAC.PostGres;

interface

uses
  System.SysUtils, System.Classes,

  FireDAC.Phys.PGDef, FireDAC.Phys.PG,

  PTLib.ORM.Connection.FireDAC,
  PTLib.ORM.SQL.Connection.FireDAC;

type
  TORMFireDACPostGresConnection = class(TORMFireDacConnection)
  protected
    procedure DoConnect(const Host, Database, Username, Password: String; const Port: Integer); override;
  public
  end;

implementation

{ TORMFireDACPostGresConnection }

procedure TORMFireDACPostGresConnection.DoConnect(const Host, Database, Username, Password: String; const Port: Integer);
begin
  inherited;

  DoConnect(
    TORMFireDACConnectionProvider.GetPostGresConnectionParameters(
      Host,
      Database,
      Username,
      Password,
      Port)
   );
end;

end.
