unit PTLib.ORM.Connection.FireDAC;

interface

uses
  System.SysUtils, System.Classes,

  FireDAC.Comp.Client,

  PTLib.ORM.Connection,
  PTLib.ORM.SQL.Connection.FireDAC,

  PTLib.Common.Types,
  PTLib.Common.Log;

type
  TORMFireDacConnection = class(TORMConnection)
  strict private
    FFireDACConnection: TFDConnection;
    FConnectionProvider: TORMFireDACConnectionProvider;
  protected
    procedure DoConnect(const Host, Database, Username, Password: String; const Port: Integer); overload; virtual;
    procedure DoConnect(const ConnectionString: String); overload; virtual;
  public
    constructor Create; override;

    procedure Connect(const Host, Database, Username, Password: String; const Port: Integer); overload;
    procedure Connect(const ConnectionString: String); overload;
  end;

implementation

resourcestring
  StrHostS = 'Host - %s';
  StrDBS = 'DB - %s';
  StrUserS = 'User - %s';
  StrPortD = 'Port - %d';

{ TORMFireDacConnection }

procedure TORMFireDacConnection.Connect(const Host, Database, Username, Password: String; const Port: Integer);
begin
  DoConnect(
    Host,
    Database,
    Username,
    Password,
    Port
  );
end;

procedure TORMFireDacConnection.Connect(const ConnectionString: String);
begin
  DoConnect(ConnectionString);
end;

constructor TORMFireDacConnection.Create;
begin
  inherited;

  FFireDACConnection := TFDConnection.Create(ORM);
  FConnectionProvider := TORMFireDACConnectionProvider.Create(ORM);
end;

procedure TORMFireDacConnection.DoConnect(const Host, Database, Username, Password: String; const Port: Integer);
begin
  TGlobalLog.Log(Self, StrHostS, [Host], LogSeverityDebug2);
  TGlobalLog.Log(Self, StrDBS, [Database], LogSeverityDebug2);
  TGlobalLog.Log(Self, StrUserS, [Username], LogSeverityDebug2);
  TGlobalLog.Log(Self, StrPortD, [Port], LogSeverityDebug2);
end;

procedure TORMFireDacConnection.DoConnect(const ConnectionString: String);
begin
  FFireDACConnection.Params.Text := ConnectionString;

  FConnectionProvider.FireDACConnection := FFireDACConnection;
  ORM.ConnectionProvider := FConnectionProvider;
end;

end.
