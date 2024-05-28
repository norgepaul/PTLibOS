unit PTLib.MARS.HTTPServer;

interface

uses
  Types, Classes, SysUtils,

  IdCustomTCPServer, IdException,

  MARS.http.Server.Indy,

  PTLib.ORM,
  PTLib.ORM.Session,
  PTLib.ORM.Interfaces;

type
  TOnNeedORM = procedure(Sender: TObject; out ORM: IORM) of object;

  TPTLibMARSIdServerContext = class(TIdServerContext)
  strict private
    FORMSession: IORMSession;
  protected
    procedure BeforeRun; override;
    procedure AfterRun; override;
  public
    function ORMSession: TORMSessionConnection; virtual;
  end;

  TPTLibMARSServer = class(TMARShttpServerIndy)
  strict private
    FOnNeedORM: TOnNeedORM;
  protected
    procedure DoOnNeedORM(out ORM: IORM); virtual;

    procedure InitComponent; override;
  published
    property OnNeedORM: TOnNeedORM read FOnNeedORM write FOnNeedORM;
  end;

implementation

{ TPTLibMARSServer }

procedure TPTLibMARSServer.DoOnNeedORM(out ORM: IORM);
begin
  ORM := nil;

  if Assigned(FOnNeedORM) then
  begin
    FOnNeedORM(Self, ORM);
  end;
end;

procedure TPTLibMARSServer.InitComponent;
begin
  inherited;

  // Set the context class to our context
  FContextClass := TPTLibMARSIdServerContext;
end;

{ TPTLibMARSIdServerContextClass }

procedure TPTLibMARSIdServerContext.AfterRun;
begin
  if FORMSession <> nil then
  begin
    if ((ExceptObject <> nil) and
       (not (Exception(ExceptObject) is EIdConnClosedGracefully))) then
    begin
      FORMSession.Connection.Rollback;

      //if ExceptObject is Exception then
      //  Log(Exception(ExceptObject).Message, llError);
    end
    else
    begin
      try
        FORMSession.Connection.Commit;
      except
        FORMSession.Connection.Rollback;
      end;
    end;

    FORMSession := nil;
  end;

  inherited;
end;

procedure TPTLibMARSIdServerContext.BeforeRun;
begin
  inherited;

end;

function TPTLibMARSIdServerContext.ORMSession: TORMSessionConnection;
var
  ORM: IORM;
begin
  if FORMSession = nil then
  begin
    TPTLibMARSServer(FServer).DoOnNeedORM(ORM);

    FORMSession := ORM.NewORMSession(nil, []) as IORMSession;
  end;
end;

end.
