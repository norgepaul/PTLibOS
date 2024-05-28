unit PTLib.GCP.Factory;

interface

uses
  PTLib.GCP.Interfaces;

type
  TGCPFactory = class
  strict private
    class var FGCP: IGCP;
    class var FLogger: IGCPLogger;
    class var FAuthenticationService: IGCPAuthentication;
    class var FSpeechService: IGCPSpeech;
    class var FPubSubService: IGCPPubSub;
  private
    class procedure CheckAPINotRegistered;

    class procedure SetAuthenticationService(const Value: IGCPAuthentication); static;
    class procedure SetLogger(const Value: IGCPLogger); static;
    class procedure SetSpeechService(const Value: IGCPSpeech); static;
  public
    class constructor Create;

    class function GCP: IGCP;

    class property AuthenticationService: IGCPAuthentication write SetAuthenticationService;
    class property Logger: IGCPLogger write SetLogger;
    class property SpeechService: IGCPSpeech write SetSpeechService;
  end;

implementation

uses
  PTLib.GCP,
  PTLib.GCP.Classes,
  PTLib.GCP.Logger,
  PTLib.GCP.Authentication,
  PTLib.GCP.Service.Speech,
  PTLib.GCP.Service.PubSub;

resourcestring
  StrPleaseDefineTheGPC = 'Please define the GPC API Interfaces before calling GCP';

{ TGCPFactory }

class procedure TGCPFactory.CheckAPINotRegistered;
begin
  if FGCP <> nil then
  begin
    raise EGCPAlreadyRegistered.Create(StrPleaseDefineTheGPC);
  end;
end;

class constructor TGCPFactory.Create;
begin
  FLogger := TLogger.Create;
  FAuthenticationService := TGCPAuthenticationService.Create;
  FSpeechService := TGCPSpeechService.Create;
  FPubSubService := TGCPPubSubService.Create;
end;

class function TGCPFactory.GCP: IGCP;
begin
  if FGCP = nil then
  begin
    FGCP := TGCP.Create(
      FLogger,
      FAuthenticationService,
      FSpeechService,
      FPubSubService
    );
  end;

  Result := FGCP;
end;

class procedure TGCPFactory.SetAuthenticationService(
  const Value: IGCPAuthentication);
begin
  CheckAPINotRegistered;

  FAuthenticationService := Value;
end;

class procedure TGCPFactory.SetLogger(const Value: IGCPLogger);
begin
  CheckAPINotRegistered;

  FLogger := Value;
end;

class procedure TGCPFactory.SetSpeechService(
  const Value: IGCPSpeech);
begin
  CheckAPINotRegistered;

  FSpeechService := Value;
end;

end.
