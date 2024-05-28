unit PTLib.GCP;

{ Google Cloud Platform APIs for Google Compute Engine instances }

interface

uses
  SysUtils, SyncObjs, Classes, IOUtils,

  XSuperObject,

  PTLib.GCP.Interfaces;

type
  TGCP = class(TInterfacedObject, IGCP, IGCPLogger)
  private
    FOAuthScope: String;
    FServiceAccount: String;
    FPrivateKeyFilename: String;
    FPrivateKey: String;

    // Internal Services
    FLogger: IGCPLogger;

    // Authentication
    FAuthenticationService: IGCPAuthentication;

    // Google Cloud Services
    FSpeechService: IGCPSpeech;
    FPubSubService: IGCPPubSub;

    procedure SaveSettings(const Filename: String);
    procedure LoadSettings(const Filename: String);
    function GetServiceAccount: String;
    function GetOAuthScope: String;
    function GetPrivateKeyFilename: String;
    function GetAccessToken: String;
  protected
    procedure InitializeGCP(const ServiceAccount, OAuthScope, PrivateKeyFilename: String);

    property AccessToken: String read GetAccessToken;
  protected
    // ILogger
    property Logger: IGCPLogger read Flogger implements IGCPLogger;
  protected
    // IGCP
    function Authentication: IGCPAuthentication;
    function Speech: IGCPSpeech;
    function PubSub: IGCPPubSub;
  public
    constructor Create(Logger: IGCPLogger; AuthenticationService: IGCPAuthentication;
      SpeechService: IGCPSpeech; PubSubService: IGCPPubSub);
    destructor Destroy; override;
  end;

implementation

uses
  PTLib.GCP.Classes;

const
  DefaultTokenExpireSeconds = 3600;

{ TGoogle }

constructor TGCP.Create(Logger: IGCPLogger; AuthenticationService: IGCPAuthentication;
  SpeechService: IGCPSpeech; PubSubService: IGCPPubSub);
begin
  FLogger := Logger;
  FAuthenticationService := AuthenticationService;
  FSpeechService := SpeechService;
  FPubSubService := PubSubService;
end;

destructor TGCP.Destroy;
begin

  inherited;
end;

procedure TGCP.SaveSettings(const Filename: String);
var
  X: ISuperObject;
begin
  ForceDirectories(ExtractFileDir(Filename));

  X := SO;

  X.S['serviceAccount'] := FServiceAccount;
  X.S['oAuthScope'] := FOAuthScope;
  X.S['privateKeyFilename'] := FPrivateKeyFilename;

  X.SaveTo(Filename, True);
end;

function TGCP.GetServiceAccount: String;
begin
  Result := FServiceAccount;
end;

procedure TGCP.InitializeGCP(const ServiceAccount, OAuthScope,
  PrivateKeyFilename: String);
begin
  if (ServiceAccount <> FServiceAccount) or
     (OAuthScope <> FOAuthScope) or
     (PrivateKeyFilename <> FPrivateKeyFilename) then
  begin
    FServiceAccount := ServiceAccount;
    FOAuthScope := OAuthScope;
    FPrivateKeyFilename := PrivateKeyFilename;
    FPrivateKey := TFile.ReadAllText(FPrivateKeyFilename);

    Authentication.ResetAccessToken;
  end;
end;

function TGCP.Speech: IGCPSpeech;
begin
  Result := FSpeechService;
end;

{ See: https://developers.google.com/identity/protocols/OAuth2ServiceAccount#authorizingrequests for more details }
function TGCP.Authentication: IGCPAuthentication;
begin
  Result :=  FAuthenticationService;
end;

function TGCP.GetAccessToken: String;
begin
  Result := Authentication.GetAccessToken(
    FServiceAccount,
    FOAuthScope,
    FPrivateKey,
    DefaultTokenExpireSeconds);
end;

procedure TGCP.LoadSettings(const Filename: String);
var
  X: ISuperObject;
begin
  X := SO(TFile.ReadAllText(Filename));

  InitializeGCP(
    X.S['serviceAccount'],
    X.S['oAuthScope'],
    X.S['privateKeyFilename']
  );
end;

function TGCP.PubSub: IGCPPubSub;
begin
  Result := FPubSubService;
end;

function TGCP.GetOAuthScope: String;
begin
  Result := FOAuthScope;
end;

function TGCP.GetPrivateKeyFilename: String;
begin
  Result := FPrivateKeyFilename;
end;

end.
