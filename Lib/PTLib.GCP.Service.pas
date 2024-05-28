unit PTLib.GCP.Service;

interface

uses
  SysUtils, System.Diagnostics, System.Threading, System.Classes,

  PTLib.GCP,
  PTLib.GCP.Network,
  PTLib.GCP.Types,
  PTLib.GCP.Interfaces;

type
  TGCPService = class(TInterfacedObject)
  strict private
    FServiceVersion: String;
    FServiceName: String;
  protected
    function GCP: IGCP;

    function BuildGCPURL(const ServiceMethod: String): String; virtual;
    function Request(const RequestType: THTTPRequestType;
  const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse;
  const ReceiveTimeout: Integer): THTTPResponseCode; virtual;

    procedure Log(const Text: String; const Severity: TLogSeverity = TLogSeverity.Info; const Timestamp: TDateTime = 0); overload;
    procedure Log(const Text: String; const Args: Array of const; const Severity: TLogSeverity = TLogSeverity.Info; const Timestamp: TDateTime = 0); overload;

    function Post(const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse;
      const ReceiveTimeout: Integer = DEFAULT_TIMEOUT_RECV): THTTPResponseCode; overload;
    procedure Post(const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse;
      const ResponseCallback: TGCPHTTPResponseCallback; const ReceiveTimeout: Integer = DEFAULT_TIMEOUT_RECV); overload;
    function Delete(const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
    function Get(const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
    function Options(const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
    function Put(const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;

    property ServiceName: String read FServiceName write FServiceName;
    property ServiceVersion: String read FServiceVersion write FServiceVersion;
  public
    constructor Create; virtual;
  end;

implementation

uses
  PTLib.GCP.Classes,
  PTLib.GCP.Factory;

{ TGCPService }

function TGCPService.BuildGCPURL(const ServiceMethod: String): String;
begin
  Result := format('https://%s.googleapis.com/%s/%s',
    [FServiceName,
     FServiceVersion,
     ServiceMethod]);
end;

constructor TGCPService.Create;
begin
  FServiceVersion := 'V1';
end;

function TGCPService.GCP: IGCP;
begin
  Result := TGCPFactory.GCP;
end;

procedure TGCPService.Log(const Text: String;
  const Severity: TLogSeverity; const Timestamp: TDateTime);
var
  Logger: IGCPLogger;
begin
  if Supports(GCP, IGCPLogger, Logger) then
  begin
    Logger.Log(Text, Severity, Timestamp);
  end;
end;

procedure TGCPService.Log(const Text: String;
  const Args: array of const; const Severity: TLogSeverity;
  const Timestamp: TDateTime);
var
  Logger: IGCPLogger;
begin
  if Supports(GCP, IGCPLogger, Logger) then
  begin
    Logger.Log(Text, Args, Severity, Timestamp);
  end;
end;

function TGCPService.Get(const ServiceMethod,
  Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
begin
  Result := Request(
    THTTPRequestType.GET,
    ServiceMethod,
    Contents,
    HTTPResponse,
    ReceiveTimeout);
end;

function TGCPService.Post(const ServiceMethod,
  Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
begin
  Result := Request(
    THTTPRequestType.POST,
    ServiceMethod,
    Contents,
    HTTPResponse,
    ReceiveTimeout);
end;

procedure TGCPService.Post(const ServiceMethod, Contents: String;
  HTTPResponse: IGCPHTTPResponse; const ResponseCallback: TGCPHTTPResponseCallback;
  const ReceiveTimeout: Integer);
var
  ResponseCode: THTTPResponseCode;
begin
  TTask.Run(
    procedure
    begin
      try
        ResponseCode := Post(
          ServiceMethod,
          Contents,
          HTTPResponse,
          ReceiveTimeout);

        ResponseCallback(
          ServiceMethod,
          Contents,
          HTTPResponse,
          nil,
          ResponseCode);
      except
        on e: Exception do
        begin
          ResponseCallback(
            ServiceMethod,
            Contents,
            HTTPResponse,
            e,
            -1);
        end;
      end;
    end
  );
end;

function TGCPService.Put(const ServiceMethod,
  Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
begin
  Result := Request(
    THTTPRequestType.PUT,
    ServiceMethod,
    Contents,
    HTTPResponse,
    ReceiveTimeout);
end;

function TGCPService.Delete(const ServiceMethod,
  Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
begin
  Result := Request(
    THTTPRequestType.DELETE,
    ServiceMethod,
    Contents,
    HTTPResponse,
    ReceiveTimeout);
end;

function TGCPService.Options(const ServiceMethod,
  Contents: String; HTTPResponse: IGCPHTTPResponse; const ReceiveTimeout: Integer): THTTPResponseCode;
begin
  Result := Request(
    THTTPRequestType.OPTIONS,
    ServiceMethod,
    Contents,
    HTTPResponse,
    ReceiveTimeout);
end;

function TGCPService.Request(const RequestType: THTTPRequestType;
  const ServiceMethod, Contents: String; HTTPResponse: IGCPHTTPResponse;
  const ReceiveTimeout: Integer): THTTPResponseCode;
var
  StopWatch: TStopwatch;
begin
  Stopwatch := TStopwatch.Create;
  StopWatch.Reset;
  Stopwatch.Start;

  Result := TNetworkService.Request(
    RequestType,
    BuildGCPURL(ServiceMethod),
    Contents,
    GCP.GetAccessToken,
    HTTPResponse,
    ReceiveTimeout);

  HTTPResponse.ResponseTime := StopWatch.Elapsed;
end;

end.
