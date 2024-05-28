unit PTLib.GCP.Service.Stackdriver.Logging;

interface

uses
  SysUtils, Classes, System.DateUtils, System.NetEncoding, JSON,
  System.JSON.Builders,

  XSuperObject,

  PTLIb.Common.Interfaces,
  PTLib.Common.JSON,

  PTLib.GCP.Types,
  PTLib.GCP.Service,
  PTLib.GCP.Interfaces;

type
  TGCPStackdriverLoggingService = class(TGCPService, IGCPStackdriverLoggingService)
  protected
    function EntriesWrite(const LogName: String; MonitoredResource: IGCPStackdriverLoggingMonitoredResource;
      Entries: IList<IGCPStackDriverLoggingLogEntry>; const Labels: TArray<TKeyValue>; const PartialSuccess,
      DryRun: Boolean): IGCPStackdriverLoggerResponse; virtual;
  public
    constructor Create; override;
  end;

implementation

uses
  PTLib.GCP.Classes;

type
  TGCPStackdriverLoggerResponse = class(THTTPResponse, IGCPStackdriverLoggerResponse);

{ TStackdriverLoggingService }

constructor TGCPStackdriverLoggingService.Create;
begin
  inherited;

  ServiceName := 'logging';
  ServiceVersion := 'v2';
end;


function TGCPStackdriverLoggingService.EntriesWrite(const LogName: String; MonitoredResource: IGCPStackdriverLoggingMonitoredResource;
  Entries: IList<IGCPStackDriverLoggingLogEntry>; const Labels: TArray<TKeyValue>; const PartialSuccess: Boolean;
  const DryRun: Boolean): IGCPStackdriverLoggerResponse;
var
  i: Integer;
  X: ISuperObject;
begin
  Result := TGCPStackdriverLoggerResponse.Create;

  X := SO;
  (*
  if Data <> '' then
  begin
    X.A['messages'].O[0].S['data'] := TNetEncoding.Base64.Encode(Data);
  end;

  for i := Low(Attributes) to High(Attributes) do
  begin
    X.A['messages'].O[0].O['attributes'].S[Attributes[i].Name] := Attributes[i].Value;
  end; *)

  POST(
    'entries:write',
    X.AsJSON,
    Result);
end;

end.
