unit PTLib.MARS.ServerContainer.Application.K8S;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Hash, System.Classes,
  System.SyncObjs,

  {$IFDEF LINUX64}
  Posix.Signal,
  {$ENDIF}

  MARS.Core.Registry, MARS.Core.Injection, MARS.Core.Attributes,
  MARS.Core.MediaType, MARS.Core.JSON, MARS.Core.MessageBodyWriters,
  MARS.Core.MessageBodyReaders, MARS.Client.Utils,

  PTLib.Common.Log,
  PTLib.Common.Types,

  PTLib.MARS.ServerContainer.Application;

type
  {../check/}
  [Path('check')]
  [Produces(TMediaType.APPLICATION_JSON)]
  TMarsServerContainerServiceApplicationK8S = class(TMarsServerContainerServiceApplication)
  strict private
    const OKResponseJSON = '{"ok": true}';

    class var K8SLivenessCS: TCriticalSection;
    class var FK8SLivenessTicks: TDateTime;
    class var FK8SReadiness: Boolean;
    class var FMaxLivenessTicks: Cardinal;
    class var FDrainStateTickCount: Cardinal;

    class function GetK8SLiveness: Boolean; static;
    class function GetK8SReadiness: Boolean; static;
    class function GetRunning: Boolean; static;
  private
    class var FRunning: Boolean;
    class var FDrainStateTicks: Cardinal;
    class function GetK8SActive: Boolean; static;
  public
    [GET, Path('/liveness'), Produces(TMediaType.APPLICATION_JSON)]
    function Liveness: TJSONRawString;

    [GET, Path('/readiness'), Produces(TMediaType.APPLICATION_JSON)]
    function Readiness: TJSONRawString;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure K8SLivenessPing;

    class property Running: Boolean read GetRunning;
    class property K8SActive: Boolean read GetK8SActive;
    class property K8SLiveness: Boolean read GetK8SLiveness;
    class property K8SReadiness: Boolean read GetK8SReadiness write FK8SReadiness;
    class property MaxLivenessTicks: Cardinal read FMaxLivenessTicks write FMaxLivenessTicks;
    class property DrainStateTickCount: Cardinal read FDrainStateTickCount write FDrainStateTickCount;
    class property DrainStateTicks: Cardinal read FDrainStateTicks;
  end;

implementation

const
  K8SLogDescription = 'K8s';

resourcestring
  StrLivenessCheckFaile = 'Liveness check failed';
  StrReadinessCheckFail = 'Readiness check failed';
  StrSShuttingDown = '%s - Shutting down, drain state active for %dms';

{ TMarsServerContainerServiceApplicationK8S }

{$IFDEF LINUX64}
procedure HandleSignals(SigNum: Integer); cdecl;
begin
  case SigNum of
    SIGTERM:
    begin
      TGlobalLog.Log(
        K8SLogDescription,
        'Shutdown signal received',
        LogSeverityInfo
      );

      if TMarsServerContainerServiceApplicationK8S.K8sActive then
      begin
        TGlobalLog.Log(
          K8SLogDescription,
          'Entering K8s drain state for %dms',
          [TMarsServerContainerServiceApplicationK8S.DrainStateTickCount],
          LogSeverityInfo
        );

        TMarsServerContainerServiceApplicationK8S.FDrainStateTicks := TThread.GetTickCount + TMarsServerContainerServiceApplicationK8S.DrainStateTickCount;
      end;

      TMarsServerContainerServiceApplicationK8S.FRunning := False;
    end;
  end;
end;
{$ENDIF}

function TMarsServerContainerServiceApplicationK8S.Liveness: TJSONRawString;
begin
  if K8SLiveness then
  begin
    Result := OKResponseJSON;
  end
  else
  begin
    raise EMARSClientHttpException.Create(StrLivenessCheckFaile);
  end;
end;

function TMarsServerContainerServiceApplicationK8S.Readiness: TJSONRawString;
var
  Error: String;
begin
  if K8SReadiness then
  begin
    Result := OKResponseJSON;
  end
  else
  begin
    Error := StrReadinessCheckFail;

    if FDrainStateTicks <> 0 then
    begin
      Error := format(StrSShuttingDown, [Error, FDrainStateTicks - TThread.GetTickCount]);
    end;

    raise EMARSClientHttpException.Create(Error);
  end;
end;

class constructor TMarsServerContainerServiceApplicationK8S.Create;
begin
  K8SLivenessCS := TCriticalSection.Create;

  FMaxLivenessTicks := 2000;
  FDrainStateTickCount := 25000;
  FDrainStateTicks := 0;
  FRunning := True;

  // Handle the shutdown signal
  {$IFDEF LINUX64}
  signal(SIGTERM, HandleSignals);
  {$ENDIF}
end;

class destructor TMarsServerContainerServiceApplicationK8S.Destroy;
begin
  FreeAndNil(K8SLivenessCS);
end;

class function TMarsServerContainerServiceApplicationK8S.GetK8SActive: Boolean;
begin
  Result := GetEnvironmentVariable('UIP_START') = '1'; // Do not localise
end;

class function TMarsServerContainerServiceApplicationK8S.GetK8SLiveness: Boolean;
begin
  K8SLivenessCS.Enter;
  try
    Result := TThread.GetTickCount - FK8SLivenessTicks < MaxLivenessTicks;
  finally
    K8SLivenessCS.Leave;
  end;
end;

class function TMarsServerContainerServiceApplicationK8S.GetK8SReadiness: Boolean;
begin
  Result := FK8SReadiness and FRunning;
end;

class function TMarsServerContainerServiceApplicationK8S.GetRunning: Boolean;
begin
  Result :=
    ((FRunning) and (FDrainStateTicks = 0)) or
    ((not FRunning) and (TThread.GetTickCount < FDrainStateTicks));
end;

class procedure TMarsServerContainerServiceApplicationK8S.K8SLivenessPing;
begin
  K8SLivenessCS.Enter;
  try
    FK8SLivenessTicks := TThread.GetTickCount;
  finally
    K8SLivenessCS.Leave;
  end;
end;

initialization
  {$IFNDEF NO_REG_K8S_APP}
  TMARSResourceRegistry.Instance.RegisterResource<TMarsServerContainerServiceApplicationK8S>;
  {$ENDIF}

end.

