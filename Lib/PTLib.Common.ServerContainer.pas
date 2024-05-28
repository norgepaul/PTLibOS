unit PTLib.Common.ServerContainer;

interface

uses
  System.SysUtils,

  PTLib.Common.Classes,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,
  PTLib.Common.Log;

type
  TServerContainer = class(TInterfacedObject,
                           IServerContainer,
                           IClassDescriptor)
  strict private
    FBridgeServices: IList<IServerContainerService>;
  protected
    function StartService(const AService: IServerContainerService): Boolean;
    function StopService(const AService: IServerContainerService): Boolean;
    procedure StartServices;
    procedure StopServices;
    procedure GetClassDescriptor(out Value: String); virtual;
  public
    constructor Create(const Services: Array of IServerContainerService);
  end;

implementation

{ TShareBikeBridge }

constructor TServerContainer.Create(const Services: Array of IServerContainerService);
var
  i: Integer;
begin
  FBridgeServices := TList<IServerContainerService>.Create;

  for i := low(Services) to high(Services) do
  begin
    FBridgeServices.Add(Services[i]);
  end;
end;

procedure TServerContainer.GetClassDescriptor(out Value: String);
begin
  Value := 'Server';
end;

function TServerContainer.StartService(const AService: IServerContainerService): Boolean;
begin
  try
    AService.Active := True;

    Result := True;

   TGlobalLog.Log(Self, 'Service started - %s', [AService.GetServiceName], LogSeverityInfo);
  except
    on e: Exception do
    begin
      TGlobalLog.Log(Self, 'Exception: Unable to start service %s - %s', [AService.GetServiceName, e.Message], LogSeverityError);

      Result := False;
    end;
  end;
end;

function TServerContainer.StopService(const AService: IServerContainerService): Boolean;
begin
  try
    AService.Active := False;

    Result := True;

    TGlobalLog.Log(Self, 'Service started - %s', [AService.GetServiceName], LogSeverityInfo);
  except
    on e: Exception do
    begin
      TGlobalLog.Log(Self, 'Exception: Unable to stop service %s - %s', [AService.GetServiceName, e.Message], LogSeverityError);

      Result := False;
    end;
  end;
end;

procedure TServerContainer.StartServices;
var
  i: Integer;
begin
  for i := 0 to pred(FBridgeServices.Count) do
  begin
    StartService(FBridgeServices[i]);
  end;
end;

procedure TServerContainer.StopServices;
var
  i: Integer;
begin
  for i := 0 to pred(FBridgeServices.Count) do
  begin
    StopService(FBridgeServices[i]);
  end;
end;

end.
