unit PTLib.GCP.Service.PubSub;

interface

uses
  SysUtils, Classes, System.DateUtils, System.NetEncoding, JSON,
  System.JSON.Builders,

  XSuperObject,

  PTLib.Common.JSON,

  PTLib.GCP.Types,
  PTLib.GCP.Service,
  PTLib.GCP.Interfaces;

type
  TGCPPubSubService = class(TGCPService, IGCPPubSub)
  protected
    function Publish(const Topic, Data: String; const Attributes: array of TKeyValue): IGCPPubSubResponse;
    function Pull(const Subscription: String; const ReturnImmediately: Boolean; const MaxMessages: Integer): IGCPPubSubResponse;
    function Acknowledge(const Subscription: String; const AckIDs: array of String): IGCPPubSubResponse; overload;
    function Acknowledge(const Subscription: String; PullResponse: IGCPPubSubResponse): IGCPPubSubResponse; overload;
  public
    constructor Create; override;
  end;

implementation

uses
  PTLib.GCP.Classes;

type
  TPubSubResponse = class(THTTPResponse, IGCPPubSubResponse);

{ TPubSubService }

constructor TGCPPubSubService.Create;
begin
  inherited;

  ServiceName := 'pubsub';
  ServiceVersion := 'v1';
end;

function TGCPPubSubService.Pull(const Subscription: String; const ReturnImmediately: Boolean; const MaxMessages: Integer): IGCPPubSubResponse;
var
  X: ISuperObject;
begin
  Result := TPubSubResponse.Create;
  X := SO;

  X.B['returnImmediately'] := ReturnImmediately;
  X.I['maxMessages'] := MaxMessages;

  POST(
    Subscription + ':' + 'pull',
    X.AsJSON,
    Result);
end;

function TGCPPubSubService.Acknowledge(const Subscription: String; const AckIDs: Array of String): IGCPPubSubResponse;
var
  X: ISuperObject;
  i: Integer;
begin
  Result := TPubSubResponse.Create;
  X := SO;

  for i := Low(AckIDs) to High(AckIDs) do
  begin
    X.A['ackIds'].S[i] := AckIDs[i];
  end;

  POST(
    Subscription + ':' + 'acknowledge',
    X.AsJSON,
    Result);
end;

function TGCPPubSubService.Acknowledge(const Subscription: String; PullResponse: IGCPPubSubResponse): IGCPPubSubResponse;
var
  X: ISuperObject;
  i, Count: Integer;
  AckIDs: Array of String;
begin
  Result := nil;

  // Is it a valid response?
  if PullResponse.HTTPResponseCode = 200 then
  begin
    X := SO(PullResponse.Content);

    Count := X.A['receivedMessages'].Length;

    if Count > 0 then
    begin
      SetLength(AckIds, Count);

      for i := 0 to pred(Count) do
      begin
        AckIds[i] := X.A['receivedMessages'].O[i].S['ackId'];
      end;

      Result := Acknowledge(Subscription, AckIDs);
    end;
  end;
end;

function TGCPPubSubService.Publish(const Topic: String; const Data: String; const Attributes: Array of TKeyValue): IGCPPubSubResponse;
var
  i: Integer;
  X: ISuperObject;
begin
  Result := TPubSubResponse.Create;
  X := SO;

  if Data <> '' then
  begin
    X.A['messages'].O[0].S['data'] := TNetEncoding.Base64.Encode(Data);
  end;

  for i := Low(Attributes) to High(Attributes) do
  begin
    X.A['messages'].O[0].O['attributes'].S[Attributes[i].Name] := Attributes[i].Value;
  end;

  POST(
    Topic + ':' + 'publish',
    X.AsJSON,
    Result);
end;

end.
