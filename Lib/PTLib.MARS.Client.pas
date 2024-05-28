unit PTLib.MARS.Client;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections,
  System.StrUtils, System.Net.HTTPClient, System.Net.HttpClientComponent,
  System.SyncObjs, System.Diagnostics,

  MARS.Client.CustomResource, MARS.Client.Resource, MARS.Client.Token,
  MARS.Client.Application, MARS.Client.Client, MARS.Client.Client.Net,
  MARS.Utils.Parameters, MARS.Core.Utils, MARS.Client.Utils,
  MARS.Core.Exceptions,

  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Timers,
  PTLib.Common.Strings,
  PTLib.Common.Log,
  PTLib.Common.Types,

  PTLib.MARS.Interfaces,
  PTLib.MARS.Classes,
  PTLib.MARS.Types;

type
  TRequestItem = record
    ID: TRequestID;
    Request: IMarsClientRequest;
    OnRequest: TBeforeRequestProc;
    OnResponse: TResponseProc;
    OnError: TErrorProc;
  end;

  TResponseItem = record
    ID: TRequestID;
    Response: IMarsClientResponse;
    e: Exception;
    OnResponse: TResponseProc;
    OnError: TErrorProc;
  end;

  TMARSRequest = class
  strict private
    FMARSToken: TMARSClientToken;
    FMARSClient: TMARSNetClient;
    FMARSApplication: TMARSClientApplication;
    FClientResource: TMARSClientResource;
  public
    constructor Create;
    destructor Destroy; override;

    property MARSToken: TMARSClientToken read FMARSToken;
    property MARSClient: TMARSNetClient read FMARSClient;
    property MARSApplication: TMARSClientApplication read FMARSApplication;
    property ClientResource: TMARSClientResource read FClientResource;
  end;

  TMARSRequestPool = class
  strict private
    FRequestPool: TObjectStack<TMARSRequest>;
    FActiveConnectionCount: Integer;
    FMaxPoolCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Get: TMARSRequest;
    procedure Return(const Value: TMARSRequest);

    function ActiveConnectionCount: Integer;
    function PooledConnectionCount: Integer;

    property MaxPoolCount: Integer read FMaxPoolCount write FMaxPoolCount;
  end;

  TPTLibMarsClient = class(TInterfacedObject,
                           IMarsClient)
  strict private
    FTimer: IPTLibGlobalTimer;
    FResponseQueue: TQueue<TResponseItem>;
    FThreadSafe: Boolean;
    FUseCompression: Boolean;
    FHost: String;
    FApplication: String;
    FUseThreadPool: Boolean;
    FLogDescription: String;
    //FDefaultHeaders: TArray<String>;
    FDefaultErrorProc: TErrorProc;
    FQueuedRequests: TQueue<TRequestItem>;
    FMaxRequests: Integer;
  private
    procedure OnTimer(Sender: TObject);
    procedure QueueResponse(const ID: TRequestID; Response: IMarsClientResponse; const e: Exception; const OnResponse: TResponseProc; const OnError: TErrorProc);
    procedure HTTPRequestInternal(const HTTPRequestType: TMARSHttpVerb; const Resource: String; const Request: TStream; const Headers: TStrings;
      const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID);
    procedure AddHeaders(const ClientResource: TMARSClientResource; const Headers: TStrings);
    function GetActiveConnections: Integer;
    function GetPooledConnections: Integer;
    function GetUseThreadPool: Boolean;
    procedure SetUseThreadPool(const Value: Boolean);
    function GetHost: String;
    procedure SetHost(const Value: String);
    procedure ProcessRequestQueue;
  protected
    const MaxParamLength = 300;
  protected
    FRequestPool: TMARSRequestPool;

    procedure HTTPRequestAsyncInternal(Request: IMarsClientRequest; const OnResponse: TResponseProc;
      const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
  public
    constructor Create(const Host: String; const Application: String; const Threadsafe: Boolean = True;
      const UseCompression: Boolean = True; const MaxRequests: Integer = 0;
      const OnError: TErrorProc = nil); virtual;
    destructor Destroy; override;

    procedure HTTPRequestAsync(Request: IMarsClientRequest;
      const OnResponse: TResponseProc; const OnError: TErrorProc;
      const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
    function HTTPRequestSync(Request: IMarsClientRequest;
      const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse;

    procedure GET(const Resource: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function GET(const Resource: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure GET(const ChunkCount: Integer; const GetResourceProc: TGetResourceProc; const Headers: TStrings; const OnResponse: TChunkResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const OnProgress: TOnProgress = nil; const ID: TRequestID = NullRequestID); overload;

    procedure POST(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function POST(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure POST(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function POST(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure PUT(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PUT(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure PUT(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PUT(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure PATCH(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PATCH(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure PATCH(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PATCH(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure DELETE(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function DELETE(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure DELETE(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function DELETE(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure HEAD(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function HEAD(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure HEAD(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function HEAD(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    property PooledConnectionCount: Integer read GetPooledConnections;
    property ActiveConnectionCount: Integer read GetActiveConnections;

    property UseThreadPool: Boolean read GetUseThreadPool write SetUseThreadPool;
    property LogDescription: String read FLogDescription write FLogDescription;

    property Host: String read GetHost write SetHost;
  end;

const
  HTTPVerbDescriptions: Array[TMARSHttpVerb] of String = (
    'GET',
    'PUT',
    'POST',
    'HEAD',
    'DELETE',
    'PATCH'
  );

implementation

{ TPTLibMarsClient }

constructor TPTLibMarsClient.Create(const Host: String; const Application: String;
  const Threadsafe: Boolean; const UseCompression: Boolean; const MaxRequests: Integer;
  const OnError: TErrorProc);
begin
  FMaxRequests := MaxRequests;

  FDefaultErrorProc := OnError;

  FRequestPool := TMARSRequestPool.Create;
  FResponseQueue := TQueue<TResponseItem>.Create;
  FQueuedRequests := TQueue<TRequestItem>.Create;
  FLogDescription := 'RESTClient';

  FHost := Host;
  FApplication := Application;
  FUseThreadPool := True;

  FThreadsafe := ThreadSafe;
  FUseCompression := UseCompression;

  if FThreadSafe then
  begin
    FTimer := TPTLibGobalTimerController.NewTimer(Self, OnTimer, 10, True);
  end;
end;

procedure TPTLibMarsClient.DELETE(const Resource, Request: String;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Delete,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.DELETE(const Resource, Request: String;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Delete,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.DELETE(const Resource: String;
  const Request: TStream; const Headers: TStrings;
  const OnResponse: TResponseProc; const OnError: TErrorProc;
  const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Delete,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.DELETE(const Resource: String; const Request: TStream;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Delete,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

destructor TPTLibMarsClient.Destroy;
begin
  FTimer := nil;

  FreeAndNil(FResponseQueue);
  FreeAndNil(FRequestPool);
  FreeAndNil(FQueuedRequests);

  inherited;
end;

procedure TPTLibMarsClient.AddHeaders(const ClientResource: TMARSClientResource; const Headers: TStrings);
var
  i: Integer;
begin
  if (Headers <> nil) and
     (ClientResource.Client is TMARSNetClient) then
  begin
    for i := 0 to pred(Headers.Count) do
    begin
      TMARSNetClient(ClientResource.Client).HTTPClient.CustomHeaders[Headers.KeyNames[i]] := Headers.Values[Headers.KeyNames[i]];
    end;
  end;
end;

function TPTLibMarsClient.GET(const Resource: String; const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Get,
      Resource,
      nil,
      Headers
    ),
    OnBeforeRequest
  );
end;

function TPTLibMarsClient.GetActiveConnections: Integer;
begin
  Result := FRequestPool.ActiveConnectionCount;
end;

function TPTLibMarsClient.GetHost: String;
begin
  Result := FHost;
end;

function TPTLibMarsClient.GetPooledConnections: Integer;
begin
  Result := FRequestPool.PooledConnectionCount;
end;

function TPTLibMarsClient.GetUseThreadPool: Boolean;
begin
  Result := FUseThreadPool;
end;

procedure TPTLibMarsClient.GET(const ChunkCount: Integer;
  const GetResourceProc: TGetResourceProc; const Headers: TStrings;
  const OnResponse: TChunkResponseProc; const OnError: TErrorProc;
  const OnBeforeRequest: TBeforeRequestProc; const OnProgress: TOnProgress;
  const ID: TRequestID);

var
  Responses: TArray<IMarsClientResponse>;
  ResponseCount: Integer;
  Failed: Boolean;

  procedure GetResource(const ChunkIndex: Integer; const ChunkResource: String);
  begin
    GET(
      ChunkResource,
      Headers,
      procedure(const ChunkID: String; Response: IMarsClientResponse)
      var
        ChunkIndex: Integer;
      begin
        if not Failed then
        begin
          ChunkIndex := StrToInt(ChunkID);
          Inc(ResponseCount);
          OnProgress(ResponseCount, ChunkCount);
          Responses[ChunkIndex] := Response;

          if ResponseCount = ChunkCount then
          begin
            OnResponse(
              ID,
              Responses
            );
          end;
        end;
      end,
      procedure(const ChunkID: String; const e: Exception; Response: IMarsClientResponse)
      begin
        if not Failed then
        begin
          Failed := True;

          OnError(
            ID,
            e,
            Response
          );
        end;
      end,
      OnBeforeRequest,
      ChunkIndex.ToString
    );
  end;

var
  i: Integer;
  ChunkResource: String;
begin
  SetLength(Responses, ChunkCount);
  ResponseCount := 0;
  Failed := False;

  for i := 0 to pred(ChunkCount) do
  begin
    ChunkResource := '';

    GetResourceProc(
      i,
      ChunkResource
    );

    GetResource(i, ChunkResource);
  end;
end;

procedure TPTLibMarsClient.OnTimer(Sender: TObject);
var
  ResponseItem: TResponseItem;
begin
  TMonitor.Enter(FResponseQueue);
  try
    if FResponseQueue.Count > 0 then
    begin
      ResponseItem := FResponseQueue.Dequeue;

      if ResponseItem.e <> nil then
      begin
        if Assigned(ResponseItem.OnError) then
        begin
          ResponseItem.OnError(ResponseItem.ID, ResponseItem.e, ResponseItem.Response);
        end;

        if Assigned(FDefaultErrorProc) then
        begin
          FDefaultErrorProc(ResponseItem.ID, ResponseItem.e, ResponseItem.Response);
        end;
      end else
      if Assigned(ResponseItem.OnResponse) then
      begin
        ResponseItem.OnResponse(ResponseItem.ID, ResponseItem.Response);
      end;
    end;
  finally
    TMonitor.Exit(FResponseQueue);
  end;

  ProcessRequestQueue;
end;

function TPTLibMarsClient.POST(const Resource, Request: String;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Post,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.POST(const Resource: String; const Request: String;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Post,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

procedure TPTLibMarsClient.GET(const Resource: String; const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestASync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Get,
      Resource,
      nil,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

procedure TPTLibMarsClient.QueueResponse(const ID: TRequestID; Response: IMarsClientResponse; const e: Exception; const OnResponse: TResponseProc; const OnError: TErrorProc);
var
  ResponseItem: TResponseItem;
begin
  if FResponseQueue <> nil then
  begin
    if FThreadSafe then
    begin
      ResponseItem.ID := ID;
      ResponseItem.Response := Response;
      ResponseItem.e := e;
      ResponseItem.OnResponse := OnResponse;
      ResponseItem.OnError := OnError;

      TMonitor.Enter(FResponseQueue);
      try
        FResponseQueue.Enqueue(ResponseItem);
      finally
        TMonitor.Exit(FResponseQueue);
      end;
    end
    else
    begin
      if e <> nil then
      begin
        if Assigned(OnError) then
        begin
          OnError(ID, e, Response);
        end;
      end else

      if Response <> nil then
      begin
        if Assigned(OnResponse) then
        begin
          OnResponse(ID, Response);
        end;
      end;
    end;
  end;
end;

procedure TPTLibMarsClient.SetHost(const Value: String);
begin
  FHost := Value;
end;

procedure TPTLibMarsClient.SetUseThreadPool(const Value: Boolean);
begin
  FUseThreadPool := Value;
end;

procedure TPTLibMarsClient.HTTPRequestInternal(const HTTPRequestType: TMARSHttpVerb; const Resource: String; const Request: TStream; const Headers: TStrings;
  const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
var
  MARSRequest: TMARSRequest;
  Response: IMarsClientResponse;
  Params: IParameters;
  Stopwatch: TStopwatch;
begin
  MARSRequest := FRequestPool.Get;
  try
    try
      if pos('http', LowerCase(Resource)) = low(Resource) then
      begin
        MARSRequest.ClientResource.SpecificURL := Resource;
      end
      else
      begin
        MARSRequest.MARSClient.MARSEngineURL := FHost;
        MARSRequest.MARSApplication.AppName := FApplication;
        MARSRequest.ClientResource.Resource := Resource;
      end;

      AddHeaders(MARSRequest.ClientResource, Headers);

      if TGlobalLog.Logging(LogSeverityDebug2) then
      begin
        TGlobalLog.Log(
          FLogDescription,
          '%s - %s - %s',
          [HTTPVerbDescriptions[HTTPRequestType],
           MARSRequest.ClientResource.URL,
           TMarsUtils.StreamToString(Request, MaxParamLength)],
          NewParameters.
            SetParam('method', HTTPVerbDescriptions[HTTPRequestType]).
            SetParam('url', MARSRequest.ClientResource.URL).
            SetParam('content', TMARSUtils.StreamToString(Request, MaxParamLength)),
          LogSeverityDebug2);
      end;

      if Assigned(OnBeforeRequest) then
      begin
        OnBeforeRequest(ID, MARSRequest.ClientResource);
      end;

      Response := TMarsClientResponse.Create;
      Response.Resource := Resource;

      Stopwatch.Reset;
      Stopwatch.Start;

      case HTTPRequestType of
        TMARSHttpVerb.Get: MARSRequest.ClientResource.Client.Get(MARSRequest.ClientResource.URL, Response.ResponseStream, MARSRequest.ClientResource.AuthToken, MARSRequest.ClientResource.Accept, MARSRequest.ClientResource.ContentType);
        TMARSHttpVerb.Put: MARSRequest.ClientResource.Client.Put(MARSRequest.ClientResource.URL, Request, Response.ResponseStream, MARSRequest.ClientResource.AuthToken, MARSRequest.ClientResource.Accept, MARSRequest.ClientResource.ContentType);
        TMARSHttpVerb.Post: MARSRequest.ClientResource.Client.Post(MARSRequest.ClientResource.URL, Request, Response.ResponseStream, MARSRequest.ClientResource.AuthToken, MARSRequest.ClientResource.Accept, MARSRequest.ClientResource.ContentType);
        TMARSHttpVerb.Delete: MARSRequest.ClientResource.Client.Delete(MARSRequest.ClientResource.URL, Request, Response.ResponseStream, MARSRequest.ClientResource.AuthToken, MARSRequest.ClientResource.Accept, MARSRequest.ClientResource.ContentType);
      end;

      Response.ResponseCode := MARSRequest.ClientResource.Client.ResponseStatusCode;
      Response.ResponseStatus := MARSRequest.ClientResource.Client.ResponseText;

      if TGlobalLog.Logging(LogSeverityDebug2) then
      begin
        TGlobalLog.Log(
          FLogDescription,
          '%s Response - %s - %s',
          [HTTPVerbDescriptions[HTTPRequestType],
           Resource,
           TMarsUtils.StreamToString(Response.ResponseStream, MaxParamLength)],
          NewParameters.
            SetParam('method', HTTPVerbDescriptions[HTTPRequestType]).
            SetParam('url', MARSRequest.ClientResource.URL).
            SetParam('response_ms', Stopwatch.ElapsedMilliseconds),
          LogSeverityDebug2);
      end;

      if Assigned(OnResponse) then
      begin
        OnResponse(ID, Response);
      end;
    except
      on e: Exception do
      begin
        if e is EMARSClientHttpException then
        begin
          Response.ResponseCode := (e as EMARSClientHttpException).StatusCode;
        end;

        Params := NewParameters.
          SetParam('error', e.Message);

        if Response.ResponseStream <> nil then
        begin
          Params.SetParam('response', Response.ResponseString);

          if Response.ResponseString <> '' then
          begin
            e.Message := Response.ResponseString;
          end;
        end;

        TGlobalLog.Log(
          FLogDescription,
          'Error - %s',
          [e.Message],
          Params,
          LogSeverityError);

        if Assigned(OnError) then
        begin
          OnError(ID, Exception(AcquireExceptionObject), Response);
        end
        else
        begin
          e.Message := e.Message;

          raise;
        end;
      end;
    end;
  finally
    FRequestPool.Return(MARSRequest);
  end;
end;

procedure TPTLibMarsClient.HEAD(const Resource, Request: String;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Head,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.HEAD(const Resource, Request: String;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Head,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.HEAD(const Resource: String; const Request: TStream;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Head,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.HEAD(const Resource: String; const Request: TStream;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Head,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.ProcessRequestQueue;
var
  RequestItem: TRequestItem;
begin
  if ((FMaxRequests = 0) or
      (ActiveConnectionCount < FMaxRequests)) and
     (FQueuedRequests.Count > 0) then
  begin
    RequestItem := FQueuedRequests.Extract;

    HTTPRequestAsyncInternal(
      RequestItem.Request,
      RequestItem.OnResponse,
      RequestItem.OnError,
      RequestItem.OnRequest,
      RequestItem.ID
    )
  end;
end;

procedure TPTLibMarsClient.HTTPRequestAsync(
  Request: IMarsClientRequest;
  const OnResponse: TResponseProc; const OnError: TErrorProc;
  const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
var
  RequestItem: TRequestItem;
begin
  RequestItem.ID := ID;
  RequestItem.Request := Request;
  RequestItem.OnRequest := OnBeforeRequest;
  RequestItem.OnResponse := OnResponse;
  RequestItem.OnError := OnError;

  FQueuedRequests.Enqueue(RequestItem);

  //ProcessRequestQueue;
end;

procedure TPTLibMarsClient.HTTPRequestAsyncInternal(Request: IMarsClientRequest;
  const OnResponse: TResponseProc; const OnError: TErrorProc;
  const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
var
  Task: ITask;
begin
  if FUseThreadPool then
  begin
    Task := TTask.Create(
      procedure()
      begin
        HTTPRequestInternal(
          Request.HTTPRequestType,
          Request.Resource,
          Request.RequestStream,
          Request.Headers,
          procedure(const ID: TRequestID; Response: IMarsClientResponse)
          begin
            QueueResponse(ID, Response, nil, OnResponse, nil);
          end,
          procedure(const ID: TRequestID; const e: Exception; Response: IMarsClientResponse)
          begin
            QueueResponse(ID, Response, e, nil, OnError);
          end,
          OnBeforeRequest,
          ID);
      end
    );

    Task.Start;
  end
  else
  begin
    TThread.CreateAnonymousThread(
      procedure()
      begin
        HTTPRequestInternal(
          Request.HTTPRequestType,
          Request.Resource,
          Request.RequestStream,
          Request.Headers,
          procedure(const ID: TRequestID; Response: IMarsClientResponse)
          begin
            QueueResponse(ID, Response, nil, OnResponse, nil);
          end,
          procedure(const ID: TRequestID; const e: Exception; Response: IMarsClientResponse)
          begin
            QueueResponse(ID, Response, e, nil, OnError);
          end,
          OnBeforeRequest);
      end
    ).Start;
  end;
end;

function TPTLibMarsClient.HTTPRequestSync(Request: IMarsClientRequest;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
var
  LocalResponse: IMarsClientResponse;
  LocalError: Exception;
begin
  Result := nil;
  LocalError := nil;

  HTTPRequestInternal(
    Request.HTTPRequestType,
    Request.Resource,
    Request.RequestStream,
    Request.Headers,
    procedure(const ID: TRequestID; Response: IMarsClientResponse)
    begin
      LocalResponse := Response;
    end,
    procedure(const ID: TRequestID; const e: Exception; Response: IMarsClientResponse)
    begin
      LocalError := e;
      LocalResponse := Response;
    end,
    OnBeforeRequest);

  if LocalError <> nil then
  begin
    if Assigned(FDefaultErrorProc) then
    begin
      FDefaultErrorProc('',  LocalError, LocalResponse);
    end;

    raise LocalError;
  end
  else
  begin
    Result := LocalResponse;
  end;
end;

procedure TPTLibMarsClient.POST(const Resource: String; const Request: TStream;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Post,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.PATCH(const Resource: String; const Request: TStream;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Patch,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.PATCH(const Resource: String; const Request: TStream;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Patch,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.PATCH(const Resource, Request: String;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Patch,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.PATCH(const Resource, Request: String;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Patch,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.POST(const Resource: String; const Request: TStream;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Post,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

function TPTLibMarsClient.PUT(const Resource: String; const Request: TStream;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Put,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.PUT(const Resource: String; const Request: TStream;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Put,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

function TPTLibMarsClient.PUT(const Resource, Request: String;
  const Headers: TStrings;
  const OnBeforeRequest: TBeforeRequestProc): IMarsClientResponse;
begin
  Result := HTTPRequestSync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Put,
      Resource,
      Request,
      Headers
    ),
    OnBeforeRequest
  );
end;

procedure TPTLibMarsClient.PUT(const Resource, Request: String;
  const Headers: TStrings; const OnResponse: TResponseProc;
  const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
begin
  HTTPRequestAsync(
    TMarsClientRequest.Create(
      TMARSHttpVerb.Put,
      Resource,
      Request,
      Headers
    ),
    OnResponse,
    OnError,
    OnBeforeRequest,
    ID
  );
end;

{ TMARSRequest }

constructor TMARSRequest.Create;
begin
  FMARSClient := TMARSNetClient.Create(nil);
  FMARSClient.HttpClient.AutomaticDecompression := [THTTPCompressionMethod.GZip];

  FMARSApplication := TMARSClientApplication.Create(nil);
  FMARSApplication.Client := FMARSClient;

  FMARSToken := TMARSClientToken.Create(nil);

  FClientResource := TMARSClientResource.Create(nil);
  FClientResource.Application := FMARSApplication;
  FClientResource.Token := FMARSToken;
end;

destructor TMARSRequest.Destroy;
begin
  FreeAndNil(FMARSApplication);
  FreeAndNil(FMARSClient);
  FreeAndNil(FMARSToken);
  FreeAndNil(FClientResource);

  inherited;
end;

{ TMARSRequestPool }

function TMARSRequestPool.ActiveConnectionCount: Integer;
begin
  TInterlocked.Exchange(Result, FActiveConnectionCount);
end;

constructor TMARSRequestPool.Create;
begin
  FRequestPool := TObjectStack<TMARSRequest>.Create(True);
  FMaxPoolCount := 40;
end;

destructor TMARSRequestPool.Destroy;
begin
  FreeAndNil(FRequestPool);

  inherited;
end;

function TMARSRequestPool.Get: TMARSRequest;
begin
  TMonitor.Enter(FRequestPool);
  try
    if FRequestPool.Count = 0 then
    begin
      Result := TMARSRequest.Create;
    end
    else
    begin
      Result := FRequestPool.Extract;
    end;

    TMARSNetClient(Result.ClientResource.Client).HTTPClient.CustHeaders.Clear;
  finally
    TMonitor.Exit(FRequestPool);
  end;

  TInterlocked.Increment(FActiveConnectionCount);
end;

function TMARSRequestPool.PooledConnectionCount: Integer;
begin
  TMonitor.Enter(FRequestPool);
  try
    Result := FRequestPool.Count;
  finally
    TMonitor.Exit(FRequestPool);
  end;
end;

procedure TMARSRequestPool.Return(const Value: TMARSRequest);
begin
  if Self = nil then
  begin
    Value.Free;
  end
  else
  begin
    TMonitor.Enter(FRequestPool);
    try
      if FRequestPool.Count >= FMaxPoolCount then
      begin
        Value.Free;
      end
      else
      begin
        FRequestPool.Push(Value);
      end;
    finally
      TMonitor.Exit(FRequestPool);
    end;

    TInterlocked.Decrement(FActiveConnectionCount);
  end;
end;

end.

