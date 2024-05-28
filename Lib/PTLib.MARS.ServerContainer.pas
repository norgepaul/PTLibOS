unit PTLib.MARS.ServerContainer;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, System.SyncObjs,
  System.ZLib, System.Zip,

  IdContext, IdHTTPWebBrokerBridge, IdHTTPHeaderInfo,

  Web.HTTPApp,

  MARS.Core.JSON, MARS.Core.Registry, MARS.Core.Attributes, MARS.Core.MediaType,
  MARS.Data.MessageBodyWriters, MARS.http.Server.Indy,
  MARS.Core.Engine, MARS.Core.URL, MARS.JOSEJWT.Token, MARS.Core.Token,
  MARS.Core.Token.Resource, MARS.Core.Application, MARS.Core.Activation,
  MARS.Core.Activation.Interfaces, MARS.Core.Utils, MARS.Core.RequestAndResponse.Interfaces,

  PTLib.Common.Classes,
  PTLib.Common.Log,
  PTLib.Common.Dates,
  PTLib.Common.Strings,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,
  PTLib.Common.ServerContainer.Service,

  PTLib.Common.Network.Types,

  PTLib.MARS.Types,
  PTLib.MARS.Classes,
  PTLib.MARS.Interfaces;

type
  [Path('token')]
  TRestToken = class(TMARSTokenResource)
  private
    //[Context] App: TMARSApplication;
  protected
    function Authenticate(const AUserName, APassword: string): Boolean; override;
  end;

  TIdHTTPAppRequestHelper = class helper for TIdHTTPAppRequest
  public
    function GetRequestInfo: TIdEntityHeaderInfo;
  end;

  TMarsServerContainerService = class(TServerContainerService,
                                      IMarsServerContainerService)
  strict private
    FMarsServer: TMARShttpServerIndy;
    FMarsEngine: TMARSEngine;
    FRestToken: TRestToken;
    FPort: Integer;
    FCompress: Boolean;
    FLogName: String;

    FApplications: IList<TObject>;

    class var FCounterActiveConnections: Int64;
    class var FCounterInvokeError: Int64;
    class var FCounterServerException: Int64;
    class var FCounterStartedUTC: Double;
    class var FCounterRequests: Int64;

    procedure ZipStream(const ASource, ADest: TStream; const WindowBits: Integer = 15);
  private
    procedure StartServer(const Port: Integer);
    procedure StopServer;
    procedure OnServerException(AContext: TIdContext; AException: Exception);
    procedure InvokeErrorHandler(const AActivation: IMARSActivation; const AException: Exception; var AHandled: Boolean);

    class function GetCounterActiveConnections: Int64; static;
    class function GetCounterInvokeErrors: Int64; static;
    class function GetCounterServerExceptions: Int64; static;
    class function GetCounterRequests: Int64; static;
    class function GetCounterStartedUTC: TDateTime; static;
  protected
    const MaxParamLength = 300;
  protected
    procedure DoActiveChanged(const Value: Boolean); override;
    procedure DoGetInformation(Value: IInformationList); override;
    procedure DoGetServiceName(out Value: String); override;

    procedure AddApplicationClass(const ApplicationClass: TShareBikeBridgeServiceRESTApplicationClass;
      const AName, ABasePath: String; const AResources: array of String; const AParametersSliceName: String); overload;
    procedure AddApplicationClass(const ApplicationClass: TShareBikeBridgeServiceRESTApplicationClass); overload;
  public
    constructor Create(const Port: Integer; const BasePath: String = '/rest'; const Compress: Boolean = True; const LogName: String = 'REST Server');
    destructor Destroy; override;

    class procedure ResetCounters;

    class property CounterStartedUTC: TDateTime read GetCounterStartedUTC;
    class property CounterActiveConnections: Int64 read GetCounterActiveConnections;
    class property CounterInvokeErrors: Int64 read GetCounterInvokeErrors;
    class property CounterServerExceptions: Int64 read GetCounterServerExceptions;
    class property CounterRequests: Int64 read GetCounterRequests;
  end;

implementation

{ TShareBikeRESTBridge }
procedure TMarsServerContainerService.AddApplicationClass(const ApplicationClass: TShareBikeBridgeServiceRESTApplicationClass; const AName, ABasePath: String;
  const AResources: array of String; const AParametersSliceName: String);
begin
  FApplications.Add(ApplicationClass.Create);

  FMarsEngine.AddApplication(AName, ABasePath, AResources, AParametersSliceName);
end;

procedure TMarsServerContainerService.AddApplicationClass(const ApplicationClass: TShareBikeBridgeServiceRESTApplicationClass);
var
  AppName,
  AppBasePath,
  AppParameterSliceName: String;
  AppResources: TArray<String>;
begin
  if ApplicationClass.GetDefaultSettings(
    AppName,
    AppBasePath,
    AppResources,
    AppParameterSliceName
  ) then
  begin
    AddApplicationClass(
      ApplicationClass,
      AppName,
      AppBasePath,
      AppResources,
      AppParameterSliceName);
  end
  else
  begin
    raise EMarsServiceException.CreateFmt('No default parameters for REST application "%s"', [ApplicationClass.ClassName]);
  end;
end;

class procedure TMarsServerContainerService.ResetCounters;
var
  LocalDateTime: Double;
begin
  TInterlocked.Exchange(FCounterActiveConnections, 0);
  TInterlocked.Exchange(FCounterInvokeError, 0);
  TInterlocked.Exchange(FCounterServerException, 0);
  TInterlocked.Exchange(FCounterRequests, 0);

  LocalDateTime := nowUTC;
  TInterlocked.Exchange(FCounterStartedUTC, LocalDateTime);
end;

procedure TMarsServerContainerService.ZipStream(const ASource: TStream; const ADest: TStream; const WindowBits: Integer);
var
  LZipStream: TZCompressionStream;
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ADest));

  LZipStream := TZCompressionStream.Create(ADest, TZCompressionLevel.zcDefault, WindowBits);
  try
    ASource.Position := 0;
    LZipStream.CopyFrom(ASource, ASource.Size);
  finally
    LZipStream.Free;
  end;
end;

constructor TMarsServerContainerService.Create(const Port: Integer; const BasePath: String; const Compress: Boolean; const LogName: String);
begin
  FPort := Port;
  FCompress := Compress;
  FLogName := LogName;

  FMarsEngine := TMARSEngine.Create;
  FMarsEngine.BasePath := BasePath;
  FMarsServer := TMARShttpServerIndy.Create(FMarsEngine);
  FRestToken := TRestToken.Create;
  FMarsServer.OnException := OnServerException;

  FApplications := TList<TObject>.Create;

  ResetCounters;

  if FCompress then
  begin
    TMARSActivation.RegisterBeforeInvoke(
      procedure(const AActivation: IMARSActivation; out AIsAllowed: Boolean)
      var
        Parameters: IParameters;
        Content, HeaderName: String;
        i: Integer;
        WebRequest: TWebRequest;
      begin
        TInterlocked.Increment(FCounterActiveConnections);
        TInterlocked.Increment(FCounterRequests);

        if TGLobalLog.Logging(LogSeverityDebug3) then
        begin
          if SameText(AActivation.Request.GetHeaderParamValue('Content-Type'), 'application/json') then
          begin
            try
              Content := copy(AActivation.Request.Content, 1, MaxParamLength);
            except
              Content := '<Binary>';
            end;
          end
          else
          begin
            Content := '<Binary>';
          end;

          WebRequest := (AActivation.Request.AsObject as TMarsWebRequest).WebRequest as TWebRequest;

          Parameters :=
            NewParameters.
              SetParam('request', AActivation.Request.Method).    // Was HTTPMethodTypeDescriptions[AActivation.Request.MethodType]
              SetParam('ip', WebRequest.RemoteIP).
              SetParam('url', AActivation.URL.URL).
              SetParam('content', Content);

          for i := 0 to pred(TIdHTTPAppRequest(WebRequest).GetRequestInfo.RawHeaders.Count) do
          begin
            HeaderName := TIdHTTPAppRequest(WebRequest).GetRequestInfo.RawHeaders.Names[I];

            Parameters.SetParam(
              'HEADER-' + HeaderName,
              TIdHTTPAppRequest(WebRequest).GetRequestInfo.RawHeaders.Values[HeaderName]);
          end;

          TGLobalLog.Log(
            Self,
            '%s Request - %s - %s',
            [AActivation.Request.Method,
             AActivation.URL.URL,
             Content],
            Parameters,
            LogSeverityDebug3);
        end;
      end
    );

    TMARSActivation.RegisterAfterInvoke(
      procedure (const AActivation: IMARSActivation)
      var
        LOutputStream: TBytesStream;
        Parameters: IParameters;
        Response: String;
      begin
        TInterlocked.Decrement(FCounterActiveConnections);

        if TGLobalLog.Logging(LogSeverityDebug3) then
        begin
          Response := TMarsUtils.StreamToString(AActivation.Response.ContentStream, MaxParamLength);

          Parameters :=
            NewParameters.
              SetParam('request', AActivation.Request.Method). // Was MethodType
              SetParam('ip', ((AActivation.Request.AsObject as TMarsWebRequest).WebRequest as TWebRequest).RemoteIP).
              SetParam('url', AActivation.URL.URL).
              SetParam('status_code', AActivation.Response.StatusCode).
              SetParam('response', Response).
              SetParam('size_original', AActivation.Response.ContentStream.Size);
        end;

        if ContainsText(AActivation.Request.GetHeaderParamValue('Accept-Encoding'), 'gzip')  then
        begin
          LOutputStream := TBytesStream.Create(nil);
          try
            ZipStream(AActivation.Response.ContentStream, LOutputStream);

            // Only return compressed data if it's worth it.
            if AActivation.Response.ContentStream.Size > LOutputStream.Size then
            begin
              if TGLobalLog.Logging(LogSeverityDebug3) then
              begin
                Parameters.SetParam('size_compressed', LOutputStream.Size);
              end;

              AActivation.Response.ContentStream.Free;
              AActivation.Response.ContentStream := LOutputStream;
              AActivation.Response.ContentEncoding := 'gzip';
            end
            else
            begin
              // Free the stream as it was bigger than the original
              LOutputStream.Free;
            end;
          except
            LOutputStream.Free;
            raise;
          end;

        if TGLobalLog.Logging(LogSeverityDebug3) then
        begin
          TGLobalLog.Log(
            Self,
            '%s Response - %s - %s',
            [AActivation.Request.Method, // Was MethodType
             AActivation.URL.URL,
             Response],
            Parameters,
            LogSeverityDebug3);
          end;
        end;
      end
    );
  end;

  TMARSActivation.RegisterInvokeError(InvokeErrorHandler);
end;

destructor TMarsServerContainerService.Destroy;
var
  i: Integer;
begin
  Active := False;

  for i := 0 to pred(FApplications.Count) do
  begin
    FApplications[i].Free;
  end;

  FreeAndNil(FRestToken);
  FreeAndNil(FMarsServer);
  FreeAndNil(FMarsEngine);

  inherited;
end;

procedure TMarsServerContainerService.InvokeErrorHandler(const AActivation: IMARSActivation;
  const AException: Exception; var AHandled: Boolean);
begin
  TGLobalLog.Log(
    Self,
    'Invoke Error - %s',
    [AException.Message],
    NewParameters.
      SetParam('error', AException.Message).
      SetParam('method', AActivation.Request.Method). // Was MethodType
      //SetParam('content', AActivation.Request.Content).
      SetParam('url', AActivation.URL.URL),
    LogSeverityError);

  TInterlocked.Increment(FCounterInvokeError);
  TInterlocked.Decrement(FCounterActiveConnections);
end;

procedure TMarsServerContainerService.OnServerException(AContext: TIdContext; AException: Exception);
begin
  TGLobalLog.Log(
    AContext,
    'Server Exception - %s',
    [AException.Message],
    NewParameters.
      SetParam('error', AException.Message).
      SetParam('ip', AContext.Binding.IP),
    LogSeverityError);

  TInterlocked.Increment(FCounterServerException);
//  TInterlocked.Decrement(FCounterActiveConnections);
end;

procedure TMarsServerContainerService.DoActiveChanged(const Value: Boolean);
begin
  inherited;

  if Value then
  begin
    StartServer(FPort);
  end
  else
  begin
    StopServer;
  end;
end;

procedure TMarsServerContainerService.DoGetInformation(Value: IInformationList);
begin
  inherited;

end;

procedure TMarsServerContainerService.DoGetServiceName(out Value: String);
begin
  Value := FLogName;
end;

class function TMarsServerContainerService.GetCounterActiveConnections: Int64;
begin
  TInterlocked.Exchange(Result, FCounterActiveConnections);
end;

class function TMarsServerContainerService.GetCounterInvokeErrors: Int64;
begin
  TInterlocked.Exchange(Result, FCounterInvokeError);
end;

class function TMarsServerContainerService.GetCounterRequests: Int64;
begin
  TInterlocked.Exchange(Result, FCounterRequests);
end;

class function TMarsServerContainerService.GetCounterServerExceptions: Int64;
begin
  TInterlocked.Exchange(Result, FCounterServerException);
end;

class function TMarsServerContainerService.GetCounterStartedUTC: TDateTime;
var
  LocalDateTime: Double;
begin
  TInterlocked.Exchange(LocalDateTime, FCounterStartedUTC);

  Result := TDateTime(LocalDateTime);
end;

procedure TMarsServerContainerService.StartServer(const Port: Integer);
var
  MarsApplication, MarsResource: String;
begin
  TGLobalLog.Log(Self, 'Starting REST server on port %d', [Port]);

  FMarsEngine.Port := Port;

  // Skip favicon requests (browser)
  FMarsEngine.BeforeHandleRequest :=
    function(const AEngine: TMARSEngine; const AURL: TMARSURL; const ARequest: IMARSRequest;
       const AResponse: IMARSResponse; var Handled: Boolean): Boolean
    begin
      Result := True;

      if (SameText(AURL.Document, 'favicon.ico')) or
         (SameText(AURL.Document, 'robots.txt')) then
      begin
        Result := False;

        Handled := True;
      end;
      (*else
      begin
        TGLobalLog.Log(
          Self,
          '%s Request - %s',
          [HTTPMethodTypeDescriptions[ARequest.MethodType],
           AURL.URL],
          NewParameters.
            SetParam('method', MethodTypeDescriptions[ARequest.MethodType]).
            SetParam('ip', ARequest.RemoteIP).
            SetParam('url', AURL.URL).
            SetParam('content', TrimText(ARequest.Content)),
          LogSeverityDebug3);
      end; *)
    end;

  FMarsServer.DefaultPort := FMarsEngine.Port;

  for MarsApplication in FMarsEngine.Applications.Keys do
  begin
    TGLobalLog.Log(Self, 'Endpoints available at "http://127.0.0.1:%d%s%s"', [Port, FMARSEngine.BasePath, FMarsEngine.Applications[MarsApplication].BasePath]);

    for MarsResource in FMarsEngine.Applications[MarsApplication].Resources.Keys do
    begin
      TGLobalLog.Log(Self, '- /%s', [MarsResource]);
    end;
  end;

  FMarsServer.Active := True;
end;

procedure TMarsServerContainerService.StopServer;
begin
  FMarsServer.Active := False;
end;

{ TRestToken }

function TRestToken.Authenticate(const AUserName, APassword: string): Boolean;
begin
  { TODO : Authenticate }
  Result := True;
end;

{ TIdHTTPAppRequestHelper }

function TIdHTTPAppRequestHelper.GetRequestInfo: TIdEntityHeaderInfo;
begin
  Result := FRequestInfo;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TRestToken>;

end.
