unit invoketest;

interface
uses
  Sysutils, classes,

  Vcl.dialogs,

  DUnitX.TestFramework,

  IdBaseComponent, IdComponent, IdServerIOHandler, IdSSL,
  IdSSLOpenSSL, IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, IdContext,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdTCPConnection,
  IdTCPClient, IdHTTP,

  PTLib.Common.Network,
  PTLib.Common.Network.Rest.Client,
  PTLib.Common.Network.Rest.Messages,
  PTLib.Common.Network.Rest.MessageThreads,
  PTLib.Common.Network.Rest.Server,

  PTLib.Common.Variants,
  PTLib.Common.Classes,
  PTLib.Common.Strings,
  PTLib.Common.LockedTypes,
  PTLib.Common.Log,
  PTLib.Common.Log.LocalFile,
  PTLib.Common.Files,
  PTLib.Common.Interfaces,
  PTLib.Common.FIFOQueue.MessageList,
  PTLib.Common.FIFOQueue;

type
  TRESTProc = reference to procedure(const Client: TIdRestClient);

  [TestFixture]
  TRESTInvokeTest = class(TObject)
  private
    FServer:  TPTLibIdRestServer;

    // Rest server method handlers
    procedure HandleProcedureInvoke(const Info: TIdRestSchemaMethodCallData);
    procedure HandleFunctionInvoke(const Info: TIdRestSchemaMethodCallData);
    procedure HandlePostInvoke(const Info: TIdRestSchemaMethodCallData);
    procedure HandlePostattachmentInvoke(const Info: TIdRestSchemaMethodCallData);

    // HTTP server resource request handler ( GET / HEAD / PEEK)
    procedure HandleHTTPGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function NewRESTMethod(const ASchemaService: TIdRestSchemaService;
      const MethodName: String; const CallBackMethod: TIdRestExecuteEntry): TIdRestSchemaMethod;
    procedure TestClientRESTMethod(const RESTProc: TRESTProc);
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('REST procedure call','1')]
    [TestCase('REST procedure call','3')]
    procedure TestRestProcedureCall(const Value: Integer);

    [Test]
    [TestCase('REST function call','9')]
    [TestCase('REST function call','12')]
    procedure TestRestFunctionCall(const Value: Integer);

    [Test]
    [TestCase('REST post call','24')]
    [TestCase('REST post call','23')]
    procedure TestRestPost(const Value: Integer);

    [Test]
    [TestCase('REST post-attachment call','10')]
    [TestCase('REST post-attachment call','24')]
    procedure TestRestPostWithAttachment(const Value: integer);

    [Test]
    [TestCase('HTTP file-get','index.html')]
    [TestCase('HTTP file-get','calc.html')]
    procedure TestHTTPGet(const Document: String);

    [Test]
    [TestCase('HTTP file-head','index.html')]
    procedure TestHTTPHead(const Document: String);
  end;

implementation

var
  TestPort: Integer;

{ TRESTInvokeTest }

function TRESTInvokeTest.NewRESTMethod(const ASchemaService: TIdRestSchemaService;
  const MethodName: String; const CallBackMethod: TIdRestExecuteEntry): TIdRestSchemaMethod;
begin
  Result := TIdRestSchemaMethodGet.Create(ASchemaService);
  Result.RestMethodName.Value := MethodName;
  Result.HandleWith(CallBackMethod);
  Result.Parameters.Add('id', TIdRestSchemaParameterType.rpNumber);
end;

procedure TRESTInvokeTest.Setup;
var
  SchemaService: TIdRestSchemaService;
begin
  Log('Server starting on port ' + TestPort.ToString);

  FServer:=TPTLibIdRestServer.Create(nil);
  FServer.DefaultPort := TestPort;
  FServer.OnCommandGet := HandleHTTPGet;

  SchemaService := TIdRestSchemaService.Create(FServer);
  SchemaService.ServiceName.Value := 'testapi';

  // Get Tests
  NewRESTMethod(SchemaService, 'testprocedure', HandleProcedureInvoke);
  NewRESTMethod(SchemaService, 'testfunction', HandlefunctionInvoke);

  // Post Tests
  NewRESTMethod(SchemaService, 'testpost', HandlePostInvoke);
  NewRESTMethod(SchemaService, 'testpostattachment', HandlePostattachmentInvoke);

  FServer.Active := true;
end;

procedure TRESTInvokeTest.HandleHTTPGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  Log('HTTP resurce request for [' + ARequestInfo.Document + '] - OK');
  AResponseInfo.ContentText := '<html><body> Document Name is ' + ARequestInfo.Document + '</body></html>';
  AResponseInfo.ContentType := 'text/html';
  AResponseInfo.WriteHeader;
  AResponseInfo.WriteContent;
end;

procedure TRESTInvokeTest.HandlePostattachmentInvoke(const Info: TIdRestSchemaMethodCallData);
var
  RestSuccessMessage: TIdRestSuccessMessage;
  Data: TStringStream;
begin
  if Info.Attachment <> nil then
  begin
    Info.Attachment.Position := 0;
    Data := TStringStream.Create;
    Data.CopyFrom(Info.Attachment,Info.Attachment.Size);
    Log(#13 + 'Server recieved attachment [' + Data.DataString + '] - OK');
  end;

  RestSuccessMessage := TIdRestSuccessMessage.Create;
  try
    with Info.SocketInfo do
    begin
      Response.ContentText := RestSuccessMessage.Serialize(Info.Params.GetParam('id'));
      Response.WriteHeader;
      response.WriteContent;
    end;
  finally
    FreeAndNil(RestSuccessMessage);
  end;
end;

{ TODO : Can the next 3 methods not be merged? They all look identical. }
procedure TRESTInvokeTest.HandlePostInvoke(const Info: TIdRestSchemaMethodCallData);
var
  RestSuccessMessage: TIdRestSuccessMessage;
begin
  RestSuccessMessage := TIdRestSuccessMessage.Create;
  try
    with Info.SocketInfo do
    begin
      Response.ContentText := RestSuccessMessage.Serialize(Info.Params.GetParam('id'));
      Response.WriteHeader;
      response.WriteContent;
    end;
  finally
    FreeAndNil(RestSuccessMessage);
  end;
end;

procedure TRESTInvokeTest.HandleFunctionInvoke(const Info: TIdRestSchemaMethodCallData);
var
  RestSuccessMessage:  TIdRestSuccessMessage;
begin
  RestSuccessMessage := TIdRestSuccessMessage.Create;
  try
    with Info.SocketInfo do
    begin
      Response.ContentText := RestSuccessMessage.Serialize(Info.Params.GetParam('id'));
      Response.WriteHeader;
      response.WriteContent;
    end;
  finally
    FreeAndNil(RestSuccessMessage);
  end;
end;

procedure TRESTInvokeTest.HandleProcedureInvoke(const Info: TIdRestSchemaMethodCallData);
var
  RestSuccessMessage:  TIdRestSuccessMessage;
begin
  RestSuccessMessage := TIdRestSuccessMessage.Create;
  try
    with Info.SocketInfo do
    begin
      Response.ContentText := RestSuccessMessage.Serialize(Info.Params.GetParam('id'));
      Response.WriteHeader;
      response.WriteContent;
    end;
  finally
    FreeAndNil(RestSuccessMessage);
  end;
end;

procedure TRESTInvokeTest.TearDown;
begin
  if Assigned(FServer) then
  begin
    FServer.active := False;

    FreeAndnil(FServer);
  end;
end;

procedure TRESTInvokeTest.TestClientRESTMethod(const RESTProc: TRESTProc);
var
  Client:  TIdRestClient;
begin
  Client := TIdRestClient.Create(nil);
  try
    Client.Host := '127.0.0.1';
    Client.Port := TestPort;
    Client.APIEntry := 'REST';

    RESTProc(Client);
  finally
    FreeAndNil(Client);
  end;
end;

procedure TRESTInvokeTest.TestRestPost(const Value: Integer);
var
  Params: TIdRestClientParameters;
  RestMessage: TIdRestMessage;
begin
  TestClientRESTMethod(
    procedure(const Client: TIdRestClient)
    begin
      Params := TIdRestClientParameters.Create;
      try
        Params.Add('id', Value.ToString );
        Client.Execute('testapi','testpost',Params,nil,TIdRestClientInvocationType.ciPOST,RestMessage);
      finally
        FreeAndNil(Params);
      end;
    end);
end;

procedure TRESTInvokeTest.TestRestPostWithAttachment(const Value: integer);
var
  Params: TIdRestClientParameters;
  RestMessage: TIdRestMessage;
  Data: TStringStream;
begin
  TestClientRESTMethod(
    procedure(const Client: TIdRestClient)
    begin
      Params := TIdRestClientParameters.Create;
      try
        Params.Add('id', Value.ToString );
        Data := TStringStream.Create('this is a test');

        Client.Execute('testapi','testpostattachment',Params,Data,TIdRestClientInvocationType.ciPOST,RestMessage);

        if RestMessage<>nil then
        begin
          if not (RestMessage.TagValue = Value.ToString) then
          begin
            raise Exception.CreateFmt('Expected sent value [%s] not [%s]',[Value.toString,RestMessage.TagValue]);
          end else
          Log('Success for sending' + Value.ToString);
        end else
        begin
          Raise Exception.Create('Invokation returned no object error');
        end;
      finally
        FreeAndNil(Params);
      end;
    end);
end;

procedure TRESTInvokeTest.TestRestFunctionCall(const Value: Integer);
var
  Params: TIdRestClientParameters;
  RestMessage: TIdRestMessage;
begin
  TestClientRESTMethod(
    procedure(const Client: TIdRestClient)
    begin
      Params := TIdRestClientParameters.Create;
      try
        Params.Add('id', Value.ToString );
        Client.Execute('testapi','testfunction',Params,nil,TIdRestClientInvocationType.ciGET,RestMessage);
        if RestMessage<>nil then
        begin
          if not (RestMessage.TagValue = Value.ToString) then
          begin
            raise Exception.CreateFmt('Expected sent value [%s] not [%s]',[Value.toString,RestMessage.TagValue]);
          end else
          Log('Success for sending' + Value.ToString);
        end else
        begin
          Raise Exception.Create('Invokation returned no object error');
        end;
      finally
        FreeAndNil(Params);
      end;
    end);
end;

procedure TRESTInvokeTest.TestRestProcedureCall(const Value: Integer);
var
  Params: TIdRestClientParameters;
  RestMessage: TIdRestMessage;
begin
  TestClientRESTMethod(
    procedure(const Client: TIdRestClient)
    begin
      Params := TIdRestClientParameters.Create;
      try
        Params.Add('id', Value.ToString );
        Client.Execute('testapi','testprocedure',Params,nil,TIdRestClientInvocationType.ciGET,RestMessage);
        if RestMessage<>nil then
        begin
          if not (RestMessage.TagValue = Value.ToString) then
          begin
            raise Exception.CreateFmt('Expected sent value [%s] not [%s]',[Value.toString,RestMessage.TagValue]);
          end else
          Log('Success for sending' + Value.ToString);
        end else
        begin
          Raise Exception.Create('Invokation returned no object error');
        end;
      finally
        FreeAndNil(Params);
      end;
    end);
end;

procedure TRESTInvokeTest.TestHTTPHead(const Document: String);
var
  Client:  TIdHttp;
begin
  // The REST Server should handle normal HTTP resource requests
  // as long as the URI namespace does not start with /REST/
  Client := TIdHttp.Create(nil);
  try
    Log('Head request for document ' + Document + ' from server [HTTP GET]');
    Client.Head('http://127.0.0.1/' + Document);
  finally
    FreeAndNil(Client);
  end;
end;

procedure TRESTInvokeTest.TestHTTPGet(const Document: String);
var
  Client:  TIdHttp;
  Content: String;
begin
  // The REST Server should handle normal HTTP resource requests
  // as long as the URI namespace does not start with /REST/
  Client := TIdHttp.Create(nil);
  try
    Log('Reading document ' + Document + ' from server [HTTP GET]');
    Content := Client.Get('http://127.0.0.1/' + Document);

  finally
    FreeAndNil(Client);
  end;
end;

initialization
  // Find an available port
  TestPort := GetNextAvailableTCPPort(80);

  TDUnitX.RegisterTestFixture(TRESTInvokeTest);

end.
