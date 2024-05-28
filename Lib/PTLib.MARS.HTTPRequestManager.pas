unit PTLib.MARS.HTTPRequestManager;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Net.URLClient,

  MARS.Client.CustomResource, MARS.Client.Resource, MARS.Client.Token,
  MARS.Client.Application, MARS.Client.Client, MARS.Client.Client.Net,
  MARS.Utils.Parameters, MARS.Core.Utils,

  PTLib.Common.Log,
  PTLib.Common.Interfaces,
  PTLib.Common.Strings,
  PTLib.Common.Network.HTTPRequestManager,

  PTLib.Mars.Client,
  PTLib.Mars.Interfaces;

type
  (*TMARSHTTPRequestThread = class(TPTLibHTTPRequestThread)
  protected
    procedure DoExecute; override;
  end; *)

  TMARSHTTPRequestManager = class(TPTLibHTTPRequestManager)
  strict private
    FHost: String;
    FApplication: String;
    FMarsClient: TPTLibMarsClient;
  private
    procedure SetApplication(const Value: String);
    procedure SetHost(const Value: String);
    function NewHTTPRequest(const ID: String; const URL: String): IHTTPRequest;
    function NewHTTPResponse(const Content: String; const Error: Exception; const StatusCode: Integer;
  const HostUnavailable: Boolean; const UnrecoverableError: Boolean): IHTTPResponseEx;
  protected
    procedure DoGetThreadClass(out ThreadClass: TPTLibHTTPRequestThreadClass); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HTTPGet(const URL: String; const Callback: THTTPRequestCallback; var ID: String; const Headers: TNetHeaders = nil; const AlwaysSend: Boolean = False): Boolean; override;
    function HTTPPost(const URL, Body: String; const Callback: THTTPRequestCallback; var ID: String; const Headers: TNetHeaders = nil; const AlwaysSend: Boolean = False): Boolean; override;

    property MarsClient: TPTLibMarsClient read FMarsClient;
  published
    property Host: String read FHost write SetHost;
    property Application: String read FApplication write SetApplication;
  end;

implementation

{ TMARSHTTPRequestThread }

(*procedure TMARSHTTPRequestThread.DoExecute;
var
  HTTPGet: IHTTPGet;
  HTTPPost: IHTTPPost;
  Response: IMarsClientResponse;
  MarsClient: TPTLibMARSClient;
begin
  MarsClient := TMARSHTTPRequestManager(HTTPRequestManager).MARSClient;
  try
    if Supports(HTTPRequest, IHTTPGet, HTTPGet) then
    begin
      Response := MarsClient.GET(HTTPRequest.URL);
    end else

    // Execute Post
    if Supports(HTTPRequest, IHTTPPost, HTTPPost) then
    begin
      Response := MarsClient.POST(
        HTTPRequest.URL,
        HTTPPost.Body);
    end;

    HTTPResponse.Content := Response.ResponseString;

    if Response.ResponseCode <> 200 then
    begin
      raise ENetURIException.Create(Response.ResponseStatus);
    end;
  except
    on e: ENetURIException do
    begin
      HTTPResponse.HostUnavailable := False;
      HTTPResponse.UnrecoverableError := True;
      HTTPResponse.Error := Exception(AcquireExceptionObject);
    end;

    on e: Exception do
    begin
      HTTPResponse.HostUnavailable := True;

      HTTPResponse.Error := Exception(AcquireExceptionObject);
    end;
  end;
end;*)

{ TMARSHTTPRequestManager }

constructor TMARSHTTPRequestManager.Create(AOwner: TComponent);
begin
  inherited;

  FMARSClient := TPTLibMarsClient.Create('', '', True);
end;

destructor TMARSHTTPRequestManager.Destroy;
begin
  FreeAndNil(FMARSClient);

  inherited;
end;

procedure TMARSHTTPRequestManager.DoGetThreadClass(out ThreadClass: TPTLibHTTPRequestThreadClass);
begin
  ThreadClass := nil; //TMARSHTTPRequestThread;
end;

function TMARSHTTPRequestManager.NewHTTPRequest(const ID: String; const URL: String): IHTTPRequest;
begin
  Result := THTTPRequest.Create;
  Result.ID := ID;
  Result.URL := URL;
end;

function TMARSHTTPRequestManager.NewHTTPResponse(const Content: String; const Error: Exception; const StatusCode: Integer;
  const HostUnavailable: Boolean; const UnrecoverableError: Boolean): IHTTPResponseEx;
begin
  Result := THTTPResponseEx.Create;
  Result.Content := Content;
  Result.Error := Error;
  Result.HostUnavailable := HostUnavailable;
  Result.UnrecoverableError := UnrecoverableError;
  Result.StatusCode := StatusCode;
end;

function TMARSHTTPRequestManager.HTTPGet(const URL: String; const Callback: THTTPRequestCallback; var ID: String; const Headers: TNetHeaders;
  const AlwaysSend: Boolean): Boolean;
begin
  if ID = '' then
  begin
    ID := CreateGUIDString;
  end;

  FMARSClient.GET(
    URL,
    nil,
    procedure(const ID: String; Response: IMarsClientResponse)
    begin
      SetOnline(True);

      if Assigned(CallBack) then
      begin
        CallBack(
          Self,
          NewHTTPRequest(ID, URL),
          NewHTTPResponse(
            Response.ResponseString,
            nil,
            Response.ResponseCode,
            False,
            False)
        );
      end;
    end,
    procedure(const ID: String; const e: Exception; Response: IMarsClientResponse)
    begin
      if Assigned(CallBack) then
      begin
        CallBack(
          Self,
          NewHTTPRequest(ID, URL),
          NewHTTPResponse(
            Response.ResponseString,
            e,
            Response.ResponseCode,
            True,
            False)   { TODO : Fix }
        );
      end;
    end,
    nil,
    ID);

  Result := True;
end;

function TMARSHTTPRequestManager.HTTPPost(const URL, Body: String; const Callback: THTTPRequestCallback; var ID: String; const Headers: TNetHeaders;
  const AlwaysSend: Boolean): Boolean;
begin
  if ID = '' then
  begin
    ID := CreateGUIDString;
  end;

  FMARSClient.POST(
    URL,
    Body,
    nil,  { TODO : Required? }
    procedure(const ID: String; Response: IMarsClientResponse)
    begin
      SetOnline(True);

      if Assigned(CallBack) then
      begin
        CallBack(
          Self,
          NewHTTPRequest(ID, URL),
          NewHTTPResponse(
            Response.ResponseString,
            nil,
            Response.ResponseCode,
            False,
            False)
        );
      end;
    end,
    procedure(const ID: String; const e: Exception; Response: IMarsClientResponse)
    begin
      if Assigned(CallBack) then
      begin
        CallBack(
          Self,
          NewHTTPRequest(ID, URL),
          NewHTTPResponse(
            Response.ResponseString,
            e,
            Response.ResponseCode,
            True,
            False)   { TODO : Fix }
        );
      end;
    end,
    nil,
    ID);

  Result := True;
end;

procedure TMARSHTTPRequestManager.SetApplication(const Value: String);
begin
  if FApplication <> Value then
  begin
    FApplication := Value;
  end;
end;

procedure TMARSHTTPRequestManager.SetHost(const Value: String);
begin
  if FHost <> Value then
  begin
    FHost := Value;
  end;
end;

end.
