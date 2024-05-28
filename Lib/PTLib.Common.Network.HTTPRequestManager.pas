{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network.HTTPRequestManager                  }
{                                                                    }
{           Copyright (c) 1998-2016 Easy-IP AS.                      }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF EASY-IP AS. THE REGISTERED DEVELOPER                  }
{   LICENSED TO DISTRIBUTE THE CODE HEREIN AND ANY ACCOMPANYING      }
{   VCL OR FMX CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.       }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM EASY-IP AS.                                  }
{                                                                    }
{********************************************************************}

unit PTLib.Common.Network.HTTPRequestManager;

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils, System.NetConsts,

  System.Net.HttpClient, System.Net.URLClient,

  PTLib.Common.Classes,
  PTLib.Common.Files,
  PTLib.Common.Interfaces,
  PTLib.Common.Thread,
  PTLib.Common.Dates,
  PTLib.Common.Types,
  PTLib.Common.Strings,
  PTLib.Common.Log;

type
  TPTLibHTTPRequestManager = class;

  TOnOnlineChanged = procedure(Sender: TObject; const Online: Boolean) of object;

  THTTPRequest = class(TInterfacedObject, IHTTPRequest)
  strict private
    FID: String;
    FURL: String;
    FHeaders: TNetHeaders;
    FEncoding: TEncoding;
  private
    function GetID: String;
    function GetURL: String;
    procedure SetID(const Value: String);
    procedure SetURL(const Value: String);
    function GetHeaders: TNetHeaders;
    procedure SetHeaders(const Value: TNetHeaders);
    function GetEncoding: TEncoding;
    procedure SetEncoding(const Value: TEncoding);
  public
    property ID: String read GetID write SetID;
    property URL: String read GetURL write SetURL;
    property Headers: TNetHeaders read GetHeaders write SetHeaders;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
  end;

  THTTPGet = class(THTTPRequest, IHTTPGet)
  end;

  THTTPPost = class(THTTPRequest, IHTTPPost)
  strict private
    FBody: String;
  private
    function GetBody: String;
    procedure SetBody(const Value: String);
  public
    property Body: String read GetBody write SetBody;
  end;

  THTTPResponseEx = class(TInterfacedObject, IHTTPResponseEx)
  strict private
    FContent: string;
    FError: Exception;
    FStatusCode: Integer;
    FHostUnavailable: Boolean;
    FUnrecoverableError: Boolean;
  private
    function GetError: Exception;
    function GetHostUnavailable: Boolean;
    function GetContent: string;
    procedure SetError(const Value: Exception);
    procedure SetHostUnavailable(const Value: Boolean);
    procedure SetContent(const Value: string);
    function GetUnrecoverableError: Boolean;
    procedure SetUnrecoverableError(const Value: Boolean);
    function GetStatusCode: Integer;
    procedure SetStatusCode(const Value: Integer);
  public
    property Content: string read GetContent write SetContent;
    property Error: Exception read GetError write SetError;
    property HostUnavailable: Boolean read GetHostUnavailable write SetHostUnavailable;
    property UnrecoverableError: Boolean read GetUnrecoverableError write SetUnrecoverableError;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
  end;

  TPTLibHTTPRequestThread = class(TPTLibHandlerThread)
  strict private
    FHTTPRequest: IHTTPRequest;
    FHTTPResponseEx: IHTTPResponseEx;
    FCallback: THTTPRequestCallback;
    FHTTPRequestManager: TPTLibHTTPRequestManager;
  protected
    procedure DoExecute; override;

    property HTTPRequest: IHTTPRequest read FHTTPRequest;
    property HTTPResponse: IHTTPResponseEx read FHTTPResponseEx;
    property Callback: THTTPRequestCallback read FCallback;
    property HTTPRequestManager: TPTLibHTTPRequestManager read FHTTPRequestManager;
  public
    constructor Create(const HTTPRequestManager: TPTLibHTTPRequestManager; const HTTPRequest: IHTTPRequest; const Callback: THTTPRequestCallback);
  end;
  TPTLibHTTPRequestThreadClass = class of TPTLibHTTPRequestThread;

  TPTLibHTTPRequestManager = class(TBasePTLibLoggerComponent)
  strict private
    FOnOnlineChanged: TOnOnlineChanged;

    FActiveThreads: TObjectDictionary<String, TPTLibHTTPRequestThread>;
    FMaxThreads: Integer;
    FOfflineUntilUTC: TDateTime;
    FMaximumNoResponseCount: Integer;
    FNoResponseWaitInterval: Cardinal;
    FNoResponseCount: Integer;
    FConnectedOnce: Boolean;
    FBytesSent: Cardinal;  { TODO : Not yet implemented. How? }
    FBytesReceived: Cardinal;
    FRequestsSent: Cardinal;
    FOnline: Boolean;

    procedure OnThreadTerminate(Sender: TObject);
  private
    function CanSend: Boolean;
  protected
    procedure DoGetThreadClass(out ThreadClass: TPTLibHTTPRequestThreadClass); virtual;
    procedure DoNewRequestThread(const HTTPRequest: IHTTPRequest; const Callback: THTTPRequestCallback; var ID: String); virtual;
    procedure DoOnlineChanged(const IsOnline: Boolean); virtual;
    procedure DoGetDefaultLogType(out LogType: String); override;
    procedure SetOnline(const Value: Boolean);

    function GetThreadClass: TPTLibHTTPRequestThreadClass;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearStatistics;

    procedure CancelAllRequests(const DisableOnTerminate: Boolean = False);
    procedure CancelRequest(const ID: String);
    function Online: Boolean;

    function HTTPGet(const URL: String; const Callback: THTTPRequestCallback; var ID: String; const Headers: TNetHeaders = nil; const AlwaysSend: Boolean = False): Boolean; virtual;
    function HTTPPost(const URL, Body: String; const Callback: THTTPRequestCallback; var ID: String; const Headers: TNetHeaders = nil; const AlwaysSend: Boolean = False): Boolean; virtual;

    function BytesSent: Cardinal;
    function BytesReceived: Cardinal;
    function RequestsSent: Cardinal;
    function OfflineUntilUTC: TDateTime;
  published
    property OnOnlineChanged: TOnOnlineChanged read FOnOnlineChanged write FOnOnlineChanged;

    property MaxThreads: Integer read FMaxThreads write FMaxThreads;
    property MaximumNoResponseCount: Integer read FMaximumNoResponseCount write FMaximumNoResponseCount;
    property NoResponseWaitInterval: Cardinal read FNoResponseWaitInterval write FNoResponseWaitInterval;
  end;

implementation

uses
  IdZlib {$IFDEF ENFORCE_INDY}, PTLib.Common.Network.HTTPRequestManager.Indy{$ENDIF};

resourcestring
  StrHTTPRequestManager = 'HTTP Request Manager';
  StrHTTPRequestCallBackError = 'HTTP Request CallBack exception - %s';
  StrUnableToContactTh = 'Unable to contact the server';

{ THTTPRequest }

function THTTPRequest.GetEncoding: TEncoding;
begin
  Result := FEncoding;
end;

function THTTPRequest.GetHeaders: TNetHeaders;
begin
  Result := FHeaders;
end;

function THTTPRequest.GetID: String;
begin
  Result := FID;
end;

function THTTPRequest.GetURL: String;
begin
  Result := FURL;
end;

procedure THTTPRequest.SetEncoding(const Value: TEncoding);
begin
  FEncoding := Value;
end;

procedure THTTPRequest.SetHeaders(const Value: TNetHeaders);
begin
  FHeaders := Value;
end;

procedure THTTPRequest.SetID(const Value: String);
begin
  FID := Value;
end;

procedure THTTPRequest.SetURL(const Value: String);
begin
  FURL := Value;
end;

{ TPTLibHTTPRequestManager }

function TPTLibHTTPRequestManager.BytesReceived: Cardinal;
begin
  Result := FBytesReceived;
end;

function TPTLibHTTPRequestManager.BytesSent: Cardinal;
begin
  Result := FBytesSent;
end;

procedure TPTLibHTTPRequestManager.CancelAllRequests(const DisableOnTerminate: Boolean);
var
  ID: String;
begin
  for ID in FActiveThreads.Keys do
  begin
    if DisableOnTerminate then
    begin
      FActiveThreads[ID].OnTerminate := nil;
    end;

    CancelRequest(ID);
  end;

  FActiveThreads.Clear;
end;

procedure TPTLibHTTPRequestManager.CancelRequest(const ID: String);
begin
  FActiveThreads[ID].Terminate;
end;

constructor TPTLibHTTPRequestManager.Create(AOwner: TComponent);
begin
  inherited;

  ClearStatistics;

  FActiveThreads := TObjectDictionary<String, TPTLibHTTPRequestThread>.Create([]);

  FMaxThreads := 10;
  FMaximumNoResponseCount := 5;
  FNoResponseWaitInterval := 10000;
  FOfflineUntilUTC := 0;
end;

destructor TPTLibHTTPRequestManager.Destroy;
begin
  CancelAllRequests(True);

  FreeAndNil(FActiveThreads);

  inherited;
end;

procedure TPTLibHTTPRequestManager.DoGetDefaultLogType(out LogType: String);
begin
  LogType := StrHTTPRequestManager;
end;

procedure TPTLibHTTPRequestManager.DoGetThreadClass(
  out ThreadClass: TPTLibHTTPRequestThreadClass);
begin
  ThreadClass := TPTLibHTTPRequestThread;
end;

function TPTLibHTTPRequestManager.GetThreadClass: TPTLibHTTPRequestThreadClass;
begin
  DoGetThreadClass(Result);
end;

function TPTLibHTTPRequestManager.OfflineUntilUTC: TDateTime;
begin
  Result := FOfflineUntilUTC;
end;

function TPTLibHTTPRequestManager.Online: Boolean;
begin
  Result := (FOfflineUntilUTC < NowUTC) and (FConnectedOnce);
end;

function TPTLibHTTPRequestManager.CanSend: Boolean;
begin
  Result :=
    (FOfflineUntilUTC < NowUTC) and
    ((FMaxThreads = 0) or
     (FActiveThreads.Count < FMaxThreads));
end;

procedure TPTLibHTTPRequestManager.ClearStatistics;
begin
  FBytesSent := 0;
  FBytesReceived := 0;
  FRequestsSent := 0;
end;

function TPTLibHTTPRequestManager.HTTPGet(const URL: String;
  const Callback: THTTPRequestCallback; var ID: String;
  const Headers: TNetHeaders; const AlwaysSend: Boolean): Boolean;
var
  HTTPGet: IHTTPGet;
begin
  Result := (AlwaysSend) or (CanSend);

  if Result then
  begin
    HTTPGet := THTTPGet.Create;
    HTTPGet.URL := URL;
    HTTPGet.Headers := Headers;

    DoNewRequestThread(HTTPGet, CallBack, ID);
  end;
end;

function TPTLibHTTPRequestManager.HTTPPost(const URL, Body: String;
  const Callback: THTTPRequestCallback; var ID: String;
  const Headers: TNetHeaders; const AlwaysSend: Boolean): Boolean;
var
  HTTPPost: IHTTPPost;
begin
  Result := (AlwaysSend) or (CanSend);

  if Result then
  begin
    HTTPPost := THTTPPost.Create;
    HTTPPost.URL := URL;
    HTTPPost.Body := Body;
    HTTPPost.Headers := Headers;

    Inc(FRequestsSent);

    DoNewRequestThread(HTTPPost, CallBack, ID);
  end;
end;

procedure TPTLibHTTPRequestManager.DoNewRequestThread(const HTTPRequest: IHTTPRequest;
  const Callback: THTTPRequestCallback; var ID: String);
var
  HTTPRequestThread: TPTLibHTTPRequestThread;
begin
  if ID = '' then
  begin
    ID := CreateGUIDString;
  end;

  HTTPRequest.ID := ID;

  HTTPRequestThread := GetThreadClass.Create(Self, HTTPRequest, Callback);
  HTTPRequestThread.FreeOnTerminate := True;
  HTTPRequestThread.OnTerminate := OnThreadTerminate;
  FActiveThreads.Add(ID, HTTPRequestThread);

  Inc(FRequestsSent);

  HTTPRequestThread.Start;
end;

procedure TPTLibHTTPRequestManager.SetOnline(const Value: Boolean);
begin
  if Value <> FOnline then
  begin
    if Value then
    begin
      FConnectedOnce := True;
    end;

    FOnline := Value;

    DoOnlineChanged(FOnline);
  end;
end;

procedure TPTLibHTTPRequestManager.DoOnlineChanged(const IsOnline: Boolean);
begin
  if Assigned(FOnOnlineChanged) then
    FOnOnlineChanged(Self, IsOnline);
end;

procedure TPTLibHTTPRequestManager.OnThreadTerminate(Sender: TObject);
var
  Thread: TPTLibHTTPRequestThread;
begin
  Thread := Sender as TPTLibHTTPRequestThread;

  if Thread.HTTPResponse.HostUnavailable then
  begin
    // If the host was not available we need to increment the count
    Inc(FNoResponseCount);

    // If we've had too many failues, go offline for a while
    if (FMaximumNoResponseCount <> 0) and
       (FNoResponseWaitInterval <> 0) and
       (FNoResponseCount >= FMaximumNoResponseCount) then
    begin
      FOfflineUntilUTC := NowUTC + (OneMillisecond * FNoResponseWaitInterval);

      SetOnline(False);
    end;
  end
  else
  begin
    FNoResponseCount := 0;

    SetOnline(True);

    FBytesReceived := FBytesReceived + Cardinal(length(Thread.HTTPResponse.Content));

    FBytesSent := FBytesSent + Cardinal(Thread.HTTPRequest.URL.Length);
  end;

  FActiveThreads.Remove(Thread.HTTPRequest.ID);

  try
    if Assigned(Thread.Callback) then
    begin
      Thread.Callback(
        Self,
        Thread.HTTPRequest,
        Thread.HTTPResponse);
    end;
  except
    on e: Exception do
    begin
      DoLog(
        StrHTTPRequestCallBackError,
        [e.Message],
        LogSeverityError);
    end;
  end;
end;

function TPTLibHTTPRequestManager.RequestsSent: Cardinal;
begin
  Result := FRequestsSent;
end;

{ THTTPResponse }

function THTTPResponseEx.GetContent: string;
begin
  Result := FContent;
end;

function THTTPResponseEx.GetError: Exception;
begin
  Result := FError;
end;

function THTTPResponseEx.GetHostUnavailable: Boolean;
begin
  Result := FHostUnavailable;
end;

function THTTPResponseEx.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function THTTPResponseEx.GetUnrecoverableError: Boolean;
begin
  Result := FUnrecoverableError;
end;

procedure THTTPResponseEx.SetContent(const Value: string);
begin
  FContent := Value;
end;

procedure THTTPResponseEx.SetError(const Value: Exception);
begin
  FError := Value;
end;

procedure THTTPResponseEx.SetHostUnavailable(const Value: Boolean);
begin
  FHostUnavailable := Value;
end;

procedure THTTPResponseEx.SetStatusCode(const Value: Integer);
begin
  FStatusCode := Value;
end;

procedure THTTPResponseEx.SetUnrecoverableError(const Value: Boolean);
begin
  FUnrecoverableError := Value;
end;

{ TPTLibHTTPRequestThread }

constructor TPTLibHTTPRequestThread.Create(const HTTPRequestManager: TPTLibHTTPRequestManager;
  const HTTPRequest: IHTTPRequest; const Callback: THTTPRequestCallback);
begin
  inherited Create(True);

  FHTTPRequestManager := HTTPRequestManager;
  FHTTPRequest := HTTPRequest;
  FHTTPResponseEx := THTTPResponseEx.Create;
  FCallback := CallBack;
end;

procedure TPTLibHTTPRequestThread.DoExecute;
var
  HTTPClient: THTTPClient;
  HTTPGet: IHTTPGet;
  HTTPPost: IHTTPPost;
  PostStrings: TStringList;
  StartTicks: Cardinal;
  ResponseStream, PostStream: TStringStream;
  Response: IHTTPResponse;
begin
  try
    HTTPClient := THTTPClient.Create;
    try
      HTTPClient.AcceptEncoding := 'gzip';

      if not Terminated then
      begin
        FHTTPRequestManager.Log(
          'Request - %s',
          [HTTPRequest.URL],
          LogSeverityDebug3);
      end;

      StartTicks := GetTickCount;

      // Execute Get
      if Supports(HTTPRequest, IHTTPGet, HTTPGet) then
      begin
        {$IFDEF ENFORCE_INDY}
          Response := TIndyHTTPResponse.Create(HTTPRequest.URL);
        {$ELSE}
          Response := HTTPClient.Get(HTTPRequest.URL, nil, HTTPGet.Headers);
        {$ENDIF}
      end else

      // Execute Post
      if Supports(HTTPRequest, IHTTPPost, HTTPPost) then
      begin
        if not HTTPPost.Body.Trim.StartsWith('{') then
        begin
          PostStrings := TStringList.Create;
          try
            PostStrings.Text := HTTPPost.Body;

            Response := HTTPClient.Post(HTTPRequest.URL, PostStrings, nil, HTTPRequest.Encoding);
          finally
            FreeAndNil(PostStrings);
          end;
        end else
        begin
          PostStream := TStringStream.Create(UTF8Encode(HTTPPost.Body));
          try
            HTTPClient.ContentType := 'application/json';

            Response := HTTPClient.Post(HTTPRequest.URL, PostStream, nil);
          finally
            PostStream.Free;
          end;
        end;
      end;

      if Pos('gzip', Response.ContentEncoding) > 0 then
      begin
        ResponseStream := TStringStream.Create;
        try
          IdZLib.DecompressStream(Response.ContentStream, ResponseStream);
          FHTTPResponseEx.Content := ResponseStream.DataString;
        finally
          ResponseStream.Free;
        end;
      end else
      begin
        FHTTPResponseEx.Content := Response.ContentAsString;
      end;

      if Response.ContentStream.Size = 0 then
      begin
        FHTTPResponseEx.Error := Exception.Create('No response content');
      end else
      if Response.StatusCode <> 200 then
      begin
        FHTTPResponseEx.Error := Exception.CreateFmt('Error: %s', [Response.ContentAsString]);
      end else
      begin
        if not Terminated then
        begin
          FHTTPRequestManager.Log(
            'Response in %s [%s]- %s - [%s]',
            [MilliSecondsToTimeString(GetTickCount - StartTicks),
             TFileUtils.FileSizeFormatStr(Response.ContentStream.Size),
             HTTPResponse.Content,
             HTTPRequest.URL],
            LogSeverityDebug3);
        end;
      end;

      FHTTPResponseEx.StatusCode := Response.StatusCode;
    finally
      FreeAndNil(HTTPClient);
    end;
  except
    on e: ENetURIException do
    begin
      FHTTPResponseEx.HostUnavailable := False;
      FHTTPResponseEx.UnrecoverableError := True;
      FHTTPResponseEx.Error := Exception.Create(StrUnableToContactTh);
    end;

    on e: Exception do
    begin
      FHTTPResponseEx.HostUnavailable := True;

      FHTTPResponseEx.Error := Exception.Create(StrUnableToContactTh);
    end;
  end;

  if (FHTTPResponseEx.Error <> nil) and
     (not Terminated) then
  begin
    FHTTPRequestManager.Log(
      'Error - %s',
      [FHTTPResponseEx.Error.Message],
      LogSeverityDebug3);
  end;
end;

{ THTTPPost }

function THTTPPost.GetBody: String;
begin
  Result := FBody;
end;

procedure THTTPPost.SetBody(const Value: String);
begin
  FBody := Value;
end;

end.
