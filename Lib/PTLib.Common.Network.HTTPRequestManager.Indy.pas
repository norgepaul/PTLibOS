unit PTLib.Common.Network.HTTPRequestManager.Indy;

interface

uses
  System.Classes, System.SysUtils, System.Net.HttpClient, System.Net.URLClient,
  System.Types;

type
  TIndyHTTPResponse = class(TInterfacedObject, IHTTPResponse)
  private
    FContentEncoding: string;
    FStatusCode: Integer;
    FResponseContent: TMemoryStream;

    function GetHeaders: TNetHeaders;
    function GetMimeType: string;
    function GetContentStream: TStream;
    function ContentAsString(const AnEncoding: TEncoding = nil): string;

    function GetHeaderValue(const AName: string): string;
    function GetContentCharSet: string;
    function GetContentEncoding: string;
    function GetContentLanguage: string;
    function GetContentLength: Int64;
    function GetDate: string;
    function GetLastModified: string;
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetVersion: THTTPProtocolVersion;
    function ContainsHeader(const AName: string): Boolean;
    function GetCookies: TCookies;
  public
    constructor Create(const AURL: string);
    destructor Destroy; override;

    property Headers: TNetHeaders read GetHeaders;
    property MimeType: string read GetMimeType;
    property ContentStream: TStream read GetContentStream;

    property HeaderValue[const AName: string]: string read GetHeaderValue;
    property ContentCharSet: string read GetContentCharSet;
    property ContentEncoding: string read GetContentEncoding;
    property ContentLanguage: string read GetContentLanguage;
    property ContentLength: Int64 read GetContentLength;
    property Date: string read GetDate;
    property LastModified: string read GetLastModified;
    property StatusText: string read GetStatusText;
    property StatusCode: Integer read GetStatusCode;
    property Version: THTTPProtocolVersion read GetVersion;
    property Cookies: TCookies read GetCookies;
    function GetAsyncResult: IAsyncResult;
    property AsyncResult: IAsyncResult read GetAsyncResult;
  end;

implementation

uses
  IdHTTP;

procedure RaiseNotImplementedYet;
begin
  raise Exception.Create('TIndyHTTPResponse: not implemented');
end;

{ TIndyHTTPResponse }

function TIndyHTTPResponse.ContainsHeader(const AName: string): Boolean;
begin
  Result := False;

  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.ContentAsString(const AnEncoding: TEncoding): string;
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create;
  try
    StringStream.LoadFromStream(FResponseContent);
    Result := StringStream.DataString;
  finally
    StringStream.Free;
  end;
end;

constructor TIndyHTTPResponse.Create(const AURL: string);
var
  IndyHTTP: TIdHTTP;
begin
  inherited Create;

  FResponseContent := TMemoryStream.Create;

  IndyHTTP := TIdHTTP.Create(nil);
  try
    IndyHTTP.Get(AURL, FResponseContent);

    FStatusCode := IndyHTTP.ResponseCode;
    FContentEncoding := IndyHTTP.Response.ContentEncoding;
    FResponseContent.Position := 0;
  finally
    IndyHTTP.Free;
  end;
end;

destructor TIndyHTTPResponse.Destroy;
begin
  FResponseContent.Free;
  inherited;
end;

function TIndyHTTPResponse.GetAsyncResult: IAsyncResult;
begin
  Result := nil;

  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetContentCharSet: string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetContentEncoding: string;
begin
  Result := FContentEncoding;
end;

function TIndyHTTPResponse.GetContentLanguage: string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetContentLength: Int64;
begin
  Result := FResponseContent.Size;
end;

function TIndyHTTPResponse.GetContentStream: TStream;
begin
  Result := FResponseContent;
end;

function TIndyHTTPResponse.GetCookies: TCookies;
begin
  Result := nil;

  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetDate: string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetHeaders: TNetHeaders;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetHeaderValue(const AName: string): string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetLastModified: string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetMimeType: string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TIndyHTTPResponse.GetStatusText: string;
begin
  RaiseNotImplementedYet;
end;

function TIndyHTTPResponse.GetVersion: THTTPProtocolVersion;
begin
  Result := THTTPProtocolVersion.UNKNOWN_HTTP;

  RaiseNotImplementedYet;
end;

end.
