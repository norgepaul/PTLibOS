unit PTLib.MARS.Classes;

interface

uses
  System.SysUtils, System.Classes,

  MARS.Core.Utils, MARS.Client.Client,

  PTLib.Common.Classes,

  PTLib.Mars.Interfaces,
  PTLib.Mars.Types;

type
  EMarsException = class(EPTLibError);
  EMarsServiceException = class(EMarsException);

  TMarsUtils = class
  public
    class function StreamToString(const AStream: TStream; const Size: Integer = 0; const AEncoding: TEncoding = nil): String;
  end;

  TMarsClientResponse = class(TInterfacedObject, IMarsClientResponse)
  strict private
    FResource: String;
    FResponseStream: TMemoryStream;
    FResponseStatus: String;
    FResponseCode: Integer;
  private
    function GetResource: String;
    function GetResponseCode: Integer;
    function GetResponseStatus: String;
    function GetResponseStream: TMemoryStream;
    function GetResponseString: String;
    procedure SetResource(const Value: String);
    procedure SetResponseCode(const Value: Integer);
    procedure SetResponseStatus(const Value: String);
  protected
    property Resource: String read GetResource write SetResource;
    property ResponseStream: TMemoryStream read GetResponseStream;
    property ResponseStatus: String read GetResponseStatus write SetResponseStatus;
    property ResponseCode: Integer read GetResponseCode write SetResponseCode;
    property ResponseString: String read GetResponseString;
  public
    destructor Destroy; override;
  end;

  TMarsClientRequest = class(TInterfacedObject, IMarsClientRequest)
  strict private
    FResource: String;
    FRequestStream: TMemoryStream;
    FHTTPRequestType: TMARSHttpVerb;
    FHeaders: TStringList;
  private
    function GetRequestStream: TMemoryStream;
    function GetResource: String;
    function GetHTTPRequestType: TMARSHttpVerb;
    function GetHeaders: TStringList;
    procedure SetResource(const Value: String);
    procedure SetHTTPRequestType(const Value: TMARSHttpVerb);
  protected
    property Resource: String read GetResource write SetResource;
    property RequestStream: TMemoryStream read GetRequestStream;
    property HTTPRequestType: TMARSHttpVerb read GetHTTPRequestType write SetHTTPRequestType;
    property Headers: TStringList read GetHeaders;
  public
    constructor Create(const AHTTPRequestType: TMARSHttpVerb; const AResource: String;
      const ARequestStream: TStream; const AHeaders: TStrings = nil); overload;
    constructor Create(const AHTTPRequestType: TMARSHttpVerb; const AResource: String;
      const ARequestText: String; const AHeaders: TStrings = nil); overload;

    destructor Destroy; override;
  end;

implementation

{ TMarsClientResponse }

destructor TMarsClientResponse.Destroy;
begin
  FreeAndNil(FResponseStream);

  inherited;
end;

function TMarsClientResponse.GetResource: String;
begin
  Result := FResource;
end;

function TMarsClientResponse.GetResponseCode: Integer;
begin
  Result := FResponseCode;
end;

function TMarsClientResponse.GetResponseStatus: String;
begin
  Result := FResponseStatus;
end;

function TMarsClientResponse.GetResponseStream: TMemoryStream;
begin
  if FResponseStream = nil then
  begin
    FResponseStream := TMemoryStream.Create;
  end;

  Result := FResponseStream;
end;

function TMarsClientResponse.GetResponseString: String;
begin
  try
    Result := TMarsUtils.StreamToString(ResponseStream);
  except
    on e: Exception do
    begin
      Result := '<Binary Data>';
    end;
  end;
end;

procedure TMarsClientResponse.SetResource(const Value: String);
begin
  FResource := Value;
end;

procedure TMarsClientResponse.SetResponseCode(const Value: Integer);
begin
  FResponseCode := Value;
end;

procedure TMarsClientResponse.SetResponseStatus(const Value: String);
begin
  FResponseStatus := Value;
end;

{ TMarsClientRequest }

constructor TMarsClientRequest.Create(const AHTTPRequestType: TMARSHttpVerb;
  const AResource: String; const ARequestStream: TStream; const AHeaders: TStrings);
begin
  FHTTPRequestType := AHTTPRequestType;
  FResource := AResource;

  if ARequestStream <> nil then
  begin
    RequestStream.CopyFrom(ARequestStream, ARequestStream.Size);
  end;

  if AHeaders <> nil then
  begin
    Headers.Assign(AHeaders);
  end;
end;

constructor TMarsClientRequest.Create(const AHTTPRequestType: TMARSHttpVerb;
  const AResource, ARequestText: String; const AHeaders: TStrings);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(ARequestText);
  try
    Create(AHTTPRequestType, AResource, StringStream, AHeaders);
  finally
    FreeAndNil(StringStream);
  end;
end;

destructor TMarsClientRequest.Destroy;
begin
  FreeAndNil(FRequestStream);
  FreeAndNil(FHeaders);

  inherited;
end;

function TMarsClientRequest.GetHeaders: TStringList;
begin
  if FHeaders = nil then
  begin
    FHeaders := TStringList.Create;
  end;

  Result := FHeaders;
end;

function TMarsClientRequest.GetHTTPRequestType: TMARSHttpVerb;
begin
  Result := FHTTPRequestType;
end;

function TMarsClientRequest.GetRequestStream: TMemoryStream;
begin
  if FRequestStream = nil then
  begin
    FRequestStream := TMemoryStream.Create;
  end;

  Result := FRequestStream;
end;

function TMarsClientRequest.GetResource: String;
begin
  Result := FResource;
end;

procedure TMarsClientRequest.SetHTTPRequestType(const Value: TMARSHttpVerb);
begin
  FHTTPRequestType := Value;
end;

procedure TMarsClientRequest.SetResource(const Value: String);
begin
  FResource := Value;
end;

{ TMarsUtils }

class function TMarsUtils.StreamToString(const AStream: TStream; const Size: Integer; const AEncoding: TEncoding): String;
var
  LBytes: TBytes;
  LEncoding: TEncoding;
  LSize: Cardinal;
begin
  Result := '';

  try
    if Assigned(AStream) then
    begin
      if Assigned(AEncoding) then
      begin
        LEncoding := AEncoding;
      end
      else
      begin
        LEncoding := TEncoding.UTF8;
      end;

      if Size = 0 then
      begin
        LSize := AStream.Size;
      end
      else
      begin
        LSize := Size;

        if LSize > AStream.Size then
        begin
          LSize := AStream.Size;
        end;
      end;

      AStream.Position := 0;
      SetLength(LBytes, LSize);
      AStream.Read(LBytes, LSize);

      Result := LEncoding.GetString(LBytes);
    end;
  except
    on e: Exception do
    begin
      Result := '<Binary>';
    end;
  end;
end;

end.
