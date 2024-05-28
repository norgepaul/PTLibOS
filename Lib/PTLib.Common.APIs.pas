unit PTLib.Common.APIs;

interface

uses
  SysUtils, Classes,

  PTLib.Common.Classes;

type
  TPTLibAPI = class(TInterfacedObject)
  strict private
    FAPIKey: String;
  protected
    procedure CheckAPIKey(const APIKey: String); virtual;

    function APIKey: String;
  public
    constructor Create(const APIKey: String);
  end;

implementation

{ TPTLibAPI }

function TPTLibAPI.APIKey: String;
begin
  Result := FAPIKey;
end;

procedure TPTLibAPI.CheckAPIKey(const APIKey: String);
begin
  if APIKey = '' then
  begin
    raise EPTLibInvalidAPIKey.Create('Invalid MapQuest API Key');
  end;
end;

constructor TPTLibAPI.Create(const APIKey: String);
begin
  FAPIKey := APIKey;

  CheckAPIKey(APIKey);
end;

end.
