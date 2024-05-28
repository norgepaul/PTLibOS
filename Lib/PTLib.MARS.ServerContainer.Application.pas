unit PTLib.MARS.ServerContainer.Application;

interface

uses
  System.SysUtils, System.JSON,

  MARS.Core.Attributes, MARS.Metadata, MARS.Metadata.JSON,
  MARS.Metadata.InjectionService, MARS.Core.MediaType,
  MARS.Metadata.Attributes, MARS.Core.Registry,

  PTLib.Common.Log;

type
  [Path('')]
  [Produces(TMediaType.TEXT_HTML)]
  TMarsServerContainerServiceApplication = class
  protected
    class procedure DoGetDefaultSettings(out AName, ABasePath: String; out AResources: TArray<String>; out AParametersSliceName: String; out Handled: Boolean); virtual;
  public
    class function GetDefaultSettings(out AName, ABasePath: String; out AResources: TArray<String>; out AParametersSliceName: String): Boolean;

    // Ping the application - common to all application
    [GET]
    function Ping: String;

    [GET, Path('/schema')]
    function MetadataSimple([Context] Metadata: TMARSApplicationMetadata): string;
  end;

implementation

{ TMarsServerContainerServiceApplication }

class procedure TMarsServerContainerServiceApplication.DoGetDefaultSettings(out AName, ABasePath: String; out AResources: TArray<String>;
  out AParametersSliceName: String; out Handled: Boolean);
begin
  Handled := False;
end;

class function TMarsServerContainerServiceApplication.GetDefaultSettings(out AName, ABasePath: String; out AResources: TArray<String>;
  out AParametersSliceName: String): Boolean;
begin
  DoGetDefaultSettings(AName, ABasePath, AResources, AParametersSliceName, Result);
end;

function TMarsServerContainerServiceApplication.Ping: String;
begin
  Result := 'Pong';
end;

function TMarsServerContainerServiceApplication.MetadataSimple(Metadata: TMARSApplicationMetadata): string;
var
  JSON: TJSONObject;
begin
  JSON := Metadata.ToJSON;
  try
    Result := JSON.ToJSON;
  finally
    FreeAndNil(JSON);
  end;
end;

initialization
  TMARSResourceRegistry.Instance.RegisterResource<TMarsServerContainerServiceApplication>;

end.
