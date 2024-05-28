unit PTLib.Common.KeyValueStore.EnvironmentVariables;

interface

uses
  System.SysUtils,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.KeyValueStore;

type
  TKeyValueStoreEnvironmentVariables = class(TKeyValueStore,
                                             IKeyValueStoreEnvironmentVariables)
  protected
    procedure DoSetValue(const Name: String; const Value: String; const KeyValueType: TKeyValueType); override;
    procedure DoGetValue(const Name: String; var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType); override;
    procedure DoValueExists(const Name: String; out Exists: Boolean); override;
    procedure DoRemoveValue(const Name: String); override;
  end;

implementation

{ TKeyValueStoreEnvironmentVariables }

procedure TKeyValueStoreEnvironmentVariables.DoGetValue(const Name: String;
  var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType);
begin
  // Return the environment variable
  Value := GetEnvironmentVariable(Name);

  Success := Value <> '';
end;

procedure TKeyValueStoreEnvironmentVariables.DoRemoveValue(const Name: String);
begin
  raise EPTLibNotSupportedError.Create('Remove value not supported');
end;

procedure TKeyValueStoreEnvironmentVariables.DoSetValue(const Name: String;
  const Value: String; const KeyValueType: TKeyValueType);
begin
  raise EPTLibNotSupportedError.Create('Set value not supported');
end;

procedure TKeyValueStoreEnvironmentVariables.DoValueExists(const Name: String;
  out Exists: Boolean);
begin
  Exists := GetEnvironmentVariable(Name) = '';
end;

end.
