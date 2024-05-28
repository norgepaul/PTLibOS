unit PTLib.Common.KeyValueStore.Memory;

interface

uses
  System.SysUtils, System.Variants, System.JSON, System.IOUtils, System.DateUtils,
  System.Generics.Collections, System.StrUtils, System.Classes,

  PTLib.Common.Strings,
  PTLib.Common.Classes,
  PTLib.Common.Utils,
  PTLib.Common.Interfaces,
  PTLib.Common.KeyValueStore;

type
  TKeyValueStoreMemory = class(TKeyValueStore,
                               IKeyValueStoreMemory)
  protected
    FValues: TDictionary<String, TPair<String, TKeyValueType>>;

    procedure DoSetValue(const Name: String; const Value: String; const KeyValueType: TKeyValueType); override;
    procedure DoGetValue(const Name: String; var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType); override;
    procedure DoValueExists(const Name: String; out Exists: Boolean); override;
    procedure DoRemoveValue(const Name: String); override;

    procedure DoClear(const Filter: String); virtual;
  public
    constructor Create(const RaiseExceptions: Boolean = True); override;
    destructor Destroy; override;

    function AsText(const Filter: String = ''; const Sort: Boolean = True): String;

    procedure Clear(const Filter: String = '');
  end;

implementation

uses
    PTLib.Common.Log;

resourcestring
  StrSkippedInvalidJSON = 'Skipped invalid JSON key value data type: %s';

{ TKeyValueStoreMemory }

function TKeyValueStoreMemory.AsText(const Filter: String; const Sort: Boolean): String;
var
  Key: String;
  Value: TPair<String, TKeyValueType>;
  Values: TStringList;
begin
  Result := '';

  BeginRead;
  try
    Values := TStringList.Create;
    try
      for Key in FValues.Keys do
      begin
        if ((Filter = '') or
            (ContainsText(Key, Filter))) and
           (FValues.TryGetValue(Key, Value)) then
        begin
          Values.Add(Key + '=' + Value.Key);
        end;
      end;

      if Sort then
      begin
        Values.Sort;
      end;

      Result := Values.Text;
    finally
      FreeAndNil(Values);
    end;
  finally
    EndRead;
  end;
end;

procedure TKeyValueStoreMemory.Clear(const Filter: String = '');
begin
  DoClear(Filter);
end;

constructor TKeyValueStoreMemory.Create(const RaiseExceptions: Boolean);
begin
  inherited;

  FValues := TDictionary<String, TPair<String, TKeyValueType>>.Create;
end;

destructor TKeyValueStoreMemory.Destroy;
begin
  FreeAndNil(FValues);

  inherited;
end;

procedure TKeyValueStoreMemory.DoClear(const Filter: String);
var
  Key: String;
begin
  BeginWrite;
  try
    if Filter = '' then
    begin
      FValues.Clear;
    end
    else
    begin
      for Key in FValues.Keys do
      begin
        if ContainsText(Key, Filter) then
        begin
          FValues.Remove(Key);
        end;
      end;
    end;
  finally
    EndWrite;
  end;
end;

procedure TKeyValueStoreMemory.DoGetValue(const Name: String;
  var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType);
var
  Pair: TPair<String, TKeyValueType>;
begin
  Success :=
    (FValues.TryGetValue(Name, Pair)) and
    (Pair.Value = KeyValueType);

  if Success then
  begin
    Value := Pair.Key;
  end;
end;

procedure TKeyValueStoreMemory.DoRemoveValue(const Name: String);
begin
  FValues.Remove(Name);
end;

procedure TKeyValueStoreMemory.DoSetValue(const Name: String;
  const Value: String; const KeyValueType: TKeyValueType);
var
  Pair: TPair<String, TKeyValueType>;
begin
  Pair.Key := Value;
  Pair.Value := KeyValueType;

  FValues.AddOrSetValue(Name, Pair);
end;

procedure TKeyValueStoreMemory.DoValueExists(const Name: String;
  out Exists: Boolean);
begin
  Exists := FValues.ContainsKey(Name);
end;

end.
