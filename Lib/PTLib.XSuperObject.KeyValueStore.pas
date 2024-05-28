unit PTLib.XSuperObject.KeyValueStore;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils,

  XSuperObject,

  PTLib.Common.Utils,
  PTLib.Common.KeyValueStore,

  PTLib.XSuperObject.Interfaces;

type
  TKeyValueStoreJson = class(TKeyValueStore, IKeyValueStoreJson)
  strict private
    FJson: ISuperObject;
  private
    function GetJson: ISuperObject;
    function GetUpdated: Boolean;
  protected
    procedure DoSetValue(const Name: String; const Value: String; const KeyValueType: TKeyValueType); override;
    procedure DoGetValue(const Name: String; var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType); override;
    procedure DoValueExists(const Name: String; out Exists: Boolean); override;
    procedure DoRemoveValue(const Name: String); override;
    property Json: ISuperObject read GetJson;
  public
    function Load(const Filename: String): Boolean; overload;
    procedure Load(const AStream: TStream); overload;
    procedure Save(const Filename: String; const FormatJson: Boolean = False); overload;
    procedure Save(const AStream: TStream; const FormatJson: Boolean = False); overload;

    function CloneJson: ISuperObject;

    property Updated: Boolean read GetUpdated;
  end;

implementation

{ TKeyValueStoreJson }

function TKeyValueStoreJson.CloneJson: ISuperObject;
begin
  BeginRead;
  try
    Result := Json.Clone;
  finally
    EndRead;
  end;
end;

procedure TKeyValueStoreJson.DoGetValue(const Name: String;
  var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType);
begin
  Success := ValueExists(Name);

  if Success then
  begin
    case KeyValueType of
      kvtUnknown: Value := Json.V[Name];
      kvtString: Value := Json.S[Name];
      kvtInt64: Value := Json.I[Name].ToString;
      kvtFloat: Value := FloatToStrUseDot(Json.F[Name]);
      kvtDateTime: Value := DateToISO8601(Json.D[Name]);
      kvtBoolean: Value := BoolToStr(Json.B[Name], True);
    end;
  end;
end;

procedure TKeyValueStoreJson.DoRemoveValue(const Name: String);
begin
  Json.Remove(Name);
end;

procedure TKeyValueStoreJson.DoSetValue(const Name, Value: String;
  const KeyValueType: TKeyValueType);
begin
  case KeyValueType of
    kvtUnknown: Json.V[Name] := Value;
    kvtString: Json.S[Name] := Value;
    kvtInt64: Json.I[Name] := StrToInt64(Value);
    kvtFloat: Json.F[Name] := StrToFloatDefUseDot(Value, 0);
    kvtDateTime: Json.D[Name] := ISO8601ToDate(Value);
    kvtBoolean: Json.B[Name] := StrToBool(Value);
  end;
end;

procedure TKeyValueStoreJson.DoValueExists(const Name: String;
  out Exists: Boolean);
begin
  Exists := Json.Contains(Name);
end;

function TKeyValueStoreJson.GetJson: ISuperObject;
begin
  if FJson = nil then
  begin
    FJson := SO;
  end;

  Result := FJson;
end;

function TKeyValueStoreJson.GetUpdated: Boolean;
begin
  Result := FUpdated;
end;

procedure TKeyValueStoreJson.Load(const AStream: TStream);
begin
  FJson := TSuperObject.ParseStream(AStream);

  FUpdated := False;
end;

function TKeyValueStoreJson.Load(const Filename: String): Boolean;
begin
  Result := FileExists(Filename);

  if Result then
  begin
    try
      FJson := TSuperObject.ParseFile(FileName);
    except
      Result := False;
    end;

    FUpdated := False;
  end;
end;

procedure TKeyValueStoreJson.Save(const Filename: String; const FormatJson: Boolean);
begin
  Json.SaveTo(Filename, FormatJson);

  FUpdated := False;
end;

procedure TKeyValueStoreJson.Save(const AStream: TStream; const FormatJson: Boolean);
begin
  Json.SaveTo(AStream, FormatJson);

  FUpdated := False;
end;

end.

