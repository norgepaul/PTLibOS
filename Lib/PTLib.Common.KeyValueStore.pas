unit PTLib.Common.KeyValueStore;

interface

uses
  System.SysUtils, System.Variants, System.DateUtils,

  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Utils;

type
  TKeyValueType = (
    kvtUnknown,
    kvtString,
    kvtInt64,
    kvtFloat,
    kvtDateTime,
    kvtBoolean
  );

  TKeyValueStore = class(TInterfacedObject,
                         IKeyValueStore)
  strict private
    FKeyValueCS: TMultiReadExclusiveWriteSynchronizer;
    FRaiseExceptions: Boolean;
  protected
    FUpdated: Boolean;
    FThreadSafe: Boolean;

    procedure DoSetValue(const Name: String; const Value: String; const KeyValueType: TKeyValueType); virtual; abstract;
    procedure DoGetValue(const Name: String; var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType); virtual; abstract;
    procedure DoValueExists(const Name: String; out Exists: Boolean); virtual; abstract;
    procedure DoRemoveValue(const Name: String); virtual; abstract;

    procedure SyncSetValue(const Name: String; const Value: Variant; const KeyValueType: TKeyValueType);
    procedure SyncGetValue(const Name: String; var Value: Variant; out Success: Boolean; const KeyValueType: TKeyValueType);
    procedure SyncValueExists(const Name: String; out Exists: Boolean);
    procedure SyncRemoveValue(const Name: String);
    procedure InternalInc(const Index: String; const Value: Int64);

    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
  private
    procedure SetStringValue(Index: String; const Value: String); overload;
    procedure SetBoolValue(Index: String; const Value: Boolean);
    procedure SetDateValue(Index: String; const Value: TDateTime);
    procedure SetFloatValue(Index: String; const Value: Double);
    procedure SetIntValue(Index: String; const Value: Int64);
    function ValueToString(const Value: Variant; const KeyValueType: TKeyValueType): String;
    function StringToValue(const Value: String; const KeyValueType: TKeyValueType): Variant;
    procedure IncIntValue(const Index: String; const Value: Int64 = 1);
    procedure DecIntValue(const Index: String; const Value: Int64 = 1);
    function ExceptionOrDefaultValue(const Text: String; const Args: array of const; const DefaultValue: Variant): Variant;
    function GetBoolValueInternal(Index: String): Boolean;
    function GetDateValueInternal(Index: String): TDateTime;
    function GetFloatValueInternal(Index: String): Double;
    function GetIntValueInternal(Index: String): Int64;
    function GetStringValueInternal(Index: String): String;
  protected
    function GetStringValue(const Index: String): String; overload;
    function GetStringValue(const Index: String; const Default: String): String; overload;
    function GetStringValue(const Index: String; out Value: String; const Default: String = ''): Boolean; overload;

    function GetBoolValue(const Index: String): Boolean; overload;
    function GetBoolValue(const Index: String; const Default: Boolean): Boolean; overload;
    function GetBoolValue(const Index: String; out Value: Boolean; const Default: Boolean): Boolean; overload;

    function GetDateValue(const Index: String): TDateTime; overload;
    function GetDateValue(const Index: String; const Default: TDateTime): TDateTime; overload;
    function GetDateValue(const Index: String; out Value: TDateTime; const Default: TDateTime): Boolean; overload;

    function GetFloatValue(const Index: String): Double; overload;
    function GetFloatValue(const Index: String; const Default: Double): Double; overload;
    function GetFloatValue(const Index: String; out Value: Double; const Default: Double): Boolean; overload;

    function GetIntValue(const Index: String): Int64; overload;
    function GetIntValue(const Index: String; const Default: Int64): Int64; overload;
    function GetIntValue(const Index: String; out Value: Int64; const Default: Int64): Boolean; overload;

    function ValueExists(const Name: String): Boolean;
    procedure RemoveValue(const Name: String);

    property S[Index: String]: String read GetStringValueInternal write SetStringValue;
    property I[Index: String]: Int64 read GetIntValueInternal write SetIntValue;
    property B[Index: String]: Boolean read GetBoolValueInternal write SetBoolValue;
    property D[Index: String]: TDateTime read GetDateValueInternal write SetDateValue;
    property F[Index: String]: Double read GetFloatValueInternal write SetFloatValue;
  public
    constructor Create(const RaiseExceptions: Boolean = True); virtual;
    destructor Destroy; override;
  end;

implementation

{ TKeyValueStore }

function TKeyValueStore.ExceptionOrDefaultValue(const Text: String;
  const Args: Array of const; const DefaultValue: Variant): Variant;
begin
  if FRaiseExceptions then
  begin
    raise EPTLibKeyValueError.CreateFmt(Text, Args);
  end
  else
  begin
    Result := DefaultValue;
  end;
end;

function TKeyValueStore.GetStringValue(const Index: String): String;
begin
  if not GetStringValue(Index, Result, '') then
  begin
    ExceptionOrDefaultValue('"%s" does not exist', [Index], '');
  end;
end;

function TKeyValueStore.GetStringValue(const Index: String;
  out Value: String; const Default: String): Boolean;
var
  StringVariant: Variant;
begin
  SyncGetValue(Index, StringVariant, Result, TKeyValueType.kvtString);

  Value := StringVariant;

  if not Result then
  begin
    Value := Default;
  end;
end;

function TKeyValueStore.GetStringValueInternal(Index: String): String;
begin
  GetStringValue(Index, Result, '');
end;

procedure TKeyValueStore.InternalInc(const Index: String; const Value: Int64);
var
  Success: Boolean;
  StrVal: String;
  IntVal: Int64;
begin
  BeginWrite;
  try
    DoGetValue(Index, StrVal, Success, TKeyValueType.kvtInt64);

    if not Success then
    begin
      StrVal := '0';
    end;

    IntVal := StrToInt64Def(StrVal, 0);

    Inc(IntVal, Value);

    DoSetValue(Index, ValueToString(IntVal, TKeyValueType.kvtInt64), TKeyValueType.kvtInt64);
  finally
    EndWrite;
  end;
end;

procedure TKeyValueStore.BeginRead;
begin
  if FThreadSafe then
  begin
    FKeyValueCS.BeginRead;
  end;
end;

procedure TKeyValueStore. BeginWrite;
begin
  if FThreadSafe then
  begin
    FKeyValueCS.BeginWrite;
  end;
end;

procedure TKeyValueStore.IncIntValue(const Index: String; const Value: Int64);
begin
  InternalInc(Index, Value);
end;

procedure TKeyValueStore.RemoveValue(const Name: String);
begin
  SyncRemoveValue(Name);
end;

procedure TKeyValueStore.SetBoolValue(Index: String; const Value: Boolean);
begin
  SyncSetValue(Index, Value, TKeyValueType.kvtBoolean);
end;

procedure TKeyValueStore.SetDateValue(Index: String; const Value: TDateTime);
begin
  SyncSetValue(Index, Value, TKeyValueType.kvtDateTime);
end;

procedure TKeyValueStore.SetFloatValue(Index: String; const Value: Double);
begin
  SyncSetValue(Index, Value, TKeyValueType.kvtFloat);
end;

procedure TKeyValueStore.SetIntValue(Index: String; const Value: Int64);
begin
  SyncSetValue(Index, Value, TKeyValueType.kvtInt64);
end;

procedure TKeyValueStore.SetStringValue(Index: String; const Value: String);
begin
  SyncSetValue(Index, Value, TKeyValueType.kvtString);
end;

function TKeyValueStore.GetBoolValue(const Index: String): Boolean;
begin
  if not GetBoolValue(Index, Result, False) then
  begin
    ExceptionOrDefaultValue('"%s" does not exist', [Index], False);
  end;
end;

function TKeyValueStore.GetDateValue(const Index: String): TDateTime;
begin
  if not GetDateValue(Index, Result, 0) then
  begin
    ExceptionOrDefaultValue('"%s" does not exist', [Index], TDateTime(0));
  end;
end;

function TKeyValueStore.GetFloatValue(const Index: String): Double;
begin
  if not GetFloatValue(Index, Result, 0) then
  begin
    ExceptionOrDefaultValue('"%s" does not exist', [Index], 0.0);
  end;
end;

function TKeyValueStore.GetIntValue(const Index: String): Int64;
begin
  if not GetIntValue(Index, Result, 0) then
  begin
    ExceptionOrDefaultValue('"%s" does not exist', [Index], 0);
  end;
end;

procedure TKeyValueStore.SyncGetValue(const Name: String; var Value: Variant; out Success: Boolean; const KeyValueType: TKeyValueType);
var
  ValueStr: String;
begin
  BeginRead;
  try
    DoGetValue(Name, ValueStr, Success, KeyValueType);
  finally
    EndRead;
  end;

  if Success then
  begin
    Value := StringToValue(ValueStr, KeyValueType);
  end;  
end;

procedure TKeyValueStore.SyncRemoveValue(const Name: String);
begin
  BeginWrite;
  try
    DoRemoveValue(Name);
  finally
    EndWrite;
  end;
end;

function TKeyValueStore.ValueToString(const Value: Variant; const KeyValueType: TKeyValueType): String;
begin
  case KeyValueType of
    kvtFloat: Result := FloatToStrUseDot(Value);
    kvtDateTime: Result := DateToISO8601(Value);
    kvtBoolean: Result := BoolToStr(Value, True);
  else
    Result := VarToStr(Value);
  end;
end;

function TKeyValueStore.StringToValue(const Value: String; const KeyValueType: TKeyValueType): Variant;
begin
  case KeyValueType of
    kvtFloat: Result := StrToFloatDefUseDot(Value, 0);
    kvtDateTime: Result := ISO8601ToDate(Value);
    kvtBoolean: Result := Value = DefaultTrueBoolStr;
    kvtInt64: Result := StrToInt64(Value);
  else
    Result := Value;
  end;
end;

procedure TKeyValueStore.SyncSetValue(const Name: String; const Value: Variant; const KeyValueType: TKeyValueType);
begin
  BeginWrite;
  try
    DoSetValue(Name, ValueToString(Value, KeyValueType), KeyValueType);
  finally
    EndWrite;
  end;
end;

procedure TKeyValueStore.SyncValueExists(const Name: String; out Exists: Boolean);
begin
  DoValueExists(Name, Exists);
end;

procedure TKeyValueStore.EndRead;
begin
  if FThreadSafe then
  begin
    FKeyValueCS.EndRead;
  end;
end;

procedure TKeyValueStore.EndWrite;
begin
  if FThreadSafe then
  begin
    FKeyValueCS.EndWrite;
  end;

  FUpdated := True;
end;

function TKeyValueStore.ValueExists(const Name: String): Boolean;
begin
  BeginRead;
  try
    DoValueExists(Name, Result);
  finally
    EndRead;
  end;
end;

constructor TKeyValueStore.Create(const RaiseExceptions: Boolean = True);
begin
  FThreadSafe := True;
  FRaiseExceptions := RaiseExceptions;

  FKeyValueCS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure TKeyValueStore.DecIntValue(const Index: String; const Value: Int64);
begin
  InternalInc(Index, -Value);
end;

destructor TKeyValueStore.Destroy;
begin
  FreeAndNil(FKeyValueCS);

  inherited;
end;

function TKeyValueStore.GetBoolValue(const Index: String; out Value: Boolean;
  const Default: Boolean): Boolean;
var
  BoolVariant: Variant;
begin
  SyncGetValue(Index, BoolVariant, Result, TKeyValueType.kvtBoolean);

  Value := BoolVariant;

  if Result then
  begin
    Value := BoolVariant;
  end
  else
  begin
    Value := Default;
  end;
end;

function TKeyValueStore.GetBoolValueInternal(Index: String): Boolean;
begin
  GetBoolValue(Index, Result, False);
end;

function TKeyValueStore.GetBoolValue(const Index: String; const Default: Boolean): Boolean;
begin
  GetBoolValue(Index, Result, Default);
end;

function TKeyValueStore.GetDateValue(const Index: String; out Value: TDateTime;
  const Default: TDateTime): Boolean;
var
  DateTimeVariant: Variant;
begin
  SyncGetValue(Index, DateTimeVariant, Result, TKeyValueType.kvtDateTime);

  Value := DateTimeVariant;

  if Result then
  begin
    Value := DateTimeVariant;
  end
  else
  begin
    Value := Default;
  end;
end;

function TKeyValueStore.GetDateValueInternal(Index: String): TDateTime;
begin
  GetDateValue(Index, Result, 0);
end;

function TKeyValueStore.GetDateValue(const Index: String; const Default: TDateTime): TDateTime;
begin
  GetDateValue(Index, Result, Default);
end;

function TKeyValueStore.GetFloatValue(const Index: String; const Default: Double): Double;
begin
  GetFloatValue(Index, Result, Default);
end;

function TKeyValueStore.GetFloatValue(const Index: String; out Value: Double;
  const Default: Double): Boolean;
var
  FloatVariant: Variant;
begin
  SyncGetValue(Index, FloatVariant, Result, TKeyValueType.kvtFloat);

  Value := FloatVariant;

  if Result then
  begin
    Value := FloatVariant;
  end
  else
  begin
    Value := Default;
  end;
end;

function TKeyValueStore.GetFloatValueInternal(Index: String): Double;
begin
  GetFloatValue(Index, Result, 0.0);
end;

function TKeyValueStore.GetIntValue(const Index: String; const Default: Int64): Int64;
begin
  GetIntValue(Index, Result, Default);
end;

function TKeyValueStore.GetIntValue(const Index: String; out Value: Int64;
  const Default: Int64): Boolean;
var
  Int64Variant: Variant;
begin
  SyncGetValue(Index, Int64Variant, Result, TKeyValueType.kvtInt64);

  if Result then
  begin
    Value := Int64Variant;
  end
  else
  begin
    Value := Default;
  end;
end;

function TKeyValueStore.GetIntValueInternal(Index: String): Int64;
begin
  GetIntValue(Index, Result, 0);
end;

function TKeyValueStore.GetStringValue(const Index, Default: String): String;
begin
  GetStringValue(Index, Result, Default);
end;

end.
