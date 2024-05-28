unit PTLib.Common.Buffers;

interface

uses
  System.SysUtils, System.Generics.Collections, System.SyncObjs,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  EBufferFull = class(EPTLibError);
  EBufferEmpty = class(EPTLibError);

  TCircularBuffer<T> = class(TInterfacedObject, ICircularBuffer<T>)
  private
    FSize: integer;
  protected
    FBuffer: TQueue<T>;
  public
    constructor Create(const Size: integer); virtual;
    destructor Destroy; override;

    procedure Write(const Value: T; const AllowOverwrite: Boolean = False); overload; virtual;
    procedure Write(const Values: TArray<T>; const AllowOverwrite: Boolean = False); overload; virtual;
    function Read: T; overload; virtual;
    function Read(out Value: T): Boolean; overload; virtual;
    function Read(const Count: Integer): TArray<T>; overload; virtual;
    function Read(const Count: Integer; out Values: TArray<T>): Boolean; overload; virtual;
    function Size: Integer; virtual;
    procedure Clear; virtual;
  end;

  TCircularBufferThreadSafe<T> = class(TCircularBuffer<T>, ICircularBuffer<T>)
  strict private
    FLock: TCriticalSection;
  protected
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(const Size: Integer); override;
    destructor Destroy; override;

    procedure Write(const Value: T; const AllowOverwrite: Boolean = False); overload; override;
    procedure Write(const Values: TArray<T>; const AllowOverwrite: Boolean = False); overload; override;
    function Read: T; overload; override;
    function Read(out Value: T): Boolean; overload; override;
    function Read(const Count: Integer): TArray<T>; overload; override;
    function Read(const Count: Integer; out Values: TArray<T>): Boolean; overload; override;
    function Size: Integer; override;
    procedure Clear; override;
  end;

  TCircularByteBuffer = class(TCircularBufferThreadSafe<Byte>, ICircularByteBuffer)
  public
    function Read(const UntilDelimiter: Byte; out Values: TBytes): Boolean; overload; virtual;
  end;

implementation

function TCircularBuffer<T>.Size: Integer;
begin
  Result := FBuffer.Count;
end;

constructor TCircularBuffer<T>.Create(const Size: integer);
begin
  inherited Create;

  FSize := Size;
  FBuffer := TQueue<T>.Create;
end;

destructor TCircularBuffer<T>.Destroy;
begin
  FBuffer.Clear;

  FreeAndNil(FBuffer);

  inherited;
end;

procedure TCircularBuffer<T>.Write(const Value: T; const AllowOverwrite: Boolean);
begin
  if FBuffer.Count >= FSize then
  begin
    if AllowOverwrite then
    begin
      FBuffer.Dequeue;
    end
    else
    begin
      raise EBufferFull.create('Unable to write, buffer full');
    end;
  end;

  FBuffer.Enqueue(Value);
end;

function TCircularBuffer<T>.Read: T;
begin
  if FBuffer.Count = 0 then
  begin
    raise EBufferEmpty.Create('The buffer is empty');
  end
  else
  begin
    Result := FBuffer.Dequeue;
  end;
end;

function TCircularBuffer<T>.Read(out Value: T): Boolean;
begin
  Result := FBuffer.Count > 0;

  if Result then
  begin
    Value := FBuffer.Dequeue;
  end;
end;

procedure TCircularBuffer<T>.Clear;
begin
  FBuffer.Clear;
end;

function TCircularBuffer<T>.Read(const Count: Integer): TArray<T>;
begin
  Read(Count, Result);
end;

function TCircularBuffer<T>.Read(const Count: Integer; out Values: TArray<T>): Boolean;
var
  i: Integer;
  Value: T;
begin
  Result := Size >= Count;

  if Result then
  begin
    SetLength(Values, Count);

    for i := 1 to Count do
    begin
      if Read(Value) then
      begin
        Values[i - 1] := Value;
      end
      else
      begin
        Exit(False);
      end;
    end;
  end;
end;

procedure TCircularBuffer<T>.Write(const Values: TArray<T>; const AllowOverwrite: Boolean);
var
  i: Integer;
begin
  for i := Low(Values) to High(Values) do
  begin
    Write(Values[i], AllowOverwrite);
  end;
end;


{ TCircularBufferThreadSafe<T> }

procedure TCircularBufferThreadSafe<T>.Clear;
begin
  Lock;
  try
    inherited;
  finally
    Unlock;
  end;
end;

function TCircularBufferThreadSafe<T>.Size: Integer;
begin
  Lock;
  try
    Result := inherited;
  finally
    Unlock;
  end;
end;

constructor TCircularBufferThreadSafe<T>.Create(const Size: Integer);
begin
  inherited;

  FLock := TCriticalSection.Create;
end;

destructor TCircularBufferThreadSafe<T>.Destroy;
begin
  FreeAndNil(FLock);

  inherited;
end;

procedure TCircularBufferThreadSafe<T>.Lock;
begin
  FLock.Enter;
end;

function TCircularBufferThreadSafe<T>.Read(const Count: Integer): TArray<T>;
begin
  Lock;
  try
    Result := inherited Read(Count);
  finally
    Unlock;
  end;
end;

function TCircularBufferThreadSafe<T>.Read(const Count: Integer; out Values: TArray<T>): Boolean;
begin
  Lock;
  try
    Result := inherited Read(Count, Values);
  finally
    Unlock;
  end;
end;

function TCircularBufferThreadSafe<T>.Read(out Value: T): Boolean;
begin
  Lock;
  try
    Result := inherited Read(Value);
  finally
    Unlock;
  end;
end;

function TCircularBufferThreadSafe<T>.Read: T;
begin
  Lock;
  try
    Result := inherited Read;
  finally
    Unlock;
  end;
end;

procedure TCircularBufferThreadSafe<T>.Unlock;
begin
  FLock.Leave;
end;

procedure TCircularBufferThreadSafe<T>.Write(const Values: TArray<T>; const AllowOverwrite: Boolean);
begin
  Lock;
  try
    inherited Write(Values, AllowOverwrite);
  finally
    Unlock;
  end;
end;

procedure TCircularBufferThreadSafe<T>.Write(const Value: T; const AllowOverwrite: Boolean);
begin
  Lock;
  try
    inherited Write(Value, AllowOverwrite);
  finally
    Unlock;
  end;
end;


{ TCircularByteBuffer }

function TCircularByteBuffer.Read(const UntilDelimiter: Byte; out Values: TBytes): Boolean;
var
  i: Integer;
  Count: Integer;
  Value: Byte;
  Temp: String;
begin
  Result := False;

  Temp := '';

  Lock;
  try
    Count := 0;

    for Value in FBuffer do
    begin
      Inc(Count);

      Temp := Temp + chr(Value);

      if Value = UntilDelimiter then
      begin
        Result := True;

        Break;
      end;
    end;

    if Result then
    begin
      SetLength(Values, Count);

      for i := 1 to Count do
      begin
        Values[i - 1] := FBuffer.Dequeue;
      end;
    end;
  finally
    UnLock;
  end;
end;

end.
