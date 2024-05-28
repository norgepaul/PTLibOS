unit PTLib.Common.Pools;

interface

uses
  System.SysUtils, System.Generics.Collections,

  PTLib.Common.Log,
  PTLib.Common.Types;

type
  TThreadSafePool<T: class> = class
  type
    TCreateItemProc = reference to procedure(out Value: T);
    TtemProc = reference to procedure(const Value: T);
  strict private
    FPool: TObjectStack<T>;
    FPopped: TList<T>;
    FOnNewItemProc: TCreateItemProc;
    FOnPoolPopProc: TtemProc;
    FOnPoolPushProc: TtemProc;
    FPoolSize: Integer;
  public
    constructor Create(const OnNewItemProc: TCreateItemProc; const OnPoolPopProc: TtemProc = nil; const OnPoolPushProc: TtemProc = nil;
      const PoolSize: Integer = 0);
    destructor Destroy; override;

    function Pop(out Value: T): Boolean;
    procedure Push(const Item: T);
  end;

implementation

{ TThreadSafePool<T> }

constructor TThreadSafePool<T>.Create(const OnNewItemProc: TCreateItemProc; const OnPoolPopProc: TtemProc = nil; const OnPoolPushProc: TtemProc = nil;
  const PoolSize: Integer = 0);
begin
  FPool := TObjectStack<T>.Create(True);
  FPopped := TList<T>.Create;

  FOnNewItemProc := OnNewItemProc;
  FOnPoolPopProc := OnPoolPopProc;
  FOnPoolPushProc := OnPoolPushProc;

  FPoolSize := PoolSize;
end;

destructor TThreadSafePool<T>.Destroy;
begin
  FreeAndNil(FPool);
  FreeAndNil(FPopped);

  inherited;
end;

function TThreadSafePool<T>.Pop(out Value: T): Boolean;
begin
  TMonitor.Enter(FPool);
  try
    if (FPoolSize > 0) and
       (FPool.Count + FPopped.Count >= FPoolSize) then
    begin
      raise Exception.CreateFmt('Max pool size of %d reached', [FPoolSize]);
    end else
    if FPool.Count = 0 then
    begin
      FOnNewItemProc(Value);
    end
    else
    begin
      Value := FPool.Extract;
    end;

    FPopped.Add(Value);
  finally
    TMonitor.Exit(FPool);
  end;

  if (Assigned(FOnPoolPopProc)) and
     (Value <> nil) then
  begin
    try
      FOnPoolPopProc(Value);
    except
      on e: Exception do
      begin
        Push(Value);

        Value := nil;

        TGLobalLog.Log(
          Self,
          'Exception removing item from pool: %s',
          [e.Message],
          LogSeverityError);
      end;
    end;
  end;

  Result := Value <> nil;
end;

procedure TThreadSafePool<T>.Push(const Item: T);
var
  idx: Integer;
begin
  if Item <> nil then
  begin
    TMonitor.Enter(FPool);
    try
      idx := FPopped.IndexOf(Item);

      if idx = -1 then
      begin
        raise Exception.Create('Unable to push an item that hasn''t been popped');
      end
      else
      begin
        FPopped.Delete(idx);
      end;

      FPool.Push(Item);
    finally
      TMonitor.Exit(FPool);
    end;

    if Assigned(FOnPoolPushProc) then
    begin
      try
        FOnPoolPushProc(Item);
      except
        on e: Exception do
        begin
          TGLobalLog.Log(
            Self,
            'Exception returning item to pool: %s',
            [e.Message],
            LogSeverityError);
        end;
      end;
    end;
  end;
end;

end.
