unit PTLib.Common.ThreadCollection;

interface

uses

  dialogs,

  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  PTLib.Common.LockedTypes;


type

  TPTThreadCollection      = class;
  TPTThreadCollectionItem  = class;

  EPTLibThreadCollectionError = class(exception);

  TPTThreadCollectionItem = class(TThread)
  private
    FOwner:     TPTThreadCollection;
    FFailed:    TPTLockedValue<Boolean>;
    FError:     TPTLockedValue<String>;
  protected
    Procedure   SignalOperationStatus;virtual;
    Procedure   FailWith(Error: String);
    Procedure   Success;
  protected
    procedure   Execute;override;
  public
    Property    Failed: TPTLockedValue<Boolean> read FFailed;
    property    LastError: TPTLockedValue<String> read FError;

    Constructor Create(AOwner:TPTThreadCollection);virtual;
    Destructor  Destroy;Override;
  end;

  IPTThreadCollection = Interface
    ['{4D9679B1-8750-4660-A312-80544BBA1E0E}']
    Procedure RegisterThread(Thread: TPTThreadCollectionitem);
    Procedure ThreadOperationFinished(Thread: TPTThreadCollectionitem);
  end;

  TPTThreadCollectionOperationDoneEvent = procedure (sender: TObject; Thread: TPTThreadCollectionItem) of object;

  TPTThreadCollection = Class(TInterfacedPersistent, IPTThreadCollection)
  private
    FOnDone:    TPTThreadCollectionOperationDoneEvent;
    FObjects:   TObjectList<TPTThreadCollectionitem>;
    FLock:      TCriticalSection;
    FDisposing: TPTLockedValue<Boolean>;
    function    GetCount:Integer;
  strict private
    // Implements: IPTThreadCollection
    Procedure   RegisterThread(Thread: TPTThreadCollectionitem);
    Procedure   ThreadOperationFinished(Thread: TPTThreadCollectionitem);
  protected
    procedure   HandleThreadTerminated(Sender: TObject);
  public
    property    Count:Integer read GetCount;

    function    Contains(Thread: TPTThreadCollectionitem):Boolean;

    function    Terminate(Thread: TPTThreadCollectionitem):Boolean;
    procedure   TerminateAndWaitForAll;

    Constructor Create;virtual;
    Destructor  Destroy;Override;
  published
    Property    OnThreadDone:TPTThreadCollectionOperationDoneEvent read FOnDone write FOnDone;
  end;

implementation

//#############################################################################
// TPTThreadCollectionItem
//#############################################################################

constructor TPTThreadCollectionItem.Create(AOwner: TPTThreadCollection);
var
  Access: IPTThreadCollection;
begin
  inherited Create(True);
  FOwner := AOwner;

  FFailed:=TPTLockedValue<Boolean>.Create;
  FError:=TPTLockedValue<String>.Create;

  if FOwner = NIL then
    Raise EPTLibThreadCollectionError.Create('Invalid thread collection error');

  if FOwner.GetInterface(IPTThreadCollection,Access) then
    Access.RegisterThread(self);
end;

Destructor TPTThreadCollectionItem.Destroy;
begin
  FFailed.Free;
  FError.Free;
  inherited;
end;

procedure TPTThreadCollectionItem.Execute;
begin
  while not terminated do
  begin
    sleep(28);
    if terminated then
    break;
  end;
  Success;
end;

procedure TPTThreadCollectionItem.FailWith(Error: String);
begin
  FFailed.Value := true;
  FError.Value := Error;

  TThread.Synchronize(NIL,
    procedure
    begin
      SignalOperationStatus;
    end);
end;

procedure TPTThreadCollectionItem.Success;
begin
  FFailed.Value := false;
  FError.Value := '';
  TThread.Synchronize(NIL,
    procedure
    begin
      SignalOperationStatus;
    end);
end;

// Note: Must be called through synchronize (see Success and FailWith)
procedure TPTThreadCollectionItem.SignalOperationStatus;
var
  Access: IPTThreadCollection;
begin
  if FOwner.GetInterface(IPTThreadCollection,Access) then
    Access.ThreadOperationFinished(self);
end;

//#############################################################################
// TPTThreadCollection
//#############################################################################

constructor TPTThreadCollection.Create;
begin
  inherited Create;
  FObjects := TObjectList<TPTThreadCollectionitem>.Create(false);
  FLock := TCriticalSection.Create;
  FDisposing:=TPTLockedValue<Boolean>.create;
  FDisposing.Value := false;
end;

destructor TPTThreadCollection.Destroy;
begin
  // Dispose of running threads before we kill this object
  if (GetCount > 0) then
    TerminateAndWaitForAll;

  FObjects.Free;
  FLock.Free;
  FDisposing.Free;
  inherited;
end;

procedure TPTThreadCollection.TerminateAndWaitForAll;
var
  Count:  Integer;
  Thread: TThread;
begin
  // Flag for mass disposing of objects (see ThreadFinished)
  FDisposing.Value := true;
  try

    // Poll count as a single read operation
    Count := getCount;

    While Count>0 do
    begin

      // Get obj pointer as read operation
      FLock.Enter;
      try
        try
          Thread := FObjects[0];
        except
          (* Note: depending on CPU and payload count may change while we
                   dispose, hence we gracefully wipe the pointer here *)
          on exception do
          begin
            Thread := NIL;
          end;
        end;
      finally
        FLock.Leave;
      end;

      // Validate first, if the thread has un-registered (write) before we get here
      // the pointer will be NIL (!)
      if Thread <> NIL then
      Begin
        Thread.Terminate;
        Thread.WaitFor;
        Thread.Free;
      end;

      dec(Count);
    end;

  finally
    FDisposing.Value := false;
  end;

end;

function TPTThreadCollection.GetCount: Integer;
begin
  FLock.Enter;
  try
    result := FObjects.Count;
  finally
    FLock.Leave;
  end;
end;

function TPTThreadCollection.Contains(Thread: TPTThreadCollectionitem):Boolean;
begin
  result := false;

  if thread<>NIL then
  begin
    FLock.Enter;
    try
      result := FObjects.IndexOf(thread) >= 0;
    finally
      FLock.Leave;
    end;
  end;
end;

function TPTThreadCollection.Terminate(Thread: TPTThreadCollectionitem):Boolean;
begin
  result := false;

  if thread<>NIL then
  begin

    // Make sure thread is in collection
    FLock.Enter;
    try
      result := FObjects.IndexOf(thread)>=0;
    finally
      FLock.Leave;
    end;

    if result then
    begin
      TThread.Queue(NIL,
        procedure
        begin
          try
            Thread.Terminate;
          except
            // An exception here means the thread was terminated
            // during the read operation
            on exception do;
          end;
        end);
    end;

  end;
end;

procedure TPTThreadCollection.RegisterThread(Thread: TPTThreadCollectionitem);
begin
  if (thread<>NIL) then
  begin
    if not contains(thread) then
    begin

      // Do not allow adding while we are killing threads (see TerminateAndWaitForAll)
      if not FDisposing.Value then
      begin

        // Adjust thread to expected behavior
        thread.FreeOnTerminate := false;
        thread.OnTerminate := HandleThreadTerminated;

        FLock.Enter;
        try
          FObjects.Add(Thread);
        finally
          FLock.Leave;
        end;

      end else
        raise EPTLibThreadCollectionError.Create('Failed to add thread, collection is being purged');
    end else
      raise EPTLibThreadCollectionError.Create('Failed to add thread, thread already exists in collection error');
  end else
    Raise EPTLibThreadCollectionError.Create('Failed to add thread to collection, reference was NIL error');
end;

procedure TPTThreadCollection.ThreadOperationFinished(Thread: TPTThreadCollectionitem);
begin
  If assigned(FOnDone) then
    FOnDone(self,Thread);
end;

procedure TPTThreadCollection.HandleThreadTerminated(Sender: TObject);
begin

  FLock.Enter;
  try
    FObjects.Remove(TPTThreadCollectionitem(sender));
  finally
    FLock.Leave;
  end;

  // Avoid lock-up when TerminateAndWaitForAll() is doing a scrape
  if not FDisposing.Value then
    TThread.Queue(NIL,
      procedure
      begin
        TThread(sender).Free;
      end);
end;



end.
