unit PTLib.Common.SerialPort.Async;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Threading,

  PTLib.Common.Types,
  PTLib.Common.Log,
  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.Thread,
  PTLib.Common.Timers,
  PTLib.Common.Buffers,
  PTLib.Common.InformationList,
  PTLib.Common.SerialPort;

type
  TSerialPortASync = class(TPTLibActiveParamsInterfacedObject,
                           ISerialPortASync)
  strict private
    FOnStateChange: TNotifyEvent;

    FSerialPortSettings: ISerialPortSettings;
    FSerialPortOptions: TSerialPortOptions;
    FSerialPortTask: ITask;
    FSerialPortState: TSerialPortState;
    FSerialPortStateCS: TMultiReadExclusiveWriteSynchronizer;
    FReset: Boolean;
    FSerialPortConnected: Boolean;
    FCompatibleDeviceDetected: Boolean;

    procedure SetSerialPortState(const SerialPort: TBaseSerialPortConnection; const Value: TSerialPortState);
  private
    function GetOnStateChange: TNotifyEvent;
    function GetSerialPortOptions: TSerialPortOptions;
    function GetSerialPortSettings: ISerialPortSettings;
    procedure SetOnStateChange(const Value: TNotifyEvent);
    function GetSerialPortState: TSerialPortState;
  protected
    function ThreadRunning: Boolean;

    procedure DoActiveChanged(const Value: Boolean); override;

    procedure DoGetInformation(Information: IInformationList); override;
    procedure SetCompatibleDeviceDetected;
    procedure DoThread; virtual;
    procedure DoSerialPortComms(const SerialPort: TBaseSerialPortConnection); virtual;
    procedure DoStateChanged(const SerialPort: TBaseSerialPortConnection; const OldValue, NewValue: TSerialPortState); virtual;
    procedure DoReset; virtual;
    procedure DoCreateSerialPort(out Value: TBaseSerialPortConnection); virtual;
    procedure DoWriteData(const SerialPort: TBaseSerialPortConnection; const Data: TBytes); virtual;
    function DoReadData(const SerialPort: TBaseSerialPortConnection; out Data: TBytes; const Count: Integer = 0): Boolean; virtual;
    procedure DoGetBytesAvailable(const SerialPort: TBaseSerialPortConnection; out Value: Integer); virtual;
    procedure DoFlush(const SerialPort: TBaseSerialPortConnection); virtual;
    function CompatibleDeviceDetected: Boolean; virtual;

    function GetLastConnectionError: String;
    procedure Reset;
    procedure Flush(const SerialPort: TBaseSerialPortConnection);
    function BytesAvailable(const SerialPort: TBaseSerialPortConnection): Integer;

    property State: TSerialPortState read GetSerialPortState;
    property SerialPortOptions: TSerialPortOptions read GetSerialPortOptions;
    property SerialPortSettings: ISerialPortSettings read GetSerialPortSettings;
    property OnStateChange: TNotifyEvent read GetOnStateChange write SetOnStateChange;
  public
    constructor Create(SerialPortSettings: ISerialPortSettings; const SerialPortOptions: TSerialPortOptions); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TSerialPortASyncBuffered = class(TSerialPortASync,
                                   ISerialPortASyncBuffered)
  private
    FWriteBuffer: IList<TBytes>;
    FWriteBufferCS: TCriticalSection;
    FReadBuffer: ICircularByteBuffer;
    FPauseWrites: Boolean;

    function GetReadBuffer: ICircularByteBuffer;
    function GetPauseWrites: Boolean;
    procedure SetPauseWrites(const Value: Boolean);
  protected
    const
      ReadBufferSize = 100000;

    procedure DoGetInformation(Information: IInformationList); override;
    procedure DoThread; override;
    procedure DoSerialPortComms(const SerialPort: TBaseSerialPortConnection); override;
    procedure DoFlush(const SerialPort: TBaseSerialPortConnection); override;

    procedure DoWrite(const Data: TBytes; const FrontOfQueue: Boolean); virtual;

    procedure DoDataWritten(const SerialPort: TSerialPortASyncBuffered; const Data: TBytes); virtual;

    procedure FlushWriteBuffer;
    procedure FlushReadBuffer;
    procedure Flush; overload;
    procedure Write(const Data: TBytes; const FrontOfQueue: Boolean = False);
    function GetWriteData(out Data: TBytes): Boolean;
    property ReadBuffer: ICircularByteBuffer read GetReadBuffer;
    function GetWriteQueueCount: Integer;
    function GetPacketsWritten: Cardinal;
    function GetBytesWritten: Cardinal;
    function GetBytesRead: Cardinal;

    property PauseWrites: Boolean read GetPauseWrites write SetPauseWrites;
  public
    constructor Create(SerialPortSettings: ISerialPortSettings; const SerialPortOptions: TSerialPortOptions); override;
    destructor Destroy; override;
  end;

implementation

{ TSerialPortASyncBuffered }

constructor TSerialPortASyncBuffered.Create(SerialPortSettings: ISerialPortSettings; const SerialPortOptions: TSerialPortOptions);
begin
  inherited;

  FWriteBuffer := TList<TBytes>.Create;
  FWriteBufferCS := TCriticalSection.Create;
  FReadBuffer := TCircularByteBuffer.Create(ReadBufferSize);
end;

destructor TSerialPortASyncBuffered.Destroy;
begin
  FreeAndNil(FWriteBufferCS);

  inherited;
end;

procedure TSerialPortASyncBuffered.DoDataWritten(const SerialPort: TSerialPortASyncBuffered; const Data: TBytes);
begin
  // Override
end;

procedure TSerialPortASyncBuffered.DoGetInformation(Information: IInformationList);
begin
  inherited;
end;

procedure TSerialPortASyncBuffered.Write(const Data: TBytes; const FrontOfQueue: Boolean);
begin
  DoWrite(Data, FrontOfQueue);
end;

procedure TSerialPortASyncBuffered.DoWrite(const Data: TBytes; const FrontOfQueue: Boolean);
begin
  FWriteBufferCS.Enter;
  try
    if FrontOfQueue then
    begin
      FWriteBuffer.Insert(0, Data);
    end
    else
    begin
      FWriteBuffer.Add(Data);
    end;
  finally
    FWriteBufferCS.Leave;
  end;
end;

function TSerialPortASyncBuffered.GetWriteData(out Data: TBytes): Boolean; // Called in context of the task thread
begin
  FWriteBufferCS.Enter;
  try
    Result :=
      (not FPauseWrites) and
      (FWriteBuffer.Count > 0);

    if Result then
    begin
      Data := FWriteBuffer[0];

      FWriteBuffer.Delete(0);

      IncParam('PacketsWritten');
      IncParam('BytesWritten', length(Data));
    end;
  finally
    FWriteBufferCS.Leave;
  end;
end;

function TSerialPortASyncBuffered.GetWriteQueueCount: Integer;
begin
  FWriteBufferCS.Enter;
  try
    Result := FWriteBuffer.Count;
  finally
    FWriteBufferCS.Leave;
  end;
end;

procedure TSerialPortASyncBuffered.SetPauseWrites(const Value: Boolean);
begin
  FPauseWrites := Value;
end;

procedure TSerialPortASyncBuffered.DoSerialPortComms(const SerialPort: TBaseSerialPortConnection); // Called in context of the task thread
var
  Data: TBytes;
begin
  inherited;

  if GetWriteData(Data) then
  begin
    DoWriteData(SerialPort, Data);

    DoDataWritten(Self, Data);
  end;

  if DoReadData(SerialPort, Data) then
  begin
    ReadBuffer.Write(Data, TSerialPortOption.spoWrapReadBuffer in SerialPortOptions);

    IncParam('BytesRead', length(Data));
  end;
end;

procedure TSerialPortASyncBuffered.DoThread;
begin
  inherited;
end;

procedure TSerialPortASyncBuffered.DoFlush(const SerialPort: TBaseSerialPortConnection);
begin
  FlushReadBuffer;
  FlushWriteBuffer;
end;

procedure TSerialPortASyncBuffered.Flush;
begin
  Flush(nil);
end;

procedure TSerialPortASyncBuffered.FlushReadBuffer;
begin
end;

procedure TSerialPortASyncBuffered.FlushWriteBuffer;
begin
  FWriteBufferCS.Enter;
  try
    FWriteBuffer.Clear;
  finally
    FWriteBufferCS.Leave;
  end;
end;

function TSerialPortASyncBuffered.GetBytesRead: Cardinal;
begin
  Result := GetParam('BytesRead', 0);
end;

function TSerialPortASyncBuffered.GetBytesWritten: Cardinal;
begin
  Result := GetParam('BytesWritten', 0);
end;

function TSerialPortASyncBuffered.GetPacketsWritten: Cardinal;
begin
  Result := GetParam('PacketsWritten', 0);
end;

function TSerialPortASyncBuffered.GetPauseWrites: Boolean;
begin
  Result := FPauseWrites;
end;

function TSerialPortASyncBuffered.GetReadBuffer: ICircularByteBuffer;
begin
  Result := FReadBuffer;
end;

{ TSerialPortASync }

function TSerialPortASync.CompatibleDeviceDetected: Boolean;
begin
  Result := FCompatibleDeviceDetected;
end;

constructor TSerialPortASync.Create(SerialPortSettings: ISerialPortSettings; const SerialPortOptions: TSerialPortOptions);
begin
  inherited Create;

  FSerialPortSettings := SerialPortSettings;
  FSerialPortOptions := SerialPortOptions;
  FSerialPortConnected := False;

  FSerialPortStateCS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TSerialPortASync.Destroy;
begin
  FOnStateChange := nil;

  Active := False;

  while FSerialPortTask <> nil do
  begin
    sleep(1);
  end;

  FreeAndNil(FSerialPortStateCS);

  inherited;
end;

procedure TSerialPortASync.DoActiveChanged(const Value: Boolean);
begin
  inherited;

  if Value then
  begin
    if FSerialPortSettings <> nil then
    begin
      TGLobalLog.Log(
        Self,
        'Serial Port Activated on "%s"',
        [FSerialPortSettings.SerialPort],
        LogSeverityInfo);
    end;

    FReset := False;
    FCompatibleDeviceDetected := False;

    DoThread;
  end
  else
  begin
    SetParam('ConnectionError', '');

    if FSerialPortTask <> nil then
    begin
      FSerialPortTask.Cancel;
    end;
  end;
end;

procedure TSerialPortASync.DoReset;
begin
  // Override
end;

procedure TSerialPortASync.DoSerialPortComms(const SerialPort: TBaseSerialPortConnection); // Called in context of the task thread
begin
  if FReset then
  begin
    FReset := False;

    DoReset;
  end;
end;

procedure TSerialPortASync.DoCreateSerialPort(out Value: TBaseSerialPortConnection);
begin
  Value := TBaseSerialPortConnection.Create;
end;

procedure TSerialPortASync.DoFlush(const SerialPort: TBaseSerialPortConnection);
begin
  SerialPort.Flush;
end;

procedure TSerialPortASync.DoWriteData(const SerialPort: TBaseSerialPortConnection; const Data: TBytes);
begin
  SerialPort.Write(Data);
end;

procedure TSerialPortASync.Flush(const SerialPort: TBaseSerialPortConnection);
begin
  DoFlush(SerialPort);
end;

procedure TSerialPortASync.DoGetBytesAvailable(const SerialPort: TBaseSerialPortConnection; out Value: Integer);
begin
  Value := SerialPort.BytesAvailable;
end;

procedure TSerialPortASync.DoGetInformation(Information: IInformationList);
begin
  Information.AddHeading('Serial Port');

  if SerialPortSettings <> nil then
  begin
    Information.AddProperty('Port', SerialPortSettings.SerialPort);
  end;

  Information.AddProperty('Compatible device detected', BoolToStr(FCompatibleDeviceDetected, True));
end;

function TSerialPortASync.BytesAvailable(const SerialPort: TBaseSerialPortConnection): Integer;
begin
  DoGetBytesAvailable(SerialPort, Result);
end;

function TSerialPortASync.DoReadData(const SerialPort: TBaseSerialPortConnection; out Data: TBytes; const Count: Integer): Boolean;
var
  ReadCount: Integer;
begin
  if Count = 0 then
  begin
    ReadCount := BytesAvailable(SerialPort);
  end
  else
  begin
    ReadCount := Count;
  end;

  Result :=
    (ReadCount > 0) and
    (BytesAvailable(SerialPort) >= ReadCount);

  if Result then
  begin
    SerialPort.ReadBytes(ReadCount, Data);
  end;
end;

procedure TSerialPortASync.DoThread; // Called in context of the task thread
begin
  FSerialPortTask := TTask.Create(
    procedure
    const
      ReconnectDelay = 5000;
    var
      SerialPort: TBaseSerialPortConnection;
      ReconnectTicks: Cardinal;
      LastError: String;
    begin
      ReconnectTicks := 0;

      DoCreateSerialPort(SerialPort);
      try
        SetSerialPortState(SerialPort, TSerialPortState.spsConnecting);

        if SerialPort <> nil then
        begin
          TGLobalLog.Log(
            Self,
            'Configuring Serial Port "%s"',
            [FSerialPortSettings.SerialPort],
            LogSeverityInfo);

          SerialPort.ConfigureSerialPort(FSerialPortSettings);
        end;

        if FSerialPortConnected then
        begin
          TGLobalLog.Log(
            Self,
            'Serial port still in use. Waiting for it to disconnect.',
            LogSeverityInfo);

          while (FSerialPortConnected) and
                (ThreadRunning) do
          begin
            Sleep(5);
          end;
        end;

        while ThreadRunning do
        begin
          if (ReconnectTicks <> 0) and
             (ReconnectTicks > TThread.GetTickCount) then
          begin
            sleep(50);

            Continue;
          end;

          try
            if SerialPort <> nil then
            begin
              TGLobalLog.Log(
                Self,
                'Connecting to Serial Port "%s"',
                [FSerialPortSettings.SerialPort],
                LogSeverityInfo);

              SerialPort.Connect;

              FSerialPortConnected := True;

              SetParam('ConnectionError', '');

              TGLobalLog.Log(
                Self,
                'Connected to Serial Port "%s"',
                [FSerialPortSettings.SerialPort],
                LogSeverityInfo);
            end;

            SetSerialPortState(SerialPort, TSerialPortState.spsConnected);

            LastError := '';

            while ThreadRunning do
            begin
              try
                DoSerialPortComms(SerialPort);
              except
                on e: ESerialPortError do
                begin
                  // We have an exception from the serial port
                  if e.Message <> LastError then
                  begin
                    TGLobalLog.Log(
                      Self,
                      'Error: %s',
                      [e.Message],
                      LogSeverityError);

                    LastError := e.Message;
                  end;

                  // We're disconnected
                  if not SerialPort.Connected then
                  begin
                    Break;
                  end;
                end;
              end;

              sleep(1);
            end;
          except
            on e: Exception do
            begin
              SetParam('ConnectionError', e.Message);

              TGLobalLog.Log(
                Self,
                'Connection Failed: %s',
                [e.Message],
                LogSeverityError);
            end;
          end;

          SetSerialPortState(SerialPort, TSerialPortState.spsDisconnected);

(*          TGLobalLog.Log(
            Self,
            'Connection Closed',
            LogSeverityInfo);*)

          if (ThreadRunning) and
             (TSerialPortOption.spoAutoReconnect in FSerialPortOptions) then
          begin
            TGLobalLog.Log(
              Self,
              'Trying again in %dms',
              [ReconnectDelay],
              LogSeverityInfo);

              ReconnectTicks := TThread.GetTickCount + ReconnectDelay;
          end
          else
          begin
            Break;
          end;
        end;
      finally
        FSerialPortConnected := False;

        FSerialPortTask := nil;

        if Assigned (SerialPort) then
        begin
          SerialPort.Disconnect;

          SetSerialPortState(SerialPort, TSerialPortState.spsIdle);

          FreeAndNil(SerialPort);
        end;
      end;
    end
  );

  FSerialPortTask.Start;
end;

function TSerialPortASync.GetSerialPortOptions: TSerialPortOptions;
begin
  Result := FSerialPortOptions;
end;

function TSerialPortASync.GetSerialPortSettings: ISerialPortSettings;
begin
  Result := FSerialPortSettings;
end;

function TSerialPortASync.GetSerialPortState: TSerialPortState;
begin
  FSerialPortStateCS.BeginRead;
  try
    Result := FSerialPortState;
  finally
    FSerialPortStateCS.EndRead;
  end;
end;

procedure TSerialPortASync.Reset;
begin
  FReset := True;
end;

function TSerialPortASync.GetLastConnectionError: String;
begin
  Result := GetParam('ConnectionError', '');
end;

function TSerialPortASync.GetOnStateChange: TNotifyEvent;
begin
  Result := FOnStateChange;
end;

procedure TSerialPortASync.SetCompatibleDeviceDetected;
begin
  if (not FCompatibleDeviceDetected) and
     (FSerialPortSettings <> nil) then
  begin
    TGLobalLog.Log(
      Self,
      'Compatible device detected on serial port "%s"',
      [FSerialPortSettings.SerialPort],
      LogSeverityInfo);

    FCompatibleDeviceDetected := True;
  end;
end;

procedure TSerialPortASync.SetOnStateChange(const Value: TNotifyEvent);
begin
   FOnStateChange := Value;
end;

procedure TSerialPortASync.SetSerialPortState(const SerialPort: TBaseSerialPortConnection; const Value: TSerialPortState); // Called in context of the task thread
var
  OldValue: TSerialPortState;
begin
  FSerialPortStateCS.BeginWrite;
  try
    OldValue := FSerialPortState;

    FSerialPortState := Value;
  finally
    FSerialPortStateCS.EndWrite;
  end;

  if OldValue <> Value then
  begin
    DoStateChanged(SerialPort, OldValue, Value);
  end;
end;

function TSerialPortASync.ThreadRunning: Boolean;
begin
  Result := (FSerialPortTask <> nil) and (FSerialPortTask.Status = TTaskStatus.Running);
end;

procedure TSerialPortASync.DoStateChanged(const SerialPort: TBaseSerialPortConnection;
  const OldValue, NewValue: TSerialPortState); // Called in context of thread
begin
  if Assigned(FOnStateChange) then
  begin
    if TSerialPortOption.spoSynchronizeEvents in SerialPortOptions then
    begin
      TThread.Synchronize(
        nil,
        procedure
        begin
          FOnStateChange(Self);
        end
      );
    end
    else
    begin
      FOnStateChange(Self);
    end;
  end;
end;

end.
