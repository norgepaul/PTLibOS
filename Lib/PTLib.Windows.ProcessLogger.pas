unit PTLib.Windows.ProcessLogger;

interface

uses
  SysUtils, Classes, WinApi.Windows,

  PTLib.Common.Classes,
  PTLib.Common.Strings,
  PTLib.Common.Thread,
  PTLib.Common.Log;

type
  TProcessLogger = class;

  TProcessLoggerThread = class(TPTLibThread)
  strict private
    FProcessLogger: TProcessLogger;
    FFilename: String;
    FParameters: String;
    FProcessID: Cardinal;
    FException: Exception;
    FData: String;
    FDelimiter: String;
    FDataBuffer: String;

    procedure DataReceived(const Data: String);
    procedure SynchronizeData;
  public
    constructor Create(const ProcessLogger: TProcessLogger; const Filename: String; const Parameters: String; const Delimiter: String);

    procedure Execute; override;

    property Error: Exception read FException;
    property ProcessID: Cardinal read FProcessID;
  end;

  TProcessDataCallback = reference to procedure(const ProcessID: Cardinal; const Data: String);

  TProcessLogger = class(TPTLibLog)
  strict private
    FProcessLoggerThread: TProcessLoggerThread;
    FFilename: String;
    FParameters: String;
    FDelimiter: String;

    procedure OnThreadTerminate(Sender: TObject);
  private
    procedure DestroyProcessLoggerThread;
    procedure SetDelimiter(const Value: String);
    procedure SetFilename(const Value: String);
    procedure SetParameters(const Value: String);
  protected
    procedure InternalDataCallback(const ProcessID: Cardinal; const Data: String);
    procedure SetActive(const Value: Boolean); override;
  published
    property Filename: String read FFilename write SetFilename;
    property Parameters: String read FParameters write SetParameters;
    property Delimiter: String read FDelimiter write SetDelimiter;
  end;

implementation

{ TProcessLogger }

procedure TProcessLogger.InternalDataCallback(const ProcessID: Cardinal;
  const Data: String);
var
  LogSeverity: Integer;
begin
  if pos('ERROR', AnsiUpperCase(Data)) <> 0 then
  begin
    LogSeverity := LogSeverityError;
  end
  else
  begin
    LogSeverity := LogSeverityInfo;
  end;

  Log(
    Data,
    '',
    LogSeverity
  );
end;

procedure TProcessLogger.OnThreadTerminate(Sender: TObject);
begin
  if FProcessLoggerThread.Error <> nil then
  begin
    Log(
      FProcessLoggerThread.Error.Message,
      '',
      LogSeverityError
    );
  end;

  DestroyProcessLoggerThread;
end;

procedure TProcessLogger.SetActive(const Value: Boolean);
begin
  if (not (csDesigning in ComponentState)) and
     (Value <> Active) then
  begin
    if Value then
    begin
      FProcessLoggerThread := TProcessLoggerThread.Create(Self, FFilename, FParameters, FDelimiter);
      FProcessLoggerThread.OnTerminate := OnThreadTerminate;
    end
    else
    begin
      DestroyProcessLoggerThread;
    end;
  end;

  inherited;
end;

procedure TProcessLogger.SetDelimiter(const Value: String);
begin
  if FDelimiter <> Value then
  begin
    FDelimiter := Value;

    Active := False;
  end;
end;

procedure TProcessLogger.SetFilename(const Value: String);
begin
  if FFilename <> Value then
  begin
    FFilename := Value;

    Active := False;
  end;
end;

procedure TProcessLogger.SetParameters(const Value: String);
begin
  if FParameters <> Value then
  begin
    FParameters := Value;

    Active := False;
  end;
end;

procedure TProcessLogger.DestroyProcessLoggerThread;
begin
  if FProcessLoggerThread <> nil then
  begin
    FProcessLoggerThread.OnTerminate := nil;
    FProcessLoggerThread.Terminate;
    FProcessLoggerThread := nil;
  end;
end;

{ TProcessLoggerThread }

constructor TProcessLoggerThread.Create(const ProcessLogger: TProcessLogger; const Filename: String; const Parameters: String; const Delimiter: String);
begin
  inherited Create(False);

  FProcessLogger := ProcessLogger;

  FreeOnTerminate := True;

  FFilename := Filename;
  FParameters := Parameters;
  FDelimiter := Delimiter;
end;

procedure TProcessLoggerThread.DataReceived(const Data: String);
begin
  FDataBuffer := FDataBuffer + Data;

  if FDelimiter = '' then
  begin
    FData := FDataBuffer;
    FDataBuffer := '';

    Synchronize(SynchronizeData);    
  end
  else
  begin
    while pos(FDelimiter, FDataBuffer) <> 0 do
    begin
      FData := NextBlock(FDataBuffer, FDelimiter);

      if FData <> '' then
      begin
        Synchronize(SynchronizeData);          
      end;
    end;
  end;
end;

procedure TProcessLoggerThread.SynchronizeData;
begin
  FProcessLogger.InternalDataCallback(FProcessID, FData);
end;

procedure TProcessLoggerThread.Execute;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  try
    // Start the process
    with SA do begin
      nLength := SizeOf(SA);
      bInheritHandle := True;
      lpSecurityDescriptor := nil;
    end;

    CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
    try
      with SI do
      begin
        FillChar(SI, SizeOf(SI), 0);
        cb := SizeOf(SI);
        dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        wShowWindow := SW_HIDE;
        hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
        hStdOutput := StdOutPipeWrite;
        hStdError := StdOutPipeWrite;
      end;
      WorkDir := ExtractFileDir(FFilename);

      Handle := CreateProcess(
        nil,
        PChar('cmd.exe /C "' + FFilename + ' ' + FParameters + '"'),
        nil,
        nil,
        True,
        0,
        nil,
        PChar(WorkDir),
        SI,
        PI);

      FProcessID := PI.dwProcessId;

      CloseHandle(StdOutPipeWrite);

      if Handle then
      begin
        try
          repeat
            WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);

            if BytesRead > 0 then
            begin
              Buffer[BytesRead] := #0;

              DataReceived(String(Buffer));
            end;
          until not WasOK or (BytesRead = 0);

          WaitForSingleObject(PI.hProcess, INFINITE);
        finally
          CloseHandle(PI.hThread);
          CloseHandle(PI.hProcess);
        end;
      end;
    finally
      CloseHandle(StdOutPipeRead);
    end;
  except
    on e: Exception do
    begin
      FException := e;
    end;
  end;
end;

end.

