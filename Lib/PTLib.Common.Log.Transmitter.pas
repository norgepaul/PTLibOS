{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Log.Transmitter                             }
{                                                                    }
{           Copyright (c) 1998-2016 Easy-IP AS.                      }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF EASY-IP AS. THE REGISTERED DEVELOPER                  }
{   LICENSED TO DISTRIBUTE THE CODE HEREIN AND ANY ACCOMPANYING      }
{   VCL OR FMX CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.       }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM EASY-IP AS.                                  }
{                                                                    }
{********************************************************************}

unit PTLib.Common.Log.Transmitter;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.Generics.Collections, System.SyncObjs,

  IdComponent, IdHTTP,

  PTLib.Common.Thread,
  PTLib.Common.Log,
  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Dates;

type
  TPTLibLogTransmitter = class;

  TLogTransmitThread = class(TPTLibThread)
  strict private
    FLogTransmitter: TPTLibLogTransmitter;
    FLogReceivers: TStringList;
    FDefaultPort: Integer;
    FTransmitInterval: Integer;
    FClientName: String;
  private
    procedure OnHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function GetLogReveiverURL(const LogReceiverIndex: Integer): String;
  public
    constructor Create(const LogTransmitter: TPTLibLogTransmitter; const LogReceivers: TStrings;
      const DefaultPort: Integer; const TransmitInterval: Integer; const ClientName: String);
    destructor Destroy; override;

    procedure Execute; override;
  end;

  TPTLibLogTransmitter = class(TBasePTLibLogReceiver)
  strict private
    FLogReceivers: TStrings;
    FDefaultPort: Integer;
    FTransmitInterval: Integer;
    FLogTransmitThread: TLogTransmitThread;
    FLogEntryBufferSize: Integer;
    FClientName: String;
    FLogQueue: IList<ILogEntry>;
    FLogCS: TCriticalSection;
  private
    procedure GetLogEntries(out LogEntries: IList<ILogEntry>);

    procedure SetLogReceivers(const Value: TStrings);
    procedure SetPort(const Value: Integer);
    procedure SetTransmitInterval(const Value: Integer);
    procedure StartStopThread(const DoStart: Boolean);
    procedure ResetThread;
    procedure SetClientName(const Value: String);
  protected
    procedure DoOnLog(const LogEntry: ILogEntry); override;
    procedure DoActiveChanged(const Active: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure Loaded; override;
  published
    property LogReceivers: TStrings read FLogReceivers write SetLogReceivers;
    property Port: Integer read FDefaultPort write SetPort;
    property TransmitInterval: Integer read FTransmitInterval write SetTransmitInterval;
    property ClientName: String read FClientName write SetClientName;
  end;

implementation

{ TPTLogTransmitter }

procedure TPTLibLogTransmitter.AfterConstruction;
begin
  inherited;
end;

constructor TPTLibLogTransmitter.Create(AOwner: TComponent);
begin
  inherited;

  FLogQueue := TList<ILogEntry>.Create;
  FLogCS := TCriticalSection.Create;

  FLogReceivers := TStringList.Create;
  FLogReceivers.Add('127.0.0.1');
  FDefaultPort := 57361;
  FTransmitInterval := 200;
  FLogEntryBufferSize := 1000;
end;

destructor TPTLibLogTransmitter.Destroy;
begin
  Active := False;

  if FLogTransmitThread <> nil then
  begin
    FLogTransmitThread.Terminate;
    FLogTransmitThread := nil;
  end;

  FreeAndNil(FLogReceivers);
  FreeAndNil(FLogCS);

  inherited;
end;

procedure TPTLibLogTransmitter.DoActiveChanged(const Active: Boolean);
begin
  inherited;

  if (not (csLoading in ComponentState)) and
     (not (csDesigning in ComponentState)) then
  begin
    StartStopThread(Active);
  end;
end;

procedure TPTLibLogTransmitter.DoOnLog(const LogEntry: ILogEntry);
begin
  inherited;

  if not Active then
  begin
    Active := True;
  end;

  FLogCS.Enter;
  try
    FLogQueue.Add(LogEntry);
  finally
    FLogCS.Leave;
  end;
end;

procedure TPTLibLogTransmitter.GetLogEntries(out LogEntries: IList<ILogEntry>);
var
  i: Integer;
begin
  LogEntries := TList<ILogEntry>.Create;

  FLogCS.Enter;
  try
    for i := 0 to pred(FLogQueue.Count) do
    begin
      LogEntries.Add(FLogQueue[i]);
    end;

    FLogQueue.Clear;
  finally
    FLogCS.Leave;
  end;
end;

procedure TPTLibLogTransmitter.Loaded;
begin
  inherited;

  StartStopThread(Active);
end;

procedure TPTLibLogTransmitter.ResetThread;
begin
  if not (csLoading in ComponentState) then
    Active := False;
end;

procedure TPTLibLogTransmitter.StartStopThread(const DoStart: Boolean);
begin
  if DoStart then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Assigned(LogProvider) then
      begin
        FLogTransmitThread := TLogTransmitThread.Create(
          Self,
          FLogReceivers,
          FDefaultPort,
          FTransmitInterval,
          FClientName);
      end;
    end;
  end
  else
  begin
    if Assigned(FLogTransmitThread) then
    begin
      FLogTransmitThread.Terminate;
      FLogTransmitThread.WaitFor;
      FreeAndNil(FLogTransmitThread);
    end;
  end;
end;

procedure TPTLibLogTransmitter.SetLogReceivers(const Value: TStrings);
begin
  FLogReceivers.Assign(Value);

  ResetThread
end;

procedure TPTLibLogTransmitter.SetPort(const Value: Integer);
begin
  if FDefaultPort <> Value then
  begin
    if FDefaultPort > 65535 then
      FDefaultPort := 65535
    else
    if FDefaultPort < 1 then
      FDefaultPort := 1;

    FDefaultPort := Value;

    ResetThread;
  end;
end;

procedure TPTLibLogTransmitter.SetTransmitInterval(const Value: Integer);
begin
  if FTransmitInterval <> Value then
  begin
    if FTransmitInterval < 0 then
      FTransmitInterval := 0;

    if FTransmitInterval = Value then
      ResetThread;
  end;
end;

procedure TPTLibLogTransmitter.SetClientName(const Value: String);
begin
  if FClientName <> Value then
  begin
    FClientName := Value;
  end;
end;

{ TLogTransmitThread }

constructor TLogTransmitThread.Create(const LogTransmitter: TPTLibLogTransmitter; const LogReceivers: TStrings;
  const DefaultPort, TransmitInterval: Integer; const ClientName: String);
begin
  inherited Create(False);

  FreeOnTerminate := False;

  FLogTransmitter := LogTransmitter;
  FLogReceivers := TStringList.Create;
  FLogReceivers.Assign(LogReceivers);
  FDefaultPort := DefaultPort;
  FTransmitInterval := TransmitInterval;
  FClientName := ClientName;
end;

destructor TLogTransmitThread.Destroy;
begin
  FreeAndNil(FLogReceivers);

  inherited;
end;

procedure TLogTransmitThread.OnHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Terminated then
    Abort;
end;

function TLogTransmitThread.GetLogReveiverURL(const LogReceiverIndex: Integer): String;
var
  p: Integer;
  Host: String;
  Port: Integer;
begin
  p := pos(':', FLogReceivers[LogReceiverIndex]);

  if p = 0 then
  begin
    Host := Trim(FLogReceivers[LogReceiverIndex]);
    Port := FDefaultPort;
  end else
  begin
    Host := Trim(copy(FLogReceivers[LogReceiverIndex], 1, p - 1));
    Port := StrToIntDef(Trim(copy(FLogReceivers[LogReceiverIndex], p + 1, MaxInt)), FDefaultPort);
  end;

  Result := 'http://' + Host + ':' + Port.ToString;
end;

procedure TLogTransmitThread.Execute;
var
  WaitTicks: Cardinal;
  LogEntries: IList<ILogEntry>;
  i, n: Integer;
  LastLogEntryTimeStamps: System.Generics.Collections.TList<TDateTime>;
  HTTPClient: TIdHTTP;
  CommaStrings, LogStrings: TStringList;
  StringStream: TStringStream;
  NoConnection: Boolean;
begin
  try
    LogStrings := TStringList.Create;
    CommaStrings := TStringList.Create;
    LastLogEntryTimeStamps := System.Generics.Collections.TList<TDateTime>.Create;
    try
      for i := 0 to pred(FLogReceivers.Count) do
      begin
        LastLogEntryTimeStamps.Add(0);
      end;

      while not Terminated do
      begin
        NoConnection := True;

        for n := 0 to pred(FLogReceivers.Count) do
        begin
          LogStrings.Clear;

          FLogTransmitter.GetLogEntries(LogEntries);

          // Get the log entries
          if LogEntries.Count > 0 then
          begin
            LogStrings.Add(FClientName);

            // Create the HTTP client
            HTTPClient := TIdHTTP.Create(nil);
            try
              HTTPClient.ConnectTimeout := 500;
              HTTPClient.OnWork := OnHTTPWork;

              for i := 0 to pred(LogEntries.Count) do
              begin
                if Terminated then
                  Break;

                CommaStrings.Clear;
                CommaStrings.Add(DateTimeToCommonDateTimeStr(LogEntries[i].TimeStampUTC));
                CommaStrings.Add(DateTimeToCommonDateTimeStr(LogEntries[i].GeneratedUTC));

                if LogEntries[i].LogType = '' then
                begin
                  CommaStrings.Add(FClientName);
                end
                else
                begin
                  CommaStrings.Add(LogEntries[i].LogType);
                end;

                CommaStrings.Add(Integer(LogEntries[i].Severity).ToString);
                CommaStrings.Add(LogEntries[i].LogText);

                LogStrings.Add(CommaStrings.CommaText);
              end;

              try
                HTTPClient.Request.Host := GetLogReveiverURL(n);

                StringStream := TStringStream.Create(LogStrings.Text);
                try
                  HTTPClient.Post(
                    GetLogReveiverURL(n),
                    StringStream);
                finally
                  FreeAndNil(StringStream);
                end;

                NoConnection := False;

                LastLogEntryTimeStamps[n] := LogEntries[pred(LogEntries.Count)].TimeStampUTC;
              except
                on e: Exception do
                begin
                  // Nothing for now
                  sleep(1);
                end;
              end;
            finally
              FreeAndNil(HTTPClient);
            end;
          end;
        end;

        // Wait
        if NoConnection then
        begin
          WaitTicks := GetTickCount + 1000; // 10 seconds
        end
        else
        begin
          WaitTicks := GetTickCount + Cardinal(FTransmitInterval);
        end;

        // Wait a little before continuing
        while (GetTickCount < WaitTicks) and (not Terminated) do
        begin
          sleep(5);
        end;
      end;
    finally
      FreeAndNil(CommaStrings);
      FreeAndNil(LogStrings);
      FreeAndNil(LastLogEntryTimeStamps);
    end;
  except
    on e: Exception do
    begin
      // Ignore the exception
    end;
  end;
end;

end.
