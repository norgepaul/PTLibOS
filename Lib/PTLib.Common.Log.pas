{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Log                                         }
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

unit PTLib.Common.Log;

interface

{.$DEFINE BREAK_LISTS}

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs,
  System.Variants,

  {$IFDEF LINUX64}
  Posix.Unistd,
  {$ENDIF}

  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Strings,
  PTLib.Common.Dates,
  PTLib.Common.Types,
  PTLib.Common.Timers,
  PTLib.Common.KeyValueStore.Memory;

(*const
  LogSeverityNoLog = 0;
  LogSeverityError = 1;
  LogSeverityWarning = 2;
  LogSeverityInfo = 3;
  LogSeverityDebug = 4;
  LogSeverityDebug2 = 5;
  LogSeverityDebug3 = 6;
  *)
type
  TBasePTLibLogReceiver = class;

  TLogEntry = class(TInterfacedObject,
                    ILogEntry)
  strict private
    FLogType: String;
    FLogText: String;
    FSeverity: TLogSeverity;
    FTimeStampUTC: TDateTime;
    FGeneratedUTC: TDateTime;
    FParameters: IParameters;
    FSender: TObject;
    FSenderName: String;
  private
    function GetLogText: String;
    function GetLogType: String;
    function GetSeverity: TLogSeverity;
    function GetTimeStampUTC: TDateTime;
    procedure SetLogText(const Value: String);
    procedure SetLogType(const Value: String);
    procedure SetSeverity(const Value: TLogSeverity);
    procedure SetTimeStampUTC(const Value: TDateTime);
    function GetGeneratedUTC: TDateTime;
    procedure SetGeneratedUTC(const Value: TDateTime);
    function GetParameters: IParameters;
    function HasParameters: Boolean;
    function GetSender: TObject;
    function GetSenderName: String;
    procedure SetSender(const Value: TObject);
    procedure SetSenderName(const Value: String);
    function GetSenderDescription: String;
    function LogTextWithParams(const MaxParamLength: Integer): String;
    function FullLogText: String;
  public
    property LogType: String read GetLogType write SetLogType;
    property LogText: String read GetLogText write SetLogText;
    property Severity: TLogSeverity read GetSeverity write SetSeverity;
    property TimeStampUTC: TDateTime read GetTimeStampUTC write SetTimeStampUTC;
    property GeneratedUTC: TDateTime read GetGeneratedUTC write SetGeneratedUTC;
    property Parameters: IParameters read GetParameters;
    property Sender: TObject read GetSender write SetSender;
    property SenderName: String read GetSenderName write SetSenderName;
  end;

  TOnLog = procedure(Sender: TObject; const LogEntry: ILogEntry) of object;
  TOnLogEx = procedure(Sender: TObject; LogEntry: ILogEntry; const Index, Count: Integer) of object;
  TOnLogProc = reference to procedure(LogEntry: ILogEntry);

  TPTLibLog = class(TComponent,
                    ILog)
  strict private
    FOnLog: TOnLog;
    FOnLogEx: TOnLogEx;

    {$IFDEF BREAK_LISTS}
    FLogQueue: IList<ILogEntry>;
    FLogHistory: IList<ILogEntry>;
    {$ELSE}
    FLogQueue: System.Generics.Collections.TList<ILogEntry>;
    FLogHistory: System.Generics.Collections.TList<ILogEntry>;
    {$ENDIF}

    FLogTimer: IPTLibGlobalTimer;
    FLogReceivers: TObjectList<TBasePTLibLogReceiver>;
    FLogCS: TCriticalSection;
    FProcessLogInterval: Integer;
    FActive: Boolean;
    FLogHistoryCount: Integer;
    FMaxLength: Integer;
  private
    procedure OnLogTimer(Sender: TObject);
    procedure SetProcessLogInterval(const Value: Integer);
    procedure RegisterLogReceiver(const LogReceiver: TBasePTLibLogReceiver);
    procedure UnRegisterLogReceiver(const LogReceiver: TBasePTLibLogReceiver);
    function FormatLogLine(const LogEntry: ILogEntry; const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]): String;
    function GetLogHistoryCount: Integer;
    procedure SetLogHistoryCount(const Value: Integer);
    class function GetDateTimeString(const TimestampUTC: TDateTime;
      const DateDisplayType: TDateDisplayType): String; static;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoProcessLogQueue; virtual;
    procedure DoOnLog(const LogEntry: ILogEntry; const Index, Count: Integer); virtual;
    procedure SetActive(const Value: Boolean); virtual;
  public
    class function LogEntryToText(LogEntry: ILogEntry; const MaxParamLength: Integer = 500; const DateDisplayType: TDateDisplayType = TDateDisplayType.ddtISO8601): String;
    class function LogEntryFromText(const LogLine: String): ILogEntry; static;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Log(const LogText: String; const LogType: String = ''; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Args: Array of const; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Args: Array of const; const LogType: String = ''; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const LogType: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime; const GeneratedUTC: TDateTime; const Parameters: IParameters); overload;
    Procedure Log(const LogEntry: ILogEntry); overload;
    procedure LoadFromStrings(const Values: TStrings);
    function GetLogHistoryItemCount: Integer;
    function GetLogHistoryItem(const Index: Integer): ILogEntry;
    function GetLogHistory(const TailEntries: Integer = 0): IList<ILogEntry>; overload;
    function GetLogHistory(const TimeStamp: TDateTime): IList<ILogEntry>; overload;
    procedure ClearHistory;
    procedure SaveToStream(const AStream: TStream; const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]);
    procedure SaveToFile(const Filename: String; const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]);
    function SaveToString(const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]): String;
  published
    property OnLog: TOnLog read FOnLog write FOnLog;
    property OnLogEx: TOnLogEx read FOnLogEx write FOnLogEx;

    property ProcessLogInterval: Integer read FProcessLogInterval write SetProcessLogInterval;
    property Active: Boolean read FActive write SetActive;
    property LogHistoryCount: Integer read GetLogHistoryCount write SetLogHistoryCount;
    property MaxLength: Integer read FMaxLength write FMaxLength;
  end;

  TTimeStampUTCFormat = (
    lfNone,
    lfDate,
    lfTime,
    lfDateTime,
    lfTimeMilliseconds,
    lfDateTimeMilliseconds,
    lsISO
  );
  
  TOnException = procedure(Sender: TObject; const e: Exception) of object;
  TOnGetSeverityText = procedure(Sender: TObject; const Severity: TLogSeverity; var SeverityText: String) of object;
  TOnHandleLogMessage = procedure(Sender: TObject; const LogText: String; const LogType: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime) of object;

  TBasePTLibLogComponent = class(TBasePTLibComponent)
  strict private
    FLogProvider: TPTLibLog;
    FOnHandleLogMessage: TOnHandleLogMessage;

    procedure SetLogProvider(const Value: TPTLibLog);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoHandleLogMessage(const LogText: String; const LogType: String;
      const Severity: TLogSeverity; const TimeStampUTC: TDateTime); virtual;

    procedure DoGetDefaultLogType(out LogType: String); virtual;
    procedure DoOnLogProviderChanged(const OldLogProvider, NewLogProvider: TPTLibLog); virtual;

    procedure DoLog(const LogText: String; const LogType: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime = 0); overload;
    procedure DoLog(const LogText: String; const Args: Array of const; const LogType: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime = 0); overload;
    procedure DoLog(const LogText: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime = 0); overload;
    procedure DoLog(const LogText: String; const Args: Array of const; const Severity: TLogSeverity; const TimeStampUTC: TDateTime = 0); overload;

    function GetDefaultLogType: String;
  published
    property LogProvider: TPTLibLog read FLogProvider write SetLogProvider;

    property OnHandleLogMessage: TOnHandleLogMessage read FOnHandleLogMessage write FOnHandleLogMessage;
  end;

  TBasePTLibLoggerComponent = class(TBasePTLibLogComponent)
  public
    procedure Log(const LogText: String; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Args: Array of const; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
  end;

  TBasePTLibLogReceiver = class(TBasePTLibLogComponent)
  strict private
    FOnException: TOnException;

    FLogProvider: TPTLibLog;
    FActive: Boolean;
    FTimeStampUTCFormat: TTimeStampUTCFormat;
    FSeverity: TLogSeverity;
    //FMinSeverity: TLogSeverity; // Deprecated
    FLocaliseTimeStamps: Boolean;
    FOnLog: TOnLog;
  private
    procedure Log(const LogEntry: ILogEntry);
    procedure SetActive(const Value: Boolean);
    procedure SetSeverity(const Value: TLogSeverity);
  protected
    procedure DoOnLogProviderChanged(const OldLogProvider, NewLogProvider: TPTLibLog); override;

    procedure DoOnLog(const LogEntry: ILogEntry); virtual;
    procedure DoOnException(const e: Exception); virtual;
    procedure DoActiveChanged(const Active: Boolean); virtual;
//    procedure DoGetLogSeverityText(const Severity: Integer; out SeverityText: String); virtual;

    function FormatTimeStamp(const TimeStamp: TDateTime): String;
    function LocaliseTimeStamp(const TimeStamp: TDateTime): TDateTime;

    property TimeStampUTCFormat: TTimeStampUTCFormat read FTimeStampUTCFormat write FTimeStampUTCFormat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetLogSeverityText(const Severity: TLogSeverity): String;
  published
    property OnException: TOnException read FOnException write FOnException;
    property OnLog: TOnLog read FOnLog write FOnLog;

    property Active: Boolean read FActive write SetActive;
    //property MaxSeverity: TLogSeverity read FSeverity write SetSeverity;
    property LocaliseTimeStamps: Boolean read FLocaliseTimeStamps write FLocaliseTimeStamps;
    property Severity: TLogSeverity read FSeverity write SetSeverity;
  end;

  TBasePTLibTextLogReceiver = class(TBasePTLibLogReceiver)
  strict private
    FOnGetSeverityText: TOnGetSeverityText;
  protected
    procedure DoOnGetSeverityText(const Severity: TLogSeverity; var SeverityText: String); virtual;
  published
    property OnGetSeverityText: TOnGetSeverityText read FOnGetSeverityText write FOnGetSeverityText;
  end;

  TGLobalLog = class
  strict private
    class var FOnLog: TOnLogProc;
    class var FLogSeverity: TLogSeverity;
    class var FCache: IKeyValueStoreMemory;
  private
    class function GetCache: IKeyValueStoreMemory; static;
  public
    class constructor Create;

    class procedure Log(const Sender: TObject; const SenderName, LogText: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime; const GeneratedUTC: TDateTime; const Parameters: IParameters); overload;

    class procedure Log(const Sender: TObject; const LogText: String; const Args: Array of const; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    class procedure Log(const Sender: TObject; const LogText: String; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    class procedure Log(const SenderName, LogText: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    class procedure Log(const SenderName, LogText: String; const Args: Array of const; const Severity: TLogSeverity; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;

    class procedure Log(const Sender: TObject; const LogText: String; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    class procedure Log(const Sender: TObject; const LogText: String; const Args: Array of const; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    class procedure Log(const SenderName: String; const LogText: String; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    class procedure Log(const SenderName: String; const LogText: String; const Args: Array of const; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;

    class function Logging(const LogSeverity: TLogSeverity): Boolean;

    class property OnLog: TOnLogProc read FOnLog write FOnLog;
    class property LogSeverity: TLogSeverity read FLogSeverity write FLogSeverity;
    class property Cache: IKeyValueStoreMemory read GetCache;
  end;

function LogSeverityToString(const LogSeverity: TLogSeverity): String;
function StringToLogSeverity(const Value: String): TLogSeverity;

implementation

uses
  System.DateUtils;

resourcestring
  StrSetLogTypeFor = 'Set Log Type for ';

function LogSeverityToString(const LogSeverity: TLogSeverity): String;
begin
  Result := LogSeverityStrings[LogSeverity];
end;

function StringToLogSeverity(const Value: String): TLogSeverity;
var
  i: TLogSeverity;
begin
  for i := Low(LogSeverityStrings) to High(LogSeverityStrings) do
  begin
    if AnsiSameText(Value, LogSeverityStrings[i]) then
    begin
      Exit(i);
    end;
  end;

  Result := LogSeverityInfo;
end;

{ TPTLibLog }

procedure TPTLibLog.ClearHistory;
begin
  FLogCS.Enter;
  try
    FLogHistory.Clear;
  finally
    FLogCS.Leave;
  end;
end;

constructor TPTLibLog.Create(AOwner: TComponent);
begin
  inherited;

  FProcessLogInterval := 50;
  FActive := True;
  FMaxLength := 2000;

  FLogTimer := TPTLibGobalTimerController.NewTimer(Self, OnLogTimer, FProcessLogInterval);

  FLogCS := TCriticalSection.Create;

  {$IFDEF BREAK_LISTS}
  FLogQueue := PTLib.Common.Classes.TList<ILogEntry>.Create;
  FLogHistory := PTLib.Common.Classes.TList<ILogEntry>.Create;
  {$ELSE}
  FLogQueue := System.Generics.Collections.TList<ILogEntry>.Create;
  FLogHistory := System.Generics.Collections.TList<ILogEntry>.Create;
  {$ENDIF}

  FLogReceivers := TObjectList<TBasePTLibLogReceiver>.Create(False);
end;

destructor TPTLibLog.Destroy;
begin
  FLogTimer.Enabled := False;

  FreeAndNil(FLogReceivers);
  FreeAndNil(FLogCS);

  {$IFNDEF BREAK_LISTS}
  FreeAndNil(FLogQueue);
  FreeAndNil(FLogHistory);
  {$ENDIF}

  inherited;
end;

procedure TPTLibLog.DoOnLog(const LogEntry: ILogEntry; const Index, Count: Integer);
begin
  if Assigned(FOnLog) then
  begin
    FOnLog(Self, LogEntry);
  end;

  if Assigned(FOnLogEx) then
  begin
    FOnLogEx(Self, LogEntry, Index, Count);
  end;
end;

procedure TPTLibLog.DoProcessLogQueue;
var
  i, n: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    FLogCS.Enter;
    try
      for i := 0 to pred(FLogQueue.Count) do
      begin
        // Add the log history entry
        FLogHistory.Add(FLogQueue[i]);

        // Remove old log history items
        while (FLogHistory.Count > FLogHistoryCount) and
              (FLogHistory.Count > 0) do
        begin
          FLogHistory.Delete(0);
        end;

        DoOnLog(FLogQueue[i], i, FLogQueue.Count);

        // Send to the receivers
        for n := 0 to pred(FLogReceivers.Count) do
        begin
          FLogReceivers[n].Log(FLogQueue[i]);
        end;
      end;
    finally
      FLogQueue.Clear;

      FLogCS.Leave;
    end;
  end;
end;

function TPTLibLog.GetLogHistory(const TimeStamp: TDateTime): IList<ILogEntry>;
var
  i: Integer;
begin
  FLogCS.Enter;
  try
    Result := TList<ILogEntry>.Create;

    // Add the entries
    for i := pred(FLogHistory.Count) downto 0 do
    begin
      if (TimeStamp <> 0) and
         (FLogHistory[i].TimeStampUTC <= TimeStamp) then
      begin
        Break;
      end;

      Result.Insert(0, FLogHistory[i]);
    end;
  finally
    FLogCS.Leave;
  end;
end;

function TPTLibLog.GetLogHistoryCount: Integer;
begin
  Result := FLogHistoryCount;
end;

function TPTLibLog.GetLogHistoryItem(const Index: Integer): ILogEntry;
begin
  FLogCS.Enter;
  try
    Result := FLogHistory[Index];
  finally
    FLogCS.Leave;
  end;
end;

function TPTLibLog.GetLogHistoryItemCount: Integer;
begin
  FLogCS.Enter;
  try
    Result := FLogHistory.Count;
  finally
    FLogCS.Leave;
  end;
end;

procedure TPTLibLog.Log(const LogText: String; const Parameters: IParameters; const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  Log(LogText, '', Severity, TimeStampUTC);
end;

procedure TPTLibLog.Log(const LogText: String; const Args: array of const; const Parameters: IParameters; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime);
begin
  Log(format(LogText, Args), '', Severity, TimeStampUTC, 0, Parameters);
end;

procedure TPTLibLog.Log(const LogText: String; const LogType: String; const Severity: TLogSeverity; const TimeStampUTC,
  GeneratedUTC: TDateTime; const Parameters: IParameters);
var
  LogEntry: ILogEntry;
begin
  if FActive then
  begin
    LogEntry := TLogEntry.Create;
    LogEntry.LogText := CTRLCharsToOrdVals(TrimText(LogText, FMaxLength), False);
    LogEntry.LogType := LogType;
    LogEntry.Severity := Severity;

    if Parameters <> nil then
    begin
      LogEntry.Parameters.Assign(Parameters);
    end;

    if TimeStampUTC = 0 then
      LogEntry.TimeStampUTC := nowUTC
    else
      LogEntry.TimeStampUTC := TimeStampUTC;

    if GeneratedUTC = 0 then
      LogEntry.GeneratedUTC := LogEntry.TimeStampUTC
    else
      LogEntry.GeneratedUTC := GeneratedUTC;

    Log(LogEntry);
  end;
end;

function TPTLibLog.GetLogHistory(const TailEntries: Integer): IList<ILogEntry>;
var
  i, FirstEntryIndex: Integer;
begin
  FLogCS.Enter;
  try
    // Find the first log entry
    if TailEntries > FLogHistory.Count then
    begin
      FirstEntryIndex := 0;
    end
    else
    begin
      FirstEntryIndex := FLogHistory.Count - TailEntries;
    end;

    Result := TList<ILogEntry>.Create;

    // Add the entries
    for i := FirstEntryIndex to pred(FLogHistory.Count) do
    begin
      Result.Add(FLogHistory[i]);
    end;
  finally
    FLogCS.Leave;
  end;
end;

procedure TPTLibLog.Log(const LogText: String; const LogType: String;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime; const GeneratedUTC: TDateTime);
begin
  Log(
    LogText,
    LogType,
    Severity,
    TimeStampUTC,
    GeneratedUTC,
    nil);
end;

procedure TPTLibLog.Log(const LogText: String; const Args: array of const;
  const LogType: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime; const GeneratedUTC: TDateTime);
begin
  Log(
    format(LogText, Args),
    LogType,
    Severity,
    TimeStampUTC,
    GeneratedUTC,
    nil);
end;

procedure TPTLibLog.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin

  end;
end;

procedure TPTLibLog.OnLogTimer(Sender: TObject);
begin
  DoProcessLogQueue;
end;

procedure TPTLibLog.RegisterLogReceiver(const LogReceiver: TBasePTLibLogReceiver);
begin
  FLogCS.Enter;
  try
    if FLogReceivers.IndexOf(LogReceiver) = -1 then
    begin
      FLogReceivers.Add(LogReceiver);

      // Make sure that the components are informed when
      // one or the other is destroyed
      FreeNotification(LogReceiver);
      LogReceiver.FreeNotification(Self);
    end;
  finally
    FLogCS.Leave;
  end;
end;

procedure TPTLibLog.SaveToFile(const Filename: String; const UseTabs: Boolean;
  const LogFormatOptions: TLogFormatOptions);
var
  FileStream: TFileStream;
begin
  ForceDirectories(ExtractFileDir(Filename));
  DeleteFile(Filename);

  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(FileStream, UseTabs, LogFormatOptions);
  finally
    FreeAndNil(FileStream);
  end;
end;

function TPTLibLog.FormatLogLine(const LogEntry: ILogEntry; const UseTabs: Boolean; const LogFormatOptions: TLogFormatOptions): String;
begin
  Result := '';

  if lfCreatedTimestamp in LogFormatOptions then
  begin
    if lfIncludeMilliSeconds in LogFormatOptions then
    begin
      AddToken(Result, DateTimeToStringWithMS(LogEntry.TimeStampUTC), #09);
    end
    else
    begin
      AddToken(Result, DateTimeToStr(LogEntry.TimeStampUTC), #09);
    end;
  end;

  if lfGeneratedTimestamp in LogFormatOptions then
  begin
    if lfIncludeMilliSeconds in LogFormatOptions then
    begin
      AddToken(Result, DateTimeToStringWithMS(LogEntry.GeneratedUTC), #09);
    end
    else
    begin
      AddToken(Result, DateTimeToStr(LogEntry.GeneratedUTC), #09);
    end;
  end;

  if lfLogType in LogFormatOptions then
  begin
    AddToken(Result, LogSeverityToString(LogEntry.Severity), #09);
  end;

  if lfLogText in LogFormatOptions then
  begin
    AddToken(Result, LogEntry.LogText, #09);
  end;
end;

procedure TPTLibLog.SaveToStream(const AStream: TStream; const UseTabs: Boolean;
  const LogFormatOptions: TLogFormatOptions);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(SaveToString(UseTabs, LogFormatOptions));
  try
    StringStream.Position := 0;
    AStream.CopyFrom(StringStream, StringStream.Size);
    AStream.Position := 0;
  finally
    FreeAndNil(StringStream);
  end;
end;

function TPTLibLog.SaveToString(const UseTabs: Boolean;
  const LogFormatOptions: TLogFormatOptions): String;
var
  i: Integer;
begin
  FLogCS.Enter;
  try
    Result := '';

    for i := 0 to pred(FLogHistory.Count) do
    begin
      AddLine(Result, FormatLogLine(FLogHistory[i], UseTabs, LogFormatOptions));
    end;
  finally
    FLogCS.Leave;
  end;
end;

procedure TPTLibLog.SetActive(const Value: Boolean);
begin
  FActive := Value;

  FLogTimer.Enabled := False;
  FLogTimer.Enabled :=
    (not (csDesigning in ComponentState)) and
    (FActive);
end;

procedure TPTLibLog.SetLogHistoryCount(const Value: Integer);
begin
  FLogHistoryCount := Value;
end;

procedure TPTLibLog.SetProcessLogInterval(const Value: Integer);
begin
  FProcessLogInterval := Value;

  FLogTimer.Interval := FProcessLogInterval;

  SetActive(FActive);
end;

procedure TPTLibLog.UnRegisterLogReceiver(
  const LogReceiver: TBasePTLibLogReceiver);
var
  Idx: Integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    FLogCS.Enter;
    try
      Idx := FLogReceivers.IndexOf(LogReceiver);

      if Idx <> -1 then
        FLogReceivers.Delete(Idx);
    finally
      FLogCS.Leave;
    end;
  end;
end;

procedure TPTLibLog.Log(const LogEntry: ILogEntry);
begin
  if (Self <> nil) and
     (not (csDestroying in ComponentState)) then
  begin
    FLogCS.Enter;
    try
      FLogQueue.Add(LogEntry);
    finally
      FLogCS.Leave;
    end;

    if ProcessLogInterval = 0 then
    begin
      OnLogTimer(Self);
    end;
  end;
end;

class function TPTLibLog.GetDateTimeString(const TimestampUTC: TDateTime; const DateDisplayType: TDateDisplayType): String;
begin
  case DateDisplayType of
    ddtLocal: Result := DateTimeToStringWithMS(TimeStampUTC);
    ddtUTC: Result := DateTimeToStringWithMS(UTCToLocal(TimeStampUTC));
  else
    Result := DateToISO8601(UTCToLocal(TimestampUTC), False);
  end;
end;

class function TPTLibLog.LogEntryToText(LogEntry: ILogEntry;
  const MaxParamLength: Integer; const DateDisplayType: TDateDisplayType): String;
var
  Params: String;
begin
  Result := format('%s%s[%s]%s[%s] %s', [
    GetDateTimeString(LogEntry.TimeStampUTC, DateDisplayType),
    #09,
    LogSeverityToString(LogEntry.Severity).PadRight(7, ' '),
    #09,
    LogEntry.GetSenderDescription,
    LogEntry.LogTextWithParams(MaxParamLength),
    Params]);

  LogEntryFromText(Result);
end;

class function TPTLibLog.LogEntryFromText(const LogLine: String): ILogEntry;

  function TrimBrackets(const Value: String): String;
  begin
    Result := Trim(copy(Value, 2, length(Value) - 2));
  end;

var
  Text: String;
  TimestampStr, SeverityStr, SenderStr: String;
begin
  Text := LogLine;

  Result := TLogEntry.Create;

  TimestampStr := NextBlock(Text, #09);
  SeverityStr := TrimBrackets(NextBlock(Text, #09));
  SenderStr := TrimBrackets(NextBlock(Text, ' '));

  try
    Result.TimeStampUTC := ISO8601ToDate(TimestampStr, True);
  except
    Result.TimeStampUTC := 0;

    Text := TimestampStr + ' - ' + Text;
  end;

  Result.Severity := StringToLogSeverity(SeverityStr);
  Result.SenderName := SenderStr;
  Result.LogText := Text;
end;

procedure TPTLibLog.LoadFromStrings(const Values: TStrings);
var
  i: Integer;
begin
  for i := 0 to pred(Values.Count) do
  begin
    Log(LogEntryFromText(Values[i]));
  end;
end;

{ TBasePTLibLogReceiver }

constructor TBasePTLibLogReceiver.Create(AOwner: TComponent);
begin
  inherited;

  FActive := True;
  FTimeStampUTCFormat := lfDateTimeMilliseconds;
  FLocaliseTimeStamps := True;
  FSeverity := High(TLogSeverity);
end;

destructor TBasePTLibLogReceiver.Destroy;
begin
  if FLogProvider <> nil then
  try
    FLogProvider.UnRegisterLogReceiver(Self);
  except
    // Ignore!
  end;

  inherited;
end;

procedure TBasePTLibLogReceiver.DoOnException(const e: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Self, e);
end;

procedure TBasePTLibLogReceiver.DoOnLog(const LogEntry: ILogEntry);
begin
  if Assigned(FOnLog) then
  begin
    FOnLog(Self, LogEntry);
  end;
end;

function TBasePTLibLogReceiver.FormatTimeStamp(const TimeStamp: TDateTime): String;
var
  MilliSecStr: String;
begin
  case FTimeStampUTCFormat of
    lfDate: Result := DateToStr(TimeStamp);
    lfTime, 
    lfTimeMilliseconds: Result := TimetoStr(TimeStamp);
    lfDateTime,
    lfDateTimeMilliseconds: Result := DateTimeToStr(TimeStamp);
  else
    Result := '';
  end;

  if FTimeStampUTCFormat in [lfTimeMilliseconds, lfDateTimeMilliseconds] then
  begin
    MilliSecStr := IntToStr(MilliSecondOf(TimeStamp));
    
    while length(MilliSecStr) < 3 do
      MilliSecStr := '0' + MilliSecStr;

    Result := Result + '.' + MilliSecStr;
  end;
end;

function TBasePTLibLogReceiver.GetLogSeverityText(
  const Severity: TLogSeverity): String;
begin
  case Severity of
    LogSeverityNone: Result := 'No Log';
    LogSeverityError: Result := 'Error';
    LogSeverityWarning: Result := 'Warning';
    LogSeverityInfo: Result := 'Info';
    LogSeverityDebug: Result := 'Debug';
    LogSeverityDebug2: Result := 'Debug2';
    LogSeverityDebug3: Result := 'Debug3';
  else
    Result := 'Unknown';
  end;
end;

function TBasePTLibLogReceiver.LocaliseTimeStamp(const TimeStamp: TDateTime): TDateTime;
begin
  if FLocaliseTimeStamps then
  begin
    Result := UTCToLocal(TimeStamp);
  end
  else
  begin
    Result := TimeStamp;
  end;
end;

procedure TBasePTLibLogReceiver.Log(const LogEntry: ILogEntry);
begin
  if (FActive) and
     ((FSeverity <> LogSeverityNone) and
      (LogEntry.Severity <= FSeverity)) then
  begin
    DoOnLog(LogEntry);
  end;
end;

procedure TBasePTLibLogReceiver.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;

    DoActiveChanged(FActive);
  end;
end;

procedure TBasePTLibLogReceiver.SetSeverity(const Value: TLogSeverity);
begin
  if FSeverity <> Value then
  begin
    FSeverity := Value;
  end;
end;

procedure TBasePTLibLogReceiver.DoActiveChanged(const Active: Boolean);
begin
  // Override
end;

(*procedure TBasePTLibLogReceiver.DoGetLogSeverityText(const Severity: TLogSeverity;
  out SeverityText: String);
begin

end;*)

procedure TBasePTLibLogReceiver.DoOnLogProviderChanged(const OldLogProvider, NewLogProvider: TPTLibLog);
begin
  if OldLogProvider <> nil then
    OldLogProvider.UnRegisterLogReceiver(Self);

  if NewLogProvider <> nil then
  begin
    NewLogProvider.RegisterLogReceiver(Self);
  end;
end;

{ TBasePTLibLogComponent }

procedure TBasePTLibLogComponent.DoLog(const LogText: String; const LogType: String;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  DoHandleLogMessage(
    LogText,
    LogType,
    Severity,
    TimeStampUTC);
end;

procedure TBasePTLibLogComponent.DoHandleLogMessage(const LogText: String; const LogType: String;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
var
  Msg: String;
begin
  Msg := LogText;

  if LogType <> '' then
  begin
    Msg := '[' + LogType + '] ' + Msg;
  end;

  if Assigned(FOnHandleLogMessage) then
  begin
    FOnHandleLogMessage(
      Self,
      Msg,
      LogType,
      Severity,
      TimeStampUTC);
  end;

  if Assigned(LogProvider) then
  begin
    LogProvider.Log(
      Msg,
      LogType,
      Severity,
      TimeStampUTC);
  end;
end;

procedure TBasePTLibLogComponent.DoLog(const LogText: String;
  const Args: array of const; const LogType: String; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime);
begin
  DoLog(format(LogText, Args), LogType, Severity, TimeStampUTC);
end;

procedure TBasePTLibLogComponent.DoLog(const LogText: String;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  DoLog(LogText, GetDefaultLogType, Severity, TimeStampUTC);
end;

procedure TBasePTLibLogComponent.DoLog(const LogText: String;
  const Args: array of const; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime);
begin
  DoLog(LogText, Args, GetDefaultLogType, Severity, TimeStampUTC);
end;

procedure TBasePTLibLogComponent.DoOnLogProviderChanged(
  const OldLogProvider, NewLogProvider: TPTLibLog);
begin
  // Override if required
end;

function TBasePTLibLogComponent.GetDefaultLogType: String;
begin
  DoGetDefaultLogType(Result);
end;

procedure TBasePTLibLogComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = TOperation.opRemove) and
     (AComponent = FLogProvider) then
  begin
    SetLogProvider(nil);
  end;
end;

procedure TBasePTLibLogComponent.SetLogProvider(const Value: TPTLibLog);
begin
  DoOnLogProviderChanged(
    FLogProvider,
    Value);

  FLogProvider := Value;

  // Make sure we're informed if the FLogProvider component is freed
  if Value <> nil then
    Value.FreeNotification(Self);
end;

{ ILogEntry }

function TLogEntry.FullLogText: String;
var
  ClassDescription: String;
  ClassDescriptor: IClassDescriptor;
begin
  Result := LogTextWithParams(500);

  if SenderName <> '' then
  begin
    ClassDescription := SenderName;
  end else

  if Supports(Sender, IClassDescriptor, ClassDescriptor) then
  begin
    ClassDescriptor.GetClassDescriptor(ClassDescription);
  end else

  begin
    if Sender <> nil then
    begin
      ClassDescription := Sender.ClassName
    end
    else
    begin
      ClassDescription := '';
    end;
  end;

  if ClassDescription <> '' then
  begin
    Result := Format('[%s] %s', [ClassDescription, Result]);
  end;
end;

function TLogEntry.GetGeneratedUTC: TDateTime;
begin
  Result := FGeneratedUTC;
end;

function TLogEntry.GetLogText: String;
begin
  Result := FLogText;
end;

function TLogEntry.GetLogType: String;
begin
  Result := FLogType;
end;

function TLogEntry.GetParameters: IParameters;
begin
  if FParameters = nil then
  begin
    FParameters := TParameters.Create;
  end;

  Result := FParameters;
end;

function TLogEntry.GetSender: TObject;
begin
  Result := FSender;
end;

function TLogEntry.GetSenderDescription: String;
var
  ClassDescriptor: IClassDescriptor;
begin
  if SenderName <> '' then
  begin
    Result := SenderName;
  end else
  begin
    if Sender = nil then
    begin
      Result := 'Unknown';
    end else
    if Supports(Sender, IClassDescriptor, ClassDescriptor) then
    begin
      ClassDescriptor.GetClassDescriptor(Result);
    end
    else
    begin
      Result := Sender.ClassName;
    end;
  end;
end;

function TLogEntry.GetSenderName: String;
begin
  Result := FSenderName;
end;

function TLogEntry.GetSeverity: TLogSeverity;
begin
  Result := FSeverity;
end;

function TLogEntry.GetTimeStampUTC: TDateTime;
begin
  Result := FTimeStampUTC;
end;

function TLogEntry.HasParameters: Boolean;
begin
  Result := (FParameters <> nil) and (FParameters.Count > 0);
end;

function TLogEntry.LogTextWithParams(const MaxParamLength: Integer): String;
var
  i: Integer;
  Params: String;
begin
  Result := FLogText;

  if HasParameters then
  begin
    Params := '';

    for i := 0 to pred(Parameters.Count) do
    begin
      AddToken(Params, format('%s=%s', [
        Parameters.Param(i).Name,
        TrimText(VarToStr(Parameters.Param(i).Value), MaxParamLength)]), ', ');
    end;

    Result := Result + ' - ' + Params;
  end;
end;

procedure TLogEntry.SetGeneratedUTC(const Value: TDateTime);
begin
  FGeneratedUTC := Value;
end;

procedure TLogEntry.SetLogText(const Value: String);
begin
  FLogText := Value;
end;

procedure TLogEntry.SetLogType(const Value: String);
begin
  FLogType := Value;
end;

procedure TLogEntry.SetSender(const Value: TObject);
begin
  FSender := Value;
end;

procedure TLogEntry.SetSenderName(const Value: String);
begin
  FSenderName := Value;
end;

procedure TLogEntry.SetSeverity(const Value: TLogSeverity);
begin
  FSeverity := Value;
end;

procedure TLogEntry.SetTimeStampUTC(const Value: TDateTime);
begin
  FTimeStampUTC := Value;
end;

procedure TBasePTLibLogComponent.DoGetDefaultLogType(out LogType: String);
begin
  LogType := ClassName;
end;

{ TBasePTLibTextLogReceiver }

procedure TBasePTLibTextLogReceiver.DoOnGetSeverityText(const Severity: TLogSeverity; var SeverityText: String);
begin
  if Assigned(FOnGetSeverityText) then
    FOnGetSeverityText(
      Self,
      Severity,
      SeverityText);
end;

{ TBasePTLibLoggerComponent }

procedure TBasePTLibLoggerComponent.Log(const LogText: String;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  DoLog(
    LogText,
    Severity,
    TimeStampUTC);
end;

procedure TBasePTLibLoggerComponent.Log(const LogText: String;
  const Args: array of const; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime);
begin
  Log(
    format(LogText, Args),
    Severity,
    TimeStampUTC);
end;

{ TGLobalLog }

class constructor TGLobalLog.Create;
begin
  FLogSeverity := LogSeverityDebug3;
end;

class procedure TGLobalLog.Log(const SenderName: String; const LogText: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime;
  const GeneratedUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      nil,
      SenderName,
      LogText,
      Severity,
      TimeStampUTC,
      0,
      nil
    );
  end;
end;

class procedure TGLobalLog.Log(const Sender: TObject; const LogText: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime;
  const GeneratedUTC: TDateTime);
var
  Descriptor: String;
  ClassDescriptor: IClassDescriptor;
begin
  if Logging(Severity) then
  begin
    Descriptor := '';

    if Supports(Sender, IClassDescriptor, ClassDescriptor) then
    begin
      ClassDescriptor.GetClassDescriptor(Descriptor);
    end
    else
    begin
      if Sender <> nil then
      begin
        Descriptor := Sender.ClassName;
      end;
    end;

    Log(Sender, Descriptor, LogText, Severity, TimeStampUTC, GeneratedUTC, nil);
  end;
end;

class procedure TGLobalLog.Log(const Sender: TObject; const LogText: String; const Args: array of const; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime; const GeneratedUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      Sender,
      format(LogText, Args),
      Severity,
      TimeStampUTC,
      GeneratedUTC
    );
  end;
end;

class procedure TGLobalLog.Log(const SenderName, LogText: String; const Args: array of const; const Severity: TLogSeverity;
  const TimeStampUTC, GeneratedUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      nil,
      SenderName,
      format(LogText, Args),
      Severity,
      TimeStampUTC,
      GeneratedUTC,
      nil
    );
  end;
end;

class procedure TGLobalLog.Log(const Sender: TObject; const SenderName, LogText: String; const Severity: TLogSeverity; const TimeStampUTC, GeneratedUTC: TDateTime;
  const Parameters: IParameters);
var
  LogEntry: ILogEntry;
  Timestamp, Generated: TDateTime;
begin
  if Logging(Severity) then
  begin
    if TimeStampUTC = 0 then
    begin
      TimeStamp := nowUTC;
    end
    else
    begin
      Timestamp := TimeStampUTC;
    end;

    if GeneratedUTC = 0 then
    begin
      Generated := nowUTC;
    end
    else
    begin
      Generated := GeneratedUTC;
    end;

    LogEntry := TLogEntry.Create;

    LogEntry.LogType := '';
    LogEntry.LogText := LogText;
    LogEntry.Severity := Severity;
    LogEntry.TimeStampUTC := TimeStamp;
    LogEntry.GeneratedUTC := Generated;

    if (Parameters <> nil) and
       (LogEntry.Parameters <> nil) then
    begin
      LogEntry.Parameters.Assign(Parameters);
    end;

    LogEntry.SenderName := SenderName;
    LogEntry.Sender := Sender;

    FOnLog(LogEntry);
  end;
end;

class procedure TGLobalLog.Log(const Sender: TObject; const LogText: String; const Parameters: IParameters; const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      Sender,
      '',
      LogText,
      Severity,
      TimeStampUTC,
      0,
      Parameters
    );
  end;
end;

class procedure TGLobalLog.Log(const Sender: TObject; const LogText: String; const Args: array of const; const Parameters: IParameters; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      Sender,
      '',
      format(LogText, Args),
      Severity,
      TimeStampUTC,
      0,
      Parameters
    );
  end;
end;

class function TGLobalLog.GetCache: IKeyValueStoreMemory;
begin
  if FCache = nil then
  begin
    FCache := TKeyValueStoreMemory.Create;
  end;

  Result := FCache;
end;

class procedure TGLobalLog.Log(const SenderName, LogText: String; const Args: array of const; const Parameters: IParameters; const Severity: TLogSeverity;
  const TimeStampUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      nil,
      SenderName,
      format(LogText, Args),
      Severity,
      TimeStampUTC,
      0,
      Parameters
    );
  end;
end;

class function TGLobalLog.Logging(const LogSeverity: TLogSeverity): Boolean;
begin
  Result :=
    (LogSeverity <= FLogSeverity) and
    (Assigned(FOnLog));
end;

class procedure TGLobalLog.Log(const SenderName, LogText: String; const Parameters: IParameters; const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  if Logging(Severity) then
  begin
    Log(
      nil,
      SenderName,
      LogText,
      Severity,
      TimeStampUTC,
      0,
      Parameters
    );
  end;
end;

end.
