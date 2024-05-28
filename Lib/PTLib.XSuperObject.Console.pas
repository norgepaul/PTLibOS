unit PTLib.XSuperObject.Console;

interface

uses
  XSuperObject,

  PTLib.Common.Log,
  PTLib.Common.Types,
  PTLib.Common.Dates,
  PTLib.Common.Console,
  PTLib.Common.Interfaces;

type
  TPTLibConsoleJSON = class(TPTLibConsole)
  private
    class function LogSeverityToGCPString(const LogSeverity: TLogSeverity): String; static;
  protected
    class procedure DoHandleLogEntry(const LogEntry: ILogEntry); override;
  end;

implementation

{ TPTLibConsoleJSON }

class function TPTLibConsoleJSON.LogSeverityToGCPString(const LogSeverity: TLogSeverity): String;
begin
  case LogSeverity of
    LogSeverityError: Result := 'ERROR';
    LogSeverityWarning: Result := 'WARNING';
    LogSeverityInfo: Result := 'INFO';
    LogSeverityDebug,
    LogSeverityDebug2,
    LogSeverityDebug3: Result := 'DEBUG';
  else
    begin
      Result := '';
    end;
  end;
end;

class procedure TPTLibConsoleJSON.DoHandleLogEntry(const LogEntry: ILogEntry);
var
  X: ISuperObject;
  i: Integer;
begin
  X := SO;

  X.D['time'] := LogEntry.TimeStampUTC;

  if LogEntry.GeneratedUTC <> LogEntry.TimeStampUTC then
  begin
    X.D['time_generated'] := LogEntry.GeneratedUTC;
  end;

  X.S['severity'] := LogSeverityToGCPString(LogEntry.Severity);
  X.S['sender'] := LogEntry.GetSenderDescription;
  X.S['message'] := '[' + LogEntry.GetSenderDescription + '] ' + LogEntry.LogText;

  if LogEntry.HasParameters then
  begin
    for i := 0 to pred(LogEntry.Parameters.Count) do
    begin
      X.O['params'].V[LogEntry.Parameters.Param(i).Name] := LogEntry.Parameters.Param(i).Value;
    end;
  end;

  EraseAndWriteLn(X.AsJSON(False, True));
end;

end.
