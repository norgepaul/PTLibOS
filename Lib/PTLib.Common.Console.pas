unit PTLib.Common.Console;

interface

uses
  System.SysUtils, System.SyncObjs, System.DateUtils, System.Variants, System.IOUtils,
  System.Types,

  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ENDIF}

  PTLib.Common.Types,
  PTLib.Common.Dates,
  PTLib.Common.Log,
  PTLib.Common.Strings,
  PTLib.Common.Interfaces;

type
  TConsoleColour = (
    ccDefault,
    ccRed,
    ccGreen,
    ccBlue,
    ccMagenta,
    ccCyan,
    ccYellow,
    ccIntenseRed,
    ccIntenseGreen,
    ccIntenseBlue,
    ccIntenseMagenta,
    ccIntenseCyan,
    ccIntenseYellow
  );

  TPTLibConsole = class
  strict private
    const TabChars = '    ';

    class var FLogCS: TCriticalSection;
    class var FLastProgressText: String;
    class var FProgressEraseCount: Integer;
    class var FDateDisplayType: TDateDisplayType;
    class var FLogFilename: String;
    {$IFDEF MSWINDOWS}
    class var FDefaultColourAttr: Word;
    class function GetConOutputHandle: THandle; static;
    {$ENDIF}
  private
    class procedure SetDefaultColors;
    class procedure SetColours(const Foreground: TConsoleColour; const Background: TConsoleColour = TConsoleColour.ccDefault);
    class procedure ResetColours;
    class procedure WriteToConsole(const Value: String; const FGColour: TConsoleColour = TConsoleColour.ccDefault; const BGColour: TConsoleColour = TConsoleColour.ccDefault); static;
    class procedure SetLogFilename(const Value: String); static;
    class procedure WriteFileLogLn(const Text: String); static;
  protected
    class procedure DoHandleLogEntry(const LogEntry: ILogEntry); virtual;

    class procedure EraseAndWrite(const Text: String; const FGColour: TConsoleColour = TConsoleColour.ccDefault; const BGColour: TConsoleColour = TConsoleColour.ccDefault);
    class procedure EraseAndWriteLn(const Text: String; const FGColour: TConsoleColour = TConsoleColour.ccDefault; const BGColour: TConsoleColour = TConsoleColour.ccDefault);
    class procedure WriteProgress(const Text: String); overload;
  public
    {$WARN UNSUPPORTED_CONSTRUCT ERROR}
    class constructor Create;
    class destructor Destroy;

    class var EnableLog: Boolean;
    class var EnableLogErrors: Boolean;
    class var EnableProgress: Boolean;
    class var ProgressWidth: Integer;
    class var MaxParameterLength: Integer;
    class var ReplaceTabsWithSpaces: Boolean;

    class procedure WriteLogLn(const Text: String; const RestoreProgress: Boolean = True;
      const FGColour: TConsoleColour = TConsoleColour.ccDefault; const BGColour: TConsoleColour = TConsoleColour.ccDefault);
    class procedure WriteLogErrorLn(const Text: String; const RestoreProgress: Boolean = True);
    class procedure WriteProgress(const Status: String; const Progress, ProgressMax: Integer); overload;
    class function LogSeverityToConsoleColour(const Value: TLogSeverity): TConsoleColour;
    class procedure EraseProgress;
    class procedure HandleLogEntry(LogEntry: ILogEntry);
    class procedure SetCursorPosition(const X, Y: Integer);
    class function GetCursorPosition: TPoint;

    class property LogFilename: String read FLogFilename write SetLogFilename;
    class property DateDisplayType: TDateDisplayType read FDateDisplayType write FDateDisplayType;
  end;

implementation

{ TPTLibConsole }

class procedure TPTLibConsole.EraseAndWriteLn(const Text: String; const FGColour: TConsoleColour; const BGColour: TConsoleColour);
begin
  EraseAndWrite(Text, FGColour, BGColour);

  WriteToConsole(#10);
end;

class constructor TPTLibConsole.Create;
begin
  FLogCS := TCriticalSection.Create;

  EnableLog := True;
  EnableProgress := True;
  ProgressWidth := 20;
  MaxParameterLength := 500;

  SetDefaultColors;
end;

class procedure TPTLibConsole.SetDefaultColors;
{$IFDEF MSWINDOWS}
var
  BufInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), BufInfo);

  FDefaultColourAttr := BufInfo.wAttributes;
end;
{$ENDIF}
{$IFDEF LINUX64}
begin
  { TODO : Implement }
end;
{$ENDIF}

class procedure TPTLibConsole.SetLogFilename(const Value: String);
begin
  if FLogFilename <> Value then
  begin
    ForceDirectories(ExtractFileDir(Value));

    FLogFilename := Value;
  end;
end;

class procedure TPTLibConsole.EraseProgress;
var
  i: Integer;
  WriteText: String;
begin
  WriteText := '';

  // Erase current progress
  for i := 1 to FProgressEraseCount do
  begin
    WriteText := WriteText + (#08 + ' ' + #08);
  end;

  FLogCS.Enter;
  try
    WriteToConsole(WriteText);
  finally
    FLogCS.Leave;
  end;

  FProgressEraseCount := 0;
end;

class procedure TPTLibConsole.HandleLogEntry(LogEntry: ILogEntry);
begin
  DoHandleLogEntry(LogEntry);
end;

class procedure TPTLibConsole.ResetColours;
{$IFDEF MSWINDOWS}
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), FDefaultColourAttr);
end;
{$ENDIF}
{$IFDEF LINUX64}
begin
  { TODO : Implement }
end;
{$ENDIF}

class destructor TPTLibConsole.Destroy;
begin
  FreeAndNil(FLogCS);
end;

class function TPTLibConsole.LogSeverityToConsoleColour(const Value: TLogSeverity): TConsoleColour;
begin
  case Value of
    LogSeverityError: Result := TConsoleColour.ccIntenseRed;
    LogSeverityWarning: Result := TConsoleColour.ccYellow;
    LogSeverityDebug: Result := TConsoleColour.ccMagenta;
    LogSeverityDebug2: Result := TConsoleColour.ccCyan;
    LogSeverityDebug3: Result := TConsoleColour.ccYellow;
  else
    begin
      Result :=TConsoleColour.ccDefault;
    end;
  end;
end;

class procedure TPTLibConsole.DoHandleLogEntry(const LogEntry: ILogEntry);
begin
  WriteLogLn(
    TPTLibLog.LogEntryToText(
      LogEntry,
      MaxParameterLength,
      DateDisplayType),
    True,
    LogSeverityToConsoleColour(LogEntry.Severity));
end;

class procedure TPTLibConsole.EraseAndWrite(const Text: String; const FGColour: TConsoleColour; const BGColour: TConsoleColour);
var
  i: Integer;
  WriteText: String;
begin
  WriteText := '';

  // Erase current progress
  for i := 1 to FProgressEraseCount do
  begin
    // Backspace the text
    WriteText := WriteText + #08;
  end;

  WriteText := WriteText + Text;

  for i := length(Text) to FProgressEraseCount do
  begin
    // Write over the remaining progress
    WriteText := WriteText + ' ';
  end;

  if FProgressEraseCount > 0 then
  begin
    for i := length(Text) to pred(FProgressEraseCount) do
    begin
      // Backspace to the correct position after the text
      WriteText := WriteText + #08;
    end;
  end;

  FProgressEraseCount := 0;

  WriteToConsole(WriteText, FGColour, BGColour);
end;

class procedure TPTLibConsole.WriteToConsole(const Value: String; const FGColour: TConsoleColour; const BGColour: TConsoleColour);
var
  ValueFixed: String;
begin
  SetColours(FGColour, BGColour);

  {$IFDEF LINUX64}
    // Erase any characters under the tab
    if ReplaceTabsWithSpaces then
    begin
      ValueFixed := StringReplace(Value, #09, '        ' + #08#08#08#08#08#08#08#08 + #09, [rfReplaceAll, rfIgnoreCase]);
    end
    else
    begin
      ValueFixed := Value;
    end;
  {$ELSE}
    ValueFixed := Value;
  {$ENDIF}

  Write(ValueFixed);

  ResetColours;
end;

class procedure TPTLibConsole.WriteLogErrorLn(
  const Text: String; const RestoreProgress: Boolean);
begin
  if EnableLogErrors then
  begin
    EraseAndWriteLn(Text);

    WriteFileLogLn(Text);

    if RestoreProgress then
    begin
      WriteProgress(FLastProgressText);
    end;
  end;
end;

class procedure TPTLibConsole.WriteFileLogLn(const Text: String);
begin
  if LogFilename <> '' then
  begin
    try
      TFile.AppendAllText(LogFilename, Text + #10);
    except
      // Nothing we can do!
    end;
  end;
end;

class procedure TPTLibConsole.WriteLogLn(const Text: String;
  const RestoreProgress: Boolean; const FGColour: TConsoleColour; const BGColour: TConsoleColour);
begin
  if EnableLog then
  begin
    FLogCS.Enter;
    try
      EraseAndWriteLn(Text, FGColour, BGColour);

      if RestoreProgress then
      begin
        WriteProgress(FLastProgressText);
      end;

      WriteFileLogLn(Text);
    finally
      FLogCS.Leave;
    end;
  end;
end;

class procedure TPTLibConsole.SetColours(const Foreground: TConsoleColour; const Background: TConsoleColour);
{$IFDEF MSWINDOWS}
  function ConsoleColourToWord(const Value: TConsoleColour; const BG: Boolean): Word;
  begin
    if BG then
    begin
      case Value of
        ccRed: Result := BACKGROUND_RED;
        ccGreen: Result := BACKGROUND_GREEN;
        ccBlue: Result := BACKGROUND_BLUE;
        ccMagenta: Result := BACKGROUND_BLUE or BACKGROUND_RED;
        ccCyan: Result := BACKGROUND_GREEN or BACKGROUND_BLUE;
        ccYellow: Result := BACKGROUND_GREEN or BACKGROUND_RED;
        ccIntenseRed: Result := BACKGROUND_RED or BACKGROUND_INTENSITY;
        ccIntenseGreen: Result := BACKGROUND_GREEN or BACKGROUND_INTENSITY;
        ccIntenseBlue: Result := BACKGROUND_BLUE or BACKGROUND_INTENSITY;
        ccIntenseMagenta: Result := BACKGROUND_BLUE or BACKGROUND_RED or BACKGROUND_INTENSITY;
        ccIntenseCyan: Result := BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY;
        ccIntenseYellow: Result := BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
      else
        begin
          Result := 0;
        end;
      end;
    end
    else
    begin
      case Value of
        ccRed: Result := FOREGROUND_RED;
        ccGreen: Result := FOREGROUND_GREEN;
        ccBlue: Result := FOREGROUND_BLUE;
        ccMagenta: Result := FOREGROUND_BLUE or FOREGROUND_RED;
        ccCyan: Result := FOREGROUND_GREEN or FOREGROUND_BLUE;
        ccYellow: Result := FOREGROUND_GREEN or FOREGROUND_RED;
        ccIntenseRed: Result := FOREGROUND_RED or FOREGROUND_INTENSITY;
        ccIntenseGreen: Result := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
        ccIntenseBlue: Result := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
        ccIntenseMagenta: Result := FOREGROUND_BLUE or FOREGROUND_RED or FOREGROUND_INTENSITY;
        ccIntenseCyan: Result := FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
        ccIntenseYellow: Result := FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
      else
        begin
          Result := 0;
        end;
      end;
    end;
  end;

var
  ColourAttr: Word;
begin
  if Foreground <> TConsoleColour.ccDefault then
  begin
    ColourAttr := ConsoleColourToWord(Foreground, False);
  end
  else
  begin
    ColourAttr := FDefaultColourAttr;
  end;

  if Background <> TConsoleColour.ccDefault then
  begin
    ColourAttr := ColourAttr or ConsoleColourToWord(Background, True);
  end;

  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), ColourAttr);
end;
{$ENDIF}
{$IFDEF LINUX64}
begin
  { TODO : Implement }
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TPTLibConsole.GetConOutputHandle: THandle;
begin
  Result := GetStdHandle(STD_OUTPUT_HANDLE);
end;
{$ENDIF}

class procedure TPTLibConsole.SetCursorPosition(const X, Y: Integer);
{$IFDEF MSWINDOWS}
var
  Coords: _COORD;
  CHandle: THandle;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  CHandle := GetConOutputHandle;

  Coords.X := X;
  Coords.Y := X;

  SetConsoleCursorPosition(CHandle, Coords);
{$ENDIF}
end;

class function TPTLibConsole.GetCursorPosition: TPoint;
{$IFDEF MSWINDOWS}
var
  BufferInfo: TConsoleScreenBufferInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetConsoleSCreenBufferInfo(GetConOutputHandle, BufferInfo);

  Result := TPoint.Create(BufferInfo.dwCursorPosition.X, BufferInfo.dwCursorPosition.Y);
{$ELSE}
  Result := TPoint.Zero;
{$ENDIF}
end;

class procedure TPTLibConsole.WriteProgress(const Status: String;
  const Progress, ProgressMax: Integer);
var
  Pct: Integer;
  i: Integer;
  ProgressText: String;
begin
  if EnableProgress then
  begin
    if (Progress <> 0) or
       (ProgressMax <> 0) then
    begin
      if ProgressMax = 0 then
      begin
        Pct := 0;
      end
      else
      begin
        Pct := Trunc((Progress / ProgressMax) * 100);
      end;

      ProgressText := '[';

      for i := 1 to ProgressWidth do
      begin
        if (i / progresswidth) * 100 <= Pct then
        begin
          ProgressText := ProgressText + '▓';
        end
        else
        begin
          ProgressText := ProgressText + '░';
        end;
      end;

      ProgressText := format('%s] %d/%d', [ProgressText, Progress, ProgressMax]);
    end;

    if Status <> '' then
    begin
      if ProgressText <> '' then
      begin
        ProgressText := ProgressText + ' - ';
      end;

      ProgressText := ProgressText + Status;
    end;

    FLogCS.Enter;
    try
      WriteProgress(ProgressText);
    finally
      FLogCS.Leave;
    end;
  end;
end;

class procedure TPTLibConsole.WriteProgress(const Text: String);
begin
  if (FLastProgressText <> Text) or
     (FProgressEraseCount = 0) then
  begin
    EraseAndWrite(Text);

    FLastProgressText := Text;
    FProgressEraseCount := length(FLastProgressText);
  end;
end;

end.
