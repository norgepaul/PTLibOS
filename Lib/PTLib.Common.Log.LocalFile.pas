{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Log.LocalFile                               }
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

unit PTLib.Common.Log.LocalFile;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,

  PTLib.Common.Interfaces,
  PTLib.Common.Log;

type
  TFileLogEntry = class(TLogEntry,
                        IFileLogEntry)
  strict private
    FFilenameOverride: String;
  private
    function GetFilenameOverride: String;
    procedure SetFilenameOverride(const Value: String);
  public
    property FilenameOverride: String read GetFilenameOverride write SetFilenameOverride;
  end;

  TFilenameType = (
    fntDefaultFilename,
    fntFilenameOverride,
    fntBoth
  );

  TPTLibFileLog = class(TBasePTLibTextLogReceiver)
  strict private
    FFileCS: TCriticalSection;

    FBaseDirectory: String;
    FDefaultFilename: String;
    FDirectoryPerDay: Boolean;
    FDefaultLogFileExtension: String;
    FDirectoryPerDayDateFormat: String;
    FShowGeneratedTimestamp: Boolean;
    FFilenameType: TFilenameType;
  private
  protected
    procedure DoOnLog(const LogEntry: ILogEntry); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCurrentLogDirectory(const TimestampUTC: TDateTime): String;
  published
    property BaseDirectory: String read FBaseDirectory write FBaseDirectory;
    property DefaultFilename: String read FDefaultFilename write FDefaultFilename;
    property DirectoryPerDay: Boolean read FDirectoryPerDay write FDirectoryPerDay;
    property DefaultLogFileExtension: String read FDefaultLogFileExtension write FDefaultLogFileExtension;
    property DirectoryPerDayDateFormat: String read FDirectoryPerDayDateFormat write FDirectoryPerDayDateFormat;
    property ShowGeneratedTimestamp: Boolean read FShowGeneratedTimestamp write FShowGeneratedTimestamp;
    property FilenameType: TFilenameType read FFilenameType write FFilenameType;
  end;

implementation

{ TPTLibFileLog }

constructor TPTLibFileLog.Create(AOwner: TComponent);
begin
  inherited;

  FFileCS := TCriticalSection.Create;

  FBaseDirectory := 'c:\logs\';
  FDefaultFilename := 'LogFile.log';
  FDirectoryPerDay := True;
  FFilenameType := TFilenameType.fntDefaultFilename;
  FDefaultLogFileExtension := '.log';
  FDirectoryPerDayDateFormat := 'YYYY-MM-DD';
end;

destructor TPTLibFileLog.Destroy;
begin
  FreeAndNil(FFileCS);

  inherited;
end;

function TPTLibFileLog.GetCurrentLogDirectory(const TimestampUTC: TDateTime): String;
begin
  // Generate the log directory
  Result := IncludeTrailingPathDelimiter(FBaseDirectory);

  if FDirectoryPerDay then
  begin
    Result := IncludeTrailingPathDelimiter(Result + FormatDateTime(FDirectoryPerDayDateFormat, LocaliseTimeStamp(TimestampUTC)));
  end;
end;

procedure TPTLibFileLog.DoOnLog(const LogEntry: ILogEntry);

  procedure WriteLine(const Filename, Text: String);
  var
    F: TextFile;
    RealFilename: String;
    FileDir: String;
  begin
    if Filename <> '' then
    begin
      FileDir := ExtractFileDir(Filename);

      RealFilename := Filename;

      if ExtractFileExt(RealFilename) = '' then
      begin
        RealFilename := RealFilename + FDefaultLogFileExtension;
      end;

      ForceDirectories(FileDir);

      FFileCS.Enter;
      try
        try
          {$I-}
          AssignFile(F, RealFilename);

          // Try to append to the log file. If it fails, create a new one
          Append(F);

          if IOResult <> 0 then
            Rewrite(F);
          try
            Writeln(F, Text);

            Flush(F);
          finally
            CloseFile(F);

            // Wait for the file to close
            while IOResult <> 0 do;
          end;
          {$I+}
        except
          on e: Exception do
          begin
            DoOnException(e);
          end;
        end;
      finally
        FFileCS.Leave;
      end;
    end;
  end;

var
  Filename1, Filename2, FileDir, LogText: String;
  FileLogEntry: IFileLogEntry;
begin
  inherited;

  LogText := TPTLibLog.LogEntryToText(LogEntry);

  // Generate the log text
(*  LogText := FormatTimestamp(LocaliseTimeStamp(LogEntry.TimeStampUTC));

  if FShowGeneratedTimestamp then
  begin
    LogText := LogText + #09 + FormatTimestamp(LocaliseTimeStamp(LogEntry.GeneratedUTC));
  end;

  SeverityText := GetLogSeverityText(LogEntry.Severity);

  DoOnGetSeverityText(
    LogEntry.Severity,
    SeverityText);

  if SeverityText <> '' then
  begin
    LogText := LogText + #09 + SeverityText;
  end;

  LogText := LogText + #09 + LogEntry.LogText;  *)

  FileDir := GetCurrentLogDirectory(LogEntry.TimestampUTC);

  Filename1 := '';
  Filename2 := '';

  if FFilenameType in [TFilenameType.fntDefaultFilename, TFilenameType.fntBoth] then
  begin
    Filename1 := FileDir + FDefaultFilename;
  end;

  if (FFilenameType in [TFilenameType.fntFilenameOverride, TFilenameType.fntBoth]) and
     (Supports(LogEntry, IFileLogEntry, FileLogEntry)) and
     (FileLogEntry.FilenameOverride <> '') then
  begin
    Filename2 := FileDir + FileLogEntry.FilenameOverride;
  end;

  WriteLine(Filename1, LogText);
  WriteLine(Filename2, LogText);
end;

{ TFileLogEntry }

function TFileLogEntry.GetFilenameOverride: String;
begin
  Result := FFilenameOverride;
end;

procedure TFileLogEntry.SetFilenameOverride(const Value: String);
begin
  FFilenameOverride := Value;
end;

end.
