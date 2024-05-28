{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Database.Services.BackupRestore.Firebird           }
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

unit PTLib.Database.Services.BackupRestore.Firebird;

interface

uses
  Windows, Messages, SysUtils, Classes, System.IOUtils, DateUtils,

  FireDAC.Phys.IB,
  FireDAC.Stan.Intf,
  FireDAC.Phys,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.IBWrapper,

  PTLib.Common.Types,
  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.Files,
  PTLib.Database.Services;


const
  CNT_DB_EXT      = '.fdb';   // Firebird database extension
  CNT_BACKUP_EXT  = '.fbb';   // Firebird database backup file extension
  FIREBIRD_SYSDBA = 'SYSDBA';

type
  TPTLibFirebirdBackup = class;

  TUnPublish = byte;

  TOnBackupComplete = procedure (sender: TObject; FinalFileName: String) of object;

  TPTLibBaseBackupRestore = class(TBasePTLibDatabaseService)
  private
    FDriverLink: TFDPhysIBDriverLink;

    FWorking: Boolean;
    FUsername: String;
    FPassword: String;
    FServerName: String;
    FPort: Integer;
    FDatabaseName: TFilename;
    FHelpContext: THelpContext;
    FBackupFiles: TStringList;
    FLibraryName: TFilename;
    FIsLocal: Boolean;
    FVerbose: Boolean;
    FEmailResults: Boolean;
    FEmailRecipients: String;

    FOnBackupComplete: TOnBackupComplete;

    procedure SplitFilesAndSizes(Files: TStringlist);
    procedure SetBackupFiles(const Value: TStringList);
    procedure CheckCanBackup;
  protected
    procedure MyProcessMessages;
    {$IF CompilerVersion < 29.0}
    procedure OnFDError(ASender: TObject; const AInitiator: IFDStanObject; var AException: Exception);
    {$else}
    procedure OnFDError(ASender, AInitiator: TObject; var AException: Exception);
    {$endif}
    procedure OnFDProgress(ASender: TFDPhysDriverService; const AMessage: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Working: Boolean read FWorking;
  published
    Property OnBackupComplete:TOnBackupComplete read FOnBackupComplete write FOnBackupComplete;

    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;
    property HelpContext: THelpContext read FHelpContext write FHelpContext;
    property ServerName: String read FServerName write FServerName;
    property Port: Integer read FPort write FPort;
    property DatabaseName: TFilename read FDatabaseName write FDatabaseName;
    property BackupFiles: TStringList read FBackupFiles write SetBackupFiles;
    property LibraryName: TFilename read FLibraryName write FLibraryName;
    property IsLocal: Boolean read FIsLocal write FIsLocal;
    property Verbose: Boolean read FVerbose write FVerbose;
    property EmailResults: Boolean read FEmailResults write FEmailResults;
    property EmailRecipients: String read FEmailRecipients write FEmailRecipients;
  end;

  TPTLibFirebirdBackup = class(TPTLibBaseBackupRestore)
  private
    FUseDateTimeAsFilename: Boolean;
    FZipBackup: Boolean;
    FBackupOptions: TIBBackupOptions;
    FTestBackup: Boolean;
    FSMTPSendEmailOnCompletion: Boolean;
    FSMTPIncludeLog: Boolean;
    FEmailRecipients: String;
    FTestRestoreFilename: String;

    procedure CheckAndCompressFiles(const SrcFilename, DstFilename: String);
    function GetValueNamePart(const Value: String): String;
  protected
    procedure OnLogStatus(Sender: TObject; const StatusTexts: array of String);

    Procedure DoSingleFileBackup(Backup: TFDIBBackup);
    procedure DoMultipleFileBackup(Backup: TFDIBBackup; FileCount: Integer; FilePartSize: Integer);

  public

    function MakeFileName(BaseName: String; IncludeDateTime: Boolean=False):String;

    function PerformBackupEx(FileCount: Integer; FilePartSize: Integer): Boolean;

    function PerformBackup: Boolean;

    constructor Create(Sender: TComponent); override;

  published
    property SMTPProperties;
    property BackupOptions: TIBBackupOptions read FBackupOptions write FBackupOptions;
    property ZipBackup: Boolean read FZipBackup write FZipBackup;
    property UseDateTimeAsFilename: Boolean read FUseDateTimeAsFilename write FUseDateTimeAsFilename;
    property TestBackup: Boolean read FTestBackup write FTestBackup;
    property SMTPSendEmailOnCompletion: Boolean read FSMTPSendEmailOnCompletion write FSMTPSendEmailOnCompletion;
    property SMTPIncludeLog: Boolean read FSMTPIncludeLog write FSMTPIncludeLog;
    property EmailRecipients: String read FEmailRecipients write FEmailRecipients;
    property TestRestoreFilename: String read FTestRestoreFilename write FTestRestoreFilename;
  end;

  TPTLibFirebirdRestore = class(TPTLibBaseBackupRestore)
  private
    FRestoreOptions: TIBRestoreOptions;
    FRestoredFiles: TStringList;
    FPageSize: Integer;
  public
    constructor Create(Sender: TComponent); override;
    destructor Destroy; override;
    procedure PerformRestore(LogFilename: String = '');
  published
    property RestoreOptions: TIBRestoreOptions read FRestoreOptions write FRestoreOptions;
    property PageSize: Integer read FPageSize write FPageSize;
    property RestoredFiles: TStringList read FRestoredFiles;
  end;

implementation

const
  PWD_BACKUP = 'fswDDFA414tSGsd3pqhwznfohfghhsadnc34rthr5DRFASd';

resourcestring
  StrCompressing = 'Compressing';
  StrZipFileAlreadyExists = 'Zip file already exists';
  StrBackupFailed = 'Backup failed';
  StrRecalculatingIndexSelectivity = 'Recalculating Index Selectivity.';
  StrRemovingTemporaryFiles = 'Removing temporary files.';
  StrTestRestoreCompleted = 'Test restore completed successfully';
  StrErrorS = 'Error: %s';
  StrBeginningTestRestore = 'Beginning test restore';
  StrUnableToConnectToBackupService = 'Unable to connect to the backup service';
  StrBackupStarted = 'Backup started';
  StrEasyIPDatabaseBackupLog = '# Easy-IP Database Backup Log';
  StrComplete = 'Complete';
  StrRestoreStarted = 'Restore started';
  StrDecompressing = 'Decompressing';
  StrRestoreCompleted = 'Restore completed';
  StrNoBackupFilesAssigned = 'No backup files assigned';
  StrNoDatabaseNameAssigned = 'No database name assigned';
  StrNoServerNameAssiged = 'No server name assigned';
  StrCompressingDatabaseToTempFile = 'Compressing database %s to temporary file %s';
  StrMovingTempFileTo = 'Moving temp file to final destinataion %s';
  StrError = 'Error: ';

procedure TPTLibFirebirdBackup.CheckAndCompressFiles(const SrcFilename, DstFilename: String);
var
  TempFilename: String;
begin
  TempFilename := TFileUtils.MakeGUIDTempFileName;

  DoOnStatus(StrCompressing);

  if (FileExists(DstFilename)) and (not DeleteFile(DstFilename)) then
  begin
    DoOnStatus(StrZipFileAlreadyExists);

    DoOnStatus(StrBackupFailed);
  end
  else
  begin
    DoOnStatus(format(StrCompressingDatabaseToTempFile, [SrcFilename, TempFilename]));

    TFileUtils.CompressFileToZip(SrcFileName, TempFilename);
    try
      DoOnStatus(format(StrMovingTempFileTo, [DstFilename]));

      CopyFile(PWideChar(TempFilename), PWideChar(DstFilename), TRUE);
    finally
      DeleteFile(TempFilename);
    end;
  end;
end;

procedure TPTLibFirebirdBackup.OnLogStatus(Sender: TObject; const StatusTexts: Array of String);
begin
  DoOnStatus(StatusTexts[0]);
end;

function TPTLibFirebirdBackup.GetValueNamePart(const Value: String): String;
var
  mIndex: Integer;
begin
  mIndex:=pos('=',value);
  if mIndex>0 then
    result:=copy(value,1,mIndex-1)
  else
    result:=Value;
end;

function TPTLibFirebirdBackup.MakeFileName(BaseName: String; IncludeDateTime: Boolean=False):String;
var
  mYear:  Word;
  mMonth: Word;
  mDay:   Word;
  mHour:  Word;
  mMinute:Word;
  mSecs:  Word;
  mMSecs: Word;

  function IntToStrPadded(Value: Integer; Range: Integer = 2):String;
  begin
    result:=IntToStr(Value);
    if (Range>0) and (result.Length < Range) then
    begin
      while (result.length < Range) do
      begin
        result:=('0' + Result);
      end;
    end;
  end;

begin
  result:= StringReplace(basename,'  ','_',[rfReplaceAll]);
  result:= StringReplace(result,' ','_',[rfReplaceAll]);
  if IncludeDateTime then
  begin
    DecodeDateTime(now,mYear,mMonth,mDay,mHour,mMinute,mSecs,mMSecs);
    result:= result + '_FB_' + IntToStr(mYear);   //** Do not alter, "_FB_" is used as a marker (!)
    result:= result + '_' + IntToStrPadded(mMonth);
    result:= result + '_' + IntToStrPadded(mDay);
    result:= result + '_' + IntToStrPadded(mHour);
    result:= result + '_' + IntToStrPadded(mMinute);
    result:= result + '_' + IntToStrPadded(mSecs);
  end;
end;

Procedure TPTLibFirebirdBackup.DoSingleFileBackup(Backup:TFDIBBackup);
var
  TempFile:     String;
  ToZipName:    String;
  BaseName:     String;
  FinalName:    String;
  FinalPath:    String;
  TempFolder:   String;
begin
  writeln('Single-file backup begins');

  // Grab the first filename, remove extension
  BaseName := ExtractFileName(BackupFiles[0]);
  BaseName := ChangeFileExt(BaseName,'');

  // Generate a "final" filename, this includes "date time" markers
  FinalName := MakeFileName(BaseName,FUseDateTimeAsFilename);

  // Generate a final path, this is the full destination path + filename
  FinalPath := IncludeTrailingPathDelimiter(ExtractFilePath(BackupFiles[0]));
  FinalPath := FinalPath + FinalName;

  // Setup our log-filename
  LogFileName := FinalPath + '.log';

  // Now attach the correct extension, depending on the mode
  if FZipBackup then
    FinalPath := FinalPath + '.zip'
  else
    FinalPath := FinalPath + CNT_BACKUP_EXT;

  // Create a folder inside system:temp
  TempFolder := TTempFile.GetUniqueTempFolder;
  if not CreateDir(TempFolder) then
    Raise Exception.Create('Failed to create temporary folder error');

  // generate a unique filename inside our temp-directory
  TempFile := IncludeTrailingPathDelimiter(TempFolder) + TFileUtils.MakeGUIDFileName;

  writeln('Basename = ' + BaseName);
  writeln('TempFile = ' + TempFile);
  writeln('Final-Name = ' + FinalName);
  writeln('Final-path = ' + FinalPath);

  // Inform backup to use temp-file
  Backup.BackupFiles.Add(TempFile);

  // Execute the actual backup
  Backup.Backup;

  try

    // Compress backup?
    if FZipBackup then
    begin

      // Delete target if it exists, otherwise report error
      if FileExists(FinalPath) then
      begin
        if not DeleteFile(FinalPath) then
        begin
          DoOnStatus(StrZipFileAlreadyExists);
          DoOnStatus(StrBackupFailed);
          exit;
        end;
      end;

      // We want the file inside the zip to be actual, not some
      // system generated filename, so we re-label it
      ToZipName:=ExtractFilePath(TempFile);
      ToZipName:=IncludeTrailingPathDelimiter(ToZipName) + BaseName + CNT_BACKUP_EXT;  //FinalName

      // Rename the tempfile. If it doesnt work try one alternative
      // before we resort to the .tmp file itself
      if not renamefile(TempFile,ToZipName) then
      begin
        ToZipName := ExtractFilePath(TempFile);
        ToZipName := IncludeTrailingPathDelimiter(ToZipName) + BaseName + '0x1' + CNT_BACKUP_EXT;  //FinalName

        if not renamefile(TempFile,ToZipName) then
        begin
          ToZipName := ChangeFileExt(TempFile,CNT_BACKUP_EXT);
        end;
      end;

      // Compress backup table directly to target
      TFileUtils.CompressFileToZip(ToZipName,FinalPath);

      // Now replace the filename internally for our cleanup code
      TempFile := ToZipName;

    end else
    Begin
      if FileExists(TempFile) then
      renameFile(TempFile,FinalPath);
    end;

    if assigned(OnBackupComplete) then
      OnBackupComplete(self,FinalPath);

  finally
    // And finally delete the temporary backup file
    if FileExists(TempFile) then
      DeleteFile(TempFile);

    // And our temp:temp folder. The system will clean up this directory
    // automatically on demand, so it's just good form and not a requirement
    if DirectoryExists(TempFolder) then
    rmDir(TempFolder);
  end;
end;

procedure TPTLibFirebirdBackup.DoMultipleFileBackup(Backup: TFDIBBackup; FileCount: Integer; FilePartSize: Integer);
var
  x:            Integer;
  BaseName:     String;
  BasePath:     String;
  TempFolder:   String;
  Text:         String;
  ZipName:      String;
  FileName:     String;
  FinalName:    String;
  RootFile:     String;
  RawFileList:  TStringList;
begin
  writeln('Multiple-file backup begins');

  // flush backup-agent files
  Backup.BackupFiles.Clear;

  // Grab the first filename, remove extension
  BaseName := ExtractFileName(BackupFiles[0]);
  BaseName := ChangeFileExt(BaseName,'');

  // grab the base-path, we need this further down
  BasePath := IncludeTrailingPathDelimiter(ExtractFileDir(BackupFiles[0]));

  // generate a date-time filename from the base-name
  FileName := MakeFileName(BaseName, FUseDateTimeAsFilename);

  // Setup our log-filename
  LogFileName := BasePath + Filename + '.log';

  // Create a folder inside system:temp
  TempFolder := TTempFile.GetUniqueTempFolder;
  if not CreateDir(TempFolder) then
    Raise Exception.Create('Failed to create temporary folder error');

  // RawFileList: contains backup-part filenames WITHOUT "=bytesize" text
  RawFileList := TStringList.Create;
  try

    // Pre-calculate filenames based on "parts" parameter
    for x:=1 to FileCount-1 do
    begin
      Text := IncludeTrailingPathDelimiter(TempFolder);
      Text := Text + FileName + '(' + IntToStr(x-1) + ')' + CNT_BACKUP_EXT;
      RawFileList.add(Text);
      Backup.BackupFiles.Add(Text + '=' + IntToStr(FilePartSize) );
    end;

    // always leave the last file without a size declarations (see docs)
    Text := IncludeTrailingPathDelimiter(TempFolder) + FileName + CNT_BACKUP_EXT;
    Backup.BackupFiles.Add(Text);
    RawFileList.add(Text);

    // Keep the root-file name for the backup
    // Note: This is tagged to the Zip:Comment chunk!
    RootFile := ExtractFileName(Text);

    // Execute the actual backup
    Backup.Backup;

    if FZipBackup then
    begin
      Writeln('Compressing files');

      // Make our zipfile name
      ZipName := IncludeTrailingPathDelimiter(TempFolder) + FileName + '.zip';
      TFileUtils.CompressFilesToZip(RawFileList,ZipName,RootFile);

      FinalName := IncludeTrailingPathDelimiter(BasePath) + FileName + '.zip';

      //Now move the zipfile to the target path
      RenameFile(ZipName, FinalName);

      // Signal final-name, we need this or the FTP wont kick in properly
      if assigned(OnBackupComplete) then
        OnBackupComplete(self,FinalName);

    end else
    begin
      Writeln('Copying backup files to target');

      for x:=0 to RawFileList.Count-1 do
      begin
        Text:=IncludeTrailingPathDelimiter(BasePath) + ExtractFileName(RawFileList[x]);

        // Attempt to delete target file first
        if FileExists(Text) then
        begin
          if not deleteFile(Text) then
            Raise Exception.CreateFmt('A filename conflict occured, file "%s" already exists error',[Text]);
        end;

        // Rename the file to the new path
        renameFile(RawFileList[x],Text);
      end;

      // Signal final-name, we need this or the FTP wont kick in properly
      FinalName := IncludeTrailingPathDelimiter(BasePath) + FileName + CNT_BACKUP_EXT;
      if assigned(OnBackupComplete) then
        OnBackupComplete(self,FinalName);
    end;

    //Cleanup
    Writeln('Cleaning up temporary files');
    for x:=0 to RawFileList.Count-1 do
    begin
      if FileExists(RawFileList[x]) then
      DeleteFile(RawFileList[x]);
    end;

  finally
    //Note: Folders in System:Temp are automatically cleaned up by the system
    rmdir(TempFolder);

    RawFileList.free;
  end;
end;


function TPTLibFirebirdBackup.PerformBackupEx(FileCount: Integer; FilePartSize: Integer): Boolean;
var
  QBackup: TFDIBBackup;
begin
  CheckCanBackup;

  FWorking:=True;
  try

    QBackup := TFDIBBackup.Create(Self);
    try

      QBackup.DriverLink := FDriverLink;
      try
        // Initialize service properties
        QBackup.BackupFiles.Clear;
        QBackup.Options := FBackupOptions;
        QBackup.UserName := FUsername;
        QBackup.Password := FPassword;
        QBackup.Database := FDatabaseName;
        QBackup.Verbose := FVerbose;

        // Initialize backup protocol and host
        if FIsLocal then
        begin
          QBackup.Protocol := ipLocal;
          QBackup.Host := '';
        end else
        begin
          QBackup.Protocol := ipTCPIP;
          QBackup.Host := format('%s/%d', [FServername, FPort]);
        end;

        // Use client-library-dll if provided
        if FLibraryName <> '' then
          QBackup.DriverLink.VendorLib := FLibraryName;

        // setup callback event handlers
        QBackup.OnProgress := OnFDProgress;
        QBackup.OnError := OnFDError;

        // Perform backup
        if (FilePartSize>0) and (FileCount>0) then
          self.DoMultipleFileBackup(QBackup,FileCount,FilePartSize)
        else
          self.DoSingleFileBackup(QBackup);

      except
        on e:Exception do
        begin
          DoOnStatus(format(StrErrorS, [e.Message]));
          raise;
        end;
      end;

      Result := true;
    finally
      FreeAndNil(QBackup);
      DoOnStatus(StrComplete);
    end;
  finally
    FWorking:=False;
  end;
end;

(* Note: When using zipped backups, a proper filename must be provided
         in BackupFiles[]. Otherwise the final zip results in purely
         ".extension" when its copied. *)
function TPTLibFirebirdBackup.PerformBackup: Boolean;
var
  Filename, ZipFilename, TempDir, NowDateTime, Ext: String;
  i: Integer;
  IBBackupService: TFDIBBackup;
  ErrorMsg, RestoreFilename: String;
  EIPDBRestore: TPTLibFirebirdRestore;
begin
  ErrorMsg := '';

  CheckCanBackup;

  FWorking := TRUE;

  IBBackupService := TFDIBBackup.Create(Self);
  try
    IBBackupService.DriverLink := FDriverLink;
    try
      // Move the file size into the TStringList object property
      SplitFilesAndSizes(TStringlist(BackupFiles));

      IBBackupService.BackupFiles.Clear;

      // Create a temporary directory
      TempDir := IncludeTrailingPathDelimiter(TPath.GetTempPath);
      if not ForceDirectories(TempDir) then
        Raise Exception.CreateFmt('Failed to create temporary path [%s] error',[tempdir]);

      for i := 0 to pred(BackupFiles.Count) do
      begin
        if ZipBackup then
          IBBackupService.BackupFiles.Add(concat(TempDir, ExtractFilename(ChangeFileExt(FBackupFiles[i], '.fbk'))))
        else
          IBBackupService.BackupFiles.Add(ChangeFileExt(FBackupFiles[i], '.fbk'));

        if Assigned(FBackupFiles.Objects[i]) then
        IBBackupService.BackupFiles[i] := format('%s=%d', [IBBackupService.BackupFiles[i], Integer(FBackupFiles.Objects[i])]);
      end;

      NowDateTime := formatDateTime('yyyy.mm.dd hh.nn.ss.zzz', now);

      // Generate Filenames
      if FZipBackup then
      begin
        Filename := TFileUtils.ExtractFilenameNoExt(FBackupFiles[0]);

        if FUseDateTimeAsFilename then
          Filename := concat(Filename, '-', NowDateTime);
      end
      else
      begin
        if FUseDateTimeAsFilename then
          for i := 0 to pred(IBBackupService.BackupFiles.Count) do
          begin
            Ext := ExtractFileExt(IBBackupService.BackupFiles[i]);
            IBBackupService.BackupFiles[i] := concat(TFileUtils.ExtractFilenameNoExt(IBBackupService.BackupFiles[i]), '-', NowDateTime, Ext);
          end;

        Filename := TFileUtils.RemoveFileExt(IBBackupService.BackupFiles[0]);
      end;

      FLogFilename := Filename + '.log';
      ZipFilename := Filename + '.fbkzip';

      // Make sure the directory exists
      if ExtractFileDir(ZipFilename) <> '' then
        ForceDirectories(ExtractFileDir(ZipFilename));

      // Remove any existing log file
      DeleteFile(FLogFilename);

      // Write the log file header
      WriteToLogFile(StrEasyIPDatabaseBackupLog);
      WriteToLogFile(format('# %s', [DateTimeToStr(now)]));
      WriteToLogFile('');
      DoOnStatus(StrBackupStarted);

      // Set the Backup Service Params
      if FIsLocal then
      begin
        IBBackupService.Protocol := ipLocal;
        IBBackupService.Host := '';
      end
      else
      begin
        IBBackupService.Protocol := ipTCPIP;
        IBBackupService.Host := format('%s/%d', [FServername, FPort]);
      end;

      IBBackupService.UserName := FUsername;
      IBBackupService.Password := FPassword;
      IBBackupService.Database := FDatabaseName;
      IBBackupService.Verbose := FVerbose;

      if FLibraryName <> '' then
        IBBackupService.DriverLink.VendorLib := FLibraryName;

      IBBackupService.Options := FBackupOptions;

      // Backup the database
      try
        // Attach the backup service
        IBBackupService.OnProgress := OnFDProgress;
        //IBBackupService.OnError := OnFDError;

        IBBackupService.Backup;

        if FZipBackup then
          CheckAndCompressFiles(IBBackupService.BackupFiles[0], ZipFilename);
      finally
        // Zip the resulting file(s)
        if FZipBackup then
        try
          for i := 0 to pred(IBBackupService.BackupFiles.Count) do
            DeleteFile(GetValueNamePart(IBBackupService.BackupFiles[i]));

          RemoveDir(TempDir);
        except
          // Don't worry about it
        end;
      end;

      Result := TRUE;

      if assigned(OnBackupComplete) then
      Begin
        if FZipBackup then
        OnBackupComplete(self,ZipFilename) else
        OnBackupComplete(self,IBBackupService.BackupFiles[0]);
      end;

      if FTestBackup then
      begin
        Result := FALSE;

        EIPDBRestore := TPTLibFirebirdRestore.Create(nil);
        try
          DoOnStatus(StrBeginningTestRestore);

          // Find a unique temporary restore filename
          if FTestRestoreFilename = '' then
          begin
            RestoreFilename := TTempFile.getTempFileName;
          end
          else
          begin
            RestoreFilename := FTestRestoreFilename;
          end;

          i := 0;

          While FileExists(format('%s%d.tmp', [RestoreFilename, i])) do
            inc(i);

          RestoreFilename := format('%s%d.tmp', [RestoreFilename, i]);

          EIPDBRestore.OnStatus := OnStatus;
          EIPDBRestore.OnProgress := OnProgress;
          EIPDBRestore.ServerName := FServerName;
          EIPDBRestore.Port := FPort;
          EIPDBRestore.Username := FUsername;
          EIPDBRestore.Password := FPassword;
          EIPDBRestore.WriteLogFile := WriteLogFile;
          EIPDBRestore.FLibraryName := FLibraryName;
          EIPDBRestore.Verbose := Verbose;
          EIPDBRestore.IsLocal := IsLocal;
          EIPDBRestore.RestoreOptions := EIPDBRestore.RestoreOptions + [roReplace];

          if FIsLocal then
            IBBackupService.Protocol := ipLocal
          else
            IBBackupService.Protocol := ipTCPIP;

          EIPDBRestore.DatabaseName := ZipFilename;

          if FZipBackup then
            EIPDBRestore.BackupFiles.Add(ZipFilename)
          else
            EIPDBRestore.BackupFiles.Assign(IBBackupService.BackupFiles);

          EIPDBRestore.DatabaseName := RestoreFilename;
          try
             EIPDBRestore.PerformRestore(FLogFilename);
             DoOnStatus(StrTestRestoreCompleted);

             Result := TRUE;
          except
            on e:Exception do
            begin
              DoOnStatus(format(StrErrorS, [ErrorMsg]));
            end;
          end;
        finally
          DoOnStatus(StrRemovingTemporaryFiles);
          DeleteFile(RestoreFilename);

          for i := 0 to pred(EIPDBRestore.RestoredFiles.Count) do
            DeleteFile(EIPDBRestore.RestoredFiles[i]);

          FreeAndNil(EIPDBRestore);
        end;
      end;
    except
      on e:Exception do
      begin
        DoOnStatus(format(StrErrorS, [ErrorMsg]));

        raise;
      end;
    end;

  finally
    // Tidy up
    DoOnStatus(StrComplete);
    FWorking := FALSE;

    FreeAndNil(IBBackupService);
  end;
end;

constructor TPTLibFirebirdBackup.Create(Sender: TComponent);
begin
  inherited;
  FZipBackup := False;
  FUseDateTimeAsFilename := False;
end;

{ TBaseBackupRestore  }

constructor TPTLibBaseBackupRestore.Create(AOwner: TComponent);
begin
  inherited;

  FDriverLink := TFDPhysIBDriverLink.Create(Self);
  FBackupFiles := TStringList.Create;

  FPort := 3050;
  FServerName := 'localhost';
  FUsername := FIREBIRD_SYSDBA;
  FVerbose := TRUE;
end;

procedure TPTLibBaseBackupRestore.MyProcessMessages;
var
   msg : TMsg;
begin
  if (Windows.PeekMessage(msg,0,0,0,PM_REMOVE) = true) then
  begin
    Windows.TranslateMessage(msg);
    Windows.DispatchMessage(msg);
  end;
end;

procedure TPTLibBaseBackupRestore.OnFDProgress(ASender: TFDPhysDriverService; const AMessage: String);
begin
  DoOnStatus(AMessage);
  MyProcessMessages;
end;

{$IF CompilerVersion < 29.0}
procedure TBaseBackupRestore.OnFDError(ASender: TObject; const AInitiator: IFDStanObject; var AException: Exception);
{$else}
procedure TPTLibBaseBackupRestore.OnFDError(ASender, AInitiator: TObject; var AException: Exception);
{$endif}
begin
  DoOnStatus(StrError + AException.Message);
  MyProcessMessages;
end;

procedure TPTLibBaseBackupRestore.SplitFilesAndSizes(Files: TStringlist);
var
  i, FileSizeInt: Integer;
begin
  for i := 0 to Files.Count - 1 do
  begin
    if pos('=', Files[i]) <> 0 then
    begin
      if TryStrToInt(Copy(Files[i], pos('=', Files[i]) + 1, length(Files[i])), FileSizeInt) then
        Files.Objects[i] := TObject(FileSizeInt)
      else
        Files.Objects[i] := nil;

      Files[i] := copy(Files[i], 1, pos('=', Files[i]) - 1);
    end;
  end;
end;

procedure TPTLibBaseBackupRestore.SetBackupFiles(const Value: TStringList);
begin
  FBackupFiles.Assign(Value);
end;

procedure TPTLibBaseBackupRestore.CheckCanBackup;
begin
  if FBackupFiles.Count < 1 then
    raise EPTLibDatabaseError.Create(StrNoBackupFilesAssigned) else
  if FDatabaseName = '' then
    raise EPTLibDatabaseError.Create(StrNoDatabaseNameAssigned) else
  if (FServername = '') and (not IsLocal) then
    raise EPTLibDatabaseError.Create(StrNoServerNameAssiged);
end;

destructor TPTLibBaseBackupRestore.Destroy;
begin
  FBackupFiles.Free;

  inherited;
end;

constructor TPTLibFirebirdRestore.Create(Sender: TComponent);
begin
  inherited;

  FRestoredFiles := TStringList.Create;
  FWriteLogFile := FALSE;
  FRestoreOptions := [roReplace, roNoValidity];
end;

destructor TPTLibFirebirdRestore.Destroy;
begin
  FRestoredFiles.Free;

  inherited;
end;

procedure TPTLibFirebirdRestore.PerformRestore(LogFileName: String = '');
var
  j: integer;
  TempPath: String;
  TempFile: String;
  IBRestoreService: TFDIBRestore;
begin
  IBRestoreService := TFDIBRestore.Create(Self);
  try
    IBRestoreService.DriverLink := FDriverLink;
    try
      DoOnStatus(StrRestoreStarted);

      // Set the Restore Service Params
      if FIsLocal then
      begin
        IBRestoreService.Protocol := ipLocal;
        IBRestoreService.Host := '';
      end
      else
      begin
        IBRestoreService.Protocol := ipTCPIP;
        IBRestoreService.Host := format('%s/%d', [FServername, FPort]);
      end;

      IBRestoreService.UserName := FUsername;
      IBRestoreService.Password := FPassword;
      IBRestoreService.Database := FDatabaseName;
      IBRestoreService.Verbose := FVerbose;

      if FLibraryName <> '' then
        FDriverLink.VendorLib := FLibraryName;

      IBRestoreService.PageSize := FPageSize;
      IBRestoreService.Options := FRestoreOptions;

      FLogFilename := LogFilename;
      TempPath := TPath.GetTempPath;

      IBRestoreService.BackupFiles.Clear;

      if (FBackupFiles.Count >= 1) and
         ((ExtractFileExt(FBackupFiles[0]) = '.gbkz') or
          (ExtractFileExt(FBackupFiles[0]) = '.fbk7z') or
          (ExtractFileExt(FBackupFiles[0]) = '.zip')) then
      begin
        DoOnStatus(StrDecompressing);

        TFileUtils.DeCompressFilesFromZip(FBackupFiles[0], TempPath);
        //DecompressZipArchive(FBackupFiles[0], TempPath, '.fbk', IBRestoreService.BackupFiles);
      end
      else
      begin
        IBRestoreService.BackupFiles.Clear;

        for j := 0 to pred(FBackupFiles.Count) do
          if FBackupFiles[j] <> '' then
            IBRestoreService.BackupFiles.Add(FBackupFiles[j]);
      end;

      FRestoredFiles.Assign(IBRestoreService.BackupFiles);

      IBRestoreService.OnProgress := OnFDProgress;
      //IBRestoreService.OnError := OnFDError;
      IBRestoreService.Restore;
    except
      on e:Exception do
      begin
        DoOnStatus(format(StrErrorS, [e.Message]));

        raise;
      end;
    end;
  finally
    DoOnStatus(StrRestoreCompleted);

    DeleteFile(TempFile);

    FreeAndNil(IBRestoreService);
  end;
end;

end.


