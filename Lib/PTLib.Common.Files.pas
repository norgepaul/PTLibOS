{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Files                                       }
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

unit PTLib.Common.Files;

interface

uses
  System.SysUtils, System.Classes, System.Zip, System.ZLib, System.IOUtils,
  System.StrUtils, System.Generics.Defaults,

  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  WinAPI.ShellAPI,
  {$ENDIF}
  {$IFDEF OSX}
  Posix.Unistd,
  {$ENDIF}
  {$IFDEF LINUX64}
  Posix.Base,
  Posix.Fcntl,
  Posix.Unistd,
  Posix.Stdio,
  Posix.Stdlib,
  {$ENDIF}

  IdHashMessageDigest,

  PTLib.Common.Types,
  PTLib.Common.Dates,
  PTLib.Common.Classes,
  PTLib.Common.Strings,
  PTLib.Common.Interfaces;

type
  TExecuteOutputProc = reference to procedure(const Handle: Pointer; const LineNumber: Integer; const Value: String; out Done: Boolean);

  TFileDateSortCompare = class(TInterfacedObject, IComparer<IFileInfo>)
    function Compare(const Left, Right: IFileInfo): Integer;
  end;

  TFilenameSortCompare = class(TInterfacedObject, IComparer<IFileInfo>)
    function Compare(const Left, Right: IFileInfo): Integer;
  end;

  TZipFileASync = class(TThread)
  strict private
    FDirectory: String;
    FZipFilename: String;
    FError: String;
    FZipProgress: TOnZipProgress;
    FZipProgressFilename: String;
    FZipProgressPosition: Int64;
    FZipFinished: Boolean;
  private
    procedure OnZipProgress(Sender: TObject; FileName: String; Header: TZipHeader; Position: Int64);
    procedure ZipProgressSync;
  public
    constructor Create(const Directory, ZipFilename: String; const ZipProgress: TOnZipProgress);

    procedure Execute; override;

    property Error: String read FError;
  end;

  TTempFile = Class
  public
    class function GetTempFileName: String;
    class function GetTempDirectory: String; deprecated 'use TIOUtils.TPath.GetTempPath';
    class function GetUniqueTempFolder: String;
    class function GetTempStream(out aStream: TFileStream):Boolean;
  end;

  TFileUtils = class
  strict private
    class function CombineCommandAndParams(const Command, Parameters: String): String;
  public
    class function ExtractFileNameNoExt(FileName: String):String;
    class function RemoveFileExt(FileName: String):String;

    class function MakeGUIDFileName(const Ext: string = ''):String;
    class function MakeGUIDTempFileName(const Ext: string = ''):String;

    class procedure CompressFilesToZip(Filenames: TStrings; ArchiveFileName: String; const Comment: String='');
    class procedure DeCompressFilesFromZip(ArchiveFileName: String; TargetPath: String);
    class procedure CompressFileToZip(const Filename: String; const ArchiveFileName: String);
    class procedure CopyFile(const Source, Destination: String);

    class function GetCmdParamValue(const ParamName: String): String;

    class function  GetArchiveComment(const ArchiveFileName: String):String;

    class function  FileSizeFormatStr(const FileSize: Int64): String;
    class function CleanFileName(const InputString: string): string;

    class function GetUniqueFilename(const Filename: String): String;
    class procedure DeleteFolderRecursive(const Path:String);
    class function DeleteFile(const Filename: String): Boolean;
    class procedure CheckValidFilename(const Filename: String; const MaximumLength: Integer = 0);
    class function GetFileHash(const Filename: String): String;
    class function GetAppFilePath: String;
    class function GetAppFilename: String;
    class function ExtractFilepathNoExt(const Filename: String): String;
    class function IsValidFilename(const Filename: String): Boolean;
    class function GetGUIDTempFileName: String;
    class function GetTempPath: String;
    class function SanitizeFilename(const Filename: String): String;
    class procedure ZipDirectoryASync(const Directory, ZipFilename: String; const ZipProgress: TOnZipProgress = nil);
    class function FindFileInPaths(var Filename: String; const Paths: TArray<String>): Boolean;
    class procedure ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const Output: TStrings); overload;
    class procedure ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; out Output: IList<String>); overload;
    class procedure ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; out Output: String); overload;
    class procedure ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const OutputProc: TExecuteOutputProc); overload;
    class function ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String): Pointer; overload;

    class function GetDirectoryString(const Dir: String): String;
    class function ScanFiles(const Path, Filter: String; const ScanFileOptions: TScanFileOptions): IList<IFileInfo>;
    class function RenameFile(const OldName, NewName: String): Boolean;
    class function FixPathDelimiters(const Value: String): String;
    class function Tail(const Filename: String; const LineCount: Integer; const MaxLineLength: Integer = 0): IList<String>;

    class function DiskSize(const Folder: String): Int64; overload;
    class function DiskFree(const Folder: String): Int64; overload;
    class function GetRemovableDrives: IList<String>;

    {$IFDEF MSWINDOWS}
    class function DriveNumberFromDriveLetter(const Drive: Char): Byte;
    class function DiskSize(const DiskID: Byte): Int64; overload;
    class function DiskFree(const DiskID: Byte): Int64; overload;
    class function ShellExec(const Filename: String; const Parameters: String = ''; const ShowState: Integer = SW_SHOWNORMAL; const WindowHandle: HWND = 0): NativeUInt;
    {$ENDIF}
  end;

  TProcessExec = class;

  TProcessExecCallback = reference to procedure(const Response, Error: String);

  TProcessExecThread = class(TThread)
  strict private
    FFilename: String;
    FParameters: String;
    FProcessID: Cardinal;
    FResponse: String;
    FError: String;
    FWorkingDir: String;
  public
    constructor Create(const Filename: String; const Parameters: String; const WorkingDir: String);

    procedure Execute; override;

    property Response: String read FResponse;
    property Error: String read FError;
    property ProcessID: Cardinal read FProcessID;
  end;

  TProcessExec = class
  strict private
    FProcessExecThread: TProcessExecThread;
    FProcessExecCallback: TProcessExecCallback;

    procedure OnThreadTerminate(Sender: TObject);
  private
    procedure DestroyProcessExecThread;
  public
    destructor Destroy; override;

    procedure ExecProcess(const Filename, Parameters, WorkingDir: String; const Callback: TProcessExecCallback);
  end;

implementation

uses
  System.Math;

resourcestring
  StrFilenamesInvalidCharacters = 'Sorry, the following characters are not allowed: %s';
  StrTheMaximumFilenameLengths = 'The maximum directory and filename lengths are %d characters';

// -----------------------------------------------------------------------------
// -- Linux Implementations
// -----------------------------------------------------------------------------

{$IFDEF LINUX64}
type
  TStreamHandle = pointer;

  function popen(const command: MarshaledAString; const _type: MarshaledAString): TStreamHandle; cdecl; external libc name _PU + 'popen';
  function pclose(filehandle: TStreamHandle): int32; cdecl; external libc name _PU + 'pclose';
  function fgets(buffer: pointer; size: int32; Stream: TStreamHAndle): pointer; cdecl; external libc name _PU + 'fgets';

class function TFileUtils.GetRemovableDrives: IList<String>;
begin
  Result := TList<String>.Create;

  { TODO : Implement }
end;

class procedure TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const Output: TStrings);
var
  Handle: TStreamHandle;
  Data: array[0..511] of uint8;
  M : TMarshaller;
begin
  try
    Handle := popen(M.AsAnsi(PWideChar(CombineCommandAndParams(Command, Parameters) + ' 2>&1')).ToPointer, 'r');
    try
      while fgets(@data[0],Sizeof(Data),Handle) <> nil do
      begin
        if Output <> nil then
        begin
          Output.Add(Copy(UTF8ToString(@Data[0]),1,UTF8ToString(@Data[0]).Length -1));
        end;
      end;
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
      Output.Add(E.ClassName + ': ' + E.Message);
  end;
end;

class procedure TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const OutputProc: TExecuteOutputProc);
var
  Handle: TStreamHandle;
  Data: array[0..511] of uint8;
  M : TMarshaller;
  Done: Boolean;
  LineCounter: Integer;
begin
  LineCounter := 0;
  Handle := nil;
  try
    Handle := popen(M.AsAnsi(PWideChar(CombineCommandAndParams(Command, Parameters) + ' 2>&1')).ToPointer,'r');
    try
      Done := False;

      while (fgets(@data[0],Sizeof(Data), Handle) <> nil) and
            (not Done) do
      begin
        if Assigned(OutputProc) then
        begin
          OutputProc(
            Handle,
            LineCounter,
            Copy(UTF8ToString(@Data[0]),1,UTF8ToString(@Data[0]).Length -1),
            Done);
        end;

        Inc(LineCounter);
      end;
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
      OutputProc(Handle, LineCounter, E.ClassName + ': ' + E.Message, Done);
  end;
end;

class function TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String): Pointer;
var
  M : TMarshaller;
begin
  //_system(M.AsAnsi(PWideChar(CombineCommandAndParams(Command, Parameters))).ToPointer);
  Result := popen(M.AsAnsi(PWideChar(CombineCommandAndParams(Command, Parameters))).ToPointer, 'r');
  //pClose(Handle);
end;

class function TFileUtils.DiskSize(const Folder: String): Int64;
var
  AResult: Int64;
begin
  AResult := 0;

  ExecuteCommand(
    'df',
    Folder,
    '',
    procedure(const Handle: Pointer; const LineNumber: Integer; const Value: String; out Done: Boolean)
    var
      Text: String;
    begin
      if LineNumber = 1 then
      begin
        Text := Value;

        NextBlock(Text, ' ');

        AResult := StrToInt64Def(NextBlock(Text, ' '), 0) * 1024;

        Done := True;
      end;
    end
  );

  Result := AResult;
end;

class function TFileUtils.DiskFree(const Folder: String): Int64;
var
  AResult: Int64;
begin
  AResult := 0;

  ExecuteCommand(
    'df',
    Folder,
    '',
    procedure(const Handle: Pointer; const LineNumber: Integer; const Value: String; out Done: Boolean)
    var
      Text: String;
    begin
      if LineNumber = 1 then
      begin
        Text := Value;

        NextBlock(Text, ' ');
        NextBlock(Text, ' ');
        NextBlock(Text, ' ');

        AResult := StrToInt64Def(NextBlock(Text, ' '), 0) * 1024;

        Done := True;
      end;
    end
  );

  Result := AResult;
end;

{$ENDIF}

// -----------------------------------------------------------------------------
// -- Windows Implementations
// -----------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

class function TFileUtils.ShellExec(const Filename: String; const Parameters: String;
  const ShowState: Integer; const WindowHandle: HWND): NativeUInt;
begin
  Result := ShellExecute(
    WindowHandle,
    'open',
    PChar(Filename),
    PChar(Parameters),
    nil,
    ShowState);
end;

class function TFileUtils.GetRemovableDrives: IList<String>;
type
  STORAGE_QUERY_TYPE = (PropertyStandardQuery = 0, PropertyExistsQuery, PropertyMaskQuery, PropertyQueryMaxDefined);
  TStorageQueryType = STORAGE_QUERY_TYPE;

  STORAGE_PROPERTY_ID = (StorageDeviceProperty = 0, StorageAdapterProperty);
  TStoragePropertyID = STORAGE_PROPERTY_ID;

  STORAGE_PROPERTY_QUERY = packed record
    PropertyId: STORAGE_PROPERTY_ID;
    QueryType: STORAGE_QUERY_TYPE;
    AdditionalParameters: array [0..9] of AnsiChar;
  end;
  TStoragePropertyQuery = STORAGE_PROPERTY_QUERY;

  STORAGE_BUS_TYPE = (BusTypeUnknown = 0, BusTypeScsi, BusTypeAtapi, BusTypeAta, BusType1394, BusTypeSsa, BusTypeFibre,
    BusTypeUsb, BusTypeRAID, BusTypeiScsi, BusTypeSas, BusTypeSata, BusTypeMaxReserved = $7F);
  TStorageBusType = STORAGE_BUS_TYPE;

  STORAGE_DEVICE_DESCRIPTOR = packed record
    Version: DWORD;
    Size: DWORD;
    DeviceType: Byte;
    DeviceTypeModifier: Byte;
    RemovableMedia: Boolean;
    CommandQueueing: Boolean;
    VendorIdOffset: DWORD;
    ProductIdOffset: DWORD;
    ProductRevisionOffset: DWORD;
    SerialNumberOffset: DWORD;
    BusType: STORAGE_BUS_TYPE;
    RawPropertiesLength: DWORD;
    RawDeviceProperties: array [0..0] of AnsiChar;
  end;
  TStorageDeviceDescriptor = STORAGE_DEVICE_DESCRIPTOR;

  function GetBusType(const Drive: Char): TStorageBusType;
  var
    H: THandle;
    Query: TStoragePropertyQuery;
    dwBytesReturned: DWORD;
    Buffer: array [0..1023] of Byte;
    sdd: TStorageDeviceDescriptor absolute Buffer;
    OldMode: UINT;
  begin
    Result := BusTypeUnknown;

    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      H := CreateFile(PChar(Format('\\.\%s:', [AnsiLowerCase(Drive)])), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
        OPEN_EXISTING, 0, 0);
      if H <> INVALID_HANDLE_VALUE then
      begin
        try
          dwBytesReturned := 0;
          FillChar(Query, SizeOf(Query), 0);
          FillChar(Buffer, SizeOf(Buffer), 0);
          sdd.Size := SizeOf(Buffer);
          Query.PropertyId := StorageDeviceProperty;
          Query.QueryType := PropertyStandardQuery;
          if DeviceIoControl(H, IOCTL_STORAGE_QUERY_PROPERTY, @Query, SizeOf(Query), @Buffer, SizeOf(Buffer), dwBytesReturned, nil) then
            Result := sdd.BusType;
        finally
          CloseHandle(H);
        end;
      end;
    finally
      SetErrorMode(OldMode);
    end;
  end;

var
  DriveBits: set of 0..25;
  i: Integer;
  Drive: Char;
begin
  Result := TList<String>.Create;

  Cardinal(DriveBits) := GetLogicalDrives;

  for i := 0 to 25 do
  begin
    if i in DriveBits then
    begin
      Drive := Chr(Ord('a') + i);
      if GetBusType(Drive) = BusTypeUsb then
        Result.Add(Drive);
    end;
  end;
end;

class function TFileUtils.DriveNumberFromDriveLetter(const Drive: Char): Byte;
begin
  Result := 1 + Ord(Drive) - Ord('A');
end;

class function TFileUtils.DiskFree(const DiskID: Byte): Int64;
begin
  Result := System.SysUtils.DiskFree(DiskID);
end;

class function TFileUtils.DiskFree(const Folder: String): Int64;
begin
  Result := DiskFree(DriveNumberFromDriveLetter(ExtractFileDrive(Folder)[low(String)]));
end;

class function TFileUtils.DiskSize(const DiskID: Byte): Int64;
begin
  Result := System.SysUtils.DiskSize(DiskID);
end;

class function TFileUtils.DiskSize(const Folder: String): Int64;
begin
  Result := DiskSize(DriveNumberFromDriveLetter(ExtractFileDrive(Folder)[low(String)]));
end;

class procedure TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const Output: TStrings);
begin
  ExecuteCommand(
    Command,
    Parameters,
    WorkingDir,
    procedure(const ProcessId: Pointer; const LineNumber: Integer; const Value: String; out Done: Boolean)
    begin
      Output.Add(Value);
    end
  );
end;

class procedure TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const OutputProc: TExecuteOutputProc);
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
  LineCounter: Integer;
  Done: Boolean;
begin
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

    if WorkingDir = '' then
    begin
      WorkDir := GetCurrentDir;
    end
    else
    begin
      WorkDir := WorkingDir;
    end;

    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CombineCommandAndParams(Command, Parameters)),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if not Handle then
    begin
      OutputProc(Pointer(PI.hProcess), 0, 'Error: ' + SysErrorMessage(GetLastError), Done);
    end
    else
      try
        LineCounter := 0;
        Done := False;

        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;

            OutputProc(Pointer(PI.hProcess), LineCounter, String(Buffer), Done);
            //Result := Result + Buffer;

            if Done then
            begin
              Break;
            end;

            Inc(LineCounter);
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
(*
class procedure TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; const OutputProc: TExecuteOutputProc);
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  Handle: Boolean;
  Done: Boolean;
  LineCounter: Integer;
  Cmd: String;
begin
  with SA do
  begin
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

    Cmd := 'cmd.exe /C ' + CombineCommandAndParams(Command, Parameters);

    Handle := CreateProcess(
      nil,
      PChar(cmd),
      nil,
      nil,
      True,
      0,
      nil,
      PChar(WorkingDir),
      SI,
      PI);

    CloseHandle(StdOutPipeWrite);

    if Handle then
    try
      Done := False;
      LineCounter := 0;

      repeat
        WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
        if BytesRead > 0 then
        begin
          Buffer[BytesRead] := #0;

          OutputProc(Pointer(PI.hProcess), LineCounter, String(Buffer), Done);

          if Done then
          begin
            Break;
          end;

          Inc(LineCounter);
        end;
      until not WasOK or (BytesRead = 0);

      WaitForSingleObject(PI.hProcess, INFINITE);
    finally
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;
        *)
class function TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String): Pointer;
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  FillChar(StartInfo, SizeOf(TStartupInfo), #0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  StartInfo.cb := SizeOf(TStartupInfo);

  CreateProcess(
    nil,
    PChar(CombineCommandAndParams(Command, Parameters)),
    nil,
    nil,
    False,
    CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS + CREATE_NO_WINDOW,
    nil,
    nil,
    StartInfo,
    ProcInfo);

  result := Pointer(ProcInfo.hProcess);
end;
{$ENDIF}


// -----------------------------------------------------------------------------
// -- Common Implementations
// -----------------------------------------------------------------------------

class function TFileUtils.CombineCommandAndParams(const Command, Parameters: String): String;
begin
  Result := Command;

  if Parameters <> '' then
  begin
    Result := Result + ' ' + Parameters;
  end;
end;

class function TFileUtils.GetDirectoryString(const Dir: String): String;
var
  Path, FileLine: String;
  Files: IList<IFileInfo>;
  i: Integer;
begin
  Path := IncludeTrailingPathDelimiter(Dir);

  Files := ScanFiles(Path, '*', [TScanFileOption.IncludeFiles, TScanFileOption.IncludeDirectories]);

  Result := '';

  for i := 0 to pred(Files.Count) do
  begin
    // Date
    FileLine := DateToCommonDateStr(Files[i].CreatedDate);

    // Time
    AddToken(FileLine, TimeToCommonTimeStr(Files[i].CreatedDate, False), #09);

    if Files[i].IsDir then
    begin
      AddToken(FileLine, '<DIR>', #09);
      AddToken(FileLine, #09#09, #09);
    end
    else
    begin
      AddToken(FileLine, '     ', #09);
      AddToken(FileLine, TFileUtils.FileSizeFormatStr(Files[i].Filesize), #09);
    end;

    AddToken(FileLine, Files[i].Filename, #09);

    AddLine(Result, FileLine);
  end;
end;

class function TFileUtils.ScanFiles(const Path, Filter: String; const ScanFileOptions: TScanFileOptions): IList<IFileInfo>;

  function IsDirectory(const SearchRec: TSearchRec): Boolean;
  begin
    Result :=
      (SearchRec.Attr and System.SysUtils.faDirectory <> 0) and
      (SearchRec.Name <> '.') and
      (SearchRec.Name <> '..');
  end;

  procedure AddToResult(const SearchRec: TSearchRec; const Path: String);
  var
    FileInfo: IFileInfo;
    IsDir: Boolean;
  begin
    IsDir := IsDirectory(SearchRec);

    if ((not IsDir) and
        (TScanFileOption.IncludeFiles in ScanFileOptions)) or
       ((IsDir) and
        (TScanFileOption.IncludeDirectories in ScanFileOptions)) then
    begin
      FileInfo := TFileInfo.Create;

      Result.Add(FileInfo);

      FileInfo.IsDir := IsDir;

      if TScanFileOption.IncludePaths in ScanFileOptions then
        FileInfo.Filename := concat(IncludeTrailingPathDelimiter(Path), SearchRec.Name)
      else
        FileInfo.Filename := SearchRec.Name;

      FileInfo.Filesize := SearchRec.Size;
      FileInfo.CreatedDate := SearchRec.TimeStamp;
      FileInfo.ModifiedDate := SearchRec.TimeStamp;
    end;
  end;

  procedure ScanFilesRec(const Path, Filter: String);
  var
    hFindFile : Integer;
    SearchRec: TSearchRec;
  begin
    hFindFile := System.SysUtils.FindFirst(IncludeTrailingPathDelimiter(Path) + Filter, faAnyFile or faDirectory, SearchRec);
    try
      If hFindFile = 0 then
      begin
        repeat
          if (SearchRec.Name <> '.') and
             (SearchRec.Name <> '..') then
          begin
            AddToResult(SearchRec, Path);

            if (IsDirectory(SearchRec)) and
               (TScanFileOption.RecursiveDirectories in ScanFileOptions) then
            begin
              ScanFilesRec(
                IncludeTrailingPathDelimiter(concat(IncludeTrailingPathDelimiter(Path), SearchRec.Name)),
                Filter);
            end;
          end;
        until System.SysUtils.FindNext(SearchRec) <> 0;
      end;
    finally
      System.SysUtils.FindClose(SearchRec);
    end;
  end;

var
  FullFilter: String;
begin
  FullFilter := Filter;

  Result := TList<IFileInfo>.Create;

  while FullFilter <> '' do
    ScanFilesRec(Path, NextBlock(FullFilter, ';'));
end;

class function TFileUtils.Tail(const Filename: String; const LineCount: Integer; const MaxLineLength: Integer): IList<String>;
{$IFDEF LINUX64}
var
  Output: IList<String>;
  Command: String;
begin
  Command := 'tail -n' + LineCount.ToString + ' ' + Filename;

  if MaxLineLength > 0 then
  begin
    Command := Command + '| cut -b 1-' + MaxLineLength.ToString;
  end;

  ExecuteCommand(
    Command,
    '',
    '',
    Output);

  Result := Output;
end;
{$ELSE}
const
  ChunkSize = 200;

  function DecOffset(var Offset: Int64; out Count: Int64): Boolean;
  begin
    Result := Offset > 0;

    if Result then
    begin
      if Offset - ChunkSize >= 0 then
      begin
        Offset := Offset - ChunkSize;
        Count := ChunkSize;
      end
      else
      begin
        Offset := 0;
        Count := Offset;
      end;
    end;
  end;

var
  FileStream: TFileStream;
  StringStream: TStringStream;
  Offset, Count: Int64;
  CurrentLine, NextLine, Chunk: String;
begin
  Result := TList<String>.Create;

  FileStream := TFileStream.Create(Filename, fmOpenRead);
  try
    Offset := FileStream.Size;
    CurrentLine := '';

    while
      (DecOffset(Offset, Count)) and
      (Result.Count < LineCount) do
    begin
      StringStream := TStringStream.Create;
      try
        FileStream.Position := Offset;
        StringStream.CopyFrom(FileStream, Count);

        Chunk := Trim(StringStream.DataString);

        while
          (Chunk <> '') and
          (Result.Count < LineCount) do
        begin
          NextLine := Trim(LastBlock(Chunk, #10));

          Chunk := Trim(Chunk);

          if CurrentLine = '' then
          begin
            CurrentLine := NextLine;
          end
          else
          begin
            CurrentLine := NextLine + CurrentLine;
          end;

          if Chunk <> '' then
          begin
            if MaxLineLength > 0 then
            begin
              CurrentLine := copy(CurrentLine, 1, MaxLineLength);
            end;

            Result.Insert(0, CurrentLine);

            CurrentLine := '';
          end;
        end;
      finally
        FreeAndNil(StringStream);
      end;
    end;

    // Add any remaining text
    if CurrentLine <> '' then
    begin
      if MaxLineLength > 0 then
      begin
        CurrentLine := copy(CurrentLine, 1, MaxLineLength);
      end;

      Result.Insert(0, CurrentLine);
    end;
  finally
    FreeAndNil(FileStream);
  end;
end;
{$ENDIF}

class function TFileUtils.DeleteFile(const Filename: String): Boolean;
begin
  Result := System.SysUtils.DeleteFile(Filename);
end;

class procedure TFileUtils.DeleteFolderRecursive(const Path: String);
var
  F: TSearchRec;
  Temp: String;
begin
  (* Note: WinAPI "deletefile" marks a file for deletion.
     This means that if there is a handle to a file open, the file wont
     be deleted by the system until the handle is void.
     In case of temporary folders this means that it's safe to perform
     a recursive operation without thinking to much about the outcome.

     Reference:
     https://msdn.microsoft.com/en-us/library/windows/desktop/aa363915(v=vs.85).aspx *)


  (* Second note: DeleteFile etc. does not send data to the recycle-bin.
     For that we need to use the SHFileOperation. Pay attention to the double-null
     bytes for the path! (unicode). And no trailingPathDelimiter!

    int silently_remove_directory(LPCTSTR dir)
    (
      int len = strlen(dir) + 2;
      char* tempdir = (char* ) malloc(len);
      memset(tempdir,0,len);
      strcpy(tempdir,dir);

      SHFILEOPSTRUCT file_op = [
        NULL,
        FO_DELETE,
        tempdir,
        "",
        FOF_NOCONFIRMATION |
        FOF_NOERRORUI |
        FOF_SILENT,
        false,
        0,
        "" ];
      int ret = SHFileOperation(&file_op);
      free(tempdir);
      return ret; // S_OK on success!
    )
  *)

  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, F) = 0 then
  begin
    try
      repeat
        Temp := IncludeTrailingPathDelimiter(Path) + F.Name;

        if (F.Attr and faDirectory <> 0) then
        begin
          if (F.Name <> '.') and (F.Name <> '..') then
          begin
            DeleteFolderRecursive(Temp);
          end;
        end else
        begin
          DeleteFile(PWideChar(Temp));
        end;
      until FindNext(F) <> 0;
    finally
      System.SysUtils.FindClose(F);
    end;

    RemoveDir(Path);
  end;
end;

class function TFileUtils.FileSizeFormatStr(const FileSize: Int64): string;
const
  cBinaryPrefixes: array[0..8] of String = ('', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB');
var
  LogValue: Integer;
begin
  if FileSize = 0 then
  begin
    Result := '0 KB';
  end
  else
  begin
    LogValue := Floor(LogN(1024, FileSize));
    Result := FloatToStrF(FileSize / Power(1024, LogValue), ffFixed, 12, 2) + ' ' + cBinaryPrefixes[LogValue];
  end;
end;

class procedure TFileUtils.CompressFileToZip(const Filename: String; const ArchiveFileName: String);
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Add(Filename);

    CompressFilesToZip(Files, ArchiveFileName);
  finally
    FreeAndNil(Files);
  end;
end;

class procedure TFileUtils.CopyFile(const Source, Destination: String);
var
  Src, Dst: TFileStream;
begin
  Src := TFileStream.Create(Source, fmOpenRead);
  try
    Dst := TFileStream.Create(Destination, fmCreate);
    try
      Src.Position := 0;

      Dst.CopyFrom(Src, Src.Size);
    finally
      FreeAndNil(Dst);
    end;
  finally
    FreeAndNil(Src);
  end;
end;

class function TFileUtils.GetAppFilename: String;
begin
  Result := ParamStr(0);
end;

class function TFileUtils.GetAppFilePath: String;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDirectoryName(GetAppFilename));
end;

class function TFileUtils.ExtractFilepathNoExt(const Filename: String): String;
var
  ExtLen: Integer;
begin
  ExtLen := Length(ExtractFileExt(Filename));
  Result := Filename;

  if ExtLen > 0 then
    Result := Copy(Result, 1, length(Result) - ExtLen);
end;

class function TFileUtils.GetArchiveComment(const ArchiveFileName: String): String;
var
  Zip: TZipFile;
begin
  SetLength(Result, 0);

  Zip := TZipFile.Create;
  try
    try
      Zip.Open(ArchiveFileName, TZipMode.zmRead);
    except
      on Exception do
      begin
        Exit;
      end;
    end;

    try
      Result := Zip.Comment;
    finally
      Zip.Close;
    end;
  finally
    FreeAndNil(Zip);
  end;
end;

class function TFileUtils.GetFileHash(const Filename: String): String;
var
  MD5: TIdHashMessageDigest5;
  FileStream: TFileStream;
begin
  if FileExists(Filename) then
  begin
    MD5 := TIdHashMessageDigest5.Create;
    FileStream := TFileStream.Create(FileName, fmOpenRead OR fmShareDenyWrite);
    try
      Result := MD5.HashStreamAsHex(FileStream);
    finally
      FreeAndNil(MD5);
      FreeAndNil(FileStream);
    end;
  end
  else
  begin
    Result := '';
  end;
end;

class function TFileUtils.GetGUIDTempFileName: String;
begin
  Result := concat(GetTempPath, CreateGUIDString);
end;

class function TFileUtils.GetUniqueFilename(const Filename: String): String;

  function InsertFileCount(Filename: String; Count: Integer): String;
  begin
    Result := concat(IncludeTrailingPathDelimiter(ExtractFilePath(Filename)),
                     TFileUtils.ExtractFilenameNoExt(Filename),
                     ' (',
                     IntToStr(Count),
                     ')',
                     ExtractFileExt(Filename));
  end;

var
  Count: Integer;
begin
  Count := 1;

  if FileExists(Filename) then
  begin
    While FileExists(InsertFileCount(Filename, Count)) do
      Inc(Count);

    Result := InsertFileCount(Filename, Count);
  end
  else
    Result := Filename;
end;

class function TFileUtils.FindFileInPaths(var Filename: String; const Paths: TArray<String>): Boolean;
var
  i: Integer;
  FilenameAndPath: String;
begin
  for i := Low(Paths) to High(Paths) do
  begin
    FilenameAndPath := concat(Paths[i], Filename);

    if FileExists(FilenameAndPath) then
    begin
      Filename := FilenameAndPath;

      Exit(True);
    end;
  end;

  Result := False;
  FilenameAndPath := '';
end;

class function TFileUtils.FixPathDelimiters(const Value: String): String;
begin
  {$IFDEF MSWINDOWS}
    Result := StringReplace(Value, '/', '\', [rfReplaceAll, rfIgnoreCase]);
  {$ELSE}
    Result := StringReplace(Value, '\', '/', [rfReplaceAll, rfIgnoreCase]);
  {$ENDIF}
end;

class function TFileUtils.GetTempPath: String;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

class function TFileUtils.IsValidFilename(const Filename: String): Boolean;
const
  InvalidFileCharArray = '/\:*?"<>|]';
var
  i: Integer;
begin
  Result := Length(Filename) > 0;

  if Result then
  begin
    for i := 1 to length(Filename) do
      if pos(Filename[i], InvalidFileCharArray) > 0 then
      begin
        Result := False;

        Break;
      end;
  end;
end;

class procedure TFileUtils.CheckValidFilename(const Filename: String; const MaximumLength: Integer);
const
  InvalidFileChars = '/ \ : * ? " < > |';
begin
  if not IsValidFilename(Filename) then
    raise Exception.Create(format(StrFilenamesInvalidCharacters, [InvalidFileChars]));

  if (MaximumLength > 0) and (length(Filename) > MaximumLength) then
    raise Exception.Create(format(StrTheMaximumFilenameLengths, [MaximumLength]));
end;

class function TFileUtils.SanitizeFilename(const Filename: String): String;
const
  InvalidFileChars = ['/', '\', ':', '*', '?', '"', '<', '>', '|'];
var
  i: Integer;
begin
  for i := Low(Filename) to High(Filename) do
  begin
    if CharInSet(Filename[i], InvalidFileChars) then
    begin
      Result := Result + '_';
    end
    else
    begin
      Result := Result + Filename[i];
    end;
  end;
end;

class procedure TFileUtils.ZipDirectoryASync(const Directory, ZipFilename: String; const ZipProgress: TOnZipProgress);
begin
  TZipFileASync.Create(Directory, ZipFilename, ZipProgress);
end;

class procedure TFileUtils.CompressFilesToZip(Filenames: TStrings; ArchiveFileName: String; const Comment: String='');
var
  Zip:  TZipFile;
  x:    Integer;
Begin
  if Filenames <> nil then
  begin
    if FileExists(ArchiveFileName) then
    begin
      TFile.Delete(ArchiveFileName);
    end;

    Zip := TZipFile.Create;
    try
      Zip.Open(ArchiveFileName,TZipMode.zmWrite);
      try
        for x := 1 to Filenames.Count do
        Begin
          Zip.Add(Filenames[x-1], ExtractFileName(Filenames[x-1]), zcDeflate);
        End;

      finally
        Zip.Comment := Comment;
        Zip.Close;
      end;

    finally
      FreeAndNil(Zip);
    end;
  end
  else
  begin
    raise Exception.Create('Failed to create Zip archive, filenames was NIL error');
  end;
end;

class Procedure TFileUtils.DeCompressFilesFromZip(ArchiveFileName: String; TargetPath: String);
var
  Zip: TZipFile;
  x: Integer;
  Target: String;
begin

  if TFile.Exists(ArchiveFileName,true) then
  begin

    // Make sure target-path exists or can be forced
    if not TDirectory.Exists(TargetPath) then
    begin
      if not ForceDirectories(TargetPath) then
      Raise Exception.Create('Failed to deflate Zip archive, invalid target path error');
    end;

    Zip:=TZipFile.Create;
    try

      Zip.Open(ArchiveFileName, TZipMode.zmRead);
      try

        Target:=IncludeTrailingPathDelimiter(TargetPath);
        for x:=1 to Zip.FileCount do
        begin
          Zip.Extract(Zip.FileName[x-1],Target,false);
        end;

      finally
        Zip.Close;
      end;

    finally
      zip.Free;
    end;

  end else
  raise Exception.Create('Failed to deflate Zip archive, archive does not exist');
end;

class function TFileUtils.MakeGUIDFileName(const Ext: string = ''):String;
var
  data: TGUID;
  text: String;
begin
  CreateGUID(data);
  text := copy(GUIDToString(Data),2,36);
  result := StringReplace(text,'-','',[rfReplaceAll]) + ext;
end;

class function TFileUtils.MakeGUIDTempFileName(const Ext: string = ''):String;
begin
  result := IncludeTrailingPathDelimiter(TPath.GetTempPath) + MakeGUIDFileName(Ext);
end;

class function TFileUtils.RemoveFileExt(FileName: String):String;
begin
  result := ExtractFileNameNoExt(ExtractFileName(Filename));
end;

class function TFileUtils.RenameFile(const OldName, NewName: String): Boolean;
begin
  Result := System.SysUtils.RenameFile(OldName, NewName);
end;

class function TFileUtils.ExtractFileNameNoExt(FileName: String):String;
var
  x:  Integer;
begin
  setLength(result,0);

  // Part of a full path? get the filename only
  if pos(System.SysUtils.PathDelim, FileName) > 0 then
  Begin
    Filename := ExtractFileName(Filename);

    if pos('.', Filename) <> 0 then
    begin
      for x := length(Filename) downto 1 do
      begin
        if Filename[x] = '.' then
        begin
          Result := Copy(Filename,1,x-1);

          Break;
        end;
      end;
    end
    else
    begin
      Result := Filename;
    end;
  end
  else
  begin
    Result := ChangeFileExt(ExtractFileName(FileName),'');
  end;
end;

{ TTempFile --- Note: Verbatim copy from Jon Lennart's library, hence formatting}

class function TTempFile.GetTempStream(out aStream: TFileStream): Boolean;
var
  mName:  String;
Begin
  aStream:=NIL;

  mName:=getTempFileName;
  try
    aStream := TFileStream.Create(mName,fmCreate);
  except
    on e: exception do
    Raise Exception.CreateFmt(
      'Failed to create temporary file [%s]' + #13
    + 'System error: "%s"',[mName,e.Message]);
  end;

  result:=True;
end;

class function TTempFile.GetUniqueTempFolder: String;
begin
  result := IncludeTrailingPathDelimiter(TPath.GetTempPath) + TFileUtils.MakeGUIDFileName;
end;

class function TTempFile.GetTempDirectory: String;
begin
  Result := TPath.GetTempPath;
end;

class Function TTempFile.GetTempFileName: String;
{$IFDEF MSWINDOWS}
var
  mtempPath: String;
  mtempFile: String;
  mBufLen:  Integer;
  mLen:     Integer;
  mMoniker: String;
{$ENDIF}
Begin
  {$IFDEF MSWINDOWS}
  (* Init result *)
  setlength(result,0);

  (* Allocate normal size path buffer *)
  SetLength(mtempPath, MAX_PATH);
  mBufLen:=MAX_PATH;

  (* obtain temp path from WINAPI *)
  mLen:=WinApi.Windows.GetTempPath(mBufLen, PChar(@mtempPath[1]));

  if mLen>0 then
  Begin
    (* MSDN:  If the return value is greater than nBufferLength,
              the return value is the length, in TCHARs, of the
              buffer required to hold the path.
              http://msdn.microsoft.com/en-us/library/windows/desktop/aa364992%28v=vs.85%29.aspx *)
    If (mLen>mBufLen) then
    Begin
      SetLength(mtempPath,mLen);
      mLen:=WinApi.Windows.GetTempPath(mLen, PChar(@mtempPath[1]));
    end;

    (* setup prefix and memory for filename, which is not the
       same as the path we obtained above *)
    mMoniker:='temp_';
    SetLength(mtempFile, MAX_PATH + mLen);

    (* Obtain the filename, include the path.
       http://msdn.microsoft.com/en-us/library/windows/desktop/aa364991%28v=vs.85%29.aspx *)
    mLen:=WinApi.Windows.GetTempFileName(PChar(@mtempPath[1]),
    PChar(@mMoniker[1]), 0,
    PChar(@mtempFile[1]));

    (* Function returns 0 (zero) if it fails, check and return *)
    if mLen<>0 then
    Result:=trim(mtempFile) else
    Raise Exception.Create(SysErrorMessage(getLastError));
  end else
  Raise Exception.Create(SysErrorMessage(getLastError));
  {$ELSE}
    Assert(False, 'Windows Only!');
  {$ENDIF}
end;

class function TFileUtils.GetCmdParamValue(const ParamName: String): String;
begin
  if not FindCmdLineSwitch(ParamName, Result) then
  begin
    Result := '';
  end;
end;

class function TFileUtils.CleanFileName(const InputString: string): string;
var
  i: integer;
  ResultWithSpaces: string;
begin
  ResultWithSpaces := InputString;

  for i := 1 to Length(ResultWithSpaces) do
  begin
    // These chars are invalid in file names.
    case ResultWithSpaces[i] of
      '/', '\', ':', '*', '?', '"', '<', '>', '|', ' ', #$D, #$A, #9:
        begin
          // Use a * to indicate a duplicate space so we can remove
          // them at the end.
          if (i > 1) and
            ((ResultWithSpaces[i - 1] = ' ') or (ResultWithSpaces[i - 1] = '*')) then
            ResultWithSpaces[i] := '*'
          else
            ResultWithSpaces[i] := ' ';
        end;
    end;
  end;

  // A * indicates duplicate spaces.  Remove them.
  result := ReplaceStr(ResultWithSpaces, '*', '');

  // Also trim any leading or trailing spaces
  result := Trim(Result);

  if result = '' then
  begin
    raise(Exception.Create('Resulting FileName was empty Input string was: '
      + InputString));
  end;
end;

class procedure TFileUtils.ExecuteCommand(const Command: String; const Parameters: String; const WorkingDir: String; out Output: String);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    ExecuteCommand(
      Command,
      Parameters,
      WorkingDir,
      Strings);

    Output := Strings.Text;
  finally
    FreeAndNil(Strings);
  end;
end;

class procedure TFileUtils.ExecuteCommand(const Command, Parameters, WorkingDir: String; out Output: IList<String>);
var
  TempOutput: TStringList;
  i: Integer;
begin
  TempOutput := TStringList.Create;
  try
    ExecuteCommand(
      Command,
      Parameters,
      WorkingDir,
      TempOutput
    );

    Output := TList<String>.Create;

    for i := 0 to pred(TempOutput.Count) do
    begin
      Output.Add(TempOutput[i]);
    end;
  finally
    FreeAndNil(TempOutput);
  end;
end;

{ TZipFileASync }

constructor TZipFileASync.Create(const Directory, ZipFilename: String; const ZipProgress: TOnZipProgress);
begin
  FDirectory := Directory;
  FZipFilename := ZipFilename;
  FZipProgress := ZipProgress;

  inherited Create(False);
end;

procedure TZipFileASync.OnZipProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
begin
  FZipProgressFilename := Filename;
  FZipProgressPosition := Position;
  FZipFinished := False;

  Synchronize(ZipProgressSync);
end;

procedure TZipFileASync.ZipProgressSync;
begin
  if Assigned(FZipProgress) then
  begin
    FZipProgress(Self, FZipProgressFilename, FZipProgressPosition, FZipFinished);
  end;
end;

procedure TZipFileASync.Execute;
var
  Zip: TZipFile;
  TempFilename: String;
Begin
  try
    TempFilename := TFileUtils.GetGUIDTempFileName + '.zip.temp';

    try
      Zip := TZipFile.Create;
      try
        Zip.ZipDirectoryContents(TempFilename, FDirectory, zcDeflate, OnZipProgress);
      finally
        FreeAndNil(Zip);
      end;

      if FileExists(FZipFilename) then
        TFile.Delete(FZipFilename);

      RenameFile(TempFilename, FZipFilename);
    finally
      TFileUtils.DeleteFile(TempFilename);

      FZipProgressFilename := '';
      FZipProgressPosition := 0;
      FZipFinished := True;

      ZipProgressSync;
    end;
  except
    on e: Exception do
    begin
      FError := e.Message;
    end;
  end;
end;

{ TFileDateSortCompare }

function TFileDateSortCompare.Compare(const Left, Right: IFileInfo): Integer;
begin
  if Left.ModifiedDate < Right.ModifiedDate then
  begin
    Result := -1;
  end else
  if Left.ModifiedDate > Right.ModifiedDate then
  begin
    Result := 1;
  end
  else
  begin
    Result := 0;
  end;
end;

{ TProcessExecThread }

constructor TProcessExecThread.Create(const Filename: String; const Parameters: String; const WorkingDir: String);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FFilename := Filename;
  FParameters := Parameters;
  FWorkingDir := WorkingDir;
end;

procedure TProcessExecThread.Execute;
var
  Results: TStringList;
  CommandLine: String;
begin
  try
    Results := TStringList.Create;
    try
      CommandLine := FFilename + ' ' + FParameters;

      TFileUtils.ExecuteCommand(
        CommandLine,
        '',
        FWorkingDir,
        Results);

      FResponse := Results.Text;
    finally
      FreeAndNil(Results);
    end;
  except
    on e: Exception do
    begin
      FError := e.Message;
    end;
  end;
end;


{ TProcessExec }

destructor TProcessExec.Destroy;
begin
  DestroyProcessExecThread;

  inherited;
end;

procedure TProcessExec.DestroyProcessExecThread;
begin
  if Assigned(FProcessExecThread) then
  begin
    FProcessExecThread.OnTerminate := nil;
    FProcessExecThread.Terminate;
    FProcessExecThread := nil;
  end;
end;

procedure TProcessExec.ExecProcess(const Filename, Parameters, WorkingDir: String; const Callback: TProcessExecCallback);
begin
  DestroyProcessExecThread;

  FProcessExecCallback := Callback;
  FProcessExecThread := TProcessExecThread.Create(Filename, Parameters, WorkingDir);
  FProcessExecThread.OnTerminate := OnThreadTerminate;
  FProcessExecThread.Start;
end;

procedure TProcessExec.OnThreadTerminate(Sender: TObject);
begin
  if Assigned(FProcessExecCallback) then
  begin
    FProcessExecCallback(FProcessExecThread.Response, FProcessExecThread.Error);
    FProcessExecThread := nil;
  end;
end;

{ TFilenameSortCompare }

function TFilenameSortCompare.Compare(const Left, Right: IFileInfo): Integer;
begin
  Result := CompareStr(Left.Filename, Right.Filename);
end;

end.

