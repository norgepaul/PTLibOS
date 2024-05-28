{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Windows.Files                                      }
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

Unit PTLib.Windows.Files;

interface

uses
  Windows, SysUtils, ShellAPI, Classes, ShlObj, Messages, TLHelp32,
  ActiveX, System.Win.ComObj, IOUtils, Registry,

  PTLib.Common.Files,
  PTLib.Common.Dates,
  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Strings;

type
  TExecuteFileOption = (
    eoHide,
    eoWait,
    eoElevate
  );
  TExecuteFileOptions = set of TExecuteFileOption;

  TElevationLevel = (
    elNotActive,
    elDefault,
    elFull,
    elLimited,
    elUnknown,
    elError
  );

function GetFileSize(FileName: String): Int64; deprecated;
function GetMyDocumentsFolder: String; deprecated;
function GetAppDataFolder: String; deprecated;
function GetCommonAppDataFolder: String; deprecated;
function GetWinTempPath: String; deprecated;
function ExecuteFile(Handle: HWND; const Filename, Paramaters: String; Options: TExecuteFileOptions = []): Integer; deprecated;
function IsUACActive: Boolean; deprecated;
function IsDirectory(dWin32FD: TWin32FindData): Boolean; deprecated;
function ScanFiles(Filenames: TStrings; Path, Filter: String; Recursive: Boolean; IncludeDirectories: Boolean = False): Integer; deprecated;
function CreateShortcut(const Filename, Target, Parameters, Description: string): Boolean; deprecated;
function ExecNewProcess(const Filename: String; WaitForProcess: Boolean): Boolean; deprecated;
procedure KillProcessesByName(const Filename: String); deprecated;
procedure KillProcess(const ProcessID: Cardinal);  deprecated;
function ProcessExists(const FileName: string): Boolean; overload;   deprecated;
function ProcessExists(const PID: Cardinal): Boolean; overload;deprecated;
Function WinExecAndWait32V2(const FileName: String; Visibility: Integer): DWORD;  deprecated;
procedure LocateFile(const Filename: String); deprecated;
function GetDirectoryString(const Dir: String): String;deprecated;
function GetConsoleOutput(const CommandLine: string; const WorkingDir: string): string; deprecated;
procedure ShellExec(const Filename: String; const Parameters: String = '';
  const ShowState: Integer = SW_SHOWNORMAL; const WindowHandle: HWND = 0); deprecated;
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime; deprecated;
function QueryWriteAccessToFolder(Path:String):Boolean; deprecated;
function GetRemovableDrives: IList<String>;  deprecated;

implementation

{$WARN SYMBOL_PLATFORM OFF}
function GetFileSize(FileName: String): Int64;
var
  sr: TSearchRec;
begin
  if FindFirst(fileName, faAnyFile, sr) = 0 then
     result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
  else
     result := -1;

  SysUtils.FindClose(sr);
end;
{$WARN SYMBOL_PLATFORM ON}

function GetRemovableDrives: IList<String>;
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

function QueryWriteAccessToFolder(Path:string): Boolean;
var
  FileName: String;
  HRESULT: THandle;
begin
  FileName := IncludeTrailingPathDelimiter(Path) + TFileUtils.MakeGUIDFileName('.tmp');

  HRESULT := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    CREATE_NEW,
    FILE_ATTRIBUTE_TEMPORARY
    or FILE_FLAG_DELETE_ON_CLOSE, // <-- DO NOT REMOVE!!!
     0);

  Result := HRESULT <> INVALID_HANDLE_VALUE;

  if Result then
    CloseHandle(HRESULT);
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);

  Result := SystemTimeToDateTime(SystemTime);
end;

procedure ShellExec(const Filename: String; const Parameters: String;
  const ShowState: Integer; const WindowHandle: HWND);
begin
  ShellExecute(
    WindowHandle,
    'open',
    PChar(Filename),
    nil,
    nil,
    ShowState);
end;

procedure LocateFile(const Filename: String);
begin
  ShellExecute(0,
               'OPEN',
               PChar('explorer.exe'),
               PChar('/select, "' + Filename + '"'),
               nil,
               SW_NORMAL);
end;

Function WinExecAndWait32V2(const FileName: String; Visibility: Integer): DWORD;

  Procedure WaitFor(processHandle: THandle);
  Var
    msg: TMsg;
    ret: DWORD;
  Begin
    Repeat
      ret := MsgWaitForMultipleObjects(1, { 1 handle to wait on }
        processHandle, { the handle }
        False, { wake on any event }
        INFINITE, { wait without timeout }
        QS_PAINT or { wake on paint messages }
        QS_SENDMESSAGE { or messages from other threads }
        );
      If ret = WAIT_FAILED Then
        Exit; { can do little here }
      If ret = (WAIT_OBJECT_0 + 1) Then
      Begin
        { Woke on a message, process paint messages only. Calling
          PeekMessage gets messages send from other threads processed.
        }

        While PeekMessage(msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) Do
          DispatchMessage(msg);
      End;
    Until ret = WAIT_OBJECT_0;
  End; { Waitfor }

Var { V1 by Pat Ritchey, V2 by P.Below }
  zAppName: array [0 .. 512] of char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
Begin { WinExecAndWait32V2 }
  StrPCopy(zAppName, FileName);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  If not CreateProcess(nil, zAppName, { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    False, { handle inheritance flag }
    CREATE_NEW_CONSOLE or { creation flags }
    NORMAL_PRIORITY_CLASS, nil, { pointer to new environment block }
    nil, { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo) { pointer to PROCESS_INF }
  Then
    result := DWORD(-1) { failed, GetLastError has error code }
  Else
  Begin
    WaitFor(ProcessInfo.hProcess);
    GetExitCodeProcess(ProcessInfo.hProcess, result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  End; { Else }
End; { WinExecAndWait32V2 }

function InternalProcessExists(const FileName: string; PID: Cardinal): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := False;

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((Filename <> '') and
        ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(FileName)) or
        (UpperCase(FProcessEntry32.szExeFile) = UpperCase(FileName)))) or
       ((PID <> 0) and
        (FProcessEntry32.th32ProcessID = PID)) then
    begin
      Result := True;

      Break;
    end;

    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;

procedure KillProcess(const ProcessID: Cardinal);
begin
  ExecNewProcess(format('taskkill.exe /f /im * /fi "PID eq %d"', [ProcessID]), False);
end;

procedure KillProcessesByName(const Filename: String);

  procedure KillAppProcess(AStruct: TProcessEntry32);
  begin
    if (SameText(AStruct.szExeFile, Filename)) and
       (AStruct.th32ProcessID <> GetCurrentProcessId) then
      KillProcess(AStruct.th32ProcessID);
  end;

var
  MyHandle: THandle;
  Struct: TProcessEntry32;
  RealFilename: String;
begin
  if Filename = '' then
  begin
    RealFilename := ExtractFilename(ParamStr(0));
  end
  else
  begin
    RealFilename := Filename;
  end;

  MyHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);

  Struct.dwSize := Sizeof(TProcessEntry32);

  if Process32First(MyHandle, Struct) then
  begin
    KillAppProcess(Struct);

    while Process32Next(MyHandle, Struct) do
      KillAppProcess(Struct);
  end;
end;

function ProcessExists(const FileName: string): Boolean;
begin
  Result := InternalProcessExists(Filename, 0);
end;

function ProcessExists(const PID: Cardinal): Boolean;
begin
  Result := InternalProcessExists('', PID);
end;

function ExecNewProcess(const Filename: String; WaitForProcess: Boolean): Boolean;
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  FillChar(StartInfo, SizeOf(TStartupInfo), #0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  StartInfo.cb := SizeOf(TStartupInfo);

  Result := CreateProcess(nil,
                          PChar(Filename),
                          nil,
                          nil,
                          False,
                          CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS + CREATE_NO_WINDOW,
                          nil,
                          nil,
                          StartInfo,
                          ProcInfo);

  if (Result) and (WaitForProcess) then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

function CreateShortcut(const Filename, Target, Parameters, Description: string): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  LinkName: string;
  InFolder: array [0..MAX_PATH-1] of Char;
begin
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  ISLink.SetDescription(PWideChar(Description));
  ISLink.SetPath(PChar(Target));
  ISLink.SetArguments(PChar(Parameters));
  ISLink.SetWorkingDirectory(PChar(ExtractFilePath(Target)));

  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder) ;

  LinkName := ChangeFileExt(Filename, '.lnk');

  if FileExists(LinkName) then
    SysUtils.DeleteFile(LinkName);

  Result := IPFile.Save(PWideChar(LinkName), False) = S_OK;
end;

function IsUACActive: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;

  // There's a chance it's active as we're on Vista or Windows 7. Now check the registry
  if CheckWin32Version(6, 0) then
  begin
    Result := False;

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
      begin
        if (Reg.ValueExists('EnableLUA')) and (Reg.ReadBool('EnableLUA')) then
          Result := True;
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;
end;

function IsDirectory(dWin32FD: TWin32FindData): Boolean;
var
  FName: string;
begin
  FName := StrPas(dWin32FD.cFileName);
  with dWin32FD do
    Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
               FILE_ATTRIBUTE_DIRECTORY) and (FName <> '.') and (FName <> '..');
end;

function ScanFiles(Filenames: TStrings; Path, Filter: String; Recursive: Boolean; IncludeDirectories: Boolean) : Integer;
var
  hFindFile : THandle;
  Win32FD : TWin32FindData;
  FileWithPath: String;
begin
  Path := IncludeTrailingPathDelimiter(Path);
  Result := 0;
  hFindFile := FindFirstFile(PChar(Path + Filter), Win32FD);
  try
    If hFindFile <> INVALID_HANDLE_VALUE then
    With Win32FD do
    Begin
      Repeat
        if (StrPas(cFileName) <> '.') and (StrPas(cFileName) <> '..') then
        Begin
          FileWithPath := concat(Path, cFileName);

          if IsDirectory(Win32FD) then
          begin
            if IncludeDirectories then
            begin
              Inc(Result);

              Filenames.AddObject(IncludeTrailingPathDelimiter(FileWithPath), TObject(1));
            end;

            if Recursive then
              Result := Result + ScanFiles(Filenames, IncludeTrailingPathDelimiter(FileWithPath), Filter, Recursive);
          end
          else
          begin
            inc(Result);

            Filenames.Add(FileWithPath);
          end;
        end;
      until not FindNextFile(hFindFile, Win32FD);
    end;
  finally
    Windows.FindClose(hFindFile);
  end;
end;

function ExecuteFile(Handle: HWND; const Filename, Paramaters: String; Options: TExecuteFileOptions): Integer;
var
  ShellExecuteInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  Result := -1;

  ZeroMemory(@ShellExecuteInfo, SizeOf(ShellExecuteInfo));
  ShellExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellExecuteInfo.Wnd := Handle;
  ShellExecuteInfo.fMask := SEE_MASK_NOCLOSEPROCESS;

  if (eoElevate in Options) and (IsUACActive) then
    ShellExecuteInfo.lpVerb := PChar('runas');

  ShellExecuteInfo.lpFile := PChar(Filename);

  if Paramaters <> '' then
    ShellExecuteInfo.lpParameters := PChar(Paramaters);

  // Show or hide the window
  if eoHide in Options then
    ShellExecuteInfo.nShow := SW_HIDE
  else
    ShellExecuteInfo.nShow := SW_SHOWNORMAL;

  if ShellExecuteEx(@ShellExecuteInfo) then
    Result := 0;

  if (Result = 0) and (eoWait in Options) then
  begin
    GetExitCodeProcess(ShellExecuteInfo.hProcess, ExitCode);

    while (ExitCode = STILL_ACTIVE) do
//          (not Application.Terminated) do
    begin
      sleep(50);

      GetExitCodeProcess(ShellExecuteInfo.hProcess, ExitCode);
    end;

    Result := ExitCode;
  end;
end;

function GetWinTempPath: String;
var
  i: Integer;
begin
  SetLength(Result, MAX_PATH);

  i := GetTempPath(MAX_PATH, {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(Result));
  if i > 0 then
  begin
    SetLength(Result, i);

    Result := IncludeTrailingPathDelimiter(Result);
  end
  else
    Result := '';
end;

function GetMyDocumentsFolder: String;
var
  pidl : PItemIDList;
  path : array [0..MAX_PATH] of widechar;
begin
  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, pidl);
  SHGetPathFromIDList(pidl, path);
  Result := path;
end;

function GetAppDataFolder: String;
var
  pidl : PItemIDList;
  path : array [0..MAX_PATH] of widechar;
begin
  SHGetSpecialFolderLocation(0, CSIDL_APPDATA, pidl);
  SHGetPathFromIDList(pidl, path);
  Result := path;
end;

function GetCommonAppDataFolder: String;
var
  pidl : PItemIDList;
  path : array [0..MAX_PATH] of widechar;
begin
  SHGetSpecialFolderLocation(0, CSIDL_COMMON_APPDATA, pidl);
  SHGetPathFromIDList(pidl, path);
  Result := path;
end;

function GetConsoleOutput(const CommandLine: string; const WorkingDir: string): string;
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
  Result := '';
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
    WorkDir := WorkingDir;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + String(Buffer);
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

function GetDirectoryString(const Dir: String): String;
var
  Path: String;
  hFindFile : THandle;
  Win32FD : TWin32FindData;
  FileLine: String;
  TempTime: _SYSTEMTIME;
begin
  Path := IncludeTrailingPathDelimiter(Dir);

  Result := '';

  hFindFile := FindFirstFile(PChar(Path + '*.*'), Win32FD);
  try
    If hFindFile <> INVALID_HANDLE_VALUE then
    Repeat
      if (StrPas(Win32FD.cFileName) <> '.') and (StrPas(Win32FD.cFileName) <> '..') then
      begin
        FileTimeToSystemTime(Win32FD.ftCreationTime, TempTime);

        // Date
        FileLine := DateToCommonDateStr(SystemTimeToDateTime(TempTime));

        // Time
        AddToken(FileLine, TimeToCommonTimeStr(SystemTimeToDateTime(TempTime), False), #09);

        if IsDirectory(Win32FD) then
        begin
          AddToken(FileLine, '<DIR>', #09);
          AddToken(FileLine, #09#09, #09);
        end
        else
        begin
          AddToken(FileLine, '     ', #09);
          AddToken(FileLine, TFileUtils.FileSizeFormatStr(Win32FD.nFileSizeLow), #09);
        end;

        AddToken(FileLine, Win32FD.cFileName, #09);
      end;

      AddLine(Result, FileLine);
    until not FindNextFile(hFindFile, Win32FD);
  finally
    Windows.FindClose(hFindFile);
  end;
end;

(*
{$WARN UNIT_PLATFORM OFF}

Uses
  Windows, SysUtils, ShellAPI, Forms, Classes, ShlObj, Messages, TLHelp32,
  ActiveX, System.Win.ComObj,

  PTLib.Common.Strings,

  ShareBike.Common.Types,

  ShareBikeLibrary_Intf;

type
  TExecuteFileOption = (
    eoHide,
    eoWait,
    eoElevate
  );
  TExecuteFileOptions = set of TExecuteFileOption;

  TElevationLevel = (
    elNotActive,
    elDefault,
    elFull,
    elLimited,
    elUnknown,
    elError
  );

  function ExecNewProcess(Filename: String; WaitForProcess: Boolean = False): Boolean;
  function ExecuteFile(Handle: HWND; const Filename, Paramaters: String; Options: TExecuteFileOptions = []): Integer;
  function ExtractFilenameNoExt(Filename: String): String;
  function ExtractFilepathNoExt(const Filename: String): String;
  function AddExtensionDot(const Extension: String): String;
  function GetAppFilePath: String;
  function RemoveFileExt(Filename: String): String;
  function GetWinTempPath: String;
  function GetWinPath: String;
  function GetWinSystemPath: String;
  function GetAppFilename: String;
  function Make83Path(const ALongPath: string): string;
  function IsFullPath(Path: String): Boolean;
  function FileTime2DateTime(FileTime: TFileTime): TDateTime;
  function ScanFiles(Filenames: TStrings; Path, Filter: String; Recursive: Boolean; IncludeDirectories: Boolean = False) : Integer; overload;
  function GetDirectoryString(const Dir: String): String;
  function GetMyDocumentsFolder: String;
  function GetAppDataFolder: String;
  function GetCommonAppDataFolder: String;
  function GetUserDataFolder: String;
  function GetCreateShareBikeDocumentFolder: String;
  function GetGUIDTempFileName: String;
  function GetUniqueFilename(Filename: String): String;
  function CheckFileName(var FileName: String; Correct: Boolean): Boolean;
  function BuildFilePath(Directory, Filename, Extension: String): String;
  function GetRelativePath(RelativeTo, AbsolutePath: String): String;
  function FormatByteSize(const Bytes: Int64): String;
  function IsValidFilename(const Filename: String): Boolean;
  procedure CheckValidFilename(const Filename: String; const MaximumLength: Integer = 0);
  function ExpandFilePathTags(const Filepath: String) : String;
  function SetFileDate(const FileName: String; const FileDate: TDateTime): Boolean;
  function GetFileSize(FileName: String): Int64;
  Function WinExecAndWait32V2(FileName: String; Visibility: integer): DWORD;
  function GetParamValue(const ParamName: String): String;
  function ParamExists(const ParamName: String): Boolean;
  function IsUACActive: Boolean;
  function GetElevationLevel: TElevationLevel;
  procedure LaunchScheduler(Handle: THandle; ApplicationType: String; ScheduleType: String; Server: String; Username: String; Password: String; Filename: String; const AdditionalParams: String = ''); overload;
  procedure LaunchScheduler(Handle: THandle; ApplicationType: String); overload;
  function GetConsoleRunFilename: String;
  procedure CreateEmptyFile(const Filename: String);
  function GetFileHash(const Filename: String): String;
  function GetServerStationInstallerFilename(const Version: String): String;
  function GetStationStationInstallerFilename(const Version: String): String;
  function GetStationInstallerDirectory: String;
  function ProcessExists(const FileName: string): Boolean; overload;
  function ProcessExists(const PID: Cardinal): Boolean; overload;
  procedure KillProcess(ProcessID: Cardinal);
  procedure KillAppProcess;
  procedure KillProcessesByName(Filename: String);
  function CreateShortcut(const Filename, Target, Parameters, Description: string): Boolean;
  procedure LocateFile(const Filename: String);
  procedure DeleteDirectory(const Directory: string);
  function GetConsoleOutput(const CommandLine: string; const WorkingDir: string): string;

 procedure PCharFromStr(const AString: String; var APChar: PChar);
 function AnsiExtractQuotedStrEx(var Src: String; Quote: Char): String;
 procedure PAnsiCharFromStr(const AString: String; var APChar: PAnsiChar);

implementation

uses
  Registry,
  IdHashMessageDigest,
  idHash;
  //ShareBike.Common.Strings.PChars;

resourcestring
  StrPathsDoNotHaveACommonBase = 'Paths do not have a common base';
  StrFilenamesInvalidCharacters = 'Sorry, the following characters are not allowed: %s';
  StrTheMaximumFilenameLengths = 'The maximum directory and filename lengths are %d characters';

const
  AppFolderName = 'ShareBike\V1';
  InvalidFileChars = '/ \ : * ? " < > |';
  InvalidFileCharArray: Set of AnsiChar = ['/', '\', ':', '*', '?', '"', '<', '>', '|'];

procedure LocateFile(const Filename: String);
begin
  ShellExecute(0,
               'OPEN',
               PChar('explorer.exe'),
               PChar('/select, "' + Filename + '"'),
               nil,
               SW_NORMAL);
end;

procedure DeleteDirectory(const Directory: string);
var
  F: TSearchRec;
begin
  if FindFirst(Directory + '\*', faAnyFile, F) = 0 then begin
    try
      repeat
        if (F.Attr and faDirectory <> 0) then begin
          if (F.Name <> '.') and (F.Name <> '..') then begin
            DeleteDirectory(Directory + '\' + F.Name);
          end;
        end else begin
          DeleteFile(PChar(Directory + '\' + F.Name));
        end;
      until FindNext(F) <> 0;
    finally
      FindClose(F);
    end;

    RemoveDir(Directory);
  end;
end;

function CreateShortcut(const Filename, Target, Parameters, Description: string): Boolean;
var
  IObject: IUnknown;
  ISLink: IShellLink;
  IPFile: IPersistFile;
  PIDL: PItemIDList;
  LinkName: string;
  InFolder: array [0..MAX_PATH-1] of Char;
begin
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink := IObject as IShellLink;
  IPFile := IObject as IPersistFile;

  ISLink.SetDescription(PWideChar(Description));
  ISLink.SetPath(PChar(Target));
  ISLink.SetArguments(PChar(Parameters));
  ISLink.SetWorkingDirectory(PChar(ExtractFilePath(Target)));

  SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY, PIDL);
  SHGetPathFromIDList(PIDL, InFolder) ;

  LinkName := ChangeFileExt(Filename, '.lnk');

  if FileExists(LinkName) then
    SysUtils.DeleteFile(LinkName);

  Result := IPFile.Save(PWideChar(LinkName), False) = S_OK;
end;

procedure KillAppProcess;
begin
  KillProcess(GetCurrentProcessId);
end;

procedure KillProcess(ProcessID: Cardinal);
begin
  ExecNewProcess(format('taskkill.exe /f /im * /fi "PID eq %d"', [ProcessID]), False);
end;

procedure KillProcessesByName(Filename: String);

  procedure KillAppProcess(AStruct: TProcessEntry32);
  begin
    if (SameText(AStruct.szExeFile, Filename)) and
       (AStruct.th32ProcessID <> GetCurrentProcessId) then
      KillProcess(AStruct.th32ProcessID);
  end;

var
  MyHandle: THandle;
  Struct: TProcessEntry32;
begin
  if Filename = '' then
    Filename := ExtractFilename(GetAppFilename);

  MyHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);

  Struct.dwSize := Sizeof(TProcessEntry32);

  if Process32First(MyHandle, Struct) then
  begin
    KillAppProcess(Struct);

    while Process32Next(MyHandle, Struct) do
      KillAppProcess(Struct);
  end;
end;

function InternalProcessExists(const FileName: string; PID: Cardinal): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  Result := False;

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);

  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((Filename <> '') and
        ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(FileName)) or
        (UpperCase(FProcessEntry32.szExeFile) = UpperCase(FileName)))) or
       ((PID <> 0) and
        (FProcessEntry32.th32ProcessID = PID)) then
    begin
      Result := True;

      Break;
    end;

    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;

function ProcessExists(const FileName: string): Boolean;
begin
  Result := InternalProcessExists(Filename, 0);
end;

function ProcessExists(const PID: Cardinal): Boolean;
begin
  Result := InternalProcessExists('', PID);
end;

function GetStationStationInstallerFilename(const Version: String): String;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFilePathTags('{app}')) + IncludeTrailingPathDelimiter(StationInstallerDir) + StationInstallerFilenamePrefix + 'V' + Version + '.exe';
end;

function GetFileHash(const Filename: String): String;
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
    Result := '';
end;

function ExecuteFile(Handle: HWND; const Filename, Paramaters: String; Options: TExecuteFileOptions): Integer;
var
  ShellExecuteInfo: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  Result := -1;

  ZeroMemory(@ShellExecuteInfo, SizeOf(ShellExecuteInfo));
  ShellExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
  ShellExecuteInfo.Wnd := Handle;
  ShellExecuteInfo.fMask := SEE_MASK_NOCLOSEPROCESS;

  if (eoElevate in Options) and (IsUACActive) then
    ShellExecuteInfo.lpVerb := PChar('runas');

  ShellExecuteInfo.lpFile := PChar(Filename);

  if Paramaters <> '' then
    ShellExecuteInfo.lpParameters := PChar(Paramaters);

  // Show or hide the window
  if eoHide in Options then
    ShellExecuteInfo.nShow := SW_HIDE
  else
    ShellExecuteInfo.nShow := SW_SHOWNORMAL;

  if ShellExecuteEx(@ShellExecuteInfo) then
    Result := 0;

  if (Result = 0) and (eoWait in Options) then
  begin
    GetExitCodeProcess(ShellExecuteInfo.hProcess, ExitCode);

    while (ExitCode = STILL_ACTIVE) and
          (not Application.Terminated) do
    begin
      sleep(50);

      GetExitCodeProcess(ShellExecuteInfo.hProcess, ExitCode);
    end;

    Result := ExitCode;
  end;
end;

function SetFileDate(const FileName: String; const FileDate: TDateTime): Boolean;
var
  FileHandle: THandle;
  FileSetDateResult: Integer;
begin
  Result := False;

  FileHandle := FileOpen(FileName, fmOpenWrite OR fmShareDenyNone);
  try
    try
      if FileHandle > 0 Then
      begin
        FileSetDateResult := FileSetDate(Filename, DateTimeToFileDate(FileDate));

        result := FileSetDateResult = 0;
      end;
    except
      Result := False;
    end;
  finally
    FileClose(FileHandle);
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}
function GetFileSize(FileName: String): Int64;
var
  sr: TSearchRec;
begin
  if FindFirst(fileName, faAnyFile, sr) = 0 then
     result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
  else
     result := -1;

  SysUtils.FindClose(sr);
end;
{$WARN SYMBOL_PLATFORM ON}

function AddExtensionDot(const Extension: String): String;
begin
  Result := Extension;

  if (length(Result) > 0) and (Result[1] <> '.') then
    Result := concat('.', Result);
end;

function GetUserDataFolder: String;
begin
  Result := concat(IncludeTrailingPathDelimiter(GetMyDocumentsFolder), AppFolderName);
end;

function IsValidFilename(const Filename: String): Boolean;
var
  i: Integer;
begin
  Result := Length(Filename) > 0;

  if Result then
    for i := 1 to length(Filename) do
      if CharInSet(Filename[i], InvalidFileCharArray) then
      begin
        Result := False;

        Break;
      end;
end;

function GetConsoleRunFilename: String;
begin
  Result := concat(GetAppFilePath, 'consolerun.exe');
end;

procedure CheckValidFilename(const Filename: String; const MaximumLength: Integer);
begin
  if not IsValidFilename(Filename) then
    raise EShareBikeCommonError.Create(format(StrFilenamesInvalidCharacters, [InvalidFileChars]));

  if (MaximumLength > 0) and (length(Filename) > MaximumLength) then
    raise EShareBikeCommonError.Create(format(StrTheMaximumFilenameLengths, [MaximumLength]));
end;

function FormatByteSize(const Bytes: Int64): String;
const
  B = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
begin
  if bytes = 0 then
    result := '0 bytes' else
  if bytes > GB then
    result := FormatFloat('#.### GB', bytes / GB) else
  if bytes > MB then
    result := FormatFloat('#.## MB', bytes / MB) else
  if bytes > KB then
    result := FormatFloat('#.## KB', bytes / KB)
  else
    result := FormatFloat('#.## bytes', bytes) ;
end;

function GetRelativePath(RelativeTo, AbsolutePath: String): String;

  procedure SplitDirectories(Directory: String; const Directories: TStringList);
  var
    i: Integer;
  begin
    for i := length(Directory) downto 1 do
    begin
      if (Directory[i] = '\') or (i = 1) then
      begin
        Directories.Insert(0, copy(Directory, i + 1, MaxInt));
        Directory := copy(Directory, 1, i - 1);
      end;
    end;
  end;

var
  AbsoluteDirectories,
  RelativeDirectories: TStringList;
  Len, LastCommonRoot, i: Integer;
begin
  Result := '';

  AbsoluteDirectories := TStringList.Create;
  RelativeDirectories := TStringList.Create;
  try
    SplitDirectories(AbsolutePath, AbsoluteDirectories);
    SplitDirectories(RelativeTo, RelativeDirectories);

    //Get the shortest of the two paths
    Len := AbsoluteDirectories.Count;
    if Len < RelativeDirectories.Count then
      Len := RelativeDirectories.Count;

    //Use to determine where in the loop we exited
    LastCommonRoot := -1;

    //Find common root
    for i := 0 to Len do
    begin
        if AbsoluteDirectories[i] = RelativeDirectories[i] then
          LastCommonRoot := i
        else
          break;
    end;

    //If we didn't find a common prefix then throw exception
    if lastCommonRoot = -1 then
      raise Exception.Create(StrPathsDoNotHaveACommonBase);

    //Build up the relative path
    //Add on the ..
    for i := LastCommonRoot + 1 to pred(AbsoluteDirectories.Count) do
      if length(AbsoluteDirectories[i]) > 0 then
        Result := concat(Result, '..\');

    //Add on the folders
    for i := LastCommonRoot + 1 to pred(RelativeDirectories.Count) do
      Result := concat(Result, RelativeDirectories[i], '\');
  finally
    // Free the stringlists
    FreeAndNil(AbsoluteDirectories);
    FreeAndNil(RelativeDirectories);
  end;
end;

function Make83Path(const ALongPath: string): string;
var
  PSrc: PChar;
  PDst: PChar;
begin
  PCharFromStr(ALongPath, PSrc);
  PDst := StrAlloc($0100);
  FillChar(PDst^, $0100, #$00);
  GetShortPathName(PSrc, PDst, Pred($0100));
  Result := StrPas(PDst);
  StrDispose(PDst);
end;

function BuildFilePath(Directory, Filename, Extension: String): String;
begin
  if (Extension <> '') and (Extension[1] <> '.') then
    Extension := concat('.', Extension);

  Result := concat(IncludeTrailingPathDelimiter(Directory), Filename, Extension);
end;

function CheckFileName(var FileName: String; Correct: Boolean): Boolean;
var
  i, idxErr: Integer;
begin
 idxErr := 0;

 for i := 1 to Length(FileName) do
   if CharInSet(FileName[i], ['/','\',':','?','*','"','<','>','|']) then
   begin
     Inc(idxErr);

     if Correct then
       FileName[i] := '_'
     else
       Break;
   end;

  Result := IdxErr = 0;
end;

function ExecNewProcess(Filename: String; WaitForProcess: Boolean): Boolean;
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  FillChar(StartInfo, SizeOf(TStartupInfo), #0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  StartInfo.cb := SizeOf(TStartupInfo);

  Result := CreateProcess(nil,
                          PChar(Filename),
                          nil,
                          nil,
                          False,
                          CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS + CREATE_NO_WINDOW,
                          nil,
                          nil,
                          StartInfo,
                          ProcInfo);

  if (Result) and (WaitForProcess) then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

Function WinExecAndWait32V2(FileName: String; Visibility: integer): DWORD;

  Procedure WaitFor(processHandle: THandle);
  Var
    msg: TMsg;
    ret: DWORD;
  Begin
    Repeat
      ret := MsgWaitForMultipleObjects(1, { 1 handle to wait on }
        processHandle, { the handle }
        False, { wake on any event }
        INFINITE, { wait without timeout }
        QS_PAINT or { wake on paint messages }
        QS_SENDMESSAGE { or messages from other threads }
      );
      If ret = WAIT_FAILED Then
        Exit; { can do little here }
      If ret = (WAIT_OBJECT_0 + 1) Then
      Begin
        { Woke on a message, process paint messages only. Calling
          PeekMessage gets messages send from other threads processed.
        }

        While PeekMessage(msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) Do
          DispatchMessage(msg);
      End;
    Until ret = WAIT_OBJECT_0;
  End; { Waitfor }

Var { V1 by Pat Ritchey, V2 by P.Below }
  zAppName: array [0 .. 512] of char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
Begin { WinExecAndWait32V2 }
  StrPCopy(zAppName, FileName);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  If not CreateProcess(nil, zAppName, { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    False, { handle inheritance flag }
    CREATE_NEW_CONSOLE or { creation flags }
    NORMAL_PRIORITY_CLASS, nil, { pointer to new environment block }
    nil, { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo) { pointer to PROCESS_INF }
  Then
    Result := DWORD(-1) { failed, GetLastError has error code }
  Else
  Begin
    WaitFor(ProcessInfo.hProcess);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  End; { Else }
End; { WinExecAndWait32V2 }

function IsUACActive: Boolean;
var
  Reg: TRegistry;
begin
  Result := False;

  // There's a chance it's active as we're on Vista or Windows 7. Now check the registry
  if CheckWin32Version(6, 0) then
  begin
    Result := False;

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System') then
      begin
        if (Reg.ValueExists('EnableLUA')) and (Reg.ReadBool('EnableLUA')) then
          Result := True;
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;
end;

function GetElevationLevel: TElevationLevel;
const
  TokenElevationType = 18;
  TokenElevationTypeDefault = 1;
  TokenElevationTypeFull    = 2;
  TokenElevationTypeLimited = 3;

var
  token: NativeUInt;
  ElevationType: Integer;
  dwSize: Cardinal;
begin
  if not IsUACActive then
    Result := elNotActive
  else
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, token) then
    try
      if GetTokenInformation(token, TTokenInformationClass(TokenElevationType), @ElevationType, SizeOf(ElevationType), dwSize) then
        case ElevationType of
          TokenElevationTypeDefault: Result := elDefault;
          TokenElevationTypeFull: Result := elFull;
          TokenElevationTypeLimited: Result := elLimited;
        else
          Result := elUnknown;
        end
      else
        Result := elError;
    finally
      CloseHandle(token);
    end
  else
    Result := elError;
end;

procedure LaunchScheduler(Handle: THandle; ApplicationType: String);
begin
  LaunchScheduler(Application.Handle, ApplicationType, '', '', '', '', '');
end;

procedure LaunchScheduler(Handle: THandle; ApplicationType: String; ScheduleType: String; Server: String; Username: String; Password: String; Filename: String; const AdditionalParams: String);
var
  Parameters, SchedulerFilename: String;
begin
  SchedulerFilename := concat('"', GetAppFilePath, 'ShareBikeScheduler.exe', '"');

  Parameters := format('-application %s', [ApplicationType]);

  if ScheduleType <> '' then
    Parameters := format(' %s -scheduletype %s ',
                         [Parameters, ScheduleType]);

  if Server <> '' then
    Parameters := format(' %s -filename "%s" -server "%s" -user %s -password %s',
                         [Parameters,
                          Filename,
                          Server,
                          Username,
                          Password]);

  if AdditionalParams <> '' then
    Parameters := concat(Parameters, ' ', AdditionalParams);

  ExecuteFile(Application.Handle, SchedulerFilename, Parameters, [eoElevate]);
end;

function GetParamValue(const ParamName: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to pred(ParamCount) do
    if (ParamStr(i) <> '') and
       (CharInSet(ParamStr(i)[1], ['-', '/', '\'])) and
       (UpperCase(Copy(ParamStr(i), 2, length(ParamStr(i)))) = UpperCase(ParamName)) then
    begin
      Result := ParamStr(i + 1);
      Break;
    end;
end;

function ParamExists(const ParamName: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 1 to ParamCount do
    if (ParamStr(i) <> '') and
       (CharInSet(ParamStr(i)[1], ['-', '/', '\'])) and
       (UpperCase(Copy(ParamStr(i), 2, length(ParamStr(i)))) = UpperCase(ParamName)) then
    begin
      Result := True;

      Break;
    end;
end;

function ExtractFilenameNoExt(Filename: String): String;
var
  ExtLen: Integer;
begin
  ExtLen := Length(ExtractFileExt(Filename));
  Result := ExtractFileName(Filename);

  if ExtLen > 0 then
    Result := Copy(Result, 1, length(Result) - ExtLen);
end;

function ExtractFilepathNoExt(const Filename: String): String;
var
  ExtLen: Integer;
begin
  ExtLen := Length(ExtractFileExt(Filename));
  Result := Filename;

  if ExtLen > 0 then
    Result := Copy(Result, 1, length(Result) - ExtLen);
end;

function GetAppFilePath: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
end;

function GetAppFilename: String;
begin
  Result := Application.Exename;
end;

function RemoveFileExt(Filename: String): String;
var
  Ext: String;
begin
  Ext := ExtractFileExt(extractfilename(Filename));
  Result := Copy(Filename, 1, Length(Filename) - Length(Ext));
end;

function GetCreateShareBikeDocumentFolder: String;
begin
  Result := concat(IncludeTrailingPathDelimiter(GetAppDataFolder), AppFolderName);
  ForceDirectories(Result);
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetWinTempPath: String;
var
  i: Integer;
begin
  SetLength(Result, MAX_PATH);

  i := GetTempPath(MAX_PATH, {$IFDEF UNICODE}PWideChar{$ELSE}PChar{$ENDIF}(Result));
  if i > 0 then
  begin
    SetLength(Result, i);

    Result := IncludeTrailingPathDelimiter(Result);
  end
  else
    Result := '';
end;

function GetWinPath: String;
var
  b : array[0..255] of widechar ;
begin
  Result := '';

  if GetWindowsDirectory(b,  sizeof(b)) > 0 then
    Result := Copy(StrPas(b),1,Length(b) -1);
end;

function GetWinSystemPath: String;
var
  b : array[0..255] of widechar ;
begin
  Result := '';

  if GetSystemDirectory(b, sizeof(b) ) > 0 then
    Result := Copy(StrPas(b),1,Length(b) -1);
end;

function IsFullPath(Path: String): Boolean;
begin
  Result := pos(':\', Path) = 2;
end;

function FileTime2DateTime(FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);

  Result := SystemTimeToDateTime(SystemTime);
end;

procedure CreateEmptyFile(const Filename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);

  FreeAndNil(FileStream);
end;

function IsDirectory(dWin32FD: TWin32FindData): Boolean;
var
  FName: string;
begin
  FName := StrPas(dWin32FD.cFileName);
  with dWin32FD do
    Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
               FILE_ATTRIBUTE_DIRECTORY) and (FName <> '.') and (FName <> '..');
end;

function ScanFiles(Filenames: TStrings; Path, Filter: String; Recursive: Boolean; IncludeDirectories: Boolean) : Integer;
var
  hFindFile : THandle;
  Win32FD : TWin32FindData;
  FileWithPath: String;
begin
  Path := IncludeTrailingPathDelimiter(Path);
  Result := 0;
  hFindFile := FindFirstFile(PChar(Path + Filter), Win32FD);
  try
    If hFindFile <> INVALID_HANDLE_VALUE then
    With Win32FD do
    Begin
      Repeat
        if (StrPas(cFileName) <> '.') and (StrPas(cFileName) <> '..') then
        Begin
          FileWithPath := concat(Path, cFileName);

          if IsDirectory(Win32FD) then
          begin
            if IncludeDirectories then
            begin
              Inc(Result);

              Filenames.AddObject(IncludeTrailingPathDelimiter(FileWithPath), TObject(1));
            end;

            if Recursive then
              Result := Result + ScanFiles(Filenames, IncludeTrailingPathDelimiter(FileWithPath), Filter, Recursive);
          end
          else
          begin
            inc(Result);

            Filenames.Add(FileWithPath);
          end;
        end;
      until not FindNextFile(hFindFile, Win32FD);
    end;
  finally
    Windows.FindClose(hFindFile);
  end;
end;

function GetGUIDTempFileName: String;
begin
  Result := concat(IncludeTrailingPathDelimiter(GetWinTempPath), CreateGUIDString);
end;

function GetUniqueFilename(Filename: String): String;

  function InsertFileCount(Filename: String; Count: Integer): String;
  begin
    Result := concat(IncludeTrailingPathDelimiter(ExtractFilePath(Filename)),
                     ExtractFilenameNoExt(Filename),
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

procedure PCharFromStr(const AString: String; var APChar: PChar);
var
  ILen: integer;
begin
  ILen := Length(AString);
  APChar := StrAlloc(ILen + 1);
  FillChar(APChar^, ILen + 1, #$00);
  Move(AString[1], APChar^, ILen);
end; {PCharFromStr}

procedure PAnsiCharFromStr(const AString: String; var APChar: PAnsiChar);
var
  ILen: integer;
begin
  ILen := Length(AString);
  APChar := AnsiStrAlloc(ILen + 1);
  FillChar(APChar^, ILen + 1, #$00);
  MoveChars(AString[1], APChar^, ILen);
end; {PCharFromStr}

function AnsiExtractQuotedStrEx(var Src: String; Quote: Char): String;
var
  TempPChar: PChar;
begin
  if (Src <> '') and (Src[1] = Quote) then
  begin
    PCharFromStr(Src, TempPChar);
    try
      Result := AnsiExtractQuotedStr(TempPChar, Quote);
      Src := TempPChar;
    finally
      //Dispose(TempPChar); { TODO : Is this a memory leak? If Uncommented throws exception }
    end;
  end
  else
    Result := Src;
end;

end. *)

end.
