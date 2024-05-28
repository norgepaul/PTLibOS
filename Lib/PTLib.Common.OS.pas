//{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.OS                                          }
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

unit PTLib.Common.OS;

interface

uses
  System.SysUtils, System.Types, System.IOUtils,

  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  WinAPI.TLHelp32,
  {$ENDIF}

  {$IFDEF LINUX64}
  Posix.Stdlib, Posix.SysStat, Posix.SysTypes, Posix.Unistd, Posix.Signal, Posix.Fcntl,
  {$ENDIF}

  {$IFDEF Android}
  Androidapi.JNI.Os,  //TJBuild
  Androidapi.Helpers, // StringToJString
  {$ENDIF}

  PTLib.Common.Files,
  PTLib.Common.InformationList,
  PTLib.Common.Interfaces;

type
  TOSUtils = class
  private
    {$IFDEF MSWINDOWS}
    class function NTSetPrivilege(const sPrivilege: string; bEnabled: Boolean): Boolean; static;
    {$ENDIF}
    class function PlatformFromPointer: integer; static;
  public
    {$IFDEF MSWINDOWS}
    class function GetOSLangID: String;
    {$ENDIF}
    class function GetOSInformation(const PropertySeparator: String = ': '): IInformationList;
    class procedure RestartPC;
    class function GetProcessID: Cardinal;
    class procedure KillCurrentProcess;
    class procedure KillProcess(const ProcessID: Cardinal);
    class function WriteHeartBeatFile(const Filename: String = ''; const Content: String = ''): Boolean;
    class procedure RemoveHeartBeatFile;
    class procedure WriteBusyFile(const Busy: Boolean);
    class function GetPIDFilename: String;
    class procedure KillProcessByFilename(const Filename: String; const ExcludeCurrentProcess: Boolean = True);
  end;

implementation

{$IFDEF MSWINDOWS}
const
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege';
  SE_TCB_NAME = 'SeTcbPrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME = 'SeBackupPrivilege';
  SE_RESTORE_NAME = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_AUDIT_NAME = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege';
{$ENDIF}

resourcestring
  StrOSVersion = 'OS Version';
  StrServicePackMinor = 'Service Pack - Minor';
  StrServicePackMajor = 'Service Pack - Major';
  StrName = 'Name';
  StrMinor = 'Minor';
  StrMajor = 'Major';
  StrBuild = 'Build';
  StrPlatform = 'Platform';
  StrArchitecture = 'Architecture';

function OSArchitectureToStr(const a: TOSVersion.TArchitecture): string;
begin
  case a of
    arIntelX86: Result := 'IntelX86';
    arIntelX64: Result := 'IntelX64';
    arARM32: Result := 'ARM32';
    arARM64: Result := 'ARM64';
  else
    Result := 'Unknown OS architecture';
  end;
end;

function OSPlatformToStr(const p: TOSVersion.TPlatform): string;
begin
  case p of
    pfWindows: Result := 'Windows';
    pfMacOS: Result := 'MacOS';
    pfiOS: Result := 'iOS';
    pfAndroid: Result := 'Android';
    pfWinRT: Result := 'WinRT';
    pfLinux: Result := 'Linux';
  else
    Result := 'Unknown OS Platform';
  end;
end;

class function TOSUtils.PlatformFromPointer: integer;
begin
  Result := SizeOf(Pointer) * 8;
end;

{$IFDEF MACOS}
function class TOSUtils.GetOSLangID: String;
var
  Languages: NSArray;
begin
  Languages := TNSLocale.OCClass.preferredLanguages;
  Result := TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TOSUtils.NTSetPrivilege(const sPrivilege: string; bEnabled: Boolean): Boolean;
var
  hToken: THandle;
  TokenPriv: TOKEN_PRIVILEGES;
  PrevTokenPriv: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
begin
  Result := True;
  // Only for Windows NT/2000/XP and later.
  if not (Win32Platform = VER_PLATFORM_WIN32_NT) then Exit;

  // obtain the processes token
  if OpenProcessToken(GetCurrentProcess(),
    TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  begin
    try
      // Get the locally unique identifier (LUID) .
      if LookupPrivilegeValue(nil, PChar(sPrivilege),
        TokenPriv.Privileges[0].Luid) then
      begin
        TokenPriv.PrivilegeCount := 1; // one privilege to set

        case bEnabled of
          True: TokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
          False: TokenPriv.Privileges[0].Attributes := 0;
        end;

        ReturnLength := 0; // replaces a var parameter
        PrevTokenPriv := TokenPriv;

        // enable or disable the privilege

        AdjustTokenPrivileges(hToken, False, TokenPriv, SizeOf(PrevTokenPriv),
          PrevTokenPriv, ReturnLength);
      end;
    finally
      CloseHandle(hToken);
    end;
  end;
  // test the return value of AdjustTokenPrivileges.
  Result := GetLastError = ERROR_SUCCESS;
  if not Result then
    raise Exception.Create(SysErrorMessage(GetLastError));
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TOSUtils.GetOSLangID: String;
var
  buffer: MarshaledString;
  UserLCID: LCID;
  BufLen: Integer;
begin // defaults
  UserLCID := GetUserDefaultLCID;
  BufLen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
  buffer := StrAlloc(BufLen);
  if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, BufLen) <> 0
  then
    Result := buffer
  else
    Result := 'en';
  StrDispose(buffer);
end;
{$ENDIF}
(*
{$ELSE}
class function TOSUtils.GetOSLangID: String;
var
  LocServ: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocServ)) then
  begin
    Result := LocServ.GetCurrentLangID;
  end;
end;
{$ENDIF}
*)

class function TOSUtils.GetOSInformation(const PropertySeparator: String): IInformationList;
begin
  Result := TInformationList.Create;
  Result.PropertySeparator := PropertySeparator;

  Result.
    AddProperty(StrOSVersion, TOSVersion.ToString).
    AddProperty(StrArchitecture, OSArchitectureToStr(TOSVersion.Architecture)).
    AddProperty(StrPlatform, OSPlatformToStr(TOSVersion.Platform) + IntToStr(PlatformFromPointer)).
    AddProperty(StrBuild, IntToStr(TOSVersion.Build)).
    AddProperty(StrMajor, IntToStr(TOSVersion.Major)).
    AddProperty(StrMinor, IntToStr(TOSVersion.Minor)).
    AddProperty(StrName, TOSVersion.Name).
    AddProperty(StrServicePackMajor, IntToStr(TOSVersion.ServicePackMajor)).
    AddProperty(StrServicePackMinor, IntToStr(TOSVersion.ServicePackMinor));
end;

{ TOSUtils }

class procedure TOSUtils.RestartPC;
begin
  {$IFDEF MSWINDOWS}
  NTSetPrivilege(SE_SHUTDOWN_NAME, True);

  ExitWindowsEx(EWX_REBOOT or EWX_FORCE, 0);
  {$ENDIF}
  {$IFDEF LINUX64}
  TFileUtils.ExecuteCommand('reboot', '', '');
  {$ENDIF}
end;

const
  HeartBeatFilename = '/var/run/%s/%s.pid';
  BusyFilename = '/var/run/%s/%s.busy';

class procedure TOSUtils.WriteBusyFile(const Busy: Boolean);
{$IFDEF LINUX64}
var
  Filename: String;
{$ENDIF}
begin
  {$IFDEF LINUX64}
  Filename := format(BusyFilename, [TFileUtils.ExtractFilenameNoExt(TFileUtils.GetAppFilename), TFileUtils.ExtractFilenameNoExt(TFileUtils.GetAppFilename)]);

  if DirectoryExists(ExtractFileDir(Filename)) then
  begin
    TFile.WriteAllText(Filename, BoolToStr(Busy, False));
  end;
  {$ENDIF}
end;

class procedure TOSUtils.RemoveHeartBeatFile;
{$IFDEF LINUX64}
var
  Filename: String;
{$ENDIF}
begin
  {$IFDEF LINUX64}
  Filename := format(HeartBeatFilename, [TFileUtils.ExtractFilenameNoExt(TFileUtils.GetAppFilename), TFileUtils.ExtractFilenameNoExt(TFileUtils.GetAppFilename)]);

  if DirectoryExists(ExtractFileDir(Filename)) then
  begin
    TFileUtils.DeleteFile(Filename);
  end;
  {$ENDIF}
end;

class function TOSUtils.GetPIDFilename: String;
const
{$IFDEF MSWINDOWS}
  HeartBeatFilename = './%s/%s.pid';
{$ELSE}
  HeartBeatFilename = '/var/run/%s/%s.pid';
{$ENDIF}
begin
  if not FindCmdLineSwitch('pidfile', Result) then
  begin
    Result := format(HeartBeatFilename, [TFileUtils.ExtractFilenameNoExt(TFileUtils.GetAppFilename), TFileUtils.ExtractFilenameNoExt(TFileUtils.GetAppFilename)]);
  end;
end;

class function TOSUtils.WriteHeartBeatFile(const Filename: String; const Content: String): Boolean;
var
  RealFilename, RealContent: String;
begin
  if Filename <> '' then
  begin
    RealFilename := Filename;
  end
  else
  begin
    RealFilename := GetPIDFilename;
  end;

  Result := DirectoryExists(ExtractFileDir(RealFilename));

  if Result then
  begin
    if Content = '' then
    begin
      RealContent := GetProcessID.ToString;
    end
    else
    begin
      RealContent := Content;
    end;

    try
      TFile.WriteAllText(RealFilename, RealContent);
    except
      Result := False;
    end;
  end;
end;

class function TOSUtils.GetProcessID: Cardinal;
begin
  {$IFDEF MSWINDOWS}
  Result := GetCurrentProcessId;
  {$ENDIF}

  {$IFDEF LINUX64}
  Result := getpid;
  {$ENDIF}
end;

class procedure TOSUtils.KillCurrentProcess;
begin
  {$IFDEF MSWINDOWS}
  TFileUtils.ExecuteCommand(
    'taskkill.exe',
    format('/f /im * /fi "PID eq %d"', [GetProcessID]),
    '');
  {$ENDIF}

  {$IFDEF LINUX64}
  Halt(0);
  {$ENDIF}
end;

class procedure TOSUtils.KillProcess(const ProcessID: Cardinal);
begin
  {$IFDEF MSWINDOWS}
  TFileUtils.ExecuteCommand(
    'taskkill.exe',
    format('/f /im * /fi "PID eq %d"', [ProcessID]),
    '');
  {$ENDIF}

  {$IFDEF LINUX64}
  TFileUtils.ExecuteCommand(
    'kill',
    format('-9 %d', [ProcessID]),
    '');
  {$ENDIF}
end;

class procedure TOSUtils.KillProcessByFilename(const Filename: String; const ExcludeCurrentProcess: Boolean);
{$IFNDEF MSWINDOWS}
begin
  // Do nothing
end;
{$ELSE}
  procedure KillAppProcess(AStruct: TProcessEntry32);
  begin
    if (SameText(AStruct.szExeFile, Filename)) and
       ((not ExcludeCurrentProcess) or
        (AStruct.th32ProcessID <> GetCurrentProcessId)) then
    begin
      KillProcess(AStruct.th32ProcessID);
    end;
  end;

var
  MyHandle: THandle;
  Struct: TProcessEntry32;
begin
  MyHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);

  Struct.dwSize := Sizeof(TProcessEntry32);

  if Process32First(MyHandle, Struct) then
  begin
    KillAppProcess(Struct);

    while Process32Next(MyHandle, Struct) do
    begin
      KillAppProcess(Struct);
    end;
  end;
end;
{$ENDIF}

end.
