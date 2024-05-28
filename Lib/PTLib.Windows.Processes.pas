{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Windows.Processes                                  }
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

unit PTLib.Windows.Processes;

interface

uses
  System.SysUtils,

  {$IFDEF MSWINDOWS}
  WinAPI.Windows, WinAPI.TLHelp32,

  System.Win.Registry,
  {$ENDIF}

  PTLib.Windows.Processes.API;

{$IFDEF MSWINDOWS}
function ExecuteProcess(const EXE: String; const AParams: string; const AJob: Boolean): TProcessInformation;
function RunningOnWINE: Boolean;
{$ENDIF}
function ExecuteProcessInternal(const EXE: String; const AParams: string; const AJob: Boolean): Cardinal;
procedure KillProcess(const ProcessID: Cardinal);
procedure KillProcessByFilename(const Filename: String; const ExcludeCurrentProcess: Boolean = True);
function GetProcessID(const Filename: String): Cardinal;


implementation

var
  JobLimit: TJobObjectExtendedLimitInformation;
  JobHandle: THandle;

{$IFDEF MSWINDOWS}
function RunningOnWINE: Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create();
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    Result := Reg.KeyExists('SOFTWARE\Wine\Wine\Config');
  finally
    FreeAndNil(Reg);
  end;
end;

function ExecuteProcess(const EXE: String;
  const AParams: string; const AJob: Boolean): TProcessInformation;
var
  SI : TStartupInfo;
  Flag: Cardinal;
  InJob: Bool;
begin
  FillChar(SI,SizeOf(SI),0);
  SI.cb := SizeOf(SI);

  if AJob then
  begin
    Flag := CREATE_BREAKAWAY_FROM_JOB;

    if JobHandle = 0 then
    begin
      JobHandle := CreateJobObject(nil, PChar(format('Global\%d', [GetCurrentProcessID])));

      if JobHandle <> 0 then
      begin
        JobLimit.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

        SetInformationJobObject(JobHandle, JobObjectExtendedLimitInformation, @JobLimit,
          SizeOf(TJobObjectExtendedLimitInformation));

        IsProcessInJob(GetCurrentProcess, 0, InJob);

        if not InJob then
          AssignProcessToJobObject(JobHandle, GetCurrentProcess);
      end;
    end;
  end
  else
  begin
    Flag := 0;
  end;

  Flag := Flag or CREATE_NO_WINDOW;

  SI.wShowWindow := SW_HIDE;

  if CreateProcess(
     nil,
     PChar(EXE + ' ' + AParams),
     nil,
     nil,
     False,
     Flag,
     nil,
     nil,
     SI,
     Result
     ) then
  begin
    { close thread handle }
    CloseHandle(Result.hThread);

    if AJob then
    begin
      AssignProcessToJobObject(JobHandle, Result.hProcess);
    end;
  end;
end;
{$ENDIF}

function ExecuteProcessInternal(const EXE: String;
  const AParams: string; const AJob: Boolean): Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := ExecuteProcess(
    EXE,
    AParams,
    AJob).dwProcessId;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

procedure KillProcessByFilename(const Filename: String; const ExcludeCurrentProcess: Boolean);
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

function GetProcessID(const Filename: String): Cardinal;
{$IFNDEF MSWINDOWS}
begin
  Result := 0;
end;
{$ELSE}
var
  MyHandle: THandle;
  Struct: TProcessEntry32;
begin
  Result := 0;

  MyHandle := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);

  Struct.dwSize := Sizeof(TProcessEntry32);

  if Process32First(MyHandle, Struct) then
  begin
    if SameText(Struct.szExeFile, Filename) then
    begin
      Result := Struct.th32ProcessID;
    end else
    begin
      while Process32Next(MyHandle, Struct) do
      begin
        if SameText(Struct.szExeFile, Filename) then
        begin
          Result := Struct.th32ProcessID;

          Break;
        end;
      end;
    end;
  end;
end;
{$ENDIF}

procedure KillProcess(const ProcessID: Cardinal);
begin
  ExecuteProcessInternal(
    'taskkill.exe',
    format('/f /im * /fi "PID eq %d"', [ProcessID]),
    False);
end;

end.
