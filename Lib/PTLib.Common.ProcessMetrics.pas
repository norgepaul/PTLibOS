{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.ProcessMetrics                              }
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

unit PTLib.Common.ProcessMetrics;

interface

uses
  System.SysUtils, System.Generics.Collections,

  {$IFDEF MSWINDOWS}
  WinAPI.Windows, WinAPI.psAPI,
  {$ENDIF}

  PTLib.Common.Utils,
  PTLib.Common.Strings,
  PTLib.Common.Files;

type
  TCPUUsageData = class
  strict private
    FPID: Cardinal;
    FHandle: Cardinal;
    FOldUser: Int64;
    FOldKernel: Int64;
    FLastUpdateTime: cardinal;
    FLastUsage: Single;
  public
    property PID: Cardinal read FPID write FPID;
    property Handle: Cardinal read FHandle write FHandle;
    property OldUser: Int64 read FOldUser write FOldUser;
    property OldKernel: Int64 read FOldKernel write FOldKernel;
    property LastUpdateTime: cardinal read FLastUpdateTime write FLastUpdateTime;
    property LastUsage: Single read FLastUsage write FLastUsage;
  end;

  TProcessMetrics = class
  strict private
    class var FProcessCPUUsage: TObjectDictionary<NativeUInt, TCPUUsageData>;
  private
    {$IFDEF MSWINDOWS}
    class function GetOrCreateUsageCounter(const PID: Cardinal; out CPUUsageData: TCPUUsageData): Boolean;
    {$ENDIF}
    {$IFDEF LINUX64}
    class function GetProcessValues(const PID: NativeUInt; out CPU: Single; out Mem: NativeUInt): Boolean;
    {$ENDIF}
  public
    {$WARN UNSUPPORTED_CONSTRUCT OFF}
    class constructor Create;
    class destructor Destroy;
    {$WARN UNSUPPORTED_CONSTRUCT ON}

    class function GetMemoryUsed: NativeUInt; overload;
    class function GetMemoryUsed(const PID: NativeUInt): NativeUInt; overload;
    class function GetCpuUsage(const PID: Cardinal): Single; overload;
    class function GetCpuUsage: Single; overload;
    class function GetPID: NativeUInt;
  end;

implementation

const
  wsMinMeasurementInterval = 250;
  { minimum amount of time that must have elapsed to calculate CPU usage, miliseconds. If time elapsed is less than this, previous result is returned, or zero, if there is no previous result. }

{ TProcessMetrics }

{$WARN UNSUPPORTED_CONSTRUCT OFF}
class constructor TProcessMetrics.Create;
begin
  FProcessCPUUsage := TObjectDictionary<NativeUInt, TCPUUsageData>.Create([doOwnsValues]);
end;

class destructor TProcessMetrics.Destroy;
begin
  FreeAndNil(FProcessCPUUsage);
end;
{$WARN UNSUPPORTED_CONSTRUCT ON}

{$IFDEF MSWINDOWS}
class function TProcessMetrics.GetOrCreateUsageCounter(const PID: Cardinal; out CPUUsageData: TCPUUsageData): Boolean;
var
  CreationTime, ExitTime, KernelTime, UserTime: _FILETIME;
  AHandle: cardinal;
begin
  Result := FProcessCPUUsage.TryGetValue(PID, CPUUsageData);

  if not Result then
  begin
    Result := False;

    // We need a handle with PROCESS_QUERY_INFORMATION privileges
    AHandle := OpenProcess(PROCESS_QUERY_INFORMATION, false, PID);

    if AHandle <> 0 then
    begin
      CPUUsageData := TCPUUsageData.Create;

      CPUUsageData.PID := PID;
      CPUUsageData.Handle := AHandle;
      CPUUsageData.LastUpdateTime := GetTickCount;
      CPUUsageData.LastUsage := 0;

      if GetProcessTimes(CPUUsageData.Handle, CreationTime, ExitTime, KernelTime, UserTime) then
      begin
        // convert _FILETIME to Int64
        CPUUsageData.oldKernel := (Int64(KernelTime.dwLowDateTime) or (KernelTime.dwHighDateTime shr 32));
        CPUUsageData.oldUser := (Int64(UserTime.dwLowDateTime) or (UserTime.dwHighDateTime shr 32));

        FProcessCPUUsage.Add(PID, CPUUsageData);

        Result := True;
      end
      else
      begin
        FreeAndNil(CPUUsageData);
      end;
    end;
  end;
end;

class function TProcessMetrics.GetCpuUsage(const PID: Cardinal): Single;
var
  CreationTime, ExitTime, KernelTime, UserTime: _FILETIME;
  DeltaMs, ThisTime: cardinal;
  Kernel, User, Delta: Int64;
  UsageData: TCPUUsageData;
begin
  Result := 0;

  if GetOrCreateUsageCounter(PID, UsageData) then
  begin
    Result := UsageData.LastUsage;

    ThisTime := GetTickCount; // Get the time elapsed since last query

    DeltaMs := ThisTime - UsageData.LastUpdateTime;

    if DeltaMs >= wsMinMeasurementInterval then
    begin
      UsageData.LastUpdateTime := ThisTime;

      GetProcessTimes(UsageData.Handle, CreationTime, ExitTime, KernelTime, UserTime);

      // convert _FILETIME to Int64.
      Kernel := (Int64(KernelTime.dwLowDateTime) or (KernelTime.dwHighDateTime shr 32));
      User := (Int64(UserTime.dwLowDateTime) or (UserTime.dwHighDateTime shr 32));

      // get the delta
      Delta := User + Kernel - UsageData.oldUser - UsageData.oldKernel;

      UsageData.oldUser := User;
      UsageData.oldKernel := Kernel;

      // Delta is in units of 100 nanoseconds, so…
      Result := (Delta / DeltaMs) / 100;

      // just in case you want to use it later, too
      UsageData.LastUsage := Result;
    end;
  end;
end;

class function TProcessMetrics.GetMemoryUsed(
  const PID: NativeUInt): NativeUInt;
var
  PCB: PROCESS_MEMORY_COUNTERS;
  AHandle : cardinal;
begin
  Result := 0;

  // Open the process
  AHandle := OpenProcess(PROCESS_QUERY_INFORMATION, false, PID);

  if AHandle > 0 then
  begin
    // Get the memory information
    GetProcessMemoryInfo(AHandle,@PCB,sizeof(PCB));

    Result := PCB.WorkingSetSize;
  end;
end;

class function TProcessMetrics.GetMemoryUsed: NativeUInt;
var
  MemoryManagerState: TMemoryManagerState;
  SmallBlockTypeState: TSmallBlockTypeState;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  GetMemoryManagerState(MemoryManagerState);
  {$WARN SYMBOL_PLATFORM ON}

  Result := MemoryManagerState.TotalAllocatedMediumBlockSize + MemoryManagerState.TotalAllocatedLargeBlockSize;

  for SmallBlockTypeState in MemoryManagerState.SmallBlockTypeStates do
  begin
    Result := Result + SmallBlockTypeState.UseableBlockSize * SmallBlockTypeState.AllocatedBlockCount;
  end;
end;

class function TProcessMetrics.GetCpuUsage: Single;
begin
  Result := GetCpuUsage(GetCurrentProcessId);
end;

class function TProcessMetrics.GetPID: NativeUInt;
begin
  Result := GetCurrentProcessId
end;
{$ENDIF}

{$IFDEF LINUX64}
class function TProcessMetrics.GetProcessValues(const PID: NativeUInt; out CPU: Single; out Mem: NativeUInt): Boolean;
var
  AFound: Boolean;
  ACPU: Single;
  AMem: NativeUInt;
begin
  ACPU := 0;
  AMem := 0;
  AFound := False;

  TFileUtils.ExecuteCommand(
    'ps',
    '-p ' + PID.ToString + ' -o %cpu,%mem',
    '',
    procedure(const Handle: Pointer; const LineNumber: Integer; const Value: String; out Done: Boolean)
    var
      Text: String;
    begin
      if (Value.Length > 1) and (Value[low(Value)] <> '%') then
      begin
        Text := Trim(Value);

        ACPU := StrToFloatDefUseDot(Trim(NextBlock(Text, ' ')), 0);

        Text := Trim(Text);

        AMem := StrToInt64Def(Trim(NextBlock(Text, '.')), 0);

        AFound := True;
      end;
    end
  );

  CPU := ACPU;
  Mem := AMem;
  Result := AFound;
end;

class function TProcessMetrics.GetPID: NativeUInt;
var
  PID: NativeInt;
begin
  PID := 0;

  TFileUtils.ExecuteCommand(
    'pidof',
    ExtractFilename(ParamStr(0)),
    '',
    procedure(const Handle: Pointer; const LineNumber: Integer; const Value: String; out Done: Boolean)
    begin
      PID := StrToInt64Def(Value, 0);
    end
  );

  Result := PID;
end;

class function TProcessMetrics.GetCpuUsage(const PID: Cardinal): Single;
var
  CPU: Single;
  Mem: NativeUInt;
begin
  if GetProcessValues(PID, CPU, Mem) then
  begin
    Result := CPU;
  end
  else
  begin
    Result := 0;
  end;
end;

class function TProcessMetrics.GetCpuUsage: Single;
begin
  Result := GetCpuUsage(GetPID);
end;

class function TProcessMetrics.GetMemoryUsed(
  const PID: NativeUInt): NativeUInt;
var
  CPU: Single;
  Mem: NativeUInt;
begin
  if GetProcessValues(PID, CPU, Mem) then
  begin
    Result := Mem;
  end
  else
  begin
    Result := 0;
  end;
end;

class function TProcessMetrics.GetMemoryUsed: NativeUInt;
begin
  Result := GetMemoryUsed(GetPID);
end;

{$ENDIF}
(*
class function TProcessMetrics.GetCpuUsage(const PID: Cardinal): Single;
begin
  Result := 0;
end;

class function TProcessMetrics.GetMemoryUsed(
  const ProcessID: NativeUInt): NativeUInt;
begin
  Result := 0;
end;

class function TProcessMetrics.GetMemoryUsed: NativeUInt;
begin
  Result := 0;
end;
{$ENDIF}*)

end.
