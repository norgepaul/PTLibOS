{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Windows.Processes.API                              }
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

unit PTLib.Windows.Processes.API;

// Found here: http://stackoverflow.com/questions/8822152/how-to-create-a-child-process-depending-on-its-parent

interface

{$IFDEF MSWINDOWS}
uses
  WinAPI.Windows;

type
  TJobObjectInfoClass = Cardinal;

  PJobObjectAssociateCompletionPort = ^TJobObjectAssociateCompletionPort;

  TJobObjectAssociateCompletionPort = Record
    CompletionKey: Pointer;
    CompletionPort: THandle;
  End;

  PJobObjectBasicLimitInformation = ^TJobObjectBasicLimitInformation;

  TJobObjectBasicLimitInformation = packed Record
    PerProcessUserTimeLimit: TLargeInteger;
    PerJobUserTimeLimit: TLargeInteger;
    LimitFlags: DWORD;
    MinimumWorkingSetSize: DWORD;
    MaximumWorkingSetSize: DWORD;
    ActiveProcessLimit: DWORD;
    Affinity: DWORD;
    PriorityClass: DWORD;
    SchedulingClass: DWORD;
  End;

  PJobObjectBasicUIRestrictions = ^TJobObjectBasicUIRestrictions;

  TJobObjectBasicUIRestrictions = Record
    UIRestrictionsClass: DWORD;
  End;

  PJobObjectEndOfJobTimeInformation = ^TJobObjectEndOfJobTimeInformation;

  TJobObjectEndOfJobTimeInformation = Record
    EndOfJobTimeAction: DWORD;
  End;

  TIOCounters =
    Record { all   fields   should   be   actually   unsigned   int64 's }
    ReadOperationCount: Int64;
    WriteOperationCount: Int64;
    OtherOperationCount: Int64;
    ReadTransferCount: Int64;
    WriteTransferCount: Int64;
    OtherTransferCount: Int64;
  End;

  PJobObjectExtendedLimitInformation = ^TJobObjectExtendedLimitInformation;

  TJobObjectExtendedLimitInformation = Record
    BasicLimitInformation: TJobObjectBasicLimitInformation;
    IoInfo: TIOCounters;
    ProcessMemoryLimit: DWORD;
    JobMemoryLimit: DWORD;
    PeakProcessMemoryUsed: DWORD;
    PeakJobMemoryUsed: DWORD;
  End;

  PJobObjectSecurityLimitInformation = ^TJobObjectSecurityLimitInformation;

  TJobObjectSecurityLimitInformation = Record
    SecurityLimitFlags: DWORD;
    JobToken: THandle;
    SidsToDisable: PTokenGroups;
    PrivilegesToDelete: PTokenPrivileges;
    RestrictedSids: PTokenGroups;
  End;

  PJobObjectBasicAccountingInformation = ^TJobObjectBasicAccountingInformation;

  TJobObjectBasicAccountingInformation = Record
    TotalUserTime: TLargeInteger;
    TotalKernelTime: TLargeInteger;
    ThisPeriodTotalUserTime: TLargeInteger;
    ThisPeriodTotalKernelTime: TLargeInteger;
    TotalPageFaultCount: DWORD;
    TotalProcesses: DWORD;
    ActiveProcesses: DWORD;
    TotalTerminatedProcesses: DWORD;
  End;

  PJobObjectBasicAndIOAccountingInformation = ^
    TJobObjectBasicAndIOAccountingInformation;

  TJobObjectBasicAndIOAccountingInformation = Record
    BasicInfo: TJobObjectBasicAccountingInformation;
    IoInfo: TIOCounters;
  End;

  PJobObjectBasicProcessIDList = ^TJobObjectBasicProcessIDList;

  TJobObjectBasicProcessIDList = Record
    NumberOfAssignedProcesses: DWORD;
    NumberOfProcessIdsInList: DWORD;
    ProcessIdList: Array [0 .. 0] of ULONG;
  End;

const
  AWSuffix = 'W';

const
  { for   TJobObjectInfoClass }
  JobObjectBasicAccountingInformation = 1;
  JobObjectBasicLimitInformation = 2;
  JobObjectBasicProcessIdList = 3;
  JobObjectBasicUIRestrictions = 4;
  JobObjectSecurityLimitInformation = 5;
  JobObjectEndOfJobTimeInformation = 6;
  JobObjectAssociateCompletionPortInformation = 7;
  JobObjectBasicAndIoAccountingInformation = 8;
  JobObjectExtendedLimitInformation = 9;
  MaxJobObjectInfoClass = 10;

  JOB_OBJECT_ASSIGN_PROCESS = $0001;
{$EXTERNALSYM JOB_OBJECT_ASSIGN_PROCESS}
  JOB_OBJECT_SET_ATTRIBUTES = $0002;
{$EXTERNALSYM JOB_OBJECT_SET_ATTRIBUTES}
  JOB_OBJECT_QUERY = $0004;
{$EXTERNALSYM JOB_OBJECT_QUERY}
  JOB_OBJECT_TERMINATE = $0008;
{$EXTERNALSYM JOB_OBJECT_TERMINATE}
  JOB_OBJECT_SET_SECURITY_ATTRIBUTES = $0010;
{$EXTERNALSYM JOB_OBJECT_SET_SECURITY_ATTRIBUTES}
  JOB_OBJECT_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1F;
{$EXTERNALSYM JOB_OBJECT_ALL_ACCESS}
  JOB_OBJECT_TERMINATE_AT_END_OF_JOB = 0;
  JOB_OBJECT_POST_AT_END_OF_JOB = 1;
  JOB_OBJECT_MSG_END_OF_JOB_TIME = 1;
  JOB_OBJECT_MSG_END_OF_PROCESS_TIME = 2;
  JOB_OBJECT_MSG_ACTIVE_PROCESS_LIMIT = 3;
  JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO = 4; { where 's   5? }
  JOB_OBJECT_MSG_NEW_PROCESS = 6;
  JOB_OBJECT_MSG_EXIT_PROCESS = 7;
  JOB_OBJECT_MSG_ABNORMAL_EXIT_PROCESS = 8;
  JOB_OBJECT_MSG_PROCESS_MEMORY_LIMIT = 9;
  JOB_OBJECT_MSG_JOB_MEMORY_LIMIT = 10;
  JOB_OBJECT_LIMIT_WORKINGSET = $00000001;
  JOB_OBJECT_LIMIT_PROCESS_TIME = $00000002;
  JOB_OBJECT_LIMIT_JOB_TIME = $00000004;
  JOB_OBJECT_LIMIT_ACTIVE_PROCESS = $00000008;
  JOB_OBJECT_LIMIT_AFFINITY = $00000010;
  JOB_OBJECT_LIMIT_PRIORITY_CLASS = $00000020;
  JOB_OBJECT_LIMIT_PRESERVE_JOB_TIME = $00000040;
  JOB_OBJECT_LIMIT_SCHEDULING_CLASS = $00000080;
  JOB_OBJECT_LIMIT_RESERVED1 = $00002000;
  JOB_OBJECT_LIMIT_RESERVED2 = $00004000;
  JOB_OBJECT_LIMIT_RESERVED3 = $00008000;
  JOB_OBJECT_LIMIT_RESERVED4 = $00010000;
  JOB_OBJECT_LIMIT_RESERVED5 = $00020000;
  JOB_OBJECT_LIMIT_RESERVED6 = $00040000;
  JOB_OBJECT_LIMIT_VALID_FLAGS = $0007FFFF;
  JOB_OBJECT_BASIC_LIMIT_VALID_FLAGS = $000000FF;
  JOB_OBJECT_EXTENDED_LIMIT_VALID_FLAGS = $00001FFF;
  JOB_OBJECT_RESERVED_LIMIT_VALID_FLAGS = $0007FFFF;
  JOB_OBJECT_UILIMIT_NONE = $00000000;
  JOB_OBJECT_UILIMIT_HANDLES = $00000001;
  JOB_OBJECT_UILIMIT_READCLIPBOARD = $00000002;
  JOB_OBJECT_UILIMIT_WRITECLIPBOARD = $00000004;
  JOB_OBJECT_UILIMIT_SYSTEMPARAMETERS = $00000008;
  JOB_OBJECT_UILIMIT_DISPLAYSETTINGS = $00000010;
  JOB_OBJECT_UILIMIT_GLOBALATOMS = $00000020;
  JOB_OBJECT_UILIMIT_DESKTOP = $00000040;
  JOB_OBJECT_UILIMIT_EXITWINDOWS = $00000080;
  JOB_OBJECT_UILIMIT_ALL = $000000FF;
  JOB_OBJECT_UI_VALID_FLAGS = $000000FF;
  JOB_OBJECT_SECURITY_NO_ADMIN = $00000001;
  JOB_OBJECT_SECURITY_RESTRICTED_TOKEN = $00000002;
  JOB_OBJECT_SECURITY_ONLY_TOKEN = $00000004;
  JOB_OBJECT_SECURITY_FILTER_TOKENS = $00000008;
  JOB_OBJECT_SECURITY_VALID_FLAGS = $0000000F;
  CREATE_BREAKAWAY_FROM_JOB = $01000000;
  //
  // Extended Limits
  //

  JOB_OBJECT_LIMIT_PROCESS_MEMORY = $00000100;
{$EXTERNALSYM JOB_OBJECT_LIMIT_PROCESS_MEMORY}
  JOB_OBJECT_LIMIT_JOB_MEMORY = $00000200;
{$EXTERNALSYM JOB_OBJECT_LIMIT_JOB_MEMORY}
  JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION = $00000400;
{$EXTERNALSYM JOB_OBJECT_LIMIT_DIE_ON_UNHANDLED_EXCEPTION}
  JOB_OBJECT_LIMIT_BREAKAWAY_OK = $00000800;
{$EXTERNALSYM JOB_OBJECT_LIMIT_BREAKAWAY_OK}
  JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK = $00001000;
{$EXTERNALSYM JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK}
  JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE = $00002000;
{$EXTERNALSYM JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE}
function CreateJobObjectA(lpJobAttributes: PSecurityAttributes; lpName: LPCSTR)
  : THandle; stdcall;
{$EXTERNALSYM CreateJobObjectA}
function CreateJobObjectW(lpJobAttributes: PSecurityAttributes; lpName: LPCWSTR)
  : THandle; stdcall;
{$EXTERNALSYM CreateJobObjectW}
function CreateJobObject(lpJobAttributes: PSecurityAttributes; lpName: LPCTSTR)
  : THandle; stdcall;
{$EXTERNALSYM CreateJobObject}
function OpenJobObjectA(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCSTR): THandle; stdcall;
{$EXTERNALSYM OpenJobObjectA}
function OpenJobObjectW(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCWSTR): THandle; stdcall;
{$EXTERNALSYM OpenJobObjectW}
function OpenJobObject(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCTSTR): THandle; stdcall;
{$EXTERNALSYM OpenJobObject}
function AssignProcessToJobObject(hJob, hProcess: THandle): BOOL; stdcall;
{$EXTERNALSYM AssignProcessToJobObject}
function TerminateJobObject(hJob: THandle; uExitCode: UINT): BOOL; stdcall;
{$EXTERNALSYM TerminateJobObject}
function IsProcessInJob(ProcessHandle, JobHandle: THandle; var Result_: BOOL)
  : BOOL; stdcall;
{$EXTERNALSYM IsProcessInJob}
Function QueryInformationJobObject(hJob: THandle;
  JobObjectInformationClass: TJobObjectInfoClass;
  lpJobObjectInformation: Pointer; cbJobObjectInformationLength: DWORD;
  lpReturnLength: PDWORD): BOOL; StdCall;
  External Kernel32 Name 'QueryInformationJobObject';

Function SetInformationJobObject(hJob: THandle;
  JobObjectInformationClass: TJobObjectInfoClass;
  lpJobObjectInformation: Pointer; cbJobObjectInformationLength: DWORD): BOOL;
  StdCall; External Kernel32 Name 'SetInformationJobObject';

function CreateJobObjectA; external Kernel32 name 'CreateJobObjectA';
function CreateJobObjectW; external Kernel32 name 'CreateJobObjectW';
function CreateJobObject; external Kernel32 name 'CreateJobObject' + AWSuffix;
function OpenJobObjectA; external Kernel32 name 'OpenJobObjectA';
function OpenJobObjectW; external Kernel32 name 'OpenJobObjectW';
function OpenJobObject; external Kernel32 name 'OpenJobObject' + AWSuffix;
function AssignProcessToJobObject;
external Kernel32 name 'AssignProcessToJobObject';
function TerminateJobObject; external Kernel32 name 'TerminateJobObject';
function IsProcessInJob; external Kernel32 name 'IsProcessInJob';

{$ENDIF}

implementation

end.
