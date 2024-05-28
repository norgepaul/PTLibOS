{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppInst.pas, released on 2003-10-07.

The Initial Developer of the Original Code is Andreas Hausladen [Andreas dott Hausladen att gmx dott de]
Portions created by Andreas Hausladen are Copyright (C) 2003 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit PTLib.Windows.Application.Instance;

interface

uses
  Windows, Messages, Vcl.Forms,
  Classes,

  PTLib.Common.Classes;

const
  AI_INSTANCECREATED = $0001;
  AI_INSTANCEDESTROYED = $0002;
  AI_USERMSG = $0003;

  AppInstDataKindNoData = -1;
  AppInstCmdLineDataKind = 1;
  AllocGranularity: Cardinal = 1;
  DefaultCritSectSpinCount = 4000;

  SECURITY_WORLD_SID_AUTHORITY: TSidIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 1));
  SECURITY_WORLD_RID = ($00000000);

  cUtilWindowExClass: TWndClass = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: SizeOf(TMethod);
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindowEx');

{$WARN SYMBOL_PLATFORM OFF}

type
  TJclAppInstances = class;
  TJclFileMappingView = class;

  TJclAddr64 = Cardinal;
  TJclAddr = TJclAddr64;
  TJclULargeInteger = ULARGE_INTEGER;
  TJclAppInstDataKind = Integer;
  TJclWaitResult = (wrAbandoned, wrError, wrIoCompletion, wrSignaled, wrTimeout);
  TJclWaitHandle = THandle;

  POptexSharedInfo = ^TOptexSharedInfo;
  TOptexSharedInfo = record
    SpinCount: Integer;      // number of times to try and enter the optex before
                             // waiting on kernel event, 0 on single processor
    LockCount: Integer;      // count of enter attempts
    ThreadId: Longword;      // id of thread that owns the optex, 0 if free
    RecursionCount: Integer; // number of times the optex is owned, 0 if free
  end;

  TJvAppInstDataKind = TJclAppInstDataKind; // = Integer

  TJclFileMappingRoundOffset = (rvDown, rvUp);

  TInstanceChangeEvent = procedure(Sender: TObject; ProcessId: Cardinal) of object;
  TUserNotifyEvent = procedure(Sender: TObject; Param: Integer) of object;
  TDataAvailableEvent = procedure(Sender: TObject; Kind: TJvAppInstDataKind;
    Data: Pointer; Size: Integer) of object;
    { Data contains the sent data and is released when the function returns }
  TCmdLineReceivedEvent = procedure(Sender: TObject; CmdLine: TStrings) of object;

  { TJvAppInstance encapsulates the TJclAppInstance class. To set a
    UniqueAppIdGuidStr you must call JclAppInst.JclAppInstances in the
    initialization section of a unit or before the forms are created (OnCreate
    is too late).
    This class is not thread safe. }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32)]
  {$ENDIF RTL230_UP}
  TPTLibAppInstances = class(TComponent)
  private
    FHandle: THandle;
    FOnInstanceCreated: TInstanceChangeEvent;
    FOnInstanceDestroyed: TInstanceChangeEvent;
    FOnUserNotify: TUserNotifyEvent;
    FOnDataAvailable: TDataAvailableEvent;
    FOnCmdLineReceived: TCmdLineReceivedEvent;
    FOnRejected: TNotifyEvent;
    FAutoActivate: Boolean;
    FMaxInstances: Integer;
    FActive: Boolean;
    FSendCmdLine: Boolean;
    FAppInstances: TJclAppInstances; // don't free, it's a singleton

    function GetAppInstances: TJclAppInstances;
    function GetUniqueAppId: string;
  protected
    procedure Loaded; override;
    procedure WndProc(var Msg: TMessage); virtual;
    function GetIsRemoteInstanceActive: Boolean;
    procedure DoInstanceCreated(ProcessId: Cardinal); virtual;
    procedure DoInstanceDestroyed(ProcessId: Cardinal); virtual;
    procedure DoUserNotify(Param: Integer); virtual;
    procedure DoDataAvailable(Kind: TJvAppInstDataKind; Data: Pointer; Size: Integer); virtual;
    procedure DoCmdLineReceived(CmdLine: TStrings); virtual;
    procedure DoRejected; virtual;
    property Handle: THandle read FHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Check;
    procedure UserNotify(Param: Integer);
    function SendData(DataKind: TJclAppInstDataKind; Data: Pointer; Size: Integer): Boolean;
    property AppInstances: TJclAppInstances read GetAppInstances;
  published
    property Active: Boolean read FActive write FActive default True;
    property AutoActivate: Boolean read FAutoActivate write FAutoActivate default True;
     { AutoActivate: True means that the first instance is brought to front
       by the second process instance. }
    property MaxInstances: Integer read FMaxInstances write FMaxInstances default 1;
     { MaxInstances: 0 means no restriction }
    property SendCmdLine: Boolean read FSendCmdLine write FSendCmdLine default True;
     { SendCmdLine: True means that the second process instance sends it's
       CmdLine to the first instance before it terminates. }
    property UniqueAppId: string read GetUniqueAppId;
    property OnInstanceCreated: TInstanceChangeEvent read FOnInstanceCreated write FOnInstanceCreated;
    property OnInstanceDestroyed: TInstanceChangeEvent read FOnInstanceDestroyed write FOnInstanceDestroyed;
    property OnUserNotify: TUserNotifyEvent read FOnUserNotify write FOnUserNotify;
    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnCmdLineReceived: TCmdLineReceivedEvent read FOnCmdLineReceived write FOnCmdLineReceived;
    property OnRejected: TNotifyEvent read FOnRejected write FOnRejected;
  end;

  TJclCustomFileMapping = class(TObject)
  private
    FExisted: Boolean;
    FHandle: THandle;
    FName: string;
    FRoundViewOffset: TJclFileMappingRoundOffset;
    FViews: TList;
    function GetCount: Integer;
    function GetView(Index: Integer): TJclFileMappingView;
  protected
    procedure ClearViews;
    procedure InternalCreate(const FileHandle: THandle; const Name: string;
      const Protect: Cardinal; MaximumSize: Int64; SecAttr: PSecurityAttributes);
    procedure InternalOpen(const Name: string; const InheritHandle: Boolean;
      const DesiredAccess: Cardinal);
  public
    constructor Create;
    constructor Open(const Name: string; const InheritHandle: Boolean; const DesiredAccess: Cardinal);
    destructor Destroy; override;
    function Add(const Access, Count: Cardinal; const Offset: Int64): Integer;
    function AddAt(const Access, Count: Cardinal; const Offset: Int64; const Address: Pointer): Integer;
    procedure Delete(const Index: Integer);
    function IndexOf(const View: TJclFileMappingView): Integer;
    property Count: Integer read GetCount;
    property Existed: Boolean read FExisted;
    property Handle: THandle read FHandle;
    property Name: string read FName;
    property RoundViewOffset: TJclFileMappingRoundOffset read FRoundViewOffset write FRoundViewOffset;
    property Views[index: Integer]: TJclFileMappingView read GetView;
  end;

  TJclFileMappingView = class(TCustomMemoryStream)
  private
    FFileMapping: TJclCustomFileMapping;
    FOffsetHigh: Cardinal;
    FOffsetLow: Cardinal;
    function GetIndex: Integer;
    function GetOffset: Int64;
  public
    constructor Create(const FileMap: TJclCustomFileMapping;
      Access, Size: Cardinal; ViewOffset: Int64);
    constructor CreateAt(FileMap: TJclCustomFileMapping; Access,
      Size: Cardinal; ViewOffset: Int64; Address: Pointer);
    destructor Destroy; override;
    function Flush(const Count: Cardinal): Boolean;
    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    function Write(const Buffer; Count: Longint): Longint; override;
    property Index: Integer read GetIndex;
    property FileMapping: TJclCustomFileMapping read FFileMapping;
    property Offset: Int64 read GetOffset;
  end;

  TJclFileMapping = class(TJclCustomFileMapping)
  private
    FFileHandle: THandle;
  public
    constructor Create(const FileName: string; FileMode: Cardinal;
      const Name: string; Protect: Cardinal; const MaximumSize: Int64;
      SecAttr: PSecurityAttributes); overload;
    constructor Create(const FileHandle: THandle; const Name: string;
      Protect: Cardinal; const MaximumSize: Int64;
      SecAttr: PSecurityAttributes); overload;
    destructor Destroy; override;
    property FileHandle: THandle read FFileHandle;
  end;

  TJclDispatcherObject = class(TObject)
  private
    FExisted: Boolean;
    FHandle: TJclWaitHandle;
    FName: string;
  public
    constructor Attach(AHandle: TJclWaitHandle);
    destructor Destroy; override;
    //function MsgWaitFor(const TimeOut: Cardinal): TJclWaitResult; Mask: DWORD): TJclWaitResult;
    //function MsgWaitForEx(const TimeOut: Cardinal): TJclWaitResult; Mask: DWORD): TJclWaitResult;
    function SignalAndWait(const Obj: TJclDispatcherObject; TimeOut: Cardinal;
      Alertable: Boolean): TJclWaitResult;
    function WaitAlertable(const TimeOut: Cardinal): TJclWaitResult;
    function WaitFor(const TimeOut: Cardinal): TJclWaitResult;
    function WaitForever: TJclWaitResult;
    property Existed: Boolean read FExisted;
    property Handle: TJclWaitHandle read FHandle;
    property Name: string read FName;
  end;

  TJclSwapFileMapping = class(TJclCustomFileMapping)
  public
    constructor Create(const Name: string; Protect: Cardinal;
      const MaximumSize: Int64; SecAttr: PSecurityAttributes);
  end;

  TJclEvent = class(TJclDispatcherObject)
  public
    constructor Create(SecAttr: PSecurityAttributes; Manual, Signaled: Boolean; const Name: string);
    constructor Open(Access: Cardinal; Inheritable: Boolean; const Name: string);
    function Pulse: Boolean;
    function ResetEvent: Boolean;
    function SetEvent: Boolean;
  end;

  TJclOptex = class(TObject)
  private
    FEvent: TJclEvent;
    FExisted: Boolean;
    FFileMapping: THandle;
    FName: string;
    FSharedInfo: POptexSharedInfo;
    function GetUniProcess: Boolean;
    function GetSpinCount: Integer;
    procedure SetSpinCount(Value: Integer);
  public
    constructor Create(const Name: string = ''; SpinCount: Integer = 4000);
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    function TryEnter: Boolean;
    property Existed: Boolean read FExisted;
    property Name: string read FName;
    property SpinCount: Integer read GetSpinCount write SetSpinCount;
    property UniProcess: Boolean read GetUniProcess;
  end;

  TJclAppInstances = class(TObject)
  private
    FCPID: DWORD;
    FAllMapping: TJclSwapFileMapping;
    FAllMappingView: TJclFileMappingView;
    FSessionMapping: TJclSwapFileMapping;
    FSessionMappingView: TJclFileMappingView;
    FUserMapping: TJclSwapFileMapping;
    FUserMappingView: TJclFileMappingView;
    FMessageID: DWORD;
    FOptex: TJclOptex;
    FUniqueAppID: string;
    function GetAllAppWnds(Index: Integer): THandle;
    function GetAllInstanceCount: Integer;
    function GetAllInstanceIndex(ProcessID: DWORD): Integer;
    function GetAllProcessIDs(Index: Integer): DWORD;
    function GetInstanceCount(MappingView: TJclFileMappingView): Integer;
    function GetInstanceIndex(MappingView: TJclFileMappingView; ProcessID: DWORD): Integer;
    function GetProcessIDs(MappingView: TJclFileMappingView; Index: Integer): DWORD;
    function GetSessionAppWnds(Index: Integer): THandle;
    function GetSessionInstanceCount: Integer;
    function GetSessionInstanceIndex(ProcessID: DWORD): Integer;
    function GetSessionProcessIDs(Index: Integer): DWORD;
    function GetUserAppWnds(Index: Integer): THandle;
    function GetUserInstanceCount: Integer;
    function GetUserInstanceIndex(ProcessID: DWORD): Integer;
    function GetUserProcessIDs(Index: Integer): DWORD;
  protected
    procedure InitData;
    procedure InitAllData;
    procedure InitSessionData;
    procedure InitUserData;
    procedure NotifyInstances(const W, L: Longint);
    procedure RemoveInstance(MappingView: TJclFileMappingView);
    procedure SecurityFree(UserInfo: PTokenUser; SID: PSID; ACL: PACL;
      SecurityDescriptor: PSecurityDescriptor; SecurityAttributes: PSecurityAttributes);
    procedure SecurityGetAllUsers(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
      out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
    procedure SecurityGetCurrentUser(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
      out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
    procedure SecurityGetCurrentUserInfo(out UserInfo: PTokenUser);
    procedure SecurityGetSecurityAttributes(OwnerSID, AccessSID: PSID; out ACL: PACL;
      out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
  public
    constructor Create;
    destructor Destroy; override;
    class function BringAppWindowToFront(const Wnd: THandle): Boolean;
    class function GetApplicationWnd(const ProcessID: DWORD): THandle;
    class procedure KillInstance;
    class function SetForegroundWindow98(const Wnd: THandle): Boolean;
    function CheckInstance(MaxInstances: Word; MaxSessionInstances: Word = 0;
      MaxUserInstances: Word = 0): Boolean;
    procedure CheckMultipleInstances(MaxInstances: Word; MaxSessionInstances: Word = 0;
      MaxUserInstances: Word = 0);
    procedure CheckSingleInstance;
    function SendCmdLineParams(const WindowClassName: string; const OriginatorWnd: THandle): Boolean;
    function SendData(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      Data: Pointer; const Size: Integer;
      OriginatorWnd: THandle): Boolean;
    function SendString(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      const S: string; OriginatorWnd: THandle): Boolean;
    function SendStrings(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      const Strings: TStrings; OriginatorWnd: THandle): Boolean;
    function SessionSwitchTo(Index: Integer): Boolean;
    function SwitchTo(Index: Integer): Boolean;
    function UserSwitchTo(Index: Integer): Boolean;
    procedure UserNotify(Param: Longint);
    property AppWnds[Index: Integer]: THandle read GetAllAppWnds;
    property InstanceIndex[ProcessID: DWORD]: Integer read GetAllInstanceIndex;
    property InstanceCount: Integer read GetAllInstanceCount;
    property MessageID: DWORD read FMessageID;
    property ProcessIDs[Index: Integer]: DWORD read GetAllProcessIDs;
    property SessionAppWnds[Index: Integer]: THandle read GetSessionAppWnds;
    property SessionInstanceIndex[ProcessID: DWORD]: Integer read GetSessionInstanceIndex;
    property SessionInstanceCount: Integer read GetSessionInstanceCount;
    property SessionProcessIDs[Index: Integer]: DWORD read GetSessionProcessIDs;
    property UserAppWnds[Index: Integer]: THandle read GetUserAppWnds;
    property UserInstanceIndex[ProcessID: DWORD]: Integer read GetUserInstanceIndex;
    property UserInstanceCount: Integer read GetUserInstanceCount;
    property UserProcessIDs[Index: Integer]: DWORD read GetUserProcessIDs;
  end;

function JclAppInstances: TJclAppInstances; overload;
function JclAppInstances(const UniqueAppIdGuidStr: string): TJclAppInstances; overload;

// Interprocess communication routines
function ReadMessageCheck(var Message: TMessage; const IgnoredOriginatorWnd: THandle): TJclAppInstDataKind;
procedure ReadMessageData(const Message: TMessage; var Data: Pointer; var Size: Integer);
procedure ReadMessageString(const Message: TMessage; out S: string);
procedure ReadMessageStrings(const Message: TMessage; const Strings: TStrings);

function SendData(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Data: Pointer; const Size: Integer): Boolean;
function SendStrings(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings): Boolean;
function SendCmdLineParams(const Wnd, OriginatorWnd: HWND): Boolean;
function SendString(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const S: string): Boolean;
procedure SetUniqueAppId(const AUniqueAppId: string);

implementation

uses
  SysUtils;

const
  sAppInstancesWindowClassName = 'JvAppInstances_WindowClass'; // do not localize
  AI_GETACTIVE = $0004;
  AI_SETACTIVE = $0005;

var
  FirstJvAppInstance: Boolean = True;
  FUniqueAppId: string;
  ProcessorCount: Cardinal = 0;

function StdWndProc(Window: THandle; Message, WParam: WPARAM;
  LParam: LPARAM): LRESULT; stdcall;
var
  Msg: Messages.TMessage;
  WndProc: TWndMethod;
begin
  TMethod(WndProc).Code := Pointer(GetWindowLongPtr(Window, 0));
  TMethod(WndProc).Data := Pointer(GetWindowLongPtr(Window, SizeOf(Pointer)));
  if Assigned(WndProc) then
  begin
    Msg.Msg := Message;
    Msg.WParam := WParam;
    Msg.LParam := LParam;
    Msg.Result := 0;
    WndProc(Msg);
    Result := Msg.Result;
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

procedure DeallocateHWndEx(Wnd: THandle);
begin
  Windows.DestroyWindow(Wnd);
end;

function AllocateHWndEx(Method: TWndMethod; const AClassName: string = ''): THandle;
var
  TempClass: TWndClass;
  UtilWindowExClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowExClass := cUtilWindowExClass;
  UtilWindowExClass.hInstance := HInstance;
  UtilWindowExClass.lpfnWndProc := @DefWindowProc;
  if AClassName <> '' then
    UtilWindowExClass.lpszClassName := PChar(AClassName);

  ClassRegistered := Windows.GetClassInfo(HInstance, UtilWindowExClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(UtilWindowExClass.lpszClassName, HInstance);
    Windows.RegisterClass(UtilWindowExClass);
  end;
  Result := Windows.CreateWindowEx(Windows.WS_EX_TOOLWINDOW, UtilWindowExClass.lpszClassName,
    '', Windows.WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);

  if Assigned(Method) then
  begin
    SetWindowLongPtr(Result, 0, LONG_PTR(TMethod(Method).Code));
    SetWindowLongPtr(Result, SizeOf(TMethod(Method).Code), LONG_PTR(TMethod(Method).Data));
    SetWindowLongPtr(Result, GWL_WNDPROC, LONG_PTR(@StdWndProc));
  end;
end;

function LockedCompareExchange(var Target: Integer; Exch, Comp: Integer): Integer;
asm
        {$IFDEF CPU32}
        // --> EAX Target
        //     EDX Exch
        //     ECX Comp
        // <-- EAX Result
        XCHG    EAX, ECX
        //     EAX Comp
        //     EDX Exch
        //     ECX Target
        LOCK CMPXCHG [ECX], EDX
        {$ENDIF CPU32}
        {$IFDEF CPU64}
        // --> RCX Target
        //     EDX Exch
        //     R8  Comp
        // <-- EAX Result
        MOV     RAX, R8
        //     RCX Target
        //     EDX Exch
        //     RAX Comp
        LOCK CMPXCHG [RCX], EDX
        {$ENDIF CPU64}
end;

procedure CardinalsToI64(out I: Int64; const LowPart, HighPart: Cardinal);
begin
  TJclULargeInteger(I).LowPart := LowPart;
  TJclULargeInteger(I).HighPart := HighPart;
end;

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
begin
  if (Value mod AllocGranularity) <> 0 then
    if Up then
      Value := ((Value div AllocGranularity) + 1) * AllocGranularity
    else
      Value := (Value div AllocGranularity) * AllocGranularity;
end;

procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);
var
  Addr: TJclAddr;
begin
  Addr := TJclAddr(Value);
  if (Addr mod AllocGranularity) <> 0 then
  begin
    if Up then
      Addr := ((Addr div AllocGranularity) + 1) * AllocGranularity
    else
      Addr := (Addr div AllocGranularity) * AllocGranularity;
    Value := Pointer(Addr);
  end;
end;

procedure I64ToCardinals(I: Int64; out LowPart, HighPart: Cardinal);
begin
  LowPart := TJclULargeInteger(I).LowPart;
  HighPart := TJclULargeInteger(I).HighPart;
end;

function SIDToString(ASID: PSID): string;
var
  SidIdAuthority: PSIDIdentifierAuthority;
  SubAuthorities, SidRev, SidSize: DWORD;
  Counter: Integer;
begin
  SidRev := 1;

  // Validate the binary SID.
  if not IsValidSid(ASid) then
    Raise EPTLibError.Create('Invalid SID');

  // Get the identifier authority value from the SID.
  SidIdAuthority := GetSidIdentifierAuthority(ASid);

  // Get the number of subauthorities in the SID.
  SubAuthorities := GetSidSubAuthorityCount(ASid)^;

  //Compute the buffer length.
  // S-SID_REVISION- + IdentifierAuthority- + subauthorities- + NULL
  SidSize := (15 + 12 + (12 * SubAuthorities) + 1) * SizeOf(CHAR);

  SetLength(Result, SidSize+1);

  // Add 'S' prefix and revision number to the string.
  Result := Format('S-%u-',[SidRev]);

  // Add SID identifier authority to the string.
  if (SidIdAuthority^.Value[0] <> 0) or (SidIdAuthority^.Value[1] <> 0) then
    Result := Result + AnsiLowerCase(Format('0x%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x',
        [USHORT(SidIdAuthority^.Value[0]),
         USHORT(SidIdAuthority^.Value[1]),
         USHORT(SidIdAuthority^.Value[2]),
         USHORT(SidIdAuthority^.Value[3]),
         USHORT(SidIdAuthority^.Value[4]),
         USHORT(SidIdAuthority^.Value[5])]))
  else
    Result := Result + Format('%u',
        [ULONG(SidIdAuthority^.Value[5])+
         ULONG(SidIdAuthority^.Value[4] shl 8)+
         ULONG(SidIdAuthority^.Value[3] shl 16)+
         ULONG(SidIdAuthority^.Value[2] shl 24)]);

  // Add SID subauthorities to the string.
  for Counter := 0 to SubAuthorities-1 do
    Result := Result + Format('-%u',[GetSidSubAuthority(ASid, Counter)^]);
end;

procedure LookupAccountBySid(Sid: PSID; out Name, Domain: WideString; Silent: Boolean);
var
  NameSize, DomainSize: DWORD;
  Use: SID_NAME_USE;
  Success: Boolean;
begin
  NameSize := 0;
  DomainSize := 0;
  Use := SidTypeUnknown;
  LookupAccountSidW(nil, Sid, nil, NameSize, nil, DomainSize, Use);
  if NameSize > 0 then
    SetLength(Name, NameSize - 1);
  if DomainSize > 0 then
    SetLength(Domain, DomainSize - 1);
  Success := LookupAccountSidW(nil, Sid, PWideChar(Name), NameSize, PWideChar(Domain), DomainSize, Use);
  if Silent and not Success then
  begin
    Name := WideString(SIDToString(Sid));
    Domain := '';
  end
  else
    Win32Check(Success);
end;

function CharReplace(var S: string; const Search, Replace: Char): Integer;
var
  P: PChar;
  Index, Len: Integer;
begin
  Result := 0;
  if Search <> Replace then
  begin
    UniqueString(S);
    P := PChar(S);
    Len := Length(S);
    for Index := 0 to Len - 1 do
    begin
      if P^ = Search then
      begin
        P^ := Replace;
        Inc(Result);
      end;
      Inc(P);
    end;
  end;
end;

procedure SetUniqueAppId(const AUniqueAppId: string);
begin
  FUniqueAppId := AUniqueAppId;
end;

constructor TPTLibAppInstances.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (csDesigning in ComponentState) then
  begin
    FHandle := AllocateHWndEx(WndProc, sAppInstancesWindowClassName);
    if FirstJvAppInstance then
    begin
      FirstJvAppInstance := False;
      AppInstances.CheckInstance($FFFF); // increase shared instance count
    end;
  end;

  FActive := True;
  FMaxInstances := 1;
  FAutoActivate := True;
  FSendCmdLine := True;
end;

destructor TPTLibAppInstances.Destroy;
begin
  if not (csDesigning in ComponentState) then
    DeallocateHWndEx(FHandle);
  inherited Destroy;
end;

procedure TPTLibAppInstances.Check;
begin
  if Active and not (csDesigning in ComponentState) then
    if MaxInstances > 0 then
      if AppInstances.InstanceIndex[GetCurrentProcessId] >= MaxInstances then  // Mantis 3990
      begin
        if GetIsRemoteInstanceActive then
        begin
          DoRejected;
          if AutoActivate then
            AppInstances.SwitchTo(0);
          if SendCmdLine then
            AppInstances.SendCmdLineParams(sAppInstancesWindowClassName, Handle);

          // As ExitProcess will prevent ANY finalization to occur, we free the
          // AppInstances object so that it can cleanup the process information
          AppInstances.Free;
          ExitProcess(0);
        end;
      end;
end;

procedure TPTLibAppInstances.DoCmdLineReceived(CmdLine: TStrings);
begin
  if Assigned(FOnCmdLineReceived) then
    FOnCmdLineReceived(Self, CmdLine);
end;

procedure TPTLibAppInstances.DoDataAvailable(Kind: TJvAppInstDataKind;
  Data: Pointer; Size: Integer);
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(Self, Kind, Data, Size);
end;

procedure TPTLibAppInstances.DoInstanceCreated(ProcessId: Cardinal);
begin
  if Assigned(FOnInstanceCreated) then
    FOnInstanceCreated(Self, ProcessId);
end;

procedure TPTLibAppInstances.DoInstanceDestroyed(ProcessId: Cardinal);
begin
  if Assigned(FOnInstanceDestroyed) then
    FOnInstanceDestroyed(Self, ProcessId);
end;

procedure TPTLibAppInstances.DoUserNotify(Param: Integer);
begin
  if Assigned(FOnUserNotify) then
    FOnUserNotify(Self, Param);
end;

procedure TPTLibAppInstances.DoRejected;
begin
  if Assigned(FOnRejected) then
    FOnRejected(Self);
end;

function TPTLibAppInstances.GetAppInstances: TJclAppInstances;
begin
  if csDesigning in ComponentState then
    Result := nil
  else
  begin
    if not Assigned(FAppInstances) then
      FAppInstances := JclAppInstances(FUniqueAppId); // create AppInstance

    Result := FAppInstances;
  end;
end;

procedure TPTLibAppInstances.Loaded;
begin
  inherited Loaded;
  Check;
end;

procedure TPTLibAppInstances.WndProc(var Msg: TMessage);
var
  Kind: TJvAppInstDataKind;
  Data: Pointer;
  Size: Integer;
  CmdLine: TStrings;
begin
  try
    if Msg.Msg = AppInstances.MessageID then
    begin
      case Msg.WParam of
        AI_INSTANCECREATED:
          if Cardinal(Msg.LParam) <> GetCurrentProcessId then
            DoInstanceCreated(Cardinal(Msg.LParam));
        AI_INSTANCEDESTROYED:
          DoInstanceDestroyed(Cardinal(Msg.LParam));
        AI_USERMSG:
          DoUserNotify(Msg.LParam);
        AI_GETACTIVE:
          SendMessage(HWND(Msg.LParam), AppInstances.MessageID,
            AI_SETACTIVE, Ord(Active));
        AI_SETACTIVE:
          Active := Msg.LParam <> 0;
      end;
    end
    else
    begin
      Kind := ReadMessageCheck(Msg, Handle);
      case Kind of
        AppInstDataKindNoData:
          ; // do nothing
        AppInstCmdLineDataKind:
          begin
            if Assigned(FOnCmdLineReceived) then
            begin
              CmdLine := TStringList.Create;
              try
                ReadMessageStrings(Msg, CmdLine);
                DoCmdLineReceived(CmdLine);
              finally
                CmdLine.Free;
              end;
            end;
            Exit;
          end;
      else
        if Assigned(FOnDataAvailable) then
        begin
          ReadMessageData(Msg, Data, Size);
          try
            DoDataAvailable(Kind, Data, Size);
          finally
            FreeMem(Data);
          end;
        end;
        Exit;
      end;
    end;
  except
    on E: Exception do
      Application.ShowException(E);
  end;

  with Msg do
    Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

procedure TPTLibAppInstances.UserNotify(Param: Integer);
begin
  AppInstances.UserNotify(Param);
end;

function TPTLibAppInstances.SendData(DataKind: TJclAppInstDataKind;
  Data: Pointer; Size: Integer): Boolean;
begin
  Result := AppInstances.SendData(sAppInstancesWindowClassName, DataKind, Data,
    Size, Handle);
end;

type
  PEnumWinData = ^TEnumWinData;
  TEnumWinData = record
    Instance: TPTLibAppInstances;
    Message: TMessage;
  end;

function EnumWinProc(Wnd: HWND; Data: PEnumWinData): BOOL; stdcall;
begin
  with Data^.Message do
    SendMessage(Wnd, Msg, WParam, LParam);
  Result := Data^.Instance.Active;
end;

function TPTLibAppInstances.GetIsRemoteInstanceActive: Boolean;
var
  I: Integer;
  Wnd: HWND;
  TID: DWORD;
  Data: TEnumWinData;
begin
  for I := 0 to AppInstances.InstanceCount - 1 do
  begin
    if AppInstances.ProcessIDs[I] = GetCurrentProcessId then
      Continue;
    Wnd := AppInstances.AppWnds[I];
    TID := GetWindowThreadProcessId(Wnd, nil);
    Data.Instance := Self;
    Data.Message.Msg := AppInstances.MessageID;
    Data.Message.WParam := AI_GETACTIVE;
    Data.Message.LParam := Handle;
    EnumThreadWindows(TID, @EnumWinProc, LPARAM(@Data));
    if not Active then
      Break;
  end;
  Result := Active;
end;

function TPTLibAppInstances.GetUniqueAppId: string;
begin
  Result := FUniqueAppId;
end;

const
  { strings to form a unique name for file mapping and optex objects }
  JclAIPrefix = 'Jcl';
  JclAIOptex = '_Otx';
  JclAIAllMapping = '_All';
  JclAISessionMapping = '_Session_';
  JclAIUserMapping = '_User_';

  { window message used for communication between instances }
  JclAIMessage = '_Msg';

  { maximum number of instance that may exist at any time }
  JclAIMaxInstances = 256;

  { name of the application window class }
  ClassNameOfTApplication = 'TApplication';

type
  { management data to keep track of application instances. this data is shared amongst all instances
    and must be appropriately protected from concurrent access at all time }

  PJclAISharedData = ^TJclAISharedData;
  TJclAISharedData = packed record
    MaxInst: Word;
    Count: Word;
    ProcessIDs: array [0..JclAIMaxInstances] of DWORD;
  end;

var
  { the single global TJclAppInstance instance }
  AppInstances: TJclAppInstances;
  ExplicitUniqueAppId: string;

//=== { TJclAppInstances } ===================================================

constructor TJclAppInstances.Create;
begin
  inherited Create;
  FCPID := GetCurrentProcessId;
  InitData;
end;

destructor TJclAppInstances.Destroy;
begin
  if FAllMapping <> nil then
    RemoveInstance(FAllMappingView);
  if FSessionMapping <> nil then
    RemoveInstance(FSessionMappingView);
  if FUserMapping <> nil then
    RemoveInstance(FUserMappingView);

  NotifyInstances(AI_INSTANCEDESTROYED, Integer(FCPID));

  FreeAndNil(FAllMapping);
  FreeAndNil(FSessionMapping);
  FreeAndNil(FUserMapping);
  FreeAndNil(FOptex);
  inherited Destroy;
end;

class function TJclAppInstances.BringAppWindowToFront(const Wnd: THandle): Boolean;
begin
  if IsIconic(Wnd) then
    SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
  Result := SetForegroundWindow98(Wnd);
end;

function TJclAppInstances.CheckInstance(MaxInstances, MaxSessionInstances, MaxUserInstances: Word): Boolean;
var
  SharedData: PJclAISharedData;
  CurrentProcessId: DWORD;
begin
  CurrentProcessId := GetCurrentProcessId;
  FOptex.Enter;
  try
    // check all instances
    SharedData := PJclAISharedData(FAllMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxInstances;
    Result := (SharedData^.MaxInst = 0) or (SharedData^.Count < SharedData^.MaxInst);
    SharedData^.ProcessIDs[SharedData^.Count] := CurrentProcessId;
    Inc(SharedData^.Count);

    // check session instances
    SharedData := PJclAISharedData(FSessionMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxSessionInstances;
    Result := Result and ((SharedData^.MaxInst = 0) or (SharedData^.Count < SharedData^.MaxInst));
    SharedData^.ProcessIDs[SharedData^.Count] := CurrentProcessId;
    Inc(SharedData^.Count);

    // check user instances
    SharedData := PJclAISharedData(FUserMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxUserInstances;
    Result := Result and ((SharedData^.MaxInst = 0) or (SharedData^.Count < SharedData^.MaxInst));
    SharedData^.ProcessIDs[SharedData^.Count] := CurrentProcessId;
    Inc(SharedData^.Count);
  finally
    FOptex.Leave;
  end;
  if Result then
    NotifyInstances(AI_INSTANCECREATED, Integer(FCPID));
end;

procedure TJclAppInstances.CheckMultipleInstances(MaxInstances, MaxSessionInstances, MaxUserInstances: Word);
begin
  if not CheckInstance(MaxInstances, MaxSessionInstances, MaxUserInstances) then
  begin
    SwitchTo(0);
    KillInstance;
  end;
end;

procedure TJclAppInstances.CheckSingleInstance;
begin
  CheckMultipleInstances(1);
end;

type
  PTopLevelWnd = ^TTopLevelWnd;
  TTopLevelWnd = record
    ProcessID: DWORD;
    Wnd: THandle;
  end;

function EnumApplicationWinProc(Wnd: THandle; Param: PTopLevelWnd): BOOL; stdcall;
var
  PID: DWORD;
  C: array [0..Length(ClassNameOfTApplication) + 1] of Char;
begin
  GetWindowThreadProcessId(Wnd, @PID);
  if (PID = Param^.ProcessID) and (GetClassName(Wnd, C, Length(C)) > 0) and (C = ClassNameOfTApplication) then
  begin
    Result := False;
    Param^.Wnd := Wnd;
  end
  else
  begin
    Result := True;
  end;
end;

class function TJclAppInstances.GetApplicationWnd(const ProcessID: DWORD): THandle;
var
  TopLevelWnd: TTopLevelWnd;
begin
  TopLevelWnd.ProcessID := ProcessID;
  TopLevelWnd.Wnd := 0;
  EnumWindows(@EnumApplicationWinProc, LPARAM(@TopLevelWnd));
  Result := TopLevelWnd.Wnd;
end;

function TJclAppInstances.GetAllAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetAllProcessIDs(Index));
end;

function TJclAppInstances.GetAllInstanceCount: Integer;
begin
  Result := GetInstanceCount(FAllMappingView);
end;

function TJclAppInstances.GetAllInstanceIndex(ProcessID: DWORD): Integer;
begin
  Result := GetInstanceIndex(FAllMappingView, ProcessID);
end;

function TJclAppInstances.GetAllProcessIDs(Index: Integer): DWORD;
begin
  Result := GetProcessIDs(FAllMappingView, Index);
end;

function TJclAppInstances.GetInstanceCount(MappingView: TJclFileMappingView): Integer;
begin
  FOptex.Enter;
  try
    Result := PJclAISharedData(MappingView.Memory)^.Count;
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetInstanceIndex(MappingView: TJclFileMappingView; ProcessID: DWORD): Integer;
var
  I: Integer;
  SharedData: PJclAISharedData;
begin
  Result := -1;
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(MappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
      if SharedData^.ProcessIDs[I] = ProcessID then
      begin
        Result := I;
        Break;
      end;
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetProcessIDs(MappingView: TJclFileMappingView; Index: Integer): DWORD;
var
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(MappingView.Memory);
    if Index >= SharedData^.Count then
      Result := 0
    else
      Result := SharedData^.ProcessIDs[Index];
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetSessionAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetProcessIDs(FSessionMappingView, Index));
end;

function TJclAppInstances.GetSessionInstanceCount: Integer;
begin
  Result := GetInstanceCount(FSessionMappingView);
end;

function TJclAppInstances.GetSessionInstanceIndex(ProcessID: DWORD): Integer;
begin
  Result := GetInstanceIndex(FSessionMappingView, ProcessID);
end;

function TJclAppInstances.GetSessionProcessIDs(Index: Integer): DWORD;
begin
  Result := GetProcessIDs(FSessionMappingView, Index);
end;

function TJclAppInstances.GetUserAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetProcessIDs(FUserMappingView, Index));
end;

function TJclAppInstances.GetUserInstanceCount: Integer;
begin
  Result := GetInstanceCount(FUserMappingView);
end;

function TJclAppInstances.GetUserInstanceIndex(ProcessID: DWORD): Integer;
begin
  Result := GetInstanceIndex(FUserMappingView, ProcessID);
end;

function TJclAppInstances.GetUserProcessIDs(Index: Integer): DWORD;
begin
  Result := GetProcessIDs(FUserMappingView, Index);
end;

const
  ACL_REVISION = 2;

type
  _ACE_HEADER = record
    AceType: BYTE;
    AceFlags: BYTE;
    AceSize: WORD;
  end;
  ACE_HEADER = _ACE_HEADER;
  PACE_HEADER = ^_ACE_HEADER;

  _ACCESS_ALLOWED_ACE = record
    Header: ACE_HEADER;
    Mask: ACCESS_MASK;
    SidStart: DWORD;
  end;

  ACCESS_ALLOWED_ACE = _ACCESS_ALLOWED_ACE;
  PACCESS_ALLOWED_ACE = ^_ACCESS_ALLOWED_ACE;

procedure TJclAppInstances.InitData;
begin
  if ExplicitUniqueAppId <> '' then
    FUniqueAppID := JclAIPrefix + ExplicitUniqueAppId
  else
    FUniqueAppID := AnsiUpperCase(JclAIPrefix + ParamStr(0));

  CharReplace(FUniqueAppID, '\', '_');

  FMessageID := RegisterWindowMessage(PChar(FUniqueAppID + JclAIMessage));

  FOptex := TJclOptex.Create(FUniqueAppID + JclAIOptex, 4000);

  InitAllData;
  InitSessionData;
  InitUserData;
end;

procedure TJclAppInstances.InitAllData;
var
  UserInfo: PTokenUser;
  ACL: PACL;
  SID: PSID;
  SecurityAttributes: PSecurityAttributes;
  SecurityDescriptor: PSecurityDescriptor;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  try
    SecurityGetAllUsers(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);

    FOptex.Enter;
    try
      FAllMapping := TJclSwapFileMapping.Create(FUniqueAppID + JclAIAllMapping,
        PAGE_READWRITE, SizeOf(TJclAISharedData), SecurityAttributes);
      FAllMappingView := FAllMapping.Views[FAllMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
      if not FAllMapping.Existed then
        FillChar(FAllMappingView.Memory^, SizeOf(TJclAISharedData), #0);
    finally
      FOptex.Leave;
    end;
  finally
    SecurityFree(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
  end;
end;

procedure TJclAppInstances.InitSessionData;
var
  UserInfo: PTokenUser;
  ACL: PACL;
  SID: PSID;
  SecurityAttributes: PSecurityAttributes;
  SecurityDescriptor: PSecurityDescriptor;
  SessionID: DWORD;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  try
    SecurityGetAllUsers(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);

    SessionID := 0;
    ProcessIdToSessionId(GetCurrentProcessId, SessionID); // RESULT
    FOptex.Enter;
    try
      FSessionMapping := TJclSwapFileMapping.Create(FUniqueAppID + JclAISessionMapping + IntToStr(SessionID),
        PAGE_READWRITE, SizeOf(TJclAISharedData), SecurityAttributes);
      FSessionMappingView := FSessionMapping.Views[FSessionMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
      if not FSessionMapping.Existed then
        FillChar(FSessionMappingView.Memory^, SizeOf(TJclAISharedData), #0);
    finally
      FOptex.Leave;
    end;
  finally
    SecurityFree(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
  end;
end;

procedure TJclAppInstances.InitUserData;
var
  UserInfo: PTokenUser;
  ACL: PACL;
  SID: PSID;
  SecurityAttributes: PSecurityAttributes;
  SecurityDescriptor: PSecurityDescriptor;
  UserName, GroupName: WideString;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  try
    SecurityGetCurrentUser(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
    LookupAccountBySid(UserInfo.User.Sid, UserName, GroupName, False);

    FOptex.Enter;
    try
      FUserMapping := TJclSwapFileMapping.Create(FUniqueAppID + JclAIUserMapping + UserName + '_' + GroupName,
        PAGE_READWRITE, SizeOf(TJclAISharedData), SecurityAttributes);
      FUserMappingView := FUserMapping.Views[FUserMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
      if not FUserMapping.Existed then
        FillChar(FUserMappingView.Memory^, SizeOf(TJclAISharedData), #0);
    finally
      FOptex.Leave;
    end;
  finally
    SecurityFree(UserInfo, SID, ACL, SecurityDescriptor, SecurityAttributes);
  end;
end;

class procedure TJclAppInstances.KillInstance;
begin
  Halt(0);
end;

function EnumNotifyWinProc(Wnd: THandle; Message: PMessage): BOOL; stdcall;
begin
  SendNotifyMessage(Wnd, Message^.Msg, Message^.WParam, Message^.LParam);
  Result := True;
end;

procedure TJclAppInstances.NotifyInstances(const W, L: Integer);
var
  I: Integer;
  Wnd: THandle;
  TID: DWORD;
  Msg: TMessage;
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FAllMappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
    begin
      Wnd := GetApplicationWnd(SharedData^.ProcessIDs[I]);
      TID := GetWindowThreadProcessId(Wnd, nil);
      while Wnd <> 0 do
      begin // Send message to TApplication queue
        if PostThreadMessage(TID, FMessageID, W, L) or
          (GetLastError = ERROR_INVALID_THREAD_ID) then
          Break;
        Sleep(1);
      end;
      Msg.Msg := FMessageID;
      Msg.WParam := W;
      Msg.LParam := L;
      EnumThreadWindows(TID, @EnumNotifyWinProc, LPARAM(@Msg));
    end;
  finally
    FOptex.Leave;
  end;
end;

procedure TJclAppInstances.RemoveInstance(MappingView: TJclFileMappingView);
var
  I: Integer;
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(MappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
      if SharedData^.ProcessIDs[I] = FCPID then
      begin
        SharedData^.ProcessIDs[I] := 0;
        Move(SharedData^.ProcessIDs[I + 1], SharedData^.ProcessIDs[I], (SharedData^.Count - I) * SizeOf(DWORD));
        Dec(SharedData^.Count);
        Break;
      end;
  finally
    FOptex.Leave;
  end;
end;

procedure TJclAppInstances.SecurityFree(UserInfo: PTokenUser; SID: PSID; ACL: PACL;
  SecurityDescriptor: PSecurityDescriptor; SecurityAttributes: PSecurityAttributes);
begin
  if Assigned(UserInfo) then
    FreeMem(UserInfo);
  if Assigned(SID) then
    FreeSID(SID);
  if Assigned(ACL) then
    FreeMem(ACL);
  if Assigned(SecurityDescriptor) then
    FreeMem(SecurityDescriptor);
  if Assigned(SecurityAttributes) then
    FreeMem(SecurityAttributes);
end;

procedure TJclAppInstances.SecurityGetAllUsers(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
  out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
var
  WorldAuth: {$IFDEF HAS_UNITSCOPE}WinApi.{$ENDIF HAS_UNITSCOPE}Windows.SID_IDENTIFIER_AUTHORITY;
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;

  SecurityGetCurrentUserInfo(UserInfo);

    // Retrieve the SID of the Everyone group.
  WorldAuth := SECURITY_WORLD_SID_AUTHORITY;
  AllocateAndInitializeSid(WorldAuth, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, SID); // RESULT

  SecurityGetSecurityAttributes(UserInfo^.User.Sid, SID, ACL, SecurityDescriptor, SecurityAttributes);
end;

procedure TJclAppInstances.SecurityGetCurrentUser(out UserInfo: PTokenUser; out SID: PSID; out ACL: PACL;
  out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
begin
  UserInfo := nil;
  ACL := nil;
  SID := nil;
  SecurityDescriptor := nil;
  SecurityAttributes := nil;
  SecurityGetCurrentUserInfo(UserInfo);
  SecurityGetSecurityAttributes(UserInfo^.User.Sid, UserInfo.User.Sid, ACL, SecurityDescriptor, SecurityAttributes);
end;

procedure TJclAppInstances.SecurityGetCurrentUserInfo(out UserInfo: PTokenUser);
var
  ProcessToken: THandle;
  TokenInfoSize: DWORD;
  HaveToken: Boolean;
begin
  UserInfo := nil;
  ProcessToken := 0;
  try
    HaveToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, ProcessToken);
    if (not HaveToken) and (GetLastError = ERROR_NO_TOKEN) then
      HaveToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, ProcessToken);
    if not HaveToken then
      RaiseLastOSError;

    if GetTokenInformation(ProcessToken, TokenUser, nil, 0, TokenInfoSize) or
     (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
       RaiseLastOSError;
    UserInfo := PTokenUser(AllocMem(TokenInfoSize));
    Win32Check(GetTokenInformation(ProcessToken, TokenUser, UserInfo, TokenInfoSize, TokenInfoSize));
  finally
    if ProcessToken <> 0 then
      CloseHandle(ProcessToken);
  end;
end;

procedure TJclAppInstances.SecurityGetSecurityAttributes(OwnerSID, AccessSID: PSID; out ACL: PACL;
  out SecurityDescriptor: PSecurityDescriptor; out SecurityAttributes: PSecurityAttributes);
var
  ACLSize: Integer;
begin
  // create the ACL
  ACLSize := SizeOf(TACL) + SizeOf(ACCESS_ALLOWED_ACE) + SizeOf(DWORD) + GetLengthSid(AccessSID);
  ACL := AllocMem(ACLSize);
  Win32Check(InitializeAcl(ACL^, ACLSize, ACL_REVISION));
  Win32Check(AddAccessAllowedAce(ACL^, ACL_REVISION, FILE_MAP_ALL_ACCESS, AccessSID));
  Assert(IsValidAcl(ACL{$IFNDEF RTL230_UP}^{$ENDIF})); // QC #102231

  // create the security descriptor
  SecurityDescriptor := AllocMem(SECURITY_DESCRIPTOR_MIN_LENGTH);
  Win32Check(InitializeSecurityDescriptor(SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION));
  Win32Check(SetSecurityDescriptorSacl(SecurityDescriptor, False, nil, True));
  Win32Check(SetSecurityDescriptorOwner(SecurityDescriptor, OwnerSID, False));
  Win32Check(SetSecurityDescriptorGroup(SecurityDescriptor, OwnerSID, False));
  Win32Check(SetSecurityDescriptorDacl(SecurityDescriptor, True, ACL, False));
  Assert(IsValidSecurityDescriptor(SecurityDescriptor));

  // create the security attributes
  SecurityAttributes := AllocMem(SizeOf(SecurityAttributes^));
  SecurityAttributes^.nLength := SizeOf(SecurityAttributes^);
  SecurityAttributes^.lpSecurityDescriptor := SecurityDescriptor;
  SecurityAttributes^.bInheritHandle := False;
end;

function TJclAppInstances.SendCmdLineParams(const WindowClassName: string; const OriginatorWnd: THandle): Boolean;
var
  TempList: TStringList;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    for I := 1 to ParamCount do
      TempList.Add(ParamStr(I));
    Result := SendStrings(WindowClassName, AppInstCmdLineDataKind, TempList, OriginatorWnd);
  finally
    TempList.Free;
  end;
end;

type
  PEnumWinRec = ^TEnumWinRec;
  TEnumWinRec = record
    WindowClassName: PChar;
    OriginatorWnd: THandle;
    CopyData: TCopyDataStruct;
    Self: TJclAppInstances;
  end;

(*function EnumWinProc(Wnd: THandle; Data: PEnumWinRec): BOOL; stdcall;
var
  ClassName: array [0..200] of Char;
  I: Integer;
  PID: DWORD;
  Found: Boolean;
  SharedData: PJclAISharedData;
begin
  if (GetClassName(Wnd, ClassName, Length(ClassName) - 1) > 0) and
    (StrComp(ClassName, Data.WindowClassName) = 0) then
  begin
    GetWindowThreadProcessId(Wnd, @PID);
    Found := False;
    Data.Self.FOptex.Enter;
    try
      SharedData := PJclAISharedData(Data.Self.FAllMappingView.Memory);
      for I := 0 to SharedData^.Count - 1 do
        if SharedData^.ProcessIDs[I] = PID then
        begin
          Found := True;
          Break;
        end;
    finally
      Data.Self.FOptex.Leave;
    end;
    if Found then
      SendMessage(Wnd, WM_COPYDATA, Data.OriginatorWnd, LPARAM(@Data.CopyData));
  end;
  Result := True;
end;*)

function TJclAppInstances.SendData(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind;
  Data: Pointer; const Size: Integer;
  OriginatorWnd: THandle): Boolean;
var
  EnumWinRec: TEnumWinRec;
begin
  Assert(DataKind <> AppInstDataKindNoData);
  EnumWinRec.WindowClassName := PChar(WindowClassName);
  EnumWinRec.OriginatorWnd := OriginatorWnd;
  EnumWinRec.CopyData.dwData := DataKind;
  EnumWinRec.CopyData.cbData := Size;
  EnumWinRec.CopyData.lpData := Data;
  EnumWinRec.Self := Self;
  Result := EnumWindows(@EnumWinProc, LPARAM(@EnumWinRec));
end;

function TJclAppInstances.SendString(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind; const S: string;
  OriginatorWnd: THandle): Boolean;
begin
  Result := SendData(WindowClassName, DataKind, PChar(S), Length(S) * SizeOf(Char), OriginatorWnd);
end;

function TJclAppInstances.SendStrings(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings;
  OriginatorWnd: THandle): Boolean;
begin
  Result := SendString(WindowClassName, DataKind, Strings.Text, OriginatorWnd);
end;

function TJclAppInstances.SessionSwitchTo(Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(SessionAppWnds[Index]);
end;

class function TJclAppInstances.SetForegroundWindow98(const Wnd: THandle): Boolean;
var
  ForeThreadID, NewThreadID: DWORD;
begin
  if GetForegroundWindow <> Wnd then
  begin
    ForeThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
    NewThreadID := GetWindowThreadProcessId(Wnd, nil);
    if ForeThreadID <> NewThreadID then
    begin
      AttachThreadInput(ForeThreadID, NewThreadID, True);
      Result := SetForegroundWindow(Wnd);
      AttachThreadInput(ForeThreadID, NewThreadID, False);
      if Result then
        Result := SetForegroundWindow(Wnd);
    end
    else
      Result := SetForegroundWindow(Wnd);
  end
  else
    Result := True;
end;

function TJclAppInstances.SwitchTo(Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(AppWnds[Index]);
end;

procedure TJclAppInstances.UserNotify(Param: Longint);
begin
  NotifyInstances(AI_USERMSG, Param);
end;

function TJclAppInstances.UserSwitchTo(Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(UserAppWnds[Index]);
end;

function JclAppInstances: TJclAppInstances;
begin
  if AppInstances = nil then
    AppInstances := TJclAppInstances.Create;
  Result := AppInstances;
end;

function JclAppInstances(const UniqueAppIdGuidStr: string): TJclAppInstances;
begin
  Assert(AppInstances = nil);
  ExplicitUniqueAppId := UniqueAppIdGuidStr;
  Result := JclAppInstances;
end;

// Interprocess communication routines
function ReadMessageCheck(var Message: TMessage; const IgnoredOriginatorWnd: THandle): TJclAppInstDataKind;
begin
  if (Message.Msg = WM_COPYDATA) and (TWMCopyData(Message).From <> IgnoredOriginatorWnd) then
  begin
    Message.Result := 1;
    Result := TJclAppInstDataKind(TWMCopyData(Message).CopyDataStruct^.dwData);
  end
  else
  begin
    Message.Result := 0;
    Result := AppInstDataKindNoData;
  end;
end;

procedure ReadMessageData(const Message: TMessage; var Data: Pointer; var Size: Integer);
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
  begin
    Size := TWMCopyData(Message).CopyDataStruct^.cbData;
    GetMem(Data, Size);
    Move(TWMCopyData(Message).CopyDataStruct^.lpData^, Data^, Size);
  end;
end;

procedure ReadMessageString(const Message: TMessage; out S: string);
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
    SetString(S, PChar(TWMCopyData(Message).CopyDataStruct^.lpData), TWMCopyData(Message).CopyDataStruct^.cbData div SizeOf(Char));
end;

procedure ReadMessageStrings(const Message: TMessage; const Strings: TStrings);
var
  S: string;
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
  begin
    ReadMessageString(Message, S);
    Strings.Text := S;
  end;
end;

function SendData(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Data: Pointer; const Size: Integer): Boolean;
var
  CopyData: TCopyDataStruct;
begin
  CopyData.dwData := DataKind;
  CopyData.cbData := Size;
  CopyData.lpData := Data;
  Result := Boolean(SendMessage(Wnd, WM_COPYDATA, OriginatorWnd, LPARAM(@CopyData)));
end;

function SendStrings(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings): Boolean;
begin
  Result := SendString(Wnd, OriginatorWnd, DataKind, Strings.Text);
end;

function SendCmdLineParams(const Wnd, OriginatorWnd: HWND): Boolean;
var
  TempList: TStringList;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    for I := 1 to ParamCount do
      TempList.Add(ParamStr(I));
    Result := SendStrings(Wnd, OriginatorWnd, AppInstCmdLineDataKind, TempList);
  finally
    TempList.Free;
  end;
end;

function SendString(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const S: string): Boolean;
begin
  Result := SendData(Wnd, OriginatorWnd, DataKind, PChar(S), Length(S) * SizeOf(Char));
end;

constructor TJclCustomFileMapping.Create;
begin
  inherited Create;
  FViews := TList.Create;
  FRoundViewOffset := rvDown;
end;

constructor TJclCustomFileMapping.Open(const Name: string;
  const InheritHandle: Boolean; const DesiredAccess: Cardinal);
begin
  Create;
  InternalOpen(Name, InheritHandle, DesiredAccess);
end;

destructor TJclCustomFileMapping.Destroy;
begin
  ClearViews;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FreeAndNil(FViews);
  inherited Destroy;
end;

function TJclCustomFileMapping.Add(const Access, Count: Cardinal; const Offset: Int64): Integer;
var
  View: TJclFileMappingView;
begin
  // The view adds itself to the FViews list
  View := TJclFileMappingView.Create(Self, Access, Count, Offset);
  Result := View.Index;
end;

function TJclCustomFileMapping.AddAt(const Access, Count: Cardinal;
  const Offset: Int64; const Address: Pointer): Integer;
var
  View: TJclFileMappingView;
begin
  // The view adds itself to the FViews list
  View := TJclFileMappingView.CreateAt(Self, Access, Count, Offset, Address);
  Result := View.Index;
end;

procedure TJclCustomFileMapping.ClearViews;
var
  I: Integer;
begin
  // Note that the view destructor removes the view object from the FViews list so we must loop
  // downwards from count to 0
  for I := FViews.Count - 1 downto 0 do
    TJclFileMappingView(FViews[I]).Free;
end;

procedure TJclCustomFileMapping.Delete(const Index: Integer);
begin
  // Note that the view destructor removes itself from FViews
  TJclFileMappingView(FViews[Index]).Free;
end;

function TJclCustomFileMapping.GetCount: Integer;
begin
  Result := FViews.Count;
end;

function TJclCustomFileMapping.GetView(Index: Integer): TJclFileMappingView;
begin
  Result := TJclFileMappingView(FViews.Items[index]);
end;

{TJclCustomFileMapping}

function TJclCustomFileMapping.IndexOf(const View: TJclFileMappingView): Integer;
begin
  Result := FViews.IndexOf(View);
end;

procedure TJclCustomFileMapping.InternalCreate(const FileHandle: THandle;
  const Name: string; const Protect: Cardinal; MaximumSize: Int64;
  SecAttr: PSecurityAttributes);
var
  MaximumSizeLow, MaximumSizeHigh: Cardinal;
begin
  FName := Name;
  I64ToCardinals(MaximumSize, MaximumSizeLow, MaximumSizeHigh);
  FHandle := CreateFileMapping(FileHandle, SecAttr, Protect, MaximumSizeHigh,
    MaximumSizeLow, PChar(Name));
  if FHandle = 0 then
    raise EPTLibError.Create('Create File Mapping');
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

procedure TJclCustomFileMapping.InternalOpen(const Name: string;
  const InheritHandle: Boolean; const DesiredAccess: Cardinal);
begin
  FExisted := True;
  FName := Name;
  FHandle := OpenFileMapping(DesiredAccess, InheritHandle, PChar(Name));
  if FHandle = 0 then
    raise EPTLibError.Create('Create File Mapping');
end;

{ TJclSwapFileMapping }

constructor TJclSwapFileMapping.Create(const Name: string; Protect: Cardinal;
  const MaximumSize: Int64; SecAttr: PSecurityAttributes);
begin
  inherited Create;
  InternalCreate(INVALID_HANDLE_VALUE, Name, Protect, MaximumSize, SecAttr);
end;

constructor TJclFileMappingView.Create(const FileMap: TJclCustomFileMapping;
  Access, Size: Cardinal; ViewOffset: Int64);
var
  BaseAddress: Pointer;
  OffsetLow, OffsetHigh: Cardinal;
begin
  inherited Create;
  if FileMap = nil then
    raise EPTLibError.Create('View Needs Mapping');
  FFileMapping := FileMap;
  // Offset must be a multiple of system memory allocation granularity
  RoundToAllocGranularity64(ViewOffset, FFileMapping.RoundViewOffset = rvUp);
  I64ToCardinals(ViewOffset, OffsetLow, OffsetHigh);
  FOffsetHigh := OffsetHigh;
  FOffsetLow := OffsetLow;
  BaseAddress := MapViewOfFile(FFileMapping.Handle, Access, FOffsetHigh, FOffsetLow, Size);
  if BaseAddress = nil then
    raise EPTLibError.Create('Create File Mapping View');
  // If we are mapping a file and size = 0 then MapViewOfFile has mapped the entire file. We must
  // figure out the size ourselves before we can call SetPointer. Since in case of failure to
  // retrieve the size we raise an exception, we also have to explicitly unmap the view which
  // otherwise would have been done by the destructor.
  if (Size = 0) and (FileMap is TJclFileMapping) then
  begin
    Size := GetFileSize(TJclFileMapping(FileMap).FFileHandle, nil);
    if Size = DWORD(-1) then
    begin
      UnMapViewOfFile(BaseAddress);
      raise EPTLibError.Create('Failed To Obtain Size');
    end;
  end;
  SetPointer(BaseAddress, Size);
  FFileMapping.FViews.Add(Self);
end;

constructor TJclFileMappingView.CreateAt(FileMap: TJclCustomFileMapping;
  Access, Size: Cardinal; ViewOffset: Int64; Address: Pointer);
var
  BaseAddress: Pointer;
  OffsetLow, OffsetHigh: Cardinal;
begin
  inherited Create;
  if FileMap = nil then
    raise EPTLibError.Create('View Needs Mapping');
  FFileMapping := FileMap;
  // Offset must be a multiple of system memory allocation granularity
  RoundToAllocGranularity64(ViewOffset, FFileMapping.RoundViewOffset = rvUp);
  RoundToAllocGranularityPtr(Address, FFileMapping.RoundViewOffset = rvUp);
  I64ToCardinals(ViewOffset, OffsetLow, OffsetHigh);
  FOffsetHigh := OffsetHigh;
  FOffsetLow := OffsetLow;
  BaseAddress := MapViewOfFileEx(FFileMapping.Handle, Access, FOffsetHigh,
    FOffsetLow, Size, Address);
  if BaseAddress = nil then
    raise EPTLibError.Create('Create File Mapping View');
  // If we are mapping a file and size = 0 then MapViewOfFile has mapped the entire file. We must
  // figure out the size ourselves before we can call SetPointer. Since in case of failure to
  // retrieve the size we raise an exception, we also have to explicitly unmap the view which
  // otherwise would have been done by the destructor.
  if (Size = 0) and (FileMap is TJclFileMapping) then
  begin
    Size := GetFileSize(TJclFileMapping(FileMap).FFileHandle, nil);
    if Size = DWORD(-1) then
    begin
      UnMapViewOfFile(BaseAddress);
      raise EPTLibError.Create('Failed To Obtain Size');
    end;
  end;
  SetPointer(BaseAddress, Size);
  FFileMapping.FViews.Add(Self);
end;

destructor TJclFileMappingView.Destroy;
var
  IndexOfSelf: Integer;
begin
  if Memory <> nil then
  begin
    UnMapViewOfFile(Memory);
    SetPointer(nil, 0);
  end;
  if FFileMapping <> nil then
  begin
    IndexOfSelf := FFileMapping.IndexOf(Self);
    if IndexOfSelf <> -1 then
      FFileMapping.FViews.Delete(IndexOfSelf);
  end;
  inherited Destroy;
end;

function TJclFileMappingView.Flush(const Count: Cardinal): Boolean;
begin
  Result := FlushViewOfFile(Memory, Count);
end;

function TJclFileMappingView.GetIndex: Integer;
begin
  Result := FFileMapping.IndexOf(Self);
end;

function TJclFileMappingView.GetOffset: Int64;
begin
  CardinalsToI64(Result, FOffsetLow, FOffsetHigh);
end;

procedure TJclFileMappingView.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TJclFileMappingView.LoadFromStream(const Stream: TStream);
begin
  if Stream.Size > Size then
    raise EPTLibError.Create('Load From Stream Size');
  Stream.Position := 0;
  Stream.ReadBuffer(Memory^, Stream.Size);
end;

function TJclFileMappingView.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
  if (Size - Position) >= Count then
  begin
    System.Move(Buffer, Pointer(TJclAddr(Memory) + TJclAddr(Position))^, Count);
    Position := Position + Count;
    Result := Count;
  end;
end;


{ TJclOptex }

constructor TJclOptex.Create(const Name: string; SpinCount: Integer);
begin
  FExisted := False;
  FName := Name;
  if Name = '' then
  begin
    // None shared optex, don't need filemapping, sharedinfo is local
    FFileMapping := 0;
    FEvent := TJclEvent.Create(nil, False, False, '');
    FSharedInfo := AllocMem(SizeOf(TOptexSharedInfo));
  end
  else
  begin
    // Shared optex, event protects access to sharedinfo. Creation of filemapping
    // doesn't need protection as it will automatically "open" instead of "create"
    // if another process already created it.
    FEvent := TJclEvent.Create(nil, False, False, 'Optex_Event_' + Name);
    FExisted := FEvent.Existed;
    FFileMapping := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
      0, SizeOf(TOptexSharedInfo), PChar('Optex_MMF_' + Name));
    Assert(FFileMapping <> 0);
    FSharedInfo := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.MapViewOfFile(FFileMapping, FILE_MAP_WRITE, 0, 0, 0);
    Assert(FSharedInfo <> nil);
  end;
  SetSpinCount(SpinCount);
end;

destructor TJclOptex.Destroy;
begin
  FreeAndNil(FEvent);
  if UniProcess then
    FreeMem(FSharedInfo)
  else
  begin
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.UnmapViewOfFile(FSharedInfo);
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CloseHandle(FFileMapping);
  end;
  inherited Destroy;
end;

procedure TJclOptex.Enter;
var
  ThreadId: Longword;
begin
  if TryEnter then
    Exit;
  ThreadId := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.GetCurrentThreadId;
  if {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedIncrement(FSharedInfo^.LockCount) = 1 then
  begin
    // Optex was unowned
    FSharedInfo^.ThreadId := ThreadId;
    FSharedInfo^.RecursionCount := 1;
  end
  else
  begin
    if FSharedInfo^.ThreadId = ThreadId then
    begin
      // We already owned it, increase ownership count
      Inc(FSharedInfo^.RecursionCount)
    end
    else
    begin
      // Optex is owner by someone else, wait for it to be released and then
      // immediately take ownership
      FEvent.WaitForever;
      FSharedInfo^.ThreadId := ThreadId;
      FSharedInfo^.RecursionCount := 1;
    end;
  end;
end;

function TJclOptex.GetSpinCount: Integer;
begin
  Result := FSharedInfo^.SpinCount;
end;

function TJclOptex.GetUniProcess: Boolean;
begin
  Result := FFileMapping = 0;
end;

procedure TJclOptex.Leave;
begin
  Dec(FSharedInfo^.RecursionCount);
  if FSharedInfo^.RecursionCount > 0 then
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedDecrement(FSharedInfo^.LockCount)
  else
  begin
    FSharedInfo^.ThreadId := 0;
    if {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedDecrement(FSharedInfo^.LockCount) > 0 then
      FEvent.SetEvent;
  end;
end;

procedure TJclOptex.SetSpinCount(Value: Integer);
begin
  if Value < 0 then
    Value := DefaultCritSectSpinCount;
  // Spinning only makes sense on multiprocessor systems
  if ProcessorCount > 1 then
    {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedExchange(Integer(FSharedInfo^.SpinCount), Value);
end;

function TJclOptex.TryEnter: Boolean;
var
  ThreadId: Longword;
  ThreadOwnsOptex: Boolean;
  SpinCount: Integer;
begin
  ThreadId := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.GetCurrentThreadId;
  SpinCount := FSharedInfo^.SpinCount;
  repeat
    //ThreadOwnsOptex := InterlockedCompareExchange(Pointer(FSharedInfo^.LockCount),
    //  Pointer(1), Pointer(0)) = Pointer(0); // not available on win95
    ThreadOwnsOptex := LockedCompareExchange(FSharedInfo^.LockCount, 1, 0) = 0;
    if ThreadOwnsOptex then
    begin
      // Optex was unowned
      FSharedInfo^.ThreadId := ThreadId;
      FSharedInfo^.RecursionCount := 1;
    end
    else
    begin
      if FSharedInfo^.ThreadId = ThreadId then
      begin
        // We already owned the Optex
        {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.InterlockedIncrement(FSharedInfo^.LockCount);
        Inc(FSharedInfo^.RecursionCount);
        ThreadOwnsOptex := True;
      end;
    end;
    Dec(SpinCount);
  until ThreadOwnsOptex or (SpinCount <= 0);
  Result := ThreadOwnsOptex;
end;

{ TJclEvent }

constructor TJclEvent.Create(SecAttr: PSecurityAttributes; Manual, Signaled: Boolean; const Name: string);
begin
  inherited Create;
  FName := Name;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.CreateEvent(SecAttr, Manual, Signaled, PChar(FName));
  if FHandle = 0 then
    raise EPTLibError.Create('Synch Create Event');
  FExisted := GetLastError = ERROR_ALREADY_EXISTS;
end;

constructor TJclEvent.Open(Access: Cardinal; Inheritable: Boolean;
  const Name: string);
begin
  FName := Name;
  FExisted := True;
  FHandle := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.OpenEvent(Access, Inheritable, PChar(Name));
  if FHandle = 0 then
    raise EPTLibError.Create('Synch Open Event');
end;

function TJclEvent.Pulse: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.PulseEvent(FHandle);
end;

function TJclEvent.ResetEvent: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.ResetEvent(FHandle);
end;

function TJclEvent.SetEvent: Boolean;
begin
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.SetEvent(FHandle);
end;

{ TJclDispatcherObject }

function MapSignalResult(const Ret: DWORD): TJclWaitResult;
begin
  case Ret of
    WAIT_ABANDONED:
      Result := wrAbandoned;
    WAIT_OBJECT_0:
      Result := wrSignaled;
    WAIT_TIMEOUT:
      Result := wrTimeout;
    WAIT_IO_COMPLETION:
      Result := wrIoCompletion;
    WAIT_FAILED:
      Result := wrError;
  else
    Result := wrError;
  end;
end;

constructor TJclDispatcherObject.Attach(AHandle: TJclWaitHandle);
begin
  inherited Create;
  FExisted := True;
  FHandle := AHandle;
  FName := '';
end;

destructor TJclDispatcherObject.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

{ TODO: Use RTDL Version of SignalObjectAndWait }

function TJclDispatcherObject.SignalAndWait(const Obj: TJclDispatcherObject;
  TimeOut: Cardinal; Alertable: Boolean): TJclWaitResult;
begin
  // Note: Do not make this method virtual! It's only available on NT 4 up...
  Result := MapSignalResult(Cardinal({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.SignalObjectAndWait(Obj.Handle, Handle, TimeOut, Alertable)));
end;

function TJclDispatcherObject.WaitAlertable(const TimeOut: Cardinal): TJclWaitResult;
begin
  Result := MapSignalResult({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForSingleObjectEx(FHandle, TimeOut, True));
end;

function TJclDispatcherObject.WaitFor(const TimeOut: Cardinal): TJclWaitResult;
begin
  Result := MapSignalResult({$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForSingleObject(FHandle, TimeOut));
end;

function TJclDispatcherObject.WaitForever: TJclWaitResult;
begin
  Result := WaitFor(INFINITE);
end;

// Wait functions
function WaitForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
var
  Handles: array of TJclWaitHandle;
  I, Count: Integer;
begin
  Count := High(Objects) + 1;
  SetLength(Handles, Count);
  for I := 0 to Count - 1 do
    Handles[I] := Objects[I].Handle;
  Result := {$IFDEF HAS_UNITSCOPE}Winapi.{$ENDIF}Windows.WaitForMultipleObjects(Count, @Handles[0], WaitAll, TimeOut);
end;

function WaitAlertableForMultipleObjects(const Objects: array of TJclDispatcherObject;
  WaitAll: Boolean; TimeOut: Cardinal): Cardinal;
var
  Handles: array of TJclWaitHandle;
  I, Count: Integer;
begin
  Count := High(Objects) + 1;
  SetLength(Handles, Count);
  for I := 0 to Count - 1 do
    Handles[I] := Objects[I].Handle;
  Result := Windows.WaitForMultipleObjectsEx(Count, @Handles[0], WaitAll, TimeOut, True);
end;

{ TJclFileMapping }

constructor TJclFileMapping.Create(const FileName: string; FileMode: Cardinal;
  const Name: string; Protect: Cardinal; const MaximumSize: Int64;
  SecAttr: PSecurityAttributes);
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  FFileHandle := THandle(FileOpen(FileName, FileMode));
  if FFileHandle = INVALID_HANDLE_VALUE then
    raise EPTLibError.Create('File Mapping Open File');
  InternalCreate(FFileHandle, Name, Protect, MaximumSize, SecAttr);
end;

constructor TJclFileMapping.Create(const FileHandle: THandle; const Name: string;
  Protect: Cardinal; const MaximumSize: Int64; SecAttr: PSecurityAttributes);
begin
  FFileHandle := INVALID_HANDLE_VALUE;
  inherited Create;
  if FileHandle = INVALID_HANDLE_VALUE then
    raise EPTLibError.Create('File Mapping Invalid Handle');
  InternalCreate(FileHandle, Name, Protect, MaximumSize, SecAttr);
  // Duplicate the handle into FFileHandle as opposed to assigning it directly. This will cause
  // FFileHandle to retrieve a unique copy which is independent of FileHandle. This makes the
  // remainder of the class, especially the destructor, easier. The caller will have to close it's
  // own copy of the handle explicitly.
  DuplicateHandle(GetCurrentProcess, FileHandle, GetCurrentProcess,
    @FFileHandle, 0, False, DUPLICATE_SAME_ACCESS);
end;

destructor TJclFileMapping.Destroy;
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);
  inherited Destroy;
end;

procedure ResetMemory(out P; Size: Longint);
begin
  if Size > 0 then
  begin
    Byte(P) := 0;
    FillChar(P, Size, 0);
  end;
end;

procedure InitSysInfo;
var
  SystemInfo: TSystemInfo;
begin
  { processor information related initialization }

  ResetMemory(SystemInfo, SizeOf(SystemInfo));
  GetSystemInfo(SystemInfo);
  ProcessorCount := SystemInfo.dwNumberOfProcessors;
end;

initialization
  InitSysInfo;

finalization

{$WARN SYMBOL_PLATFORM ON}

end.
