{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Windows.Services                                   }
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

unit PTLib.Windows.Services;

interface

uses
  System.Classes, System.SysUtils,

  System.Win.Registry,

  WinAPI.Windows, WinAPI.Messages, WinAPI.WinSvc,

  Vcl.Consts, Vcl.Forms, Vcl.SvcMgr;

type
  TStartupType = (
    stBoot,
    stSystem,
    stAutomatic,
    stManual,
    stDisabled
  );

  TDebugServiceApplication = class (TServiceApplication)
  private
    procedure OnExceptionHandler(Sender: TObject; E: Exception);
  public
    procedure Run; override;
  end;

  TDebugServiceThread = class (TThread)
  private
    fService : TService;
    procedure ProcessRequests(WaitForMessage: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create (AService : TService);
  end;

  TWindowsService = class
  public
    class function ServiceGetStatus(const sMachine, sService: string): Integer;
    class function ServiceStart(const aMachine, aServiceName: string): boolean;
    class function ServiceStop(const aMachine, aServiceName: string): boolean;
    class function ServiceStatusToText(const Status: Integer): String;
    class function IsServiceRunning(const sMachine, sService: String): Boolean;
    class function SetServiceStartupType(const aServiceName: String; StartupType: TStartupType): Boolean;
  end;

implementation

const
  OneSecond = 1 / 24 / 60 / 60;

{ TDebugServiceApplication }

procedure TDebugServiceApplication.OnExceptionHandler(Sender: TObject; E: Exception);
begin
  DoHandleException(E);
end;

procedure TDebugServiceApplication.Run;
var
  i: Integer;
  Service: TService;
  Thread: TThread;
begin
  Vcl.Forms.Application.OnException := OnExceptionHandler;
  try
    for i := 0 to ComponentCount - 1 do
    begin
      if Components [i] is TService then
      begin
        Service := TService (Components [i]);
        Thread := TDebugServiceThread.Create(Service);
        Thread.Start;
        Service.Tag := Integer(Thread);
      end;
    end;

    while not Vcl.Forms.Application.Terminated do
      Vcl.Forms.Application.HandleMessage;

    for i := 0 to ComponentCount - 1 do
    begin
      if Components [i] is TService then
      begin
        Service := TService (Components [i]);
        Thread := TThread(Service.Tag);
        PostThreadMessage(Thread.ThreadID, WM_QUIT, 0, 0);
        Thread.WaitFor;
        FreeAndNil (Thread)
      end;
    end;
  finally
  end;
end;

{ TDebugServiceThread }

constructor TDebugServiceThread.Create(AService: TService);
begin
  fService := AService;

  inherited Create(True);
end;

procedure TDebugServiceThread.Execute;
var
  Msg: TMsg;
  Started: Boolean;
begin
  PeekMessage(Msg, 0, WM_USER, WM_USER, PM_NOREMOVE); { Create message queue }

  try
    Started := True;

    if Assigned(FService.OnStart) then
      FService.OnStart(FService, Started);

    if Started then
    try
      if Assigned(FService.OnExecute) then
        FService.OnExecute(FService)
      else
        ProcessRequests(True);

      ProcessRequests(False);
    except
      on E: Exception do
        FService.LogMessage(Format(SServiceFailed,[SExecute, E.Message]));
    end;
  except
    on E: Exception do
      FService.LogMessage(Format(SServiceFailed,[SStart, E.Message]));
  end;
end;

procedure TDebugServiceThread.ProcessRequests(WaitForMessage: Boolean);
var
  Msg: TMsg;
  MsgResult: Boolean;
begin
  while True do
  begin
    if Terminated and WaitForMessage then
      Break;

    if WaitForMessage then
      MsgResult := GetMessage(Msg, 0, 0, 0)
    else
      MsgResult := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);

    if not MsgResult then
      Break;

    DispatchMessage(Msg);
  end;
end;

(*uses


  PTLib.Common.Thread;        *)

class function TWindowsService.SetServiceStartupType(const aServiceName: String; StartupType: TStartupType): Boolean;
var
  Reg: TRegistry;
  Path: string;
  StartupTypeInt: Integer;
begin
  case StartupType of
    stBoot: StartupTypeInt := 0;
    stSystem: StartupTypeInt := 1;
    stAutomatic: StartupTypeInt := 2;
    stManual: StartupTypeInt := 3;
    stDisabled: StartupTypeInt := 4;
  else
    StartupTypeInt := 3;
  end;

  Reg := TRegistry.Create;
  try
    with Reg do
    begin
      RootKey := HKEY_LOCAL_MACHINE;
      Path := concat('System\CurrentControlSet\services\', aServiceName);

      Result := KeyExists(Path);

      if Result then
      begin
        OpenKey(Path, True);

        WriteInteger('Start', StartupTypeInt);
      end;
    end;
  finally
    Reg.CloseKey;

    FreeAndNil(Reg);
  end;
end;

class function TWindowsService. ServiceStart(const aMachine, aServiceName : string ): boolean;
// aMachine is UNC path or local machine if left empty
var
  h_manager,h_svc: SC_Handle;
  svc_status: TServiceStatus;
  Temp: PChar;
  dwCheckPoint: DWord;
  Timeout: TDateTime;
begin
  svc_status.dwCurrentState := 1;
  h_manager := OpenSCManagerW(PWideChar(aMachine), Nil,
                             SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenServiceW(h_manager, PWideChar(aServiceName),
                         SERVICE_START or SERVICE_QUERY_STATUS);
    if h_svc > 0 then
    begin
      temp := nil;
      if (StartService(h_svc,0,temp)) then
        if (QueryServiceStatus(h_svc,svc_status)) then
        begin
          Timeout := now + (OneSecond * 10);

          while (SERVICE_RUNNING <> svc_status.dwCurrentState) and (now < Timeout) do
          begin
            dwCheckPoint := svc_status.dwCheckPoint;

            Sleep(svc_status.dwWaitHint);

            if (not QueryServiceStatus(h_svc,svc_status)) then
              break;

            if (svc_status.dwCheckPoint < dwCheckPoint) then
            begin
              // QueryServiceStatus didn't increment dwCheckPoint
              break;
            end;
          end;
        end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := SERVICE_RUNNING = svc_status.dwCurrentState;
end;

class function TWindowsService. ServiceStop(const aMachine,aServiceName : string ): boolean;
// aMachine is UNC path or local machine if left empty
var
  h_manager,h_svc   : SC_Handle;
  svc_status     : TServiceStatus;
  dwCheckPoint : DWord;
  Timeout: TDateTime;
begin
  h_manager:=OpenSCManager(PChar(aMachine),nil,
                           SC_MANAGER_CONNECT);
  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager,PChar(aServiceName),
                         SERVICE_STOP or SERVICE_QUERY_STATUS);

    if h_svc > 0 then
    begin
      if(ControlService(h_svc,SERVICE_CONTROL_STOP,
                        svc_status))then
      begin
        if(QueryServiceStatus(h_svc,svc_status))then
        begin
          Timeout := now + (OneSecond * 10);

          while (SERVICE_STOPPED <> svc_status.dwCurrentState) and (now < Timeout) do
          begin
            dwCheckPoint := svc_status.dwCheckPoint;
            Sleep(svc_status.dwWaitHint);

            if(not QueryServiceStatus(h_svc,svc_status))then
            begin
              // couldn't check status
              break;
            end;

            if(svc_status.dwCheckPoint < dwCheckPoint)then
              break;

          end;
        end;
      end;
      CloseServiceHandle(h_svc);
    end;
    CloseServiceHandle(h_manager);
  end;

  Result := SERVICE_STOPPED = svc_status.dwCurrentState;
end;

class function TWindowsService. ServiceGetStatus(const sMachine, sService: string ): Integer;
  {
  Results:

  SERVICE_STOPPED
  SERVICE_RUNNING
  SERVICE_PAUSED
  SERVICE_START_PENDING
  SERVICE_STOP_PENDING
  SERVICE_CONTINUE_PENDING
  SERVICE_PAUSE_PENDING
  }
var
  h_manager, h_svc: SC_Handle;
  service_status     : TServiceStatus;
  hStat : Integer;
begin
  hStat := -1;
  h_manager := OpenSCManager(PChar(sMachine) ,Nil,
                             SC_MANAGER_CONNECT);

  if h_manager > 0 then
  begin
    h_svc := OpenService(h_manager,PChar(sService),
                      SERVICE_QUERY_STATUS);

    if h_svc > 0 then
    begin
      if(QueryServiceStatus(h_svc, service_status)) then
        hStat := service_status.dwCurrentState;

      CloseServiceHandle(h_svc);
    end;

    CloseServiceHandle(h_manager);
  end;

  Result := hStat;
end;

class function TWindowsService. ServiceStatusToText(const Status: Integer): String;
begin
  case Status of
    -1: Result := 'Not installed';
    SERVICE_STOPPED: Result := 'Stopped';
    SERVICE_RUNNING: Result := 'Running';
    SERVICE_PAUSED: Result := 'Paused';
    SERVICE_START_PENDING: Result := 'Starting...';
    SERVICE_STOP_PENDING: Result := 'Stopping...';
    SERVICE_CONTINUE_PENDING: Result := 'Continuing...';
    SERVICE_PAUSE_PENDING: Result := 'Pausing...';
  else
    Result := 'Unknown';
  end; {of case}
end;

class function TWindowsService. IsServiceRunning(const sMachine, sService: String): Boolean;
begin
  Result := ServiceGetStatus(sMachine, sService) = SERVICE_RUNNING;
end;

end.


