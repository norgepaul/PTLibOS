{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Scheduler                                   }
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

unit PTLib.Common.Scheduler;

interface

uses
  Windows, Classes, SysUtils, Messages,

  Generics.Collections,

  PTLib.Common.Log,
  PTLib.Common.Thread.Manager;

type
  TScheduleType = (stContinuously, stOnceADay);

  IScheduleParameters = interface
    ['{128190EE-3EF2-484E-AC20-D140A1F40E0B}']
    function GetInterval: Integer;
    function GetRandomOffset: Integer;
    function GetScheduleType: TScheduleType;

    property Interval: Integer read GetInterval;
    property RandomOffset: Integer read GetRandomOffset;
    property ScheduleType: TScheduleType read GetScheduleType;
  end;

  TScheduleParameters = class(TInterfacedObject, IScheduleParameters)
  private
    FInterval: Integer;
    FRandomOffset: Integer;
    FScheduleType: TScheduleType;

    function GetInterval: Integer;
    function GetRandomOffset: Integer;
    function GetScheduleType: TScheduleType;
  public
    constructor Create(Interval: Integer; RandomOffset: Integer = -1;
      ScheduleType: TScheduleType = stContinuously);
  end;

  TScheduledProc = procedure of object;

  TScheduledThread = class(TManagedThread)
  private
    FParameters: IScheduleParameters;
    FProc: TScheduledProc;
  protected
    procedure DoExecute; override;
  public
    constructor Create(Proc: TScheduledProc; Parameters: IScheduleParameters); reintroduce;
  end;

  TScheduler = class(TComponent)
  public const
    EverySecond = 1;
    EveryMinute = EverySecond * 60;
    EveryHour   = EveryMinute * 60;
    EveryDay    = EveryHour   * 24;
  private type
    TScheduledProcInfo = record
      Parameters: IScheduleParameters;
      Proc: TScheduledProc;
    end;
  private
    FCounter: UInt64;
    FSheduledProcs: TDictionary<Integer, TScheduledProcInfo>;
    FWndHandle: THandle;
    FTimerId: Cardinal;
    FThreadManager: TThreadManager;
    FActive: Boolean;
    procedure SetThreadManager(const Value: TThreadManager);
  protected
    procedure RunScheduledProcs;
    procedure TimerProc(var Message: TMessage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Schedule(ID: Integer; Proc: TScheduledProc; Parameters: IScheduleParameters);
    procedure Unschedule(ID: Integer);
  published
    property Active: Boolean read FActive write FActive;
    property ThreadManager: TThreadManager read FThreadManager write SetThreadManager;
  end;

implementation

uses
  DateUtils;

{ TScheduler }

constructor TScheduler.Create(AOwner: TComponent);
begin
  inherited;

  FSheduledProcs := TDictionary<Integer, TScheduledProcInfo>.Create;
  FCounter := 0;

  FWndHandle := AllocateHWnd(TimerProc);
  FTimerId := SetTimer(FWndHandle, 0, 1000, nil);

  FActive := True;
end;

procedure TScheduler.Unschedule(ID: Integer);
begin
  FSheduledProcs.Remove(ID);
end;

destructor TScheduler.Destroy;
begin
  KillTimer(FWndHandle, FTimerId);
  DeallocateHWnd(FWndHandle);

  FSheduledProcs.Free;

  inherited;
end;

procedure TScheduler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = TOperation.opRemove) and (AComponent = FThreadManager) then
    FThreadManager := nil;
end;

procedure TScheduler.RunScheduledProcs;
var
  CurrentTime: Integer;
  ScheduledProcPair: TPair<Integer, TScheduledProcInfo>;
begin
  if csDesigning in ComponentState then
    Exit;

  Assert(Assigned(FThreadManager));

  Inc(FCounter);
  CurrentTime := Trunc(TimeOf(Now) * 60 * 60 * 24);

  for ScheduledProcPair in FSheduledProcs do
    if ((ScheduledProcPair.Value.Parameters.ScheduleType = TScheduleType.stContinuously)
        and ((FCounter mod UInt64(ScheduledProcPair.Value.Parameters.Interval)) = 0))
    or
       ((ScheduledProcPair.Value.Parameters.ScheduleType = TScheduleType.stOnceADay)
        and (CurrentTime = ScheduledProcPair.Value.Parameters.Interval))
    then
    begin
      if FActive then
        FThreadManager.NewRequestThread(
          TScheduledThread.Create(ScheduledProcPair.Value.Proc, ScheduledProcPair.Value.Parameters));
    end;
end;

procedure TScheduler.Schedule(ID: Integer; Proc: TScheduledProc; Parameters: IScheduleParameters);
var
  ProcInfo: TScheduledProcInfo;
begin
  ProcInfo.Parameters := Parameters;
  ProcInfo.Proc := Proc;

  FSheduledProcs.AddOrSetValue(ID, ProcInfo);
end;

procedure TScheduler.SetThreadManager(const Value: TThreadManager);
begin
  if FThreadManager <> Value then
  begin
    if Assigned(FThreadManager) then
      FThreadManager.RemoveFreeNotification(Self);

    FThreadManager := Value;
    FThreadManager.FreeNotification(Self);
  end;
end;

procedure TScheduler.TimerProc(var Message: TMessage);
begin
  if (csDestroying in ComponentState) or not FActive then
    Exit;

  if Message.Msg = WM_TIMER then
    RunScheduledProcs
  else
    DefWindowProc(FWndHandle, Message.Msg, Message.wParam, Message.lParam);
end;

{ TScheduledThread }

constructor TScheduledThread.Create(Proc: TScheduledProc; Parameters: IScheduleParameters);
begin
  inherited Create;
  FParameters := Parameters;
  FProc := Proc;
end;

procedure TScheduledThread.DoExecute;
begin
  if FParameters.RandomOffset > 0 then
    Sleep(Random(FParameters.RandomOffset) * 1000);

  FProc;
end;

{ TScheduleParameters }

constructor TScheduleParameters.Create(Interval: Integer; RandomOffset: Integer; ScheduleType: TScheduleType);
begin
  inherited Create;
  FInterval := Interval;
  FRandomOffset := RandomOffset;
  FScheduleType := ScheduleType;
end;

function TScheduleParameters.GetInterval: Integer;
begin
  Result := FInterval;
end;

function TScheduleParameters.GetRandomOffset: Integer;
begin
  Result := FRandomOffset;
end;

function TScheduleParameters.GetScheduleType: TScheduleType;
begin
  Result := FScheduleType;
end;

end.
