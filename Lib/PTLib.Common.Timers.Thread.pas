unit PTLib.Common.Timers.Thread;

{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Timers                                      }
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

interface

uses
  SysUtils, Classes,

  PTLib.Common.Thread;

type
  TPTLibThreadTimer = class;

  TTimerThread = class(TPTLibThread)
  private
    FTimer: TPTLibThreadTimer;
  protected
    procedure DoExecute;
  public
    constructor CreateTimerThread(const Timer: TPTLibThreadTimer);
    procedure Execute; override;
  end;

  TPTLibThreadTimer = class(TComponent)
  private
    {$WARN SYMBOL_PLATFORM OFF}{$IFDEF MSWINDOWS}
    FPriority: TThreadPriority;
    {$ENDIF}{$WARN SYMBOL_PLATFORM ON}
    FInterval: Integer;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    FTimerThread: TTimerThread;
    FSynchronizedExecute: Boolean;

    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(const Value: Integer);
  protected
    procedure StartTimer;
    procedure StopTimer;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Integer read FInterval write SetInterval;
    property SynchronizedExecute: Boolean read FSynchronizedExecute write FSynchronizedExecute;
    {$WARN SYMBOL_PLATFORM OFF}{$IFDEF MSWINDOWS}
    property ThreadPriority: TThreadPriority read FPriority write FPriority default tpNormal;
    {$ENDIF}{$WARN SYMBOL_PLATFORM ON}
  end;

implementation

{ TTimerThread }

constructor TTimerThread.CreateTimerThread(const Timer: TPTLibThreadTimer);
begin
  inherited Create(True);

  FTimer := Timer;
  FreeOnTerminate := True;
end;

procedure TTimerThread.Execute;
var
  FinishTicks: Cardinal;
begin
  while not Terminated do
  begin
    FinishTicks := GetTickCount + Cardinal(FTimer.FInterval);

    while (not Terminated) and
          (FinishTicks >= GetTickCount) do
    begin
      Sleep(5);
    end;

    if not Terminated then
    begin
      if FTimer.SynchronizedExecute then
        Synchronize(DoExecute)
      else
        DoExecute;
    end;
  end;
end;

procedure TTimerThread.DoExecute;
begin
  if (not (csDestroying in FTimer.ComponentState)) and
     (Assigned(FTimer.OnTimer)) then
  begin
    FTimer.OnTimer(FTimer);
  end;
end;


{ TPTLibThreadTimer }

constructor TPTLibThreadTimer.Create(Owner: TComponent);
begin
  inherited;

  FTimerThread := nil;

  {$WARN SYMBOL_PLATFORM OFF}{$IFDEF MSWINDOWS}
  FPriority := tpNormal;
  {$ENDIF}{$WARN SYMBOL_PLATFORM ON}
  FSynchronizedExecute := True;
end;

destructor TPTLibThreadTimer.Destroy;
begin
  FOnTimer := nil;

  StopTimer;

  inherited;
end;

procedure TPTLibThreadTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;

    if FEnabled then
      StartTimer
    else
      StopTimer;
  end;
end;

procedure TPTLibThreadTimer.SetInterval(const Value: Integer);
begin
  if FInterval <> Value then
  begin
    Enabled := False;

    FInterval := Value;
  end;
end;

procedure TPTLibThreadTimer.StartTimer;
begin
  if not (csDesigning in ComponentState) then
  begin
    FTimerThread := TTimerThread.CreateTimerThread(Self);
    {$WARN SYMBOL_PLATFORM OFF}{$IFDEF MSWINDOWS}
    FTimerThread.Priority := FPriority;
    {$ENDIF}{$WARN SYMBOL_PLATFORM ON}
    FTimerThread.Start;
  end;
end;

procedure TPTLibThreadTimer.StopTimer;
begin
  if FTimerThread <> nil then
  begin
    FTimerThread.Terminate;

    FTimerThread := nil;
  end;
end;

end.


