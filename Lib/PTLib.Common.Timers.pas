unit PTLib.Common.Timers;

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
  System.SysUtils, System.Classes, System.Diagnostics,

  {$IFNDEF CONSOLE}
    {$if Defined(MSWINDOWS) and not Defined(FMX)}
      Vcl.ExtCtrls
    {$ELSE}
      FMX.Types
    {$ENDIF},
  {$ENDIF}

  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.Thread;

type
  {$IFNDEF CONSOLE}
  TPTLibTimer = class(TTimer);
  {$ENDIF}

  TPTLibGlobalTimer = class;

  TOnException = reference to procedure(e: Exception);

  TPTLibGobalTimerController = class
  strict private
    class var FTotalMS: Int64;

    class var FOnException: TOnException;

    class var FTimers: IList<TPTLibGlobalTimer>;
    {$IFNDEF CONSOLE}
    class var FTimer: TPTLibTimer;
    class var FTimerCheckInterval: Integer;
    class procedure Loop;
    class procedure Reset;
    class procedure OnTimer(Sender: TObject);
    class procedure SetTimerCheckInterval(const Value: Integer); static;
    {$ENDIF}
  protected
    class procedure RegisterTimer(const Timer: TPTLibGlobalTimer);
    class procedure UnRegisterTimer(const Timer: TPTLibGlobalTimer);
    class procedure DoException(e: Exception); 
  public
    class constructor Create;
    class destructor Destroy;

    {$IFDEF CONSOLE}
    // As there is no message loop in console applications you should call
    // Loop rgularly to fire the timers.
    class procedure Loop;
    {$ENDIF}

    class function NewTimer(const Name: String; const TimerProc: TTimerProc; const Interval: Cardinal = 1000; const Enabled: Boolean = True): IPTLibGlobalTimer; overload;
    class function NewTimer(const Owner: TObject; const TimerProc: TTimerProc; const Interval: Cardinal = 1000; const Enabled: Boolean = True): IPTLibGlobalTimer; overload;
    class function TimerInfo: IInformationList;

    class property OnException: TOnException read FOnException write FOnException;
    {$IFNDEF CONSOLE}
    class property TimerCheckInterval: Integer read FTimerCheckInterval write SetTimerCheckInterval;
    {$ENDIF}
  end;

  TPTLibGlobalTimer = class(TInterfacedObject,
                            IPTLibGlobalTimer)
  strict private
    FName: String;
    FEnabled: Boolean;
    FInterval: Cardinal;
  protected
    FNextTicks: Cardinal;
    FTimerProc: TTimerProc;
    FLastMS: Int64;

    procedure StartTimer;
  private
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    function GetEnabled: Boolean;
    function GetInterval: Cardinal;
    function GetOnTimer: TTimerProc;
    procedure SetOnTimer(const Value: TTimerProc);
    function GetName: String;
    function GetLastMS: Int64;
  public
    constructor Create(const Name: String; const TimerProc: TTimerProc; const Interval: Cardinal; const Enabled: Boolean);
    destructor Destroy; override;

    property Name: String read GetName;
    property OnTimer: TTimerProc read GetOnTimer write SetOnTimer;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property LastMS: Int64 read GetLastMS;
  end;

implementation

uses
  PTLib.Common.InformationList;

{ TPTLibGlobalTimer }

constructor TPTLibGlobalTimer.Create(const Name: String; const TimerProc: TTimerProc; const Interval: Cardinal; const Enabled: Boolean);
begin
  Assert(Assigned(TimerProc), 'TimerProc cannot be nil');

  TPTLibGobalTimerController.RegisterTimer(Self);

  FName := Name;
  FTimerProc := TimerProc;
  FInterval := Interval;
  FEnabled := Enabled;
end;

destructor TPTLibGlobalTimer.Destroy;
begin
  TPTLibGobalTimerController.UnRegisterTimer(Self);

  inherited;
end;

function TPTLibGlobalTimer.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TPTLibGlobalTimer.GetInterval: Cardinal;
begin
  Result := FInterval;
end;

function TPTLibGlobalTimer.GetLastMS: Int64;
begin
  Result := FLastMS;
end;

function TPTLibGlobalTimer.GetName: String;
begin
  Result := FName;
end;

function TPTLibGlobalTimer.GetOnTimer: TTimerProc;
begin
  Result := FTimerProc;
end;

procedure TPTLibGlobalTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;

    if FEnabled then
    begin
      StartTimer;
    end;
  end;
end;

procedure TPTLibGlobalTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
  begin
    FInterval := Value;

    StartTimer;
  end;
end;

procedure TPTLibGlobalTimer.SetOnTimer(const Value: TTimerProc);
begin
  FTimerProc := Value;
end;

procedure TPTLibGlobalTimer.StartTimer;
begin
  FNextTicks := TThread.GetTickCount + FInterval;
end;

{ TPTLibGobalTimerController }

class constructor TPTLibGobalTimerController.Create;
begin
  FTimers := TList<TPTLibGlobalTimer>.Create;

  {$IFNDEF CONSOLE}
  FTimer := TPTLibTimer.Create(nil);
  FTimer.OnTimer := OnTimer;
  FTimerCheckInterval := 50;

  Reset;
  {$ENDIF}
end;

class destructor TPTLibGobalTimerController.Destroy;
begin
  {$IFNDEF CONSOLE}
  FreeAndNil(FTimer);
  {$ENDIF}
end;

{$IFNDEF CONSOLE}
class procedure TPTLibGobalTimerController.OnTimer(Sender: TObject);
begin
  Loop;
end;

class procedure TPTLibGobalTimerController.SetTimerCheckInterval(
  const Value: Integer);
begin
  if FTimerCheckInterval <> Value then
  begin
    FTimerCheckInterval := Value;

    Reset;
  end;
end;

class procedure TPTLibGobalTimerController.Reset;
begin
  FTimer.Enabled := False;
  FTimer.Interval := FTimerCheckInterval;
  FTimer.Enabled := True;
end;
{$ENDIF}

class function TPTLibGobalTimerController.TimerInfo: IInformationList;
var
  i: Integer;
begin
  Result := TInformationList.Create;

  Result.AddProperty('Total Loop', FTotalMS.ToString + 'ms');

  for i := 0 to pred(FTimers.Count) do
  begin
    Result.AddProperty(FTimers[i].Name, format('Interval: %dms | Last Execution Time: %dms', [FTimers[i].Interval, FTimers[i].LastMS]));
  end;
end;

class procedure TPTLibGobalTimerController.Loop;
var
  i: Integer;
  Stopwatch, StopwatchTotal: TStopwatch;
begin
  StopwatchTotal.Reset;
  StopwatchTotal.Start;

  try
    for i := 0 to pred(FTimers.Count) do
    begin
      if (i < FTimers.Count) and
         (FTimers[i].Enabled) and
         (FTimers[i].Interval > 0) and
         (Assigned(FTimers[i].FTimerProc)) and
         (FTimers[i].FNextTicks <= TThread.GetTickCount) then
      begin
        try
          Stopwatch.Reset;
          Stopwatch.Start;

          FTimers[i].FTimerProc(FTimers[i]);

          FTimers[i].FLastMS := Stopwatch.ElapsedMilliseconds;
        except
          on e: Exception do
          begin
            DoException(e);
          end;
        end;

        FTimers[i].StartTimer;
      end;
    end;
  except
    on e: Exception do
    begin
      DoException(e);
    end;
  end;

  FTotalMS := StopwatchTotal.ElapsedMilliseconds;
end;

class function TPTLibGobalTimerController.NewTimer(const Owner: TObject; const TimerProc: TTimerProc; const Interval: Cardinal;
  const Enabled: Boolean): IPTLibGlobalTimer;
begin
  Result := NewTimer(
    Owner.ClassName,
    TimerProc,
    Interval,
    Enabled);
end;

class procedure TPTLibGobalTimerController.DoException(e: Exception);
begin
  if Assigned(FOnException) then
  begin
    FOnException(e);
  end
  else
  begin
    raise e;
  end;
end;

class function TPTLibGobalTimerController.NewTimer(const Name: String; const TimerProc: TTimerProc;
  const Interval: Cardinal; const Enabled: Boolean): IPTLibGlobalTimer;
begin
  Result := TPTLibGlobalTimer.Create(
    Name,
    TimerProc,
    Interval,
    Enabled);
end;


class procedure TPTLibGobalTimerController.RegisterTimer(
  const Timer: TPTLibGlobalTimer);
begin
  if FTimers.IndexOf(Timer) = -1 then
  begin
    FTimers.Add(Timer);
  end;
end;

class procedure TPTLibGobalTimerController.UnRegisterTimer(
  const Timer: TPTLibGlobalTimer);
var
  idx: Integer;
begin
  idx := FTimers.IndexOf(Timer);

  if idx <> -1 then
  begin
    FTimers.Delete(idx);
  end;
end;

end.


