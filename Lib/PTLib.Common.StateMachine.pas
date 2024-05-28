{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.StateMachine                                }
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

unit PTLib.Common.StateMachine;

// Based on TStateMachine by Malcolm Groves - https://github.com/malcolmgroves/TStateMachine

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.DateUtils,

  PTLib.Common.Interfaces,
  PTLib.Common.TypeInfo,
  PTLib.Common.Timers,
  PTLib.Common.Classes,
  PTLib.Common.Dates,
  PTLib.Common.InformationList;

{ TODO : What should happen if you add a Guard, OnEntry or OnExit and there is
  already one? Fail, or maintain a list? }
{ TODO : GuardProc and TransitionProc should take generic parameters that
  describe the states/triggers involved }

type
  EStateMachineException = class(EPTLibError);
  EGuardFailure = class(EStateMachineException);
  EUnknownTrigger = class(EStateMachineException);
  EUnknownState = class(EStateMachineException);
  EInvalidStateMachine = class(EStateMachineException);

  TGuardProc = reference to function: boolean;
  TTransitionProc = reference to procedure;
  TTimerProc = reference to procedure;
  TEnumeratedTypeDescriptionProc = reference to procedure(const EnumeratedTriggerValue: Integer; var Description: String);

  TStateErrorProc = reference to procedure(const e: Exception; const ErrorDescription: String; var Handled: Boolean);

  TStateMachineEventType = (
    setEnter,
    setLeave,
    setTimer,
    setTimeout,
    setTrigger
  );

  TTriggerHolder<TState, TTrigger> = class
  strict private
    FTrigger: TTrigger;
    FDestination: TState;
    FGuard: TGuardProc;
  public
    constructor Create(ATrigger: TTrigger; ADestination: TState;
      AGuard: TGuardProc = nil); virtual;
    function CanExecute: boolean;
    property Destination: TState read FDestination;
  end;

  TPTLibStateMachine<TState, TTrigger> = class;

  TTStateHolder<TState, TTrigger> = class
  strict private
    FTriggers: TObjectDictionary<TTrigger, TTriggerHolder<TState, TTrigger>>;
    FState: TState;
    FStateMachine: TPTLibStateMachine<TState, TTrigger>;
    FOnEntry: TTransitionProc;
    FOnExit: TTransitionProc;
    FTimeoutTrigger: TTrigger;
    FTimeoutMS: Cardinal;
    FOnTimer: TTimerProc;
    FTimerMS: Cardinal;
    FEnteredUTC: TDateTime;

    function GetTriggerCount: Integer;
  private
    procedure Execute(ATrigger: TTrigger);
  protected
    FTriggeredBy: TTrigger;

    procedure Enter;
    procedure Exit;
    procedure Timer;
  public
    constructor Create(AStateMachine: TPTLibStateMachine<TState, TTrigger>;
      AState: TState); virtual;
    destructor Destroy; override;
    function Trigger(ATrigger: TTrigger; ADestination: TState;
      AGuard: TGuardProc = nil): TTStateHolder<TState, TTrigger>;
    function OnEntry(AOnEntry: TTransitionProc): TTStateHolder<TState, TTrigger>;
    function OnExit(AOnExit: TTransitionProc): TTStateHolder<TState, TTrigger>;
    function OnTimeout(ATimeoutTrigger: TTrigger; TimeoutMS: Cardinal): TTStateHolder<TState, TTrigger>;
    function OnTimer(AOnTimer: TTimerProc; Interval: Cardinal = 50): TTStateHolder<TState, TTrigger>;
    function Initial: TTStateHolder<TState, TTrigger>;
    function TriggerExists(ATrigger: TTrigger) : boolean;
    property TriggerCount: Integer read GetTriggerCount;
    property TriggeredBy: TTrigger read FTriggeredBy;
    property State: TState read FState;
    property TimeoutTrigger: TTrigger read FTimeoutTrigger;
    property TimeoutMS: Cardinal read FTimeoutMS;
    property TimerMS: Cardinal read FTimerMS;
    property EnteredUTC: TDateTime read FEnteredUTC;
  end;

  TPTLibStateMachine<TState, TTrigger> = class
  strict private
    FOnEnterState: TTransitionProc;
    FOnLeaveState: TTransitionProc;
    FOnStateLeft: TTransitionProc;
    FOnStateEntered: TTransitionProc;
    FOnStateError: TStateErrorProc;
    FOnNeedStateDescription: TEnumeratedTypeDescriptionProc;
    FOnNeedTriggerDescription: TEnumeratedTypeDescriptionProc;

    FStates: TObjectDictionary<TState, TTStateHolder<TState, TTrigger>>;
    FCurrentState: TState;
    FPreviousState: TState;
    FInitialState: TState;
    FHasIntialValue: Boolean;
    FStateTigger: TTrigger;
    FActive: boolean;
    FTimer: IPTLibGlobalTimer;
    FTimeoutTicks: Cardinal;
    FTimerIntervalTicks: Cardinal;
    FTimeoutAt: TDateTime;
    FTransitioned: Boolean;
    FLastTimer: TDateTime;
    FName: String;

    function GetStateCount: Integer;
    procedure SetActive(const Value: boolean);
    function GetInitialState: TTStateHolder<TState, TTrigger>;
    function GetCurrentState: TTStateHolder<TState, TTrigger>;
  private
    procedure OnTimer(Sender: TObject);
    function GetPreviousState: TState;
    procedure TransitionToState(const AState: TState; const ATrigger: TTrigger); overload;
    procedure TransitionToState(const AState: TState; AFirstTime: boolean = False); overload;
    procedure SetInitialState(const AState: TState);
  protected
    procedure DoEnterState; virtual;
    procedure DoStateEntered; virtual;
    procedure DoLeaveState; virtual;
    procedure DoLeftState; virtual;
    procedure DoStateError(const e: Exception; const AState: TState; const ATrigger: TTrigger; const EventType: TStateMachineEventType); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const Name: String); overload; virtual;
    destructor Destroy; override;

    function AddState(AState: TState): TTStateHolder<TState, TTrigger>;

    procedure Trigger(ATrigger: TTrigger);
    procedure Restart;
    function TimeoutTicksRemaining: Cardinal;
    function TimeoutAt: TDateTime;
    function GetStateDescription(const AState: TState): String;
    function GetTriggerDescription(const ATrigger: TTrigger): String;

    function GetInformation(const PropertySeparator: String = '='): IInformationList; virtual;

    function OnEnterState(const AOnEnterState: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
    function OnStateEntered(const AOnStateEntered: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
    function OnLeaveState(const AOnLeaveState: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
    function OnStateLeft(const AOnStateLeft: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
    function OnStateError(const AOnStateError: TStateErrorProc): TPTLibStateMachine<TState, TTrigger>;
    function OnNeedStateDescription(const AOnNeedStateDescription: TEnumeratedTypeDescriptionProc): TPTLibStateMachine<TState, TTrigger>;
    function OnNeedTriggerDescription(const AOnNeedTriggerDescription: TEnumeratedTypeDescriptionProc): TPTLibStateMachine<TState, TTrigger>;

    property StateCount: Integer read GetStateCount;
    property CurrentState: TTStateHolder<TState, TTrigger> read GetCurrentState;
    property PreviousState: TState read GetPreviousState;
    property StateTigger: TTrigger read FStateTigger;
    property InitialState: TTStateHolder<TState, TTrigger> read GetInitialState;
    property Active: boolean read FActive write SetActive;
  end;

implementation


{ TTriggerHolder<TState, TTrigger> }

function TTriggerHolder<TState, TTrigger>.CanExecute: boolean;
begin
  if Assigned(FGuard) then
    Result := FGuard
  else
    Result := True;
end;

constructor TTriggerHolder<TState, TTrigger>.Create(ATrigger: TTrigger;
  ADestination: TState; AGuard: TGuardProc);
begin
  inherited Create;
  FTrigger := ATrigger;
  FDestination := ADestination;
  FGuard := AGuard;
end;


{ TTStateHolder<TState, TTrigger> }

function TTStateHolder<TState, TTrigger>.Trigger(ATrigger: TTrigger;
  ADestination: TState; AGuard: TGuardProc): TTStateHolder<TState, TTrigger>;
var
  LConfiguredTrigger: TTriggerHolder<TState, TTrigger>;
begin
  LConfiguredTrigger := TTriggerHolder<TState, TTrigger>.Create(ATrigger,
    ADestination, AGuard);

  FTriggers.Add(ATrigger, LConfiguredTrigger);

  Result := self;
end;

procedure TTStateHolder<TState, TTrigger>.Timer;
begin
  if Assigned(FOnTimer) then
  begin
    FOnTimer();
  end;
end;

constructor TTStateHolder<TState, TTrigger>.Create(AStateMachine
  : TPTLibStateMachine<TState, TTrigger>; AState: TState);
begin
  inherited Create;
  FStateMachine := AStateMachine;
  FTriggers := TObjectDictionary < TTrigger, TTriggerHolder < TState,
    TTrigger >>.Create([doOwnsValues]);
  FState := AState;
end;

destructor TTStateHolder<TState, TTrigger>.Destroy;
begin
  FreeAndNil(FTriggers);
  inherited;
end;

procedure TTStateHolder<TState, TTrigger>.Enter;
begin
  FEnteredUTC := nowUTC;

  if Assigned(FOnEntry) then
    FOnEntry();
end;

procedure TTStateHolder<TState, TTrigger>.Execute(ATrigger: TTrigger);
var
  LTrigger: TTriggerHolder<TState, TTrigger>;
begin
  if not FStateMachine.Active then
  begin
    FStateMachine.Active := True;
    //raise EStateMachineException.Create('StateMachine not active');
  end;

  if not FTriggers.TryGetValue(ATrigger, LTrigger) then
    raise EUnknownTrigger.CreateFmt('State "%s" does not support the trigger - %s',
      [FStateMachine.GetStateDescription(FState),
       FStateMachine.GetTriggerDescription(ATrigger)]);

  if not LTrigger.CanExecute then
    raise EGuardFailure.CreateFmt('Guard on State "%s", Trigger "%s" prevented execution',
      [FStateMachine.GetStateDescription(FState),
       FStateMachine.GetTriggerDescription(ATrigger)]);

  FStateMachine.TransitionToState(LTrigger.Destination, ATrigger);
end;

procedure TTStateHolder<TState, TTrigger>.Exit;
begin
  if not FStateMachine.Active then
    raise EStateMachineException.Create('StateMachine not active');

  if Assigned(FOnExit) then
    FOnExit();
end;

function TTStateHolder<TState, TTrigger>.GetTriggerCount: Integer;
begin
  if Assigned(FTriggers) then
    Result := FTriggers.Count;
end;

function TTStateHolder<TState, TTrigger>.Initial
  : TTStateHolder<TState, TTrigger>;
begin
  FStateMachine.SetInitialState(FState);
  Result := self;
end;

function TTStateHolder<TState, TTrigger>.TriggerExists(
  ATrigger: TTrigger): boolean;
var
  LTrigger: TTriggerHolder<TState, TTrigger>;
begin
  Result := FTriggers.TryGetValue(ATrigger, LTrigger);
end;

function TTStateHolder<TState, TTrigger>.OnTimer(
  AOnTimer: TTimerProc; Interval: Cardinal): TTStateHolder<TState, TTrigger>;
begin
  FOnTimer := AOnTimer;
  FTimerMS := Interval;

  Result := Self;
end;

function TTStateHolder<TState, TTrigger>.OnEntry(AOnEntry: TTransitionProc)
  : TTStateHolder<TState, TTrigger>;
begin
  FOnEntry := AOnEntry;
  Result := self;
end;

function TTStateHolder<TState, TTrigger>.OnExit(AOnExit: TTransitionProc)
  : TTStateHolder<TState, TTrigger>;
begin
  FOnExit := AOnExit;
  Result := self;
end;

function TTStateHolder<TState, TTrigger>.OnTimeout(ATimeoutTrigger: TTrigger;
  TimeoutMS: Cardinal): TTStateHolder<TState, TTrigger>;
begin
  FTimeoutMS := TimeoutMS;
  FTimeoutTrigger := ATimeoutTrigger;

  Result := Self;
end;

constructor TPTLibStateMachine<TState, TTrigger>.Create;
begin
  inherited Create;

  if FName <> '' then
  begin
    FTimer := TPTLibGobalTimerController.NewTimer(FName, OnTimer, 50);
  end
  else
  begin
    FTimer := TPTLibGobalTimerController.NewTimer(Self, OnTimer, 50);
  end;

  FStates := TObjectDictionary <TState, TTStateHolder<TState, TTrigger>>.Create([doOwnsValues]);
end;

constructor TPTLibStateMachine<TState, TTrigger>.Create(const Name: String);
begin
  FName := Name;

  if FName <> '' then
  begin
    FName := FName + '|StateMachine';
  end;

  Create;
end;

destructor TPTLibStateMachine<TState, TTrigger>.Destroy;
begin
  FStates.Free;

  inherited;
end;

procedure TPTLibStateMachine<TState, TTrigger>.DoEnterState;
begin
  if Assigned(FOnEnterState) then
    FOnEnterState();
end;

procedure TPTLibStateMachine<TState, TTrigger>.DoStateEntered;
begin
  if Assigned(FOnStateEntered) then
    FOnStateEntered();
end;

procedure TPTLibStateMachine<TState, TTrigger>.DoLeaveState;
begin
  if Assigned(FOnLeaveState) then
    FOnLeaveState();
end;

procedure TPTLibStateMachine<TState, TTrigger>.DoLeftState;
begin
  if Assigned(FOnStateLeft) then
    FOnStateLeft();
end;

procedure TPTLibStateMachine<TState, TTrigger>.DoStateError(const e: Exception; const AState: TState;
  const ATrigger: TTrigger; const EventType: TStateMachineEventType);

const
  StateMachineEventTypeDescriptions: Array[TStateMachineEventType] of String = (
    'Enter',
    'Leave',
    'Timer',
    'Timeout',
    'Trigger'
  );

var
  Handled: Boolean;
  ErrorDescription: String;
begin
  Handled := False;

  if Assigned(FOnStateError) then
  begin
    ErrorDescription := format('State machine exception (%s) in state "%s" triggered by "%s" in event type "%s": %s', [
      e.ClassName,
      GetStateDescription(AState),
      GetTriggerDescription(ATrigger),
      StateMachineEventTypeDescriptions[EventType],
      e.Message
    ]);

    FOnStateError(
      e,
      ErrorDescription,
      Handled);

    Handled := True;
  end;

  if not Handled then
  begin
    Restart;
    //raise e;
  end;
end;

function TPTLibStateMachine<TState, TTrigger>.OnEnterState(
  const AOnEnterState: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnEnterState := AOnEnterState;

  Result := Self;
end;

function TPTLibStateMachine<TState, TTrigger>.OnStateEntered(
  const AOnStateEntered: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnStateEntered := AOnStateEntered;

  Result := Self;
end;

function TPTLibStateMachine<TState, TTrigger>.OnLeaveState(
  const AOnLeaveState: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnLeaveState := AOnLeaveState;

  Result := Self;
end;

function TPTLibStateMachine<TState, TTrigger>.OnStateLeft(const AOnStateLeft: TTransitionProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnStateLeft := AOnStateLeft;

  Result := Self;
end;

function TPTLibStateMachine<TState, TTrigger>.OnNeedStateDescription(
  const AOnNeedStateDescription: TEnumeratedTypeDescriptionProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnNeedStateDescription := AOnNeedStateDescription;
end;

function TPTLibStateMachine<TState, TTrigger>.OnNeedTriggerDescription(
  const AOnNeedTriggerDescription: TEnumeratedTypeDescriptionProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnNeedTriggerDescription := AOnNeedTriggerDescription;
end;

function TPTLibStateMachine<TState, TTrigger>.OnStateError(
  const AOnStateError: TStateErrorProc): TPTLibStateMachine<TState, TTrigger>;
begin
  FOnStateError := AOnStateError;

  Result := Self;
end;

procedure TPTLibStateMachine<TState, TTrigger>.OnTimer(Sender: TObject);
begin
  FLastTimer := nowUTC;

  if CurrentState <> nil then
  begin
    if (FTimeoutTicks <> 0) and
       (FTimeoutTicks < TThread.GetTickCount) then
    begin
      try
        CurrentState.Execute(CurrentState.TimeoutTrigger);
      except
        on e: Exception do
        begin
          DoStateError(
            e,
            CurrentState.State,
            FStateTigger,
            TStateMachineEventType.setTimeout
          );
        end;
      end;
    end else
    if (FTimerIntervalTicks <> 0) and
       (FTimerIntervalTicks < TThread.GetTickCount) then
    begin
      try
        CurrentState.Timer;
      except
        on e: Exception do
        begin
          DoStateError(
            e,
            CurrentState.State,
            FStateTigger,
            TStateMachineEventType.setTimer
          );
        end;
      end;

      FTimerIntervalTicks := TThread.GetTickCount + CurrentState.TimerMS;
    end;
  end;
end;

procedure TPTLibStateMachine<TState, TTrigger>.Restart;
begin
  Active := False;
  Active := True;
end;

function TPTLibStateMachine<TState, TTrigger>.GetCurrentState
  : TTStateHolder<TState, TTrigger>;
var
  LCurrentState: TTStateHolder<TState, TTrigger>;
begin
  if not FStates.TryGetValue(FCurrentState, LCurrentState) then
    raise EUnknownState.Create('Unable to find Current State');

  Result := LCurrentState;
end;

function TPTLibStateMachine<TState, TTrigger>.GetInformation(const PropertySeparator: String): IInformationList;
begin
  Result := TInformationList.Create;
  Result.PropertySeparator := PropertySeparator;

  if (not Active) then
  begin
    Result.AddProperty('State', 'Not Active');
  end
  else
  begin
    Result.AddProperty('State', GetStateDescription(CurrentState.State));

    if FTransitioned then
    begin
      Result.AddProperty('Triggered By', GetTriggerDescription(FStateTigger));
      Result.AddProperty('Previous State', GetStateDescription(FPreviousState));
    end;

    if TimeoutAt <> 0 then
    begin
      Result.AddProperty(
        'State Timeout',
        DateTimeToStringWithDifference(TimeoutAt, now)); // + ' (' + SecondsToTimeString((FTimeoutTicks - TThread.GetTickCount) div 1000) + ')');
    end;
  end;
end;

function TPTLibStateMachine<TState, TTrigger>.GetInitialState
  : TTStateHolder<TState, TTrigger>;
var
  LInitialState: TTStateHolder<TState, TTrigger>;
begin
  if not FHasIntialValue then
    raise EInvalidStateMachine.Create('StateMachine has not initial state');

  if not FStates.TryGetValue(FInitialState, LInitialState) then
    raise EUnknownState.Create('Unable to find Initial State');

  Result := LInitialState;
end;

function TPTLibStateMachine<TState, TTrigger>.GetPreviousState: TState;
begin
  Result := FPreviousState;
end;

function TPTLibStateMachine<TState, TTrigger>.GetStateCount: Integer;
begin
  if Assigned(FStates) then
    Result := FStates.Count;
end;

function TPTLibStateMachine<TState, TTrigger>.GetStateDescription(
  const AState: TState): String;
begin
  Result := GetEnumName(TypeInfo(TState), AState);

  if Assigned(FOnNeedStateDescription) then
  begin
    FOnNeedStateDescription(GetEnumValue(TypeInfo(TState), Result), Result);
  end;
end;

function TPTLibStateMachine<TState, TTrigger>.GetTriggerDescription(
  const ATrigger: TTrigger): String;
begin
  Result := GetEnumName(TypeInfo(TTrigger), ATrigger);

  if Assigned(FOnNeedTriggerDescription) then
  begin
    FOnNeedTriggerDescription(GetEnumValue(TypeInfo(TTrigger), Result), Result);
  end;
end;

procedure TPTLibStateMachine<TState, TTrigger>.SetActive(const Value: boolean);
begin
  if FActive <> Value then
  begin
    if Value and not FHasIntialValue then
      raise EInvalidStateMachine.Create('StateMachine has no initial state specified');

    FActive := Value;

    if FActive then
    begin
      TransitionToState(FInitialState, True);

      FTransitioned := False;

      DoEnterState;
    end;

    FTimer.Enabled := FActive;
  end;
end;

procedure TPTLibStateMachine<TState, TTrigger>.SetInitialState(const AState: TState);
begin
  if FHasIntialValue then
    raise EInvalidStateMachine.Create('StatMachine cannot have two Initial States');

  FInitialState := AState;
  FHasIntialValue := True;
end;

procedure TPTLibStateMachine<TState, TTrigger>.TransitionToState(
  const AState: TState; const ATrigger: TTrigger);
begin
  TransitionToState(AState, False);

  CurrentState.FTriggeredBy := ATrigger;

  try
    // Fire the event
    DoEnterState;

    CurrentState.Enter;

    DoStateEntered;
  except
    on e: Exception do
    begin
      DoStateError(
        e,
        CurrentState.State,
        FStateTigger,
        TStateMachineEventType.setEnter
      );
    end;
  end;
end;

function TPTLibStateMachine<TState, TTrigger>.TimeoutAt: TDateTime;
begin
  Result := FTimeoutAt;
end;

function TPTLibStateMachine<TState, TTrigger>.TimeoutTicksRemaining: Cardinal;
begin
  if FTimeoutTicks = 0 then
  begin
    Result := 0;
  end
  else
  begin
    Result := FTimeoutTicks - TThread.GetTickCount;
  end;
end;

procedure TPTLibStateMachine<TState, TTrigger>.TransitionToState
  (const AState: TState; AFirstTime: boolean);
begin
  if not Active then
    raise EStateMachineException.Create('StateMachine not active');

  if not FStates.ContainsKey(AState) then
    raise EUnknownState.CreateFmt(
      'Unable to find Configured State "%s"',
      [GetStateDescription(AState)]);

  // only exit if not the first transition to initial state
  if not AFirstTime then
  begin
    try
      DoLeaveState;

      CurrentState.Exit;

      DoLeftState;
    except
      on e: Exception do
      begin
        DoStateError(
          e,
          CurrentState.State,
          FStateTigger,
          TStateMachineEventType.setLeave
        );
      end;
    end;
  end;

  // Set the previous and current states
  FPreviousState := FCurrentState;
  FCurrentState := AState;

  // Start the timeout timer
  if CurrentState.TimeoutMS > 0 then
  begin
    FTimeoutTicks := TThread.GetTickCount + CurrentState.TimeoutMS;
    FTimeoutAt := now + (CurrentState.TimeoutMS * OneMilliSecond);
  end
  else
  begin
    FTimeoutTicks := 0;
    FTimeoutAt := 0;
  end;

  // Start the timer
  if CurrentState.TimerMS > 0 then
  begin
    FTimerIntervalTicks := TThread.GetTickCount + CurrentState.TimerMS;
  end
  else
  begin
    FTimerIntervalTicks := 0;
    FTimeoutAt := 0;
  end;

  // Enter the new state
  if AFirstTime then
  begin
    try
      CurrentState.Enter;
    except
      on e: Exception do
      begin
        DoStateError(
          e,
          CurrentState.State,
          FStateTigger,
          TStateMachineEventType.setEnter
        );
      end;
    end;
  end;
end;

procedure TPTLibStateMachine<TState, TTrigger>.Trigger(ATrigger: TTrigger);
var
  AState: TState;
begin
  AState := CurrentState.State;

  try
    CurrentState.Execute(ATrigger);

    FStateTigger := ATrigger;

    FTransitioned := True;
  except
    on e: Exception do
    begin
      DoStateError(
        e,
        CurrentState.State,
        FStateTigger,
        TStateMachineEventType.setTrigger
      );

      Exit;
    end;
  end;
end;

function TPTLibStateMachine<TState, TTrigger>.AddState(AState: TState)
  : TTStateHolder<TState, TTrigger>;
begin
  Result := TTStateHolder<TState, TTrigger>.Create(self, AState);

  FStates.Add(AState, Result);
end;

end.
