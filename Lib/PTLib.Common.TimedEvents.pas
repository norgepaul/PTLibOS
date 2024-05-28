{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.TimedEvents                                 }
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

unit PTLib.Common.TimedEvents;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils,

  PTLib.Common.Log,
  PTLib.Common.Dates,
  PTLib.Common.Timers,
  PTLib.Common.Types,
  PTLib.Common.InformationList,
  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  ITimedEvent = interface
    ['{9101D986-C644-412D-8EDB-2AC51987ACEA}']
    function GetStartUTC: TDateTime;
    function GetTimedEventParamCallback: TTimedEventParamCallback;
    function GetParameters: IParameters;
    function GetEventName: String;
    function GetSender: TObject;
    function GetTimedEventCallback: TTimedEventCallback;
    function GetAutoRestart: Boolean;
    function GetDelayMS: Cardinal;
    function GetStartTicks: Cardinal;
    procedure SetParameters(const Value: IParameters);
    procedure SetEventName(const Value: String);
    procedure SetSender(const Value: TObject);
    procedure SetTimedEventParamCallback(const Value: TTimedEventParamCallback);
    procedure SetTimedEventCallback(const Value: TTimedEventCallback);
    procedure SetAutoRestart(const Value: Boolean);
    procedure SetDelayMS(const Value: Cardinal);
    procedure SetStartTicks(const Value: Cardinal);
    property TimedEventParamCallback: TTimedEventParamCallback read GetTimedEventParamCallback write SetTimedEventParamCallback;
    property Parameters: IParameters read GetParameters write SetParameters;
    property StartUTC: TDateTime read GetStartUTC;
    property Sender: TObject read GetSender write SetSender;
    property EventName: String read GetEventName write SetEventName;
    property TimedEventCallback: TTimedEventCallback read GetTimedEventCallback write SetTimedEventCallback;
    property AutoRestart: Boolean read GetAutoRestart write SetAutoRestart;
    property DelayMS: Cardinal read GetDelayMS write SetDelayMS;
    property StartTicks: Cardinal read GetStartTicks write SetStartTicks;
  end;

  TTimedEvent = class(TInterfacedObject,
                      ITimedEvent)
  private
    FTimedEventCallback: TTimedEventCallback;
    FTimedEventParamCallback: TTimedEventParamCallback;
    FParameters: IParameters;
    FStartTicks: Cardinal;
    FSender: TObject;
    FEventName: String;
    FAutoRestart: Boolean;
    FDelayMS: Integer;
  private
    function GetStartUTC: TDateTime;
    function GetTimedEventParamCallback: TTimedEventParamCallback;
    function GetParameters: IParameters;
    function GetEventName: String;
    function GetSender: TObject;
    function GetTimedEventCallback: TTimedEventCallback;
    procedure SetParameters(const Value: IParameters);
    procedure SetEventName(const Value: String);
    procedure SetSender(const Value: TObject);
    procedure SetTimedEventParamCallback(const Value: TTimedEventParamCallback);
    procedure SetTimedEventCallback(const Value: TTimedEventCallback);
    function GetAutoRestart: Boolean;
    procedure SetAutoRestart(const Value: Boolean);
    function GetDelayMS: Cardinal;
    procedure SetDelayMS(const Value: Cardinal);
    function RemainingTicks: Cardinal;
    function GetStartTicks: Cardinal;
    procedure SetStartTicks(const Value: Cardinal);
  public
    property TimedEventParamCallback: TTimedEventParamCallback read GetTimedEventParamCallback write SetTimedEventParamCallback;
    property Parameters: IParameters read GetParameters write SetParameters;
    property StartUTC: TDateTime read GetStartUTC;
    property StartTicks: Cardinal read GetStartTicks write SetStartTicks;
    property Sender: TObject read GetSender write SetSender;
    property EventName: String read GetEventName write SetEventName;
    property TimedEventCallback: TTimedEventCallback read GetTimedEventCallback write SetTimedEventCallback;
    property AutoRestart: Boolean read GetAutoRestart write SetAutoRestart;
    property DelayMS: Cardinal
     read GetDelayMS write SetDelayMS;
  end;

  TPTLibTimedEvents = class(TBasePTLibLoggerComponent,
                            ITimedEvents,
                            IInformationProvider)
  strict private
    FThreadTimer: IPTLibGlobalTimer;
    FCallbackList: IList<ITimedEvent>;
    FInterval: Integer;
  private
    procedure OnTimer(Sender: TObject);
    procedure SetInterval(const Value: Integer);
    function FindEventIndexBySenderName(const Sender: TObject; const EventName: String): Integer;
    procedure InternalAddEvent(const EventCallBack: TTimedEventCallback;
      const EventParamCallBack: TTimedEventParamCallback; const DelayMS: Cardinal;
      const Parameters: IParameters; const Sender: TObject; const EventName: String;
      const AutoRestart: Boolean); overload;
    procedure InternalAddEvent(TimedEvent: ITimedEvent); overload;
  protected
    procedure DoGetInformation(const InformationList: IInformationList); virtual;
    procedure DoGetDefaultLogType(out LogType: String); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure AddEvent(const EventCallBack: TTimedEventCallback; const DelayMS: Cardinal; const Sender: TObject = nil; const EventName: String = ''; const AutoRestart: Boolean = False); overload;
    procedure AddEvent(const EventParamCallBack: TTimedEventParamCallback; const DelayMS: Cardinal;
      const Parameters: IParameters; const Sender: TObject = nil; const EventName: String = ''; const AutoRestart: Boolean = False); overload;
    procedure AddEvent(const EventCallBack: TTimedEventParamCallback; const DelayMS: Cardinal;
      const ParameterNames: Array of String; const ParameterValues: Array of Variant; const Sender: TObject = nil; const EventName: String = ''; const AutoRestart: Boolean = False); overload;
    function GetEventTimeUTC(const Sender: TObject; const EventName: String = ''): TDateTime;
    function CancelEvent(const Sender: TObject; const EventName: String = ''): Boolean;
    function EventExists(const Sender: TObject; const EventName: String = ''): Boolean;
    function RestartEvent(const Sender: TObject; const EventName: String = ''): Boolean;
    function GetInformation(const PropertySeparator: String = '='): IInformationList;
  published
    property Interval: Integer read FInterval write SetInterval;
  end;

implementation

resourcestring
  StrEXCEPTIONS = 'EXCEPTION: %s';

{ TPTLibTimedEvents }

procedure TPTLibTimedEvents.InternalAddEvent(const EventCallBack: TTimedEventCallback;
  const EventParamCallBack: TTimedEventParamCallback; const DelayMS: Cardinal;
  const Parameters: IParameters; const Sender: TObject; const EventName: String;
  const AutoRestart: Boolean);
var
  TimedEvent: ITimedEvent;
begin
  // Create a new timed event interface
  TimedEvent := TTimedEvent.Create;
  TimedEvent.TimedEventCallback := EventCallBack;
  TimedEvent.TimedEventParamCallback := EventParamCallBack;
  TimedEvent.DelayMS := DelayMS;
  TimedEvent.Sender := Sender;
  TimedEvent.EventName := EventName;
  TimedEvent.AutoRestart := AutoRestart;

  // If we don't have a Parameter interface, create one now. This allows the callback
  // to assume that Parameters are assigned.
  if Parameters = nil then
  begin
    TimedEvent.Parameters := TParameters.Create;
  end
  else
  begin
    TimedEvent.Parameters := Parameters;
  end;

  // Remove events with the same name and owner
  if (Sender <> nil) and
     (EventName <> '') then
  begin
    CancelEvent(
      Sender,
      EventName);
  end;

  InternalAddEvent(TimedEvent);
end;

procedure TPTLibTimedEvents.InternalAddEvent(TimedEvent: ITimedEvent);
var
  i: Integer;
begin
  TimedEvent.StartTicks := TThread.GetTickCount + TimedEvent.DelayMS;

  FCallbackList.Lock;
  try
    // Insert the timed event in the correct position
    for i := 0 to pred(FCallbackList.Count) do
    begin
      if TimedEvent.StartTicks <= FCallbackList[i].StartTicks then
      begin
        FCallbackList.Insert(i, TimedEvent);

        Exit;
      end;
    end;

    // If we didn't find a position, add it to the end
    FCallbackList.Add(TimedEvent);
  finally
    FCallbackList.Unlock;
  end;
end;

procedure TPTLibTimedEvents.AddEvent(const EventParamCallBack: TTimedEventParamCallback;
  const DelayMS: Cardinal; const Parameters: IParameters; const Sender: TObject;
  const EventName: String; const AutoRestart: Boolean);
begin
  InternalAddEvent(
    nil,
    EventParamCallBack,
    DelayMS,
    Parameters,
    Sender,
    EventName,
    AutoRestart);
end;

procedure TPTLibTimedEvents.AddEvent(const EventCallBack: TTimedEventParamCallback;
  const DelayMS: Cardinal; const ParameterNames: array of String;
  const ParameterValues: array of Variant; const Sender: TObject;
  const EventName: String; const AutoRestart: Boolean);
var
  Parameters: IParameters;
begin
  Parameters := TParameters.Create;

  Parameters.SetParameters(
    ParameterNames,
    ParameterValues);

  AddEvent(
    EventCallBack,
    DelayMS,
    Parameters,
    Sender,
    EventName,
    AutoRestart);
end;

procedure TPTLibTimedEvents.AddEvent(const EventCallBack: TTimedEventCallback;
  const DelayMS: Cardinal; const Sender: TObject; const EventName: String; const AutoRestart: Boolean);
begin
  InternalAddEvent(
    EventCallBack,
    nil,
    DelayMS,
    nil,
    Sender,
    EventName,
    AutoRestart);
end;

function TPTLibTimedEvents.CancelEvent(
  const Sender: TObject; const EventName: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  FCallbackList.Lock;
  try
    for i := pred(FCallbackList.Count) downto 0 do
    begin
      if (Sender = FCallbackList[i].Sender) and
         (SameText(EventName, FCallbackList[i].EventName)) then
      begin
        FCallbackList.Delete(i);

        Result := True;

        Break;
      end;
    end;
  finally
    FCallbackList.Unlock;
  end;
end;

constructor TPTLibTimedEvents.Create(AOwner: TComponent);
begin
  inherited;

  FCallbackList := TList<ITimedEvent>.Create;

  FInterval := 50;

  FThreadTimer := TPTLibGobalTimerController.NewTimer(Self, OnTimer, FInterval, not (csDesigning in ComponentState));
end;

procedure TPTLibTimedEvents.DoGetDefaultLogType(out LogType: String);
begin
  LogType := 'Timed Events';
end;

procedure TPTLibTimedEvents.DoGetInformation(
  const InformationList: IInformationList);
var
  i: Integer;
  SenderClass: String;
begin
  FCallbackList.Lock;
  try
    for i := 0 to pred(FCallbackList.Count) do
    begin
      if FCallbackList[i].Sender = nil then
      begin
        SenderClass := 'None';
      end
      else
      begin
        SenderClass := FCallbackList[i].Sender.ClassName + ' (' + Integer(FCallbackList[i].Sender).ToString + ')';
      end;

      InformationList.AddProperty(
        i.ToString,
        'Event: %s | Expires: %s | Sender: %s',
        [FCallbackList[i].EventName,
         DateTimeToStringWithDifference(FCallbackList[i].StartUTC, nowUTC, False, 'Expired'),
         SenderClass])
    end;
  finally
    FCallbackList.Unlock;
  end;
end;

function TPTLibTimedEvents.EventExists(
  const Sender: TObject; const EventName: String): Boolean;
begin
  FCallbackList.Lock;
  try
    Result := FindEventIndexBySenderName(
      Sender,
      EventName) <> -1;
  finally
    FCallbackList.Unlock;
  end;
end;

function TPTLibTimedEvents.FindEventIndexBySenderName(
  const Sender: TObject; const EventName: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to pred(FCallbackList.Count) do
  begin
    if (Sender = FCallbackList[i].Sender) and
       (SameText(EventName, FCallbackList[i].EventName)) then
    begin
      Result := i;

      Break;
    end;
  end;
end;

function TPTLibTimedEvents.GetEventTimeUTC(const Sender: TObject; const EventName: String): TDateTime;
var
  idx: Integer;
begin
  FCallbackList.Lock;
  try
    idx := FindEventIndexBySenderName(
      Sender,
      EventName);

    if idx = -1 then
    begin
      Result := 0;
    end
    else
    begin
      Result := FCallbackList[idx].StartUTC;
    end;
  finally
    FCallbackList.Unlock;
  end;
end;

function TPTLibTimedEvents.GetInformation(const PropertySeparator: String = '='): IInformationList;
begin
  Result := TInformationList.Create;
  Result.PropertySeparator := PropertySeparator;

  DoGetInformation(Result);
end;

procedure TPTLibTimedEvents.OnTimer(Sender: TObject);
var
  CurrentTicks: Cardinal;
  TimedEvent: ITimedEvent;
begin
  CurrentTicks := TThread.GetTickCount;

  while (FCallBackList.Count > 0) and
        (FCallBackList[0].StartTicks < CurrentTicks) do
  begin
    // Make a copy, just in case the timed event is deleted in the callback
    TimedEvent := FCallBackList[0];
    try
      FCallBackList.Delete(0);

      if TimedEvent.TimedEventCallback <> nil then
      begin
        TimedEvent.TimedEventCallback();
      end
      else
      if TimedEvent.TimedEventParamCallback <> nil then
      begin
        TimedEvent.TimedEventParamCallback(TimedEvent.Parameters);
      end;
    except
      on e: exception do
      begin
        Log(
          StrEXCEPTIONS,
          [e.Message],
          LogSeverityError);
      end;
    end;

    if TimedEvent.AutoRestart then
    begin
      InternalAddEvent(TimedEvent);
    end;
  end;
end;

function TPTLibTimedEvents.RestartEvent(const Sender: TObject;
  const EventName: String): Boolean;
var
  i: Integer;
  Event: ITimedEvent;
begin
  Result := False;

  FCallbackList.Lock;
  try
    for i := pred(FCallbackList.Count) downto 0 do
    begin
      if (Sender = FCallbackList[i].Sender) and
         (SameText(EventName, FCallbackList[i].EventName)) then
      begin
        Event := FCallbackList[i];

        FCallbackList.Delete(i);

        InternalAddEvent(Event);

        Result := True;
      end;
    end;
  finally
    FCallbackList.Unlock;
  end;
end;

procedure TPTLibTimedEvents.SetInterval(const Value: Integer);
begin
  if FInterval <> Value then
  begin
    FThreadTimer.Enabled := False;
    FThreadTimer.Interval := Value;
    FThreadTimer.Enabled := (Value <> 0) and (not (csDesigning in ComponentState));
  end;
end;

{ TTimedEvent }

function TTimedEvent.GetAutoRestart: Boolean;
begin
  Result := FAutoRestart;
end;

function TTimedEvent.GetDelayMS: Cardinal;
begin
  Result := FDelayMS;
end;

function TTimedEvent.GetEventName: String;
begin
  Result := FEventName;
end;

function TTimedEvent.GetParameters: IParameters;
begin
  Result := FParameters;
end;

function TTimedEvent.GetSender: TObject;
begin
  Result := FSender;
end;

function TTimedEvent.GetStartTicks: Cardinal;
begin
  Result := FStartTicks;
end;

function TTimedEvent.GetStartUTC: TDateTime;
begin
  Result := nowUTC + (OneMilliSecond * RemainingTicks);
end;

function TTimedEvent.RemainingTicks: Cardinal;
begin
  Result := FStartTicks - TThread.GetTickCount;
end;

function TTimedEvent.GetTimedEventCallback: TTimedEventCallback;
begin
  Result := FTimedEventCallback;
end;

function TTimedEvent.GetTimedEventParamCallback: TTimedEventParamCallback;
begin
  Result := FTimedEventParamCallback;
end;

procedure TTimedEvent.SetAutoRestart(const Value: Boolean);
begin
  FAutoRestart := Value;
end;

procedure TTimedEvent.SetDelayMS(const Value: Cardinal);
begin
  FDelayMS := Value;
end;

procedure TTimedEvent.SetEventName(const Value: String);
begin
  FEventName := Value;
end;

procedure TTimedEvent.SetParameters(const Value: IParameters);
begin
  FParameters := Value;
end;

procedure TTimedEvent.SetSender(const Value: TObject);
begin
  FSender := Value;
end;

procedure TTimedEvent.SetStartTicks(const Value: Cardinal);
begin
  FStartTicks := Value;
end;

procedure TTimedEvent.SetTimedEventCallback(const Value: TTimedEventCallback);
begin
  FTimedEventCallback := Value;
end;

procedure TTimedEvent.SetTimedEventParamCallback(const Value: TTimedEventParamCallback);
begin
  FTimedEventParamCallback := Value;
end;

end.
