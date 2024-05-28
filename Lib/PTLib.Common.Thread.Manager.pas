{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Thread.Manager                              }
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

unit PTLib.Common.Thread.Manager;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Generics.Collections, ActiveX;

type
  TThreadManager = class;

  EManagedThreadError = class(Exception);

  IManagedThreadResult = interface
    ['{5F2F2D61-10F0-448A-818E-945AE5D1187D}']
    function GetTickCount: Cardinal;
    function GetError: String;
    function GetID: Int64;
    function GetRequestType: Integer;
    function GetResult: IInterface;

    procedure SetTickCount(const Value: Cardinal);
    procedure SetID(const Value: Int64);
    procedure SetError(const Value: String);
    procedure SetRequestType(const Value: Integer);
    procedure SetResult(const Value: IInterface);

    property ID: Int64 read GetID write SetID;
    property TickCount: Cardinal read GetTickCount write SetTickCount;
    property Error: String read GetError write SetError;
    property RequestType: Integer read GetRequestType write SetRequestType;
    property Result: IInterface read GetResult write SetResult;
  end;

  TManagedThreadResult = class(TInterfacedObject,
                               IManagedThreadResult)
  strict private
    FError: String;
    FTickCount: Cardinal;
    FID: Int64;
    FRequestType: Integer;
    FResult: IInterface;

    function GetTickCount: Cardinal;
    function GetError: String;
    function GetID: Int64;
    function GetRequestType: Integer;
    function GetResult: IInterface;

    procedure SetTickCount(const Value: Cardinal);
    procedure SetID(const Value: Int64);
    procedure SetError(const Value: String);
    procedure SetRequestType(const Value: Integer);
    procedure SetResult(const Value: IInterface);
  public
    property ID: Int64 read GetID write SetID;
    property TickCount: Cardinal read GetTickCount write SetTickCount;
    property Error: String read GetError write SetError;
    property RequestType: Integer read GetRequestType write SetRequestType;
    property Result: IInterface read GetResult write SetResult;
  end;

  TManagedThreadCallBack = reference to procedure(const ManagedThreadResult: IManagedThreadResult);
  TOnLog = procedure(Sender: TObject; const LogMessage: String) of object;

  TManagedThread = class(TThread)
  strict private
    FLogMessage: String;

    procedure LogMessageSync;
  private
    FID: Int64;
    FIsSynchronous: Boolean;
    FAutoCoInitialize: Boolean;
  protected
    FThreadManager: TThreadManager;
    FManagedThreadResult: IManagedThreadResult;

    // You must override this procedure in your descendant class. It's basically
    // the Thread's Execute procedure wrapped in some error handling
    procedure DoExecute; virtual; abstract;
    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute; virtual;

    procedure Log(const LogMessage: String); overload; virtual;
    procedure Log(const LogMessage: String; const Args: Array of const); overload; virtual;
  public
    constructor Create; virtual;

    procedure Execute; override;

    property Result: IManagedThreadResult read FManagedThreadResult;
  end;

  TManagedThreadRequest = class
  public
    ID: Int64;
    ManagedThreadCallBack: TManagedThreadCallBack;
    ManagedThread: TManagedThread;
    StartTicks: Cardinal;
    Owner: TObject;
  end;

  TOnRequest = procedure(Sender: TObject; const ManagedThreadRequest: TManagedThreadRequest) of object;

  TThreadManager = class(TComponent)
  strict private
    FOnStartRequest: TOnRequest;
    FOnFinishRequest: TOnRequest;
    FOnLog: TOnLog;

    FCurrentRequestID: Int64;
    FAutoCoInitialize: Boolean;
    FRequestCriticalSection: TCriticalSection;
    FRequests: TObjectDictionary<Int64, TManagedThreadRequest>;

    procedure RemoveRequest(const ID: Int64);
    procedure OnManagedThreadTerminate(Sender: TObject);
  protected
    procedure DoOnLog(const LogMessage: String); virtual;
    procedure DoManagedThreadTerminate(const ManagedThreadResult: IManagedThreadResult); virtual;
    procedure DoOnStartRequest(const ManagedThreadRequest: TManagedThreadRequest); virtual;
    procedure DoOnFinishRequest(const ManagedThreadRequest: TManagedThreadRequest); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function NewRequestThread(const ManagedThread: TManagedThread;
      const ManagedThreadCallBack: TManagedThreadCallBack = nil;
      const Owner: TObject = nil): Int64;

    procedure CancelRequest(const ID: Int64); virtual;
    procedure CancelRequests(const Owner: TObject = nil); virtual;
    procedure CancelRequestsAndWait; virtual;
  published
    property OnStartRequest: TOnRequest read FOnStartRequest write FOnStartRequest;
    property OnFinishRequest: TOnRequest read FOnFinishRequest write FOnFinishRequest;
    property OnLog: TOnLog read FOnLog write FOnLog;

    property AutoCoInitialize: Boolean read FAutoCoInitialize write FAutoCoInitialize;
  end;

implementation

{ TManagedThreadResult }

function TManagedThreadResult.GetError: String;
begin
  Result := FError;
end;

function TManagedThreadResult.GetID: Int64;
begin
  Result := FID;
end;

function TManagedThreadResult.GetRequestType: Integer;
begin
  Result := FRequestType;
end;

function TManagedThreadResult.GetResult: IInterface;
begin
  Result := FResult;
end;

function TManagedThreadResult.GetTickCount: Cardinal;
begin
  Result := FTickCount;
end;

procedure TManagedThreadResult.SetError(const Value: String);
begin
  FError := Value;
end;

procedure TManagedThreadResult.SetID(const Value: Int64);
begin
  FID := Value;
end;

procedure TManagedThreadResult.SetRequestType(const Value: Integer);
begin
  FRequestType := Value;
end;

procedure TManagedThreadResult.SetResult(const Value: IInterface);
begin
  FResult := Value;
end;

procedure TManagedThreadResult.SetTickCount(const Value: Cardinal);
begin
  FTickCount := Value;
end;

{ TThreadManager }

procedure TThreadManager.CancelRequest(const ID: Int64);
begin
  // Call the internal procedure
  RemoveRequest(ID);
end;

procedure TThreadManager.DoManagedThreadTerminate(
  const ManagedThreadResult: IManagedThreadResult);
var
  ManagedThreadRequest: TManagedThreadRequest;
begin
  // Deal with a reply from a web service
  FRequestCriticalSection.Enter;
  try
    // If there isn't an ID in the dictionary, something has gone wrong.
    if not FRequests.TryGetValue(ManagedThreadResult.ID, ManagedThreadRequest) then
    begin
      raise EManagedThreadError.CreateFmt('Unable to find managed thread ID: %d', [ManagedThreadRequest.ID])
    end
    else
    begin
      // Calculate the time it took to process the request
      ManagedThreadResult.TickCount := TThread.GetTickCount - ManagedThreadRequest.StartTicks;

      DoOnFinishRequest(ManagedThreadRequest);

      if ManagedThreadRequest.ManagedThreadCallBack <> nil then
      begin
        // Pass the result back to the caller
        ManagedThreadRequest.ManagedThreadCallBack(ManagedThreadResult);
      end;
    end;
  finally
    FRequestCriticalSection.Leave;
  end;
end;

procedure TThreadManager.OnManagedThreadTerminate(Sender: TObject);
begin
  try
    // Only call DoOnThreadTerminated if the user hasn't
    // cancelled the thread.
    { TODO : Perhaps add a queue for the replies }
    if not TManagedThread(Sender).Terminated then
    begin
      DoManagedThreadTerminate(TManagedThread(Sender).FManagedThreadResult);
    end;
  finally
    RemoveRequest(TManagedThread(Sender).FID);
  end;
end;

function TThreadManager.NewRequestThread(const ManagedThread: TManagedThread;
  const ManagedThreadCallBack: TManagedThreadCallBack; const Owner: TObject): Int64;
var
  ManagedThreadRequest: TManagedThreadRequest;
begin
  ManagedThreadRequest := TManagedThreadRequest.Create;

  FRequestCriticalSection.Enter;
  try
    // Increment the unique ID
    Inc(FCurrentRequestID);

    ManagedThreadRequest.ID := FCurrentRequestID;
    ManagedThreadRequest.StartTicks := TThread.GetTickCount;
    ManagedThreadRequest.ManagedThreadCallBack := ManagedThreadCallBack;
    ManagedThreadRequest.ManagedThread := ManagedThread;
    ManagedThreadRequest.ManagedThread.OnTerminate := OnManagedThreadTerminate;
    ManagedThreadRequest.ManagedThread.FID := FCurrentRequestID;
    ManagedThreadRequest.ManagedThread.FThreadManager := Self;
    ManagedThreadRequest.ManagedThread.FAutoCoInitialize := FAutoCoInitialize;
    ManagedThreadRequest.Owner := Owner;

    // Return the ID
    Result := FCurrentRequestID;

    // Add the request to the request dictionary
    FRequests.Add(FCurrentRequestID, ManagedThreadRequest);

    DoOnStartRequest(ManagedThreadRequest);

    ManagedThreadRequest.ManagedThread.Start;
  finally
    // Always make sure you leave the critical section
    FRequestCriticalSection.Leave;
  end;
end;

procedure TThreadManager.CancelRequests(const Owner: TObject);
var
  ID: Int64;
begin
  // Remove all the requests for a specific object
  FRequestCriticalSection.Enter;
  try
    for ID in FRequests.Keys do
    begin
      if (Owner = nil) or
         (FRequests[ID].Owner = Owner) then
      begin
        FRequests[ID].ManagedThread.Terminate;
        FRequests[ID].ManagedThreadCallBack := nil;
      end;
    end;
  finally
    FRequestCriticalSection.Leave;
  end;
end;

procedure TThreadManager.RemoveRequest(const ID: Int64);
begin
  // Remove a specific request from the list
  FRequestCriticalSection.Enter;
  try
    if FRequests.ContainsKey(ID) then
    begin
      // Terminate the thread now if it is still running
      if not FRequests[ID].ManagedThread.Terminated then
        FRequests[ID].ManagedThread.Terminate;

      FRequests.Remove(ID);
    end;
  finally
    FRequestCriticalSection.Leave;
  end;
end;

procedure TThreadManager.CancelRequestsAndWait;
var
  Threads: Array of THandle;
  i: Int64;
  Count: Integer;
  Wait: Cardinal;
begin
  FRequestCriticalSection.Enter;
  try
    SetLength(Threads, FRequests.Count);

    Count := 0;

    for i in FRequests.Keys do
    begin
      FRequests[i].ManagedThread.OnTerminate := nil;
      FRequests[i].ManagedThreadCallBack := nil;
      Threads[Count] := FRequests[i].ManagedThread.Handle;
      FRequests[i].ManagedThread.Terminate;

      Inc(Count);
    end;
  finally
    FRequestCriticalSection.Leave;
  end;

  repeat
    Wait := WaitForMultipleObjects(length(Threads), @Threads[0], True, 500);
  until Wait <> WAIT_TIMEOUT;
end;

constructor TThreadManager.Create(AOwner: TComponent);
begin
  inherited;

  FRequestCriticalSection := TCriticalSection.Create;
  FRequests := TObjectDictionary<Int64, TManagedThreadRequest>.Create([doOwnsValues]);

  FAutoCoInitialize := True;
end;

destructor TThreadManager.Destroy;
begin
  CancelRequestsAndWait;

  // Free the internal objects
  FreeAndNil(FRequests);
  FreeAndNil(FRequestCriticalSection);

  inherited;
end;

procedure TThreadManager.DoOnFinishRequest(
  const ManagedThreadRequest: TManagedThreadRequest);
begin
  if Assigned(FOnFinishRequest) then
    FOnFinishRequest(Self, ManagedThreadRequest);
end;

procedure TThreadManager.DoOnLog(const LogMessage: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, LogMessage);
end;

procedure TThreadManager.DoOnStartRequest(
  const ManagedThreadRequest: TManagedThreadRequest);
begin
  if Assigned(FOnStartRequest) then
    FOnStartRequest(Self, ManagedThreadRequest);
end;

{ TManagedThread }

constructor TManagedThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
end;

procedure TManagedThread.DoAfterExecute;
begin
  if FAutoCoInitialize then
    CoUninitialize;
end;

procedure TManagedThread.DoBeforeExecute;
begin
  if FAutoCoInitialize then
    CoInitialize(nil);
end;

procedure TManagedThread.Execute;
begin
  try
    FManagedThreadResult := TManagedThreadResult.Create;
    FManagedThreadResult.ID := FID;

    DoBeforeExecute;
    try
      // All the functionality is performed in the DoExecute procedure
      // Simply override it in descendent classes
      DoExecute;
    finally
      DoAfterExecute;
    end;
  except
    // We catch any exceptions here. Never allow an exception to leave
    // a thread. The calling thread needs to check the ThreadException
    // or Error properties
    on e: Exception do
    begin
      FManagedThreadResult.Error := e.Message;
    end;
  end;
end;

procedure TManagedThread.Log(const LogMessage: String);
begin
  // Synchronize the LogMessage back to the main thread
  FLogMessage := LogMessage;

  if FIsSynchronous then
    LogMessageSync
  else
    Synchronize(LogMessageSync);
end;

procedure TManagedThread.Log(const LogMessage: String;
  const Args: array of const);
begin
  Log(format(LogMessage, Args));
end;

procedure TManagedThread.LogMessageSync;
begin
  FThreadManager.DoOnLog(FLogMessage);
end;

end.

