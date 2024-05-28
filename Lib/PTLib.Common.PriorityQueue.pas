{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.PriorityQueue                               }
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

unit PTLib.Common.PriorityQueue;

interface

{.$DEFINE DEBUG_LOG_MESSAGES}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.DateUtils,

  {$IFDEF MSWINDOWS}
  WinApi.Windows,
  {$ENDIF}

  {$IFDEF LINUX64}
  Posix.Unistd,
  Posix.Stdio,
  {$ENDIF}

  PTLib.Common.Files,
  PTLib.Common.Log,
  PTLib.Common.Timers,
  PTLib.Common.Classes,
  PTLib.Common.Dates,
  PTLib.Common.Strings,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,
  PTLib.Common.InformationList;

const
  QueueASync = 'aync';
  QueueLowPriority = 'low_priority';
  QueueMediumPriority = 'medium_priority';
  QueueMediumPriorityNonPersistent = 'medium_priority_non_persistent';
  QueueHighPriority = 'high_priority';

type
  TPTLibPriorityQueues = class;

  {$M+}
  TPriorityQueueItem = class(TJSONSerialisable,
                             IPriorityQueueItem)
  strict private
    FID: String;
    FQueuedUTC: TDateTime;
    FSentUTC: TDateTime;
    FStatus: TPriorityQueueMessageStatus;
    FQueueName: String;
    FRetry: Integer;
    FExpiresUTC: TDateTime;
    FRetryAtUTC: TDateTime;

    function GetID: String;
    function GetQueuedUTC: TDateTime;
    function GetSentUTC: TDateTime;
    function GetRetry: Integer;
    function GetStatus: TPriorityQueueMessageStatus;
    function GetQueueName: String;
    function GetExpiresUTC: TDateTime;
    procedure SetID(const Value: String);
    procedure SetQueuedUTC(const Value: TDateTime);
    procedure SetSentUTC(const Value: TDateTime);
    procedure SetStatus(const Value: TPriorityQueueMessageStatus);
    procedure SetQueueName(const Value: String);
    procedure SetRetry(const Value: Integer);
    procedure SetExpiresUTC(const Value: TDateTime);
    function GetDescription: String;
  private
    function GetRetryAtURT: TDateTime;
    procedure SetRetryAtUTC(const Value: TDateTime);
  protected
    procedure DoGetDescription(out Description: String); virtual;
  published
    property ID: String read GetID write SetID;
    property QueuedUTC: TDateTime read GetQueuedUTC write SetQueuedUTC;
    property SentUTC: TDateTime read GetSentUTC write SetSentUTC;
    property Status: TPriorityQueueMessageStatus read GetStatus write SetStatus;
    property QueueName: String read GetQueueName write SetQueueName;
    property Retry: Integer read GetRetry write SetRetry;
    property ExpiresUTC: TDateTime read GetExpiresUTC write SetExpiresUTC;
    property RetryAtUTC: TDateTime read GetRetryAtURT write SetRetryAtUTC;
  end;
  {$M-}
  TQueueItemClass = class of TPriorityQueueItem;

  {$M+}
  TPriorityQueueDataItem = class(TPriorityQueueItem,
                                 IPriorityQueueDataItem)
  strict private
    FData: String;
  private
    function GetData: String;
    procedure SetData(const Value: String);
  published
    property Data: String read GetData write SetData;
  end;
  {$M-}

  TBasePriorityQueue = class(TObject)
  strict private
    FQueueName: String;
    FQueue: IList<IPriorityQueueItem>;
    FPriorityQueues: TPTLibPriorityQueues;
    FRetryAttempts: Integer;
    FMaxSendCount: Integer;
    FRetryDelayMS: Integer;
  protected
    property Queue: IList<IPriorityQueueItem> read FQueue;
    property PriorityQueues: TPTLibPriorityQueues read FPriorityQueues;

    procedure DoAdd(const QueueItem: IPriorityQueueItem; out Added: Boolean); virtual;
    procedure DoRemove(const ID: String; out Removed: Boolean); virtual;
    procedure DoGetNext(out QueueItem: IPriorityQueueItem); virtual;
    procedure DoRequeueItem(const ID: String; const IncrementRetryCount: Boolean); virtual;
    procedure DoFindItem(const ID: String; out QueueItem: IPriorityQueueItem); virtual;
  public
    constructor Create(const QueueName: String; const PriorityQueues: TPTLibPriorityQueues; const RetryAttempts: Integer); virtual;

    function Add(const QueueItem: IPriorityQueueItem): Boolean;
    function Remove(const ID: String): Boolean;
    function GetNext(out QueueItem: IPriorityQueueItem): Boolean;
    function FindQueueItem(const ID: String): IPriorityQueueItem;
    procedure Requeue(const ID: String; const IncrementRetryCount: Boolean = True);
    function GetQueue: IList<IPriorityQueueItem>;
    function QueueName: String;

    property MaxSendCount: Integer read FMaxSendCount write FMaxSendCount;
    property ResendDelayMS: Integer read FRetryDelayMS write FRetryDelayMS;
    property RetryAttempts: Integer read FRetryAttempts write FRetryAttempts;
  end;

  TVolatilePriorityQueue = class(TBasePriorityQueue)
  end;

  TBasePersistentPriorityQueue = class(TBasePriorityQueue)
  end;

  TSingleFilePersitentPriorityQueue = class(TBasePersistentPriorityQueue)
  strict private
    FFilename: String;
    FLoading: Boolean;
  protected
    function Filename: String;

    procedure DoAdd(const QueueItem: IPriorityQueueItem; out Added: Boolean); override;
    procedure DoRemove(const ID: String; out Removed: Boolean); override;

    procedure DoSaveQueue; virtual;
    procedure DoLoadQueue; virtual;
  public
    constructor Create(const QueueName: String; const PriorityQueues: TPTLibPriorityQueues; const RetryAttempts: Integer; const Directory: String); reintroduce;
  end;

  TMultiFilePersistentPriorityQueue = class(TBasePersistentPriorityQueue)
  strict private
    FDirectory: String;
  private
    function GetQueueItemFilename(const ID: String): String;
  protected
    function Directory: String;

    procedure DoAdd(const QueueItem: IPriorityQueueItem; out Added: Boolean); override;
    procedure DoRemove(const ID: String; out Removed: Boolean); override;

    procedure DoLoadQueue; virtual;
  public
    constructor Create(const QueueName: String; const PriorityQueues: TPTLibPriorityQueues; const RetryAttempts: Integer; const Directory: String); reintroduce;
  end;

  TOnProcessItem = procedure(Sender: TObject; const QueueItem: IPriorityQueueItem; out MessageSent: Boolean) of object;

  TPTLibPriorityQueues = class(TBasePTLibLogComponent,
                               IInformationProvider)
  private
    FOnProcessItem: TOnProcessItem;
    FQueues: TObjectList<TBasePriorityQueue>;

    FDirectory: String;
    FActive: Boolean;
    FQueueTimer: IPTLibGlobalTimer;
    FProcessing: TDictionary<String, IPriorityQueueItem>;
    FMaxSendCount: Integer;

    procedure SetActive(const Value: Boolean);
    procedure SetDirectory(const Value: String);
    procedure OnQueueTimer(Sender: TObject);
    function GetTotalQueued: Integer;
  protected
    procedure DoProcessItems; virtual;
    procedure DoOnProcessItem(const QueueItem: IPriorityQueueItem; var MessageSent: Boolean); virtual;
    procedure DoProcessItem(const QueueItem: IPriorityQueueItem; var MessageSent: Boolean); virtual;
    procedure DoItemProcessed(const ID: String; const Success: Boolean; const UnrecoverableError: Boolean; out QueueItem: IPriorityQueueItem); virtual;
    procedure DoSetActive(const Value: Boolean); virtual;
    procedure DoQueueItem(const QueueItem: IPriorityQueueItem); virtual;
    procedure DoRequeueItem(QueueItem: IPriorityQueueItem); virtual;
    procedure DoGetQueueItemClass(out QueueItemClass: TQueueItemClass); virtual;
    procedure DoItemRemoved(QueueItem: IPriorityQueueItem); virtual;

    procedure DoGetDefaultLogType(out LogType: String); override;

    procedure AddQueue(const Queue: TBasePriorityQueue); virtual;
    procedure RemoveQueue(const QueueName: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Add a queue item to the queue
    procedure QueueItem(const PriorityQueueItem: IPriorityQueueItem);

    // Confirm is a message has been sent successfully or failed
    procedure ItemProcessed(const ID: String; const Success: Boolean; const UnrecoverableError: Boolean; out QueueItem: IPriorityQueueItem);

    // Create a new queue item
    function NewQueueItem(const QueueName: String): IPriorityQueueItem; virtual;

    // Return a copy of a queue
    function GetQueueItems(const QueueName: String): IList<IPriorityQueueItem>;

    function GetQueueItemClass: TQueueItemClass;

    function GetInformation(const PropertySeparator: String = '='): IInformationList;

    function GetQueueFromName(const QueueName: String): TBasePriorityQueue;
  published
    property OnProcessItem: TOnProcessItem read FOnProcessItem write FOnProcessItem;

    property Active: Boolean read FActive write SetActive;
    property Directory: String read FDirectory write SetDirectory;
    property MaxSendCount: Integer read FMaxSendCount write FMaxSendCount;
    property TotalQueued: Integer read GetTotalQueued;
  end;

  TPTLibMultiPriorityQueues = class(TPTLibPriorityQueues)
  protected
    function GetQueueNameFromPriority(const Priority: TPriorityQueuePriority): String;

    // Override with the correct queue item class
    procedure DoSetActive(const Value: Boolean); override;
  public
    function GetQueueFromPriority(const Priority: TPriorityQueuePriority): TBasePriorityQueue;
  end;

  TPTLibMultiPriorityStringDataQueues = class(TPTLibMultiPriorityQueues)
  protected
    // Override with the correct queue item class
    procedure DoGetQueueItemClass(out QueueItemClass: TQueueItemClass); override;
  public
    // Create a new queue item with data
    function NewQueueItem(const Priority: TPriorityQueuePriority; const Data: String = ''): IPriorityQueueDataItem; reintroduce;
  end;

implementation

const
  MediumPriorityQueueFilename = 'MediumPriorityQueue.txt';
  HighPriorityQueueFilename = 'HighPriorityQueue.txt';
  QueueItemFileExtension = '.queueitem';

resourcestring
  StrUnableToConfirm = 'Unable to confirm sending of message ID "%s". Message not found in send queue';
  StrUnableToSaveHigh = 'Unable to save high priority queue to "%s" - %s';
  StrLoadingHigh = 'Loading high priority queue from "%s"';
  StrUnableToParseJSON = 'Unable to parse JSON queue item - %s - "%s"';
  StrPriorityQueueMustBeActive = 'String Queue must be active before Queueing a message';
  StrAddedSToTheQueue = 'Added %s to the queue';
  StrSkipped = 'Skipped %s as it is already in the queue';
  StrRemoved = 'Removed %s from the queue';
  StrUnableToRemove = 'Unable to remove "%s" from the queue as it doesn''t exist';
  StrMessageSRequeued = 'Message "%s" requeued (retry: %d)';
  StrMessageSSent = 'Message "%s" sent';
  StrInvalidPriorityType = 'Invalid Priority Queue Name';
  StrQueue = 'Queue';

{ TPTLibHighPriorityQueue }

constructor TSingleFilePersitentPriorityQueue.Create(const QueueName: String; const PriorityQueues: TPTLibPriorityQueues;
  const RetryAttempts: Integer; const Directory: String);
begin
  inherited Create(QueueName, PriorityQueues, RetryAttempts);

  FFilename := IncludeTrailingPathDelimiter(Directory) + QueueName + '.queue';

  // Make sure the directory exists
  ForceDirectories(ExtractFileDir(Filename));

  // Load the queue from disk
  DoLoadQueue;
end;

procedure TSingleFilePersitentPriorityQueue.DoLoadQueue;
var
  i: Integer;
  QueueItems: TStringList;
  QueueItem: IPriorityQueueItem;
begin
  // Check the file exists
  if FileExists(Filename) then
  begin
    {$IFDEF DEBUG_LOG_MESSAGES}
    PriorityQueues.DoLog(
      StrLoadingHigh,
      [Filename],
      LogSeverityDebug);
    {$ENDIF}

    // Clear the queue
    Queue.Clear;

    QueueItems := TStringList.Create;
    try
      // Load the queue strings from the file
      QueueItems.LoadFromFile(Filename);

      // Set the loading flag to stop the queue getting saved each time
      // we add a new item
      FLoading := True;
      try
        // Step through each line. Each line is a JSON object
        for i := 0 to pred(QueueItems.Count) do
        begin
          // Create the new queue item
          QueueItem := PriorityQueues.GetQueueItemClass.Create;

          // Deserialise the item from the JSON string
          try
            QueueItem.FromJSONString(QueueItems[i]);

            QueueItem.Status := TPriorityQueueMessageStatus.sqmReQueued;

            Add(QueueItem);
          except
            on e: Exception do
            begin
              PriorityQueues.DoLog(
                StrUnableToParseJSON,
                [e.Message,
                 QueueItems[i]],
                LogSeverityError);
            end;
          end;
        end;
      finally
        FLoading := False;
      end;
    finally
      FreeAndNil(QueueItems);
    end;
  end;
end;

procedure TSingleFilePersitentPriorityQueue.DoRemove(const ID: String;
  out Removed: Boolean);
begin
  inherited;

  // If the item has been removed, save the queue
  if Removed then
  begin
    DoSaveQueue;
  end;  
end;

procedure TSingleFilePersitentPriorityQueue.DoAdd(
  const QueueItem: IPriorityQueueItem; out Added: Boolean);
begin
  inherited;

  // If the item was added and we are not loading the queue, save it
  if (Added) and (not FLoading) then
  begin
    DoSaveQueue;
  end;
end;

procedure TSingleFilePersitentPriorityQueue.DoSaveQueue;
var
  i: Integer;
  QueueItems: TStringList;
begin
  QueueItems := TStringList.Create;
  try
    // Add all the queue items to the string list as JSON objects
    for i := 0 to pred(Queue.Count) do
    begin
      QueueItems.Add(Queue.Items[i].ToJSONString(False));
    end;

    // Save the queue file
    try
      QueueItems.SaveToFile(Filename);
    except
      on e: Exception do
      begin
        PriorityQueues.DoLog(
          StrUnableToSaveHigh,
          [Filename,
           e.Message],
          LogSeverityError);
      end;
    end;
  finally
    FreeAndNil(QueueItems);
  end;
end;

function TSingleFilePersitentPriorityQueue.Filename: String;
begin
  Result := FFilename;
end;

{ TPriorityQueueItem }

procedure TPriorityQueueItem.DoGetDescription(out Description: String);
begin
  Description := ID;
end;

function TPriorityQueueItem.GetDescription: String;
begin
  DoGetDescription(Result);
end;

function TPriorityQueueItem.GetExpiresUTC: TDateTime;
begin
  Result := FExpiresUTC;
end;

function TPriorityQueueItem.GetID: String;
begin
  Result := FID;
end;

function TPriorityQueueItem.GetQueueName: String;
begin
  Result := FQueueName;
end;

function TPriorityQueueItem.GetQueuedUTC: TDateTime;
begin
  Result := FQueuedUTC;
end;

function TPriorityQueueItem.GetRetry: Integer;
begin
  Result := FRetry;
end;

function TPriorityQueueItem.GetRetryAtURT: TDateTime;
begin
  Result := FRetryAtUTC;
end;

function TPriorityQueueItem.GetSentUTC: TDateTime;
begin
  Result := FSentUTC;
end;

function TPriorityQueueItem.GetStatus: TPriorityQueueMessageStatus;
begin
  Result := FStatus;
end;

procedure TPriorityQueueItem.SetExpiresUTC(const Value: TDateTime);
begin
  FExpiresUTC := Value;
end;

procedure TPriorityQueueItem.SetID(const Value: String);
begin
  FID := Value;
end;

procedure TPriorityQueueItem.SetQueueName(const Value: String);
begin
  FQueueName := Value;
end;

procedure TPriorityQueueItem.SetQueuedUTC(const Value: TDateTime);
begin
  FQueuedUTC := Value;
end;

procedure TPriorityQueueItem.SetRetry(const Value: Integer);
begin
  FRetry := Value;
end;

procedure TPriorityQueueItem.SetRetryAtUTC(const Value: TDateTime);
begin
  FRetryAtUTC := Value;
end;

procedure TPriorityQueueItem.SetSentUTC(const Value: TDateTime);
begin
  FSentUTC := Value;
end;

procedure TPriorityQueueItem.SetStatus(
  const Value: TPriorityQueueMessageStatus);
begin
  FStatus := Value;
end;

{ TPriorityQueues }

procedure TPTLibPriorityQueues.AddQueue(
  const Queue: TBasePriorityQueue);
begin
  FQueues.Add(Queue)
end;

constructor TPTLibPriorityQueues.Create(AOwner: TComponent);
begin
  inherited;

  FQueues := TObjectList<TBasePriorityQueue>.Create(True);
  FProcessing := TDictionary<String, IPriorityQueueItem>.Create;
  
  FDirectory := IncludeTrailingPathDelimiter('c:') + IncludeTrailingPathDelimiter('MessageQueues');

  FQueueTimer := TPTLibGobalTimerController.NewTimer(Self, OnQueueTimer, 100, False);
  //FQueueTimer.Enabled := False;
  //FQueueTimer.Interval := 100;
  //FQueueTimer.OnTimer := OnQueueTimer;

  FMaxSendCount := 20;
end;

destructor TPTLibPriorityQueues.Destroy;
begin
  Active := False;

  FreeAndNil(FProcessing);
  FreeAndNil(FQueues);

  inherited;
end;

procedure TPTLibPriorityQueues.OnQueueTimer(Sender: TObject);
begin
  DoProcessItems;
end;

procedure TPTLibPriorityQueues.QueueItem(
  const PriorityQueueItem: IPriorityQueueItem);
begin
  if not Active then
  begin
    raise EPTLibPriorityQueueError.Create(StrPriorityQueueMustBeActive);
  end
  else
  begin
    DoQueueItem(PriorityQueueItem);
  end;
end;

procedure TPTLibPriorityQueues.RemoveQueue(const QueueName: String);
begin

end;

procedure TPTLibPriorityQueues.ItemProcessed(const ID: String;
  const Success: Boolean; const UnrecoverableError: Boolean; out QueueItem: IPriorityQueueItem);
begin
  DoItemProcessed(ID, Success, UnrecoverableError, QueueItem);
end;

procedure TPTLibPriorityQueues.DoOnProcessItem(
  const QueueItem: IPriorityQueueItem; var MessageSent: Boolean);
begin
  if Assigned(FOnProcessItem) then
  begin
    FOnProcessItem(Self, QueueItem, MessageSent);
  end;
end;

procedure TPTLibPriorityQueues.DoProcessItem(
  const QueueItem: IPriorityQueueItem; var MessageSent: Boolean);
begin
  if MessageSent then
  begin
    {$IFDEF DEBUG_LOG_MESSAGES}
    DoLog(
      StrMessageSSent,
      [QueueItem.ID],
      LogSeverityDebug3);
    {$ENDIF}
  end;
end;

procedure TPTLibPriorityQueues.DoProcessItems;

  procedure ProcessQueue(const Queue: TBasePriorityQueue);
  var
    QueueItem: IPriorityQueueItem;
    MessageProcessed: Boolean;
    MaxCount: Integer;
  begin
    if Queue.MaxSendCount <> 0 then
    begin
      MaxCount := Queue.MaxSendCount;
    end
    else
    begin
      MaxCount := MaxSendCount;
    end;

    if ((MaxCount = 0) or
        (FProcessing.Count < MaxCount)) and
       (Queue.GetNext(QueueItem)) then
    begin
      // Has the message already expired?
      if (QueueItem.ExpiresUTC <> 0) and
         (QueueItem.ExpiresUTC < NowUTC) then
      begin
        Queue.Remove(QueueItem.ID);
      end
      else
      begin
        MessageProcessed := False;

        // Send the message
        DoProcessItem(QueueItem, MessageProcessed);

        if not MessageProcessed then
        begin
          DoOnProcessItem(QueueItem, MessageProcessed);
        end;

        if MessageProcessed then
        begin
          // Add to the sending list
          FProcessing.Add(QueueItem.ID, QueueItem);
        end
        else
        begin
          // Requeue the message
          if (Queue.RetryAttempts <> -1) and
             (QueueItem.Retry >= Queue.RetryAttempts) then
          begin
            // Never requeue low priority messages
            Queue.Remove(QueueItem.ID);
          end
          else
          begin
            // Requeue the message
            Queue.Requeue(QueueItem.ID, False);
          end;
        end;
      end;
    end;
  end;

var
  i: Integer;
begin
  for i := 0 to pred(FQueues.Count) do
  begin
    ProcessQueue(FQueues[i]);
  end;
end;

function TPTLibPriorityQueues.GetInformation(
  const PropertySeparator: String): IInformationList;

  procedure AddQueue(const Queue: TBasePriorityQueue);
  var
    QueueList: IList<IPriorityQueueItem>;
    PriorityQueueItem: IPriorityQueueItem;
    i: Integer;
  begin
    QueueList := Queue.GetQueue;

    if QueueList.Count > 0 then
    begin
      Result.AddHeading(Queue.QueueName + ' - ' + QueueList.Count.ToString + ' item(s)');

      for i := 0 to pred(QueueList.Count) do
      begin
        if Supports(QueueList[i], IPriorityQueueItem, PriorityQueueItem) then
        begin
          Result.AddProperty(PriorityQueueItem.ID, PriorityQueueItem.GetDescription + ' - Retry: ' + PriorityQueueItem.Retry.ToString);
        end;
      end;
    end;
  end;

var
  i: Integer;
begin
  Result := TInformationList.Create(PropertySeparator);

  for i := 0 to pred(FQueues.Count) do
  begin
    AddQueue(FQueues[i]);
  end;
end;

function TPTLibPriorityQueues.GetQueueFromName(const QueueName: String): TBasePriorityQueue;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(FQueues.Count) do
  begin
    if SameText(QueueName, FQueues[i].QueueName) then
    begin
      Result := FQueues[i];

      Break;
    end;
  end;

  if Result = nil then
  begin
    raise EPTLibPriorityQueueError.Create(StrInvalidPriorityType);
  end;
end;

function TPTLibPriorityQueues.GetQueueItemClass: TQueueItemClass;
begin
  DoGetQueueItemClass(Result);
end;

function TPTLibPriorityQueues.GetQueueItems(
  const QueueName: String): IList<IPriorityQueueItem>;
begin
  if Active then
  begin
    Result := GetQueueFromName(QueueName).GetQueue;
  end
  else
  begin
    Result := TList<IPriorityQueueItem>.Create;
  end;
end;

function TPTLibPriorityQueues.GetTotalQueued: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to pred(FQueues.Count) do
  begin
    Inc(Result, FQueues[i].Queue.Count);
  end;
end;

procedure TPTLibPriorityQueues.DoGetDefaultLogType(out LogType: String);
begin
  LogType := 'Priority Queue';
end;

procedure TPTLibPriorityQueues.DoGetQueueItemClass(
  out QueueItemClass: TQueueItemClass);
begin
  QueueItemClass := TPriorityQueueItem;
end;

procedure TPTLibPriorityQueues.DoItemProcessed(const ID: String; const Success: Boolean; const UnrecoverableError: Boolean; out QueueItem: IPriorityQueueItem);
var
  Queue: TBasePriorityQueue;
begin
  QueueItem := nil;

  if FProcessing.TryGetValue(ID, QueueItem) then
  begin
    FProcessing.Remove(ID);

    Queue := GetQueueFromName(QueueItem.QueueName);

    if (not UnrecoverableError) and
       (not Success) and
       ((Queue.RetryAttempts = -1) or
        (QueueItem.Retry < Queue.RetryAttempts)) then
    begin
      Queue.Requeue(ID);
    end
    else
    begin
      Queue.Remove(ID);
    end;
  end
  else
  begin
    DoLog(
      StrUnableToConfirm,
      [ID],
      LogSeverityError);
  end;
end;

procedure TPTLibPriorityQueues.DoItemRemoved(QueueItem: IPriorityQueueItem);
begin
 // Override
end;

function TPTLibPriorityQueues.NewQueueItem(const QueueName: String): IPriorityQueueItem;
begin
  Result := GetQueueItemClass.Create;
  Result.QueueName := QueueName;

  QueueItem(Result);
end;

procedure TPTLibPriorityQueues.DoQueueItem(const QueueItem: IPriorityQueueItem);
begin
  QueueItem.ID := CreateGUIDString;
  QueueItem.QueuedUTC := nowUTC;
  QueueItem.SentUTC := 0;
  QueueItem.Status := TPriorityQueueMessageStatus.sqmQueued;

  GetQueueFromName(QueueItem.QueueName).Add(QueueItem);
end;

procedure TPTLibPriorityQueues.DoRequeueItem(QueueItem: IPriorityQueueItem);
begin
  // Override
end;

procedure TPTLibPriorityQueues.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    DoSetActive(Value);
  end;
end;

procedure TPTLibPriorityQueues.DoSetActive(const Value: Boolean);
begin
  // If we are activating, clear the processing items
  if Value then
  begin
    FProcessing.Clear;
  end
  else
  begin
    FQueues.Clear;
  end;

  FQueueTimer.Enabled := (Value) and (not (csDesigning in ComponentState));

  FActive := Value;
end;

procedure TPTLibPriorityQueues.SetDirectory(const Value: String);
begin
  FDirectory := Value;
end;

{ TBasePriorityQueue }

constructor TBasePriorityQueue.Create(const QueueName: String; const PriorityQueues: TPTLibPriorityQueues;
  const RetryAttempts: Integer);
begin
  FQueueName := QueueName;
  FPriorityQueues := PriorityQueues;
  FRetryAttempts := RetryAttempts;
  FRetryDelayMS := 3000;

  FQueue := TList<IPriorityQueueItem>.Create;
end;

procedure TBasePriorityQueue.DoFindItem(const ID: String;
  out QueueItem: IPriorityQueueItem);
var
  i: Integer;
begin
  QueueItem := nil;

  // Try to find a matching queue item
  for i := 0 to pred(FQueue.Count) do
  begin
    if SameText(FQueue[i].ID, ID) then
    begin
      QueueItem := FQueue[i];

      Break;
    end;
  end;
end;

procedure TBasePriorityQueue.DoGetNext(out QueueItem: IPriorityQueueItem);
var
  i: Integer;
begin
  QueueItem := nil;

  // Search the queue for the next message that is queued
  for i := 0 to pred(FQueue.Count) do
  begin
    if (FQueue[i].Status in [TPriorityQueueMessageStatus.sqmQueued, TPriorityQueueMessageStatus.sqmReQueued]) and
       (FQueue[i].RetryAtUTC <= nowUTC) then
    begin
      QueueItem := FQueue[i];
      QueueItem.Status := TPriorityQueueMessageStatus.sqmSending;
      QueueItem.SentUTC := nowUTC;

      Break;
    end;
  end;
end;

procedure TBasePriorityQueue.DoAdd(const QueueItem: IPriorityQueueItem; out Added: Boolean);
var
  TempQueueItem: IPriorityQueueItem;
begin
  DoFindItem(QueueItem.ID, TempQueueItem);

  Added := TempQueueItem = nil;

  if Added then
  begin
    {$IFDEF DEBUG_LOG_MESSAGES}
    PriorityQueues.DoLog(
      StrAddedSToTheQueue,
      [QueueItem.ID],
      LogSeverityDebug3);
    {$ENDIF}
  end
  else
  begin
    PriorityQueues.DoLog(
      StrSkipped,
      [QueueItem.ID],
      LogSeverityWarning);
  end;

  if Added then
  begin    
    FQueue.Add(QueueItem);
  end;
end;

procedure TBasePriorityQueue.DoRemove(const ID: String;
  out Removed: Boolean);
var
  i: Integer;
begin
  Removed := False;

  // Try to find the message then remove it from the queue
  for i := 0 to pred(FQueue.Count) do
  begin
    if SameText(FQueue[i].ID, ID) then
    begin
      FPriorityQueues.DoItemRemoved(FQueue[i]);

      FQueue.Delete(i);

      Removed := True;

      Break;
    end;
  end;

  if Removed then
  begin
    {$IFDEF DEBUG_LOG_MESSAGES}
    PriorityQueues.DoLog(
      StrRemoved,
      [ID],
      LogSeverityDebug3);
    {$ENDIF}
  end
  else
  begin
    PriorityQueues.DoLog(
      StrUnableToRemove,
      [ID],
      LogSeverityWarning);
  end;
end;

procedure TBasePriorityQueue.DoRequeueItem(const ID: String; const IncrementRetryCount: Boolean);
var
  QueueItem: IPriorityQueueItem;
begin
  DoFindItem(ID, QueueItem);

  if QueueItem <> nil then
  begin
    FPriorityQueues.DoRequeueItem(QueueItem);

    QueueItem.SentUTC := 0;
    QueueItem.Status := TPriorityQueueMessageStatus.sqmReQueued;
    QueueItem.RetryAtUTC := nowUTC + (FRetryDelayMS * OneMillisecond);

    if IncrementRetryCount then
    begin
      QueueItem.Retry := QueueItem.Retry + 1;

      PriorityQueues.DoLog(
        StrMessageSRequeued,
        [ID, QueueItem.Retry],
        LogSeverityWarning);
    end;
  end;
end;

function TBasePriorityQueue.FindQueueItem(
  const ID: String): IPriorityQueueItem;
begin
  Result := nil;

  FQueue.Lock;
  try
    DoFindItem(ID, Result);
  finally
    FQueue.Unlock;
  end;
end;

function TBasePriorityQueue.GetNext(
  out QueueItem: IPriorityQueueItem): Boolean;
begin
  DoGetNext(QueueItem);

  Result := QueueItem <> nil;
end;

function TBasePriorityQueue.GetQueue: IList<IPriorityQueueItem>;
var
  i: Integer;
begin
  // Copy a the internal queue to the IList result
  Result := TList<IPriorityQueueItem>.Create;

  Queue.Lock;
  try
    for i := 0 to pred(Queue.Count) do
    begin
      Result.Add(Queue[i]);
    end;
  finally
    Queue.Unlock;
  end;
end;

function TBasePriorityQueue.QueueName: String;
begin
  Result := FQueueName;
end;

function TBasePriorityQueue.Add(const QueueItem: IPriorityQueueItem): Boolean;
begin
  FQueue.Lock;
  try
    DoAdd(QueueItem, Result);
  finally
    FQueue.Unlock;
  end;
end;

function TBasePriorityQueue.Remove(
  const ID: String): Boolean;
begin
  FQueue.Lock;
  try
    DoRemove(ID, Result);
  finally
    FQueue.Unlock;
  end;
end;

procedure TBasePriorityQueue.Requeue(const ID: String; const IncrementRetryCount: Boolean);
begin
  FQueue.Lock;
  try
    DoRequeueItem(ID, IncrementRetryCount);
  finally
    FQueue.Unlock;
  end;
end;

{ TPriorityQueueDataItem }

function TPriorityQueueDataItem.GetData: String;
begin
  Result := FData;
end;

procedure TPriorityQueueDataItem.SetData(const Value: String);
begin
  FData := Value;
end;

{ TPTLibPriorityStringDataQueues }

procedure TPTLibMultiPriorityStringDataQueues.DoGetQueueItemClass(
  out QueueItemClass: TQueueItemClass);
begin
  QueueItemClass := TPriorityQueueDataItem;
end;

function TPTLibMultiPriorityStringDataQueues.NewQueueItem(
  const Priority: TPriorityQueuePriority; const Data: String): IPriorityQueueDataItem;
begin
  Result := GetQueueItemClass.Create as IPriorityQueueDataItem;
  Result.SetData(Data);
  Result.QueueName := GetQueueNameFromPriority(Priority);

  QueueItem(Result);
end;

{ TMultiFilePersistentPriorityQueue }

constructor TMultiFilePersistentPriorityQueue.Create(const QueueName: String;
  const PriorityQueues: TPTLibPriorityQueues; const RetryAttempts: Integer;
  const Directory: String);
begin
  inherited Create(QueueName, PriorityQueues, RetryAttempts);

  FDirectory := IncludeTrailingPathDelimiter(Directory);

  // Make sure the directory exists
  ForceDirectories(Directory);

  // Load the queue from disk
  DoLoadQueue;
end;

function TMultiFilePersistentPriorityQueue.Directory: String;
begin
  Result := FDirectory;
end;

function TMultiFilePersistentPriorityQueue.GetQueueItemFilename(const ID: String): String;
begin
  Result := Directory + ID + QueueItemFileExtension;
end;

procedure TMultiFilePersistentPriorityQueue.DoAdd(
  const QueueItem: IPriorityQueueItem; out Added: Boolean);
begin
  inherited;

  if Added then
  begin
    QueueItem.SaveToFile(GetQueueItemFilename(QueueItem.ID));
  end;
end;

procedure TMultiFilePersistentPriorityQueue.DoLoadQueue;
var
  Files: IList<IFileInfo>;
  i: Integer;
  QueueItem: IPriorityQueueItem;
begin
  Queue.Clear;

  Files := TFileUtils.ScanFiles(Directory, '*' + QueueItemFileExtension, [IncludeFiles, IncludePaths]);

  for i := 0 to pred(Files.Count) do
  begin
    // Create the new queue item
    QueueItem := PriorityQueues.GetQueueItemClass.Create;

    // Deserialise the item from the JSON string
    try
      QueueItem.LoadFromFile(Files[i].Filename);
      QueueItem.Status := TPriorityQueueMessageStatus.sqmReQueued;

      Add(QueueItem);
    except
      on e: Exception do
      begin
        PriorityQueues.DoLog(
          StrUnableToParseJSON,
          [e.Message,
           Files[i].Filename],
          LogSeverityError);

        RenameFile(Files[i].Filename, Files[i].Filename + '.broken');
      end;
    end;
  end;
end;

procedure TMultiFilePersistentPriorityQueue.DoRemove(const ID: String;
  out Removed: Boolean);
begin
  inherited;

  if Removed then
  begin
    if not System.SysUtils.DeleteFile(GetQueueItemFilename(ID)) then
    begin
      if not RenameFile(GetQueueItemFilename(ID), GetQueueItemFilename(ID) + '.todelete') then
      begin
        PriorityQueues.DoLog(
          'Unable to delete or rename queue file "%s"',
          [GetQueueItemFilename(ID)],
          LogSeverityError);
      end;
    end;
  end;
end;

{ TPTLibMultiPriorityQueues }

procedure TPTLibMultiPriorityQueues.DoSetActive(const Value: Boolean);
begin
  inherited;

  if Value then
  begin
    AddQueue(TVolatilePriorityQueue.Create(QueueASync, Self, 0));
    AddQueue(TVolatilePriorityQueue.Create(QueueLowPriority, Self, 3));
    AddQueue(TMultiFilePersistentPriorityQueue.Create(QueueMediumPriority, Self, -1, Directory));
    AddQueue(TVolatilePriorityQueue.Create(QueueMediumPriorityNonPersistent, Self, MaxInt));
    AddQueue(TSingleFilePersitentPriorityQueue.Create(QueueHighPriority, Self, -1, Directory));
  end;
end;

function TPTLibMultiPriorityQueues.GetQueueFromPriority(
  const Priority: TPriorityQueuePriority): TBasePriorityQueue;
begin
  Result := GetQueueFromName(GetQueueNameFromPriority(Priority));
end;

function TPTLibMultiPriorityQueues.GetQueueNameFromPriority(
  const Priority: TPriorityQueuePriority): String;
begin
  case Priority of
    sqpASync: Result := QueueASync;
    sqpLow: Result := QueueLowPriority;
    sqpMedium: Result := QueueMediumPriority;
    sqpHigh: Result := QueueHighPriority;
    sqpMediumNonPersistent: Result := QueueMediumPriorityNonPersistent;
  else
    Result := '';
  end;
end;

end.


