{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network.PriorityHTTPQueue                   }
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

unit PTLib.Common.Network.PriorityHTTPQueue;

interface

uses
  SysUtils, Classes, Generics.Collections,

  PTLib.Common.Classes,
  PTLib.Common.Types,
  PTLib.Common.Strings,
  PTLib.Common.Interfaces,
  PTLib.Common.PriorityQueue,
  PTLib.Common.Network.HTTPRequestManager;

type
  IPriorityHTTPQueueDataItem = interface(IPriorityQueueItem)
    ['{A027C656-3010-488E-B965-86A233F05A60}']
    function GetBody: String;
    function GetHTTPRequestType: THTTPRequestType;
    function GetURL: String;
    function GetCallBack: THTTPRequestCallback;
    procedure SetBody(const Value: String);
    procedure SetHTTPRequestType(const Value: THTTPRequestType);
    procedure SetURL(const Value: String);
    procedure SetCallBack(const Value: THTTPRequestCallback);
    property URL: String read GetURL write SetURL;
    property Body: String read GetBody write SetBody;
    property HTTPRequestType: THTTPRequestType read GetHTTPRequestType write SetHTTPRequestType;
    property CallBack: THTTPRequestCallback read GetCallBack write SetCallBack;
  end;

  {$M+}
  TPriorityHTTPQueueDataItem = class(TPriorityQueueItem,
                                     IPriorityHTTPQueueDataItem)
  strict private
    FURL: String;
    FBody: String;
    FHTTPRequestType: THTTPRequestType;
    FCallBack: THTTPRequestCallback;
  private
    function GetBody: String;
    function GetHTTPRequestType: THTTPRequestType;
    function GetURL: String;
    function GetCallBack: THTTPRequestCallback;
    procedure SetBody(const Value: String);
    procedure SetHTTPRequestType(const Value: THTTPRequestType);
    procedure SetURL(const Value: String);
    procedure SetCallBack(const Value: THTTPRequestCallback);
  public
    property CallBack: THTTPRequestCallback read GetCallBack write SetCallBack;
  published
    property URL: String read GetURL write SetURL;
    property Body: String read GetBody write SetBody;
    property HTTPRequestType: THTTPRequestType read GetHTTPRequestType write SetHTTPRequestType;
  end;
  {$M-}

  TPTLibPriorityHTTPQueue = class(TPTLibMultiPriorityQueues)
  strict private
    FHTTPRequestManager: TPTLibHTTPRequestManager;
  private
    procedure RequestCallBack(const PTLibHTTPRequestManager: TObject;
      const HTTPRequest: IHTTPRequest; const HTTPResponse: IHTTPResponseEx);
  protected
    procedure DoProcessItem(const QueueItem: IPriorityQueueItem; var MessageSent: Boolean); override;
    procedure DoGetQueueItemClass(out QueueItemClass: TQueueItemClass); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SendRequest(const Priority: TPriorityQueuePriority; const URL, Body: String;
      const CallBack: THTTPRequestCallback = nil; const HTTPRequestType: THTTPRequestType = ssPOST; const ExpiresUTC: TDateTime = 0): IPriorityHTTPQueueDataItem;

    property HTTPRequestManager: TPTLibHTTPRequestManager read FHTTPRequestManager;
  end;

implementation

uses
  PTLib.Common.Log;

resourcestring
  StrHTTPRequestSentAt = 'HTTP request in "%s" sent at %s, status code %d%s - URL %s - ID %s';
  StrHTTPRequestQueued = 'HTTP Request queued in "%s" at %s, expires %s - URL %s - ID %s';

{ TPTLibQueuedHTTPRequestManager }

constructor TPTLibPriorityHTTPQueue.Create(AOwner: TComponent);
begin
  inherited;

  FHTTPRequestManager := TPTLibHTTPRequestManager.Create(Self);
end;

destructor TPTLibPriorityHTTPQueue.Destroy;
begin
  inherited;
end;

procedure TPTLibPriorityHTTPQueue.DoGetQueueItemClass(
  out QueueItemClass: TQueueItemClass);
begin
  QueueItemClass := TPriorityHTTPQueueDataItem;
end;

procedure TPTLibPriorityHTTPQueue.DoProcessItem(const QueueItem: IPriorityQueueItem;
  var MessageSent: Boolean);
var
  PriorityHTTPQueueDataItem: IPriorityHTTPQueueDataItem;
  ID: String;
  HTTPRequest: IHTTPRequest;
  HTTPResponse: IHTTPResponseEx;
begin
  // Make sure we support the queue item interface
  if Supports(QueueItem, IPriorityHTTPQueueDataItem, PriorityHTTPQueueDataItem) then
  begin
    ID := QueueItem.ID;

    // Send the message
    case PriorityHTTPQueueDataItem.HTTPRequestType of
      ssGET:
        begin
          MessageSent := FHTTPRequestManager.HTTPGet(
            PriorityHTTPQueueDataItem.URL,
            RequestCallBack,
            ID);
        end;

      ssPOST:
        begin
          MessageSent := FHTTPRequestManager.HTTPPost(
            PriorityHTTPQueueDataItem.URL,
            PriorityHTTPQueueDataItem.Body,
            RequestCallBack,
            ID);
        end;
    end;
  end
  else
  begin
    MessageSent := False;
  end;

  if (not MessageSent) and
     ((QueueItem.QueueName = QueueASync) or
      (QueueItem.QueueName = QueueLowPriority)) then
  begin
    // This is an ASync or LowPriority Request we need to fire the callback
    HTTPRequest := THTTPRequest.Create;
    HTTPRequest.URL := PriorityHTTPQueueDataItem.URL;

    HTTPResponse := THTTPResponseEx.Create;
    HTTPResponse.Error := EPTLibPriorityQueueError.Create('Host unavailable');
    HTTPResponse.HostUnavailable := True;

    RequestCallBack(
      FHTTPRequestManager,
      HTTPRequest,
      HTTPResponse);
  end;
end;

function TPTLibPriorityHTTPQueue.SendRequest(const Priority: TPriorityQueuePriority;
  const URL, Body: String; const CallBack: THTTPRequestCallback; const HTTPRequestType: THTTPRequestType;
  const ExpiresUTC: TDateTime): IPriorityHTTPQueueDataItem;
begin
  // Create the new queue item and set the URL
  Result := TPriorityHTTPQueueDataItem.Create;
  Result.URL := URL;
  Result.Body := Body;
  Result.CallBack := CallBack;
  Result.HTTPRequestType := HTTPRequestType;
  Result.QueueName := GetQueueNameFromPriority(Priority);
  Result.ExpiresUTC := ExpiresUTC;

  // Send to the queue
  QueueItem(Result);

  DoLog(
    StrHTTPRequestQueued,
    [Result.QueueName,
     DateTimeToStr(Result.QueuedUTC),
     DateTimeToStrOrNever(Result.ExpiresUTC),
     URL,
     Result.ID],
    LogSeverityDebug3);
end;

procedure TPTLibPriorityHTTPQueue.RequestCallBack(const PTLibHTTPRequestManager: TObject;
  const HTTPRequest: IHTTPRequest; const HTTPResponse: IHTTPResponseEx);
var
  QueueItem: IPriorityQueueItem;
  PriorityHTTPQueueDataItem: IPriorityHTTPQueueDataItem;
  Error: String;
begin
  // Inform the queue that the item has been processed
  ItemProcessed(
    HTTPRequest.ID,
    (not HTTPResponse.HostUnavailable) and ((HTTPResponse.StatusCode = 200) or (HTTPResponse.StatusCode = 404)),
    False,
    QueueItem);

  // If we find a callback, call it now and remove it from the dictionary
  if Supports(QueueItem, IPriorityHTTPQueueDataItem, PriorityHTTPQueueDataItem) then
  begin
    if HTTPResponse.Error = nil then
    begin
      Error := '';
    end
    else
    begin
      Error := ', Error "' + HTTPResponse.Error.Message + '"';
    end;

    DoLog(
      StrHTTPRequestSentAt,
      [PriorityHTTPQueueDataItem.QueueName,
       DateTimeToStr(PriorityHTTPQueueDataItem.SentUTC),
       HTTPResponse.StatusCode,
       Error,
       PriorityHTTPQueueDataItem.URL,
       PriorityHTTPQueueDataItem.ID],
      LogSeverityDebug3);

    if Assigned(PriorityHTTPQueueDataItem.CallBack) then
    begin
      PriorityHTTPQueueDataItem.CallBack(PTLibHTTPRequestManager, HTTPRequest, HTTPResponse);
    end;
  end;
end;

{ TPriorityHTTPQueueDataItem }

function TPriorityHTTPQueueDataItem.GetBody: String;
begin
  Result := FBody;
end;

function TPriorityHTTPQueueDataItem.GetCallBack: THTTPRequestCallback;
begin
  Result := FCallBack;
end;

function TPriorityHTTPQueueDataItem.GetHTTPRequestType: THTTPRequestType;
begin
  Result := FHTTPRequestType;
end;

function TPriorityHTTPQueueDataItem.GetURL: String;
begin
  Result := FURL;
end;

procedure TPriorityHTTPQueueDataItem.SetBody(const Value: String);
begin
  FBody := Value;
end;

procedure TPriorityHTTPQueueDataItem.SetCallBack(
  const Value: THTTPRequestCallback);
begin
  FCallBack := Value;
end;

procedure TPriorityHTTPQueueDataItem.SetHTTPRequestType(
  const Value: THTTPRequestType);
begin
  FHTTPRequestType := Value;
end;

procedure TPriorityHTTPQueueDataItem.SetURL(const Value: String);
begin
  FURL := Value;
end;

end.
