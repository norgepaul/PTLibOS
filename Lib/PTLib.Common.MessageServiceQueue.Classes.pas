unit PTLib.Common.MessageServiceQueue.Classes;

interface

uses
  SysUtils,

  PTLib.Common.Dates,
  PTLib.Common.Types,
  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TMessageServiceItem = class(TPTLibInterfacedObject, IMessageServiceItem)
  strict private
    FItemID: String;
    FQueuedUTC: TDateTime;
    FSentUTC: TDateTime;
    FFailedUTC: TDateTime;
    FCompletedUTC: TDateTime;
    FRetry: Integer;
    FRetryCount: Integer;
    FStatus: TMessageServiceItemStatus;
    FError: String;
  private
    function GetCompletedUTC: TDateTime;
    function GetError: String;
    function GetFailedUTC: TDateTime;
    function GetItemID: String;
    function GetQueuedUTC: TDateTime;
    function GetRetry: Integer;
    function GetRetryCount: Integer;
    function GetSentUTC: TDateTime;
    function GetStatus: TMessageServiceItemStatus;
    procedure SetCompletedUTC(const Value: TDateTime);
    procedure SetError(const Value: String);
    procedure SetFailedUTC(const Value: TDateTime);
    procedure SetItemID(const Value: String);
    procedure SetQueuedUTC(const Value: TDateTime);
    procedure SetRetry(const Value: Integer);
    procedure SetRetryCount(const Value: Integer);
    procedure SetSentUTC(const Value: TDateTime);
    procedure SetStatus(const Value: TMessageServiceItemStatus);
  protected
    procedure DoSend; virtual; abstract;
  public
    procedure Send;

    property ItemID: String read GetItemID write SetItemID;
    property QueuedUTC: TDateTime read GetQueuedUTC write SetQueuedUTC;
    property SentUTC: TDateTime read GetSentUTC write SetSentUTC;
    property FailedUTC: TDateTime read GetFailedUTC write SetFailedUTC;
    property CompletedUTC: TDateTime read GetCompletedUTC write SetCompletedUTC;
    property Retry: Integer read GetRetry write SetRetry;
    property RetryCount: Integer read GetRetryCount write SetRetryCount;
    property Status: TMessageServiceItemStatus read GetStatus write SetStatus;
    property Error: String read GetError write SetError;
  end;

implementation

{ TMessageServiceItem }

function TMessageServiceItem.GetCompletedUTC: TDateTime;
begin
  Result := FCompletedUTC;
end;

function TMessageServiceItem.GetError: String;
begin
  Result := FError;
end;

function TMessageServiceItem.GetFailedUTC: TDateTime;
begin
  Result := FFailedUTC;
end;

function TMessageServiceItem.GetItemID: String;
begin
  Result := FItemID;
end;

function TMessageServiceItem.GetQueuedUTC: TDateTime;
begin
  Result := FQueuedUTC;
end;

function TMessageServiceItem.GetRetry: Integer;
begin
  Result := FRetry;
end;

function TMessageServiceItem.GetRetryCount: Integer;
begin
  Result := FRetryCount;
end;

function TMessageServiceItem.GetSentUTC: TDateTime;
begin
  Result := FSentUTC;
end;

function TMessageServiceItem.GetStatus: TMessageServiceItemStatus;
begin
  Result := FStatus;
end;

procedure TMessageServiceItem.Send;
begin
  FSentUTC := NowUTC;
  try
    DoSend;

    FCompletedUTC := NowUTC;
  except
    on e: Exception do
    begin
      FError := e.Message;

      FFailedUTC := NowUTC;

      Inc(FRetry);
    end;
  end;
end;

procedure TMessageServiceItem.SetCompletedUTC(const Value: TDateTime);
begin
  FCompletedUTC := Value;
end;

procedure TMessageServiceItem.SetError(const Value: String);
begin
  FError := Value;
end;

procedure TMessageServiceItem.SetFailedUTC(const Value: TDateTime);
begin
  FFailedUTC := Value;
end;

procedure TMessageServiceItem.SetItemID(const Value: String);
begin
  FItemID := Value;
end;

procedure TMessageServiceItem.SetQueuedUTC(const Value: TDateTime);
begin
  FQueuedUTC := Value;
end;

procedure TMessageServiceItem.SetRetry(const Value: Integer);
begin
  FRetry := Value;
end;

procedure TMessageServiceItem.SetRetryCount(const Value: Integer);
begin
  FRetryCount := Value;
end;

procedure TMessageServiceItem.SetSentUTC(const Value: TDateTime);
begin
  FSentUTC := Value;
end;

procedure TMessageServiceItem.SetStatus(const Value: TMessageServiceItemStatus);
begin
  FStatus := Value;
end;

end.
