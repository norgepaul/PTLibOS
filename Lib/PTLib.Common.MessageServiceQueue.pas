unit PTLib.Common.MessageServiceQueue;

interface

uses
  Classes, SysUtils, Generics.Collections, vcl.ExtCtrls, SyncObjs,
  XMLDoc, XMLIntf, XmlDom, ActiveX,

  IdMessage, IdSMTP, IdComponent, IdEmailAddress,
  IdAttachmentFile,  IdIOHandler,  IdIOHandlerSocket,
  IdIOHandlerStack, IdSSLOpenSSLHeaders,  IdSSL, IdSSLOpenSSL,
  IdExplicitTLSClientServerBase, IdHTTP, IdURI, IdText,

  PTLib.Common.MessageServiceQueue.Classes,

  PTLib.Common.Classes,
  PTLib.Common.Utils,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,
  PTLib.Common.Log;

type
  TExternalMessageQueue = class;

  TOnMessageEvent = procedure(Sender: TObject; const MessageItem: IMessageServiceItem) of object;
  TOnCommand = procedure(Sender: TObject; const Command: String) of object;

  TLoggedIdSMTP = class(TIdSMTP)
  private
    FOnCommand: TOnCommand;
  protected
    procedure PrepareCmd(var aCmd: string); override;
  published
    property OnCommand: TOnCommand read FOnCommand write FOnCommand;
  end;

  TExternalMessageQueueOptions = class(TPersistent)
  private
    procedure SetMaxThreads(const Value: Integer);
    procedure SetTimeout(const Value: Integer);
  protected
    FTimeout: Integer;
    FMaxThreads: Integer;
    FRetries: Integer;
  public
    constructor Create;

    procedure Assign(Source: TPersistent); override;
  published
    property MaxThreads: Integer read FMaxThreads write SetMaxThreads;
    property Retries: Integer read FRetries write FRetries;
    property Timeout: Integer read FTimeout write SetTimeout;
  end;

  TSMTPEmailOptions = class(TExternalMessageQueueOptions)
  strict private
    FEmailContentType: TMessageServiceEMailType;
    FHTMLHeader: TStringList;
    FHTMLFooter: TStringList;
    FPlainHeader: TStringList;
    FPlainFooter: TStringList;
    FServer: String;
    FUser: String;
    FPassword: String;
    FPort: Integer;
    FUseAuthentication: Boolean;
    FUseSSL: Boolean;
  private
    procedure SetHTMLFooter(const Value: TStringList);
    procedure SetHTMLHeader(const Value: TStringList);
    procedure SetPlainFooter(const Value: TStringList);
    procedure SetPlainHeader(const Value: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property EmailContentType: TMessageServiceEMailType read FEmailContentType write FEmailContentType;
    property Server: String read FServer write FServer;
    property User: String read FUser write FUser;
    property Password: String read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property UseAuthentication: Boolean read FUseAuthentication write FUseAuthentication;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property HTMLHeader: TStringList read FHTMLHeader write SetHTMLHeader;
    property HTMLFooter: TStringList read FHTMLFooter write SetHTMLFooter;
    property PlainHeader: TStringList read FPlainHeader write SetPlainHeader;
    property PlainFooter: TStringList read FPlainFooter write SetPlainFooter;
  end;

  TSMSOptions = class(TExternalMessageQueueOptions)
  strict private
    FSMSServiceType: TMessageServiceSMSProvider;
    FAPIKey: String;
    FAPISecret: String;
    FMaxMessageLength: Integer;
  published
    constructor Create;

    procedure Assign(Source: TPersistent); override;

    property SMSServiceType: TMessageServiceSMSProvider read FSMSServiceType write FSMSServiceType;
    property APIKey: String read FAPIKey write FAPIKey;
    property APISecret: String read FAPISecret write FAPISecret;
    property MaxMessageLength: Integer read FMaxMessageLength write FMaxMessageLength;
  end;

  TBaseMessageQueueThread = class(TThread)
  protected
    FMessageItem: IMessageServiceItem;
    FMessageQueue: TExternalMessageQueue;
    FActive: Boolean;
    FExternalMessageQueueOptions: TExternalMessageQueueOptions;

    procedure SendMessagelInternal; virtual;
    procedure ConnectInternal; virtual;
    procedure DisconnectInternal; virtual;
  public
    constructor Create(const MessageQueue: TExternalMessageQueue; const ExternalMessageQueueOptions: TExternalMessageQueueOptions);
    destructor Destroy; override;

    procedure Execute; override;
    function SendMessage(MessageItem: IMessageServiceItem): Boolean; virtual;

    property MessageItem: IMessageServiceItem read FMessageItem;
  end;

  TEmailThread = class(TBaseMessageQueueThread)
  strict private
    FSMTPClient: TLoggedIdSMTP;
    FSSL: TIdSSLIOHandlerSocketOpenSSL;
    FSMTPEmailOptions: TSMTPEmailOptions;
  private
    procedure CreateSMTPClient;
    procedure DestroySMTPClient;
    procedure OnSMTPCommand(Sender: TObject; const Command: String);
    procedure OnIdStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    function Connected: Boolean;
  protected
    procedure SendMessagelInternal; override;
    procedure ConnectInternal; override;
    procedure DisconnectInternal; override;
  public
    constructor Create(MessageQueue: TExternalMessageQueue; const SMTPEmailOptions: TSMTPEmailOptions);
    destructor Destroy; override;
  end;

  TSMSThread = class(TBaseMessageQueueThread)
  protected
    FSMSOptions: TSMSOptions;

    procedure SendMessagelInternal; override;
  public
    constructor Create(MessageQueue: TExternalMessageQueue; const SMSOptions: TSMSOptions);
    destructor Destroy; override;
  end;

  TMessageQueueOptions = class(TPersistent)
  strict private
    FSMTPEmailSettings: TSMTPEmailOptions;
    FSMSOptions: TSMSOptions;

    procedure SetSMSOptions(const Value: TSMSOptions);
    procedure SetSMTPEmailSettings(const Value: TSMTPEmailOptions);
  published
    constructor Create;
    destructor Destroy; override;

    property SMTPEmailSettings: TSMTPEmailOptions read FSMTPEmailSettings write SetSMTPEmailSettings;
    property SMSOptions: TSMSOptions read FSMSOptions write SetSMSOptions;
  end;

  TUserMessageDestinationQueue = class(TObject)
  strict private
    FMessageQueue: TQueue<IMessageServiceItem>;
    FActiveThreads: TDictionary<Pointer, TBaseMessageQueueThread>;
    FThreadPool: TQueue<TBaseMessageQueueThread>;
    FSentMessages: TQueue<IMessageServiceItem>;
    FSentMessagesCS: TCriticalSection;
    FCompletedCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CancelQueuedMessages;
    procedure IncProcessedCount;

    property MessageQueue: TQueue<IMessageServiceItem> read FMessageQueue;
    property ActiveThreads: TDictionary<Pointer, TBaseMessageQueueThread> read FActiveThreads;
    property ThreadPool: TQueue<TBaseMessageQueueThread> read FThreadPool;
    property SentMessages: TQueue<IMessageServiceItem> read FSentMessages;
    property SentMessagesCS: TCriticalSection read FSentMessagesCS;
    property CompletedCount: Integer read FCompletedCount;
  end;

  TUserMessageDestinationQueues = class(TObjectDictionary<TExternalMessageDestination, TUserMessageDestinationQueue>)
  public
    constructor Create;
    destructor Destroy; override;

    procedure CancelQueuedMessages;
  end;

  TExternalMessageQueue = class(TBasePTLibLogComponent)
  private
    FOnMessageSending: TOnMessageEvent;
    FOnMessageSent: TOnMessageEvent;
    FOnMessageFailed: TOnMessageEvent;

    FMessageQueueOptions: TMessageQueueOptions;
    FActive: Boolean;
    FQueueInterval: Integer;
    FQueueTimer: TTimer;
    UserMessageDestinationQueues: TUserMessageDestinationQueues;
    FUserMessageDestinations: TExternalMessageDestinations;

    procedure SetActive(const Value: Boolean);
    procedure SetQueueInterval(const Value: Integer);
    procedure OnQueueTimer(Sender: TObject);
    function ProcessQueues: Boolean;
    procedure OnMessageThreadTerminate(Sender: TObject);
    procedure SetMessageQueueOptions(const Value: TMessageQueueOptions);
    procedure SetUserMessageDestinations(const Value: TExternalMessageDestinations);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoOnMessageSending(const MessageItem: IMessageServiceItem); virtual;
    procedure DoOnMessageSent(const MessageItem: IMessageServiceItem); virtual;
    procedure DoOnMessageFailed(const MessageItem: IMessageServiceItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SendMessage(const ExternalMessage: IMessageServiceItem);

    function GetQueueCount(const UserMessageDestination: TExternalMessageDestination): Integer;
    function GetActiveCount(const UserMessageDestination: TExternalMessageDestination): Integer;
    function GetPooledCount(const UserMessageDestination: TExternalMessageDestination): Integer;
    function GetCompletedCount(const UserMessageDestination: TExternalMessageDestination): Integer;
    function GetProcessedCount(const UserMessageDestination: TExternalMessageDestination): Integer;

    function GetCompletedMessage(const UserMessageDestination: TExternalMessageDestination; var MessageItem: IMessageServiceItem): Boolean;

    procedure CancelAllQueuedMessages;
  published
    property OnMessageSending: TOnMessageEvent read FOnMessageSending write FOnMessageSending;
    property OnMessageSent: TOnMessageEvent read FOnMessageSent write FOnMessageSent;
    property OnMessageFailed: TOnMessageEvent read FOnMessageFailed write FOnMessageFailed;

    property Active: Boolean read FActive write SetActive;
    property QueueInterval: Integer read FQueueInterval write SetQueueInterval;
    property Options: TMessageQueueOptions read FMessageQueueOptions write SetMessageQueueOptions;
    property UserMessageDestinations: TExternalMessageDestinations read FUserMessageDestinations write SetUserMessageDestinations;
  end;

implementation

resourcestring
  StrUnableToLoadSSLDLLs = 'Unable to load SSL dlls';
  StrErrorSendingEmail = 'Error Sending Email ID %d - %s';
  StrErrorSendingEmailRetry = 'Error sending %s ID: %d - %s [%d remaining retry(s)]';
  StrEmailSentIdD = 'Email sent [Id: %d, To: %s, CC: %s, BCC: %s, Subject: %s, Attachment: %s]';
  StrNo = 'No';
  StrYes = 'Yes';
  StrEmailConnectionError = 'Connection Error: %s';
  StrEmailErrorPurging = 'Error purging queue - %s';
  StrEmailPurgingEmail = 'Purging email queue.';
  StrEmailConnectingTo = 'Connecting to %s';
  StrEmailEmailSentTo = 'Email sent to %s, CC: %s, BCC: %s [%s]';
  StrEmailDPendingEmailsFound = 'Processing pending emails.';
  StrEmailNoPendingEmailsFound = 'No pending emails found';
  StrEmailRetrievingPendingEmails = 'Retrieving pending emails';
  StrEmailConnecting = 'Connecting';
  StrEmailCriticalError = 'Critical Error Sending %s ID: %d - %s';
  StrConnectedToS = 'Connected to %s';
  StrUpdatingDatabaseMessageQueue = 'Updating database email queue';
  StrDeletingEmailAttachment = 'Deleting email attachment file: %s';
  StrUnableToDeleteAttachment = 'Unable to delete attachment file: %s';
  StrUnableToSendTheEmailNoRecipients = 'Unable to send the email as there were no recipients [ID: %d]';
  StrNoConnectionToTheDatabase = 'No connection to the database. Unable to retrieve queued emails.';
  StrReceivedEmailFrom = 'Received email from %s (%s)';
  StrConnectingToS = 'Connecting to POP server %s';
  StrDNewMessages = '%d new messages';
  StrRerievingDMessage = 'Rerieving %d message header(s)';
  StrEmail = '';
  StrEmailErrorConnectingTo = 'Error connecting to %s - %s';
  StrDisconnectedFromS = 'Disconnected from %s';
  StrSendingEmailToS = 'Sending email to %s from %s [ID: %d]';
  StrInvalidNexmoRespon = 'Invalid Nexmo response: %s';
  StrSendingSMSToSI = 'Sending SMS via Nexmo to %s [ID: %d] [URL: %s]';
  StrReplyFromNexmoS = 'Reply from Nexmo: %s';
  StrErrorSendingSS = 'Error sending %s: %s [ID: %d]';
  StrUserMessageDestinationSIs = 'User Message Destination "%s" is not enabled';

function SelectNode(xnRoot: IXmlNode; const nodePath: WideString): IXmlNode;
var
  intfSelect : IDomNodeSelect;
  dnResult : IDomNode;
  intfDocAccess : IXmlDocumentAccess;
  doc: TXmlDocument;
begin
  Result := nil;

  if not Assigned(xnRoot)
    or not Supports(xnRoot.DOMNode, IDomNodeSelect, intfSelect) then
    Exit;

  dnResult := intfSelect.selectNode(nodePath);
  if Assigned(dnResult) then
  begin
    if Supports(xnRoot.OwnerDocument, IXmlDocumentAccess, intfDocAccess) then
      doc := intfDocAccess.DocumentObject
    else
      doc := nil;
    Result := TXmlNode.Create(dnResult, nil, doc);
  end;
end;

{ TSNMPMessageQueue }

procedure TExternalMessageQueue.CancelAllQueuedMessages;
begin
  UserMessageDestinationQueues.CancelQueuedMessages;
end;

constructor TExternalMessageQueue.Create(AOwner: TComponent);
begin
  inherited;

  FQueueTimer := TTimer.Create(Self);
  FQueueTimer.Enabled := False;
  FQueueTimer.OnTimer := OnQueueTimer;

  FMessageQueueOptions := TMessageQueueOptions.Create;
  UserMessageDestinationQueues := TUserMessageDestinationQueues.Create;

  FQueueInterval := 20;

  FUserMessageDestinations := [emdEmail, emdSMS];
end;

destructor TExternalMessageQueue.Destroy;
begin
  Active := False;

  FreeAndNil(UserMessageDestinationQueues);
  FreeAndNil(FMessageQueueOptions);

  inherited;
end;

procedure TExternalMessageQueue.OnQueueTimer(Sender: TObject);
begin
  ProcessQueues;
end;

function TExternalMessageQueue.ProcessQueues: Boolean;
var
  MessageItem: IMessageServiceItem;
  SendThread: TBaseMessageQueueThread;
  UserMessageDestination: TExternalMessageDestination;
  CanSend: Boolean;
begin
  Result := False;

  for UserMessageDestination := Low(TExternalMessageDestination) to High(TExternalMessageDestination) do
  begin
    if (UserMessageDestination in FUserMessageDestinations) and
       (UserMessageDestinationQueues[UserMessageDestination].MessageQueue.Count > 0) then
    begin
      case UserMessageDestination of
        emdEmail: CanSend := UserMessageDestinationQueues[UserMessageDestination].ActiveThreads.Count < Options.SMTPEmailSettings.MaxThreads;
        emdSMS: CanSend := UserMessageDestinationQueues[UserMessageDestination].ActiveThreads.Count < Options.SMSOptions.MaxThreads;
        emdWebEvent: CanSend := UserMessageDestinationQueues[UserMessageDestination].ActiveThreads.Count < Options.SMSOptions.MaxThreads;
        emdShareBikeApp: CanSend := False;
        emdFaceBook: CanSend := False;
        emdTwitter: CanSend := False;
      else
        CanSend := False;
      end;

      if CanSend then
      begin
        SendThread := nil;

        MessageItem :=  UserMessageDestinationQueues[UserMessageDestination].MessageQueue.Dequeue;

        if UserMessageDestinationQueues[UserMessageDestination].ThreadPool.Count > 0 then
          SendThread := UserMessageDestinationQueues[UserMessageDestination].ThreadPool.Dequeue
        else
        begin
          case UserMessageDestination of
            emdEmail:
              begin
                SendThread := TEmailThread.Create(Self,
                                                  Options.SMTPEmailSettings);
              end;

            emdSMS:
              begin
                SendThread := TSMSThread.Create(Self,
                                                Options.SMSOptions);
              end;

            emdShareBikeApp: ;
            emdFaceBook: ;
            emdTwitter: ;
          end;
        end;

        if SendThread <> nil then
        begin
          SendThread.OnTerminate := OnMessageThreadTerminate;

          SendThread.SendMessage(MessageItem);

          DoOnMessageSending(SendThread.MessageItem);

          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TExternalMessageQueue.OnMessageThreadTerminate(Sender: TObject);
var
  MessageThread: TBaseMessageQueueThread;
begin
  if Sender is TBaseMessageQueueThread then
  begin
    MessageThread := Sender as TBaseMessageQueueThread;

    UserMessageDestinationQueues[MessageThread.MessageItem.UserMessageDestination].SentMessagesCS.Enter;
    try
      UserMessageDestinationQueues[MessageThread.MessageItem.UserMessageDestination].SentMessages.Enqueue(MessageThread.MessageItem);
    finally
      UserMessageDestinationQueues[MessageThread.MessageItem.UserMessageDestination].SentMessagesCS.Leave;
    end;

    if MessageThread.MessageItem.Error <> '' then
      DoOnMessageFailed(MessageThread.MessageItem)
    else
      DoOnMessageSent(MessageThread.MessageItem);

    UserMessageDestinationQueues[MessageThread.MessageItem.UserMessageDestination].ActiveThreads.Remove(Sender);
    //FThreadPool.Enqueue(MessageThread);
  end;
end;

procedure TExternalMessageQueue.DoOnMessageFailed(const MessageItem: IMessageServiceItem);
begin
  if Assigned(FOnMessageFailed) then
    FOnMessageFailed(Self, MessageItem);
end;

procedure TExternalMessageQueue.DoOnMessageSending(const MessageItem: IMessageServiceItem);
begin
  if Assigned(FOnMessageSending) then
    FOnMessageSending(Self, MessageItem);
end;

procedure TExternalMessageQueue.DoOnMessageSent(const MessageItem: IMessageServiceItem);
begin
  if Assigned(FOnMessageSent) then
    FOnMessageSent(Self, MessageItem);
end;

procedure TExternalMessageQueue.DoOnStatus(const StatusText: String;
  LogLevel: TLCLogLevel);
begin
  if Assigned(FStatusLog) then
    FStatusLog.Log(StatusText, LogLevel);
end;

procedure TExternalMessageQueue.DoOnStatus(const StatusText: String;
  const Args: array of const; LogLevel: TLCLogLevel);
begin
  DoOnStatus(format(StatusText, Args), LogLevel);
end;

function TExternalMessageQueue.GetActiveCount(
  const UserMessageDestination: TExternalMessageDestination): Integer;
begin
  Result := UserMessageDestinationQueues[UserMessageDestination].ActiveThreads.Count;
end;

function TExternalMessageQueue.GetCompletedCount(
  const UserMessageDestination: TExternalMessageDestination): Integer;
begin
  UserMessageDestinationQueues[UserMessageDestination].SentMessagesCS.Enter;
  try
    Result := UserMessageDestinationQueues[UserMessageDestination].SentMessages.Count;
  finally
    UserMessageDestinationQueues[UserMessageDestination].SentMessagesCS.Leave;
  end;
end;

function TExternalMessageQueue.GetCompletedMessage(const UserMessageDestination: TExternalMessageDestination; var MessageItem: IMessageServiceItem): Boolean;
begin
  UserMessageDestinationQueues[UserMessageDestination].SentMessagesCS.Enter;
  try
    Result := UserMessageDestinationQueues[UserMessageDestination].SentMessages.Count > 0;

    if Result then
    begin
      MessageItem := UserMessageDestinationQueues[UserMessageDestination].SentMessages.Dequeue;

      UserMessageDestinationQueues[UserMessageDestination].IncProcessedCount;
    end;
  finally
    UserMessageDestinationQueues[UserMessageDestination].SentMessagesCS.Leave;
  end;
end;

function TExternalMessageQueue.GetPooledCount(
  const UserMessageDestination: TExternalMessageDestination): Integer;
begin
  Result := UserMessageDestinationQueues[UserMessageDestination].ThreadPool.Count;
end;

function TExternalMessageQueue.GetProcessedCount(
  const UserMessageDestination: TExternalMessageDestination): Integer;
begin
  Result := UserMessageDestinationQueues[UserMessageDestination].CompletedCount;
end;

function TExternalMessageQueue.GetQueueCount(
  const UserMessageDestination: TExternalMessageDestination): Integer;
begin
  Result := UserMessageDestinationQueues[UserMessageDestination].MessageQueue.Count;
end;

procedure TExternalMessageQueue.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if AComponent = FStatusLog then
      FStatusLog := nil
  end;
end;

procedure TExternalMessageQueue.SendMessage(const ExternalMessage: IMessageServiceItem);
begin
  if not Active then
    Active := True;

  if ExternalMessage.UserMessageDestination in FUserMessageDestinations then
    UserMessageDestinationQueues[ExternalMessage.UserMessageDestination].MessageQueue.Enqueue(ExternalMessage)
  else
    raise Exception.CreateFmt(StrUserMessageDestinationSIs, [UserMessageDestinationDescriptions[ExternalMessage.UserMessageDestination]]);
end;

procedure TExternalMessageQueue.SetActive(const Value: Boolean);
var
  UserMessageDestination: TExternalMessageDestination;
begin
  if FActive <> Value then
  begin
    if Value then
    begin
      for UserMessageDestination := Low(TExternalMessageDestination) to High(TExternalMessageDestination) do
        if UserMessageDestination in FUserMessageDestinations then
          UserMessageDestinationQueues.Add(UserMessageDestination, TUserMessageDestinationQueue.Create);
    end
    else
    begin
      CancelAllQueuedMessages;
    end;

    FQueueTimer.Enabled := (Value) and (not (csDesigning in ComponentState));

    FActive := Value;
  end;
end;

procedure TExternalMessageQueue.SetMessageQueueOptions(
  const Value: TMessageQueueOptions);
begin
  FMessageQueueOptions.Assign(Value);
end;

procedure TExternalMessageQueue.SetQueueInterval(const Value: Integer);
begin
  if Value = 0 then
    FQueueInterval := 1
  else
    FQueueInterval := Value;

  FQueueTimer.Interval := FQueueInterval;
end;

procedure TExternalMessageQueue.SetUserMessageDestinations(
  const Value: TExternalMessageDestinations);
begin
  Active := False;

  FUserMessageDestinations := Value;
end;

{ TEmailThread }

constructor TEmailThread.Create(MessageQueue: TExternalMessageQueue;
  const SMTPEmailOptions: TSMTPEmailOptions);
begin
  inherited Create(MessageQueue, SMTPEmailOptions);

  FSMTPEmailOptions := TSMTPEmailOptions.Create;
  FSMTPEmailOptions.Assign(SMTPEmailOptions);
end;

procedure TEmailThread.OnSMTPCommand(Sender: TObject; const Command: String);
begin
  FMessageQueue.DoOnStatus(StrEmail + Command, llDebug3);
end;

procedure TEmailThread.OnIdStatus(ASender: TObject; const AStatus: TIdStatus;
   const AStatusText: string);
begin
  FMessageQueue.DoOnStatus(StrEmail + AStatusText, llDebug3);
end;

procedure TEmailThread.CreateSMTPClient;
var
  RealPort: Integer;
begin
  DestroySMTPClient;

  if FSMTPEmailOptions.Port = 0 then
  begin
    if FSMTPEmailOptions.UseSSL then
      RealPort := 587
    else
      RealPort := 25;
  end
  else
    RealPort := FSMTPEmailOptions.Port;

  try
    FSMTPClient := TLoggedIdSMTP.Create(nil);
    FSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

    FSMTPClient.OnCommand := OnSMTPCommand;

    if FSMTPEmailOptions.UseAuthentication then
    begin
      FSMTPClient.AuthType := satDefault;
      FSMTPClient.Username := FSMTPEmailOptions.User;
      FSMTPClient.Password := FSMTPEmailOptions.Password;
    end
    else
      FSMTPClient.AuthType := satNone;

    FSMTPClient.Host := FSMTPEmailOptions.Server;
    FSMTPClient.Port := RealPort;
    FSMTPClient.OnStatus := OnIdStatus;
    FSMTPClient.OnCommand := OnSMTPCommand;

    if FSMTPEmailOptions.UseSSL then
    begin
      FSSL.Port := RealPort;
      FSSL.Destination := format('%s:%d', [FSMTPClient.Host, FSSL.Port]);

      FSMTPClient.IOHandler := FSSL;
      FSMTPClient.UseTLS := utUseExplicitTLS;
      FSSL.OnStatus := OnIdStatus;
    end;

    // Check that the SSL dlls can be loaded
    if (FSMTPEmailOptions.UseSSL) and (not IdSSLOpenSSLHeaders.Load) then
      raise Exception.Create(StrUnableToLoadSSLDLLs);
  except
    on e: Exception do
    begin
      DestroySMTPClient;

      raise;
    end;
  end;
end;

destructor TEmailThread.Destroy;
begin
  DestroySMTPClient;

  FreeAndNil(FSMTPEmailOptions);

  inherited;
end;

procedure TEmailThread.DestroySMTPClient;
begin
  FreeAndNil(FSMTPClient);
  FreeAndNil(FSSL);
end;

procedure TEmailThread.DisconnectInternal;
begin
  if Connected then
  begin
    try
      FSMTPClient.Disconnect(True);
    except
      // Just in case... you know Indy!
    end;

    FMessageQueue.DoOnStatus(StrDisconnectedFromS, [FSMTPEmailOptions.Server], llInfo);
  end;
end;

procedure TEmailThread.ConnectInternal;
begin
  if not Connected then
  begin
    CreateSMTPClient;

    FMessageQueue.DoOnStatus(StrEmailConnectingTo, [FSMTPEmailOptions.Server], llInfo);

    try
      FSMTPClient.Connect;

      FMessageQueue.DoOnStatus(StrConnectedToS, [FSMTPEmailOptions.Server], llInfo);
    except
      on e: Exception do
      begin
        FMessageQueue.DoOnStatus(StrEmailErrorConnectingTo, [FSMTPEmailOptions.Server, e.Message], llDebug3);

        raise;
      end;
    end;
  end;
end;

function TEmailThread.Connected: Boolean;
begin
  Result := (FSMTPClient <> nil) and (FSMTPClient.Connected);
end;

procedure TEmailThread.SendMessagelInternal;
var
  ThisAttachment: TIdAttachmentFile;
  IdMessage: TIdMessage;
  MultiPart: TIdText;
  TempBuffer: TStringList;
begin
  IdMessage := TIdMessage.Create(nil);
  try
    if FMessageItem.Body <> '' then
    begin
      TempBuffer := TStringList.Create;
      TempBuffer.Text := String(UTF8Encode(FMessageItem.Body));
      MultiPart := TIdText.Create(IdMessage.MessageParts, TempBuffer);
      MultiPart.ContentType := 'text/plain; charset=utf-8';
    end;

    if FMessageItem.BodyHTML <> '' then
    begin
      TempBuffer := TStringList.Create;
      TempBuffer.Text := String(UTF8Encode(FMessageItem.BodyHTML));
      MultiPart := TIdText.Create(IdMessage.MessageParts, TempBuffer);
      MultiPart.ContentType := 'text/html; charset=utf-8';
    end;

    IdMessage.Charset := 'utf-8';
    IdMessage.Encoding := meMime;
    IdMessage.Priority := TIdMessagePriority(0);
    IDMessage.Subject := FMessageItem.Subject;
    IdMessage.From.Text := FMessageItem.Sender;
    IdMessage.Recipients.EMailAddresses := FMessageItem.Recipients;
    IdMessage.CCList.EMailAddresses := FMessageItem.RecipientsCC;
    IdMessage.BccList.EMailAddresses := FMessageItem.RecipientsBCC;

    //Note: We should have a routine to detect common content-types.
    //      E.g PDF's would be "application/PDF" etc. etc.

    ThisAttachment := nil;

    if FileExists(FMessageItem.AttachmentFilename) then
    begin
      ThisAttachment := TIdAttachmentFile.Create(
        IdMessage.MessageParts,
        FMessageItem.AttachmentFilename);

      ThisAttachment.CharSet := '';
      ThisAttachment.ContentType := 'application/octet-stream';
      ThisAttachment.FileIsTempFile := True;
      ThisAttachment.FileName := FMessageItem.AttachmentFilename;
    end;

    try
      try
        FMessageQueue.DoOnStatus(StrSendingEmailToS,
          [FMessageItem.Sender,
           FMessageItem.Recipients,
           FMessageItem.ID],
           llDebug2);

        FSMTPClient.Send(IdMessage);
      except
        on e: Exception do
        raise;
      end;
    finally
      if Assigned(ThisAttachment) then
        FreeAndNil(ThisAttachment);
    end;
  finally
    FreeAndNil(IdMessage);
  end;
end;


{ TLoggedIdSMTP }

procedure TLoggedIdSMTP.PrepareCmd(var aCmd: string);
begin
  inherited;

  if Assigned(FOnCommand) then
    FOnCommand(Self, aCmd);
end;

{ TBaseMessageQueueThread }

procedure TBaseMessageQueueThread.ConnectInternal;
begin
  // Override if required
end;

constructor TBaseMessageQueueThread.Create(const MessageQueue: TExternalMessageQueue;
  const ExternalMessageQueueOptions: TExternalMessageQueueOptions);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FExternalMessageQueueOptions := TExternalMessageQueueOptions.Create;

  FMessageQueue := MessageQueue;

  FExternalMessageQueueOptions.Assign(ExternalMessageQueueOptions);
end;

destructor TBaseMessageQueueThread.Destroy;
begin
  FreeAndNil(FExternalMessageQueueOptions);

  inherited;
end;

procedure TBaseMessageQueueThread.DisconnectInternal;
begin
  // Override if required
end;

procedure TBaseMessageQueueThread.Execute;
var
  Retry: Integer;
  Done: Boolean;
  ErrorIsException: Boolean;
begin
  //while not Terminated do
  begin
    Retry := 0;
    Done := False;
    ErrorIsException := False;
    try
      repeat
        try
          ErrorIsException := False;

          ConnectInternal;

          SendMessagelInternal;

          Done := True;
        except
          on e: Exception do
          begin
            if Retry <  FExternalMessageQueueOptions.Retries then
              FMessageQueue.DoOnStatus(format(StrErrorSendingEmailRetry, [UserMessageDestinationDescriptions[FMessageItem.UserMessageDestination], FMessageItem.ID, e.Message, FExternalMessageQueueOptions.Retries - Retry - 1]), llWarning);

            FMessageItem.Error := e.Message;

            Inc(Retry);

            ErrorIsException := True;
          end;
        end;
      until (Done) or
            (Retry >= FExternalMessageQueueOptions.Retries) or
            (Terminated);
    except
      on e: Exception do
      begin
        FMessageQueue.DoOnStatus(format(StrEmailCriticalError, [UserMessageDestinationDescriptions[FMessageItem.UserMessageDestination], FMessageItem.ID, e.Message, Retry]), llError);
        FMessageItem.Error := e.Message;

        ErrorIsException := True;
      end;
    end;

    try
      DisconnectInternal;
    except
      // Don't allow exception to escape!
    end;
  end;

  if (FMessageItem.Error <> '') and
     (not ErrorIsException) then
   FMessageQueue.DoOnStatus(format(StrErrorSendingSS, [UserMessageDestinationDescriptions[FMessageItem.UserMessageDestination], FMessageItem.Error, FMessageItem.ID]), llError);
end;

function TBaseMessageQueueThread.SendMessage(
  MessageItem: IMessageServiceItem): Boolean;
begin
  Result := not FActive;

  if Result then
  begin
    FMessageItem := MessageItem;

    FActive := True;

    Start;
  end;
end;

procedure TBaseMessageQueueThread.SendMessagelInternal;
begin
  // Override if required
end;

{ TMessageQueueOptions }

constructor TMessageQueueOptions.Create;
begin
  FSMTPEmailSettings := TSMTPEmailOptions.Create;
  FSMSOptions := TSMSOptions.Create;
end;

destructor TMessageQueueOptions.Destroy;
begin
  FreeAndNil(FSMTPEmailSettings);
  FreeAndNil(FSMSOptions);

  inherited;
end;

procedure TMessageQueueOptions.SetSMSOptions(const Value: TSMSOptions);
begin
  FSMSOptions.Assign(Value);
end;

procedure TMessageQueueOptions.SetSMTPEmailSettings(
  const Value: TSMTPEmailOptions);
begin
  FSMTPEmailSettings.Assign(Value);
end;

{ TExternalMessageQueueOptions }

procedure TExternalMessageQueueOptions.Assign(Source: TPersistent);
var
  Src: TExternalMessageQueueOptions;
begin
  if Source is TExternalMessageQueueOptions then
  begin
    Src := Source as TExternalMessageQueueOptions;

    FTimeout := Src.Timeout;
    FMaxThreads := Src.MaxThreads;
    FRetries := Src.Retries;
  end
  else
    inherited;
end;

constructor TExternalMessageQueueOptions.Create;
begin
  FRetries := 0;
  FTimeout := 10000;
  FMaxThreads := 2;
end;

procedure TExternalMessageQueueOptions.SetMaxThreads(const Value: Integer);
begin
  FMaxThreads := Value;

  if FMaxThreads < 1 then
    FMaxThreads := 1;
end;

procedure TExternalMessageQueueOptions.SetTimeout(const Value: Integer);
begin
  FTimeout := Value;

  if FTimeout < 50 then
    FTimeout := 1;
end;

{ TUserMessageDestinationQueue }

procedure TUserMessageDestinationQueue.CancelQueuedMessages;
var
  Thread: TBaseMessageQueueThread;
begin
  for Thread in FActiveThreads.Values do
    Thread.Terminate;

  while FActiveThreads.Count > 0 do
    sleep(1);

  FMessageQueue.Clear;
end;

constructor TUserMessageDestinationQueue.Create;
begin
  FMessageQueue := TQueue<IMessageServiceItem>.Create;
  FActiveThreads := TDictionary<Pointer, TBaseMessageQueueThread>.Create;
  FThreadPool := TQueue<TBaseMessageQueueThread>.Create;
  FSentMessages := TQueue<IMessageServiceItem>.Create;
  FSentMessagesCS := TCriticalSection.Create;
end;

destructor TUserMessageDestinationQueue.Destroy;
begin
  CancelQueuedMessages;

  while FThreadPool.Count > 0 do
    FThreadPool.Dequeue.Free;

  FreeAndNil(FMessageQueue);
  FreeAndNil(FActiveThreads);
  FreeAndNil(FThreadPool);
  FreeAndNil(FSentMessages);
  FreeAndNil(FSentMessagesCS);

  inherited;
end;

procedure TUserMessageDestinationQueue.IncProcessedCount;
begin
  Inc(FCompletedCount);
end;

{ TUserMessageDestinationQueues }

procedure TUserMessageDestinationQueues.CancelQueuedMessages;
var
  UserMessageDestination: TExternalMessageDestination;
begin
  for UserMessageDestination in Keys do
    Items[UserMessageDestination].CancelQueuedMessages;
end;

constructor TUserMessageDestinationQueues.Create;
begin
  inherited Create([doOwnsValues]);
end;

destructor TUserMessageDestinationQueues.Destroy;
begin

  inherited;
end;

{ TSMSThread }

constructor TSMSThread.Create(MessageQueue: TExternalMessageQueue; const SMSOptions: TSMSOptions);
begin
  inherited Create(MessageQueue, SMSOptions);

  FSMSOptions := TSMSOptions.Create;
  FSMSOptions.Assign(SMSOptions);
end;

destructor TSMSThread.Destroy;
begin
  FreeAndNil(FSMSOptions);

  inherited;
end;

procedure TSMSThread.SendMessagelInternal;
const
  NexmoAPIURL = 'https://rest.nexmo.com/sms/xml?api_key=%s&api_secret=%s&from=%s&to=%s&text=%s';
var
  HTTPGet: TIdHTTP;
  XMLDoc: IXMLDocument;
  StringStream: TStringStream;
  URL: String;
  XMLNode: IXMLNode;
  MessageText: String;
begin
  StringStream := TStringStream.Create;
  HTTPGet := TIdHTTP.Create(nil);
  try
    HTTPGet.ConnectTimeout := FSMSOptions.Timeout;

    if FSMSOptions.MaxMessageLength > 0 then
      MessageText := copy(FMessageItem.Body, 1, FSMSOptions.MaxMessageLength)
    else
      MessageText := FMessageItem.Body;

    URL := format(NexmoAPIURL, [FSMSOptions.APIKey,
                                FSMSOptions.APISecret,
                                FMessageItem.Sender,
                                FMessageItem.Recipients,
                                FMessageItem.Body]);

    URL := TIdURI.URLEncode(URL);

    FMessageQueue.DoOnStatus(StrSendingSMSToSI, [FMessageItem.Recipients, FMessageItem.ID, URL], llDebug2);

    HTTPGet.Get(URL, StringStream);

    StringStream.Position := 0;

    FMessageQueue.DoOnStatus(StrReplyFromNexmoS, [StringStream.DataString], llDebug2);

    CoInitialize(nil);

    XMLDoc := TXMLDocument.Create(nil);
    try
      XMLDoc.Active := True;
      XMLDoc.LoadFromStream(StringStream);

      XMLNode := SelectNode(XMLDoc.DocumentElement, '/mt-submission-response/messages/message/status');

      if XMLNode <> nil then
        FMessageItem.ErrorCode := StrToIntDef(XMLNode.NodeValue, 0)
      else
        raise Exception.CreateFmt(StrInvalidNexmoRespon, [copy(XMLDoc.XML.Text, 1, 200)]);

      if FMessageItem.ErrorCode = 0 then
        FMessageItem.SentMessageID := SelectNode(XMLDoc.DocumentElement, '/mt-submission-response/messages/message/messageId').NodeValue
      else
        FMessageItem.Error := SelectNode(XMLDoc.DocumentElement, '/mt-submission-response/messages/message/errorText').NodeValue;

      XMLNode := SelectNode(XMLDoc.DocumentElement, '/mt-submission-response/messages/message/messagePrice');

      if XMLNode <> nil then
        FMessageItem.MessagePrice := StrToFloatDefUseDot(XMLNode.NodeValue, 0);

      XMLNode := SelectNode(XMLDoc.DocumentElement, '/mt-submission-response/messages/message/remainingBalance');

      if XMLNode <> nil then
        FMessageItem.RemainingBalance := StrToFloatDefUseDot(XMLNode.NodeValue, 0);
    finally
      XMLDoc := nil;

      CoUnInitialize;
    end;
  finally
    FreeAndNil(HTTPGet);
    FreeAndNil(StringStream);
  end;
end;

{ TSMSOptions }

procedure TSMSOptions.Assign(Source: TPersistent);
var
  Src: TSMSOptions;
begin
  inherited;

  if Source is TSMSOptions then
  begin
    Src := Source as TSMSOptions;

    FSMSServiceType := Src.SMSServiceType;
    FAPIKey := Src.APIKey;
    FAPISecret := Src.APISecret;
    FMaxMessageLength := Src.MaxMessageLength;
  end;
end;

constructor TSMSOptions.Create;
begin
  inherited;

  FMaxMessageLength := 160;
end;

{ TSMTPEmailOptions }

procedure TSMTPEmailOptions.Assign(Source: TPersistent);
var
  Src: TSMTPEmailOptions;
begin
  inherited;

  if Source is TSMTPEmailOptions then
  begin
    Src := Source as TSMTPEmailOptions;

    FEmailContentType := Src.EmailContentType;
    FServer := Src.Server;
    FUser := Src.User;
    FPassword := Src.Password;
    FPort := Src.Port;
    FUseAuthentication := Src.UseAuthentication;
    FUseSSL := Src.UseSSL;
    FHTMLHeader.Assign(Src.HTMLHeader);
    FHTMLFooter.Assign(Src.HTMLFooter);
    FPlainHeader.Assign(Src.PlainHeader);
    FPlainFooter.Assign(Src.PlainFooter);
  end;
end;

constructor TSMTPEmailOptions.Create;
begin
  inherited;

  FHTMLHeader := TStringList.Create;
  FHTMLFooter := TStringList.Create;
  FPlainHeader := TStringList.Create;
  FPlainFooter := TStringList.Create;
end;

destructor TSMTPEmailOptions.Destroy;
begin
  FreeAndNil(FHTMLHeader);
  FreeAndNil(FHTMLFooter);
  FreeAndNil(FPlainHeader);
  FreeAndNil(FPlainFooter);

  inherited;
end;

procedure TSMTPEmailOptions.SetHTMLFooter(const Value: TStringList);
begin
  FHTMLFooter.Assign(Value);
end;

procedure TSMTPEmailOptions.SetHTMLHeader(const Value: TStringList);
begin
  FHTMLHeader.Assign(Value);
end;

procedure TSMTPEmailOptions.SetPlainFooter(const Value: TStringList);
begin
  FPlainFooter.Assign(Value);
end;

procedure TSMTPEmailOptions.SetPlainHeader(const Value: TStringList);
begin
  FPlainHeader.Assign(Value);
end;

end.
