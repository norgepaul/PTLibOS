unit PTLib.Common.Network.Email;

interface

uses
  System.Classes, System.SysUtils,  System.SyncObjs, System.Threading,

  PTLib.Common.Log,
  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Types,

  IdPOP3, IdMessage, IdSSLOpenSSLHeaders, IdSSLOpenSSL,
  IdExplicitTLSClientServerBase;

type
  IIncomingEmail = interface(IEmail)
    ['{680D7C01-397C-4338-A0CA-17871B476070}']
    function GetEmail: TIdMessage;
    property Email: TIdMessage read GetEmail;
  end;

  TIncomingEmail = class(TInterfacedObject, IIncomingEmail)
  strict private
    FEmail: TIdMessage;
  private
    function GetEmail: TIdMessage;
  protected
    property Email: TIdMessage read GetEmail;
  public
    destructor Destroy; override;
  end;

  TPTLibBasePopClient = class(TBasePTLibLogComponent)
  strict private
    FIncomingEmailCS: TCriticalSection;
    FPopServer: String;
    FUsername: String;
    FPassword: String;
    FUseSSL: Boolean;
    FPopPort: Integer;
    FDeleteRetrievedMessages: Boolean;
    FReceivingEmails: Boolean;

    FIncomingEmail: IList<IIncomingEmail>;
  protected
    procedure DoReceiveEmailsThread; virtual;
    procedure DoReceiveEmails; virtual;
    procedure DoReceiveEmail(IncomingEmail: IIncomingEmail); virtual;
    function DoNextIncomingEmail(out IncomingEmail: IIncomingEmail): Boolean;

    property PopServer: String read FPopServer write FPopServer;
    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property PopPort: Integer read FPopPort write FPopPort;
    property DeleteRetrievedMessages: Boolean read FDeleteRetrievedMessages write FDeleteRetrievedMessages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPTLibPopClient = class(TPTLibBasePopClient)
  published
    property PopServer;
    property Username;
    property Password;
    property UseSSL;
    property PopPort;
    property DeleteRetrievedMessages;
  public
    procedure ReceiveEmails;
    function NextIncomingEmail(out IncomingEmail: IIncomingEmail): Boolean;
  end;

  TBaseGmailClient = class(TPTLibPopClient)
  public
    constructor Create(AOwner: TComponent); override;

    property Username;
    property Password;
    property DeleteRetrievedMessages;
  end;

  TGmailClient = class(TBaseGmailClient)
  end;

implementation

procedure TPTLibBasePopClient.DoReceiveEmailsThread;
var
  POPClient: TIdPOP3;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  i: Integer;
  MsgCount: Integer;
  Msg: IIncomingEmail;
begin
  POPClient := TIdPOP3.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    if FUseSSL then
    begin
      SSL.Host := FPOPServer;
      SSL.Port := FPOPPort;
      SSL.Destination := format('%s:%d', [SSL.Host, SSL.Port]);
      SSL.SSLOptions.Method := sslvTLSv1_2; //sslvSSLv3;
      SSL.SSLOptions.Mode := sslmUnassigned;
      //SSL.OnStatus := OnIdStatus;

      POPClient.IOHandler := SSL;
      POPClient.UseTLS := utUseImplicitTLS;
    end;

    POPClient.Host := FPOPServer;
    POPClient.Username := FUsername;
    POPClient.Password := FPassword;
    POPClient.AutoLogin := TRUE;
    POPClient.Port := FPOPPort;
    DoLog('Connecting to %s', [FPOPServer], LogSeverityDebug3);

    POPClient.Connect;
    try
      DoLog('Connected to %s', [FPOPServer], LogSeverityDebug3);

      MsgCount := POPClient.CheckMessages;

      DoLog('%d new messages', [MsgCount], LogSeverityDebug3);

      for i := MsgCount downto 1 do
      begin
        DoLog('Retrieving message #%d', [MsgCount], LogSeverityDebug3);

        Msg := TIncomingEmail.Create;

        if POPClient.Retrieve(i, Msg.Email) then
        begin
          DoReceiveEmail(Msg);

          if FDeleteRetrievedMessages then
          begin
            POPClient.Delete(i);
          end;
        end;
      end;
    finally
      POPClient.Disconnect;
    end;
  finally
    FreeAndNil(POPClient);
    FreeAndNil(SSL);
    FReceivingEmails := False;
  end;
end;

{ TPTLibBasePopClient }

constructor TPTLibBasePopClient.Create(AOwner: TComponent);
begin
  inherited;

  FIncomingEmailCS := TCriticalSection.Create;
  FIncomingEmail := TList<IIncomingEmail>.Create;
end;

destructor TPTLibBasePopClient.Destroy;
begin
  FreeAndNil(FIncomingEmailCS);

  inherited;
end;

procedure TPTLibBasePopClient.DoReceiveEmail(IncomingEmail: IIncomingEmail);
begin
  FIncomingEmailCS.Enter;
  try
    FIncomingEmail.Add(IncomingEmail);
  finally
    FIncomingEmailCS.Leave;
  end;
end;

function TPTLibBasePopClient.DoNextIncomingEmail(
  out IncomingEmail: IIncomingEmail): Boolean;
begin
  FIncomingEmailCS.Enter;
  try
    Result := FIncomingEmail.Count > 0;

    if Result then
    begin
      IncomingEmail := FIncomingEmail[0];
      FIncomingEmail.Delete(0);
    end;
  finally
    FIncomingEmailCS.Leave;
  end;
end;

procedure TPTLibBasePopClient.DoReceiveEmails;
begin
  if not FReceivingEmails then
  begin
    FReceivingEmails := True;

    TTask.Run(
      procedure
      begin
        DoReceiveEmailsThread;
      end
    );
  end;
end;

{ TBaseGmailClient }

constructor TBaseGmailClient.Create(AOwner: TComponent);
begin
  inherited;

  PopServer := 'pop.gmail.com';
  PopPort := 995;
  UseSSL := True;
end;

{ TIncomingEmail }

destructor TIncomingEmail.Destroy;
begin
  FreeAndNil(FEmail);

  inherited;
end;

function TIncomingEmail.GetEmail: TIdMessage;
begin
  if FEMail = nil then
  begin
    FEmail := TIdMessage.Create(nil);
  end;

  Result := FEmail;
end;

{ TPTLibPopClient }

function TPTLibPopClient.NextIncomingEmail(
  out IncomingEmail: IIncomingEmail): Boolean;
begin
  Result := DoNextIncomingEmail(IncomingEmail);
end;

procedure TPTLibPopClient.ReceiveEmails;
begin
  DoReceiveEmails;
end;

end.
