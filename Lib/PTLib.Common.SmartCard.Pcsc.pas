unit PTLib.Common.SmartCard.Pcsc;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.SyncObjs,
  System.Types,

  PTLib.Common.Utils,
  PTLib.Common.Log,
  PTLib.Common.Types,
  PTLib.Common.Strings,
  PTLib.Common.Interfaces,
  PTLib.Common.SmartCard,
  PTLib.Common.SmartCard.Types,
  PTLib.Common.SmartCard.Pcsc.DLL.Types;

type
  TSCardContext = LongInt;
  TSReaderHandle = LongInt;

  TPTLibSmartCardPcsc = class(TPTLibSmartCard,
                              IPTLibSmartCardPCSC)
  strict private
    FCheckTask: ITask;
    FReaderName: String;
    FCardContext: TSCardContext;
    FReaderHandle: TSReaderHandle;
    FAttrProtocol: Integer;
    FStatus: Integer;
    FLastStates: TSmartCardReaderStates;
    FReadCardContext: Boolean;
    FCardData: TCardData;
    FCardDataCS: TCriticalSection;

    function GetCardData: TCardData;
  private
    const CARD_READ_TIMEOUT = -1; //500;

    function CheckResult(const Value: Cardinal; const IgnoreErrors: Array of Cardinal): Cardinal; overload;
    function CheckResult(const Value: Cardinal): Cardinal; overload;
    function GetCardContext: TSCardContext;
    procedure EstablishCardContext;
    procedure ReleaseCardContext;
    procedure SpringCardReaderStateChanged(const States: TSmartCardReaderStates;  const Data: TBytes);
    procedure SetCardData(const Value: TCardData);
    procedure Connect;
    procedure Disconnect;
  protected
    procedure DoSetActive(const Value: Boolean); override;
    procedure DoGetInformation(const Information: IInformationList); override;
  protected
    procedure DoReaderStateChanged(const States: TSmartCardReaderStates; const Data: TBytes); virtual;
    procedure DoConnectCard(const ReaderName: String); virtual;
    procedure DoDisconnectCard; virtual;
    procedure DoGetReaderList(out Values: TArray<String>); virtual;
    procedure DoCheckState(const StateChanged: TStateChangedProc; const ReadTimeout: Integer); virtual;
    procedure DoGetResponseFromCard(const APDU: TBytes; out Response: TBytes; out Success: Boolean); virtual;
    procedure DoGetReaderStates(out Value: TSmartCardReaderStates); virtual;
    function GetReaderList: TArray<String>;
    procedure ConnectCard(const ReaderName: String);
    procedure DisconnectCard;
    procedure CheckState(const StateChanged: TStateChangedProc = nil; const ReadTimeout: Integer = 5000);
    procedure CheckStateASync(const StateChanged: TStateChangedProc; const OnException: TProc<Exception>);
    function GetResponseFromCard(const APDU: TBytes; out Response: TBytes): Boolean;
    function GetReaderStates: TSmartCardReaderStates;
    function GetReaderStateDescription: String;
    function GetReaderName: String;
    function EstablishContext: TSCardContext;
    procedure ReleaseContext(const Context: TSCardContext);
    function CardConnected: Boolean;
    function ReaderConnected: Boolean;

    property CardData: TCardData read GetCardData;
    property Context: TSCardContext read GetCardContext;
  public
    constructor Create(const CardUIDChanged: TCardUIDChanged); reintroduce;
    destructor Destroy; override;
  end;

  TPTLibSmartCardPcscAutoUID = class(TPTLibSmartCardPcsc)
  strict private
    FDefaultReaderName: String;
  private
    procedure SetDefaultReaderName(const Value: String);
  protected
    procedure DoSetActive(const Value: Boolean); override;

    property DefaultReaderName: String read FDefaultReaderName write SetDefaultReaderName;
  end;

implementation

uses
  PTLib.Common.SmartCard.Pcsc.DLL;

const
  SpringcardReaderName = 'SpringCard H663';

{ TPTLibSmartCardPcsc }

function TPTLibSmartCardPcsc.CheckResult(const Value: Cardinal; const IgnoreErrors: Array of Cardinal): Cardinal;
var
  Error: String;
begin
  // Check result
  Result := Value;

  if (Result <> 0) and
     (not TArrayUtils<Cardinal>.Contains(Value, IgnoreErrors)) then
  begin
    Error := '';

    case Result of
      F_INTERNAL_ERROR: Error := 'F_INTERNAL_ERROR';
      //E_CANCELLED: Error := 'E_CANCELLED';
      E_INVALID_HANDLE: Error := 'E_INVALID_HANDLE';
      E_INVALID_PARAMETER: Error := 'E_INVALID_PARAMETER';
      E_INVALID_TARGET: Error := 'E_INVALID_TARGET';
      E_NO_MEMORY: Error := 'E_NO_MEMORY';
      F_WAITED_TOO_LONG: Error := 'F_WAITED_TOO_LONG';
      E_INSUFFICIENT_BUFFER: Error := 'E_INSUFFICIENT_BUFFER';
      E_UNKNOWN_READER: Error := 'E_UNKNOWN_READER';
      E_TIMEOUT: Error := 'E_TIMEOUT';
      E_SHARING_VIOLATION: Error := 'E_SHARING_VIOLATION';
      E_NO_SMARTCARD: Error := 'E_NO_SMARTCARD';
      E_UNKNOWN_CARD: Error := 'E_UNKNOWN_CARD';
      E_CANT_DISPOSE: Error := 'E_CANT_DISPOSE';
      E_PROTO_MISMATCH: Error := 'E_PROTO_MISMATCH';
      E_NOT_READY: Error := 'E_NOT_READY';
      E_INVALID_VALUE: Error := 'E_INVALID_VALUE';
      E_SYSTEM_CANCELLED: Error := 'E_SYSTEM_CANCELLED';
      F_COMM_ERROR: Error := 'F_COMM_ERROR';
      F_UNKNOWN_ERROR: Error := 'F_UNKNOWN_ERROR';
      E_INVALID_ATR: Error := 'E_INVALID_ATR';
      E_NOT_TRANSACTED: Error := 'E_NOT_TRANSACTED';
      E_READER_UNAVAILABLE: Error := 'E_READER_UNAVAILABLE';
      P_SHUTDOWN: Error := 'P_SHUTDOWN';
      E_PCI_TOO_SMALL: Error := 'E_PCI_TOO_SMALL';
      E_READER_UNSUPPORTED: Error := 'E_READER_UNSUPPORTED';
      E_DUPLICATE_READER: Error := 'E_DUPLICATE_READER';
      E_CARD_UNSUPPORTED: Error := 'E_CARD_UNSUPPORTED';
      E_NO_SERVICE: Error := 'E_NO_SERVICE';
      E_SERVICE_STOPPED: Error := 'E_SERVICE_STOPPED';
      E_UNEXPECTED: Error := 'E_UNEXPECTED';
      E_ICC_INSTALLATION: Error := 'E_ICC_INSTALLATION';
      E_ICC_CREATEORDER: Error := 'E_ICC_CREATEORDER';
      E_UNSUPPORTED_FEATURE: Error := 'E_UNSUPPORTED_FEATURE';
      E_DIR_NOT_FOUND: Error := 'E_DIR_NOT_FOUND';
      E_FILE_NOT_FOUND: Error := 'E_FILE_NOT_FOUND';
      E_NO_DIR: Error := 'E_NO_DIR';
      E_NO_FILE: Error := 'E_NO_FILE';
      E_NO_ACCESS: Error := 'E_NO_ACCESS';
      E_WRITE_TOO_MANY: Error := 'E_WRITE_TOO_MANY';
      E_BAD_SEEK: Error := 'E_BAD_SEEK';
      E_INVALID_CHV: Error := 'E_INVALID_CHV';
      E_UNKNOWN_RES_MNG: Error := 'E_UNKNOWN_RES_MNG';
      E_NO_SUCH_CERTIFICATE: Error := 'E_NO_SUCH_CERTIFICATE';
      E_CERTIFICATE_UNAVAILABLE: Error := 'E_CERTIFICATE_UNAVAILABLE';
      E_NO_READERS_AVAILABLE: Error := 'E_NO_READERS_AVAILABLE';
      E_COMM_DATA_LOST: Error := 'E_COMM_DATA_LOST';
      E_NO_KEY_CONTAINER: Error := 'E_NO_KEY_CONTAINER';
      W_UNSUPPORTED_CARD: Error := 'W_UNSUPPORTED_CARD';
      W_UNRESPONSIVE_CARD: Error := 'W_UNRESPONSIVE_CARD';
      W_UNPOWERED_CARD: Error := 'W_UNPOWERED_CARD';
      W_RESET_CARD: Error := 'W_RESET_CARD';
      W_SECURITY_VIOLATION: Error := 'W_SECURITY_VIOLATION';
      W_WRONG_CHV: Error := 'W_WRONG_CHV';
      W_CHV_BLOCKED: Error := 'W_CHV_BLOCKED';
      W_EOF: Error := 'W_EOF';
      W_CANCELLED_BY_USER: Error := 'W_CANCELLED_BY_USER';
      W_CARD_NOT_AUTHENTICATED: Error := 'W_CARD_NOT_AUTHENTICATED';
    end;

    if Error <> '' then
    begin
      raise ESmartCardError.Create(format('Error - %s - %d', [Error, Result]), Result);
    end;
  end;
end;

procedure TPTLibSmartCardPcsc.EstablishCardContext;
begin
  FLastStates := [];

  CheckResult(
    {$IFDEF MSWINDOWS}SCardConnectW{$ELSE}SCardConnect{$ENDIF}(
      Context,
      {$IFDEF MSWINDOWS}PWideChar(GetReaderName){$ELSE}PAnsiChar(AnsiString(GetReaderName)){$ENDIF},
      SCARD_SHARE_EXCLUSIVE,
      SCARD_PROTOCOL_TX,
      FReaderHandle,
      @FAttrProtocol
    )
  );
end;

function TPTLibSmartCardPcsc.ReaderConnected: Boolean;
begin
  Result := FReaderHandle <> 0;
end;

procedure TPTLibSmartCardPcsc.ReleaseCardContext;
begin
  if FReaderHandle <> 0 then
  begin
    SCardDisconnect(
      FReaderHandle,
      SCARD_RESET_CARD);
  end;
end;

function TPTLibSmartCardPcsc.CheckResult(const Value: Cardinal): Cardinal;
begin
  Result := CheckResult(Value, []);
end;

procedure TPTLibSmartCardPcsc.DoDisconnectCard;
begin
  if FReaderHandle <> 0 then
  begin
    SCardCancel(FReaderHandle);
    FReaderHandle := 0;
  end;
end;

procedure TPTLibSmartCardPcsc.Connect;
begin
  FStatus := SCARD_STATE_UNAWARE;

  FCardContext := EstablishContext;

  TGlobalLog.Log(Self, 'PCSC reader activated', LogSeverityInfo);
end;

procedure TPTLibSmartCardPcsc.Disconnect;
begin
  DisconnectCard;

  ReleaseCardContext;

  ReleaseContext(FCardContext);

  TGlobalLog.Log(Self, 'PCSC reader deactivated', LogSeverityInfo);
end;

procedure TPTLibSmartCardPcsc.DoSetActive(const Value: Boolean);
begin
  if Value then
  begin
    Connect;
  end
  else
  begin
    Disconnect;
  end;

  inherited;
end;

function TPTLibSmartCardPcsc.EstablishContext: TSCardContext;
begin
  CheckResult(
    SCardEstablishContext(
      SCARD_SCOPE_SYSTEM,
      nil,
      nil,
      @Result));
end;

procedure TPTLibSmartCardPcsc.DoGetResponseFromCard(const APDU: TBytes; out Response: TBytes; out Success: Boolean);
var
  RetVar: Cardinal;
  Ppci: Pointer;
  SLen: Cardinal;
  ALen: DWORD;
  {$IFNDEF MSWINDOWS}
  RLen: LPDWORD;
  {$ENDIF}
begin
  Success := False;

  if Length(APDU) <= MAXAPDULENGTH then
  begin
    SetLength(Response, MAXAPDULENGTH);

    case FAttrProtocol of
      SCARD_PROTOCOL_T0: Ppci := @SCARD_PCI_T0;
      SCARD_PROTOCOL_T1: Ppci := @SCARD_PCI_T1;
    else
      Ppci := nil;
    end;

    SLen := Length(APDU);
    ALen := Length(Response);

    {$IFDEF MSWINDOWS}
    RetVar :=
      SCardTransmit(
        FReaderHandle,
        Ppci,
        Pointer(APDU),
        SLen,
        nil,
        Pointer(Response),
        @ALen
      );
    {$ELSE}
    new(RLen);
    try
      RLen^ := ALen;

      RetVar :=
        SCardTransmit(
          FReaderHandle,
          Ppci,
          @APDU[0],
          SLen,
          nil,
          @Response[0],
          RLen
        );

      ALen := RLen^;
    finally
      Dispose(RLen);
    end;
    {$ENDIF}

    CheckResult(Retvar);

    SetLength(Response, ALen);

    Success := True;
  end;
end;

procedure TPTLibSmartCardPcsc.SetCardData(const Value: TCardData);
begin
  FCardDataCS.Enter;
  try
    FCardData := Value;
  finally
    FCardDataCS.Leave;
  end;
end;

procedure TPTLibSmartCardPcsc.SpringCardReaderStateChanged(const States: TSmartCardReaderStates; const Data: TBytes);
var
  Response: TBytes;
  UID: String;
  i: Integer;
  LocalCardData: TCardData;
begin
  inherited;

  LocalCardData.ATR := Data;

  if length(Data) = 0 then
  begin
    LocalCardData.CardType := '';
    LocalCardData.Protocol := '';
    LocalCardData.UID := '';

    SetCardData(LocalCardData);

    SetCardUID('');
  end
  else
  begin
    if length(Data) >= 19 then
    begin
      LocalCardData.Protocol := SmartCardProtocolToString(Data[12]);

      if (Data[4] = $80) and
         (Data[5] = $4F) and
         (Data[6] = $0C) and
         (Data[7] = $A0) and
         (Data[8] = $00) and
         (Data[9] = $00) and
         (Data[10] = $03) and
         (Data[11] = $06) then
      begin
        LocalCardData.CardType := SmartCardIDToName(IntToHex(Data[13]) + IntToHex(Data[14]));
      end
      else
      begin
        LocalCardData.CardType := 'Unknown Card Type';
      end;
    end
    else
    begin
      LocalCardData.CardType := 'Smartcard';
    end;

    if (not (TSmartCardReaderState.crsInUse in States)) and
       (GetResponseFromCard([$FF, $CA, $00, $00], Response)) then
    begin
      UID := '';

      for i := low(Response) to high(Response) do
      begin
        UID := UID + IntToHex(Response[i]);
      end;

      LocalCardData.UID := UID;

      SetCardData(LocalCardData);

      SetCardUID(UID);
    end;
  end;
end;

procedure TPTLibSmartCardPcsc.DoReaderStateChanged(const States: TSmartCardReaderStates; const Data: TBytes);
begin
  if pos(SpringcardReaderName, FReaderName) = 1 then
  begin
    SpringCardReaderStateChanged(States, Data);
  end;
end;

function TPTLibSmartCardPcsc.GetCardContext: TSCardContext;
begin
  Result := FCardContext;
end;

function TPTLibSmartCardPcsc.CardConnected: Boolean;
begin
  Result := FCardContext <> 0;
end;

procedure TPTLibSmartCardPcsc.DoGetInformation(const Information: IInformationList);
begin
  inherited;

  if Active then
  begin
    Information.AddProperty('Connected', BooleanToString(ReaderConnected));

    if ReaderConnected then
    begin
      Information.AddProperty('Connected to', FReaderName);

      if CardConnected then
      begin
        Information.AddProperty('Card Type', CardData.CardType);
        Information.AddProperty('Protocol', CardData.Protocol);
      end;
    end;
  end;
end;

procedure TPTLibSmartCardPcsc.DoGetReaderList(out Values: TArray<String>);
{$IFDEF MSWINDOWS}
var
  Size: LongInt;
  Readers: String;
begin
  // Get the size of the reader list
  CheckResult(
    SCardListReadersW(
      Context,
      nil,
      nil,
      Size
    )
  );

  // Set the length of the readers string
  SetLength(Readers, Size);

  // Get the readers
  CheckResult(
    SCardListReadersW(
      Context,
      nil,
      PWideChar(Readers),
      Size
    )
  );

  SetLength(Values, 0);

  // Add the readers to the Values
  while Length(Readers) > 0 do
  begin
    SetLength(Values, Length(Values) + 1);

    Values[high(Values)] := NextBlock(Readers, #00);
  end;
end;
{$ELSE}
var
  Size: LongInt;
  Readers: PAnsiChar;
  ReadersStr: String;
begin
  // Get the size of the reader list
  CheckResult(
    SCardListReaders(
      Context,
      nil,
      nil,
      Size
    )
  );

  // Set the length of the readers string
  GetMem(Readers, Size);
  try
    // Get the readers
    CheckResult(
      SCardListReaders(
        Context,
        nil,
        Readers,
        Size
      )
    );

    SetLength(Values, 0);

    ReadersStr := String(Readers);
  finally
    Dispose(Readers);
  end;

  // Add the ReadersStr to the Values
  while Length(ReadersStr) > 0 do
  begin
    SetLength(Values, Length(Values) + 1);

    Values[high(Values)] := NextBlock(ReadersStr, #00);
  end;
end;
{$ENDIF}

procedure TPTLibSmartCardPcsc.DoGetReaderStates(out Value: TSmartCardReaderStates);
begin
  Value := [];

  if FStatus = SCARD_STATE_UNAWARE then
  begin
    Value := Value + [TSmartCardReaderState.crsUnaware];
  end;

  if FStatus and SCARD_STATE_EMPTY <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsEmpty];
  end;

  if FStatus and SCARD_STATE_PRESENT <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsPresent];
  end;

  if FStatus and SCARD_STATE_MUTE <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsMute];
  end;

  if FStatus and SCARD_STATE_UNPOWERED <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsUnpowered];
  end;

  if FStatus and SCARD_STATE_ATRMATCH <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsATMatch];
  end;

  if FStatus and SCARD_STATE_EXCLUSIVE <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsExclusive];
  end;

  if FStatus and SCARD_STATE_INUSE <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsInUse];
  end;

  if FStatus and SCARD_STATE_IGNORE <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsIgnore];
  end;

  if FStatus and SCARD_STATE_UNKNOWN <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsUnknown];
  end;

  if FStatus and SCARD_STATE_UNAVAILABLE <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsUnavailable];
  end;

  if FStatus and SCARD_STATE_CHANGED <> 0 then
  begin
    Value := Value + [TSmartCardReaderState.crsChanged];
  end;
end;

procedure TPTLibSmartCardPcsc.DoCheckState(const StateChanged: TStateChangedProc;
  const ReadTimeout: Integer);
var
  States: Array[0..1] of {$IFDEF MSWINDOWS}SCARD_READERSTATEA{$ELSE}SCARD_READERSTATEW{$ENDIF};
  i: Integer;
  Data: TBytes;
begin
  FillChar(States, SizeOf(States), #0);

  States[0].szReader := {$IFDEF MSWINDOWS}PAnsiChar(AnsiString(GetReaderName)){$ELSE}PWideChar(AnsiString(GetReaderName)){$ENDIF};
  States[0].pvUserData := nil;
  States[0].dwCurrentState := FStatus;

  ReleaseCardContext;

  FLastStates := GetReaderStates;

  // Get the size of the reader list
  {$IFDEF MSWINDOWS}
  CheckResult(
    SCardGetStatusChangeA(
      Context,
      ReadTimeout,
      States,
      1
    ),
    [E_TIMEOUT]
  );
  {$ENDIF}
  {$IFDEF LINUX64}
  CheckResult(
    SCardGetStatusChange(
      Context,
      ReadTimeout,
      States,
      1
    ),
    [E_TIMEOUT]
  );
  {$ENDIF}

  if Active then
  begin
    FStatus := States[0].dwEventState;

    if FLastStates * GetReaderStates <> [] then
    begin
      SetLength(Data, States[0].cbAtr);

      for i := 0 to pred(States[0].cbAtr) do
      begin
        Data[i] := States[0].rgbAtr[i];
      end;

      if TSmartCardReaderState.crsPresent in GetReaderStates then
      begin
        FReadCardContext := False;
      end;

      if (not FReadCardContext) and
         (TSmartCardReaderState.crsPresent in GetReaderStates) and
         (not (TSmartCardReaderState.crsInUse in GetReaderStates)) then
      begin
        EstablishCardContext;

        FReadCardContext := True;
      end;

      DoReaderStateChanged(GetReaderStates, Data);

      if Assigned(StateChanged) then
      begin
        StateChanged(GetReaderStates, Data);
      end;
    end;
  end;
end;

procedure TPTLibSmartCardPcsc.ReleaseContext(const Context: TSCardContext);
begin
  SCardCancel(Context);

  SCardReleaseContext(Context);

  FCardContext := THandle(0);
end;

function TPTLibSmartCardPcsc.GetCardData: TCardData;
begin
  FCardDataCS.Enter;
  try
    Result := FCardData;
  finally
    FCardDataCS.Leave;
  end;
end;

function TPTLibSmartCardPcsc.GetReaderList: TArray<String>;
begin
  DoGetReaderList(Result);
end;

function TPTLibSmartCardPcsc.GetReaderName: String;
begin
  Result := FReaderName;
end;

function TPTLibSmartCardPcsc.GetReaderStates: TSmartCardReaderStates;
begin
  DoGetReaderStates(Result);
end;

function TPTLibSmartCardPcsc.GetReaderStateDescription: String;
begin
  Result := SmartCardReaderStatesToDescription(GetReaderStates);
end;

function TPTLibSmartCardPcsc.GetResponseFromCard(const APDU: TBytes; out Response: TBytes): Boolean;
begin
  DoGetResponseFromCard(APDU, Response, Result);
end;

procedure TPTLibSmartCardPcsc.CheckState(const StateChanged: TStateChangedProc; const ReadTimeout: Integer);
begin
  DoCheckState(StateChanged, ReadTimeout);
end;

procedure TPTLibSmartCardPcsc.CheckStateASync(const StateChanged: TStateChangedProc;
  const OnException: TProc<Exception>);
begin
  if Active then
  begin
    FCheckTask := TTask.Create(
      procedure()
      begin
        while (FCheckTask <> nil) and
              (FCheckTask.Status = TTaskStatus.Running) and
              (Active) do
        begin
          try
            DoCheckState(
              procedure(const States: TSmartCardReaderStates; const Data: TBytes)
              begin
                if (Active) and
                   (FCheckTask.Status = TTaskStatus.Running) and
                   (Assigned(StateChanged)) then
                begin
                  StateChanged(States, Data);
                end;
              end,
              CARD_READ_TIMEOUT
            );
          except
            on e: Exception do
            begin
              OnException(e);

              if (e is ESmartCardError) then
              begin
                Connect;
              end;

              sleep(1000);
            end;
          end;
        end;

        sleep(1);
      end
    );

    FCheckTask.Start;
  end;
end;

procedure TPTLibSmartCardPcsc.ConnectCard(const ReaderName: String);
begin
  FReaderName := ReaderName;

  DoConnectCard(ReaderName);
end;

constructor TPTLibSmartCardPcsc.Create(const CardUIDChanged: TCardUIDChanged);
begin
  inherited;

  FCardDataCS := TCriticalSection.Create;
end;

destructor TPTLibSmartCardPcsc.Destroy;
begin
  FreeAndNil(FCardDataCS);

  inherited;
end;

procedure TPTLibSmartCardPcsc.DisconnectCard;
begin
  if Assigned(FCheckTask) then
  begin
    FCheckTask.Cancel;
    FCheckTask := nil;
  end;

  FReaderName := '';

  DoDisconnectCard;
end;

procedure TPTLibSmartCardPcsc.DoConnectCard(const ReaderName: String);
begin
  // Override
end;

{ TPTLibSmartCardPcscAutoUID }

procedure TPTLibSmartCardPcscAutoUID.SetDefaultReaderName(const Value: String);
begin
  if FDefaultReaderName <> Value then
  begin
    Active := False;

    FDefaultReaderName := Value;
  end;
end;

procedure TPTLibSmartCardPcscAutoUID.DoSetActive(const Value: Boolean);
var
  Readers: TArray<String>;
  ReaderName: String;
begin
  inherited;

  if Value then
  begin
    ReaderName := '';

    if FDefaultReaderName = '' then
    begin
      Readers := GetReaderList;

      if length(Readers) > 0 then
      begin
        ReaderName := Readers[0];
      end
    end
    else
    begin
      ReaderName := FDefaultReaderName;
    end;

    if ReaderName = '' then
    begin
      raise ESmartCardError.Create('Unable to find a compatible PCSC smart card reader', E_READER_UNSUPPORTED);
    end

    else
    begin
      ConnectCard(ReaderName);

      CheckStateASync(
        nil,
        procedure(e: Exception)
        begin
          if (e is ESmartCardError) then
          begin
            TGlobalLog.Log(Self, e.Message, LogSeverityError);
          end;
        end
      );
    end;
  end;
end;

end.
