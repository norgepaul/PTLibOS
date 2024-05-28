unit PTLib.Common.SmartCard;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.SyncObjs,
  System.DateUtils,

  PTLib.Common.Classes,
  PTLib.Common.Dates,
  PTLib.Common.Interfaces,
  PTLib.Common.Log,
  PTLib.Common.Types,
  PTLib.Common.Strings,
  PTLib.Common.Timers,
  PTLib.Common.InformationList,

  PTLib.Common.SmartCard.Types;

type
  ESmartCardError = class(EPTLibError)
  strict private
    FErrorCode: Cardinal;
  public
    constructor Create(const Msg: string; const ErrorCode: Cardinal); virtual;

    property ErrorCode: Cardinal read FErrorCode;
  end;

  TPTLibSmartCard = class(TInterfacedObjectInfo,
                          IPTLibSmartCard,
                          IClassDescriptor)
  private
    FOnActiveChanged: TNotifyEvent;
    FActive: Boolean;
    FCardUIDChanged: TCardUIDChanged;
    FUID: String;
    FLastUID: String;
    FRFIDCS: TCriticalSection;
    FRFIDChanged: Boolean;
    FRFIDChangedTimer: IPTLibGlobalTimer;
    FReverseUID: Boolean;
    FLastRFIDChange: TDateTime;

    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetCardUID: String;
    function GetLastUID: String;
    function GetConnected: Boolean;
    procedure RFIDChangedTimer(Sender: TObject);
    function GetReverseUID: Boolean;
    procedure SetReverseUID(const Value: Boolean);
    function GetOnActiveChanged: TNotifyEvent;
    procedure SetOnActiveChanged(const Value: TNotifyEvent);
    function GetLastRFIDChangeMs: Cardinal;
    procedure ResetLastRFIDChange;
  protected
    procedure DoGetInformation(const Information: IInformationList); override;

    procedure SetCardUID(const UID: String);
    procedure DoSetActive(const Value: Boolean); virtual;
    procedure DoCardUIDChanged(const UID: String); virtual;
    procedure GetClassDescriptor(out Value: String); virtual;
    procedure DoOnActiveChanged; virtual;

    property Active: Boolean read GetActive write SetActive;
    property CardUID: String read GetCardUID;
    property LastCardUID: String read GetLastUID;
    property Connected: Boolean read GetConnected;
    property ReverseUID: Boolean read GetReverseUID write SetReverseUID;
    property LastRFIDChangeMs: Cardinal read GetLastRFIDChangeMs;
  public
    constructor Create(const CardUIDChanged: TCardUIDChanged); virtual;
    destructor Destroy; override;

    property OnActiveChanged: TNotifyEvent read GetOnActiveChanged write SetOnActiveChanged;
  end;

function NewSmartCardPCSC(const CardUIDChanged: TCardUIDChanged): IPTLibSmartCardPCSC;
function NewSmartCardSpringCardSerial(const SerialPort: String; const CardUIDChanged: TCardUIDChanged; const SynchronizeEvents: Boolean): IPTLibSmartCardSerial;
function NewSmartCardGemProxSerial(const SerialPort: String; const CardUIDChanged: TCardUIDChanged; const SynchronizeEvents: Boolean): IPTLibSmartCardSerial;
function NewSmartCardFileData(const CardUIDChanged: TCardUIDChanged; const Filename: String): IPTLibSmartCardFileData;

implementation

uses
  PTLib.Common.SmartCard.Serial,
  PTLib.Common.SmartCard.FileData
  {$if Defined(MSWINDOWS) or defined(LINUX64)}
  ,PTLib.Common.SmartCard.Pcsc
  {$ENDIF}
  ;

resourcestring
  StrUnableToActivateT = 'Unable to activate the smart card reader - %s';

function NewSmartCardPCSC(const CardUIDChanged: TCardUIDChanged): IPTLibSmartCardPCSC;
begin
  Result := TPTLibSmartCardPcscAutoUID.Create(CardUIDChanged);
end;

function NewSmartCardFileData(const CardUIDChanged: TCardUIDChanged; const Filename: String): IPTLibSmartCardFileData;
begin
  Result := TPTLibSmartCardFileData.Create(CardUIDChanged, Filename);
end;

function NewSmartCardSpringCardSerial(const SerialPort: String; const CardUIDChanged: TCardUIDChanged; const SynchronizeEvents: Boolean): IPTLibSmartCardSerial;
begin
  Result := TPTLibSmartCardSerialPortSpringcard.Create(SerialPort, CardUIDChanged, SynchronizeEvents);
end;

function NewSmartCardGemProxSerial(const SerialPort: String; const CardUIDChanged: TCardUIDChanged; const SynchronizeEvents: Boolean): IPTLibSmartCardSerial;
begin
  Result := TPTLibSmartCardSerialPortGemProx.Create(SerialPort, CardUIDChanged, SynchronizeEvents);
end;

{ TPTLibSmartCard }

constructor TPTLibSmartCard.Create(const CardUIDChanged: TCardUIDChanged);
begin
  FRFIDCS := TCriticalSection.Create;

  FCardUIDChanged := CardUIDChanged;
  FRFIDChangedTimer := TPTLibGobalTimerController.NewTimer(Self, RFIDChangedTimer, 5);
end;

destructor TPTLibSmartCard.Destroy;
begin
  Active := False;

  FreeAndNil(FRFIDCS);

  inherited;
end;

procedure TPTLibSmartCard.ResetLastRFIDChange;
begin
  FLastRFIDChange := nowUTC;
end;

procedure TPTLibSmartCard.RFIDChangedTimer(Sender: TObject);
begin
  if FRFIDChanged then
  begin
    FRFIDChanged := False;

    ResetLastRFIDChange;

    if Assigned(FCardUIDChanged) then
    begin
      FCardUIDChanged(CardUID);
    end;
  end;
end;

procedure TPTLibSmartCard.DoCardUIDChanged(const UID: String);
begin
  FRFIDChanged := True;
end;

procedure TPTLibSmartCard.DoGetInformation(const Information: IInformationList);
begin
  inherited;

  Information.AddHeading('Smartcard Reader');
  Information.AddProperty('Active', BooleanToString(Active));
  Information.AddProperty('Current UID', CardUID);
  Information.AddProperty('Last UID', LastCardUID);
end;

procedure TPTLibSmartCard.DoSetActive(const Value: Boolean);
begin
end;

function TPTLibSmartCard.GetActive: Boolean;
begin
  Result := FActive;
end;

function TPTLibSmartCard.GetCardUID: String;
begin
  FRFIDCS.Enter;
  try
    Result := FUID;
  finally
    FRFIDCS.Leave;
  end;
end;

procedure TPTLibSmartCard.GetClassDescriptor(out Value: String);
begin
  Value := 'SmartCardReader';
end;

function TPTLibSmartCard.GetConnected: Boolean;
begin
  Result := Active;
end;

function TPTLibSmartCard.GetLastRFIDChangeMs: Cardinal;
begin
  if FLastRFIDChange = 0 then
  begin
    Result := 0;
  end
  else
  begin
    Result := MillisecondsBetween(nowUTC, FLastRFIDChange);
  end;
end;

function TPTLibSmartCard.GetLastUID: String;
begin
  FRFIDCS.Enter;
  try
    Result := FLastUID;
  finally
    FRFIDCS.Leave;
  end;
end;

function TPTLibSmartCard.GetOnActiveChanged: TNotifyEvent;
begin
  Result := FOnActiveChanged;
end;

function TPTLibSmartCard.GetReverseUID: Boolean;
begin
  Result := FReverseUID;
end;

procedure TPTLibSmartCard.DoOnActiveChanged;
begin
  if Assigned(FOnActiveChanged) then
  begin
    FOnActiveChanged(Self);
  end;
end;

procedure TPTLibSmartCard.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    try
      DoSetActive(Value);

      DoOnActiveChanged;

      ResetLastRFIDChange;
    except
      on e: Exception do
      begin
        FActive := not Value;

        TGLobalLog.Log(
          Self,
          StrUnableToActivateT,
          [e.Message],
          LogSeverityError);

        raise;
      end;
    end;
  end;
end;

procedure TPTLibSmartCard.SetCardUID(const UID: String);
var
  ClippedUID: String;
begin
  FRFIDCS.Enter;
  try
    ClippedUID := copy(UID, 1, 8);

    if FReverseUID then
    begin
      ClippedUID := ReverseByteString(ClippedUID);
    end;

    if FUID <> ClippedUID then
    begin
      if ClippedUID <> '' then
      begin
        FLastUID := ClippedUID;
      end;

      FUID := ClippedUID;

      TGLobalLog.Log(
        Self,
        'UID Changed: "%s"',
        [FUID],
        LogSeverityDebug2);

      if Assigned(FCardUIDChanged) then
      begin
        DoCardUIDChanged(ClippedUID);
      end;
    end;
  finally
    FRFIDCS.Leave;
  end;
end;

procedure TPTLibSmartCard.SetOnActiveChanged(const Value: TNotifyEvent);
begin
  FOnActiveChanged := Value;
end;

procedure TPTLibSmartCard.SetReverseUID(const Value: Boolean);
begin
  FReverseUID := Value;
end;

{ ESmartCardError }

constructor ESmartCardError.Create(const Msg: string;
  const ErrorCode: Cardinal);
begin
  inherited Create(Msg);

  FErrorCode := ErrorCode;
end;

end.
