unit PTLib.Common.SmartCard.Serial;

interface

uses
  System.SysUtils, System.Classes, System.DateUtils,

  PTLib.Common.Interfaces,
  PTLib.Common.Dates,
  PTLib.Common.Classes,
  PTLib.Common.Strings,
  PTLib.Common.Log,
  PTLib.Common.Types,
  PTLib.Common.Utils,
  PTLib.Common.SerialPort,
  PTLib.Common.SerialPort.Async,
  PTLib.Common.SmartCard.Types;

type
  TPTLibSmartCardSerial = class(TSerialPortASync,
                                IPTLibSmartCardSerial)
  strict private
    FCardUIDChanged: TCardUIDChanged;
    FUID: String;
    FLastUID: String;
    FSerialPortSettings: ISerialPortSettings;
    FSynchronizeEvents: Boolean;
    FReverseUID: Boolean;
    FLastRFIDChange: TDateTime;
  private
    function GetCardUID: String;
    function GetLastUID: String;
    function GetConnected: Boolean;
    function GetSerialPortSettings: ISerialPortSettings;
    function GetReverseUID: Boolean;
    procedure SetReverseUID(const Value: Boolean);
    function GetLastRFIDChangeMs: Cardinal;
    procedure ResetLastRFIDChange;
  protected
    procedure DoGetInformation(Value: IInformationList); override;

    procedure DoConfigureSerialPortSettings(SerialPortSettings: ISerialPortSettings); virtual; abstract;

    procedure DoCardUIDChanged(const UID: String); virtual;
    procedure GetClassDescriptor(out Value: String); virtual;
    procedure SetCardUID(const UID: String);
    procedure SetCardData(const Data: TBytes);

    property CardUID: String read GetCardUID;
    property LastCardUID: String read GetLastUID;
    property Connected: Boolean read GetConnected;
    property SerialPortSettings: ISerialPortSettings read GetSerialPortSettings;
    property ReverseUID: Boolean read GetReverseUID write SetReverseUID;
    property LastRFIDChangeMs: Cardinal read GetLastRFIDChangeMs;
  public
    constructor Create(const SerialPort: String; const CardUIDChanged: TCardUIDChanged; const SynchronizeEvents: Boolean); reintroduce; overload;
  end;

  TPTLibSmartCardSerialPortSpringCard = class(TPTLibSmartCardSerial)
  strict private
    type
      TSpringCardCommandType = (
        cspringReadFirmwareVersion,
        cspringTurnOnRFField,
        cspringReadCardRFID
      );
  strict private
    FCommandType: TSpringCardCommandType;
  protected
    procedure DoConfigureSerialPortSettings(SerialPortSettings: ISerialPortSettings); override;
    procedure DoSerialPortComms(const SerialPort: TBaseSerialPortConnection); override;
    procedure DoCreateSerialPort(out Value: TBaseSerialPortConnection); override;
  end;

  TPTLibSmartCardSerialPortGemProx = class(TPTLibSmartCardSerial)
  strict private
    type
      TGemProxReadState = (
        grsNone,
        grsFirmware,
        grsReadRFID1,
        grsReadRFID2
      );
  private
    FReadState: TGemProxReadState;
    FLastData: TBytes;
    FNoReadCount: Integer;
  protected
    procedure DoConfigureSerialPortSettings(SerialPortSettings: ISerialPortSettings); override;
    procedure DoSerialPortComms(const SerialPort: TBaseSerialPortConnection); override;
    procedure DoCreateSerialPort(out Value: TBaseSerialPortConnection); override;
  end;

implementation

{ TPTLibSmartCardSpringCardSerial }

procedure TPTLibSmartCardSerial.SetCardData(const Data: TBytes);
begin
  if length(Data) > 0 then
  begin
    SetCompatibleDeviceDetected;
  end;

  SetCardUID(BytesToHex(Data));
end;

constructor TPTLibSmartCardSerial.Create(const SerialPort: String; const CardUIDChanged: TCardUIDChanged; const SynchronizeEvents: Boolean);
begin
  FSerialPortSettings := TSerialPortSettings.Create;
  DoConfigureSerialPortSettings(FSerialPortSettings);
  FSerialPortSettings.SerialPort := SerialPort;
  FCardUIDChanged := CardUIDChanged;
  FSynchronizeEvents := SynchronizeEvents;

  inherited Create(FSerialPortSettings, [TSerialPortOption.spoAutoReconnect]);
end;

procedure TPTLibSmartCardSerial.DoCardUIDChanged(const UID: String);
begin
  if Assigned(FCardUIDChanged) then
  begin
    if FSynchronizeEvents then
    begin
      TThread.Synchronize(
        nil,
        procedure
        begin
          FCardUIDChanged(UID);

          ResetLastRFIDChange;
        end
      );
    end
    else
    begin
      FCardUIDChanged(UID);

      ResetLastRFIDChange;
    end;
  end;
end;

procedure TPTLibSmartCardSerial.GetClassDescriptor(out Value: String);
begin
  Value := 'SerialSmartCardReader';
end;

procedure TPTLibSmartCardSerial.DoGetInformation(Value: IInformationList);
begin
  inherited;

  Value.AddHeading('Serial Port Smartcard Reader - ' + ClassName);
  Value.AddProperty('Active', BooleanToString(Active));
  Value.AddProperty('Current UID', CardUID);
  Value.AddProperty('Last UID', LastCardUID);
end;

procedure TPTLibSmartCardSerial.SetCardUID(const UID: String);
var
  ClippedUID: String;
begin
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
end;

procedure TPTLibSmartCardSerial.SetReverseUID(const Value: Boolean);
begin
  FReverseUID := Value;
end;

function TPTLibSmartCardSerial.GetCardUID: String;
begin
  Result := FUID;
end;

function TPTLibSmartCardSerial.GetConnected: Boolean;
begin
  Result := State = TSerialPortState.spsConnected;
end;

function TPTLibSmartCardSerial.GetLastRFIDChangeMs: Cardinal;
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

function TPTLibSmartCardSerial.GetLastUID: String;
begin
  Result := FLastUID;
end;

function TPTLibSmartCardSerial.GetReverseUID: Boolean;
begin
  Result := FReverseUID;
end;

function TPTLibSmartCardSerial.GetSerialPortSettings: ISerialPortSettings;
begin
  Result := FSerialPortSettings;
end;

procedure TPTLibSmartCardSerial.ResetLastRFIDChange;
begin
  FLastRFIDChange := nowUTC;
end;

{ TPTLibSmartCardSerialPortSpringCard }

procedure TPTLibSmartCardSerialPortSpringCard.DoConfigureSerialPortSettings(SerialPortSettings: ISerialPortSettings);
begin
  SerialPortSettings.BaudRate := 38400;
end;

procedure TPTLibSmartCardSerialPortSpringCard.DoCreateSerialPort(out Value: TBaseSerialPortConnection);
begin
  inherited;

  Value.LogDescription := 'SpringCardSerialPort';
  Value.SetDebugLowLevel(True);
end;

procedure TPTLibSmartCardSerialPortSpringCard.DoSerialPortComms(const SerialPort: TBaseSerialPortConnection);
const
  Terminator: TBytes = [$0A, $0D];
var
  WriteData, Echo, Ack, Data: TBytes;
begin
  case FCommandType of
    cspringReadFirmwareVersion: WriteData := [$4F, $00];
    cspringTurnOnRFField: WriteData := [$58, $02, $0A, $64];
    cspringReadCardRFID: WriteData := [$60, $02, $00, $83];
  end;

  SerialPort.Flush;

  WriteData := BytesToEncodedASCIIBytes(WriteData);

  SerialPort.Write([ord('$')] + WriteData + Terminator);

  if (SerialPort.ReadBytesUntil(Terminator[0], Echo)) and
     (SerialPort.ReadBytesUntil(Terminator[0], Ack)) and
     (SerialPort.ReadBytesUntil(Terminator[0], Data)) then
  begin
    if Ack[0] <> ord('+') then
    begin
      TGLobalLog.Log(
        Self,
        'No ack received for command "%s"',
        [BytesToOrdVals(WriteData, False)],
        LogSeverityWarning);
    end
    else
    begin
      // Seems like a card reader bug - sometimes it returns "> " as the first two chars.
      // We need to remove them
      if SameBytes(CopyBytes(Data, 0, 2), [62, 32])  then
      begin
        Data := CopyBytes(Data, 2, length(Data));
      end;

      case FCommandType of
        cspringReadFirmwareVersion:
          begin
            //SetParam('FirmwareVersion', Trim(BytesToString(Data)));

            FCommandType := cspringTurnOnRFField;
          end;

        cspringTurnOnRFField:
          begin
            FCommandType := cspringReadCardRFID;
          end;

        cspringReadCardRFID:
          begin
            if SameBytes(CopyBytes(Data, 0, 8), BytesToEncodedASCIIBytes([$00, $06, $00, $01])) then
            begin
              //SetParam('SmartCardType', TSmartCardIDType.sct4ByteID);
              SetCardData(EncodedASCIIBytesToBytes(CopyBytes(Data, 8, 8)));
            end else
            if SameBytes(CopyBytes(Data, 0, 8), BytesToEncodedASCIIBytes([$00, $09, $00, $01])) then
            begin
              //SetParam('SmartCardType', TSmartCardIDType.sct7ByteID);
              SetCardData(EncodedASCIIBytesToBytes(CopyBytes(Data, 8, 14)));
            end
            else
            begin
              //SetParam('SmartCardType', TSmartCardIDType.sctUnknown);
              SetCardData([]);
            end;

            //FCommandType := cspringTurnOnRFField;
          end;
      end;
    end;
  end;

  sleep(100);
end;

{ TPTLibSmartCardSerialPortGemProx }

procedure TPTLibSmartCardSerialPortGemProx.DoConfigureSerialPortSettings(SerialPortSettings: ISerialPortSettings);
begin
  SerialPortSettings.BaudRate := 9600;
end;

procedure TPTLibSmartCardSerialPortGemProx.DoCreateSerialPort(out Value: TBaseSerialPortConnection);
begin
  inherited;

  Value.LogDescription := 'GemProxSerialPort';
  Value.SetDebugLowLevel(True);
end;

procedure TPTLibSmartCardSerialPortGemProx.DoSerialPortComms(const SerialPort: TBaseSerialPortConnection);

  function BytesToRFIDBytes(const Data: TBytes; const Count: Integer): TBytes;
  var
    i: Integer;
  begin
    Result := [];

    for i := 6 + Count - 1 downto 6 do
    begin
      Result := Result + [Data[i]];
    end;
  end;

var
  PayloadLength: Byte;
  Data, RFIDData: TBytes;
  Preamble, DummyByte: Byte;
begin
  case FReadState of
    grsNone:
      begin
        //Log(StrGemProxCardReader);

        FReadState := grsFirmware;

        Exit;
      end;

    grsFirmware:
      begin
        //Log(StrGemProxReadingFirm);

        FReadState := grsReadRFID1;

        Data := [66, 64, 02, 225, 255, 1, 227];

        Exit;
      end;

    grsReadRFID1, grsReadRFID2:
      begin
        if FReadState = grsReadRFID1 then
        begin
          Data := [66, 64, 02, 226, 1, 227];

          FReadState := grsReadRFID2;
        end
        else
        begin
          Data := [66, 0, 02, 226, 1, 163];

          FReadState := grsReadRFID1;
        end;
      end;
  end;

  SerialPort.Flush;
  SerialPort.Write(Data);

  // Preamble
  while (SerialPort.ReadByte(Preamble)) and
        (Preamble <> 36) do;

  // Dummy Byte
  SerialPort.ReadByte(DummyByte);

  // Payload length
  SerialPort.ReadByte(PayloadLength);

  if SerialPort.ReadBytes(PayloadLength, Data) then
  begin
    if length(Data) in [12, 15] then
    begin
      FNoReadCount := 0;

      if length(Data) = 12 then
      begin
        RFIDData := BytesToRFIDBytes(Data, 4);
      end else
      if length(Data) = 15 then
      begin
        RFIDData := BytesToRFIDBytes(Data, 7);
      end;

      SetCardData(RFIDData);
    end else

    begin
      if FNoReadCount = 2 then
      begin
        SetCardData([]);
      end;

      Inc(FNoReadCount);
    end;

    FLastData := Data;
  end;

  sleep(100);
end;

end.

