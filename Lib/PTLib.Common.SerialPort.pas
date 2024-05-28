unit PTLib.Common.SerialPort;

{.$DEFINE DEBUG_LOW_LEVEL}

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs,

  PTLib.Common.Classes,
  PTLib.Common.Timers,
  PTLib.Common.Thread,
  PTLib.Common.Interfaces,
  PTLib.Common.Types,
  PTLib.Common.Strings,
  PTLib.Common.Buffers,
  PTLib.Common.InformationList,
  PTLib.Common.Files,
  PTLib.Common.Log;

type
  //TSerialPortThreaded = class;

  TSHandle = {$IFDEF LINUX64}Integer{$ELSE}THandle{$ENDIF};

  ESerialPortError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(const ErrorCode: Integer; const Msg: string);

    property ErrorCode: Integer read FErrorCode;
  end;

  TOnStateChange = procedure(const Sender: TObject; const SerialPortState: TSerialPortState) of object;
  TOnError = procedure(const Sender: TObject; const e: Exception) of object;
  TOnDataWritten = procedure(const Sender: TObject; const Data: TBytes) of object;

  TBaseSerialPortConnection = class(TObject)
  public
    const
      ErrorCodeUnknown = 90000;
      ErrorCodeTerminatorNotFoundTimeout = 90001;
      ErrorCodeSerialPortNotActive = 90002;
      ErrorCodeUnableToConnect = 90003;
      ErrorCodeSerialPortSetupFailed = 90004;
      ErrorCodeUnableToRetrieveSerialPortState = 90005;
      ErrorCodeUnableToBuildSerialPortDCB = 90006;
      ErrorCodeUnableToSetSerialPortState = 90007;
      ErrorCodeUnableToSetSerialPortTimeouts = 90008;
      ErrorCodeTimeout = 90009;
      ErrorCodeWriteCountMismatch = 90009;
      ErrorCodeWriteBufferFull = 90010;

    type
      TSerialOption = (
        sopCanonical,
        sopEcho,
        sopEchoErase,
        sopEchoKill,
        sopEchoNl,
        sopEnableSignals,
        sopHangUpOnClose,
        sopHardwareFlowControl,
        sopIgnoreModemLines,
        sopNoFlushAfterInterrupt
      );
      TSerialOptions = set of TSerialOption;

      TSerialInputOption = (
        sioDisableReceiver,
        sioEnableProcessing,
        sioEnableXonXoff,
        sioIgnoreBreak,
        sioIgnoreCr,
        sioIgnoreParityError,
        sioMarkParityError,
        sioParityCheck,
        sioStripOffEighthBit,
        sioTranslateCrToNl,
        sioTranslateNlToCr
      );
      TSerialInputOptions = set of TSerialInputOption;

      TSerialOutputOption = (
        sopAnyCharIsXon,
        sooEnableProcessing,
        sooEnableXonXoff,
        sooTranslateCrToNl,
        sooTranslateNlToCrLf,
        sooUseFillCharacter
      );
      TSerialOutputOptions = set of TSerialOutputOption;
  private
    procedure CreateHandle;
    procedure FreeHandle;
    procedure ConfigureConnection;
    function ReadBuffer(const Buffer: Pointer; const Count: Integer): Boolean;
    procedure WriteBuffer(const Buffer: Pointer; const Count: Integer);
    function GetLogDescription: String;
    procedure SetLogDescription(const Value: String);
  protected
    FHandle: TSHandle;
    FSerialPortSettings: ISerialPortSettings;
    FActive: Boolean;
    FSerialInputOptions: TSerialInputOptions;
    FSerialOutputOptions: TSerialOutputOptions;
    FSerialOptions: TSerialOptions;
    FLogDescription: String;
    FDebugLowLevel: Boolean;

    procedure LogBytes(const Text: String; const Data: TBytes);
    procedure CheckActive;
    procedure CheckResult(const Value: Boolean);
    procedure RaiseError(const ErrorCode: Integer; const ErrorMsg: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class procedure GetSerialPortNames(const Values: TStrings; const DeviceTypes: Array of String; const ActiveOnly: Boolean);

    procedure Connect;
    procedure Disconnect;

    procedure ConfigureSerialPort(const SerialPortSettings: ISerialPortSettings); virtual;
    function Connected: Boolean; virtual;

    procedure Write(const Data: TBytes);
    function ReadByte(out Value: Byte): Boolean; overload;
    function BytesAvailable: Integer;
    procedure Flush;
    procedure SetDebugLowLevel(const Value: Boolean);

    function ReadBytesUntil(const Terminator: Byte): TBytes; overload; virtual;
    function ReadBytesUntil(const Terminator: Byte; out Data: TBytes): Boolean; overload; virtual;
    function ReadBytes(const Count: Integer): TBytes; overload; virtual;
    function ReadBytes(const Count: Integer; out Data: TBytes): Boolean; overload; virtual;
    function ReadByte: Byte; overload;

    property Active: Boolean read FActive;
    property SerialInputOptions: TSerialInputOptions read FSerialInputOptions write FSerialInputOptions;
    property SerialOutputOptions: TSerialOutputOptions read FSerialOutputOptions write FSerialOutputOptions;
    property SerialOptions: TSerialOptions read FSerialOptions write FSerialOptions;
    property LogDescription: String read GetLogDescription write SetLogDescription;
  end;

  TSerialPortConnection = class(TBaseSerialPortConnection)
  public
    procedure WriteText(const Text: String; const Encoding: TEncoding);
    function ReadText(const Delimiter: Byte; const Encoding: TEncoding): String;
  end;

implementation

{$IFDEF LINUX64}
uses
  {$WARN UNIT_PLATFORM OFF}
  Posix.Fcntl, Posix.Unistd, Posix.StrOpts, Posix.Time, Posix.SysStat,
  Posix.Termios, Linuxapi.KernelIoctl;

const
  LibraryName = 'libc.so';

function tcsetattr(fd: Integer; optional_actions: Integer; var termios_p: termios): Integer; cdecl; external LibraryName name {_PU +} 'tcsetattr'; {$EXTERNALSYM tcsetattr}
{$ENDIF}

{$IFDEF MSWINDOWS}
uses
  WinAPI.Windows, System.Win.Registry, Winapi.MMSystem;
{$ENDIF}

resourcestring
  StrSerialPortIsNotA = 'Serial port is not active';
  StrNoTerminatorTimeou = 'No terminator timeout';
  StrUnableToConnect = 'Unable to connect';
  StrTriedToSendDByt = 'Tried to send %d bytes, but only %d bytes written';
  StrReadTimeout = 'Read timeout';
  StrWriteBufferFull = 'Write buffer full';
  StrSerialPortError = 'Serial port error: %s';
  StrSerialPortStateCh = 'Serial port state changed from %s to %s';

{ ESerialPortError }

constructor ESerialPortError.Create(const ErrorCode: Integer; const Msg: string);
begin
  inherited Create(Msg);

  FErrorCode := ErrorCode;
end;

{ TBaseSerialPortConnection }

procedure TBaseSerialPortConnection.ConfigureSerialPort(const SerialPortSettings: ISerialPortSettings);
begin
  FSerialPortSettings := SerialPortSettings;
end;

function TBaseSerialPortConnection.ReadByte: Byte;
begin
  ReadByte(Result);
end;

function TBaseSerialPortConnection.ReadBytes(const Count: Integer): TBytes;
var
  i: Integer;
  Data: Byte;
begin
  SetLength(Result, 0);

  for i := 1 to Count do
  begin
    if ReadByte(Data) then
    begin
      Result := Result + [Data];
    end;
  end;
end;

function TBaseSerialPortConnection.GetLogDescription: String;
begin
  Result := FLogDescription;
end;

procedure TBaseSerialPortConnection.LogBytes(const Text: String; const Data: TBytes);
begin
  {$IFDEF DEBUG_LOW_LEVEL}
  if FDebugLowLevel then
  begin
    TGlobalLog.Log(
      LogDescription,
      Text,
      NewParameters.
        SetParam('Bytes', BytesToOrdVals(Data, True)),
      LogSeverityDebug3);
  end;
  {$ENDIF}
end;

procedure TBaseSerialPortConnection.CheckResult(const Value: Boolean);
var
  LastError: LongWord;
  ErrorMessage: string;
begin
  if not Value then
  begin
    LastError := GetLastError;

    if LastError <> 0 then
    begin
      SetLastError(0);

      ErrorMessage := SysErrorMessage(LastError);

      if ErrorMessage = '' then
      begin
        ErrorMessage := 'Error: ' + IntToStr(LastError);
      end;

      {$IFDEF DEBUG_LOW_LEVEL}
      if FDebugLowLevel then
      begin
        TGLobalLog.Log(
          LogDescription,
          'Error - %d',
          [LastError],
          LogSeverityDebug3);
      end;
      {$ENDIF}

      RaiseError(LastError, ErrorMessage);
    end;
  end;
end;

function TBaseSerialPortConnection.ReadBytes(const Count: Integer; out Data: TBytes): Boolean;
var
  i: Integer;
  AByte: Byte;
begin
  Result := True;

  SetLength(Data, 0);

  for i := 1 to Count do
  begin
    if ReadByte(AByte) then
    begin
      Data := Data + [AByte];
    end
    else
    begin
      Result := False;

      Break;
    end;
  end;

  if Result then
  begin
    LogBytes('READ  <<<', Data);
  end;
end;

function TBaseSerialPortConnection.ReadBytesUntil(const Terminator: Byte): TBytes;
begin
  if not ReadBytesUntil(Terminator, Result) then
  begin
    raise ESerialPortError.Create(ErrorCodeTerminatorNotFoundTimeout, StrNoTerminatorTimeou);
  end;
end;

function TBaseSerialPortConnection.ReadBytesUntil(const Terminator: Byte; out Data: TBytes): Boolean;
var
  AByte: Byte;
  FinishTicks: Cardinal;
begin
  Result := True;

  SetLength(Data, 0);

  FinishTicks := TThread.GetTickCount + Cardinal(FSerialPortSettings.Timeout);

  while True do
  begin
    if TThread.GetTickCount > FinishTicks then
    begin
      LogBytes('READ TIMEOUT <<<', Data);

      Result := False;

      Break;
    end
    else
    if ReadByte(AByte) then
    begin
      Data := Data + [AByte];

      if AByte = Terminator then
      begin
        Break;
      end;
    end;
  end;

  if Result then
  begin
    LogBytes('READ  <<<', Data);
  end;
end;

procedure TBaseSerialPortConnection.SetDebugLowLevel(const Value: Boolean);
begin
  FDebugLowLevel := Value;
end;

procedure TBaseSerialPortConnection.SetLogDescription(const Value: String);
begin
  FLogDescription := Value;
end;

procedure TBaseSerialPortConnection.CheckActive;
begin
  if not Active then
  begin
    RaiseError(ErrorCodeSerialPortNotActive, StrSerialPortIsNotA);
  end;
end;

procedure TBaseSerialPortConnection.RaiseError(const ErrorCode: Integer; const ErrorMsg: string);
begin
  raise ESerialPortError.Create(ErrorCode, ErrorMsg);
end;

constructor TBaseSerialPortConnection.Create;
begin
  FLogDescription := ClassName;

  FHandle := TSHandle(-1);
end;

destructor TBaseSerialPortConnection.Destroy;
begin
  Disconnect;

  inherited;
end;

{$IFDEF LINUX64}
class procedure TBaseSerialPortConnection.GetSerialPortNames(
  const Values: TStrings; const DeviceTypes: Array of String; const ActiveOnly: Boolean);
const
  USBPath = '/dev/serial/by-id/';
var
  Files: IList<IFileInfo>;
  i, n: Integer;
  FullPath: String;
  Filename: String;
begin
  Values.Clear;

  Files := TFileUtils.ScanFiles(USBPath, '*', [TScanFileOption.IncludeFiles]);

  for i := 0 to pred(Files.Count) do
  begin
    if length(DeviceTypes) = 0 then
    begin
      Values.Add(USBPath + Files[i].Filename);
    end
    else
    begin
      Filename := Files[i].Filename;

      for n := Low(DeviceTypes) to High(DeviceTypes) do
      begin
        if pos(DeviceTypes[n], Filename) <> 0 then //= low(Files[i].Filename) then
        begin
          FullPath := USBPath + Files[i].Filename;

          if Values.IndexOf(FullPath) = -1 then
          begin
            Values.Add(FullPath);
          end;
        end;
      end;
    end;
  end;
end;

(*class procedure TBaseSerialPortConnection.GetSerialPortNames(
  const Values: TStrings; const DeviceTypes: Array of String; const ActiveOnly: Boolean);
var
  Files: IList<IFileInfo>;
  i: Integer;
  ResStrings: TStringList;
  n: Integer;
begin
  Values.Clear;

  Files := TFileUtils.ScanFiles('/dev', 'ttyUSB*', [TScanFileOption.IncludeFiles, TScanFileOption.IncludeDirectories, TScanFileOption.IncludePaths]);

  ResStrings := TStringList.Create;
  try
    for i := 0 to pred(Files.Count) do
    begin
      ResStrings.Clear;

      TFileUtils.ExecuteCommand('udevadm', 'info ' + Files[i].Filename, '', ResStrings);

      for n := 0 to pred(ResStrings.Count) do
      begin
        if (length(DeviceTypes) = 0) or
           (IsStringInStringArray(ResStrings[n], DeviceTypes)) then
        begin
          Values.Add(Files[i].Filename);
        end;
      end;
    end;
  finally
    FreeAndNil(ResStrings);
  end;

  { TODO : How do we only return the active serial ports? }
end;  *)

function BaudRateToIndex(const BaudRate: Cardinal): Integer;
const
  EncodedBaudRate: array [0..31] of Cardinal = (
    0, B0, B50, B75, B110, B134, B150, B200, B300,
    B600, B1200, B1800, B2400, B4800, B9600, B19200, B38400,
    B57600, B115200, B230400, B460800, B500000, B576000,
    B921600, B1000000, B1152000, B1500000, B2000000, B2500000,
    B3000000, B3500000, B4000000
  );
var
  BaudRateIndex: Integer;
begin
  case BaudRate of
    0: BaudRateIndex := 0;
    50: BaudRateIndex := 2;
    75: BaudRateIndex := 3;
    110: BaudRateIndex := 4;
    134: BaudRateIndex := 5;
    150: BaudRateIndex := 6;
    200: BaudRateIndex := 7;
    300: BaudRateIndex := 8;
    600: BaudRateIndex := 9;
    1200: BaudRateIndex := 10;
    1800: BaudRateIndex := 11;
    2400: BaudRateIndex := 12;
    4800: BaudRateIndex := 13;
    9600: BaudRateIndex := 14;
    19200: BaudRateIndex := 15;
    38400: BaudRateIndex := 16;
    57600: BaudRateIndex := 17;
    115200: BaudRateIndex := 18;
    230400: BaudRateIndex := 19;
    460800: BaudRateIndex := 20;
    500000: BaudRateIndex := 21;
    576000: BaudRateIndex := 22;
    921600: BaudRateIndex := 23;
    1000000: BaudRateIndex := 24;
    1152000: BaudRateIndex := 25;
    1500000: BaudRateIndex := 26;
    2000000: BaudRateIndex := 27;
    2500000: BaudRateIndex := 28;
    3000000: BaudRateIndex := 29;
    3500000: BaudRateIndex := 30;
    4000000: BaudRateIndex := 31;
  else
    begin
      BaudRateIndex := 15;
    end;
  end;

  Result := EncodedBaudRate[BaudRateIndex];
end;

function EncodeDataBits(const DataBits: Integer): Cardinal;
begin
  case Databits of
    5: Result := CS5;
    6: Result := CS6;
    7: Result := CS7;
    8: Result := CS8;
  else
    begin
      Result := 0;
    end;
  end;
end;

function EncodeParity(const Parity: TSerialPortParity): Cardinal;
begin
  case Parity of
    paOdd: Result := PARENB or PARODD;
    paEven: Result := PARENB;
  else
    begin
      Result := 0;
    end;
  end;
end;

function EncodeStopBits(const StopBits: TSerialPortStopBits): Cardinal;
begin
  case StopBits of
    ssOne: Result := CSTOPB;
  else
    begin
      Result := 0;
    end;
  end
end;

function TBaseSerialPortConnection.BytesAvailable: Integer;
begin
  CheckActive;

  CheckResult(ioctl(FHandle, FIONREAD, @Result) = 0);
end;

procedure TBaseSerialPortConnection.FreeHandle;
begin
  if FHandle <> -1 then
  begin
    try
      CheckResult(__close(FHandle) = 0);
    finally
      FHandle := TSHandle(-1);
    end;
  end;
end;

procedure TBaseSerialPortConnection.CreateHandle;
var
  Flags: Integer;
  Name: TBytes;
begin
  FreeHandle;

  SetLastError(0);
  try
    Flags := O_RDWR or O_NOCTTY;

    Flags := Flags or O_NONBLOCK;

    Name := TEncoding.UTF8.GetBytes(FSerialPortSettings.SerialPort + #0);

    FHandle := __open(@Name[0], Flags, 0);

    CheckResult(FHandle <> -1);

    FActive := True;

    Flush;

    ConfigureConnection;
  except
    FreeHandle;

    raise;
  end;
end;

procedure TBaseSerialPortConnection.ConfigureConnection;
var
  Termio: Termios;
begin
  CheckResult(tcgetattr(FHandle, Termio) = 0);

  CheckResult(cfsetispeed(Termio, BaudRateToIndex(FSerialPortSettings.BaudRate)) = 0);
  CheckResult(cfsetospeed(Termio, BaudRateToIndex(FSerialPortSettings.BaudRate)) = 0);

  Termio.c_cflag := (Termio.c_cflag and not CSIZE) or EncodeDataBits(FSerialPortSettings.DataBits);
  Termio.c_cflag := (Termio.c_cflag and not PARENB and not PARODD) or EncodeParity(FSerialPortSettings.Parity);
  Termio.c_cflag := (Termio.c_cflag and not CSTOPB) or EncodeStopBits(FSerialPortSettings.StopBits);

  Termio.c_cc[VTIME] := 0;
  Termio.c_cc[VMIN] := 0;

  Termio.c_iflag := Termio.c_iflag and not (IXANY or IXOFF or IGNBRK or IGNCR or IGNPAR or PARMRK or INPCK or ISTRIP or ICRNL or INLCR or IXON);
  Termio.c_oflag := Termio.c_oflag and not (OPOST or OCRNL or ONLCR or OFILL);
  Termio.c_lflag := Termio.c_lflag and not (ICANON or ECHO or ECHOE or ECHOK or ECHONL or ISIG or NOFLSH or IEXTEN);
  Termio.c_cflag := Termio.c_cflag and not (CLOCAL or HUPCL or CRTSCTS or CREAD);

  if not (sioDisableReceiver in FSerialInputOptions) then
    Termio.c_cflag := Termio.c_cflag or CREAD;
  if sioEnableProcessing in FSerialInputOptions then
    Termio.c_lflag := Termio.c_lflag or IEXTEN;
  if sioEnableXonXoff in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or IXOFF;
  if sioIgnoreBreak in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or IGNBRK;
  if sioIgnoreCr in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or IGNCR;
  if sioIgnoreParityError in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or IGNPAR;
  if sioMarkParityError in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or PARMRK;
  if sioParityCheck in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or INPCK;
  if sioStripOffEighthBit in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or ISTRIP;
  if sioTranslateCrToNl in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or ICRNL;
  if sioTranslateNlToCr in FSerialInputOptions then
    Termio.c_iflag := Termio.c_iflag or INLCR;
  if sooEnableProcessing in FSerialOutputOptions then
    Termio.c_oflag := Termio.c_oflag or OPOST;
  if sopAnyCharIsXon in FSerialOutputOptions then
    Termio.c_iflag := Termio.c_iflag or IXANY;
  if sooEnableXonXoff in FSerialOutputOptions then
    Termio.c_iflag := Termio.c_iflag or IXON;
  if sooTranslateCrToNl in FSerialOutputOptions then
    Termio.c_oflag := Termio.c_oflag or OCRNL;
  if sooTranslateNlToCrLf in FSerialOutputOptions then
    Termio.c_oflag := Termio.c_oflag or ONLCR;
  if sooUseFillCharacter in FSerialOutputOptions then
    Termio.c_oflag := Termio.c_oflag or OFILL;
  if sopCanonical in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or ICANON;
  if sopEcho in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or ECHO;
  if sopEchoErase in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or ECHOE;
  if sopEchoKill in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or ECHOK;
  if sopEchoNl in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or ECHONL;
  if sopEnableSignals in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or ISIG;
  if sopNoFlushAfterInterrupt in SerialOptions then
    Termio.c_lflag := Termio.c_lflag or NOFLSH;
  if sopIgnoreModemLines in SerialOptions then
    Termio.c_cflag := Termio.c_cflag or CLOCAL;
  if sopHangUpOnClose in SerialOptions then
    Termio.c_cflag := Termio.c_cflag or HUPCL;
  if sopHardwareFlowControl in SerialOptions then
    Termio.c_cflag := Termio.c_cflag or CRTSCTS;

  CheckResult(tcsetattr(FHandle, TCSANOW, Termio) = 0);
end;

procedure TBaseSerialPortConnection.Flush;
begin
  if Active then
  begin
    CheckResult(tcflush(FHandle, TCIOFLUSH) = 0);
  end;
end;

procedure TBaseSerialPortConnection.Connect;
begin
  if not Connected then
  begin
    CreateHandle;
  end;
end;

procedure TBaseSerialPortConnection.Disconnect;
begin
  if Active then
  begin
    FreeHandle;

    FActive := False;
  end;
end;

function TBaseSerialPortConnection.ReadByte(out Value: Byte): Boolean;
begin
  Result := ReadBuffer(@Value, SizeOf(Value));
end;

function TBaseSerialPortConnection.ReadBuffer(const Buffer: Pointer; const Count: Integer): Boolean;
var
  BytesRead: Integer;
  EndTime: Cardinal;
begin
  CheckActive;

  Result := False;

  if Count > 0 then
  begin
    if FSerialPortSettings.Timeout > 0 then
    begin
      EndTime := TThread.GetTickCount + Cardinal(FSerialPortSettings.Timeout);

      while BytesAvailable < Count do
      begin
        if TThread.GetTickCount > EndTime then
        begin
          RaiseError(ErrorCodeTimeout, StrReadTimeout);
        end
        else
        begin
          Sleep(1);
        end;
      end;
    end;

    BytesRead := __read(FHandle, Buffer, Count);

    Result := BytesRead > 0;
  end;
end;

procedure TBaseSerialPortConnection.WriteBuffer(const Buffer: Pointer; const Count: Integer);
var
  BytesWriten: Integer;
begin
  CheckActive;

  if Count > 0 then
  begin
    BytesWriten := __write(
      FHandle,
      Buffer,
      Count);

    CheckResult(BytesWriten <> -1);

    if BytesWriten <> Count then
    begin
      RaiseError(ErrorCodeWriteCountMismatch, format(StrTriedToSendDByt, [Count, BytesWriten]));
    end;
  end;
end;

procedure TBaseSerialPortConnection.Write(const Data: TBytes);
begin
  if Data <> nil then
  begin
    WriteBuffer(
      @Data[0],
      Length(Data));
  end;
end;

function TBaseSerialPortConnection.Connected: Boolean;
begin
  { TODO : Fix }
  Result := FActive;
end;
{$ENDIF LINUX64} // Linux64

{$IFDEF MSWINDOWS}
class procedure TBaseSerialPortConnection.GetSerialPortNames(
  const Values: TStrings; const DeviceTypes: Array of String; const ActiveOnly: Boolean);

const
  ValidSerialPortGUIDs: Array[0..0] of String = (
    '{4d36e978-e325-11ce-bfc1-08002be10318}'
  );

  procedure GetActiveSerialPortNames(const Values: TStrings);
  var
  Names: TStringList;
  i: Integer;
  Reg: TRegistry;
  begin
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      
      if Reg.OpenKeyReadOnly('\HARDWARE\DEVICEMAP\SERIALCOMM') then
      begin
        Names := TStringList.Create;
        try
          Reg.GetValueNames(Names);

          for i := 0 to Names.Count - 1 do
          begin
            if (Reg.GetDataType(Names[i]) = rdString) and
               (Values.IndexOf(Names[i]) = -1) then
            begin
              Values.Add(Reg.ReadString(Names[i]));
            end;
          end;
        finally
          FreeAndNil(Names);
        end
      end;
    finally
      FreeAndNil(Reg);
    end;
  end;

  function IsRequiredDeviceType(const DeviceType: String): Boolean;
  var
    i: Integer;
  begin
    Result := length(DeviceTypes) = 0;

    if not Result then
    begin
      for i := Low(DeviceTypes) to High(DeviceTypes) do
      begin
        if SameText(DeviceType, DeviceTypes[i]) then
        begin
          Exit(True);
        end;
      end;
    end;
  end;

const
  Key = '\SYSTEM\CurrentControlSet\Enum';
var
  i, j, k: Integer;
  KeyNames, SubKeyNames, SubSubKeyNames, ActivePorts: TStringList;
  Reg: TRegistry;
begin
  Values.Clear;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKeyReadOnly(Key) then
    begin
      KeyNames := TStringList.Create;
      SubKeyNames := TStringList.Create;
      SubSubKeyNames := TStringList.Create;
      try
        Reg.GetKeyNames(KeyNames);

        for i := 0 to KeyNames.Count - 1 do
        begin
          if IsRequiredDeviceType(KeyNames[i]) then
          begin
            if Reg.OpenKeyReadOnly(Key + '\' + KeyNames[i]) then
            begin
              SubKeyNames.Clear;

              Reg.GetKeyNames(SubKeyNames);

              for j := 0 to SubKeyNames.Count - 1 do
              begin
                if Reg.OpenKeyReadOnly(Key + '\' + KeyNames[i] + '\' + SubKeyNames[j]) then
                begin
                  SubSubKeyNames.Clear;

                  Reg.GetKeyNames(SubSubKeyNames);

                  for k := 0 to SubSubKeyNames.Count - 1 do
                  begin
                    if Reg.OpenKeyReadOnly(Key + '\' + KeyNames[i] + '\' + SubKeyNames[j] + '\' + SubSubKeyNames[k]) then
                    begin
                      if IsStringInStringArray(LowerCase(Reg.ReadString('ClassGUID')), ValidSerialPortGUIDs) then
                      begin
                        if (Reg.OpenKeyReadOnly(Key + '\' + KeyNames[i] + '\' + SubKeyNames[j] + '\' + SubSubKeyNames[k] + '\' + 'Device Parameters')) and
                           (Values.IndexOf(Reg.ReadString('PortName')) = -1) then
                        begin
                          Values.Add(Reg.ReadString('PortName'));
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        FreeAndNil(SubSubKeyNames);
        FreeAndNil(SubKeyNames);
        FreeAndNil(KeyNames);
      end;
    end;
  finally
    FreeAndNil(Reg);
  end;

  if ActiveOnly then
  begin
    ActivePorts := TStringList.Create;
    try
      GetActiveSerialPortNames(ActivePorts);

      for i := pred(Values.Count) downto 0 do
      begin
        if ActivePorts.IndexOf(Values[i]) = -1 then
        begin
          Values.Delete(i);
        end;
      end;
    finally
      FreeAndNil(ActivePorts);
    end;
  end;

  if Values is TStringList then
  begin
    TStringList(Values).Sort;
  end;
end;

procedure TBaseSerialPortConnection.CreateHandle;
const
  Prefix = '\\.\';
var
  FullPortName: String;
begin
  FullPortName := FSerialPortSettings.SerialPort;

  if pos(Prefix, FullPortName) <> 1 then
  begin
    FullPortName := Prefix + FullPortName;
  end;

  {$IFDEF DEBUG_LOW_LEVEL}
  if FDebugLowLevel then
  begin
    TGLobalLog.Log(
      LogDescription,
      'Creating serial port handle - %s',
      [FullPortName],
      LogSeverityDebug3);
  end;
  {$ENDIF}

  FHandle := CreateFile(
    PWideChar(FullPortName),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0);

  FActive := FHandle <> INVALID_HANDLE_VALUE;

  if not FACtive then
  begin
    CheckResult(False);
  end;
end;

function TBaseSerialPortConnection.Connected: Boolean;
var
  Errors: DWord;
  ComStat: TComStat;
  LastError: LongWord;
begin
  CheckActive;

  ClearCommError(
    FHandle,
    Errors,
    @ComStat);

  LastError := GetLastError;

  Result := LastError = 0;

  SetLastError(0);
end;

procedure TBaseSerialPortConnection.FreeHandle;
begin
  CloseHandle(FHandle);
end;

procedure TBaseSerialPortConnection.ConfigureConnection;
const
  RxBufferSize = 256;
  TxBufferSize = 256;
var
  DCB: TDCB;
  CommTimeouts: TCommTimeouts;
begin
  if not SetupComm(
    FHandle,
    RxBufferSize,
    TxBufferSize) then
  begin
    RaiseError(ErrorCodeSerialPortSetupFailed, 'Serial port set up failed');
  end;

  if not GetCommState(
    FHandle,
    DCB) then
  begin
    RaiseError(ErrorCodeUnableToRetrieveSerialPortState, 'Unable to retrieve serial port state');
  end;

  // change current settings
  with DCB do
  begin
    BaudRate := FSerialPortSettings.BaudRate;
    ByteSize := FSerialPortSettings.DataBits;

    case FSerialPortSettings.Parity of
      paNone: Parity := NOPARITY;
      paOdd: Parity := ODDPARITY;
      paEven: Parity := EVENPARITY;
      paMark: Parity := MARKPARITY;
      paSpace: Parity := SPACEPARITY;
    end;

    case FSerialPortSettings.StopBits of
      ssOnePointFive: StopBits := ONE5STOPBITS;
      ssTwo: StopBits := TWOSTOPBITS;
    else
      StopBits := ONESTOPBIT;
    end;

    Flags := Flags or 1;

    if FSerialPortSettings.RTSToggle then
    begin
      Flags := (Flags and not $00003000) or (RTS_CONTROL_TOGGLE shl 12);
    end;
  end;

  CheckResult(SetCommState(FHandle, DCB));

  with CommTimeouts do
  begin
    ReadIntervalTimeout := 0;
    ReadTotalTimeoutMultiplier := 0;
    ReadTotalTimeoutConstant := FSerialPortSettings.Timeout;
    WriteTotalTimeoutMultiplier := 0;
    WriteTotalTimeoutConstant := FSerialPortSettings.Timeout;
  end;

  if not SetCommTimeouts(
    FHandle,
    CommTimeouts) then
  begin
    RaiseError(ErrorCodeUnableToSetSerialPortTimeouts, 'Unable to set serial port timeouts');
  end;

  if FSerialPortSettings.DTR then
  begin
    EscapeCommFunction(FHandle, SETDTR);
  end
  else
  begin
    EscapeCommFunction(FHandle, CLRDTR);
  end;

  if FSerialPortSettings.RTS then
  begin
    EscapeCommFunction(FHandle, SETRTS);
  end
  else
  begin
    EscapeCommFunction(FHandle, CLRRTS);
  end;

  CheckResult(PurgeComm(FHandle, PURGE_RXABORT or PURGE_RXCLEAR));
  CheckResult(PurgeComm(FHandle, PURGE_TXABORT or PURGE_TXCLEAR));
  CheckResult(SetupComm(FHandle, 4096, 2048));
end;

procedure TBaseSerialPortConnection.Flush;
begin
  if Active then
  begin
    CheckResult(FlushFileBuffers(FHandle));

    if BytesAvailable > 0 then
    begin
      ReadBytes(BytesAvailable);
    end;
  end;
end;

procedure TBaseSerialPortConnection.Connect;
begin
  if not FActive then
  begin
    CreateHandle;

    ConfigureConnection;

    FActive := True;
  end;
end;

procedure TBaseSerialPortConnection.Disconnect;
begin
  if Active then
  begin
    try
      FreeHandle;
    except
      // Not much we can do
    end;

    FActive := False;
  end;
end;

function TBaseSerialPortConnection.ReadBuffer(const Buffer: Pointer; const Count: Integer): Boolean;
var
  ReadCount: LongWord;
begin
  CheckActive;

  CheckResult(
    ReadFile(
      FHandle,
      Buffer^,
      Count,
      ReadCount,
      nil));

  Result := ReadCount = LongWord(Count);
end;

function TBaseSerialPortConnection.ReadByte(out Value: Byte): Boolean;
begin
  Result := ReadBuffer(
    @Value,
    SizeOf(Value));

  if not Result then
  begin
    LogBytes('READ NO DATA AVAILABLE <<<', []);
  end;
end;

procedure TBaseSerialPortConnection.Write(const Data: TBytes);
begin
  if Data <> nil then
  begin
    LogBytes('WRITE >>>', Data);

    WriteBuffer(
      @Data[0],
      Length(Data));
  end;
end;

procedure TBaseSerialPortConnection.WriteBuffer(const Buffer: Pointer; const Count: Integer);
var
  WriteCount: LongWord;
begin
  CheckResult(WriteFile(FHandle, Buffer^, Count, WriteCount, nil));
end;

function TBaseSerialPortConnection.BytesAvailable: Integer;
var
  Errors: DWord;
  ComStat: TComStat;
begin
  CheckActive;

  CheckResult(
    ClearCommError(
      FHandle,
      Errors,
      @ComStat));

  Result := ComStat.cbInQue;
end;
{$ENDIF MSWINDOWS}


{ TSerialPortConnection }

procedure TSerialPortConnection.WriteText(const Text: String; const Encoding: TEncoding);
begin
  Write(Encoding.GetBytes(Text));
end;

function TSerialPortConnection.ReadText(const Delimiter: Byte; const Encoding: TEncoding): String;
begin
  Result := Encoding.GetString(ReadBytesUntil(Delimiter));
end;

initialization
  {$IFDEF MSWINDOWS}
  timeBeginPeriod(1);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  timeEndPeriod(1);
  {$ENDIF}

end.
