unit PTLib.Common.SmartCard.Types;

interface

uses
  System.SysUtils;

type
  TCardUIDChanged = reference to procedure(const CardUID: String);

  TSmartCardReaderState = (
    crsUnaware,
    crsIgnore,
    crsChanged,
    crsUnknown,
    crsUnavailable,
    crsEmpty,
    crsPresent,
    crsATMatch,
    crsExclusive,
    crsInUse,
    crsMute,
    crsUnpowered
  );
  TSmartCardReaderStates = set of TSmartCardReaderState;

  TStateChangedProc = reference to procedure(const State: TSmartCardReaderStates; const Data: TBytes);

  TSmartCardIDType = (
    sctUnknown,
    sct4ByteID,
    sct7ByteID
  );

  TSerialSmartCardType = (
    sstSpringcard,
    sstGemProx
  );

  TCardData = record
    ATR: TBytes;
    CardType: String;
    Protocol: String;
    UID: String;
  end;

const
  SmartCardReaderStateDescriptions: Array[TSmartCardReaderState] of String = (
    'Unaware',
    'Ignore',
    'Changed',
    'Unknown',
    'Unavailable',
    'Empty',
    'Present',
    'ATMatch',
    'Exclusive',
    'InUse',
    'Mute',
    'Unpowered'
  );

  SmartCardIDTypeDescriptions: Array[TSmartCardIDType] of String = (
    'Unknown',
    '4 Byte ID',
    '7 Byte ID'
  );

  SerialSmartCardTypeDescriptions: Array[TSerialSmartCardType] of String = (
    'Springcard',
    'GemProx'
  );

function SmartCardProtocolToString(const Protocol: Integer): String;
function SmartCardIDToName(const ID: String): String;
function SmartCardReaderStatesToDescription(const Value: TSmartCardReaderStates): String;

implementation

uses
  PTLib.Common.Strings;

const
  SmartCardNames: Array[0..38, 0..1] of String = (
    ('0001', 'NXP Mifare Standard 1k'),
    ('0002', 'NXP Mifare Standard 4k'),
    ('0003', 'NXP Mifare UltraLight, Other Type 2 NFC Tags(NFC Forum) with a capacity <= 64 bytes'),
    ('0006', 'ST Micro Electronics SR176'),
    ('0007', 'ST Micro Electronics SRI4K, SRIX4K, SRIX512, SRI512, SRT512'),
    ('000A', 'Atmel AT88SC0808CRF'),
    ('000B', 'Atmel AT88SC1616CRF'),
    ('000C', 'Atmel AT88SC3216CRF'),
    ('000D', 'Atmel AT88SC6416CRF'),
    ('0012', 'Texas Instruments TAG IT'),
    ('0013', 'ST Micro Electronics LRI512'),
    ('0014', 'NXP ICODE SLI'),
    ('0015', 'NXP ICODE1'),
    ('0021', 'ST Micro Electronics LRI64'),
    ('0024', 'ST Micro Electronics LR12'),
    ('0025', 'ST Micro Electronics LRI128'),
    ('0026', 'NXP Mifare Mini'),
    ('002F', 'Innovision Jewel'),
    ('0030', 'Innovision Topaz (NFC Forum type 1 tag)'),
    ('0034', 'Atmel AT88RF04C'),
    ('0035', 'NXP ICODE SL2'),
    ('003A', 'NXP Mifare UltraLight C. Other Type 2 NFC Tags(NFC Forum) with a capacity > 64 bytes'),
    ('FFA0', 'Generic/unknown 14443-A card'),
    ('FFA1', 'Kovio RF barcode'),
    ('FFB0', 'Generic/unknown 14443-B card'),
    ('FFB1', 'ASK CTS 256B'),
    ('FFB2', 'ASK CTS 512B'),
    ('FFB3', 'Pre-standard ST Micro Electronics SRI 4K'),
    ('FFB4', 'Pre-standard ST Micro Electronics SRI X512'),
    ('FFB5', 'Pre-standard ST Micro Electronics SRI 512'),
    ('FFB6', 'Pre-standard ST Micro Electronics SRT 512'),
    ('FFB7', 'Inside Contactless PICOTAG/PICOPASS'),
    ('FFB8', 'Generic Atmel AT88SC / AT88RF card'),
    ('FFC0', 'Calypso card using the Innovatron protoco'),
    ('FFD0', 'Generic ISO 15693 from unknown manufacturer'),
    ('FFD1', 'Generic ISO 15693 from EM Marin (or Legic)'),
    ('FFD2', 'Generic ISO 15693 from ST Micro Electronics, block number on 8 bits'),
    ('FFD3', 'Generic ISO 15693 from ST Micro Electronics, block number on 16'),
    ('FFFF', 'Virtual card (test only)')
  );

function SmartCardReaderStatesToDescription(const Value: TSmartCardReaderStates): String;
var
  i: TSmartCardReaderState;
begin
  Result := '';

  for i := Low(TSmartCardReaderState) to High(TSmartCardReaderState) do
  begin
    if i in Value then
    begin
      AddToken(Result, SmartCardReaderStateDescriptions[i], ', ');
    end;
  end;
end;

function SmartCardProtocolToString(const Protocol: Integer): String;
begin
  case Protocol of
    1: Result := 'ISO 14443 A, level 1';
    2: Result := 'ISO 14443 A, level 2';
    3: Result := 'ISO 14443 A, level 3 or 4 (and Mifare)';
    5: Result := 'ISO 14443 B, level 1';
    6: Result := 'ISO 14443 B, level 2';
    7: Result := 'ISO 14443 B, level 3 or 4';
    9: Result := 'ICODE 1';
    11: Result := '"ISO 15693';
  else
    Result := 'No information given';
  end;
end;

function SmartCardIDToName(const ID: String): String;
var
  i: Integer;
begin
  Result := 'Unknown Card';

  for i := Low(SmartCardNames) to High(SmartCardNames) do
  begin
    if SmartCardNames[i][0] = ID then
    begin
      Result := SmartCardNames[i][1];

      Break;
    end;
  end;
end;

end.
