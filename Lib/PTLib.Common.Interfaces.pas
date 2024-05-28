{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Interfaces                                  }
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

unit PTLib.Common.Interfaces;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.Generics.Defaults,

  PTLib.Common.Types,
  PTLib.Common.SmartCard.Types;

type
  IParameters = interface;
  IHTTPRequest = interface;
  IHTTPResponseEx = interface;

  TTimedEventCallback = reference to procedure;
  TTimedEventParamCallback = reference to procedure(const Parameters: IParameters);
  TServerRequestCallBack = reference to procedure(const ID: String; const MessageType: String; const Parameters: IParameters; const Error: Exception);
  THTTPRequestCallback = reference to procedure(const PTLibHTTPRequestManager: TObject; const HTTPRequest: IHTTPRequest; const HTTPResponse: IHTTPResponseEx);
  TTimerProc = reference to procedure(Sender: TObject);

  IHTTPRequest = interface
    ['{18DEF019-C162-499C-9613-F746800005A2}']
    function GetID: String;
    function GetURL: String;
    procedure SetID(const Value: String);
    procedure SetURL(const Value: String);
    function GetHeaders: TNetHeaders;
    procedure SetHeaders(const Value: TNetHeaders);
    function GetEncoding: TEncoding;
    procedure SetEncoding(const Value: TEncoding);
    property ID: String read GetID write SetID;
    property URL: String read GetURL write SetURL;
    property Headers: TNetHeaders read GetHeaders write SetHeaders;
    property Encoding: TEncoding read GetEncoding write SetEncoding;
  end;

  IHTTPGet = interface(IHTTPRequest)
    ['{56A86CC9-85CF-44FF-A3F9-7A57F9B03B9D}']
  end;

  IHTTPPost = interface(IHTTPRequest)
    ['{2FF8D28C-BEE2-429D-959B-949FEC916C97}']
    function GetBody: String;
    procedure SetBody(const Value: String);
    property Body: String read GetBody write SetBody;
  end;

  IHTTPResponseEx = interface
    ['{E32A7301-9B2E-4CD4-9061-828C4B76CF5B}']
    function GetError: Exception;
    function GetHostUnavailable: Boolean;
    function GetContent: string;
    procedure SetError(const Value: Exception);
    procedure SetHostUnavailable(const Value: Boolean);
    procedure SetContent(const Value: string);
    function GetUnrecoverableError: Boolean;
    procedure SetUnrecoverableError(const Value: Boolean);
    function GetStatusCode: Integer;
    procedure SetStatusCode(const Value: Integer);
    property Content: string read GetContent write SetContent;
    property Error: Exception read GetError write SetError;
    property HostUnavailable: Boolean read GetHostUnavailable write SetHostUnavailable;
    property UnrecoverableError: Boolean read GetUnrecoverableError write SetUnrecoverableError;
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
  end;

  (*IMessageServiceItem = interface
    ['{3C7D24A5-0C9E-41A1-8288-BD4D42C8347A}']
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
  end;  *)

  IJSONSerialisable = interface
    ['{00955FB8-3308-4E97-B069-882FBF132FB0}']
    procedure ToJSON(const JSONArray: TJSONArray); overload;
    function ToJSON: TJSONArray; overload;
    function ToJSONString(const FormatJSON: Boolean = False): string;
    procedure FromJSON(const JSONArray: TJSONArray);
    procedure FromJSONString(const JSON: String);
    function GetHash(const FormatJSON: Boolean = False): String;
    function LoadFromFile(const Filename: String): Boolean;
    procedure SaveToFile(const Filename: String; const Formatted: Boolean = False);
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToStream(const AStream: TStream; const Formatted: Boolean = False);
  end;

  IList<T> = interface(IJSONSerialisable)
    // No GUID for generic interfaces
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; const Value: T);
    procedure Sort(const AComparer: IComparer<T>);
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    function Add(const Value: T): Integer;
    procedure Clear;
    function IndexOf(const Value: T): Integer;
    function Remove(Value: T): Integer;
    procedure Delete(Index: Integer);
    function DeleteByValue(const Value: T; const AComparer: IComparer<T>): Boolean;
    procedure Insert(Index: Integer; const Value: T);
    procedure Lock;
    procedure Unlock;
  end;

  IObjectList<T> = interface(IList<T>)
    function GetOwnsObjects: Boolean;
    procedure Sort(const AComparer: IComparer<T>);
    procedure SetOwnsObjects(const Value: Boolean);
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  IParameter = interface
    ['{2E1CA9CE-2AEE-4516-B734-0C237E71CA71}']
    function GetName: String;
    function GetValue: Variant;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: Variant);
    property Name: String read GetName write SetName;
    property Value: Variant read GetValue write SetValue;
    procedure Assign(const Parameter: IParameter);
  end;

  IParameters = interface(IJSONSerialisable)
    ['{067DA7E3-D419-4F00-B7C0-14B456462712}']
    function SetParam(const Name: String; const Value: Variant): IParameters;
    function GetParam(const Name: String): Variant; overload;
    function GetParam(const Name: String; const Default: Variant): Variant; overload;
    procedure DeleteParam(const Name: String);
    function ParamExists(const Name: String): Boolean;
    function Count: Integer;
    function Param(const Index: Integer): IParameter;
    function FindParam(const Name: String): IParameter;
    function SetParameters(const ParamNames: Array of String;
      const ParamValues: Array of Variant): IParameters;
    procedure Assign(const Parameters: IParameters);
    procedure Clear;
    procedure ToStrings(const Strings: TStrings);
    procedure FromStrings(const Values: TStrings);
    function ToURIParameters: String;
  end;

  IAssignableObject = interface
    ['{4F44C0EA-458D-454E-9A54-3F58B70AC7AF}']
    procedure Assign(const AObject: TObject);
    procedure AssignTo(const AObject: TObject);
  end;

  IUsageTimeStatistic = interface
    ['{F1A2A022-76B6-4B2A-81D5-7CA35FB76A03}']
    function GetAverageDuration: Single;
    function GetCount: Integer;
    function GetHighDuration: Single;
    function GetHighTimeStamp: TDateTime;
    function GetLowDuration: Single;
    function GetLowTimeStamp: TDateTime;
    function GetTotalDuration: Single;
    procedure SetCount(const Value: Integer);
    procedure SetHighDuration(const Value: Single);
    procedure SetHighTimeStamp(const Value: TDateTime);
    procedure SetLowDuration(const Value: Single);
    procedure SetLowTimeStamp(const Value: TDateTime);
    procedure SetTotalDuration(const Value: Single);
    function GetID: String;
    procedure SetID(const Value: String);
    property LowTimeStamp: TDateTime read GetLowTimeStamp write SetLowTimeStamp;
    property HighTimeStamp: TDateTime read GetHighTimeStamp write SetHighTimeStamp;
    property LowDuration: Single read GetLowDuration write SetLowDuration;
    property HighDuration: Single read GetHighDuration write SetHighDuration;
    property TotalDuration: Single read GetTotalDuration write SetTotalDuration;
    property Count: Integer read GetCount write SetCount;
    property ID: String read GetID write SetID;
    property AverageDuration: Single read GetAverageDuration;
  end;

  IFileInfo = interface
    ['{813C34D2-A7FD-4351-BBE6-74781458B076}']
    function GetCreatedDate: TDateTime;
    function GetFilename: UnicodeString;
    function GetFilesize: Int64;
    function GetIsDir: Boolean;
    function GetModifiedDate: TDateTime;
    procedure SetCreatedDate(const Value: TDateTime);
    procedure SetFilename(const Value: UnicodeString);
    procedure SetFilesize(const Value: Int64);
    procedure SetIsDir(const Value: Boolean);
    procedure SetModifiedDate(const Value: TDateTime);
    property Filename: UnicodeString read GetFilename write SetFilename;
    property Filesize: Int64 read GetFilesize write SetFilesize;
    property ModifiedDate: TDateTime read GetModifiedDate write SetModifiedDate;
    property CreatedDate: TDateTime read GetCreatedDate write SetCreatedDate;
    property IsDir: Boolean read GetIsDir write SetIsDir;
  end;

  ILogEntry = interface
    ['{6747CA3E-7ADF-4EBC-9E43-32DA215A238E}']
    function GetLogText: String;
    function GetLogType: String;
    function GetSeverity: TLogSeverity;
    function GetTimeStampUTC: TDateTime;
    procedure SetLogText(const Value: String);
    procedure SetLogType(const Value: String);
    procedure SetSeverity(const Value: TLogSeverity);
    procedure SetTimeStampUTC(const Value: TDateTime);
    function GetGeneratedUTC: TDateTime;
    procedure SetGeneratedUTC(const Value: TDateTime);
    function GetParameters: IParameters;
    function HasParameters: Boolean;
    function GetSender: TObject;
    function GetSenderName: String;
    procedure SetSender(const Value: TObject);
    procedure SetSenderName(const Value: String);
    function GetSenderDescription: String;
    function FullLogText: String;
    function LogTextWithParams(const MaxParamLength: Integer = 500): String;
    property LogType: String read GetLogType write SetLogType;
    property LogText: String read GetLogText write SetLogText;
    property Severity: TLogSeverity read GetSeverity write SetSeverity;
    property TimeStampUTC: TDateTime read GetTimeStampUTC write SetTimeStampUTC;
    property GeneratedUTC: TDateTime read GetGeneratedUTC write SetGeneratedUTC;
    property Parameters: IParameters read GetParameters;
    property Sender: TObject read GetSender write SetSender;
    property SenderName: String read GetSenderName write SetSenderName;
  end;

  IRemoteLogEntry = interface(ILogEntry)
    ['{27CAE2A8-EB07-4928-A3BA-39AC6D4CD2F1}']
    function GetClientName: String;
    procedure SetClientName(const Value: String);
    property ClientName: String read GetClientName write SetClientName;
  end;

  ILog = interface
    ['{BFA908E4-89A1-48D6-AB8E-EEF5CB6EFB14}']
    procedure Log(const LogText: String; const LogType: String = ''; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Args: Array of const; const Parameters: IParameters; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Args: Array of const; const LogType: String = ''; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0; const GeneratedUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const LogType: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime; const GeneratedUTC: TDateTime; const Parameters: IParameters); overload;
    Procedure Log(const LogEntry: ILogEntry); overload;
    function GetLogHistoryItemCount: Integer;
    function GetLogHistoryItem(const Index: Integer): ILogEntry;
    function GetLogHistory(const TailEntries: Integer = 0): IList<ILogEntry>; overload;
    function GetLogHistory(const TimeStamp: TDateTime): IList<ILogEntry>; overload;
    procedure ClearHistory;
    procedure SaveToStream(const AStream: TStream; const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]);
    procedure SaveToFile(const Filename: String; const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]);
    function SaveToString(const UseTabs: Boolean = True;
      const LogFormatOptions: TLogFormatOptions = [lfCreatedTimestamp, lfIncludeMilliSeconds, lfLogType, lfLogText]): String;
    function GetLogHistoryCount: Integer;
    procedure SetLogHistoryCount(const Value: Integer);
  end;

  IClassDescriptor = interface
    ['{C4AABA14-8C89-4901-B3CB-C20DA6503032}']
    procedure GetClassDescriptor(out Value: String);
  end;

  IFileLogEntry = interface(ILogEntry)
    ['{61DBC416-5D7A-4F5B-922B-1B7346C1FF7C}']
    function GetFilenameOverride: String;
    procedure SetFilenameOverride(const Value: String);
    property FilenameOverride: String read GetFilenameOverride write SetFilenameOverride;
  end;

  IConfiguration = interface
    ['{A895FAFA-48AB-4A0E-845D-14BAA3E491ED}']
    procedure WriteString(const SectionPath, ValueName, Value: String);
    procedure WriteEncodedString(const SectionPath, ValueName, Value: String);
    procedure WriteInteger(const SectionPath, ValueName: String; const Value: Integer);
    procedure WriteBoolean(const SectionPath, ValueName: String; const Value: Boolean);
    procedure WriteStringList(const SectionPath, ValueName: String; const Values: TStrings);
    procedure WriteEnumeratedType(const SectionPath, ValueName: String; const EnumInfo: PTypeInfo; const EnumParam: Integer);
    procedure WriteSet(const SectionPath, ValueName: String; const SetInfo: PTypeInfo; const SetParam);
    procedure WriteStream(const SectionPath, ValueName: String; const Stream: TStream);
    function ReadString(const SectionPath, ValueName, DefaultValue: String): String;
    function ReadEncodedString(const SectionPath, ValueName, DefaultValue: String): String;
    function ReadInteger(const SectionPath, ValueName: String; const DefaultValue: Integer): Integer;
    function ReadBoolean(const SectionPath, ValueName: String; const DefaultValue: Boolean): Boolean;
    procedure ReadStringList(const SectionPath, ValueName: String; const Values: TStrings);
    function ReadEnumeratedType(const SectionPath, ValueName: String; EnumInfo: PTypeInfo): Integer;
    procedure ReadSet(const SectionPath, ValueName: String; const SetInfo: PTypeInfo; var SetParam);
    function ReadStream(const SectionPath, ValueName: String; const Stream: TStream): Boolean;
    function DeleteSection(const SectionPath: String): Boolean;
    function DeleteValue(const SectionPath: String; ValueName: String): Boolean;
    function SectionPathExists(const SectionPath: String): Boolean;
    function ValueNameExists(const SectionPath, ValueName: String): Boolean;
    procedure GetValueNames(const SectionPath: String; const ValueNames: TStrings);
    procedure GetSectionNames(const SectionPath: String; const SectionNames: TStrings; IncludePath: Boolean);
    function ToText: String;
    procedure FromText(Value: String);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(Filename: String);
    procedure LoadFromFile(Filename: String);
    function AppendSection(const RootSection, AdditionalPath: String): String;
    procedure SetBaseSectionPath(const Value: String);
    function GetBaseSectionPath: String;
    procedure SetSectionDelimiter(const Value: Char);
    function GetSectionDelimiter: Char;
    procedure SetCreateSectionPaths(const Value: Boolean);
    function GetCreateSectionPaths: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetRaiseExceptions(const Value: Boolean);
    function GetRaiseExceptions: Boolean;
    procedure ReadComponents(const BaseComponent: TComponent; const SectionPath: String);
    procedure WriteComponents(const BaseComponent: TComponent; const SectionPath: String);
  end;

  IConfigurationProvider = interface
    ['{95F0F875-47E1-48D3-A792-78F949590A74}']
    procedure SaveToConfig(const Configuration: IConfiguration; SectionPath: String);
    procedure LoadFromConfig(const Configuration: IConfiguration; SectionPath: String);
  end;

  ICommandLine = interface
  ['{32F575E6-A873-4F6E-BB0E-BF8A7D4D0AF9}']
    function Clear: ICommandLine;
    function SetCommandLine(const CommandLine: String): ICommandLine;
    function GetCommand: String;
    function SetCommand(const Command: String): ICommandLine;
    function AddCommandLine: ICommandLine;
    function Append(CommandLine: ICommandLine): ICommandLine;
    function CommandIs(const Command: String): Boolean;
    function HasCommand: Boolean;
    function GetIndex: Integer;
    function SetIndex(Index: Integer): ICommandLine;
    function SetParam(const ParamName: String; const ParamValue: Variant): ICommandLine; overload;
    function SetParam(const ParamName: String): ICommandLine; overload;
    function Text: String;
    function CommandLineText(const UseTokens: Boolean = False): String; overload;
    function CommandLineText(const Index: Integer; const UseTokens: Boolean = False): String; overload;
    function ParamExists(const ParamName: String): Boolean;
    function GetParam(const ParamName: String; RaiseException: Boolean = True): Variant;
    function DeleteParam(const ParamName: String): ICommandLine;
    function ParamCount: Integer;
    function First: ICommandLine;
    function Next: ICommandLine;
    function Previous: ICommandLine;
    function Last: ICommandLine;
    function Count: Integer;
    function Eof: Boolean;
    function SetSwitch(const SwitchChar: Char): ICommandLine;

    function TokenExists(const TokenIndex: Integer): Boolean; overload;
    function TokenExists(const TokenName: String): Boolean; overload;
    function GetToken(const TokenIndex: Integer; const IsInteger: Boolean = False;
      const MinIntegerValue: Integer = 0; const MaxIntegerValue: Integer = MaxInt): Variant;
    procedure RemoveTokensBefore(const TokenIndex: Integer);
    function GetTokenString(const TokenIndex: Integer; const Default: String): String;
    function GetTokenValue(const TokenName: String; const IsInteger: Boolean = False;
      const MinIntegerValue: Int64 = 0; const MaxIntegerValue: Int64 = MaxLongInt): Variant;
    function TokenIs(const TokenIndex: Integer; const TokenValue: String): Boolean;
    function TokenCount: Integer;
  end;

  IInformationList = interface
    ['{0CDFEBC2-325A-49E9-AE4C-60476A2E53E5}']
    function GetPropertySeparator: String;
    procedure SetPropertySeparator(const Value: String);
    function GetHeadingPostFix: String;
    function GetHeadingPrefix: String;
    procedure SetHeadingPostFix(const Value: String);
    procedure SetHeadingPrefix(const Value: String);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    function Clear: IInformationList;
    function AddHeading(const Text: String; const MetaData: String = ''): IInformationList;
    function AddProperty(const PropertyName: String; const PropertyValue: Variant; const EmptyValue: String = 'None'; const MetaData: String = ''): IInformationList; overload;
    function AddProperty(const PropertyName: String; const PropertyValue: String; const Args: Array of const; const MetaData: String = ''): IInformationList; overload;
    function AddLine(const LineText: String; const MetaData: String = ''): IInformationList; overload;
    function AddLine(const LineText: String; const Args: Array of const; const MetaData: String = ''): IInformationList; overload;
    function AppendInformation(const Information: IInformationList; const LinePrefix: String = ''): IInformationList;
    property PropertySeparator: String read GetPropertySeparator write SetPropertySeparator;
    property HeadingPrefix: String read GetHeadingPrefix write SetHeadingPrefix;
    property HeadingPostFix: String read GetHeadingPostFix write SetHeadingPostFix;
    property Lines: TStrings read GetLines write SetLines;
    function GetProperty(const Index: Integer): String;
    function GetValue(const Index: Integer): String;
    function GetMetaData(const Index: Integer): String;
  end;

  ITimedEvents = interface
    ['{63DCD57B-CD89-4D92-B118-287A6D4659B6}']
    procedure AddEvent(const EventCallBack: TTimedEventCallback; const DelayMS: Cardinal;
      const Sender: TObject = nil; const EventName: String = ''; const AutoRestart: Boolean = False); overload;
    procedure AddEvent(const EventParamCallBack: TTimedEventParamCallback; const DelayMS: Cardinal;
        const Parameters: IParameters; const Sender: TObject = nil; const EventName: String = '';
        const AutoRestart: Boolean = False); overload;
    procedure AddEvent(const EventCallBack: TTimedEventParamCallback; const DelayMS: Cardinal;
      const ParameterNames: Array of String; const ParameterValues: Array of Variant; const Sender: TObject = nil;
      const EventName: String = ''; const AutoRestart: Boolean = False); overload;
    function CancelEvent(const Sender: TObject; const EventName: String = ''): Boolean;
    function EventExists(const Sender: TObject; const EventName: String = ''): Boolean;
    function RestartEvent(const Sender: TObject; const EventName: String = ''): Boolean;
    function GetInformation(const PropertySeparator: String = '='): IInformationList;
    function GetEventTimeUTC(const Sender: TObject; const EventName: String = ''): TDateTime;
  end;

  IInformationProvider = interface
    ['{67251F00-0596-4EF5-8222-BBBB274AE48F}']
    function GetInformation(const PropertySeparator: String = '='): IInformationList;
  end;

  IPTLibLocaliser = interface
    ['{AA46E847-8546-4949-88CD-A53DDBA7F1E3}']
    function FindTranslation(const TranslationID, LanguageID, InstallationID: String; var Translation: String): Boolean;
    function LocaliseText(const TranslationID, LanguageID, InstallationID: String; const ParamNames: Array of String; const ParamValues: Array of Variant; var Translation: String): Boolean; overload;
    function LocaliseText(const TranslationID, LanguageID, InstallationID: String; const Params: IParameters; var Translation: String): Boolean; overload;
    procedure GetSupportedLanguageIDs(const Values: TStrings; const InstallationID: String = '');
  end;

  IDataHistory<T> = interface
    function GetSampleCount: Integer;
    procedure SetSampleCount(const Value: Integer);
    function Samples: IList<T>;
    procedure AddSample(const Value: T);
    property SampleCount: Integer read GetSampleCount write SetSampleCount;
  end;

  IPriorityQueueItem = interface(IJSONSerialisable)
    ['{1B667622-ADD1-4F47-AF6F-D7DF634DB045}']
    function GetID: String;
    function GetQueuedUTC: TDateTime;
    function GetSentUTC: TDateTime;
    function GetRetry: Integer;
    function GetStatus: TPriorityQueueMessageStatus;
    function GetQueueName: String;
    function GetExpiresUTC: TDateTime;
    function GetRetryAtURT: TDateTime;
    procedure SetID(const Value: String);
    procedure SetQueuedUTC(const Value: TDateTime);
    procedure SetSentUTC(const Value: TDateTime);
    procedure SetStatus(const Value: TPriorityQueueMessageStatus);
    procedure SetQueueName(const Value: String);
    procedure SetRetry(const Value: Integer);
    procedure SetExpiresUTC(const Value: TDateTime);
    procedure SetRetryAtUTC(const Value: TDateTime);
    function GetDescription: String;
    property ID: String read GetID write SetID;
    property QueuedUTC: TDateTime read GetQueuedUTC write SetQueuedUTC;
    property SentUTC: TDateTime read GetSentUTC write SetSentUTC;
    property Status: TPriorityQueueMessageStatus read GetStatus write SetStatus;
    property QueueName: String read GetQueueName write SetQueueName;
    property Retry: Integer read GetRetry write SetRetry;
    property RetryAtUTC: TDateTime read GetRetryAtURT write SetRetryAtUTC;
    property ExpiresUTC: TDateTime read GetExpiresUTC write SetExpiresUTC;
  end;

  IPriorityQueueDataItem = interface(IPriorityQueueItem)
    ['{48611797-EBD7-4641-8CF4-9EB7A0ABB2A8}']
    function GetData: String;
    procedure SetData(const Value: String);
    property Data: String read GetData write SetData;
  end;

  IMapAPI = interface
    ['{7B1C677A-775E-44DF-8F9A-2601164E26E8}']
    function GeoCode(const Address: String; out Latitude, Longitude: Double): Integer;
    function ReverseGeoCode(const Latitude, Longitude: Double; out Address: String): Integer;
    function Elevation(const Latitude, Longitude: Double; out Elevation: Integer): Integer;
  end;

  IEmail = interface
    ['{6768FB96-011D-4FE6-8C88-0F7CD18F3E35}']

  end;

  ITextFilter = interface
    ['{E95908E6-5A76-4040-A861-D85CD91BFA89}']
    procedure SetFilter(const Value: String);
    function GetFilter: String;
    function IsMatch(const Value: String; const CaseSensitive: Boolean = False; const UseFilterPrefixes: Boolean = False;
      const ExactMatch: Boolean = False; const MatchAll: Boolean = False): Boolean;
    property Filter: String read GetFilter write SetFilter;
  end;

  ISerialPortSettings = interface
    ['{8CB6ECD1-AC16-4714-9C50-3B00DA65FD01}']
    function GetBaudRate: Integer;
    function GetSerialPort: String;
    function GetDataBits: Integer;
    function GetHardFlow: Boolean;
    function GetMaxResponseLength: Integer;
    function GetParity: TSerialPortParity;
    function GetRTSToggle: Boolean;
    function GetSoftFlow: Boolean;
    function GetStopBits: TSerialPortStopBits;
    function GetTimeout: Integer;
    procedure SetBaudRate(const Value: Integer);
    procedure SetSerialPort(const Value: String);
    procedure SetDataBits(const Value: Integer);
    procedure SetHardFlow(const Value: Boolean);
    procedure SetMaxResponseLength(const Value: Integer);
    procedure SetParity(const Value: TSerialPortParity);
    procedure SetRTSToggle(const Value: Boolean);
    procedure SetSoftFlow(const Value: Boolean);
    procedure SetStopBits(const Value: TSerialPortStopBits);
    procedure SetTimeout(const Value: Integer);
    function GetDTR: Boolean;
    function GetRTS: Boolean;
    procedure SetDTR(const Value: Boolean);
    procedure SetRTS(const Value: Boolean);
    property SerialPort: String read GetSerialPort write SetSerialPort;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property BaudRate: Integer read GetBaudRate write SetBaudRate;
    property DataBits: Integer read GetDataBits write SetDataBits;
    property Parity: TSerialPortParity read GetParity write SetParity;
    property StopBits: TSerialPortStopBits read GetStopBits write SetStopBits;
    property SoftFlow: Boolean read GetSoftFlow write SetSoftFlow;
    property HardFlow: Boolean read GetHardFlow write SetHardFlow;
    property MaxResponseLength: Integer read GetMaxResponseLength write SetMaxResponseLength;
    property RTSToggle: Boolean read GetRTSToggle write SetRTSToggle;
    property RTS: Boolean read GetRTS write SetRTS;
    property DTR: Boolean read GetDTR write SetDTR;
  end;

  ICircularBuffer<T> = interface
    ['{FE1C2322-3847-4F45-B78D-0D32686AED03}']
    procedure Write(const Value: T; const AllowOverwrite: Boolean = False); overload;
    procedure Write(const Values: TArray<T>; const AllowOverwrite: Boolean = False); overload;
    function Read: T; overload;
    function Read(out Value: T): Boolean; overload;
    function Read(const Count: Integer): TArray<T>; overload;
    function Read(const Count: Integer; out Values: TArray<T>): Boolean; overload;
    procedure Clear;
    function Size: Integer;
  end;

  ICircularByteBuffer = interface(ICircularBuffer<Byte>)
  ['{84D3C5C9-1D1F-4A53-BF4B-9619DAC238A5}']
    function Read(const UntilDelimiter: Byte; out Values: TBytes): Boolean; overload;
  end;

  IPTLibSmartCard = interface(IInformationProvider)
    ['{F6CE8A5A-AA70-4C59-917F-0146AB2995E6}']
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetCardUID: String;
    function GetLastUID: String;
    function GetLastRFIDChangeMs: Cardinal;
    function GetConnected: Boolean;
    function GetReverseUID: Boolean;
    procedure ResetLastRFIDChange;
    procedure SetReverseUID(const Value: Boolean);
    procedure SetCardUID(const UID: String);
    property Active: Boolean read GetActive write SetActive;
    property CardUID: String read GetCardUID;
    property LastCardUID: String read GetLastUID;
    property Connected: Boolean read GetConnected;
    property ReverseUID: Boolean read GetReverseUID write SetReverseUID;
    property LastRFIDChangeMs: Cardinal read GetLastRFIDChangeMs;
  end;

  IPTLibSmartCardPCSC = interface(IPTLibSmartCard)
    ['{97ED8090-EA3F-4240-9E67-885BB65A8D86}']
    function GetCardData: TCardData;
    function GetReaderList: TArray<String>;
    procedure ConnectCard(const ReaderName: String);
    procedure DisconnectCard;
    procedure CheckState(const StateChanged: TStateChangedProc = nil; const ReadTimeout: Integer = 1000);
    procedure CheckStateASync(const StateChanged: TStateChangedProc; const OnException: TProc<Exception>);
    function GetReaderStates: TSmartCardReaderStates;
    function GetReaderStateDescription: String;
    function GetReaderName: String;
    function GetResponseFromCard(const APDU: TBytes; out Response: TBytes): Boolean;
    function CardConnected: Boolean;
    function ReaderConnected: Boolean;
    property CardData: TCardData read GetCardData;
  end;

  IPTLibSmartCardSerial = interface(IPTLibSmartCard)
    ['{573796E8-BDC9-49C8-859B-D7EDBD7BDD40}']
    function GetSerialPortSettings: ISerialPortSettings;
    property SerialPortSettings: ISerialPortSettings read GetSerialPortSettings;
  end;

  IPTLibSmartCardFileData = interface(IPTLibSmartCard)
    ['{5B75647F-3D19-4B42-A5D9-AD01B80F23C1}']
  end;

  IPTLibActiveInterfacedObject = interface(IInformationProvider)
    ['{3569773A-D089-4797-9FF2-B565A1CA0B9B}']
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    property Active: Boolean read GetActive write SetActive;
  end;

  IPTLibActiveParamsInterfacedObject = interface(IPTLibActiveInterfacedObject)
    ['{C6A0E5C5-9F43-4569-903E-115AD8D57827}']
    function GetParam(const Name: String; const Default: Variant): Variant;
    procedure SetParam(const Name: String; const Value: Variant);
  end;

  IPTLibGlobalTimer = interface
    ['{06744F85-AB6C-4663-9D50-6E153F05362A}']
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    function GetName: String;
    function GetEnabled: Boolean;
    function GetInterval: Cardinal;
    function GetOnTimer: TTimerProc;
    function GetLastMS: Int64;
    procedure SetOnTimer(const Value: TTimerProc);
    property Name: String read GetName;
    property OnTimer: TTimerProc read GetOnTimer write SetOnTimer;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property LastMS: Int64 read GetLastMS;
  end;

  ISerialPortASync = interface(IPTLibActiveParamsInterfacedObject)
    ['{75AD9193-FA51-484A-A8D9-608B180F547E}']
    function GetSerialPortState: TSerialPortState;
    function GetSerialPortOptions: TSerialPortOptions;
    function GetSerialPortSettings: ISerialPortSettings;
    procedure SetOnStateChange(const Value: TNotifyEvent);
    function GetLastConnectionError: String;
    function GetOnStateChange: TNotifyEvent;
    procedure Reset;
    function CompatibleDeviceDetected: Boolean;
    property State: TSerialPortState read GetSerialPortState;
    property SerialPortOptions: TSerialPortOptions read GetSerialPortOptions;
    property SerialPortSettings: ISerialPortSettings read GetSerialPortSettings;
    property OnStateChange: TNotifyEvent read GetOnStateChange write SetOnStateChange;
  end;

  ISerialPortASyncBuffered = interface(ISerialPortASync)
    ['{DE281CF6-CC1E-4A1A-9C65-293A9D313DB8}']
    procedure FlushWriteBuffer;
    procedure FlushReadBuffer;
    function GetReadBuffer: ICircularByteBuffer;
    function GetPauseWrites: Boolean;
    procedure SetPauseWrites(const Value: Boolean);
    procedure Flush;
    procedure Write(const Data: TBytes; const FrontOfQueue: Boolean = False);
    function GetWriteData(out Data: TBytes): Boolean;
    property ReadBuffer: ICircularByteBuffer read GetReadBuffer;
    function GetWriteQueueCount: Integer;
    function GetPacketsWritten: Cardinal;
    function GetBytesWritten: Cardinal;
    function GetBytesRead: Cardinal;
    property PauseWrites: Boolean read GetPauseWrites write SetPauseWrites;
  end;

  IKeyValueStore = interface
    ['{9B98ED63-8288-4E14-9F9E-17D5C47441B9}']
    function GetBoolValueInternal(Index: String): Boolean;
    function GetDateValueInternal(Index: String): TDateTime;
    function GetFloatValueInternal(Index: String): Double;
    function GetIntValueInternal(Index: String): Int64;
    function GetStringValueInternal(Index: String): String;
    procedure SetStringValue(Index: String; const Value: String); overload;
    procedure SetBoolValue(Index: String; const Value: Boolean);
    procedure SetDateValue(Index: String; const Value: TDateTime);
    procedure SetFloatValue(Index: String; const Value: Double);
    procedure SetIntValue(Index: String; const Value: Int64);
    procedure IncIntValue(const Index: String; const Value: Int64 = 1);
    procedure DecIntValue(const Index: String; const Value: Int64 = 1);
    function GetStringValue(const Index: String): String; overload;
    function GetStringValue(const Index: String; const Default: String): String; overload;
    function GetStringValue(const Index: String; out Value: String; const Default: String): Boolean; overload;
    function GetBoolValue(const Index: String): Boolean; overload;
    function GetBoolValue(const Index: String; const Default: Boolean): Boolean; overload;
    function GetBoolValue(const Index: String; out Value: Boolean; const Default: Boolean): Boolean; overload;
    function GetDateValue(const Index: String): TDateTime; overload;
    function GetDateValue(const Index: String; const Default: TDateTime): TDateTime; overload;
    function GetDateValue(const Index: String; out Value: TDateTime; const Default: TDateTime): Boolean; overload;
    function GetFloatValue(const Index: String): Double; overload;
    function GetFloatValue(const Index: String; const Default: Double): Double; overload;
    function GetFloatValue(const Index: String; out Value: Double; const Default: Double): Boolean; overload;
    function GetIntValue(const Index: String): Int64; overload;
    function GetIntValue(const Index: String; const Default: Int64): Int64; overload;
    function GetIntValue(const Index: String; out Value: Int64; const Default: Int64): Boolean; overload;
    function ValueExists(const Name: String): Boolean;
    procedure RemoveValue(const Name: String);
    property S[Index: String]: String read GetStringValueInternal write SetStringValue;
    property I[Index: String]: Int64 read GetIntValueInternal write SetIntValue;
    property B[Index: String]: Boolean read GetBoolValueInternal write SetBoolValue;
    property D[Index: String]: TDateTime read GetDateValueInternal write SetDateValue;
    property F[Index: String]: Double read GetFloatValueInternal write SetFloatValue;
  end;

  IKeyValueStoreEnvironmentVariables = interface(IKeyValueStore)
    ['{12A14AB8-C208-47BE-BFB1-131989BCF572}']
  end;

  IKeyValueStoreMemory = interface(IKeyValueStore)
    ['{115238A3-3830-47A2-B2E9-86D44437DE3F}']
    function AsText(const Filter: String = ''; const Sort: Boolean = True): String;
    procedure Clear(const Filter: String = '');
  end;

  IServerContainerService = interface(IPTLibActiveInterfacedObject)
    ['{FC5325B8-2900-4218-9723-FC4DD6FA8CE0}']
    function GetServiceName: String;
  end;

  IServerContainer = interface
    ['{9D0F5267-0242-47E9-921C-8D8A9187DF7D}']
    function StartService(const AService: IServerContainerService): Boolean;
    function StopService(const AService: IServerContainerService): Boolean;
    procedure StartServices;
    procedure StopServices;
  end;

implementation

end.







