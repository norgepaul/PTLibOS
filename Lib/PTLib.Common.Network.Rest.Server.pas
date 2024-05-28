{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network.Rest.Server                         }
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

unit PTLib.Common.Network.Rest.Server;

interface

uses
  System.SysUtils, System.Classes, System.Syncobjs, System.Generics.Collections,

  System.NetEncoding, System.JSON, IdHTTP, idURI, idContext, IdGlobal,
  IdCustomHTTPServer, IdHTTPServer, IdSSLOpenSSL, IdServerIOHandler, IdSSL,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSocketHandle, IdCustomTCPServer,
  IdTCPConnection, IdYarn,

  PTLib.Common.Network.Rest.Messages,
  PTLib.Common.Classes,
  PTLib.Common.Dates,
  PTLib.Common.LockedTypes,
  PTLib.Common.Log,
  PTLib.Common.Log.LocalFile,
  PTLib.Common.Strings,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,
  PTLib.Common.Files;

type
  // Forward declarations
  TIdRestSchemaMethod = class;
  TPTLibIdRestServer = class;
  TIdRestSchemaService = class;
  TIdRestSchemaMethods = class;
  TIdRestSchemaMethodClass = class of TIdRestSchemaMethod;

  // Exception types
  EIdRestServer = class(Exception);
  EIdRestServices = class(Exception);
  EIdRestService = class(Exception);
  EIdRestMethods = class(Exception);
  EIdRestMethod = class(Exception);
  EIdRestExecute = class(Exception);
  EIdRestSchemaValidation = class(Exception);
  EIdRestSchemaGeneration = class(Exception);

  TIdRestSchemaParameterType = (rpUnknown = 0, // Undefined
    rpBool, // Boolean
    rpNumber, // Number [int|float]
    rpText, // String
    rpTimestamp);

  // We only support a low number of attachment types
  TIdRestAttachmentType = (raUnknown = 0, // Non supported attachment types
    raURL, // application/x-www-form-urlencoded <default>
    raJSON, // "application/json"
    raMultipart); // "multipart/form-data"

  // HTTP access call-types for service methods
  TIdRestSchemaMethodAccess = (rcUnknown = 0, rcGET, // HTTP-GET    <URI only>
    rcPOST, // HTTP-POST   <attachment>
    rcPUT, // HTTP-PUT    <attachment>
    rcDELETE); // HTTP-DELETE <attachment>

  TCustomRestServerContext = class(TIdServerContext)
  private
    function GetContextIdentifier: string;
  protected
    FMethodName: string;
    FTimestamp: TDateTime;
    FExceptionOccured: Boolean;
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList = nil); override;

    procedure SetExceptionOccured;
    procedure SetMethodName(const Name: string);
    function GetMethodName: String;

    property ContextIdentifier: string read GetContextIdentifier;
    property Timestamp: TDateTime read FTimestamp;
  end;

  (* About ConnectionInfo:
    =====================
    When a rest method is invoked, information about the current service
    and method is passed as a parameter (TPTRestCallData).
    TPTRestConnectionInfo is a property of TPTRestCallData, and it exists
    to provide information about the current HTTP request.
    It also exposes the context object, which is of special interest to
    customized rest servers. *)
  TPTRestConnectionInfo = class(TObject)
  strict private
    FContext: TCustomRestServerContext;
    FRequest: TIdHTTPRequestInfo;
    FResponse: TIdHTTPResponseInfo;
  strict protected
    function GetContext: TCustomRestServerContext; virtual;
    procedure SetContext(Value: TCustomRestServerContext); virtual;

    function GetRequest: TIdHTTPRequestInfo; virtual;
    procedure SetRequest(Value: TIdHTTPRequestInfo); virtual;

    function GetResponse: TIdHTTPResponseInfo; virtual;
    procedure SetResponse(Value: TIdHTTPResponseInfo); virtual;
  public
    property Context: TCustomRestServerContext read GetContext write SetContext;
    property Request: TIdHTTPRequestInfo read GetRequest write SetRequest;
    property Response: TIdHTTPResponseInfo read GetResponse write SetResponse;
  end;

  (* About TPTRestCallData:
    ======================
    When a rest method is invoked, an instance of TPTRestCallData is provided
    as a parameter. This object exposes more or less everything a method
    needs to know in order to execute properly.

    Custom REST servers can also implement their own call-data classes
    if they need more information.

    You can set the call-data class by calling the server:

    FServer.InstallCallData(YourCallDataClassType);

    Please note that this must be called BEFORE you start the server! *)
  TPTRestCallData = class(TObject)
  strict private
    FService: TIdRestSchemaService;
    FMethod: TIdRestSchemaMethod;
    FSocketInfo: TPTRestConnectionInfo;
    FParams: IParameters;
    FLog: ILog;
    FAttachment: TStream;
  strict protected
    function GetService: TIdRestSchemaService; virtual;
    procedure SetService(Value: TIdRestSchemaService); virtual;

    function GetMethodObj: TIdRestSchemaMethod; virtual;
    procedure SetMethodObj(Value: TIdRestSchemaMethod); virtual;

    function GetSocketInfo: TPTRestConnectionInfo; virtual;
    procedure SetSocketInfo(Value: TPTRestConnectionInfo); virtual;

    function GetParams: IParameters; virtual;
    procedure SetParams(Value: IParameters); virtual;

    function GetLog: ILog; virtual;
    procedure SetLog(Value: ILog); virtual;

    function GetAttachment: TStream; virtual;
    procedure SetAttachment(Value: TStream); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Service: TIdRestSchemaService read GetService write SetService;
    property Method: TIdRestSchemaMethod read GetMethodObj write SetMethodObj;
    property SocketInfo: TPTRestConnectionInfo read GetSocketInfo
      write SetSocketInfo;
    property Params: IParameters read GetParams write SetParams;
    property Log: ILog read GetLog write SetLog;
    property Attachment: TStream read GetAttachment write SetAttachment;
  end;

  // This is used when installing a custom call-data class in the server
  TPTRestCallDataClass = class of TPTRestCallData;

  IIdRestElementSchema = interface
    ['{CA36927C-7125-4D4F-8E6D-0A7B23F3E837}']
    procedure BuildElementSchema(Buffer: TPTLibTabbedStringList);
    function GetElementSchema: String;
  end;

  IIdRestMethodSchema = interface(IIdRestElementSchema)
    ['{4352861F-7C88-40FA-AFB3-9DBF335BA5D0}']
    procedure Qualify(Params: TDictionary<String, String>);
  end;

  IIdRestMethods = interface
    ['{3D44D879-1E95-4BC5-B313-281A9CA1C757}']
    function RegisterMethod(RestMethod: TIdRestSchemaMethod)
      : TIdRestSchemaMethod;
    function GetMethodByName(RestMethodName: String;
      out RestMethod: TIdRestSchemaMethod): Boolean;
  end;

  IIdRestServer = interface
    ['{A158776B-69A9-4932-A8B5-E637B9BCB7C5}']
    procedure RegisterService(Service: TIdRestSchemaService);
    procedure UnRegisterService(Service: TIdRestSchemaService);
    procedure UnRegisterServiceByName(ServiceName: String);
    procedure EnableService(ServiceName: String; Value: Boolean);
    function GetServiceEnabled(ServiceName: String): Boolean;
    function GetServiceByName(ServiceName: String; out Service: TIdRestSchemaService; const CreateService: Boolean): Boolean;
    function GetServerLog: ILog;
  end;

  IIdRestMethodDispatch = interface
    ['{358BD2D2-3C30-40FF-BCFB-E1996DAB2483}']
    procedure Execute(const Info: TPTRestCallData);
  end;

  // Anonymous callback handler for TIdRestSchemaMethod
  TIdRestExecuteEntry = reference to procedure(const Info: TPTRestCallData);

  // Callback handler for ordinary "event" like methods
  TIdRestExecuteEntryEvent = procedure(const Info: TPTRestCallData) of object;

  (* TIdRestSchemaParameter:
    =======================
    This class represents a single parameter defined for a call.
    All rest services auto-generates a JSON service schema which clients
    can use to invoke the services correctly.

    Note: The server is expected to validate calls based on the
    schema for the method. *)
  TIdRestSchemaParameter = class(TObject)
  strict private
    FName: TPTLockedValue<String>;
    FKind: TPTLockedValue<TIdRestSchemaParameterType>;
    FDefault: String;
  public
    constructor Create; virtual;
    destructor Destroy; Override;

    property Name: TPTLockedValue<String> read FName;
    property DataType: TPTLockedValue<TIdRestSchemaParameterType> read FKind;
    property &Default: String read FDefault write FDefault;
  end;

  (* TIdRestSchemaParameters:
    ========================
    This is a thread safe collection class for schema parameters.
    Since schemas are generated "on the fly" whenever a browser or client
    performs a name-space request all aspects must be made thread safe *)

  TIdRestSchemaParameters = class(TPTLockedObjectList)
  strict private
    FParent: TPTLockedValue<TIdRestSchemaMethod>;
  public
    constructor Create(AOwner: TIdRestSchemaMethod); reintroduce;
    destructor Destroy; Override;

    function Contains(Instance: TIdRestSchemaMethod): Boolean; reintroduce;
    function Lock: TObjectList<TIdRestSchemaParameter>; reintroduce;

    function ObjectOf(const RestParamName: String): TIdRestSchemaParameter;

    function Add(RestParamObj: TIdRestSchemaParameter)
      : TIdRestSchemaParameter; overload;
    function Add(Name: String; DataType: TIdRestSchemaParameterType;
      DefaultValue: String = ''): TIdRestSchemaParameter; overload;

    procedure Delete(RestParamName: String); overload;
    procedure Delete(RestParamObj: TIdRestSchemaParameter); overload;
  public
    property Parent: TPTLockedValue<TIdRestSchemaMethod> read FParent;
  end;

  TIdRestServerMethodAuthenticateEvent = procedure(sender: TIdRestSchemaMethod;
    Info: TPTRestCallData; var Allow: Boolean) of object;

  (* TIdRestSchemaMethod:
    ==============
    This is the baseclass for all server-side, invokable methods.
    As of writing we support essentially only two variations:
    1- GET
    2- POST

    However the REST protocol can be expanded with PUT and DELETE
    commands, representing storage and removal in it's simplest forms. *)
  TIdRestSchemaMethod = class(TInterfacedPersistent, IIdRestMethodSchema,
    IIdRestMethodDispatch)
  strict private
    FAttachment: TPTLockedValue<Boolean>;
    FAttachType: TPTLockedValue<TIdRestAttachmentType>;
    FMethods: TIdRestSchemaMethods;
    FService: TIdRestSchemaService;
    FName: string;
    FParams: TIdRestSchemaParameters;
    FFluent: Boolean;

    FAuthorizationRequired: Boolean;
    FOnAuth: TIdRestServerMethodAuthenticateEvent;

    FHandler: TIdRestExecuteEntry; // Anonymous method version
    FHandler2: TIdRestExecuteEntryEvent; // Old-school event based callback

  strict protected
    FAccess: TIdRestSchemaMethodAccess;

    { IMPLEMENTS: IidRestElementSchema }
    procedure AddAttachmentSchemaInfo(Buffer: TPTLibTabbedStringList);
      virtual; abstract;
    procedure BuildElementSchema(Buffer: TPTLibTabbedStringList);
    function GetElementSchema: String;
    procedure Qualify(Params: TDictionary<String, String>);

  strict protected
    function DoAuthenticate(const Info: TPTRestCallData): Boolean; virtual;
  strict protected
    // IMPLEMENTS: IIdRestMethodDispatch
    procedure Execute(const Info: TPTRestCallData); virtual;

  public
    constructor Create(AOwner: TIdRestSchemaService; const MethodName: string;
      const AuthorizationRequired: Boolean = False); virtual;
    destructor Destroy; override;

    // Use this method to define a handler for the service-method
    procedure HandleWith(Entrypoint: TIdRestExecuteEntry); overload;
    procedure HandleWith(ObjectEntryPoint: TIdRestExecuteEntryEvent); overload;

    property Fluent: Boolean read FFluent write FFluent;
    property Schema: String read GetElementSchema;
    property Parameters: TIdRestSchemaParameters read FParams;
    property RestService: TIdRestSchemaService read FService;
    property Parent: TIdRestSchemaMethods read FMethods;
    property RestMethodName: string read FName;
    property Access: TIdRestSchemaMethodAccess read FAccess;
    property Attachment: TPTLockedValue<Boolean> read FAttachment;
    property AttachmentType: TPTLockedValue<TIdRestAttachmentType> read FAttachType;

    property AuthorizationRequired: Boolean read FAuthorizationRequired;
    property OnAuthenticate: TIdRestServerMethodAuthenticateEvent read FOnAuth
      write FOnAuth;
  end;

  // Get version of a server-side method
  TIdRestSchemaMethodGet = class(TIdRestSchemaMethod)
  strict protected
    procedure AddAttachmentSchemaInfo(Buffer: TPTLibTabbedStringList); override;
  end;

  // Post version. This class supports attachments
  TIdRestSchemaMethodPost = class(TIdRestSchemaMethod)
  strict protected
    procedure AddAttachmentSchemaInfo(Buffer: TPTLibTabbedStringList); override;
  public
    constructor Create(AOwner: TIdRestSchemaService; const MethodName: string;
      const AuthorizationRequired: Boolean = False); override;
  end;

  // Method collection, thread safe
  TIdRestSchemaMethods = class(TPTLockedObjectList, IIdRestMethods,
    IIdRestElementSchema)
  strict private
    FParent: TIdRestSchemaService;
  strict private
    { IMPLEMENTS: IIdRestMethods }
    function GetMethodByName(RestMethodName: String;
      out RestMethod: TIdRestSchemaMethod): Boolean;
    function RegisterMethod(RestMethod: TIdRestSchemaMethod)
      : TIdRestSchemaMethod;
  strict protected
    procedure BuildElementSchema(Buffer: TPTLibTabbedStringList); virtual;
    function GetElementSchema: String; virtual;
  public
    constructor Create(AOwner: TIdRestSchemaService); reintroduce; virtual;

    function Lock: TObjectList<TIdRestSchemaMethod>; reintroduce;
    function Contains(RestMethod: TIdRestSchemaMethod): Boolean;
      reintroduce; overload;
    function Contains(RestMethodName: String): Boolean; reintroduce; overload;
    procedure Execute(Info: TPTRestCallData);

    property Parent: TIdRestSchemaService read FParent;
  end;

  (* REST Service class:
    ===================
    Each service class contains X number of members which can be invoked
    over HTTP. Much like Remobjects exposes services, our framework does
    more or less the same thing.

    The URI scheme for services are:

    http://HostName[:port]/REST/ServiceClass/MethodClass

    The /REST/ keyword is fundamental here as it tells the server that
    this is indeed a REST invocation. All other URL's are handled by Indy's
    default handler *)
  TIdRestSchemaService = class(TInterfacedPersistent, IIdRestElementSchema)
  strict private
    FParent: TPTLibIdRestServer;
    FName: string;
    FEncoding: TPTLockedValue<String>;
    FMethods: TIdRestSchemaMethods;
    FEnabled: TPTLockedValue<Boolean>;
  strict protected
    procedure BuildElementSchema(Buffer: TPTLibTabbedStringList); virtual;
    function GetElementSchema: String; virtual;
  public
    constructor Create(AOwner: TPTLibIdRestServer; const Name: string); virtual;
    destructor Destroy; override;

    property Enabled: TPTLockedValue<Boolean> read FEnabled;
    property ServiceName: string read FName;
    property Encoding: TPTLockedValue<String> read FEncoding;
    property Parent: TPTLibIdRestServer read FParent;
    property Methods: TIdRestSchemaMethods read FMethods;
    property Schema: String read GetElementSchema;
  end;

  TIdRestSchemaServices = class(TPTLockedObjectList, IIdRestElementSchema)
  strict private
    FParent: TPTLibIdRestServer;
  strict protected
    // IMPLEMENTS:  IIdRestElementSchema
    procedure BuildElementSchema(Buffer: TPTLibTabbedStringList); virtual;
    function GetElementSchema: String; virtual;
  public
    constructor Create(AOwner: TPTLibIdRestServer); reintroduce; virtual;

    function ObjectOf(const ServiceName: String): TIdRestSchemaService;
    function Contains(Instance: TIdRestSchemaService): Boolean; reintroduce;
    function Lock: TObjectList<TIdRestSchemaService>; reintroduce;
  public
    property Parent: TPTLibIdRestServer read FParent;
  end;

  TIdRestServerLog = Class(TBasePTLibLoggerComponent)
  end;

  TPtLibIdRestServerAuthenticateEvent = procedure(sender: TObject;
    UserName: String; Password: String; var Allow: Boolean) of object;

  TPTLibIdRestServer = class(TIdCustomHTTPServer, IIdRestServer,
    IIdRestElementSchema)
  strict private
    FLock: TCriticalSection;
    FServices: TIdRestSchemaServices;
    FRestRootURI: TPTLockedValue<String>;
    FServeSchemas: TPTLockedValue<Boolean>;
    FLogComponent: TIdRestServerLog;
    FServeHtmlErrors: TPTLockedValue<Boolean>;
    FAuthenticate: TPtLibIdRestServerAuthenticateEvent;
    FCallDataClass: TPTRestCallDataClass;
    FSSLHandler: TIdServerIOHandlerSSLOpenSSL;
    FHandlerPassword: String;
    FSSLEnabled: Boolean;
    FSSLRootCertificateFile: String;
    FSSLCertificateFile: String;
    FSSLKeyFile: String;
    FSSLCertificatePassword: String;
  strict private
    { IMPLEMENTS: IIdRestServer }
    procedure RegisterService(Service: TIdRestSchemaService);
    procedure UnRegisterService(Service: TIdRestSchemaService);
    procedure UnRegisterServiceByName(ServiceName: String);
    procedure EnableService(ServiceName: String; Value: Boolean);

    function GetServerLog: ILog;

    function ConvertParameters(const Params: TDictionary<string, string>;
      const Method: TIdRestSchemaMethod): IParameters;

    procedure SetupSSL;
  strict protected
    function DoQuerySSLPort(APort: TIdPort): Boolean; override;
    procedure SetActive(AValue: Boolean); override;

    { IMPLEMENTS: IIdRestElementSchema }
    procedure BuildElementSchema(Buffer: TPTLibTabbedStringList); virtual;
    function GetElementSchema: String; virtual;
  strict protected
    function GetHtmlErrors: Boolean;
    procedure SetHtmlErrors(Value: Boolean);

    function GetServeSchema: Boolean;
    procedure SetServeSchema(Value: Boolean);

    function GetLogComponent: TIdRestServerLog; virtual;
    procedure SetLogComponent(Value: TIdRestServerLog); virtual;

    function GetServiceEnabled(ServiceName: String): Boolean;
    function GetServiceByName(ServiceName: String; out Service: TIdRestSchemaService; const CreateService: Boolean): Boolean;

    function GetWorkingThreadCount: Integer;

    { Implements: SSL methods }
    procedure SSLGetPassword(var Password: String);
    function SSLVerifyPeer(Certificate: TIdX509; AOk: Boolean;
      ADepth, AError: Integer): Boolean;

    procedure HandleAuthentication(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean);
  protected
    (* Overrides for INDY, Notes:
      ==========================

      1. The server retains 100% ordinary Indy behavior. The REST dispatching
      will only occur if the path begins with "/REST/", all other URI's
      are treated by Indy's standard behavior.

      2. ServeSchema and ServeHtmlErrors are meant for debugging and
      mimic'ing RemObjects RODL exposure. With a slight modification we
      can detect if the client is actually a browser rather than a REST
      or SOAP client and only issue HTML-errors when a browser is involved.

      3. The DoRestCommand method deals with both GET and POST messages. We
      have overriden DoCommandGet and DoCommandOther and check for /REST/
      namespace in the document path -- if the call does not involve
      rest services, we allow ordinary indy behavior to take over.
      But the whole REST handling is isolated in DoRestCommand()
    *)
    procedure DoCommandError(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      AException: Exception); override;

    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;

    procedure DoCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); override;

    procedure DoRestCommand(Uri: TIdURI; AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    procedure DoConfigureRestServices; virtual; abstract;
    procedure DoTooManyConnections(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo); virtual;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;

    procedure InstallCallData(CallDataClass: TPTRestCallDataClass);

    property Schema: String read GetElementSchema;
    property Logging: TIdRestServerLog read FLogComponent;
  published
    property OnCommandGet;
    property OnAuthenticate: TPtLibIdRestServerAuthenticateEvent
      read FAuthenticate write FAuthenticate;

    property ServeHtmlErrors: Boolean read GetHtmlErrors write SetHtmlErrors
      default true;
    property ServeSchema: Boolean read GetServeSchema write SetServeSchema
      default true;

    property SSLEnabled: Boolean read FSSLEnabled write FSSLEnabled;
    property SSLRootCertificateFile: String read FSSLRootCertificateFile write FSSLRootCertificateFile;
    property SSLCertificateFile: String read FSSLCertificateFile write FSSLCertificateFile;
    property SSLKeyFile: String read FSSLKeyFile write FSSLKeyFile;
    property SSLCertificatePassword: String read FSSLCertificatePassword write FSSLCertificatePassword;

    property WorkingThreadCount: Integer read GetWorkingThreadCount;
  end;

function DQuoteStr(Value: String): String;

implementation

var
  __ACCESS_STR: Array [TIdRestSchemaMethodAccess] of String = (
    'unknown',
    'get',
    'post',
    'put',
    'delete'
  );

  __PARTYP_STR: Array [TIdRestSchemaParameterType] of String = (
    'unknown',
    'bool',
    'number',
    'text',
    'timestamp'
  );

  __ATTACH_TYPE: Array [TIdRestAttachmentType] of String = (
    'text/unknown',
    'application/x-www-form-urlencoded',
    'application/json',
    'multipart/form-data'
  );

resourcestring
  CNT_ERR_INVOC_Maelformed =
    'Invalid REST invocation, maelformed service reference error [%s]';
  CNT_ERR_INVOC_Unknown_Service =
    'Invalid REST invocation, unknown service [%s] error';
  CNT_ERR_INVOC_Service_disabled =
    'Invalid REST invocation, service [%s] is disabled error';
  CNT_ERR_INVOC_Service_Method_NotExposed =
    'Invalid REST invocation, service "%s" does not expose method %s() error';
  CNT_ERR_INVOC_Internal_Error = 'REST invocation failed, internal error [%s]';
  CNT_ERR_INVOC_Method_Protocol =
    'Invalid REST invocation, wrong accessor mode (expected [HTTP-%s]) for %s() error';

  CNT_ERR_INVOC_Execute =
    'Invocation of REST method failed, system threw exception [%s] with message [%s]';
  CNT_ERR_INVOC_Execute_Invalid_Info =
    'Invocation of REST method failed, internal dispatch record maelformed error';
  CNT_ERR_INVOC_Execute_Error =
    'REST invoke for %s.%s() failed, system threw exception %s with message [%s] error';

  CNT_ERR_INVOC_Attachment_Wrong =
    'Invocation of REST method failed, non matching attachment type, expected [%s] not [%s]';
  CNT_ERR_INVOC_Attachment_Missing =
    'Invocation of REST method failed, attachment missing, expected [%s] stream error';

  CNT_ERR_SCHEMA_FailedGenerate =
    'Failed to construct schema from %s, system threw exception %s with message [%s]';
  CNT_ERR_SERVER_FailedRegisterService_NIL =
    'Failed to register service, reference was NIL error';

function AttachmentTypeToContentType(AType: TIdRestAttachmentType): String;
begin
  result := __ATTACH_TYPE[AType];
end;

Function ContentTypeToAttachmentType(AType: String): TIdRestAttachmentType;
begin
  AType := lowercase(trim(AType));
  result := raUnknown;

  if AType.Length > 0 then
  begin
    if AType.Equals('application/json') then
    begin
      result := raJSON;
    end
    else if AType.Equals('x-www-form-urlencoded') then
    begin
      result := raURL;
    end
    else if AType.Equals('multipart/form-data') then
    begin
      result := raMultipart;
    end;
  end;
end;

function ParameterDataTypeToStr(DataType: TIdRestSchemaParameterType): String;
begin
  result := __PARTYP_STR[DataType];
end;

Function AccessMethodToStr(Access: TIdRestSchemaMethodAccess): String;
begin
  result := __ACCESS_STR[Access];
end;

function HTTPCommandToRestMethodAccess(Value: String)
  : TIdRestSchemaMethodAccess;
begin
  Value := Value.ToLower.trim;
  if Value.Equals('get') then
  begin
    result := rcGET;
  end
  else if Value.Equals('post') then
  begin
    result := rcPOST;
  end
  else if Value.Equals('put') then
  begin
    result := rcPUT;
  end
  else if Value.Equals('delete') then
  begin
    result := rcDELETE;
  end
  else
  begin
    result := rcUnknown;
  end;
end;

function DQuoteStr(Value: String): String;
Begin
  result := '"' + Value + '"';
end;

function JSONPair(Name, Value: String; Last: Boolean = true): String; overload;
begin
  result := DQuoteStr(Name) + ': ' + DQuoteStr(Value);
  if not Last then
  begin
    result := result + ',';
  end;
end;

function JSONPair(Name: String): String; overload;
Begin
  result := DQuoteStr(Name) + ': ';
end;

// ############################################################################
// TPTLibIdRestServer
// ############################################################################

function TPTLibIdRestServer.ConvertParameters(const Params
  : TDictionary<string, string>; const Method: TIdRestSchemaMethod)
  : IParameters;
var
  Param: TPair<string, string>;
  ParamInfo: TIdRestSchemaParameter;
begin
  result := TParameters.Create;

  for Param in Params do
  begin
    ParamInfo := Method.Parameters.ObjectOf(Param.Key);

    if Assigned(ParamInfo) then
    begin

      case ParamInfo.DataType.Value of
        rpUnknown:
          begin
            // This should never happen, the type should be clearly defined
            // by the code creating the service/method layer
          end;
        rpBool:
          begin
            result.SetParam(Param.Key, StrToBool(Param.Value));
          end;
        rpNumber:
          begin
            result.SetParam(Param.Key, StrToInt(Param.Value));
          end;
        rpText:
          begin
            result.SetParam(Param.Key, Param.Value);
          end;
        rpTimestamp:
          begin
            result.SetParam(Param.Key,
              CommonDateTimeStrToDateTime(Param.Value));
          end;
      end;

    end;
  end;
end;

constructor TPTLibIdRestServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ContextClass := TCustomRestServerContext;

  FLock := TCriticalSection.Create;

  AutoStartSession := False;
  SessionState := true;

  FRestRootURI := TPTLockedValue<String>.Create;
  FRestRootURI.Value := 'REST';

  FServeHtmlErrors := TPTLockedValue<Boolean>.Create;
  FServeSchemas := TPTLockedValue<Boolean>.Create;

  FServices := TIdRestSchemaServices.Create(self);
  SetLogComponent(TIdRestServerLog.Create(self));

  FCallDataClass := TPTRestCallData;

  DoConfigureRestServices;
end;

destructor TPTLibIdRestServer.Destroy;
begin
  FSSLHandler.Free;

  FLogComponent.Free;
  FRestRootURI.Free;
  FServeHtmlErrors.Free;
  FServeSchemas.Free;
  FServices.Free;

  FLock.Free;
  inherited;
end;

procedure TPTLibIdRestServer.InstallCallData(CallDataClass
  : TPTRestCallDataClass);
begin
  if not Active then
  begin
    FCallDataClass := CallDataClass;
  end
  else
  begin
    Raise Exception.Create
      ('Server calldata cannot be changed while the server is active error');
  end;
end;

procedure TPTLibIdRestServer.SetupSSL;
var
  LHandle: TIdSocketHandle;
begin
  if FSSLEnabled then
  begin
    if not Assigned(FSSLHandler) then
    begin
      FSSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(Self);
    end;

    self.IOHandler := FSSLHandler;

    // Setup the SSL handler
    FSSLHandler.SSLOptions.RootCertFile := FSSLRootCertificateFile;
    FSSLHandler.SSLOptions.CertFile := FSSLCertificateFile;
    FSSLHandler.SSLOptions.KeyFile := FSSLKeyFile;
    FSSLHandler.SSLOptions.Method := sslvTLSv1_2;
    FSSLHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    FSSLHandler.OnGetPassword := SSLGetPassword;
    FSSLHandler.OnVerifyPeer := SSLVerifyPeer;

    // See "SSLGetPassword" for the use of this
    FHandlerPassword := FSSLCertificatePassword;

    // flush whatever bindings may be there
    Bindings.Clear;

    // HTTP port (normally 80)
    LHandle := Bindings.Add;
    LHandle.Port := DefaultPort;
    //LHandle.IP := '';         ??
    //LHandle.Bind;             ??
  end;
end;

function TPTLibIdRestServer.SSLVerifyPeer(Certificate: TIdX509; AOk: Boolean;
  ADepth, AError: Integer): Boolean;
begin
  result := true;
end;

procedure TPTLibIdRestServer.SSLGetPassword(var Password: String);
begin
  Password := FHandlerPassword;
end;

procedure TPTLibIdRestServer.HandleAuthentication(AContext: TIdContext;
  const AAuthType, AAuthData: String; var VUsername, VPassword: String;
  var VHandled: Boolean);
begin
end;

procedure TPTLibIdRestServer.BuildElementSchema(Buffer: TPTLibTabbedStringList);
var
  IAccess: IIdRestElementSchema;
begin
  Buffer.BeginSection;
  try
    Buffer.Add('{');
    Buffer.BeginSection;
    try

      Buffer.Add(JSONPair('server', ServerSoftware, False));
      Buffer.Add(JSONPair('server-time', DateTimeToCommonDateTimeStr(now,
        true), False));
      Buffer.Add(DQuoteStr('services') + ': [');

      if FServices.GetInterface(IIdRestElementSchema, IAccess) then
      begin
        IAccess.BuildElementSchema(Buffer);
      end;

      Buffer.Add(']');
    finally
      Buffer.EndSection;
    end;
  finally
    Buffer.Add('}');
    Buffer.EndSection;
  end;
end;

function TPTLibIdRestServer.GetElementSchema: String;
var
  LBuffer: TPTLibTabbedStringList;
begin
  LBuffer := TPTLibTabbedStringList.Create;
  try

    try
      BuildElementSchema(LBuffer);
    except
      on e: Exception do
      begin
        raise EIdRestSchemaGeneration.CreateFmt(CNT_ERR_SCHEMA_FailedGenerate,
          [ClassName, e.ClassName, e.Message]);
      end;
    end;

    result := LBuffer.Text;

  finally
    LBuffer.Free;
  end;
end;

function TPTLibIdRestServer.GetHtmlErrors: Boolean;
begin
  result := FServeHtmlErrors.Value;
end;

procedure TPTLibIdRestServer.SetActive(AValue: Boolean);
begin
  if AValue then
    SetupSSL;

  inherited;
end;

procedure TPTLibIdRestServer.SetHtmlErrors(Value: Boolean);
begin
  FServeHtmlErrors.Value := Value;
end;

function TPTLibIdRestServer.GetServeSchema: Boolean;
begin
  result := FServeSchemas.Value;
end;

procedure TPTLibIdRestServer.SetServeSchema(Value: Boolean);
begin
  FServeSchemas.Value := Value;
end;

function TPTLibIdRestServer.GetLogComponent: TIdRestServerLog;
begin
  FLock.Enter;
  try
    if FLogComponent = NIL then
    begin
      FLogComponent := TIdRestServerLog.Create(NIL);
    end;

    result := FLogComponent;

  finally
    FLock.Leave;
  end;
end;

procedure TPTLibIdRestServer.SetLogComponent(Value: TIdRestServerLog);
begin
  FLock.Enter;
  try
    if (Value <> FLogComponent) then
    begin

      if Assigned(FLogComponent) then
      begin
        FLogComponent.RemoveFreeNotification(self);
        FLogComponent := NIL;
      end;

      if Assigned(Value) then
      begin
        FLogComponent := Value;
        FLogComponent.FreeNotification(self);
      end;

    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPTLibIdRestServer.EnableService(ServiceName: String; Value: Boolean);
var
  mService: TIdRestSchemaService;
begin
  if GetServiceByName(ServiceName, mService, False) then
  Begin
    mService.Enabled.Value := Value;
  end;
end;

function TPTLibIdRestServer.GetServiceEnabled(ServiceName: String): Boolean;
var
  mService: TIdRestSchemaService;
begin
  result := False;
  if GetServiceByName(ServiceName, mService, False) then
  Begin
    result := mService.Enabled.Value;
  end;
end;

function TPTLibIdRestServer.GetWorkingThreadCount: Integer;
begin
  Result := Contexts.Count;
end;

Procedure TPTLibIdRestServer.UnRegisterServiceByName(ServiceName: String);
var
  mService: TIdRestSchemaService;
begin
  ServiceName := ServiceName.ToLower.trim;
  if ServiceName.Length > 0 then
  begin

    FLock.Enter;
    try
      mService := FServices.ObjectOf(ServiceName)
    finally
      FLock.Leave;
    end;

    if mService <> nil then
    begin
      UnRegisterService(mService);
    end;

  end;
end;

procedure TPTLibIdRestServer.UnRegisterService(Service: TIdRestSchemaService);
var
  mIndex: Integer;
  mList: TObjectList<TIdRestSchemaService>;
begin
  if Service <> nil then
  begin

    mList := FServices.Lock;
    try
      mIndex := mList.IndexOf(Service);
      if (mIndex >= 0) then
      begin
        mList.Delete(mIndex);
      end;
    finally
      FServices.UnLock;
    end;

  end;
end;

Procedure TPTLibIdRestServer.RegisterService(Service: TIdRestSchemaService);
var
  mList: TObjectList<TIdRestSchemaService>;
begin
  if Service <> nil then
  begin

    mList := FServices.Lock;
    try
      if not mList.Contains(Service) then
      Begin
        mList.Add(Service);
      end;
    finally
      FServices.UnLock;
    end;

  end
  else
  begin
    Raise EIdRestServer.Create(CNT_ERR_SERVER_FailedRegisterService_NIL);
  end;
end;

function TPTLibIdRestServer.GetServerLog: ILog;
begin
  result := NIL;

  FLock.Enter;
  try

    if Assigned(FLogComponent) then
    Begin
      FLogComponent.GetInterface(ILog, result)
    end;

  finally
    FLock.Leave;
  end;
end;

function TPTLibIdRestServer.GetServiceByName(ServiceName: String;
  out Service: TIdRestSchemaService; const CreateService: Boolean): Boolean;
begin
  Service := FServices.ObjectOf(ServiceName);

  if (Service = nil) and
     (CreateService) then
  begin
    Service := TIdRestSchemaService.Create(Self, ServiceName);
  end;

  Result := Service <> nil;
end;

procedure TPTLibIdRestServer.DoCommandError(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  AException: Exception);
var
  mHtml: TStringlist;
  mJSONError: TIdRestErrorMessage;
begin
  TCustomRestServerContext(AContext).SetExceptionOccured;

  // First issue to log if available   // Removed by Paul 14/10/2016
  //FLock.Enter;
  //try
  if Assigned(FLogComponent) then
    FLogComponent.Log(AException.Message, LogSeverityDebug3, now);
  //finally
  //  FLock.Leave;
  //end;

  // Not a rest operation? Ok, execute "normal" HTTP behavior
  if not ARequestInfo.Uri.ToLower.Contains('/rest/') then
  begin
    Inherited;

    exit;
  end;

  (* ERROR SCHEME
    ============
    Some REST implementations makes use of the standard HTTP errors and re-cycles
    them for new purposes. This is in my view to ruin the HTTP standard. REST
    should exist purely as an abstraction, existing "on top of" the established
    norm and under no circumstances interfere with it.

    As such, all REST calls result in "200 OK" no matter what went wrong.
    This is what both Facebook and Twitter decided to do, and for good reason.

    Source: http://apigee.com/about/blog/technology/restful-api-design-what-about-errors

    In our case we also use Indy, which is very sensitive to exceptions -- and
    the less exceptions we throw serverside the better *)

  AResponseInfo.ResponseNo := 200;
  AResponseInfo.ContentText := AException.Message;
  AResponseInfo.ContentType := 'application/json';

  // Serve HTML errors?
  if FServeHtmlErrors.Value then
  begin
    mHtml := TStringlist.Create;
    try
      mHtml.Add('<HTML>');
      mHtml.Add('<HEAD>');
      mHtml.Add('<Title>REST Server error ' + AException.ClassName +
        '</TITLE>');
      mHtml.Add('</HEAD>');
      mHtml.Add('<body>');

      mHtml.Add('<font face="verdana"');
      mHtml.Add(
        '<div style="background-color:#976537"><h3>An error occured on the server</h3></div>');

      mHtml.Add('<p>System threw exception ' + AException.ClassName +
        ' with error:</p>');
      mHtml.Add('<font color=#FF0000>');
      mHtml.Add('<p>' + AException.ClassName + ' - ' + AException.Message
        + '</p>');

      mHtml.Add('</body>');
      mHtml.Add('</HTML>');

      AResponseInfo.ResponseNo := 200;
      AResponseInfo.ContentStream := NIL;
      AResponseInfo.ContentText := mHtml.Text;
      AResponseInfo.ContentType := 'text/html';
    finally
      mHtml.Free;
    end;

    AResponseInfo.WriteHeader;
    AResponseInfo.WriteContent;
  end
  else
  begin
    // Standard OK response
    AResponseInfo.ResponseNo := 200;
    AResponseInfo.ContentType := 'application/json';

    (* Send ordinary JSON error:
      This generates a standard error-message envelope,
      and includes the exception information directly *)
    mJSONError := TIdRestErrorMessage.CreateWithError(AException);
    try
      AResponseInfo.ContentText := mJSONError.Serialize;
    finally
      mJSONError.Free;
    end;
  end;
end;

procedure TPTLibIdRestServer.DoRestCommand(Uri: TIdURI; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  mText: String;
  mServiceName: String;
  mMethodName: String;
  mServiceObj: TIdRestSchemaService;
  mMethodObj: TIdRestSchemaMethod;
  x: Integer;
  xpos: Integer;
  mParams: TDictionary<String, String>;
  mParamName: String;
  mParamData: String;
  mResponseData: String;

  IMethodsAccess: IIdRestMethods;
  CallInfo: TPTRestCallData;

  IMethodAccess: IIdRestMethodSchema;

  mValid: Boolean;
begin
  TCustomRestServerContext(AContext).SetMethodName(ARequestInfo.RawHTTPCommand);

  if Assigned(FAuthenticate) then
  begin
    // Make sure user/pass is valid
    mValid := true;
    FAuthenticate(self, ARequestInfo.UserName, ARequestInfo.Password, mValid);

    // not valid? Ok, generate a response
    if not mValid then
    begin
      Raise Exception.Create
        ('Authentication failed, service not allowed error');
    end;
  end;

  mText := Uri.Path;

  // Setup default response values
  AResponseInfo.ResponseNo := 200; // HTTP-OK
  AResponseInfo.ContentText := '';
  AResponseInfo.ContentType := 'application/json';

  // delete "/rest/" prefix
  Delete(mText, 1, 6);

  // Grab service-name
  xpos := pos('/', mText);
  if xpos > 1 then
  begin
    mServiceName := copy(mText, 1, xpos - 1);
    Delete(mText, 1, xpos);
  end
  else
  begin
    if (mText = '') or (mText = '/') then
    begin
      AResponseInfo.ContentText := self.Schema;
      exit;
    end
    else
    begin
      Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Maelformed,
        [ARequestInfo.Uri]);
      exit;
    end;
  end;

  // Make sure service exists
  if not GetServiceByName(mServiceName, mServiceObj, False) then
  begin
    Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Unknown_Service,
      [mServiceName]);
    exit;
  end;

  // Make sure service is enabled
  if not GetServiceEnabled(mServiceName) then
  begin
    Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Service_disabled,
      [mServiceName]);
    exit;
  end;

  // Grab Method-name, method is always the document (!)
  mMethodName := Uri.Document.ToLower.trim;

  // Schema service request?
  if (mMethodName.Length < 1) or (mMethodName.Equals('/')) then
  begin
    AResponseInfo.ContentText := mServiceObj.Schema;
    exit;
  end;

  // Make sure service exposes this method
  if mServiceObj.Methods.GetInterface(IIdRestMethods, IMethodsAccess) then
  begin
    if not IMethodsAccess.GetMethodByName(mMethodName, mMethodObj) then
    begin
      raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Service_Method_NotExposed,
        [mServiceName, Uri.Document]);
    end;
  end
  else
  begin
    Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Internal_Error,
      ['service method collection does not support method access']);
  end;

  (* Check if this is a method schema request? Since all REST calls must
    define at least one parameter or attachment, we know its a schema
    request if the request is missing params. *)
  if (ARequestInfo.Params.Count < 1) then
  Begin
    if ARequestInfo.Command.ToLower = 'get' then
    Begin
      AResponseInfo.ContentText := mMethodObj.Schema;
      exit;
    end;
  end;

  (* Make sure protocol access matches that defined for the method.
    GET, POST, HEAD and PUT serve very different purposes under REST *)
  if HTTPCommandToRestMethodAccess(ARequestInfo.Command) <> mMethodObj.Access
  then
  begin
    Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Method_Protocol,
      [AccessMethodToStr(mMethodObj.Access), mMethodName]);
  end;

  (* We only support 3 types of encoding:
    1: application/x-www-form-urlencoded
    2: application/json
    3: multipart/form-data *)
  if ContentTypeToAttachmentType(ARequestInfo.ContentType) = raUnknown then
  begin
    if ARequestInfo.Params.Count < 1 then
    begin
      Raise EIdRestServer.CreateFmt
        ('REST invoke for %s.%s() failed, no content error',
        [mServiceName, mMethodName]);
      exit;
    end;
  end;

  // Now deal with parameters and qualification
  mParams := TDictionary<String, String>.Create;
  try

    // Populate dictionary with parameters.
    // Note: GET does not support attachments, so all params will be
    // in the form of ?Name=Value&Name2=Value2 pairs..
    for x := 1 to ARequestInfo.Params.Count do
    begin
      mParamName := ARequestInfo.Params.Names[x - 1];
      mParamData := ARequestInfo.Params.Values[mParamName];
      if not mParams.ContainsKey(mParamName) then
      begin
        mParams.Add(mParamName, mParamData);
      end;
    end;

    if mMethodObj.GetInterface(IIdRestMethodSchema, IMethodAccess) then
    begin
      try
        IMethodAccess.Qualify(mParams)
      except
        on e: Exception do
          raise;
      end;
    end;

    // Create instance of our call-data
    CallInfo := FCallDataClass.Create;
    try
      CallInfo.Service := mServiceObj;
      CallInfo.Method := mMethodObj;
      CallInfo.Params := ConvertParameters(mParams, mMethodObj);
      CallInfo.Log := GetServerLog;
      CallInfo.Attachment := nil;

      CallInfo.SocketInfo.Context := AContext as TCustomRestServerContext;
      CallInfo.SocketInfo.Request := ARequestInfo;
      CallInfo.SocketInfo.Response := AResponseInfo;

      // Check if we support attachments?
      if not(mMethodObj.Access in [rcUnknown, rcGET]) then
      begin

        // Check if there is an actual datastream attached
        if (ARequestInfo.PostStream <> NIL) then
        Begin
          // Check that attachment-type matches schema
          if not(mMethodObj.AttachmentType.Value = ContentTypeToAttachmentType
            (ARequestInfo.ContentType)) then
          begin
            Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Attachment_Wrong,
              [AttachmentTypeToContentType(mMethodObj.AttachmentType.Value),
              ARequestInfo.ContentType]);
          end
          else
          begin
            // Attachment-type OK, grab the request stream
            CallInfo.Attachment := ARequestInfo.PostStream;
          end;
        end
        else
        begin
          // Check if this method demands an attachment?
          if mMethodObj.Attachment.Value then
          begin
            Raise EIdRestServer.CreateFmt(CNT_ERR_INVOC_Attachment_Missing,
              [AttachmentTypeToContentType(mMethodObj.AttachmentType.Value)]);
          end;
        end;
      end;

      // Execute method, re-raise exception if anything goes wrong
      mServiceObj.Methods.Execute(CallInfo);

      AResponseInfo.ContentText := mResponseData;
    finally
      if Assigned(CallInfo) then
        FreeAndNIL(CallInfo);
    end;

  finally
    mParams.Free;
  end;
end;

procedure TPTLibIdRestServer.DoTooManyConnections(AContext: TIdContext; AResponseInfo: TIdHTTPResponseInfo);
begin
  raise Exception.Create('Too many connections');
end;

procedure TPTLibIdRestServer.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  mUri: TIdURI;
begin
  mUri := TIdURI.Create(ARequestInfo.Uri);
  try
    // URI target's REST namespace?
    if mUri.Path.StartsWith('/rest/', true) then
    begin
      // Execute as REST
      DoRestCommand(mUri, AContext, ARequestInfo, AResponseInfo);
    end
    else
    begin
      // Let the default indy behavior kick in
      inherited;
    end;
  finally
    mUri.Free;
  end;
end;

procedure TPTLibIdRestServer.DoCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  mUri: TIdURI;
begin
  mUri := TIdURI.Create(ARequestInfo.Uri);
  try
    // URI target's REST namespace?
    if mUri.Path.StartsWith('/rest/', true) then
    begin
      // Execute as REST
      DoRestCommand(mUri, AContext, ARequestInfo, AResponseInfo);
    end
    else
    begin
      // Let the default indy behavior kick in
      inherited;
    end;
  finally
    mUri.Free;
  end;
end;

function TPTLibIdRestServer.DoQuerySSLPort(APort: TIdPort): Boolean;
begin
  result := FSSLEnabled;
end;

// ############################################################################
// TIdServices
// ############################################################################

constructor TIdRestSchemaServices.Create(AOwner: TPTLibIdRestServer);
begin
  inherited Create(true);

  FParent := AOwner;

  if not Assigned(AOwner) then
  begin
    Raise EIdRestMethods.CreateFmt('%s requires parent instance of type %s',
      [ClassName, AOwner.ClassType.ClassName]);
  end;
end;

function TIdRestSchemaServices.Lock: TObjectList<TIdRestSchemaService>;
begin
  result := TObjectList<TIdRestSchemaService>(inherited Lock);
end;

function TIdRestSchemaServices.Contains(Instance: TIdRestSchemaService)
  : Boolean;
begin
  result := inherited Contains(Instance);
end;

function TIdRestSchemaServices.ObjectOf(const ServiceName: String)
  : TIdRestSchemaService;
var
  mItem: TIdRestSchemaService;
  mList: TObjectList<TIdRestSchemaService>;
begin
  result := NIL;

  if ServiceName.Length > 0 then
  begin
    mList := Lock;
    try
      for mItem in mList do
      begin
        if SameText(mItem.ServiceName, ServiceName) then
        begin
          result := mItem;
          break;
        end;
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TIdRestSchemaServices.BuildElementSchema
  (Buffer: TPTLibTabbedStringList);
var
  IAccess: IIdRestElementSchema;
  mService: TIdRestSchemaService;
  mList: TObjectList<TIdRestSchemaService>;
begin
  Buffer.BeginSection;
  try

    mList := Lock;
    try
      // Build schema-parts for all contained services
      for mService in mList do
      begin

        // Generate and add schema part for service
        if mService.GetInterface(IIdRestElementSchema, IAccess) then
        begin
          IAccess.BuildElementSchema(Buffer);
        end;

        // Add comma if it's not the last service in our list
        if not mService.Equals(mList.Last) then
        begin
          Buffer.Add(',');
        end;
      end;

    finally
      UnLock;
    end;

  finally
    Buffer.EndSection;
  end;
end;

function TIdRestSchemaServices.GetElementSchema: String;
var
  mBuffer: TPTLibTabbedStringList;
begin
  mBuffer := TPTLibTabbedStringList.Create;
  try

    try
      BuildElementSchema(mBuffer);
    except
      on e: Exception do
      begin
        raise EIdRestSchemaGeneration.CreateFmt(CNT_ERR_SCHEMA_FailedGenerate,
          [ClassName, e.ClassName, e.Message]);
      end;
    end;

    result := mBuffer.Text;
  finally
    mBuffer.Free;
  end;
end;

// ############################################################################
// TIdRestSchemaParameter
// ############################################################################

constructor TIdRestSchemaParameter.Create;
begin
  inherited;
  FName := TPTLockedValue<String>.Create;
  FKind := TPTLockedValue<TIdRestSchemaParameterType>.Create;
  FKind.Value := rpUnknown;
end;

destructor TIdRestSchemaParameter.Destroy;
begin
  FName.Free;
  FKind.Free;
  inherited;
end;

// ############################################################################
// TIdRestSchemaParameters
// ############################################################################

constructor TIdRestSchemaParameters.Create(AOwner: TIdRestSchemaMethod);
begin
  inherited Create(true);
  FParent := TPTLockedValue<TIdRestSchemaMethod>.Create;
  FParent.Value := AOwner;
end;

destructor TIdRestSchemaParameters.Destroy;
begin
  FParent.Free;
  inherited;
end;

function TIdRestSchemaParameters.Contains
  (Instance: TIdRestSchemaMethod): Boolean;
begin
  result := inherited Contains(Instance);
end;

function TIdRestSchemaParameters.Lock: TObjectList<TIdRestSchemaParameter>;
begin
  result := TObjectList<TIdRestSchemaParameter>(inherited Lock);
end;

procedure TIdRestSchemaParameters.Delete(RestParamName: String);
var
  mObj: TIdRestSchemaParameter;
  mIndex: Integer;
  mItems: TObjectList<TIdRestSchemaParameter>;
begin
  mObj := ObjectOf(RestParamName);
  if mObj <> NIL then
  Begin
    mItems := Lock;
    try
      mIndex := mItems.IndexOf(mObj);
      if mIndex >= 0 then
        mItems.Delete(mIndex);
    finally
      UnLock;
    end;
  end;
end;

procedure TIdRestSchemaParameters.Delete(RestParamObj: TIdRestSchemaParameter);
var
  mIndex: Integer;
  mItems: TObjectList<TIdRestSchemaParameter>;
begin
  if RestParamObj <> nil then
  begin
    mItems := Lock;
    try
      mIndex := mItems.IndexOf(RestParamObj);

      if mIndex >= 0 then
      begin
        mItems.Delete(mIndex);
      end;

    finally
      UnLock;
    end;
  end;
end;

function TIdRestSchemaParameters.ObjectOf(const RestParamName: String)
  : TIdRestSchemaParameter;
var
  mObj: TIdRestSchemaParameter;
  mItems: TObjectList<TIdRestSchemaParameter>;
begin
  result := nil;

  if RestParamName.Length > 0 then
  begin
    mItems := Lock;
    try
      for mObj in mItems do
      begin
        if SameText(mObj.Name.Value, RestParamName) then
        begin
          result := mObj;
          break;
        end;
      end;
    finally
      UnLock;
    end;
  end;
end;

function TIdRestSchemaParameters.Add(RestParamObj: TIdRestSchemaParameter)
  : TIdRestSchemaParameter;
var
  mItems: TObjectList<TIdRestSchemaParameter>;
begin
  result := RestParamObj;
  if RestParamObj <> nil then
  begin
    if ObjectOf(RestParamObj.Name.Value) = NIL then
    begin
      mItems := Lock;
      try
        if mItems.IndexOf(RestParamObj) < 0 then
        begin
          mItems.Add(RestParamObj)
        end;
      finally
        UnLock;
      end;
    end;
  end;
end;

function TIdRestSchemaParameters.Add(Name: String;
  DataType: TIdRestSchemaParameterType; DefaultValue: String)
  : TIdRestSchemaParameter;
begin
  result := ObjectOf(Name);
  if result = nil then
  begin
    result := TIdRestSchemaParameter.Create;
    result.Name.Value := Name;
    result.DataType.Value := DataType;
    result.Default := DefaultValue;

    Add(result);
  end;
end;

// ############################################################################
// TIdRestSchemaService
// ############################################################################

constructor TIdRestSchemaService.Create(AOwner: TPTLibIdRestServer;
  const Name: string);
var
  mServer: IIdRestServer;
begin
  inherited Create;

  FEncoding := TPTLockedValue<String>.Create;
  FName := Name;
  FEnabled := TPTLockedValue<Boolean>.Create;
  FMethods := TIdRestSchemaMethods.Create(self);

  FEncoding.Value := 'text/base64';
  FEnabled.Value := true;

  if Assigned(AOwner) then
  begin
    FParent := AOwner;

    // Register this service with the server
    if AOwner.GetInterface(IIdRestServer, mServer) then
    begin
      mServer.RegisterService(self);
    end;

  end
  else
  begin
    Raise EIdRestMethods.CreateFmt('%s requires parent instance of type %s',
      [ClassName, AOwner.ClassType.ClassName]);
  end;
end;

destructor TIdRestSchemaService.Destroy;
begin
  FreeAndNIL(FEncoding);
  FreeAndNIL(FEnabled);
  FreeAndNIL(FMethods);
  inherited;
end;

function TIdRestSchemaService.GetElementSchema: String;
var
  mTemp: TPTLibTabbedStringList;
begin
  mTemp := TPTLibTabbedStringList.Create;
  try
    try
      BuildElementSchema(mTemp);
    except
      on e: Exception do
      begin
        raise EIdRestSchemaGeneration.CreateFmt(CNT_ERR_SCHEMA_FailedGenerate,
          [ClassName, e.ClassName, e.Message]);
      end;
    end;

    result := mTemp.Text;
  finally
    mTemp.Free;
  end;
end;

procedure TIdRestSchemaService.BuildElementSchema
  (Buffer: TPTLibTabbedStringList);
var
  IAccess: IIdRestElementSchema;
begin
  Buffer.BeginSection;
  try
    Buffer.Add('{');
    Buffer.BeginSection;
    try
      Buffer.Add(JSONPair('service', FName, False));
      Buffer.Add(JSONPair('encoding', FEncoding.Value, False));
      Buffer.Add(DQuoteStr('methods') + ': [');

      if FMethods.GetInterface(IIdRestElementSchema, IAccess) then
      begin
        IAccess.BuildElementSchema(Buffer);
      end;

      Buffer.Add(']');
    finally
      Buffer.EndSection;
    end;
  finally
    Buffer.Add('}');
    Buffer.EndSection;
  end;
end;

// ############################################################################
// TIdRestSchemaMethods
// ############################################################################

constructor TIdRestSchemaMethods.Create(AOwner: TIdRestSchemaService);
begin
  inherited Create(true);
  FParent := AOwner;

  if not Assigned(AOwner) then
  begin
    Raise EIdRestMethods.CreateFmt('%s requires parent instance of type %s',
      [ClassName, AOwner.ClassType.ClassName]);
  end;
end;

function TIdRestSchemaMethods.GetElementSchema: String;
var
  mTemp: TPTLibTabbedStringList;
begin
  mTemp := TPTLibTabbedStringList.Create;
  try
    try
      BuildElementSchema(mTemp);
    except
      on e: Exception do
      begin
        raise EIdRestSchemaGeneration.CreateFmt(CNT_ERR_SCHEMA_FailedGenerate,
          [ClassName, e.ClassName, e.Message]);
      end;
    end;

    result := mTemp.Text;
  finally
    mTemp.Free;
  end;
end;

procedure TIdRestSchemaMethods.BuildElementSchema
  (Buffer: TPTLibTabbedStringList);
var
  mObj: TIdRestSchemaMethod;
  mItems: TObjectList<TIdRestSchemaMethod>;
  mAccess: IIdRestMethodSchema;
begin
  mItems := Lock;
  try
    Buffer.BeginSection;
    try

      for mObj in mItems do
      begin
        if mObj.GetInterface(IIdRestMethodSchema, mAccess) then
        begin
          mAccess.BuildElementSchema(Buffer);
        end;

        if mObj <> mItems.Last then
        begin
          Buffer.Add(',');
        end;
      end;

    finally
      Buffer.EndSection;
    end;

  finally
    self.UnLock;
  end;
end;

procedure TIdRestSchemaMethods.Execute(Info: TPTRestCallData);
var
  IAccess: IIdRestMethodDispatch;
begin
  if (Info.Method = NIL) or (Info.Service = NIL) or (Info.Params = NIL) then
  begin
    Raise EIdRestExecute.Create(CNT_ERR_INVOC_Execute_Invalid_Info);
    exit;
  end;

  if Info.Method.GetInterface(IIdRestMethodDispatch, IAccess) then
  begin
    IAccess.Execute(Info);
  end;
end;

function TIdRestSchemaMethods.RegisterMethod(RestMethod: TIdRestSchemaMethod)
  : TIdRestSchemaMethod;
var
  mAccess: TObjectList<TIdRestSchemaMethod>;
begin
  result := RestMethod;

  if Assigned(RestMethod) then
  begin
    if not GetMethodByName(RestMethod.RestMethodName, result) then
    begin
      mAccess := Lock;
      try
        if not mAccess.Contains(RestMethod) then
        begin
          mAccess.Add(RestMethod);
        end;
      finally
        UnLock;
      end;
    end;
  end;
end;

function TIdRestSchemaMethods.GetMethodByName(RestMethodName: String;
  out RestMethod: TIdRestSchemaMethod): Boolean;
var
  mAccess: TObjectList<TIdRestSchemaMethod>;
  mItem: TIdRestSchemaMethod;
begin
  result := False;
  RestMethod := NIL;
  RestMethodName := RestMethodName.ToLower.trim;
  if RestMethodName.Length > 0 then
  begin
    mAccess := Lock;
    try
      for mItem in mAccess do
      begin
        if mItem.RestMethodName.ToLower.trim.Equals(RestMethodName) then
        begin
          RestMethod := mItem;
          result := true;
          break;
        end;
      end;
    finally
      UnLock;
    end;
  end;
end;

function TIdRestSchemaMethods.Contains(RestMethod: TIdRestSchemaMethod)
  : Boolean;
begin
  result := inherited Contains(RestMethod);
end;

function TIdRestSchemaMethods.Contains(RestMethodName: String): Boolean;
var
  mAccess: TObjectList<TIdRestSchemaMethod>;
  mItem: TIdRestSchemaMethod;
begin
  result := False;
  RestMethodName := RestMethodName.ToLower.trim;
  if RestMethodName.Length > 0 then
  begin
    mAccess := Lock;
    try
      for mItem in mAccess do
      begin
        result := mItem.RestMethodName.ToLower.Equals(RestMethodName);
        if result then
          break;
      end;
    finally
      UnLock;
    end;
  end;
end;

function TIdRestSchemaMethods.Lock: TObjectList<TIdRestSchemaMethod>;
begin
  result := TObjectList<TIdRestSchemaMethod>(inherited Lock);
end;

// ############################################################################
// TIdRestSchemaMethodGet
// ############################################################################

procedure TIdRestSchemaMethodGet.AddAttachmentSchemaInfo
  (Buffer: TPTLibTabbedStringList);
begin
  // HTTP-GET does not support attachment (post-stream)
end;

// ############################################################################
// TIdRestSchemaMethodPost
// ############################################################################

constructor TIdRestSchemaMethodPost.Create(AOwner: TIdRestSchemaService;
  const MethodName: string; const AuthorizationRequired: Boolean = False);
begin
  inherited;
  FAccess := TIdRestSchemaMethodAccess.rcPOST;
end;

procedure TIdRestSchemaMethodPost.AddAttachmentSchemaInfo
  (Buffer: TPTLibTabbedStringList);
begin
  Buffer.Add(',');
  Buffer.BeginSection;
  try
    Buffer.Add(JSONPair('attachment', BoolToStr(Attachment.Value,
      true), False));
    Buffer.Add(JSONPair('attachment-type',
      AttachmentTypeToContentType(AttachmentType.Value)));
  finally
    Buffer.EndSection;
  end;
end;

// ############################################################################
// TIdRestSchemaMethod
// ############################################################################

constructor TIdRestSchemaMethod.Create(AOwner: TIdRestSchemaService;
  const MethodName: string; const AuthorizationRequired: Boolean = False);
var
  mHost: IIdRestMethods;
begin
  inherited Create;
  FName := MethodName;
  FAuthorizationRequired := AuthorizationRequired;

  FFluent := False;

  // Setup attachment support
  FAttachment := TPTLockedValue<Boolean>.Create;
  FAttachment.Value := False;

  // Initialize attachment type to JSON
  FAttachType := TPTLockedValue<TIdRestAttachmentType>.Create;
  FAttachType.Value := TIdRestAttachmentType.raJSON;

  FAccess := TIdRestSchemaMethodAccess.rcGET;

  FParams := TIdRestSchemaParameters.Create(self);

  if Assigned(AOwner) then
  begin
    FService := AOwner;
    FMethods := AOwner.Methods;

    // Register with methods host
    if AOwner.Methods.GetInterface(IIdRestMethods, mHost) then
    begin
      mHost.RegisterMethod(self);
    end;

  end
  else
  begin
    Raise EIdRestMethods.CreateFmt('%s requires parent instance of type %s',
      [ClassName, AOwner.ClassType.ClassName]);
  end;

end;

destructor TIdRestSchemaMethod.Destroy;
begin
  FParams.Free;
  FAttachment.Free;
  FAttachType.Free;
  inherited;
end;

procedure TIdRestSchemaMethod.Qualify(Params: TDictionary<String, String>);
var
  Key: String;
  Param: TIdRestSchemaParameter;
  mBool: Boolean;
  mInt: Integer;
  mFloat: Double;
begin
  if FFluent then
    exit;

  for Key in Params.Keys do
  begin
    Param := Parameters.ObjectOf(Key);
    if Param <> nil then
    Begin
      case Param.DataType.Value of
        rpUnknown:
          begin
            // This should never happen, the type should be clearly defined
            // by the code creating the service/method layer
          end;
        rpBool:
          begin
            if not TryStrToBool(Params.Items[Key], mBool) then
            begin
              Raise EIdRestSchemaValidation.CreateFmt
                ('REST method schema validation failed, parameter %s expected to be of type %s',
                [Key, 'boolean']);
            end;
          end;
        rpNumber:
          begin
            if not TryStrToInt(Params.Items[Key], mInt) and
              not TryStrToFloat(Params.Items[Key], mFloat) then
            begin
              Raise EIdRestSchemaValidation.CreateFmt
                ('REST method schema validation failed, parameter %s expected to be of type %s',
                [Key, 'boolean']);
            end;
          end;
        rpText:
          begin
            // You cant really test for text in that manner
            // mRaw := Params.Items[Key];
            // mText := TNetEncoding.Base64.Decode(mRaw);
          end;
      end;
    end
    else
    begin
      Raise EIdRestSchemaValidation.CreateFmt
        ('REST method schema validation failed, parameter %s not found in schema for method %s.%s error',
        [Key, FService.ServiceName, RestMethodName]);
    end;
  end;
end;

function TIdRestSchemaMethod.GetElementSchema: String;
var
  mTemp: TPTLibTabbedStringList;
begin
  mTemp := TPTLibTabbedStringList.Create;
  try

    try
      BuildElementSchema(mTemp);
    except
      on e: Exception do
      begin
        raise EIdRestSchemaGeneration.CreateFmt(CNT_ERR_SCHEMA_FailedGenerate,
          [ClassName, e.ClassName, e.Message]);
      end;
    end;

    result := mTemp.Text;
  finally
    mTemp.Free;
  end;
end;

procedure TIdRestSchemaMethod.BuildElementSchema
  (Buffer: TPTLibTabbedStringList);
var
  Param: TIdRestSchemaParameter;
  Items: TObjectList<TIdRestSchemaParameter>;
begin
  Buffer.Add('{');
  Buffer.BeginSection;
  try
    Buffer.Add(JSONPair('name', FName, False));
    Buffer.Add(JSONPair('access', AccessMethodToStr(FAccess), False));

    if not FParams.Empty then
    begin
      Buffer.Add(DQuoteStr('parameters') + ': [');
      Buffer.BeginSection;
      try
        Items := FParams.Lock;
        try
          for Param in Items do
          begin
            Buffer.Add('{');
            Buffer.BeginSection;
            try
              Buffer.Add(JSONPair('name', Param.Name.Value, False));
              Buffer.Add(JSONPair('datatype',
                ParameterDataTypeToStr(Param.DataType.Value), False));
              Buffer.Add(JSONPair('default', string(Param.Default)));
            finally
              Buffer.EndSection;
            end;
            Buffer.Add('}');

            if Param <> Items.Last then
              Buffer.Add(',');

          end;
        finally
          FParams.UnLock;
        end;
      finally
        Buffer.EndSection;
        Buffer.Add(']');
      end;
    end
    else
    begin
      Buffer.Add(DQuoteStr('parameters') + ': []');
    end;
  finally
    Buffer.EndSection;
  end;

  AddAttachmentSchemaInfo(Buffer);

  Buffer.Add('}');
end;

function TIdRestSchemaMethod.DoAuthenticate(const Info
  : TPTRestCallData): Boolean;
begin
  result := False;

  // Check that we require authentication
  if FAuthorizationRequired then
  begin
    // Check that we have an event handler assigned
    if Assigned(FOnAuth) then
    begin
      (* Execute authenticate, we dont care how - and just pass the
        execution information. This contains everything *)
      FOnAuth(self, Info, result);
    end;
  end;
end;

procedure TIdRestSchemaMethod.Execute(const Info: TPTRestCallData);
begin
  if FAuthorizationRequired then
  begin
    if not DoAuthenticate(Info) then
    begin
      raise EIdRestExecute.Create('Authentication required');
    end;
  end;

  if Assigned(FHandler) then
  begin
    try
      FHandler(Info)
    except
      on e: Exception do
      begin
        raise;

        // Raise EIdRestExecute.CreateFmt('Execution of %s.%s failed, native implementation threw exception %s with message [%s]',
        // [Info.Service.ServiceName, Info.Method.RestMethodName, e.ClassName, e.Message]);
      end;
    end;
  end
  else

    if Assigned(FHandler2) then
  begin
    try
      FHandler2(Info);
    except
      on e: Exception do
      begin
        raise;

        // Raise EIdRestExecute.CreateFmt('Execution of %s.%s failed, native implementation threw exception %s with message [%s]',
        // [Info.Service.ServiceName, Info.Method.RestMethodName, e.ClassName, e.Message]);
      end;
    end;
  end
  else
  begin
    raise EIdRestExecute.CreateFmt
      ('Execution of %s.%s failed, native implementation not assigned error',
      [Info.Service.ServiceName, Info.Method.RestMethodName]);
  end;
end;

procedure TIdRestSchemaMethod.HandleWith(ObjectEntryPoint
  : TIdRestExecuteEntryEvent);
begin
  FHandler2 := ObjectEntryPoint;
end;

procedure TIdRestSchemaMethod.HandleWith(Entrypoint: TIdRestExecuteEntry);
begin
  FHandler := Entrypoint;
end;

// #############################################################################
// TPTRestCallData
// #############################################################################

constructor TPTRestCallData.Create;
begin
  inherited;
  FSocketInfo := TPTRestConnectionInfo.Create;
end;

destructor TPTRestCallData.Destroy;
begin
  if Assigned(FSocketInfo) then
  begin
    FreeAndNIL(FSocketInfo);
  end;
  inherited;
end;

function TPTRestCallData.GetAttachment: TStream;
begin
  result := FAttachment;
end;

function TPTRestCallData.GetLog: ILog;
begin
  result := FLog;
end;

function TPTRestCallData.GetMethodObj: TIdRestSchemaMethod;
begin
  result := FMethod;
end;

function TPTRestCallData.GetParams: IParameters;
begin
  result := FParams;
end;

function TPTRestCallData.GetService: TIdRestSchemaService;
begin
  result := FService;
end;

function TPTRestCallData.GetSocketInfo: TPTRestConnectionInfo;
begin
  result := FSocketInfo;
end;

procedure TPTRestCallData.SetAttachment(Value: TStream);
begin
  FAttachment := Value;
end;

procedure TPTRestCallData.SetLog(Value: ILog);
begin
  FLog := Value;
end;

procedure TPTRestCallData.SetMethodObj(Value: TIdRestSchemaMethod);
begin
  FMethod := Value;
end;

procedure TPTRestCallData.SetParams(Value: IParameters);
begin
  FParams := Value;
end;

procedure TPTRestCallData.SetService(Value: TIdRestSchemaService);
begin
  FService := Value;
end;

procedure TPTRestCallData.SetSocketInfo(Value: TPTRestConnectionInfo);
begin
  // Release socketinfo object already there
  if Assigned(FSocketInfo) then
  begin
    FreeAndNIL(FSocketInfo);
  end;

  FSocketInfo := Value;
end;

// #############################################################################
// TPTRestConnectionInfo
// #############################################################################

function TPTRestConnectionInfo.GetContext: TCustomRestServerContext;
begin
  result := FContext;
end;

function TPTRestConnectionInfo.GetRequest: TIdHTTPRequestInfo;
begin
  result := FRequest;
end;

function TPTRestConnectionInfo.GetResponse: TIdHTTPResponseInfo;
begin
  result := FResponse;
end;

procedure TPTRestConnectionInfo.SetContext(Value: TCustomRestServerContext);
begin
  FContext := Value;
end;

procedure TPTRestConnectionInfo.SetRequest(Value: TIdHTTPRequestInfo);
begin
  FRequest := Value;
end;

procedure TPTRestConnectionInfo.SetResponse(Value: TIdHTTPResponseInfo);
begin
  FResponse := Value;
end;

{ TCustomRestServerContext }

constructor TCustomRestServerContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;

  FTimestamp := nowUTC;
end;

function TCustomRestServerContext.GetContextIdentifier: string;
begin
  Result :=
    Connection.Socket.Binding.PeerIP + ':' +
    Connection.Socket.Binding.PeerPort.ToString + '-' +
    DateTimeToISODateTimeStr(FTimestamp);

  if FMethodName <> '' then
  begin
    Result := Result + '-' + FMethodName;
  end;
end;

function TCustomRestServerContext.GetMethodName: String;
begin
  Result := FMethodName;
end;

procedure TCustomRestServerContext.SetExceptionOccured;
begin
  FExceptionOccured := True;
end;

procedure TCustomRestServerContext.SetMethodName(const Name: string);
begin
  FMethodName := Name;
end;

end.
