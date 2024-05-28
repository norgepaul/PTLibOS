unit PTLib.MARS.Interfaces;

interface

uses
  System.Classes, System.SysUtils, Web.HTTPApp,

  MARS.Client.Resource, MARS.Client.Client,

  PTLib.Common.Interfaces,

  PTLib.MARS.Types;

type
  IMarsClientResponse = interface;

  TBeforeRequestProc = reference to procedure(const ID: String; const Client: TMARSClientResource);
  TResponseProc = reference to procedure(const ID: String; Response: IMarsClientResponse);
  TChunkResponseProc = reference to procedure(const ID: String; Response: TArray<IMarsClientResponse>);
  TErrorProc = reference to procedure(const ID: String; const e: Exception; Response: IMarsClientResponse);
  TGetResourceProc = reference to procedure(const ChunkIndex: Integer; out ChunkResource: String);
  TOnProgress = reference to procedure(const Index, Total: Integer);

  IMarsClientRequest = interface
    ['{EEDEBFEE-6BF2-4A7C-B9D8-D4F4EF7FDCD9}']
    function GetRequestStream: TMemoryStream;
    function GetResource: String;
    function GetHTTPRequestType: TMARSHttpVerb;
    function GetHeaders: TStringList;
    procedure SetResource(const Value: String);
    procedure SetHTTPRequestType(const Value: TMARSHttpVerb);
    property Resource: String read GetResource write SetResource;
    property RequestStream: TMemoryStream read GetRequestStream;
    property HTTPRequestType: TMARSHttpVerb read GetHTTPRequestType write SetHTTPRequestType;
    property Headers: TStringList read GetHeaders;
  end;

  IMarsServerContainerServiceApplication = interface
    ['{FF4FB1A0-D4E9-4E67-BA33-FA81D6A4D1A3}']
  end;

  IMarsServerContainerService = interface(IServerContainerService)
    ['{19CED123-1E63-4B43-99DC-22D763E169E3}']
    procedure AddApplicationClass(const ApplicationClass: TShareBikeBridgeServiceRESTApplicationClass;
      const AName, ABasePath: String; const AResources: array of String; const AParametersSliceName: String); overload;
    procedure AddApplicationClass(const ApplicationClass: TShareBikeBridgeServiceRESTApplicationClass); overload;
  end;

  IMarsClient = interface
    ['{73DF13EF-0431-43E6-B1A3-ED64489B2ECD}']
    function GetUseThreadPool: Boolean;
    procedure SetUseThreadPool(const Value: Boolean);
    function GetHost: String;
    procedure SetHost(const Value: String);

    procedure HTTPRequestAsync(Request: IMarsClientRequest; const OnResponse: TResponseProc;
      const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc; const ID: TRequestID);
    function HTTPRequestSync(Request: IMarsClientRequest;
      const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse;

    procedure GET(const Resource: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function GET(const Resource: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure GET(const ChunkCount: Integer; const GetResourceProc: TGetResourceProc; const Headers: TStrings; const OnResponse: TChunkResponseProc; const OnError: TErrorProc; const OnBeforeRequest: TBeforeRequestProc = nil; const OnProgress: TOnProgress = nil; const ID: TRequestID = NullRequestID); overload;

    procedure POST(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function POST(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure POST(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function POST(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure PUT(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PUT(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure PUT(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PUT(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure PATCH(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PATCH(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure PATCH(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function PATCH(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure DELETE(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function DELETE(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure DELETE(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function DELETE(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    procedure HEAD(const Resource: String; const Request: String; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function HEAD(const Resource: String; const Request: String; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;
    procedure HEAD(const Resource: String; const Request: TStream; const Headers: TStrings; const OnResponse: TResponseProc; const OnError: TErrorProc = nil; const OnBeforeRequest: TBeforeRequestProc = nil; const ID: TRequestID = NullRequestID); overload;
    function HEAD(const Resource: String; const Request: TStream; const Headers: TStrings = nil; const OnBeforeRequest: TBeforeRequestProc = nil): IMarsClientResponse; overload;

    property UseThreadPool: Boolean read GetUseThreadPool write SetUseThreadPool;
    property Host: String read GetHost write SetHost;
end;

  IMarsClientResponse = interface
    ['{1254A955-D972-471B-864C-98A9057D5CCC}']
    function GetResource: String;
    function GetResponseCode: Integer;
    function GetResponseStatus: String;
    function GetResponseStream: TMemoryStream;
    function GetResponseString: String;
    procedure SetResource(const Value: String);
    procedure SetResponseCode(const Value: Integer);
    procedure SetResponseStatus(const Value: String);
    property Resource: String read GetResource write SetResource;
    property ResponseStream: TMemoryStream read GetResponseStream;
    property ResponseStatus: String read GetResponseStatus write SetResponseStatus;
    property ResponseCode: Integer read GetResponseCode write SetResponseCode;
    property ResponseString: String read GetResponseString;
  end;

implementation

end.
