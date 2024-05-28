{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Classes                                     }
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

unit PTLib.Common.Classes;

interface

{.$DEFINE DEBUG_INTERFACES}

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections, System.SyncObjs,
  System.JSON, REST.Json, System.Hash, System.Variants, System.NetEncoding,
  System.TypInfo,

  PTLib.Common.Interfaces,
  PTLib.Common.Variants,
  PTLib.Common.Types,
  PTLib.Common.Serialization;

type
  EPTLibError = class(Exception);
  EPTLibParamError = class(EPTLibError);
  EPTLibAssignableObjectError = class(EPTLibError);
  EPTLibDatabaseError = class(EPTLibError);
  EPTLibJSONError = class(EPTLibError);
  EPTLibPriorityQueueError = class(EPTLibError);
  EPTLibSerilizationError = class(EPTLibError);
  EPTLibUnsupportedInterface = class(EPTLibError);
  EPTLibDateError = class(EPTLibError);
  EPTLibUnsupportedPlatformError = class(EPTLibError);
  EPTLibInvalidAPIKey = class(EPTLibError);
  EPTLibInvalidJSONObject = class(EPTLibError);
  EPTLibInvalidData = class(EPTLibError);
  EPTLibMissingHost = class(EPTLibError);
  EPTLibClipboardError = class(EPTLibError);
  EPTLibKeyValueError = class(EPTLibError);
  EPTLibNotSupportedError = class(EPTLibError);
  EPTLibUnableToLoadJSON = class(EPTLibError);

  TBasePTLibComponent = class(TComponent)
  end;

  {$IFNDEF DEBUG_INTERFACES}
  TPTLibInterfacedObject = TInterfacedObject;
  {$ELSE}
  TPTLibInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure DoOnRefCountDecremented(const RefCount: Integer); virtual;
    procedure DoOnRefCountIncremented(const RefCount: Integer); virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    function GetRefCount: Integer;
  end;
  {$ENDIF}

  TParameter = class(TInterfacedObject,
                     IParameter)
  strict private
    FName: String;
    FValue: Variant;
  private
    function GetName: String;
    function GetValue: Variant;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: Variant);
    procedure Assign(const Parameter: IParameter);
  public
    property Name: String read GetName write SetName;
    property Value: Variant read GetValue write SetValue;
  end;

  TBaseJSONSerialisable = class(TInterfacedObject)
  strict private
    FLoadingFromJSON: Boolean;
  protected
    procedure DoToJSON(const JSONArray: TJSONArray); virtual;
    procedure DoFromJSON(const JSONArray: TJSONArray); virtual;
  public
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

    property LoadingFromJSON: Boolean read FLoadingFromJSON;
  end;

  TJSONSerialisable = class(TBaseJSONSerialisable, IJSONSerialisable);

  TList<T> = class(TBaseJSONSerialisable,
                   IList<T>)
  strict private
    FInternalList: System.Generics.Collections.TList<T>;
  private
    procedure OnNotify(Sender: TObject; const Item: T;
      Action: TCollectionNotification);
  protected
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; const Value: T);
    function Add(const Value: T): Integer;
    procedure Clear;
    function IndexOf(const Value: T): Integer;
    function Remove(Value: T): Integer;
    procedure Delete(Index: Integer);
    procedure Sort(const AComparer: IComparer<T>);
    procedure Insert(Index: Integer; const Value: T);
    procedure Lock;
    procedure Unlock;
    function DeleteByValue(const Value: T; const AComparer: IComparer<T>): Boolean;

    procedure Notify(const Value: T; Action: TCollectionNotification); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  TSerializableList<I: IJSONSerialisable; T: Class, constructor> = class(TList<I>, IJSONSerialisable)
  private
    function ObjectToInterface(var AObject: T): I;
  protected
    procedure DoToJSON(const JSONArray: TJSONArray); override;
    procedure DoFromJSON(const JSONArray: TJSONArray); override;
  end;

  TPTLibObjectList<T: class> = class(TList<T>,
                                     IObjectList<T>)
  private
    FOwnsObjects: Boolean;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TParameters = class(TJSONSerialisable,
                      IParameters)
  strict private
    FParameters: IList<IParameter>;

    function SetParam(const Name: String; const Value: Variant): IParameters;
    function GetParam(const Name: String): Variant; overload;
    function GetParam(const Name: String; const Default: Variant): Variant; overload;
    procedure DeleteParam(const Name: String);
    function ParamExists(const Name: String): Boolean;
    function Count: Integer;
    function Param(const Index: Integer): IParameter;
    function FindParam(const Name: String): IParameter;
    function GetParamIndex(const Name: String): Integer;
    function SetParameters(const ParamNames: Array of String;
      const ParamValues: Array of Variant): IParameters;
    procedure Assign(const Parameters: IParameters);
    procedure Clear;
    procedure ToStrings(const Strings: TStrings);
    procedure FromStrings(const Values: TStrings);
    function ToURIParameters: String;
  private
    procedure CreateClasses;
  protected
    procedure DoToJSON(const JSONArray: TJSONArray); override;
    procedure DoFromJSON(const JSONArray: TJSONArray); override;
  public
    constructor Create; overload;
    constructor Create(const ParamNames: Array of String;
      const ParamValues: Array of Variant); overload;
  end;

  TAssignableObject = class(TInterfacedObject,
                            IAssignableObject)
  protected
    procedure Assign(const AObject: TObject); virtual;
    procedure AssignTo(const AObject: TObject); virtual;
  end;

  TFileInfo = class(TInterfacedObject,
                    IFileInfo)
  private
    FFilename: UnicodeString;
    FFilesize: Int64;
    FModifiedDate: TDateTime;
    FCreatedDate: TDateTime;
    FIsDir: Boolean;

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
  public
    property Filename: UnicodeString read GetFilename write SetFilename;
    property Filesize: Int64 read GetFilesize write SetFilesize;
    property ModifiedDate: TDateTime read GetModifiedDate write SetModifiedDate;
    property CreatedDate: TDateTime read GetCreatedDate write SetCreatedDate;
    property IsDir: Boolean read GetIsDir write SetIsDir;
  end;

  TNullable<T> = record
  private
    FValue: T;
    FHasValue: IInterface;
    function GetValue: T;
    function GetHasValue: Boolean;
  public
    constructor Create(AValue: T);
    function GetValueOrDefault: T; overload;
    function GetValueOrDefault(Default: T): T; overload;
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue;

    class operator NotEqual(ALeft, ARight: TNullable<T>): Boolean;
    class operator Equal(ALeft, ARight: TNullable<T>): Boolean;

    class operator Implicit(Value: TNullable<T>): T;
    class operator Implicit(Value: T): TNullable<T>;
//    class operator Explicit(Value: TNullable<T>): T;
  end;

  TSerialPortSettings = class(TInterfacedObject,
                              ISerialPortSettings)
  strict private
    FSerialPort: String;
    FTimeout: Integer;
    FBaudRate: Integer;
    FDataBits: Integer;
    FParity: TSerialPortParity;
    FStopBits: TSerialPortStopBits;
    FSoftFlow: Boolean;
    FHardFlow: Boolean;
    FMaxResponseLength: Integer;
    FRTSToggle: Boolean;
    FRTS: Boolean;
    FDTR: Boolean;
  private
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
  public
    constructor Create; virtual;

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

  TPTLibActiveInterfacedObject = class(TInterfacedObject,
                                       IPTLibActiveInterfacedObject)
  strict private
    FActive: Boolean;
  private
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    procedure DoActiveChanged(const Value: Boolean); virtual;
    procedure DoGetInformation(Value: IInformationList); virtual;

    function GetInformation(const PropertySeparator: String = '='): IInformationList;

    property Active: Boolean read GetActive write SetActive;
  end;

  TPTLibActiveParamsInterfacedObject = class(TPTLibActiveInterfacedObject,
                                             IPTLibActiveParamsInterfacedObject)
  strict private
    FParamCS: TMultiReadExclusiveWriteSynchronizer;
    FParams: IParameters;

    function GetParams: IParameters;
  protected
    property Params: IParameters read GetParams;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    function GetParam(const Name: String; const Default: Variant): Variant;
    procedure SetParam(const Name: String; const Value: Variant);
    procedure IncParam(const Name: String; const Value: Cardinal = 1);
  end;

function NewParameters: IParameters;
procedure SetFlagInterface(var Intf: IInterface);

implementation

uses
  PTLib.Common.InformationList;

resourcestring
  StrParameterSDoesNotExist = 'Parameter "%s" does not exist';
  StrParamNameCountMust = 'ParamName count must be the same as ParamValue count';
  StrUnableToAssign = 'Unable to assign "%s" to "%s"';

var
  RefCountCS: TCriticalSection;

function NewParameters: IParameters;
begin
  Result := TParameters.Create;
end;

{ TParameter }

procedure TParameter.Assign(const Parameter: IParameter);
begin
  Name := Parameter.Name;
  Value := Parameter.Value;
end;

function TParameter.GetName: String;
begin
  Result := FName;
end;

function TParameter.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TParameter.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TParameter.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TParameters }

procedure TParameters.Assign(const Parameters: IParameters);
var
  i: Integer;
begin
  Clear;

  for i := 0 to pred(Parameters.Count) do
  begin
    Self.SetParam(
      Parameters.Param(i).Name,
      Parameters.Param(i).Value);
  end;
end;

procedure TParameters.Clear;
begin
  FParameters.Clear;
end;

function TParameters.Count: Integer;
begin
  Result := FParameters.Count;
end;

constructor TParameters.Create;
begin
  CreateClasses;
end;

constructor TParameters.Create(const ParamNames: array of String;
  const ParamValues: array of Variant);
begin
  CreateClasses;

  SetParameters(ParamNames, ParamValues);
end;

procedure TParameters.CreateClasses;
begin
  FParameters := TList<IParameter>.Create;
end;

procedure TParameters.DeleteParam(const Name: String);
var
  idx: Integer;
begin
  idx := GetParamIndex(Name);

  if idx <> -1 then
  begin
    FParameters.Delete(idx);
  end;
end;

procedure TParameters.DoFromJSON(const JSONArray: TJSONArray);
var
  ItemName: string;
  ItemType: string;
  ItemData: string;
  ItemValue: TJSONValue;
begin
  for ItemValue in JSONArray do
  begin
    if ItemValue.TryGetValue<string>('value', ItemData) then
    begin
      ItemName := ItemValue.GetValue<string>('name');
      ItemType := ItemValue.GetValue<string>('type');

      SetParam(ItemName, GetVariantFromTextDeclaration(ItemType, ItemData));
    end;
  end;
end;

procedure TParameters.DoToJSON(const JSONArray: TJSONArray);
var
  I: Integer;
  JSONItem: TJSONObject;
  Value: string;
begin
  for I := 0 to pred(Count) do
  begin
    Value := VariantToString(Param(i).Value);

    if Value <> '' then
    begin
      JSONItem := TJSONObject.Create;
      JSONArray.Add(JSONItem);
      JSONItem.AddPair('name', Param(I).Name);
      JSONItem.AddPair('type', GetVariantTypeName(Param(I).Value));
      JSONItem.AddPair('value', Value);
    end;
  end;
end;

function TParameters.FindParam(const Name: String): IParameter;
var
  UpperName: String;
  i: Integer;
begin
  Result := nil;

  UpperName := AnsiUpperCase(Name);

  for i := 0 to pred(Count) do
    if AnsiSameText(UpperName, Param(i).Name) then
      Exit(Param(i));
end;

procedure TParameters.FromStrings(const Values: TStrings);
var
  i: Integer;
  Name, Value: String;
begin
  Clear;

  if Values <> nil then
  begin
    for i := 0 to pred(Values.Count) do
    begin
      Name := Values.Names[i].Trim;

      if Name.Length > 0 then
      begin
        Value := Values.Values[Name].Trim;

        SetParam(Name, Value);
      end;
    end;
  end;
end;

function TParameters.GetParam(const Name: String;
  const Default: Variant): Variant;
var
  Parameter: IParameter;
begin
  Parameter := FindParam(Name);

  if Parameter = nil then
    Result := Default
  else
    Result := Parameter.Value;
end;

function TParameters.GetParamIndex(const Name: String): Integer;
var
  UpperName: String;
  i: Integer;
begin
  Result := -1;

  UpperName := AnsiUpperCase(Name);

  for i := 0 to pred(Count) do
  begin
    if AnsiSameText(UpperName, Param(i).Name) then
      Exit(i);
  end;
end;

function TParameters.GetParam(const Name: String): Variant;
var
  Parameter: IParameter;
begin
  Parameter := FindParam(Name);

  if Parameter = nil then
    raise EPTLibParamError.CreateFmt(StrParameterSDoesNotExist, [Name])
  else
    Result := Parameter.Value;
end;

function TParameters.Param(const Index: Integer): IParameter;
begin
  Result := FParameters[Index];
end;

function TParameters.ParamExists(const Name: String): Boolean;
begin
  Result := FindParam(Name) <> nil;
end;

function TParameters.SetParam(const Name: String; const Value: Variant): IParameters;
var
  Parameter: IParameter;
begin
  Parameter := FindParam(Name);

  if Parameter = nil then
  begin
    Parameter := TParameter.Create;
    Parameter.Name := Name;
    FParameters.Add(Parameter);
  end;

  Parameter.Value := Value;

  Result := Self;
end;

function TParameters.SetParameters(const ParamNames: array of String;
  const ParamValues: array of Variant): IParameters;
var
  i: Integer;
begin
  Assert(Length(ParamNames) = length(ParamValues), StrParamNameCountMust);

  FParameters.Clear;

  for i := Low(ParamNames) to High(ParamNames) do
    SetParam(ParamNames[i], ParamValues[i]);

  Result := Self;
end;

procedure TParameters.ToStrings(const Strings: TStrings);
var
  i: Integer;
begin
  for i := 0 to pred(Count) do
  begin
    if FParameters[i].Name.Length > 0 then
    begin
      Strings.Add(
        FParameters[i].Name +
        Strings.NameValueSeparator +
        VariantToString(FParameters[i].Value));
    end;
  end;
end;

function TParameters.ToURIParameters: String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to pred(Count) do
  begin
    if i = 0 then
    begin
      Result := Result + '?';
    end else
    begin
      Result := Result + '&';
    end;

    Result :=
      Result +
      TNetEncoding.URL.Encode(FParameters[i].Name) +
      '=' +
      TNetEncoding.URL.Encode(VariantToString(FParameters[i].Value));
  end;
end;

{ TList<T> }

function TList<T>.Add(const Value: T): Integer;
begin
  Result := FInternalList.Add(Value);
end;

procedure TList<T>.Clear;
begin
  FInternalList.Clear;
end;

constructor TList<T>.Create;
begin
  FInternalList := System.Generics.Collections.TList<T>.Create;
  FInternalList.OnNotify := OnNotify;
end;

procedure TList<T>.OnNotify(Sender: TObject; const Item: T; Action: TCollectionNotification);
begin
  Notify(Item, Action);
end;

procedure TList<T>.Delete(Index: Integer);
begin
  FInternalList.Delete(Index);
end;

function TList<T>.DeleteByValue(const Value: T; const AComparer: IComparer<T>): Boolean;
var
  i: Integer;
  Value1, Value2: T;
begin
  Result := False;

  for i := pred(Count) downto 0 do
  begin
    if AComparer.Compare(Value, Items[i]) = 0 then    
    begin
      Delete(i);

      Result := True;
    end;
  end;
end;

destructor TList<T>.Destroy;
begin
  FreeAndNil(FInternalList);

  inherited;
end;

function TList<T>.GetCount: Integer;
begin
  Result := FInternalList.Count;
end;

function TList<T>.GetItem(Index: Integer): T;
begin
  Result := FInternalList[Index];
end;

function TList<T>.IndexOf(const Value: T): Integer;
begin
  Result := FInternalList.IndexOf(Value);
end;

procedure TList<T>.Insert(Index: Integer; const Value: T);
begin
  FInternalList.Insert(Index, Value);
end;

procedure TList<T>.Lock;
begin
  TMonitor.Enter(FInternalList);
end;

procedure TList<T>.Notify(const Value: T; Action: TCollectionNotification);
begin

end;

function TList<T>.Remove(Value: T): Integer;
begin
  Result := FInternalList.Remove(Value);
end;

procedure TList<T>.SetCount(Value: Integer);
begin
  FInternalList.Count := Value;
end;

procedure TList<T>.SetItem(Index: Integer; const Value: T);
begin
  FInternalList.Items[Index] := Value;
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  FInternalList.Sort(AComparer);
end;

procedure TList<T>.Unlock;
begin
  TMonitor.Exit(FInternalList);
end;

{ TAssignableObject }

procedure TAssignableObject.Assign(const AObject: TObject);
begin
  raise EPTLibAssignableObjectError.CreateFmt(StrUnableToAssign, [AObject.ClassName, ClassName]);
end;

procedure TAssignableObject.AssignTo(const AObject: TObject);
begin
  raise EPTLibAssignableObjectError.CreateFmt(StrUnableToAssign, [ClassName, AObject.ClassName]);
end;

{ TPTLibInterfacedObject }

{$IFDEF DEBUG_INTERFACES}
procedure TPTLibInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  TInterlocked.Decrement(FRefCount);
end;

procedure TPTLibInterfacedObject.BeforeDestruction;
begin
  if FRefCount <> 0 then
    raise Exception.Create('Invalid pointer');
end;

procedure TPTLibInterfacedObject.DoOnRefCountDecremented(
  const RefCount: Integer);
begin
  // Override
end;

procedure TPTLibInterfacedObject.DoOnRefCountIncremented(
  const RefCount: Integer);
begin
  // Override
end;

function TPTLibInterfacedObject.GetRefCount: Integer;
begin
  RefCountCS.Enter;
  try
    Result := FRefCount;
  finally
    RefCountCS.Leave;
  end;
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TPTLibInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TPTLibInterfacedObject(Result).FRefCount := 1;
end;

function TPTLibInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TPTLibInterfacedObject._AddRef: Integer;
begin
  RefCountCS.Enter;
  try
    Inc(FRefCount);

    Result := FRefCount;

    DoOnRefCountIncremented(Result);
  finally
    RefCountCS.Leave;
  end;
end;

function TPTLibInterfacedObject._Release: Integer;
begin
  Result := 0;

  RefCountCS.Enter;
  try
    try
      Dec(FRefCount);

      Result := FRefCount;

      DoOnRefCountDecremented(Result);

      if FRefCount = 0 then
        Destroy;
    except
      on e: Exception do
      begin
        raise Exception.CreateFmt('Interface release exception in "%s"', [ClassName]);
      end;
    end;
  finally
    RefCountCS.Leave;
  end;
end;
{$ENDIF}

{ TFileInfo }

function TFileInfo.GetCreatedDate: TDateTime;
begin
  Result := FCreatedDate;
end;

function TFileInfo.GetFilename: UnicodeString;
begin
  Result := FFilename;
end;

function TFileInfo.GetFilesize: Int64;
begin
  Result := FFilesize;
end;

function TFileInfo.GetIsDir: Boolean;
begin
  Result := FIsDir;
end;

function TFileInfo.GetModifiedDate: TDateTime;
begin
  Result := FModifiedDate;
end;

procedure TFileInfo.SetCreatedDate(const Value: TDateTime);
begin
  FCreatedDate := Value;
end;

procedure TFileInfo.SetFilename(const Value: UnicodeString);
begin
  FFilename := Value;
end;

procedure TFileInfo.SetFilesize(const Value: Int64);
begin
  FFilesize := Value;
end;

procedure TFileInfo.SetIsDir(const Value: Boolean);
begin
  FIsDir := Value;
end;

procedure TFileInfo.SetModifiedDate(const Value: TDateTime);
begin
  FModifiedDate := Value;
end;

  function NopAddref(inst: Pointer): Integer; stdcall;
  begin
    Result := -1;
  end;

  function NopRelease(inst: Pointer): Integer; stdcall;
  begin
    Result := -1;
  end;

  function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj)
    : HResult; stdcall;
  begin
    Result := E_NOINTERFACE;
  end;

procedure SetFlagInterface(var Intf: IInterface);
const
  FlagInterfaceVTable: array [0 .. 2] of Pointer = (@NopQueryInterface,
                                                    @NopAddref,
                                                    @NopRelease);
  FlagInterfaceInstance: Pointer = @FlagInterfaceVTable;

begin
  Intf := IInterface(@FlagInterfaceInstance);
end;


{ Nullable<T> }

constructor TNullable<T>.Create(AValue: T);
begin
  FValue := AValue;
  SetFlagInterface(FHasValue);
end;

class operator TNullable<T>.Equal(ALeft, ARight: TNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue = ARight.HasValue;
end;

(*class operator TNullable<T>.Explicit(Value: TNullable<T>): T;
begin
  Result := Value.Value;
end;*)

function TNullable<T>.GetHasValue: Boolean;
begin
  Result := FHasValue <> nil;
end;

function TNullable<T>.GetValue: T;
begin
  if not HasValue then
    raise Exception.Create('Invalid operation, Nullable type has no value');
  Result := FValue;
end;

function TNullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default (T);
end;

function TNullable<T>.GetValueOrDefault(Default: T): T;
begin
  if not HasValue then
    Result := Default
  else
    Result := FValue;
end;

class operator TNullable<T>.Implicit(Value: TNullable<T>): T;
begin
  Result := Value.Value;
end;

class operator TNullable<T>.Implicit(Value: T): TNullable<T>;
begin
  Result := TNullable<T>.Create(Value);
end;

class operator TNullable<T>.NotEqual(ALeft, ARight: TNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := not Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue <> ARight.HasValue;
end;

{ TJSONSerialisable }

procedure TBaseJSONSerialisable.DoFromJSON(const JSONArray: TJSONArray);
begin
  FLoadingFromJSON := True;
  try
    TDeserializer.ToObject(Self, JSONArray);
  finally
    FLoadingFromJSON := False;
  end;
end;

procedure TBaseJSONSerialisable.DoToJSON(const JSONArray: TJSONArray);
begin
  TSerializer.ToJSON(JSONArray, Self);
end;

procedure TBaseJSONSerialisable.FromJSON(const JSONArray: TJSONArray);
begin
  DoFromJSON(JSONArray);
end;

procedure TBaseJSONSerialisable.FromJSONString(const JSON: String);
var
  JSONArray: TJSONArray;
//  temp: TStringList;
begin
  // Debug - Remove when fixed
(* Temp := TStringlist.Create;
   Temp.Text := JSON;
   Temp.SaveToFile('c:\temp\Reply.json');*)

  JSONArray := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
  try
    FromJSON(JSONArray);
  finally
    FreeAndNil(JSONArray);
  end;
end;

function TBaseJSONSerialisable.GetHash(const FormatJSON: Boolean = False): String;
begin
  Result := THashMD5.GetHashString(ToJSONString(FormatJSON));
end;

function TBaseJSONSerialisable.LoadFromFile(const Filename: String): Boolean;
var
  FileStream: TFileStream;
begin
  Result := FileExists(Filename);

  if Result then
  begin
    FileStream := TFileStream.Create(Filename, fmOpenRead);
    try
      LoadFromStream(FileStream);
    finally
      FreeAndNil(FileStream);
    end;
  end;
end;

procedure TBaseJSONSerialisable.LoadFromStream(const AStream: TStream);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create;
  try
    AStream.Position := 0;

    StringStream.CopyFrom(AStream, AStream.Size);

    FromJSONString(StringStream.DataString);
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TBaseJSONSerialisable.SaveToFile(const Filename: String; const Formatted: Boolean);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(FileStream, Formatted);
  finally
    FreeAndNil(FileStream);
  end;
end;

procedure TBaseJSONSerialisable.SaveToStream(const AStream: TStream; const Formatted: Boolean);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(ToJSONString(Formatted));
  try
    StringStream.Position := 0;

    AStream.CopyFrom(StringStream, StringStream.Size);
  finally
    FreeAndNil(StringStream);
  end;
end;

function TBaseJSONSerialisable.ToJSON: TJSONArray;
begin
  Result := TJSONArray.Create;
  try
    ToJSON(Result);
  except
    on e: Exception do
    begin
      FreeAndNil(Result);

      raise;
    end;
  end;
end;

procedure TBaseJSONSerialisable.ToJSON(const JSONArray: TJSONArray);
begin
  DoToJSON(JSONArray);
end;

function TBaseJSONSerialisable.ToJSONString(const FormatJSON: Boolean): string;
var
  JSONArray: TJSONArray;
begin
  JSONArray := ToJSON;
  try
    if FormatJSON then
    begin
      {$IF CompilerVersion >= 33}
      Result := JSONArray.Format;
      {$ELSE}
      Result := TJson.Format(JSONArray);
      {$ENDIF}
    end
    else
    begin
      Result := JSONArray.ToJSON;
    end;
  finally
    FreeAndNil(JSONArray);
  end;
end;

{ TPTLibObjectList<T> }

constructor TPTLibObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FOwnsObjects := AOwnsObjects;
end;

function TPTLibObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

procedure TPTLibObjectList<T>.Notify(const Value: T;
  Action: TCollectionNotification);
begin
  inherited;

  if OwnsObjects and (Action = cnRemoved) then
    Value.DisposeOf;
end;

procedure TPTLibObjectList<T>.SetOwnsObjects(const Value: Boolean);
begin
  FOwnsObjects := Value;
end;

{ IListSerializable<T> }

function TSerializableList<I, T>.ObjectToInterface(var AObject: T): I;
var
  ObjectClassName, ObjectGUID: String;
begin
  if not Supports(AObject, GetTypeData(TypeInfo(I))^.Guid, Result) then
  begin
    ObjectClassName := AObject.ClassName;
    ObjectGUID := GUIDToString(GetTypeData(TypeInfo(I))^.GUID);

    AObject.Free;
    AObject := nil;

    raise EPTLibUnsupportedInterface.CreateFmt(
      'Object class "%s" does not support interface "%s"',
      [ObjectClassName, ObjectGUID]);
  end;
end;

procedure TSerializableList<I, T>.DoFromJSON(const JSONArray: TJSONArray);
var
  Item: T;
  InterfaceObject: I;
  i: Integer;
begin
  Clear;

  for i := 0 to pred(JSONArray.Count) do
  begin
    // Create the new object
    Item := T.Create;
    try
      // Convert the object to an interface
      InterfaceObject := ObjectToInterface(Item);
    except
      on e: Exception do
      begin
        // Free the object if we didn't manage to get an interface
        FreeAndNil(Item);

        raise;
      end;
    end;

    // Deserialize the JSON to the interface
    InterfaceObject.FromJSONString(JSONArray.Items[i].ToJSON);

    // Add the interface
    Add(InterfaceObject);
  end;
end;

procedure TSerializableList<I, T>.DoToJSON(const JSONArray: TJSONArray);
var
  i: Integer;
  JSONSerialisable: IJSONSerialisable;
begin
  // Serialise all the interfaces to the JSONArray
  for i := 0 to pred(Count) do
  begin
    JSONArray.Add(Items[i].ToJSON);
  end;
end;

{ TSerialPortSettings }

constructor TSerialPortSettings.Create;
begin
  FSerialPort := 'COM20';
  FTimeout := 500;
  FBaudRate := 19200;
  FDataBits := 8;
  FParity := TSerialPortParity.paNone;
  FStopBits := TSerialPortStopBits.ssOne;
  FSoftFlow := False;
  FHardFlow := False;
  FMaxResponseLength := 1000;
  FRTSToggle := False;
end;

function TSerialPortSettings.GetBaudRate: Integer;
begin
  Result := FBaudRate;
end;

function TSerialPortSettings.GetSerialPort: String;
begin
  Result := FSerialPort;
end;

function TSerialPortSettings.GetDataBits: Integer;
begin
  Result := FDataBits;
end;

function TSerialPortSettings.GetDTR: Boolean;
begin
  Result := FDTR;
end;

function TSerialPortSettings.GetHardFlow: Boolean;
begin
  Result := FHardFlow;
end;

function TSerialPortSettings.GetMaxResponseLength: Integer;
begin
  Result := FMaxResponseLength;
end;

function TSerialPortSettings.GetParity: TSerialPortParity;
begin
  Result := FParity;
end;

function TSerialPortSettings.GetRTS: Boolean;
begin
  Result := FRTS;
end;

function TSerialPortSettings.GetRTSToggle: Boolean;
begin
  Result := FRTSToggle;
end;

function TSerialPortSettings.GetSoftFlow: Boolean;
begin
  Result := FSoftFlow;
end;

function TSerialPortSettings.GetStopBits: TSerialPortStopBits;
begin
  Result := FStopBits;
end;

function TSerialPortSettings.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TSerialPortSettings.SetBaudRate(const Value: Integer);
begin
  FBaudRate := Value;
end;

procedure TSerialPortSettings.SetSerialPort(const Value: String);
begin
  FSerialPort := Value;
end;

procedure TSerialPortSettings.SetDataBits(const Value: Integer);
begin
  FDataBits := Value;
end;

procedure TSerialPortSettings.SetDTR(const Value: Boolean);
begin
  FDTR := Value;
end;

procedure TSerialPortSettings.SetHardFlow(const Value: Boolean);
begin
  FHardFlow := Value;
end;

procedure TSerialPortSettings.SetMaxResponseLength(const Value: Integer);
begin
  FMaxResponseLength := Value;
end;

procedure TSerialPortSettings.SetParity(const Value: TSerialPortParity);
begin
  FParity := Value;
end;

procedure TSerialPortSettings.SetRTS(const Value: Boolean);
begin
  FRTS := Value;
end;

procedure TSerialPortSettings.SetRTSToggle(const Value: Boolean);
begin
  FRTSToggle := Value;
end;

procedure TSerialPortSettings.SetSoftFlow(const Value: Boolean);
begin
  FSoftFlow := Value;
end;

procedure TSerialPortSettings.SetStopBits(const Value: TSerialPortStopBits);
begin
  FStopBits := Value;
end;

procedure TSerialPortSettings.SetTimeout(const Value: Integer);
begin
  FTimeout := Value;
end;

{ TActiveInterfacedObject }

procedure TPTLibActiveInterfacedObject.DoActiveChanged(const Value: Boolean);
begin
  // Override
end;

procedure TPTLibActiveInterfacedObject.DoGetInformation(Value: IInformationList);
begin
  // Override
end;

function TPTLibActiveInterfacedObject.GetActive: Boolean;
begin
  Result := FActive;
end;

function TPTLibActiveInterfacedObject.GetInformation(const PropertySeparator: String): IInformationList;
begin
  Result := TInformationList.Create('=');

  DoGetInformation(Result);
end;

procedure TPTLibActiveInterfacedObject.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    DoActiveChanged(Value);

    FActive := Value;
  end;
end;

{ TPTLibActiveParamsInterfacedObject }

procedure TPTLibActiveParamsInterfacedObject.BeforeDestruction;
begin
  inherited;

  Active := False;
end;

constructor TPTLibActiveParamsInterfacedObject.Create;
begin
  FParamCS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TPTLibActiveParamsInterfacedObject.Destroy;
begin
  FreeAndNil(FParamCS);

  inherited;
end;

function TPTLibActiveParamsInterfacedObject.GetParam(const Name: String; const Default: Variant): Variant;
begin
  FParamCS.BeginRead;
  try
    Result := Params.GetParam(Name, Default);
  finally
    FParamCS.EndRead;
  end;
end;

function TPTLibActiveParamsInterfacedObject.GetParams: IParameters;
begin
  if FParams = nil then
  begin
    FParams := TParameters.Create;
  end;

  Result := FParams;
end;

procedure TPTLibActiveParamsInterfacedObject.IncParam(const Name: String; const Value: Cardinal);
var
  CurrentValue: Cardinal;
begin
  FParamCS.BeginWrite;
  try
    CurrentValue := Params.GetParam(Name, 0);

    Params.SetParam(Name, CurrentValue + Value);
  finally
    FParamCS.EndWrite;
  end;
end;

procedure TPTLibActiveParamsInterfacedObject.SetParam(const Name: String; const Value: Variant);
begin
  FParamCS.BeginWrite;
  try
    Params.SetParam(Name, Value);
  finally
    FParamCS.EndWrite;
  end;
end;

initialization
  RefCountCS := TCriticalSection.Create;

finalization
  FreeAndNil(RefCountCS);

end.



