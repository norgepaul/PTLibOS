{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Cache                                          }
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

unit PTLib.ORM.Cache;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Variants, System.Hash,
  System.TypInfo, System.RTTI,

  PTLib.Common.Strings,
  PTLib.Common.TypeInfo,
  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.Files,
  PTLib.Common.Types,

  PTLib.ORM.Classes,
  PTLib.ORM.Interfaces,
  PTLib.ORM.Types;

type
  TCacheOption = (
    NoStoreHash,
    NoStoreJSON,
    NoPrimaryKey,
    FormatJSON);

  TCacheOptions = set of TCacheOption;

  TOnNeedORM = procedure(Sender: TObject; out AORM: IORM) of object;

  TORMObjectCache = class;

  {$M+}
  TORMCacheItem<T: class, constructor> = class(TJSONSerialisable, IORMCacheItem<T>)
  strict private
    FValue: T;
    FHash: String;
    FTimestamp: TDateTime;
    FJSON: String;
    FORMObjectCache: TORMObjectCache;
    FCacheOptions: TCacheOptions;
  private
    function GetValue: T;
    function GetHash: String;
    function GetTimestamp: TDateTime;
    function GetJSON: String;
    function InternalGetJSON: String;
  public
    constructor Create(const ORMObjectCache: TORMObjectCache; const Value: T; const CacheOptions: TCacheOptions);
    destructor Destroy; override;

    property Hash: String read GetHash;
    property JSON: String read GetJSON;
  published
    property Value: T read GetValue;
    property Timestamp: TDateTime read GetTimestamp;
  end;
  {$M-}

  TORMObjectCache = class(TComponent)
  private const
    PersistFileExtension = '.cache';
  private
    FOnNeedORM: TOnNeedORM;
    FCache: TObjectDictionary<TClass, TDictionary<String, IInterface>>;
    FBaseCacheDirectory: String;
    FPersistentClasses: IList<String>;
    FDistributedCacheClasses: IList<String>;
    FDistributedCacheHosts: String;
    FDistributedCacheAPIKey: String;
    FDistributedCacheUpdateInterval: Integer;
    FOnlyUpdateChanges: Boolean;

    function ObjectToKey(const PrimaryKeyValues: TPrimaryKeyArray): String;
    function GetPrimaryKeyValues(const AObject: TObject): TPrimaryKeyArray;
    function IsPersistentClass(const AClass: TClass): Boolean;
  protected
    function GetPersistDirectory<T: Class, constructor>: String;
    function GetPersistFilename<T: Class, constructor>(const Key: String; const CacheItem: IORMCacheItem<T>): String;

    procedure DoOnNeedORM(out AORM: IORM); virtual;

    function GetORM: IORM;
    function GetObjectCache(const AClass: TClass): TDictionary<String, IInterface>;

    procedure DoAddOrSetValue(const AClass: TClass; const Key: String; const ORMCacheObject: IInterface); virtual;
    procedure DoRemoveValue(const AClass: TClass; const Key: String); virtual;
    function DoGetValue<T: Class, constructor>(const Key: String; out ORMCacheObject: IORMCacheItem<T>): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;

    procedure Clear; overload;
    procedure Clear(const AClass: TClass); overload;

    procedure Add<T: Class, constructor>(const AObject: TObject; const CacheOptions: TCacheOptions = []);
    procedure AddList<T: Class, constructor>(const AList: TObject; const CacheOptions: TCacheOptions = []);
    procedure Remove<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray);
    function Get<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; out ORMCacheObject: IORMCacheItem<T>): Boolean; overload;
    function Get<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray): IORMCacheItem<T>; overload;
    function GetHash<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; out Hash: String): Boolean; overload;
    function GetHash<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray): String; overload;
    function GetJSON<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; out JSON: String): Boolean; overload;
    function GetJSON<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray): String; overload;
    function CloneObject<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; out AObject: T): Boolean; overload;
    function GetObject<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray): T; overload;
    function Exists<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray): Boolean;

    procedure SetPersistentClass<T: Class, constructor>;
    procedure SetDistributedClass<T: Class, constructor>;

    property ObjectCache[const AClass: TClass]: TDictionary<String, IInterface> read GetObjectCache;
  published
    property OnNeedORM: TOnNeedORM read FOnNeedORM write FOnNeedORM;

    property BaseCacheDirectory: String read FBaseCacheDirectory write FBaseCacheDirectory;
    property DistributedCacheHosts: String read FDistributedCacheHosts write FDistributedCacheHosts;
    property DistributedCacheAPIKey: String read FDistributedCacheAPIKey write FDistributedCacheAPIKey;
    property DistributedCacheUpdateInterval: Integer read FDistributedCacheUpdateInterval write FDistributedCacheUpdateInterval;
    property OnlyUpdateChanges: Boolean read FOnlyUpdateChanges write FOnlyUpdateChanges;
  end;

implementation

uses
  PTLib.ORM.Serialization;

resourcestring
  StrPleaseAssignOnNeedORM = 'Please assign the OnNeedORM event';

{ TORMCacheObject<T> }

constructor TORMCacheItem<T>.Create(const ORMObjectCache: TORMObjectCache;
  const Value: T; const CacheOptions: TCacheOptions);
begin
  FORMObjectCache := ORMObjectCache;
  FCacheOptions := CacheOptions;
  FValue := Value;
end;

destructor TORMCacheItem<T>.Destroy;
begin
  FreeAndNil(FValue);

  inherited;
end;

function TORMCacheItem<T>.InternalGetJSON: String;
begin
  Result := TORMSerializer.ToJSONString(
    FORMObjectCache.GetORM,
    FValue,
    TCacheOption.FormatJSON in FCacheOptions);
end;

function TORMCacheItem<T>.GetHash: String;
var
  TempJSON: String;
begin
  // Do we already have a hash value?
  if FHash <> '' then
  begin
    // Yes, return it now
    Result := FHash;
  end
  else
  begin
    // Grab the JSON if it already exists
    TempJSON := GetJSON;

    // If we didn't get anything we're not storing JSON so serialise now
    if JSON = '' then
    begin
      TempJSON := InternalGetJSON;
    end;

    // Generate the hash value
    Result := THashMD5.GetHashString(TempJSON);

    // Store the hash unless we've been told not to
    if not (TCacheOption.NoStoreHash in FCacheOptions) then
    begin
      FHash := Result;
    end;
  end;
end;

function TORMCacheItem<T>.GetJSON: String;
var
  TempHash: String;
begin
  // Do we already have a JSON value?
  if FJSON <> '' then
  begin
    // Yes, return it now
    Result := FJSON;
  end
  else
  begin
    Result := InternalGetJSON;

    // Store the JSON unless we've been told not to
    if not (TCacheOption.NoStoreJSON in FCacheOptions) then
    begin
      FJSON := Result;
    end;
  end;
end;

function TORMCacheItem<T>.GetTimestamp: TDateTime;
begin
  Result := FTimestamp;
end;

function TORMCacheItem<T>.GetValue: T;
begin
  Result := FValue;
end;

{ TORMObjectCache }
   
function TORMObjectCache.GetPrimaryKeyValues(const AObject: TObject): TPrimaryKeyArray;
var
  TableMapping: IORMTableMapping;
  i: Integer;
begin
  // Find the ORM table mapping for the object
  TableMapping := GetORM.FindTableMapping(AObject);

  SetLength(Result, TableMapping.PrimaryKeyFieldMappings.Count);

  // Add the primary key values to the TPrimaryKeyArray
  for i := 0 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
  begin
    Result[i] := GetPropValue(
      AObject,
      TableMapping.PrimaryKeyFieldMappings[i].PropertyName);
  end;
end;

function TORMObjectCache.DoGetValue<T>(const Key: String;
  out ORMCacheObject: IORMCacheItem<T>): Boolean;
var
  Dictionary: TDictionary<String, IInterface>;
  TempInterface: IInterface;
begin
  Result := FCache.TryGetValue(T, Dictionary);

  if Result then
  begin
    Result := Dictionary.TryGetValue(Key, TempInterface);

    if Result then
    begin
      ORMCacheObject := TempInterface as IORMCacheItem<T>;
    end;
  end;
end;

procedure TORMObjectCache.Lock;
begin
  TMonitor.Enter(FCache);
end;

function TORMObjectCache.IsPersistentClass(const AClass: TClass): Boolean;
begin
  Result := FPersistentClasses.IndexOf(AClass.ClassName) <> -1;
end;

procedure TORMObjectCache.Add<T>(const AObject: TObject;
  const CacheOptions: TCacheOptions = []);
var
  ORMCacheObject: IORMCacheItem<T>;
  CacheObject: TObject;
  Key: String;
begin
  // Clone the source object
  CacheObject := TORMObjectCloner.Clone(
    GetORM,
    AObject);
  try
    // Create the cache item
    ORMCacheObject := TORMCacheItem<T>.Create(
      Self,
      CacheObject,
      CacheOptions);

    // Calculate the key
    if TCacheOption.NoPrimaryKey in CacheOptions then
    begin
      Key := ObjectToKey([]);
    end
    else
    begin
      Key := ObjectToKey(GetPrimaryKeyValues(AObject));
    end;

    // Add to the cache
    Lock;
    try
      // Set the dictionary value
      DoAddOrSetValue(
        T,
        Key,
        ORMCacheObject);

      if IsPersistentClass(T) then
      begin
        ORMCacheObject.SaveToFile(GetPersistFilename<T>(Key, ORMCacheObject));
      end;
    finally
      Unlock;
    end;
  except
    on e: Exception do
    begin
      // Make sure we free the cache object if an error occurs
      FreeAndNil(CacheObject);

      raise;
    end;
  end;
end;

function TORMObjectCache.Get<T>(const PrimaryKeyValues: TPrimaryKeyArray;
  out ORMCacheObject: IORMCacheItem<T>): Boolean;
var
  Key: String;
begin
  // Calculate the key
  Key := ObjectToKey(PrimaryKeyValues);

  Lock;
  try
    // Try to find the cached object
    Result := DoGetValue<T>(
      Key,
      ORMCacheObject);
  finally
    Unlock;
  end;
end;

function TORMObjectCache.Exists<T>(
  const PrimaryKeyValues: TPrimaryKeyArray): Boolean;
var
  Key: String;
  ORMCacheObject: IORMCacheItem<T>;
begin
  // Calculate the key
  Key := ObjectToKey(PrimaryKeyValues);

  Lock;
  try
    // Try to find the cached object
    Result := DoGetValue<T>(Key, ORMCacheObject);
  finally
    Unlock;
  end;
end;

function TORMObjectCache.ObjectToKey(const PrimaryKeyValues: TPrimaryKeyArray): String;
var
  i: Integer;
begin
  Result := '';

  // Subsequent parts are the primary keys in order of importance
  for i := Low(PrimaryKeyValues) to High(PrimaryKeyValues) do
  begin
    AddToken(
      Result,
      VarToStr(PrimaryKeyValues[i]), '|');
  end;
end;

procedure TORMObjectCache.Remove<T>(
  const PrimaryKeyValues: TPrimaryKeyArray);
var
  Key: String;
begin
  // Calculate the key
  Key := ObjectToKey(PrimaryKeyValues);

  Lock;
  try
    // Remove the cached object
    DoRemoveValue(T, Key);
  finally
    Unlock;
  end;
end;

procedure TORMObjectCache.DoRemoveValue(const AClass: TClass; const Key: String);
var
  Dictionary: TDictionary<String, IInterface>;
begin
  if FCache.TryGetValue(AClass, Dictionary) then
  begin
    Dictionary.Remove(Key);

    if Dictionary.Count = 0 then
    begin
      FCache.Remove(AClass);
    end;
  end;
end;

procedure TORMObjectCache.SetDistributedClass<T>;
begin
  FDistributedCacheClasses.Add(T.ClassName);
end;

procedure TORMObjectCache.SetPersistentClass<T>;
var
  Files: IList<IFileInfo>;
  i: Integer;
  CacheItem: IORMCacheItem<T>;
begin
  FPersistentClasses.Add(T.ClassName);

  // Load any existing persisted objects
  Files := TFileUtils.ScanFiles(GetPersistDirectory<T>, '*' + PersistFileExtension, [IncludeFiles, IncludePaths]);

  for i := 0 to pred(Files.Count) do
  begin
    {$IF CompilerVersion > 32}
      Assert(False, 'How do we fix this in Godzilla?');
    {$ELSE}
      CacheItem := TORMCacheItem<T>.Create(Self, T, []);
    {$ENDIF}

    CacheItem.LoadFromFile(Files[i].Filename);
  end;
end;

procedure TORMObjectCache.Unlock;
begin
  TMonitor.Exit(FCache);
end;

constructor TORMObjectCache.Create(AOwner: TComponent);
begin
  inherited;

  FCache := TObjectDictionary<TClass, TDictionary<String, IInterface>>.Create([doOwnsValues]);
  FPersistentClasses := TList<String>.Create;
  FDistributedCacheClasses := TList<String>.Create;
  FOnlyUpdateChanges := True;
end;

destructor TORMObjectCache.Destroy;
begin
  FreeAndNil(FCache);

  inherited;
end;

procedure TORMObjectCache.DoOnNeedORM(out AORM: IORM);
begin
  AORM := nil;

  if Assigned(FOnNeedORM) then
  begin
    FOnNeedORM(Self, AORM);
  end
  else
  begin
    // Can't continue if we don't have an ORM
    raise EORMEventNotAssigned.Create(StrPleaseAssignOnNeedORM);
  end;
end;

function TORMObjectCache.Get<T>(
  const PrimaryKeyValues: TPrimaryKeyArray): IORMCacheItem<T>;
begin
  Get<T>(PrimaryKeyValues, Result);
end;

function TORMObjectCache.GetPersistDirectory<T>: String;
begin
  if FBaseCacheDirectory = '' then
  begin
    Result := ExtractFileDir(ParamStr(0));
  end
  else
  begin
    Result := FBaseCacheDirectory;
  end;

  Result := IncludeTrailingPathDelimiter(Result) + T.ClassName;
end;

function TORMObjectCache.GetPersistFilename<T>(const Key: String; const CacheItem: IORMCacheItem<T>): String;
begin
  Result := GetPersistDirectory<T>;

  ForceDirectories(Result);

  Result := IncludeTrailingPathDelimiter(Result) + Key + PersistFileExtension;
end;

function TORMObjectCache.GetHash<T>(
  const PrimaryKeyValues: TPrimaryKeyArray): String;
begin
  GetHash<T>(PrimaryKeyValues, Result);
end;

function TORMObjectCache.GetHash<T>(
  const PrimaryKeyValues: TPrimaryKeyArray; out Hash: String): Boolean;
var
  ORMCacheObject: IORMCacheItem<T>;
begin
  Result := Get<T>(PrimaryKeyValues, ORMCacheObject);

  if Result then
  begin
    Hash := ORMCacheObject.Hash;
  end;
end;

function TORMObjectCache.GetJSON<T>(
  const PrimaryKeyValues: TPrimaryKeyArray; out JSON: String): Boolean;
var
  ORMCacheObject: IORMCacheItem<T>;
begin
  Result := Get<T>(PrimaryKeyValues, ORMCacheObject);

  if Result then
  begin
    JSON := ORMCacheObject.JSON;
  end;
end;

function TORMObjectCache.GetJSON<T>(
  const PrimaryKeyValues: TPrimaryKeyArray): String;
begin
  GetJSON<T>(PrimaryKeyValues, Result);
end;

procedure TORMObjectCache.AddList<T>(const AList: TObject;
  const CacheOptions: TCacheOptions);
var
  GetItemMethod: TRTTIMethod;
  ItemCount: Integer;
  i: Integer;
  ItemValue: TValue;
begin
  Clear(T);

  // Make sure this is a list and get it's properties
  GetListPropertiesEx(AList, GetItemMethod, ItemCount);

  // Step through each list item adding it to the cache
  for i := 0 to pred(ItemCount) do
  begin
    Add<T>(GetItemMethod.Invoke(AList, [I]).AsObject, CacheOptions);
  end;
end;

procedure TORMObjectCache.DoAddOrSetValue(const AClass: TClass; const Key: String;
  const ORMCacheObject: IInterface);
var
  Dictionary: TDictionary<String, IInterface>;
begin
  Dictionary := GetObjectCache(AClass);

  Dictionary.AddOrSetValue(Key, ORMCacheObject);
end;

procedure TORMObjectCache.Clear(const AClass: TClass);
begin
  Lock;
  try
    if FCache.ContainsKey(AClass) then    
    begin
      FCache.Remove(AClass); 
    end;
  finally
    Unlock;
  end;
end;

procedure TORMObjectCache.Clear;
begin
  Lock;
  try
    FCache.Clear;
  finally
    Unlock;
  end;
end;

function TORMObjectCache.CloneObject<T>(const PrimaryKeyValues: TPrimaryKeyArray;
  out AObject: T): Boolean;   
var
  Key: String;
  ORMCacheObject: IORMCacheItem<T>;
begin
  // Calculate the key
  Key := ObjectToKey(PrimaryKeyValues);

  Lock;
  try
    // Try to find the cached object
    Result := DoGetValue<T>(
      Key,
      ORMCacheObject);

    if Result then
    begin
      AObject := TORMObjectCloner.Clone<T>(GetORM, ORMCacheObject.Value);
    end;
  finally
    Unlock;
  end;
end;

function TORMObjectCache.GetObject<T>(
  const PrimaryKeyValues: TPrimaryKeyArray): T;
begin
  CloneObject<T>(PrimaryKeyValues, Result);
end;

function TORMObjectCache.GetObjectCache(
  const AClass: TClass): TDictionary<String, IInterface>;
begin
  if not FCache.TryGetValue(AClass, Result) then
  begin
    Result := TDictionary<String, IInterface>.Create;

    FCache.Add(AClass, Result);
  end;
end;

function TORMObjectCache.GetORM: IORM;
begin
  DoOnNeedORM(Result);
end;

end.
