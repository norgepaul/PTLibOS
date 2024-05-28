{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Session                                        }
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

unit PTLib.ORM.Session;

interface

uses
  System.SysUtils, System.Classes, Data.DB, System.Generics.Collections, System.TypInfo, System.RTTI, System.Variants,

  PTLib.Common.Strings,
  PTLib.Common.TypeInfo,

  PTLib.ORM.Cache,
  PTLib.ORM.SQL.Criteria,
  PTLib.ORM.Classes,
  PTLib.ORM.SQL.Connection,
  PTLib.ORM.Types,
  PTLib.ORM.Factory,
  PTLib.ORM.Interfaces;

type
  TORMSessionConnection = class;

  TORMSessionBeforeEvent = procedure(Sender: TORMSessionConnection; const ObjectClass: TClass) of object;
  TORMSessionBeforeSaveEvent = procedure(Sender: TORMSessionConnection; const AObject: TObject) of object;
  TORMSessionDeleteBeforeEvent = procedure(Sender: TORMSessionConnection; const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray; const SoftDelete: Boolean) of object;
  TORMSessionUnDeleteBeforeEvent = procedure(Sender: TORMSessionConnection; const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray) of object;
  TORMSessionSaveBeforeEvent = procedure(Sender: TORMSessionConnection; const AObject: TObject; const RecordCreated: Boolean) of object;
  TOnObjectArrayBeforeEvent = procedure(Sender: TORMSessionConnection; const ObjectClass: TClass; const SQLCriteria: ISQLComparitor) of object;
  TOnSQLBeforeRead = procedure(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset) of object;
  TOnSQLBeforeWrite = procedure(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand) of object;

  TOnSQLAfterRead = procedure(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset; const TickCount: Cardinal) of object;
  TOnSQLAfterWrite = procedure(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand; const TickCount: Cardinal; const RowsAffected: Integer) of object;
  TOnSQLError = procedure(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception) of object;
  TORMSessionAfterEvent = procedure(Sender: TORMSessionConnection; const AObject: TObject; const TickCount: Cardinal) of object;
  TORMSessionDeleteAfterEvent = procedure(Sender: TORMSessionConnection; const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray; const SoftDelete: Boolean; const TickCount: Cardinal) of object;
  TORMSessionUnDeleteAfterEvent = procedure(Sender: TORMSessionConnection; const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray; const TickCount: Cardinal) of object;
  TORMSessionSaveAfterEvent = procedure(Sender: TORMSessionConnection; const AObject: TObject; const RecordCreated: Boolean; const TickCount: Cardinal; const PrimaryKeys: TPrimaryKeyArray) of object;
  TOnObjectArrayAfterEvent = procedure(Sender: TORMSessionConnection; const ORMObjectClass: TClass; const SQLCriteria: ISQLComparitor; const TickCount: Cardinal; const Count: Integer) of object;
  TOnSQLTransactionError = procedure(Sender: TORMSessionConnection; const e: Exception) of object;

  TOnNeedSQLConnection = procedure(Sender: TORMSessionConnection; var SQLConnection: ISQLConnection) of object;

  TORMSessionConnection = class
  strict private
    FSessionOptions: TSessionOptions;
    FSQLConnection: ISQLConnection;
    FORM: IORM;
    FOnSQLBeforeRead: TOnSQLBeforeRead;
    FOnSQLAfterRead: TOnSQLAfterRead;
    FOnSQLBeforeWrite: TOnSQLBeforeWrite;
    FOnSQLAfterWrite: TOnSQLAfterWrite;
    FORMSessionOptions: TORMSessionOptions;
    FSessionEndBehaviour: TORMSessionEndBehaviour;
    FOnBeforeObjectLoaded: TORMSessionBeforeEvent;
    FOnBeforeObjectSaved: TORMSessionBeforeSaveEvent;
    FOnAfterObjectLoaded: TORMSessionAfterEvent;
    FOnAfterObjectSaved: TORMSessionSaveAfterEvent;
    FOnBeforeObjectDeleted: TORMSessionDeleteBeforeEvent;
    FOnAfterObjectDeleted: TORMSessionDeleteAfterEvent;
    FOnBeforeObjectUnDeleted: TORMSessionUnDeleteBeforeEvent;
    FOnAfterObjectUnDeleted: TORMSessionUnDeleteAfterEvent;
    FOnBeforeArrayLoaded: TOnObjectArrayBeforeEvent;
    FOnAfterArrayLoaded: TOnObjectArrayAfterEvent;
    FOnNeedSQLConnection: TOnNeedSQLConnection;
    FOnSQLTransactionError: TOnSQLTransactionError;
    FOnSQLError: TOnSQLError;
    FSQLQueryCallbacks: ISQLQueryCallbacks;
  private
    procedure SetPropertyValue(const AObject: TObject; const PropertyName: String;
      const PropertyValue: Variant);
    procedure SetObjectPrimaryKeysFromPrimaryKeyArray(
      const AObject: TObject; const TableMapping: IORMTableMapping;
      const PrimaryKeyArray: TPrimaryKeyArray);
    function GetObjectSQLSelectCriteria(const ObjectClass: TClass;
      const SQLComparitor: ISQLComparitor; const Options: TORMOptions): ISQLSelectCriteria;
    procedure SetObjectPropertiesFromDataset(const AObject: TObject;
      const SQLDataset: ISQLDataset; const Options: TORMOptions);
    function GetPrimaryKeySQLComparitor(const ORMObjectClass: TClass; const TableMapping: IORMTableMapping;
      const PrimaryKeyValues: TPrimaryKeyArray): ISQLComparitor;
    function GetOneToManyForeignKeySQLComparitor(const ParentObject: TObject;
      const SrcFieldMapping: IORMFieldMapping; const ParentTableMapping: IORMTableMapping): ISQLComparitor;
    function ObjectToInterface<I: IInterface>(var AObject: TObject): I;
    procedure AddSearchFieldsToSQLComparitor(
      const SQLSelectCriteria: ISQLSelectCriteria; const SearchTerm: String;
      const SearchFields: TStrings; const RelationalOperator: TSQLRelationalOperator);
    procedure ProcessSelectCriteriaDatasetOptions(
      const SQLSelectCriteria: ISQLSelectCriteria);
    function FieldHasDefaultValue(const AObject: TObject; const FieldMapping: IORMFieldMapping): Boolean;
    procedure ImportCSVRecord<T: class, constructor>(const TableMapping: IORMTableMapping;
      const ColumnHeaders, ColumnData: String; const KeyFieldNames: Array of String; const Log: TStrings);
  protected
    procedure OnAfterSQLQuery(const SQLQuery: ISQLQuery; const TickCount: Cardinal);
    procedure OnBeforeSQLQuery(const SQLQuery: ISQLQuery);
    procedure OnSQLQueryException(const SQLQuery: ISQLQuery; const e: Exception);

    procedure DoNeedDefaultObjectProperties(const AObject: TObject); virtual;
    procedure DoOnSQLBeforeRead(const SQLDataset: ISQLDataset); virtual;
    procedure DoOnSQLAfterRead(const SQLDataset: ISQLDataset; const TickCount: Cardinal); virtual;
    procedure DoOnSQLBeforeWrite(const SQLCommand: ISQLCommand); virtual;
    procedure DoOnSQLAfterWrite(const SQLCommand: ISQLCommand; const TickCount: Cardinal; const RowsAffected: Integer); virtual;
    procedure DoOnBeforeObjectLoaded(const ObjectClass: TClass); virtual;
    procedure DoOnBeforeObjectSaved(const AObject: TObject); virtual;
    procedure DoOnAfterObjectLoaded(const AObject: TObject; const TickCount: Cardinal); virtual;
    procedure DoOnAfterObjectSaved(const AObject: TObject; const RecordCreated: Boolean; const TickCount: Cardinal; const PrimaryKeys: TPrimaryKeyArray); virtual;
    procedure DoOnBeforeObjectDeleted(const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray; const SoftDelete: Boolean); virtual;
    procedure DoOnAfterObjectDeleted(const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray; const SoftDelete: Boolean; const TickCount: Cardinal); virtual;
    procedure DoOnBeforeObjectUnDeleted(const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray); virtual;
    procedure DoOnAfterObjectUnDeleted(const AClass: TClass; const PrimaryKeys: TPrimaryKeyArray; const TickCount: Cardinal); virtual;
    procedure DoOnBeforeArrayLoaded(const ObjectClass: TClass; const SQLCriteria: ISQLComparitor); virtual;
    procedure DoOnAfterArrayLoaded(const ObjectClass: TClass;
      const SQLCriteria: ISQLComparitor; const TickCount: Cardinal;
      const Count: Integer); virtual;
    procedure DoOnSQLError(const SQLQuery: ISQLQuery; const e: Exception); virtual;
    procedure DoOnNeedSQLConnection(var SQLConnection: ISQLConnection); virtual;
    procedure DoOnSQLTransactionError(const e: Exception); virtual;

    function GetSQLConnection: ISQLConnection;

    function DoIsPrimaryKeyEmpty(const AObject: TObject;
      PrimaryKeyMapping: IORMFieldMapping): Boolean; virtual;
    function DoFixVariantType(const Value: Variant; const FieldMapping: IORMFieldMapping): Variant; virtual;

    function DoGetAttributeMacroValue(const Value: Variant): Variant; virtual;
    function DoGetCurrentTimeStamp: TDateTime; virtual;
    function DoGetCurrentTimeStampUTC: TDateTime; virtual;
    function DoGetObjectPropertyValue(const AObject: TObject; const FieldMapping: IORMFieldMapping;
      const DBField: TField): Variant; virtual;
    function DoProcessSubClass(const AObject: TObject): Boolean; virtual;
//    procedure DoAssignORMObject(const Src, Dst: TObject; const TableMapping: IORMTableMapping = nil); virtual;
    procedure DoAssignORMObjectList(const ORMObjectArray: TObjectArray; const DstList: TObject); virtual;

    function DoGetDatabaseFieldValue(const AObject: TObject;
      const FieldMapping: IORMFieldMapping): Variant; virtual;

    procedure DoDatasetToORMObject(const AObject: TObject;
      const SQLDataset: ISQLDataset; const TableMapping: IORMTableMapping;
      const Options: TORMOptions); virtual;

    procedure DoORMObjectToSQLInsertCriteria(const AObject: TObject;
      const TableMapping: IORMTableMapping;  const ID: Variant; const RecordCreated: Boolean;
      const SQLInsertCriteria: ISQLInsertCriteria;
      const Options: TORMOptions); virtual;

    procedure LoadFromDB(const AObject: TObject; const PrimaryKeyValues: TPrimaryKeyArray; const TableName: String;
      const TableMapping: TORMTableMapping; const Options: TORMOptions); overload;
    procedure SetDeleteFields(const ObjectClass: TClass; const PrimaryKeyValues: TPrimaryKeyArray; const UnDeleting: Boolean);

    procedure LoadListFromDB(var Objects: TObjectArray; const ObjectClass: TClass;
      const SQLWhereComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria; const Options: TORMOptions);
    function SaveToDB(const AObject: TObject; const Options: TORMOptions;
      var RecordCreated: Boolean; const ObjectClass: TClass = nil): TPrimaryKeyArray; overload;

    function GetPropertyValue(const AObject: TObject; const PropertyName: String): Variant;

    procedure LoadObjectInternal<T: Class, constructor>(const Options: TORMOptions; const SQLWhereComparitor: ISQLComparitor; out ORMObject: T); overload;
    procedure DeleteObjectInternal<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; const OverrideSoftDelete: Boolean = False);
    procedure UnDeleteObjectInternal<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray);
    function SaveObjectInternal(const AObject: TObject; const Options: TORMOptions): TPrimaryKeyArray; overload;
    function LoadArrayInternal<T: Class, constructor; L: Class, constructor>(const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria; const Options: TORMOptions; out Count: Integer): L;
    function LoadInterfaceArrayInternal<T: Class, IInterface, constructor; I: IInterface>(const SQLComparitor: ISQLComparitor;
      const OrderBy: ISQLOrderByCriteria; const Options: TORMOptions): IORMList<I>;
  public
    constructor Create(const CustomORM: IORM; const SQLConnection: ISQLConnection;
      const ORMSessionOptions: TORMSessionOptions); virtual;
    destructor Destroy; override;

    property OnBeforeObjectLoaded: TORMSessionBeforeEvent read FOnBeforeObjectLoaded write FOnBeforeObjectLoaded;
    property OnBeforeObjectSaved: TORMSessionBeforeSaveEvent read FOnBeforeObjectSaved write FOnBeforeObjectSaved;
    property OnAfterObjectLoaded: TORMSessionAfterEvent read FOnAfterObjectLoaded write FOnAfterObjectLoaded;
    property OnAfterObjectSaved: TORMSessionSaveAfterEvent read FOnAfterObjectSaved write FOnAfterObjectSaved;
    property OnBeforeObjectDeleted: TORMSessionDeleteBeforeEvent read FOnBeforeObjectDeleted write FOnBeforeObjectDeleted;
    property OnAfterObjectDeleted: TORMSessionDeleteAfterEvent read FOnAfterObjectDeleted write FOnAfterObjectDeleted;
    property OnBeforeObjectUnDeleted: TORMSessionUnDeleteBeforeEvent read FOnBeforeObjectUnDeleted write FOnBeforeObjectUnDeleted;
    property OnAfterObjectUnDeleted: TORMSessionUnDeleteAfterEvent read FOnAfterObjectUnDeleted write FOnAfterObjectUnDeleted;
    property OnBeforeArrayLoaded: TOnObjectArrayBeforeEvent read FOnBeforeArrayLoaded write FOnBeforeArrayLoaded;
    property OnAfterArrayLoaded: TOnObjectArrayAfterEvent read FOnAfterArrayLoaded write FOnAfterArrayLoaded;
    property OnSQLBeforeRead: TOnSQLBeforeRead read FOnSQLBeforeRead write FOnSQLBeforeRead;
    property OnSQLAfterRead: TOnSQLAfterRead read FOnSQLAfterRead write FOnSQLAfterRead;
    property OnSQLBeforeWrite: TOnSQLBeforeWrite read FOnSQLBeforeWrite write FOnSQLBeforeWrite;
    property OnSQLAfterWrite: TOnSQLAfterWrite read FOnSQLAfterWrite write FOnSQLAfterWrite;
    property OnSQLError: TOnSQLError read FOnSQLError write FOnSQLError;
    property OnNeedSQLConnection: TOnNeedSQLConnection read FOnNeedSQLConnection write FOnNeedSQLConnection;
    property OnSQLTransactionError: TOnSQLTransactionError read FOnSQLTransactionError write FOnSQLTransactionError;

    function CreateObject(const AClass: TClass): TObject; overload;

    function LoadObject<T: Class, constructor>(const ID: Variant): T; overload;
    function LoadObject<T: Class, constructor>(const ID: Variant; const Options: TORMOptions): T; overload;
    procedure LoadObject<T: Class, constructor>(const ID: Variant; const Options: TORMOptions; out ORMObject: T); overload;
    function LoadObject<T: Class, constructor>(const Where: ISQLComparitor): T; overload;
    function LoadObject<T: Class, constructor>(const Where: ISQLComparitor; const Options: TORMOptions): T; overload;
    procedure LoadObject<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; const Options: TORMOptions; out ORMObject: T); overload;

    function LoadInterface<T: Class, constructor; I: IInterface>(const ID: Variant): I; overload;
    function LoadInterface<T: Class, constructor; I: IInterface>(const ID: Variant; const Options: TORMOptions): I; overload;
    procedure LoadInterface<T: Class, constructor; I: IInterface>(const ID: Variant; const Options: TORMOptions; out ORMInterface: I); overload;
    function LoadInterface<T: Class, constructor; I: IInterface>(const Where: ISQLComparitor): I; overload;
    function LoadInterface<T: Class, constructor; I: IInterface>(const Where: ISQLComparitor; const Options: TORMOptions): I; overload;
    procedure LoadInterface<T: Class, constructor; I: IInterface>(const PrimaryKeyValues: TPrimaryKeyArray; const Options: TORMOptions; out ORMInterface: I); overload;

    function LoadObjectList<T: Class, constructor; L: Class, constructor>(const SQLComparitor: ISQLComparitor = nil): L; overload;
    function LoadObjectList<T: Class, constructor; L: Class, constructor>(const SQLComparitor: ISQLComparitor; const Options: TORMOptions): L; overload;
    function LoadObjectList<T: Class, constructor; L: Class, constructor>(const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria; const Options: TORMOptions): L; overload;
    function LoadArray<T: Class, IInterface, constructor; I: IInterface>(const SQLComparitor: ISQLComparitor = nil): IORMList<I>; overload;
    function LoadArray<T: Class, IInterface, constructor; I: IInterface>(const SQLComparitor: ISQLComparitor; const Options: TORMOptions): IORMList<I>; overload;
    function LoadArray<T: Class, IInterface, constructor; I: IInterface>(const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria; const Options: TORMOptions): IORMList<I>; overload;

    function LoadDataset(const Criteria: ISQLSelectCriteria): ISQLDataset; overload;
    function LoadDataset<T: Class>(const Options: TORMOptions = []): ISQLDataset; overload;
    function LoadDataset<T: Class>(const SQLComparitor: ISQLComparitor; const Options: TORMOptions = []): ISQLDataset; overload;
    function GenerateObjectSQLSelectCriteria<T: Class>(const Options: TORMOptions = []): ISQLSelectCriteria;

    function SaveObject(const AObject: TObject): TPrimaryKeyArray; overload;
    function SaveObject(const AObject: TObject; const Options: TORMOptions): TPrimaryKeyArray; overload;

    function SaveInterface(const ORMInterface: IInterface): TPrimaryKeyArray; overload;
    function SaveInterface(const ORMInterface: IInterface; const Options: TORMOptions): TPrimaryKeyArray; overload;

    procedure DeleteObject<T: Class, constructor>(const ID: Variant; const OverrideSoftDelete: Boolean = False); overload;
    procedure DeleteObject<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray; const OverrideSoftDelete: Boolean = False); overload;

    procedure UnDeleteObject<T: Class, constructor>(const ID: Variant); overload;
    procedure UnDeleteObject<T: Class, constructor>(const PrimaryKeyValues: TPrimaryKeyArray); overload;

    procedure ImportCSVData<T: class, constructor>(const Data, Log: TStrings; const KeyFieldNames: array of String);

    function NewCommand(const SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean = True): ISQLCommand; overload;
    function NewCommand(const SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean = True): ISQLCommand; overload;
    function NewCommand(const SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean = True): ISQLCommand; overload;
    function NewCommand(const SQL: string; const ParamNames: array of String; const ParamValues: array of Variant;
      const Execute: TCommandExecutType = ceExecute): ISQLCommand; overload;
    function NewDataset(const QueryCriteria: ISQLSelectCriteria; const SQLDatasetClass: TSQLDatasetClass = nil): ISQLDataset; overload;
    function NewDataset(const SQL: string; const ParamNames: array of String;
       const ParamValues: array of Variant; const SQLDatasetClass: TSQLDatasetClass = nil; const Open: Boolean = True): ISQLDataset; overload;
    function NewDataset(const SQL: string; const SQLDatasetClass: TSQLDatasetClass = nil; const Open: Boolean = True): ISQLDataset; overload;

    function GetObjectPropertiesAsText(const AObject: TObject;
      const Indent: String = '  |-'; const ObjectClass: TClass = nil): String;
    function IsPrimaryKeyEmpty(const AObject: TObject): Boolean;

    procedure Start; virtual;
    procedure Commit; virtual;
    procedure Rollback; virtual;

    property ORM: IORM read FORM;
    property SQLConnection: ISQLConnection read GetSQLConnection;

    procedure DoResetSession; virtual;

    property SessionOptions: TSessionOptions read FSessionOptions;
  end;
  TORMSessionConnectionClass = class of TORMSessionConnection;

  IORMSession = interface
    ['{487CF5C4-DF49-43AB-823C-579DCEA703DA}']
    function GetConnection: TORMSessionConnection;

    property Connection: TORMSessionConnection read GetConnection;
  end;

  TORMSession = class(TInterfacedObject, IORMSession)
  private
    FORMSessionConnection: TORMSessionConnection;

    function GetConnection: TORMSessionConnection;
  public
    constructor Create(const ORM: IORM; const ORMSessionConnectionClass: TORMSessionConnectionClass;
      const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions); virtual;
    destructor Destroy; override;

    property Connection: TORMSessionConnection read GetConnection;
  end;

  TORMSessionObject = class(TObject)
  strict private
    FORMSessionConnection: TORMSessionConnection;
  protected
   property ORMSessionConnection: TORMSessionConnection read FORMSessionConnection;
  public
    constructor Create(const ORMSessionConnection: TORMSessionConnection); virtual;
  end;

implementation

uses
  System.DateUtils,

  PTLib.ORM.Serialization;

resourcestring
  StrUnknownObjectForTable = '[DB] Unknown ID for table "%s"';
  StrUnknownIDDForTable = '[DB] Unknown ID (%d) for table "%s"';
  StrMissingRecordForProperty = 'Missing record for property %s, ID %d';
  StrPrimaryKeyValueCo = 'Primary key value count must equal primary key field mapping count';
  StrOnlyOnePrimaryKey = 'Only one primary key allowed when saving child objects - %s.%s';
  StrIncorrectNumberOfPrimaryKeyValues = 'Incorrect number of primary key values for %s';
  StrParentORMClasses = 'Parent ORM classes can only use one primary key';
  StrSubClassNoAttribute = 'SubClass "%s.%s" has no relationship attribute';
  StrMissingAttributeOneToMany = 'Missing OneToMany attribute for %s';
  StrMissingOneToManyForeignKey = 'Missing OneToMany foreign key field names for %s';
  StrNoDoAssignORMObject = 'No DoAssignORMObjectList override to handle a %s list';
  StrInvalidFieldMapping = 'Invalid field mapping for %s';
  StrPrimaryAndForeign = 'Primary and foreign key counts do not match for %s';
  StrSearchTermNotCompatible = 'Search term "%s" is not compatible with the field "%s"';
  StrSearchTermTooLong = 'Search term "%s" is too long for field "%s"';

procedure TORMSessionConnection.Start;
begin
  if (not GetSQLConnection.InTransaction) and
     (not (ManualTransactions in FORMSessionOptions)) then
  begin
    GetSQLConnection.BeginTransaction;
  end;
end;

procedure TORMSessionConnection.UnDeleteObject<T>(const ID: Variant);
begin
  UnDeleteObject<T>([ID]);
end;

procedure TORMSessionConnection.UnDeleteObject<T>(
  const PrimaryKeyValues: TPrimaryKeyArray);
begin
  UnDeleteObjectInternal<T>(PrimaryKeyValues);
end;

procedure TORMSessionConnection.UnDeleteObjectInternal<T>(
  const PrimaryKeyValues: TPrimaryKeyArray);
var
  ORMObject: T;
  TickCount: Cardinal;
begin
  DoOnBeforeObjectUnDeleted(T, PrimaryKeyValues);

  TickCount := TThread.GetTickCount;

  SetDeleteFields(T, PrimaryKeyValues, True);

  DoOnAfterObjectUnDeleted(T, PrimaryKeyValues, TThread.GetTickCount - TickCount);
end;

procedure TORMSessionConnection.Rollback;
begin
  if (GetSQLConnection.InTransaction) and
     (not (ManualTransactions in FORMSessionOptions)) then
  try
    GetSQLConnection.RollbackTransaction;
  except
    on e: Exception do
    begin
      DoOnSQLTransactionError(e);

      raise;
    end;
  end;
end;

function TORMSessionConnection.LoadObjectList<T, L>(
  const SQLComparitor: ISQLComparitor): L;
begin
  Result := LoadObjectList<T, L>(
    SQLComparitor,
    []);
end;

function TORMSessionConnection.LoadObjectList<T, L>(const SQLComparitor: ISQLComparitor;
  const Options: TORMOptions): L;
begin
  Result := LoadObjectList<T, L>(
    SQLComparitor,
    nil,
    Options);
end;

function TORMSessionConnection.LoadArrayInternal<T, L>(
  const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria;
  const Options: TORMOptions; out Count: Integer): L;

var
  Objects: TObjectArray;
  i: Integer;
  RTTIMethod: TRttiMethod;
  RTTIContext: TRttiContext;
  RTTIType: TRttiType;
  LParams: TArray<TRttiParameter>;
  Found: Boolean;
begin
  { TODO :
    When this function is called it finds the correct Add(xxx) method. However,
    it returns the Add method (no params) instead. I have absolutely no idea why.
    Adding the same code below works. Figure out why! }

  (*FindRTTIMethod(
    Result,
    'Add',
    [TypeInfo(T)],
    RTTIMethod);*)

  // ------------------
  Result := nil;
  Found := False;

  RTTIContext := TRttiContext.Create;
  RttiType := RTTIContext.GetType(L);

  for RTTIMethod in RttiType.GetMethods do
  begin
    if SameText(RTTIMethod.Name, 'Add') then
    begin
      LParams := RTTIMethod.GetParameters;

      if (length(LParams) = 1) and
         (LParams[0].ParamType.Handle = TypeInfo(T)) then
      begin
        Found := True;

        Break;
      end;
    end;
  end;

  if not Found then
    raise Exception.Create('Result list does not contain an Add method with the correct parameter type');

  try
    LoadListFromDB(
      Objects,
      T,
      SQLComparitor,
      OrderBy,
      Options);

    Result := L.Create;

    for i := low(Objects) to high(Objects) do
      RTTIMethod.Invoke(Result, [Objects[i]]);

    Count := length(Objects);
  except
    on e: Exception do
    begin
      if not (NoException in Options) then
        raise;
    end;
  end;
end;

function TORMSessionConnection.LoadArray<T, I>(
  const SQLComparitor: ISQLComparitor): IORMList<I>;
begin
  Result := LoadInterfaceArrayInternal<T, I>(
    SQLComparitor,
    nil,
    []);
end;

function TORMSessionConnection.LoadArray<T, I>(
  const SQLComparitor: ISQLComparitor; const Options: TORMOptions): IORMList<I>;
begin
  Result := LoadInterfaceArrayInternal<T, I>(
    SQLComparitor,
    nil,
    Options);
end;

procedure TORMSessionConnection.LoadInterface<T, I>(const ID: Variant;
  const Options: TORMOptions; out ORMInterface: I);
var
  AObject: TObject;
begin
  LoadObject<T>(ID, Options, AObject);

  if AObject = nil then
    ORMInterface := nil
  else
    ORMInterface := ObjectToInterface<I>(AObject);
end;

function TORMSessionConnection.LoadInterface<T, I>(const ID: Variant;
  const Options: TORMOptions): I;
begin
  LoadInterface<T, I>(ID, Options, Result);
end;

function TORMSessionConnection.LoadInterface<T, I>(
  const Where: ISQLComparitor): I;
begin
  Result := LoadInterface<T, I>(Where, []);
end;

procedure TORMSessionConnection.LoadInterface<T, I>(
  const PrimaryKeyValues: TPrimaryKeyArray; const Options: TORMOptions;
  out ORMInterface: I);
var
  AObject: TObject;
begin
  LoadObject<T>(PrimaryKeyValues, Options, AObject);

  if AObject = nil then
    ORMInterface := nil
  else
    ORMInterface := ObjectToInterface<I>(AObject);
end;

function TORMSessionConnection.LoadInterface<T, I>(const Where: ISQLComparitor;
  const Options: TORMOptions): I;
var
  AObject: TObject;
begin
  LoadObjectInternal<T>(Options, Where, AObject);

  if AObject = nil then
    Result := nil
  else
    Result := ObjectToInterface<I>(AObject);
end;

function TORMSessionConnection.ObjectToInterface<I>(var AObject: TObject): I;
var
  ObjectClassName, ObjectGUID: String;
begin
  if not Supports(AObject, GetTypeData(TypeInfo(I))^.Guid, Result) then
  begin
    ObjectClassName := AObject.ClassName;
    ObjectGUID := GUIDToString(GetTypeData(TypeInfo(I))^.GUID);

    AObject.Free;
    AObject := nil;

    raise EORMUnsupportedInterface.CreateFmt('Object class "%s" does not support interface "%s"',
                                             [ObjectClassName,
                                              ObjectGUID]);
  end;
end;

function TORMSessionConnection.LoadInterfaceArrayInternal<T, I>(
  const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria;
  const Options: TORMOptions): IORMList<I>;
var
  Objects: TObjectArray;
  n: Integer;
  IntObj: I;
begin
  try
    LoadListFromDB(
      Objects,
      T,
      SQLComparitor,
      OrderBy,
      Options);

    Result := TORMList<I>.Create;

    for n := Low(Objects) to High(Objects) do
    begin
      if Objects[n] <> nil then
        Result.Add(ObjectToInterface<I>(Objects[n]));
    end;
  except
    on e: Exception do
    begin
      if not (NoException in Options) then
        raise;
    end;
  end;
end;

procedure TORMSessionConnection.AddSearchFieldsToSQLComparitor(
  const SQLSelectCriteria: ISQLSelectCriteria;
  const SearchTerm: String; const SearchFields: TStrings;
  const RelationalOperator: TSQLRelationalOperator);
var
  i, n: Integer;
  SearchComparitor: ISQLComparitor;
  TableMapping: IORMTableMapping;
  TableName, FieldName: String;
  FieldMapping: IORMFieldMapping;
  SearchTerms: TStringList;
  DummyInt: Integer;
  DummyInt64: Int64;
  DummyFloat: Double;
  AddedSearchTerm: Boolean;
  SearchTermError: String;
begin
  if (SearchFields.Count > 0) and (SearchTerm <> '') then
  begin
    AddedSearchTerm := False;
    SearchTermError := '';

    SearchTerms := TStringList.Create;
    try
      SearchTerms.Delimiter := ' ';
      SearchTerms.CommaText := SearchTerm;

      for n := 0 to pred(SearchTerms.Count) do
      begin
        SearchComparitor := nil;

        for i := 0 to pred(SearchFields.Count) do
        begin
          if pos('.', SearchFields[i]) > 0 then
          begin
            TableName := copy(SearchFields[i], 1, pos('.', SearchFields[i]) - 1);
            FieldName := copy(SearchFields[i], pos('.', SearchFields[i]) + 1, MaxInt);
          end
          else
          begin
            TableName := SQLSelectCriteria.TableName;
            FieldName := SearchFields[i];
          end;

          TableMapping := FORM.FindTableMappingByTableName(TableName);

          FieldMapping := TableMapping.FieldMappings.FindByDatabaseFieldName(FieldName);

          if ((FieldMapping.DatabaseFieldSize = 0) or
              (FieldMapping.DatabaseFieldSize >= length(SearchTerms[n]))) then
          begin
            if (not ((FieldMapping.PropertyInfo.Kind in [tkInteger]) and (not TryStrToInt(SearchTerms[n], DummyInt)))) and
               (not ((FieldMapping.PropertyInfo.Kind in [tkInt64]) and (not TryStrToInt64(SearchTerms[n], DummyInt64)))) and
               (not ((FieldMapping.PropertyInfo.Kind in [tkFloat]) and (not TryStrToFloat(SearchTerms[n], DummyFloat)))) then
            begin
              AddedSearchTerm := True;

              if SearchComparitor = nil then
              begin
                SearchComparitor := TOFac.Comparitor
                  (SearchFields[i], RelationalOperator, SearchTerms[n])
              end
              else
              begin
                SearchComparitor.&Or
                  (SearchFields[i], RelationalOperator, SearchTerms[n]);
              end;
            end
            else
            begin
              AddToken(SearchTermError, format(StrSearchTermNotCompatible, [SearchTerms[n], FieldName]), #10);
            end;
          end
          else
          begin
            AddToken(SearchTermError, format(StrSearchTermTooLong, [SearchTerms[n], FieldName]), #10);
          end;
        end;

        if SearchComparitor <> nil then
        begin
          if SQLSelectCriteria.GetWhereCriteria = nil then
            SQLSelectCriteria.Where(SearchComparitor)
          else
            SQLSelectCriteria.GetWhereCriteria.&And(SearchComparitor);
        end;
      end;

      if not AddedSearchTerm then
      begin
        raise EORMIncompatibleSearchTerms.Create('Unable to perform the search:' + #10 + #10 + SearchTermError);
      end;
    finally
      FreeAndNil(SearchTerms);
    end;
  end;
end;

procedure TORMSessionConnection.ProcessSelectCriteriaDatasetOptions(
  const SQLSelectCriteria: ISQLSelectCriteria);
var
  OriginalOrderByCriteria: ISQLOrderByCriteria;
  OriginalFieldCriteriaList: IORMList<ISQLFieldNameItem>;
  i: Integer;
  SQLDataset: ISQLDataset;
begin
  SQLSelectCriteria.SetParam(FORM.GetUTCOffsetParameterName, SessionOptions.UTCOffset);

  if SessionOptions.Search.SearchTerm <> '' then
  begin
    AddSearchFieldsToSQLComparitor(
      SQLSelectCriteria,
      SessionOptions.Search.SearchTerm,
      SessionOptions.Search.SearchFields,
      SessionOptions.Search.SearchOperator);
  end;

  // Set the timestamp criteria
  if (SessionOptions.TimestampOptions.Enabled) and
     (SessionOptions.TimestampOptions.DateTimeFieldName <> '') then
  begin
    if SessionOptions.TimestampOptions.FirstDateTime <> 0 then
      SQLSelectCriteria.Where(TOFac.Comparitor(
        SQLSelectCriteria.TableName + '.' + SessionOptions.TimestampOptions.DateTimeFieldName,
          GreaterThanOrEqualTo,
          SessionOptions.TimestampOptions.FirstDateTime,
          vtParameter));

    if SessionOptions.TimestampOptions.LastDateTime <> 0 then
      SQLSelectCriteria.Where(TOFac.Comparitor(
        SQLSelectCriteria.TableName + '.' + SessionOptions.TimestampOptions.DateTimeFieldName,
          LessThanOrEqualTo,
          SessionOptions.TimestampOptions.LastDateTime,
          vtParameter));
  end;

  // Set the paging SQLSelectCriteria
  if SessionOptions.Paging.PagingType in [poPaged, poPagedReturnTotalCount] then
  begin
    // If the PageType is poPagedReturnTotalCount, we need
    // to query the database for the record count
    if SessionOptions.Paging.PagingType = poPagedReturnTotalCount then
    begin
      OriginalOrderByCriteria := SQLSelectCriteria.GetOrderByCriteria;
      OriginalFieldCriteriaList := TORMList<ISQLFieldNameItem>.Create;

      for i := 0 to pred(SQLSelectCriteria.GetFieldCriteriaList.Count) do
        OriginalFieldCriteriaList.Add(SQLSelectCriteria.GetFieldCriteriaList[i]);

      SQLSelectCriteria.GetFieldCriteriaList.Clear;
      SQLSelectCriteria.OrderBy(nil);

      SQLSelectCriteria.Field('Count(*)');
      try
        SQLDataset := NewDataset(SQLSelectCriteria);

        SessionOptions.Paging.RecordCount := SQLDataset.Fields[0].AsInteger;
      finally
        // Restore the original SQLSelectCriteria
        SQLSelectCriteria.OrderBy(OriginalOrderByCriteria);

        SQLSelectCriteria.GetFieldCriteriaList.Clear;

        for i := 0 to pred(OriginalFieldCriteriaList.Count) do
          SQLSelectCriteria.GetFieldCriteriaList.Add(OriginalFieldCriteriaList[i]);
      end;
    end;

    SQLSelectCriteria.First(SessionOptions.Paging.PageSize);
    SQLSelectCriteria.Skip((SessionOptions.Paging.Page - 1) * SessionOptions.Paging.PageSize);
  end;
end;

function TORMSessionConnection.LoadDataset(
  const Criteria: ISQLSelectCriteria): ISQLDataset;
begin
  ProcessSelectCriteriaDatasetOptions(Criteria);

  Result := NewDataset(Criteria);
end;

function TORMSessionConnection.LoadDataset<T>(const SQLComparitor: ISQLComparitor;
  const Options: TORMOptions): ISQLDataset;
begin
  Result := LoadDataset(GetObjectSQLSelectCriteria(T, SQLComparitor, Options));
end;

function TORMSessionConnection.LoadDataset<T>(
  const Options: TORMOptions): ISQLDataset;
begin
  Result := LoadDataset(GetObjectSQLSelectCriteria(T, nil, Options));
end;

procedure TORMSessionConnection.DoDatasetToORMObject(const AObject: TObject;
  const SQLDataset: ISQLDataset; const TableMapping: IORMTableMapping;
  const Options: TORMOptions);
var
  i: Integer;
  Field: TField;
  NewValue: Variant;
  AStream: TStream;
  BlobStream: TStream;
  PropertyObject: TObject;
begin
  if SQLDataset.IsEmpty then
  begin
    raise EORMObjectNotFoundError.CreateFmt(StrUnknownObjectForTable, [TableMapping.TableName]);
  end
  else
  begin
    for i := 0 to pred(TableMapping.FieldMappings.Count) do
    begin
      if (TableMapping.FieldMappings[i].PropertyName <> '') and
         (TableMapping.FieldMappings[i].DatabaseFieldName <> '') then
      begin
        Field := SQLDataset.FieldByName(TableMapping.FieldMappings[i].DatabaseFieldName);

        if TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass then
        begin
          PropertyObject := GetPropertyObject(AObject, TableMapping.FieldMappings[i].PropertyName);

          if PropertyObject is TStream then
          begin
            AStream := TStream(PropertyObject);
            BlobStream := Field.DataSet.CreateBlobStream(Field, TBlobStreamMode.bmRead);
            try
              BlobStream.Position := 0;
              AStream.CopyFrom(BlobStream, BlobStream.Size);
            finally
              FreeAndNil(BlobStream);
            end;
          end;
        end
        else
        begin
          NewValue := DoGetObjectPropertyValue(AObject, TableMapping.FieldMappings[i], Field);

          SetPropertyValue(AObject, TableMapping.FieldMappings[i].PropertyName, NewValue);
        end;
      end;
    end;
  end;
end;

function TORMSessionConnection.GenerateObjectSQLSelectCriteria<T>(
  const Options: TORMOptions): ISQLSelectCriteria;
begin
  Result := GetObjectSQLSelectCriteria(T, nil, Options);
end;

function TORMSessionConnection.GetObjectPropertiesAsText(const AObject: TObject;
  const Indent: String; const ObjectClass: TClass): String;

const
  CRLF = #13#10;

  procedure AddProperty(const Name: String; const Value: Variant);
  var
    RealValue: String;
  begin
    if Value = Null then
      RealValue := 'Null'
    else
      RealValue := VarToStr(Value);

    AddToken(Result,
             Indent +
             format('%s=%s', [Name,
                              RealValue]),
             CRLF);
  end;

var
  TableMapping: IORMTableMapping;
  i: Integer;
  ParentText: String;
  ChildObject: TObject;
  RealClass: TClass;
begin
  Result := '';

  if ObjectClass = nil then
    RealClass := AObject.ClassType
  else
    RealClass := ObjectClass;

  if (RealClass <> nil) and
     (RealClass.InheritsFrom(TObject)) then
  begin
    TableMapping := FORM.FindTableMapping(RealClass.ClassName, False);

    if TableMapping <> nil then
    begin
      for i := 0 to pred(TableMapping.FieldMappings.Count) do
      begin
        if TableMapping.FieldMappings[i].PropertyInfo <> nil then
        begin
          if TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass then
          begin
            ChildObject := GetPropertyObject(AObject, TableMapping.FieldMappings[i].PropertyName);

            AddToken(Result, Indent + TableMapping.FieldMappings[i].PropertyName + '=[' + ChildObject.ClassName + ']', CRLF);

            Result := Result + CRLF + GetObjectPropertiesAsText(ChildObject, '  '  + Indent, nil);

            { TODO : Not sure why we need to do this again, but the previous TableMapping is currupted for some reason! }
            TableMapping := FORM.FindTableMapping(RealClass.ClassName, False);
          end
          else
          begin
            AddProperty(TableMapping.FieldMappings[i].PropertyName,
                        GetPropValue(AObject,
                                     TableMapping.FieldMappings[i].PropertyName,
                                     True));
          end;
        end;
      end;

      ParentText := GetObjectPropertiesAsText(AObject, Indent, RealClass.ClassParent);

      if ParentText <> '' then
        Result := Result + CRLF;

      Result := Result + ParentText;
    end;
  end;
end;

function TORMSessionConnection.GetPropertyValue(const AObject: TObject;
  const PropertyName: String): Variant;
begin
  Result := GetPropValue(AObject, PropertyName);
end;

function TORMSessionConnection.GetSQLConnection: ISQLConnection;
begin
  if FSQLConnection = nil then
    DoOnNeedSQLConnection(FSQLConnection);

  if FSQLConnection = nil then
    raise EORMNoConnectionavailable.Create('Please assign an SQL Connection')
  else
    Result := FSQLConnection;
end;

procedure TORMSessionConnection.SetPropertyValue(const AObject: TObject;
  const PropertyName: String; const PropertyValue: Variant);
begin
  SetPropValue(AObject, PropertyName, PropertyValue);
end;

procedure TORMSessionConnection.OnBeforeSQLQuery(const SQLQuery: ISQLQuery);
var
  SQLDataset: ISQLDataset;
  SQLCommand: ISQLCommand;
begin
  if Supports(SQLQuery, ISQLDataset, SQLDataset) then
    DoOnSQLBeforeRead(SQLDataset) else
  if Supports(SQLQuery, ISQLCommand, SQLCommand) then
    DoOnSQLBeforeWrite(SQLCommand);
end;

procedure TORMSessionConnection.OnAfterSQLQuery(const SQLQuery: ISQLQuery; const TickCount: Cardinal);
var
  SQLDataset: ISQLDataset;
  SQLCommand: ISQLCommand;
begin
  if Supports(SQLQuery, ISQLDataset, SQLDataset) then
    DoOnSQLAfterRead(SQLDataset, TickCount) else
  if Supports(SQLQuery, ISQLCommand, SQLCommand) then
    DoOnSQLAfterWrite(SQLCommand, TickCount, SQLCommand.RowsAdded);
end;

procedure TORMSessionConnection.OnSQLQueryException(const SQLQuery: ISQLQuery;
  const e: Exception);
begin
  DoOnSQLError(SQLQuery, e);
end;

function TORMSessionConnection.NewCommand(
  const SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean = True): ISQLCommand;
begin
  Result := SQLConnection.NewCommand(
    SQLDeleteCriteria,
    Execute,
    FSQLQueryCallbacks);
end;

function TORMSessionConnection.NewCommand(
  const SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean = True): ISQLCommand;
begin
  Result := SQLConnection.NewCommand(
    SQLInsertCriteria,
    Execute,
    FSQLQueryCallbacks);
end;

function TORMSessionConnection.NewCommand(
  const SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean = True): ISQLCommand;
begin
  Result := SQLConnection.NewCommand(
    SQLUpdateCriteria,
    Execute,
    FSQLQueryCallbacks);
end;

function TORMSessionConnection.DoFixVariantType(const Value: Variant;
  const FieldMapping: IORMFieldMapping): Variant;
var
  PropName: String;
begin
  PropName :=  GetTypeName(FieldMapping.PropertyInfo);

  if (SameText(PropName, 'date')) or
     (SameText(PropName, 'tdate')) or
     (SameText(PropName, 'time')) or
     (SameText(PropName, 'ttime')) or
     (SameText(PropName, 'datetime')) or
     (SameText(PropName, 'tdatetime')) then
  begin
    if Value = 0 then
      Result := 0
    else
      Result := TDateTime(Value);
  end
  else
  begin
    Result := Value;
  end;
end;

function TORMSessionConnection.DoGetObjectPropertyValue(const AObject: TObject;
  const FieldMapping: IORMFieldMapping; const DBField: TField): Variant;
begin
  if FieldMapping.PropertyInfo.Kind <> tkClass then
  begin
    if DBField.IsNull then
    begin
      // Don't change as we're not using nullable types. Best we can do is leave
      // the value alone!
      case FieldMapping.PropertyInfo.Kind of
        tkInteger,
        tkInt64,
        tkEnumeration,
        tkFloat: Result := 0;
        tkChar,
        tkString,
        tkWChar,
        tkLString,
        tkWString,
        tkUString: Result := '';
      end;
    end
    else
    begin
      if FieldMapping.PropertyInfo.Kind = TTypeKind.tkEnumeration then
      begin
        if SameText(GetTypeInfoName(FieldMapping.PropertyInfo), 'boolean') then
          Result := DBField.AsString = SQLConnection.SQLEngine.GetTokenBooleanTrue
        else
          Result := DBField.AsVariant;
      end
      else
      begin
        Result := DoFixVariantType(DBField.AsVariant,
                                   FieldMapping);
      end;
    end;
  end;
end;

procedure TORMSessionConnection.DoNeedDefaultObjectProperties(
  const AObject: TObject);
begin
  // Override to set default object properties
end;

procedure TORMSessionConnection.DoOnAfterObjectDeleted(const AClass: TClass;
  const PrimaryKeys: TPrimaryKeyArray; const SoftDelete: Boolean; const TickCount: Cardinal);
begin
  if Assigned(FOnAfterObjectDeleted) then
    FOnAfterObjectDeleted(Self, AClass, PrimaryKeys, SoftDelete, TickCount);
end;

procedure TORMSessionConnection.DoOnAfterObjectLoaded(
  const AObject: TObject; const TickCount: Cardinal);
begin
  if Assigned(FOnAfterObjectLoaded) then
    FOnAfterObjectLoaded(Self, AObject, TickCount);
end;

procedure TORMSessionConnection.DoOnAfterObjectSaved(
  const AObject: TObject; const RecordCreated: Boolean; const TickCount: Cardinal;
  const PrimaryKeys: TPrimaryKeyArray);
begin
  if Assigned(FOnAfterObjectSaved) then
    FOnAfterObjectSaved(
      Self,
      AObject,
      RecordCreated,
      TickCount,
      PrimaryKeys);
end;

procedure TORMSessionConnection.DoOnAfterObjectUnDeleted(const AClass: TClass;
  const PrimaryKeys: TPrimaryKeyArray; const TickCount: Cardinal);
begin
  if Assigned(FOnAfterObjectUnDeleted) then
    FOnAfterObjectUnDeleted(Self, AClass, PrimaryKeys, TickCount);
end;

procedure TORMSessionConnection.DoOnNeedSQLConnection(
  var SQLConnection: ISQLConnection);
begin
  if Assigned(FOnNeedSQLConnection) then
    FOnNeedSQLConnection(
      Self,
      SQLConnection);
end;

procedure TORMSessionConnection.DoOnBeforeObjectDeleted(const AClass: TClass;
  const PrimaryKeys: TPrimaryKeyArray; const SoftDelete: Boolean);
begin
  if Assigned(FOnBeforeObjectDeleted) then
    FOnBeforeObjectDeleted(Self, AClass, PrimaryKeys, SoftDelete);
end;

procedure TORMSessionConnection.DoOnBeforeObjectLoaded(const ObjectClass: TClass);
begin
  if Assigned(FOnBeforeObjectLoaded) then
    FOnBeforeObjectLoaded(Self, ObjectClass);
end;

procedure TORMSessionConnection.DoOnBeforeObjectSaved(const AObject: TObject);
begin
  if Assigned(FOnBeforeObjectSaved) then
    FOnBeforeObjectSaved(Self, AObject);
end;

procedure TORMSessionConnection.DoOnBeforeObjectUnDeleted(const AClass: TClass;
  const PrimaryKeys: TPrimaryKeyArray);
begin
  if Assigned(FOnBeforeObjectUnDeleted) then
    FOnBeforeObjectUnDeleted(Self, AClass, PrimaryKeys);
end;

procedure TORMSessionConnection.DoOnAfterArrayLoaded(
  const ObjectClass: TClass; const SQLCriteria: ISQLComparitor;
  const TickCount: Cardinal; const Count: Integer);
begin
  if Assigned(FOnAfterArrayLoaded) then
    FOnAfterArrayLoaded(Self, ObjectClass, SQLCriteria, TickCount, Count);
end;

procedure TORMSessionConnection.DoOnBeforeArrayLoaded(
  const ObjectClass: TClass; const SQLCriteria: ISQLComparitor);
begin
  if Assigned(FOnBeforeArrayLoaded) then
    FOnBeforeArrayLoaded(Self, ObjectClass, SQLCriteria);
end;

procedure TORMSessionConnection.DoOnSQLAfterRead(const SQLDataset: ISQLDataset;
  const TickCount: Cardinal);
begin
  if Assigned(FOnSQLAfterRead) then
    FOnSQLAfterRead(Self, SQLDataset, TickCount);
end;

procedure TORMSessionConnection.DoOnSQLAfterWrite(const SQLCommand: ISQLCommand;
  const TickCount: Cardinal; const RowsAffected: Integer);
begin
  if Assigned(FOnSQLAfterWrite) then
    FOnSQLAfterWrite(Self, SQLCommand, TickCount, RowsAffected);
end;

procedure TORMSessionConnection.DoOnSQLBeforeRead(
  const SQLDataset: ISQLDataset);
begin
  if Assigned(FOnSQLBeforeRead) then
    FOnSQLBeforeRead(Self, SQLDataset);
end;

procedure TORMSessionConnection.DoOnSQLBeforeWrite(
  const SQLCommand: ISQLCommand);
begin
  if Assigned(FOnSQLBeforeWrite) then
    FOnSQLBeforeWrite(Self, SQLCommand);
end;

procedure TORMSessionConnection.DoOnSQLError(const SQLQuery: ISQLQuery;
  const e: Exception);
begin
  if Assigned(FOnSQLError) then
    FOnSQLError(Self, SQLQuery, e);
end;

procedure TORMSessionConnection.DoOnSQLTransactionError(const e: Exception);
begin
  if Assigned(FOnSQLTransactionError) then
    FOnSQLTransactionError(Self, e);
end;

function TORMSessionConnection.FieldHasDefaultValue(const AObject: TObject; const FieldMapping: IORMFieldMapping): Boolean;
var
  NullValue: Variant;
begin
  if FieldMapping.Attributes.GetAttribute(AttributeNull, NullValue) then
  begin
    Result := GetPropertyValue(AObject, FieldMapping.PropertyName) = NullValue;
  end
  else
  begin
    Result := True;
  end;
end;

procedure TORMSessionConnection.DoORMObjectToSQLInsertCriteria(
  const AObject: TObject; const TableMapping: IORMTableMapping; const ID: Variant;
  const RecordCreated: Boolean; const SQLInsertCriteria: ISQLInsertCriteria;
  const Options: TORMOptions);
var
  i: Integer;
  AttributeValue: Variant;
  ChildObject: TObject;
  NullValue: Variant;
  ATimeStamp: TDateTime;
begin
  // Add the set value fields for this class. Only set the non primary key
  // fields as they were already set above
  for i := 0 to pred(TableMapping.FieldMappings.Count) do
  begin
    if (TableMapping.FieldMappings[i].DatabaseFieldName <> '') and
       (TableMapping.FieldMappings[i].PropertyName <> '') and
       (not (TableMapping.FieldMappings[i].Attributes.HasAttribute(AttributePrimaryKey))) and
       (not (TableMapping.FieldMappings[i].Attributes.HasAttribute(AttributeNoStore))) then
    begin
      if TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeOnInsert, AttributeValue) then
      begin
        if RecordCreated then
        begin
          if FieldHasDefaultValue(AObject, TableMapping.FieldMappings[i]) then
          begin
            SQLInsertCriteria.Value(
              TableMapping.FieldMappings[i].DatabaseFieldName,
              DoGetAttributeMacroValue(AttributeValue));
          end
          else
          begin
            SQLInsertCriteria.Value(
              TableMapping.FieldMappings[i].DatabaseFieldName,
              DoGetDatabaseFieldValue(AObject, TableMapping.FieldMappings[i]));
          end;
        end
        else
        begin
          if ((TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeNull, NullValue)) and
              (GetPropertyValue(AObject, TableMapping.FieldMappings[i].PropertyName) <> NullValue)) then
          begin
            if (SameText(AttributeValue, MacroCurrentTimestamp)) or
               (SameText(AttributeValue, MacroCurrentTimestampUTC)) then
            begin
              ATimeStamp := GetPropertyValue(AObject, TableMapping.FieldMappings[i].PropertyName);

              SQLInsertCriteria.Value(
                TableMapping.FieldMappings[i].DatabaseFieldName,
                ATimestamp);
            end
            else
            begin
              SQLInsertCriteria.Value(
                TableMapping.FieldMappings[i].DatabaseFieldName,
                GetPropertyValue(AObject, TableMapping.FieldMappings[i].PropertyName));
            end
          end;
        end;
      end else
      if (not RecordCreated) and
         (TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeOnUpdate, AttributeValue)) then
      begin
        if FieldHasDefaultValue(AObject, TableMapping.FieldMappings[i]) then
        begin
          SQLInsertCriteria.Value(
            TableMapping.FieldMappings[i].DatabaseFieldName,
            DoGetAttributeMacroValue(AttributeValue));
        end
        else
        begin
          SQLInsertCriteria.Value(
            TableMapping.FieldMappings[i].DatabaseFieldName,
            DoGetDatabaseFieldValue(AObject, TableMapping.FieldMappings[i]));
        end;
      end
      else
      if not TableMapping.FieldMappings[i].Attributes.HasAttribute(AttributeOnSoftDelete) then
      begin
        if TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass then
        begin
          ChildObject := GetPropertyObject(AObject, TableMapping.FieldMappings[i].PropertyName);

          if ChildObject is TStream then
          begin
            SQLInsertCriteria.Value(
              TableMapping.FieldMappings[i].DatabaseFieldName,
              ChildObject as TMemoryStream);
          end;
        end
        else
        begin
          SQLInsertCriteria.Value(
            TableMapping.FieldMappings[i].DatabaseFieldName,
            DoGetDatabaseFieldValue(AObject, TableMapping.FieldMappings[i]));
        end;
      end;
    end;
  end;
end;

function TORMSessionConnection.LoadObject<T>(
  const Where: ISQLComparitor): T;
begin
  Result := LoadObject<T>(Where, []);
end;

function TORMSessionConnection.LoadObject<T>(
  const Where: ISQLComparitor; const Options: TORMOptions): T;
begin
  LoadObjectInternal<T>(Options, Where, Result);
end;

procedure TORMSessionConnection.LoadObject<T>(
  const PrimaryKeyValues: TPrimaryKeyArray; const Options: TORMOptions;
  out ORMObject: T);
var
  SQLComparitor: ISQLComparitor;
  TickCount: Cardinal;
  TableMapping: IORMTableMapping;
begin
  TickCount := TThread.GetTickCount;

  ORMObject := nil;

  DoOnBeforeObjectLoaded(T);

  // If we didn't load the object from the cache, load it from the DB
  if ORMObject = nil then
  begin
    TableMapping := FORM.FindTableMapping(T.ClassName);

    // Generate the comparitor for the primary key(s)
    SQLComparitor := GetPrimaryKeySQLComparitor(
      T,
      TableMapping,
      PrimaryKeyValues);

    // Load the object
    LoadObjectInternal<T>(Options, SQLComparitor, ORMObject);
  end;

  // Fire the after object loaded event
  DoOnAfterObjectLoaded(ORMObject, TThread.GetTickCount - TickCount);
end;

procedure TORMSessionConnection.LoadFromDB(const AObject: TObject;
  const PrimaryKeyValues: TPrimaryKeyArray; const TableName: String;
  const TableMapping: TORMTableMapping; const Options: TORMOptions);
var
  SQLSelectCriteria: ISQLSelectCriteria;
  WhereComparitor: ISQLComparitor;
  i: Integer;
begin
  Assert(length(PrimaryKeyValues) = TableMapping.PrimaryKeyFieldMappings.Count, StrPrimaryKeyValueCo);

  WhereComparitor := nil;
  SQLSelectCriteria := TOFac.Select.
                       From(TableName);

  for i := 0 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
  begin
    SQLSelectCriteria.
      Field(TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName);

    if WhereComparitor = nil then
      WhereComparitor := TOFac.Comparitor(TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName,
                                          EqualTo,
                                          PrimaryKeyValues[i],
                                          vtParameter)
    else
      WhereComparitor.&And(TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName,
                           EqualTo,
                           PrimaryKeyValues[i]);
  end;

  SQLSelectCriteria.Where(WhereComparitor);

  for i := 0 to pred(TableMapping.FieldMappings.Count) do
    SQLSelectCriteria.Field(TableMapping.FieldMappings[i].DatabaseFieldName);

  DoDatasetToORMObject(AObject,
                       NewDataset(SQLSelectCriteria),
                       TableMapping,
                       Options);
end;

function TORMSessionConnection.LoadInterface<T, I>(const ID: Variant): I;
begin
  Result := LoadInterface<T, I>(ID, []);
end;

function TORMSessionConnection.GetPrimaryKeySQLComparitor(
  const ORMObjectClass: TClass; const TableMapping: IORMTableMapping;
  const PrimaryKeyValues: TPrimaryKeyArray): ISQLComparitor;
var
  i: Integer;
begin
  // We need to create one. First check that the primary key values
  // match the PrimaryKeyMapping count
  if length(PrimaryKeyValues) <> TableMapping.PrimaryKeyFieldMappings.Count then
    raise EORMIncorrectPrimaryKeyCountError.CreateFmt('Incorrect number of primary key values for %s', [ORMObjectClass.ClassName]);

  // Create the comparitor
  Result := TOFac.Comparitor(TableMapping.PrimaryKeyFieldMappings[0].DatabaseFieldName,
                             EqualTo,
                             PrimaryKeyValues[0],
                             vtParameter);

  // Add any remaing primary key values
  for i := 1 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
    Result.&And(TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName, EqualTo, PrimaryKeyValues[i]);
end;

function TORMSessionConnection.GetOneToManyForeignKeySQLComparitor(
  const ParentObject: TObject; const SrcFieldMapping: IORMFieldMapping;
  const ParentTableMapping: IORMTableMapping): ISQLComparitor;
var
  i: Integer;
  Attribute: Variant;
  Attributes: TStringList;
begin
  if not SrcFieldMapping.Attributes.GetAttribute(AttributeOneToMany, Attribute) then
    raise EORMError.CreateFmt(StrMissingAttributeOneToMany, [ParentObject.ClassName]);

  Attributes := TStringList.Create;
  try
    Attributes.CommaText := Attribute;

    if Attributes.Count = 0 then
      raise EORMError.CreateFmt(StrMissingOneToManyForeignKey, [ParentObject.ClassName]) else
    if Attributes.Count <> ParentTableMapping.PrimaryKeyFieldMappings.Count then
      raise EORMError.CreateFmt(StrPrimaryAndForeign, [ParentObject.ClassName]);

    // Create the comparitor
    Result := TOFac.Comparitor(Attributes[0],
                               EqualTo,
                               GetPropertyValue(ParentObject,
                                                ParentTableMapping.PrimaryKeyFieldMappings[0].PropertyName),
                               vtParameter);

    // Add any remaing primary key values
    for i := 1 to pred(Attributes.Count) do
      Result.&And(Attributes[i],
                  EqualTo,
                  GetPropertyValue(ParentObject,
                                   ParentTableMapping.PrimaryKeyFieldMappings[i].PropertyName),
                  vtParameter);
  finally
    FreeAndNil(Attributes);
  end;
end;

procedure TORMSessionConnection.LoadObjectInternal<T>(
  const Options: TORMOptions; const SQLWhereComparitor: ISQLComparitor;
  out ORMObject: T);
var
  ObjectLoaded: Boolean;
  TickCount: Cardinal;
  i: Integer;
  Objects: TObjectList<T>;
  Count: Integer;
begin
  ORMObject := nil;

  try
    Objects := LoadArrayInternal<T, TObjectList<T>>(
      SQLWhereComparitor,
      nil,
      Options + [FirstOnly],
      Count);
    try
      Objects.OwnsObjects := False;

      if Count = 0 then
        raise EORMError.CreateFmt('Record not found for class "%s" - %s',
                                  [T.ClassName,
                                   SQLConnection.SQLEngine.GenerateComparisonSQL(SQLWhereComparitor, nil)]);

      ORMObject := Objects[0];
    finally
      FreeAndNil(Objects);
    end;
  except
    on e: Exception do
    begin
      if NoException in Options then
        FreeAndNil(ORMObject)
      else
        raise;
    end;
  end;
end;

function TORMSessionConnection.LoadObjectList<T, L>(
  const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria;
  const Options: TORMOptions): L;
var
  TickCount: Cardinal;
  Count: Integer;
begin
  DoOnBeforeArrayLoaded(T,  SQLComparitor);

  TickCount := TThread.GetTickCount;

  Result := LoadArrayInternal<T, L>(
    SQLComparitor,
    OrderBy,
    Options,
    Count);

  DoOnAfterArrayLoaded(T,  SQLComparitor, TThread.GetTickCount - TickCount, Count);
end;

procedure TORMSessionConnection.LoadObject<T>(const ID: Variant;
  const Options: TORMOptions; out ORMObject: T);
begin
  LoadObject<T>([ID], Options, ORMObject);
end;

function TORMSessionConnection.LoadObject<T>(const ID: Variant;
  const Options: TORMOptions): T;
begin
  LoadObject<T>(ID, Options, Result);
end;

function TORMSessionConnection.SaveObjectInternal(const AObject: TObject;
  const Options: TORMOptions): TPrimaryKeyArray;
var
  RecordCreated: Boolean;
  TickCount: Cardinal;
begin
  RecordCreated := False;

  DoOnBeforeObjectSaved(AObject);

  TickCount := TThread.GetTickCount;

  try
    Result := SaveToDB(
      AObject,
      Options,
      RecordCreated);

    DoOnAfterObjectSaved(
      AObject,
      RecordCreated,
      TThread.GetTickCount - TickCount,
      Result);
  except
    on e: Exception do
    begin
      if not (NoException in Options) then
        raise;
    end;
  end;
end;

function TORMSessionConnection.SaveInterface(
  const ORMInterface: IInterface): TPrimaryKeyArray;
begin
  Result := SaveInterface(ORMInterface, []);
end;

function TORMSessionConnection.SaveInterface(const ORMInterface: IInterface;
  const Options: TORMOptions): TPrimaryKeyArray;
begin
  Result := SaveObject(ORMInterface as TObject, Options);
end;

function TORMSessionConnection.SaveObject(const AObject: TObject;
  const Options: TORMOptions): TPrimaryKeyArray;
begin
  Result := SaveObjectInternal(AObject, Options);
end;

function TORMSessionConnection.DoGetCurrentTimeStamp: TDateTime;
begin
  Result := now;
end;

function TORMSessionConnection.DoGetCurrentTimeStampUTC: TDateTime;
begin
  Result := TTimeZone.Local.ToUniversalTime(now);
end;

function TORMSessionConnection.DoGetDatabaseFieldValue(const AObject: TObject;
  const FieldMapping: IORMFieldMapping): Variant;
var
  AttributeValue: Variant;
begin
  if FieldMapping.PropertyInfo.Kind <> tkClass then
  begin
    if (FieldMapping.Attributes.GetAttribute(AttributeNull, AttributeValue)) and
       (GetPropertyValue(AObject, FieldMapping.PropertyName) = AttributeValue) then
    begin
      Result := Null;
    end
    else
    begin
      Result := DoFixVariantType(GetPropertyValue(AObject, FieldMapping.PropertyName),
                                 FieldMapping);

      if FieldMapping.PropertyInfo.Kind = TTypeKind.tkEnumeration then
      begin
        if SameText(GetTypeInfoName(FieldMapping.PropertyInfo), PropertyTypeBoolean) then
        begin
          if Result then
            Result := SQLConnection.SQLEngine.GetTokenBooleanTrue
          else
            Result := SQLConnection.SQLEngine.GetTokenBooleanFalse;
        end
        else
        begin
          Result := GetEnumValue(FieldMapping.PropertyInfo, Result);
        end;
      end;
    end;
  end;
end;

function TORMSessionConnection.NewCommand(const SQL: string; const ParamNames: array of String;
  const ParamValues: array of Variant; const Execute: TCommandExecutType): ISQLCommand;
begin
  Result := SQLConnection.NewCommand(
    SQL,
    ParamNames,
    ParamValues,
    Execute,
    FSQLQueryCallbacks)
end;

function TORMSessionConnection.NewDataset(const SQL: string;
  const SQLDatasetClass: TSQLDatasetClass; const Open: Boolean): ISQLDataset;
begin
  Result := NewDataset(
    SQL,
    [],
    [],
    SQLDatasetClass,
    Open)
end;

function TORMSessionConnection.NewDataset(const SQL: string; const ParamNames: array of String;
  const ParamValues: array of Variant; const SQLDatasetClass: TSQLDatasetClass; const Open: Boolean): ISQLDataset;
begin
  Result := SQLConnection.NewDataset(
    SQL,
    ParamNames,
    ParamValues,
    Open,
    FSQLQueryCallbacks,
    SQLDatasetClass)
end;

function TORMSessionConnection.NewDataset(const QueryCriteria: ISQLSelectCriteria; const SQLDatasetClass: TSQLDatasetClass): ISQLDataset;
var
  SQL: String;
  Parameters: ISQLParameters;
begin
  Parameters := TSQLParameters.Create;

  SQLConnection.SQLEngine.GenerateSQL(
    QueryCriteria,
    SQL,
    Parameters);

  Result := GetSQLConnection.NewDataset(
    SQL,
    Parameters,
    False,
    FSQLQueryCallbacks,
    SQLDatasetClass);

  Result.Open;
end;

function TORMSessionConnection.DoProcessSubClass(const AObject: TObject): Boolean;
begin
  Result := (not (AObject is TStream)) and
            (AObject <> nil);
end;

procedure TORMSessionConnection.DoResetSession;
begin
  FSessionOptions.ResetToDefault;
end;

function TORMSessionConnection.GetObjectSQLSelectCriteria(const ObjectClass: TClass;
  const SQLComparitor: ISQLComparitor; const Options: TORMOptions): ISQLSelectCriteria;

  function CanSelectField(const FieldMapping: IORMFieldMapping): Boolean;
  begin
    Result := (FieldMapping.DatabaseFieldName <> '') and
              (FieldMapping.PropertyName <> '');

    if Result then
    begin
      if ProcessGridAttributes in Options then
      begin
        if (FieldMapping.Attributes.HasAttribute(AttributeGridNone)) or
           (FieldMapping.Attributes.HasAttribute(AttributeNoLoad)) then
          Result := False;
      end;
    end;
  end;

var
  TableMapping, PreviousTableMapping: IORMTableMapping;
  ORMObjectClassStep: TClass;
  JoinComparitor: ISQLComparitor;
  FieldName: String;
  AttributeValue: Variant;
  i: Integer;
begin
  Result := nil;

  ORMObjectClassStep := ObjectClass;
  PreviousTableMapping := nil;

  // Keep going until we reach a base class
  while (ORMObjectClassStep <> nil) and
        (ORM.IsValidORMObject(ORMObjectClassStep)) do
  begin
    // Find the table mapping of the Object Class. An exception will
    // be raised if the table mapping is not found
    TableMapping := FORM.FindTableMapping(ORMObjectClassStep.ClassName);

    // Is this the first pass?
    if Result = nil then
    begin
      if TableMapping.TableName = '' then
      begin
        // Find the parent class
        ORMObjectClassStep := ORMObjectClassStep.ClassParent;

        Continue;
      end
      else
      begin
        // Create the select criteria
        Result := TOFac.Select.
                  From
                    (TableMapping.TableName).
                  Where
                    (SQLComparitor);
      end;
    end;

    if (TableMapping.TableName = '') and
       (PreviousTableMapping <> nil) then
    begin
      // Add the select fields for this class
      for i := 0 to pred(TableMapping.FieldMappings.Count) do
      begin
        FieldName := PreviousTableMapping.TableName + '.' + TableMapping.FieldMappings[i].DatabaseFieldName;

        if CanSelectField(TableMapping.FieldMappings[i]) then
          Result.Field(FieldName);

        if (ProcessGridAttributes in Options) and
           (TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeGridLocalTimestampField, AttributeValue)) then
        begin
          Result.Field(TOFac.FieldDateLocalise(
            FieldName,
            SessionOptions.UTCOffset),
            AttributeValue);
        end;

        if ((SessionOptions.IncludeDeleted in [TIncludeDeleted.idNo]) or
            (NoDeleted in Options)) and
           (TableMapping.FieldMappings[i].Attributes.HasAttribute(AttributeOnSoftDelete)) then
        begin
          if Result.GetWhereCriteria = nil then
            Result.Where(TOFac.Comparitor(FieldName, &Is, Null))
          else
            Result.GetWhereCriteria.&And(TOFac.Comparitor(FieldName, &Is, Null))
        end;
      end;
    end
    else
    begin
      // Add the select fields for this class
      for i := 0 to pred(TableMapping.FieldMappings.Count) do
      begin
        FieldName := TableMapping.TableName + '.' + TableMapping.FieldMappings[i].DatabaseFieldName;

        if CanSelectField(TableMapping.FieldMappings[i]) then
          Result.Field(FieldName);

        if (ProcessGridAttributes in Options) and
           (TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeGridLocalTimestampField, AttributeValue)) then
        begin
          Result.Field(TOFac.FieldDateLocalise(
            FieldName,
            SessionOptions.UTCOffset),
            AttributeValue);
        end;

        if ((SessionOptions.IncludeDeleted in [TIncludeDeleted.idNo]) or
            (NoDeleted in Options)) and
           (TableMapping.FieldMappings[i].Attributes.HasAttribute(AttributeOnSoftDelete)) then
        begin
          if Result.GetWhereCriteria = nil then
            Result.Where(TOFac.Comparitor(FieldName, &Is, Null))
          else
            Result.GetWhereCriteria.&And(TOFac.Comparitor(FieldName, &Is, Null))
        end;
      end;

      if PreviousTableMapping <> nil then
      begin
        JoinComparitor :=
          TOFac.Comparitor(
            PreviousTableMapping.TableName +
              '.' +
              PreviousTableMapping.PrimaryKeyFieldMappings[0].DatabaseFieldName,
            EqualTo,
            TableMapping.TableName +
              '.' +
              TableMapping.PrimaryKeyFieldMappings[0].DatabaseFieldName,
            vtFieldReference);

        for i := 1 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
          JoinComparitor.&And(PreviousTableMapping.TableName + '.' + PreviousTableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName,
                              EqualTo,
                              TableMapping.TableName + '.' + TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName,
                              vtFieldReference);

        // Add the join
        Result.Join
          (TableMapping.TableName,
           '',
           JoinComparitor);
      end;

      PreviousTableMapping := TableMapping;
    end;

    // Quit the loop now if we're not processing parent classes
    if NoProcessParentClasses in Options then
      Break;

    // Find the parent class
    ORMObjectClassStep := ORMObjectClassStep.ClassParent;
  end;
end;

(*procedure TORMSessionConnection.DoAssignORMObject(const Src, Dst: TObject;
  const TableMapping: IORMTableMapping);
var
  DstTableMapping: IORMTableMapping;
  i: Integer;
begin
  if not Dst.InheritsFrom(Src.ClassType) then
    raise EORMError.CreateFmt(StrCannotAssignSTo, [Src.ClassName, Dst.ClassName]);

  if TableMapping = nil then
    DstTableMapping := FORM.FindTableMapping(Dst)
  else
    DstTableMapping := TableMapping;

  for i := 0 to pred(DstTableMapping.FieldMappings.Count) do
  begin
    if DstTableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass then
    begin
      DoAssignORMObject(GetPropertyObject(Src, DstTableMapping.FieldMappings[i].PropertyName),
                        GetPropertyObject(Dst, DstTableMapping.FieldMappings[i].PropertyName));
    end
    else
    begin
      SetPropertyValue(Dst,
                       DstTableMapping.FieldMappings[i].PropertyName,
                       GetPropertyValue(Src,
                                        DstTableMapping.FieldMappings[i].PropertyName));
    end;
  end;
end;          *)

procedure TORMSessionConnection.DoAssignORMObjectList(const ORMObjectArray: TObjectArray;
  const DstList: TObject);
begin
  TransferArrayItems(DstList, ORMObjectArray);
end;

procedure TORMSessionConnection.SetObjectPropertiesFromDataset(const AObject: TObject;
  const SQLDataset: ISQLDataset; const Options: TORMOptions);
var
  ORMObjectClassStep: TClass;
  TableMapping, SubClassTableMapping: IORMTableMapping;
  FieldMapping: IORMFieldMapping;
  PropertyObject: TObject;
  Field: TField;
  i: Integer;
  ORMObjectArray: TObjectArray;
  PropertyClassType: TClass;
  Attribute: Variant;
begin
  // Now we step through the object class hierarchy again, setting the properties
  // from the dataset
  ORMObjectClassStep := AObject.ClassType;

  // Walk the object hierarchy
  while (ORMObjectClassStep <> nil) and
        (ORM.IsValidORMObject(ORMObjectClassStep)) do
  begin
    // Find the table mapping
    TableMapping := FORM.FindTableMapping(ORMObjectClassStep.ClassName);

    // Copy the dataset fields to the object properties
    DoDatasetToORMObject(AObject, SQLDataset, TableMapping, Options);

    // Process child classes
    for i := 0 to pred(TableMapping.FieldMappings.Count) do
    begin
      if (TableMapping.FieldMappings[i].PropertyInfo <> nil) and
         (TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass) then
      begin
        // Get the object from the class property
        PropertyObject := GetPropertyObject(AObject, TableMapping.FieldMappings[i].PropertyName);

        if DoProcessSubClass(PropertyObject) then
        begin
          // If the object is assigned, try to load it using the ID stored in the
          // foreign key field
          FieldMapping := TableMapping.FieldMappings.FindByPropertyName(
                             TableMapping.FieldMappings[i].PropertyName);

          if not (NoProcessChildClasses in Options) then
          begin
            if FieldMapping.Attributes.HasAttribute(AttributeOneToOne) then
            begin
              Field := SQLDataset.FindField(TableMapping.FieldMappings[i].DatabaseFieldName);

              if (Field <> nil) and
                 (not Field.IsNull) then
              begin
                SubClassTableMapping := FORM.FindTableMapping(PropertyObject);

                LoadListFromDB(ORMObjectArray,
                               PropertyObject.ClassType,
                               GetPrimaryKeySQLComparitor(PropertyObject.ClassType,
                                                          SubClassTableMapping,
                                                          [Field.Value]),
                               nil,
                               Options + [FirstOnly]);
                try
                  TORMObjectCloner.AssignTo(
                    FORM,
                    ORMObjectArray[0],
                    PropertyObject);
                finally
                  // Make sure we free the objects in the array
                  ClearObjectArray(ORMObjectArray);
                end;
              end
              else
              begin
                raise EORMError.CreateFmt(StrInvalidFieldMapping, [TableMapping.FieldMappings[i].PropertyName]);
              end;
            end else
            if FieldMapping.Attributes.HasAttribute(AttributeOneToMany) then
            begin
              FieldMapping.Attributes.GetAttribute(AttributeArrayClassType, Attribute);

              PropertyClassType := FORM.FindTableMapping(Attribute).ObjectClass;

              LoadListFromDB(
                ORMObjectArray,
                PropertyClassType,
                GetOneToManyForeignKeySQLComparitor(
                  AObject,
                  FieldMapping,
                  TableMapping),
                  nil,

                  Options - [FirstOnly]);
              try
                DoAssignORMObjectList(ORMObjectArray, PropertyObject);
              except
                on e: Exception do
                begin
                  ClearObjectArray(ORMObjectArray);

                  raise;
                end;
              end;
            end
            else
            begin
              raise EORMError.CreateFmt(StrSubClassNoAttribute,
                [PropertyObject.ClassName, TableMapping.FieldMappings[i].PropertyName]);
            end;
          end;
        end;
      end;
    end;

    // Quit the loop now if we're not processing parent classes
    if NoProcessParentClasses in Options then
      Break;

    // Find the parent class
    ORMObjectClassStep := ORMObjectClassStep.ClassParent;
  end;
end;

procedure TORMSessionConnection.LoadListFromDB(var Objects: TObjectArray;
  const ObjectClass: TClass; const SQLWhereComparitor: ISQLComparitor;
  const OrderBy: ISQLOrderByCriteria; const Options: TORMOptions);
var
  SQLSelectCriteria: ISQLSelectCriteria;
  i: Integer;
  SQLDataset: ISQLDataset;
  AObject: TObject;
begin
  Start;

  try
    SQLSelectCriteria := GetObjectSQLSelectCriteria(
      ObjectClass,
      SQLWhereComparitor,
      Options).
      OrderBy(OrderBy);

    if FirstOnly in Options then
      SQLSelectCriteria.First(1);

    SQLDataset := NewDataset(SQLSelectCriteria);

    SetLength(Objects, SQLDataset.RecordCount);
    i := 0;
    while not SQLDataset.Eof do
    begin
      AObject := CreateObject(ObjectClass);
      try
        SetObjectPropertiesFromDataset(AObject,
                                       SQLDataset,
                                       Options);

        if i >= Length(Objects) then
          SetLength(Objects, SQLDataset.RecordCount);

        Objects[i] := AObject;
        Inc(i);

        if FirstOnly in Options then
          Break;

        SQLDataset.Next;
      except
        on e: Exception do
        begin
          for i := Low(Objects) to High(Objects) do
            Objects[i].Free;

          SetLength(Objects, 0);

          raise;
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      FreeAndNil(AObject);

      raise;
    end;
  end;
end;


procedure TORMSessionConnection.SetObjectPrimaryKeysFromPrimaryKeyArray(const AObject: TObject;
  const TableMapping: IORMTableMapping; const PrimaryKeyArray: TPrimaryKeyArray);
var
  i: Integer;
begin
  if length(PrimaryKeyArray) <> TableMapping.PrimaryKeyFieldMappings.Count then
    raise EORMIncorrectPrimaryKeyCountError.CreateFmt(StrIncorrectNumberOfPrimaryKeyValues, [TableMapping.ObjectClass]);

  for i := 0 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
    SetPropertyValue(AObject,
                     TableMapping.PrimaryKeyFieldMappings[i].PropertyName,
                     PrimaryKeyArray[i]);
end;

function TORMSessionConnection.IsPrimaryKeyEmpty(const AObject: TObject): Boolean;
var
  TableMapping: IORMTableMapping;
  i: Integer;
begin
  Result := True;

  TableMapping := ORM.FindTableMapping(AObject);

  for i := 0 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
  begin
    if TableMapping.PrimaryKeyFieldMappings[i].PropertyName <> '' then
    begin
      if not DoIsPrimaryKeyEmpty(
        AObject,
        TableMapping.PrimaryKeyFieldMappings[i]) then
      begin
        Exit(False);
      end;
    end;
  end;
end;

function TORMSessionConnection.DoIsPrimaryKeyEmpty(const AObject: TObject;
  PrimaryKeyMapping: IORMFieldMapping): Boolean;
begin
  case PrimaryKeyMapping.PropertyInfo.Kind of
    tkInteger,
    tkFloat,
    tkInt64:
      Result :=
            GetPropertyValue(
              AObject,
              PrimaryKeyMapping.PropertyName) = 0;
    tkEnumeration:
      Result := False; { TODO : How can an enumeration be empty? }
//        Integer(GetPropertyValue(
//        AObject,
//        PrimaryKeyMapping.PropertyName)) = -1;
    tkChar,
    tkString,
    tkSet,
    tkWChar,
    tkLString,
    tkWString,
    tkUString: Result := GetPropertyValue(AObject,
                                          PrimaryKeyMapping.PropertyName) = '';
  else
    raise EORMUnsupportedPrimaryKeyType.CreateFmt('Unsupported primary key type for field "%s"', [PrimaryKeyMapping.DatabaseFieldName]);
  end;
end;

function TORMSessionConnection.SaveToDB(const AObject: TObject;
  const Options: TORMOptions; var RecordCreated: Boolean;
  const ObjectClass: TClass): TPrimaryKeyArray;
var
  UpdateSQLList: IORMList<ISQLParameter>;
  NameValuePair: ISQLParameter;
  TableMapping: IORMTableMapping;
  ORMObjectClass: TClass;
  PropertyObject: TObject;
  SQLInsertCriteria: ISQLInsertCriteria;
  i: Integer;
  GeneratorName: Variant;
  PrimaryKeyValue: Variant;
  ChildClassPrimaryKeyValues, ParentClassPrimaryKeyValues: TPrimaryKeyArray;
  FirstPass: Boolean;
  ID: Variant;
  RecordCreatedDummy: Boolean;
  FieldIDGenerator: IFieldIDGenerator;
  SequencePropertyName: String;
begin
  // Make sure that the Object is assigned
  if AObject <> nil then
  begin
    SequencePropertyName := '';

    // Make sure the session has started
    Start;

    // Find the class of the root object
    if ObjectClass = nil then
      ORMObjectClass := AObject.ClassType
    else
      ORMObjectClass := ObjectClass;

    // Find the table mapping
    UpdateSQLList := TORMList<ISQLParameter>.Create;

    // If we have a parent with a table name, save it first
    if (ORM.IsValidORMObject(ORMObjectClass.ClassParent)) and
       (FORM.FindTableMapping(ORMObjectClass.ClassParent.ClassName).TableName <> '') then
    begin
      ParentClassPrimaryKeyValues := SaveToDB(
        AObject,
        Options,
        RecordCreatedDummy,
        ORMObjectClass.ClassParent);

      TableMapping := FORM.FindTableMapping(ORMObjectClass.ClassName);

      SetObjectPrimaryKeysFromPrimaryKeyArray(AObject, TableMapping, ParentClassPrimaryKeyValues);
    end;

    // Only process child object is not disabled
    if not (NoProcessChildClasses in Options) then
    begin
      // Walk the object hierarchy
      if (ORMObjectClass <> nil) and
         (ORM.IsValidORMObject(ORMObjectClass)) then
      begin
        // Find the table mapping of the Object Class. An exception will
        // be raised if the table mapping is not found
        TableMapping := FORM.FindTableMapping(ORMObjectClass.ClassName);

        // Process child classes
        for i := 0 to pred(TableMapping.FieldMappings.Count) do
        begin
          if (TableMapping.FieldMappings[i].PropertyInfo <> nil) and
             (TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass) then
          begin
            // Get the object from the class property
            PropertyObject := GetPropertyObject(AObject, TableMapping.FieldMappings[i].PropertyName);

            if DoProcessSubClass(PropertyObject) then
            begin
              // Save the child class and store the primary key
              ChildClassPrimaryKeyValues := SaveToDB(
                PropertyObject,
                Options,
                RecordCreatedDummy);

              if length(ChildClassPrimaryKeyValues) = 1 then
              begin
                NameValuePair := TSQLParameter.Create;

                NameValuePair.Name := TableMapping.FieldMappings[i].DatabaseFieldName;
                NameValuePair.Value := ChildClassPrimaryKeyValues[0];

                UpdateSQLList.Add(NameValuePair);
              end
              else
              begin
                { TODO : Perhaps improve this later }
                raise EORMParentClassOnlyAllowedOnePrimaryKey.Create(StrParentORMClasses);
              end;
            end;
          end;
        end;
      end;
    end;

    // Keep going until we reach a base class
    while (ORMObjectClass <> nil) and
          (ORM.IsValidORMObject(ORMObjectClass)) do
    begin
      // Is this the first pass?
      FirstPass := SQLInsertCriteria = nil;

      // Find the table mapping of the Object Class. An exception will
      // be raised if the table mapping is not found
      TableMapping := FORM.FindTableMapping(ORMObjectClass.ClassName);

      if TableMapping.Attributes.HasAttribute(AttributeNoStore) then
        raise EORMUnableToStoreObject.Create('Unable to store the object as it has the NoStore attribute');

      // If this is a different table, we're nothing more to do here. The
      // parent tables were dealt with above.
      if (not FirstPass) and
         (TableMapping.TableName <> '') then
        Break;

      if FirstPass then
      begin
        SQLInsertCriteria := TOFac.UpdateOrInsertInto(TableMapping.TableName);

        SetLength(Result, TableMapping.PrimaryKeyFieldMappings.Count);

        // Set the primary key values
        for i := 0 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
        begin
          if TableMapping.PrimaryKeyFieldMappings[i].PropertyName <> '' then
          begin
            // Is the primary key value zero
            if DoIsPrimaryKeyEmpty(AObject, TableMapping.PrimaryKeyFieldMappings[i]) then
            begin
              RecordCreated := True;

              // Try to find the generator
              if TableMapping.PrimaryKeyFieldMappings[i].Attributes.GetAttribute(AttributeSequence, GeneratorName) then
              begin
                if SequencePropertyName <> '' then
                begin
                  raise EORMSequenceError.CreateFmt('Only one auto increment field/sequence allowed per object. %s has multiple', [AObject.ClassName]);
                end
                else
                begin
                  SequencePropertyName := TableMapping.PrimaryKeyFieldMappings[i].PropertyName;

                  if (VarToStr(GeneratorName) <> '') and
                     (not SQLConnection.SupportsAutoIncFields) and
                     (Supports(SQLConnection, IFieldIDGenerator, FieldIDGenerator)) then
                  begin
                    SetPropertyValue(AObject,
                                     TableMapping.PrimaryKeyFieldMappings[i].PropertyName,
                                     FieldIDGenerator.GetGeneratorValue(VarToStr(GeneratorName)));
                  end;
                end;
              end;
            end;
          end;

          PrimaryKeyValue := DoGetDatabaseFieldValue(AObject, TableMapping.PrimaryKeyFieldMappings[i]);

          // Set the primary key value
          SQLInsertCriteria.Value(
            TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName,
            PrimaryKeyValue);

          Result[i] := PrimaryKeyValue;
        end;
      end;

      if TableMapping.PrimaryKeyFieldMappings.Count = 0 then
        ID := 0
      else
        ID := GetPropertyValue(AObject, TableMapping.PrimaryKeyFieldMappings[0].PropertyName);

      DoORMObjectToSQLInsertCriteria(
        AObject,
        TableMapping,
        ID,
        RecordCreated,
        SQLInsertCriteria,
        Options);

      // Add the foreign key link field values
      for i := 0 to pred(UpdateSQLList.Count) do
      begin
        SQLInsertCriteria.Value(
          UpdateSQLList[i].Name,
          UpdateSQLList[i].Value);
      end;

      // Quit the loop now if we're not processing parent classes
      if NoProcessParentClasses in Options then
        Break;

      // Find the parent class
      ORMObjectClass := ORMObjectClass.ClassParent;
    end;

    // If we arrive here we have successfully added all the fields for the object
    // and its parents. Now we perform the query
    NewCommand(SQLInsertCriteria);

    if (SequencePropertyName <> '') and
       (SQLConnection.SupportsAutoIncFields) and
       (length(Result) > 0) then
    begin
      // Return the last auto inc field
      Result[0] := SQLConnection.GetLastAutoIncValue;

      SetPropertyValue(
        AObject,
        SequencePropertyName,
        Result[0]);
    end;
  end;
end;

function TORMSessionConnection.DoGetAttributeMacroValue(const Value: Variant): Variant;
begin
  if SameText(Value, MacroCurrentTimestamp) then
    Result := DoGetCurrentTimeStamp else
  if SameText(Value, MacroCurrentTimestampUTC) then
    Result := DoGetCurrentTimeStampUTC
  else
    Result := Value;
end;

function TORMSessionConnection.SaveObject(const AObject: TObject): TPrimaryKeyArray;
begin
  Result := SaveObject(AObject, []);
end;

procedure TORMSessionConnection.Commit;
begin
  if (GetSQLConnection.InTransaction) and
     (not (ManualTransactions in FORMSessionOptions)) then
  try
    GetSQLConnection.CommitTransaction;
  except
    on e: Exception do
    begin
      DoOnSQLTransactionError(e);

      raise;
    end;
  end;
end;

constructor TORMSessionConnection.Create(const CustomORM: IORM;
  const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions);
begin
  FORM := CustomORM;
  FSQLConnection := SQLConnection;
  FORMSessionOptions := ORMSessionOptions;

  FSQLQueryCallbacks := TSQLQueryCallbacks.Create;
  FSQLQueryCallbacks.OnBeforeQueryCallback := OnBeforeSQLQuery;
  FSQLQueryCallbacks.OnAfterQueryCallback := OnAfterSQLQuery;
  FSQLQueryCallbacks.OnExceptionCallback := OnSQLQueryException;

  FSessionEndBehaviour := FORM.GetORMSessionEndBehaviour;
  FSessionOptions := TSessionOptions.Create;
end;

procedure TORMSessionConnection.DeleteObject<T>(const ID: Variant; const OverrideSoftDelete: Boolean);
begin
  DeleteObjectInternal<T>([ID], OverrideSoftDelete);
end;

procedure TORMSessionConnection.DeleteObject<T>(const PrimaryKeyValues: TPrimaryKeyArray; const OverrideSoftDelete: Boolean);
begin
  DeleteObjectInternal<T>(PrimaryKeyValues, OverrideSoftDelete);
end;

procedure TORMSessionConnection.SetDeleteFields(const ObjectClass: TClass; const PrimaryKeyValues: TPrimaryKeyArray; const UnDeleting: Boolean);
var
  RealObjectClass: TClass;
  TableMapping: IORMTableMapping;
  UpdateCriteria: ISQLUpdateCriteria;
  i: Integer;
  AttributeValue: Variant;
begin
  RealObjectClass := ObjectClass;

  while ORM.IsValidORMObject(RealObjectClass) do
  begin
    TableMapping := FORM.FindTableMapping(RealObjectClass.ClassName);

    if UpdateCriteria = nil then
    begin
      UpdateCriteria := TOFac.
        Update
          (TableMapping.TableName).
        Where(TOFac.Comparitor
          (TableMapping.PrimaryKeyFieldMappings, PrimaryKeyValues));
    end
    else
    begin
      if TableMapping.TableName <> '' then
      begin
        SetDeleteFields(RealObjectClass, PrimaryKeyValues, UnDeleting);

        Break;
      end;
    end;

    for i := 0 to pred(TableMapping.FieldMappings.Count) do
    begin
      if (not (TableMapping.FieldMappings[i].Attributes.HasAttribute(AttributePrimaryKey))) and
         ((TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeOnSoftDelete, AttributeValue))) then
      begin
        if UnDeleting then
          UpdateCriteria.Value(
            TableMapping.FieldMappings[i].DatabaseFieldName,
            Null)
        else
          UpdateCriteria.Value(
            TableMapping.FieldMappings[i].DatabaseFieldName,
            DoGetAttributeMacroValue(AttributeValue));
      end;
    end;

    RealObjectClass := RealObjectClass.ClassParent;
  end;

  if (UpdateCriteria <> nil) and
     (UpdateCriteria.GetFieldValueList.Count > 0) then
    NewCommand(UpdateCriteria);
end;

procedure TORMSessionConnection.DeleteObjectInternal<T>(const PrimaryKeyValues: TPrimaryKeyArray; const OverrideSoftDelete: Boolean);
var
  TableMapping: IORMTableMapping;
  DeleteCriteria: ISQLDeleteCriteria;
  WhereComparitor: ISQLComparitor;
  i: Integer;
  SoftDelete: Boolean;
  TickCount: Cardinal;
begin
  TableMapping := FORM.FindTableMapping(T.ClassName);

  if length(PrimaryKeyValues) <> TableMapping.PrimaryKeyFieldMappings.Count then
    raise EORMIncorrectPrimaryKeyCountError.Create('Primary key value count must equal primary key field mapping count');

  SoftDelete :=
    (not OverrideSoftDelete) and
    (TableMapping.Attributes.HasAttribute(AttributeSoftDelete));

  DoOnBeforeObjectDeleted(T, PrimaryKeyValues, SoftDelete);

  TickCount := TThread.GetTickCount;

  // If the softdlete attribute is set we only want to mark the object as deleted
  if SoftDelete then
  begin
    SetDeleteFields(T, PrimaryKeyValues, False);
  end
  else
  begin
    WhereComparitor := TOFac.Comparitor(TableMapping.PrimaryKeyFieldMappings[0].DatabaseFieldName,
                                        EqualTo,
                                        PrimaryKeyValues[0],
                                        vtParameter);

    for i := 1 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
      WhereComparitor.&And(TableMapping.PrimaryKeyFieldMappings[i].DatabaseFieldName,
                           EqualTo,
                           PrimaryKeyValues[i]);

    DeleteCriteria := TOFac.
      DeleteFrom
        (TableMapping.TableName).
      Where
        (WhereComparitor);

    NewCommand(DeleteCriteria);
  end;

  DoOnAfterObjectDeleted(T, PrimaryKeyValues, SoftDelete, TThread.GetTickCount - TickCount);
end;

destructor TORMSessionConnection.Destroy;
begin
  try
    try
      if GetSQLConnection.InTransaction then
      begin
        case FSessionEndBehaviour of
          TORMSessionEndBehaviour.Rollback:
            begin
              // Rollback the transaction
              Rollback;
            end;

          TORMSessionEndBehaviour.Commit:
            try
              // Try to commit
              Commit;
            except
              on e: Exception do
              begin
                // Log the exception

                // If the commit fails, we need to rollback the transaction otherwise it will remain open
                Rollback;

                raise;
              end;
            end;
        end;
      end;
    except
      on e: Exception do
      begin
        // All exceptions will be logged, but never allow the exception to escape the destructor
      end;
    end;
  finally
    // Clear the session settings
    DoResetSession;

    // Free the session options
    FreeAndNil(FSessionOptions);
  end;

  inherited;
end;

function TORMSessionConnection.LoadObject<T>(const ID: Variant): T;
begin
  Result := LoadObject<T>(ID, []);
end;

function TORMSessionConnection.CreateObject(const AClass: TClass): TObject;
var
  RTTIContext: TRttiContext;
  RTTIType: TRTTIType;
  Value: TValue;
begin
  // This is required in order to call the correct constructor
  RTTIContext := TRttiContext.Create;
  RTTIType := RTTIContext.GetType(AClass);
  Value := RTTIType.GetMethod('Create').Invoke(RTTIType.AsInstance.MetaclassType,[]);

  Result:= Value.AsObject;
  try
    DoNeedDefaultObjectProperties(Result);
  except
    on e: Exception do
    begin
      FreeAndNil(Result);

      raise;
    end;
  end;
end;

procedure TORMSessionConnection.ImportCSVRecord<T>(const TableMapping: IORMTableMapping;
  const ColumnHeaders, ColumnData: String; const KeyFieldNames: Array of String; const Log: TStrings);
var
  AOBject: T;
  Headers, Data: TStringList;
  Where: ISQLComparitor;
  FieldMapping: IORMFieldMapping;
  i: Integer;
  KeyFieldValues: String;
begin
  Headers := TStringList.Create;
  Data := TStringList.Create;
  try
    Headers.CommaText := UpperCase(ColumnHeaders);
    Data.CommaText := ColumnData;

    if (Headers.Count = 0) or (Data.Count = 0) then
    begin
      raise EORMImportError.Create('Column counts cannot equal zero');
    end;

    // Check the counts match
    if Headers.Count <> Data.Count then
    begin
      raise EORMImportError.CreateFmt('Column count (%d) does not match the header count (%d)', [Data.Count, Headers.Count]);
    end;

    KeyFieldValues := '';

    // Add the first key field comparitor
    Where := TOFac.Comparitor(
      KeyFieldNames[0],
      TSQLRelationalOperator.EqualTo,
      Data[Headers.IndexOf(UpperCase(KeyFieldNames[0]))]);

    AddToken(KeyFieldValues, Data[Headers.IndexOf(UpperCase(KeyFieldNames[0]))]);

    // Add the additional key field comparitors
    for i := Low(KeyFieldNames) + 1 to High(KeyFieldNames) do
    begin
      Where.&And(
        KeyFieldNames[i],
        TSQLRelationalOperator.EqualTo,
        Data[Headers.IndexOf(UpperCase(KeyFieldNames[i]))]);

      AddToken(KeyFieldValues, Data[Headers.IndexOf(UpperCase(KeyFieldNames[i]))]);
    end;

    // Try to load the object
    AObject := LoadObject<T>(Where, [NoException]);
    try
      // If the object doesn't exist, create it now
      if AObject = nil then
      begin
        AObject := T.Create;

        Log.Add(format('Adding "%s"', [KeyFieldValues]));
      end
      else
      begin
        Log.Add(format('Updating "%s"', [KeyFieldValues]));
      end;

      // Fill the object with data
      for i := 0 to pred(Headers.Count) do
      begin
        // Find the field mapping
        FieldMapping := TableMapping.FieldMappings.FindByDatabaseFieldName(Headers[i]);

        // Set the value
        SetPropertyValue(AObject, FieldMapping.PropertyName, Data[i]);
      end;

      // Save object to the DB
      SaveObject(AObject);

      sleep(1);
    finally
      FreeAndNil(AObject);
    end;
  finally
    FreeAndNil(Data);
    FreeAndNil(Headers);
  end;
end;

procedure TORMSessionConnection.ImportCSVData<T>(const Data, Log: TStrings;
  const KeyFieldNames: Array of String);
var
  Columns: TStringList;
  TableMapping: IORMTableMapping;
  i: Integer;
  ColumnCount: Integer;
begin
  if Data.Count = 0 then
  begin
    raise EORMImportError.Create('No data to import');
  end;

  if Length(KeyFieldNames) = 0 then
  begin
    raise EORMImportError.Create('You must supply at least 1 key field name');
  end;

  // Set the field names from the column headers
  Columns := TStringList.Create;
  try
    Columns.CommaText := Data[0];

    // Check we have field names
    if Columns.Count = 0 then
    begin
      raise EORMImportError.Create('No column headers found');
    end;

    ColumnCount := Columns.Count;

    // Find the ORM table mapping
    TableMapping := ORM.FindTableMapping(T.Classname);

    // Check all the field names exist
    for i := 0 to pred(Columns.Count) do
    begin
      if TableMapping.FieldMappings.FindByDatabaseFieldName(Columns[i]) = nil then
      begin
        raise EORMImportError.CreateFmt('Database field name "%s" not found in table "%s"', [Columns[i], TableMapping.TableName]);
      end;
    end;

    Log.Clear;

    for i := 1 to pred(Data.Count) do
    begin
      try
        ImportCSVRecord<T>(TableMapping, Data[0], Data[i], KeyFieldNames, Log);
      except
        on e: Exception do
        begin
          Log.Add(format('Error line %d: %s', [i, e.Message]));
        end;
      end;
    end;
  finally
    FreeAndNil(Columns);
  end;
end;

{ TORMSession }

constructor TORMSession.Create(const ORM: IORM; const ORMSessionConnectionClass: TORMSessionConnectionClass;
  const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions);
begin
  FORMSessionConnection := ORMSessionConnectionClass.Create(
    ORM,
    SQLConnection,
    ORMSessionOptions);
end;

destructor TORMSession.Destroy;
begin
  FreeAndNil(FORMSessionConnection);

  inherited;
end;

function TORMSession.GetConnection: TORMSessionConnection;
begin
  Result := FORMSessionConnection;
end;

{ TORMSessionObject }

constructor TORMSessionObject.Create(const ORMSessionConnection: TORMSessionConnection);
begin
  FORMSessionConnection := ORMSessionConnection;
end;

function TORMSessionConnection.LoadArray<T, I>(
  const SQLComparitor: ISQLComparitor; const OrderBy: ISQLOrderByCriteria;
  const Options: TORMOptions): IORMList<I>;
begin
  Result := LoadInterfaceArrayInternal<T, I>(
    SQLComparitor,
    OrderBy,
    Options);
end;

end.



