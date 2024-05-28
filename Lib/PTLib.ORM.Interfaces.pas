{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Interfaces                                     }
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

unit PTLib.ORM.Interfaces;

interface

uses
  System.SysUtils, System.Classes, Data.DB, System.Generics.Collections, System.TypInfo,

  PTLib.Common.Interfaces,
  PTLib.ORM.Types;

type
  ISQLNativeConnection = IInterface;
  TSQLNativeConnectionObject = TObject;

  ISQLComparitor = interface;
  ISQLSelectCriteria = interface;
  IORMTableMapping = interface;
  IORMFieldMapping = interface;
  ISQLQuery = interface;
  ISQLEngine = interface;
  ISQLUpdateCriteria = interface;
  ISQLInsertCriteria = interface;
  ISQLDeleteCriteria = interface;
  ISQLConnection = interface;
  ISQLParameters = interface;
  ISQLQueryCallbacks = interface;

  TBaseSQLQuery = class(TInterfacedObject)
  public
    constructor Create(const SQL: String; const Parameters: ISQLParameters;
      const SQLQueryCallbacks: ISQLQueryCallbacks); virtual; abstract;
  end;

  TSQLQueryClass = class of TBaseSQLQuery;

  ISmartPointer<T> = reference to function: T;

  TObjectWalkFunc = reference to function(const AObject: TObject; const TableMapping: IORMTableMapping;
    const FieldMapping: IORMFieldMapping; const InheritanceLevel, ClassDepth: Integer): Boolean;
  TClassWalkFunc = reference to function(const TableMapping: IORMTableMapping;
    const FieldMapping: IORMFieldMapping; const InheritanceLevel, ClassDepth: Integer): Boolean;

  TSQLQueryOnBeforeCallback = procedure(const SQLQuery: ISQLQuery) of object;
  TSQLQueryOnAfterCallback = procedure(const SQLQuery: ISQLQuery; const TickCount: Cardinal) of object;
  TSQLQueryExceptionCallback = procedure(const SQLQuery: ISQLQuery; const e: Exception) of object;

  IORMCacheItem<T> = interface(IJSONSerialisable)
    ['{285FBD33-8FC8-4F77-9550-A1ECC75E6166}']
    function GetValue: T;
    function GetHash: String;
    function GetTimestamp: TDateTime;
    function GetJSON: String;
    property Value: T read GetValue;
    property Hash: String read GetHash;
    property Timestamp: TDateTime read GetTimestamp;
    property JSON: String read GetJSON;
  end;

  ISQLQueryCallbacks = interface
    ['{02A3BE96-B0D1-4D0C-A36F-063685324305}']
    function GetOnAfterQueryCallback: TSQLQueryOnAfterCallback;
    function GetOnBeforeQueryCallback: TSQLQueryOnBeforeCallback;
    function GetOnExceptionCallback: TSQLQueryExceptionCallback;
    procedure SetOnAfterQueryCallback(const Value: TSQLQueryOnAfterCallback);
    procedure SetOnBeforeQueryCallback(const Value: TSQLQueryOnBeforeCallback);
    procedure SetOnExceptionCallback(const Value: TSQLQueryExceptionCallback);
    property OnBeforeQueryCallback: TSQLQueryOnBeforeCallback read GetOnBeforeQueryCallback write SetOnBeforeQueryCallback;
    property OnAfterQueryCallback: TSQLQueryOnAfterCallback read GetOnAfterQueryCallback write SetOnAfterQueryCallback;
    property OnExceptionCallback: TSQLQueryExceptionCallback read GetOnExceptionCallback write SetOnExceptionCallback;
  end;

  IORMList<T> = interface
    // No GUID for generic interfaces
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; const Value: T);
    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    function Add(const Value: T): Integer;
    function Insert(const Index: Integer; const Value: T): Integer;
    procedure Clear;
    function IndexOf(const Value: T): Integer;
    function Remove(Value: T): Integer;
    procedure Delete(Index: Integer);
  end;

  ISQLParameter = interface
    ['{59949B11-2449-45A2-9AE1-D07D0C6843F1}']
    function GetName: String;
    function GetValue: Variant;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: Variant);
    function GetValueStream: TStream;
    procedure SetValueStream(const Value: TStream);
    function GetVarType: TVarType;
    procedure SetVarType(const Value: TVarType);
    function GetFieldType: TFieldType;
    procedure SetFieldType(const Value: TFieldType);
    function ValueStreamAsMemoryStream: TMemoryStream;
    property Name: String read GetName write SetName;
    property Value: Variant read GetValue write SetValue;
    property ValueStream: TStream read GetValueStream write SetValueStream;
    property VarType: TVarType read GetVarType write SetVarType;
    property FieldType: TFieldType read GetFieldType write SetFieldType;
  end;

  ISQLParameters = interface
    ['{F3022801-F9BB-494E-8B3B-248507782318}']
    function FindParam(const ParameterName: String): ISQLParameter;
    function SetParam(const ParamName: String; const ParamValue: Variant; const ParamType: TFieldType): ISQLParameter; overload;
    function SetParam(const ParamName: String; const ParamValue: TStream): ISQLParameter; overload;
    procedure SetParams(const ParamNames: array of String; const ParamValues: array of Variant);
    function Count: Integer;
    procedure Clear;
    function GetItems(Index: Integer): ISQLParameter;
    procedure SetItems(Index: Integer; const Value: ISQLParameter);
    property Items[Index: Integer]: ISQLParameter read GetItems write SetItems; default;
  end;

  IORMField = interface
    ['{9ED4FD79-2BFB-4D67-983D-F7147E84C2B6}']
    function GetCalculated: Boolean;
    function GetCanModify: Boolean;
    function GetDataSize: Integer;
    function GetDataType: TFieldType;
    function GetDisplayName: string;
    function GetFieldNo: Integer;
    function GetFullName: string;
    function GetIsIndexField: Boolean;
    function GetLifeCycle: TFieldLifeCycle;
    function GetOffset: Integer;
    function GetSize: Integer;
    function GetFieldName: String;
    procedure SetCalculated(const Value: Boolean);
    procedure SetCanModify(const Value: Boolean);
    procedure SetDataSize(const Value: Integer);
    procedure SetDataType(const Value: TFieldType);
    procedure SetDisplayName(const Value: string);
    procedure SetFieldNo(const Value: Integer);
    procedure SetFullName(const Value: string);
    procedure SetIsIndexField(const Value: Boolean);
    procedure SetLifeCycle(const Value: TFieldLifeCycle);
    procedure SetOffset(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure SetFieldName(const Value: String);
    function GetAlignment: TAlignment;
    function GetAutoGenerateValue: TAutoRefreshFlag;
    function GetConstraintErrorMessage: string;
    function GetCustomConstraint: string;
    function GetDefaultExpression: string;
    function GetDisplayLabel: string;
    function GetDisplayWidth: Integer;
    function GetFieldKind: TFieldKind;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetAutoGenerateValue(const Value: TAutoRefreshFlag);
    procedure SetConstraintErrorMessage(const Value: string);
    procedure SetCustomConstraint(const Value: string);
    procedure SetDefaultExpression(const Value: string);
    procedure SetDisplayLabel(const Value: string);
    procedure SetDisplayWidth(const Value: Integer);
    procedure SetFieldKind(const Value: TFieldKind);
    property FieldName: String read GetFieldName write SetFieldName;
    property Calculated: Boolean read GetCalculated write SetCalculated;
    property CanModify: Boolean read GetCanModify write SetCanModify;
    property LifeCycle: TFieldLifeCycle read GetLifeCycle write SetLifeCycle;
    property DataSize: Integer read GetDataSize write SetDataSize;
    property DataType: TFieldType read GetDataType write SetDataType;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property FieldNo: Integer read GetFieldNo write SetFieldNo;
    property FullName: string read GetFullName write SetFullName;
    property IsIndexField: Boolean read GetIsIndexField write SetIsIndexField;
    property Offset: Integer read GetOffset write SetOffset;
    property Size: Integer read GetSize write SetSize;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property AutoGenerateValue: TAutoRefreshFlag read GetAutoGenerateValue write SetAutoGenerateValue;
    property CustomConstraint: string read GetCustomConstraint write SetCustomConstraint;
    property ConstraintErrorMessage: string read GetConstraintErrorMessage write SetConstraintErrorMessage;
    property DefaultExpression: string read GetDefaultExpression write SetDefaultExpression;
    property DisplayLabel: string read GetDisplayLabel write SetDisplayLabel;
    property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth;
    property FieldKind: TFieldKind read GetFieldKind write SetFieldKind;
  end;

  IConcatMacroItem = interface
    ['{B8403079-2444-42A7-B677-60DC754DB1A2}']
    function GetValue: String;
    function GetNullValue: String;
    procedure SetValue(const Value: String);
    procedure SetNullValue(const Value: String);
    function GetAddQuotes: Boolean;
    procedure SetAddQuotes(const Value: Boolean);
    property Value: String read GetValue write SetValue;
    property AddQuotes: Boolean read GetAddQuotes write SetAddQuotes;
    property NullValue: String read GetNullValue write SetNullValue;
  end;

  ISQLMacro = interface
    ['{8674DCBD-D1DF-4CB9-9682-6F28A0D10E08}']
  end;

  ISQLMacroConcat = interface(ISQLMacro)
    ['{206A0038-DC75-4768-89E2-4000A13B4EC6}']
    function GetValues: IORMList<IConcatMacroItem>;
    function Add(const Value: String; const NullValue: String = ''; const AddQuotes: Boolean = False): ISQLMacroConcat;
    property Values: IORMList<IConcatMacroItem> read GetValues;
  end;

  ISQLFieldAlias = interface
    ['{2CA9E5A2-E33D-474F-B8A2-C381B117C204}']
    function GetAlias: String;
    procedure SetAlias(const Value: String);
    property Alias: String read GetAlias write SetAlias;
  end;

  ISQLFieldName = interface
    ['{B6381136-DA5C-4AEF-A895-64786C2CD694}']
    function GetFieldName: String;
    procedure SetFieldName(const Value: String);
    property FieldName: String read GetFieldName write SetFieldName;
  end;

  ISQLTextFieldWithAlias = interface(ISQLFieldName)
    ['{9928DE5A-74A4-412A-AC6D-5B81EA13CE9F}']
    function GetAlias: String;
    procedure SetAlias(const Value: String);
    property Alias: String read GetAlias write SetAlias;
  end;

  ISQLMacroFieldName = interface(ISQLMacro)
    ['{B2F1952E-6483-43D9-826B-0FE0440658C6}']
    function GetFieldName: String;
    procedure SetFieldName(const Value: String);
    property FieldName: String read GetFieldName write SetFieldName;
  end;

  ISQLMacroDate = interface(ISQLMacro)
    ['{41D90851-C373-43C6-BB4D-439AB10487AC}']
    function GetDateUnit: TSQLMacroDateUnit;
    procedure SetDateUnit(const Value: TSQLMacroDateUnit);
    property DateUnit: TSQLMacroDateUnit read GetDateUnit write SetDateUnit;
  end;

  ISQLMacroDateLocalise = interface(ISQLMacroFieldName)
    ['{FD703174-043E-44A8-AC16-58FA062C8A03}']
    function GetUTCOffsetMinutes: Integer;
    procedure SetUTCOffsetMinutes(const Value: Integer);
    property UTCOffsetMinutes: Integer read GetUTCOffsetMinutes write SetUTCOffsetMinutes;
  end;

  ISQLMacroTimestampDiff = interface(ISQLMacroDate)
    ['{233FD65F-0575-4350-A428-9DA35F763070}']
    function GetEndDateField: String;
    function GetStartDateField: String;
    procedure SetEndDateField(const Value: String);
    procedure SetStartDateField(const Value: String);
    property StartDateField: String read GetStartDateField write SetStartDateField;
    property EndDateField: String read GetEndDateField write SetEndDateField;
  end;

  ISQLOrderByItem = interface(ISQLFieldName)
    ['{BDE59786-31CC-49C1-9413-A193B5384167}']
    function GetOrderByDirection: TSQLOrderByDirection;
    procedure SetOrderByDirection(const Value: TSQLOrderByDirection);
    property OrderByDirection: TSQLOrderByDirection read GetOrderByDirection write SetOrderByDirection;
  end;

  ISQLFieldNameItem = interface(ISQLTextFieldWithAlias)
    ['{9A5D641F-9A60-4D17-84CF-B26B770E526F}']
    function GetMacro: ISQLMacro;
    procedure SetMacro(const Value: ISQLMacro);
    function GetSelectCriteria: ISQLSelectCriteria;
    procedure SetSelectCriteria(const Value: ISQLSelectCriteria);
    property Macro: ISQLMacro read GetMacro write SetMacro;
    property SelectCriteria: ISQLSelectCriteria read GetSelectCriteria write SetSelectCriteria;
  end;

  ISQLFieldValueItem = interface(ISQLFieldName)
    ['{0E5C1C87-00E2-4FA9-A12F-76D95E1D924A}']
    function GetValue: Variant;
    function GetValueStream: TStream;
    procedure SetValue(const Value: Variant);
    procedure SetValueStream(const Stream: TStream);
    procedure SetParamType(const Value: TFieldType);
    function GetUseParameter: Boolean;
    function GetParamType: TFieldType;
    procedure SetUseParameter(const Value: Boolean);
    property Value: Variant read GetValue write SetValue;
    property ValueStream: TStream read GetValueStream write SetValueStream;
    property UseParameter: Boolean read GetUseParameter write SetUseParameter;
    property ParamType: TFieldType read GetParamType write SetParamType;
  end;

  ISQLJoinItem = interface
    ['{694E144C-0FF2-43E8-A201-FC45647AF659}']
    function GetAlias: String;
    function GetComparitor: ISQLComparitor;
    function GetJoinType: TSQLTableJoin;
    function GetTableName: String;
    procedure SetAlias(const Value: String);
    procedure SetComparitor(const Value: ISQLComparitor);
    procedure SetJoinType(const Value: TSQLTableJoin);
    procedure SetTableName(const Value: String);
    property TableName: String read GetTableName write SetTableName;
    property Alias: String read GetAlias write SetAlias;
    property JoinType: TSQLTableJoin read GetJoinType write SetJoinType;
    property Comparitor: ISQLComparitor read GetComparitor write SetComparitor;
  end;

  ISQLQuery = interface
    ['{2DF8FDFD-A908-472A-9794-6BD574AC04AC}']
    function GetSQL: String;
    procedure SetSQL(const SQL: String);
    function GetParameters: ISQLParameters;

    property SQL: String read GetSQL write SetSQL;
    property Parameters: ISQLParameters read GetParameters;
    function GetFormattedSQL(const ReplaceParameters: Boolean): String;
  end;

  ISQLDataset = interface(ISQLQuery)
    ['{AF7E35FA-B431-46B6-A6FC-FE9C598DFF01}']
    function Dataset: TDataset;
    function NativeDataset: IInterface;
    procedure Open;
    procedure Close;

    function FieldByName(const FieldName: string): TField;
    function FindField(const FieldName: string): TField;
    function FindFirst: Boolean;
    function FindLast: Boolean;
    function FindNext: Boolean;
    function FindPrior: Boolean;
    procedure First;
    procedure GetFieldNames(List: TStrings);
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    function MoveBy(Distance: Integer): Integer;
    procedure Next;
    procedure Prior;
    function Bof: Boolean;
    function Eof: Boolean;
    function FieldCount: Integer;
    function Fields: TFields;
    function RecordCount: Integer;
    function RecNo: Integer;
    procedure GoToRecord(const RecordNumber: Integer);
    function GetParameters: ISQLParameters;
    function ToJSONString(const IncludeMetaData: Boolean = True): String;
    procedure LoadFromJSON(const JSON: String);
    procedure LoadFromStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto);
    procedure SaveToStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto); overload;
    procedure SaveToStream(const AStream: TStream; const DatasetStoreItems: TDatasetStoreItems; const DatasetStreamType: TDatasetStreamType = dssAuto); overload;
  end;

  ISQLCommand = interface(ISQLQuery)
    ['{49043603-E7AF-4CC9-9964-33214DDD2C46}']
    function GetRowsAdded: Integer;
    procedure SetRowsAdded(const Value: Integer);
    function Execute: Integer; overload;
    function Open: Integer; overload;
    function NativeSQLCommand: IInterface;
    property RowsAdded: Integer read GetRowsAdded write SetRowsAdded;
    function FieldByName(const FieldName: string): TField;
    function Fields: TFields;
  end;

  ISQLConnection = interface
    ['{E2051BE5-4908-4AEC-ADFA-29AD5A45E815}']
    function DoNewCommand(const SQL: string; const Parameters: ISQLParameters; const Execute: TCommandExecutType;
      const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand; overload;
    function DoNewDataset(const SQL: string; const Parameters: ISQLParameters; const Open: Boolean;
      const SQLQueryCallbacks: ISQLQueryCallbacks; const SQLDatasetClass: TSQLQueryClass): ISQLDataset; overload;
    procedure GetTableNames(const TableNames: TStrings);
    procedure GetTableFields(const TableName: String; out Fields: IORMList<IORMField>);
    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    function InTransaction: Boolean;
    function NativeConnection: TObject;
    function NativeTransaction: TObject;
    function GetAttachmentID: Cardinal;
    function NewCommand(const SQL: string; const Parameters: ISQLParameters = nil; const Execute: TCommandExecutType = ceExecute;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQL: string; const ParamNames: array of String; const ParamValues: array of Variant;
      const Execute: TCommandExecutType = ceExecute; const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewDataset(const SQL: string; const Parameters: ISQLParameters = nil; const Open: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil; const SQLDatasetClass: TSQLQueryClass = nil): ISQLDataset; overload;
    function NewDataset(const QueryCriteria: ISQLSelectCriteria;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil; const SQLDatasetClass: TSQLQueryClass = nil): ISQLDataset; overload;
    function NewDataset(const SQL: string; const ParamNames: array of String;
       const ParamValues: array of Variant; const Open: Boolean = True;
       const SQLQueryCallbacks: ISQLQueryCallbacks = nil; const SQLDatasetClass: TSQLQueryClass = nil): ISQLDataset; overload;
    function SQLEngine: ISQLEngine;
    function NewCommand(const SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function SupportsAutoIncFields: Boolean;
    function GetLastAutoIncValue(const Name: String = ''): Variant;
    function GetDatabaseProperties: IParameters;

    function KillLongRunningTransactions(const TransactionAgeSeconds: Integer): Boolean;

    function GetTransactionIsolation: TSQLTransactionIsolation;
    function GetTransactionReadOnly: Boolean;
    procedure SetTransactionIsolation(const Value: TSQLTransactionIsolation);
    procedure SetTransactionReadOnly(const Value: Boolean);
    procedure CancelConnectionSQLCommands(const ConnectionID: Cardinal = 0);

    property TransactionIsolation: TSQLTransactionIsolation read GetTransactionIsolation write SetTransactionIsolation;
    property TransactionReadOnly: Boolean read GetTransactionReadOnly write SetTransactionReadOnly;
  end;

  ISQLCriteria = interface
    ['{EC7B5A03-3538-4910-BB52-8D84B9BDE691}']
    procedure CacheID(const ID: String);

    function GetCacheID: String;
    function TableName: String;
    function TableAlias: String;
  end;

  ISQLOrderByCriteria = interface
    ['{92154796-B8E5-402C-BB4F-BBDA62037FB7}']
    function Add(const FieldName: String; const OrderByDirection: TSQLOrderByDirection = Ascending): ISQLOrderByCriteria;
    function GetOrderByCriteriaList: IORMList<ISQLOrderByItem>;
  end;

  ISQLGroupByCriteria = interface
    ['{92154796-B8E5-402C-BB4F-BBDA62037FB7}']
    function Add(const FieldName: String): ISQLGroupByCriteria;
    function GetGroupByCriteriaList: IORMList<String>;
  end;

  ISQLComparitor = interface
    ['{40140270-22B7-494E-BC15-5688D8D93E25}']
    function &And(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const Value: Variant; const ValueType: TSQLValueType = vtDefault): ISQLComparitor; overload;
    function &Or(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const Value: Variant; const ValueType: TSQLValueType = vtDefault): ISQLComparitor; overload;
    function &And(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor; overload;
    function &Or(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor; overload;
    function &And(const InnerComparitor: ISQLComparitor): ISQLComparitor; overload;
    function &Or(const InnerComparitor: ISQLComparitor): ISQLComparitor; overload;
    function GetFieldName: string;
    function GetInnerComparitor: ISQLComparitor;
    function GetNextComparitor: ISQLComparitor;
    function GetRelationalOperator: TSQLRelationalOperator;
    function GetValue: Variant;
    function GetSQLOperatorExpression: TSQLOperatorExpression;
    function GetSQLSelectCriteria: ISQLSelectCriteria;
    function GetValueType: TSQLValueType;
    function GetRoot: ISQLComparitor;
    procedure SetNextComparitor(const Comparitor: ISQLComparitor);
    procedure SetInnerComparitor(const Comparitor: ISQLComparitor);
    function Clone: ISQLComparitor;

    property FieldName: string read GetFieldName;
    property RelationalOperator: TSQLRelationalOperator read GetRelationalOperator;
    property Value: Variant read GetValue;
    property NextComparitor: ISQLComparitor read GetNextComparitor write SetNextComparitor;
    property InnerComparitor: ISQLComparitor read GetInnerComparitor write SetInnerComparitor;
    property OperatorExpression: TSQLOperatorExpression read GetSQLOperatorExpression;
  end;

  ISQLWhereCriteria = interface(ISQLCriteria)
    ['{5593E798-3B1B-4D83-896A-6536ED083B47}']
    function GetWhereCriteria: ISQLComparitor;
  end;

  ISQLSelectCriteria = interface(ISQLWhereCriteria)
    ['{44847513-E159-49A1-892B-CABBA3E40887}']
    function From(const TableName: String; const Alias: String = ''): ISQLSelectCriteria;
    function First(const Value: Integer): ISQLSelectCriteria;
    function Skip(const Value: Integer): ISQLSelectCriteria;
    function Where(const Comparitor: ISQLComparitor; const AppendOperator: TSQLOperatorExpression = &And): ISQLSelectCriteria;
    function Having(const Comparitor: ISQLComparitor; const AppendOperator: TSQLOperatorExpression = &And): ISQLSelectCriteria;
    function OrderBy(const OrderByCriteria: ISQLOrderByCriteria): ISQLSelectCriteria;
    function Field(const FieldName: String; const Alias: String; const Position: Integer = -1): ISQLSelectCriteria; overload;
    function Field(const FieldName: String; const Position: Integer): ISQLSelectCriteria; overload;
    function Field(const FieldName: String): ISQLSelectCriteria; overload;
    function Field(const SelectCriteria: ISQLSelectCriteria; const Alias: String; const Position: Integer = -1): ISQLSelectCriteria; overload;
    function Field(const SQLMacro: ISQLMacro; const Alias: String = ''; const Position: Integer = -1): ISQLSelectCriteria; overload;
    function Join(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function InnerJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function LeftJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function RightJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function FullJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function Join(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function InnerJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function LeftJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function RightJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function FullJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function SetParam(const ParamName: String; const ParamValue: Variant; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLSelectCriteria;
    function Distinct(const IsDistinct: Boolean = True): ISQLSelectCriteria;
    function GroupBy(const GroupByCriteria: ISQLGroupByCriteria): ISQLSelectCriteria;

    function RemoveField(const FieldName: String): ISQLSelectCriteria;

    function GetFirstCount: Integer;
    function GetOrderByCriteria: ISQLOrderByCriteria;
    function GetSkipCount: Integer;
    function GetWhereCriteria: ISQLComparitor;
    function GetFieldCriteriaList: IORMList<ISQLFieldNameItem>;
    function GetJoinCriteriaList: IORMList<ISQLJoinItem>;
    function TableName: String;
    function GetParameters: ISQLParameters;
    function GetDistinct: Boolean;
    function GetHavingCriteria: ISQLComparitor;
    function GetGroupByCriteria: IORMList<String>;
  end;

  IBaseSQUpdateCriteria = interface(ISQLWhereCriteria)
    ['{E7E942F3-9C74-47CD-91FD-48ABAA0981ED}']
    function GetFieldValueList: IORMList<ISQLFieldValueItem>;
  end;

  IBaseORMQueryUpdateCriteria = interface(IBaseSQUpdateCriteria)
    ['{09B58B66-266D-4B76-A2C1-2F2DD339DE19}']
    function GetReturnsList: TArray<String>;
  end;

  ISQLInsertCriteria = interface(IBaseORMQueryUpdateCriteria)
    ['{44847513-E159-49A1-892B-CABBA3E40887}']
    function Value(const FieldName: String; const Value: Variant; const UseParameter: Boolean = True; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLInsertCriteria; overload;
    function Value(const FieldName: String; const Value: TStream): ISQLInsertCriteria; overload;
    function GetUpdateExistingRecords: Boolean;
    function Returning(const FieldNames: TArray<String>): ISQLInsertCriteria;
  end;

  ISQLUpdateCriteria = interface(IBaseSQUpdateCriteria)
    ['{7B500793-3073-4AF9-B4D2-76D567F8E68D}']
    function Value(const FieldName: String; const Value: Variant; const UseParameter: Boolean = True; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLUpdateCriteria; overload;
    function Value(const FieldName: String; const Value: TStream): ISQLUpdateCriteria; overload;
    function Returning(const FieldNames: TArray<String>): ISQLUpdateCriteria;
    function Where(const ComparisonCriteria: ISQLComparitor): ISQLUpdateCriteria;
  end;

  ISQLDeleteCriteria = interface(ISQLWhereCriteria)
    ['{6061CB72-8584-44C3-9BB8-C5142DD3D1BC}']
    function Where(const ComparisonCriteria: ISQLComparitor): ISQLDeleteCriteria;
  end;

  ISQLEngine = interface
    ['{5A9EC079-1F91-40D6-88BE-AFD781D02D78}']
    procedure GenerateSQL(const QueryCriteria: ISQLSelectCriteria;
       var SQL: String; const ORMParameters: ISQLParameters); overload;
    procedure GenerateSQL(const QueryCriteria: ISQLInsertCriteria;
       var SQL: String; const ORMParameters: ISQLParameters); overload;
    procedure GenerateSQL(const QueryCriteria: ISQLUpdateCriteria;
       var SQL: String; const ORMParameters: ISQLParameters); overload;
    procedure GenerateSQL(const QueryCriteria: ISQLDeleteCriteria;
       var SQL: String; const ORMParameters: ISQLParameters); overload;

    function GenerateFieldSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: iSQLParameters): String;
    function GenerateFromSQL(const QueryCriteria: ISQLCriteria; const Parameters: iSQLParameters): String;
    function GenerateFirstSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: iSQLParameters): String;
    function GenerateSkipSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: iSQLParameters): String;
    function GenerateWhereSQL(const QueryCriteria: ISQLWhereCriteria; const Parameters: iSQLParameters): String;
    function GenerateOrderBySQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: iSQLParameters): String;
    function GenerateComparisonSQL(const ComparisonCriteria: ISQLComparitor; const QueryCriteria: ISQLWhereCriteria = nil; const Parameters: iSQLParameters = nil): String;
    function GenerateJoinSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: iSQLParameters): String;
    function GetRootComparisonCriteria(const ComparisonCriteria: ISQLComparitor): ISQLComparitor;

    function GetTokenBooleanFalse: String;
    function GetTokenBooleanTrue: String;
    procedure SetTokenBooleanFalse(const Value: String);
    procedure SetTokenBooleanTrue(const Value: String);
    function BooleanToToken(const Value: Boolean): String;

    property TokenBooleanTrue: String read GetTokenBooleanTrue write SetTokenBooleanTrue;
    property TokenBooleanFalse: String read GetTokenBooleanFalse write SetTokenBooleanFalse;
  end;

  IAttributes = interface
    ['{309DB827-1C15-48E2-B3B4-03E4D4B5B909}']
    procedure AddAttributes(var AttributeString: String);
    procedure SetAttributeValue(const AttributeName: String; const AttributeValue: Variant);
    function HasAttribute(const AttributeName: String): Boolean;
    function GetAttribute(const AttributeName: String; out AttributeValue: Variant): Boolean;
    function GetAttributeWithDefault(const AttributeName: String; const DefaultAttributeValue: Variant): Variant;
  end;

  IORMFieldMapping = interface
    ['{2D1DB0A8-923D-4435-A57F-501AF43AEA7F}']
    function GetAttributes: IAttributes;
    function GetDatabaseFieldName: String;
    function GetPropertyInfo: PTypeInfo;
    function GetPropertyName: String;
    function GetDatabaseFieldSize: Integer;
    procedure SetAttributes(const Value: IAttributes);
    procedure SetDatabaseFieldName(const Value: String);
    procedure SetPropertyInfo(const Value: PTypeInfo);
    procedure SetPropertyName(const Value: String);
    procedure SetDatabaseFieldSize(const Value: Integer);
    property DatabaseFieldName: String read GetDatabaseFieldName write SetDatabaseFieldName;
    property PropertyName: String read GetPropertyName write SetPropertyName;
    property PropertyInfo: PTypeInfo read GetPropertyInfo write SetPropertyInfo;
    property Attributes: IAttributes read GetAttributes write SetAttributes;
    property DatabaseFieldSize: Integer read GetDatabaseFieldSize write SetDatabaseFieldSize;
  end;

  IORMFieldMappings = interface
    ['{52C502E0-E254-4D61-A14C-9FE772B99EF0}']
    function GetItem(Index: Integer): IORMFieldMapping;
    procedure SetItem(Index: Integer; const Value: IORMFieldMapping);
    function AddFieldMapping(const ORMObject: TOBject; const Attributes: String): IORMFieldMapping; overload;
    function AddFieldMapping(const PropertyName, DatabaseFieldName: String; const PropertyInfo: PTypeInfo): IORMFieldMapping; overload;
    procedure AddFieldMapping(const ORMFieldMapping: IORMFieldMapping); overload;
    function FindByPropertyName(const PropertyName: String; const RaiseException: Boolean = True): IORMFieldMapping;
    function FindByDatabaseFieldName(const DatabaseFieldName: String; const RaiseException: Boolean = True): IORMFieldMapping;
    function Count: Integer;
    procedure Clear;
    property Items[Index: Integer]: IORMFieldMapping read GetItem write SetItem; default;
  end;

  IORMTableMapping = interface
    ['{F48443D1-DA38-46C0-AD4F-F96F26F1B229}']
    function GetAttributes: IAttributes;
    function GetFieldMappings: IORMFieldMappings;
    function GetObjectClass: TClass;
    function GetPrimaryKeyFieldMappings: IORMFieldMappings;
    function GetTableName: String;
    procedure SetObjectClass(const Value: TClass);
    procedure SetTableName(const Value: String);
    property PrimaryKeyFieldMappings: IORMFieldMappings read GetPrimaryKeyFieldMappings;
    property FieldMappings: IORMFieldMappings read GetFieldMappings;
    property ObjectClass: TClass read GetObjectClass write SetObjectClass;
    property TableName: String read GetTableName write SetTableName;
    property Attributes: IAttributes read GetAttributes;
  end;

  IORMTableMappings = interface
    ['{A7CFC1CB-7259-4C33-B066-3500F462A54E}']
    procedure AddTableFieldMappings(const ORMObjectClass: TClass; const TableName: String;
      const DatabaseFieldMappings: Array of String; const SQLConnection: ISQLConnection = nil);
    function GetTableFieldMappings(const ORMObject: TObject; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function GetTableFieldMappings(const ORMObjectClassName: String; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function GetTableFieldMappingsByTableName(const TableName: String; const RaiseException: Boolean = True): IORMTableMapping;
  end;

  IORM = interface
    ['{0A7F7672-E283-42FA-862E-D5B9CC331135}']
    function GetORMSessionEndBehaviour: TORMSessionEndBehaviour;
    function GetUTCOffsetParameterName: String;
    function FindAttributesByFieldName(const AClassName, FieldName: String): IAttributes;
    function ValidateObject(const AObject: TObject; var ErrorMessage: String; const IncludeChildClasses: Boolean = True): Boolean; overload;
    procedure ValidateObject(const AObject: TObject; const IncludeChildClasses: Boolean = True); overload;
    procedure WalkObject(const AObject: TObject;  const IncludeChildClasses, IncludeParentClasses: Boolean;
      const ObjectWalkFunc: TObjectWalkFunc);
    procedure WalkClass(const AClass: TClass; const IncludeChildClasses, IncludeParentClasses: Boolean;
      const ClassWalkFunc: TClassWalkFunc);

    function FindTableMapping(const ORMObject: TObject; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function FindTableMapping(const ORMObjectClassName: String; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function FindTableMappingByTableName(const TableName: String; const RaiseException: Boolean = True): IORMTableMapping;

    function FindMatchingAttributes(const AClass: TClass; const AttributeNames: TStrings; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function FindMatchingAttributes(const AClasses: array of TClass; const AttributeNames: TStrings; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function FindMatchingAttributes(const AClasses: array of TClass; const AttributeName: String; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function FindMatchingAttributes(const AClass: TClass; const AttributeName: String; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;

    function IsValidORMObject(const ObjectClass: TClass): Boolean;

    function NewORMSession(const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions): IInterface;
  end;

  IFieldIDGenerator = interface
    ['{483E3626-4DA5-4009-83DD-A7A4636E1FB8}']
    function GetGeneratorValue(const GeneratorName: String): Variant;
  end;

implementation

end.







