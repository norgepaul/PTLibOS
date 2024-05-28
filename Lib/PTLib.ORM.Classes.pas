{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Classes                      }
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

unit PTLib.ORM.Classes;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Generics.Collections, System.SyncObjs, Data.DB,
  System.RTTI, System.Variants,

  PTLib.Common.TypeInfo,

  PTLib.ORM.Types,
  PTLib.ORM.Interfaces;

type
  EORMError = class(Exception);
  EORMObjectNotFoundError = class(EORMError);
  EORMGeneratorFoundError = class(EORMError);
  EORMIncorrectPrimaryKeyCountError = class(EORMError);
  EORMParentClassOnlyAllowedOnePrimaryKey = class(EORMError);
  EORMFieldMacroNotFound = class(EORMError);
  EORMNoConnectionavailable = class(EORMError);
  EORMUnsupportedPrimaryKeyType = class(EORMError);
  EORMUnsupportedInterface = class(EORMError);
  EORMObjectValidationError = class(EORMError);
  EORMGeneratorNotSupportedError = class(EORMError);
  EORMAttachmentIDNotSupportedError = class(EORMError);
  EORMUnableToStoreObject = class(EORMError);
  EORMFunctionNotImplemented = class(EORMError);
  EORMDatabaseFieldNotFound = class(EORMError);
  EORMEventNotAssigned = class(EORMError);
  EORMIncompatibleSearchTerms = class(EORMError);
  EORMImportError = class(EORMError);
  EORMSequenceError = class(EORMError);

  TObjectArray = Array of TObject;

  TSQLFieldName = class(TInterfacedObject,
                        ISQLMacroFieldName)
  strict private
    FFieldName: String;
  protected
    function GetFieldName: String;
    procedure SetFieldName(const Value: String);
  public
    property FieldName: String read GetFieldName write SetFieldName;
  end;

  TSQLOrderByItem = class(TSQLFieldName,
                          ISQLOrderByItem)
  strict private
    FOrderByDirection: TSQLOrderByDirection;
  private
    function GetOrderByDirection: TSQLOrderByDirection;
    procedure SetOrderByDirection(const Value: TSQLOrderByDirection);
  public
    property OrderByDirection: TSQLOrderByDirection read GetOrderByDirection write SetOrderByDirection;
  end;

  TSQLFieldNameItem = class(TSQLFieldName,
                            ISQLFieldNameItem)
  strict private
    FAlias: String;
    FMacro: ISQLMacro;
    FSelectCriteria: ISQLSelectCriteria;
  private
    function GetAlias: String;
    procedure SetAlias(const Value: String);
    function GetMacro: ISQLMacro;
    procedure SetMacro(const Value: ISQLMacro);
    function GetSelectCriteria: ISQLSelectCriteria;
    procedure SetSelectCriteria(const Value: ISQLSelectCriteria);
  public
    property Alias: String read GetAlias write SetAlias;
    property Macro: ISQLMacro read GetMacro write SetMacro;
    property SelectCriteria: ISQLSelectCriteria read GetSelectCriteria write SetSelectCriteria;
  end;

  TSQLFieldValueItem = class(TSQLFieldName,
                             ISQLFieldValueItem)
  strict private
    FValue: Variant;
    FValueStream: TStream;
    FUseParameter: Boolean;
    FParamType: TFieldType;
  private
    function GetValue: Variant;
    function GetValueStream: TStream;
    procedure SetValue(const Value: Variant);
    procedure SetValueStream(const Stream: TStream);
    function GetUseParameter: Boolean;
    procedure SetUseParameter(const Value: Boolean);
    function GetParamType: TFieldType;
    procedure SetParamType(const Value: TFieldType);
  public
    destructor Destroy; override;

    property Value: Variant read GetValue write SetValue;
    property ValueStream: TStream read GetValueStream write SetValueStream;
    property UseParameter: Boolean read GetUseParameter write SetUseParameter;
    property ParamType: TFieldType read GetParamType write SetParamType;
  end;

  TSQLMacroDate = class(TSQLFieldName,
                        ISQLMacroDate)
  strict private
    FDateUnit: TSQLMacroDateUnit;
  private
    function GetDateUnit: TSQLMacroDateUnit;
    procedure SetDateUnit(const Value: TSQLMacroDateUnit);
  public
    constructor Create;

    property DateUnit: TSQLMacroDateUnit read GetDateUnit write SetDateUnit;
  end;

  TSQLMacroDateLocalise = class(TSQLFieldName,
                                ISQLMacroDateLocalise)
  strict private
    FUTCOffsetMinutes: Integer;
  protected
    function GetUTCOffsetMinutes: Integer;
    procedure SetUTCOffsetMinutes(const Value: Integer);
  public
    property UTCOffsetMinutes: Integer read GetUTCOffsetMinutes write SetUTCOffsetMinutes;
  end;

  TSQLMacroTimestampDiff = class(TSQLMacroDate,
                                 ISQLMacroTimestampDiff)
  strict private
    FStartDateField: String;
    FEndDateField: String;
  private
    function GetEndDateField: String;
    function GetStartDateField: String;
    procedure SetEndDateField(const Value: String);
    procedure SetStartDateField(const Value: String);
  public
    property StartDateField: String read GetStartDateField write SetStartDateField;
    property EndDateField: String read GetEndDateField write SetEndDateField;
  end;

  TConcatMacroItem = class(TInterfacedObject,
                           IConcatMacroItem)
  strict private
    FValue: String;
    FNullValue: String;
    FAddQuotes: Boolean;
  private
    function GetValue: String;
    procedure SetValue(const Value: String);
    function GetNullValue: String;
    procedure SetNullValue(const Value: String);
    function GetAddQuotes: Boolean;
    procedure SetAddQuotes(const Value: Boolean);
  public
    property Value: String read GetValue write SetValue;
    property AddQuotes: Boolean read GetAddQuotes write SetAddQuotes;
    property NullValue: String read GetNullValue write SetNullValue;
  end;

  TSQLMacroConcat = class(TInterfacedObject,
                               ISQLMacroConcat)
  strict private
    FValues: IORMList<IConcatMacroItem>;
  private
    function GetValues: IORMList<IConcatMacroItem>;
  public
    constructor Create;

    function Add(const Value: String; const NullValue: String = ''; const AddQuotes: Boolean = False): ISQLMacroConcat;

    property Values: IORMList<IConcatMacroItem> read GetValues;
  end;

  TSQLJoinItem = class(TInterfacedObject,
                       ISQLJoinItem)
  strict private
    FTableName: String;
    FAlias: String;
    FJoinType: TSQLTableJoin;
    FComparitor: ISQLComparitor;
  private
    function GetAlias: String;
    function GetComparitor: ISQLComparitor;
    function GetJoinType: TSQLTableJoin;
    function GetTableName: String;
    procedure SetAlias(const Value: String);
    procedure SetComparitor(const Value: ISQLComparitor);
    procedure SetJoinType(const Value: TSQLTableJoin);
    procedure SetTableName(const Value: String);
  public
    property TableName: String read GetTableName write SetTableName;
    property Alias: String read GetAlias write SetAlias;
    property JoinType: TSQLTableJoin read GetJoinType write SetJoinType;
    property Comparitor: ISQLComparitor read GetComparitor write SetComparitor;
  end;

  TSmartPointer<T: class, constructor> = class(TInterfacedObject, ISmartPointer<T>)
  private
    FValue: T;
  public
    constructor Create; overload;
    constructor Create(AValue: T); overload;
    destructor Destroy; override;
    function Invoke: T;
  end;

  TAttributes = class(TInterfacedObject,
                      IAttributes)
  strict private
    FAttributes: TDictionary<String, Variant>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddAttributes(var AttributeString: String);
    procedure SetAttributeValue(const AttributeName: String; const AttributeValue: Variant);
    function HasAttribute(const AttributeName: String): Boolean;
    function GetAttribute(const AttributeName: String; out AttributeValue: Variant): Boolean;
    function GetAttributeWithDefault(const AttributeName: String; const DefaultAttributeValue: Variant): Variant;
  end;

  TORMFieldMapping = class(TInterfacedObject,
                           IORMFieldMapping)
  private
    FDatabaseFieldName: String;
    FPropertyName: String;
    FPropertyInfo: PTypeInfo;
    FAttributes: IAttributes;
    FDatabaseFieldSize: Integer;
    function GetAttributes: IAttributes;
    function GetDatabaseFieldName: String;
    function GetPropertyInfo: PTypeInfo;
    function GetPropertyName: String;
    procedure SetAttributes(const Value: IAttributes);
    procedure SetDatabaseFieldName(const Value: String);
    procedure SetPropertyInfo(const Value: PTypeInfo);
    procedure SetPropertyName(const Value: String);
    function GetDatabaseFieldSize: Integer;
    procedure SetDatabaseFieldSize(const Value: Integer);
  public
    constructor Create;

    property DatabaseFieldName: String read GetDatabaseFieldName write SetDatabaseFieldName;
    property PropertyName: String read GetPropertyName write SetPropertyName;
    property PropertyInfo: PTypeInfo read GetPropertyInfo write SetPropertyInfo;
    property Attributes: IAttributes read GetAttributes write SetAttributes;
    property DatabaseFieldSize: Integer read GetDatabaseFieldSize write SetDatabaseFieldSize;
  end;

  TORMFieldMappings = class(TInterfacedObject,
                            IORMFieldMappings)
  strict private
    FFieldMappings: IORMList<IORMFieldMapping>;
  private
    function GetItem(Index: Integer): IORMFieldMapping;
    procedure SetItem(Index: Integer; const Value: IORMFieldMapping);
  public
    constructor Create;

    function AddFieldMapping(const AObject: TObject; const Attributes: String): IORMFieldMapping; overload;
    function AddFieldMapping(const PropertyName, DatabaseFieldName: String; const PropertyInfo: PTypeInfo): IORMFieldMapping; overload;
    procedure AddFieldMapping(const ORMFieldMapping: IORMFieldMapping); overload;
    function FindByPropertyName(const PropertyName: String; const RaiseException: Boolean = True): IORMFieldMapping;
    function FindByDatabaseFieldName(const DatabaseFieldName: String; const RaiseException: Boolean = True): IORMFieldMapping;
    function Count: Integer;
    procedure Clear;
    property Items[Index: Integer]: IORMFieldMapping read GetItem write SetItem; default;
  end;

  TORMTableMapping = class(TInterfacedObject,
                           IORMTableMapping)
  strict private
    FPrimaryKeyFieldMappings: IORMFieldMappings;
    FFieldMappings: IORMFieldMappings;
    FObjectClass: TClass;
    FTableName: String;
    FAttributes: IAttributes;
  private
    function GetAttributes: IAttributes;
    function GetFieldMappings: IORMFieldMappings;
    function GetObjectClass: TClass;
    function GetPrimaryKeyFieldMappings: IORMFieldMappings;
    function GetTableName: String;
    procedure SetObjectClass(const Value: TClass);
    procedure SetTableName(const Value: String);
  public
    constructor Create;

    property PrimaryKeyFieldMappings: IORMFieldMappings read GetPrimaryKeyFieldMappings;
    property FieldMappings: IORMFieldMappings read GetFieldMappings;
    property ObjectClass: TClass read GetObjectClass write SetObjectClass;
    property TableName: String read GetTableName write SetTableName;
    property Attributes: IAttributes read GetAttributes;
  end;

  TORMTableMappings = class(TInterfacedObject,
                            IORMTableMappings)
  strict private
    FORMObjectMappingDictionary: TDictionary<String, IORMTableMapping>;
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTableFieldMappings(const ObjectClass: TClass; const TableName: String;
      const DatabaseFieldMappings: Array of String; const SQLConnection: ISQLConnection = nil); overload;
    procedure AddTableFieldMappings(const ORMTableMapping: IORMTableMapping); overload;
    function GetTableFieldMappings(const AObject: TObject; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function GetTableFieldMappings(const ObjectClassName: String; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function GetTableFieldMappingsByTableName(const TableName: String; const RaiseException: Boolean = True): IORMTableMapping;
  end;

  TSQLParameter = class(TInterfacedObject,
                        ISQLParameter)
  strict private
    FName: String;
    FValue: Variant;
    FValueStream: TStream;
    FVarType: TVarType;
    FMemoryStream: TMemoryStream;
    FFieldType: TFieldType;
  private
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
  public
    destructor Destroy; override;

    function ValueStreamAsMemoryStream: TMemoryStream;

    property Name: String read GetName write SetName;
    property Value: Variant read GetValue write SetValue;
    property ValueStream: TStream read GetValueStream write SetValueStream;
    property VarType: TVarType read GetVarType write SetVarType;
    property FieldType: TFieldType read GetFieldType write SetFieldType;
  end;

  TSQLParameters = class(TInterfacedObject,
                         ISQLParameters)
  strict private
    FSQLParameters: IORMList<ISQLParameter>;
  private
    function GetItems(Index: Integer): ISQLParameter;
    procedure SetItems(Index: Integer; const Value: ISQLParameter);
    function FindOrAddParam(const ParamName: String): ISQLParameter;
  public
    constructor Create; overload;
    constructor Create(const ParameterNames: Array of String;
      const ParameterValues: Array of Variant); overload;

    procedure Clear;
    function FindParam(const ParameterName: String): ISQLParameter;
    function SetParam(const ParamName: String; const ParamValue: Variant; const ParamType: TFieldType): ISQLParameter; overload;
    function SetParam(const ParamName: String; const ParamValue: TStream): ISQLParameter; overload;
    procedure SetParams(const ParamNames: array of String; const ParamValues: array of Variant);
    function Count: Integer;
    property Items[Index: Integer]: ISQLParameter read GetItems write SetItems; default;
  end;

  TORMField = class(TInterfacedObject,
                    IORMField)
  strict private
    FFieldName: String;
    FCalculated: Boolean;
    FCanModify: Boolean;
    FLifeCycle: TFieldLifeCycle;
    FDataSize: Integer;
    FDataType: TFieldType;
    FDisplayName: string;
    FFieldNo: Integer;
    FFullName: string;
    FIsIndexField: Boolean;
    FOffset: Integer;
    FSize: Integer;
    FAlignment: TAlignment;
    FAutoGenerateValue: TAutoRefreshFlag;
    FCustomConstraint: string;
    FConstraintErrorMessage: string;
    FDefaultExpression: string;
    FDisplayLabel: string;
    FDisplayWidth: Integer;
    FFieldKind: TFieldKind;
  private
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
  public
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

  TSessionPagingOptions = class(TObject)
  strict private
    FPagingType: TPagingType;
    FPage: Integer;
    FPageSize: Integer;
    FRecordCount: Integer;
  public
    procedure Assign(const SessionPagingOptions: TSessionPagingOptions); virtual;

    property PagingType: TPagingType read FPagingType write FPagingType;
    property Page: Integer read FPage write FPage;
    property PageSize: Integer read FPageSize write FPageSize;
    property RecordCount: Integer read FRecordCount write FRecordCount;
  end;

  TSessionSearchOptions = class(TObject)
  strict private
    FSearchTerm: String;
    FSearchFields: TStringList;
    FSearchOperator: TSQLRelationalOperator;
  private
    procedure SetSearchFields(const Value: TStringList);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(const SessionSearchOptions: TSessionSearchOptions); virtual;

    property SearchTerm: String read FSearchTerm write FSearchTerm;
    property SearchFields: TStringList read FSearchFields write SetSearchFields;
    property SearchOperator: TSQLRelationalOperator read FSearchOperator write FSearchOperator;
  end;

  TSessionTimestampOptions = class(TObject)
  strict private
    FFirstDateTime: TDateTime;
    FLastDateTime: TDateTime;
    FEnabled: Boolean;
    FDateTimeFieldName: String;
  public
    procedure Assign(const SessionTimestampOptions: TSessionTimestampOptions); virtual;

    property FirstDateTime: TDateTime read FFirstDateTime write FFirstDateTime;
    property LastDateTime: TDateTime read FLastDateTime write FLastDateTime;
    property Enabled: Boolean read FEnabled write FEnabled;
    property DateTimeFieldName: String read FDateTimeFieldName write FDateTimeFieldName;
  end;

  TSessionOptions = class(TObject)
  strict private
    FSessionPagingOptions: TSessionPagingOptions;
    FSessionSearchOptions: TSessionSearchOptions;
    FSessionTimestampOptions: TSessionTimestampOptions;
    FIncludeDeleted: TIncludeDeleted;
    FUTCOffset: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetToDefault; virtual;
    procedure Assign(const SessionOptions: TSessionOptions); virtual;

    property Paging: TSessionPagingOptions read FSessionPagingOptions;
    property Search: TSessionSearchOptions read FSessionSearchOptions;
    property TimestampOptions: TSessionTimestampOptions read FSessionTimestampOptions write FSessionTimestampOptions;
    property IncludeDeleted: TIncludeDeleted read FIncludeDeleted write FIncludeDeleted;
    property UTCOffset: Integer read FUTCOffset write FUTCOffset;
  end;

  TORMList<T> = class(TInterfacedObject,
                   IORMList<T>)
  strict private
    FInternalList: System.Generics.Collections.TList<T>;
  private
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; const Value: T);
    function Add(const Value: T): Integer;
    procedure Clear;
    function IndexOf(const Value: T): Integer;
    function Remove(Value: T): Integer;
    procedure Delete(Index: Integer);
    function Insert(const Index: Integer; const Value: T): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount write SetCount;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

function GetPropertyObject(const AObject: TObject;
  const PropertyName: String): TObject;
procedure ClearObjectArray(const ObjectArray: TObjectArray);
procedure TransferArrayItems(const Instance: TObject;
  const ObjectArray: TObjectArray; const AddMethodName: String = 'Add');

implementation

resourcestring
  StrInvalidDatabaseFie = 'Invalid Database ORM Field Mapping: %s';
  StrUnknownTableMappin = 'Unknown ORM Table Mapping: %s';
  StrUnknownPropertyS = 'Unknown ORM property "%s" for object "%s"';
  StrUnknownPropertyName = 'Unknown ORM Property name "%s"';
  StrUnknownDatabaseFie = 'Unknown ORM Database field name "%s"';
  StrMethodSNotFound = 'ORM list Method %s not found';
  StrParamNameAndParamV = 'ParamName and ParamValue counts must match.';

procedure TransferArrayItems(const Instance: TObject;
  const ObjectArray: TObjectArray; const AddMethodName: String);
var
  Found: Boolean;
  LMethod: TRttiMethod;
  LIndex: Integer;
  LParams: TArray<TRttiParameter>;
  i: Integer;
  RTTIContext: TRttiContext;
  RttiType: TRttiType;
begin
  Found := False;
  LMethod := nil;

  if length(ObjectArray) > 0 then
  begin
    RTTIContext := TRttiContext.Create;
    RttiType := RTTIContext.GetType(Instance.ClassInfo);

    for LMethod in RttiType.GetMethods do
    begin
      if SameText(LMethod.Name, AddMethodName) then
      begin
        LParams := LMethod.GetParameters;

        if length(LParams) = 1 then
        begin
          Found := True;

          for LIndex := 0 to length(LParams) - 1 do
          begin
            if LParams[LIndex].ParamType.Handle <> TValue(ObjectArray[0]).TypeInfo
            then
            begin
              Found := False;

              Break;
            end;
          end;
        end;

        if Found then
          Break;
      end;
    end;

    if Found then
    begin
      for i := Low(ObjectArray) to High(ObjectArray) do
      begin
        LMethod.Invoke(Instance, [ObjectArray[i]]);
      end;
    end
    else
    begin
      raise Exception.CreateFmt(StrMethodSNotFound, [AddMethodName]);
    end;
  end;
end;

function IsAttribute(const Value: String): Boolean;
begin
  Result := (Value <> '') and
            (Value[1] = '[') and
            (Value[Value.Length] = ']');
end;

function GetPropertyObject(const AObject: TObject;
  const PropertyName: String): TObject;
begin
  Result := TObject(NativeInt(GetPropValue(AObject, PropertyName)));
end;

procedure ClearObjectArray(const ObjectArray: TObjectArray);
var
  i: Integer;
begin
  for i := Low(ObjectArray) to High(ObjectArray) do
    ObjectArray[i].Free;
end;

(*class function TObjectHelper.CreateInstance<T>: T;
var
  AValue: TValue;
  ctx: TRttiContext;
  rType: TRttiType;
  AMethCreate: TRttiMethod;
  instanceType: TRttiInstanceType;
begin
  ctx := TRttiContext.Create;

  rType := ctx.GetType(TypeInfo(T));

  for AMethCreate in rType.GetMethods do
  begin
    if (AMethCreate.IsConstructor) and
       (Length(AMethCreate.GetParameters) = 0) then
    begin
      instanceType := rType.AsInstance;

      AValue := AMethCreate.Invoke(instanceType.MetaclassType, []);

      Exit(AValue.AsType<T>);
    end;
  end;
end; *)

{ TSP<T> }

constructor TSmartPointer<T>.Create;
begin
  inherited;

  FValue := T.Create;
end;

constructor TSmartPointer<T>.Create(AValue: T);
begin
  inherited Create;

  if AValue = nil then
    FValue := T.Create
  else
    FValue := AValue;
end;

destructor TSmartPointer<T>.Destroy;
begin
  FValue.Free;

  inherited;
end;

function TSmartPointer<T>.Invoke: T;
begin
  Result := FValue;
end;

{ TORMTableMappings }

constructor TORMTableMapping.Create;
begin
  FPrimaryKeyFieldMappings := TORMFieldMappings.Create;
  FFieldMappings := TORMFieldMappings.Create;
  FAttributes := TAttributes.Create;
end;

function TORMTableMapping.GetAttributes: IAttributes;
begin
  Result := FAttributes;
end;

function TORMTableMapping.GetFieldMappings: IORMFieldMappings;
begin
  Result := FFieldMappings;
end;

function TORMTableMapping.GetObjectClass: TClass;
begin
  Result := FObjectClass;
end;

function TORMTableMapping.GetPrimaryKeyFieldMappings: IORMFieldMappings;
begin
  Result := FPrimaryKeyFieldMappings;
end;

function TORMTableMapping.GetTableName: String;
begin
  Result := FTableName;
end;

procedure TORMTableMapping.SetObjectClass(const Value: TClass);
begin
  FObjectClass := Value;
end;

procedure TORMTableMapping.SetTableName(const Value: String);
begin
  FTableName := Value;
end;

{ TORMTableMappings }

procedure TORMTableMappings.AddTableFieldMappings(const ObjectClass: TClass;
  const TableName: String; const DatabaseFieldMappings: Array of String; const SQLConnection: ISQLConnection);

  function FindFieldParam(const FieldParams: TStrings; const ParamName: String; var ParamValue: String): Boolean;
  var
    i: Integer;
  begin
    Result := False;

    for i := 0 to pred(FieldParams.Count) do
    begin
      if SameText(Trim(FieldParams.Names[i]), ParamName) then
      begin
        Result := True;

        ParamValue := FieldParams.Values[FieldParams.Names[i]]
      end;
    end;
  end;

  procedure AddFieldMappings(const ORMTableMapping: IORMTableMapping);
  var
    i: Integer;
    AObject: TObject;
    FieldMapping: IORMFieldMapping;
  begin
    AObject := ObjectClass.Create;
    try
      // Build the list of known fields
      for i := Low(DatabaseFieldMappings) to High(DatabaseFieldMappings) do
      begin
        FieldMapping := ORMTableMapping.FieldMappings.AddFieldMapping(
          AObject,
          DatabaseFieldMappings[i]);

        if FieldMapping.Attributes.HasAttribute(AttributePrimaryKey) then
          ORMTableMapping.PrimaryKeyFieldMappings.AddFieldMapping(
            AObject,
            DatabaseFieldMappings[i]);
      end;
    finally
      FreeAndNil(AObject);
    end;
  end;

var
  ORMTableMapping: IORMTableMapping;
  Attributes: String;
  Fields: IORMList<IORMField>;
  Found: Boolean;
  i, n: Integer;
begin
  ORMTableMapping := TORMTableMapping.Create;

  ORMTableMapping.ObjectClass := ObjectClass;

  Attributes := TableName;

  ORMTableMapping.Attributes.AddAttributes(Attributes);

  ORMTableMapping.TableName := Attributes;

  AddFieldMappings(ORMTableMapping);

  if (ORMTableMapping.TableName <> '') and
     (SQLConnection <> nil) then
  begin
    SQLConnection.GetTableFields(
      ORMTableMapping.TableName,
      Fields);

    for i := 0 to pred(ORMTableMapping.FieldMappings.Count) do
    begin
      if (ORMTableMapping.FieldMappings[i].PropertyName <> '') and
         (ORMTableMapping.FieldMappings[i].DatabaseFieldName <> '') then
      begin
        Found := False;

        for n := 0 to pred(Fields.Count) do
        begin
          if SameText(Fields[n].FieldName, ORMTableMapping.FieldMappings[i].DatabaseFieldName) then
          begin
            Found := True;

            if Fields[n].DataType in [ftString, ftFixedChar, ftWideString, ftFixedWideChar] then
            begin
              ORMTableMapping.FieldMappings[i].DatabaseFieldSize := Fields[n].Size;

              Break;
            end;
          end;
        end;

        if not Found then
        begin
          raise EORMDatabaseFieldNotFound.CreateFmt('Database field "%s" not found in table "%s"',
            [ORMTableMapping.FieldMappings[i].DatabaseFieldName,
             TableName]);
        end;
      end;
    end;
  end;

  AddTableFieldMappings(ORMTableMapping);
end;

procedure TORMTableMappings.AddTableFieldMappings(const ORMTableMapping: IORMTableMapping);
begin
  FLock.BeginWrite;
  try
    // Remove the entry if it already exists
    if FORMObjectMappingDictionary.ContainsKey(UpperCase(ORMTableMapping.ObjectClass.ClassName)) then
    begin
      FORMObjectMappingDictionary.Remove(UpperCase(ORMTableMapping.ObjectClass.ClassName));
    end;

    FORMObjectMappingDictionary.Add(UpperCase(ORMTableMapping.ObjectClass.ClassName), ORMTableMapping);
  finally
    FLock.EndWrite;
  end;
end;

constructor TORMTableMappings.Create;
begin
  FORMObjectMappingDictionary := TDictionary<String, IORMTableMapping>.Create;
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TORMTableMappings.Destroy;
begin
  FreeAndNil(FORMObjectMappingDictionary);
  FreeAndNil(FLock);

  inherited;
end;

function TORMTableMappings.GetTableFieldMappings(
  const ObjectClassName: String; const RaiseException: Boolean): IORMTableMapping;
begin
  FLock.BeginRead;
  try
    if (not FORMObjectMappingDictionary.TryGetValue(UpperCase(ObjectClassName), Result)) and
       (RaiseException) then
      raise EORMError.CreateFmt(StrUnknownTableMappin, [ObjectClassName]);
  finally
    FLock.EndRead;
  end;
end;

function TORMTableMappings.GetTableFieldMappingsByTableName(const TableName: String;
  const RaiseException: Boolean): IORMTableMapping;
var
  Key: String;
begin
  FLock.BeginRead;
  try
    Result := nil;

    for Key in FORMObjectMappingDictionary.Keys do
    begin
      if (SameText(FORMObjectMappingDictionary[Key].TableName, TableName)) and
         (not FORMObjectMappingDictionary[Key].Attributes.HasAttribute(AttributePartialTable)) then
      begin
        Result := FORMObjectMappingDictionary[Key];

        Break;
      end;
    end;

    if (RaiseException) and
       (Result = nil) then
      raise EORMError.CreateFmt(StrUnknownTableMappin, [TableName]);
  finally
    FLock.EndRead;
  end;
end;

function TORMTableMappings.GetTableFieldMappings(
  const AObject: TObject; const RaiseException: Boolean): IORMTableMapping;
begin
  FLock.BeginRead;
  try
    if (not FORMObjectMappingDictionary.TryGetValue(UpperCase(AObject.ClassName), Result)) and
       (RaiseException) then
      raise EORMError.CreateFmt(StrUnknownTableMappin, [AObject.ClassName]);
  finally
    FLock.EndRead;
  end;
end;

{ TORMFieldMappings }

function TORMFieldMappings.AddFieldMapping(const AObject: TObject;
  const Attributes: String): IORMFieldMapping;
var
  i, p: Integer;
  PropInfo: PTypeInfo;
  PropertyNames: TStringList;
  AttributeValues, PropertyName, FieldName: String;
begin
  Result := TORMFieldMapping.Create;

  p := pos('=', Attributes);
  PropertyName := Trim(copy(Attributes, 1, p - 1));

  AttributeValues := copy(Attributes, p + 1, MaxInt);

  Result.Attributes.AddAttributes(AttributeValues);

  // Find the params
  FieldName := AttributeValues;

  PropInfo := nil;

  PropertyNames := TStringList.Create;
  try
    GetPropertyNames(AObject, PropertyNames);

    for i := 0 to pred(PropertyNames.Count) do
    begin
      if SameText(PropertyNames[i], PropertyName) then
      begin
        PropInfo := GetPropInfo(AObject, PropertyName, [])^.PropType^;

        Break;
      end;
    end;
  finally
    FreeAndNil(PropertyNames);
  end;

  if (PropInfo = nil) and
     (FieldName <> '') and
     (PropertyName <> '') then
    raise EORMError.CreateFmt(StrUnknownPropertyS, [PropertyName, AObject.ClassName]);

  Result.DatabaseFieldName := FieldName;
  Result.PropertyName := PropertyName;
  Result.PropertyInfo := PropInfo;

  FFieldMappings.Add(Result);
end;

{ TSQLParameter }

destructor TSQLParameter.Destroy;
begin
  FreeAndNil(FMemoryStream);

  inherited;
end;

function TSQLParameter.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TSQLParameter.GetName: String;
begin
  Result := FName;
end;

function TSQLParameter.GetValue: Variant;
begin
  Result := FValue;
end;

function TSQLParameter.GetValueStream: TStream;
begin
  Result :=  FValueStream;
end;

function TSQLParameter.GetVarType: TVarType;
begin
  Result := FVarType;
end;

procedure TSQLParameter.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TSQLParameter.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TSQLParameter.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

procedure TSQLParameter.SetValueStream(const Value: TStream);
begin
  if FValueStream = nil then
    FValueStream := TMemoryStream.Create
  else
    FValueStream.Position := 0;

  Value.Position := 0;
  FValueStream.CopyFrom(Value, Value.Size);
end;

procedure TSQLParameter.SetVarType(const Value: TVarType);
begin
  FVarType := Value;
end;

function TSQLParameter.ValueStreamAsMemoryStream: TMemoryStream;
begin
  if FMemoryStream <> nil then
  begin
    Result := FMemoryStream;
  end else
  if FValueStream = nil then
  begin
    Result := nil;
  end
  else
  begin
    FMemoryStream := TMemoryStream.Create;
    FValueStream.Position := 0;
    FMemoryStream.CopyFrom(FValueStream, FValueStream.Size);
    Result := FMemoryStream;
  end;
end;

{ TSQLParameters }

procedure TSQLParameters.Clear;
begin
  FSQLParameters.Clear;
end;

function TSQLParameters.Count: Integer;
begin
  Result := FSQLParameters.Count;
end;

procedure TSQLParameters.SetParams(const ParamNames: array of String;
  const ParamValues: array of Variant);
var
  i: Integer;
begin
  Assert(high(ParamNames) = high(ParamValues), StrParamNameAndParamV);

  for i := 0 to high(ParamNames) do
    SetParam(ParamNames[i], ParamValues[i], TFieldType.ftUnknown);
end;

constructor TSQLParameters.Create(const ParameterNames: array of String;
  const ParameterValues: array of Variant);
var
  i: Integer;
begin
  Create;

  Assert(length(ParameterNames) = Length(ParameterValues), 'Value and name count do not match');

  for i := Low(ParameterNames) to High(ParameterNames) do
    SetParam(ParameterNames[i], ParameterValues[i], TFieldType.ftUnknown);
end;

constructor TSQLParameters.Create;
begin
  FSQLParameters := TORMList<ISQLParameter>.Create;
end;

function TSQLParameters.FindParam(const ParameterName: String): ISQLParameter;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(Count) do
    if SameText(FSQLParameters.Items[i].Name, ParameterName) then
      Exit(FSQLParameters.Items[i]);
end;

function TSQLParameters.GetItems(Index: Integer): ISQLParameter;
begin
  Result := FSQLParameters[Index];
end;

procedure TSQLParameters.SetItems(Index: Integer; const Value: ISQLParameter);
begin
  FSQLParameters[Index] := Value;
end;

function TSQLParameters.SetParam(const ParamName: String;
  const ParamValue: TStream): ISQLParameter;
begin
  Result := FindOrAddParam(ParamName);

  Result.ValueStream := ParamValue;
end;

function TSQLParameters.FindOrAddParam(const ParamName: String): ISQLParameter;
begin
  Result := FindParam(ParamName);

  if Result = nil then
  begin
    Result := TSQLParameter.Create;
    Result.Name := ParamName;

    FSQLParameters.Add(Result);
  end;
end;

function TSQLParameters.SetParam(const ParamName: String;
  const ParamValue: Variant; const ParamType: TFieldType): ISQLParameter;
begin
  Result := FindOrAddParam(ParamName);

  Result.Value := ParamValue;
  Result.FieldType := ParamType;
  //Result.VarType := VarType;
end;

{ TORMFieldMappings }

function TORMFieldMappings.FindByPropertyName(const PropertyName: String;
  const RaiseException: Boolean): IORMFieldMapping;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(FFieldMappings.Count) do
    if SameText(PropertyName, FFieldMappings.Items[i].PropertyName) then
    begin
      Result := FFieldMappings.Items[i];

      Break;
    end;

  if (RaiseException) and
     (Result = nil) then
    raise EORMError.CreateFmt(StrUnknownPropertyName, [PropertyName]);
end;

procedure TORMFieldMappings.AddFieldMapping(
  const ORMFieldMapping: IORMFieldMapping);
begin
  FFieldMappings.Add(ORMFieldMapping);
end;

function TORMFieldMappings.AddFieldMapping(const PropertyName,
  DatabaseFieldName: String; const PropertyInfo: PTypeInfo): IORMFieldMapping;
begin
  Result := TORMFieldMapping.Create;

  Result.PropertyName := PropertyName;
  Result.PropertyInfo := PropertyInfo;
  Result.DatabaseFieldName := DatabaseFieldName;

  AddFieldMapping(Result);
end;

procedure TORMFieldMappings.Clear;
begin
  FFieldMappings.Clear;
end;

function TORMFieldMappings.Count: Integer;
begin
  Result := FFieldMappings.Count;
end;

function TORMFieldMappings.GetItem(Index: Integer): IORMFieldMapping;
begin
  Result := FFieldMappings[Index];
end;

procedure TORMFieldMappings.SetItem(Index: Integer;
  const Value: IORMFieldMapping);
begin
  FFieldMappings[Index] := Value;
end;

constructor TORMFieldMappings.Create;
begin
  FFieldMappings := TORMList<IORMFieldMapping>.Create;
end;

function TORMFieldMappings.FindByDatabaseFieldName(const DatabaseFieldName: String;
  const RaiseException: Boolean): IORMFieldMapping;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(FFieldMappings.Count) do
    if SameText(DatabaseFieldName, FFieldMappings.Items[i].DatabaseFieldName) then
    begin
      Result := FFieldMappings.Items[i];

      Break;
    end;

  if (RaiseException) and
     (Result = nil) then
    raise EORMError.CreateFmt(StrUnknownDatabaseFie, [DatabaseFieldName]);
end;

{ TORMFieldMapping }

constructor TORMFieldMapping.Create;
begin
  FAttributes := TAttributes.Create;
end;

function TORMFieldMapping.GetAttributes: IAttributes;
begin
  Result := FAttributes;
end;

function TORMFieldMapping.GetDatabaseFieldName: String;
begin
  Result := FDatabaseFieldName;
end;

function TORMFieldMapping.GetDatabaseFieldSize: Integer;
begin
  Result := FDatabaseFieldSize;
end;

function TORMFieldMapping.GetPropertyInfo: PTypeInfo;
begin
  Result := FPropertyInfo;
end;

function TORMFieldMapping.GetPropertyName: String;
begin
  Result := FPropertyName;
end;

procedure TORMFieldMapping.SetAttributes(const Value: IAttributes);
begin
  FAttributes := Value;
end;

procedure TORMFieldMapping.SetDatabaseFieldName(const Value: String);
begin
  FDatabaseFieldName := Value;
end;

procedure TORMFieldMapping.SetDatabaseFieldSize(const Value: Integer);
begin
  FDatabaseFieldSize := Value;
end;

procedure TORMFieldMapping.SetPropertyInfo(const Value: PTypeInfo);
begin
  FPropertyInfo := Value;
end;

procedure TORMFieldMapping.SetPropertyName(const Value: String);
begin
  FPropertyName := Value;
end;

{ TAttributes }

procedure TAttributes.AddAttributes(var AttributeString: String);
var
  Attributes: TStringList;
  Attribute, AttributeName, AttributeValue: String;
  i, p: Integer;
begin
  Attributes := TStringList.Create;
  try
    Attributes.StrictDelimiter := True;

    // Load the field name into comma text (just in case we use additional params)
    Attributes.CommaText := AttributeString;

    // Strip square brackets
    for i := pred(Attributes.Count) downto 0 do
    begin
      Attribute := Trim(Attributes[i]);

      if IsAttribute(Attribute) then
      begin
        // Remove the brackets
        Attribute := Trim(copy(Attributes[i], 2, Attributes[i].Length - 2));

        // Remove the attribute
        Attributes.Delete(i);

        // Get the name and value
        p := pos('=', Attribute);

        if p = 0 then
        begin
          AttributeName := Attribute;
          AttributeValue := '';
        end
        else
        begin
          AttributeName := Trim(copy(Attribute, 1, p - 1));
          AttributeValue := VarToStr(Trim(copy(Attribute, p + 1, MaxInt)));
        end;

        // Add the attribute
        SetAttributeValue(AttributeName, AttributeValue);
      end;
    end;

    AttributeString := Trim(Attributes.Text);
  finally
    FreeAndNil(Attributes);
  end;
end;

constructor TAttributes.Create;
begin
  FAttributes := TDictionary<String, Variant>.Create;
end;

destructor TAttributes.Destroy;
begin
  FreeAndNil(FAttributes);

  inherited;
end;

function TAttributes.GetAttributeWithDefault(const AttributeName: String; const DefaultAttributeValue: Variant): Variant;
begin
  if not GetAttribute(AttributeName, Result) then
    Result := DefaultAttributeValue;
end;

function TAttributes.GetAttribute(const AttributeName: String; out AttributeValue: Variant): Boolean;
begin
  Result := FAttributes.TryGetValue(UpperCase(AttributeName), AttributeValue);
end;

function TAttributes.HasAttribute(const AttributeName: String): Boolean;
begin
  Result := FAttributes.ContainsKey(UpperCase(AttributeName));
end;

procedure TAttributes.SetAttributeValue(const AttributeName: String; const AttributeValue: Variant);
begin
  FAttributes.AddOrSetValue(UpperCase(AttributeName), AttributeValue);
end;

{ TSessionOptions }

procedure TSessionOptions.Assign(const SessionOptions: TSessionOptions);
begin
  Paging.Assign(SessionOptions.Paging);
  Search.Assign(SessionOptions.Search);
  TimestampOptions.Assign(SessionOptions.TimestampOptions);

  IncludeDeleted := SessionOptions.IncludeDeleted;
  UTCOffset := SessionOptions.UTCOffset;
end;

constructor TSessionOptions.Create;
begin
  FSessionPagingOptions := TSessionPagingOptions.Create;
  FSessionSearchOptions := TSessionSearchOptions.Create;
  FSessionTimestampOptions := TSessionTimestampOptions.Create;

  ResetToDefault;
end;

destructor TSessionOptions.Destroy;
begin
  FreeAndNil(FSessionPagingOptions);
  FreeAndNil(FSessionSearchOptions);
  FreeAndNil(FSessionTimestampOptions);

  inherited;
end;

procedure TSessionOptions.ResetToDefault;
begin
  Paging.PagingType := poNone;
  Paging.Page := 0;
  Paging.PageSize := 0;
  Paging.RecordCount := 0;

  Search.SearchTerm := '';
  Search.SearchFields.Clear;

  FIncludeDeleted := TIncludeDeleted.idDefault;
  FUTCOffset := 0;
end;

{ TSQLFieldName }

function TSQLFieldName.GetFieldName: String;
begin
  Result := FFieldName;
end;

procedure TSQLFieldName.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

{ TSQLOrderByItem }

function TSQLOrderByItem.GetOrderByDirection: TSQLOrderByDirection;
begin
  Result := FOrderByDirection;
end;

procedure TSQLOrderByItem.SetOrderByDirection(
  const Value: TSQLOrderByDirection);
begin
  FOrderByDirection := Value;
end;

{ TSQLFieldNameItem }

function TSQLFieldNameItem.GetAlias: String;
begin
  Result := FAlias;
end;

function TSQLFieldNameItem.GetMacro: ISQLMacro;
begin
  Result := FMacro;
end;

function TSQLFieldNameItem.GetSelectCriteria: ISQLSelectCriteria;
begin
  Result := FSelectCriteria;
end;

procedure TSQLFieldNameItem.SetAlias(const Value: String);
begin
  FAlias := Value;
end;

procedure TSQLFieldNameItem.SetMacro(const Value: ISQLMacro);
begin
  FMacro := Value;
end;

procedure TSQLFieldNameItem.SetSelectCriteria(const Value: ISQLSelectCriteria);
begin
  FSelectCriteria := Value;
end;

{ TSQLFieldValueItem }

destructor TSQLFieldValueItem.Destroy;
begin
  FreeAndNil(FValueStream);

  inherited;
end;

function TSQLFieldValueItem.GetParamType: TFieldType;
begin
  Result := FParamType;
end;

function TSQLFieldValueItem.GetUseParameter: Boolean;
begin
  Result := FUseParameter;
end;

function TSQLFieldValueItem.GetValue: Variant;
begin
  Result := FValue;
end;

function TSQLFieldValueItem.GetValueStream: TStream;
begin
  Result := FValueStream;
end;

procedure TSQLFieldValueItem.SetParamType(const Value: TFieldType);
begin
  FParamType := Value;
end;

procedure TSQLFieldValueItem.SetUseParameter(const Value: Boolean);
begin
  FUseParameter := Value;
end;

procedure TSQLFieldValueItem.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

procedure TSQLFieldValueItem.SetValueStream(const Stream: TStream);
begin
  if FValueStream = nil then
    FValueStream := TMemoryStream.Create;

  Stream.Position := 0;
  FValueStream.CopyFrom(Stream, Stream.Size);
end;

{ TSQLJoinItem }

function TSQLJoinItem.GetAlias: String;
begin
  Result := FAlias;
end;

function TSQLJoinItem.GetComparitor: ISQLComparitor;
begin
  Result := FComparitor;
end;

function TSQLJoinItem.GetJoinType: TSQLTableJoin;
begin
  Result := FJoinType;
end;

function TSQLJoinItem.GetTableName: String;
begin
  Result := FTableName;
end;

procedure TSQLJoinItem.SetAlias(const Value: String);
begin
  FAlias := Value;
end;

procedure TSQLJoinItem.SetComparitor(const Value: ISQLComparitor);
begin
  FComparitor := Value;
end;

procedure TSQLJoinItem.SetJoinType(const Value: TSQLTableJoin);
begin
  FJoinType := Value;
end;

procedure TSQLJoinItem.SetTableName(const Value: String);
begin
  FTableName := Value;
end;

{ TSQLMacroDateLocalise }

function TSQLMacroDateLocalise.GetUTCOffsetMinutes: Integer;
begin
  Result := FUTCOffsetMinutes;
end;

procedure TSQLMacroDateLocalise.SetUTCOffsetMinutes(const Value: Integer);
begin
  FUTCOffsetMinutes := Value;
end;

{ TORMList<T> }

function TORMList<T>.Add(const Value: T): Integer;
begin
  Result := FInternalList.Add(Value);
end;

procedure TORMList<T>.Clear;
begin
  FInternalList.Clear;
end;

constructor TORMList<T>.Create;
begin
  FInternalList := System.Generics.Collections.TList<T>.Create;
end;

procedure TORMList<T>.Delete(Index: Integer);
begin
  FInternalList.Delete(Index);
end;

destructor TORMList<T>.Destroy;
begin
  FreeAndNil(FInternalList);

  inherited;
end;

function TORMList<T>.GetCount: Integer;
begin
  Result := FInternalList.Count;
end;

function TORMList<T>.GetItem(Index: Integer): T;
begin
  Result := FInternalList[Index];
end;

function TORMList<T>.IndexOf(const Value: T): Integer;
begin
  Result := FInternalList.IndexOf(Value);
end;

function TORMList<T>.Insert(const Index: Integer; const Value: T): Integer;
begin
  if (Index < 0) or
     (Index >= Count) then
  begin
    Result := Add(Value)
  end
  else
  begin
    FInternalList.Insert(Index, Value);

    Result := Index;
  end;
end;

function TORMList<T>.Remove(Value: T): Integer;
begin
  Result := FInternalList.Remove(Value);
end;

procedure TORMList<T>.SetCount(Value: Integer);
begin
  FInternalList.Count := Value;
end;

procedure TORMList<T>.SetItem(Index: Integer; const Value: T);
begin
  FInternalList.Items[Index] := Value;
end;

{ TSessionSearchOptions }

procedure TSessionSearchOptions.Assign(const SessionSearchOptions: TSessionSearchOptions);
begin
  SearchTerm := SessionSearchOptions.SearchTerm;
  SearchFields.Assign(SessionSearchOptions.SearchFields);
end;

constructor TSessionSearchOptions.Create;
begin
  FSearchFields := TStringList.Create;

  FSearchOperator := TSQLRelationalOperator.Containing;
end;

destructor TSessionSearchOptions.Destroy;
begin
  FreeAndNil(FSearchFields);

  inherited;
end;

procedure TSessionSearchOptions.SetSearchFields(const Value: TStringList);
begin
  FSearchFields.Assign(Value);
end;

{ TSQLMacroConcat }

function TSQLMacroConcat.Add(const Value, NullValue: String;
  const AddQuotes: Boolean): ISQLMacroConcat;
var
  ConcatMacroItem: IConcatMacroItem;
begin
  Result := Self;

  ConcatMacroItem := TConcatMacroItem.Create;
  ConcatMacroItem.Value := Value;
  ConcatMacroItem.NullValue := NullValue;
  ConcatMacroItem.AddQuotes := AddQuotes;

  FValues.Add(ConcatMacroItem);
end;

constructor TSQLMacroConcat.Create;
begin
  FValues := TORMList<IConcatMacroItem>.Create;
end;

function TSQLMacroConcat.GetValues: IORMList<IConcatMacroItem>;
begin
  Result := FValues;
end;

{ TConcatMacroItem }

function TConcatMacroItem.GetAddQuotes: Boolean;
begin
  Result := FAddQuotes;
end;

function TConcatMacroItem.GetNullValue: String;
begin
  Result := FNullValue;
end;

function TConcatMacroItem.GetValue: String;
begin
  Result := FValue;
end;

procedure TConcatMacroItem.SetAddQuotes(const Value: Boolean);
begin
  FAddQuotes := Value;
end;

procedure TConcatMacroItem.SetNullValue(const Value: String);
begin
  FNullValue := Value;
end;

procedure TConcatMacroItem.SetValue(const Value: String);
begin
  FValue := Value;
end;

{ TSessionTimestampOptions }

procedure TSessionTimestampOptions.Assign(const SessionTimestampOptions: TSessionTimestampOptions);
begin
  FirstDateTime := SessionTimestampOptions.FirstDateTime;
  LastDateTime := SessionTimestampOptions.LastDateTime;
  Enabled := SessionTimestampOptions.Enabled;
  DateTimeFieldName := SessionTimestampOptions.DateTimeFieldName;
end;

{ TSessionPagingOptions }

procedure TSessionPagingOptions.Assign(const SessionPagingOptions: TSessionPagingOptions);
begin
  PagingType := SessionPagingOptions.PagingType;
  Page := SessionPagingOptions.Page;
  PageSize := SessionPagingOptions.PageSize;
  RecordCount := SessionPagingOptions.RecordCount;
end;

{ TORMField }

function TORMField.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TORMField.GetAutoGenerateValue: TAutoRefreshFlag;
begin
  Result := FAutoGenerateValue;
end;

function TORMField.GetCalculated: Boolean;
begin
  Result := FCalculated;
end;

function TORMField.GetCanModify: Boolean;
begin
  Result := FCanModify;
end;

function TORMField.GetConstraintErrorMessage: string;
begin
  Result := FConstraintErrorMessage;
end;

function TORMField.GetCustomConstraint: string;
begin
  Result := FCustomConstraint;
end;

function TORMField.GetDataSize: Integer;
begin
  Result := FDataSize;
end;

function TORMField.GetDataType: TFieldType;
begin
  Result := FDataType;
end;

function TORMField.GetDefaultExpression: string;
begin
  Result := FDefaultExpression;
end;

function TORMField.GetDisplayLabel: string;
begin
  Result := FDisplayLabel;
end;

function TORMField.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TORMField.GetDisplayWidth: Integer;
begin
  Result := FDisplayWidth;
end;

function TORMField.GetFieldNo: Integer;
begin
  Result := FFieldNo;
end;

function TORMField.GetFullName: string;
begin
  Result := FFullName;
end;

function TORMField.GetIsIndexField: Boolean;
begin
  Result := FIsIndexField;
end;

function TORMField.GetLifeCycle: TFieldLifeCycle;
begin
  Result := FLifeCycle;
end;

function TORMField.GetFieldKind: TFieldKind;
begin
  Result := FFieldKind;
end;

function TORMField.GetFieldName: String;
begin
  Result := FFieldName;
end;

function TORMField.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TORMField.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TORMField.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
end;

procedure TORMField.SetAutoGenerateValue(const Value: TAutoRefreshFlag);
begin
  FAutoGenerateValue := Value;
end;

procedure TORMField.SetCalculated(const Value: Boolean);
begin
  FCalculated := Value;
end;

procedure TORMField.SetCanModify(const Value: Boolean);
begin
  FCanModify := Value;
end;

procedure TORMField.SetConstraintErrorMessage(const Value: string);
begin
  FConstraintErrorMessage := Value;
end;

procedure TORMField.SetCustomConstraint(const Value: string);
begin
  FCustomConstraint := Value;
end;

procedure TORMField.SetDataSize(const Value: Integer);
begin
  FDataSize := Value;
end;

procedure TORMField.SetDataType(const Value: TFieldType);
begin
  FDataType := Value;
end;

procedure TORMField.SetDefaultExpression(const Value: string);
begin
  FDefaultExpression := Value;
end;

procedure TORMField.SetDisplayLabel(const Value: string);
begin
  FDisplayLabel := Value;
end;

procedure TORMField.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TORMField.SetDisplayWidth(const Value: Integer);
begin
  FDisplayWidth := Value;
end;

procedure TORMField.SetFieldNo(const Value: Integer);
begin
  FFieldNo := Value;
end;

procedure TORMField.SetFullName(const Value: string);
begin
  FFullName := Value;
end;

procedure TORMField.SetIsIndexField(const Value: Boolean);
begin
  FIsIndexField := Value;
end;

procedure TORMField.SetLifeCycle(const Value: TFieldLifeCycle);
begin
  FLifeCycle := Value;
end;

procedure TORMField.SetFieldKind(const Value: TFieldKind);
begin
  FFieldKind := Value;
end;

procedure TORMField.SetFieldName(const Value: String);
begin
  FFieldName := Value;
end;

procedure TORMField.SetOffset(const Value: Integer);
begin
  FOffset := Value;
end;

procedure TORMField.SetSize(const Value: Integer);
begin
  FSize := Value;
end;

{ TSQLMacroDate }

constructor TSQLMacroDate.Create;
begin
  FDateUnit := TSQLMacroDateUnit.duMinute;
end;

function TSQLMacroDate.GetDateUnit: TSQLMacroDateUnit;
begin
  Result := FDateUnit;
end;

procedure TSQLMacroDate.SetDateUnit(const Value: TSQLMacroDateUnit);
begin
  FDateUnit := Value;
end;

{ TSQLMacroTimestampDiff }

function TSQLMacroTimestampDiff.GetEndDateField: String;
begin
  Result := FEndDateField;
end;

function TSQLMacroTimestampDiff.GetStartDateField: String;
begin
  Result := FStartDateField;
end;

procedure TSQLMacroTimestampDiff.SetEndDateField(const Value: String);
begin
  FEndDateField := Value;
end;

procedure TSQLMacroTimestampDiff.SetStartDateField(const Value: String);
begin
  FStartDateField := Value;
end;

end.





