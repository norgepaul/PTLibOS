{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM                                                }
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

unit PTLib.ORM;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes,
  System.Variants, System.SyncObjs, Data.DB, System.TypInfo,

  PTLib.ORM.Interfaces,
  PTLib.ORM.Classes,
  PTLib.ORM.Session,
  PTLib.ORM.Types;

type
  TOnNeedORMObjectMapping = procedure(Sender: TObject; const ORMObjectClassName: String; const TableMappings: IORMTableMappings) of object;
  TOnORMSessionEvent = procedure(Sender: TObject; const ORMSession: IORMSession) of object;
  TOnBeforeORMSessionCreated = procedure(Sender: TObject;
    var ORMSessionConnectionClass: TORMSessionConnectionClass; var SQLConnection: ISQLConnection;
    var ORMSessionOptions: TORMSessionOptions) of object;
  TOnNeedORMSessionConnectionClass = procedure(Sender: TObject; var ORMSessionConnectionClass: TORMSessionConnectionClass) of object;
  TOnIsValidORMObject = procedure(Sender: TObject; const ORMObjectClass: TClass; var IsValidORMObject: Boolean) of object;

  TCustomORM = class;

  TORMConnectionProvider = class(TComponent)
  public
    function NewConnection: ISQLConnection; virtual;
  end;

  TORMLink = class(TInterfacedObject, IORM)
  private
    FORM: TCustomORM;
  protected
    property ORM: TCustomORM read FORM write FORM implements IORM;
  end;

  TDataTypeInspector = Class
  public
    type
      TDataTypeMapping = (diExact,diCommon);

    class function  QueryDataType(const Value:Variant; const MapOptions:TDataTypeMapping=diCommon):TFieldType;
  end;

  TCustomORM = class(TComponent,
                     IORM)
  private
    FOnNeedORMObjectMapping: TOnNeedORMObjectMapping;
    FOnAfterORMSessionCreated: TOnORMSessionEvent;
    FOnBeforeORMSessionCreated: TOnBeforeORMSessionCreated;
    FOnNeedORMSessionConnectionClass: TOnNeedORMSessionConnectionClass;
    FOnIsValidORMObject: TOnIsValidORMObject;

    FORMSessionEndBehaviour: TORMSessionEndBehaviour;
    FORMLink: IORM;
    FNeedNativeConnectionCS: TCriticalSection;
    FUTCOffsetParameterName: String;
    FConnectionProvider: TORMConnectionProvider;
    FMatchingAttributeNames: TStrings;
    FMatchingFieldMappings: IORMFieldMappings;

    function InternalGetORMSession(const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions): IORMSession;
    function GetORMSessionEndBehaviour: TORMSessionEndBehaviour;
    procedure SetORMSessionEndBehaviour(const Value: TORMSessionEndBehaviour);
    function GetUTCOffsetParameterName: String;
    function GetConnectionProvider: TORMConnectionProvider;
    function FindMatchingAttributesClassWalkFunc(
      const TableMapping: IORMTableMapping;
      const FieldMapping: IORMFieldMapping; const InheritanceLevel,
      ClassDepth: Integer): Boolean;
    function NewORMSession(const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions): IInterface;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoOnNeedORMObjectMapping(const ORMObjectClassName: String; const TableMappings: IORMTableMappings); virtual;
    procedure DoNeedORMSessionConnectionClass(var ORMSessionConnectionClass: TORMSessionConnectionClass); virtual;
    procedure DoOnBeforeORMSessionCreated(var ORMSessionConnectionClass: TORMSessionConnectionClass;
      var SQLConnection: ISQLConnection; var ORMSessionOptions: TORMSessionOptions); virtual;
    procedure DoOnAfterORMSessionCreated(const ORMSession: IORMSession); virtual;
    function DoValidORMObject(const ObjectClass: TClass): Boolean; virtual;
    procedure DoOnIsValidORMObject(const ORMObjectClass: TClass; var IsValidORMObject: Boolean); virtual;
  protected
    property OnNeedORMObjectMapping: TOnNeedORMObjectMapping read FOnNeedORMObjectMapping write FOnNeedORMObjectMapping;
    property OnBeforeORMSessionCreated: TOnBeforeORMSessionCreated read FOnBeforeORMSessionCreated write FOnBeforeORMSessionCreated;
    property OnAfterORMSessionCreated: TOnORMSessionEvent read FOnAfterORMSessionCreated write FOnAfterORMSessionCreated;
    property OnNeedORMSessionConnectionClass: TOnNeedORMSessionConnectionClass read FOnNeedORMSessionConnectionClass write FOnNeedORMSessionConnectionClass;
    property OnIsValidORMObject: TOnIsValidORMObject read FOnIsValidORMObject write FOnIsValidORMObject;

    property UTCOffsetParameterName: String read GetUTCOffsetParameterName;
    property ORMSessionEndBehaviour: TORMSessionEndBehaviour read GetORMSessionEndBehaviour write SetORMSessionEndBehaviour;
    property ConnectionProvider: TORMConnectionProvider read FConnectionProvider write FConnectionProvider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function NewSession: IORMSession; overload;
    function NewSession(const SQLConnection: ISQLConnection): IORMSession; overload;
    function NewSession(const ORMSessionOptions: TORMSessionOptions): IORMSession; overload;
    function NewSession(const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions): IORMSession; overload;

    function FindTableMapping(const AObject: TObject; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function FindTableMapping(const ORMObjectClassName: String; const RaiseException: Boolean = True): IORMTableMapping; overload;
    function FindTableMappingByTableName(const TableName: String; const RaiseException: Boolean = True): IORMTableMapping;
    function FindAttributesByFieldName(const AClassName, FieldName: String): IAttributes;
    function ValidateObject(const AObject: TObject; var ErrorMessage: String; const IncludeChildClasses: Boolean = True): Boolean; overload;
    procedure ValidateObject(const AObject: TObject; const IncludeChildClasses: Boolean = True); overload;

    procedure WalkObject(const AObject: TObject;  const IncludeChildClasses, IncludeParentClasses: Boolean;
      const ObjectWalkFunc: TObjectWalkFunc);
    procedure WalkClass(const AClass: TClass; const IncludeChildClasses, IncludeParentClasses: Boolean;
      const ClassWalkFunc: TClassWalkFunc);
    function FindMatchingAttributes(const AClass: TClass; const AttributeNames: TStrings; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function FindMatchingAttributes(const AClasses: array of TClass;
      const AttributeNames: TStrings; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function FindMatchingAttributes(const AClasses: array of TClass; const AttributeName: String; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function FindMatchingAttributes(const AClass: TClass; const AttributeName: String; const IncludeChildClasses: Boolean = True; const IncludeParentClasses: Boolean = True): IORMFieldMappings; overload;
    function IsValidORMObject(const ObjectClass: TClass): Boolean;
  end;

  TORM = class(TCustomORM)
  published
    property OnNeedORMObjectMapping;
    property OnBeforeORMSessionCreated;
    property OnAfterORMSessionCreated;
    property OnNeedORMSessionConnectionClass;
    property OnIsValidORMObject;

    property ORMSessionEndBehaviour;
    property UTCOffsetParameterName;
    property ConnectionProvider;
  end;

  TOnLog = procedure(Sender: TObject; const LogMessage: String) of object;

  TORMComponent = class(TComponent)
  strict private
    FOnLog: TOnLog;

    FORM: TORM;
    FORMSession: IORMSession;
  private
    function GetORMSession: TORMSessionConnection;
    procedure CheckORM;
    procedure SetORM(const Value: TORM);
    function GetNewORMSession: IORMSession;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoOnLog(const LogMessage: String); virtual;

    property ORMSession: TORMSessionConnection read GetORMSession;
  public
    procedure Log(const LogMessage: String); overload; virtual;
    procedure Log(const LogMessage: String; const Args: Array of const); overload; virtual;

    property NewORMSession: IORMSession read GetNewORMSession;
  published
    property OnLog: TOnLog read FOnLog write FOnLog;

    property ORM: TORM read FORM write SetORM;
  end;

implementation

uses
  PTLib.ORM.Registry,
  PTLib.ORM.Serialization;

resourcestring
  StrInvalidPropertyValueName = 'Invalid property value name: %s';
  StrFieldnamesCountMustEqualPropertyNames = 'Fieldnames count must equal PropertyNames count';
  StrPleaseAssignAHandler = 'Please assign a handler for the OnNeedNativeConnection event';
  StrORMConnectionCannot = 'NativeConnection cannot be nil';
  StrCachingNotImplemen = 'Caching not implemented for this type';
  StrCannotAssignSTo = 'Cannot assign %s to %s';

{ TDataTypeInspector }

(* This function does a quick inspection of a variant. Using "common"
   as the mapping option will map legacy types to more modern datatypes.
   E.g "smallint" will be mapped to "integer" and "single" will map to
   "float" (double).
   Databases work faster when the fields are of intrinsic type, dividable
   by the CPU architecture (32 bits, 64 bits). So datatypes like "date"
   and "time" should be combined into ftDateTime (unless you explicitly
   need to read time separately from date as a part of your query). *)
class function TDataTypeInspector.QueryDataType(const Value:Variant;
         const MapOptions:TDataTypeMapping=diCommon):TFieldType;
begin
  (* Default to unknown *)
  result:=TFieldType.ftUnknown;

  case TVarData(value).VType of
  varSmallint:
    begin
      case MapOptions of
      diExact:  result:=TFieldType.ftSmallInt;
      diCommon: result:=TFieldType.ftInteger;
      end;
    end;
  varInteger:   result:=TFieldType.ftInteger;
  varSingle:
    begin
      case MapOptions of
      diExact:  result:=TFieldType.ftSingle;
      diCommon: result:=TFieldType.ftFloat;
      end;
    end;
  varDouble:    result:=TFieldType.ftFloat;
  varCurrency:  result:=TFieldType.ftCurrency;
  varDate:
    begin
      case MapOptions of
      diExact:  result:=TFieldType.ftDate;
      diCommon: result:=TFieldType.ftDateTime;
      end;
    end;
  varOleStr,
  varString,
  varUString:   result:=TFieldType.ftString;

  varBoolean:   result:=TFieldType.ftBoolean;
  varShortInt:
    begin
      case MapOptions of
      diExact:  result:=TFieldType.ftShortInt;
      diCommon: result:=TFieldType.ftInteger;
      end;
    end;
  varByte:
    begin
      case MapOptions of
      diExact:  result:=TFieldType.ftByte;
      diCommon: result:=TFieldType.ftInteger;
      end;
    end;
  varWord:      result:=TFieldType.ftWord;
  varLongWord:  result:=TFieldType.ftLongword;
  varInt64:     result:=TFieldType.ftLargeint;
  varUInt64:    result:=TFieldType.ftLargeint;
  end;
end;

{ TCustomORM }

constructor TCustomORM.Create(AOwner: TComponent);
begin
  inherited;

  FORMLink := TORMLink.Create;
  TORMLink(FORMLink).ORM := Self;

  FNeedNativeConnectionCS := TCriticalSection.Create;

  FUTCOffsetParameterName := 'UTC_OFFSET_MINUTES';
end;

destructor TCustomORM.Destroy;
begin
  TORMLink(FORMLink).ORM := nil;

  FreeAndNil(FNeedNativeConnectionCS);

  inherited;
end;

procedure TCustomORM.DoOnNeedORMObjectMapping(const ORMObjectClassName: String;
  const TableMappings: IORMTableMappings);
begin
  if Assigned(FOnNeedORMObjectMapping) then
    FOnNeedORMObjectMapping(Self, ORMObjectClassName, TableMappings);
end;

function TCustomORM.DoValidORMObject(const ObjectClass: TClass): Boolean;
begin
  Result := ObjectClass <> nil;

  DoOnIsValidORMObject(ObjectClass, Result);
end;

procedure TCustomORM.DoOnAfterORMSessionCreated(const ORMSession: IORMSession);
begin
  if Assigned(FOnAfterORMSessionCreated) then
    FOnAfterORMSessionCreated(Self, ORMSession);
end;

procedure TCustomORM.DoOnBeforeORMSessionCreated(
  var ORMSessionConnectionClass: TORMSessionConnectionClass;
  var SQLConnection: ISQLConnection; var ORMSessionOptions: TORMSessionOptions);
begin
  if Assigned(FOnBeforeORMSessionCreated) then
    FOnBeforeORMSessionCreated(
      Self,
      ORMSessionConnectionClass,
      SQLConnection,
      ORMSessionOptions);
end;

procedure TCustomORM.DoOnIsValidORMObject(const ORMObjectClass: TClass; var IsValidORMObject: Boolean);
begin
  if Assigned(FOnIsValidORMObject) then
    FOnIsValidORMObject(Self, ORMObjectClass, IsValidORMObject);
end;

function TCustomORM.GetConnectionProvider: TORMConnectionProvider;
begin
  Result := FConnectionProvider;
end;

function TCustomORM.GetORMSessionEndBehaviour: TORMSessionEndBehaviour;
begin
  Result := FORMSessionEndBehaviour;
end;

function TCustomORM.GetUTCOffsetParameterName: String;
begin
  Result := FUTCOffsetParameterName;
end;

procedure TCustomORM.ValidateObject(const AObject: TObject; const IncludeChildClasses: Boolean);
var
  ErrorMessage: String;
begin
  if not ValidateObject(AObject, ErrorMessage, IncludeChildClasses) then
    raise EORMObjectValidationError.Create(ErrorMessage);
end;

function TCustomORM.FindAttributesByFieldName(const AClassName,
  FieldName: String): IAttributes;
var
  CurrentClassName: String;
  TableMapping: IORMTableMapping;
  i: Integer;
begin
  Result := nil;

  CurrentClassName := AClassName;

  TableMapping := FindTableMapping(CurrentClassName, False);

  while TableMapping <> nil do
  begin
    for i := 0 to pred(TableMapping.FieldMappings.Count) do
    begin
      if SameText(TableMapping.FieldMappings[i].DatabaseFieldName, FieldName) then
      begin
        Result := TableMapping.FieldMappings[i].Attributes;

        Break;
      end;
    end;

    if TableMapping.ObjectClass.ClassParent = nil then
    begin
      Break;
    end
    else
    begin
      CurrentClassName := TableMapping.ObjectClass.ClassParent.ClassName;

      TableMapping := FindTableMapping(CurrentClassName, False);
    end;
  end;
end;

procedure TCustomORM.WalkClass(const AClass: TClass;
  const IncludeChildClasses, IncludeParentClasses: Boolean; const ClassWalkFunc: TClassWalkFunc);

  function WalkClassPropertiesRec(const ARecClass: TClass;
    const InheritanceLevel, ClassDepth: Integer): Boolean;
  var
    CurrentClassName: String;
    TableMapping: IORMTableMapping;
    i: Integer;
    InheritanceLevelInternal: Integer;
  begin
    Result := True;
    InheritanceLevelInternal := InheritanceLevel;

    if ARecClass <> nil then
    begin
      CurrentClassName := ARecClass.ClassName;

      TableMapping := FindTableMapping(CurrentClassName, False);

      // Did we find a table mapping?
      while TableMapping <> nil do
      begin
        // Step through all the properties
        for i := 0 to pred(TableMapping.FieldMappings.Count) do
        begin
          // Is this field a class?
          if (IncludeChildClasses) and
             (TableMapping.FieldMappings[i].PropertyInfo <> nil) and
             (TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass) then
          begin
            Result := WalkClassPropertiesRec(
              TableMapping.FieldMappings[i].PropertyInfo.TypeData.ClassType,
              InheritanceLevelInternal,
              ClassDepth + 1);
          end
          else
          begin
            Result := ClassWalkFunc(TableMapping, TableMapping.FieldMappings[i], InheritanceLevelInternal, ClassDepth);
          end;

          if not Result then
            Break;
        end;

        if (Result) and
           (IncludeParentClasses) and
           (TableMapping.ObjectClass.ClassParent <> nil) and
           (IsValidORMObject(TableMapping.ObjectClass.ClassParent)) then
        begin
          CurrentClassName := TableMapping.ObjectClass.ClassParent.ClassName;

          TableMapping := FindTableMapping(CurrentClassName, False);

          Inc(InheritanceLevelInternal);
        end
        else
        begin
          TableMapping := nil;
        end;
      end;
    end;
  end;

begin
  WalkClassPropertiesRec(AClass, 0, 0);
end;

function TCustomORM.FindMatchingAttributes(
  const AClasses: array of TClass; const AttributeNames: TStrings;
  const IncludeChildClasses, IncludeParentClasses: Boolean): IORMFieldMappings;
var
  i, n: Integer;
  FieldMappings: IORMFieldMappings;
begin
  Result := TORMFieldMappings.Create;

  for i := Low(AClasses) to High(AClasses) do
  begin
    FieldMappings := FindMatchingAttributes(AClasses[i], AttributeNames, IncludeChildClasses, IncludeParentClasses);

    for n := 0 to pred(FieldMappings.Count) do
      Result.AddFieldMapping(FieldMappings[n]);
  end;
end;

function TCustomORM.FindMatchingAttributes(const AClass: TClass;
  const AttributeNames: TStrings; const IncludeChildClasses, IncludeParentClasses: Boolean): IORMFieldMappings;
begin
  FMatchingFieldMappings := TORMFieldMappings.Create;

  FMatchingAttributeNames := AttributeNames;
  try
    WalkClass(AClass, IncludeChildClasses, IncludeParentClasses, FindMatchingAttributesClassWalkFunc);
  finally
    Result := FMatchingFieldMappings;

    FMatchingFieldMappings := nil;
    FMatchingAttributeNames := nil;
  end;
end;

function TCustomORM.FindMatchingAttributes(const AClasses: array of TClass;
  const AttributeName: String; const IncludeChildClasses, IncludeParentClasses: Boolean): IORMFieldMappings;
var
  AttributeNames: TStringList;
begin
  AttributeNames := TStringList.Create;
  try
    AttributeNames.Add(AttributeName);

    Result := FindMatchingAttributes(AClasses, AttributeNames, IncludeChildClasses, IncludeParentClasses);
  finally
    FreeAndNil(AttributeNames);
  end;
end;

function TCustomORM.FindMatchingAttributes(const AClass: TClass;
  const AttributeName: String; const IncludeChildClasses, IncludeParentClasses: Boolean): IORMFieldMappings;
var
  AttributeNames: TStringList;
begin
  AttributeNames := TStringList.Create;
  try
    AttributeNames.Add(AttributeName);

    Result := FindMatchingAttributes(AClass, AttributeNames, IncludeChildClasses, IncludeParentClasses);
  finally
    FreeAndNil(AttributeNames);
  end;
end;

function TCustomORM.FindMatchingAttributesClassWalkFunc(
  const TableMapping: IORMTableMapping; const FieldMapping: IORMFieldMapping;
  const InheritanceLevel, ClassDepth: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to pred(FMatchingAttributeNames.Count) do
    if FieldMapping.Attributes.HasAttribute(FMatchingAttributeNames[i]) then
    begin
      FMatchingFieldMappings.AddFieldMapping(FieldMapping);

      Break;
    end;
end;

procedure TCustomORM.WalkObject(const AObject: TObject;
  const IncludeChildClasses, IncludeParentClasses: Boolean; const ObjectWalkFunc: TObjectWalkFunc);

  function WalkObjectPropertiesRec(const ARecObject: TObject;
    const InheritanceLevel, ClassDepth: Integer): Boolean;
  var
    CurrentClassName: String;
    TableMapping: IORMTableMapping;
    i: Integer;
    InheritanceLevelInternal: Integer;
  begin
    Result := True;
    InheritanceLevelInternal := InheritanceLevel;

    if ARecObject <> nil then
    begin
      CurrentClassName := ARecObject.ClassName;

      TableMapping := FindTableMapping(CurrentClassName, False);

      // Did we find a table mapping?
      while TableMapping <> nil do
      begin
        // Step through all the properties
        for i := 0 to pred(TableMapping.FieldMappings.Count) do
        begin
          // Is this field a class?
          if (IncludeChildClasses) and
             (TableMapping.FieldMappings[i].PropertyInfo <> nil) and
             (TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass) then
          begin
            Result := WalkObjectPropertiesRec(
              GetObjectProp(
                ARecObject,
                TableMapping.FieldMappings[i].PropertyName),
              InheritanceLevelInternal,
              ClassDepth + 1);
          end
          else
          begin
            Result := ObjectWalkFunc(
              ARecObject,
              TableMapping,
              TableMapping.FieldMappings[i],
              InheritanceLevelInternal,
              ClassDepth);
          end;

          if not Result then
            Break;
        end;

        if (Result) and
           (IncludeParentClasses) and
           (TableMapping.ObjectClass.ClassParent <> nil) and
           (IsValidORMObject(TableMapping.ObjectClass.ClassParent)) then
        begin
          CurrentClassName := TableMapping.ObjectClass.ClassParent.ClassName;

          TableMapping := FindTableMapping(CurrentClassName, False);

          Inc(InheritanceLevelInternal);
        end
        else
        begin
          TableMapping := nil;
        end;
      end;
    end;
  end;

begin
  WalkObjectPropertiesRec(AObject, 0, 0);
end;

function TCustomORM.FindTableMapping(const ORMObjectClassName: String; const RaiseException: Boolean): IORMTableMapping;
begin
  Result := TORMClassRegistry.GetTableFieldMappings(
    ORMObjectClassName,
    False);

  if Result = nil then
  begin
    DoOnNeedORMObjectMapping(
      ORMObjectClassName,
      TORMClassRegistry.TableMappings);

    Result := TORMClassRegistry.GetTableFieldMappings(
      ORMObjectClassName,
      RaiseException);
  end;
end;

function TCustomORM.FindTableMappingByTableName(const TableName: String;
  const RaiseException: Boolean): IORMTableMapping;
begin
  Result := TORMClassRegistry.GetTableFieldMappingsByTableName(
    TableName,
    False);
end;

function TCustomORM.FindTableMapping(const AObject: TObject; const RaiseException: Boolean): IORMTableMapping;
begin
  Result := FindTableMapping(AObject.ClassName, RaiseException);
end;

procedure TCustomORM.DoNeedORMSessionConnectionClass(
  var ORMSessionConnectionClass: TORMSessionConnectionClass);
begin
  ORMSessionConnectionClass := TORMSessionConnection;

  if Assigned(FOnNeedORMSessionConnectionClass) then
    FOnNeedORMSessionConnectionClass(Self, ORMSessionConnectionClass);
end;

function TCustomORM.NewORMSession(const SQLConnection: ISQLConnection;
  const ORMSessionOptions: TORMSessionOptions): IInterface;
begin
  Result := InternalGetORMSession(SQLConnection, ORMSessionOptions);
end;

function TCustomORM.InternalGetORMSession(
  const SQLConnection: ISQLConnection; const ORMSessionOptions: TORMSessionOptions): IORMSession;
var
  ORMSessionConnectionClass: TORMSessionConnectionClass;
  RealORMSessionOptions: TORMSessionOptions;
  RealSQLConnection: ISQLConnection;
begin
  RealORMSessionOptions := ORMSessionOptions;

  DoNeedORMSessionConnectionClass(ORMSessionConnectionClass);

  DoOnBeforeORMSessionCreated(
    ORMSessionConnectionClass,
    RealSQLConnection,
    RealORMSessionOptions);

  if (RealSQLConnection = nil) and
     (GetConnectionProvider <> nil) then
  begin
    RealSQLConnection := GetConnectionProvider.NewConnection;
  end;

  Result := TORMSession.Create(
    FORMLink,
    ORMSessionConnectionClass,
    RealSQLConnection,
    RealORMSessionOptions);

  DoOnAfterORMSessionCreated(Result);
end;

function TCustomORM.IsValidORMObject(const ObjectClass: TClass): Boolean;
begin
  { TODO : Do we need a critical section here? Don't think so! }
  DoOnIsValidORMObject(
    ObjectClass,
    Result);
end;

function TCustomORM.NewSession(
  const ORMSessionOptions: TORMSessionOptions): IORMSession;
begin
  Result := NewSession(nil, []);
end;

function TCustomORM.NewSession(const SQLConnection: ISQLConnection;
  const ORMSessionOptions: TORMSessionOptions): IORMSession;
begin
  Result := InternalGetORMSession(SQLConnection, ORMSessionOptions);
end;

procedure TCustomORM.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FConnectionProvider then
      FConnectionProvider := nil;
  end;
end;

procedure TCustomORM.SetORMSessionEndBehaviour(
  const Value: TORMSessionEndBehaviour);
begin
  FORMSessionEndBehaviour := Value;
end;

function TCustomORM.ValidateObject(const AObject: TObject;
  var ErrorMessage: String; const IncludeChildClasses: Boolean): Boolean;
var
  CurrentClassName: String;
  TableMapping: IORMTableMapping;
  i: Integer;
  Attribute: Variant;
  NullValue: Variant;
begin
  Result := True;

  if ErrorMessage = '' then
  begin
    CurrentClassName := AObject.ClassName;

    TableMapping := FindTableMapping(CurrentClassName, False);

    // Did we find a table mapping?
    while (Result) and
          (TableMapping <> nil) do
    begin
      // Step through all the properties
      for i := 0 to pred(TableMapping.FieldMappings.Count) do
      begin
        // Is this field a class?
        if (IncludeChildClasses) and
           (TableMapping.FieldMappings[i].PropertyInfo <> nil) and
           (TableMapping.FieldMappings[i].PropertyInfo.Kind = tkClass) then
        begin
          ValidateObject(GetObjectProp(AObject, TableMapping.FieldMappings[i].PropertyName), ErrorMessage);
        end
        else
        begin
          // Do we allow 'nulls'
          if TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeNotNull, Attribute) then
          begin
            // Find the real 'null' value
            if TableMapping.FieldMappings[i].Attributes.GetAttribute(AttributeNull, Attribute) then
              NullValue := Attribute
            else
              NullValue := '';

            if GetPropValue(AObject, TableMapping.FieldMappings[i].PropertyName) = NullValue then
              ErrorMessage := format('%s must have a value', [TableMapping.FieldMappings[i].PropertyName]);
          end;
        end;

        Result := ErrorMessage = '';

        if not Result then
          Break;
      end;

      if (Result) and
         (TableMapping.ObjectClass.ClassParent <> nil) then
      begin
        CurrentClassName := TableMapping.ObjectClass.ClassParent.ClassName;

        TableMapping := FindTableMapping(CurrentClassName, False);
      end;
    end;
  end;
end;

function TCustomORM.NewSession(
  const SQLConnection: ISQLConnection): IORMSession;
begin
  Result := NewSession(SQLConnection, []);
end;

function TCustomORM.NewSession: IORMSession;
begin
  Result := NewSession(nil)
end;

{ TORMConnectionProvider }

function TORMConnectionProvider.NewConnection: ISQLConnection;
begin
  raise EORMNoConnectionavailable.Create('Unable to provice a connection. No TORMConnectionProvide assigned');
end;

{ TORMComponent }

procedure TORMComponent.CheckORM;
begin
  if FORM = nil then
    raise EORMError.Create('Please assign an ORM component');
end;

procedure TORMComponent.DoOnLog(const LogMessage: String);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, LogMessage);
end;

function TORMComponent.GetNewORMSession: IORMSession;
begin
  CheckORM;

  Result := FORM.NewSession;
end;

function TORMComponent.GetORMSession: TORMSessionConnection;
begin
  if FORMSession = nil then
    FORMSession := NewORMSession;

  Result := FORMSession.Connection;  
end;

procedure TORMComponent.Log(const LogMessage: String);
begin
  DoOnLog(LogMessage);
end;

procedure TORMComponent.Log(const LogMessage: String;
  const Args: array of const);
begin
  DoOnLog(format(LogMessage, Args));
end;

procedure TORMComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if Operation = TOperation.opRemove then
  begin
    if AComponent = FORM then
      ORM := nil;
  end;
end;

procedure TORMComponent.SetORM(const Value: TORM);
begin
  FORM := Value;
end;

end.

