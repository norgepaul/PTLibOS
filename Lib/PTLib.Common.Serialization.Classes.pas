{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Serialization.Classes                       }
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

unit PTLib.Common.Serialization.Classes;

interface

uses
  System.TypInfo, System.JSON, Generics.Collections,

  PTlib.Common.Dates,
  PTLib.Common.TypeInfo;

type
  ISerializerPropertyMapping = interface
    ['{7EC1C399-5E79-4363-95DF-5F5610348467}']
    function GetIsList: Boolean;
    function GetPropertyInfo: PTypeInfo;
    function GetPropertyName: string;

    function GetListItemClassType: TClass;
    function IsDefaultValue(const Value: Variant): Boolean;

    property IsList: Boolean read GetIsList;
    property PropertyInfo: PTypeInfo read GetPropertyInfo;
    property PropertyName: string read GetPropertyName;
  end;

  ISerializerClassMapping = interface
    ['{22F223F2-85B2-4A04-8182-D12BEEFB1E62}']
    function GetCount: Integer;
    function GetIsList: Boolean;
    function GetItem(I: Integer): ISerializerPropertyMapping;
    function GetObjectClass: TClass;

    function GetListItemClassType: TClass;

    property Count: Integer read GetCount;
    property IsList: Boolean read GetIsList;
    property Items[I: Integer]: ISerializerPropertyMapping read GetItem; default;

    property ObjectClass: TClass read GetObjectClass;
  end;

  ISerializerClassMappingFactory = interface
    ['{C774A487-F01F-430C-A728-16C4B7985679}']
    function GetClassMapping(ClassType: TClass): ISerializerClassMapping;
    function IsValidObject(ClassType: TClass): Boolean;
    function WalkParentClasses: Boolean;
  end;

  TCustomObjectWalker = class abstract
  protected
    function FindByName(JSON: TJSONArray; const Name: string; const RaiseException: Boolean = True): TJSONObject;

    function InternalWalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
      const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping; const IsListItem: Boolean = False): Boolean; virtual;

    function WalkList(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; virtual;
    function WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const ItemClassType: TClass; const Index: Integer): Boolean; virtual;
    function WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; virtual;
    procedure WalkObject(const ClassMappingFactory: ISerializerClassMappingFactory; const AObject: TObject); virtual;
    function WalkFunc(const AObject: TObject; const ClassMapping: ISerializerClassMapping;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; virtual; abstract;

    class procedure CheckObjectAssigned(const AObject: TObject);
  end;

  TCustomSerializer = class(TCustomObjectWalker)
  protected
    FStack: TStack<TJSONValue>;

    function WalkList(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;
    function WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const ItemClassType: TClass; const Index: Integer): Boolean; override;
    function WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;
    function WalkFunc(const AObject: TObject; const ClassMapping: ISerializerClassMapping;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;

    class function InternalToJSON(ClassMappingFactory: ISerializerClassMappingFactory; AObject: TObject): TJSONArray; overload;
    class procedure InternalToJSON(const JSON: TJSONArray; ClassMappingFactory: ISerializerClassMappingFactory; AObject: TObject); overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCustomDeserializer = class(TCustomObjectWalker)
  protected
    FStack: TStack<TJSONValue>;
    class function ObjectToInterface<I>(var AObject: TObject): I; static;

    function WalkList(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;
    function WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const ItemClassType: TClass; const Index: Integer): Boolean; override;
    function WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;
    function WalkFunc(const AObject: TObject; const ClassMapping: ISerializerClassMapping;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;

    class function InternalToObject<T: class, constructor>(ClassMappingFactory: ISerializerClassMappingFactory; JSON: TJSONArray): T; overload;
    class procedure InternalToObject(const AObject: TObject; ClassMappingFactory: ISerializerClassMappingFactory; JSON: TJSONArray); overload;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCustomObjectCloner = class(TCustomObjectWalker)
  protected
    FStack: TStack<TObject>;

    function WalkList(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;
    function WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const ItemClassType: TClass; const Index: Integer): Boolean; override;
    function WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory; const ARecObject: TObject;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;
    function WalkFunc(const AObject: TObject; const ClassMapping: ISerializerClassMapping;
      const PropertyMapping: ISerializerPropertyMapping): Boolean; override;

    class procedure InternalAssignTo(ClassMappingFactory: ISerializerClassMappingFactory; SrcObject, DstObject: TObject);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils, System.Rtti, System.Variants, System.RTLConsts,

  PTLib.Common.Variants, PTLib.Common.Utils, PTlib.Common.Classes;

resourcestring
  StrSerializationObject = 'Serialization object cannot be nil';

const
  sJSONItems = 'Items';
  sType = 'type';

{ TCustomObjectWalker }

function TCustomObjectWalker.WalkList(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  RttiMethod: TRttiMethod;
  I, ItemCount: Integer;
  ItemValue: TValue;
  Item: TObject;
  ItemClassType: TClass;
  ClassMapping: ISerializerClassMapping;
begin
  Result := ARecObject <> nil;

  if Result then
  begin
    RttiContext := TRttiContext.Create;
    RttiType := RttiContext.GetType(ARecObject.ClassType);

    RttiProperty := RttiType.GetProperty('Count');
    if not Assigned(RttiProperty) then
      raise EPTLibSerilizationError.CreateFmt('Class "%s" does not contain Count property', [ARecObject.ClassName]);

    ItemCount := RttiProperty.GetValue(ARecObject).AsInteger;

    RttiMethod := RttiType.GetMethod('GetItem');
    if not Assigned(RttiMethod) then
      RttiMethod := RttiType.GetMethod('GetItemRef');
    if not Assigned(RttiMethod) then
      raise EPTLibSerilizationError.CreateFmt('Class "%s" does not contain GetItem/GetItemRef methods', [ARecObject.ClassName]);

    if PropertyMapping = nil then
    begin
      ClassMapping := ClassMappingFactory.GetClassMapping(ARecObject.ClassType);

      ItemClassType:= ClassMapping.GetListItemClassType;
    end
    else
    begin
      ItemClassType := PropertyMapping.GetListItemClassType;
    end;

    for I := 0 to ItemCount - 1 do
    begin
      ItemValue := RttiMethod.Invoke(ARecObject, [I]);

      if Assigned(ItemValue.TypeInfo) and (ItemValue.TypeInfo.Kind = tkInterface) then
        Item := ItemValue.AsInterface as TObject
      else if ItemValue.IsObject then
        Item := ItemValue.AsObject
      else
        Item := ItemValue.AsType<Pointer>;

      WalkListItemPropertiesRec(ClassMappingFactory, Item, ItemClassType, I);
    end;
  end;
end;

function TCustomObjectWalker.WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const ItemClassType: TClass; const Index: Integer): Boolean;
begin
  Result := InternalWalkObjectPropertiesRec(ClassMappingFactory, ARecObject, nil);
end;

procedure TCustomObjectWalker.WalkObject(const ClassMappingFactory: ISerializerClassMappingFactory;
  const AObject: TObject);
begin
  InternalWalkObjectPropertiesRec(ClassMappingFactory, AObject, nil);
end;

function TCustomObjectWalker.WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
begin
  Result := InternalWalkObjectPropertiesRec(ClassMappingFactory, ARecObject, PropertyMapping);
end;

class procedure TCustomObjectWalker.CheckObjectAssigned(const AObject: TObject);
begin
  if AObject = nil then
  begin
    raise EPTLibSerilizationError.Create(StrSerializationObject);
  end;
end;

function TCustomObjectWalker.FindByName(JSON: TJSONArray;
  const Name: string; const RaiseException: Boolean): TJSONObject;
var
  Value: TJSONValue;
  JSONItem: TJSONValue;
begin
  Result := nil;

  for JSONItem in JSON do
    if JSONItem.TryGetValue<TJSONValue>(Name, Value) then
    begin
      Result := JSONItem as TJSONObject;
      Break;
    end;

  if not Assigned(Result) and RaiseException then
    raise EPTLibSerilizationError.CreateFmt('Propety name "%s" not found', [Name]);
end;

function TCustomObjectWalker.InternalWalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping; const IsListItem: Boolean = False): Boolean;
var
  ClassMapping: ISerializerClassMapping;
  i: Integer;
begin
  Result := True;

  if ARecObject <> nil then
  begin
    ClassMapping := ClassMappingFactory.GetClassMapping(ARecObject.ClassType);

    // Did we find a table mapping?
    while ClassMapping <> nil do
    begin
      // Step through all the properties
      for i := 0 to pred(ClassMapping.Count) do
      begin
        // Is this field a class?
        if (ClassMapping[i].PropertyInfo <> nil) and
           (ClassMapping[i].PropertyInfo.Kind = tkClass) then
        begin
          if ClassMapping[i].IsList then
          begin
            Result := WalkList(
                ClassMappingFactory,
                GetObjectProp(
                  ARecObject,
                  ClassMapping[i].PropertyName),
                ClassMapping[i]);
          end
          else
          begin
            Result := WalkObjectPropertiesRec(
              ClassMappingFactory,
              GetObjectProp(
                ARecObject,
                ClassMapping[i].PropertyName),
              ClassMapping[i]);
          end;
        end
        else
        begin
          Result := WalkFunc(
            ARecObject,
            ClassMapping,
            ClassMapping[i]);
        end;

        if not Result then
          Break;
      end;

      if (Result) and
         (ClassMappingFactory.WalkParentClasses) and
         (ClassMapping.ObjectClass.ClassParent <> nil) and
         (ClassMappingFactory.IsValidObject(ClassMapping.ObjectClass.ClassParent)) then
      begin
        ClassMapping := ClassMappingFactory.GetClassMapping(ClassMapping.ObjectClass.ClassParent);
      end
      else
      begin
        ClassMapping := nil;
      end;
    end;
  end;
end;

{ TCustomSerializer }

constructor TCustomSerializer.Create;
begin
  inherited;
  FStack := TStack<TJSONValue>.Create;
end;

destructor TCustomSerializer.Destroy;
begin
  FStack.Free;
  inherited;
end;

class procedure TCustomSerializer.InternalToJSON(const JSON: TJSONArray;
  ClassMappingFactory: ISerializerClassMappingFactory;
  AObject: TObject);
var
  ClassMapping: ISerializerClassMapping;
begin
  CheckObjectAssigned(AObject);

  with Create do
  try
    FStack.Push(JSON);
    try
      ClassMapping := ClassMappingFactory.GetClassMapping(AObject.ClassType);

      if ClassMapping.IsList then
      begin
        WalkList(ClassMappingFactory, AObject, nil);
      end
      else
      begin
        WalkObject(ClassMappingFactory, AObject);
      end;
    except
      FStack.Pop.Free;

      raise;
    end;

    //Result := FStack.Pop as TJSONObject;
  finally
    Free;
  end;
end;

class function TCustomSerializer.InternalToJSON(
  ClassMappingFactory: ISerializerClassMappingFactory;
  AObject: TObject): TJSONArray;
begin
  Result := TJSONArray.Create;
  try
    InternalToJSON(Result, ClassMappingFactory, AObject);
  except
    on e: Exception do
    begin
      FreeAndNil(Result);

      raise;
    end;
  end;
end;

function TCustomSerializer.WalkFunc(const AObject: TObject;
  const ClassMapping: ISerializerClassMapping;
  const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  JSONArray: TJSONArray;
  JSONItem: TJSONObject;
  Value: Variant;
  StrValue: string;
begin
  Result := True;

  if PropertyMapping.PropertyName = '' then
    Exit;

  Value := GetPropValue(AObject, PropertyMapping.PropertyName, False);
  if PropertyMapping.IsDefaultValue(Value) then
    Exit;

  JSONArray := FStack.Peek as TJSONArray;

  JSONItem := TJSONObject.Create;
  JSONArray.Add(JSONItem);

  // Fixes problems with TDateTime registering as a double
  if GetTypeInfoName(PropertyMapping.PropertyInfo) = 'TDateTime' then
    JSONItem.AddPair(PropertyMapping.PropertyName, DateTimeToCommonDateTimeStr(Value, True)) else
  begin
    if PropertyMapping.PropertyInfo.Kind = tkFloat then
      StrValue := FloatToStrUseDot(Value)
    else
      StrValue := VarToStr(Value);

    JSONItem.AddPair(TJSONPair.Create(PropertyMapping.PropertyName, StrValue));
    if PropertyMapping.PropertyInfo.Kind = tkVariant then
      JSONItem.AddPair(sType, GetVariantTypeName(Value));
  end;
end;

function TCustomSerializer.WalkList(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  PropertyName: String;
begin
  if PropertyMapping = nil then
  begin
    PropertyName := sJSONItems;
  end
  else
  begin
    PropertyName := PropertyMapping.PropertyName;
  end;

  JSONArray := TJSONArray.Create;
  JSONObject := TJSONObject.Create;
  JSONObject.AddPair(PropertyName, JSONArray);

  (FStack.Peek as TJSONArray).Add(JSONObject);

  FStack.Push(JSONArray);
  try
    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

function TCustomSerializer.WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const ItemClassType: TClass; const Index: Integer): Boolean;
var
  JSONArray: TJSONArray;
begin
  JSONArray := FStack.Peek as TJSONArray;
  FStack.Push(TJSONArray.Create);
  JSONArray.Add(TJSONArray(FStack.Peek));
  try
    Result := inherited;
  finally
    if JSONArray <> nil then
    begin
      FStack.Pop;
    end;
  end;
end;

function TCustomSerializer.WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  JSONObject: TJSONObject;
  JSONValue: TJSONArray;
begin
  JSONObject := TJSONObject.Create;
  JSONValue := TJSONArray.Create;
  (FStack.Peek as TJSONArray).Add(JSONObject);

  JSONObject.AddPair(PropertyMapping.PropertyName, JSONValue);

  FStack.Push(JSONValue);
  try
    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

{ TCustomDeserializer }

constructor TCustomDeserializer.Create;
begin
  inherited;

  FStack := TStack<TJSONValue>.Create;
end;

destructor TCustomDeserializer.Destroy;
begin
  FStack.Free;

  inherited;
end;

class function TCustomDeserializer.ObjectToInterface<I>(
  var AObject: TObject): I;
var
  ObjectClassName, ObjectGUID: String;
begin
  if not Supports(AObject, GetTypeData(TypeInfo(I))^.Guid, Result) then
  begin
    ObjectClassName := AObject.ClassName;
    ObjectGUID := GUIDToString(GetTypeData(TypeInfo(I))^.GUID);

    AObject.Free;
    AObject := nil;

    raise EPTLibSerilizationError.CreateFmt(
      'Object class "%s" does not support interface "%s"',
      [ObjectClassName,
       ObjectGUID]);
  end;
end;

class procedure TCustomDeserializer.InternalToObject(const AObject: TObject;
  ClassMappingFactory: ISerializerClassMappingFactory;
  JSON: TJSONArray);
var
  ClassMapping: ISerializerClassMapping;
begin
  CheckObjectAssigned(AObject);
  CheckObjectAssigned(JSON);

  with Create do
  try
    FStack.Push(JSON);
    try
      ClassMapping := ClassMappingFactory.GetClassMapping(AObject.ClassType);

      if ClassMapping.IsList then
      begin
        WalkList(ClassMappingFactory, AObject, nil);
      end
      else
      begin
        WalkObject(ClassMappingFactory, AObject);
      end;
    except
      FStack.Pop;

      raise;
    end;
  finally
    Free;
  end;
end;

class function TCustomDeserializer.InternalToObject<T>(
  ClassMappingFactory: ISerializerClassMappingFactory;
  JSON: TJSONArray): T;
begin
  Result := T.Create;
  try
    InternalToObject(Result, ClassMappingFactory, JSON);
  except
    on e: Exception do
    begin
      FreeAndNil(Result);

      raise;
    end;
  end;
end;

function TCustomDeserializer.WalkFunc(const AObject: TObject;
  const ClassMapping: ISerializerClassMapping;
  const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  JSONValue: TJSONValue;
  PropInfo: PPropInfo;
  ItemType, ItemData: string;
begin
  Result := True;

  if PropertyMapping.PropertyName = '' then
    Exit;

  JSONValue := FindByName((FStack.Peek as TJSONArray), PropertyMapping.PropertyName, False);
  if not Assigned(JSONValue) then
    Exit;

  PropInfo := GetPropInfo(AObject, PropertyMapping.PropertyName);

  ItemData := JSONValue.GetValue<string>(PropertyMapping.PropertyName);

  if PropInfo.PropType^^.Kind = tkVariant then
  begin
    ItemType := JSONValue.GetValue<string>(sType);

    SetPropValue(AObject, PropertyMapping.PropertyName, GetVariantFromTextDeclaration(ItemType, ItemData));
  end
  else
  begin
    case PropInfo.PropType^^.Kind of
      tkChar,
      tkWChar:
        SetOrdProp(AObject, PropInfo, StrToInt(ItemData));
      tkInteger:
        SetOrdProp(AObject, PropInfo, StrToInt(ItemData));
      tkEnumeration:
        begin
          // Fixed TDateTime problem which registers as a float
          if SameText(String(itemdata), 'true') then
          begin
            ItemData := '1'
          end else
          if SameText(String(itemdata), 'false') then
          begin
            ItemData := '0'
          end;

          SetOrdProp(AObject, PropInfo, StrToInt(ItemData));
        end;
      tkSet:
        SetOrdProp(AObject, PropInfo, StrToInt(ItemData));
      tkFloat:
        begin
          // Fixed TDateTime problem which registers as a float
          if PropInfo.PropType <> nil then
          begin
            if GetTypeInfoName(PropInfo.propType^) = 'TDateTime' then
            begin
              SetFloatProp(AObject, PropInfo, CommonDateTimeStrToDateTime(ItemData));
            end
            else
            begin
              SetFloatProp(AObject, PropInfo, StrToFloatDefUseDot(ItemData, 0));
            end;
          end
          else
          begin
            SetFloatProp(AObject, PropInfo, StrToFloatDefUseDot(ItemData, 0));
          end;
        end;
      tkString, tkLString:
        SetStrProp(AObject, PropInfo, ItemData);
      {$IFNDEF NEXTGEN}
      tkWString:
        SetWideStrProp(AObject, PropInfo, ItemData);
      {$ENDIF}
      tkUString:
        SetStrProp(AObject, PropInfo, ItemData); //SB: ??
      tkInt64:
        SetInt64Prop(AObject, PropInfo, StrToInt(ItemData));
    else
      raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType,
        [GetTypeName(PropInfo.PropType^)]);
    end;
  end;
end;

function TCustomDeserializer.WalkList(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod, AddMethod: TRttiMethod;
  I, ItemCount: Integer;
  Params: TArray<TRttiParameter>;
  ItemClassType: TClass;
  Item: TObject;
  PropertyName: String;
begin
  if PropertyMapping = nil then
  begin
    ItemClassType := ClassMappingFactory.GetClassMapping(ARecObject.ClassType).GetListItemClassType;
    PropertyName := sJSONItems;
  end
  else
  begin
    ItemClassType := PropertyMapping.GetListItemClassType;
    PropertyName := PropertyMapping.PropertyName;
  end;

  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(ARecObject.ClassType);

  AddMethod := nil;
  for RttiMethod in RttiType.GetMethods do
  begin
    if SameText(RttiMethod.Name, 'Add') then
    begin
      Params := RTTIMethod.GetParameters;

      if (Length(Params) = 1) and (Params[0].ParamType.Handle = ItemClassType.ClassInfo) then
      begin
        AddMethod := RttiMethod;
        Break;
      end;
    end;
  end;

  if not Assigned(AddMethod) then
    raise EPTLibSerilizationError.CreateFmt('Class "%s" does not contain an Add method with the correct parameter type', [ARecObject.ClassName]);

  FStack.Push(FindByName((FStack.Peek as TJSONArray), PropertyName).GetValue(PropertyName));
  try
    ItemCount := (FStack.Peek as TJSONArray).Count;

    for I := 0 to ItemCount - 1 do
    begin
      Item := ItemClassType.Create;
      if Length(AddMethod.GetParameters) = 1 then
        AddMethod.Invoke(ARecObject, [Item]);
    end;

    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

function TCustomDeserializer.WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const ItemClassType: TClass;
  const Index: Integer): Boolean;
begin
  FStack.Push((FStack.Peek as TJSONArray).Items[Index]);
  try
    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

function TCustomDeserializer.WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
begin
  FStack.Push(FindByName((FStack.Peek as TJSONArray), PropertyMapping.PropertyName).GetValue(PropertyMapping.PropertyName));
  try
    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

{ TCustomObjectCloner }

constructor TCustomObjectCloner.Create;
begin
  inherited;

  FStack := TStack<TObject>.Create;
end;

destructor TCustomObjectCloner.Destroy;
begin
  FStack.Free;
  inherited;
end;

class procedure TCustomObjectCloner.InternalAssignTo(
  ClassMappingFactory: ISerializerClassMappingFactory; SrcObject,
  DstObject: TObject);
var
  ClassMapping: ISerializerClassMapping;
begin
  if not DstObject.InheritsFrom(SrcObject.ClassType) then
    raise EPTLibSerilizationError.Create('SrcObject and DstObject are not compatible');

  with Create do
  try
    FStack.Push(DstObject);
    try
      ClassMapping := ClassMappingFactory.GetClassMapping(SrcObject.ClassType);

      if ClassMapping.IsList then
      begin
        WalkList(ClassMappingFactory, SrcObject, nil);
      end
      else
      begin
        WalkObject(ClassMappingFactory, SrcObject);
      end;
    except
      FStack.Pop;

      raise;
    end;
  finally
    Free;
  end;
end;

function TCustomObjectCloner.WalkFunc(const AObject: TObject;
  const ClassMapping: ISerializerClassMapping;
  const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
begin
  Result := True;

  if PropertyMapping.PropertyName = '' then
    Exit;

  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(AObject.ClassType);

  Prop := RttiType.GetProperty(PropertyMapping.PropertyName);

  Prop.SetValue(FStack.Peek, Prop.GetValue(AObject));
end;

function TCustomObjectCloner.WalkList(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  ClearMethod: TRttiMethod;
  CountProperty: TRttiProperty;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(ARecObject.ClassType);

  CountProperty := RttiType.GetProperty('Count');
  if not Assigned(CountProperty) then
    raise EPTLibSerilizationError.CreateFmt('Class "%s" does not contain Count property', [ARecObject.ClassName]);

  ClearMethod := RttiType.GetMethod('Clear');
  if not Assigned(ClearMethod) then
    raise EPTLibSerilizationError.CreateFmt('Class "%s" does not contain a Clear method', [ARecObject.ClassName]);

  if PropertyMapping <> nil then
  begin
    FStack.Push(GetObjectProp(FStack.Peek, PropertyMapping.PropertyName));
  end;
  try
    ClearMethod.Invoke(FStack.Peek, []);

    Result := inherited;
  finally
    if PropertyMapping <> nil then
    begin
      FStack.Pop;
    end;
  end;
end;

function TCustomObjectCloner.WalkListItemPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const ItemClassType: TClass; const Index: Integer): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod, AddMethod: TRttiMethod;
  Params: TArray<TRttiParameter>;
  Item: TObject;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(FStack.Peek.ClassType);

  AddMethod := nil;
  for RttiMethod in RttiType.GetMethods do
  begin
    if SameText(RttiMethod.Name, 'Add') then
    begin
      Params := RttiMethod.GetParameters;

      if (Length(Params) = 1) and (Params[0].ParamType.Handle = ItemClassType.ClassInfo) then
      begin
        AddMethod := RttiMethod;
        Break;
      end;
    end;
  end;

  if not Assigned(AddMethod) then
    raise EPTLibSerilizationError.CreateFmt('Class "%s" does not contain an Add method with the correct parameter type', [ARecObject.ClassName]);

  Item := ItemClassType.Create;
  if Length(AddMethod.GetParameters) = 1 then
    AddMethod.Invoke(FStack.Peek, [Item]);

  FStack.Push(Item);
  try
    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

function TCustomObjectCloner.WalkObjectPropertiesRec(const ClassMappingFactory: ISerializerClassMappingFactory;
  const ARecObject: TObject; const PropertyMapping: ISerializerPropertyMapping): Boolean;
begin
  FStack.Push(GetObjectProp(FStack.Peek, PropertyMapping.PropertyName));
  try
    Result := inherited;
  finally
    FStack.Pop;
  end;
end;

end.
