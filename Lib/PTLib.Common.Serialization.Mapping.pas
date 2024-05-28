{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Serialization.Mapping                       }
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

unit PTLib.Common.Serialization.Mapping;

interface

uses
  System.TypInfo, System.Rtti,

  Generics.Collections,

  PTLib.Common.Serialization.Classes;

type
  TRTTISerializerPropertyMapping = class(TInterfacedObject, ISerializerPropertyMapping)
  private
    FRttiProperty: TRttiProperty;

    function GetIsList: Boolean;
    function GetPropertyInfo: PTypeInfo;
    function GetPropertyName: string;

    function IsDefaultValue(const Value: Variant): Boolean;
    function GetListItemClassType: TClass;
  public
    constructor Create(RttiProperty: TRttiProperty);
  end;

  TRTTISerializerClassMapping = class(TInterfacedObject, ISerializerClassMapping)
  private
    FRttiContext: TRttiContext;
    FRttiType: TRttiType;
    FClassType: TClass;
    FProps: TList<TRttiProperty>;

    function GetCount: Integer;
    function GetIsList: Boolean;
    function GetItem(I: Integer): ISerializerPropertyMapping;
    function GetObjectClass: TClass;
    function GetListItemClassType: TClass;
  public
    constructor Create(ClassType: TClass);
    destructor Destroy; override;
  end;

  TRTTISerializerClassMappingFactory = class(TInterfacedObject, ISerializerClassMappingFactory)
  private
    function GetClassMapping(ClassType: TClass): ISerializerClassMapping;
    function IsValidObject(ClassType: TClass): Boolean;
    function WalkParentClasses: Boolean;
  end;

implementation

uses
  PTLib.Common.Serialization;

{ TRTTISerializerPropertyMapping }

constructor TRTTISerializerPropertyMapping.Create(RttiProperty: TRttiProperty);
begin
  inherited Create;

  FRttiProperty := RttiProperty;
end;

function TRTTISerializerPropertyMapping.GetIsList: Boolean;
var
  Attribute: TCustomAttribute;
begin
  Result := False;

  for Attribute in FRttiProperty.GetAttributes do
    if Attribute is SerializerListAttribute then
    begin
      Result := True;
      Break;
    end;
end;

function TRTTISerializerPropertyMapping.GetListItemClassType: TClass;
var
  Attribute: TCustomAttribute;
begin
  Result := nil;

  for Attribute in FRttiProperty.GetAttributes do
    if Attribute is SerializerListAttribute then
    begin
      Result := SerializerListAttribute(Attribute).ElementType;
      Break;
    end;
end;

function TRTTISerializerPropertyMapping.GetPropertyInfo: PTypeInfo;
begin
  Result := FRttiProperty.PropertyType.Handle;
end;

function TRTTISerializerPropertyMapping.GetPropertyName: string;
begin
  Result := FRttiProperty.Name;
end;

function TRTTISerializerPropertyMapping.IsDefaultValue(
  const Value: Variant): Boolean;
var
  Attribute: TCustomAttribute;
begin
  Result := False;

  for Attribute in FRttiProperty.GetAttributes do
    if Attribute is SerializerDefaultAttribute then
    begin
      if Value = SerializerDefaultAttribute(Attribute).Value then
      begin
        Result := True;
        Break;
      end;
    end;
end;

{ TRTTISerializerClassMapping }

constructor TRTTISerializerClassMapping.Create(ClassType: TClass);
var
  Prop: TRttiProperty;
begin
  inherited Create;

  FRttiContext := TRttiContext.Create;
  FClassType := ClassType;
  FRttiType := FRttiContext.GetType(ClassType);

  FProps := TList<TRttiProperty>.Create;

  for Prop in FRttiType.GetProperties do
    if Prop.Visibility = mvPublished then
    begin
      if (Prop.IsReadable and Prop.IsWritable) or (Prop.IsReadable and (Prop.PropertyType.TypeKind = tkClass)) then
        FProps.Add(Prop);
    end;
end;

destructor TRTTISerializerClassMapping.Destroy;
begin
  FProps.Free;

  inherited;
end;

function TRTTISerializerClassMapping.GetCount: Integer;
begin
  Result := FProps.Count;
end;

function TRTTISerializerClassMapping.GetIsList: Boolean;
var
  Attribute: TCustomAttribute;
begin
  Result := False;

  for Attribute in FRttiType.GetAttributes do
    if Attribute is SerializerListAttribute then
    begin
      Result := True;
      Break;
    end;
end;

function TRTTISerializerClassMapping.GetItem(
  I: Integer): ISerializerPropertyMapping;
begin
  Result := TRTTISerializerPropertyMapping.Create(FProps[I]);
end;

function TRTTISerializerClassMapping.GetListItemClassType: TClass;
var
  Attribute: TCustomAttribute;
begin
  Result := nil;

  for Attribute in FRttiType.GetAttributes do
    if Attribute is SerializerListAttribute then
    begin
      Result := SerializerListAttribute(Attribute).ElementType;
      Break;
    end;
end;

function TRTTISerializerClassMapping.GetObjectClass: TClass;
begin
  Result := FClassType;
end;

{ TRTTISerializerClassMappingFactory }

function TRTTISerializerClassMappingFactory.GetClassMapping(
  ClassType: TClass): ISerializerClassMapping;
begin
  Result := TRTTISerializerClassMapping.Create(ClassType);
end;

function TRTTISerializerClassMappingFactory.IsValidObject(
  ClassType: TClass): Boolean;
begin
  Result := ClassType <> TObject;
end;

function TRTTISerializerClassMappingFactory.WalkParentClasses: Boolean;
begin
  Result := False;
end;

end.
