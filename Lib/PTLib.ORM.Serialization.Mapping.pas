{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Serialization.Mapping                          }
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

unit PTLib.ORM.Serialization.Mapping;

interface

uses
  System.TypInfo, System.Variants, System.Generics.Collections,

  PTLib.Common.Serialization.Classes,

  PTLib.ORM.Interfaces;

type
  TORMSerializerPropertyMapping = class(TInterfacedObject, ISerializerPropertyMapping)
  private
    FORM: IORM;
    FFieldMapping: IORMFieldMapping;

    function GetIsList: Boolean;
    function GetPropertyInfo: PTypeInfo;
    function GetPropertyName: string;

    function IsDefaultValue(const Value: Variant): Boolean;
    function GetListItemClassType: TClass;
  public
    constructor Create(const FieldMapping: IORMFieldMapping; const ORM: IORM);
  end;

  TORMSerializerClassMapping = class(TInterfacedObject, ISerializerClassMapping)
  private
    FORM: IORM;
    FTableMapping: IORMTableMapping;

    function GetCount: Integer;
    function GetIsList: Boolean;
    function GetItem(I: Integer): ISerializerPropertyMapping;
    function GetObjectClass: TClass;
    function GetListItemClassType: TClass;
  public
    constructor Create(const TableMapping: IORMTableMapping; const ORM: IORM);
  end;

  TORMSerializerClassMappingFactory = class(TInterfacedObject, ISerializerClassMappingFactory)
  private
    FORM: IORM;

    function GetClassMapping(ClassType: TClass): ISerializerClassMapping;
    function IsValidObject(ClassType: TClass): Boolean;
    function WalkParentClasses: Boolean;
  public
    constructor Create(const ORM: IORM);
  end;

implementation

uses
  PTLib.ORM.Types;

{ TORMSerializerClassMapping }

constructor TORMSerializerClassMapping.Create(
  const TableMapping: IORMTableMapping; const ORM: IORM);
begin
  inherited Create;
  FORM := ORM;
  FTableMapping := TableMapping;
end;

function TORMSerializerClassMapping.GetCount: Integer;
begin
  Result := FTableMapping.FieldMappings.Count;
end;

function TORMSerializerClassMapping.GetListItemClassType: TClass;
var
  ClassName: Variant;
begin
  FTableMapping.Attributes.GetAttribute(AttributeArrayClassType, ClassName);

  Result := FORM.FindTableMapping(VarToStr(ClassName), False).ObjectClass;
end;

function TORMSerializerClassMapping.GetObjectClass: TClass;
begin
  Result := FTableMapping.ObjectClass;
end;

function TORMSerializerClassMapping.GetIsList: Boolean;
begin
  Result :=
    (FTableMapping.Attributes.HasAttribute(AttributeOneToMany)) or
    (FTableMapping.Attributes.HasAttribute(AttributeArrayClassType));
end;

function TORMSerializerClassMapping.GetItem(I: Integer): ISerializerPropertyMapping;
begin
  Result := TORMSerializerPropertyMapping.Create(FTableMapping.FieldMappings[I], FORM);
end;

{ TORMSerializerClassMappingFactory }

constructor TORMSerializerClassMappingFactory.Create(const ORM: IORM);
begin
  inherited Create;
  FORM := ORM;
end;

function TORMSerializerClassMappingFactory.GetClassMapping(
  ClassType: TClass): ISerializerClassMapping;
var
  TableMapping: IORMTableMapping;
begin
  TableMapping := FORM.FindTableMapping(ClassType.ClassName, False);

  if Assigned(TableMapping) then
    Result := TORMSerializerClassMapping.Create(TableMapping, FORM)
  else
    Result := nil;
end;

function TORMSerializerClassMappingFactory.IsValidObject(
  ClassType: TClass): Boolean;
begin
  Result := FORM.IsValidORMObject(ClassType);
end;

function TORMSerializerClassMappingFactory.WalkParentClasses: Boolean;
begin
  Result := True;
end;

{ TORMSerializerPropertyMapping }

constructor TORMSerializerPropertyMapping.Create(
  const FieldMapping: IORMFieldMapping; const ORM: IORM);
begin
  inherited Create;
  FORM := ORM;
  FFieldMapping := FieldMapping;
end;

function TORMSerializerPropertyMapping.GetIsList: Boolean;
begin
  Result :=
    (FFieldMapping.Attributes.HasAttribute(AttributeOneToMany)) or
    (FFieldMapping.Attributes.HasAttribute(AttributeArrayClassType));
end;

function TORMSerializerPropertyMapping.GetListItemClassType: TClass;
var
  ClassName: Variant;
begin
  FFieldMapping.Attributes.GetAttribute(AttributeArrayClassType, ClassName);

  Result := FORM.FindTableMapping(VarToStr(ClassName), False).ObjectClass;;
end;

function TORMSerializerPropertyMapping.GetPropertyInfo: PTypeInfo;
begin
  Result := FFieldMapping.PropertyInfo;
end;

function TORMSerializerPropertyMapping.GetPropertyName: string;
begin
  Result := FFieldMapping.PropertyName;
end;

function TORMSerializerPropertyMapping.IsDefaultValue(
  const Value: Variant): Boolean;
begin
  Result := False;
end;

end.
