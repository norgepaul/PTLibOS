{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Serialization                               }
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

unit PTLib.Common.Serialization;

interface

uses
  System.JSON,

  Generics.Collections,

  PTLib.Common.Serialization.Classes,
  PTLib.Common.Serialization.Mapping;

type
  SerializerListAttribute = class(TCustomAttribute)
  private
    FElementType: TClass;
  public
    constructor Create(ElementType: TClass);

    property ElementType: TClass read FElementType;
  end;

  SerializerDefaultAttribute = class(TCustomAttribute)
  protected
    FValue: Variant;
  public
    constructor Create(const DefaultValue: string); overload;
    constructor Create(const DefaultValue: Integer); overload;
    constructor Create(const DefaultValue: Boolean); overload;

    property Value: Variant read FValue;
  end;

  TSerializer = class(TCustomSerializer)
  public
    class function ToJSON(AObject: TObject): TJSONArray; overload;
    class function ToJSON(AInterface: IInterface): TJSONArray; overload;
    class function ToJSONString(AObject: TObject; const FormatJSON: Boolean = False): string; overload;
    class function ToJSONString(AInterface: IInterface; const FormatJSON: Boolean = False): string; overload;
    class procedure ToJSON(const JSON: TJSONArray; AObject: TObject); overload;
    class procedure ToJSON(const JSON: TJSONArray; AInterface: IInterface); overload;
  end;

  TDeserializer = class(TCustomDeserializer)
  public
    class function ToObject<T: class, constructor>(JSON: TJSONArray): T; overload;
    class function ToObject<T: class, constructor>(const JSON: string): T; overload;
    class function ToObject<T: class, constructor; I: IInterface>(JSON: TJSONArray): I; overload;
    class function ToObject<T: class, constructor; I: IInterface>(const JSON: string): I; overload;
    class procedure ToObject(const AObject: TObject; JSON: TJSONArray); overload;
    class procedure ToObject(const AObject: TObject; const JSON: string); overload;
    class procedure ToObject(const AInterface: IInterface; JSON: TJSONArray); overload;
    class procedure ToObject(const AInterface: IInterface; const JSON: string); overload;
  end;

  TObjectCloner = class(TCustomObjectCloner)
  public
    class procedure AssignTo(SrcObject, DstObject: TObject);
    class function Clone<T: class, constructor>(AObject: TObject): T; overload;
    class function Clone(AObject: TObject): TObject; overload;
  end;

implementation

uses
  System.SysUtils, REST.Json;

{ SerializerListAttribute }

constructor SerializerListAttribute.Create(ElementType: TClass);
begin
  inherited Create;
  FElementType := ElementType;
end;

{ SerializerDefaultAttribute }

constructor SerializerDefaultAttribute.Create(const DefaultValue: string);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor SerializerDefaultAttribute.Create(const DefaultValue: Integer);
begin
  inherited Create;
  FValue := DefaultValue;
end;

constructor SerializerDefaultAttribute.Create(const DefaultValue: Boolean);
begin
  inherited Create;
  FValue := DefaultValue;
end;

{ TSerializer }

class function TSerializer.ToJSON(AInterface: IInterface): TJSONArray;
begin
  Result := ToJSON(AInterface as TObject);
end;

class function TSerializer.ToJSON(AObject: TObject): TJSONArray;
begin
  Result := InternalToJSON(TRTTISerializerClassMappingFactory.Create, AObject);
end;

class procedure TSerializer.ToJSON(const JSON: TJSONArray; AObject: TObject);
begin
  InternalToJSON(JSON, TRTTISerializerClassMappingFactory.Create, AObject);
end;

class procedure TSerializer.ToJSON(const JSON: TJSONArray;
  AInterface: IInterface);
begin
  ToJSON(JSON, AInterface as TObject);
end;

class function TSerializer.ToJSONString(AInterface: IInterface;
  const FormatJSON: Boolean): string;
begin
  Result := ToJSONString(AInterface as TObject, FormatJSON)
end;

class function TSerializer.ToJSONString(AObject: TObject;
  const FormatJSON: Boolean): string;
var
  JSONArray: TJSONArray;
begin
  JSONArray := ToJSON(
    AObject);
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

{ TDeserializer }

class procedure TDeserializer.ToObject(const AObject: TObject;
  const JSON: string);
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
  try
    ToObject(AObject, JSONArray);
  finally
    FreeAndNil(JSONArray);
  end;
end;

class procedure TDeserializer.ToObject(const AObject: TObject;
  JSON: TJSONArray);
begin
  InternalToObject(AObject, TRTTISerializerClassMappingFactory.Create, JSON);
end;

class procedure TDeserializer.ToObject(const AInterface: IInterface;
  const JSON: string);
begin
  ToObject(AInterface as TObject, JSON);
end;

class procedure TDeserializer.ToObject(const AInterface: IInterface;
  JSON: TJSONArray);
begin
  ToObject(AInterface as TObject, JSON);
end;

class function TDeserializer.ToObject<T, I>(const JSON: string): I;
var
  AObject: TObject;
begin
  AObject := ToObject<T>(JSON);

  Result := ObjectToInterface<I>(AObject);
end;

class function TDeserializer.ToObject<T, I>(JSON: TJSONArray): I;
var
  AObject: TObject;
begin
  AObject := ToObject<T>(JSON);

  Result := ObjectToInterface<I>(AObject);
end;

class function TDeserializer.ToObject<T>(const JSON: string): T;
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
  try
    Result := ToObject<T>(JSONArray);
  finally
    FreeAndNil(JSONArray);
  end;
end;

class function TDeserializer.ToObject<T>(JSON: TJSONArray): T;
begin
  Result := InternalToObject<T>(TRTTISerializerClassMappingFactory.Create, JSON);
end;

{ TObjectCloner }

class procedure TObjectCloner.AssignTo(SrcObject, DstObject: TObject);
begin
  InternalAssignTo(TRTTISerializerClassMappingFactory.Create, SrcObject, DstObject);
end;

class function TObjectCloner.Clone(AObject: TObject): TObject;
begin
  Result := AObject.ClassType.Create;
  try
    TObjectCloner.AssignTo(
      AObject,
      Result);
  except
    on e: Exception do
    begin
      FreeAndNil(Result);

      raise;
    end;
  end;
end;

class function TObjectCloner.Clone<T>(AObject: TObject): T;
var
  JSON: TJSONObject;
begin
  Result := T.Create;
  try
    AssignTo(AObject, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.
