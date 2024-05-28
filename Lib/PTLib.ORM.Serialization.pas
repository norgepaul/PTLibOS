{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Serialization                                  }
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

unit PTLib.ORM.Serialization;

interface

uses
  System.JSON,

  PTLib.Common.Serialization.Classes,

  PTLib.ORM.Interfaces;

type
  TORMSerializer = class(TCustomSerializer)
  public
    class function ToJSON(const ORM: IORM; AObject: TObject): TJSONArray; overload;
    class function ToJSON(const ORM: IORM; AInterface: IInterface): TJSONArray; overload;
    class function ToJSONString(const ORM: IORM; AObject: TObject; const FormatJSON: Boolean = False): string; overload;
    class function ToJSONString(const ORM: IORM; AInterface: IInterface; const FormatJSON: Boolean = False): String; overload;
  end;

  TORMDeserializer = class(TCustomDeserializer)
  public
    class function ToObject<T: class, constructor>(const ORM: IORM; JSON: TJSONArray): T; overload;
    class function ToObject<T: class, constructor>(const ORM: IORM; const JSON: String): T; overload;
    class function ToObject<T: class, constructor; I: IInterface>(const ORM: IORM; JSON: TJSONArray): I; overload;
    class function ToObject<T: class, constructor; I: IInterface>(const ORM: IORM; const JSON: String): I; overload;
  end;

  TORMObjectCloner = class(TCustomObjectCloner)
  public
    class procedure AssignTo(const ORM: IORM; SrcObject, DstObject: TObject);
    class function Clone<T: class, constructor>(const ORM: IORM; AObject: TObject): T; overload;
    class function Clone(const ORM: IORM; AObject: TObject): TObject; overload;
  end;

implementation

uses
  System.SysUtils, REST.Json, PTLib.ORM.Serialization.Mapping;

{ TORMSerializer }

class function TORMSerializer.ToJSON(const ORM: IORM; AObject: TObject): TJSONArray;
begin
  Result := InternalToJSON(TORMSerializerClassMappingFactory.Create(ORM), AObject);
end;

class function TORMSerializer.ToJSONString(const ORM: IORM;
  AInterface: IInterface; const FormatJSON: Boolean): String;
begin
  Result := ToJSONString(ORM, AInterface as TObject, FormatJSON)
end;

class function TORMSerializer.ToJSON(const ORM: IORM;
  AInterface: IInterface): TJSONArray;
begin
  Result := ToJSON(ORM, AInterface as TObject);
end;

class function TORMSerializer.ToJSONString(const ORM: IORM;
  AObject: TObject; const FormatJSON: Boolean): String;
var
  JSONArray: TJSONArray;
begin
  JSONArray := ToJSON(
    ORM,
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

{ TORMDeserializer }

class function TORMDeserializer.ToObject<T, I>(const ORM: IORM;
  JSON: TJSONArray): I;
var
  AObject: TObject;
begin
  AObject := ToObject<T>(ORM, JSON);

  Result := ObjectToInterface<I>(AObject);
end;

class function TORMDeserializer.ToObject<T, I>(const ORM: IORM;
  const JSON: String): I;
var
  AObject: TObject;
begin
  AObject := ToObject<T>(ORM, JSON);

  Result := ObjectToInterface<I>(AObject);
end;

class function TORMDeserializer.ToObject<T>(const ORM: IORM;
  const JSON: String): T;
var
  JSONArray: TJSONArray;
begin
  JSONArray := TJSONObject.ParseJSONValue(JSON) as TJSONArray;
  try
    Result := ToObject<T>(ORM, JSONArray);
  finally
    FreeAndNil(JSONArray);
  end;
end;

class function TORMDeserializer.ToObject<T>(const ORM: IORM;
  JSON: TJSONArray): T;
begin
  Result := InternalToObject<T>(TORMSerializerClassMappingFactory.Create(ORM), JSON);
end;

{ TORMObjectCloner }

class procedure TORMObjectCloner.AssignTo(const ORM: IORM; SrcObject,
  DstObject: TObject);
begin
  InternalAssignTo(TORMSerializerClassMappingFactory.Create(ORM), SrcObject, DstObject);
end;

class function TORMObjectCloner.Clone<T>(const ORM: IORM; AObject: TObject): T;
begin
  Result := T.Create;
  try
    AssignTo(ORM, AObject, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TORMObjectCloner.Clone(const ORM: IORM; AObject: TObject): TObject;
begin
  Result := AObject.ClassType.Create;
  try
    TORMObjectCloner.AssignTo(
      ORM,
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

end.

