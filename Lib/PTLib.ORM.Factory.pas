{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Factory                      }
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

unit PTLib.ORM.Factory;

interface

uses
  PTLib.ORM.Types,
  PTLib.ORM.Interfaces;

type
  TOFac = class
    // Query types
    class function Select: ISQLSelectCriteria;
    class function InsertInto(const TableName: String; TableAlias: String = ''): ISQLInsertCriteria;
    class function UpdateOrInsertInto(const TableName: String; TableAlias: String = ''): ISQLInsertCriteria;
    class function Update(const TableName: String; TableAlias: String = ''): ISQLUpdateCriteria;
    class function DeleteFrom(const TableName: String; TableAlias: String = ''): ISQLDeleteCriteria;

    // Comparitors
    class function Comparitor(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
        const Value: Variant; const ValueType: TSQLValueType = vtDefault): ISQLComparitor; overload;
    class function Comparitor(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor; overload;
    class function Comparitor(const PrimaryKeyMappings: IORMFieldMappings;
      const PrimaryKeyValues: TPrimaryKeyArray): ISQLComparitor; overload;

    // Fields
    class function FieldDateLocalise(const FieldName: String; const UTCOffsetMinutes: Integer): ISQLMacroDateLocalise;
    class function FieldConcat: ISQLMacroConcat;
    class function TimeStampDiff(const StartDateField, EndDateField: String; const DateUnit: TSQLMacroDateUnit = TSQLMacroDateUnit.duMinute): ISQLMacroTimestampDiff;

    //
    class function OrderBy: ISQLOrderByCriteria;
    class function GroupBy: ISQLGroupByCriteria;

    // Parameters
    class function Parameters: ISQLParameters;
  end;

implementation

uses
  System.SysUtils,

  PTLib.ORM.Classes,
  PTLib.ORM.SQL.Criteria;

resourcestring
  StrPrimaryKeyMappings = 'Primary key mappings and value counts do not match';

{ TOFac }

class function TOFac.Comparitor(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
  const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor;
begin
  Result := TSQLComparitor.Create(
    TSQLOperatorExpression.None,
    FieldName,
    RelationalOperator,
    SQLSelectCriteria);
end;

class function TOFac.Comparitor(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
  const Value: Variant; const ValueType: TSQLValueType): ISQLComparitor;
begin
  Result := TSQLComparitor.Create(
    TSQLOperatorExpression.None,
    FieldName,
    RelationalOperator,
    Value,
    ValueType);
end;

class function TOFac.Comparitor(const PrimaryKeyMappings: IORMFieldMappings;
  const PrimaryKeyValues: TPrimaryKeyArray): ISQLComparitor;
var
  i: Integer;
begin
  if PrimaryKeyMappings.Count <> length(PrimaryKeyValues) then
    raise EORMIncorrectPrimaryKeyCountError.Create(StrPrimaryKeyMappings);

  Result := TOFac.Comparitor(PrimaryKeyMappings[0].DatabaseFieldName,
                             EqualTo,
                             PrimaryKeyValues[0]);

  for i := 1 to pred(PrimaryKeyMappings.Count) do
    Result.&And(PrimaryKeyMappings[i].DatabaseFieldName,
                EqualTo,
                PrimaryKeyValues[i]);
end;

class function TOFac.TimeStampDiff(const StartDateField, EndDateField: String;
  const DateUnit: TSQLMacroDateUnit): ISQLMacroTimestampDiff;
begin
  Result := TSQLMacroTimestampDiff.Create;
  Result.StartDateField := StartDateField;
  Result.EndDateField := EndDateField;
  Result.DateUnit := DateUnit;
end;

class function TOFac.DeleteFrom(const TableName: String; TableAlias: String): ISQLDeleteCriteria;
begin
  Result := TSQLDeleteCriteria.Create(
    TableName,
    TableAlias);

  Result.CacheID(IntToStr(Cardinal(System.ReturnAddress)));
end;

class function TOFac.FieldConcat: ISQLMacroConcat;
begin
  Result := TSQLMacroConcat.Create;
end;

class function TOFac.FieldDateLocalise(const FieldName: String; const UTCOffsetMinutes: Integer): ISQLMacroDateLocalise;
begin
  Result := TSQLMacroDateLocalise.Create;
  Result.UTCOffsetMinutes := UTCOffsetMinutes;
  Result.FieldName := FieldName;
end;

class function TOFac.GroupBy: ISQLGroupByCriteria;
begin
  Result := TSQLGroupByCriteria.Create;
end;

class function TOFac.InsertInto(const TableName: String; TableAlias: String): ISQLInsertCriteria;
begin
  Result := TSQLInsertCriteria.Create(
    TableName,
    TableAlias);
  Result.CacheID(IntToStr(Cardinal(System.ReturnAddress)));
end;

class function TOFac.OrderBy: ISQLOrderByCriteria;
begin
  Result := TSQLOrderByCriteria.Create;
end;

class function TOFac.Parameters: ISQLParameters;
begin
  Result := TSQLParameters.Create;
end;

class function TOFac.Select: ISQLSelectCriteria;
begin
  Result := TORMSelectQueryCriteria.Create;
  Result.CacheID(IntToStr(Cardinal(System.ReturnAddress)));
end;

class function TOFac.Update(const TableName: String; TableAlias: String): ISQLUpdateCriteria;
begin
  Result := TSQLUpdateCriteria.Create(
    TableName,
    TableAlias);
  Result.CacheID(IntToStr(Cardinal(System.ReturnAddress)));
end;

class function TOFac.UpdateOrInsertInto(const TableName: String; TableAlias: String): ISQLInsertCriteria;
begin
  Result := TSQLUpdateOrInsertCriteria.Create(
    TableName,
    TableAlias);
  Result.CacheID(IntToStr(Cardinal(System.ReturnAddress)));
end;

end.
