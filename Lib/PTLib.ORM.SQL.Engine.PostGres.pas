unit PTLib.ORM.SQL.Engine.PostGres;
{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.SQL.Engine.PostGres                            }
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

interface

uses
  SysUtils,

  PTLib.Common.Strings,

  PTLib.ORM.Types,
  PTLib.ORM.Factory,
  PTLib.ORM.Interfaces,
  PTLib.ORM.SQL.Criteria,
  PTLib.ORM.SQL.Engine.MySQL,
  PTLib.ORM.Registry;

type
  TORMSQLEnginePostGres = class(TORMSQLEngineMySQLBase)
  protected
    function DoGetFieldMacroText(const QueryCriteria: ISQLSelectCriteria;
      const FieldCriteria: ISQLFieldNameItem;
      const Parameters: ISQLParameters): String; override;
    function DoGetRelationalOperatorString(const RelationalOperator: TSQLRelationalOperator): String; override;
  public
    procedure GenerateSQL(const QueryCriteria: ISQLInsertCriteria;
      var SQL: String; const Parameters: ISQLParameters); override;
  end;

implementation

{ TORMSQLEnginePostGres }

function TORMSQLEnginePostGres.DoGetRelationalOperatorString(
  const RelationalOperator: TSQLRelationalOperator): String;
begin
  case RelationalOperator of
    Containing: Result := '%s LIKE ''%%'' || %s || ''%%''';
  else
    Result := inherited;
  end;
end;

procedure TORMSQLEnginePostGres.GenerateSQL(
  const QueryCriteria: ISQLInsertCriteria; var SQL: String;
  const Parameters: ISQLParameters);
var
  FieldNames, Params, PrimaryKeys, ReturningFields: String;
  TableMapping: IORMTableMapping;
  i: Integer;
begin
  TableMapping := TORMClassRegistry.GetTableFieldMappingsByTableName(QueryCriteria.TableName, False);

  if TableMapping = nil then
  begin
    PrimaryKeys := QueryCriteria.GetFieldValueList.Items[0].FieldName;
  end
  else
  begin
    PrimaryKeys := '';

    for i := 0 to pred(TableMapping.PrimaryKeyFieldMappings.Count) do
    begin
      AddTokenEx(PrimaryKeys, TableMapping.PrimaryKeyFieldMappings.Items[i].DatabaseFieldName, ',');
    end;
  end;

  SQL := FTokenInsertInto;

  FieldNames := GenerateFieldList(QueryCriteria, Parameters, gftFieldNames);
  Params := GenerateFieldList(QueryCriteria, Parameters, gftInsertParams);

  SQL :=
    SQL +
    FTokenLineFeed +
    FormatTableName(QueryCriteria.TableName) +
    FTokenLineFeed +
    FTokenOpenParanthesis + FieldNames + FTokenCloseParanthesis +
    FTokenLineFeed +
    FTokenInsertValues +
    FTokenLineFeed +
    FTokenOpenParanthesis + Params + FTokenCloseParanthesis;

  if QueryCriteria.GetUpdateExistingRecords then
  begin
    AddTokenEx(SQL, 'ON CONFLICT (' + PrimaryKeys + ') DO UPDATE SET', FTokenLineFeed);
    AddTokenEx(SQL, GenerateFieldList(QueryCriteria, Parameters, gftUpdateFieldsAndParams), FTokenLineFeed);
  end;

  ReturningFields := '';

  for i := Low(QueryCriteria.GetReturnsList) to High(QueryCriteria.GetReturnsList) do
  begin
    AddToken(ReturningFields, QueryCriteria.GetReturnsList[i], FTokenFieldDelimiter);
  end;

  if ReturningFields <> '' then
  begin
    SQL := SQL +
      FTokenLineFeed +
      FTokenReturning +
      FTokenWhiteSpace +
      ReturningFields;
  end;
end;

function TORMSQLEnginePostGres.DoGetFieldMacroText(
  const QueryCriteria: ISQLSelectCriteria;
  const FieldCriteria: ISQLFieldNameItem;
  const Parameters: ISQLParameters): String;
var
  SQLMacroDateLocalise: ISQLMacroDateLocalise;
  SQLMacroTimestampDiff: ISQLMacroTimestampDiff;
  Param: ISQLParameter;
begin
  if Supports(FieldCriteria.Macro, ISQLMacroDateLocalise, SQLMacroDateLocalise) then
  begin
    // This should be fixed - test for a while and see
    { TODO : If I leave the original code, Firedac uses the wrong value for the parameters 120 -> 5 - Why??? }
    Param := AddParam(Parameters, SQLMacroDateLocalise.UTCOffsetMinutes);

    Result := format('%s + (:%s * INTERVAL ''1 minute'')',
                     [FormatFieldName(SQLMacroDateLocalise.FieldName),
                      Param.Name]);
  end else

  if Supports(FieldCriteria.Macro, ISQLMacroTimestampDiff, SQLMacroTimestampDiff) then
  begin
    Result :=
      format('EXTRACT(%s FROM %s) - EXTRACT(%s FROM %s)',
        [DateUnitToText(SQLMacroTimestampDiff.DateUnit),
         FormatFieldName(SQLMacroTimestampDiff.EndDateField),
         DateUnitToText(SQLMacroTimestampDiff.DateUnit),
         FormatFieldName(SQLMacroTimestampDiff.StartDateField)]);
  end else

  begin
    Result := inherited;
  end;
end;

end.
