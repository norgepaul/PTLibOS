{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.SQL.Engine.Firebird                            }
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

unit PTLib.ORM.SQL.Engine.MySQL;

interface

uses
  SysUtils,

  PTLib.Common.Strings,

  PTLib.ORM.Types,
  PTLib.ORM.Factory,
  PTLib.ORM.Interfaces,
  PTLib.ORM.SQL.Criteria,
  PTLib.ORM.SQL.Engine;

type
  TORMSQLEngineMySQLBase = class(TBaseORMSQLEngine)
  protected
    procedure DoDefineTokens; override;
    function DoGetFieldMacroText(const QueryCriteria: ISQLSelectCriteria;
      const FieldCriteria: ISQLFieldNameItem; const Parameters: ISQLParameters): String; override;
    function DoGetRelationalOperatorString(const RelationalOperator: TSQLRelationalOperator): String; override;
  public
    procedure GenerateSQL(const QueryCriteria: ISQLSelectCriteria;
       var SQL: String; const Parameters: ISQLParameters); override;
    procedure GenerateSQL(const QueryCriteria: ISQLInsertCriteria;
       var SQL: String; const Parameters: ISQLParameters); override;
  end;

  TORMSQLEngineMySQL = class(TORMSQLEngineMySQLBase)
  protected
    function DoFormatTableName(const TableName: String): String; override;
  end;

implementation

{ TORMSQLEngineMySQL }

procedure TORMSQLEngineMySQLBase.DoDefineTokens;
begin
  inherited;

  FTokenLineFeed := ' ';
  FTokenFirst := 'LIMIT';
  FTokenSkip := 'OFFSET';
end;

procedure TORMSQLEngineMySQLBase.GenerateSQL(
  const QueryCriteria: ISQLSelectCriteria; var SQL: String;
  const Parameters: ISQLParameters);
var
  i: Integer;
begin
  SQL := FTokenSelect;

  if QueryCriteria.GetDistinct then
  begin
    AddTokenEx(SQL, FTokenDistinct, FTokenWhiteSpace);
  end;

  // Add Field SQL
  AddTokenEx(SQL, GenerateFieldSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add From SQL
  AddTokenEx(SQL, GenerateFromSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Join SQL
  AddTokenEx(SQL, GenerateJoinSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Where SQL
  AddTokenEx(SQL, GenerateWhereSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Group By SQL
  AddTokenEx(SQL, GenerateGroupBySQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Having SQL
  AddTokenEx(SQL, GenerateHavingSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Order By SQL
  AddTokenEx(SQL, GenerateOrderBySQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add First SQL
  AddTokenEx(SQL, GenerateFirstSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Skip SQL
  AddTokenEx(SQL, GenerateSkipSQL(QueryCriteria, Parameters), FTokenLineFeed);

  for i := 0 to pred(QueryCriteria.GetParameters.Count) do
  begin
    Parameters.SetParam(
      QueryCriteria.GetParameters[i].Name,
      QueryCriteria.GetParameters[i].Value,
      QueryCriteria.GetParameters[i].FieldType);
  end;
end;

function TORMSQLEngineMySQLBase.DoGetFieldMacroText(
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

    Result := format('TIMESTAMPADD(minute, :%s, %s)',
                     [Param.Name,
                      FormatFieldName(SQLMacroDateLocalise.FieldName)]);
  end else

  if Supports(FieldCriteria.Macro, ISQLMacroTimestampDiff, SQLMacroTimestampDiff) then
  begin
    Result := format('TIMESTAMPDIFF(%s, %s, %s)',
                     [DateUnitToText(SQLMacroTimestampDiff.DateUnit),
                      FormatFieldName(SQLMacroTimestampDiff.StartDateField),
                      FormatFieldName(SQLMacroTimestampDiff.EndDateField)]);
  end else

  begin
    Result := inherited;
  end;
end;

function TORMSQLEngineMySQLBase.DoGetRelationalOperatorString(
  const RelationalOperator: TSQLRelationalOperator): String;
begin
  case RelationalOperator of
    Containing: Result := '%s COLLATE UTF8_GENERAL_CI LIKE CONCAT(''%%'', %s, ''%%'')';
  else
    Result := inherited;
  end;
end;

procedure TORMSQLEngineMySQLBase.GenerateSQL(
  const QueryCriteria: ISQLInsertCriteria; var SQL: String;
  const Parameters: ISQLParameters);
var
  FieldNames, Params: String;
begin
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
    AddTokenEx(SQL, 'ON DUPLICATE KEY UPDATE', FTokenLineFeed);
    AddTokenEx(SQL, GenerateFieldList(QueryCriteria, Parameters, gftUpdateFieldsAndParams), FTokenLineFeed);
  end;
end;

{ TORMSQLEngineMySQL }

function TORMSQLEngineMySQL.DoFormatTableName(const TableName: String): String;
begin
  Result := AnsiLowerCase(TableName);
end;

end.
