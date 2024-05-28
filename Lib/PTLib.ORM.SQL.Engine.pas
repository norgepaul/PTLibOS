{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.SQL.Engine                                     }
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

unit PTLib.ORM.SQL.Engine;

interface

uses
  Classes, SysUtils, Variants, Data.DB,

  PTLib.Common.Strings,

  PTLib.ORM.Interfaces,
  PTLib.ORM.Classes,
  PTLib.ORM.Types,
  PTLib.ORM.SQL.Criteria;

type
  TBaseORMSQLEngine = class(TInterfacedObject,
                            ISQLEngine)
  private
    procedure SetTokenBooleanFalse(const Value: String);
    procedure SetTokenBooleanTrue(const Value: String);
  protected
    FTokenFieldAliasAs: String;
    FTokenLineFeed: String;
    FTokenBooleanTrue: String;
    FTokenBooleanFalse: String;
    FTokenFirst: String;
    FTokenSkip: String;
    FTokenFrom: String;
    FTokenOrderByAscending: String;
    FTokenOrderByDescending: String;
    FTokenWhere: String;
    FTokenHaving: String;
    FTokenGroupBy: String;
    FTokenParamPrefix: String;
    FParamNamePrefix: String;
    FTokenFieldDelimiter: String;
    FTokenOrderBy: String;
    FTokenSelect: String;
    FTokenAnd: String;
    FTokenOr: String;
    FTokenOpenParanthesis: String;
    FTokenCloseParanthesis: String;
    FTokenJoinOn: String;
    FTokenJoinInner: String;
    FTokenJoinLeft: String;
    FTokenJoinRight: String;
    FTokenJoinFull: String;
    FTokenDelete: String;
    FTokenInsertOrUpdateInto: String;
    FTokenInsertInto: String;
    FTokenInsertValues: String;
    FTokenUpdate: String;
    FTokenUpdateSet: String;
    FTokenNull: String;
    FTokenWhiteSpace: String;
    FTokenDistinct: String;
    FTokenReturning: String;

    function AddWhiteSpacePadding(const Value: String): String;
    function GetFieldNameWithPrefix(const QueryCriteria: ISQLSelectCriteria;
      const FieldCriteria: ISQLFieldNameItem; const Parameters: ISQLParameters): String;
    function GenerateFieldList(const ORMBaseUpdateQueryCriteria: IBaseSQUpdateCriteria;
      const Parameters: ISQLParameters; const ListType: TGeneratedFieldListType): String;
    function GetTokenBooleanFalse: String;
    function GetTokenBooleanTrue: String;
    function AddParam(const Params: ISQLParameters; const SQLFieldValueItem: ISQLFieldValueItem): ISQLParameter; overload;
    function AddParam(const Params: ISQLParameters; const Value: Variant; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLParameter; overload;
    function AddParam(const Params: ISQLParameters; const Value: TStream): ISQLParameter; overload;
    function GetRootComparisonCriteria(const ComparisonCriteria: ISQLComparitor): ISQLComparitor;

    procedure DoDefineTokens; virtual;
    function DoDoubleQuoteString(const Value: String): String; virtual;
    function DoSingleQuoteString(const Value: String): String; virtual;
    function DoGetParamValueString(const Value: Variant; const UseQuotes: Boolean): String; virtual;
    function DoGetFieldMacroText(const QueryCriteria: ISQLSelectCriteria;
      const FieldCriteria: ISQLFieldNameItem; const Parameters: ISQLParameters): String; virtual;
    function DoDateUnitToText(const DateUnit: TSQLMacroDateUnit): String; virtual;
    function DoFormatTableName(const TableName: String): String; virtual;
    function DoFormatFieldName(const FieldName: String): String; virtual;
    function DoGetRelationalOperatorString(const RelationalOperator: TSQLRelationalOperator): String; virtual;
    procedure AddTokenEx(var Value: String; const Token, Seperator: String);

    function IsValidIdentifier(const Identifier: string): Boolean;
  public
    constructor Create;

    procedure GenerateSQL(const QueryCriteria: ISQLSelectCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;
    procedure GenerateSQL(const QueryCriteria: ISQLInsertCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;
    procedure GenerateSQL(const QueryCriteria: ISQLUpdateCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;
    procedure GenerateSQL(const QueryCriteria: ISQLDeleteCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;

    (*procedure GenerateSQL(const QueryCriteria: ISQLSelectCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;
    procedure GenerateSQL(const QueryCriteria: ISQLInsertCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;
    procedure GenerateSQL(const QueryCriteria: ISQLUpdateCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;
    procedure GenerateSQL(const QueryCriteria: ISQLDeleteCriteria;
       var SQL: String; const Parameters: ISQLParameters); overload; virtual;  *)

    function BooleanToToken(const Value: Boolean): String;
    function DateUnitToText(const DateUnit: TSQLMacroDateUnit): String;
    function FormatTableName(const TableName: String): String;
    function FormatFieldName(const FieldName: String): String;

    function GenerateFieldSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateFromSQL(const QueryCriteria: ISQLCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateFirstSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateSkipSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateWhereSQL(const QueryCriteria: ISQLWhereCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateHavingSQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateGroupBySQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateOrderBySQL(const QueryCriteria: ISQLSelectCriteria; const Parameters: ISQLParameters): String; virtual;
    function GenerateComparisonSQL(const ComparisonCriteria: ISQLComparitor;
      const QueryCriteria: ISQLWhereCriteria = nil; const Parameters: ISQLParameters = nil): String;
    function GenerateJoinSQL(const QueryCriteria: ISQLSelectCriteria;
      const Parameters: ISQLParameters): String;

    property TokenFieldAliasAs: String read FTokenFieldAliasAs write FTokenFieldAliasAs;
    property TokenLineFeed: String read FTokenLineFeed write FTokenLineFeed;
    property TokenFirst: String read FTokenFirst write FTokenFirst;
    property TokenSkip: String read FTokenSkip write FTokenSkip;
    property TokenFrom: String read FTokenFrom write FTokenFrom;
    property TokenOrderByAscending: String read FTokenOrderByAscending write FTokenOrderByAscending;
    property TokenOrderByDescending: String read FTokenOrderByDescending write FTokenOrderByDescending;
    property TokenWhere: String read FTokenWhere write FTokenWhere;
    property TokenHaving: String read FTokenHaving write FTokenHaving;
    property TokenGroupBy: String read FTokenGroupBy write FTokenGroupBy;
    property TokenParamPrefix: String read FTokenParamPrefix write FTokenParamPrefix;
    property ParamNamePrefix: String read FParamNamePrefix write FParamNamePrefix;
    property TokenFieldDelimiter: String read FTokenFieldDelimiter write FTokenFieldDelimiter;
    property TokenOrderBy: String read FTokenOrderBy write FTokenOrderBy;
    property TokenSelect: String read FTokenSelect write FTokenSelect;
    property TokenAnd: String read FTokenAnd write FTokenAnd;
    property TokenOr: String read FTokenOr write FTokenOr;
    property TokenOpenParanthesis: String read FTokenOpenParanthesis write FTokenOpenParanthesis;
    property TokenCloseParanthesis: String read FTokenCloseParanthesis write FTokenCloseParanthesis;
    property TokenJoinOn: String read FTokenJoinOn write FTokenJoinOn;
    property TokenJoinInner: String read FTokenJoinInner write FTokenJoinInner;
    property TokenJoinLeft: String read FTokenJoinLeft write FTokenJoinLeft;
    property TokenJoinRight: String read FTokenJoinRight write FTokenJoinRight;
    property TokenJoinFull: String read FTokenJoinFull write FTokenJoinFull;
    property TokenDelete: String read FTokenDelete write FTokenDelete;
    property TokenInsertOrUpdateInto: String read FTokenInsertOrUpdateInto write FTokenInsertOrUpdateInto;
    property TokenInsertInto: String read FTokenInsertInto write FTokenInsertInto;
    property TokenInsertValues: String read FTokenInsertValues write FTokenInsertValues;
    property TokenUpdate: String read FTokenUpdate write FTokenUpdate;
    property TokenUpdateSet: String read FTokenUpdateSet write FTokenUpdateSet;
    property TokenNull: String read FTokenNull write FTokenNull;
    property TokenWhiteSpace: String read FTokenWhiteSpace write FTokenWhiteSpace;
    property TokenDistinct: String read FTokenDistinct write FTokenDistinct;
    property TokenReturning: String read FTokenReturning write FTokenReturning;

    property TokenBooleanTrue: String read GetTokenBooleanTrue write SetTokenBooleanTrue;
    property TokenBooleanFalse: String read GetTokenBooleanFalse write SetTokenBooleanFalse;
  end;

implementation

uses
  System.Character;

const
  ParamPrefix = ':';
  ParameterValueTypes = [vtDefault, vtParameter];

{ TBaseORMSQLEngine }

function TBaseORMSQLEngine.AddParam(const Params: ISQLParameters; const SQLFieldValueItem: ISQLFieldValueItem): ISQLParameter;
begin
  // Add the primary key parameter
  if SQLFieldValueItem.ValueStream <> nil then
  begin
    Result := AddParam(Params, SQLFieldValueItem.ValueStream);
  end
  else
  begin
    Result := AddParam(Params, SQLFieldValueItem.Value, SQLFieldValueItem.ParamType);
  end;
end;

constructor TBaseORMSQLEngine.Create;
begin
  inherited;

  DoDefineTokens;
end;

function TBaseORMSQLEngine.DoDateUnitToText(
  const DateUnit: TSQLMacroDateUnit): String;
begin
  case DateUnit of
    duMilliSecond: Result := 'MILLISECOND';
    duSecond: Result := 'SECOND';
    duMinute: Result := 'MINUTE';
    duHour: Result := 'HOUR';
    duDay: Result := 'DAY';
    duMonth: Result := 'MONTH';
    duYear: Result := 'YEAR';
  end;
end;

procedure TBaseORMSQLEngine.DoDefineTokens;
begin
  FTokenFieldAliasAs := 'AS';
  FTokenLineFeed := #13#10;
  FTokenBooleanTrue := 'T';
  FTokenBooleanFalse := 'F';
  FTokenFirst := 'FIRST';
  FTokenSkip := 'SKIP';
  FTokenFrom := 'FROM';
  FTokenOrderByAscending := 'ASC';
  FTokenOrderByDescending := 'DESC';
  FTokenWhere := 'WHERE';
  FTokenHaving := 'HAVING';
  FTokenGroupBy := 'GROUP BY';
  FTokenParamPrefix := ':';
  FParamNamePrefix := 'P';
  FTokenFieldDelimiter := ',';
  FTokenOrderBy := 'ORDER BY';
  FTokenSelect := 'SELECT';
  FTokenAnd := 'AND';
  FTokenOr := 'OR';
  FTokenOpenParanthesis := '(';
  FTokenCloseParanthesis := ')';
  FTokenJoinOn := 'ON';
  FTokenJoinInner := 'INNER JOIN';
  FTokenJoinLeft := 'LEFT JOIN';
  FTokenJoinRight := 'RIGHT JOIN';
  FTokenJoinFull := 'FULL JOIN';
  FTokenDelete := 'DELETE';
  FTokenInsertOrUpdateInto := 'UPDATE OR INSERT INTO';
  FTokenInsertInto := 'INSERT INTO';
  FTokenInsertValues := 'VALUES';
  FTokenUpdate := 'UPDATE';
  FTokenUpdateSet := 'SET';
  FTokenNull := 'NULL';
  FTokenDistinct := 'DISTINCT';
  FTokenReturning := 'RETURNING';
  FTokenWhiteSpace := ' ';
end;

function TBaseORMSQLEngine.DoGetParamValueString(const Value: Variant; const UseQuotes: Boolean): String;
begin
  if Value = Null then
  begin
    Result := FTokenNull
  end
  else
  begin
    Result := VarToStr(Value);

    if (UseQuotes) and (VarIsStr(Value)) then
      Result := DoDoubleQuoteString(Result);
  end;
end;

function TBaseORMSQLEngine.DoDoubleQuoteString(const Value: String): String;
begin
  Result := AnsiQuotedStr(Value, '"');
end;

function TBaseORMSQLEngine.DoFormatFieldName(const FieldName: String): String;
begin
  Result := FieldName;
end;

function TBaseORMSQLEngine.DoFormatTableName(const TableName: String): String;
begin
  Result := TableName;
end;

function TBaseORMSQLEngine.DoSingleQuoteString(const Value: String): String;
begin
  Result := AnsiQuotedStr(Value, '''');
end;

function TBaseORMSQLEngine.FormatFieldName(const FieldName: String): String;
var
  TablePart, FieldPart: String;
  LastDotIdx: Integer;
begin
  if pos('.', FieldName) = 0 then
  begin
    Result := DoFormatFieldName(FieldName);
  end
  else
  begin
    LastDotIdx := Epos('.', FieldName);

    FieldPart := copy(FieldName, LastDotIdx + 1, MaxInt);
    TablePart := copy(FieldName, low(FieldName), LastDotIdx - 1);

    Result := DoFormatTableName(TablePart) + '.' + DoFormatFieldName(FieldPart);
  end;
end;

function TBaseORMSQLEngine.FormatTableName(const TableName: String): String;
begin
  Result := DoFormatTableName(TableName);
end;

function TBaseORMSQLEngine.AddParam(const Params: ISQLParameters;
  const Value: Variant; const ParamType: TFieldType): ISQLParameter;
begin
  Result := Params.SetParam(FParamNamePrefix + IntToStr(Params.Count + 1), Value, ParamType);
end;

function TBaseORMSQLEngine.AddParam(const Params: ISQLParameters;
  const Value: TStream): ISQLParameter;
begin
  Result := Params.SetParam(FParamNamePrefix + IntToStr(Params.Count + 1), Value);
end;

function TBaseORMSQLEngine.AddWhiteSpacePadding(const Value: String): String;
begin
  Result := FTokenWhiteSpace + Value + FTokenWhiteSpace;
end;

function TBaseORMSQLEngine.BooleanToToken(const Value: Boolean): String;
begin
  if Value then
  begin
    Result := TokenBooleanTrue;
  end
  else
  begin
    Result := TokenBooleanFalse;
  end;
end;

procedure TBaseORMSQLEngine.GenerateSQL(
  const QueryCriteria: ISQLDeleteCriteria; var SQL: String;
  const Parameters: ISQLParameters);
begin
  SQL := FTokenDelete;

  // Add From SQL
  AddTokenEx(SQL, GenerateFromSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Where SQL
  AddTokenEx(SQL, GenerateWhereSQL(QueryCriteria, Parameters), FTokenLineFeed);
end;

function TBaseORMSQLEngine.GenerateFieldSQL(
  const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
var
  FieldExpression: String;
  i: Integer;
begin
  Result := '';

  for i := 0 to pred(QueryCriteria.GetFieldCriteriaList.Count) do
  begin
    FieldExpression := GetFieldNameWithPrefix(QueryCriteria, QueryCriteria.GetFieldCriteriaList[i], Parameters);

    AddTokenEx(Result, FieldExpression, FTokenFieldDelimiter);
  end;
end;

function TBaseORMSQLEngine.GenerateFirstSQL(
  const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
begin
  if QueryCriteria.GetFirstCount <> -1 then
    Result := format(FTokenFirst + ' %d', [QueryCriteria.GetFirstCount])
  else
    Result := '';
end;

function TBaseORMSQLEngine.GenerateFromSQL(
  const QueryCriteria: ISQLCriteria;
  const Parameters: ISQLParameters): String;
begin
  if QueryCriteria.TableName <> '' then
  begin
    Result := FTokenFrom;

    AddTokenEx(Result, FormatTableName(QueryCriteria.TableName), FTokenWhiteSpace);
    AddTokenEx(Result, FormatTableName(QueryCriteria.TableAlias), FTokenWhiteSpace);
  end
  else
  begin
    Result := '';
  end;
end;

function TBaseORMSQLEngine.GenerateGroupBySQL(const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
var
  i: Integer;
  FieldName: String;
begin
  Result := '';

  if QueryCriteria.GetGroupByCriteria <> nil then
  begin
    for i := 0 to pred(QueryCriteria.GetGroupByCriteria.Count) do
    begin
      FieldName := QueryCriteria.GetGroupByCriteria[i];

      AddTokenEx(
        Result,
        FieldName,
        FTokenFieldDelimiter);
    end;
  end;

  if Result <> '' then
    Result := FTokenGroupBy + FTokenLineFeed + Result;
end;

function TBaseORMSQLEngine.GenerateHavingSQL(const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
begin
  Result := GenerateComparisonSQL(GetRootComparisonCriteria(QueryCriteria.GetHavingCriteria),
                                  QueryCriteria,
                                  Parameters);

  if Result <> '' then
    Result := FTokenHaving + FTokenLineFeed + Result;
end;

function TBaseORMSQLEngine.GenerateFieldList(const ORMBaseUpdateQueryCriteria: IBaseSQUpdateCriteria;
  const Parameters: ISQLParameters; const ListType: TGeneratedFieldListType): String;
var
  i: Integer;
  Param: ISQLParameter;
begin
  Result := '';

  for i := 0 to pred(ORMBaseUpdateQueryCriteria.GetFieldValueList.Count) do
    case ListType of
      gftFieldNames: AddTokenEx(Result, FormatFieldName(ORMBaseUpdateQueryCriteria.GetFieldValueList[i].FieldName), ',');

      gftInsertParams:
        begin
          if (Parameters <> nil) and
             (ORMBaseUpdateQueryCriteria.GetFieldValueList[i].UseParameter) then
          begin
            Param := AddParam(Parameters, ORMBaseUpdateQueryCriteria.GetFieldValueList[i]);

            AddTokenEx(Result, FTokenParamPrefix + Param.Name, ',');
          end
          else
          begin
            AddTokenEx(
              Result,
              DoGetParamValueString(ORMBaseUpdateQueryCriteria.GetFieldValueList[i].Value, False),
              ',');
          end;
        end;

      gftUpdateFieldsAndParams:
        begin
          if (Parameters <> nil) and
             (ORMBaseUpdateQueryCriteria.GetFieldValueList[i].UseParameter) then
          begin
            Param := AddParam(Parameters, ORMBaseUpdateQueryCriteria.GetFieldValueList[i]);

            AddTokenEx(Result, FormatFieldName(ORMBaseUpdateQueryCriteria.GetFieldValueList[i].FieldName) + '=' + FTokenParamPrefix + Param.Name, ',');
          end
          else
          begin
            AddTokenEx(Result, FormatFieldName(ORMBaseUpdateQueryCriteria.GetFieldValueList[i].FieldName) + '=' +
              DoGetParamValueString(ORMBaseUpdateQueryCriteria.GetFieldValueList[i].Value, False), ',');
          end;
        end;
    end;
end;

procedure TBaseORMSQLEngine.GenerateSQL(
  const QueryCriteria: ISQLInsertCriteria; var SQL: String;
  const Parameters: ISQLParameters);
var
  FieldNames, Params, ReturningFields: String;
  i: Integer;
begin
  if QueryCriteria.GetUpdateExistingRecords then
    SQL := FTokenInsertOrUpdateInto
  else
    SQL := FTokenInsertInto;

  FieldNames := GenerateFieldList(QueryCriteria, Parameters, gftFieldNames);
  Params := GenerateFieldList(QueryCriteria, Parameters, gftInsertParams);

  SQL := SQL +
         FTokenLineFeed +
         FormatTableName(QueryCriteria.TableName) +
         FTokenLineFeed +
         FTokenOpenParanthesis + FieldNames + FTokenCloseParanthesis +
         FTokenLineFeed +
         FTokenInsertValues +
         FTokenLineFeed +
         FTokenOpenParanthesis + Params + FTokenCloseParanthesis;

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

function TBaseORMSQLEngine.GenerateJoinSQL(
  const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
var
  i: Integer;
  JoinSQL: String;
  ORMComparitor: ISQLComparitor;
begin
  for i := 0 to pred(QueryCriteria.GetJoinCriteriaList.Count) do
  begin
    if Supports(QueryCriteria.GetJoinCriteriaList[i].Comparitor, ISQLComparitor, ORMComparitor) then
    begin
      case QueryCriteria.GetJoinCriteriaList[i].JoinType of
        InnerJoin: JoinSQL := FTokenJoinInner;
        LeftJoin: JoinSQL := FTokenJoinLeft;
        RightJoin: JoinSQL := FTokenJoinRight;
        FullJoin: JoinSQL := FTokenJoinFull;
      end;

      // Add the table name
      AddTokenEx(JoinSQL, FormatTableName(QueryCriteria.GetJoinCriteriaList[i].TableName), FTokenWhiteSpace);

      // Add the table alias (if any)
      AddTokenEx(JoinSQL, FormatTableName(QueryCriteria.GetJoinCriteriaList[i].Alias), FTokenWhiteSpace);

      JoinSQL := JoinSQL + FTokenWhiteSpace + FTokenJoinOn + FTokenLineFeed;

      JoinSQL := JoinSQL + GenerateComparisonSQL(
        GetRootComparisonCriteria(ORMComparitor),
        QueryCriteria,
        Parameters);

      AddTokenEx(Result, JoinSQL, FTokenLineFeed);
    end;
  end;
end;

function TBaseORMSQLEngine.GenerateOrderBySQL(
  const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
var
  i: Integer;
  DirectionString, FieldName: String;
begin
  Result := '';

  if QueryCriteria.GetOrderByCriteria <> nil then
  begin
    for i := 0 to pred(QueryCriteria.GetOrderByCriteria.GetOrderByCriteriaList.Count) do
    begin
      case QueryCriteria.GetOrderByCriteria.GetOrderByCriteriaList[i].OrderByDirection of
        Ascending: DirectionString := FTokenOrderByAscending;
        Descending: DirectionString := FTokenOrderByDescending;
      end;

      FieldName := FormatFieldName(QueryCriteria.GetOrderByCriteria.GetOrderByCriteriaList[i].FieldName);

      AddTokenEx(Result,
               FieldName + FTokenWhiteSpace + DirectionString,
               FTokenFieldDelimiter);
    end;
  end;

  if Result <> '' then
    Result := FTokenOrderBy + FTokenLineFeed + Result;
end;

function TBaseORMSQLEngine.DateUnitToText(const DateUnit: TSQLMacroDateUnit): String;
begin
  Result := DoDateUnitToText(DateUnit);
end;

function TBaseORMSQLEngine.DoGetFieldMacroText(const QueryCriteria: ISQLSelectCriteria;
  const FieldCriteria: ISQLFieldNameItem; const Parameters: ISQLParameters): String;
var
  SQLMacroDateLocalise: ISQLMacroDateLocalise;
  SQLMacroConcat: ISQLMacroConcat;
  SQLMacroTimestampDiff: ISQLMacroTimestampDiff;
  Param: ISQLParameter;
  i: Integer;
  NewValue: String;
begin
  if Supports(FieldCriteria.Macro, ISQLMacroDateLocalise, SQLMacroDateLocalise) then
  begin
    Param := AddParam(Parameters, SQLMacroDateLocalise.UTCOffsetMinutes);

    Result := format('DATEADD(minute, :%s, %s)',
                     [Param.Name,
                      FormatFieldName(SQLMacroDateLocalise.FieldName)]);
  end else
  if Supports(FieldCriteria.Macro, ISQLMacroConcat, SQLMacroConcat) then
  begin
    Result := '';

    for i := 0 to pred(SQLMacroConcat.Values.Count) do
    begin
      if SQLMacroConcat.Values[i].AddQuotes then
      begin
        NewValue := DoSingleQuoteString(SQLMacroConcat.Values[i].Value);
      end
      else
      begin
        NewValue := 'COALESCE(' +
                    FormatFieldName(SQLMacroConcat.Values[i].Value) +
                    ',' +
                    DoSingleQuoteString(SQLMacroConcat.Values[i].NullValue) +
                    ')';
      end;

      if Result <> '' then
        Result := Result + ' || ';

      Result := Result + NewValue;
    end;
  end else

  if Supports(FieldCriteria.Macro, ISQLMacroTimestampDiff, SQLMacroTimestampDiff) then
  begin
    Result := format('DATEDIFF(%s, %s, %s)',
                     [DateUnitToText(SQLMacroTimestampDiff.DateUnit),
                      FormatFieldName(SQLMacroTimestampDiff.StartDateField),
                      FormatFieldName(SQLMacroTimestampDiff.EndDateField)]);
  end else

  begin
    raise EORMFieldMacroNotFound.Create('Field macro not supported');
  end;
end;

function TBaseORMSQLEngine.GetFieldNameWithPrefix(const QueryCriteria: ISQLSelectCriteria;
  const FieldCriteria: ISQLFieldNameItem; const Parameters: ISQLParameters): String;
begin
  Result := '';

  if FieldCriteria.Macro <> nil then
  begin
    Result := DoGetFieldMacroText(QueryCriteria, FieldCriteria, Parameters);
  end else
  if FieldCriteria.SelectCriteria <> nil then
  begin
    GenerateSQL(
      FieldCriteria.SelectCriteria,
      Result,
      Parameters);

    Result := '(' + Result + ')';
  end else
  begin
    Result := FormatFieldName(FieldCriteria.FieldName);

    if (QueryCriteria.TableAlias <> '') and IsValidIdentifier(Result) then
      Result := FormatTableName(QueryCriteria.TableAlias) + '.' + Result;
  end;

  if FieldCriteria.Alias <> '' then
  begin
    Result := Result + FTokenWhiteSpace + FTokenFieldAliasAs + FTokenWhiteSpace + FieldCriteria.Alias;
  end;
end;

function TBaseORMSQLEngine.GetTokenBooleanFalse: String;
begin
  Result := FTokenBooleanFalse;
end;

function TBaseORMSQLEngine.GetTokenBooleanTrue: String;
begin
  Result := FTokenBooleanTrue;
end;

function TBaseORMSQLEngine.IsValidIdentifier(const Identifier: string): Boolean;
var
  Ch: Char;
begin
  // TODO : review it later
  Result := True;

  for Ch in Identifier do
    if not Ch.IsLetterOrDigit and (Ch <> '_') then
      Exit(False);
end;

procedure TBaseORMSQLEngine.SetTokenBooleanFalse(const Value: String);
begin
  FTokenBooleanFalse := Value;
end;

procedure TBaseORMSQLEngine.SetTokenBooleanTrue(const Value: String);
begin
  FTokenBooleanTrue := Value;
end;

procedure TBaseORMSQLEngine.AddTokenEx(var Value: String; const Token: String; const Seperator: String);
begin
  if (Value <> '') and
     (Value[high(Value)] <> Seperator) then
  begin
    Value := concat(Value, Seperator);
  end;

  Value := concat(Value, Token);
end;

procedure TBaseORMSQLEngine.GenerateSQL(
  const QueryCriteria: ISQLSelectCriteria; var SQL: String;
  const Parameters: ISQLParameters);
var
  i: Integer;
begin
  SQL := FTokenSelect;

  // Add First SQL
  AddTokenEx(SQL, GenerateFirstSQL(QueryCriteria, Parameters), FTokenLineFeed);

  // Add Skip SQL
  AddTokenEx(SQL, GenerateSkipSQL(QueryCriteria, Parameters), FTokenLineFeed);

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

  for i := 0 to pred(QueryCriteria.GetParameters.Count) do
  begin
    Parameters.SetParam(
      QueryCriteria.GetParameters[i].Name,
      QueryCriteria.GetParameters[i].Value,
      QueryCriteria.GetParameters[i].FieldType);
  end;
end;

function TBaseORMSQLEngine.GenerateSkipSQL(
  const QueryCriteria: ISQLSelectCriteria;
  const Parameters: ISQLParameters): String;
begin
  if QueryCriteria.GetSkipCount <> -1 then
    Result := format(FTokenSkip + ' %d', [QueryCriteria.GetSkipCount])
  else
    Result := '';
end;

procedure TBaseORMSQLEngine.GenerateSQL(
  const QueryCriteria: ISQLUpdateCriteria; var SQL: String;
  const Parameters: ISQLParameters);
var
  FieldsAndParams: String;
begin
  SQL := FTokenUpdate;

  FieldsAndParams := GenerateFieldList(QueryCriteria, Parameters, gftUpdateFieldsAndParams);

  SQL := SQL +
         FTokenLineFeed +
         FormatTableName(QueryCriteria.TableName) +
         FTokenLineFeed +
         FTokenUpdateSet +
         FTokenLineFeed +
         FieldsAndParams;

  AddTokenEx(SQL, GenerateWhereSQL(QueryCriteria, Parameters), FTokenLineFeed);
end;

function TBaseORMSQLEngine.GetRootComparisonCriteria(const ComparisonCriteria: ISQLComparitor): ISQLComparitor;
begin
  Result := ComparisonCriteria;

  if Result <> nil then
    Result := Result.GetRoot;
end;

function TBaseORMSQLEngine.GenerateComparisonSQL(const ComparisonCriteria: ISQLComparitor;
  const QueryCriteria: ISQLWhereCriteria; const Parameters: ISQLParameters): String;

  function GetOperatorExpressionValue(const OperatorExpression: TSQLOperatorExpression): String;
  begin
    case OperatorExpression of
      None: Result := '';
      &And: Result := AddWhiteSpacePadding(FTokenAnd);
      &Or: Result := AddWhiteSpacePadding(FTokenOr);
    end;
  end;

var
  RelationalExpression: String;
  Parameter: ISQLParameter;
  FieldName, SQL: String;
  InValue: String;
  RealValue: Variant;
  RealRelationalOperator: TSQLRelationalOperator;
begin
  Result := '';

  if not Assigned(ComparisonCriteria) then
    Exit;

  if ComparisonCriteria.InnerComparitor <> nil then
  begin
    Result := Result +
              GetOperatorExpressionValue(ComparisonCriteria.InnerComparitor.OperatorExpression) +
              FTokenOpenParanthesis +
              GenerateComparisonSQL(ComparisonCriteria.InnerComparitor,
                                    QueryCriteria,
                                    Parameters) +
              FTokenCloseParanthesis;
  end else
  begin
    FieldName := FormatFieldName(ComparisonCriteria.FieldName);

    RealRelationalOperator := ComparisonCriteria.RelationalOperator;

    if (ComparisonCriteria.GetValueType in ParameterValueTypes) and
       (pos('.', FormatFieldName(ComparisonCriteria.FieldName)) = 0) and
       (QueryCriteria <> nil) and
       (QueryCriteria.TableAlias <> '') then
    begin
      FieldName := FormatTableName(QueryCriteria.TableAlias) + '.' + FieldName;
    end;

    if ComparisonCriteria.GetSQLSelectCriteria <> nil then
    begin
      SQL := '';

      GenerateSQL(
        ComparisonCriteria.GetSQLSelectCriteria,
        SQL,
        Parameters);

      if RealRelationalOperator in [&In] then
        RealValue := SQL
      else
        RealValue := '(' + SQL + ')';
    end else
    if ComparisonCriteria.Value = Null then
    begin
      RealValue := FTokenNull;
    end
    else
    begin
      RealValue := ComparisonCriteria.Value;

      if ComparisonCriteria.GetValueType in [TSQLValueType.vtFieldReference] then
      begin
        RealValue := FormatFieldName(RealValue);
      end;
    end;

    RelationalExpression := DoGetRelationalOperatorString(RealRelationalOperator);

    if RealRelationalOperator in [&In] then
    begin
      if ComparisonCriteria.GetSQLSelectCriteria <> nil then
      begin
        GenerateSQL(
          ComparisonCriteria.GetSQLSelectCriteria,
          InValue,
          Parameters);
      end
      else
      begin
        InValue := ComparisonCriteria.Value;
      end;

      RelationalExpression :=
        format(RelationalExpression, [FormatFieldName(FieldName),
                                      InValue]);
    end
    else
    begin
      if (Parameters <> nil) and
         (ComparisonCriteria.Value <> NULL) and
         (ComparisonCriteria.GetValueType in ParameterValueTypes) then
// Removed - too dangerous
//          ((ComparisonCriteria.GetValueType in [vtDefault]) and
//           (pos('.', VarToStr(ComparisonCriteria.Value)) = 0))) then
      begin
        Parameter := AddParam(Parameters,
                              ComparisonCriteria.Value);

        RelationalExpression := format(RelationalExpression, [FormatFieldName(FieldName),
                                                              ParamPrefix + Parameter.Name]);
      end
      else
      begin
        RelationalExpression := format(RelationalExpression, [FormatFieldName(ComparisonCriteria.FieldName),
                                                              RealValue]);
      end;
    end;

    Result := Result + FTokenOpenParanthesis + RelationalExpression + FTokenCloseParanthesis;
  end;

  // Add the next Expression
  if ComparisonCriteria.NextComparitor <> nil then
  begin
    Result := Result +
              GetOperatorExpressionValue(ComparisonCriteria.NextComparitor.OperatorExpression) +
              GenerateComparisonSQL(ComparisonCriteria.NextComparitor,
                                    QueryCriteria,
                                    Parameters);
  end;
end;

function TBaseORMSQLEngine.DoGetRelationalOperatorString(const RelationalOperator: TSQLRelationalOperator): String;
begin
    case RelationalOperator of
      EqualTo: Result := '%s=%s';
      NotEqualTo: Result := '%s<>%s';
      LessThan: Result := '%s<%s';
      GreaterThan: Result := '%s>%s';
      LessThanOrEqualTo: Result := '%s<=%s';
      GreaterThanOrEqualTo: Result := '%s>=%s';
      Like: Result := '%s LIKE %s';
      Containing: Result := '%s CONTAINING %s';
      &Is:  Result := '%s IS %s';
      IsNot:  Result := '%s IS NOT %s';
      &In: Result := '%s IN (%s)';
      NotIn: Result := '%s NOT IN (%s)';
      SimilarTo: Result := '%s SIMILAR TO %s';
    end;
end;

function TBaseORMSQLEngine.GenerateWhereSQL(
  const QueryCriteria: ISQLWhereCriteria;
  const Parameters: ISQLParameters): String;
begin
  Result := '';

  Result := GenerateComparisonSQL(GetRootComparisonCriteria(QueryCriteria.GetWhereCriteria),
                                  QueryCriteria,
                                  Parameters);

  if Result <> '' then
    Result := FTokenWhere + FTokenLineFeed + Result;
end;

(*procedure TBaseORMSQLEngine.GenerateSQL(const QueryCriteria: ISQLInsertCriteria; var SQL: String;
  const Parameters: ISQLParameters);
begin
  Parameters.Clear;

  GenerateSQL(
    QueryCriteria,
    SQL,
    Parameters,
    ParameterIndex);
end;

procedure TBaseORMSQLEngine.GenerateSQL(const QueryCriteria: ISQLSelectCriteria; var SQL: String;
  const Parameters: ISQLParameters);
begin
  Parameters.Clear;

  GenerateSQL(
    QueryCriteria,
    SQL,
    Parameters);
end;

procedure TBaseORMSQLEngine.GenerateSQL(const QueryCriteria: ISQLDeleteCriteria; var SQL: String;
  const Parameters: ISQLParameters);
begin
  Parameters.Clear;

  GenerateSQL(
    QueryCriteria,
    SQL,
    Parameters);
end;

procedure TBaseORMSQLEngine.GenerateSQL(const QueryCriteria: ISQLUpdateCriteria; var SQL: String;
  const Parameters: ISQLParameters);
begin
  Parameters.Clear;

  ParameterIndex := 1;

  GenerateSQL(
    QueryCriteria,
    SQL,
    Parameters);
end;   *)

end.
