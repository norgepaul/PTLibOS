{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.SQL.Criteria                                   }
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

unit PTLib.ORM.SQL.Criteria;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Classes, Data.DB,

  PTLib.ORM.Classes,
  PTLib.ORM.Types,
  PTLib.ORM.Interfaces;

type
  TSQLComparitor = class(TInterfacedObject, ISQLComparitor)
  private
    FFieldName: string;
    FRelationalOperator: TSQLRelationalOperator;
    FValue: Variant;
    FSQLOperatorExpression: TSQLOperatorExpression;
    FValueType: TSQLValueType;
    FSQLSelectCriteria: ISQLSelectCriteria;

    FNextComparitor: ISQLComparitor;
    FInnerComparitor: ISQLComparitor;

    { LastComparitor field is used in root comparitor only }
    FLastComparitor: ISQLComparitor;
    function GetLastComparitor: ISQLComparitor;

    function &And(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const Value: Variant; const ValueType: TSQLValueType = vtDefault): ISQLComparitor; overload;
    function &Or(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const Value: Variant; const ValueType: TSQLValueType = vtDefault): ISQLComparitor; overload;
    function &And(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor; overload;
    function &Or(const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor; overload;
    function &And(const InnerComparitor: ISQLComparitor): ISQLComparitor; overload;
    function &Or(const InnerComparitor: ISQLComparitor): ISQLComparitor; overload;

    function GetFieldName: string;
    function GetInnerComparitor: ISQLComparitor;
    function GetNextComparitor: ISQLComparitor;
    function GetRelationalOperator: TSQLRelationalOperator;
    function GetValue: Variant;
    function GeTSQLOperatorExpression: TSQLOperatorExpression;
    function GetValueType: TSQLValueType;
    function GetSQLSelectCriteria: ISQLSelectCriteria;
    function GetRoot: ISQLComparitor;
    procedure SetNextComparitor(const Comparitor: ISQLComparitor);
    procedure SetInnerComparitor(const Comparitor: ISQLComparitor);
    function Clone: ISQLComparitor;
  public
    constructor Create(const SQLOperatorExpression: TSQLOperatorExpression;
      const FieldName: String; const RelationalOperator: TSQLRelationalOperator;
      const SQLSelectCriteria: ISQLSelectCriteria); overload; virtual;
    constructor Create(const SQLOperatorExpression: TSQLOperatorExpression;
      const FieldName: String; const RelationalOperator: TSQLRelationalOperator; const Value: Variant;
      const ValueType: TSQLValueType); overload; virtual;
    constructor Create(const SQLOperatorExpression: TSQLOperatorExpression;
      const InnerComparitor: ISQLComparitor); overload; virtual;

    destructor Destroy; override;

    property FieldName: string read GetFieldName;
    property RelationalOperator: TSQLRelationalOperator read GetRelationalOperator;
    property Value: Variant read GetValue;
    property NextComparitor: ISQLComparitor read GetNextComparitor;
    property InnerComparitor: ISQLComparitor read GetInnerComparitor;
    property OperatorExpression: TSQLOperatorExpression read GetSQLOperatorExpression;
  end;

  TSQLOrderByCriteria = class(TInterfacedObject,
                              ISQLOrderByCriteria)
  strict private
    FSQLOrderByItems: IORMList<ISQLOrderByItem>;
  public
    constructor Create;

    function Add(const FieldName: String; const OrderByDirection: TSQLOrderByDirection = Ascending): ISQLOrderByCriteria;
    function GetOrderByCriteriaList: IORMList<ISQLOrderByItem>;
  end;

  TSQLGroupByCriteria = class(TInterfacedObject,
                              ISQLGroupByCriteria)
  strict private
    FSQLGroupByItems: IORMList<String>;
  public
    constructor Create;

    function Add(const FieldName: String): ISQLGroupByCriteria;
    function GetGroupByCriteriaList: IORMList<String>;
  end;

  TORMQueryCriteria = class(TInterfacedObject, ISQLCriteria)
  strict private
    FCacheID: String;
  protected
    FTableName: String;
    FTableAlias: String;

    procedure CacheID(const ID: String);

    function GetCacheID: String;
    function TableName: String;
    function TableAlias: String;
  end;

  TORMWhereCriteria = class(TORMQueryCriteria)
  private
    FWhereCriteria: ISQLComparitor;
  protected
    function GetWhereCriteria: ISQLComparitor;
  end;

  TORMSelectQueryCriteria = class(TORMWhereCriteria, ISQLSelectCriteria)
  strict private
    FFirstCount: Integer;
    FSkipCount: Integer;
    FOrderByCriteria: ISQLOrderByCriteria;
    FFieldCriteriaList: IORMList<ISQLFieldNameItem>;
    FJoinCriteriaList: IORMList<ISQLJoinItem>;
    FGroupByCriteria: ISQLGroupByCriteria;
    FHavingCriteria: ISQLComparitor;
    FParameters: ISQLParameters;
    FDistinct: Boolean;
  private
    function JoinInternal(const TableName, TableAlias: String; const JoinType: TSQLTableJoin;
      const Comparitor: ISQLComparitor): ISQLSelectCriteria;
  protected
    function From(const TableName: String; const Alias: String = ''): ISQLSelectCriteria;
    function First(const Value: Integer): ISQLSelectCriteria;
    function Skip(const Value: Integer): ISQLSelectCriteria;
    function Where(const Comparitor: ISQLComparitor; const AppendOperator: TSQLOperatorExpression = &And): ISQLSelectCriteria;
    function OrderBy(const OrderByCriteria: ISQLOrderByCriteria): ISQLSelectCriteria;
    function Field(const FieldName: String; const Alias: String; const Position: Integer = -1): ISQLSelectCriteria; overload;
    function Field(const FieldName: String; const Position: Integer): ISQLSelectCriteria; overload;
    function Field(const FieldName: String): ISQLSelectCriteria; overload;
    function Field(const SQLMacro: ISQLMacro; const Alias: String; const Position: Integer = -1): ISQLSelectCriteria; overload;
    function Field(const SelectCriteria: ISQLSelectCriteria; const Alias: String; const Position: Integer = -1): ISQLSelectCriteria; overload;
    function Join(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function InnerJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function LeftJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function RightJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function FullJoin(const TableName, TableAlias: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function Join(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function InnerJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function LeftJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function RightJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function FullJoin(const TableName: String; const Comparitor: ISQLComparitor): ISQLSelectCriteria; overload;
    function SetParam(const ParamName: String; const ParamValue: Variant; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLSelectCriteria;
    function Distinct(const IsDistinct: Boolean = True): ISQLSelectCriteria;
    function GroupBy(const GroupByCriteria: ISQLGroupByCriteria): ISQLSelectCriteria;
    function Having(const Comparitor: ISQLComparitor; const AppendOperator: TSQLOperatorExpression = &And): ISQLSelectCriteria;

    function RemoveField(const FieldName: String): ISQLSelectCriteria;

    function GetFirstCount: Integer;
    function GetOrderByCriteria: ISQLOrderByCriteria;
    function GetSkipCount: Integer;
    function GetFieldCriteriaList: IORMList<ISQLFieldNameItem>;
    function GetJoinCriteriaList: IORMList<ISQLJoinItem>;
    function GetParameters: ISQLParameters;
    function GetDistinct: Boolean;
    function GetHavingCriteria: ISQLComparitor;
    function GetGroupByCriteria: IORMList<String>;
  public
    constructor Create; virtual;
  end;

  TBaseORMQueryUpdateCriteria = class(TORMWhereCriteria, IBaseORMQueryUpdateCriteria)
  private
    FFieldValueList: IORMList<ISQLFieldValueItem>;
    FReturnsList: TArray<String>;

    function FindOrAddFieldValue(const FieldName: String): ISQLFieldValueItem;
  protected
    function AddFieldValue(const FieldName: String; const Value: Variant; const UseParameter: Boolean; const ParamType: TFieldType): ISQLFieldValueItem; overload;
    function AddFieldValue(const FieldName: String; const Value: TStream): ISQLFieldValueItem; overload;
    procedure SetReturning(const FieldNames: TArray<String>); overload;

    function FindFieldValue(const FieldName: String): ISQLFieldValueItem;

    function GetFieldValueList: IORMList<ISQLFieldValueItem>;
    function GetReturnsList: TArray<String>;
  public
    constructor Create(const TableName: String; const TableAlias: String = ''); virtual;
  end;

  TSQLInsertCriteria = class(TBaseORMQueryUpdateCriteria, ISQLInsertCriteria)
  private
    FMemoryStream: TStream;
  protected
    FUpdateExistingRecords: Boolean;

    function Value(const FieldName: String; const Value: Variant; const UseParameter: Boolean = True; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLInsertCriteria; overload;
    function Value(const FieldName: String; const Value: TStream): ISQLInsertCriteria; overload;
    function Returning(const FieldNames: TArray<String>): ISQLInsertCriteria; overload;

    function GetUpdateExistingRecords: Boolean;
  public
    destructor Destroy; override;
  end;

  TSQLUpdateOrInsertCriteria = class(TSQLInsertCriteria)
  public
    constructor Create(const TableName: String; const TableAlias: String = ''); override;
  end;

  TSQLUpdateCriteria = class(TBaseORMQueryUpdateCriteria, ISQLUpdateCriteria)
  protected
    function Value(const FieldName: String; const Value: Variant; const UseParameter: Boolean = True; const ParamType: TFieldType = TFieldType.ftUnknown): ISQLUpdateCriteria; overload;
    function Value(const FieldName: String; const Value: TStream): ISQLUpdateCriteria; overload;
    function Returning(const FieldNames: TArray<String>): ISQLUpdateCriteria; overload;
    function Where(const Comparitor: ISQLComparitor): ISQLUpdateCriteria;
    function GetWhereCriteria: ISQLComparitor;
  end;

  TSQLDeleteCriteria = class(TORMWhereCriteria, ISQLDeleteCriteria)
  protected
    function Where(const Comparitor: ISQLComparitor): ISQLDeleteCriteria;
    function GetWhereCriteria: ISQLComparitor;
  public
    constructor Create(const TableName: String; const TableAlias: String = ''); virtual;
  end;

implementation

uses
  System.Variants;

resourcestring
  StrPrimaryKeyMappings = 'Primary key mappings and value counts do not match';

{ TORMQueryCriteria }

constructor TORMSelectQueryCriteria.Create;
begin
  inherited;

  FFieldCriteriaList := TORMList<ISQLFieldNameItem>.Create;
  FJoinCriteriaList := TORMList<ISQLJoinItem>.Create;
  FParameters := TSQLParameters.Create;
  FGroupByCriteria := TSQLGroupByCriteria.Create;

  FFirstCount := -1;
  FSkipCount := -1;
end;

function TORMSelectQueryCriteria.Distinct(const IsDistinct: Boolean): ISQLSelectCriteria;
begin
  FDistinct := IsDistinct;

  Result := Self;
end;

function TORMSelectQueryCriteria.Field(const SQLMacro: ISQLMacro;
  const Alias: String; const Position: Integer): ISQLSelectCriteria;
var
  FieldCriteria: ISQLFieldNameItem;
begin
  FieldCriteria := TSQLFieldNameItem.Create;
  FFieldCriteriaList.Insert(Position, FieldCriteria);
  FieldCriteria.Macro := SQLMacro;
  FieldCriteria.Alias := Alias;

  Result := Self;
end;

function TORMSelectQueryCriteria.Field(const FieldName: String;
  const Position: Integer): ISQLSelectCriteria;
begin
  Result := Field(FieldName, '', Position);
end;

function TORMSelectQueryCriteria.Field(
  const FieldName: String): ISQLSelectCriteria;
begin
  Result := Field(FieldName, '', -1);
end;

function TORMSelectQueryCriteria.First(const Value: Integer): ISQLSelectCriteria;
begin
  FFirstCount := Value;

  Result := Self;
end;

function TORMSelectQueryCriteria.From(
  const TableName: String; const Alias: String): ISQLSelectCriteria;
begin
  FTableName := TableName;
  FTableAlias := Alias;

  Result := Self;
end;

function TORMSelectQueryCriteria.FullJoin(const TableName: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := FullJoin(
    TableName,
    '',
    Comparitor);
end;

function TORMSelectQueryCriteria.FullJoin(const TableName, TableAlias: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := JoinInternal(TableName,
                         TableAlias,
                         TSQLTableJoin.FullJoin,
                         Comparitor);
end;

function TORMSelectQueryCriteria.GetDistinct: Boolean;
begin
  Result := FDistinct;
end;

function TORMSelectQueryCriteria.GetFieldCriteriaList: IORMList<ISQLFieldNameItem>;
begin
  Result := FFieldCriteriaList;
end;

function TORMSelectQueryCriteria.GetFirstCount: Integer;
begin
  Result := FFirstCount;
end;

function TORMSelectQueryCriteria.GetGroupByCriteria: IORMList<String>;
begin
  Result := FGroupByCriteria.GetGroupByCriteriaList;
end;

function TORMSelectQueryCriteria.GetHavingCriteria: ISQLComparitor;
begin
  Result := FHavingCriteria;
end;

function TORMSelectQueryCriteria.GetOrderByCriteria: ISQLOrderByCriteria;
begin
  Result := FOrderByCriteria;
end;

function TORMSelectQueryCriteria.GetParameters: ISQLParameters;
begin
  Result := FParameters;
end;

function TORMSelectQueryCriteria.GetSkipCount: Integer;
begin
  Result := FSkipCount;
end;

function TORMSelectQueryCriteria.GroupBy(const GroupByCriteria: ISQLGroupByCriteria): ISQLSelectCriteria;
begin
  FGroupByCriteria := GroupByCriteria;

  Result := Self;
end;

function TORMSelectQueryCriteria.Having(const Comparitor: ISQLComparitor;
  const AppendOperator: TSQLOperatorExpression): ISQLSelectCriteria;
begin
  if FHavingCriteria = nil then
  begin
    FHavingCriteria := Comparitor;
  end
  else
  begin
    case AppendOperator of
      None: FHavingCriteria := Comparitor;
      &And: FHavingCriteria.&And(Comparitor);
      &Or: FHavingCriteria.&Or(Comparitor);
    end;
  end;

  Result := Self;
end;

function TORMSelectQueryCriteria.InnerJoin(const TableName: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := InnerJoin(
    TableName,
    '',
    Comparitor);
end;

function TORMSelectQueryCriteria.InnerJoin(const TableName, TableAlias: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := JoinInternal(TableName,
                         TableAlias,
                         TSQLTableJoin.InnerJoin,
                         Comparitor);
end;

function TORMSelectQueryCriteria.Join(const TableName, TableAlias: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := JoinInternal(TableName,
                         TableAlias,
                         TSQLTableJoin.InnerJoin,
                         Comparitor);
end;

function TORMSelectQueryCriteria.Join(const TableName: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := Join(
    TableName,
    '',
    Comparitor);
end;

function TORMSelectQueryCriteria.JoinInternal(const TableName, TableAlias: String;
  const JoinType: TSQLTableJoin; const Comparitor: ISQLComparitor): ISQLSelectCriteria;
var
  JoinCriteria: TSQLJoinItem;
begin
  JoinCriteria := TSQLJoinItem.Create;
  FJoinCriteriaList.Add(JoinCriteria);

  JoinCriteria.TableName := TableName;
  JoinCriteria.Alias := TableAlias;
  JoinCriteria.JoinType := JoinType;
  JoinCriteria.Comparitor := Comparitor;

  Result := Self;
end;

function TORMSelectQueryCriteria.LeftJoin(const TableName: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := LeftJoin(
    TableName,
    '',
    Comparitor);
end;

function TORMSelectQueryCriteria.LeftJoin(const TableName, TableAlias: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := JoinInternal(TableName,
                         TableAlias,
                         TSQLTableJoin.LeftJoin,
                         Comparitor);
end;

function TORMSelectQueryCriteria.GetJoinCriteriaList: IORMList<ISQLJoinItem>;
begin
  Result := FJoinCriteriaList;
end;

function TORMSelectQueryCriteria.OrderBy(const OrderByCriteria: ISQLOrderByCriteria): ISQLSelectCriteria;
begin
  FOrderByCriteria := OrderByCriteria;

  Result := Self;
end;

function TORMSelectQueryCriteria.RemoveField(const FieldName: String): ISQLSelectCriteria;
var
  i: Integer;
begin
  Result := Self;

  for i := 0 to pred(FFieldCriteriaList.Count) do
    if SameText(FFieldCriteriaList[i].FieldName, FieldName) then
    begin
      FFieldCriteriaList.Delete(i);

      Break;
    end;
end;

function TORMSelectQueryCriteria.RightJoin(const TableName: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := RightJoin(
    TableName,
    '',
    Comparitor);
end;

function TORMSelectQueryCriteria.RightJoin(const TableName, TableAlias: String;
  const Comparitor: ISQLComparitor): ISQLSelectCriteria;
begin
  Result := JoinInternal(TableName,
                         TableAlias,
                         TSQLTableJoin.RightJoin,
                         Comparitor);
end;

function TORMSelectQueryCriteria.Field(const FieldName,
  Alias: String; const Position: Integer): ISQLSelectCriteria;
var
  FieldCriteria: ISQLFieldNameItem;
begin
  { TODO :
    Removing this code reduces field SQL generation time by approx 33%.
    Tried with TDictionary, but even worse. If a field is repeated,
    the SQL server will throw an error. }

  (*for i := 0 to pred(FFieldCriteriaList.Count) do
  begin
    if SameText(FFieldCriteriaList[i].FieldName, FieldName) then
    begin
      FieldCriteria := FFieldCriteriaList[i];

      Break;
    end;
  end; *)

  FieldCriteria := TSQLFieldNameItem.Create;
  FFieldCriteriaList.Insert(Position, FieldCriteria);
  FieldCriteria.FieldName := FieldName;

  FieldCriteria.Alias := Alias;

  Result := Self;
end;

function TORMSelectQueryCriteria.SetParam(const ParamName: String;
  const ParamValue: Variant; const ParamType: TFieldType): ISQLSelectCriteria;
begin
  FParameters.SetParam(ParamName, ParamValue, ParamType);

  Result := Self;
end;

function TORMSelectQueryCriteria.Skip(const Value: Integer): ISQLSelectCriteria;
begin
  FSkipCount := Value;

  Result := Self;
end;

function TORMSelectQueryCriteria.Where(const Comparitor: ISQLComparitor;
  const AppendOperator: TSQLOperatorExpression): ISQLSelectCriteria;
begin
  if FWhereCriteria = nil then
  begin
    FWhereCriteria := Comparitor;
  end
  else
  begin
    case AppendOperator of
      None: FWhereCriteria := Comparitor;
      &And: FWhereCriteria.&And(Comparitor);
      &Or: FWhereCriteria.&Or(Comparitor);
    end;
  end;

  Result := Self;
end;

{ TORMQueryCriteria }

procedure TORMQueryCriteria.CacheID(const ID: String);
begin
  FCacheID := ID;
end;

function TORMQueryCriteria.GetCacheID: String;
begin
  Result := FCacheID;
end;

function TORMQueryCriteria.TableAlias: String;
begin
  Result := FTableAlias;
end;

function TORMQueryCriteria.TableName: String;
begin
  Result := FTableName;
end;

{ TComparitor }

function TSQLComparitor.&And(const FieldName: String;
  const RelationalOperator: TSQLRelationalOperator;
  const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor;
begin
  GetLastComparitor.NextComparitor := TSQLComparitor.Create(TSQLOperatorExpression.&And,
                                                            FieldName,
                                                            RelationalOperator,
                                                            SQLSelectCriteria);

  FLastComparitor := GetLastComparitor.NextComparitor;

  Result := Self;
end;

function TSQLComparitor.Clone: ISQLComparitor;

  function CloneComparitor(const Src: ISQLComparitor): ISQLComparitor;
  begin
    if Src.GetSQLSelectCriteria <> nil then
      Result := TSQLComparitor.Create(Src.OperatorExpression,
                                      Src.FieldName,
                                      Src.RelationalOperator,
                                      Src.GetSQLSelectCriteria)
    else
      Result := TSQLComparitor.Create(Src.OperatorExpression,
                                      Src.FieldName,
                                      Src.RelationalOperator,
                                      Src.Value,
                                      Src.GetValueType);
  end;

  function CloneRec(const Src: ISQLComparitor): ISQLComparitor;
  begin
    if Src <> nil then
    begin
      Result := CloneComparitor(Src);

      Result.InnerComparitor := CloneRec(Src.InnerComparitor);
      Result.NextComparitor := CloneRec(Src.NextComparitor);
    end else
      Result := nil;
  end;

begin
  Result := CloneRec(GetRoot);
end;

constructor TSQLComparitor.Create(const SQLOperatorExpression: TSQLOperatorExpression;
  const InnerComparitor: ISQLComparitor);
begin
  inherited Create;
  FInnerComparitor := InnerComparitor;
  FSQLOperatorExpression := SQLOperatorExpression;
end;

constructor TSQLComparitor.Create(const SQLOperatorExpression: TSQLOperatorExpression;
  const FieldName: String;
  const RelationalOperator: TSQLRelationalOperator; const Value: Variant;
  const ValueType: TSQLValueType);
begin
  inherited Create;

  FRelationalOperator := RelationalOperator;

  if VarIsNull(Value) then
    case RelationalOperator of
      EqualTo:   FRelationalOperator := &Is;
      NotEqualTo: FRelationalOperator := &IsNot;
    end;

  FSQLOperatorExpression := SQLOperatorExpression;
  FFieldName := FieldName;
  FValue := Value;
  FValueType := ValueType;
end;

destructor TSQLComparitor.Destroy;
begin
  inherited;
end;

constructor TSQLComparitor.Create(const SQLOperatorExpression: TSQLOperatorExpression;
  const FieldName: String;
  const RelationalOperator: TSQLRelationalOperator; const SQLSelectCriteria: ISQLSelectCriteria);
begin
  inherited Create;

  FSQLOperatorExpression := SQLOperatorExpression;
  FFieldName := FieldName;
  FRelationalOperator := RelationalOperator;
  FSQLSelectCriteria := SQLSelectCriteria;
  FValueType := vtSQLStatement;
end;

function TSQLComparitor.GetFieldName: string;
begin
  Result := FFieldName;
end;

function TSQLComparitor.GetInnerComparitor: ISQLComparitor;
begin
  Result := FInnerComparitor;
end;

function TSQLComparitor.GetLastComparitor: ISQLComparitor;
begin
  if Assigned(FLastComparitor) then
    Result := FLastComparitor
  else
    Result := Self;
end;

function TSQLComparitor.GetNextComparitor: ISQLComparitor;
begin
  Result := FNextComparitor;
end;

function TSQLComparitor.GeTSQLOperatorExpression: TSQLOperatorExpression;
begin
  Result := FSQLOperatorExpression;
end;

function TSQLComparitor.GetRelationalOperator: TSQLRelationalOperator;
begin
  Result := FRelationalOperator;
end;

function TSQLComparitor.GetSQLSelectCriteria: ISQLSelectCriteria;
begin
  Result := FSQLSelectCriteria;
end;

function TSQLComparitor.GetRoot: ISQLComparitor;
begin
  Result := Self;

//  Assert(Assigned(Result));
//  While Result.GetOwnerComparitor <> nil do
//    Result := Result.GetOwnerComparitor;
end;

function TSQLComparitor.GetValueType: TSQLValueType;
begin
  Result := FValueType;
end;

function TSQLComparitor.GetValue: Variant;
begin
  Result := FValue;
end;

function TSQLComparitor.&And(const FieldName: String;
  const RelationalOperator: TSQLRelationalOperator;
  const Value: Variant; const ValueType: TSQLValueType): ISQLComparitor;
begin
  GetLastComparitor.NextComparitor := TSQLComparitor.Create(TSQLOperatorExpression.&And,
                                                            FieldName,
                                                            RelationalOperator,
                                                            Value,
                                                            ValueType);

  FLastComparitor := GetLastComparitor.NextComparitor;

  Result := Self;
end;

function TSQLComparitor.&Or(const FieldName: String;
  const RelationalOperator: TSQLRelationalOperator;
  const Value: Variant; const ValueType: TSQLValueType): ISQLComparitor;
begin
  GetLastComparitor.NextComparitor := TSQLComparitor.Create(TSQLOperatorExpression.&Or,
                                                            FieldName,
                                                            RelationalOperator,
                                                            Value,
                                                            ValueType);

  FLastComparitor := GetLastComparitor.NextComparitor;

  Result := Self;
end;

{ TBaseORMQueryUpdateCriteria }

function TBaseORMQueryUpdateCriteria.FindOrAddFieldValue(const FieldName: String): ISQLFieldValueItem;
begin
  Result := FindFieldValue(FieldName);

  if Result = nil then
  begin
    Result := TSQLFieldValueItem.Create;
    FFieldValueList.Add(Result);
    Result.FieldName := FieldName;
  end;
end;

function TBaseORMQueryUpdateCriteria.AddFieldValue(const FieldName: String;
  const Value: Variant; const UseParameter: Boolean; const ParamType: TFieldType): ISQLFieldValueItem;
begin
  Result := FindOrAddFieldValue(FieldName);
  Result.Value := Value;
  Result.UseParameter := UseParameter;
  Result.ParamType := ParamType;
end;

function TBaseORMQueryUpdateCriteria.AddFieldValue(const FieldName: String;
  const Value: TStream): ISQLFieldValueItem;
begin
  Result := FindOrAddFieldValue(FieldName);
  Result.ValueStream := Value;
  Result.UseParameter := True;
end;

constructor TBaseORMQueryUpdateCriteria.Create(const TableName: String;
  const TableAlias: String);
begin
  inherited Create;

  FFieldValueList := TORMList<ISQLFieldValueItem>.Create;
  FTableName := TableName;
  FTableAlias := TableAlias;
end;

function TBaseORMQueryUpdateCriteria.FindFieldValue(
  const FieldName: String): ISQLFieldValueItem;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(FFieldValueList.Count) do
    if SameText(FFieldValueList[i].FieldName, FieldName) then
      Exit(FFieldValueList[i]);
end;

function TBaseORMQueryUpdateCriteria.GetFieldValueList: IORMList<ISQLFieldValueItem>;
begin
  Result := FFieldValueList;
end;

function TBaseORMQueryUpdateCriteria.GetReturnsList: TArray<String>;
begin
  Result := FReturnsList;
end;

procedure TBaseORMQueryUpdateCriteria.SetReturning(const FieldNames: TArray<String>);
begin
  FReturnsList := FieldNames;
end;

{ TORMWhereCriteria }

function TORMWhereCriteria.GetWhereCriteria: ISQLComparitor;
begin
  Result := FWhereCriteria;
end;

{ TORMInsertQueryCriteria }

destructor TSQLInsertCriteria.Destroy;
begin
  FreeAndNil(FMemoryStream);

  inherited;
end;

function TSQLInsertCriteria.GetUpdateExistingRecords: Boolean;
begin
  Result := FUpdateExistingRecords;
end;

function TSQLInsertCriteria.Returning(const FieldNames: TArray<String>): ISQLInsertCriteria;
begin
  SetReturning(FieldNames);

  Result := Self;
end;

function TSQLInsertCriteria.Value(const FieldName: String;
  const Value: TStream): ISQLInsertCriteria;
begin
  AddFieldValue(FieldName, Value);

  Result := Self;
end;

function TSQLInsertCriteria.Value(const FieldName: String;
  const Value: Variant; const UseParameter: Boolean; const ParamType: TFieldType): ISQLInsertCriteria;
begin
  AddFieldValue(FieldName, Value, UseParameter, ParamType);

  Result := Self;
end;

{ TORMUpdateQueryCriteria }

function TSQLUpdateCriteria.GetWhereCriteria: ISQLComparitor;
begin
  Result := FWhereCriteria;
end;

function TSQLUpdateCriteria.Returning(const FieldNames: TArray<String>): ISQLUpdateCriteria;
begin
  SetReturning(FieldNames);

  Result := Self;
end;

function TSQLUpdateCriteria.Value(const FieldName: String;
  const Value: TStream): ISQLUpdateCriteria;
begin
  AddFieldValue(FieldName, Value);

  Result := Self;
end;

function TSQLUpdateCriteria.Value(const FieldName: String;
  const Value: Variant; const UseParameter: Boolean; const ParamType: TFieldType): ISQLUpdateCriteria;
begin
  AddFieldValue(FieldName, Value, UseParameter, ParamType);

  Result := Self;
end;

function TSQLUpdateCriteria.Where(
  const Comparitor: ISQLComparitor): ISQLUpdateCriteria;
begin
  FWhereCriteria := Comparitor;

  Result := Self;
end;

{ TORMDeleteQueryCriteria }

constructor TSQLDeleteCriteria.Create(const TableName: String; const TableAlias: String);
begin
  inherited Create;

  FTableName := TableName;
  FTableAlias := TableAlias;
end;

function TSQLDeleteCriteria.GetWhereCriteria: ISQLComparitor;
begin
  Result := FWhereCriteria;
end;

function TSQLDeleteCriteria.Where(
  const Comparitor: ISQLComparitor): ISQLDeleteCriteria;
begin
  FWhereCriteria := Comparitor;

  Result := Self;
end;

function TSQLComparitor.&And(
  const InnerComparitor: ISQLComparitor): ISQLComparitor;
begin
  GetLastComparitor.NextComparitor := TSQLComparitor.Create(TSQLOperatorExpression.&And, InnerComparitor);
  FLastComparitor := GetLastComparitor.NextComparitor;

  Result := Self;
end;

function TSQLComparitor.&Or(
  const InnerComparitor: ISQLComparitor): ISQLComparitor;
begin
  GetLastComparitor.NextComparitor := TSQLComparitor.Create(TSQLOperatorExpression.&Or, InnerComparitor);
  FLastComparitor := GetLastComparitor.NextComparitor;

  Result := Self;
end;

function TSQLComparitor.&Or(const FieldName: String;
  const RelationalOperator: TSQLRelationalOperator;
  const SQLSelectCriteria: ISQLSelectCriteria): ISQLComparitor;
begin
  GetLastComparitor.NextComparitor := TSQLComparitor.Create(TSQLOperatorExpression.&Or,
                                                            FieldName,
                                                            RelationalOperator,
                                                            SQLSelectCriteria);

  FLastComparitor := GetLastComparitor.NextComparitor;

  Result := Self;
end;

procedure TSQLComparitor.SetInnerComparitor(const Comparitor: ISQLComparitor);
begin
  FInnerComparitor := Comparitor;
end;

procedure TSQLComparitor.SetNextComparitor(const Comparitor: ISQLComparitor);
begin
  FNextComparitor := Comparitor;
end;

{ TSQLOrderByCriteria }

function TSQLOrderByCriteria.Add(const FieldName: String;
  const OrderByDirection: TSQLOrderByDirection): ISQLOrderByCriteria;
var
  OrderByItem: TSQLOrderByItem;
begin
  OrderByItem := TSQLOrderByItem.Create;
  OrderByItem.FieldName := FieldName;
  OrderByItem.OrderByDirection := OrderByDirection;

  FSQLOrderByItems.Add(OrderByItem);

  Result := Self;
end;

constructor TSQLOrderByCriteria.Create;
begin
  FSQLOrderByItems := TORMList<ISQLOrderByItem>.Create
end;

function TSQLOrderByCriteria.GetOrderByCriteriaList: IORMList<ISQLOrderByItem>;
begin
  Result := FSQLOrderByItems;
end;

{ TSQLGroupByCriteria }

function TSQLGroupByCriteria.Add(const FieldName: String): ISQLGroupByCriteria;
begin
  FSQLGroupByItems.Add(FieldName);

  Result := Self;
end;

constructor TSQLGroupByCriteria.Create;
begin
  FSQLGroupByItems := TORMList<String>.Create;
end;

function TSQLGroupByCriteria.GetGroupByCriteriaList: IORMList<String>;
begin
  Result := FSQLGroupByItems;
end;

function TORMSelectQueryCriteria.Field(const SelectCriteria: ISQLSelectCriteria; const Alias: String;
  const Position: Integer): ISQLSelectCriteria;
var
  FieldCriteria: ISQLFieldNameItem;
begin
  FieldCriteria := TSQLFieldNameItem.Create;
  FFieldCriteriaList.Insert(Position, FieldCriteria);
  FieldCriteria.SelectCriteria := SelectCriteria;
  FieldCriteria.Alias := Alias;

  Result := Self;
end;

{ TSQLUpdateOrInsertCriteria }

constructor TSQLUpdateOrInsertCriteria.Create(const TableName: String; const TableAlias: String);
begin
  inherited;

  FUpdateExistingRecords := True;
end;

end.
