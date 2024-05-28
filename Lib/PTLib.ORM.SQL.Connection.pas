{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.SQL.Connection                                 }
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

unit PTLib.ORM.SQL.Connection;

interface

uses
  System.Classes, Data.DB, System.SysUtils, System.Generics.Collections, System.Variants,

  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Strings,

  PTLib.ORM.SQL.Criteria,
  PTLib.ORM.Classes,
  PTLib.ORM.Types,
  PTLib.ORM.Interfaces;

type
  TSQLQueryCallbacks = class(TInterfacedObject,
                             ISQLQueryCallbacks)
  strict private
    FOnBeforeQueryCallback: TSQLQueryOnBeforeCallback;
    FOnAfterQueryCallback: TSQLQueryOnAfterCallback;
    FOnExceptionCallback: TSQLQueryExceptionCallback;
  private
    function GetOnAfterQueryCallback: TSQLQueryOnAfterCallback;
    function GetOnBeforeQueryCallback: TSQLQueryOnBeforeCallback;
    function GetOnExceptionCallback: TSQLQueryExceptionCallback;

    procedure SetOnAfterQueryCallback(const Value: TSQLQueryOnAfterCallback);
    procedure SetOnBeforeQueryCallback(const Value: TSQLQueryOnBeforeCallback);
    procedure SetOnExceptionCallback(const Value: TSQLQueryExceptionCallback);
  public
    property OnBeforeQueryCallback: TSQLQueryOnBeforeCallback read GetOnBeforeQueryCallback write SetOnBeforeQueryCallback;
    property OnAfterQueryCallback: TSQLQueryOnAfterCallback read GetOnAfterQueryCallback write SetOnAfterQueryCallback;
    property OnExceptionCallback: TSQLQueryExceptionCallback read GetOnExceptionCallback write SetOnExceptionCallback;
  end;

  TSQLQuery = class(TBaseSQLQuery, ISQLQuery)
  strict private
    FSQL: String;
    FParameters: ISQLParameters;
    FSQLQueryCallbacks: ISQLQueryCallbacks;
  private
    function GetParameters: ISQLParameters;
    function GetSQL: String;
    procedure SetSQL(const Value: String);
    function GetFormattedSQL(const ReplaceParameters: Boolean): String;
  protected
    procedure DoOnBeforeCallback; virtual;
    procedure DoOnAfterCallback(const TickCount: Cardinal); virtual;
    procedure DoOnExceptionCallback(const e: Exception); virtual;
  public
    constructor Create(const SQL: String; const Parameters: ISQLParameters;
      const SQLQueryCallbacks: ISQLQueryCallbacks); override;

    property SQL: String read GetSQL write SetSQL;
    property Parameters: ISQLParameters read GetParameters;
  end;

  TSQLDataset = class(TSQLQuery, ISQLDataset)
  protected
    procedure DoOpen; virtual; abstract;
    procedure DoToJSONString(out Value: String; const IncludeMetaData: Boolean); virtual; abstract;
  public
    function Dataset: TDataset; virtual; abstract;
    procedure Open;
    procedure Close;
    function NativeDataset: IInterface; virtual; abstract;

    procedure DisableControls;
    procedure EnableControls;
    function FieldByName(const FieldName: string): TField;
    function FindField(const FieldName: string): TField;
    function FindFirst: Boolean;
    function FindLast: Boolean;
    function FindNext: Boolean;
    function FindPrior: Boolean;
    procedure First;
    procedure GetFieldNames(List: TStrings);
    function IsEmpty: Boolean;
    procedure Last;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant;
    function MoveBy(Distance: Integer): Integer;
    procedure Next;
    procedure Prior;
    function Bof: Boolean;
    function Eof: Boolean;
    function FieldCount: Integer;
    function Fields: TFields;
    function RecordCount: Integer;
    function RecNo: Integer;
    procedure GoToRecord(const RecordNumber: Integer);
    function ToJSONString(const IncludeMetaData: Boolean = True): String;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
    procedure LoadFromJSON(const JSON: String);
    procedure LoadFromStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto); virtual;
    procedure SaveToStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto); overload;
    procedure SaveToStream(const AStream: TStream; const DatasetStoreItems: TDatasetStoreItems; const DatasetStreamType: TDatasetStreamType = dssAuto); overload; virtual;
  end;

  TSQLDatasetClass = class of TSQLDataset;

  TORMSQLCommand = class(TSQLQuery, ISQLCommand)
  strict private
    FRowsAdded: Integer;
  private
    function GetRowsAdded: Integer;
    procedure SetRowsAdded(const Value: Integer);
  protected
    function DoExecute: Integer; overload; virtual; abstract;
    function DoOpen: Integer; overload; virtual; abstract;
    procedure DoGetFieldByName(const FieldName: String; out Field: TField); virtual; abstract;
    procedure DoGetFields(out Fields: TFields); virtual; abstract;
  public
    function Execute: Integer; overload;
    function Open: Integer; overload;
    function NativeSQLCommand: IInterface; virtual; abstract;
    property RowsAdded: Integer read GetRowsAdded write SetRowsAdded;
    function FieldByName(const FieldName: string): TField;
    function Fields: TFields;
  end;

  TORMConnection = class(TInterfacedObject, ISQLConnection)
  protected
    FConnectionString: String;
    FSQLEngine: ISQLEngine;
    FTransactionIsolation: TSQLTransactionIsolation;
    FTransactionReadOnly: Boolean;

    function DoGetSQLEngine: ISQLEngine; virtual;
  protected
    // Abstract methods
    function DoNewCommand(const SQL: string; const Parameters: ISQLParameters; const Execute: TCommandExecutType;
      const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand; overload; virtual; abstract;
    function DoNewDataset(const SQL: string; const Parameters: ISQLParameters; const Open: Boolean;
      const SQLQueryCallbacks: ISQLQueryCallbacks; const SQLDatasetClass: TSQLQueryClass): ISQLDataset; overload; virtual; abstract;
    procedure GetTableNames(const TableNames: TStrings); virtual; abstract;
    procedure GetTableFields(const TableName: String; out Fields: IORMList<IORMField>); virtual; abstract;
    procedure BeginTransaction; virtual; abstract;
    procedure CommitTransaction; virtual; abstract;
    procedure RollbackTransaction; virtual; abstract;
    function InTransaction: Boolean; virtual; abstract;
    function NativeConnection: TObject; virtual; abstract;
    function NativeTransaction: TObject; virtual; abstract;
    function GetAttachmentID: Cardinal; virtual; abstract;
    function SupportsAutoIncFields: Boolean; virtual;
    function GetTransactionIsolation: TSQLTransactionIsolation;
    function GetTransactionReadOnly: Boolean;
    procedure SetTransactionIsolation(const Value: TSQLTransactionIsolation); virtual;
    procedure SetTransactionReadOnly(const Value: Boolean); virtual;
    procedure CancelConnectionSQLCommands(const ConnectionID: Cardinal); virtual;
    function GetLastAutoIncValue(const Name: String = ''): Variant; virtual;
    function GetDatabaseProperties: IParameters; virtual;
    function KillLongRunningTransactions(const TransactionAgeSeconds: Integer): Boolean; virtual;
  private
    function InsertCriteriaToCommandExecuteType(SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean): TCommandExecutType;
    function UpdateCriteriaToCommandExecuteType(SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean): TCommandExecutType;
    function DeleteCriteriaToCommandExecuteType(SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean): TCommandExecutType;
  protected
    // SQL Commands
    function NewCommand(const SQL: string; const Parameters: ISQLParameters = nil; const Execute: TCommandExecutType = ceExecute;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQL: string; const ParamNames: array of String; const ParamValues: array of Variant;
      const Execute: TCommandExecutType = ceExecute; const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;
    function NewCommand(const SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil): ISQLCommand; overload;

    // SQL Datasets
    function NewDataset(const SQL: string; const Parameters: ISQLParameters = nil; const Open: Boolean = True;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil; const SQLDatasetClass: TSQLQueryClass = nil): ISQLDataset; overload;
    function NewDataset(const QueryCriteria: ISQLSelectCriteria;
      const SQLQueryCallbacks: ISQLQueryCallbacks = nil; const SQLDatasetClass: TSQLQueryClass = nil): ISQLDataset; overload;
    function NewDataset(const SQL: string; const ParamNames: array of String;
       const ParamValues: array of Variant; const Open: Boolean = True;
       const SQLQueryCallbacks: ISQLQueryCallbacks = nil; const SQLDatasetClass: TSQLQueryClass = nil): ISQLDataset; overload;

    // General
    function SQLEngine: ISQLEngine;

    property TransactionIsolation: TSQLTransactionIsolation read GetTransactionIsolation write SetTransactionIsolation;
    property TransactionReadOnly: Boolean read GetTransactionReadOnly write SetTransactionReadOnly;
  public
    constructor Create(const ConnectionString: String); virtual;
  end;
  TORMConnectionClass = class of TORMConnection;

implementation

{ TSQLDataset }

function TSQLDataset.Bof: Boolean;
begin
  Result := Dataset.Bof;
end;

procedure TSQLDataset.Close;
begin
  Dataset.Close;
end;

function TSQLDataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
begin
  Result := Dataset.CreateBlobStream(Field, Mode);
end;

procedure TSQLDataset.DisableControls;
begin
  Dataset.DisableControls;
end;

procedure TSQLDataset.EnableControls;
begin
  Dataset.EnableControls;
end;

function TSQLDataset.Eof: Boolean;
begin
  Result := Dataset.Eof;
end;

function TSQLDataset.FieldByName(const FieldName: string): TField;
begin
  Result := Dataset.FieldByName(FieldName);
end;

function TSQLDataset.FieldCount: Integer;
begin
  Result := Dataset.FieldCount;
end;

function TSQLDataset.Fields: TFields;
begin
  Result := Dataset.Fields;
end;

function TSQLDataset.FindField(const FieldName: string): TField;
begin
  Result := Dataset.FindField(FieldName);
end;

function TSQLDataset.FindFirst: Boolean;
begin
  Result := Dataset.FindFirst;
end;

function TSQLDataset.FindLast: Boolean;
begin
  Result := Dataset.FindLast;
end;

function TSQLDataset.FindNext: Boolean;
begin
  Result := Dataset.FindNext;
end;

function TSQLDataset.FindPrior: Boolean;
begin
  Result := Dataset.FindPrior;
end;

procedure TSQLDataset.First;
begin
  Dataset.First;
end;

procedure TSQLDataset.GetFieldNames(List: TStrings);
begin
  Dataset.GetFieldNames(List);
end;

procedure TSQLDataset.GoToRecord(const RecordNumber: Integer);
begin
  Dataset.RecNo := RecordNumber;
end;

function TSQLDataset.IsEmpty: Boolean;
begin
  Result := Dataset.IsEmpty;
end;

procedure TSQLDataset.Last;
begin
  Dataset.Last;
end;

procedure TSQLDataset.LoadFromJSON(const JSON: String);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(JSON);
  try
    StringStream.Position := 0;

    LoadFromStream(StringStream, TDatasetStreamType.dssJSON);
  finally
    FreeAndNil(StringStream);
  end;
end;

procedure TSQLDataset.LoadFromStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto);
begin
  raise EORMError.Create('Streaming not implemented');
end;

function TSQLDataset.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  Result := Dataset.Locate(KeyFields, KeyValues, Options);
end;

function TSQLDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := Dataset.Lookup(KeyFields, KeyValues,  ResultFields);
end;

function TSQLDataset.MoveBy(Distance: Integer): Integer;
begin
 Result := Dataset.MoveBy(Distance);
end;

procedure TSQLDataset.Next;
begin
  Dataset.Next;
end;

procedure TSQLDataset.Open;
var
  TickCount: Cardinal;
begin
  DoOnBeforeCallback;
  try
    TickCount := TThread.GetTickCount;

    DoOpen;

    DoOnAfterCallback(TThread.GetTickCount - TickCount);
  except
    on e: Exception do
    begin
      DoOnExceptionCallback(e);

      raise;
    end;
  end;
end;

procedure TSQLDataset.Prior;
begin
  Dataset.Prior;
end;

function TSQLDataset.RecNo: Integer;
begin
  Result := Dataset.RecNo;
end;

function TSQLDataset.RecordCount: Integer;
begin
  Result := Dataset.RecordCount;
end;

procedure TSQLDataset.SaveToStream(const AStream: TStream;
  const DatasetStoreItems: TDatasetStoreItems;
  const DatasetStreamType: TDatasetStreamType);
begin
  raise EORMError.Create('Streaming not implemented');
end;

function TSQLDataset.ToJSONString(const IncludeMetaData: Boolean): String;
begin
  DoToJSONString(Result, IncludeMetaData);
end;

procedure TSQLDataset.SaveToStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto);
begin
  SaveToStream(
    AStream,
    [diMeta, diData, diDelta],
    DatasetStreamType);
end;

{ TORMConnection }

procedure TORMConnection.CancelConnectionSQLCommands(const ConnectionID: Cardinal);
begin
  raise EORMFunctionNotImplemented.Create('This function is not implemented for the current connection type');
end;

constructor TORMConnection.Create(const ConnectionString: String);
begin
  FConnectionString := ConnectionString;
end;

function TORMConnection.DoGetSQLEngine: ISQLEngine;
begin
  Assert(False, 'Please override DoGetSQLEngine');
end;

function TORMConnection.GetDatabaseProperties: IParameters;
begin
  Result := NewParameters;
end;

function TORMConnection.GetLastAutoIncValue(const Name: String): Variant;
begin
  Result := 0;
end;

function TORMConnection.GetTransactionIsolation: TSQLTransactionIsolation;
begin
  Result := FTransactionIsolation;
end;

function TORMConnection.GetTransactionReadOnly: Boolean;
begin
  Result := FTransactionReadOnly;
end;

function TORMConnection.KillLongRunningTransactions(
  const TransactionAgeSeconds: Integer): Boolean;
begin
  Result := False;
end;

function TORMConnection.NewDataset(const SQL: string; const Parameters: ISQLParameters; const Open: Boolean;
  const SQLQueryCallbacks: ISQLQueryCallbacks; const SQLDatasetClass: TSQLQueryClass): ISQLDataset;
begin
  Result := DoNewDataset(
    SQL,
    Parameters,
    Open,
    SQLQueryCallbacks,
    SQLDatasetClass);
end;

procedure TORMConnection.SetTransactionIsolation(const Value: TSQLTransactionIsolation);
begin
  FTransactionIsolation := Value;
end;

procedure TORMConnection.SetTransactionReadOnly(const Value: Boolean);
begin
  FTransactionReadOnly := Value;
end;

function TORMConnection.SQLEngine: ISQLEngine;
begin
  if FSQLEngine = nil then
    FSQLEngine := DoGetSQLEngine;

  Result := FSQLEngine;
end;

function TORMConnection.SupportsAutoIncFields: Boolean;
begin
  Result := True;
end;

function TORMConnection.NewCommand(const SQL: string; const Parameters: ISQLParameters; const Execute: TCommandExecutType;
  const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand;
begin
  Result := DoNewCommand(
    SQL,
    Parameters,
    Execute,
    SQLQueryCallbacks)
end;

function TORMConnection.NewDataset(const QueryCriteria: ISQLSelectCriteria;
  const SQLQueryCallbacks: ISQLQueryCallbacks; const SQLDatasetClass: TSQLQueryClass): ISQLDataset;
var
  SQL: String;
  Parameters: ISQLParameters;
begin
  Parameters := TSQLParameters.Create;

  SQLEngine.GenerateSQL(QueryCriteria, SQL, Parameters);

  Result := NewDataset(
    SQL,
    Parameters,
    False,
    SQLQueryCallbacks,
    SQLDatasetClass);

  Result.Open;
end;

function TORMConnection.NewCommand(const SQL: string; const ParamNames: array of String;
  const ParamValues: array of Variant; const Execute: TCommandExecutType;
  const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand;
var
  Params: ISQLParameters;
begin
  Params := TSQLParameters.Create;

  Params.SetParams(ParamNames, ParamValues);

  Result := NewCommand(
    SQL,
    Params,
    Execute,
    SQLQueryCallbacks);
end;

function TORMConnection.NewDataset(const SQL: string; const ParamNames: array of String;
  const ParamValues: array of Variant; const Open: Boolean; const SQLQueryCallbacks: ISQLQueryCallbacks;
  const SQLDatasetClass: TSQLQueryClass): ISQLDataset;
var
  Params: ISQLParameters;
begin
  Params := TSQLParameters.Create;

  Params.SetParams(ParamNames, ParamValues);

  Result := NewDataset(
    SQL,
    Params,
    Open,
    SQLQueryCallbacks,
    SQLDatasetClass);
end;

function TORMConnection.InsertCriteriaToCommandExecuteType(SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean): TCommandExecutType;
begin
  if not Execute then
  begin
    Result := TCommandExecutType.ceNone;
  end
  else
  begin
    if Length(SQLInsertCriteria.GetReturnsList) = 0 then
    begin
      Result := TCommandExecutType.ceExecute;
    end
    else
    begin
      Result := TCommandExecutType.ceOpen;
    end;
  end;
end;

function TORMConnection.UpdateCriteriaToCommandExecuteType(SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean): TCommandExecutType;
begin
  if not Execute then
  begin
    Result := TCommandExecutType.ceNone;
  end
  else
  begin
    Result := TCommandExecutType.ceExecute;
  end;
end;

function TORMConnection.DeleteCriteriaToCommandExecuteType(SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean): TCommandExecutType;
begin
  if not Execute then
  begin
    Result := TCommandExecutType.ceNone;
  end
  else
  begin
    Result := TCommandExecutType.ceExecute;
  end;
end;

function TORMConnection.NewCommand(const SQLUpdateCriteria: ISQLUpdateCriteria; const Execute: Boolean;
  const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand;
var
  Parameters: ISQLParameters;
  SQL: String;
begin
  Parameters := TSQLParameters.Create;

  SQLEngine.GenerateSQL(SQLUpdateCriteria, SQL, Parameters);

  Result := NewCommand(
    SQL,
    Parameters,
    UpdateCriteriaToCommandExecuteType(SQLUpdateCriteria, Execute),
    SQLQueryCallbacks);
end;

function TORMConnection.NewCommand(const SQLInsertCriteria: ISQLInsertCriteria; const Execute: Boolean;
  const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand;
var
  Parameters: ISQLParameters;
  SQL: String;
begin
  Parameters := TSQLParameters.Create;

  SQLEngine.GenerateSQL(SQLInsertCriteria, SQL, Parameters);

  Result := NewCommand(
    SQL,
    Parameters,
    InsertCriteriaToCommandExecuteType(SQLInsertCriteria, Execute),
    SQLQueryCallbacks);
end;

function TORMConnection.NewCommand(const SQLDeleteCriteria: ISQLDeleteCriteria; const Execute: Boolean;
  const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand;
var
  Parameters: ISQLParameters;
  SQL: String;
begin
  Parameters := TSQLParameters.Create;

  SQLEngine.GenerateSQL(SQLDeleteCriteria, SQL, Parameters);

  Result := NewCommand(
    SQL,
    Parameters,
    DeleteCriteriaToCommandExecuteType(SQLDeleteCriteria, Execute),
    SQLQueryCallbacks);
end;

{ TSQLQuery }

constructor TSQLQuery.Create(const SQL: String; const Parameters: ISQLParameters;
  const SQLQueryCallbacks: ISQLQueryCallbacks);
begin
  FSQL := SQL;
  FParameters := Parameters;
  FSQLQueryCallbacks := SQLQueryCallbacks;
end;

procedure TSQLQuery.DoOnAfterCallback(const TickCount: Cardinal);
begin
  if (FSQLQueryCallbacks <> nil) and
     (Assigned(FSQLQueryCallbacks.OnAfterQueryCallback)) then
    FSQLQueryCallbacks.OnAfterQueryCallback(Self, TickCount);
end;

procedure TSQLQuery.DoOnBeforeCallback;
begin
  if (FSQLQueryCallbacks <> nil) and
     (Assigned(FSQLQueryCallbacks.OnBeforeQueryCallback)) then
    FSQLQueryCallbacks.OnBeforeQueryCallback(Self);
end;

procedure TSQLQuery.DoOnExceptionCallback(const e: Exception);
begin
  if (FSQLQueryCallbacks <> nil) and
     (Assigned(FSQLQueryCallbacks.OnExceptionCallback)) then
    FSQLQueryCallbacks.OnExceptionCallback(Self, e);
end;

function TSQLQuery.GetParameters: ISQLParameters;
begin
  Result := FParameters;
end;

function TSQLQuery.GetSQL: String;
begin
  Result := FSQL;
end;

procedure TSQLQuery.SetSQL(const Value: String);
begin
  FSQL := Value;
end;

function TSQLQuery.GetFormattedSQL(const ReplaceParameters: Boolean): String;
var
  i: Integer;
  Value: String;
  VType: Word;
begin
  Result := GetSQL;

  if ReplaceParameters then
  begin
    for i := pred(GetParameters.Count) downto 0 do
    begin
      if Parameters[i].Value = Null then
        Value := StrNull
      else
      begin
        Value := TrimText(VarToStr(Parameters[i].Value));
        VType := VarType(Parameters[i].Value);

        if (VType = varDate) or
           (VType = varString) or
           (VType = varUString) or
           (VType = varBoolean) then
          Value := AnsiQuotedStr(Value, '''')
        else
        begin
          Value := '<..>';
        end;
      end;

      { TODO : This could be done better. Bit hacky and probably won't always work }
      Result := StringReplace(Result, ':' + Parameters[i].Name, Value, [rfReplaceAll]);
    end;
  end;
end;

{ TORMSQLCommand }

function TORMSQLCommand.Execute: Integer;
var
  TickCount: Cardinal;
begin
  Result := 0;

  DoOnBeforeCallback;
  try
    TickCount := TThread.GetTickCount;

    RowsAdded := DoExecute;

    DoOnAfterCallback(TThread.GetTickCount - TickCount);
  except
    on e: Exception do
    begin
      DoOnExceptionCallback(e);

      raise;
    end;
  end;
end;

function TORMSQLCommand.FieldByName(const FieldName: string): TField;
begin
  DoGetFieldByName(FieldName, Result);
end;

function TORMSQLCommand.Fields: TFields;
begin
  DoGetFields(Result);
end;

function TORMSQLCommand.GetRowsAdded: Integer;
begin
  Result := FRowsAdded;
end;

function TORMSQLCommand.Open: Integer;
var
  TickCount: Cardinal;
begin
  Result := 0;

  DoOnBeforeCallback;
  try
    TickCount := TThread.GetTickCount;

    RowsAdded := DoOpen;

    DoOnAfterCallback(TThread.GetTickCount - TickCount);
  except
    on e: Exception do
    begin
      DoOnExceptionCallback(e);

      raise;
    end;
  end;
end;

procedure TORMSQLCommand.SetRowsAdded(const Value: Integer);
begin
  FRowsAdded := Value;
end;

{ TSQLQueryCallbacks }

function TSQLQueryCallbacks.GetOnAfterQueryCallback: TSQLQueryOnAfterCallback;
begin
  Result := FOnAfterQueryCallback;
end;

function TSQLQueryCallbacks.GetOnBeforeQueryCallback: TSQLQueryOnBeforeCallback;
begin
  Result := FOnBeforeQueryCallback;
end;

function TSQLQueryCallbacks.GetOnExceptionCallback: TSQLQueryExceptionCallback;
begin
  Result := FOnExceptionCallback;
end;

procedure TSQLQueryCallbacks.SetOnAfterQueryCallback(const Value: TSQLQueryOnAfterCallback);
begin
  FOnAfterQueryCallback := Value;
end;

procedure TSQLQueryCallbacks.SetOnBeforeQueryCallback(const Value: TSQLQueryOnBeforeCallback);
begin
  FOnBeforeQueryCallback := Value;
end;

procedure TSQLQueryCallbacks.SetOnExceptionCallback(const Value: TSQLQueryExceptionCallback);
begin
  FOnExceptionCallback := Value;
end;

end.
