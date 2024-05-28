{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.SQL.Connection.FireDAC                         }
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

unit PTLib.ORM.SQL.Connection.FireDAC;

interface

uses
  Classes, DB, SysUtils, Generics.Collections, SyncObjs,

  FireDAC.Comp.Client,
  FireDAC.Comp.Dataset,
  FireDAC.Stan.Def,
  FireDAC.Stan.Param,
  FireDAC.Stan.Option,
  FireDAC.Stan.ASync,
  FireDAC.Stan.Intf,
  FireDAC.DApt,
  FireDAC.Phys.Intf,

  PTLib.Common.Interfaces,

  PTLib.ORM,
  PTLib.ORM.Session,
  PTLib.ORM.SQL.Connection,
  PTLib.ORM.Classes,
  PTLib.ORM.SQL.Criteria,
  PTLib.ORM.Types,
  PTLib.ORM.Factory,
  PTLib.ORM.Interfaces;

type
  TBaseFireDACSQLDataset = class(TSQLDataset)
  protected
    FFDConnection: TFDConnection;
  private
    function DatasetStreamTypeToFireDACDatastreamType(const DatasetStreamType: TDatasetStreamType): TFDStorageFormat;
    function DatasetStoreItemsToFireDACStoreItems(const DatasetStoreItems: TDatasetStoreItems): TFDStoreItems;
  protected
    procedure DoToJSONString(out Value: String; const IncludeMetaData: Boolean); override;
  public
    function NativeDataset: IInterface; override;
  end;

  TFireDACSQLDataset = class(TBaseFireDACSQLDataset)
  strict private
    FFDDataset: TFDQuery;
  protected
    procedure DoOpen; override;
  public
    destructor Destroy; override;

    procedure LoadFromStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto); override;
    procedure SaveToStream(const AStream: TStream; const DatasetStoreItems: TDatasetStoreItems; const DatasetStreamType: TDatasetStreamType = dssAuto); override;
    function Dataset: TDataset; override;
  end;

  TFireDACMemDataset = class(TBaseFireDACSQLDataset)
  strict private
    FFDDataset: TFDMemTable;
  protected
    FUnidirectional: Boolean;

    procedure DoOpen; override;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    procedure LoadFromStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto); override;
    procedure SaveToStream(const AStream: TStream; const DatasetStoreItems: TDatasetStoreItems; const DatasetStreamType: TDatasetStreamType = dssAuto); override;
    function Dataset: TDataset; override;
  end;

  TFireDACUnidirectionalMemDataset = class(TFireDACMemDataset)
  public
    constructor Create; override;
  end;

  TFireDACSQLCommand = class(TORMSQLCommand)
  protected
    FFDDataset: TFDQuery;
    FFDConnection: TFDConnection;

    function DoExecute: Integer; overload; override;
    function DoOpen: Integer; override;
    procedure DoGetFieldByName(const FieldName: String; out Field: TField); override;
    procedure DoGetFields(out Fields: TFields); override;
  public
    destructor Destroy; override;

    function NativeSQLCommand: IInterface; override;
  end;

  TFireDACSQLConnection = class(TORMConnection,
                                IFieldIDGenerator)
  private
    FFDConnection: TFDConnection;
    FFDTransaction: TFDTransaction;
    FAttachementID: Cardinal;
  protected
    procedure Connect; virtual;
  protected
    function DoNewCommand(const SQL: string; const Parameters: ISQLParameters; const Execute: TCommandExecutType;
      const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand; overload; override;
    function DoNewDataset(const SQL: string; const Parameters: ISQLParameters; const Open: Boolean;
      const SQLQueryCallbacks: ISQLQueryCallbacks; const SQLDatasetClass: TSQLQueryClass): ISQLDataset; override;
    procedure GetTableNames(const TableNames: TStrings); override;
    procedure GetTableFields(const TableName: String; out Fields: IORMList<IORMField>); override;
    procedure BeginTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function InTransaction: Boolean; override;
    function NativeConnection: TObject; override;
    function NativeTransaction: TObject; override;
    procedure SetTransactionIsolation(const Value: TSQLTransactionIsolation); override;
    procedure SetTransactionReadOnly(const Value: Boolean); override;
    function GetAttachmentID: Cardinal; override;
    procedure CancelConnectionSQLCommands(const ConnectionID: Cardinal = 0); override;
    function SupportsAutoIncFields: Boolean; override;
    function GetLastAutoIncValue(const Name: String = ''): Variant; override;
    function GetDatabaseProperties: IParameters; override;
    function KillLongRunningTransactions(const TransactionAgeSeconds: Integer): Boolean; override;

    // IFieldIDGenerator
    function GetGeneratorValue(const GeneratorName: String): Variant; virtual;

    function DoGetSQLEngine: ISQLEngine; override;
  public
    constructor Create(const ConnectionString: String); override;
    destructor Destroy; override;
  end;

  TORMFireDACConnectionProvider = class(TORMConnectionProvider)
  private
    FFireDACConnection: TFDCustomConnection;

    procedure SetFireDACConnection(const Value: TFDCustomConnection);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function NewConnection: ISQLConnection; override;

    class function GetPostGresConnectionParameters(const Server, Database, Username, Password: String; const Port: Integer): String;
  published
    property FireDACConnection: TFDCustomConnection read FFireDACConnection write SetFireDACConnection;
  end;

const
  DBFirebird = 'FB';
  DBMySQL = 'MySQL';
  DBPostGres = 'PG';

implementation

uses
  System.Variants,

  PTLib.ORM.SQL.Engine,
  PTLib.ORM.SQL.Engine.Firebird,
  PTLib.ORM.SQL.Engine.MySQL,
  PTLib.ORM.SQL.Engine.PostGres;

const
  BaseConnectionString = 'DriverID=%s'#10'Server=%s'#10'Port=%d'#10'Database=%s'#10'User_Name=%s'#10'Password=%s';

procedure AssignFireDACParams(const SQLParameters: ISQLParameters;
  const FDParams: TFDParams; const ConvertBooleans: Boolean);
var
  i: Integer;
  Param: TFDParam;
begin
  if SQLParameters <> nil then
  begin
    for i := 0 to pred(SQLParameters.Count) do
    begin
      Param := FDParams.FindParam(SQLParameters[i].Name);

      if Param = nil then
      begin
        Param := FDParams.Add;
        Param.ParamType := ptInput;
        Param.Name := SQLParameters[i].Name;
      end;

      if SQLParameters[i].FieldType <> ftUnknown then
      begin
        Param.DataType := SQLParameters[i].FieldType;
      end
      else
      begin
        // Fix for MySQL
        if VarIsClear(SQLParameters[i].Value) then
        begin
          Param.DataType := TFieldType.ftLargeint;
        end else

        if SQLParameters[i].Value = Null then
        begin
          Param.DataType := TFieldType.ftVariant;
        end
        else
        begin
          Param.DataType := VarTypeToDataType(VarType(SQLParameters[i].Value));
        end;
      end;
      
      if SQLParameters[i].ValueStream <> nil then
      begin
        if SQLParameters[i].ValueStream.Size <> 0 then
        begin
          Param.SetBlobRawData(SQLParameters[i].ValueStream.Size, SQLParameters[i].ValueStreamAsMemoryStream.Memory);
        end
        else
        begin
          Param.Value := Null;
        end;
      end
      else
      begin
        if (ConvertBooleans) and
           (Param.DataType = ftBoolean) then
        begin
          Param.DataType := TFieldType.ftString;

          if SQLParameters[i].Value then
          begin
            Param.Value := 'T';
          end
          else
          begin
            Param.Value := 'F';
          end;
        end
        else
        begin
          Param.Value := SQLParameters[i].Value;
        end;
      end;
    end;
  end;
end;

procedure TFireDACSQLDataset.LoadFromStream(const AStream: TStream; const DatasetStreamType: TDatasetStreamType = dssAuto);
begin
  FFDDataset.LoadFromStream(AStream, DatasetStreamTypeToFireDACDatastreamType(DatasetStreamType));
end;

procedure TFireDACSQLDataset.SaveToStream(const AStream: TStream; const DatasetStoreItems: TDatasetStoreItems; const DatasetStreamType: TDatasetStreamType = dssAuto);
begin
  FFDDataset.ResourceOptions.StoreItems := DatasetStoreItemsToFireDACStoreItems(DatasetStoreItems);
  FFDDataset.SaveToStream(AStream, DatasetStreamTypeToFireDACDatastreamType(DatasetStreamType));
end;

procedure TFireDACSQLDataset.DoOpen;
begin
  inherited;

  if FFDDataset = nil then
  begin
    FFDDataset := TFDQuery.Create(nil);
    FFDDataset.Connection := FFDConnection;
    FFDDataset.FetchOptions.RecordCountMode := cmFetched;
    FFDDataset.FetchOptions.Cache := FFDDataset.FetchOptions.Cache - [fiMeta];
  end;

  FFDDataset.Active := False;

  FFDDataset.SQL.Text := SQL;
  AssignFireDACParams(Parameters, FFDDataset.Params, FFDConnection.DriverName <> DBPostGres);

  FFDDataset.Active := True;
end;

function TFireDACSQLDataset.Dataset: TDataset;
begin
  Result := FFDDataset;
end;

destructor TFireDACSQLDataset.Destroy;
begin
  if FFDDataset <> nil then
  begin
    FFDDataset.Close;

    FreeAndNil(FFDDataset);
  end;

  inherited;
end;

{ TBaseFireDACSQLDataset }

function TBaseFireDACSQLDataset.DatasetStoreItemsToFireDACStoreItems(
  const DatasetStoreItems: TDatasetStoreItems): TFDStoreItems;
var
  StoreItem: TDatasetStoreItem;
begin
  Result := [];

  for StoreItem in DatasetStoreItems do
  begin
    case StoreItem of
      diMeta: Result := Result + [TFDStoreItem.siMeta];
      diData: Result := Result + [TFDStoreItem.siData];
      diDelta: Result := Result + [TFDStoreItem.siDelta];
      diVisible: Result := Result + [TFDStoreItem.siVisible];
    end;
  end;
end;

function TBaseFireDACSQLDataset.DatasetStreamTypeToFireDACDatastreamType(const DatasetStreamType: TDatasetStreamType): TFDStorageFormat;
begin
  case DatasetStreamType of
    dssXML: Result := sfXML;
    dssBinary: Result := sfBinary;
    dssJSON: Result := sfJSON;
  else
    Result := sfAuto;
  end;
end;

function TBaseFireDACSQLDataset.NativeDataset: IInterface;
begin
  Result := nil;
end;

procedure TBaseFireDACSQLDataset.DoToJSONString(out Value: String; const IncludeMetaData: Boolean);
var
  StringStream: TStringStream;
  DatasetStoreItems: TDatasetStoreItems;
begin
  inherited;

  StringStream := TStringStream.Create;
  try
    DatasetStoreItems := [diData];

    if IncludeMetaData then
    begin
      DatasetStoreItems := DatasetStoreItems + [diMeta];
    end;

    SaveToStream(
      StringStream,
      DatasetStoreItems,
      TDatasetStreamType.dssJSON);

    Value := StringStream.DataString;
  finally
    FreeAndNil(StringStream);
  end;
end;

{ TFireDACSQLCommand }

destructor TFireDACSQLCommand.Destroy;
begin
  FreeAndNil(FFDDataset);

  inherited;
end;

function TFireDACSQLCommand.DoExecute: Integer;
begin
  if FFDDataset <> nil then
  begin
    FFDDataset.Free;
  end;

  FFDDataset := TFDQuery.Create(nil);
  FFDDataset.Connection := FFDConnection;
  FFDDataset.SQL.Text := SQL;

  AssignFireDACParams(Parameters, FFDDataset.Params, FFDConnection.DriverName <> DBPostGres);

  FFDDataset.ExecSQL;

  Result := FFDDataset.RowsAffected;
end;

procedure TFireDACSQLCommand.DoGetFieldByName(const FieldName: String; out Field: TField);
begin
  Field := FFDDataset.FieldByName(FieldName);
end;

procedure TFireDACSQLCommand.DoGetFields(out Fields: TFields);
begin
  Fields := FFDDataset.Fields;
end;

function TFireDACSQLCommand.DoOpen: Integer;
begin
  if FFDDataset <> nil then
  begin
    FFDDataset.Free;
  end;

  FFDDataset := TFDQuery.Create(nil);
  FFDDataset.Connection := FFDConnection;
  FFDDataset.SQL.Text := SQL;

  AssignFireDACParams(Parameters, FFDDataset.Params, FFDConnection.DriverName <> DBPostGres);

  FFDDataset.Open;

  Result := FFDDataset.RowsAffected;
end;

function TFireDACSQLCommand.NativeSQLCommand: IInterface;
begin

end;

{ TFireDACSQLConnection }

procedure TFireDACSQLConnection.BeginTransaction;
begin
  inherited;

  FFDTransaction.StartTransaction;
end;

procedure TFireDACSQLConnection.CancelConnectionSQLCommands(const ConnectionID: Cardinal);
var
  SQLDeleteCriteria: ISQLDeleteCriteria;
begin
  if FFDConnection.DriverName = DBFirebird then
  begin
    // Firebird
    SQLDeleteCriteria := TOFac.
      DeleteFrom
        ('MON$STATEMENTS');

    if ConnectionID = 0 then
    begin
      SQLDeleteCriteria.
        Where
          (TOFac.Comparitor
          ('MON$ATTACHMENT_ID', NotEqualTo, 'CURRENT_CONNECTION', TSQLValueType.vtFieldReference));
    end
    else
    begin
      SQLDeleteCriteria.
        Where
          (TOFac.Comparitor
          ('MON$ATTACHMENT_ID', EqualTo, ConnectionID));
    end;
  end else

  if FFDConnection.DriverName = DBPostGres then
  begin
    NewDataset(
      TOFac.Select.
          Field
            (format('pg_terminate_backend(%d)', [ConnectionID]))).Open;
  end else

  if FFDConnection.DriverName = DBMySQL then
  begin
    // KILL QUERY
    (*SQLDataset := NewDataset(
        TOFac.Select.
          Field
            ('CONNECTION_ID()'));

      FAttachementID := SQLDataset.Fields[0].AsLargeInt;
    end;*)
  end
  else
    inherited;
end;

procedure TFireDACSQLConnection.CommitTransaction;
begin
  inherited;

  if InTransaction then
    FFDTransaction.Commit;
end;

procedure TFireDACSQLConnection.Connect;
begin
  FAttachementID := 0;

  FFDConnection.Connected := True;
end;

constructor TFireDACSQLConnection.Create(const ConnectionString: String);
begin
  inherited;

  FFDConnection := TFDConnection.Create(nil);
  FFDConnection.Params.Text := ConnectionString;

  FFDTransaction := TFDTransaction.Create(nil);
  FFDTransaction.Options.ReadOnly := False;

  FFDConnection.Transaction := FFDTransaction;

  TransactionIsolation := tiReadCommitted;
end;

destructor TFireDACSQLConnection.Destroy;
begin
  FreeAndNil(FFDTransaction);
  FreeAndNil(FFDConnection);

  inherited;
end;

function TFireDACSQLConnection.DoGetSQLEngine: ISQLEngine;
begin
  if FFDConnection.DriverName = DBFirebird then
  begin
    Result := TORMSQLEngineFirebird.Create;
  end else
  if FFDConnection.DriverName = DBMySQL then
  begin
    Result := TORMSQLEngineMySQL.Create;
  end
  else
  if FFDConnection.DriverName = DBPostGres then
  begin
    Result := TORMSQLEnginePostGres.Create;
  end
  else
  begin
    Result := TBaseORMSQLEngine.Create;
  end;
end;

function TFireDACSQLConnection.GetAttachmentID: Cardinal;
var
  SQLDataset: ISQLDataset;
begin
  if FAttachementID = 0 then
  begin
    if FFDConnection.DriverName = DBFirebird then
    begin
      SQLDataset := NewDataset(
        TOFac.Select.
          Field
            ('CURRENT_CONNECTION').
          From
            ('MON$DATABASE'));

      FAttachementID := SQLDataset.Fields[0].AsLargeInt;
    end else
    if FFDConnection.DriverName = DBMySQL then
    begin
      SQLDataset := NewDataset(
        TOFac.Select.
          Field
            ('CONNECTION_ID()'));

      FAttachementID := SQLDataset.Fields[0].AsLargeInt;
    end else
    if FFDConnection.DriverName = DBPostGres then
    begin
      SQLDataset := NewDataset(
        TOFac.Select.
          Field
            ('pg_backend_pid()'));

      FAttachementID := SQLDataset.Fields[0].AsLargeInt;
    end
    else
    begin
      raise EORMGeneratorNotSupportedError.Create('The database type does not support attachment IDs');
    end;
  end;

  Result := FAttachementID;
end;

function TFireDACSQLConnection.GetDatabaseProperties: IParameters;
var
  SQLDataset: ISQLDataset;
begin
  Result := inherited;

  if AnsiSameStr(FFDConnection.DriverName, DBFirebird) then
  begin
    SQLDataset :=
      NewDataset(TOFac.
        Select.
          Field('MON$DATABASE_NAME').
          Field('MON$PAGE_SIZE').
          Field('MON$PAGES').
          Field('MON$ODS_MAJOR').
          Field('MON$ODS_MINOR').
        From
          ('MON$DATABASE'));

    Result.SetParam('Database Name', SQLDataset.Fields[0].AsString);
    Result.SetParam('Page Size', SQLDataset.Fields[1].AsString);
    Result.SetParam('Pages', SQLDataset.Fields[2].AsString);
    Result.SetParam('ODS Major', SQLDataset.Fields[3].AsString);
    Result.SetParam('ODS Minor', SQLDataset.Fields[4].AsString);

    SQLDataset :=
      NewDataset(TOFac.
        Select.
          First(1).
          Field('MON$TOP_TRANSACTION').
          Field('MON$OLDEST_TRANSACTION').
          Field('MON$OLDEST_ACTIVE').
          Field('MON$TRANSACTION_ID').
        From
          ('MON$TRANSACTIONS').
        OrderBy
          (TOFac.OrderBy.Add
          ('MON$TRANSACTION_ID', Descending)));

    Result.SetParam('Top Transaction', SQLDataset.Fields[0].AsString);
    Result.SetParam('Oldest Transaction', SQLDataset.Fields[1].AsString);
    Result.SetParam('Oldest Active Transaction', SQLDataset.Fields[2].AsString);
    Result.SetParam('Next Transaction', SQLDataset.Fields[3].AsString);
  end else

  if AnsiSameStr(FFDConnection.DriverName, DBMySQL) then
  begin

  end;
end;

function TFireDACSQLConnection.GetGeneratorValue(const GeneratorName: String): Variant;
var
  SQLDataset: ISQLDataset;
begin
  if FFDConnection.DriverName = DBFirebird then
  begin
    SQLDataset := NewDataset(
      TOFac.Select.
        Field
          (format('GEN_ID(%s, 1)', [GeneratorName])).
        From
          ('RDB$DATABASE'));

    Result := SQLDataset.Fields[0].AsVariant;
  end else
  if FFDConnection.DriverName = DBPostGres then
  begin
    SQLDataset := NewDataset(
      TOFac.Select.
        Field
          (format('NEXTVAL(''%s'')', [AnsiLowerCase(GeneratorName)])));

    Result := SQLDataset.Fields[0].AsVariant;
  end
  else
  begin
    raise EORMAttachmentIDNotSupportedError.Create('The database type does not support generators');
  end;
end;

function TFireDACSQLConnection.GetLastAutoIncValue(const Name: String): Variant;
var
  FDDataset: TFDQuery;
begin
  if SupportsAutoIncFields then
  begin
    if AnsiSameStr(FFDConnection.DriverName, DBMySQL) then
    begin
      FDDataset := TFDQuery.Create(nil);
      try
        FDDataset.Connection := FFDConnection;
        FDDataset.SQL.Text := 'SELECT LAST_INSERT_ID()';
        FDDataset.Active := True;

        Result := FDDataset.Fields[0].AsLargeInt;
      finally
        FreeAndNil(FDDataset);
      end;
    end else

    if AnsiSameStr(FFDConnection.DriverName, DBPostGres) then
    begin
      FDDataset := TFDQuery.Create(nil);
      try
        FDDataset.Connection := FFDConnection;

        if Name = '' then
        begin
          FDDataset.SQL.Text := 'SELECT LASTVAL()';
        end
        else
        begin
          FDDataset.SQL.Text := 'SELECT CURRVAL(''' + Name + ''')';
        end;
        FDDataset.Active := True;

        Result := FDDataset.Fields[0].AsLargeInt;
      finally
        FreeAndNil(FDDataset);
      end;
    end;
  end
  else
  begin
    Result := inherited;
  end;
end;

procedure TFireDACSQLConnection.GetTableFields(const TableName: String;
  out Fields: IORMList<IORMField>);
var
  SQLDataset: ISQLDataset;
  Field: IORMField;
  i: Integer;
begin
  SQLDataset := NewDataset(
    TOFac.Select.
      First(0).
      Field('*').
    From
      (TableName));

  Fields := TORMList<IORMField>.Create;

  for i := 0 to pred(SQLDataset.Fields.Count) do
  begin
    Field := TORMField.Create;

    Field.FieldName := SQLDataset.Fields[i].FieldName;
    Field.Calculated := SQLDataset.Fields[i].Calculated;
    Field.CanModify := SQLDataset.Fields[i].CanModify;
    Field.LifeCycle := SQLDataset.Fields[i].LifeCycle;
    Field.DataSize := SQLDataset.Fields[i].DataSize;
    Field.DataType := SQLDataset.Fields[i].DataType;
    Field.DisplayName := SQLDataset.Fields[i].DisplayName;
    Field.FieldNo := SQLDataset.Fields[i].FieldNo;
    Field.FullName := SQLDataset.Fields[i].FullName;
    Field.IsIndexField := SQLDataset.Fields[i].IsIndexField;
    Field.Offset := SQLDataset.Fields[i].Offset;
    Field.Size := SQLDataset.Fields[i].Size;
    Field.Alignment := SQLDataset.Fields[i].Alignment;
    Field.AutoGenerateValue := SQLDataset.Fields[i].AutoGenerateValue;
    Field.CustomConstraint := SQLDataset.Fields[i].CustomConstraint;
    Field.ConstraintErrorMessage := SQLDataset.Fields[i].ConstraintErrorMessage;
    Field.DefaultExpression := SQLDataset.Fields[i].DefaultExpression;
    Field.DisplayLabel := SQLDataset.Fields[i].DisplayLabel;
    Field.DisplayWidth := SQLDataset.Fields[i].DisplayWidth;
    Field.FieldKind := SQLDataset.Fields[i].FieldKind;

    Fields.Add(Field);
  end;
end;

procedure TFireDACSQLConnection.GetTableNames(const TableNames: TStrings);
begin
  FFDConnection.GetTableNames(
    '',
    '',
    '',
    TableNames,
    [osMy],
    [tkTable]);
end;

function TFireDACSQLConnection.InTransaction: Boolean;
begin
  Result := FFDTransaction.Active;
end;

function TFireDACSQLConnection.KillLongRunningTransactions(
  const TransactionAgeSeconds: Integer): Boolean;
begin
  Result := False;

  if AnsiSameStr(FFDConnection.DriverName, DBFirebird) then
  begin
    NewCommand(TOFac.
      DeleteFrom('MON$STATEMENTS').
      Where(TOFac.Comparitor(
        'CURRENT_TIMESTAMP',
        GreaterThan,
        format('DATEADD(second, %d, MON$TIMESTAMP)', [TransactionAgeSeconds]),
        TSQLValueType.vtValue)));

    CommitTransaction;
  end;
end;

function TFireDACSQLConnection.NativeConnection: TObject;
begin
  Result := FFDConnection;
end;

function TFireDACSQLConnection.NativeTransaction: TObject;
begin
  Result := FFDTransaction;
end;

function TFireDACSQLConnection.DoNewCommand(const SQL: string; const Parameters: ISQLParameters; const Execute: TCommandExecutType;
      const SQLQueryCallbacks: ISQLQueryCallbacks): ISQLCommand;
begin
  Result := TFireDACSQLCommand.Create(
    SQL,
    Parameters,
    SQLQueryCallbacks);

  TFireDACSQLCommand(Result).FFDConnection := FFDConnection;

  case Execute of
    ceExecute: Result.Execute;
    ceOpen: Result.Open;
  end
end;

function TFireDACSQLConnection.DoNewDataset(const SQL: string;
  const Parameters: ISQLParameters; const Open: Boolean;
  const SQLQueryCallbacks: ISQLQueryCallbacks; const SQLDatasetClass: TSQLQueryClass): ISQLDataset;
var
  RealDatasetClass: TSQLQueryClass;
begin
  inherited;

  if SQLDatasetClass = nil then
  begin
    RealDatasetClass := TFireDACSQLDataset;
  end
  else
  begin
    RealDatasetClass := SQLDatasetClass;
  end;

  Result := TSQLDatasetClass(RealDatasetClass).Create(
    SQL,
    Parameters,
    SQLQueryCallbacks);

  TBaseFireDACSQLDataset(Result).FFDConnection := FFDConnection;

  if Open then
    Result.Open;
end;

procedure TFireDACSQLConnection.RollbackTransaction;
begin
  inherited;

  if InTransaction then
    FFDTransaction.Rollback;
end;

procedure TFireDACSQLConnection.SetTransactionIsolation(const Value: TSQLTransactionIsolation);
begin
  inherited;

  case Value of
    tiUnspecified: FFDTransaction.Options.Isolation := xiUnspecified;
    tiDirtyRead: FFDTransaction.Options.Isolation := xiDirtyRead;
    tiReadCommitted: FFDTransaction.Options.Isolation := xiReadCommitted;
    tiRepeatableRead: FFDTransaction.Options.Isolation := xiRepeatableRead;
    tiSnapshot: FFDTransaction.Options.Isolation := xiSnapshot;
    tiSerializible: FFDTransaction.Options.Isolation := xiSerializible;
  end;
end;

procedure TFireDACSQLConnection.SetTransactionReadOnly(const Value: Boolean);
begin
  inherited;

  FFDTransaction.Options.ReadOnly := Value;
end;

function TFireDACSQLConnection.SupportsAutoIncFields: Boolean;
begin
  Result :=
    (not SameText(FFDConnection.DriverName, DBFirebird));
end;

{ TORMFireDACConnectionProvider }

constructor TORMFireDACConnectionProvider.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TORMFireDACConnectionProvider.Destroy;
begin

  inherited;
end;

class function TORMFireDACConnectionProvider.GetPostGresConnectionParameters(const Server, Database, Username, Password: String; const Port: Integer): String;
begin
  Result := format(BaseConnectionString,
    ['PG',
     Server,
     Port,
     Database,
     Username,
     Password]);
end;

function TORMFireDACConnectionProvider.NewConnection: ISQLConnection;
begin
  Assert(FFireDACConnection <> nil, 'Please set a FireDACConnection');

  Result := TFireDACSQLConnection.Create(FFireDACConnection.Params.Text);
end;

procedure TORMFireDACConnectionProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and
     (AComponent = FFireDACConnection) then
  begin
    FireDACConnection := nil;
  end;
end;

procedure TORMFireDACConnectionProvider.SetFireDACConnection(
  const Value: TFDCustomConnection);
begin
  FFireDACConnection := Value;
end;

{ TFireDACMemDataset }

constructor TFireDACMemDataset.Create;
begin
  inherited Create('', nil, nil);

  FUnidirectional := False;
end;

function TFireDACMemDataset.Dataset: TDataset;
begin
  Result := FFDDataset;
end;

destructor TFireDACMemDataset.Destroy;
begin
  FreeAndNil(FFDDataset);

  inherited;
end;

procedure TFireDACMemDataset.DoOpen;
var
  TempDataset: TFDQuery;
  MemStream: TMemoryStream;
begin
  inherited;

  TempDataset := TFDQuery.Create(nil);
  try
    TempDataset.Connection := FFDConnection;
    TempDataset.FetchOptions.RecordCountMode := cmFetched;
    TempDataset.FetchOptions.Cache := TempDataset.FetchOptions.Cache - [fiMeta];
    TempDataset.FetchOptions.Unidirectional := FUnidirectional;

    TempDataset.SQL.Text := SQL;
    AssignFireDACParams(Parameters, TempDataset.Params, FFDConnection.DriverName <> DBPostGres);

    TempDataset.Active := True;

    MemStream := TMemoryStream.Create;
    try
      TempDataset.SaveToStream(MemStream);
      MemStream.Position := 0;

      LoadFromStream(MemStream);
    finally
      FreeAndNil(MemStream);
    end;
  finally
    FreeAndNil(TempDataset);
  end;
end;

procedure TFireDACMemDataset.LoadFromStream(const AStream: TStream;
  const DatasetStreamType: TDatasetStreamType);
begin
  if FFDDataset = nil then
  begin
    FFDDataset := TFDMemTable.Create(nil);
  end;

  AStream.Position := 0;
  FFDDataset.LoadFromStream(AStream, DatasetStreamTypeToFireDACDatastreamType(DatasetStreamType));
end;

procedure TFireDACMemDataset.SaveToStream(const AStream: TStream;
  const DatasetStoreItems: TDatasetStoreItems;
  const DatasetStreamType: TDatasetStreamType);
begin
  FFDDataset.ResourceOptions.StoreItems := DatasetStoreItemsToFireDACStoreItems(DatasetStoreItems);
  FFDDataset.SaveToStream(AStream, DatasetStreamTypeToFireDACDatastreamType(DatasetStreamType));
end;

{ TFireDACUnidirectionalMemDataset }

constructor TFireDACUnidirectionalMemDataset.Create;
begin
  inherited;

  FUnidirectional := True;
end;

end.
