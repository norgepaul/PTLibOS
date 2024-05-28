unit PTLib.ORM.KeyValueStore;

interface

// LINUX
// Install Postgres client
// sudo apt-get install libpq5
// sudo ln -s /usr/lib/x86_64-linux-gnu/libpq.so.5 /usr/lib/x86_64-linux-gnu/libpq.so

uses
  System.SysUtils, System.Variants, System.DateUtils, System.Generics.Collections,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.KeyValueStore,
  PTLib.Common.Utils,
  PTLib.Common.Log,
  PTLib.Common.Types,

  PTLib.ORM.Interfaces,
  PTLib.ORM.Types,
  PTLib.ORM.Session,
  PTLib.ORM.Factory;

type
  TOnNeedORM = reference to procedure(out ORM: IORM);

  IKeyValueStoreORM = interface(IKeyValueStore)
    ['{EEB02D59-33CB-491F-AE09-51B173E11C1B}']
    function GetNameValues(const NamePrefix: String): TArray<TPair<String, String>>;
    procedure TestDBConnection;
    function NewORMSession: IORMSession;
  end;

  TKeyValueStoreORM = class(TKeyValueStore,
                            IKeyValueStoreORM)
  strict private
    FOnNeedORM: TOnNeedORM;
    FTableName: String;
    FKeyFieldName: String;
    FValueFieldName: String;
    FSelectItemSQL: String;
    FSelectNamesSQL: String;
    FInsertUpdateSQL: String;
    FDeleteSQL: String;
  private
    procedure GenerateSQL;
    procedure OnSQLBeforeRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset);
    procedure OnSQLBeforeWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand);
    procedure OnSQLError(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception);
  protected
    procedure DoSetValue(const Name: String; const Value: String; const KeyValueType: TKeyValueType); override;
    procedure DoGetValue(const Name: String; var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType); override;
    procedure DoValueExists(const Name: String; out Exists: Boolean); override;
    procedure DoRemoveValue(const Name: String); override;

    procedure DoGetNameValues(const NamePrefix: String; out NameValues: TArray<TPair<String, String>>); virtual;

    procedure TestDBConnection;

    function ORM: IORM;
    function NewORMSession: IORMSession;
  public
    constructor Create(const TableName, KeyFieldName, ValueFieldName: String; const OnNeedORM: TOnNeedORM); reintroduce;

    function GetNameValues(const NamePrefix: String): TArray<TPair<String, String>>;
  end;

implementation

{ TKeyValueStoreORM }

constructor TKeyValueStoreORM.Create(const TableName, KeyFieldName, ValueFieldName: String; const OnNeedORM: TOnNeedORM);
begin
  inherited Create;

  // Thread safety is not required as each database request is performed in it's
  // own transaction
  FThreadSafe := False;

  FOnNeedORM := OnNeedORM;
  FTableName := TableName;
  FKeyFieldName := KeyFieldName;
  FValueFieldName := ValueFieldName;

  // Cache all the required SQL so we don't generate it with each request
  GenerateSQL;

  TestDBConnection;
end;

function TKeyValueStoreORM.NewORMSession: IORMSession;
begin
  Result := ORM.NewORMSession(nil, []) as IORMSession;
  Result.Connection.OnSQLBeforeRead := OnSQLBeforeRead;
  Result.Connection.OnSQLBeforeWrite := OnSQLBeforeWrite;
  Result.Connection.OnSQLError := OnSQLError;
end;

procedure TKeyValueStoreORM.OnSQLBeforeRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset);
begin
  TGLobalLog.Log(
    Self,
    'DB Select - %s',
    [SQLDataset.GetFormattedSQL(True)],
    LogSeverityDebug3);
end;

procedure TKeyValueStoreORM.OnSQLBeforeWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand);
begin
  TGLobalLog.Log(
    Self,
    'DB Command - %s',
    [SQLCommand.GetFormattedSQL(True)],
    LogSeverityDebug3);
end;

procedure TKeyValueStoreORM.OnSQLError(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception);
begin
  TGLobalLog.Log(
    Self,
    'DB Error - %s',
    [e.Message],
    LogSeverityError);
end;

procedure TKeyValueStoreORM.GenerateSQL;
var
  SelectItemSQL, SelectNamesSQL: ISQLSelectCriteria;
  InsertUpdateSQL: ISQLInsertCriteria;
  DeleteSQL: ISQLDeleteCriteria;
  Parameters: ISQLParameters;
  ORMSession: IORMSession;
begin
  // Generate the SELECT and UPDATE SQL with the appropriate database syntax
  ORMSession := NewORMSession;

  Parameters := TOFac.Parameters;

  // Create the SQL abstracts
  SelectItemSQL := TOFac.
    Select.
      Field(FValueFieldName).
    From
      (FTableName).
    Where(
      TOFac.Comparitor(FKeyFieldName, TSQLRelationalOperator.EqualTo, ':P1', TSQLValueType.vtFieldReference));

  SelectNamesSQL := TOFac.
    Select.
      Field(FKeyFieldName).
      Field(FValueFieldName).
    From
      (FTableName).
    Where(
      TOFac.Comparitor(FKeyFieldName, TSQLRelationalOperator.Like, ':P1', TSQLValueType.vtFieldReference));

  InsertUpdateSQL := TOFac.
    UpdateOrInsertInto
      (FTableName).
    Value(FKeyFieldName, ':P1', False).
    Value(FValueFieldName, ':P2', False);

  DeleteSQL := TOFac.
    DeleteFrom(FTableName).
    Where(
      TOFac.Comparitor(FKeyFieldName, TSQLRelationalOperator.EqualTo, ':P1', TSQLValueType.vtFieldReference));

  // Convert the abstracts into SQL text
  ORMSession.Connection.SQLConnection.SQLEngine.GenerateSQL(SelectItemSQL, FSelectItemSQL, Parameters);
  ORMSession.Connection.SQLConnection.SQLEngine.GenerateSQL(SelectNamesSQL, FSelectNamesSQL, Parameters);
  ORMSession.Connection.SQLConnection.SQLEngine.GenerateSQL(InsertUpdateSQL, FInsertUpdateSQL, Parameters);
  ORMSession.Connection.SQLConnection.SQLEngine.GenerateSQL(DeleteSQL, FDeleteSQL, Parameters);
end;

function TKeyValueStoreORM.GetNameValues(
  const NamePrefix: String): TArray<TPair<String, String>>;
begin
  DoGetNameValues(NamePrefix, Result);
end;

procedure TKeyValueStoreORM.DoGetValue(const Name: String; var Value: String; out Success: Boolean; const KeyValueType: TKeyValueType);
var
  ORMSession: IORMSession;
  SQLDataset: ISQLDataset;
begin
  ORMSession := NewORMSession;
  try
    ORMSession.Connection.SQLConnection.BeginTransaction;

    SQLDataset := ORMSession.Connection.NewDataset(
      FSelectItemSQL,
      ['P1'],
      [Name]);

    Success := not SQLDataset.IsEmpty;

    if Success then
    begin
      Value := SQLDataset.Fields[0].AsString;
    end;

    ORMSession.Connection.SQLConnection.CommitTransaction;
  except
    on e: Exception do
    begin
      TGLobalLog.Log(
        Self,
        'Get Error: %s',
        [e.Message],
        LogSeverityError);

      ORMSession.Connection.Rollback;
    end;
  end;
end;

procedure TKeyValueStoreORM.DoGetNameValues(const NamePrefix: String;
  out NameValues: TArray<TPair<String, String>>);
var
  ORMSession: IORMSession;
  SQLDataset: ISQLDataset;
  Pair: TPair<String, String>;
begin
  ORMSession := NewORMSession;
  try
    ORMSession.Connection.SQLConnection.BeginTransaction;

    SQLDataset := ORMSession.Connection.NewDataset(
      FSelectNamesSQL,
      ['P1'],
      [NamePrefix + '%']);

    while not SQLDataset.Eof do
    begin
      Pair.Key := SQLDataset.Fields[0].AsString;
      Pair.Value := SQLDataset.Fields[1].AsString;

      NameValues := NameValues + [Pair];
    end;

    ORMSession.Connection.SQLConnection.CommitTransaction;
  except
    on e: Exception do
    begin
      ORMSession.Connection.Rollback;
    end;
  end;
end;

procedure TKeyValueStoreORM.DoRemoveValue(const Name: String);
var
  ORMSession: IORMSession;
begin
  ORMSession := NewORMSession;
  try
    ORMSession.Connection.SQLConnection.BeginTransaction;

    ORMSession.Connection.NewCommand(
      FDeleteSQL,
      ['P1'],
      [Name]);

    ORMSession.Connection.SQLConnection.CommitTransaction;
  except
    on e: Exception do
    begin
      TGLobalLog.Log(
        Self,
        'Remove Error: %s',
        [e.Message],
        LogSeverityError);

      ORMSession.Connection.Rollback;
    end;
  end;
end;

procedure TKeyValueStoreORM.DoSetValue(const Name: String; const Value: String; const KeyValueType: TKeyValueType);
var
  ORMSession: IORMSession;
begin
  ORMSession := NewORMSession;
  try
    ORMSession.Connection.SQLConnection.BeginTransaction;

    ORMSession.Connection.NewCommand(
      FInsertUpdateSQL,
      ['P1', 'P2'],
      [Name, Value]);

    ORMSession.Connection.SQLConnection.CommitTransaction;
  except
    on e: Exception do
    begin
      TGLobalLog.Log(
        Self,
        'Set Error: %s',
        [e.Message],
        LogSeverityError);

      ORMSession.Connection.Rollback;
    end;
  end;
end;

procedure TKeyValueStoreORM.DoValueExists(const Name: String; out Exists: Boolean);
var
  Dummy: String;
begin
  DoGetValue(Name, Dummy, Exists, TKeyValueType.kvtUnknown);
end;

function TKeyValueStoreORM.ORM: IORM;
begin
  FOnNeedORM(Result);
end;

procedure TKeyValueStoreORM.TestDBConnection;
var
  Success: Boolean;
  Value: String;
begin
  DoGetValue('', Value, Success, TKeyValueType.kvtUnknown);
end;

end.
