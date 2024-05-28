unit PTLib.ORM.Connection.RODataAbstract;

interface

uses
  SysUtils, DB, TypInfo, RTTI, Generics.Collections,

  uROTypes, uROClasses,

  PTLib.ORM,
  PTLib.ORM.Session,
  PTLib.ORM.Classes,
  PTLib.ORM.SQL.Criteria,
  PTLib.ORM.Interfaces,
  PTLib.ORM.Connection;

type
  TDAConnectionHelper = class helper for TDAEConnection
  public
    procedure Releasing;
  end;

  TRODASQLDataset = class(TSQLDataset)
  protected
    FDADataset: IDADataset;
    FDAConnection: IDAConnection;
  public
    function Dataset: TDataset; override;
    function NativeDataset: IInterface; override;
    procedure Open; override;
  end;

  TRODAORMCommand = class(TORMSQLCommand)
  protected
    FDASQLCommand: IDASQLCommand;
    FDAConnection: IDAConnection;
  public
    function Execute: Integer; override;
    function NativeSQLCommand: IInterface; override;
  end;

  TRODAORMConnection = class(TORMConnection)
  private
    FDAConnection: IDAConnection;

    function GetDAConnection: IDAConnection;
  protected
    function NewCommand(const SQL: string; const Parameters: ISQLParameters; const Execute: Boolean = True): ISQLCommand; override;
    function NewDataset(const SQL: string; const Parameters: ISQLParameters; const Open: Boolean = True): ISQLDataset; override;
    function GetGeneratorValue(const GeneratorName: String): Int64; override;
    procedure BeginTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    function InTransaction: Boolean; override;
    function NativeConnection: TObject; override;

    property DAConnection: IDAConnection read GetDAConnection;
  public
    constructor Create(const ConnectionString: String); overload; override;
    constructor Create(const DAConnection: IDAConnection); overload;
    destructor Destroy; override;
  end;

implementation

resourcestring
  StrTheNativeConnection = 'The Native Connection does not support IDAConnection';

procedure AssignDAParams(const ORMParameters: ISQLParameters;
  const DAParamCollection: TDAParamCollection);
var
  i: Integer;
  DAParam: TDAParam;
  ROStream: IROStream;
begin
  if ORMParameters <> nil then
  begin
    for i := 0 to pred(ORMParameters.Count) do
    begin
      DAParam := DAParamCollection.FindParam(ORMParameters[i].Name);

      if DAParam = nil then
      begin
        DAParam := DAParamCollection.Add;
        DAParam.ParamType := daptInput;
        DAParam.Name := ORMParameters[i].Name;
      end;

      if ORMParameters[i].ValueStream <> nil then
      begin
        ORMParameters[i].ValueStream.Position := 0;

        ROStream := TROStream.Create(ORMParameters[i].ValueStream, False);

        DAParam.LoadFromStream(ROStream);
      end
      else
      begin
        DAParam.Value := ORMParameters[i].Value;
      end;
    end;
  end;
end;

{ TRODAORMConnection }

procedure TRODAORMConnection.BeginTransaction;
begin
  inherited;

  if not DAConnection.InTransaction then
    DAConnection.BeginTransaction;
end;

procedure TRODAORMConnection.CommitTransaction;
begin
  inherited;

  if DAConnection.InTransaction then
    DAConnection.CommitTransaction;
end;

constructor TRODAORMConnection.Create(const DAConnection: IDAConnection);
begin
  inherited Create('');

  FDAConnection := DAConnection;
end;

constructor TRODAORMConnection.Create(
  const ConnectionString: String);
begin
  inherited;
end;

destructor TRODAORMConnection.Destroy;
begin
  if DAConnection <> nil then
    TDAEConnection(DAConnection).Releasing;

  inherited;
end;

function TRODAORMConnection.GetDAConnection: IDAConnection;
begin
  Result := FDAConnection;
end;

function TRODAORMConnection.GetGeneratorValue(
  const GeneratorName: String): Int64;
const
  FirebirdGeneratorSQL = 'SELECT GEN_ID(%s, 1) FROM RDB$DATABASE';
var
  DADataset: IDADataset;
begin
  DADataset := DAConnection.NewDataset(format(FirebirdGeneratorSQL, [GeneratorName]));
  DADataset.Open;

  Result := DADataset.Fields[0].AsLargeInt;
end;

function TRODAORMConnection.InTransaction: Boolean;
begin
  Result := DAConnection.InTransaction;
end;

function TRODAORMConnection.NativeConnection: TObject;
begin
  Result := nil; //
end;

function TRODAORMConnection.NewCommand(const SQL: string;
  const Parameters: ISQLParameters; const Execute: Boolean): ISQLCommand;
begin
  Result := TRODAORMCommand.Create(SQL, Parameters);
  TRODAORMCommand(Result).FDAConnection := DAConnection;

  if Execute then
    Result.Execute
end;

function TRODAORMConnection.NewDataset(const SQL: string;
  const Parameters: ISQLParameters; const Open: Boolean): ISQLDataset;
begin
  inherited;

  Result := TRODASQLDataset.Create(SQL, Parameters);
  TRODASQLDataset(Result).FDAConnection := DAConnection;

  if Open then
    Result.Open;
end;

procedure TRODAORMConnection.RollbackTransaction;
begin
  inherited;

  if DAConnection.InTransaction then
    DAConnection.RollbackTransaction;
end;

{ TRODASQLDataset }

function TRODASQLDataset.Dataset: TDataset;
begin
  Result := FDADataset.Dataset;
end;

function TRODASQLDataset.NativeDataset: IInterface;
begin
  Result := FDADataset;
end;

procedure TRODASQLDataset.Open;
begin
  inherited;

  FDADataset := FDAConnection.NewDataset(SQL);
  AssignDAParams(Parameters, FDADataset.Params);

  FDADataset.Open;
end;

{ TRODAORMCommand }

function TRODAORMCommand.Execute: Integer;
begin
  inherited;

  FDASQLCommand := FDAConnection.NewCommand(SQL, stSQL);
  AssignDAParams(Parameters, FDASQLCommand.Params);

  Result := FDASQLCommand.Execute;
end;

function TRODAORMCommand.NativeSQLCommand: IInterface;
begin
  Result := FDASQLCommand;
end;

{ TDAConnectionHelper }

procedure TDAConnectionHelper.Releasing;
begin
  Self.fReleasing := True;
end;

end.
