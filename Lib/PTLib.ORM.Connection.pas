unit PTLib.ORM.Connection;

interface

uses
  System.SysUtils, System.Classes,

  PTLib.ORM,
  PTLib.ORM.Interfaces,
  PTLib.ORM.Session,

  PTLib.Common.Classes,
  PTLib.Common.Types,
  PTLib.Common.Log;

type
  TORMConnection = class
  strict private
    FORM: TORM;
    FOnSQLBeforeRead: TOnSQLBeforeRead;
    FOnSQLAfterRead: TOnSQLAfterRead;
    FOnSQLBeforeWrite: TOnSQLBeforeWrite;
    FOnSQLAfterWrite: TOnSQLAfterWrite;
    FOnSQLTransactionError: TOnSQLTransactionError;
    FOnSQLError: TOnSQLError;
  private
    procedure InternalOnSQLAfterRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset; const TickCount: Cardinal);
    procedure InternalOnSQLAfterWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand; const TickCount: Cardinal; const RowsAffected: Integer);
    procedure InternalOnSQLBeforeRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset);
    procedure InternalOnSQLBeforeWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand);
    procedure InternalOnSQLError(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception);
    procedure InternalOnSQLTransactionError(Sender: TORMSessionConnection; const e: Exception);
    procedure InternalOnAfterORMSessionCreate(Sender: TObject; const ORMSession: IORMSession);
  protected
    procedure DoOnSQLAfterRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset; const TickCount: Cardinal); virtual;
    procedure DoOnSQLAfterWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand; const TickCount: Cardinal; const RowsAffected: Integer); virtual;
    procedure DoOnSQLBeforeRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset); virtual;
    procedure DoOnSQLBeforeWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand); virtual;
    procedure DoOnSQLError(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception); virtual;
    procedure DoOnSQLTransactionError(Sender: TORMSessionConnection; const e: Exception); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property OnSQLBeforeRead: TOnSQLBeforeRead read FOnSQLBeforeRead write FOnSQLBeforeRead;
    property OnSQLAfterRead: TOnSQLAfterRead read FOnSQLAfterRead write FOnSQLAfterRead;
    property OnSQLBeforeWrite: TOnSQLBeforeWrite read FOnSQLBeforeWrite write FOnSQLBeforeWrite;
    property OnSQLAfterWrite: TOnSQLAfterWrite read FOnSQLAfterWrite write FOnSQLAfterWrite;
    property OnSQLTransactionError: TOnSQLTransactionError read FOnSQLTransactionError write FOnSQLTransactionError;
    property OnSQLError: TOnSQLError read FOnSQLError write FOnSQLError;

    property ORM: TORM read FORM;
  end;

implementation

{ TORMFireDacConnection }

constructor TORMConnection.Create;
begin
  FORM := TORM.Create(nil);

  FORM.OnAfterORMSessionCreated := InternalOnAfterORMSessionCreate;
end;

destructor TORMConnection.Destroy;
begin
  FreeAndNil(FORM);

  inherited;
end;

procedure TORMConnection.DoOnSQLAfterRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset; const TickCount: Cardinal);
begin
  TGLobalLog.Log(
    Self,
    'DB Read',
    NewParameters.
      SetParam('response_ms', TickCount).
      SetParam('sql', SQLDataset.GetFormattedSQL(True)),
     LogSeverityDebug3);

  if Assigned(FOnSQLAfterRead) then
  begin
    FOnSQLAfterRead(Sender, SQLDataset, Tickcount);
  end;
end;

procedure TORMConnection.DoOnSQLAfterWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand; const TickCount: Cardinal;
  const RowsAffected: Integer);
begin
  TGLobalLog.Log(
    Self,
    'DB Command',
    NewParameters.
      SetParam('response_ms', TickCount).
      SetParam('row_affected', RowsAffected).
      SetParam('sql', SQLCommand.GetFormattedSQL(True)),
    LogSeverityDebug3);

  if Assigned(FOnSQLAfterWrite) then
  begin
    FOnSQLAfterWrite(Sender, SQLCommand, Tickcount, RowsAffected);
  end;
end;

procedure TORMConnection.DoOnSQLBeforeRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset);
begin
  if Assigned(FOnSQLBeforeRead) then
  begin
    FOnSQLBeforeRead(Sender, SQLDataset)
  end;
end;

procedure TORMConnection.DoOnSQLBeforeWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand);
begin
  if Assigned(FOnSQLBeforeWrite) then
  begin
    FOnSQLBeforeWrite(Sender, SQLCommand)
  end;
end;

procedure TORMConnection.DoOnSQLError(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception);
begin
  TGLobalLog.Log(
    Self,
    'DB Error',
    NewParameters.
      SetParam('sql', SQLQuery.GetFormattedSQL(True)).
      SetParam('error', e.Message),
    LogSeverityError);

  if Assigned(FOnSQLError) then
  begin
    FOnSQLError(Sender, SQLQuery, e);
  end;
end;

procedure TORMConnection.DoOnSQLTransactionError(Sender: TORMSessionConnection; const e: Exception);
begin
  TGLobalLog.Log(
    Self,
    'DB Transaction Error',
    NewParameters.
      SetParam('error', e.Message),
    LogSeverityError);

  if Assigned(FOnSQLTransactionError) then
  begin
    FOnSQLTransactionError(Sender, e);
  end;
end;

procedure TORMConnection.InternalOnSQLBeforeRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset);
begin
  DoOnSQLBeforeRead(Sender, SQLDataset);
end;

procedure TORMConnection.InternalOnSQLBeforeWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand);
begin
  DoOnSQLBeforeWrite(Sender, SQLCommand);
end;

procedure TORMConnection.InternalOnAfterORMSessionCreate(Sender: TObject; const ORMSession: IORMSession);
begin
  ORMSession.Connection.OnSQLBeforeRead := InternalOnSQLBeforeRead;
  ORMSession.Connection.OnSQLAfterRead := InternalOnSQLAfterRead;
  ORMSession.Connection.OnSQLBeforeWrite := InternalOnSQLBeforeWrite;
  ORMSession.Connection.OnSQLAfterWrite := InternalOnSQLAfterWrite;
  ORMSession.Connection.OnSQLTransactionError := InternalOnSQLTransactionError;
  ORMSession.Connection.OnSQLError := InternalOnSQLError;
end;

procedure TORMConnection.InternalOnSQLAfterRead(Sender: TORMSessionConnection; const SQLDataset: ISQLDataset; const TickCount: Cardinal);
begin
  DoOnSQLAfterRead(Sender, SQLDataset, TickCount);
end;

procedure TORMConnection.InternalOnSQLAfterWrite(Sender: TORMSessionConnection; const SQLCommand: ISQLCommand; const TickCount: Cardinal; const RowsAffected: Integer);
begin
  DoOnSQLAfterWrite(Sender, SQLCommand, TickCount, RowsAffected);
end;

procedure TORMConnection.InternalOnSQLTransactionError(Sender: TORMSessionConnection; const e: Exception);
begin
  DoOnSQLTransactionError(Sender, e);
end;

procedure TORMConnection.InternalOnSQLError(Sender: TORMSessionConnection; const SQLQuery: ISQLQuery; const e: Exception);
begin
  DoOnSQLError(Sender, SQLQuery, e);
end;

end.
