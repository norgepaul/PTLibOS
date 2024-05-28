{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Database.Services                                  }
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

unit PTLib.Database.Services;

interface

uses
  Classes, SysUtils,

  FireDAC.DApt,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TOnProgress = procedure(Sender: TObject; Progress, ProgressMax: Integer) of object;
  TOnStatus = procedure(Sender: TObject; StatusText: String) of object;

  TSMTPProperties = class(TPersistent)
  strict private
    FServer: String;
    FUsername: String;
    FPassword: String;
    FSenderEmail: String;
    FPort: Integer;
    FRequiresAuthentication: Boolean;
    FRequiresSSL: Boolean;
  published
    property Server: String read FServer write FServer;
    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;
    property SenderEmail: String read FSenderEmail write FSenderEmail;
    property Port: Integer read FPort write FPort;
    property RequiresAuthentication: Boolean read FRequiresAuthentication write FRequiresAuthentication;
    property RequiresSSL: Boolean read FRequiresSSL write FRequiresSSL;
  end;

  TBasePTLibDatabaseService = class(TBasePTLibComponent) //, IConfigurationProvider)
  strict private
    FOnProgress: TOnProgress;
    FOnStatus: TOnStatus;

    FSMTPProperties: TSMTPProperties;
  protected
    FLogFilename: String;
    FWriteLogFile: Boolean;

    procedure DoOnStatus(StatusText: String); virtual;
    procedure DoOnProgress(Progress, ProgressMax: Integer); virtual;

    //procedure SaveToConfig(const Config: IConfiguration; RootKey: String); virtual;
    //procedure LoadFromConfig(const Config: IConfiguration; RootKey: String); virtual;

    procedure WriteToLogFile(const LogText: String); virtual;
    function SendEmail(const Recipient, CC, BCC, Subject, Body, AttachmentFilename: String; var Error: String): Boolean; virtual;

    property SMTPProperties: TSMTPProperties read FSMTPProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnStatus: TOnStatus read FOnStatus write FOnStatus;

    Property  LogFileName: String read FLogFileName write FLogFileName;
    property  WriteLogFile: Boolean read FWriteLogFile write FWriteLogFile;
  end;

implementation


{ TBaseDatabaseService }

constructor TBasePTLibDatabaseService.Create(AOwner: TComponent);
begin
  inherited;

  FSMTPProperties := TSMTPProperties.Create;

  FWriteLogFile := TRUE;
end;

destructor TBasePTLibDatabaseService.Destroy;
begin
  FreeAndNil(FSMTPProperties);

  inherited;
end;

procedure TBasePTLibDatabaseService.DoOnStatus(StatusText: String);
begin
  StatusText := StringReplace(StatusText, 'gbak: ', '', [rfIgnoreCase]);
  WriteToLogFile(format('%s: %s', [formatDateTime('hh:nn:ss:zzz', now), StatusText]));

  if Assigned(FOnStatus) then
    FOnStatus(Self, StatusText);
end;

(*procedure TBaseDatabaseService.LoadFromConfig(
  const Config: IConfiguration; RootKey: String);
begin
  inherited;

  FLogFilename := Config.ReadString(RootKey, 'LogFilename', FLogFilename);
  FWriteLogFile := Config.ReadBoolean(RootKey, 'WriteLogFile', FWriteLogFile);
end;

procedure TBaseDatabaseService.SaveToConfig(const Config: IConfiguration;
  RootKey: String);
begin
  inherited;

  Config.WriteString(RootKey, 'LogFilename', FLogFilename);
  Config.WriteBoolean(RootKey, 'WriteLogFile', FWriteLogFile);
end;*)

function TBasePTLibDatabaseService.SendEmail(const Recipient, CC, BCC, Subject, Body,
  AttachmentFilename: String; var Error: String): Boolean;
(* var
  EmailThread: TEmailThread;
  Queue:  TExternalMessageQueue; *)
begin
  Result := False;
  (* Queue:=TExternalMessageQueue.Create(NIL);
  try
    EmailThread := TEmailThread.Create(nil);
    try

      EmailQueue.SMTPServer := SMTPProperties.Server;
      EmailQueue.SMTPUsername := SMTPProperties.Username;
      EmailQueue.SMTPPassword := SMTPProperties.Password;
      EmailQueue.SMTPPort := SMTPProperties.Port;
      EmailQueue.EmailSender := SMTPProperties.SenderEmail;
      EmailQueue.UseAuthentication := SMTPProperties.RequiresAuthentication;
      EmailQueue.UseSSL := SMTPProperties.RequiresSSL;

      Result := EmailQueue.SendMessage(SMTPProperties.SenderEmail, Recipient, CC, BCC, Subject, Body, AttachmentFilename, Error);
    finally
      FreeAndNil(EmailQueue);
    end;
  finally
    Queue.Free;
  end;   *)
end;

procedure TBasePTLibDatabaseService.DoOnProgress(Progress, ProgressMax: Integer);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Progress, ProgressMax);
end;

procedure TBasePTLibDatabaseService.WriteToLogFile(const LogText: String);
var
  LogFile: TextFile;
begin
  if (FWriteLogFile) and
     (FLogFilename <> '') then
  try
    AssignFile(LogFile, FLogFilename);

    if FileExists(FLogFilename) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    try
      WriteLn(LogFile, LogText);
    finally
      CloseFile(LogFile);
    end;
  except
    // Don't allow exceptions
  end;
end;

end.
