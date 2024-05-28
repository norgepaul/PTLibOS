{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network.HttpFileDownload                    }
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

unit PTLib.Common.Network.HttpFileDownload;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.Math,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdIOHandler, IdIOHandlerStream,

  PTLib.Common.Files,
  PTLib.Common.LockedTypes;

type
  TPTLibHTTPFileDownload = class;

  TPTLibHTTPDownloadThread = Class(TThread)
  strict private
    FUrl: TPTLockedValue<String>;
    FLocalFile: TPTLockedValue<String>;
    FUsername: TPTLockedValue<String>;
    FPassword: TPTLockedValue<String>;
    FChunk: TPTLockedValue<Integer>;
    FBusy: TPTLockedValue<Boolean>;
    FTotal: TPTLockedValue<Int64>;
    FCurrent: TPTLockedValue<Int64>;
    FOwner: TPTLibHTTPFileDownload;
    FDiff: Int64;
    FException: Exception;
    FStatus: String;
  strict private
    procedure OnWorkStarts(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure OnWorkEnds(ASender: TObject; AWorkMode: TWorkMode);
    procedure OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure OnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: String);

    procedure SignalException;
    procedure SignalProgress;
    procedure SignalFinished;
    procedure SignalCancelled;
  private
    procedure SignalStatus;
    procedure UpdateStatus(const Status: String);
    function GetLocalFileSize(const Filename: String): Int64;
  protected
    procedure DoTerminate; override;
  public
    constructor Create(AOwner: TPTLibHTTPFileDownload); reintroduce;
    destructor Destroy; override;

    procedure Execute; override;

    property Busy: TPTLockedValue<Boolean> read FBusy;
    property ChunkSize: TPTLockedValue<Integer> read FChunk;
    property RemoteURL: TPTLockedValue<String> read FUrl;
    property LocalFile: TPTLockedValue<String> read FLocalFile;
    property Username: TPTLockedValue<String> read FUsername;
    property Password: TPTLockedValue<String> read FPassword;
  end;

  TOnDownloadEvent = procedure(Sender: TObject; const URL, Filename: String) of object;
  TOnDownloadStatus = procedure(Sender: TObject; const URL, Filename, Status: String) of object;
  TOnDownloadError = procedure(Sender: TObject; const e: Exception; const URL, Filename: String) of object;
  TOnDownloadProcess = procedure(Sender: TObject; const Position, Max: Int64; var Cancel: Boolean) of object;

  TPTLibHTTPFileDownload = class(TComponent)
  strict private
    FOnDownloadStart: TOnDownloadEvent;
    FOnDownloadComplete: TOnDownloadEvent;
    FOnDownloadCancelled: TOnDownloadEvent;
    FOnDownloadError: TOnDownloadError;
    FOnProgress: TOnDownloadProcess;
    FOnDownloadStatus: TOnDownloadStatus;

    FThread: TPTLibHTTPDownloadThread;
    FChunk: Integer;
    FUrl: String;
    FLocal: String;
    FShutdown: Boolean;
    FUsername: String;
    FPassword: String;

    function GetBusy: Boolean;
    procedure HandleDownloadTerminated(Sender: TObject);
    procedure SetChunk(Value: Integer);
    procedure SetUrl(const Value: String);
    procedure SetLocal(const Value: String);
  protected
    procedure DoDownloadProgress(const Position, Max: Int64; var Cancel: Boolean); virtual;
    procedure DoDownloadStatus(const URL, Filename, Status: String); virtual;
    procedure DoDownloadStart(const URL, Filename: String); virtual;
    procedure DoDownloadComplete(const URL, Filename: String); virtual;
    procedure DoDownloadCancelled(const URL, Filename: String); virtual;
    procedure DoDownloadError(const e: Exception; const URL, Filename: String); virtual;
  public
    procedure Download(const URL, LocalFilename: String);
    procedure Cancel;
    procedure BeforeDestruction; override;

    property RemoteURL: String read FUrl;
    property LocalFile: String read FLocal;
    property Busy: Boolean read GetBusy;
  published
    property OnProgress: TOnDownloadProcess read FOnProgress write FOnProgress;
    property OnDownloadStart: TOnDownloadEvent read FOnDownloadStart write FOnDownloadStart;
    property OnDownloadError: TOnDownloadError read FOnDownloadError write FOnDownloadError;
    property OnDownloadComplete: TOnDownloadEvent read FOnDownloadComplete  write FOnDownloadComplete;
    property OnDownloadCancelled: TOnDownloadEvent read FOnDownloadCancelled write FOnDownloadCancelled;
    property OnDownloadStatus: TOnDownloadStatus read FOnDownloadStatus write FOnDownloadStatus;

    property ChunkSize: Integer read FChunk write SetChunk;
    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;
  end;

implementation

resourcestring
  StrFailedToDownload = 'Failed to download [%s], download already active error';
  StrDownloadingFile = 'Downloading file...';
  StrRetrievingRemoteFi = 'Retrieving remote file size';
  StrDownloadingSOf = 'Downloading - %s of %s';

{ TPTLibHTTPDownloadThread }

constructor TPTLibHTTPDownloadThread.Create(AOwner: TPTLibHTTPFileDownload);
begin
  inherited Create(True);

  FOwner := AOwner;
  FreeOnTerminate := False;

  FUrl := TPTLockedValue<String>.Create;
  FLocalFile := TPTLockedValue<String>.Create;
  FChunk := TPTLockedValue<Integer>.Create;
  FBusy := TPTLockedValue<Boolean>.Create;
  FUsername := TPTLockedValue<String>.Create;
  FPassword := TPTLockedValue<String>.Create;
  FTotal := TPTLockedValue<Int64>.Create;
  FCurrent := TPTLockedValue<Int64>.Create;
end;

destructor TPTLibHTTPDownloadThread.Destroy;
begin
  FreeAndNil(FUrl);
  FreeAndNil(FLocalFile);
  FreeAndNil(FChunk);
  FreeAndNil(FBusy);
  FreeAndNil(FUsername);
  FreeAndNil(FPassword);
  FreeAndNil(FTotal);
  FreeAndNil(FCurrent);

  inherited;
end;

procedure TPTLibHTTPDownloadThread.DoTerminate;
begin
  inherited;

  Synchronize(SignalCancelled);

  FOwner := nil;
end;

procedure TPTLibHTTPDownloadThread.OnWorkStarts(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  if not Terminated then
  begin
    FTotal.Value := AWorkCountMax;
    FCurrent.Value := 0;
    Queue(SignalProgress);
  end;
end;

procedure TPTLibHTTPDownloadThread.OnWorkEnds(ASender: TObject;
  AWorkMode: TWorkMode);
begin
  if not Terminated then
  begin
    FCurrent.Value := FTotal.Value;
    Queue(SignalProgress);
  end;
end;

procedure TPTLibHTTPDownloadThread.OnStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: String);
begin
  UpdateStatus(AStatusText);
end;

procedure TPTLibHTTPDownloadThread.SignalStatus;
begin
  if Assigned(FOwner) then
  begin
    FOwner.DoDownloadStatus(FURL.Value, FLocalFile.Value, FStatus);
  end;
end;

procedure TPTLibHTTPDownloadThread.UpdateStatus(const Status: String);
begin
  FStatus := Status;

  Queue(SignalStatus);
end;

procedure TPTLibHTTPDownloadThread.OnWork(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
const
  CNT_SIGNAL = 1024 * 100;
begin
  if Terminated then
  begin
    TIdHttp(ASender).OnWorkBegin := nil;
    TIdHttp(ASender).OnWorkEnd := nil;
    TIdHttp(ASender).OnWork := nil;

    if TIdHttp(ASender).Connected then
    begin
      TIdHttp(ASender).Disconnect;
    end;
  end
  else
  begin
    FCurrent.Value := AWorkCount;

    if not Terminated then
    begin
      if FCurrent.Value - FDiff > CNT_SIGNAL then
      begin
        FDiff := FCurrent.Value;

        Queue(SignalProgress);

        UpdateStatus(format(StrDownloadingSOf, [
          TFileUtils.FileSizeFormatStr(AWorkCount),
          TFileUtils.FileSizeFormatStr(FTotal.Value)]));
      end;
    end;
  end;
end;

procedure TPTLibHTTPDownloadThread.SignalProgress;
var
  Cancel: Boolean;
begin
  Cancel := False;

  if Assigned(FOwner) then
  begin
    FOwner.DoDownloadProgress(FCurrent.Value, FTotal.Value, Cancel);

    if Cancel then
    begin
      Terminate;
    end;
  end;
end;

procedure TPTLibHTTPDownloadThread.SignalCancelled;
begin
  if Assigned(FOwner) then
  begin
    FOwner.DoDownloadCancelled(RemoteURL.Value, LocalFile.Value);
  end;
end;

procedure TPTLibHTTPDownloadThread.SignalException;
begin
  if Assigned(FOwner) then
  begin
    FOwner.DoDownloadError(FException, FURL.Value, FLocalFile.Value);
  end;
end;

procedure TPTLibHTTPDownloadThread.SignalFinished;
begin
  if Assigned(FOwner) then
  begin
    FOwner.DoDownloadComplete(FURL.Value, FLocalFile.Value);
  end;
end;

function TPTLibHTTPDownloadThread.GetLocalFileSize(const Filename: String): Int64;
var
  FileStream: TFileStream;
begin
  Result := 0;

  if FileExists(Filename) then
  begin
    FileStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
    try
      Result := FileStream.Size;
    finally
      FreeAndNil(FileStream);
    end;
  end;
end;

procedure TPTLibHTTPDownloadThread.Execute;
var
  HTTP: TIdHttp;
  Offset: Int64;
  Total: Int64;
  FileStream: TFileStream;
begin
  FBusy.Value := True;
  Offset := 0;

  HTTP := TIdHttp.Create(nil);
  try
    try
      HTTP.OnWorkBegin := OnWorkStarts;
      HTTP.OnWorkEnd := OnWorkEnds;
      HTTP.OnWork := OnWork;
      HTTP.OnStatus := OnStatus;

      HTTP.Request.Username := FUsername.Value;
      HTTP.Request.Password := FPassword.Value;

      // Have we downloaded this file in-part before?
      if FileExists(FLocalFile.Value) then
      begin
        // Use current size as the offset when downloading
        Offset := GetLocalFileSize(FLocalFile.Value);
      end;

      UpdateStatus(StrRetrievingRemoteFi);

      // Query server about the size of the remote file
      HTTP.Head(FUrl.Value);
      Total := HTTP.Response.ContentLength;

      if not Terminated then
      begin
        // Open up File
        if FileExists(FLocalFile.Value) then
        begin
          FileStream := TFileStream.Create(FLocalFile.Value, fmOpenWrite or
            fmShareDenyNone);
        end
        else
        begin
          FileStream := TFileStream.Create(FLocalFile.Value,
            fmCreate or fmShareDenyNone)
        end;

        // Continue downloading
        try
          // Continue download or start from scratch?
          if (Offset > 0) and (Offset < Total) then
          begin
            HTTP.Request.Range := IntToStr(Offset) + '-' + IntToStr(Total);
          end;

          FileStream.Position := FileStream.Size;

          UpdateStatus(StrDownloadingFile);

          HTTP.Get(FUrl.Value, FileStream);

          if not Terminated then
          begin
            Synchronize(SignalFinished);
          end;
        finally
          FreeAndNil(FileStream);
        end;
      end;
    except
      on e: Exception do
      begin
        FException := e;

        Synchronize(SignalException);
      end;
    end;
  finally
    FreeAndNil(HTTP);

    FBusy.Value := false;
  end;
end;


{ TPTLibHTTPFileDownload }

procedure TPTLibHTTPFileDownload.BeforeDestruction;
begin
  inherited;

  if GetBusy then
  begin
    FShutdown := True;
    FThread.Terminate;
  end;
end;

procedure TPTLibHTTPFileDownload.DoDownloadProgress(const Position, Max: Int64; var Cancel: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Max, Cancel);
end;

procedure TPTLibHTTPFileDownload.DoDownloadStart(const URL, Filename: String);
begin
  if Assigned(FOnDownloadStart) then
    FOnDownloadStart(Self, URL, Filename);
end;

procedure TPTLibHTTPFileDownload.DoDownloadStatus(const URL, Filename,
  Status: String);
begin
  if Assigned(FOnDownloadStatus) then
    FOnDownloadStatus(Self, URL, Filename, Status);
end;

procedure TPTLibHTTPFileDownload.DoDownloadCancelled(const URL, Filename: String);
begin
  if Assigned(FOnDownloadCancelled) then
    FOnDownloadCancelled(Self, URL, Filename);
end;

procedure TPTLibHTTPFileDownload.DoDownloadComplete(const URL, Filename: String);
begin
  if Assigned(FOnDownloadComplete) then
    FOnDownloadComplete(Self, URL, Filename);
end;

procedure TPTLibHTTPFileDownload.DoDownloadError(const e: Exception;
  const URL, Filename: String);
begin
  if Assigned(FOnDownloadError) then
    FOnDownloadError(Self, e, URL, Filename);
end;

procedure TPTLibHTTPFileDownload.Cancel;
begin
  if GetBusy then
  begin
    if not FThread.Terminated then
    begin
      FThread.Terminate;

      FThread := nil;
    end;
  end;
end;

procedure TPTLibHTTPFileDownload.Download(const URL, LocalFilename: String);
begin
  if not GetBusy then
  begin
    SetUrl(Url);
    SetLocal(LocalFilename);

    FThread := TPTLibHTTPDownloadThread.Create(Self);
    FThread.LocalFile.Value := LocalFilename;
    FThread.RemoteURL.Value := Url;
    FThread.ChunkSize.Value := FChunk;
    FThread.Username.Value := FUsername;
    FThread.Password.Value := FPassword;
    FThread.OnTerminate := HandleDownloadTerminated;
    FThread.FreeOnTerminate := True;

    DoDownloadStart(URL, LocalFilename);

    FThread.Start;
  end
  else
  begin
    raise Exception.CreateFmt(StrFailedToDownload, [Url]);
  end;
end;

function TPTLibHTTPFileDownload.GetBusy: Boolean;
begin
  result := Assigned(FThread);
end;

procedure TPTLibHTTPFileDownload.HandleDownloadTerminated(Sender: TObject);
begin
  if not FShutdown then
  begin
    FThread := nil;
  end;
end;

procedure TPTLibHTTPFileDownload.SetChunk(Value: Integer);
begin
  if not GetBusy then
  begin
    FChunk := System.Math.EnsureRange(Value, 1000000, 4000000);
  end;
end;

procedure TPTLibHTTPFileDownload.SetLocal(const Value: String);
begin
  if not GetBusy then
  begin
    FLocal := Value;
  end;
end;

procedure TPTLibHTTPFileDownload.SetUrl(const Value: String);
begin
  if not GetBusy then
  begin
    FUrl := Value;
  end;
end;

end.
