{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Log.Receiver                                }
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

unit PTLib.Common.Log.Receiver;

interface

uses
  Classes, SysUtils, DateUtils,  Generics.Collections,

  IdComponent, IdHTTPServer, IdContext, IdCustomHTTPServer,

  PTLib.Common.Thread,
  PTLib.Common.Log,
  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Dates,
  PTLib.Common.Types,
  PTLib.Common.Network.SimpleHTTPServer;

type
  TRemoteLogEntry = class(TLogEntry,
                          IRemoteLogEntry)
  strict private
    FClientName: String;
  private
    function GetClientName: String;
    procedure SetClientName(const Value: String);
  public
    property ClientName: String read GetClientName write SetClientName;
  end;

  TPTLibLogReceiver = class(TPTLibLoggedHTTPServer)
  private
  protected
    procedure DoServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    procedure DoOnLogProviderChanged(const OldLogProvider, NewLogProvider: TPTLibLog); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TPTLibLogReceiver }

constructor TPTLibLogReceiver.Create(AOwner: TComponent);
begin
  inherited;

  Port := 57361;
end;

procedure TPTLibLogReceiver.DoOnLogProviderChanged(
  const OldLogProvider, NewLogProvider: TPTLibLog);
begin
  inherited;

  DoResetServer;
end;

procedure TPTLibLogReceiver.DoServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LogStrings, CommaStrings: TStringList;
  i: Integer;
  RemoteLogEntry: IRemoteLogEntry;
  ClientName: String;
begin
  if LogProvider <> nil then
  begin
    LogStrings := TStringList.Create;
    CommaStrings := TStringList.Create;
    try
      try
        LogStrings.LoadFromStream(ARequestInfo.PostStream);

        if LogStrings.Count >= 1 then
        begin
          ClientName := LogStrings[0];

          for i := 1 to pred(LogStrings.Count) do
          begin
            CommaStrings.CommaText := LogStrings[i];

            if CommaStrings.Count = 5 then
            begin
              RemoteLogEntry := TRemoteLogEntry.Create;
              RemoteLogEntry.ClientName := ClientName;
              RemoteLogEntry.LogType := CommaStrings[2];
              RemoteLogEntry.LogText := CommaStrings[4];
              RemoteLogEntry.Severity := TLogSeverity(StrToIntDef(CommaStrings[3], 0));
              RemoteLogEntry.TimeStampUTC := UTCToLocal(CommonDateTimeStrToDateTime(CommaStrings[0]));
              RemoteLogEntry.GeneratedUTC := UTCToLocal(CommonDateTimeStrToDateTime(CommaStrings[1]));

              LogProvider.Log(RemoteLogEntry);
            end;
          end;
        end;
      except
        on e: Exception do
        begin
          //DoOnException(e);
        end;
      end;
    finally
      FreeAndNil(CommaStrings);
      FreeAndNil(LogStrings);
    end;
  end;
end;

{ TRemoteLogEntry }

function TRemoteLogEntry.GetClientName: String;
begin
  Result := FClientName;
end;

procedure TRemoteLogEntry.SetClientName(const Value: String);
begin
  FClientName := Value;
end;

end.
