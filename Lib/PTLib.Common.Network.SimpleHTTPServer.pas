{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network.SimpleHTTPServer                    }
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

unit PTLib.Common.Network.SimpleHTTPServer;

interface

uses
  Classes, SysUtils, DateUtils,  Generics.Collections,

  IdComponent, IdTcpServer, IdHTTPServer, IdContext, IdCustomHTTPServer,

  PTLib.Common.Network,
  PTLib.Common.Thread,
  PTLib.Common.Log,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Dates;

type
  TPTLibLoggedServer = class(TBasePTLibLogComponent)
  strict private
    FActive: Boolean;
    FPort: Integer;
    FAutoFindFreePort: Boolean;
    FActivePort: Integer;
  private
    procedure SetPort(const Value: Integer);
    procedure SetActive(const Value: Boolean);
    function GetActivePort: Integer;
  protected
    procedure DoResetServer; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Loaded; override;

    property ActivePort: Integer read GetActivePort;
  published
    property Port: Integer read FPort write SetPort;
    property Active: Boolean read FActive write SetActive;
    property AutoFindFreePort: Boolean read FAutoFindFreePort write FAutoFindFreePort;
  end;

  TPTLibLoggedTCPServer = class(TPTLibLoggedServer)
  strict private
    FIdTcpServer: TIdTcpServer;
  private
    procedure OnExecute(AContext: TIdContext);
  protected
    procedure DoResetServer; override;
    procedure DoExecute(AContext: TIdContext); virtual; abstract;
  end;

  TPTLibLoggedHTTPServer = class(TPTLibLoggedServer)
  strict private
    FIdHTTPServer: TIdHTTPServer;
  private
    procedure OnServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  protected
    procedure DoResetServer; override;

    procedure DoServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual; abstract;
  end;

implementation

procedure TPTLibLoggedServer.Loaded;
begin
  inherited;

  DoResetServer;
end;

constructor TPTLibLoggedServer.Create(AOwner: TComponent);
begin
  inherited;

  FPort := 57361;
  FActive := False;
  FAutoFindFreePort := True;
end;

procedure TPTLibLoggedServer.DoResetServer;
begin
  if (FActive) and
     (not (csDesigning in ComponentState)) then
  begin
    FActivePort := FPort;

    if AutoFindFreePort then
    begin
      FActivePort := GetNextAvailableTCPPort(FActivePort);
    end;
  end;
end;

function TPTLibLoggedServer.GetActivePort: Integer;
begin
  Result := FActivePort;
end;

procedure TPTLibLoggedServer.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;

    DoResetServer;
  end;
end;

procedure TPTLibLoggedServer.SetPort(const Value: Integer);
begin
  if FPort <> Value then
  begin
    FPort := Value;

    if FPort > 65535 then
      FPort := 65535
    else
    if FPort < 1 then
      FPort := 1;

    DoResetServer;
  end;
end;

{ TPTLibLoggedHTTPServer }

procedure TPTLibLoggedHTTPServer.DoResetServer;
begin
  inherited;

  FreeAndNil(FIdHTTPServer);

  if (Active) and
     (not (csDesigning in ComponentState)) then
  begin
    FIdHTTPServer := TIdHTTPServer.Create(Self);
    FIdHTTPServer.OnCommandGet := OnServerCommandGet;
    FIdHTTPServer.DefaultPort := ActivePort;
    FIdHTTPServer.Bindings.DefaultPort := ActivePort;
    FIdHTTPServer.Active := True;
  end;
end;

procedure TPTLibLoggedHTTPServer.OnServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  try
    DoServerCommandGet(
      AContext,
      ARequestInfo,
      AResponseInfo);
  except
    on e: Exception do
    begin
      if Assigned(LogProvider) then
      begin
        LogProvider.Log(
          'Error: ' + e.Message,
          '',
          LogSeverityError);
      end;
    end;
  end;
end;

{ TPTLibLoggedTCPServer }

procedure TPTLibLoggedTCPServer.DoResetServer;
begin
  inherited;

  FreeAndNil(FIdTcpServer);

  if (Active) and
     (not (csDesigning in ComponentState)) then
  begin
    FIdTcpServer := TIdTcpServer.Create(Self);
    FIdTcpServer.OnExecute := OnExecute;
    FIdTcpServer.DefaultPort := ActivePort;
    FIdTcpServer.Bindings.DefaultPort := ActivePort;
    FIdTcpServer.Active := True;
  end;
end;

procedure TPTLibLoggedTCPServer.OnExecute(AContext: TIdContext);
begin
  try
    DoExecute(
      AContext);
  except
    on e: Exception do
    begin
      if Assigned(LogProvider) then
      begin
        LogProvider.Log(
          'Error: ' + e.Message,
          '',
          LogSeverityError);
      end;
    end;
  end;
end;

end.
