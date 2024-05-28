{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network                                     }
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

unit PTLib.Common.Network;

interface

uses
  System.Classes, System.SysUtils,

  IdStack, IdComponent, IdHTTPServer, IdContext, IdCustomHTTPServer;

function GetNextAvailableTCPPort(const StartPort: Word): Word;
procedure GetLocalIPAddress(const Values: TStrings); overload;
function GetLocalIPAddress(const Prefix: String = ''): String; overload;

implementation

procedure GetLocalIPAddress(const Values: TStrings);
var
  StackLocalAddressList: TIdStackLocalAddressList;
  i: Integer;
begin
  TIdStack.IncUsage;
  try
    StackLocalAddressList := TIdStackLocalAddressList.Create;
    try
      GStack.GetLocalAddressList(StackLocalAddressList);

      Values.Clear;

      for i := 0 to pred(StackLocalAddressList.Count) do
        Values.Add(StackLocalAddressList[i].IPAddress);
    finally
      FreeAndNil(StackLocalAddressList);
    end;
  finally
    TIdStack.DecUsage;
  end;
end;

function GetLocalIPAddress(const Prefix: String): String;
var
  IPs: TStringList;
  i: Integer;
begin
  Result := '';

  IPs := TStringList.Create;
  try
    GetLocalIPAddress(IPs);

    for i := 0 to pred(IPs.Count) do
    begin
      if (IPs[i] <> '127.0.0.1') and
         (pos(':', IPs[i]) = 0) and
         ((Prefix = '') or
          (pos(Prefix, IPs[i]) = low(IPs[i]))) then
      begin
        Exit(IPs[i])
      end;
    end;

    // Didn't find a prefixed IP
    for i := 0 to pred(IPs.Count) do
    begin
      if (IPs[i] <> '127.0.0.1') and
         (pos(':', IPs[i]) = 0) then
      begin
        Exit(IPs[i])
      end;
    end;

    // We didn't find a non loopback IP, so return what we have.
    if IPs.Count > 0 then
    begin
      Result := IPs[0];
    end;
  finally
    FreeAndNil(IPs);
  end;
end;

function GetNextAvailableTCPPort(const StartPort: Word): Word;
var
  HTTPServer: TIdHTTPServer;
  Port: Word;
  Available: Boolean;
begin
  Available := False;

  HTTPServer := TIdHTTPServer.Create(nil);
  try
    Port := StartPort;

    repeat
      HTTPServer.Bindings.Clear;
      HTTPServer.DefaultPort := Port;

      try
        HTTPServer.Active := True;

        Available := True;
      except
        on e: exception do
        begin
          Available := False;

          Inc(Port);
        end;
      end;
    until Available;

    Result := Port;
  finally
    FreeAndNil(HTTPServer);
  end;
end;

end.
