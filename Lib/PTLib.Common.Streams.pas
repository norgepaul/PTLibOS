{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Streams                                     }
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

unit PTLib.Common.Streams;

interface

uses
  Classes, SysUtils;

function StringToStream(const InString: String; const OutStream: TStream): Boolean;
function StreamToString(const InStream: TStream): String;

implementation

function StringToStream(const InString: String; const OutStream: TStream): Boolean;
var
  AByte: Byte;
  p: Integer;
begin
  Result := InString <> '';

  if Result then
  begin
    p := 1;

    while p < length(InString) do
    begin
      AByte := StrToInt(format('$%s%s', [InString[p], InString[p + 1]]));

      OutStream.Write(AByte, SizeOf(AByte));

      Inc(p, 2);
    end;
  end;
end;

function StreamToString(const InStream: TStream): String;
var
  AByte: Byte;
begin
  InStream.Position := 0;

  While InStream.Position < InStream.Size do
  begin
    InStream.Read(AByte, SizeOf(Byte));
    Result := concat(Result, IntToHex(AByte, 2));
  end;
end;

end.
