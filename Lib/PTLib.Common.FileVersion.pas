{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.FileVersion                                 }
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

unit PTLib.Common.FileVersion;

interface

uses
  SysUtils

  {$IFDEF MSWINDOWS}
  , WinAPI.Windows
  {$ELSEIF OSX}
  , Macapi.CoreFoundation
  {$ENDIF}
  ;

function GetAppVersionStr: string; overload;
function GetAppVersionStr(const Filename: String): string; overload;

implementation

function GetAppVersionStr: String;
begin
  Result := GetAppVersionStr(ParamStr(0));
end;

{$IFDEF MSWINDOWS}

function GetAppVersionStr(const Filename: String): string;
var
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Result := '0.0.0.0';

  Size := GetFileVersionInfoSize(PChar(Filename), Handle);

  if Size <> 0 then
  begin
    SetLength(Buffer, Size);
    if (GetFileVersionInfo(PChar(Filename), Handle, Size, Buffer)) and
       (VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size)) then
    begin
      Result := Format('%d.%d.%d.%d',
        [LongRec(FixedPtr.dwFileVersionMS).Hi,  //major
         LongRec(FixedPtr.dwFileVersionMS).Lo,  //minor
         LongRec(FixedPtr.dwFileVersionLS).Hi,  //release
         LongRec(FixedPtr.dwFileVersionLS).Lo]) //build
    end;
  end;
end;

{$ELSEIF MACOS}

function GetAppVersionStr(const Filename: String): string;
var
  CFStr: CFStringRef;
  Range: CFRange;
begin
  CFStr := CFBundleGetValueForInfoDictionaryKey(
    CFBundleGetMainBundle, kCFBundleVersionKey);
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

{$ELSE}

function GetAppVersionStr(const Filename: String): string;
begin
  Result := 'V0.0.0.1';
end;

{$ENDIF}

end.
