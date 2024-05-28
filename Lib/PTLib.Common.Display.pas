{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Display                                     }
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

unit PTLib.Common.Display;

interface

uses
  System.SysUtils

  {$IFDEF MSWINDOWS}
  , WinAPI.Windows
  {$ENDIF}

  , PTLib.Common.Classes,
  PTLib.Common.Types;

procedure SetDisplayOrientation(const Orientation: TDisplayOrientation; const DeviceName: String = '');
procedure ShowCursor(const DoShow: Boolean);
function DegreesToScreenOrientation(const Degrees: Cardinal): TDisplayOrientation;

const
  DMDO_DEFAULT: Cardinal  = 0;
  DMDO_90: Cardinal  = 1;
  DMDO_180: Cardinal  = 2;
  DMDO_270: Cardinal  = 3;

implementation

const
  DM_DISPLAYORIENTATION = $00800000;
  ENUM_CURRENT_SETTINGS =-1;

function DegreesToScreenOrientation(const Degrees: Cardinal): TDisplayOrientation;
begin
  case Degrees of
    0: Result := TDisplayOrientation.orDefault;
    90: Result := TDisplayOrientation.or90;
    180: Result := TDisplayOrientation.or180;
    270: Result := TDisplayOrientation.or270;
  else
    raise Exception.Create('Invalid display rotation degrees - 0, 90, 180 or 270');
  end;
end;

{$IFDEF MSWINDOWS}
procedure ChangeOrientationInternal(const NewOrientation:DWORD; const DeviceName: PWideChar = nil);
var
  dm: TDeviceMode;
  dwTemp: DWORD;
  dmDisplayOrientation: DWORD;
begin
   ZeroMemory(@dm, sizeof(dm));

   dm.dmSize   := sizeof(dm);

   if EnumDisplaySettings(DeviceName, DWORD(ENUM_CURRENT_SETTINGS), dm) then
   begin
      Move(dm.dmScale,dmDisplayOrientation,SizeOf(dmDisplayOrientation));
      // swap width and height
      if Odd(dmDisplayOrientation)<>Odd(NewOrientation) then
      begin
       dwTemp := dm.dmPelsHeight;
       dm.dmPelsHeight:= dm.dmPelsWidth;
       dm.dmPelsWidth := dwTemp;
      end;

      if dmDisplayOrientation <> NewOrientation then
      begin
        Move(NewOrientation,dm.dmScale,SizeOf(NewOrientation));

        if (ChangeDisplaySettings(dm, 0)<>DISP_CHANGE_SUCCESSFUL) then
        begin
          RaiseLastOSError;
        end;
      end;
   end;
end;
{$ENDIF}

procedure SetDisplayOrientation(const Orientation: TDisplayOrientation; const DeviceName: String = '');
{$IFDEF MSWINDOWS}
var
  OrientationWord: DWord;
  DeviceNamePChar: PWideChar;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    case Orientation of
      or90: OrientationWord := DMDO_90;
      or180: OrientationWord := DMDO_180;
      or270: OrientationWord := DMDO_270;
    else
      OrientationWord := DMDO_DEFAULT;
    end;

    if DeviceName = '' then
    begin
      DeviceNamePChar := nil;
    end
    else
    begin
      DeviceNamePChar := PWideChar(DeviceName);
    end;

    ChangeOrientationInternal(OrientationWord, DeviceNamePChar);
  {$ELSE}
    raise EPTLibUnsupportedPlatformError.Create('Change orientation does not support the current OS');
  {$ENDIF}
end;

procedure ShowCursor(const DoShow: Boolean);
begin
  {$IFDEF MSWINDOWS}
  WinAPI.Windows.ShowCursor(False);
  {$ENDIF}
end;

end.
