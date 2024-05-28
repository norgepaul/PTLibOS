{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.View.HourGlass                              }
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

unit PTLib.VCL.HourGlass;

Interface

Uses
  Vcl.Controls, Vcl.Forms;

procedure ShowHourGlass;
procedure HideHourGlass;
procedure ForceHideHourGlass;

Var
  FHourGlassCount: Integer = 0;
  DisableHourGlass: Boolean = FALSE;

Implementation

procedure ShowHourGlass;
begin
  if not DisableHourGlass then
    Screen.Cursor := crHourGlass;
  Inc(FHourGlassCount);
end;

procedure HideHourGlass;
begin
  Dec(FHourGlassCount);

  if FHourGlassCount < 0 then
    FHourGlassCount := 0;

  if FHourGlassCount = 0 then
    Screen.Cursor := crDefault;
end;

procedure ForceHideHourGlass;
begin
  FHourGlassCount := 0;
  Screen.Cursor := crDefault;
end;

end.
