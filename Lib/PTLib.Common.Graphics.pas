{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Graphics                                    }
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

unit PTLib.Common.Graphics;

interface

uses
  SysUtils, Types, System.UITypes;

function ScaleRect(const OriginalWidth, NewWidth, OriginalHeight, NewHeight: Integer; const Rotated: Boolean): TRect;
function ColorToAlphaColor(const Value: TColor; const Alpha: Byte = 255): TAlphaColor;

implementation

function ColorToAlphaColor(const Value: TColor; const Alpha: Byte): TAlphaColor;
var
  CREC: TColorRec;
  AREC: TAlphaColorRec;
begin
  CREC.Color := Value;

  AREC.A := Alpha;
  AREC.B := CREC.B;
  AREC.G := CREC.G;
  AREC.R := CREC.R;

  Result := AREC.Color;
end;

function ScaleRect(const OriginalWidth, NewWidth, OriginalHeight, NewHeight: Integer; const Rotated: Boolean): TRect;
const
  Proportional = True;
  Stretch = True;
  Center = True;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  if Rotated then
  begin
    h := OriginalWidth;
    w := OriginalHeight;
  end
  else
  begin
    w := OriginalWidth;
    h := OriginalHeight;
  end;

  cw := NewWidth;
  ch := NewHeight;

  if Stretch or (Proportional and ((w > cw) or (h > ch))) then
  begin
    if Proportional and (w > 0) and (h > 0) then
    begin
      xyaspect := w / h;
      if w > h then
      begin
        w := cw;
        h := Trunc(cw / xyaspect);
        if h > ch then  // woops, too big
        begin
          h := ch;
          w := Trunc(ch * xyaspect);
        end;
      end
      else
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
        if w > cw then  // woops, too big
        begin
          w := cw;
          h := Trunc(cw / xyaspect);
        end;
      end;
    end
    else
    begin
      w := cw;
      h := ch;
    end;
  end;

  with Result do
  begin
    Left := 0;
    Top := 0;

    if Rotated then
    begin
      Right := h;
      Bottom := w;

      if Center then
        OffsetRect(Result, (cw - h) div 2, (ch - w) div 2);
    end
    else
    begin
      Right := w;
      Bottom := h;

      if Center then
        OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);
    end;
  end;
end;

end.
