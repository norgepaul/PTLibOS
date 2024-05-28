{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Maps                                        }
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

unit PTLib.Common.Maps;

// Functions taken from
// http://www.delphiforfun.org/programs/Math_Topics/Lat-Long%20Distance.htm

interface

uses
  SysUtils, Math;

function SphericalDistanceMetres(lat1, lon1, lat2, lon2: extended): Extended;
function ApproxEllipticalDistanceMetres(llat1, llon1, llat2, llon2: Extended): Extended;
function MetresToDistanceString(Metres: Integer): String;

implementation

const
  FLATTENING: extended = 1.0 / 298.257223563; { Fractional reduction of radius to poles }
  ERAD: extended = 6378.135; { Earth's radius in km at equator }

function deg2rad(deg: extended): extended;
begin
  result := deg * PI / 180.0;
end;

function rad2deg(rad: extended): extended;
begin
  result := rad / PI * 180.0;
end;

function MetresToDistanceString(Metres: Integer): String;
var
  RealKilometres, RealMetres: Integer;
begin
  if Metres = 0 then
    Result := '-'
  else
  begin
    Result := '';

    RealKilometres := Metres div 1000;
    RealMetres := Metres mod 1000;

    if RealKilometres >= 1 then
      Result := format('%dkm ', [RealKilometres]);

    Result := Format('%s%dm', [Result, RealMetres]);
  end;
end;

function ApproxEllipticalDistanceMetres(llat1, llon1, llat2, llon2: Extended): extended;
var
  lat1, lat2, lon1, lon2: extended;
  f, g, l: extended;
  sing, cosl, cosf, sinl, sinf, cosg: extended;
  S, C, W, R, H1, H2, D: extended;
begin
  if (llat1 = llat2) and
     (llon1 = llon2) then
  begin
    Result := 0;
  end
  else
  begin
    lat1 := deg2rad(llat1);
    lon1 := -deg2rad(llon1);
    lat2 := deg2rad(llat2);
    lon2 := -deg2rad(llon2);

    f := (lat1 + lat2) / 2.0;
    g := (lat1 - lat2) / 2.0;
    l := (lon1 - lon2) / 2.0;

    sing := sin(g);
    cosl := cos(l);
    cosf := cos(f);
    sinl := sin(l);
    sinf := sin(f);
    cosg := cos(g);

    S := sing * sing * cosl * cosl + cosf * cosf * sinl * sinl;
    C := cosg * cosg * cosl * cosl + sinf * sinf * sinl * sinl;
    W := arctan2(sqrt(S), sqrt(C));
    R := sqrt((S * C)) / W;
    H1 := (3 * R - 1.0) / (2.0 * C);
    H2 := (3 * R + 1.0) / (2.0 * S);
    D := 2 * W * ERAD;

    result := (D * (1 + FLATTENING * H1 * sinf * sinf * cosg * cosg - FLATTENING *
      H2 * cosf * cosf * sing * sing)) * 1000;
  end;
end;

function SphericalDistanceMetres(lat1, lon1, lat2, lon2: extended): Extended;
var
  theta: extended;
begin
  theta := lon1 - lon2;

  result := sin(deg2rad(lat1)) * sin(deg2rad(lat2)) + cos(deg2rad(lat1)) *
    cos(deg2rad(lat2)) * cos(deg2rad(theta));

  result := rad2deg(Arccos(result)) * 60 * 1.1515;

  result := result * 1609.344 { metres }
end;

end.
