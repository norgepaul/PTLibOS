{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Transformations                             }
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

unit PTLib.Common.Transformations;

interface

uses
  Types,

  PTLib.Common.Types;

function ScaleRect(const Rect1: TRect; Rect2: TRect; OriginalWidth, OriginalHeight: Integer): TRect;
function RotateRect(ParentRect, ARect: TRect; Rotation: TRectRotation): TRect;
function SameRect(Rect1, Rect2: TRect): Boolean;

implementation

function SameRect(Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left = Rect2.Left) and
            (Rect1.Top = Rect2.Top) and
            (Rect1.Right = Rect2.Right) and
            (Rect1.Bottom = Rect2.Bottom);
end;

function ScaleRect(const Rect1: TRect; Rect2: TRect; OriginalWidth, OriginalHeight: Integer): TRect;
begin
  Result.Left := Round((Rect2.Left * (Rect1.Width / OriginalWidth)));
  Result.Right := Round((Rect2.Right * (Rect1.Width / OriginalWidth)));
  Result.Top := Round((Rect2.Top * (Rect1.Height / OriginalHeight)));
  Result.Bottom := Round((Rect2.Bottom * (Rect1.Height / OriginalHeight)));
end;

function RotateRect(ParentRect, ARect: TRect; Rotation: TRectRotation): TRect;
begin
  case Rotation of
    rr0: Result := ARect;

    rr90: Result := Rect(ARect.Top,
                          ParentRect.Bottom - ARect.Right,
                          ARect.Bottom,
                          ParentRect.Bottom - ARect.Left);

    rr180: Result := Rect(ParentRect.Right - ARect.Right,
                          ParentRect.Bottom - ARect.Bottom,
                          ParentRect.Right - ARect.Left,
                          ParentRect.Bottom - ARect.Top);

    rr270: Result := Rect(ParentRect.Right - ARect.Bottom,
                         ARect.Left,
                         ParentRect.Right - ARect.Top,
                         ARect.Right);
  end;
end;

end.

