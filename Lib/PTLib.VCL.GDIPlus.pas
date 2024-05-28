{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.GDIPlus                                        }
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

unit PTLib.VCL.GDIPlus;

interface

uses
  Classes, WinAPI.Windows, System.UITypes, System.Types, SysUtils,

  Vcl.Graphics,

  GDIPObj, GDIPAPI,

  PTLib.VCL.GDIPlus.Interfaces;

type
  TGDIHelper = class(TInterfacedObject, IGDIHelper)
  strict private
    FGPGraphics: TGPGraphics;
    FOwnsGPGraphics: Boolean;
  protected
    property GPGraphics: TGPGraphics read FGPGraphics;
  protected
    function DrawCloseButton(const ARect: TRect; const Color, BackgroundColor: TColor; const PenThickness: Single; const Opacity: Integer = 255): IGDIHelper;
  public
    constructor Create(const Canvas: TCanvas); overload;
    constructor Create(const GPGraphics: TGPGraphics); overload;
    destructor Destroy; override;
  end;

function GPCreateSolidBrush(const Color: TColor; const Opacity: Integer = 255): TGPSolidBrush;

function MakeGDIPColor(const C: TColor; const Alpha: Byte): Cardinal;
function CreateRoundRectangle(const ARect: TRect; const Radius: integer): TGPGraphicsPath;
procedure GPFillRectangle(const ACanvas: TGPGraphics; const ARect: TRect; const Color: TColor; const Opacity: Integer = 255);
function BitmapToGPBitmap(Bitmap: TBitmap): TGPBitmap;
function GraphicToGPImage(const Graphic: TGraphic): TGPImage;
function RectToGPRectF(ARect: TRect): TGPRectF;
procedure GPDrawCloseButton(const ACanvas: TGPGraphics; const ARect: TRect;
  const Color, BackgroundColor: TColor; const PenThickness: Single; const Opacity: Integer = 255);
function CentreRect(const OuterRect, InnerRect: TRect): TRect;

implementation

function CentreRect(const OuterRect, InnerRect: TRect): TRect;
begin
  Result.Left := Trunc(OuterRect.Left + (OuterRect.Width - InnerRect.Width) / 2);
  Result.Top := Trunc(OuterRect.Top + (OuterRect.Height - InnerRect.Height) / 2);
  Result.Width := InnerRect.Width;
  Result.Height := InnerRect.Height;
end;

procedure GPDrawCloseButton(const ACanvas: TGPGraphics; const ARect: TRect;
  const Color, BackgroundColor: TColor; const PenThickness: Single; const Opacity: Integer);
var
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  ButtonRect: TRect;
  CrossResize: Integer;
begin
  ButtonRect := ARect;

  Brush := TGPSolidBrush.Create(MakeGDIPColor(Color, Opacity));
  try
    ACanvas.FillEllipse(
      Brush,
      ButtonRect.Left,
      ButtonRect.Top,
      ButtonRect.Width,
      ButtonRect.Height);
  finally
    FreeAndNil(Brush);
  end;

  CrossResize := Round((ARect.Width / 100) * 30);

  ButtonRect.Inflate(-CrossResize, -CrossResize);

  Pen := TGPPen.Create(MakeGDIPColor(BackgroundColor, 255));
  try
    Pen.SetWidth(PenThickness);
    ACanvas.DrawLine(Pen, ButtonRect.Left, ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
    ACanvas.DrawLine(Pen, ButtonRect.Left, ButtonRect.Bottom, ButtonRect.Right, ButtonRect.Top);
  finally
    FreeAndNil(Pen);
  end;
end;

function MakeGDIPColor(const C: TColor; const Alpha: Byte): Cardinal;
var
  tmpRGB : TColorRef;
begin
  tmpRGB := ColorToRGB(C);

  result := ((DWORD(GetBValue(tmpRGB)) shl  BlueShift) or
             (DWORD(GetGValue(tmpRGB)) shl GreenShift) or
             (DWORD(GetRValue(tmpRGB)) shl   RedShift) or
             (DWORD(Alpha) shl AlphaShift));
end;

function RectToGPRectF(ARect: TRect): TGPRectF;
begin
  Result.X := ARect.Left;
  Result.Y := ARect.Top;
  Result.Width := RectWidth(ARect);
  Result.Height := RectHeight(ARect);
end;

function CreateRoundRectangle(const ARect: TRect;
  const Radius: integer): TGPGraphicsPath;
var
  path : TGPGraphicsPath;
  l, t, w, h, d : integer;
  Rectangle: TGPRect;
begin
  Rectangle := MakeRect(ARect);

  path := TGPGraphicsPath.Create;
  l := rectangle.X;
  t := rectangle.y;
  w := rectangle.Width;
  h := rectangle.Height;
  d := radius div 2; // divide by 2

  // the lines beween the arcs are automatically added by the path
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  path.CloseFigure();
  result := path;
end;

function GPCreateSolidBrush(const Color: TColor; const Opacity: Integer): TGPSolidBrush;
begin
  Result := TGPSolidBrush.Create(MakeGDIPColor(Color, Opacity));
end;

procedure GPFillRectangle(const ACanvas: TGPGraphics;const ARect: TRect;
  const Color: TColor; const Opacity: Integer);
var
  Brush: TGPBrush;
begin
  Brush := GPCreateSolidBrush(Color, Opacity);
  try
    ACanvas.FillRectangle(Brush, MakeRect(ARect));
  finally
    FreeAndNil(Brush);
  end;
end;

function GraphicToGPImage(const Graphic: TGraphic): TGPImage;
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    Graphic.SaveToStream(MemStream);

    MemStream.Position := 0;
  except
    FreeAndNil(MemStream);

    raise;
  end;

  Result := TGPImage.Create(TStreamAdapter.Create(MemStream, soOwned));
end;

function BitmapToGPBitmap(Bitmap: TBitmap): TGPBitmap;
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(MemStream);

    MemStream.Position := 0;
  except
    FreeAndNil(MemStream);

    raise;
  end;

  Result := TGPBitmap.Create(TStreamAdapter.Create(MemStream, soOwned));
end;

{ TGDIHelper }

constructor TGDIHelper.Create(const Canvas: TCanvas);
begin
  FGPGraphics := TGPGraphics.Create(Canvas.Handle);
  FOwnsGPGraphics := True;
end;

constructor TGDIHelper.Create(const GPGraphics: TGPGraphics);
begin
  FGPGraphics := GPGraphics;
  FOwnsGPGraphics := False;
end;

destructor TGDIHelper.Destroy;
begin
  if FOwnsGPGraphics then
  begin
    FreeAndNil(FGPGraphics);
  end;

  inherited;
end;

function TGDIHelper.DrawCloseButton(const ARect: TRect; const Color,
  BackgroundColor: TColor; const PenThickness: Single;
  const Opacity: Integer): IGDIHelper;
var
  Pen: TGPPen;
  Brush: TGPSolidBrush;
  ButtonRect: TRect;
  CrossResize: Integer;
begin
  Result := Self;

  ButtonRect := ARect;

  Brush := TGPSolidBrush.Create(MakeGDIPColor(Color, Opacity));
  try
    GPGraphics.FillEllipse(
      Brush,
      ButtonRect.Left,
      ButtonRect.Top,
      ButtonRect.Width,
      ButtonRect.Height);
  finally
    FreeAndNil(Brush);
  end;

  CrossResize := Round((ARect.Width / 100) * 30);

  ButtonRect.Inflate(-CrossResize, -CrossResize);

  Pen := TGPPen.Create(MakeGDIPColor(BackgroundColor, 255));
  try
    Pen.SetWidth(PenThickness);
    GPGraphics.DrawLine(Pen, ButtonRect.Left, ButtonRect.Top, ButtonRect.Right, ButtonRect.Bottom);
    GPGraphics.DrawLine(Pen, ButtonRect.Left, ButtonRect.Bottom, ButtonRect.Right, ButtonRect.Top);
  finally
    FreeAndNil(Pen);
  end;
end;

end.
