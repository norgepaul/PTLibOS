{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.Graphics                                       }
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

unit PTLib.VCL.Graphics;

interface

uses
  WinAPI.Messages, WinAPI.Windows,

  System.Classes, System.SysUtils, System.UITypes,

  Vcl.Controls, Vcl.Graphics, Vcl.GraphUtil, Vcl.Imaging.JPEG,
  Vcl.Forms;

type
  TRotation = (
    r0,
    r90,
    r180,
    r270
  );

procedure CopyControlToBitmap(AWinControl: TWinControl; Bitmap: TBitmap; X, Y: Integer);
procedure ScaleImage(Bitmap, ScaledBitmap: TBitmap; NewHeight, NewWidth: Integer);
procedure PaintControlToCanvas(SrcControl: TControl; TargetCanvas: TCanvas);
procedure DrawHTMLText(const ACanvas: TCanvas; var ARect: TRect; const Text: String; NoDraw: Boolean);
function RotateBitmap(const Bitmap: TBitmap; const Rotation: TRotation): TBitmap;
function ColorBetween(const ColorA, ColorB: TColor; const Percent: Integer): TColor;
function GetScreenShotJpg(Scale: Real): TJPEGImage;
procedure ScreenToBitmap(BMP: TBitmap);
function GetPctDiffernceBetweenBitmaps(BMP1, BMP2: TBitmap): Integer;
procedure DrawTextXOR(const ACanvas: TCanvas; const ARect: TRect; const Text: string);
procedure StretchDraw(ACanvas: TCanvas; DestRect: TRect; Graphic: TGraphic; DoStretch: Boolean = True); overload;
procedure StretchDraw(ACanvas: TCanvas; DestRect: TRect; Bitmap: TBitmap; DoStretch: Boolean = True); overload;
function ScaleDisplay(const OriginalWidth, NewWidth, OriginalHeight, NewHeight: Integer; const Rotated: Boolean): TRect;
procedure AdjustJPGSize(const JPEGImage: TJPEGImage; const MaxWidth: Integer;
  const MaxHeight: Integer);
function FixDPIPixels(const PixelCount: Integer): Integer;
function FixFontSize(const Value: Integer): Integer;

implementation

const
  MaxPixelCount = 65536;   // or some other arbitrarily large value

type
  EBitmapError = class(Exception);
  TRGBArray = array[0..MaxPixelCount-1] of TRGBTriple;
  pRGBArray = ^TRGBArray;

function FixFontSize(const Value: Integer): Integer;
begin
  Result := Round(Int64(Value) * Int64(Screen.PixelsPerInch) / 92);
end;

function FixDPIPixels(const PixelCount: Integer): Integer;
begin
  Result := MulDiv(PixelCount, Screen.PixelsPerInch, 96);
end;

procedure StretchDraw(ACanvas: TCanvas; DestRect: TRect; Bitmap: TBitmap; DoStretch: Boolean);
var
  DstBitmap: TBitmap;
begin
  DstBitmap := TBitmap.Create;
  try
    if (not DoStretch) or
       ((Bitmap.Height = DestRect.Height) and
        (Bitmap.Width = DestRect.Width)) then
    begin
      BitBlt(ACanvas.Handle, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY)
    end
    else
    begin
      ScaleImage(Bitmap, DstBitmap, DestRect.Height, DestRect.Width);

      BitBlt(ACanvas.Handle, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, DstBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  finally
    FreeAndNil(DstBitmap);
  end;
end;

procedure StretchDraw(ACanvas: TCanvas; DestRect: TRect; Graphic: TGraphic; DoStretch: Boolean);
var
  SrcBitmap, DstBitmap: TBitmap;
begin
  if Graphic <> nil then
  begin
    SrcBitmap := TBitmap.Create;
    DstBitmap := TBitmap.Create;
    try
      SrcBitmap.Height := Graphic.Height;
      SrcBitmap.Width := Graphic.Width;
      SrcBitmap.Canvas.Draw(0, 0, Graphic);

      if (not DoStretch) or
         ((Graphic.Height = DestRect.Height) and
          (Graphic.Width = DestRect.Width)) then
      begin
        BitBlt(ACanvas.Handle, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, SrcBitmap.Canvas.Handle, 0, 0, SRCCOPY)
      end
      else
      begin
        ScaleImage(SrcBitmap, DstBitmap, DestRect.Height, DestRect.Width);

        BitBlt(ACanvas.Handle, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, DstBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    finally
      FreeAndNil(DstBitmap);
      FreeAndNil(SrcBitmap);
    end;
  end;
end;

procedure DrawTextXOR(const ACanvas: TCanvas; const ARect: TRect; const Text: string);
var
  Bmp: TBitmap;
  RectText: TRect;
  AText: String;
begin
  if ACanvas.TextWidth(Text) < ARect.Width then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Canvas.Font.Assign(ACanvas.Font);

      Bmp.SetSize(ARect.Width, ARect.Height);

      Bmp.Canvas.Brush.Color := clBlack;
      Bmp.Canvas.FillRect(Rect(0, 0, ARect.Width, ARect.Height));
      Bmp.Canvas.Font.Color := clWhite;

      RectText := Rect(0, 0, ARect.Width, ARect.Height);
      AText := Text;

      Bmp.Canvas.TextRect(RectText, AText, [tfCenter]);

      BitBlt(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height, Bmp.Canvas.Handle, 0, 0, SRCINVERT);
    finally
      FreeAndNil(Bmp);
    end;
  end;
end;

procedure ScreenToBitmap(BMP: TBitmap);
var
  DC: HDC;
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crNone;
  DC := GetDC(GetDesktopWindow) ;
  try
    BMP.Width := GetDeviceCaps (DC, HORZRES);
    BMP.Height := GetDeviceCaps (DC, VERTRES);

    BitBlt(BMP.Canvas.Handle, 0, 0, BMP.Width, BMP.Height, DC, 0, 0, SRCCOPY);
  finally
    ReleaseDC(GetDesktopWindow, DC);
    Screen.Cursor := OldCursor;
  end;
end;

function GetPctDiffernceBetweenBitmaps(BMP1, BMP2: TBitmap): Integer;
var
  X, Y: Integer;
  MatchingCount: Int64;
  Pixels1, Pixels2: pRGBArray;
begin
  Result := 100;

  if (BMP1.Height = BMP2.Height) and
     (BMP1.Width = BMP2.Width) then
  begin
    MatchingCount := 0;

    for Y := 0 to BMP1.Height - 1 do
    begin
      // get pointer to the currently iterated row's raw data
      Pixels1 := BMP1.ScanLine[Y];
      Pixels2 := BMP2.ScanLine[Y];

      // iterate the row's pixels from left to right in the whole bitmap width
      for X := 0 to BMP1.Width - 1 do
        if Pixels1[X].rgbtRed * Pixels1[X].rgbtGreen * Pixels1[X].rgbtBlue =
           Pixels2[X].rgbtRed * Pixels2[X].rgbtGreen * Pixels2[X].rgbtBlue then
          Inc(MatchingCount);
    end;

    Result := Trunc((MatchingCount / (BMP1.Height * BMP1.Width)) * 100);
  end;
end;

function GetScreenShotJpg(Scale: Real): TJPEGImage;
var
  BMP, ScaledBMP: TBitmap;
begin
  BMP := TBitmap.Create;
  try
    // Grab the screenshot
    ScreenToBitmap(Bmp);

    // Resize the BMP
    ScaledBMP := TBitmap.Create;
    try
      ScaleImage(BMP, ScaledBMP, Round(Scale * BMP.Width), Round(Scale * BMP.Height));

      Result := TJPEGImage.Create;

      Result.Assign(ScaledBMP);
    finally
      FreeAndNil(ScaledBMP);
    end;
  finally
    FreeAndNil(BMP);
  end;
end;

procedure CopyControlToBitmap(AWinControl: TWinControl; Bitmap: TBitmap; X, Y: Integer);
var
 SrcDC: HDC;
begin
  SrcDC := GetDC(AWinControl.Handle);
  try
    Bitmap.Height := AWinControl.Height;
    Bitmap.Width := AWinControl.Width;

    BitBlt(Bitmap.Canvas.Handle, X, Y, AWinControl.ClientWidth, AWinControl.ClientHeight, SrcDC, 0, 0, SRCCOPY);
  finally
     ReleaseDC(AWinControl.Handle, SrcDC);
  end;
end;

procedure ScaleImage(Bitmap, ScaledBitmap: TBitmap; NewHeight, NewWidth: Integer);
begin
  ScaledBitmap.Width := NewWidth;
  ScaledBitmap.Height := NewHeight;

  Vcl.GraphUtil.ScaleImage(Bitmap, ScaledBitmap, NewWidth / Bitmap.Width);

  //ScaledBitmap.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap)
end;

function ColorBetween(const ColorA, ColorB: TColor; const Percent: Integer): TColor;
var
  R1, G1, B1: Byte;
  R2, G2, B2: Byte;
begin
  R1:= GetRValue(ColorA);
  G1:= GetGValue(ColorA);
  B1:= GetBValue(ColorA);
  R2:= GetRValue(ColorB);
  G2:= GetGValue(ColorB);
  B2:= GetBValue(ColorB);

  Result:= RGB(
    Percent * (R2-R1) div 100 + R1,
    Percent * (G2-G1) div 100 + G1,
    Percent * (B2-B1) div 100 + B1
  );
end;

procedure PaintControlToCanvas(SrcControl: TControl; TargetCanvas: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  if SrcControl.Parent <> nil then
  begin
    DC := TargetCanvas.Handle;

    SaveIndex := SaveDC(DC);
    try
      GetViewportOrgEx(DC, Position);

      SetViewportOrgEx(DC, Position.x - SrcControl.Left, Position.y - SrcControl.Top, nil);

      IntersectClipRect(DC, 0, 0, SrcControl.Parent.ClientWidth, SrcControl.Parent.ClientHeight);

      SrcControl.Parent.Perform(WM_ERASEBKGND, DC, 0);
      SrcControl.Parent.Perform(WM_PAINT, DC, 0);
    finally
      RestoreDC(DC, SaveIndex);
    end;
  end;
end;

function RotateBitmap(const Bitmap: TBitmap; const Rotation: TRotation): TBitmap;

  procedure RotateBmp180(Src, Dest: TBitmap);
  VAR
    i: Integer;
    j: Integer;
    rowIn: pRGBArray;
    rowOut: pRGBArray;
  begin
    Dest.Width := Src.Width;
    Dest.Height := Src.Height;
    Src.PixelFormat := pf24bit;
    Dest.PixelFormat := pf24bit;

    for j := 0 TO Src.Height - 1 do
    begin
      rowIn := Src.ScanLine[j];
      rowOut := Dest.ScanLine[Src.Height - j - 1];

      for i := 0 TO Src.Width - 1 do
        rowOut[Src.Width - i - 1] := rowIn[i]
    end;

    Dest.PixelFormat := pf32bit;
    Src.PixelFormat := pf32bit;
  end;

  procedure RotateBmp90(Src, Dest: TBitmap);
  var
    X, Y: Integer;
    dY: array of PDWORD; // Array for destination scanline
    sH, dH: Integer; // Height variables
    P: PDWORD; // Source pointer
  begin
    try
      Dest.Width := Src.Height;
      Dest.Height := Src.Width;

      sH := Src.Height - 1;
      dH := Dest.Height - 1;

      // Initialize dynamic array
      SetLength(dY, dH + 1);

      // Save pointers to array for acceleration
      for Y := 0 to dH do
        dY[Y] := Dest.ScanLine[Y];

      // Copy Src horizontal lines to be Dest vertical by +90 degree
      for Y := 0 to sH do
      begin
        P := Src.ScanLine[Y];

        for X := dH downto 0 do
        begin
          dY[X]^ := P^;

          Inc(dY[X]);
          Inc(P);
        end;
      end;
    finally
      SetLength(dY, 0);
    end;
  end;

  procedure RotateBmp270(Src, Dest: TBitmap);
  var
    X, Y: Integer;
    dY: array of PDWORD; // Array for destination scanline
    sH, dH: Integer; // Height variables
    P: PDWORD; // Source pixel pointer
  begin
    try
      Dest.Width := Src.Height;
      Dest.Height := Src.Width;

      sH := Src.Height - 1;
      dH := Dest.Height - 1;

      // Initialize dynamic array
      SetLength(dY, dH + 1);

      // Save pointers to array for acceleration
      for Y := 0 to dH do
        dY[Y] := Dest.ScanLine[Y];

      // Copy Src horizontal lines to be Dest vertical by +270 degree
      for Y := sH downto 0 do
      begin
        P := Src.ScanLine[Y];

        for X := 0 to dH do
        begin
          dY[X]^ := P^;

          Inc(dY[X]);
          Inc(P);
        end;
      end;
    finally
      SetLength(dY, 0);
    end;
  end;

begin
 if Bitmap.PixelFormat <> pf32bit then
   Bitmap.PixelFormat := pf32bit;

  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;

  case Rotation of
    r0: Result.Assign(Bitmap);
    r90: RotateBmp90(Bitmap, Result);
    r180: RotateBmp180(Bitmap, Result);
    r270: RotateBmp270(Bitmap, Result);
  end;
end;

procedure DrawHTMLText(const ACanvas: TCanvas; var ARect: TRect; const Text: String; NoDraw: Boolean);
(*DrawHTML - Draws text on a canvas using tags based on a simple subset of HTML/CSS

  <B> - Bold e.g. <B>This is bold</B>
  <I> - Italic e.g. <I>This is italic</I>
  <U> - Underline e.g. <U>This is underlined</U>
  <font-color=x> Font colour e.g.
                <font-color=clRed>Delphi red</font-color>
                <font-color=#FFFFFF>Web white</font-color>
                <font-color=$000000>Hex black</font-color>
  <font-size=x> Font size e.g. <font-size=30>This is some big text</font-size>
  <font-family> Font family e.g. <font-family=Arial>This is arial</font-family>*)

  function CloseTag(const ATag: String): String;
  begin
    Result := concat('/', ATag);
  end;

  function GetTagValue(const ATag: String): String;
  var
    p: Integer;
  begin
    p := pos('=', ATag);

    if p = 0 then
      Result := ''
    else
      Result := copy(ATag, p + 1, MaxInt);
  end;

  function ColorCodeToColor(const Value: String): TColor;
  var
    HexValue: String;
  begin
    Result := 0;

    if Value <> '' then
    begin
      if (length(Value) >= 2) and (copy(Uppercase(Value), 1, 2) = 'CL') then
      begin
        // Delphi colour
        Result := StringToColor(Value);
      end else
      if Value[1] = '#' then
      begin
        // Web colour
        HexValue := copy(Value, 2, 6);

        Result := RGB(StrToInt('$'+Copy(HexValue, 1, 2)),
                      StrToInt('$'+Copy(HexValue, 3, 2)),
                      StrToInt('$'+Copy(HexValue, 5, 2)));
      end
      else
        // Hex or decimal colour
        Result := StrToIntDef(Value, 0);
    end;
  end;

const
  TagBold = 'B';
  TagItalic = 'I';
  TagUnderline = 'U';
  TagBreak = 'BR';
  TagFontSize = 'FONT-SIZE';
  TagFontFamily = 'FONT-FAMILY';
  TagFontColour = 'FONT-COLOR';
  TagColour = 'COLOUR';

var
  x, y, idx, CharWidth, MaxCharHeight: Integer;
  CurrChar: Char;
  Tag, TagValue: String;
  PreviousFontColour: TColor;
  PreviousFontFamily: String;
  PreviousFontSize: Integer;
  PreviousColour: TColor;
begin
  PreviousFontColour := ACanvas.Font.Color;
  PreviousFontFamily := ACanvas.Font.Name;
  PreviousFontSize := ACanvas.Font.Size;
  PreviousColour := ACanvas.Brush.Color;

  x := ARect.Left;
  y := ARect.Top + 1;
  idx := 1;

  MaxCharHeight := ACanvas.TextHeight('Ag');

  While idx <= length(Text) do
  begin
    CurrChar := Text[idx];

    // Is this a tag?
    if CurrChar = '<' then
    begin
      Tag := '';

      inc(idx);

      // Find the end of then tag
      while (Text[idx] <> '>') and (idx <= length(Text)) do
      begin
        Tag := concat(Tag,  UpperCase(Text[idx]));

        inc(idx);
      end;

      ///////////////////////////////////////////////////
      // Simple tags
      ///////////////////////////////////////////////////
      if Tag = TagBold then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold] else

      if Tag = TagItalic then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic] else

      if Tag = TagUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline] else

      if Tag = TagBreak then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end else

      ///////////////////////////////////////////////////
      // Closing tags
      ///////////////////////////////////////////////////
      if Tag = CloseTag(TagBold) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold] else

      if Tag = CloseTag(TagItalic) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic] else

      if Tag = CloseTag(TagUnderline) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline] else

      if Tag = CloseTag(TagFontSize) then
        ACanvas.Font.Size := PreviousFontSize else

      if Tag = CloseTag(TagFontFamily) then
        ACanvas.Font.Name := PreviousFontFamily else

      if Tag = CloseTag(TagFontColour) then
        ACanvas.Font.Color := PreviousFontColour else

      if Tag = CloseTag(TagColour) then
        ACanvas.Brush.Color := PreviousColour else

      ///////////////////////////////////////////////////
      // Tags with values
      ///////////////////////////////////////////////////
      begin
        // Get the tag value (everything after '=')
        TagValue := GetTagValue(Tag);

        if TagValue <> '' then
        begin
          // Remove the value from the tag
          Tag := copy(Tag, 1, pos('=', Tag) - 1);

          if Tag = TagFontSize then
          begin
            PreviousFontSize := ACanvas.Font.Size;
            ACanvas.Font.Size := StrToIntDef(TagValue, ACanvas.Font.Size);
          end else

          if Tag = TagFontFamily then
          begin
            PreviousFontFamily := ACanvas.Font.Name;
            ACanvas.Font.Name := TagValue;
          end;

          if Tag = TagFontColour then
          begin
            PreviousFontColour := ACanvas.Font.Color;

            try
              ACanvas.Font.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end else

          if Tag = TagColour then
          begin
            PreviousColour := ACanvas.Brush.Color;

            try
              ACanvas.Brush.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end;
        end;
      end;
    end
    else
    // Draw the character if it's not a ctrl char
    if CurrChar >= #32 then
    begin
      CharWidth := ACanvas.TextWidth(CurrChar);

      if x + CharWidth > ARect.Right then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end;

      if (not NoDraw) and (y + MaxCharHeight <= ARect.Bottom) then
      begin
        //ACanvas.Brush.Style := bsClear;

        ACanvas.TextOut(x, y, CurrChar);
      end;

      x := x + CharWidth;
    end;

    inc(idx);
  end;

  ARect := Rect(ARect.Left, ARect.Top, ARect.Right, y + MaxCharHeight)
end;

function ScaleDisplay(const OriginalWidth, NewWidth, OriginalHeight, NewHeight: Integer; const Rotated: Boolean): TRect;
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

procedure ResizeBitmap(abmp:TBitmap; NuWidth,NuHeight:integer);
var
  xscale, yscale         : Single;
  sfrom_y, sfrom_x       : Single;
  ifrom_y, ifrom_x       : Integer;
  to_y, to_x             : Integer;
  weight_x, weight_y     : array[0..1] of Single;
  weight                 : Single;
  new_red, new_green     : Integer;
  new_blue               : Integer;
  total_red, total_green : Single;
  total_blue             : Single;
  ix, iy                 : Integer;
  bTmp : TBitmap;
  sli, slo : pRGBArray;
begin
  abmp.PixelFormat := pf24bit;
  bTmp := TBitmap.Create;
  bTmp.PixelFormat := pf24bit;
  bTmp.Width := NuWidth;
  bTmp.Height := NuHeight;
  xscale := bTmp.Width / (abmp.Width-1);
  yscale := bTmp.Height / (abmp.Height-1);
  for to_y := 0 to bTmp.Height-1 do begin
    sfrom_y := to_y / yscale;
    ifrom_y := Trunc(sfrom_y);
    weight_y[1] := sfrom_y - ifrom_y;
    weight_y[0] := 1 - weight_y[1];
    for to_x := 0 to bTmp.Width-1 do begin
      sfrom_x := to_x / xscale;
      ifrom_x := Trunc(sfrom_x);
      weight_x[1] := sfrom_x - ifrom_x;
      weight_x[0] := 1 - weight_x[1];
      total_red   := 0.0;
      total_green := 0.0;
      total_blue  := 0.0;
      for ix := 0 to 1 do begin
        for iy := 0 to 1 do begin
          sli := abmp.Scanline[ifrom_y + iy];
          new_red := sli[ifrom_x + ix].rgbtRed;
          new_green := sli[ifrom_x + ix].rgbtGreen;
          new_blue := sli[ifrom_x + ix].rgbtBlue;
          weight := weight_x[ix] * weight_y[iy];
          total_red   := total_red   + new_red   * weight;
          total_green := total_green + new_green * weight;
          total_blue  := total_blue  + new_blue  * weight;
        end;
      end;
      slo := bTmp.ScanLine[to_y];
      slo[to_x].rgbtRed := Round(total_red);
      slo[to_x].rgbtGreen := Round(total_green);
      slo[to_x].rgbtBlue := Round(total_blue);
    end;
  end;
  abmp.Width := bTmp.Width;
  abmp.Height := bTmp.Height;
  abmp.Canvas.Draw(0,0,bTmp);
  bTmp.Free;
end;

procedure AdjustJPGSize(const JPEGImage: TJPEGImage; const MaxWidth: Integer;
  const MaxHeight: Integer);
var
  BitmapSrc, BitmapDst: TBitmap;
  NewRect: TRect;
begin
  if (JPEGImage.Width > MaxWidth) or
     (JPEGImage.Height > MaxHeight) then
  begin
    NewRect := ScaleDisplay(
      JPEGImage.Width,
      MaxWidth,
      JPEGImage.Height,
      MaxHeight,
      False);

    BitmapDst := TBitmap.Create;
    BitmapSrc := TBitmap.Create;
    try
      // Draw the JPEG to the src bitmap
      BitmapSrc.Height := JPEGImage.Height;
      BitmapSrc.Width := JPEGImage.Width;
      BitmapSrc.Canvas.Draw(0, 0, JPEGImage);

      ScaleImage(
        BitmapSrc,
        BitmapDst,
        NewRect.Height,
        NewRect.Width);

      JPEGImage.Assign(BitmapDst);
    finally
      FreeAndNil(BitmapSrc);
      FreeAndNil(BitmapDst);
    end;
  end;
end;

end.
