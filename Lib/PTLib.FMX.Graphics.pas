{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.FMX.Graphics                                       }
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

unit PTLib.FMX.Graphics;

interface

uses
  System.SysUtils, System.UITypes,

  FMX.Platform, FMX.Types, FMX.Graphics

  {$IFDEF ZXIngQRCode}
  ,DelphiZXIngQRCode
  {$ENDIF}
  ;

type
  {$IFNDEF ZXIngQRCode}
  TQRCodeEncoding = (
    qrAuto
  );
  {$ENDIF}

  TFMXGraphics = class
    class procedure GenerateQRCode(const Text: String; const DstBitmap: TBitmap; const QRCodeEncoding: TQRCodeEncoding = qrAuto;
      const Scale: Integer = 1; const QuietZone: Integer = 4; const ColourOn: TAlphaColor = TAlphaColorRec.Black;
      const ColourOff: TAlphaColor = TAlphaColorRec.White);
    class procedure SetCursor(const Cursor: TCursor);
  end;

implementation

class procedure TFMXGraphics.SetCursor(const Cursor: TCursor);
var
  CS: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;

    if Assigned(CS) then
    begin
      CS.SetCursor(Cursor);
    end;
  end;
end;

class procedure TFMXGraphics.GenerateQRCode(const Text: String; const DstBitmap: TBitmap; const QRCodeEncoding: TQRCodeEncoding;
  const Scale: Integer; const QuietZone: Integer; const ColourOn: TAlphaColor; const ColourOff: TAlphaColor);
{$IFNDEF ZXIngQRCode}
begin
  Assert(False, 'GenerateQRCode requires that you define the conditional define constant "ZXIngQRCode" and have a library path to the DelphiZXIngCode libary - https://github.com/foxitsoftware/DelphiZXingQRCode');
end;
{$ELSE}
  procedure DrawPixel(const Data: TBitmapData; const Column, Row: Integer; const Colour: TAlphaColor);
  var
    i, n: Integer;
  begin
    for i := 0 to pred(Scale) do
    begin
      for n := 0 to pred(Scale) do
      begin
        Data.SetPixel((Column * Scale) + i, (Row * Scale) + n, Colour);
      end;
    end;
  end;

var
  QRCode: TDelphiZXingQRCode;
  Row, Column: Integer;
  Data: TBitmapData;
begin
  QRCode := TDelphiZXingQRCode.Create;
  try
    QRCode.Data := Text;
    QRCode.Encoding := QRCodeEncoding;
    QRCode.QuietZone := QuietZone;

    DstBitmap.Height := QRCode.Rows * Scale;
    DstBitmap.Width := QRCode.Columns * Scale;

    DstBitmap.Map(TMapAccess.Write, Data);
    try
      for Row := 0 to QRCode.Rows - 1 do
      begin
        for Column := 0 to QRCode.Columns - 1 do
        begin
          if (QRCode.IsBlack[Row, Column]) then
          begin
            DrawPixel(Data, Column, Row, ColourOn);
          end else
          begin
            DrawPixel(Data, Column, Row, ColourOff);
          end;
        end;
      end;
    finally
      DstBitmap.Unmap(Data);
    end;
  finally
    FreeAndNil(QRCode);
  end;
end;
{$ENDIF}

end.
