{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.OpenStreetMaps                              }
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

unit PTLib.Common.OpenStreetMaps;

interface

uses
  SysUtils, Classes,

  IdHTTP;

type
  TOnFinished = procedure(Sender: TObject; const AStream: TStream; const Error: String) of object;

  TOpenStreetMapsBitmapThread = class(TThread)
  private
    FLatitude: Double;
    FLongitude: Double;
    FZoomLevel: Integer;
    FParameters: String;
    FHeight: Integer;
    FWidth: Integer;
    FImageStream: TMemoryStream;
    FError: String;
  public
    constructor Create(const Latitude: Double; const Longitude: Double; const Height, Width, ZoomLevel: Integer; const Parameters: String);
    destructor Destroy; override;

    procedure Execute; override;

    property Error: String read FError;
    property ImageStream: TMemoryStream read FImageStream;
  end;

  TOpenStreetMapsBitmap = class(TComponent)
  private
    FOnFinished: TOnFinished;

    FOpenStreetMapsBitmapThread: TOpenStreetMapsBitmapThread;

    procedure OnThreadTerminate(Sender: TObject);
  protected
    procedure DoOnFinished(const AStream: TStream; const Error: String); virtual;
  public
    procedure LoadMap(const Latitude, Longitude: Double; const Height: Integer = 800; const Width: Integer = 600; const ZoomLevel: Integer = 17; const Parameters: String = '');
  published
    property OnFinished: TOnFinished read FOnFinished write FOnFinished;
  end;

implementation

uses
  PTLib.Common.Utils;

{ TOpenStreetMapsBitmapThread }

constructor TOpenStreetMapsBitmapThread.Create(const Latitude,
  Longitude: Double; const Height, Width, ZoomLevel: Integer;
  const Parameters: String);
begin
  inherited Create(True);

  FImageStream := TMemoryStream.Create;

  FLatitude := Latitude;
  FLongitude := Longitude;
  FHeight := Height;
  FWidth := Width;
  FZoomLevel := ZoomLevel;
  FParameters := Parameters;
end;

destructor TOpenStreetMapsBitmapThread.Destroy;
begin
  FreeAndNil(FImageStream);

  inherited;
end;

procedure TOpenStreetMapsBitmapThread.Execute;
const
  OpenMapsPNGURL = 'http://staticmap.openstreetmap.de/staticmap.php?center=%s,%s&zoom=%d&size=%dx%d';
var
  HTTPGet: TIdHTTP;
  URL: String;
begin
  HTTPGet := TIdHTTP.Create(nil);
  try
    HTTPGet.ConnectTimeout := 10000;

    URL := format(OpenMapsPNGURL, [FloatToStrUseDot(FLatitude),
                                   FloatToStrUseDot(FLongitude),
                                   FZoomLevel,
                                   FWidth,
                                   FHeight]);

    try
      HTTPGet.Get(URL, FImageStream);

      FImageStream.Position := 0;
    except
      on e: Exception do
        FError := e.Message;
    end;
  finally
    FreeAndNil(HTTPGet);
  end;
end;

{ TOpenStreetMapsBitmap }

procedure TOpenStreetMapsBitmap.DoOnFinished(const AStream: TStream;
  const Error: String);
begin
  if Assigned(FOnFinished) then
    FOnFinished(
      Self,
      AStream,
      Error);
end;

procedure TOpenStreetMapsBitmap.LoadMap(const Latitude, Longitude: Double;
  const Height, Width, ZoomLevel: Integer; const Parameters: String);
begin
  if FOpenStreetMapsBitmapThread = nil then
  begin
    FOpenStreetMapsBitmapThread := TOpenStreetMapsBitmapThread.Create(Latitude,
                                                                      Longitude,
                                                                      Height,
                                                                      Width,
                                                                      ZoomLevel,
                                                                      Parameters);
    FOpenStreetMapsBitmapThread.OnTerminate := OnThreadTerminate;
    FOpenStreetMapsBitmapThread.Start;
  end;
end;

procedure TOpenStreetMapsBitmap.OnThreadTerminate(Sender: TObject);
begin
  try
    DoOnFinished(
      FOpenStreetMapsBitmapThread.ImageStream,
      FOpenStreetMapsBitmapThread.Error);
  finally
    FOpenStreetMapsBitmapThread := nil;
  end;
end;

end.

