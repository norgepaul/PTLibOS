unit PTLib.VCL.Utils;

interface

uses
  System.SysUtils, System.Classes, System.NetEncoding, System.Types,

  WinAPI.Windows,

  Vcl.Imaging.Jpeg, Vcl.Graphics, Vcl.GraphUtil, Vcl.Controls;

type
  TVCLUtils = class
  public
    class function TextToJPEG(const Value: String): TJPegImage;
    class procedure StretchDraw(const ACanvas: TCanvas; const DestRect: TRect; const Graphic: TGraphic; const DoStretch: Boolean = True); static;
    class procedure ScaleImage(const Bitmap, ScaledBitmap: TBitmap; const NewHeight, NewWidth: Integer); static;
    class procedure EnableControlAndChildren(Control: TWinControl; DoEnable: Boolean); overload;
    class procedure EnableControlAndChildren(Control: TWinControl; DoEnable: Boolean; const SkipClassNames: Array of String); overload;
    class procedure EnableControls(ParentControl: TWinControl; const Controls: Array of TWinControl; DoEnable: Boolean);
  end;

implementation

class function TVCLUtils.TextToJPEG(const Value: String): TJPegImage;

  function TextToJPEGLegacy(const Value: String): TJPegImage;
  var
    StringStream: TStringStream;
  begin
    StringStream := TStringStream.Create(TNetEncoding.Base64.Decode(Value));
    try
      Result := TJPEGImage.Create;
      try
        StringStream.Position := 0;

        Result.LoadFromStream(StringStream);
      except
        FreeAndNil(Result);

        raise;
      end;
    finally
      FreeAndNil(StringStream);
    end;
  end;

var
  MemStream: TMemoryStream;
  StringStream: TStringStream;
begin
  MemStream := TMemoryStream.Create;
  StringStream := TStringStream.Create(Value);
  try
    StringStream.Position := 0;
    TNetEncoding.Base64.Decode(StringStream, MemStream);

    Result := TJPEGImage.Create;
    try
      MemStream.Position := 0;

      Result.LoadFromStream(MemStream);
    except
      on e: Exception do
      begin
        FreeAndNil(Result);

        if e.Classname = 'EJPEG' then
        begin
          Result := TextToJPEGLegacy(Value);
        end
        else
        begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(MemStream);
    FreeAndNil(StringStream);
  end;
end;

class procedure TVCLUtils.ScaleImage(const Bitmap, ScaledBitmap: TBitmap;
  const NewHeight, NewWidth: Integer);
begin
  ScaledBitmap.Width := NewWidth;
  ScaledBitmap.Height := NewHeight;

  Vcl.GraphUtil.ScaleImage(Bitmap, ScaledBitmap, NewWidth / Bitmap.Width);
end;

class procedure TVCLUtils.StretchDraw(const ACanvas: TCanvas;
  const DestRect: TRect; const Graphic: TGraphic; const DoStretch: Boolean);
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

class procedure TVCLUtils.EnableControlAndChildren(Control: TWinControl; DoEnable: Boolean; const SkipClassNames: Array of String);
var
  i: Integer;
  Skip: Boolean;
begin
  Skip := False;

  for i := Low(SkipClassNames) to High(SkipClassNames) do
    if pos(LowerCase(SkipClassNames[i]), LowerCase(Control.ClassName)) <> 0 then
    begin
      Skip := True;

      Break;
    end;

  if not Skip then
    Control.Enabled := DoEnable;

  for i := 0 to Control.ControlCount - 1 do
    if Control.Controls[i] is TWinControl then
      EnableControlAndChildren(Control.Controls[i] as TWinControl, DoEnable, SkipClassNames);
end;

class procedure TVCLUtils.EnableControlAndChildren(Control: TWinControl; DoEnable: Boolean);
begin
  EnableControlAndChildren(Control, DoEnable, []);
end;

class procedure TVCLUtils.EnableControls(ParentControl: TWinControl; const Controls: Array of TWinControl; DoEnable: Boolean);

  function ContainsControl(Control: TWinControl): Boolean;
  var
    i: Integer;
  begin
    Result := False;

    for i := low(Controls) to high(Controls) do
      if Controls[i] = Control then
        Exit(True);
  end;

var
  i: Integer;
begin
  for i := 0 to pred(ParentControl.ControlCount) do
    if ParentControl.Controls[i] is TWinControl then
    begin
      if ContainsControl(TWinControl(ParentControl.Controls[i])) then
        ParentControl.Controls[i].Enabled := DoEnable;

      EnableControls(ParentControl.Controls[i] as TWinControl, Controls, DoEnable);
    end;
end;


end.

