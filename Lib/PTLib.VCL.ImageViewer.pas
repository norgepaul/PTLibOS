unit PTLib.VCL.ImageViewer;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes, System.Types,

  Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Forms,
  Vcl.MPlayer,

  PTLib.Common.Types,
  PTLib.Common.Transformations,

  PTLib.VCL.StretchImage,
  PTLib.VCL.Utils;

type
  TImageViewer = class(TCustomControl)
  private
    FStretch: Boolean;
    FAllowBiggerThanOriginal: Boolean;
    FCenter: Boolean;
    FProportional: Boolean;
    FImageRect: TRect;
    FPicture: TPicture;
    FBackgroundColor: TColor;
    FCanvasBmp: TBitmap;
    FUpdateCount: Integer;
    FPausedUpdateCount: Integer;

    procedure SetStretch(const Value: Boolean);
    procedure SetAllowBiggerThanOriginal(const Value: Boolean);
    procedure SetCenter(const Value: Boolean);
    procedure SetProportional(const Value: Boolean);
    function CalculateImageRect: TRect;
    procedure SetBackgroundColor(const Value: TColor);
    procedure Redraw;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure DrawCanvas; virtual;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBackgroundImage(Image: TBitmap); overload;
    procedure SetBackgroundImage(Image: TGraphic); overload;

    procedure BeginUpdate;
    procedure EndUpdate;

    function BackgroundRect: TRect;
  published
    property Align;
    property PopupMenu;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Visible;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Stretch: Boolean read FStretch write SetStretch;
    property AllowBiggerThanOriginal: Boolean read FAllowBiggerThanOriginal write SetAllowBiggerThanOriginal;
    property Center: Boolean read FCenter write SetCenter;
    property Proportional: Boolean read FProportional write SetProportional;
  end;

implementation

{ TImageViewer }

function TImageViewer.BackgroundRect: TRect;
begin
  Result := FImageRect;
end;

constructor TImageViewer.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csAcceptsControls];

  FPicture := TPicture.Create;

  FCanvasBmp := TBitmap.Create;
  FCanvasBmp.PixelFormat := pf32Bit;

  AllowBiggerThanOriginal := False;
  Center := True;
  Stretch := True;
  Proportional := True;
  DoubleBuffered := True;

  if not (csDesigning in ComponentState) then
    Caption := '';
end;

destructor TImageViewer.Destroy;
begin
  FreeAndNil(FPicture);

  inherited;
end;

procedure TImageViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

procedure TImageViewer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TImageViewer.SetAllowBiggerThanOriginal(const Value: Boolean);
begin
  if FAllowBiggerThanOriginal <> Value then
  begin
    FAllowBiggerThanOriginal := Value;

    Redraw;
  end;
end;

procedure TImageViewer.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;

    Redraw;
  end;
end;

procedure TImageViewer.SetBackgroundImage(Image: TGraphic);
begin
  FPicture.Assign(Image);

  Redraw;
end;

procedure TImageViewer.SetBackgroundImage(Image: TBitmap);
begin
  FPicture.Bitmap := Image;

  Redraw;
end;

procedure TImageViewer.SetCenter(const Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;

    Redraw;
  end;
end;

procedure TImageViewer.BeginUpdate;
begin
  if FUpdateCount = 0 then
    FPausedUpdateCount := 0;

  Inc(FUpdateCount);
end;

function TImageViewer.CalculateImageRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
  x, y: Integer;
begin
  w := FPicture.Width;
  h := FPicture.Height;
  cw := ClientWidth;
  ch := ClientHeight;
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
    Right := w;
    Bottom := h;
  end;

  if Center then
    OffsetRect(Result, (cw - w) div 2, (ch - h) div 2);

  if (not FAllowBiggerThanOriginal) and
     ((Result.Width > FPicture.Width) or
      (Result.Height > FPicture.Height)) then
  begin
    if Center then
    begin
      x := (cw div 2) - (FPicture.Width div 2);
      y := (ch div 2) - (FPicture.Height div 2);
    end
    else
    begin
      x := 0;
      y := 0;
    end;

    Result := Rect(x, y, x + FPicture.Width, y + FPicture.Height);
  end;
end;

procedure TImageViewer.DrawCanvas;
begin
  FCanvasBmp.Width := ClientWidth;
  FCanvasBmp.Height := ClientHeight;

  FImageRect := CalculateImageRect;

  FCanvasBmp.Canvas.Brush.Color := FBackgroundColor;
  FCanvasBmp.Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));

  TVclUtils.StretchDraw(FCanvasBmp.Canvas, FImageRect, FPicture.Graphic);

  BitBlt(Canvas.Handle, 0, 0, FCanvasBmp.Width, FCanvasBmp.Height, FCanvasBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TImageViewer.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);

    if (FUpdateCount = 0) and
       (FPausedUpdateCount > 0) then
    begin
      FPausedUpdateCount := 0;

      Redraw;
    end;
  end;
end;

procedure TImageViewer.PaintWindow(DC: HDC);
begin
  Canvas.Lock;
  try
    Canvas.Handle := DC;
    try
      DrawCanvas;
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TImageViewer.Redraw;
begin
  if FUpdateCount = 0 then
  begin
    Repaint;
  end
  else
  begin
    Inc(FPausedUpdateCount);
  end;
end;

procedure TImageViewer.SetProportional(const Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;

    Redraw;
  end;
end;

procedure TImageViewer.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;

    Redraw;
  end;
end;

end.
