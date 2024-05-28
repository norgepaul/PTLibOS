unit PTLib.VCL.StretchImage;

interface

uses
  System.Classes, System.SysUtils, System.Types,

  WinAPI.Windows, WinAPI.Messages,

  Vcl.ExtCtrls, Vcl.Graphics, Vcl.Controls, Vcl.Forms,

  UxTheme,

  PTLib.VCL.Utils;

type
  TStretchImage = class(TGraphicControl)
  private
    FPicture: TPicture;
    FOnProgress: TProgressEvent;
    FStretch: Boolean;
    FCenter: Boolean;
    FIncrementalDisplay: Boolean;
    FTransparent: Boolean;
    FDrawing: Boolean;
    FProportional: Boolean;
    FAllowBiggerThanOriginal: Boolean;
    FImageRect: TRect;

    function GetCanvas: TCanvas;
    procedure PictureChanged(Sender: TObject);
    procedure SetCenter(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetAllowBiggerThanOriginal(const Value: Boolean);
  protected
    function CanObserve(const ID: Integer): Boolean; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function DoPaletteChange: Boolean;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure Progress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    function CalculateImageRect: TRect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ImageRect: TRect;

    property Canvas: TCanvas read GetCanvas;
  published
    property AllowBiggerThanOriginal: Boolean read FAllowBiggerThanOriginal write SetAllowBiggerThanOriginal;

    property Align;
    property Anchors;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property IncrementalDisplay: Boolean read FIncrementalDisplay write FIncrementalDisplay default False;
    property ParentShowHint;
    property Picture: TPicture read FPicture write SetPicture;
    property PopupMenu;
    property Proportional: Boolean read FProportional write SetProportional default false;
    property ShowHint;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Touch;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
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
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

resourcestring
  SImageCanvasNeedsBitmap = 'Can only modify an image if it contains a bitmap';

constructor TStretchImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csReplicatable, csPannable, csOpaque];
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPicture.OnProgress := Progress;
  Height := 105;
  Width := 105;
end;

destructor TStretchImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

function TStretchImage.GetPalette: HPALETTE;
begin
  Result := 0;
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette;
end;

function TStretchImage.ImageRect: TRect;
begin
  Result := FImageRect;
end;

function TStretchImage.CanObserve(const ID: Integer): Boolean;
begin
  Result := False;
  if ID = TObserverMapping.EditLinkID then
    Result := True;
end;

function TStretchImage.CalculateImageRect: TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
  x, y: Integer;
begin
  w := Picture.Width;
  h := Picture.Height;
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
     ((Result.Width > Picture.Width) or
      (Result.Height > Picture.Height)) then
  begin
    if Center then
    begin
      x := (cw div 2) - (Picture.Width div 2);
      y := (ch div 2) - (Picture.Height div 2);
    end
    else
    begin
      x := 0;
      y := 0;
    end;

    Result := Rect(x, y, x + Picture.Width, y + Picture.Height);
  end;
end;

procedure TStretchImage.Paint;

  procedure DoBufferedPaint(Canvas: TCanvas);
  var
    MemDC: HDC;
    Rect: TRect;
    PaintBuffer: HPAINTBUFFER;
  begin
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      Canvas.Handle := MemDC;
      TVCLUtils.StretchDraw(Canvas, FImageRect, Picture.Graphic);
      BufferedPaintMakeOpaque(PaintBuffer, Rect);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end;

var
  Save: Boolean;
begin
  FImageRect := CalculateImageRect;

  if csDesigning in ComponentState then
    with inherited Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
  Save := FDrawing;
  FDrawing := True;
  try
    if (csGlassPaint in ControlState) and (Picture.Graphic <> nil) and
       not Picture.Graphic.SupportsPartialTransparency then
      DoBufferedPaint(inherited Canvas)
    else
      TVclUtils.StretchDraw(inherited Canvas, FImageRect, Picture.Graphic);
  finally
    FDrawing := Save;
  end;
end;

function TStretchImage.DoPaletteChange: Boolean;
var
  ParentForm: TCustomForm;
  Tmp: TGraphic;
begin
  Result := False;
  Tmp := Picture.Graphic;
  if Visible and (not (csLoading in ComponentState)) and (Tmp <> nil) and
    (Tmp.PaletteModified) then
  begin
    if (Tmp.Palette = 0) then
      Tmp.PaletteModified := False
    else
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and ParentForm.Active and Parentform.HandleAllocated then
      begin
        if FDrawing then
          ParentForm.Perform(wm_QueryNewPalette, 0, 0)
        else
          PostMessage(ParentForm.Handle, wm_QueryNewPalette, 0, 0);
        Result := True;

        Tmp.PaletteModified := False;
      end;
    end;
  end;
end;

procedure TStretchImage.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if FIncrementalDisplay and RedrawNow then
  begin
    if DoPaletteChange then Update
    else Paint;
  end;
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

function TStretchImage.GetCanvas: TCanvas;
var
  Bitmap: TBitmap;
begin
  if Picture.Graphic = nil then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := Width;
      Bitmap.Height := Height;
      Picture.Graphic := Bitmap;
    finally
      Bitmap.Free;
    end;
  end;
  if Picture.Graphic is TBitmap then
    Result := TBitmap(Picture.Graphic).Canvas
  else
    raise EInvalidOperation.Create(SImageCanvasNeedsBitmap);
end;

procedure TStretchImage.SetAllowBiggerThanOriginal(const Value: Boolean);
begin
  FAllowBiggerThanOriginal := Value;

  PictureChanged(Self);
end;

procedure TStretchImage.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    PictureChanged(Self);
  end;
end;

procedure TStretchImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TStretchImage.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    PictureChanged(Self);
  end;
end;

procedure TStretchImage.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    PictureChanged(Self);
  end;
end;

procedure TStretchImage.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    PictureChanged(Self);
  end;
end;

procedure TStretchImage.PictureChanged(Sender: TObject);
var
  G: TGraphic;
  D : TRect;
begin
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkEdit(Observers) then
      TLinkObservers.EditLinkModified(Observers);

  if AutoSize and (Picture.Width > 0) and (Picture.Height > 0) then
  	SetBounds(Left, Top, Picture.Width, Picture.Height);

  G := Picture.Graphic;

  if G <> nil then
  begin
    if not ((G is TMetaFile) or (G is TIcon)) then
      G.Transparent := FTransparent;

    D := CalculateImageRect;

    if (not G.Transparent) and (D.Left <= 0) and (D.Top <= 0) and
       (D.Right >= Width) and (D.Bottom >= Height) then
      ControlStyle := ControlStyle + [csOpaque]
    else  // picture might not cover entire clientrect
      ControlStyle := ControlStyle - [csOpaque];
    if DoPaletteChange and FDrawing then Update;
  end
  else ControlStyle := ControlStyle - [csOpaque];
  if not FDrawing then Invalidate;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    if TLinkObservers.EditLinkIsEditing(Observers) then
      TLinkObservers.EditLinkUpdate(Observers);
end;

function TStretchImage.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := True;
  if not (csDesigning in ComponentState) or (Picture.Width > 0) and
    (Picture.Height > 0) then
  begin
    if Align in [alNone, alLeft, alRight] then
      NewWidth := Picture.Width;
    if Align in [alNone, alTop, alBottom] then
      NewHeight := Picture.Height;
  end;
end;

end.
