unit PTLib.FMX.MediaPlayer.Helpers.Win;

// Code adapted from https://www.delphipraxis.net/186753-mediaplayer-firemonkey-2.html

interface

uses
  System.Classes,
  System.Types,
  FMX.Media,
  FMX.Controls,
  FMX.Forms,
  FMX.Types
  {$IFDEF MSWINDOWS}
  ,
  Winapi.DirectShow9,
  Winapi.Windows,
  FMX.Media.Win,
  FMX.Platform.Win
  {$ENDIF};

type
  {$IFDEF MSWINDOWS}
  TWindowsMediaHelper = class helper for TWindowsMedia
     procedure Stretch;
  end;
  {$ENDIF}

implementation

{TWindowsMediaHelper}

{$IFDEF MSWINDOWS}
procedure TWindowsMediaHelper.Stretch;
var
  P: TPointF;
  R: TRect;
  Bounds: TRectF;
  Form: TCommonCustomForm;
  // this is just updated to TRecF.Fit to support scaling up
  function MyRectFit ( var R: TRectF; const BoundsRect: TRectF): Single;
  var
    ratio: single;
  begin
    Result := 1;
    if BoundsRect.Width * BoundsRect.Height = 0
    then
      Exit;
    if (R.Width / BoundsRect.Width)> (R.Height / BoundsRect.Height)
    then
      ratio := R.Width / BoundsRect.Width
    else
      ratio := R.Height / BoundsRect.Height;
    // UPDATED
    R := RectF (0, 0, R.Width / ratio, R. Height / ratio);
    Result := ratio;
    RectCenter (R, BoundsRect);
  end;

var
  leFWnd: HWND;
  leControl: TControl;
  leFVMRWindowlessControl: IVMRWindowlessControl9;
begin
  // This is a weird hack bug - https://stackoverflow.com/questions/37351215/how-to-access-a-private-field-from-a-class-helper-in-delphi-10-1-berlin
  With Self do
  begin
    leFWnd := FWnd;
    leControl := TControl(FControl);
    leFVMRWindowlessControl := FVMRWindowlessControl9;
  end;

  if leFWnd <> 0
  then
    begin
      if (leControl <> nil ) and not (csDesigning in Control.ComponentState) and(Control.ParentedVisible) and (Control.Root <> nil ) and
        (Control.Root.GetObject is TCommonCustomForm) then
        begin
          Form := TCommonCustomForm (Control.Root.GetObject);
          P := Self.GetVideoSize;
          Bounds := TRectF.Create(0, 0, P.X, P.Y);
          // UPDATED: Bounds.Fit (RectF (0, 0, Control.AbsoluteWidth, Control.AbsoluteHeight));
          MyRectFit (Bounds, RectF (0, 0, Control.AbsoluteWidth, Control.AbsoluteHeight));
          Bounds.Offset (Control.AbsoluteRect.Left, Control.AbsoluteRect.Top);
          SetParent (leFWnd, FmxHandleToHWND (Form.Handle));
          SetWindowPos (leFWnd, 0, Bounds.Round.Left, Bounds.Round.Top, Bounds.Round.Width,
            Bounds.Round.Height, 0);
          R.Create (0, 0, Bounds.Round.Width, Bounds.Round.Height);

          if leFVMRWindowlessControl <> nil then
            leFVMRWindowlessControl.SetVideoPosition(nil , @R);

          ShowWindow (leFWnd, SW_SHOW)
        end
      else
        ShowWindow (leFWnd, SW_HIDE)
    end;
end;
{$ENDIF}

end.
