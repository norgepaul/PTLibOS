unit PTLib.FMX.MediaPlayer.Helpers;

// Code adapted from https://www.delphipraxis.net/186753-mediaplayer-firemonkey-2.html

// Here the routing for the different platforms is done

interface

uses
  FMX.Media;

type
  TMediaPlayerControlHelper = class helper for TMediaPlayerControl
   public
    procedure Stretch;
  end;

  TMediaPlayerHelper = class helper for TMediaPlayer
   public
    procedure Stretch;
  end;

  TMediaHelper = class helper for TMedia
   public
    procedure Stretch;
  end;

implementation

uses
  FMX.Consts
{$IFDEF MSWINDOWS}
    , FMX.Media.Win, PTLib.FMX.MediaPlayer.Helpers.Win
{$ENDIF}
{$IFDEF MACOS}
    , FMX.Media.AVFoundation
{$IFDEF IOS}
    , FMX.Media.iOS //, FMX.Media.iOS.Helpers {TODO}
{$ELSE}
    , FMX.Media.Mac //, FMX.Media.Mac.Helpers {TODO}
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
    , FMX.Media.Android //, FMX.Media.Android.Helpers {TODO}
{$ENDIF ANDROID}
    ;

{TMediaHelper}

procedure TMediaHelper.Stretch;
begin
{$IFDEF MSWINDOWS}
  // Windows
  if Self is TWindowsMedia then
    TWindowsMedia(Self).Stretch;
{$ENDIF}
{$IFDEF MACOS}
{$IFDEF IOS}
  // iOS
{$ELSE}
  // OSX
{$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  // Android
{$ENDIF ANDROID}
end;

{TMediaPlayerHelper}

procedure TMediaPlayerHelper.Stretch;
begin
  if Assigned (Media) and (State = TMediaState.Playing) then
    Media.Stretch;
end;

{TMediaPlayerControlHelper}

procedure TMediaPlayerControlHelper.Stretch;
begin
  if Assigned (MediaPlayer) then
    MediaPlayer.Stretch;
end;

end.
