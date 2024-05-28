{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.AwesomeEdit.Classes                            }
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

unit PTLib.VCL.AwesomeEdit.Classes;

interface

uses
  Windows, Classes, SysUtils, Types, Messages, Vcl.Imaging.PngImage,

  Vcl.Graphics, Vcl.Controls, Vcl.StdCtrls, Vcl.Forms,

  Winapi.GDIPAPI, WinAPI.GDIPOBJ,

  PTLib.VCL.GDIPlus,
  PTLib.VCL.Graphics,
  PTLib.VCL.AwesomeEdit.Types,
  PTLib.VCL.AwesomeEdit.Interfaces;

type
  TAwesomeSizeConstraints = class(TPersistent)
  private
    FMaxHeight: TConstraintSize;
    FMaxWidth: TConstraintSize;
    FMinHeight: TConstraintSize;
    FMinWidth: TConstraintSize;
    FOnChange: TNotifyEvent;
    procedure SetConstraints(const Index: Integer;
      const Value: TConstraintSize);
  protected
    procedure Change; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property MaxHeight: TConstraintSize index 0 read FMaxHeight write SetConstraints default 0;
    property MaxWidth: TConstraintSize index 1 read FMaxWidth write SetConstraints default 0;
    property MinHeight: TConstraintSize index 2 read FMinHeight write SetConstraints default 0;
    property MinWidth: TConstraintSize index 3 read FMinWidth write SetConstraints default 0;
  end;

  TAwesomeCollectionItem = class(TCollectionItem)
  strict private
    FAwesomeEdit: IAwesomeEdit;
  protected
    procedure DoUpdated; virtual;
    procedure DoRedraw; virtual;
  public
    constructor Create(Collection: TCollection); override;

    property AwesomeEdit: IAwesomeEdit read FAwesomeEdit;
  end;

  TAwesomeItemCollection = class(TOwnedCollection)
  protected
    procedure SetItem(Index: Integer; Value: TAwesomeCollectionItem);
    function GetItem(Index: Integer): TAwesomeCollectionItem;
  public
    constructor Create(AOwner: TComponent);

    property Items[Index: Integer]: TAwesomeCollectionItem read GetItem write SetItem; default;
    function Add: TAwesomeCollectionItem; virtual;
  end;

  TAwesomePersistent = class(TPersistent)
  strict private
    FOwner: IAwesomeEdit;
    FOnUpdated: TNotifyEvent;
  protected
    procedure DoUpdated; virtual;
    procedure DoRepaint; virtual;
  public
    constructor Create(const AOwner: IAwesomeEdit); virtual;

    property Owner: IAwesomeEdit read FOwner;
  published
    property OnUpdated: TNotifyEvent read FOnUpdated write FOnUpdated;
  end;

  TAwesomeSizeablePersistent = class(TAwesomePersistent)
  strict private
    FMargins: TMargins;
    FWidth: Integer;
    FConstraints: TAwesomeSizeConstraints;
    FState: TAwesomeButtonState;
    FBoundingRect: TRect;
  private
    procedure OnChange(Sender: TObject);

    procedure SetMargins(const Value: TMargins);
    procedure SetWidth(const Value: Integer);
    procedure SetConstraints(const Value: TAwesomeSizeConstraints);
    procedure SetState(const Value: TAwesomeButtonState);
    procedure SetBoundingRect(const Value: TRect);
  protected
    procedure DoGetBoundingRect(var ARect: TRect); virtual;
    procedure DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer); virtual;
    procedure DoPaint(const ACanvas: TGPGraphics; var ARect: TRect); virtual;
    procedure DoDebugPaint(const ACanvas: TGPGraphics; var ARect: TRect);
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;

    function GetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer): Integer;
    function GetBoundingRect(const OuterRect: TRect): TRect;
    procedure PaintTo(const ACanvas: TGPGraphics);

    property State: TAwesomeButtonState read FState write SetState;
    property BoundingRect: TRect read FBoundingRect write SetBoundingRect;
  published
    property Margins: TMargins read FMargins write SetMargins;
    property Width: Integer read FWidth write SetWidth;
    property Constraints: TAwesomeSizeConstraints read FConstraints write SetConstraints;
  end;

  TAwesomeVisiblePersistent = class(TAwesomeSizeablePersistent)
  strict private
    FVisible: Boolean;
  private
    procedure SetVisible(const Value: Boolean);
  published
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TCustomAwesomeColor = class(TAwesomePersistent)
  strict private
    FForegroundStartColor: TColor;
    FForegroundEndColor: TColor;
    FBackgroundStartColor: TColor;
    FBackgroundEndColor: TColor;
    FOpacity: Byte;
    FBorderColor: TColor;
    FRadius: Single;
    FBorderThickness: Single;
  private
    procedure SetForegroundEndColor(const Value: TColor);
    procedure SetOpacity(const Value: Byte);
    procedure SetForegroundStartColor(const Value: TColor);
    procedure SetBorderThickness(const Value: Single);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBackgroundEndColor(const Value: TColor);
    procedure SetBackgroundStartColor(const Value: TColor);
    procedure SetRadius(const Value: Single);
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;
  protected
    property ForegroundStartColor: TColor read FForegroundStartColor write SetForegroundStartColor;
    property ForegroundEndColor: TColor read FForegroundEndColor write SetForegroundEndColor;
    property Opacity: Byte read FOpacity write SetOpacity;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderThickness: Single read FBorderThickness write SetBorderThickness;
    property BackgroundStartColor: TColor read FBackgroundStartColor write SetBackgroundStartColor;
    property BackgroundEndColor: TColor read FBackgroundEndColor write SetBackgroundEndColor;
    property Radius: Single read FRadius write SetRadius;
  end;

  TAwesomeColor = class(TCustomAwesomeColor)
  published
    property ForegroundStartColor;
    property ForegroundEndColor;
    property BackgroundStartColor;
    property BackgroundEndColor;
    property Opacity;
    property BorderColor;
    property BorderThickness;
    property Radius;
  end;

  TAwesomeColorStates = class(TAwesomePersistent)
  strict private
    FNormal: TAwesomeColor;
    FHot: TAwesomeColor;
    FPressed: TAwesomeColor;
    FDisabled: TAwesomeColor;
  private
    procedure SetDisabled(const Value: TAwesomeColor);
    procedure SetHot(const Value: TAwesomeColor);
    procedure SetNormal(const Value: TAwesomeColor);
    procedure SetPressed(const Value: TAwesomeColor);
  protected
    procedure DoGetStateColor(const State: TAwesomeButtonState; out StateColor: TAwesomeColor); virtual;
  public
    function GetStateColor(const State: TAwesomeButtonState): TAwesomeColor;
    procedure SetStateColors(const State: TAwesomeButtonState; const ForegroundColor,
      BackgroundColor: TColor; const Opacity: Byte = 255); overload;
    procedure SetStateColors(const State: TAwesomeButtonState; const ForegroundColorStart,
      ForegroundColorEnd, BackgroundColorStart, BackgroundColorEnd: TColor; const Opacity: Byte = 255); overload;
  published
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;

    property Normal: TAwesomeColor read FNormal write SetNormal;
    property Hot: TAwesomeColor read FHot write SetHot;
    property Pressed: TAwesomeColor read FPressed write SetPressed;
    property Disabled: TAwesomeColor read FDisabled write SetDisabled;
  end;

  TAwesomePicture = class(TAwesomePersistent)
  strict private
    FNormal: TPicture;
    FHighDPI: TPicture;
  private
    procedure SetHighDPI(const Value: TPicture);
    procedure SetNormal(const Value: TPicture);
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;
  published
    property Normal: TPicture read FNormal write SetNormal;
    property HighDPI: TPicture read FHighDPI write SetHighDPI;
  end;

  TAwesomeImageStates = class(TAwesomePersistent)
  strict private
    FNormal: TAwesomePicture;
    FHot: TAwesomePicture;
    FPressed: TAwesomePicture;
    FDisabled: TAwesomePicture;
  private
    procedure SetDisabled(const Value: TAwesomePicture);
    procedure SetHot(const Value: TAwesomePicture);
    procedure SetNormal(const Value: TAwesomePicture);
    procedure SetPressed(const Value: TAwesomePicture);
  protected
    procedure DoGetStateGraphic(const ARect: TRect; const State: TAwesomeButtonState; out Graphic: TGraphic); virtual;
  public
    function GetStateGraphic(const ARect: TRect; const State: TAwesomeButtonState): TGraphic;
    procedure SetImagesFromResource(const ResourceNameNormal, ResourceNameHot,
      ResourceNamePressed, ResourceNameDisabled: String; const HighDPI: Boolean); virtual;
  published
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;

    property Normal: TAwesomePicture read FNormal write SetNormal;
    property Hot: TAwesomePicture read FHot write SetHot;
    property Pressed: TAwesomePicture read FPressed write SetPressed;
    property Disabled: TAwesomePicture read FDisabled write SetDisabled;
  end;

  TAwesomeCloseButton = class(TAwesomeVisiblePersistent)
  strict private
    FColors: TAwesomeColorStates;
  private
    procedure SetColors(const Value: TAwesomeColorStates);
  protected
    procedure DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer); override;
    procedure DoGetBoundingRect(var ARect: TRect); override;
    procedure DoPaint(const ACanvas: TGPGraphics; var ARect: TRect); override;
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;
  published
    property Colors: TAwesomeColorStates read FColors write SetColors;
  end;

  TAwesomeCaption = class(TAwesomeVisiblePersistent)
  strict private
    FColors: TAwesomeColorStates;
    FFont: TFont;
    FText: String;
  private
    procedure SetColors(const Value: TAwesomeColorStates);
    procedure SetText(const Value: String);
  protected
    procedure DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer); override;
    procedure DoPaint(const ACanvas: TGPGraphics; var ARect: TRect); override;
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;
  published
    property Colors: TAwesomeColorStates read FColors write SetColors;
    property Font: TFont read FFont write FFont;
    property Text: String read FText write SetText;
  end;

  TAwesomeImage = class(TAwesomeVisiblePersistent)
  strict private
    FStateImages: TAwesomeImageStates;
    FImageStyle: TAwesomeImageButtonStyle;
  private
    procedure SetStateImages(const Value: TAwesomeImageStates);
    procedure SetImageStyle(const Value: TAwesomeImageButtonStyle);
  protected
    procedure DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer); override;
    procedure DoGetBoundingRect(var ARect: TRect); override;
    procedure DoPaint(const ACanvas: TGPGraphics; var ARect: TRect); override;
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;
  published
    property StateImages: TAwesomeImageStates read FStateImages write SetStateImages;
    property ImageStyle: TAwesomeImageButtonStyle read FImageStyle write SetImageStyle;
  end;

  TAwesomeBackground = class(TAwesomeVisiblePersistent)
  strict private
    FColors: TAwesomeColorStates;
  private
    procedure SetColors(const Value: TAwesomeColorStates);
  protected
    procedure DoPaint(const ACanvas: TGPGraphics; var ARect: TRect); override;
    procedure DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer); override;
    procedure DoGetBoundingRect(var ARect: TRect); override;
  public
    constructor Create(const AOwner: IAwesomeEdit); override;
    destructor Destroy; override;
  published
    property Colors: TAwesomeColorStates read FColors write SetColors;
  end;

  TInternalEdit = class(TCustomEdit)
  private
    procedure ResetProperties;
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;

    function GetFont: TFont;
    procedure SetColor(const Value: TColor);
    procedure Loaded; override;
  published
    property Alignment;
    property AutoSelect;
    property BiDiMode;
    property CharCase;
    property DragCursor;
    property DragKind;
    property DragMode;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property NumbersOnly;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property Text;
    property TextHint;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  PTLib.VCL.AwesomeEdit;

{ TAwesomePersistent }

constructor TAwesomePersistent.Create(
  const AOwner: IAwesomeEdit);
begin
  FOwner := AOwner;
end;

procedure TAwesomePersistent.DoUpdated;
begin
  if Assigned(FOnUpdated) then
  begin
    FOnUpdated(Self);
  end;

  if Assigned(FOwner) then
  begin
    FOwner.PropertyUpdated(Self);
  end;
end;

procedure TAwesomePersistent.DoRepaint;
begin
  if Assigned(FOwner) then
  begin
    FOwner.Invalidate;
  end;
end;

{ TAwesomeColor }

constructor TCustomAwesomeColor.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FForegroundStartColor := clWhite;
  FForegroundEndColor := clWhite;
  FBackgroundStartColor := clBlack;
  FBackgroundEndColor := clBlack;
  FOpacity := 255;
  FBorderColor := clGray;
  FBorderThickness := 1;
  FRadius := 10;
end;

destructor TCustomAwesomeColor.Destroy;
begin

  inherited;
end;

procedure TCustomAwesomeColor.SetBackgroundEndColor(const Value: TColor);
begin
  if FBackgroundEndColor <> Value then
  begin
    FBackgroundEndColor := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetBackgroundStartColor(const Value: TColor);
begin
  if FBackgroundStartColor <> Value then
  begin
    FBackgroundStartColor := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetBorderThickness(const Value: Single);
begin
  if FBorderThickness <> Value then
  begin
    FBorderThickness := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetForegroundEndColor(const Value: TColor);
begin
  if FForegroundEndColor <> Value then
  begin
    FForegroundEndColor := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetRadius(const Value: Single);
begin
  if FRadius <> Value then
  begin
    FRadius := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeColor.SetForegroundStartColor(const Value: TColor);
begin
  if FForegroundStartColor <> Value then
  begin
    FForegroundStartColor := Value;

    DoUpdated;
  end;
end;

{ TAwesomeImageStates<T> }

constructor TAwesomeImageStates.Create(const AOwner: IAwesomeEdit);
begin
  inherited Create(AOwner);

  FNormal := TAwesomePicture.Create(AOwner);
  FHot := TAwesomePicture.Create(AOwner);
  FPressed := TAwesomePicture.Create(AOwner);
  FDisabled := TAwesomePicture.Create(AOwner);
end;

destructor TAwesomeImageStates.Destroy;
begin
  FreeAndNil(FNormal);
  FreeAndNil(FHot);
  FreeAndNil(FPressed);
  FreeAndNil(FDisabled);

  inherited;
end;

procedure TAwesomeImageStates.DoGetStateGraphic(const ARect: TRect;
  const State: TAwesomeButtonState; out Graphic: TGraphic);

  function GetCorrectDPIGraphic(const AwesomePicture: TAwesomePicture): TGraphic;
  begin
    Result := nil;

    if (AwesomePicture.Normal.Graphic <> nil) and
       (AwesomePicture.Normal.Graphic.Width + 2 > ARect.Width) and
       (AwesomePicture.Normal.Graphic.Height + 2> ARect.Height) then
    begin
      Result := AwesomePicture.Normal.Graphic;
    end
    else
    if AwesomePicture.HighDPI.Graphic <> nil then
    begin
      Result := AwesomePicture.HighDPI.Graphic;
    end;
  end;

begin
  Graphic := nil;

  case State of
    bsNormal: Graphic := GetCorrectDPIGraphic(FNormal);
    bsHot: Graphic := GetCorrectDPIGraphic(FHot);
    bsPressed: Graphic := GetCorrectDPIGraphic(FPressed);
  else
    Graphic := GetCorrectDPIGraphic(FNormal);
  end;

  if Graphic = nil then
  begin
    Graphic := GetCorrectDPIGraphic(FNormal);

    if Graphic = nil then
    begin
      if FNormal.HighDPI <> nil then
      begin
        Graphic := FNormal.HighDPI.Graphic;
      end else
      if FNormal.Normal <> nil then
      begin
        Graphic := FNormal.Normal.Graphic;
      end;
    end;
  end;
end;

function TAwesomeImageStates.GetStateGraphic(const ARect: TRect;
  const State: TAwesomeButtonState): TGraphic;
begin
  DoGetStateGraphic(ARect, State, Result);
end;

procedure TAwesomeImageStates.SetImagesFromResource(
  const ResourceNameNormal, ResourceNameHot, ResourceNamePressed, ResourceNameDisabled: String; const HighDPI: Boolean);
var
  Png: TPngImage;
begin
  Png := TPngImage.Create;
  try
    Png.LoadFromResourceName(HInstance, ResourceNameNormal);
    if HighDPI then
    begin
      Normal.HighDPI.Graphic := Png;
    end
    else
    begin
      Normal.Normal.Graphic := Png;
    end;

    Png.LoadFromResourceName(HInstance, ResourceNameHot);
    if HighDPI then
    begin
      Hot.HighDPI.Graphic := Png;
    end
    else
    begin
      Hot.Normal.Graphic := Png;
    end;

    Png.LoadFromResourceName(HInstance, ResourceNamePressed);
    if HighDPI then
    begin
      Pressed.HighDPI.Graphic := Png;
    end
    else
    begin
      Pressed.Normal.Graphic := Png;
    end;

    Png.LoadFromResourceName(HInstance, ResourceNameDisabled);
    if HighDPI then
    begin
      Disabled.HighDPI.Graphic := Png;
    end
    else
    begin
      Disabled.Normal.Graphic := Png;
    end;
  finally
    FreeAndNil(Png);
  end;
end;


procedure TAwesomeImageStates.SetDisabled(const Value: TAwesomePicture);
begin
  FDisabled.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeImageStates.SetHot(const Value: TAwesomePicture);
begin
  FHot.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeImageStates.SetNormal(const Value: TAwesomePicture);
begin
  FNormal.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeImageStates.SetPressed(const Value: TAwesomePicture);
begin
  FPressed.Assign(Value);

  DoUpdated;
end;

{ TAwesomeStatess }

constructor TAwesomeColorStates.Create(const AOwner: IAwesomeEdit);
begin
  inherited Create(AOwner);

  FNormal := TAwesomeColor.Create(AOwner);
  FHot := TAwesomeColor.Create(AOwner);
  FPressed := TAwesomeColor.Create(AOwner);
  FDisabled := TAwesomeColor.Create(AOwner);
end;

destructor TAwesomeColorStates.Destroy;
begin
  FreeAndNil(FNormal);
  FreeAndNil(FHot);
  FreeAndNil(FPressed);
  FreeAndNil(FDisabled);

  inherited;
end;

procedure TAwesomeColorStates.DoGetStateColor(const State: TAwesomeButtonState;
  out StateColor: TAwesomeColor);
begin
  case State of
    bsNormal: StateColor := FNormal;
    bsHot: StateColor := FHot;
    bsPressed: StateColor := FPressed
  else
    StateColor := FDisabled;
  end;
end;

function TAwesomeColorStates.GetStateColor(const State: TAwesomeButtonState): TAwesomeColor;
begin
  DoGetStateColor(State, Result);
end;

procedure TAwesomeColorStates.SetDisabled(const Value: TAwesomeColor);
begin
  FDisabled.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeColorStates.SetHot(const Value: TAwesomeColor);
begin
  FHot.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeColorStates.SetNormal(const Value: TAwesomeColor);
begin
  FNormal.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeColorStates.SetPressed(const Value: TAwesomeColor);
begin
  FPressed.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeColorStates.SetStateColors(const State: TAwesomeButtonState;
  const ForegroundColor, BackgroundColor: TColor; const Opacity: Byte);
begin
  SetStateColors(
    State,
    ForegroundColor,
    ForegroundColor,
    BackgroundColor,
    BackgroundColor,
    Opacity);
end;

procedure TAwesomeColorStates.SetStateColors(const State: TAwesomeButtonState;
  const ForegroundColorStart, ForegroundColorEnd, BackgroundColorStart,
  BackgroundColorEnd: TColor; const Opacity: Byte);
var
  AwesomeColor: TAwesomeColor;
begin
  AwesomeColor := GetStateColor(State);

  AwesomeColor.ForegroundStartColor := ForegroundColorStart;
  AwesomeColor.ForegroundEndColor := ForegroundColorEnd;
  AwesomeColor.BackgroundStartColor := BackgroundColorStart;
  AwesomeColor.BackgroundEndColor := BackgroundColorEnd;
  AwesomeColor.Opacity := Opacity;
end;

{ TAwesomeVisiblePersistent }

procedure TAwesomeVisiblePersistent.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;

    DoUpdated;
  end;
end;

{ TAwesomeCloseButton }

constructor TAwesomeCloseButton.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FColors := TAwesomeColorStates.Create(AOwner);
  FColors.Normal.Opacity := 100;
  FColors.Hot.Opacity := 200;
  FColors.Pressed.Opacity := 255;
  FColors.Disabled.Opacity := 50;

  Constraints.MinWidth := 10;
  Constraints.MinHeight := 10;
  Constraints.MaxHeight := 40;
  Constraints.MaxWidth := 40;
end;

destructor TAwesomeCloseButton.Destroy;
begin
  FreeAndNil(FColors);

  inherited;
end;

procedure TAwesomeCloseButton.DoGetBoundingRect(var ARect: TRect);
begin
  inherited;

  if ARect.Height > ARect.Width then
  begin
    ARect.Height := ARect.Width;
  end else
  if ARect.Width > ARect.Height then
  begin
    ARect.Width := ARect.Height;
  end;
end;

procedure TAwesomeCloseButton.DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer);
begin
  inherited;

  Inc(AWidth, AHeight);
end;

procedure TAwesomeCloseButton.DoPaint(const ACanvas: TGPGraphics;
  var ARect: TRect);
var
  StateColor: TAwesomeColor;
begin
  inherited;

  StateColor := Colors.GetStateColor(State);

  if StateColor <> nil then
  begin
    GPDrawCloseButton(
      ACanvas,
      ARect,
      StateColor.BackgroundStartColor,
      StateColor.ForegroundStartColor,
      FixDPIPixels(Trunc(StateColor.BorderThickness)),
      StateColor.Opacity);
  end;
end;

procedure TAwesomeCloseButton.SetColors(const Value: TAwesomeColorStates);
begin
  FColors.Assign(Value);

  DoUpdated;
end;

{ TTAwesomePicture }

constructor TAwesomePicture.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FNormal := TPicture.Create;
  FHighDPI := TPicture.Create;
end;

destructor TAwesomePicture.Destroy;
begin
  FreeAndNil(FNormal);
  FreeAndNil(FHighDPI);

  inherited;
end;

procedure TAwesomePicture.SetHighDPI(const Value: TPicture);
begin
  FHighDPI.Assign(Value);

  DoUpdated;
end;

procedure TAwesomePicture.SetNormal(const Value: TPicture);
begin
  FNormal.Assign(Value);

  DoUpdated;
end;

{ TAwesomeText }

constructor TAwesomeCaption.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FFont := TFont.Create;
  FColors := TAwesomeColorStates.Create(AOwner);

  FColors.SetStateColors(TAwesomeButtonState.bsNormal, clWindowText, clNone);
  FColors.SetStateColors(TAwesomeButtonState.bsHot, clWindowText, clNone);
  FColors.SetStateColors(TAwesomeButtonState.bsPressed, clWindowText, clNone);
  FColors.SetStateColors(TAwesomeButtonState.bsDisabled, clInactiveCaption, clNone);

  Margins.SetBounds(3, 3, 3, 3);
end;

destructor TAwesomeCaption.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FColors);

  inherited;
end;

procedure TAwesomeCaption.DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer);
begin
  inherited;

  ACanvas.Font.Assign(FFont);

  AWidth := AWidth + ACanvas.TextExtent(FText).cx + FixDPIPixels(Margins.Left) + FixDPIPixels(Margins.Right);
end;

procedure TAwesomeCaption.DoPaint(const ACanvas: TGPGraphics; var ARect: TRect);
var
  Brush: TGPSolidBrush;
  GPFont: TGPFont;
  GPStringFormat: TGPStringFormat;
  Rect: TGPRectF;
  TextRect: TRect;
  StateColor: TAwesomeColor;
begin
  inherited;

  StateColor := Colors.GetStateColor(State);

  if StateColor <> nil then
  begin
    TextRect := ARect;

    Brush := TGPSolidBrush.Create(MakeGDIPColor(
      StateColor.ForegroundStartColor, StateColor.Opacity));
    try
      GPStringFormat := TGPStringFormat.Create;
      try
        GPStringFormat.SetTrimming(StringTrimmingNone);
        GPStringFormat.SetFormatFlags(StringFormatFlagsNoWrap);
        GPStringFormat.SetLineAlignment(StringAlignmentCenter);
        GPStringFormat.SetAlignment(StringAlignmentCenter);

        Rect.X := TextRect.Left;
        Rect.Y := TextRect.Top;
        Rect.Width := TextRect.Width;
        Rect.Height := TextRect.Height;

        GPFont := TGPFont.Create(Font.Name, Font.Size);
        try
          ACanvas.DrawString(
            WideString(Text),
            length(Text),
            GPFont,
            Rect,
            GPStringFormat,
            Brush)
        finally
          FreeAndNil(GPFont);
        end;
      finally
        FreeAndNil(GPStringFormat);
      end;
    finally
      FreeAndNil(Brush);
    end;
  end;
end;

procedure TAwesomeCaption.SetColors(const Value: TAwesomeColorStates);
begin
  FColors.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeCaption.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;

    DoUpdated;
  end;
end;

{ TAwesomeSizeablePersistent }

constructor TAwesomeSizeablePersistent.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FMargins := TMargins.Create(nil);
  FMargins.OnChange := OnChange;
  FConstraints := TAwesomeSizeConstraints.Create;
  FConstraints.OnChange := OnChange;

  FMargins.SetControlBounds(5,5,5,5);
end;

destructor TAwesomeSizeablePersistent.Destroy;
begin
  FreeAndNil(FMargins);
  FreeAndNil(FConstraints);

  inherited;
end;

procedure TAwesomeSizeablePersistent.DoGetBoundingRect(var ARect: TRect);
begin
  // Override
end;

procedure TAwesomeSizeablePersistent.DoGetIdealWidth(const ACanvas: TCanvas;
  const AHeight: Integer; var AWidth: Integer);
begin
  AWidth := 0;
end;

procedure TAwesomeSizeablePersistent.DoDebugPaint(const ACanvas: TGPGraphics;
  var ARect: TRect);
var
  Pen: TGPPen;
  MarginRect: TRect;
  TextBrush: TGPSolidBrush;
  RectText: String;
  GPFont: TGPFont;
  TextRect: TGPRectF;
  GPStringFormat: TGPStringFormat;
begin
  Pen := TGPPen.Create(MakeGDIPColor(clRed, 150));
  try
    ACanvas.DrawRectangle(Pen, MakeRect(ARect));

    MarginRect := Rect(
      ARect.Left + FMargins.Left,
      ARect.Top + FMargins.Top,
      ARect.Right - FMargins.Right,
      ARect.Bottom - FMargins.Bottom);

    Pen.SetColor(MakeGDIPColor(clGreen, 150));

    ACanvas.DrawRectangle(Pen, MakeRect(MarginRect));

    RectText := format('%d,%d,%d,%d',
      [MarginRect.Left, MarginRect.Top, MarginRect.Right, MarginRect.Bottom]);

    GPStringFormat := TGPStringFormat.Create;
    GPFont := TGPFont.Create('Courier New', 6);
    TextBrush := TGPSolidBrush.Create(MakeGDIPColor(clBlack, 255));
    try
      TextRect.X := ARect.Left;
      TextRect.Y := ARect.Top;
      TextRect.Width := ARect.Width;
      TextRect.Height := ARect.Height;

      ACanvas.DrawString(
        WideString(RectText),
        length(RectText),
        GPFont,
        TextRect,
        GPStringFormat,
        TextBrush);
    finally
      FreeAndNil(TextBrush);
      FreeAndNil(GPFont);
      FreeAndNil(GPStringFormat);
    end;
  finally
    FreeAndNil(Pen);
  end;
end;

procedure TAwesomeSizeablePersistent.DoPaint(const ACanvas: TGPGraphics;
  var ARect: TRect);
begin
  ARect.Left := ARect.Left + FMargins.Left;
  ARect.Right := ARect.Right - FMargins.Right;
  ARect.Top := ARect.Top + FMargins.Top;
  ARect.Bottom := ARect.Bottom - FMargins.Bottom;
end;

function TAwesomeSizeablePersistent.GetBoundingRect(const OuterRect: TRect): TRect;
begin
  Result := OuterRect;

  // Reduce by Margins
 (* Result.Left := Result.Left + FixDPIPixels(FMargins.Left);
  Result.Right := Result.Right - FixDPIPixels(FMargins.Right);
  Result.Top := Result.Top + FixDPIPixels(FMargins.Top);
  Result.Bottom := Result.Bottom - FixDPIPixels(FMargins.Bottom);*)

  DoGetBoundingRect(Result);

  if (FConstraints.MaxWidth <> 0) and (Result.Width > FixDPIPixels(FConstraints.MaxWidth)) then
  begin
    Result.Width := FixDPIPixels(FConstraints.MaxWidth);
  end;

  if (FConstraints.MaxWidth <> 0) and (Result.Width > FixDPIPixels(FConstraints.MaxWidth)) then
  begin
    Result.Height := FixDPIPixels(FConstraints.MaxHeight);
  end;

  if (FConstraints.MinWidth <> 0) and (Result.Width < FixDPIPixels(FConstraints.MinWidth)) then
  begin
    Result.Width := FixDPIPixels(FConstraints.MinWidth);
  end;

  if (FConstraints.MinHeight <> 0) and (Result.Height < FixDPIPixels(FConstraints.MinHeight)) then
  begin
    Result.Height := FixDPIPixels(FConstraints.MinHeight);
  end;

  Result := CenteredRect(OuterRect, Result);
end;

function TAwesomeSizeablePersistent.GetIdealWidth(const ACanvas: TCanvas;
  const AHeight: Integer): Integer;
begin
  if Width <> 0 then
  begin
    Result := FixDPIPixels(Width);
  end
  else
  begin
    DoGetIdealWidth(ACanvas, AHeight, Result);
  end;

  if (FConstraints.MaxWidth <> 0) and
     (Result > FixDPIPixels(FConstraints.MaxWidth)) then
  begin
    Result := FixDPIPixels(FConstraints.MaxWidth);
  end;

  if (FConstraints.MinWidth <> 0) and
     (Result < FixDPIPixels(FConstraints.MinWidth)) then
  begin
    Result := FixDPIPixels(FConstraints.MinWidth);
  end;
end;

procedure TAwesomeSizeablePersistent.OnChange(Sender: TObject);
begin
  DoUpdated;
end;

procedure TAwesomeSizeablePersistent.PaintTo(const ACanvas: TGPGraphics);
var
  PaintRect: TRect;
begin
  if (not (Self is TAwesomeVisiblePersistent)) or
     (TAwesomeVisiblePersistent(Self).Visible) then
  begin
    PaintRect := FBoundingRect;

    //DoDebugPaint(ACanvas, PaintRect);
    DoPaint(ACanvas, PaintRect);
  end;
end;

procedure TAwesomeSizeablePersistent.SetBoundingRect(const Value: TRect);
begin
  FBoundingRect := Value;
end;

procedure TAwesomeSizeablePersistent.SetConstraints(
  const Value: TAwesomeSizeConstraints);
begin
  FConstraints.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeSizeablePersistent.SetMargins(const Value: TMargins);
begin
  //FMargins.Assign(Value);

  DoUpdated;
end;

procedure TAwesomeSizeablePersistent.SetState(const Value: TAwesomeButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;

    DoRepaint;
  end;
end;

procedure TAwesomeSizeablePersistent.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;

    DoUpdated;
  end;
end;

{ TAwesomeImage }

constructor TAwesomeImage.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FStateImages := TAwesomeImageStates.Create(AOwner);
  FImageStyle := TAwesomeImageButtonStyle.biStretch;
end;

destructor TAwesomeImage.Destroy;
begin
  FreeAndNil(FStateImages);

  inherited;
end;

procedure TAwesomeImage.DoGetBoundingRect(var ARect: TRect);
var
  Graphic: TGraphic;
begin
  inherited;

  case FImageStyle of
    biDefault,
    biCentre:
      begin
        Graphic := FStateImages.GetStateGraphic(ARect, TAwesomeButtonState.bsNormal);

        if Graphic <> nil then
        begin
          ARect.Width := FixDPIPixels(Graphic.Width);
          ARect.Height := FixDPIPixels(Graphic.Height);
        end;
      end;

    biStretch:
    begin
      ARect.Height := ARect.Width;
    end;
  end;
end;

procedure TAwesomeImage.DoGetIdealWidth(const ACanvas: TCanvas; const AHeight: Integer; var AWidth: Integer);
begin
  inherited;

  AWidth := AWidth + AHeight;
end;

procedure TAwesomeImage.DoPaint(const ACanvas: TGPGraphics; var ARect: TRect);
var
  GPImage: TGPImage;
  AGraphic: TGraphic;
  X, Y: Integer;
  ResizedRect: TRect;
  ImageAttributes: TGPImageAttributes;
begin
  inherited;

  AGraphic := StateImages.GetStateGraphic(ARect, State);

  if AGraphic <> nil then
  begin
    ImageAttributes := TGPImageAttributes.Create;
    GPImage := GraphicToGPImage(AGraphic);
    try
      X := 0;
      Y := 0;

      case ImageStyle of
        biStretch:
          begin
            ResizedRect := ScaleDisplay(
              AGraphic.Width,
              ARect.Width - 2,
              AGraphic.Height,
              ARect.Height - 2,
              False);

            ResizedRect.SetLocation(ARect.Left + 1, ARect.Top + 1);
            ACanvas.DrawImage(GPImage, MakeRect(ResizedRect));
          end;
        else
        begin
          case ImageStyle of
            biCentre:
              begin
                X := ARect.Left + (ARect.Width div 2) - (AGraphic.Width div 2);
                Y := ARect.Top + (ARect.Height div 2) - (AGraphic.Height div 2);
              end;
          end;

          ACanvas.DrawImage(
            GPImage,
            X,
            Y);
        end;
      end;
    finally
      FreeAndNil(GPImage);
      FreeAndNil(ImageAttributes);
    end;
  end;
end;

procedure TAwesomeImage.SetImageStyle(const Value: TAwesomeImageButtonStyle);
begin
  if FImageStyle <> Value then
  begin
    FImageStyle := Value;

    DoUpdated;
  end;
end;

procedure TAwesomeImage.SetStateImages(const Value: TAwesomeImageStates);
begin
  FStateImages.Assign(Value);

  DoUpdated;
end;

{ TAwesomeBackground }

constructor TAwesomeBackground.Create(const AOwner: IAwesomeEdit);
begin
  inherited;

  FColors := TAwesomeColorStates.Create(AOwner);

  FColors.SetStateColors(TAwesomeButtonState.bsNormal, clBlue, clBlue, 200);
  FColors.SetStateColors(TAwesomeButtonState.bsHot, clBlue, clBlue, 225);
  FColors.SetStateColors(TAwesomeButtonState.bsPressed, clBlue, clBlue, 255);
  FColors.SetStateColors(TAwesomeButtonState.bsDisabled, clBlue, clBlue, 50);

  Visible := True;
end;

destructor TAwesomeBackground.Destroy;
begin
  FreeAndNil(FColors);

  inherited;
end;

procedure TAwesomeBackground.DoGetBoundingRect(var ARect: TRect);
begin
  inherited;
end;

procedure TAwesomeBackground.DoGetIdealWidth(const ACanvas: TCanvas;
  const AHeight: Integer; var AWidth: Integer);
begin
  AWidth := AHeight;
end;

procedure TAwesomeBackground.DoPaint(const ACanvas: TGPGraphics;
  var ARect: TRect);
var
  RoundedRect: TGPGraphicsPath;
  Brush: TGPLinearGradientBrush;
  Pen: TGPPen;
  StateColor: TAwesomeColor;
  SmoothingMode: TSmoothingMode;
begin
  inherited;

  StateColor := Colors.GetStateColor(State);

  if StateColor <> nil then
  begin
    SmoothingMode := ACanvas.GetSmoothingMode;

    if SmoothingMode <> TSmoothingMode.SmoothingModeDefault then
    begin
      ACanvas.SetSmoothingMode(SmoothingMode);
    end;
    try
      RoundedRect := CreateRoundRectangle(ARect, FixDPIPixels(Trunc(StateColor.Radius)));
      try
        Brush := TGPLinearGradientBrush.Create(
          MakeRect(ARect),
          MakeGDIPColor(StateColor.BackgroundStartColor, StateColor.Opacity),
          MakeGDIPColor(StateColor.BackgroundEndColor, StateColor.Opacity),
          TLinearGradientMode.LinearGradientModeVertical);
        try
          ACanvas.FillPath(Brush, RoundedRect);
        finally
          FreeAndNil(Brush);
        end;

        if StateColor.BorderThickness > 0 then
        begin
          Pen := TGPPen.Create(MakeGDIPColor(StateColor.BorderColor, StateColor.Opacity));
          try
            Pen.SetWidth(FixDPIPixels(Trunc(StateColor.BorderThickness)));

            ACanvas.DrawPath(Pen, RoundedRect);
          finally
            FreeAndNil(Pen);
          end;
        end;
      finally
        FreeAndNil(RoundedRect);
      end;
    finally
      ACanvas.SetSmoothingMode(SmoothingMode);
    end;
  end;
end;

procedure TAwesomeBackground.SetColors(const Value: TAwesomeColorStates);
begin
  FColors.Assign(Value);

  DoUpdated;
end;

{ TAwesomeCollectionItem }

constructor TAwesomeCollectionItem.Create(Collection: TCollection);
begin
  inherited;

  if not Supports(Collection.Owner, IAwesomeEdit, FAwesomeEdit) then
  begin
    Assert(False, 'Collection Owner must support IAwesomeEdit');
  end;
end;

procedure TAwesomeCollectionItem.DoUpdated;
begin
  FAwesomeEdit.PropertyUpdated(Self);
end;

procedure TAwesomeCollectionItem.DoRedraw;
begin
  FAwesomeEdit.Invalidate;
end;

{ TAwesomeItemCollection }

function TAwesomeItemCollection.Add: TAwesomeCollectionItem;
begin
  Result := TAwesomeCollectionItem(inherited Add);
end;

constructor TAwesomeItemCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TAwesomeCollectionItem);
end;

function TAwesomeItemCollection.GetItem(Index: Integer): TAwesomeCollectionItem;
begin
  Result := inherited GetItem(Index) as TAwesomeCollectionItem;
end;

procedure TAwesomeItemCollection.SetItem(Index: Integer;
  Value: TAwesomeCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TInternalEdit.Change;
begin
  inherited;

  if Owner is TCustomBaseAwesomeEdit then
  begin
    TCustomBaseAwesomeEdit(Owner).EditChanged;
  end;

  ResetProperties;
end;

procedure TInternalEdit.ResetProperties;
begin
  BorderStyle := bsNone;
  ParentFont := True;
  ParentColor := True;
  ParentBackground := True;
end;

constructor TInternalEdit.Create(AOwner: TComponent);
begin
  inherited;

 // ControlStyle := ControlStyle - [csNeedsBorderPaint];
end;

function TInternalEdit.GetFont: TFont;
begin
  Result := Font;
end;

procedure TInternalEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Owner is TCustomBaseAwesomeEdit then
  begin
    TCustomBaseAwesomeEdit(Owner).EditKeyDown(Key, Shift);
  end;
end;

procedure TInternalEdit.KeyPress(var Key: Char);
begin
  inherited;

  if Owner is TCustomBaseAwesomeEdit then
  begin
    TCustomBaseAwesomeEdit(Owner).EditKeyPress(Key);
  end;
end;

procedure TInternalEdit.Loaded;
begin
  inherited;

  ResetProperties;
end;

procedure TInternalEdit.SetColor(const Value: TColor);
begin
  Color := Value;
end;

procedure TInternalEdit.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

{ TAwesomeSizeConstraints }

procedure TAwesomeSizeConstraints.AssignTo(Dest: TPersistent);
begin
  inherited;

end;

procedure TAwesomeSizeConstraints.Change;
begin
  if Assigned(FOnChange) then
  begin
    FOnChange(Self);
  end;
end;

procedure TAwesomeSizeConstraints.SetConstraints(const Index: Integer;
  const Value: TConstraintSize);
begin
  case Index of
    0:
      if Value <> FMaxHeight then
      begin
        FMaxHeight := Value;

        if (Value > 0) and (Value < FMinHeight) then
        begin
          FMinHeight := Value;
        end;

        Change;
      end;
    1:
      if Value <> FMaxWidth then
      begin
        FMaxWidth := Value;

        if (Value > 0) and (Value < FMinWidth) then
        begin
          FMinWidth := Value;
        end;

        Change;
      end;
    2:
      if Value <> FMinHeight then
      begin
        FMinHeight := Value;

        if (FMaxHeight > 0) and (Value > FMaxHeight) then
        begin
          FMaxHeight := Value;
        end;

        Change;
      end;
    3:
      if Value <> FMinWidth then
      begin
        FMinWidth := Value;

        if (FMaxWidth > 0) and (Value > FMaxWidth) then
        begin
          FMaxWidth := Value;
        end;

        Change;
      end;
  end;
end;

end.
