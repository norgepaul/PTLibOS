{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.AwesomeEdit                                    }
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

unit PTLib.VCL.AwesomeEdit;

interface

uses
  WinAPI.Windows, WinAPI.Messages, WinAPI.UxTheme,

  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,

  Vcl.Menus, Vcl.Graphics,

  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Themes,

  GDIPObj, GDIPAPI,

  PTLib.VCL.GDIPlus,
  PTLib.VCL.Graphics,
  PTLib.VCL.AwesomeEdit.Classes,
  PTLib.VCL.AwesomeEdit.Interfaces,
  PTLib.VCL.AwesomeEdit.Types;

type
  TOnPaint = procedure(Sender: TObject; const ACanvas: TGPGraphics; const ARect: TRect; const AHandled: Boolean) of object;
  TButtonClickEvent = procedure(Sender: TObject; const ClickArea: TAwesomeButtonEditHitTestArea; var Handled: Boolean) of object;

  TCustomAwesomeButton = class(TAwesomeCollectionItem)
  strict private
    // Events
    FOnVisibilityChanged: TNotifyEvent;
    FOnClick: TButtonClickEvent;
    FOnPaint: TOnPaint;

    // Published Properties
    FBackground: TAwesomeBackground;
    FImage: TAwesomeImage;
    FCaption: TAwesomeCaption;
    FCloseButton: TAwesomeCloseButton;
    FEnabled: Boolean;
    FPosition: TAwesomeButtonPosition;
    FVisible: Boolean;
    FMargins: TMargins;
    FWidth: Integer;
    FPopupMenu: TPopupMenu;
    FCursor: TCursor;
    FSmoothingMode: TSmoothingMode;
    FUseAllSpace: Boolean;
    FAutoWidth: Boolean;

    // Public Propeties
    FState: TAwesomeButtonState;
    FBounds: TRect;
    FOuterBounds: TRect;
  private
    FInternalVisible: Boolean;
    FInternalButton: Boolean;

    function GetButtonWidth(const ACanvas: TCanvas; const AHeight: Integer): Integer;
    procedure Click(const Pt: TPoint; var Handled: Boolean);

    function IsHit(const Pt: TPoint): Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetState(const Value: TAwesomeButtonState);
    procedure SetVisible(const Value: Boolean);
    procedure SetBounds(const Value: TRect);
    procedure SetPosition(const Value: TAwesomeButtonPosition);
    procedure SetWidth(const Value: Integer);
    procedure SetMargins(const Value: TMargins);
    procedure SetCursor(const Value: TCursor);
    procedure SetBackground(const Value: TAwesomeBackground);
    procedure SetCloseButton(const Value: TAwesomeCloseButton);
    procedure SetImage(const Value: TAwesomeImage);
    procedure SetCaption(const Value: TAwesomeCaption);
    procedure SetSmoothingMode(const Value: TSmoothingMode);
    procedure SetUseAllSpace(const Value: Boolean);
    procedure SetStates(const Pt: TPoint);
    procedure OnChange(Sender: TObject);
    procedure UpdateInternalRects;
    procedure SetAutoWidth(const Value: Boolean);
    procedure RecalculateRects;
  protected
    procedure DoOnPaint(const ACanvas: TGPGraphics; const OuterRect: TRect; var ARect: TRect); virtual;
    procedure DoButtonClickEvent(const ClickArea: TAwesomeButtonEditHitTestArea; var Handled: Boolean); virtual;
    procedure DoMouseMove(const Pt: TPoint); virtual;

    procedure DoPaint(const ACanvas: TGPGraphics; var ARect: TRect); virtual;
    procedure DoGetButtonWidth(const ACanvas: TCanvas; const AHeight: Integer; out AWidth: Integer); virtual;
    function GetDisplayName: string; override;
    procedure DoClick(const Pt: TPoint; var Handled: Boolean); virtual;
    procedure DoIsHit(const Pt: TPoint; var IsHit: Boolean); virtual;
    procedure DoOnVisibilityChanged; virtual;
    procedure DoSetControlRects; virtual;

    property Bounds: TRect read FBounds write SetBounds;

    property OnPaint: TOnPaint read FOnPaint write FOnPaint;
    property OnVisibilityChanged: TNotifyEvent read FOnVisibilityChanged write FOnVisibilityChanged;
    property OnClick: TButtonClickEvent read FOnClick write FOnClick;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property State: TAwesomeButtonState read FState write SetState;
    property Visible: Boolean read FVisible write SetVisible;
    property Position: TAwesomeButtonPosition read FPosition write SetPosition;
    property Width: Integer read FWidth write SetWidth;
    property Margins: TMargins read FMargins write SetMargins;
    property Cursor: TCursor read FCursor write SetCursor;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property SmoothingMode: TSmoothingMode read FSmoothingMode write SetSmoothingMode;
    property UseAllSpace: Boolean read FUseAllSpace write SetUseAllSpace;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;

    property Background: TAwesomeBackground read FBackground write SetBackground;
    property Image: TAwesomeImage read FImage write SetImage;
    property Caption: TAwesomeCaption read FCaption write SetCaption;
    property CloseButton: TAwesomeCloseButton read FCloseButton write SetCloseButton;
    property Internal: Boolean read FInternalButton;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure PaintTo(const ACanvas: TGPGraphics);
    function GetBounds: TRect;
    function HitTest(const Pt: TPoint): TAwesomeButtonEditHitTestArea;
  end;

  TAwesomeButton = class(TCustomAwesomeButton)
  published
    property OnPaint;
    property OnVisibilityChanged;
    property OnClick;

    property Background;
    property Image;
    property Caption;
    property CloseButton;
    property Enabled;
    property Visible;
    property Position;
    property Width;
    property Margins;
    property Cursor;
    property PopupMenu;
    property SmoothingMode;
    property UseAllSpace;
  end;
  TAwesomeButtonClass = class of TAwesomeButton;

  TAwesomeButtonCollection = class(TOwnedCollection)
  protected
    procedure SetItem(Index: Integer; Value: TAwesomeButton);
    function GetItem(Index: Integer): TAwesomeButton;
    procedure Update(Item: TCollectionItem); override;
  public
    property Items[Index: Integer]: TAwesomeButton read GetItem write SetItem; default;
    function Add: TAwesomeButton; virtual;
  end;

  TCustomBaseAwesomeEdit = class(TCustomControl, IAwesomeEdit)
  private
    FOnTextChanged: TNotifyEvent;

    FButtonsUser: TAwesomeButtonCollection;
    FButtonsInternal: TAwesomeButtonCollection;
    FButtonList: TObjectList<TCustomAwesomeButton>;

    FMinEditWidth: Integer;
    FBackgroundColor: TColor;
    FBorderStyle: TAwesomeEditBorderStyle;

    procedure SetBackgroundColor(const Value: TColor);
    procedure SetText(const Value: String);
    procedure SetTextHint(const Value: String);
    function GetText: String;
    function GetTextHint: String;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderRadius(const Value: Integer);
    procedure SetBorderThickness(const Value: Single);
    procedure SetAutoFontSize(const Value: Boolean);
    procedure SetFontSize;
    procedure SetBorderStyle(const Value: TAwesomeEditBorderStyle);
    procedure SetMinEditWidth(const Value: Integer);
    function GetControlStateID: Integer;
    procedure PropertyUpdated(Sender: TObject);
    procedure ButtonListUpdated(Sender: TObject);
    procedure SetAwesomeButtons(const Value: TAwesomeButtonCollection);
    function GetButtonCollectionClass: TAwesomeButtonClass;
    function GetMouseDown: Boolean;
    function GetCanvas: TCanvas;
    procedure UpdateButtonList;
    procedure OnChange(Sender: TObject);
    procedure SetEditMargins(const Value: TMargins);
  protected
    FMouseDownHitTest: TAwesomeEditHitTest;
    FEditRect: TRect;
    FInnerRect: TRect;
    FControlRect: TRect;
    FEdit: TInternalEdit;
    FText: String;
    FTextHint: String;
    FBorderRadius: Integer;
    FBorderColor: TColor;
    FBorderThickness: Single;
    FInternalMargins: TMargins;
    FEditMargins: TMargins;
    FAutoFontSize: Boolean;
    FMouseButton: TMouseButton;
    FMouseDown: Boolean;
    FUpdateCount: Integer;
    FUpdateCalled: Boolean;
    FBorderPath: TGPGraphicsPath;
    FPainted: Boolean;

    function AddInternalButton<T: TCustomAwesomeButton>(const Position: TAwesomeButtonPosition = TAwesomeButtonPosition.ebpRight): T;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;
    procedure MouseMove(Shift: TShiftState; x, y: Integer); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;

    procedure CreateWnd; override;
    procedure Paint; override;

    procedure DoUpdated; virtual;
    procedure DoBeforeDrawButtons(const ACanvas: TGPGraphics; const ARect: TRect); virtual;
    procedure DoDrawButtons(const GPCanvas: TGPGraphics); virtual;
    procedure DoAfterDrawButtons(const ACanvas: TGPGraphics; const ARect: TRect); virtual;
    procedure DoSetControlRects; virtual;
    procedure DoSetControlProperties; virtual;
    procedure DoButtonClicked(const ButtonIndex: Integer); virtual;
    procedure DoOnEditChange; virtual;
    procedure DoSetButtonStates(const x, y: Integer); virtual;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditKeyPress(var Key: Char); virtual;
    procedure DoGetButtonCollectionClass(out AwesomeButtonClass: TAwesomeButtonClass); virtual;

    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;

    property Text: String read GetText write SetText;
    property TextHint: String read GetTextHint write SetTextHint;
    property BorderRadius: Integer read FBorderRadius write SetBorderRadius;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderThickness: Single read FBorderThickness write SetBorderThickness;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property AutoFontSize: Boolean read FAutoFontSize write SetAutoFontSize;
    property BorderStyle: TAwesomeEditBorderStyle read FBorderStyle write SetBorderStyle;
    property MinEditWidth: Integer read FMinEditWidth write SetMinEditWidth;
    property EditMargins: TMargins read FEditMargins write SetEditMargins;

    property Buttons: TAwesomeButtonCollection read FButtonsUser write SetAwesomeButtons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HitTest(const pt: TPoint; const IgnoreDisabledControls: Boolean = False): TAwesomeEditHitTest;
    procedure EditChanged;
    procedure EditKeyDown(var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(var Key: Char);
    procedure BeginUpdate;
    procedure EndUpdate;
    
    property EditControl: TInternalEdit read FEdit;

    property InternalMargins: TMargins read FInternalMargins;
    property ButtonList: TObjectList<TCustomAwesomeButton> read FButtonList;
  end;

  TBaseAwesomeEdit = class(TCustomBaseAwesomeEdit)
  published
    property Buttons;
    property EditMargins;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderRadius;
    property BorderColor;
    property BorderStyle;
    property BorderThickness;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property AutoFontSize;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
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
    property OnTextChanged;
  end;

  TAwesomeEdit = class(TBaseAwesomeEdit);

implementation

{ TCustomBaseAwesomeEdit }

function TCustomBaseAwesomeEdit.AddInternalButton<T>(
  const Position: TAwesomeButtonPosition): T;
begin
  Result := T(FButtonsInternal.Add);

  Result.Position := Position;
  Result.FInternalButton := True;
end;

procedure TCustomBaseAwesomeEdit.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomBaseAwesomeEdit.ButtonListUpdated(Sender: TObject);
begin
  UpdateButtonList;

  Updated;
end;

procedure TCustomBaseAwesomeEdit.UpdateButtonList;
var
  i: Integer;
begin
  FButtonList.Clear;

  for i := 0 to pred(FButtonsInternal.Count) do
  begin
    FButtonList.Add(FButtonsInternal[i]);
  end;

  for i := 0 to pred(FButtonsUser.Count) do
  begin
    FButtonList.Add(FButtonsUser[i]);
  end;
end;

procedure TCustomBaseAwesomeEdit.CMColorChanged(var Message: TMessage);
begin
  inherited;

  FEdit.SetColor(Color);
end;

procedure TCustomBaseAwesomeEdit.CMFontChanged(var Message: TMessage);
begin
  FEdit.GetFont.Assign(Font);
  FEdit.ScaleBy(Screen.PixelsPerInch, 96);
  //FEdit.GetFont.Size := 13;

  DoUpdated;
end;

procedure TCustomBaseAwesomeEdit.CMMouseLeave(var Msg: TMessage);
begin
  DoSetButtonStates(-1, -1);
end;

procedure TCustomBaseAwesomeEdit.DoGetButtonCollectionClass(out AwesomeButtonClass: TAwesomeButtonClass);
begin
  AwesomeButtonClass := TAwesomeButton;
end;

procedure TCustomBaseAwesomeEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if Key = 13 then
  begin
    Key := 00;
  end;
end;

procedure TCustomBaseAwesomeEdit.DoEditKeyPress(var Key: Char);
begin
  inherited;

  if Key = #13 then
  begin
    Key := #00;
  end;
end;

function TCustomBaseAwesomeEdit.GetButtonCollectionClass: TAwesomeButtonClass;
begin
  DoGetButtonCollectionClass(Result);
end;

constructor TCustomBaseAwesomeEdit.Create(AOwner: TComponent);
begin
  inherited;

  FButtonsUser := TAwesomeButtonCollection.Create(Self, GetButtonCollectionClass);
  FButtonsInternal := TAwesomeButtonCollection.Create(Self, GetButtonCollectionClass);
  FButtonList := TObjectList<TCustomAwesomeButton>.Create(False);

  FEdit := TInternalEdit.Create(Self);
  FEdit.SetSubComponent(True);
  (*FEdit.ScaleBy(Screen.PixelsPerInch, 96);*)

  FBackgroundColor := clWindow;
  FBorderRadius := 10;
  FBorderColor := clLtGray;
  FBorderThickness := 1;
  FAutoFontSize := True;
  FMinEditWidth := 50;
  FBorderStyle := TAwesomeEditBorderStyle.aebStandard;

  ParentColor := False;
  ParentBackground := False;
  Color := clWindow;
  Height := 22;
  Width := 200;

  FInternalMargins := TMargins.Create(Self);
  FInternalMargins.SetBounds(0, 0, 0, 0);

  FEditMargins := TMargins.Create(nil);
  FEditMargins.SetBounds(3, 0, 0, 0);
  FEditMargins.OnChange := OnChange;

  (*FInternalMargins.Top := 1;
  FInternalMargins.Left := 1;
  FInternalMargins.Right := 1;
  FInternalMargins.Bottom := 1;*)
end;

procedure TCustomBaseAwesomeEdit.OnChange(Sender: TObject);
begin
  DoUpdated;
end;

function TCustomBaseAwesomeEdit.GetControlStateID: Integer;
begin
  if not Enabled then
  begin
    Result := ETS_DISABLED;
  end else
  if (Focused) or (FEdit.Focused) then
  begin
    Result := ETS_FOCUSED;
  end
  else
  begin
    Result := ETS_NORMAL;
  end;
end;

function TCustomBaseAwesomeEdit.GetMouseDown: Boolean;
begin
  Result := FMouseDown;
end;

procedure TCustomBaseAwesomeEdit.Paint;
var
  Bitmap: TBitmap;
  GPCanvas: TGPGraphics;
  ThemeRect: TRect;
begin
  if not FPainted then
  begin
    DoSetControlRects;
    DoSetButtonStates(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    DoSetControlProperties;

    FPainted := True;
  end;

  Bitmap := TBitmap.Create;
  try
    Bitmap.Height := Height;
    Bitmap.Width := Width;

    GPCanvas := TGPGraphics.Create(Bitmap.Canvas.Handle);
    try
      GPCanvas.SetSmoothingMode(TSmoothingMode.SmoothingModeHighQuality);

      // Draw the border
      DoBeforeDrawButtons(GPCanvas, FControlRect);

      if BorderStyle = TAwesomeEditBorderStyle.aebPlatform then
      begin
        GetThemeBackgroundContentRect(StyleServices.Theme[teEdit], Bitmap.Canvas.Handle, 0, GetControlStateID, FControlRect, @ThemeRect);

        DrawThemeBackground(StyleServices.Theme[teEdit], Bitmap.Canvas.Handle, EP_EDITBORDER_NOSCROLL, GetControlStateID, FControlRect, @FControlRect);

        GPCAnvas.SetClip(MakeRect(ThemeRect), CombineModeIntersect);
      end;

      // Draw the buttons
      DoDrawButtons(GPCanvas);

      DoAfterDrawButtons(GPCanvas, FControlRect);
    finally
      FreeAndNil(GPCanvas);
    end;

    FEdit.PaintTo(Bitmap.Canvas.Handle, FEdit.Left, FEdit.Top);

    // Reduce flicker by excluding the edit control rect
    ExcludeClipRect(
      Canvas.Handle,
      FEditRect.Left,
      FEditRect.Top,
      FEditRect.Right,
      FEditRect.Bottom);
    
    BitBlt(Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  finally
    FreeAndNil(Bitmap);
  end;
end;

procedure TCustomBaseAwesomeEdit.PropertyUpdated(Sender: TObject);
begin
  DoUpdated;
end;

destructor TCustomBaseAwesomeEdit.Destroy;
begin
  FreeAndNil(FButtonsUser);
  FreeAndNil(FInternalMargins);
  FreeAndNil(FBorderPath);
  FreeAndNil(FButtonsUser);
  FreeAndNil(FButtonList);
  FreeAndNil(FEditMargins);
  FreeAndNil(FButtonsInternal);

  inherited;
end;

procedure TCustomBaseAwesomeEdit.SetFontSize;
var
  Size: TSize;
begin
  if (HandleAllocated) and (FAutoFontSize) then
  begin
    Canvas.Font.Size := 1;
    Size := Canvas.TextExtent('Wj');

    while Size.cy < FEditRect.Height do
    begin
      Canvas.Font.Size := Canvas.Font.Size + 1;

      Size := Canvas.TextExtent('Wj');
    end;

    FEdit.GetFont.Size := Canvas.Font.Size - 1;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetMinEditWidth(const Value: Integer);
begin
  if FMinEditWidth <> Value then
  begin
    FMinEditWidth := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.DoUpdated;
begin
  if HandleAllocated then
  begin
    if FUpdateCount = 0 then
    begin
      DoSetControlRects;
      DoSetButtonStates(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      DoSetControlProperties;

      Invalidate;
    end
    else
    begin
      FUpdateCalled := True;
    end;
  end;
end;

procedure TCustomBaseAwesomeEdit.EditChanged;
begin
  FText := FEdit.Text;

  DoOnEditChange;
end;

procedure TCustomBaseAwesomeEdit.EditKeyPress(var Key: Char);
begin
  DoEditKeyPress(Key);
end;

procedure TCustomBaseAwesomeEdit.EditKeyDown(var Key: Word; Shift: TShiftState);
begin
  DoEditKeyDown(Key, Shift);
end;

procedure TCustomBaseAwesomeEdit.EndUpdate;
begin
  Dec(FUpdateCount);

  if (FUpdateCount = 0) and
     (FUpdateCalled) then
  begin
    DoUpdated;

    FUpdateCalled := False;
  end;
end;

procedure TCustomBaseAwesomeEdit.CreateWnd;
begin
  inherited;

  FEdit.Parent := Self;

  DoSetControlRects;
end;

function TCustomBaseAwesomeEdit.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

function TCustomBaseAwesomeEdit.GetText: String;
begin
  Result := FEdit.Text;
end;

function TCustomBaseAwesomeEdit.GetTextHint: String;
begin
  Result := FEdit.TextHint;
end;

function TCustomBaseAwesomeEdit.HitTest(const pt: TPoint; const IgnoreDisabledControls: Boolean): TAwesomeEditHitTest;
var
  i: Integer;
  ButtonEditHitTestArea: TAwesomeButtonEditHitTestArea;
begin
  Result.ButtonIndex := -1;
  Result.HitTestArea := TAwesomeEditHitTestArea.htNowhere;
  Result.ButtonHitTestArea := TAwesomeButtonEditHitTestArea.bhtNowhere;

  if FEditRect.Contains(pt) then
  begin
    Result.HitTestArea := TAwesomeEditHitTestArea.htEdit;
  end else
  begin
    for i := 0 to pred(ButtonList.Count) do
    begin
      ButtonEditHitTestArea := ButtonList[i].HitTest(pt);

      if ButtonEditHitTestArea <> TAwesomeButtonEditHitTestArea.bhtNowhere then
      begin
        Result.ButtonIndex := i;
        Result.HitTestArea := TAwesomeEditHitTestArea.htButton;
        Result.ButtonHitTestArea := ButtonEditHitTestArea;

        Break;
      end;
    end;
  end;
end;

procedure TCustomBaseAwesomeEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  pt: TPoint;
  MouseDownPoint: TPoint;
begin
  inherited;

  FMouseDownHitTest := HitTest(Point(x, y));
  FMouseButton := Button;
  MouseDownPoint := Point(X, Y);
  FMouseDown := True;

  DoSetButtonStates(x, y);

  if (FMouseDownHitTest.HitTestArea = TAwesomeEditHitTestArea.htButton) and
     (FMouseDownHitTest.ButtonHitTestArea <> TAwesomeButtonEditHitTestArea.bhtCloseButton) then
  begin
    if (ButtonList[FMouseDownHitTest.ButtonIndex].PopupMenu <> nil) and
       (ButtonList[FMouseDownHitTest.ButtonIndex].IsHit(MouseDownPoint)) then
    begin
      pt := ClientToScreen(Point(
        ButtonList[FMouseDownHitTest.ButtonIndex].Bounds.Left,
        ButtonList[FMouseDownHitTest.ButtonIndex].Bounds.Bottom + 1));

      ButtonList[FMouseDownHitTest.ButtonIndex].PopupMenu.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TCustomBaseAwesomeEdit.MouseMove(Shift: TShiftState; x, y: Integer);
var
  i: Integer;
begin
  inherited;

  DoSetButtonStates(x, y);

  for i := 0 to pred(ButtonList.Count) do
  begin
    ButtonList[i].DoMouseMove(Point(x, y));
  end;

  //Invalidate;
end;

procedure TCustomBaseAwesomeEdit.DoSetButtonStates(const x, y: Integer);
var
  i: Integer;
  AHitTest: TAwesomeEditHitTest;
begin
  for i := 0 to pred(FButtonList.Count) do
  begin
    FButtonList[i].SetStates(Point(X, Y));
  end;

  AHitTest := HitTest(Point(x, y), False);

  if AHitTest.HitTestArea = TAwesomeEditHitTestArea.htButton then
  begin
    Cursor := ButtonList[AHitTest.ButtonIndex].Cursor;

    if (FMouseDown) and
       (FMouseDownHitTest.ButtonIndex = AHitTest.ButtonIndex) then
    begin
      ButtonList[AHitTest.ButtonIndex].State := TAwesomeButtonState.bsPressed;
    end
    else
    begin
      ButtonList[AHitTest.ButtonIndex].State := TAwesomeButtonState.bsHot;
    end;
  end
  else
  begin
    Cursor := crDefault;
  end;
end;

procedure TCustomBaseAwesomeEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  AHitTest: TAwesomeEditHitTest;
  Handled: Boolean;
begin
  inherited;

  FMouseDown := False;

  AHitTest := HitTest(Point(x, y));

  if (FMouseDownHitTest.HitTestArea = TAwesomeEditHitTestArea.htButton) and
     (FMouseDownHitTest.ButtonIndex = AHitTest.ButtonIndex) then
  begin
    ButtonList[FMouseDownHitTest.ButtonIndex].Click(Point(X, Y), Handled);

    if not Handled then
    begin
      DoButtonClicked(FMouseDownHitTest.ButtonIndex);
    end;
  end;

  DoSetButtonStates(x, y);
end;

procedure TCustomBaseAwesomeEdit.SetAutoFontSize(const Value: Boolean);
begin
  if Value <> FAutoFontSize then
  begin
    FAutoFontSize := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetAwesomeButtons(
  const Value: TAwesomeButtonCollection);
begin
  FButtonsUser.Assign(Value);
end;

procedure TCustomBaseAwesomeEdit.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetBorderRadius(const Value: Integer);
begin
  if FBorderRadius <> Value then
  begin
    FBorderRadius := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetBorderStyle(
  const Value: TAwesomeEditBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetBorderThickness(const Value: Single);
begin
  if FBorderThickness <> Value then
  begin
    FBorderThickness := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetEditMargins(const Value: TMargins);
begin
  if FEditMargins <> Value then
  begin
    FEditMargins := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    FEdit.Text := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.SetTextHint(const Value: String);
begin
  if FTextHint <> Value then
  begin
    FTextHint := Value;
    FEdit.TextHint := Value;

    DoUpdated;
  end;
end;

procedure TCustomBaseAwesomeEdit.WMSize(var Message: TWMSize);
begin
  DoUpdated;
end;

procedure TCustomBaseAwesomeEdit.DoSetControlProperties;
begin
  FEdit.Left := FEditRect.Left;
  FEdit.Top := FEditRect.Top;
  FEdit.Width := FEditRect.Width;
  FEdit.Height := FEditRect.Height;

  SetFontSize;
end;

procedure TCustomBaseAwesomeEdit.DoSetControlRects;
const
  ButtonWidth = 16;
  ButtonMargin = 2;
var
  MarginLeft, MarginRight, OuterMarginLeft, OuterMarginRight: Integer;
  NewBounds: TRect;
  i: Integer;
  BorderMargin: Integer;
begin
  case BorderStyle of
    aebStandard: BorderMargin := Trunc(FBorderThickness);
    aebPlatform: BorderMargin := 2;
  else
    BorderMargin := 0;
  end;

  FControlRect := Rect(0, 0, Width - 1, Height - 1);

  FInnerRect := Rect(
    FixDPIPixels(FInternalMargins.Left) + FixDPIPixels(BorderMargin),
    FixDPIPixels(FInternalMargins.Top) + FixDPIPixels(BorderMargin),
    Width - FixDPIPixels(FInternalMargins.Right) - FixDPIPixels(BorderMargin),
    Height - FixDPIPixels(FInternalMargins.Bottom) - FixDPIPixels(FInternalMargins.Top) - (FixDPIPixels(BorderMargin) * 2));

  OuterMarginLeft := 0;
  MarginLeft := FInnerRect.Left;
  OuterMarginRight := Width;
  MarginRight := FInnerRect.Right;

  for i := 0 to pred(ButtonList.Count) do
  begin
    if (ButtonList[i].Visible) and (ButtonList[i].Position <> ebpNone) then
    begin
      case ButtonList[i].Position of
        ebpLeft:
          begin
            if ButtonList[i].UseAllSpace then
            begin
              NewBounds := Rect(
                OuterMarginLeft,
                0,
                OuterMarginLeft + ButtonList[i].GetButtonWidth(Canvas, FInnerRect.Height),
                Height);
            end
            else
            begin
              NewBounds := Rect(
                MarginLeft,
                FInnerRect.Top,
                MarginLeft + ButtonList[i].GetButtonWidth(Canvas, FInnerRect.Height),
                FInnerRect.Bottom);
            end;

            ButtonList[i].FInternalVisible := NewBounds.Right < MarginRight - FixDPIPixels(FMinEditWidth);

            if ButtonList[i].FInternalVisible then
            begin
              ButtonList[i].Bounds := NewBounds;

              MarginLeft := NewBounds.Right;
              OuterMarginLeft := MarginLeft;
            end;
          end;

        ebpRight:
          begin
            if ButtonList[i].UseAllSpace then
            begin
              NewBounds := Rect(
                OuterMarginRight - ButtonList[i].GetButtonWidth(Canvas, FInnerRect.Height),
                0,
                OuterMarginRight,
                Height);
            end
            else
            begin
              NewBounds := Rect(
                MarginRight - ButtonList[i].GetButtonWidth(Canvas, FInnerRect.Height),
                FInnerRect.Top,
                MarginRight,
                FInnerRect.Bottom);
            end;

            ButtonList[i].FInternalVisible := NewBounds.Left > MarginLeft + FixDPIPixels(FMinEditWidth);

            if ButtonList[i].FInternalVisible then
            begin
              ButtonList[i].Bounds := NewBounds;
              MarginRight := NewBounds.Left;
              OuterMarginRight := MarginRight;
            end;
          end;
      end;
    end;
  end;

  FEditRect.Left := MarginLeft +  FixDPIPixels(FEditMargins.Left);
  FEditRect.Right := MarginRight +  FixDPIPixels(FEditMargins.Right);
  FEditRect.Top := FInnerRect.Top +  FixDPIPixels(FEditMargins.Top);
  FEditRect.Bottom := FInnerRect.Bottom +  FixDPIPixels(FEditMargins.Bottom);
end;

procedure TCustomBaseAwesomeEdit.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TCustomBaseAwesomeEdit.DoAfterDrawButtons(const ACanvas: TGPGraphics;
  const ARect: TRect);
var
  Pen: TGPPen;
begin
  ACanvas.ResetClip;

  Pen := TGPPen.Create(MakeGDIPColor(BorderColor, 255));
  try
    Pen.SetWidth(FBorderThickness);
    ACanvas.DrawPath(Pen, FBorderPath);
  finally
    FreeAndNil(Pen);
  end;
end;

procedure TCustomBaseAwesomeEdit.DoBeforeDrawButtons(const ACanvas: TGPGraphics; const ARect: TRect);
var
  GPBrush: TGPSolidBrush;
begin
  FreeAndNil(FBorderPath);

  case BorderStyle of
    aebStandard:
      begin
        FBorderPath := CreateRoundRectangle(
          TRect.Create(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom),
          FixDPIPixels(BorderRadius));

        GPBrush := TGPSolidBrush.Create(MakeGDIPColor(BackgroundColor, 255));
        try
          ACanvas.FillRectangle(GPBrush, MakeRect(ARect));
          GPBrush.SetColor(MakeGDIPColor(Color, 255));
          ACanvas.FillPath(GPBrush, FBorderPath);
        finally
          FreeAndNil(GPBrush);
        end;

        ACanvas.ResetClip;
        ACanvas.SetClip(FBorderPath, CombineModeIntersect);
      end;

    aebNone:
      begin
        GPBrush := TGPSolidBrush.Create(MakeGDIPColor(Color, 255));
        try
          ACanvas.FillRectangle(GPBrush, MakeRect(FControlRect));
        finally
          FreeAndNil(GPBrush);
        end;
      end;
  end;

end;

procedure TCustomBaseAwesomeEdit.DoDrawButtons(const GPCanvas: TGPGraphics);
var
  i: Integer;
begin
  for i := 0 to pred(FButtonsInternal.Count) do
  begin
    FButtonsInternal[i].PaintTo(GPCanvas);
  end;

  for i := 0 to pred(ButtonList.Count) do
  begin
    ButtonList[i].PaintTo(GPCanvas);
  end;
end;

procedure TCustomBaseAwesomeEdit.DoOnEditChange;
begin
  if Assigned(FOnTextChanged) then
    FOnTextChanged(Self);
end;

procedure TCustomBaseAwesomeEdit.DoButtonClicked(const ButtonIndex: Integer);
begin
  // Override
end;


{ TPTLibEditButton }

constructor TCustomAwesomeButton.Create(Collection: TCollection);
begin
  inherited;

  FBackground := TAwesomeBackground.Create(AwesomeEdit);
  FImage := TAwesomeImage.Create(AwesomeEdit);
  FCaption := TAwesomeCaption.Create(AwesomeEdit);
  FCloseButton := TAwesomeCloseButton.Create(AwesomeEdit);

  FMargins := TMargins.Create(nil);
  FMargins.SetBounds(1, 0, 1, 0);
  FMargins.OnChange := OnChange;

  FState := TAwesomeButtonState.bsNormal;
  FEnabled := True;
  FVisible := True;
  FPosition := TAwesomeButtonPosition.ebpRight;
  FCursor := crArrow;
  FInternalVisible := True;
  FSmoothingMode := TSmoothingMode.SmoothingModeDefault;
  FAutoWidth := True;
end;

destructor TCustomAwesomeButton.Destroy;
begin
  FreeAndNil(FMargins);
  FreeAndNil(FBackground);
  FreeAndNil(FImage);
  FreeAndNil(FCaption);
  FreeAndNil(FCloseButton);

  inherited;
end;

procedure TCustomAwesomeButton.OnChange(Sender: TObject);
begin
  DoUpdated;
end;

procedure TCustomAwesomeButton.DoGetButtonWidth(const ACanvas: TCanvas; const AHeight: Integer; out AWidth: Integer);
var
  ImageWidth, TextWidth, CloseButtonWidth: Integer;
begin
  if Width <> 0 then
  begin
    AWidth := Width;
  end
  else
  begin
    ImageWidth := 0;
    TextWidth := 0;
    CloseButtonWidth := 0;
    AWidth := 0;

    // If we have a non zero value for width, set it now
    if Image.Visible then
    begin
      ImageWidth := Image.GetIdealWidth(ACanvas, AHeight);
    end;

    if Caption.Visible then
    begin
      TextWidth := Caption.GetIdealWidth(ACanvas, AHeight);
    end;

    if CloseButton.Visible then
    begin
      CloseButtonWidth := CloseButton.GetIdealWidth(ACanvas, AHeight);
    end;

    AWidth := ImageWidth + TextWidth + CloseButtonWidth;

    if AWidth = 0 then
    begin
      if Background.Visible then
      begin
        AWidth := Background.GetIdealWidth(ACanvas, AHeight);
      end;
    end;
  end;
end;

procedure TCustomAwesomeButton.DoButtonClickEvent(
  const ClickArea: TAwesomeButtonEditHitTestArea; var Handled: Boolean);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, ClickArea, Handled);
end;

procedure TCustomAwesomeButton.DoClick(const Pt: TPoint; var Handled: Boolean);
var
  ClickArea: TAwesomeButtonEditHitTestArea;
begin
  Handled := False;

  ClickArea := HitTest(Pt);

  DoButtonClickEvent(ClickArea, Handled);

  if not Handled then
  begin
    if ClickArea = TAwesomeButtonEditHitTestArea.bhtCloseButton then
    begin
      Visible := False;
    end;
  end;
end;

procedure TCustomAwesomeButton.DoIsHit(const Pt: TPoint; var IsHit: Boolean);
begin
  IsHit := True;
end;

procedure TCustomAwesomeButton.DoMouseMove(const Pt: TPoint);
begin
  SetStates(Pt);
end;

procedure TCustomAwesomeButton.SetStates(const Pt: TPoint);
var
  ButtonHitTest: TAwesomeButtonEditHitTestArea;
  DefaultState, CloseButtonState: TAwesomeButtonState;
begin
  ButtonHitTest := HitTest(pt);

  if not FEnabled then
  begin
    DefaultState := TAwesomeButtonState.bsDisabled;
    CloseButtonState := TAwesomeButtonState.bsDisabled;
  end
  else
  begin
    DefaultState := TAwesomeButtonState.bsNormal;
    CloseButtonState := TAwesomeButtonState.bsNormal;
  end;

  if ButtonHitTest <> TAwesomeButtonEditHitTestArea.bhtNowhere then
  begin
    if FEnabled then
    begin
      if AwesomeEdit.GetMouseDown then
      begin
        if ButtonHitTest = bhtCloseButton then
        begin
          DefaultState := TAwesomeButtonState.bsHot;
          CloseButtonState := TAwesomeButtonState.bsPressed;
        end
        else
        begin
          DefaultState := TAwesomeButtonState.bsPressed;
          CloseButtonState := TAwesomeButtonState.bsNormal;
        end;
      end
      else
      begin
        DefaultState := TAwesomeButtonState.bsHot;

        if ButtonHitTest = bhtCloseButton then
        begin
          CloseButtonState := TAwesomeButtonState.bsHot;
        end
        else
        begin
          CloseButtonState := TAwesomeButtonState.bsNormal;
        end;
      end;
    end;
  end;

  Background.State := DefaultState;
  Caption.State := DefaultState;
  Image.State := DefaultState;
  CloseButton.State := CloseButtonState;
end;

procedure DoClick(const Pt: TPoint; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TCustomAwesomeButton.DoOnPaint(const ACanvas: TGPGraphics; const OuterRect: TRect; var ARect: TRect);
var
  Handled: Boolean;
begin
  Handled := False;

  if Assigned(FOnPaint) then
  begin
    FOnPaint(
      Self,
      ACanvas,
      ARect,
      Handled);
  end;

  if not Handled then
  begin
    if UseAllSpace then
    begin
      DoPaint(ACanvas, FOuterBounds);
    end
    else
    begin
      DoPaint(ACanvas, ARect);
    end;
  end;
end;

procedure TCustomAwesomeButton.DoOnVisibilityChanged;
begin
  if Assigned(FOnVisibilityChanged) then
    FOnVisibilityChanged(Self);
end;

procedure TCustomAwesomeButton.DoPaint(const ACanvas: TGPGraphics;
  var ARect: TRect);
begin
  Background.PaintTo(ACanvas);
  Caption.PaintTo(ACanvas);
  Image.PaintTo(ACanvas);
  CloseButton.PaintTo(ACanvas);
end;

procedure TCustomAwesomeButton.DoSetControlRects;
begin

end;

function TCustomAwesomeButton.GetBounds: TRect;
begin
  Result := FBounds;
end;

function TCustomAwesomeButton.GetButtonWidth(const ACanvas: TCanvas; const AHeight: Integer): Integer;
begin
  DoGetButtonWidth(ACanvas, AHeight, Result);
end;

function TCustomAwesomeButton.GetDisplayName: string;
begin
  Result := 'EditButton';
end;

function TCustomAwesomeButton.HitTest(
  const Pt: TPoint): TAwesomeButtonEditHitTestArea;
begin
  Result := TAwesomeButtonEditHitTestArea.bhtNowhere;

  if FCloseButton.BoundingRect.Contains(Pt) then
  begin
    Result := TAwesomeButtonEditHitTestArea.bhtCloseButton;
  end else
  if FCaption.BoundingRect.Contains(Pt) then
  begin
    Result := TAwesomeButtonEditHitTestArea.bhtText;
  end else
  if FImage.BoundingRect.Contains(Pt) then
  begin
    Result := TAwesomeButtonEditHitTestArea.bhtImage;
  end else
  if FBackground.BoundingRect.Contains(Pt) then
  begin
    Result := TAwesomeButtonEditHitTestArea.bhtBackground;
  end;
end;

function TCustomAwesomeButton.IsHit(const Pt: TPoint): Boolean;
begin
  DoIsHit(Pt, Result);
end;

procedure TCustomAwesomeButton.Click(const Pt: TPoint; var Handled: Boolean);
begin
  DoClick(pt, Handled);
end;

procedure TCustomAwesomeButton.PaintTo(const ACanvas: TGPGraphics);
var
  ARect: TRect;
begin
  if (Visible) and (FInternalVisible) then
  begin
    ARect := FBounds;

    DoOnPaint(ACanvas, FOuterBounds, ARect);
  end;
end;

procedure TCustomAwesomeButton.SetBounds(const Value: TRect);
begin
  if (FBounds.Left <> Value.Left) or
     (FBounds.Right <> Value.Right) or
     (FBounds.Top <> Value.Top) or
     (FBounds.Bottom <> Value.Bottom) then
  begin
    FBounds := Value;

    UpdateInternalRects;
  end;
end;

procedure TCustomAwesomeButton.UpdateInternalRects;
var
  MarginLeft: Integer;
begin
  MarginLeft := Bounds.Left;

  if Background.Visible then
  begin
    Background.BoundingRect := Bounds;
  end;

  Background.BoundingRect := Background.GetBoundingRect(FBounds);

  // If we have a non zero value for width, set it now
  if Image.Visible then
  begin
    FImage.BoundingRect :=
      Image.GetBoundingRect(Rect(
        MarginLeft,
        FBounds.Top,
        MarginLeft + Image.GetIdealWidth(AwesomeEdit.GetCanvas, FBounds.Height),
        FBounds.Bottom));

    MarginLeft := FImage.BoundingRect.Right;
  end;

  if Caption.Visible then
  begin
    Caption.BoundingRect :=
      Caption.GetBoundingRect(Rect(
        MarginLeft,
        FBounds.Top,
        MarginLeft + Caption.GetIdealWidth(AwesomeEdit.GetCanvas, FBounds.Height),
        FBounds.Bottom));

    MarginLeft := Caption.BoundingRect.Right;
  end;

  if CloseButton.Visible then
  begin
    CloseButton.BoundingRect :=
      CloseButton.GetBoundingRect(Rect(
        MarginLeft,
        FBounds.Top,
        MarginLeft + CloseButton.GetIdealWidth(AwesomeEdit.GetCanvas, FBounds.Height),
        FBounds.Bottom));
  end;
end;

procedure TCustomAwesomeButton.SetPosition(
  const Value: TAwesomeButtonPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeButton.SetAutoWidth(const Value: Boolean);
begin
  if FAutoWidth <> Value then
  begin
    FAutoWidth := Value;

    RecalculateRects;
  end;
end;

procedure TCustomAwesomeButton.RecalculateRects;
begin

end;

procedure TCustomAwesomeButton.SetBackground(const Value: TAwesomeBackground);
begin
  FBackground.Assign(Value);

  DoUpdated;
end;

procedure TCustomAwesomeButton.SetCloseButton(const Value: TAwesomeCloseButton);
begin
  FCloseButton.Assign(Value);

  DoUpdated;
end;

procedure TCustomAwesomeButton.SetCursor(const Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;

    DoUpdated;

    DoRedraw;
  end;
end;

procedure TCustomAwesomeButton.SetImage(const Value: TAwesomeImage);
begin
  FImage.Assign(Value);

  DoUpdated;
end;

procedure TCustomAwesomeButton.SetMargins(const Value: TMargins);
begin
  if (FMargins.Left <> Value.Left) or
     (FMargins.Right <> Value.Right) or
     (FMargins.Top <> Value.Top) or
     (FMargins.Bottom <> Value.Bottom) then
  begin
    FMargins := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeButton.SetSmoothingMode(const Value: TSmoothingMode);
begin
  if FSmoothingMode <> Value then
  begin
    FSmoothingMode := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeButton.SetState(const Value: TAwesomeButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;

    DoRedraw;
  end;
end;

procedure TCustomAwesomeButton.SetCaption(const Value: TAwesomeCaption);
begin
  FCaption.Assign(Value);

  DoUpdated;
end;

procedure TCustomAwesomeButton.SetUseAllSpace(const Value: Boolean);
begin
  if FUseAllSpace <> Value then
  begin
    FUseAllSpace := Value;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;

    DoOnVisibilityChanged;

    DoUpdated;
  end;
end;

procedure TCustomAwesomeButton.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;

    DoUpdated;
  end;
end;

{ TAwesomeButtonCollection }

function TAwesomeButtonCollection.Add: TAwesomeButton;
begin
  Result := TAwesomeButton(inherited Add);
end;

function TAwesomeButtonCollection.GetItem(Index: Integer): TAwesomeButton;
begin
  Result := inherited GetItem(Index) as TAwesomeButton;
end;

procedure TAwesomeButtonCollection.SetItem(Index: Integer;
  Value: TAwesomeButton);
begin
  inherited SetItem(Index, Value);
end;

procedure TAwesomeButtonCollection.Update(Item: TCollectionItem);
var
  AwesomeEdit: IAwesomeEdit;
begin
  inherited;

  if Supports(Owner, IAwesomeEdit, AwesomeEdit) then
  begin
    AwesomeEdit.ButtonListUpdated(Self);
  end;
end;

end.

