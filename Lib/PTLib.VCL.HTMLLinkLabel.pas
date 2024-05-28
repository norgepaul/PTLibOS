{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLinkLabel.pas, released 2002-01-06.

The Initial Developer of the Original Code is David Polberger <dpol att swipnet dott se>
Portions created by David Polberger are Copyright (C) 2002 David Polberger.
All Rights Reserved.

Contributor(s): Bianconi, Cetkovsky

Current Version: 2.00

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
  Please see the accompanying documentation.
Description:
  LinkLabel.pas contains the main component, TJvLinkLabel, a rich-text label.
  It makes use of the renderer and parser stored in Renderer.pas and Parser.pas,
  respectively.

  Note: Documentation for this unit can be found in Doc\Source.txt and
        Doc\Readme.txt!
-----------------------------------------------------------------------------}
// $Id$

unit PTLib.VCL.HTMLLinkLabel;

interface

uses
  Windows, WinAPI.Messages,

  System.SysUtils, System.Classes, System.Types, System.UITypes,

  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics,

  PTLib.Common.Classes,
  PTLib.VCL.HTMLLinkLabel.Parser,
  PTLib.VCL.HTMLLinkLabel.Renderer,
  PTLib.VCL.HTMLLinkLabel.Tree;

type
  ELinkLabelError = class(EPTLibError);

  TLinkClickEvent = procedure(Sender: TObject; LinkNumber: Integer;
    LinkText, LinkParam: string) of object;  // added LinkParam by Cetkovsky
  TDynamicTagInitEvent = procedure(Sender: TObject; out Source: string;
    Number: Integer) of object;

  TCustomPTLibHTMLLinkLabel = class(TGraphicControl, IDynamicNodeHandler)
  private
    FText: TStringList;
    FRenderer: IRenderer;
    FActiveLinkNode: TLinkNode;
    FHotLinks: Boolean;
    FLinkCursor: TCursor;
    FAutoHeight: Boolean;
    FMarginWidth: Integer;
    FMarginHeight: Integer;
    FOriginalCursor: TCursor;
    FOnCaptionChanged: TNotifyEvent;
    FOnLinkClick: TLinkClickEvent;
    FOnDynamicTagInit: TDynamicTagInitEvent;
    FParser: IParser;
    FLayout: TTextLayout;
    FCaption: TCaption;
    FMouseOver: Boolean;

    procedure SetText(const Value: TCaption);
    procedure SetTransparent(const Value: Boolean);
    function GetLinkColor: TColor;
    function GetLinkStyle: TFontStyles;
    procedure SetLinkColor(const Value: TColor);
    procedure SetLinkStyle(const Value: TFontStyles);
    procedure SynchronizeRootAndFont;
    function GetLinkColorClicked: TColor;
    procedure SetLinkColorClicked(const Value: TColor);
    function GetLinkColorHot: TColor;
    procedure SetLinkColorHot(const Value: TColor);
    procedure ActivateLinkNodeAtPos(const P: TPoint; State: TLinkState);
    procedure DeactivateActiveLinkNode;
    procedure HandleDynamicNode(out Source: string; const Node: TDynamicNode);
    function GetTransparent: Boolean;
    function IsActiveLinkNodeClicked: Boolean;
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
    procedure SetLayout(AValue: TTextLayout);
  protected
    FNodeTree: TNodeTree;

    function BaseWndProc(Msg: Cardinal; WParam: WPARAM = 0; LParam: LPARAM = 0): LRESULT;

    procedure TextChanged; reintroduce; virtual;
    procedure FontChanged; reintroduce; dynamic;
    procedure Paint; override;
    procedure DrawBackground(NodeAtPoint: TLinkNode);
    function CreateParser: IParser; virtual;
    function CreateRenderer: IRenderer; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(Control: TControl); reintroduce; dynamic;
    procedure DoCaptionChanged; virtual;
    procedure DoLinkClicked(LinkNumber: Integer; LinkText, LinkParam: string); virtual;  // added LinkParam by Cetkovsky
    procedure DoDynamicTagInit(out Source: string; Number: Integer); virtual;
    property Parser: IParser read FParser;
    property Renderer: IRenderer read FRenderer;
    property Caption: TCaption read FCaption write SetText;
    property Text: TStrings read GetStrings write SetStrings;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LinkColor: TColor read GetLinkColor write SetLinkColor default clBlue;
    property LinkColorClicked: TColor read GetLinkColorClicked write SetLinkColorClicked default clRed;
    property LinkColorHot: TColor read GetLinkColorHot write SetLinkColorHot default clPurple;
    property LinkCursor: TCursor read FLinkCursor write FLinkCursor default crHandPoint;
    property LinkStyle: TFontStyles read GetLinkStyle write SetLinkStyle default [fsUnderline];
    property HotLinks: Boolean read FHotLinks write FHotLinks default False;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property MarginWidth: Integer read FMarginWidth write SetMarginWidth default 0;
    property MarginHeight: Integer read FMarginHeight write SetMarginHeight default 0;
    property OnDynamicTagInit: TDynamicTagInitEvent read FOnDynamicTagInit write FOnDynamicTagInit;
    property OnCaptionChanged: TNotifyEvent read FOnCaptionChanged write FOnCaptionChanged;
    property OnLinkClick: TLinkClickEvent read FOnLinkClick write FOnLinkClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpdateDynamicTag(Number: Integer; const Source: string);
    function GetDynamicTagContents(Number: Integer): string;
  end;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TPTLibHTMLLinkLabel = class(TCustomPTLibHTMLLinkLabel)
  published
    property Caption;
    property Text;
    property Anchors;
    property Transparent;
    property Layout;
    property LinkColor;
    property LinkColorClicked;
    property LinkColorHot;
    property LinkCursor;
    property LinkStyle;
    property HotLinks;
    property AutoHeight;
    property MarginWidth;
    property MarginHeight;

    property OnDynamicTagInit;
    property OnCaptionChanged;
    property OnLinkClick;

    property Enabled;                 // Cetkovsky

    property Align;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Font;
    property Height default 17;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 160;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseMove;
  end;

implementation

const
  crNewLinkHand = 1;

procedure CreateWMMessage(var Mesg: TMessage; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM);
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WParam;
  Mesg.LParam := LParam;
  Mesg.Result := 0;
end;

constructor TCustomPTLibHTMLLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinkCursor := crHandPoint;
  FText := TStringList.Create;
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  //IncludeThemeStyle(Self, [csParentBackground]);
  Width := 160;
  Height := 17;
  FNodeTree := TNodeTree.Create;
  FAutoHeight := True;

  // Give descendant components an opportunity to replace the default classes
  FParser := CreateParser;
  FParser.SetDynamicNodeHandler(Self);
  FRenderer := CreateRenderer;

  FLayout := tlTop;

  SetLinkColor(clBlue);
  SetLinkColorClicked(clBlue);
  SetLinkColorHot(clBlue);
  SetLinkStyle([fsUnderline]);

  Transparent := True;
  ParentColor := True;
end;

destructor TCustomPTLibHTMLLinkLabel.Destroy;
begin
  FNodeTree.Free;
  FText.Free;
  inherited Destroy;
end;

function TCustomPTLibHTMLLinkLabel.BaseWndProc(Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT;
var
  Mesg: TMessage;
begin
  CreateWMMessage(Mesg, Msg, WParam, LParam);

  inherited WndProc(Mesg);

  Result := Mesg.Result;
end;

procedure TCustomPTLibHTMLLinkLabel.ActivateLinkNodeAtPos(const P: TPoint; State: TLinkState);
var
  NodeAtPoint: TLinkNode;
  Pt: TPoint;
  TmpRect: TRect;

  function IsNewNode: Boolean;
  begin
    { We must only redraw the TLinkNode if it either isn't the same as the
      currently active TLinkNode (FActiveLinkNode), or if we're trying to change
      the state (that is, alter the color). }
    Result := (FActiveLinkNode <> NodeAtPoint);
    if not Result and Assigned(FActiveLinkNode) then
      Result := FActiveLinkNode.State <> State;
  end;

begin
  // Changes Control's canvas point to relative coordinates
  Pt := Point(P.X - FNodeTree.Root.StartingPoint.X,P.Y - FNodeTree.Root.StartingPoint.Y);

  if FNodeTree.IsPointInNodeClass(Pt, TLinkNode) then
  begin
    NodeAtPoint := FNodeTree.GetNodeAtPointOfClass(Pt, TLinkNode) as TLinkNode;
    if Assigned(NodeAtPoint) and IsNewNode then
    begin
      DeactivateActiveLinkNode;
      NodeAtPoint.State := State;
      FActiveLinkNode := NodeAtPoint;
      TmpRect := ClientRect;
      InflateRect(TmpRect, -FMarginWidth, -FMarginHeight);
      Canvas.Lock;
      try
        DrawBackground(NodeAtPoint);
        FRenderer.RenderNode(Canvas, TmpRect, NodeAtPoint);
      finally
        Canvas.Unlock;
      end;
    end;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.DeactivateActiveLinkNode;
var
  TmpRect: TRect;
begin
  if Assigned(FActiveLinkNode) then
  try
    FActiveLinkNode.State := lsNormal;
    TmpRect := ClientRect;
    InflateRect(TmpRect, -FMarginWidth, -FMarginHeight);
    Canvas.Lock;
    try
      DrawBackground(FActiveLinkNode);
      FRenderer.RenderNode(Canvas, TmpRect, FActiveLinkNode);
    finally
      Canvas.Unlock;
    end;
  finally
    FActiveLinkNode := nil;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.FontChanged;

  procedure ClearWordInfo;
  var
    Enum: INodeEnumerator;
  begin
    Enum := FNodeTree.GetTopLevelNodeEnumerator(TStringNode);
    while Enum.HasNext do
      (Enum.GetNext as TStringNode).ClearWordInfo;
  end;

begin
  BaseWndProc(CM_FONTCHANGED);

  SynchronizeRootAndFont;
  ClearWordInfo;
  Invalidate;
end;

procedure TCustomPTLibHTMLLinkLabel.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;

  FMouseOver := False;

  BaseWndProc(CM_MOUSELEAVE, 0, Windows.LPARAM(Control));

  //if Assigned(FOnMouseLeave) then
  //  FOnMouseLeave(Self);

  if FHotLinks and not IsActiveLinkNodeClicked then
    DeactivateActiveLinkNode;
end;

procedure TCustomPTLibHTMLLinkLabel.TextChanged;
begin
  BaseWndProc(CM_TEXTCHANGED);

  Invalidate;
end;

function TCustomPTLibHTMLLinkLabel.CreateParser: IParser;
begin
  { Descendant components wishing to use another parser (implementing the
    IParser interface) should override this routine and provide their own. A
    pointer to this object should be returned.

    function TMyLinkLabel.CreateParser: IParser;
    begin
      Result := TMyParser.Create;
    end; }
  Result := TDefaultParser.Create;
end;

function TCustomPTLibHTMLLinkLabel.CreateRenderer: IRenderer;
begin
  // Please refer to the comment in TCustomPTLibHTMLLinkLabel.CreateParser above.
  Result := TDefaultRenderer.Create;
end;

procedure TCustomPTLibHTMLLinkLabel.DoCaptionChanged;
begin
  if Assigned(FOnCaptionChanged) then
    FOnCaptionChanged(Self);
end;

procedure TCustomPTLibHTMLLinkLabel.DoDynamicTagInit(out Source: string; Number: Integer);
begin
  if Assigned(FOnDynamicTagInit) then
    FOnDynamicTagInit(Self, Source, Number);
end;

 // added LinkParam by Cetkovsky
procedure TCustomPTLibHTMLLinkLabel.DoLinkClicked(LinkNumber: Integer; LinkText, LinkParam: string);
begin
  if Assigned(FOnLinkClick) then
    FOnLinkClick(Self, LinkNumber, LinkText, LinkParam);
end;

procedure TCustomPTLibHTMLLinkLabel.DrawBackground(NodeAtPoint: TLinkNode);
var
  TmpR: TRect;
  Enum: IRectEnumerator;
begin
  if (NodeAtPoint <> nil) and (Parent <> nil) and Parent.HandleAllocated then
  begin
    Enum := NodeAtPoint.GetRectEnumerator;
    if Enum.HasNext then
    begin
      TmpR := Enum.GetNext;
      while Enum.HasNext do
        UnionRect(TmpR, TmpR, Enum.GetNext);
    end;

    if Transparent then
    begin
      OffsetRect(TmpR, Left, Top);
      PerformEraseBackground(Self, Canvas.Handle);//, Point(0, 0), TmpR);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(TmpR);
    end;
  end;
end;

function TCustomPTLibHTMLLinkLabel.GetDynamicTagContents(Number: Integer): string;
var
  Node: TAreaNode;
begin
  { Note that the output of this method is not serialized, that is, it will be
    plain text, with no tags present. In other words, simply the contents of
    the TStringNodes owned by the sought TDynamicNode. }
  Node := FNodeTree.GetSpecificNodeOfClass(Number, TDynamicNode) as TAreaNode;
  if Assigned(Node) then
    Result := Node.Text
  else
    raise ELinkLabelError.Create('Unable To Locate Mode');
end;

function TCustomPTLibHTMLLinkLabel.GetLinkColor: TColor;
begin
  Result := FRenderer.LinkColor;
end;

function TCustomPTLibHTMLLinkLabel.GetLinkColorClicked: TColor;
begin
  Result := FRenderer.LinkColorClicked;
end;

function TCustomPTLibHTMLLinkLabel.GetLinkColorHot: TColor;
begin
  Result := FRenderer.LinkColorHot;
end;

function TCustomPTLibHTMLLinkLabel.GetLinkStyle: TFontStyles;
begin
  Result := FRenderer.LinkStyle;
end;

function TCustomPTLibHTMLLinkLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TCustomPTLibHTMLLinkLabel.HandleDynamicNode(out Source: string; const Node: TDynamicNode);
begin
  if Assigned(Node) then
    DoDynamicTagInit(Source, Node.Number);
end;

function TCustomPTLibHTMLLinkLabel.IsActiveLinkNodeClicked: Boolean;
begin
  Result := Assigned(FActiveLinkNode);
  if Result then
    Result := FActiveLinkNode.State = lsClicked;
end;

procedure TCustomPTLibHTMLLinkLabel.Loaded;
begin
  inherited Loaded;
  FOriginalCursor := Cursor;
end;

procedure TCustomPTLibHTMLLinkLabel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  // Ignore Mouse down
  //inherited MouseDown(Button, Shift, X, Y);
  //ActivateLinkNodeAtPos(Point(X, Y), lsClicked);
end;

procedure TCustomPTLibHTMLLinkLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  inherited MouseMove(Shift, X, Y);

  Pt := Point(X - FNodeTree.Root.StartingPoint.X,Y - FNodeTree.Root.StartingPoint.Y);
  if FNodeTree.IsPointInNodeClass(Pt, TLinkNode) then
  begin
    Cursor := LinkCursor;
    if FHotLinks and not IsActiveLinkNodeClicked then
      ActivateLinkNodeAtPos(Point(X, Y), lsHot);
  end
  else
  begin
    Cursor := FOriginalCursor;
    if FHotLinks and not IsActiveLinkNodeClicked then
      DeactivateActiveLinkNode;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  NodeAtPoint: TLinkNode;
  Pt: TPoint;
begin
  Pt := Point(X - FNodeTree.Root.StartingPoint.X,Y - FNodeTree.Root.StartingPoint.Y);

  if FNodeTree.IsPointInNodeClass(Pt, TLinkNode) then
  begin
    NodeAtPoint := FNodeTree.GetNodeAtPointOfClass(Pt, TLinkNode) as TLinkNode;

    if Assigned(NodeAtPoint) then
      DoLinkClicked(NodeAtPoint.Number, NodeAtPoint.Text, NodeAtPoint.Param);
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.Paint;
var
  TmpBmp: TBitmap;
  TmpRect: TRect;
begin
  TmpBmp := nil;
  if Assigned(FNodeTree) then
  begin
    (*if not Transparent then
    begin
      // repaint canvas
      DrawThemedBackground(Self, Canvas, ClientRect);
    end;*)

    try
      Canvas.Font := Font;
      TmpBmp := TBitmap.Create;
      TmpRect := ClientRect;
      TmpBmp.Canvas.Brush.Color := Color;
      TmpBmp.Canvas.Brush.Style := bsSolid;
      TmpBmp.Height := TmpRect.Bottom - (FMarginHeight shl 1) + 1;  // TmpRect.Top = 0, ignore it
      TmpBmp.Width  := TmpRect.Right - (FMarginWidth shl 1) + 1;    // TmpRect.left = 0, ignore it
      TmpBmp.Canvas.Font.Assign(Canvas.Font);
      TmpBmp.Canvas.Pen.Assign(Canvas.Pen);

      if Transparent then
      begin
        TmpBmp.Canvas.CopyRect(ClientRect, Canvas, ClientRect);
        TmpBmp.Canvas.Brush.Style := bsClear;
      end;
      Canvas.Brush.Style := bsClear;

      // Set new start point
      // The new start point is relative to temporary canvas, Left & Top Corner
      FNodeTree.Root.StartingPoint := Point(0,0);
      FRenderer.RenderTree(TmpBmp.Canvas, Rect(0,0,TmpBmp.Width - 1,TmpBmp.Height - 1), FNodeTree);

      //  Set new height e don't draw in this pass.
      //  Wait for next paint event.
      //  Allow correctly layout position and improve some performance
      if FAutoHeight and
        (Align in [alNone, alTop, alBottom]) and
        (ClientHeight <> (FRenderer.GetTextHeight + (FMarginHeight shl 1)) ) then
        ClientHeight := FRenderer.GetTextHeight + (FMarginHeight shl 1)
      else
      begin
        TmpRect := ClientRect;
        InflateRect(TmpRect, -FMarginWidth, -FMarginHeight);

        case FLayout of
          tlTop:
            begin
              // Nothing to do
            end;
          tlCenter:
            begin
              TmpRect.Top := TmpRect.Top +
                (TmpRect.Bottom - TmpRect.Top - FRenderer.GetTextHeight) div 2;
              if TmpRect.Top < FMarginHeight then
                TmpRect.Top := FMarginHeight;
            end;
          tlBottom:
            begin
              TmpRect.Top := TmpRect.Bottom - FRenderer.GetTextHeight;
              if TmpRect.Top < FMarginHeight then
                TmpRect.Top := FMarginHeight;
            end;
        end;
        // Adjust Root start point relative to control's canvas.
        FNodeTree.Root.StartingPoint := Point(TmpRect.Left, TmpRect.Top);
        Canvas.Draw(TmpRect.Left, TmpRect.Top, TmpBmp);
      end;
    finally
      TmpBmp.Free;
    end;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    Invalidate;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetText(const Value: TCaption);
begin
  if Value <> Caption then
  begin
    Text.Clear;
    FCaption := Value;
    Text.Add(Caption);
    FActiveLinkNode := nil; // We're about to free the tree containing the node it's pointing to
    FNodeTree.Free;
    ResetNodeCount;
    FNodeTree := FParser.Parse(Value);
    SynchronizeRootAndFont;
    Invalidate;
    DoCaptionChanged;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetLinkColor(const Value: TColor);
begin
  if Value <> GetLinkColor then
  begin
    FRenderer.LinkColor := Value;
    Invalidate;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetLinkColorClicked(const Value: TColor);
begin
  if Value <> GetLinkColorClicked then
    FRenderer.LinkColorClicked := Value;
end;

procedure TCustomPTLibHTMLLinkLabel.SetLinkColorHot(const Value: TColor);
begin
  FRenderer.LinkColorHot := Value;
end;

procedure TCustomPTLibHTMLLinkLabel.SetLinkStyle(const Value: TFontStyles);
begin
  if Value <> GetLinkStyle then
  begin
    FRenderer.LinkStyle := Value;
    Invalidate;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetMarginHeight(const Value: Integer);
begin
  if FMarginHeight <> Value then
  begin
    FMarginHeight := Value;
    Resize;
    Invalidate;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetMarginWidth(const Value: Integer);
begin
  if FMarginWidth <> Value then
  begin
    FMarginWidth := Value;
    Resize;
    Invalidate;
  end;
end;

function TCustomPTLibHTMLLinkLabel.GetStrings: TStrings;
begin
  Result := FText;
end;

procedure TCustomPTLibHTMLLinkLabel.SetStrings(const Value: TStrings);
begin
  FText.Assign(Value);  SetText(FText.Text);
end;

procedure TCustomPTLibHTMLLinkLabel.SetLayout(AValue: TTextLayout);
begin
  if FLayout <> AValue then
  begin
    FLayout := AValue;
    Invalidate;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SetTransparent(const Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
    begin
      ControlStyle := ControlStyle - [csOpaque];
      //ExcludeThemeStyle(Self, [csParentBackground]);
    end
    else
    begin
      ControlStyle := ControlStyle + [csOpaque];
      //IncludeThemeStyle(Self, [csParentBackground]);
    end;
    Invalidate;
  end;
end;

procedure TCustomPTLibHTMLLinkLabel.SynchronizeRootAndFont;
begin
  if Assigned(FNodeTree) then
    with FNodeTree.Root do
    begin
      Styles := Font.Style;
      Color := Font.Color;
    end;
end;

procedure TCustomPTLibHTMLLinkLabel.UpdateDynamicTag(Number: Integer; const Source: string);
var
  NodeEnum: INodeEnumerator;
  Parser: IParser;
  CurrentNode: TDynamicNode;
begin
  NodeEnum := FNodeTree.GetTopLevelNodeEnumerator(TDynamicNode);
  while NodeEnum.HasNext do
  begin
    CurrentNode := NodeEnum.GetNext as TDynamicNode;
    if CurrentNode.Number = Number then
    begin
      Parser := CreateParser;
      CurrentNode.DestroyChildren;
      Parser.AddSourceTreeToDynamicNode(CurrentNode, Source);
      Repaint;
      Exit;
    end;
  end;

  raise ELinkLabelError.Create('Tag Not Found');
end;

end.
