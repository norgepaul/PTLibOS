unit PTLib.VCL.VirtualTrees;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes,
  System.Generics.Collections,

  Windows,

  Vcl.Dialogs, Vcl.ImgList, Vcl.Controls, Vcl.Graphics,

  VirtualTrees, VirtualTrees.Header, VirtualTrees.Types,

  PTLib.Common.Utils,
  PTLib.Common.Interfaces,
  PTLib.Common.Strings,

  PTLib.Vcl.Clipboard,
  PTLib.Vcl.HourGlass,
  PTLib.Vcl.Graphics;

type
  TNodeIndices = TDictionary<PVirtualNode, Cardinal>;

  TVTExportOption = (
    vteSelectedOnly,
    vteVisibleOnly,
    vteQuoteStrings,
    vteIncludeHeader,
    vteIncludeChildren
  );
  TVTExportOptions = set of TVTExportOption;

  TRealIndexNodeData = record
    NodeIndex: Cardinal;
  end;
  PRealIndexNodeData = ^TRealIndexNodeData;

  TTreeOption2 = (
    to2DoubleClickHeaderResize,
    toAutoCheckSelectedNodes,
    toHTMLSupport,
    toSupressEventsOnAutoCheckNodes,
    toUseDataAsNodeIndex
  );
  TTreeOptions2 = set of TTreeOption2;

  TCheckTypes = (
    lcCheck,
    lcUnCheck,
    lcReverseCheck
  );

  TSelectionType = (
    stSingleSelect,
    stMultiSelect,
    stCheckBoxes
  );

  TCheckControl = (
    ccNone,
    ccForceChildrenChecked,
    ccForceChildrenUnchecked,
    ccForceParentsChecked,
    ccForceParentsUnchecked
  );
  TCheckControls = set of TCheckControl;

  TOnUserMessage = procedure(Sender: TObject; UserMessage: String) of object;
  TOnIsNodeSearchHit = procedure(Sender: TObject; const Node: PVirtualNode; const SearchTerms: TStringList; var IsHit: Boolean) of object;

  TNodeProc = reference to function(Node: PVirtualNode): Boolean;

  TPTLibBaseVirtualTree = class(TCustomVirtualStringTree, IConfigurationProvider)
  private
    FOnUserMessage: TOnUserMessage;
    FChecking: Boolean;
    FOnIsNodeSearchHit: TOnIsNodeSearchHit;

    procedure SetInternalImages(const Value: TCustomImageList);
    function GetInternalImages: TCustomImageList;
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
    procedure SetHintText(const Value: String);
    function GetNodeOrRootNode(const Node: PVirtualNode): PVirtualNode;
  protected
    FSaveLoadWithParentForm: Boolean;
    FTreeOptions2: TTreeOptions2;
    FInternalImages: TImageList;
    FSearchStrings: TStringList;
    FSearchText: String;
    FNodeIndices: TNodeIndices;
    FHintText: String;
    FSearchColumnHeader: String;
    FSearchExactMatch: Boolean;

    procedure DoOnUserMessage(const UserMMessage: String); virtual;
    function DrawHTML(ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer; virtual;

    procedure LoadColumnSettings(Config: IConfiguration; RootKey: String);
    procedure SaveColumnSettings(Config: IConfiguration; RootKey: String);
    function GetSaveLoadWithParentForm: Boolean;
//    procedure LoadPngImagesFromResource(const Resourcename: String); virtual;
    procedure AddDefaultColumns(const ColumnNames: Array of String; const ColumnWidths: Array of Integer);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetOptionsClass: TTreeOptionsClass; override;
//    procedure DoHeaderClick({$IF CompilerVersion >= 29.0}const{$endif} HitInfo: TVTHeaderHitInfo); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure CheckNodes(const Node: PVirtualNode; const Checked: Boolean); virtual;
    procedure CheckNode(Node: PVirtualNode; const Checked: Boolean); virtual;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure DoAfterPaint(Canvas: TCanvas); override;
    procedure Loaded; override;
    procedure UpdateVisibleItems(const OnlyUpdateVisible: Boolean = False; const ScrollFocusedIntoView: Boolean = True); virtual;
    function IsNodeVisible(const Node: PVirtualNode): Boolean; virtual;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    procedure InternalDisconnectNode(Node: PVirtualNode; KeepFocus: Boolean; Reindex: Boolean = True; ParentClearing: Boolean = False); override;
    function IsSearchHit(const SearchTerms: TStringList; const SearchText: String): Boolean;

    property SaveLoadWithParentForm: Boolean read GetSaveLoadWithParentForm write FSaveLoadWithParentForm;
    property TreeOptions2: TTreeOptions2 read FTreeOptions2 write FTreeOptions2;

    property InternalImages: TCustomImageList read GetInternalImages write SetInternalImages;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    property HintText: String read FHintText write SetHintText;

    property OnUserMessage: TOnUserMessage read FOnUserMessage write FOnUserMessage;
    property OnIsNodeSearchHit: TOnIsNodeSearchHit read FOnIsNodeSearchHit write FOnIsNodeSearchHit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Search(const SearchText: String; const ColumnHeader: String = ''; const ExactMatch: Boolean = False; const SearchAll: Boolean = False);
    procedure SearchColumns(const SearchText: String; const IgnoreColumns: TArray<Integer>);

    function VisibleChildCount(const Node: PVirtualNode): Integer;
    procedure GetNodeText(const Node: PVirtualNode; const ColumnIndex: TColumnIndex; const TextType: TVSTTextType; out CellText: String); overload;
    function GetNodeText(const Node: PVirtualNode; const ColumnIndex: TColumnIndex; const TextType: TVSTTextType): String; overload;
    procedure CopyToClipboard(const Options: TVTExportOptions;
      const FirstColumn: Integer = -1; const LastColumn: Integer = -1); reintroduce; virtual;
    procedure SaveToFile(const Filename: String; const Options: TVTExportOptions); virtual;
    procedure SaveToFileWithDialog(const Options: TVTExportOptions); virtual;
    //procedure AutoFitColumns(Animated: Boolean = False; SmartAutoFitType: TSmartAutoFitType = TSmartAutoFitType.smaUseColumnOption;
    //  RangeStartCol: Integer = NoColumn; RangeEndCol: Integer = NoColumn; IncludeHeader: Boolean = True);
    procedure ToStrings(const Values: TStrings; const Options: TVTExportOptions = [vteIncludeHeader, vteQuoteStrings];
      const FirstColumn: Integer = -1; const LastColumn: Integer = -1); virtual;
    procedure CheckItems(const CheckType: TCheckTypes; const OnlySelected: Boolean);
    function GetNodeFromIndex(const Index: Cardinal; const ParentNode: PVirtualNode = nil): PVirtualNode;
    procedure SelectNodeByIndex(const Index: Cardinal; const ParentNode: PVirtualNode = nil);
    procedure SelectNodeEx(const Node: PVirtualNode; ScrollNodeIntoView: Boolean = True);
    procedure SelectSiblings(Node: PVirtualNode);
    function IterateSelectedNodes(const NodeProc: TNodeProc): Boolean;
    procedure DeleteSelectedNodesRefocus;
    procedure HideSelectedNodesRefocus(DoHide: Boolean = True);
    procedure PrintToBMP(Image: TBitmap; PrintHeader: Boolean);
    function GetLastSelected: PVirtualNode;
    procedure CollapseAllBelow(const Node: PVirtualNode);
    procedure ExpandAllBelow(Node: PVirtualNode); virtual;
    function GetNextNodeAfterSelectedNodesRemoved: PVirtualNode;
    function ColumnByName(const ColumnName: String): TVirtualTreeColumn;
    procedure SaveToConfig(const Config: IConfiguration; RootKey: String); virtual;
    procedure LoadFromConfig(const Config: IConfiguration; RootKey: String); virtual;
    function GetRealNodeIndex(Node: PVirtualNode): Cardinal;
    procedure ScaleNodesToFontSize;
    procedure MakeChildrenVisible(const Node: PVirtualNode; const IsVisible: Boolean);
  end;

  TPTLibVirtualTree = class(TPTLibBaseVirtualTree)
  published
    property OnUserMessage;

    property SaveLoadWithParentForm;
    property TreeOptions2;
    property HintText;

    property AccessibleName;
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderStyle;
    property BottomSpace;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property Ctl3D;
    property CustomCheckImages;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DefaultText;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    {$WARNINGS OFF}
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    {$WARNINGS ON}
    property Visible;
    property WantTabs;

    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterGetMaxColumnWidth;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnCanSplitterResizeColumn;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnCompareNodes;
    {$ifdef COMPILER_5_UP}
      property OnContextPopup;
    {$endif COMPILER_5_UP}
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollbar;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
    property OnIsNodeSearchHit;
  end;

implementation

resourcestring
  StrSave = 'Save';
  StrTextFilesTxt = 'Text files (*.txt)|*.txt|All files (*.*)|*.*';

//{$R ShareBike.dres}

{ TShareBikeBaseCustomVirtualTree }

procedure TPTLibBaseVirtualTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
begin
  FNodeIndices.AddOrSetValue(Node, Node.Index);

  inherited;
end;

function TPTLibBaseVirtualTree.GetRealNodeIndex(Node: PVirtualNode): Cardinal;
begin
  Assert(Node <> nil, 'Node cannot be nil');

  if not (vsInitialized in Node.States) then
    InitNode(Node);

  Result := FNodeIndices[Node];
end;

constructor TPTLibBaseVirtualTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInternalImages := TImageList.Create(Self);
  FSearchStrings := TStringList.Create;
  FNodeIndices := TNodeIndices.Create;
  DefaultNodeHeight := 20;

  Header.Options := Header.Options + [hoDblClickResize, hoHeaderClickAutoSort];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions - [toDisableAutoscrollOnFocus];
  Header.Options := Header.Options - [hoShowSortGlyphs];
  TreeOptions2 := TreeOptions2 + [toAutoCheckSelectedNodes, toSupressEventsOnAutoCheckNodes];
  Font.Size := 10;
  Header.Font.Size := 10;
  //TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toUseExplorerTheme];
end;

destructor TPTLibBaseVirtualTree.Destroy;
begin
  FreeAndNil(FInternalImages);
  FreeAndNil(FSearchStrings);
  FreeAndNil(FNodeIndices);

  inherited;
end;

function TPTLibBaseVirtualTree.DrawHTML(ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer;
(*DrawHTML - Draws text on a canvas using tags based on a simple subset of HTML/CSS

  <B> - Bold e.g. <B>This is bold</B>
  <I> - Italic e.g. <I>This is italic</I>
  <U> - Underline e.g. <U>This is underlined</U>
  <font-color=x> Font colour e.g.
                <font-color=clRed>Delphi red</font-color>
                <font-color=#FFFFFF>Web white</font-color>
                <font-color=$000000>Hex black</font-color>
  <font-size=x> Font size e.g. <font-size=30>This is some big text</font-size>
  <font-family> Font family e.g. <font-family=Arial>This is arial</font-family>*)

  function CloseTag(const ATag: String): String;
  begin
    Result := concat('/', ATag);
  end;

  function GetTagValue(const ATag: String): String;
  var
    p: Integer;
  begin
    p := pos('=', ATag);

    if p = 0 then
      Result := ''
    else
      Result := copy(ATag, p + 1, MaxInt);
  end;

  function ColorCodeToColor(const Value: String): TColor;
  var
    HexValue: String;
  begin
    Result := 0;

    if Value <> '' then
    begin
      if (length(Value) >= 2) and (copy(Uppercase(Value), 1, 2) = 'CL') then
      begin
        // Delphi colour
        Result := StringToColor(Value);
      end else
      if Value[1] = '#' then
      begin
        // Web colour
        HexValue := copy(Value, 2, 6);

        Result := RGB(StrToInt('$'+Copy(HexValue, 1, 2)),
                      StrToInt('$'+Copy(HexValue, 3, 2)),
                      StrToInt('$'+Copy(HexValue, 5, 2)));
      end
      else
        // Hex or decimal colour
        Result := StrToIntDef(Value, 0);
    end;
  end;

const
  TagBold = 'B';
  TagItalic = 'I';
  TagUnderline = 'U';
  TagBreak = 'BR';
  TagFontSize = 'FONT-SIZE';
  TagFontFamily = 'FONT-FAMILY';
  TagFontColour = 'FONT-COLOR';
  TagColour = 'COLOUR';

var
  x, y, idx, CharWidth, MaxCharHeight: NativeInt;
  CurrChar: Char;
  Tag, TagValue: String;
  PreviousFontColour: TColor;
  PreviousFontFamily: String;
  PreviousFontSize: Integer;
  PreviousColour: TColor;
  FoundTag: Boolean;
  OldIdx: Integer;
begin
  Result := 0;

  ACanvas.Font.Size := Canvas.Font.Size;
  ACanvas.Font.Name := Canvas.Font.Name;
  ACanvas.Font.Color := Canvas.Font.Color;
  ACanvas.Font.Style := Canvas.Font.Style;

  PreviousFontColour := ACanvas.Font.Color;
  PreviousFontFamily := ACanvas.Font.Name;
  PreviousFontSize := ACanvas.Font.Size;
  PreviousColour := ACanvas.Brush.Color;

  InflateRect(ARect, -(Indent + Cardinal(TextMargin)) , 0);

  x := ARect.Left;
  y := ARect.Top + 1;
  idx := 1;

  MaxCharHeight := ACanvas.TextHeight('Ag');

  While idx <= length(Text) do
  begin
    CurrChar := Text[idx];

    FoundTag := False;

    // Is this a tag?
    if CurrChar = '<' then
    begin
      OldIdx := Idx;
      FoundTag := True;

      Tag := '';

      inc(idx);

      // Find the end of then tag
      while (Text[idx] <> '>') and (idx <= length(Text)) do
      begin
        Tag := concat(Tag,  UpperCase(Text[idx]));

        inc(idx);
      end;

      ///////////////////////////////////////////////////
      // Simple tags
      ///////////////////////////////////////////////////
      if Tag = TagBold then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsBold] else

      if Tag = TagItalic then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic] else

      if Tag = TagUnderline then
        ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline] else

      if Tag = TagBreak then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end else

      ///////////////////////////////////////////////////
      // Closing tags
      ///////////////////////////////////////////////////
      if Tag = CloseTag(TagBold) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsBold] else

      if Tag = CloseTag(TagItalic) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic] else

      if Tag = CloseTag(TagUnderline) then
        ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline] else

      if Tag = CloseTag(TagFontSize) then
        ACanvas.Font.Size := PreviousFontSize else

      if Tag = CloseTag(TagFontFamily) then
        ACanvas.Font.Name := PreviousFontFamily else

      if Tag = CloseTag(TagFontColour) then
        ACanvas.Font.Color := PreviousFontColour else

      if Tag = CloseTag(TagColour) then
        ACanvas.Brush.Color := PreviousColour else

      ///////////////////////////////////////////////////
      // Tags with values
      ///////////////////////////////////////////////////
      begin
        // Get the tag value (everything after '=')
        TagValue := GetTagValue(Tag);

        if TagValue = '' then
        begin
          FoundTag := False;
          Idx := OldIdx;
        end
        else
        begin
          // Remove the value from the tag
          Tag := copy(Tag, 1, pos('=', Tag) - 1);

          if Tag = TagFontSize then
          begin
            PreviousFontSize := ACanvas.Font.Size;
            ACanvas.Font.Size := StrToIntDef(TagValue, ACanvas.Font.Size);
          end else

          if Tag = TagFontFamily then
          begin
            PreviousFontFamily := ACanvas.Font.Name;
            ACanvas.Font.Name := TagValue;
          end else

          if Tag = TagFontColour then
          begin
            PreviousFontColour := ACanvas.Font.Color;

            try
              ACanvas.Font.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end else

          if Tag = TagColour then
          begin
            PreviousColour := ACanvas.Brush.Color;

            try
              ACanvas.Brush.Color := ColorCodeToColor(TagValue);
            except
              //Just in case the canvas colour is invalid
            end;
          end else
          begin
            FoundTag := False;
            Idx := OldIdx;
          end;
        end;
      end;
    end;

    // Draw the character if it's not a ctrl char
    if (CurrChar >= #32) and (not FoundTag) then
    begin
      CharWidth := ACanvas.TextWidth(CurrChar);

      {if x + CharWidth > ARect.Right then
      begin
        x := ARect.Left;

        inc(y, MaxCharHeight);
      end; }

      if y + MaxCharHeight < ARect.Bottom then
      begin
        ACanvas.Brush.Style := bsClear;

        ACanvas.TextOut(x, y, CurrChar);
      end;

      x := x + CharWidth;
      Result := Result + CharWidth;
    end;

    inc(idx);
  end;
end;

function TPTLibBaseVirtualTree.GetNodeText(const Node: PVirtualNode;
  const ColumnIndex: TColumnIndex; const TextType: TVSTTextType): String;
begin
  GetNodeText(Node, ColumnIndex, TextType, Result);
end;

procedure TPTLibBaseVirtualTree.GetNodeText(const Node: PVirtualNode;
  const ColumnIndex: TColumnIndex; const TextType: TVSTTextType;
  out CellText: String);
var
  CellTextEventArgs: TVSTGetCellTextEventArgs;
begin
  CellTextEventArgs.Node := Node;
  CellTextEventArgs.Column := ColumnIndex;

  DoGetText(CellTextEventArgs);

  case TextType of
    ttNormal: CellText := CellTextEventArgs.CellText;
    ttStatic: CellText := CellTextEventArgs.StaticText;
  end;
end;

procedure TPTLibBaseVirtualTree.ScaleNodesToFontSize;
var
  TextHeight: Integer;
begin
  TextHeight := Canvas.TextExtent('hj').Height;

  IterateSubtree(
    nil,
    procedure (Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
    begin
      Sender.NodeHeight[Node] := TextHeight + 2;
    end,
    nil
  );
end;

procedure TPTLibBaseVirtualTree.DoAfterCellPaint(Canvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  CellText: UnicodeString;
begin
  inherited;

  if toHTMLSupport in TreeOptions2 then
  begin
    GetNodeText(Node, Column, ttNormal, CellText);

    DrawHTML(CellRect, Canvas, CellText);
  end;
end;

procedure TPTLibBaseVirtualTree.DoAfterPaint(Canvas: TCanvas);
var
  Text: String;
  Rect: TRect;
begin
  inherited;

  if (RootNodeCount = 0) and
     (FHintText <> '') then
  begin
    Canvas.Font.Color := clGray;
    Canvas.Brush.Color := Color;

    Text := FHintText;
    Rect := ClientRect;
    Canvas.TextRect(Rect, Text, [tfCenter, tfVerticalCenter, tfSingleLine]);
  end;
end;

procedure TPTLibBaseVirtualTree.DoChecked(Node: PVirtualNode);
begin
  inherited;

  if (toAutoCheckSelectedNodes in FTreeOptions2) and (not FChecking) then
  try
    FChecking := True;

    CheckNodes(Node, Node.CheckState = csCheckedNormal);
  finally
    FChecking := False;
  end;
end;

procedure TPTLibBaseVirtualTree.CheckNode(Node: PVirtualNode; const Checked: Boolean);
begin
  // Check the node only if it is not already checked
  if (Checked) and (Node.CheckState = csUncheckedNormal) then
  begin
    if toSupressEventsOnAutoCheckNodes in TreeOptions2 then
      Node.CheckState := csCheckedNormal
    else
      CheckState[Node] := csCheckedNormal;
  end
  else
  if (not Checked) and (Node.CheckState = csCheckedNormal) then
  begin
    if toSupressEventsOnAutoCheckNodes in TreeOptions2 then
      Node.CheckState := csUnCheckedNormal
    else
      CheckState[Node] := csUnCheckedNormal;
  end;
end;

procedure TPTLibBaseVirtualTree.CheckNodes(const Node: PVirtualNode; const Checked: Boolean);
var
  WalkNode: PVirtualNode;
begin
  if (not Assigned(Node)) or (not Assigned(FocusedNode)) or (GetNodeLevel(Node) <> GetNodeLevel(FocusedNode)) then
    Exit;

  BeginUpdate;
  try
    // If the node we're checking is not selected we don't need to check any others
    if not Selected[Node] then
    begin
      CheckNode(Node, Checked);

      SelectNodeEx(Node);
    end
    else
    begin
      // Walk all selected nodes and check them
      WalkNode := GetFirstSelected;

      while Assigned(WalkNode) do
      begin
        CheckNode(WalkNode, Checked);

        WalkNode := GetNextSelected(WalkNode);
      end;
    end;
  finally
    EndUpdate;
  end;

  Invalidate;
end;

(*procedure TPTLibBaseVirtualTree.DoHeaderClick({$IF CompilerVersion >= 29.0}const{$endif} HitInfo: TVTHeaderHitInfo);
begin
  inherited;

  // Sort the list
  ShowHourGlass;
  try
    if Header.SortColumn = HitInfo.Column then
    Begin
      if Header.SortDirection = sdAscending then
        Header.SortDirection := sdDescending
      else
        Header.SortDirection := sdAscending;
    end
    else
    begin
      Header.SortColumn := HitInfo.Column;
      Header.SortDirection := sdAscending;
    end;

    Sort(nil, Header.SortColumn, Header.SortDirection);

    Invalidate;
  finally
    HideHourGlass;
  end;
end;*)

procedure TPTLibBaseVirtualTree.DoOnUserMessage(
  const UserMMessage: String);
begin
  if Assigned(FOnUserMessage) then
    FOnUserMessage(Self, UserMMessage);
end;

procedure TPTLibBaseVirtualTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
begin
  if not (toHTMLSupport in FTreeOptions2) then
    inherited;
end;

function TPTLibBaseVirtualTree.GetNodeFromIndex(const Index: Cardinal; const ParentNode: PVirtualNode): PVirtualNode;
begin
  Result := GetFirstChild(ParentNode);

  While Assigned(Result) do
  begin
    if Result.Index = Index then
      Break;

    Result := Result.NextSibling;
  end;
end;

function TPTLibBaseVirtualTree.IterateSelectedNodes(const NodeProc: TNodeProc): Boolean;
var
  Node: PVirtualNode;
begin
  Result := True;

  Node := GetFirstSelected;

  while Assigned(Node) do
  begin
    if not NodeProc(Node) then
      Result := False;

    Node := GetNextSelected(Node);
  end;
end;

function TPTLibBaseVirtualTree.GetOptions: TStringTreeOptions;
begin
  Result:= inherited TreeOptions as TStringTreeOptions;
end;

function TPTLibBaseVirtualTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result:= TStringTreeOptions;
end;

procedure TPTLibBaseVirtualTree.SelectNodeEx(const Node: PVirtualNode; ScrollNodeIntoView: Boolean);
begin
  ClearSelection;

  FocusedNode := Node;

  if Assigned(Node) then
  begin
    Selected[FocusedNode] := True;

    if ScrollNodeIntoView then
      ScrollIntoView(FocusedNode, False);
  end;
end;

procedure TPTLibBaseVirtualTree.SelectSiblings(Node: PVirtualNode);
begin
  if Assigned(Node) then
  begin
    while Node.Index > 0 do
      Node := Node.PrevSibling;

    while Assigned(Node) do
    begin
      Selected[Node] := True;

      Node := Node.NextSibling;
    end;
  end;
end;

procedure TPTLibBaseVirtualTree.SelectNodeByIndex(const Index: Cardinal; const ParentNode: PVirtualNode = nil);
begin
  SelectNodeEx(GetNodeFromIndex(Index, ParentNode));
end;

function TPTLibBaseVirtualTree.GetNextNodeAfterSelectedNodesRemoved: PVirtualNode;
begin
  Result := FocusedNode;

  While (Assigned(Result)) and (Selected[Result]) do
    Result := GetNextVisibleSibling(Result);

  if not Assigned(Result) then
  begin
    Result := FocusedNode;

    While (Assigned(Result)) and (Selected[Result]) and (Result <> RootNode) do
      Result := GetPreviousVisibleSibling(Result);
  end;
end;

procedure TPTLibBaseVirtualTree.DeleteSelectedNodesRefocus;
var
  Node: PVirtualNode;
begin
  Node := GetNextNodeAfterSelectedNodesRemoved;

  DeleteSelectedNodes;

  if toUseDataAsNodeIndex in TreeOptions2 then
    ReinitChildren(nil, True);

  if Assigned(Node) then
    SelectNodeEx(Node);
end;

procedure TPTLibBaseVirtualTree.HideSelectedNodesRefocus(DoHide: Boolean);
var
  Node, HideNode: PVirtualNode;
begin
  Node := GetNextNodeAfterSelectedNodesRemoved;

  HideNode := GetFirstSelected;

  While Assigned(HideNode) do
  begin
    IsVisible[HideNode] := not DoHide;

    HideNode := GetNextSelected(HideNode);
  end;

  if Assigned(Node) then
    SelectNodeEx(Node);
end;

procedure TPTLibBaseVirtualTree.PrintToBMP(Image: TBitmap; PrintHeader: Boolean);
var
  SaveTreeFont: TFont;                 // Remembers the tree's current font.
  SaveHeaderFont: TFont;               // Remembers the header's current font.
  ImgRect,                             // Describes the dimensions of Image.
  TreeRect: TRect;                     // The total VTree dimensions.
  P: TPoint;                           // Used by PaintTree.
  Options: TVTInternalPaintOptions;    // Used by PaintTree.
  SaveColor: TColor;                   // Remembers the VTree Color.
  LogFont: TLogFont;
begin
  SaveTreeFont := TFont.Create;

  ShowHourGlass;
  BeginUpdate;
  try
    // Grid lines are the only parts which are desirable when printing.
    Options := [poGridLines];

    // Remember the tree font.
    SaveTreeFont.Assign(Font);

    // Create a new font for printing which does not use clear type output (but is antialiased, if possible)
    // and which has the highest possible quality.
    GetObject(Font.Handle, SizeOf(TLogFont), @LogFont);
    LogFont.lfQuality := ANTIALIASED_QUALITY;
    Font.Handle := CreateFontIndirect(LogFont);

    // Create an image that will hold the complete VTree
    Image.PixelFormat := pf32Bit;

    TreeRect := GetTreeRect;

    Image.Width := TreeRect.Right - TreeRect.Left;
    P := Point(0, 0);
    if (hoVisible in Header.Options) and PrintHeader then
    begin
      Inc(TreeRect.Bottom, Header.Height);
      Inc(P.Y, Header.Height);
    end;
    Image.Height := TreeRect.Bottom - TreeRect.Top;

    ImgRect.Left := 0;
    ImgRect.Top := 0;
    ImgRect.Right := Image.Width;

    // Force the background to white color during the rendering.
    SaveColor := Color;
    Color := clWhite;

    // Print header if it is visible.
    if (hoVisible in Header.Options) and PrintHeader then
    begin
      SaveHeaderFont := TFont.Create;
      try
        SaveHeaderFont.Assign(Header.Font);
        // Create a new font for printing which does not use clear type output (but is antialiased, if possible)
        // and which has the highest possible quality.
        GetObject(Header.Font.Handle, SizeOf(TLogFont), @LogFont);
        LogFont.lfQuality := ANTIALIASED_QUALITY;
        Header.Font.Handle := CreateFontIndirect(LogFont);
        ImgRect.Bottom := Header.Height;
        Header.Columns.PaintHeader(Image.Canvas.Handle, ImgRect, 0);
        Header.Font := SaveHeaderFont;
      finally
        SaveHeaderFont.Free;
      end;
    end;

    // The image's height is already adjusted for the header if it is visible.
    ImgRect.Bottom := Image.Height;

    PaintTree(Image.Canvas, ImgRect, P, Options);
    Color := SaveColor;

    // Restore tree font.
    Font := SaveTreeFont;
  finally
    SaveTreeFont.Free;
    HideHourGlass;
    EndUpdate;
  end;
end;

procedure TPTLibBaseVirtualTree.CheckItems(const CheckType: TCheckTypes; const OnlySelected: Boolean);
var
  Node: PVirtualNode;
begin
  BeginUpdate;
  try
    if OnlySelected then
      Node := GetFirstSelected
    else
      Node := GetFirst;

    While Assigned(Node) do
    begin
      if CheckType = lcReverseCheck then
      begin
        if CheckState[Node] = csCheckedNormal then
          CheckState[Node] := csUnCheckedNormal
        else
          CheckState[Node] := csCheckedNormal;
      end else
      if CheckType = lcCheck then
        CheckState[Node] := csCheckedNormal else
      if CheckType = lcUnCheck then
        CheckState[Node] := csUnCheckedNormal;

      if OnlySelected then
        Node := GetNextSelected(Node)
      else
        Node := Node.NextSibling;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPTLibBaseVirtualTree.ToStrings(const Values: TStrings; const Options: TVTExportOptions;
      const FirstColumn: Integer; const LastColumn: Integer);

var
  FirstColumnIndex, LastColumnIndex: Integer;

  procedure AddText(var s: String; NewText: String);
  begin
    if s <> '' then
    begin
      s := concat(s, ',');
    end;

    if vteQuoteStrings in Options then
    begin
      s := concat(s, AnsiQuotedStr(NewText, '"'))
    end
    else
    begin
      s := concat(s, NewText);
    end;
  end;

  function GetNodeText(const Node: PVirtualNode): String;
  var
    i: Integer;
  begin
    Result := '';

    if Header.Columns.Count = 0 then
    begin
      Result := Text[Node, -1]
    end
    else
    begin
      for i := FirstColumnIndex to LastColumnIndex do
      begin
        if coVisible in Header.Columns[i].Options then
        begin
          AddText(Result, Text[Node, TColumnIndex(i)]);
        end;
      end;

      if toHTMLSupport in TreeOptions2 then
      begin
        Result := RemoveHTMLTags(Result);
      end;
    end;

    Result := ''.PadLeft(GetNodeLevel(Node) * 2, ' ') + Result;
  end;

  procedure IterateNodes(const Node: PVirtualNode);
  begin
    if Node <> nil then
    begin
      if ((IsVisible[Node]) or
          (not (vteVisibleOnly in Options))) and
         ((Selected[Node]) or
          (not (vteSelectedOnly in Options))) then
      begin
        Values.Add(GetNodeText(Node));
      end;

      if vteIncludeChildren in Options then
      begin
        IterateNodes(Node.FirstChild);
      end;

      IterateNodes(Node.NextSibling);
    end;
  end;

var
  i: Integer;
  Node: PVirtualNode;
  LineText: String;
begin
  if FirstColumn = -1 then
  begin
    FirstColumnIndex := 0;
  end
  else
  begin
    FirstColumnIndex := FirstColumn;
  end;

  if LastColumn = -1 then
  begin
    LastColumnIndex := pred(Header.Columns.Count);
  end
  else
  begin
    LastColumnIndex := LastColumn;
  end;

  if vteIncludeHeader in Options then
  begin
    LineText := '';

    for i := FirstColumnIndex to LastColumnIndex do
    begin
      if coVisible in Header.Columns[i].Options then
      begin
        AddText(LineText, Header.Columns[i].Text);
      end;
    end;

    Values.Add(LineText);
  end;

  if vteSelectedOnly in Options then
    Node := GetFirstSelected
  else
    Node := GetFirst;

  IterateNodes(Node);
end;

procedure TPTLibBaseVirtualTree.SaveToFile(const Filename: String; const Options: TVTExportOptions);
var
  SaveStrings: TStringList;
begin
  SaveStrings := TStringList.Create;
  try
    ToStrings(SaveStrings, Options);

    SaveStrings.SaveToFile(Filename);
  finally
    FreeAndNil(SaveStrings);
  end;
end;

procedure TPTLibBaseVirtualTree.SaveToFileWithDialog(const Options: TVTExportOptions);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.DefaultExt := '.txt';
    SaveDialog.Title := StrSave;
    SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
    SaveDialog.Filter := StrTextFilesTxt;

    if SaveDialog.Execute then
      SaveToFile(SaveDialog.Filename, Options);
  finally
    FreeAndNil(SaveDialog);
  end;
end;

function TPTLibBaseVirtualTree.GetInternalImages: TCustomImageList;
begin
  Result := inherited Images;
end;

function TPTLibBaseVirtualTree.GetLastSelected: PVirtualNode;
begin
  Result := GetLast;

  if (Assigned(Result)) and (not Selected[Result]) then
    Result := GetPreviousSelected(Result);
end;

function TPTLibBaseVirtualTree.GetSaveLoadWithParentForm: Boolean;
begin
  Result := FSaveLoadWithParentForm
end;

procedure TPTLibBaseVirtualTree.ExpandAllBelow(Node: PVirtualNode);
var
  NodeLev: Cardinal;
  RootNode: PVirtualNode;
  ExpandAll: Boolean;
begin
  ExpandAll := not Assigned(Node);

  if ExpandAll then
    Node := GetFirst;

  RootNode := Node;

  BeginUpdate;
  ShowHourGlass;
  try
    While Assigned(RootNode) do
    begin
      Expanded[Node] := True;
      NodeLev := GetNodeLevel(Node);
      Node := Node.FirstChild;

      While (Assigned(Node)) and ((GetNodeLevel(Node) > NodeLev) or (ExpandAll)) do
      begin
        Expanded[Node] := True;

        Node := GetNext(Node);
      end;

      if ExpandAll then
      begin
        RootNode := RootNode.NextSibling;
        Node := RootNode;
      end
      else
        Break;
    end;
  finally
    HideHourGlass;
    EndUpdate;
  end;
end;

procedure TPTLibBaseVirtualTree.CollapseAllBelow(const Node: PVirtualNode);
begin
  if Assigned(Node) then
  begin
    BeginUpdate;
    try
      DeleteChildren(Node);
    finally
      EndUpdate;
    end;
  end;
end;

function TPTLibBaseVirtualTree.ColumnByName(
  const ColumnName: String): TVirtualTreeColumn;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(Header.Columns.Count) do
    if ColumnName = Header.Columns[i].Text then
    begin
      Result := Header.Columns[i];

      Break;
    end;
end;

procedure TPTLibBaseVirtualTree.AddDefaultColumns(
  const ColumnNames: array of String; const ColumnWidths: array of Integer);
var
  i: Integer;
  Column: TVirtualTreeColumn;
begin
  Header.Columns.Clear;

  if High(ColumnNames) <> high(ColumnWidths) then
    raise Exception.Create('Number of column names must match the number of column widths.') // Do not localise
  else
  begin
    for i := low(ColumnNames) to high(ColumnNames) do
    begin
      Column := Header.Columns.Add;

      Column.Text := ColumnNames[i];

      if ColumnWidths[i] > 0 then
      begin
        Column.Width := ScaledPixels(ColumnWidths[i]);
      end
      else
      begin
        Header.AutoSizeIndex := Column.Index;
        Header.Options := Header.Options + [hoAutoResize];
      end;
    end;
  end;
end;

(*procedure TPTLibBaseVirtualTree.AutoFitColumns(Animated: Boolean; SmartAutoFitType: TSmartAutoFitType;
      RangeStartCol: Integer; RangeEndCol: Integer; IncludeHeader: Boolean);
var
  i, TextW: Integer;
begin
  //Canvas.Font.Assign(Header.Font);

  // Fit the column text
  Header.AutoFitColumns(Animated, SmartAutoFitType, RangeStartCol, RangeEndCol);

  (*// Fit the header text
  for i := 0 to pred(Header.Columns.Count) do
  begin
    TextW := Canvas.TextWidth(Header.Columns[i].Text) + FixDPIPixels(50);

    if (TextW > Header.Columns[i].Width) and
       (TextW < Header.Columns[i].MaxWidth) and
       (TextW > Header.Columns[i].MinWidth) then
    begin
      Header.Columns[i].Width := TextW;
    end;
  end;*)
//end;

procedure TPTLibBaseVirtualTree.LoadFromConfig(
  const Config: IConfiguration; RootKey: String);
begin
  LoadColumnSettings(Config, RootKey);
end;

procedure TPTLibBaseVirtualTree.MakeChildrenVisible(const Node: PVirtualNode;
  const IsVisible: Boolean);
var
  ChildNode: PVirtualNode;
begin
  ChildNode := GetNodeOrRootNode(Node).FirstChild;

  while Assigned(ChildNode) do
  begin
    Self.IsVisible[ChildNode] := IsVisible;

    ChildNode := ChildNode.NextSibling;
  end;
end;

function TPTLibBaseVirtualTree.GetNodeOrRootNode(const Node: PVirtualNode): PVirtualNode;
begin
  if Node = nil then
  begin
    Result := RootNode;
  end
  else
  begin
    Result := Node;
  end;
end;

(*procedure TPTLibBaseVirtualTree.LoadPngImagesFromResource(
  const Resourcename: String);
begin
  if not (csDesigning in ComponentState) then
  begin
    try
      TExtImageList(FInternalImages).AddFromPngResource(Resourcename);
    except
      ShowMessage(format('Unable to load resource "%s".', [ResourceName]));
    end;

    SetInternalImages(FInternalImages);
  end;
end;    *)

procedure TPTLibBaseVirtualTree.SetHintText(const Value: String);
begin
  FHintText := Value;

  Invalidate;
end;

procedure TPTLibBaseVirtualTree.SetInternalImages(const Value: TCustomImageList);
begin
  if (Assigned(Value)) or (csDesigning in ComponentState) then
    inherited Images := Value
  else
    Images := FInternalImages;
end;

procedure TPTLibBaseVirtualTree.SetOptions(
  const Value: TStringTreeOptions);
begin
  inherited TreeOptions.Assign(Value);
end;

procedure TPTLibBaseVirtualTree.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
  end;
end;

procedure TPTLibBaseVirtualTree.SaveToConfig(
  const Config: IConfiguration; RootKey: String);
begin
  SaveColumnSettings(Config, RootKey);
end;

// const IncludeHeader, SelectedOnly: Boolean;
procedure TPTLibBaseVirtualTree.CopyToClipboard(const Options: TVTExportOptions;
  const FirstColumn: Integer; const LastColumn: Integer);
var
  CopyStrings: TStringList;
begin
  CopyStrings := TStringList.Create;
  try
    ToStrings(CopyStrings, Options, FirstColumn, LastColumn);

    SaveTextToClipboard(CopyStrings.Text);
  finally
    FreeAndNil(CopyStrings);
  end;
end;

procedure TPTLibBaseVirtualTree.LoadColumnSettings(Config: IConfiguration; RootKey: String);

  function FindColumnByText(ColumnText: String): TVirtualTreeColumn;
  var
    i: Integer;
  begin
    Result := Nil;

    for i := 0 to pred(Header.Columns.Count) do
      if SameText(ColumnText, Header.Columns[i].Text) then
      begin
        Result := Header.Columns[i];
        Break;
      end;
  end;

var
  i: Integer;
  Column: TVirtualTreeColumn;
  BaseSection, Section: String;
begin
  BaseSection := Config.AppendSection(RootKey, 'Header');

  for i := 0 to pred(Config.ReadInteger(BaseSection, 'Count', 0)) do
  begin
    Section := Config.AppendSection(BaseSection, format('Column-%d', [i]));

    Column := FindColumnByText(Config.ReadString(Section, 'Text', ''));

    if Assigned(Column) then
    begin
      Column.Width := Config.ReadInteger(Section, 'Width', Column.Width);
      Column.Position := Config.ReadInteger(Section, 'Position', Column.Position);
    end;
  end;

  if Config.ValueNameExists(Section, 'SortDirection') then
    Header.SortDirection := TSortDirection(Config.ReadEnumeratedType(Section, 'SortDirection', TypeInfo(TSortDirection)));

  Header.SortColumn := Config.ReadInteger(Section, 'SortColumn', Header.SortColumn);
end;

procedure TPTLibBaseVirtualTree.Loaded;
begin
  inherited;

  Header.Options := Header.Options + [hoDblClickResize];
end;

procedure TPTLibBaseVirtualTree.Search(const SearchText: String;
  const ColumnHeader: String; const ExactMatch: Boolean; const SearchAll: Boolean);
var
  OnlyUpdateVisible: Boolean;
begin
  OnlyUpdateVisible :=
    (not SearchAll) and
     ((pos(FSearchText, AnsiLowerCase(SearchText)) = 1) and
      (pos(' or ', AnsiLowerCase(SearchText)) = 0) and
      (pos(' or ', FSearchText) = 0));

  FSearchText := AnsiLowercase(SearchText);
  FSearchColumnHeader := ColumnHeader;
  FSearchExactMatch := ExactMatch;

  FSearchStrings.CommaText := FSearchText;

  UpdateVisibleItems(OnlyUpdateVisible);
end;

procedure TPTLibBaseVirtualTree.SearchColumns(const SearchText: String; const IgnoreColumns: TArray<Integer>);
var
  i: Integer;
  SearchStrings: TStringList;
begin
  SearchStrings := TStringList.Create;
  try
    SearchStrings.CommaText := AnsiLowerCase(SearchText);

    for i := 0 to pred(Header.Columns.Count) do
    begin
      if not TArrayUtils<Integer>.Contains(i, IgnoreColumns) then
      begin
        if IsMatch(AnsiLowerCase(Header.Columns[i].Text), SearchStrings) then
        begin
          Header.Columns[i].Options := Header.Columns[i].Options + [coVisible];
        end
        else
        begin
          Header.Columns[i].Options := Header.Columns[i].Options - [coVisible];
        end;
      end;
    end;
  finally
    FreeAndNil(SearchStrings);
  end;
end;

procedure TPTLibBaseVirtualTree.UpdateVisibleItems(const OnlyUpdateVisible: Boolean;
  const ScrollFocusedIntoView: Boolean);
var
  Node: PVirtualNode;
begin
  BeginUpdate;
  try
    if OnlyUpdateVisible then
      Node := GetFirstVisible
    else
      Node := GetFirst;

    while Assigned(Node) do
    begin
      IsVisible[Node] := IsNodeVisible(Node);

      if OnlyUpdateVisible then
        Node := GetNextVisibleSibling(Node)
      else
        Node := Node.NextSibling;
    end;

    if (FocusedNode = nil) or
       (not IsVisible[FocusedNode]) then
    begin
      if GetPreviousVisible(FocusedNode) <> nil then
      begin
        SelectNodeEx(GetPreviousVisible(FocusedNode));
      end else
      if GetNextVisible(FocusedNode) <> nil then
      begin
        SelectNodeEx(GetNextVisible(FocusedNode));
      end
      else
      begin
        SelectNodeEx(nil);
      end;
    end;

    if (ScrollFocusedIntoView) and
       (Assigned(FocusedNode)) and
       (IsVisible[FocusedNode]) then
    begin
      ScrollIntoView(FocusedNode, True, False);
    end;

    Invalidate;
  finally
    EndUpdate;
  end;
end;

function TPTLibBaseVirtualTree.VisibleChildCount(
  const Node: PVirtualNode): Integer;
var
  StepNode: PVirtualNode;
begin
  Result := 0;

  if Node = nil then
  begin
    StepNode := GetFirstVisibleChild(RootNode);
  end
  else
  begin
    StepNode := GetFirstVisibleChild(Node);
  end;

  while StepNode <> nil do
  begin
    Inc(Result);

    StepNode := GetNextVisibleSibling(StepNode);
  end;
end;

procedure TPTLibBaseVirtualTree.InternalDisconnectNode(Node: PVirtualNode;
  KeepFocus: Boolean; Reindex: Boolean; ParentClearing: Boolean);
begin
  if FNodeIndices <> nil then
    FNodeIndices.Remove(Node);

  inherited;
end;

function TPTLibBaseVirtualTree.IsNodeVisible(const Node: PVirtualNode): Boolean;
var
  i: Integer;
  LowerLogText, CellText: String;
begin
  if Assigned(FOnIsNodeSearchHit) then
  begin
    Result := False;

    FOnIsNodeSearchHit(Self, Node, FSearchStrings, Result);
  end
  else
  begin
    Result := FSearchStrings.Count = 0;
    LowerLogText := '';

    if not Result then
    begin
      if Header.Columns.Count > 0 then
      begin
        for i := 0 to pred(Header.Columns.Count) do
        begin
          if (coVisible in Header.Columns[i].Options) and
             ((FSearchColumnHeader = '') or
              (SameText(FSearchColumnHeader, Header.Columns[i].Text))) then
          begin
            GetNodeText(Node, i, ttNormal, CellText);

            AddToken(LowerLogText, AnsiLowerCase(CellText), ' ');
          end;
        end;
      end
      else
      begin
        GetNodeText(Node, -1, ttNormal, CellText);

        LowerLogText := AnsiLowerCase(CellText);
      end;

      Result := IsSearchHit(FSearchStrings, LowerLogText);
    end;
  end;
end;

function TPTLibBaseVirtualTree.IsSearchHit(const SearchTerms: TStringList; const SearchText: String): Boolean;
var
  i: Integer;
  IsAnd: Boolean;
begin
  Result := False;
  IsAnd := True;

  for i := 0 to pred(FSearchStrings.Count) do
  begin
    if SameText(SearchTerms[i], 'or') then
    begin
      IsAnd := False;
    end
    else
    if SameText(SearchTerms[i], 'and') then
    begin
      IsAnd := True;
    end
    else
    begin
      if ((FSearchExactMatch) and (FSearchStrings[i] = SearchText)) or
         ((not FSearchExactMatch) and (pos(FSearchStrings[i], SearchText) <> 0)) then
      begin
        Result := True;
      end
      else
      begin
        if IsAnd then
        begin
          Result := False;
        end;
      end;
    end;
  end;
end;

procedure TPTLibBaseVirtualTree.SaveColumnSettings(Config: IConfiguration; RootKey: String);
var
  i: Integer;
  BaseSection, Section: String;
begin
  BaseSection := Config.AppendSection(RootKey, 'Header');

  // Remove any existing header settings
  Config.DeleteSection(BaseSection);

  Config.WriteInteger(BaseSection, 'Count', Header.Columns.Count);

  for i := 0 to pred(Header.Columns.Count) do
  begin
    Section := Config.AppendSection(BaseSection, format('Column-%d', [i]));

    Config.WriteString(Section, 'Text', Header.Columns[i].Text);
    Config.WriteInteger(Section, 'Width', Header.Columns[i].Width);
    Config.WriteInteger(Section, 'Position', Header.Columns[i].Position);
  end;

  Config.WriteEnumeratedType(Section, 'SortDirection', TypeInfo(TSortDirection), Integer(Header.SortDirection));
  Config.WriteInteger(Section, 'SortColumn', Header.SortColumn);
end;


end.
