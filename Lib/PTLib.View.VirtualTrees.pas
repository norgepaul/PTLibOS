unit PTLib.View.VirtualTrees;

interface

uses
  Windows, classes, sysutils, Vcl.imglist, Vcl.controls, Vcl.graphics,
  types, UITypes, Generics.Collections,

  VirtualTrees;

type
  TRealIndexNodeData = record
    NodeIndex: Cardinal;
  end;
  PRealIndexNodeData = ^TRealIndexNodeData;

  TVisibleObject = (
    voAll,
    voSubnetsWithIPs,
    voSubnetsWithoutIPs,
    voSubnetsWithComments,
    voSubnetsWithoutComments
  );
  TVisibleObjects = set of TVisibleObject;

  TTreeOption2 = (
    to2DoubleClickHeaderResize,
    toAutoCheckSelectedNodes,
    toHTMLSupport,
    toUseDataAsNodeIndex,
    toSortOnNodeValues,
    toDisableAutoScrollOnFocusChange
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
    ccForceParentsUnchecked,
    ccOnlyAllowCheckedItemsInOneAutonomousNetwork
  );
  TCheckControls = set of TCheckControl;

  TOnUserMessage = procedure(Sender: TObject; UserMessage: String) of object;
  TOnSearchComparison = procedure(Sender: TObject; Node: PVirtualNode; const SearchTerms: TStrings; var IsMatch: Boolean) of object;

  TNodeProc = reference to function(Node: PVirtualNode): Boolean;
  TColumnWidthList = class(TList<Integer>);

  TEasyIPBaseCustomVirtualTree = class(TCustomVirtualStringTree, IConfigurationProvider, IExportProvider, IDataExportProvider)
  private
    FOnUserMessage: TOnUserMessage;
    FOnSearchComparison: TOnSearchComparison;
    FOnGetExportDescription: TOnGetExportDescription;
    FOnGetExportType: TOnGetExportType;

    FChecking: Boolean;
    FSearchText: String;
    FSearchStrings: TStringList;
    FNodeUserIndices: TDictionary<PVirtualNode, Integer>;

    procedure SetImages(const Value: TCustomImageList);
    function GetImages: TCustomImageList;
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const Value: TStringTreeOptions);
    procedure SetStopWatch(const Value: TEasyIPStopWatch);
    procedure UpdateVisibleItems(OnlyUpdateVisible: Boolean);
    procedure SetColumnLengths(SelectedOnly, IncludeHeader: Boolean);
  protected
    FStatusLog: TStatusLog;
    FSaveLoadWithParentForm: Boolean;
    FTreeOptions2: TTreeOptions2;
    FInternalImages: TcxImageList;
    FIncrementalSearchColumn: TColumnIndex;
    FStopWatch: TEasyIPStopWatch;
    FExportNode: PVirtualNode;
    FExportColumnWidths: TColumnWidthList;
    FExportOptions: TExportOptions;
    FExportingData: Boolean;

    function GetTotalNodeCount: Cardinal;

    procedure SaveToConfig(const Config: IEasyIPConfiguration; RootKey: String); virtual;
    procedure LoadFromConfig(const Config: IEasyIPConfiguration; RootKey: String); virtual;
    procedure DoOnUserMessage(const UserMMessage: String); virtual;
    function DrawHTML(ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer; virtual;

    procedure LoadColumnSettings(Config: IEasyIPConfiguration; RootKey: String);
    procedure SaveColumnSettings(Config: IEasyIPConfiguration; RootKey: String);
    function GetSaveLoadWithParentForm: Boolean;
    procedure LoadPngImagesFromResource(const Resourcename: String); virtual;
    procedure AddDefaultColumns(const ColumnNames: Array of String; const ColumnWidths: Array of Integer);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetOptionsClass: TTreeOptionsClass; override;
    procedure DoHeaderClick(HitInfo: TVTHeaderHitInfo); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoFocusChange(Node: PVirtualNode; Column: TColumnIndex); override;
    procedure CheckNodes(const Node: PVirtualNode; const Checked: Boolean); virtual;
    procedure CheckNode(Node: PVirtualNode; const Checked: Boolean); virtual;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure DoInitNode(Parent, Node: PVirtualNode; var InitStates: TVirtualNodeInitStates); override;
    function DoCompare(Node1, Node2: PVirtualNode; Column: TColumnIndex): Integer; override;
    function DoIncrementalSearch(Node: PVirtualNode; const Text: UnicodeString): Integer; override;
    procedure Loaded; override;
    procedure StartStopwatch(const Description: String; PauseAfterStart: Boolean = FALSE); virtual;
    procedure StopStopwatch(UnPauseBeforeStop: Boolean = FALSE); virtual;
    procedure DoOnGetExportDescription(var ExportDescription: String);
    procedure DoOnGetExportType(var ExportType: TExportType);
    procedure DoFreeNode(Node: PVirtualNode); override;

    procedure DoOnSearchComparison(Node: PVirtualNode; const SearchTerms: TStrings; var IsMatch: Boolean); virtual;
    function GetControlDescription: String; virtual;

    // IDataExportProvider
    function Eof: Boolean; virtual;
    procedure First; virtual;
    procedure Next; virtual;
    function GetFieldValue(Column: TExportColumn): Variant; virtual;
    function RecordCount: Integer; virtual;
    procedure FillColumns(Columns: TExportColumns; Bands: ExportBands; RightToLeft: Boolean); virtual;
    procedure Configure(ExportOptions: TExportOptions); virtual;
    function GetExportDescription: String; virtual;
    function GetSupportExportOptions: TExportOptions; virtual;
    function GetSelectedCount: Integer; virtual;
    procedure StartExport; virtual;
    procedure EndExport; virtual;
    function HasColumnHeaders: Boolean; virtual;
    function GetExportType: TExportType; virtual;

    property StatusLog: TStatusLog read FStatusLog write FStatusLog;
    property StopWatch: TEasyIPStopWatch read FStopWatch write SetStopWatch;
    property SaveLoadWithParentForm: Boolean read GetSaveLoadWithParentForm write FSaveLoadWithParentForm;
    property TreeOptions2: TTreeOptions2 read FTreeOptions2 write FTreeOptions2;
    property IncrementalSearchColumn: TColumnIndex read FIncrementalSearchColumn write FIncrementalSearchColumn;
    property Images: TCustomImageList read GetImages write SetImages;
    property TreeOptions: TStringTreeOptions read GetOptions write SetOptions;
    property OnUserMessage: TOnUserMessage read FOnUserMessage write FOnUserMessage;
    property OnSearchComparison: TOnSearchComparison read FOnSearchComparison write FOnSearchComparison;
    property OnGetExportDescription: TOnGetExportDescription read FOnGetExportDescription write FOnGetExportDescription;
    property OnGetExportType: TOnGetExportType read FOnGetExportType write FOnGetExportType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CopySelectedToClipboard;

    procedure Search(const SearchText: String);
    procedure AutoFitColumns(Animated: Boolean = FALSE; SmartAutoFitType: TSmartAutoFitType = smaUseColumnOption; RangeStartCol: Integer = NoColumn; RangeEndCol: Integer = NoColumn; IncludeHeader: Boolean = TRUE);
    procedure ToStrings(const Values: TStrings; const IncludeHeader: Boolean = TRUE);
    procedure CheckItems(const CheckType: TCheckTypes; const OnlySelected: Boolean);
    function GetNodeFromIndex(const Index: Cardinal; const ParentNode: PVirtualNode = nil): PVirtualNode;
    procedure SelectNodeByIndex(const Index: Cardinal; const ParentNode: PVirtualNode = nil);
    procedure SelectNodeEx(const Node: PVirtualNode; ScrollNodeIntoView: Boolean = TRUE; Centre: Boolean = FALSE);
    procedure SelectSiblings(Node: PVirtualNode);
    function IterateSelectedNodes(const NodeProc: TNodeProc): Boolean;
    procedure DeleteSelectedNodesRefocus;
    procedure HideSelectedNodesRefocus(DoHide: Boolean = TRUE);
    procedure PrintToBMP(Image: TBitmap; PrintHeader: Boolean);
    function GetLastSelected: PVirtualNode;
    procedure CollapseAllBelow(const Node: PVirtualNode);
    procedure ExpandAllBelow(Node: PVirtualNode); virtual;
    function GetNextNodeAfterSelectedNodesRemoved: PVirtualNode;
    function ColumnByName(const ColumnName: String): TVirtualTreeColumn;
    function GetRealNodeIndex(Node: PVirtualNode): Cardinal;
    function GetRealFocusedNodeIndex: Cardinal;
    procedure Collapse(Node: PVirtualNode);
    procedure SetUserIndex(Node: PVirtualNode; UserIndex: Integer); virtual;
    function GetUserIndex(Node: PVirtualNode): Integer; virtual;
    function GetFocusedUserIndex: Integer; virtual;
    procedure AutoFitColumnsAndHeader(MaxRows: Integer = 0; IncludeStaticText: Boolean = FALSE);
    procedure SetUserRootNodeCount(NodeCount: Integer; ParentNode: PVirtualNode = nil);

    function AddChild(Parent: PVirtualNode; UserData: Pointer = nil; UserIndex: Integer = -1): PVirtualNode; reintroduce;
  end;

  TEasyIPBaseVirtualTree = class(TEasyIPBaseCustomVirtualTree)
  published
    property OnUserMessage;

    property StopWatch;
    property StatusLog;
    property SaveLoadWithParentForm;
    property TreeOptions2;

    property Align;
    property Alignment;
    property Anchors;
    property BorderStyle;
    property ButtonStyle;
    property BorderWidth;
    property CheckImageKind;
    property Color;
    property Colors;
    property Constraints;
    property DefaultNodeHeight;
    property DefaultText;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property Enabled;
    property Font;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property NodeDataSize;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RootNodeCount;
    property ScrollBarOptions;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property TreeOptions;
    property Visible;
    property WantTabs;
    property ClipboardFormats;
    property IncrementalSearchColumn;

    property OnAddToSelection;
    property OnRemoveFromSelection;
    property OnAfterCellPaint;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeCellPaint;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
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
    property OnGetText;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetNodeDataSize;
    property OnGetPopupMenu;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragging;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnStartDock;
    property OnStartDrag;
    property OnStructureChange;
    property OnUpdating;
    property OnDrawText;
  end;

  TEasyIPVirtualTree = class(TEasyIPBaseCustomVirtualTree)
  published
    property OnUserMessage;
    property OnSearchComparison;
    property OnGetExportDescription;
    property OnGetExportType;

    property StopWatch;
    property StatusLog;

    property SaveLoadWithParentForm;
    property TreeOptions2;

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
    property HintAnimation;
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

    property OnAddToSelection;
    property OnRemoveFromSelection;
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
    property OnDrawText;
  end;

implementation

uses
  Dialogs,
  EasyIP.Common.Strings,
  EasyIP.Common.UserInterface.ExtImgList,
  EasyIP.Common.UserInterface.HourGlass;

resourcestring
  StrUnableToLoadResource = 'Unable to load resource "%s": %s';
  StrTree = 'Tree';

{ TEasyIPBaseCustomVirtualTree }

constructor TEasyIPBaseCustomVirtualTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInternalImages := TcxImageList.Create(Self);
  FSearchStrings := TStringList.Create;
  FExportColumnWidths := TColumnWidthList.Create;
  FNodeUserIndices := TDictionary<PVirtualNode, Integer>.Create;

  Header.Options := Header.Options + [hoDblClickResize];
  TreeOptions.AutoOptions := TreeOptions.AutoOptions - [toDisableAutoscrollOnFocus];
  Header.Options := Header.Options - [hoShowSortGlyphs];
  TreeOptions2 := TreeOptions2 + [toAutoCheckSelectedNodes];
  //TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toUseExplorerTheme];
end;

destructor TEasyIPBaseCustomVirtualTree.Destroy;
begin
  FreeAndNilEx(FInternalImages);
  FreeAndNilEx(FSearchStrings);
  FreeAndNilEx(FExportColumnWidths);
  FreeAndNilEx(FNodeUserIndices);

  inherited;
end;

procedure TEasyIPBaseCustomVirtualTree.SetColumnLengths(SelectedOnly, IncludeHeader: Boolean);
var
  i, len: Integer;
  Node: PVirtualNode;
  CellText: String;
begin
  if SelectedOnly then
    Node := GetFirstSelected
  else
    Node := GetFirstVisible;

  FExportColumnWidths.Clear;

  for i := 0 to pred(Header.Columns.Count) do
    if IncludeHeader then
      FExportColumnWidths.Add(length(Header.Columns[i].Text))
    else
      FExportColumnWidths.Add(0);

  while Assigned(Node) do
  begin
    for i := 0 to pred(Header.Columns.Count) do
    begin
      DoGetText(Node, i, ttNormal, CellText);

      if toHTMLSupport in TreeOptions2 then
        CellText := StripHTMLTags(CellText);

      len := length(CellText);

      if toShowStaticText in TreeOptions.StringOptions then
      begin
        DoGetText(Node, i, ttStatic, CellText);

        if CellText <> '' then
          len := len + Length(CellText) + 3;
      end;

      if (i = Header.MainColumn) and
         (eoIncludeHierarchy in FExportOptions) then
      begin
        len := len + Integer(GetNodeLevel(Node));

        if toShowRoot in TreeOptions.PaintOptions then
          Inc(len);
      end;

      if len > FExportColumnWidths[i] then
        FExportColumnWidths[i] := len;
    end;

    if SelectedOnly then
      Node := GetNextSelected(Node)
    else
      Node := GetNextVisible(Node);
  end;
end;

function TEasyIPBaseCustomVirtualTree.DrawHTML(ARect: TRect; const ACanvas: TCanvas; const Text: String): Integer;
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
  x, y, idx, oldidx, CharWidth, MaxCharHeight: Integer;
  CurrChar: Char;
  Tag, TagValue: String;
  PreviousFontColour: TColor;
  PreviousFontFamily: String;
  PreviousFontSize: Integer;
  PreviousColour: TColor;
  IsTag: Boolean;
begin
  Result := 0;

  ACanvas.Font.Assign(Font);

  //ACanvas.Font.Size := Canvas.Font.Size;
  //ACanvas.Font.Name := Canvas.Font.Name;
  //ACanvas.Font.Color := Canvas.Font.Color;
  //ACanvas.Font.Style := Canvas.Font.Style;

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

    // Is this a tag?
    if CurrChar = '<' then
    begin
      IsTag := TRUE;

      Tag := '';

      oldidx := idx;

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

        IsTag := TagValue <> '';

        if IsTag then
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
          end;

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
          end
          else
            IsTag := FALSE;
        end;
      end;

      if IsTag then
      begin
        inc(idx);

        Continue;
      end
      else
        idx := oldidx;
    end;

    // Draw the character if it's not a ctrl char
    if CurrChar >= #32 then
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

procedure TEasyIPBaseCustomVirtualTree.DoAfterCellPaint(Canvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  CellText: UnicodeString;
  NewWidth: Integer;
begin
  inherited;

  if toHTMLSupport in TreeOptions2 then
  begin
    DoGetText(Node, Column, ttNormal, CellText);

    NewWidth := DrawHTML(CellRect, Canvas, CellText);

    if Column = -1 then
    begin
      if NewWidth > ClientWidth then
        ClientWidth := NewWidth
    end
    else
      if Header.Columns[Column].Width < NewWidth then
        Header.Columns[Column].Width := NewWidth;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.DoChecked(Node: PVirtualNode);
begin
  inherited;

  if (toAutoCheckSelectedNodes in FTreeOptions2) and
     (Selected[Node]) and
     (vsInitialized in Node.States) and
     (not FChecking) then
  try
    FChecking := TRUE;

    CheckNodes(Node, Node.CheckState = csCheckedNormal);
  finally
    FChecking := FALSE;
  end;
end;

function TEasyIPBaseCustomVirtualTree.DoCompare(Node1, Node2: PVirtualNode;
  Column: TColumnIndex): Integer;
var
  NodeText1, NodeText2: String;
begin
  Result := -3;

  if Assigned(OnCompareNodes) then
    OnCompareNodes(Self, Node1, Node2, Column, Result);

  if Result = -3 then
  begin
    if toSortOnNodeValues in TreeOptions2 then
    begin
      DoGetText(Node1, Column, ttNormal, NodeText1);
      DoGetText(Node2, Column, ttNormal, NodeText2);

      Result := CompareStr(NodeText1, NodeText2);
    end
    else
      Result := 0;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.DoFocusChange(Node: PVirtualNode;
  Column: TColumnIndex);
begin
  inherited;

  if not (toDisableAutoScrollOnFocusChange in TreeOptions2) then
    ScrollIntoView(Node, FALSE, FALSE) ;
end;

procedure TEasyIPBaseCustomVirtualTree.DoFreeNode(Node: PVirtualNode);
begin
  // Remove the user index if it exists
  SetUserIndex(Node, -1);

  inherited;
end;

procedure TEasyIPBaseCustomVirtualTree.SetUserRootNodeCount(NodeCount: Integer; ParentNode: PVirtualNode);
var
  Node: PVirtualNode;
  SelectedNodes: TList;
  i, Index, FocusedIndex: Integer;
  PreviousOffset: TPoint;
begin
  PreviousOffset := OffsetXY;
  SelectedNodes := TList.Create;
  BeginUpdate;
  try
    // Save the state
    Index := 0;
    FocusedIndex := -1;

    if ParentNode = nil then
      Node := GetFirst
    else
      Node := ParentNode.FirstChild;

    while Node <> nil do
    begin
      if Node = FocusedNode then
        FocusedIndex := Index;

      if Selected[Node] then
        SelectedNodes.Add(Pointer(Index));

      Inc(Index);

      Node := Node.NextSibling;
    end;

    // Clear the nodes
    if ParentNode = nil then
      Clear
    else
      DeleteChildren(ParentNode, TRUE);

    // Add the nodes
    for i := 0 to pred(NodeCount) do
      AddChild(ParentNode, nil, i);

    // Sort the tree before restoring the state
    SortTree(Header.SortColumn, Header.SortDirection);

    // Restore the state
    // Save the state
    Index := 0;

    ClearSelection;

    if ParentNode = nil then
      Node := GetFirst
    else
      Node := ParentNode.FirstChild;

    while Node <> nil do
    begin
      if FocusedIndex = Index then
        FocusedNode := Node;

      if SelectedNodes.IndexOf(Pointer(Index)) <> -1 then
        Selected[Node] := TRUE;

      Inc(Index);

      Node := Node.NextSibling;
    end;
  finally
    OffsetXY := PreviousOffset;
    EndUpdate;
    FreeAndNilEx(SelectedNodes);
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.CheckNode(Node: PVirtualNode; const Checked: Boolean);
begin
  // Check the node only if it is not already checked
  if (Checked) and (CheckState[Node] = csUncheckedNormal) then
    CheckState[Node] := csCheckedNormal else
  if (not Checked) and (CheckState[Node] = csCheckedNormal) then
    CheckState[Node] := csUnCheckedNormal
end;

procedure TEasyIPBaseCustomVirtualTree.CheckNodes(const Node: PVirtualNode; const Checked: Boolean);
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
        if IsVisible[WalkNode] then
          CheckNode(WalkNode, Checked);

        WalkNode := GetNextSelected(WalkNode);
      end;
    end;
  finally
    EndUpdate;
  end;

  Invalidate;
end;

function TEasyIPBaseCustomVirtualTree.DoIncrementalSearch(Node: PVirtualNode;
  const Text: UnicodeString): Integer;
var
  NodeText: String;
  SearchColumn: TColumnIndex;
begin
  Result := inherited DoIncrementalSearch(Node, Text);

  if (toSortOnNodeValues in TreeOptions2) and (Assigned(Node)) then
  begin
    if toExtendedFocus in TreeOptions.SelectionOptions then
      SearchColumn := FocusedColumn
    else
      SearchColumn := FIncrementalSearchColumn;

    DoGetText(Node, SearchColumn, ttNormal, NodeText);

    // Compare the node texts
    if SameText(copy(NodeText, 1, Length(Text)), Text) then
      Result := 0
    else
      Result := 1;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.DoHeaderClick(HitInfo: TVTHeaderHitInfo);
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
end;

procedure TEasyIPBaseCustomVirtualTree.DoInitNode(Parent, Node: PVirtualNode;
  var InitStates: TVirtualNodeInitStates);
var
  RealIndexNodeData: PRealIndexNodeData;
  IsMatch: Boolean;
begin
  if toUseDataAsNodeIndex in TreeOptions2 then
  begin
    if NodeDataSize = 0 then
      NodeDataSize := SizeOf(TRealIndexNodeData);

    RealIndexNodeData := GetNodeData(Node);

    if Assigned(RealIndexNodeData) then
      RealIndexNodeData.NodeIndex := Node.Index;
  end;

  inherited;

  // Is this node really visible?
  if IsVisible[Node] then
  begin
    IsMatch := TRUE;
    DoOnSearchComparison(Node, FSearchStrings, IsMatch);
    IsVisible[Node] := IsMatch;
  end;
end;

function TEasyIPBaseCustomVirtualTree.GetRealNodeIndex(Node: PVirtualNode): Cardinal;
var
  RealIndexNodeData: PRealIndexNodeData;
begin
  Assert(Node <> nil, 'Node cannot be nil');

  if not (vsInitialized in Node.States) then
    InitNode(Node);

  Result := Node.Index;

  if toUseDataAsNodeIndex in TreeOptions2 then
  begin
    RealIndexNodeData := GetNodeData(Node);

    if Assigned(RealIndexNodeData) then
      Result := RealIndexNodeData.NodeIndex;
  end;
end;

function TEasyIPBaseCustomVirtualTree.GetRealFocusedNodeIndex: Cardinal;
begin
  Result := GetRealNodeIndex(FocusedNode);
end;

procedure TEasyIPBaseCustomVirtualTree.DoOnUserMessage(
  const UserMMessage: String);
begin
  if Assigned(FOnUserMessage) then
    FOnUserMessage(Self, UserMMessage);
end;

procedure TEasyIPBaseCustomVirtualTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
begin
  if not (toHTMLSupport in FTreeOptions2) then
    inherited;
end;

function TEasyIPBaseCustomVirtualTree.GetNodeFromIndex(const Index: Cardinal; const ParentNode: PVirtualNode): PVirtualNode;
begin
  Result := GetFirstChild(ParentNode);

  While Assigned(Result) do
  begin
    if Result.Index = Index then
      Break;

    Result := Result.NextSibling;
  end;
end;

function TEasyIPBaseCustomVirtualTree.IterateSelectedNodes(const NodeProc: TNodeProc): Boolean;
var
  Node: PVirtualNode;
begin
  Result := TRUE;

  Node := GetFirstSelected;

  while Assigned(Node) do
  begin
    if not NodeProc(Node) then
      Result := FALSE;

    Node := GetNextSelected(Node);
  end;
end;

function TEasyIPBaseCustomVirtualTree.GetOptions: TStringTreeOptions;
begin
  Result:= inherited TreeOptions as TStringTreeOptions;
end;

function TEasyIPBaseCustomVirtualTree.GetOptionsClass: TTreeOptionsClass;
begin
  Result:= TStringTreeOptions;
end;

procedure TEasyIPBaseCustomVirtualTree.SelectNodeEx(const Node: PVirtualNode; ScrollNodeIntoView: Boolean; Centre: Boolean);
begin
  ClearSelection;

  if Assigned(Node) then
  begin
    FocusedNode := Node;
    Selected[Node] := TRUE;

    if ScrollNodeIntoView then
      ScrollIntoView(Node, Centre);
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.SelectSiblings(Node: PVirtualNode);
begin
  if Assigned(Node) then
  begin
    while Node.Index > 0 do
      Node := Node.PrevSibling;

    while Assigned(Node) do
    begin
      Selected[Node] := TRUE;

      Node := Node.NextSibling;
    end;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.Search(const SearchText: String);
var
  OnlyUpdateVisible: Boolean;
begin
  OnlyUpdateVisible := pos(FSearchText, LowerCase(SearchText)) = 1;

  FSearchText := Lowercase(SearchText);

  FSearchStrings.CommaText := FSearchText;

  UpdateVisibleItems(OnlyUpdateVisible);
end;

function TEasyIPBaseCustomVirtualTree.GetControlDescription: String;
begin
  Result := StrTree;

  DoOnGetExportDescription(Result);
end;

procedure TEasyIPBaseCustomVirtualTree.UpdateVisibleItems(OnlyUpdateVisible: Boolean);
var
  Node: PVirtualNode;
  IsMatch: Boolean;
begin
  BeginUpdate;
  try
    if OnlyUpdateVisible then
      Node := GetFirstVisible
    else
      Node := GetFirst;

    while Assigned(Node) do
    begin
      IsMatch := TRUE;

      if FSearchStrings.Count = 0 then
        IsMatch := TRUE
      else
        DoOnSearchComparison(Node, FSearchStrings, IsMatch);

      IsVisible[Node] := IsMatch;

      if OnlyUpdateVisible then
        Node := GetNextVisibleSibling(Node)
      else
        Node := Node.NextSibling;
    end;

    if (Assigned(FocusedNode)) and (IsVisible[FocusedNode]) then
      ScrollIntoView(FocusedNode, TRUE, FALSE);

    Invalidate;
  finally
    EndUpdate;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.DoOnGetExportDescription(
  var ExportDescription: String);
begin
  if Assigned(FOnGetExportDescription) then
    FOnGetExportDescription(Self, ExportDescription);
end;

procedure TEasyIPBaseCustomVirtualTree.DoOnGetExportType(
  var ExportType: TExportType);
begin
  if Assigned(FOnGetExportType) then
    FOnGetExportType(Self, ExportType);
end;

procedure TEasyIPBaseCustomVirtualTree.DoOnSearchComparison(Node: PVirtualNode; const SearchTerms: TStrings; var IsMatch: Boolean);
var
  i, Matches: Integer;
  SearchTerm, ColumnText, StaticText: String;
  n: Integer;
begin
  if SearchTerms.Count > 0 then
  begin
    if Assigned(FOnSearchComparison) then
      FOnSearchComparison(Self, Node, SearchTerms, IsMatch)
    else
    begin
      StaticText := '';
      
      for n := 0 to pred(Header.Columns.Count) do
      begin     
        if coVisible in Header.Columns[n].Options then
        begin
          DoGetText(Node, n, ttNormal, ColumnText); 
          ColumnText := LowerCase(ColumnText);
      
          if toShowStaticText in TreeOptions.StringOptions then
          begin
            DoGetText(Node, n, ttStatic, StaticText); 

            StaticText := LowerCase(StaticText);
          end;
      
          Matches := 0;
      
          for i := 0 to pred(SearchTerms.Count) do
          begin
            SearchTerm := LowerCase(SearchTerms[i]);

            if (pos(SearchTerm, ColumnText) > 0) or
               (pos(SearchTerm, StaticText) > 0) then
            begin
              Inc(Matches);
            end;
          end;

          IsMatch := Matches = SearchTerms.Count;      

          if IsMatch then
            Break;
        end;
      end;
    end;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.SelectNodeByIndex(const Index: Cardinal; const ParentNode: PVirtualNode = nil);
begin
  SelectNodeEx(GetNodeFromIndex(Index, ParentNode));
end;

function TEasyIPBaseCustomVirtualTree.GetNextNodeAfterSelectedNodesRemoved: PVirtualNode;
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

procedure TEasyIPBaseCustomVirtualTree.DeleteSelectedNodesRefocus;
var
  Node: PVirtualNode;
begin
  Node := GetNextNodeAfterSelectedNodesRemoved;

  DeleteSelectedNodes;

  if toUseDataAsNodeIndex in TreeOptions2 then
    ReinitChildren(nil, TRUE);

  if Assigned(Node) then
    SelectNodeEx(Node);
end;

function TEasyIPBaseCustomVirtualTree.HasColumnHeaders: Boolean;
begin
  Result := (hoVisible in Header.Options) and (Header.Columns.Count > 0);
end;

procedure TEasyIPBaseCustomVirtualTree.HideSelectedNodesRefocus(DoHide: Boolean);
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

procedure TEasyIPBaseCustomVirtualTree.PrintToBMP(Image: TBitmap; PrintHeader: Boolean);
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

function TEasyIPBaseCustomVirtualTree.RecordCount: Integer;
begin
  if eoSelectedOnly in FExportOptions then
    Result := GetSelectedCount
  else
    Result := VisibleCount;
end;

function TEasyIPBaseCustomVirtualTree.GetTotalNodeCount: Cardinal;
begin
  Result := RootNodeCount;
end;

procedure TEasyIPBaseCustomVirtualTree.CheckItems(const CheckType: TCheckTypes; const OnlySelected: Boolean);
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

procedure TEasyIPBaseCustomVirtualTree.ToStrings(const Values: TStrings; const IncludeHeader: Boolean);

  procedure AddText(var s: String; NewText: String);
  begin
    if s <> '' then
      s := concat(s, ',');

    s := concat(s, AnsiQuotedStr(NewText, '"'))
  end;

var
  i: Integer;
  Node: PVirtualNode;
  LineText: String;
begin
  if IncludeHeader then
  begin
    LineText := '';

    for i := 0 to pred(Header.Columns.Count) do
      AddText(LineText, Header.Columns[i].Text);

    Values.Add(LineText);
  end;

  Node := GetFirst;
  While Assigned(Node) do
  begin
    LineText := '';

    for i := 0 to pred(Header.Columns.Count) do
      AddText(LineText, Text[Node, TColumnIndex(i)]);

    Values.Add(LineText);

    Node := Node.NextSibling;
  end;
end;

function TEasyIPBaseCustomVirtualTree.GetExportDescription: String;
begin
  Result := '';

  DoOnGetExportDescription(Result);
end;

function TEasyIPBaseCustomVirtualTree.GetExportType: TExportType;
begin
  Result := etVirtualGrid;

  DoOnGetExportType(Result);
end;

function TEasyIPBaseCustomVirtualTree.GetFieldValue(
  Column: TExportColumn): Variant;
var
  CellText, IndentText: String;
  RealIndex: Integer;
  i: Integer;
  ButtonText: Char;
  StaticText: String;
begin
  if Header.Columns.Count = 0 then
    RealIndex := -1
  else
    RealIndex := Column.Index;

  DoGetText(FExportNode, RealIndex, ttNormal, CellText);

  if toShowStaticText in TreeOptions.StringOptions then
  begin
    DoGetText(FExportNode, RealIndex, ttStatic, StaticText);

    if StaticText <> '' then
      CellText := CellText + ' (' + StaticText + ')';
  end;

  if toHTMLSupport in TreeOptions2 then
    CellText := StripHTMLTags(CellText);

  if (eoIncludeHierarchy in FExportOptions) and
     ((Column.Index = Header.MainColumn) or
     (Header.MainColumn = -1)) then
  begin
    IndentText := '';

    for i := 0 to GetNodeLevel(FExportNode) do
      IndentText := IndentText + ' ';

    if length(IndentText) > 0 then
    begin
      // Remove the root?
      if not (toShowRoot in TreeOptions.PaintOptions) then
        Delete(IndentText, 1, 1);

      if length(IndentText) > 0 then
      begin
        if not HasChildren[FExportNode] then
          ButtonText := ' '
        else
        if Expanded[FExportNode] then
          ButtonText := '-'
        else
          ButtonText := '+';

        if toShowButtons in TreeOptions.PaintOptions then
          IndentText[length(IndentText)] := ButtonText;
      end;
    end;

    CellText := IndentText + CellText;
  end;

  if (Header.Columns.Count > 0) and
     (eoFixedWidthColumns in FExportOptions) and
     (Column.Index < FExportColumnWidths.Count) then
    CellText := AddTrailingSpaces(CellText, Integer(FExportColumnWidths[Column.Index]) + 1);

  Result := CellText;
end;

function TEasyIPBaseCustomVirtualTree.GetImages: TCustomImageList;
begin
  Result := inherited Images;
end;

function TEasyIPBaseCustomVirtualTree.GetLastSelected: PVirtualNode;
begin
  Result := GetLast;

  if (Assigned(Result)) and (not Selected[Result]) then
    Result := GetPreviousSelected(Result);
end;

function TEasyIPBaseCustomVirtualTree.GetSaveLoadWithParentForm: Boolean;
begin
  Result := FSaveLoadWithParentForm
end;

function TEasyIPBaseCustomVirtualTree.GetSelectedCount: Integer;
begin
  Result := SelectedCount;
end;

function TEasyIPBaseCustomVirtualTree.GetSupportExportOptions: TExportOptions;
begin
  Result := [eoFixedWidthColumns,
             eoCalculateColumnWidthsInChars,
             eoAddColumnHeadings,
             eoSelectedOnly];

  if [toShowRoot, toShowButtons] * TreeOptions.PaintOptions <> [] then
    Result := Result + [eoIncludeHierarchy];
end;

procedure TEasyIPBaseCustomVirtualTree.EndExport;
begin
  FExportingData := FALSE;
end;

function TEasyIPBaseCustomVirtualTree.Eof: Boolean;
begin
  Result := FExportNode = nil;
end;

procedure TEasyIPBaseCustomVirtualTree.ExpandAllBelow(Node: PVirtualNode);
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
      Expanded[Node] := TRUE;
      NodeLev := GetNodeLevel(Node);
      Node := Node.FirstChild;

      While (Assigned(Node)) and ((GetNodeLevel(Node) > NodeLev) or (ExpandAll)) do
      begin
        Expanded[Node] := TRUE;

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

procedure TEasyIPBaseCustomVirtualTree.Collapse(Node: PVirtualNode);
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
      Expanded[Node] := FALSE;
      NodeLev := GetNodeLevel(Node);
      Node := Node.FirstChild;

      While (Assigned(Node)) and ((GetNodeLevel(Node) > NodeLev) or (ExpandAll)) do
      begin
        Expanded[Node] := FALSE;

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

procedure TEasyIPBaseCustomVirtualTree.FillColumns(Columns: TExportColumns;
  Bands: ExportBands; RightToLeft: Boolean);

  procedure AddExportColumn(ColumnIndex: Integer; IsVisible: Boolean);
  var
    ExportColumn: TExportColumn;
  begin
    ExportColumn := Columns.Add;

    if ColumnIndex > -1 then
    begin
      ExportColumn.Title.Caption := Header.Columns[ColumnIndex].Text;
      ExportColumn.Width := Header.Columns[ColumnIndex].Width;
      ExportColumn.Index := ColumnIndex;
      ExportColumn.FieldName := ExportColumn.Title.Caption;
      ExportColumn.Visible := IsVisible;

      if (eoFixedWidthColumns in FExportOptions) and
         (eoAddColumnHeadings in FExportOptions) and
         (ColumnIndex < FExportColumnWidths.Count) then
        ExportColumn.Title.Caption := AddTrailingSpaces(ExportColumn.Title.Caption, Integer(FExportColumnWidths[ColumnIndex]) + 1);

      ExportColumn.DisplayName := ExportColumn.Title.Caption;

      if ColumnIndex < FExportColumnWidths.Count then
        ExportColumn.Width := Integer(FExportColumnWidths[Integer(ColumnIndex)]);
    end
    else
    begin
      ExportColumn.Index := 0;
      ExportColumn.FieldName := 'Value';
    end;
  end;

var
  i: Integer;
begin
  if Header.Columns.Count = 0 then
    AddExportColumn(-1, TRUE)
  else
  for i := 0 to pred(Header.Columns.Count) do
    AddExportColumn(i, coVisible in Header.Columns[i].Options);
end;

procedure TEasyIPBaseCustomVirtualTree.First;
begin
  if eoSelectedOnly in FExportOptions then
    FExportNode := GetFirstSelected
  else
    FExportNode := GetFirstVisible;
end;

procedure TEasyIPBaseCustomVirtualTree.CollapseAllBelow(const Node: PVirtualNode);
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

function TEasyIPBaseCustomVirtualTree.ColumnByName(
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

procedure TEasyIPBaseCustomVirtualTree.Configure(ExportOptions: TExportOptions);
begin
  FExportOptions := ExportOptions;

  if [eoFixedWidthColumns, eoCalculateColumnWidthsInChars] * ExportOptions <> [] then
    SetColumnLengths(eoSelectedOnly in ExportOptions, eoAddColumnHeadings in ExportOptions)
  else
    FExportColumnWidths.Clear;
end;

procedure TEasyIPBaseCustomVirtualTree.CopySelectedToClipboard;
var
  ExportEngine: TBaseEasyIPDataExportEngine;
  ClipExport: TSMExportToClipboard;
begin
  ExportEngine := TBaseEasyIPDataExportEngine.Create(nil);
  try
    ExportEngine.ExportComponent := Self;
    ExportEngine.ExportOptions := [eoCalculateColumnWidthsInChars,
                                   eoFixedWidthColumns];

    if hoVisible in Header.Options then
      ExportEngine.ExportOptions := ExportEngine.ExportOptions + [eoAddColumnHeadings];

    if toShowRoot in TreeOptions.PaintOptions then
      ExportEngine.ExportOptions := ExportEngine.ExportOptions + [eoIncludeHierarchy];

    if SelectedCount > 1 then
      ExportEngine.ExportOptions := ExportEngine.ExportOptions + [eoSelectedOnly];

    ClipExport := TSMExportToClipboard.Create(nil);
    try
      ClipExport.DataEngine := ExportEngine;
      ClipExport.ColumnSource := csDataEngine;
      ClipExport.AnimatedStatus := FALSE;
      ClipExport.Options := ClipExport.Options - [soWaitCursor, soShowMessage];
      ClipExport.AddTitle := TRUE;

      ShowHourGlass;
      BeginUpdate;
      StartExport;
      try
        ClipExport.Execute;
      finally
        EndExport;
        EndUpdate;
        HideHourGlass;
      end;
    finally
      FreeAndNil(ClipExport);
    end;
  finally
    FreeAndNil(ExportEngine);
  end;
end;

function TEasyIPBaseCustomVirtualTree.AddChild(Parent: PVirtualNode;
  UserData: Pointer; UserIndex: Integer): PVirtualNode;
begin
  Result := inherited AddChild(Parent, UserData);

  SetUserIndex(Result, UserIndex);
end;

procedure TEasyIPBaseCustomVirtualTree.SetUserIndex(Node: PVirtualNode; UserIndex: Integer);
begin
  if (Node <> nil) and (FNodeUserIndices <> nil) then
  begin
    if UserIndex = -1 then
    begin
      if FNodeUserIndices.ContainsKey(Node) then
        FNodeUserIndices.Remove(Node);
    end
    else
      FNodeUserIndices.AddOrSetValue(Node, UserIndex);
  end;
end;

function TEasyIPBaseCustomVirtualTree.GetUserIndex(Node: PVirtualNode): Integer;
begin
  if (Node <> nil) and
     (FNodeUserIndices <> nil) and
     (FNodeUserIndices.ContainsKey(Node)) then
    Result := FNodeUserIndices[Node]
  else
    Result := -1;
end;

function TEasyIPBaseCustomVirtualTree.GetFocusedUserIndex: Integer;
begin
  Result := GetUserIndex(FocusedNode);
end;

procedure TEasyIPBaseCustomVirtualTree.AddDefaultColumns(
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
        Column.Width := ColumnWidths[i]
      else
      begin
        Header.AutoSizeIndex := Column.Index;
        Header.Options := Header.Options + [hoAutoResize];
      end;
    end;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.AutoFitColumns(Animated: Boolean; SmartAutoFitType: TSmartAutoFitType;
      RangeStartCol: Integer; RangeEndCol: Integer; IncludeHeader: Boolean);
var
  i, TextW: Integer;
begin
  // Fit the column text
  Header.AutoFitColumns(Animated, SmartAutoFitType, RangeStartCol,RangeEndCol);

  // Fit the header text
  for i := 0 to pred(Header.Columns.Count) do
  begin
    TextW := Canvas.TextWidth(Header.Columns[i].Text) + 15;

    if (TextW > Header.Columns[i].Width) and (TextW < Header.Columns[i].MaxWidth) and (TextW > Header.Columns[i].MinWidth) then
      Header.Columns[i].Width := TextW;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.AutoFitColumnsAndHeader(MaxRows: Integer; IncludeStaticText: Boolean);
var
  i, TextW, ColWidth: Integer;
  Node: PVirtualNode;
  RowCount: Integer;
  DisplayText, NodeText: String;
begin
  BeginUpdate;
  try
    // Fit the header text
    for i := 0 to pred(Header.Columns.Count) do
    begin
      ColWidth := 30;

      TextW := Canvas.TextWidth(Header.Columns[i].Text) + 15;

      if (TextW < Header.Columns[i].MaxWidth) and
         (TextW > Header.Columns[i].MinWidth) then
        ColWidth := TextW;

      RowCount := 0;

      Node := GetFirst;

      while Node <> nil do
      begin
        DoGetText(Node, i, ttNormal, DisplayText);
        //DisplayText :=  GetCellText(Node, i, ttNormal, FShowOidLabel, FFormatValues, FALSE);

        if IncludeStaticText then
        begin
          DoGetText(Node, i, ttStatic, NodeText);

          DisplayText := DisplayText + NodeText + '  ';
        end;

        TextW := Canvas.TextWidth(DisplayText) + 15;

        if (TextW > ColWidth) and
           (TextW < Header.Columns[i].MaxWidth) then
          ColWidth := TextW;

        Inc(RowCount);

        if (MaxRows <> 0) and (RowCount >= MaxRows) then
          Break;

        Node := GetNext(Node);
      end;

      Header.Columns[i].Width := ColWidth;
    end;
  finally
    EndUpdate;
  end;
end;


procedure TEasyIPBaseCustomVirtualTree.LoadFromConfig(
  const Config: IEasyIPConfiguration; RootKey: String);
begin
  LoadColumnSettings(Config, RootKey);
end;

procedure TEasyIPBaseCustomVirtualTree.LoadPngImagesFromResource(
  const Resourcename: String);
begin
  try
    TExtImageList(FInternalImages).AddFromPngResource(Resourcename);
  except
    on e: Exception do
    begin
      if not (csDesigning in ComponentState) then
        ShowMessage(format(StrUnableToLoadResource, [ResourceName, e.Message]));
    end;
  end;

  SetImages(FInternalImages);
end;

procedure TEasyIPBaseCustomVirtualTree.SetImages(const Value: TCustomImageList);
begin
  if (Assigned(Value)) or (csDesigning in ComponentState) then
    inherited Images := Value
  else
    Images := FInternalImages;
end;

procedure TEasyIPBaseCustomVirtualTree.SetOptions(
  const Value: TStringTreeOptions);
begin
  inherited TreeOptions.Assign(Value);
end;

procedure TEasyIPBaseCustomVirtualTree.SetStopWatch(
  const Value: TEasyIPStopWatch);
begin
  FStopWatch := Value;
end;

procedure TEasyIPBaseCustomVirtualTree.Next;
begin
  if FEXportNode = nil then
    First
  else
  begin
    if eoSelectedOnly in FExportOptions then
      FExportNode := GetNextSelected(FExportNode)
    else
      FExportNode := GetNextVisible(FExportNode);
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if AComponent = FStatusLog then
      FStatusLog := nil else

    if AComponent = StatusLog then
      FStopWatch := nil;
  end;
end;

procedure TEasyIPBaseCustomVirtualTree.StartExport;
begin
  FExportingData := TRUE;
end;

procedure TEasyIPBaseCustomVirtualTree.StartStopwatch(const Description: String; PauseAfterStart: Boolean);
begin
  if Assigned(FStopWatch) then
    FStopWatch.AddStopWatch(Description, PauseAfterStart);
end;

procedure TEasyIPBaseCustomVirtualTree.StopStopwatch(UnPauseBeforeStop: Boolean);
begin
  if Assigned(FStopWatch) then
    FStopWatch.RemoveStopWatch(UnPauseBeforeStop);
end;

procedure TEasyIPBaseCustomVirtualTree.SaveToConfig(
  const Config: IEasyIPConfiguration; RootKey: String);
begin
  SaveColumnSettings(Config, RootKey);
end;

procedure TEasyIPBaseCustomVirtualTree.LoadColumnSettings(Config: IEasyIPConfiguration; RootKey: String);

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

  Header.SortColumn := Config.ReadInteger(BaseSection, 'SortColumnIndex', Header.SortColumn);
  Header.SortDirection := TSortDirection(Config.ReadEnumeratedType(BaseSection, 'SortColumnDirection', TypeInfo(TSortDirection)));
end;

procedure TEasyIPBaseCustomVirtualTree.Loaded;
begin
  inherited;

  Header.Options := Header.Options + [hoDblClickResize];
end;

procedure TEasyIPBaseCustomVirtualTree.SaveColumnSettings(Config: IEasyIPConfiguration; RootKey: String);
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

  Config.WriteInteger(BaseSection, 'SortColumnIndex', Header.SortColumn);
  Config.WriteEnumeratedType(BaseSection, 'SortColumnDirection', TypeInfo(TSortDirection), Integer(Header.SortDirection));
end;

end.
