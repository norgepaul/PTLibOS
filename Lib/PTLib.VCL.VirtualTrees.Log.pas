unit PTLib.VCL.VirtualTrees.Log;

interface

uses
  Windows,

  WinAPI.Messages,

  System.Classes, System.SysUtils, System.Types, System.UITypes,
  System.SyncObjs, System.Generics.Collections,

  Vcl.Graphics, Vcl.ImgList, Vcl.Menus, Vcl.ExtCtrls, Vcl.Dialogs,

  VirtualTrees, VirtualTrees.Header, VirtualTrees.Types,

  PTLib.Common.Timers,
  PTLib.Common.Dates,
  PTLib.Common.Strings,
  PTLib.Common.Utils,
  PTLib.Common.Log,
  PTLib.Common.Types,
  PTLib.Common.Interfaces,

  PTLib.VCL.Clipboard,
  PTLib.VCL.HourGlass,
  PTLib.VCL.VirtualTrees;

type
  TLClogPopupMenuItem = (
    pmiSaveLog,
    pmiSaveSelected,
    pmiCopyToClipboard,
    pmiCopySelectedToClipBoard,
    pmiShowTimestamp,
    pmiClear,
    pmiShowDate,
    pmiDividers,
    pmiAutoScroll
  );
  TLClogPopupMenuItems = set of TLClogPopupMenuItem;

  TLogNodeData = record
  strict private
    FLogEntry: ILogEntry;
  private
    procedure SetLogEntry(const Value: ILogEntry);
  public
    property LogEntry: ILogEntry read FLogEntry write SetLogEntry;
  end;
  PLogNodeData = ^TLogNodeData;

  TOnLog = procedure(Sender: TObject; var LogText: String; var CancelEntry: Boolean; LogLevel: TLogSeverity) of object;
  TOnPopupMenuItemClick = procedure(Sender: TObject; MenuItem: TMenuItem) of object;
  TOnAfterLog = procedure(Sender: TObject; Node: PVirtualNode) of object;

  TVirtualLogPopupmenu = class(TPopupMenu)
  private
    FOwner: TComponent;
    FOnPopupMenuItemClick: TOnPopupMenuItemClick;

    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    property OnPopupMenuItemClick: TOnPopupMenuItemClick read FOnPopupMenuItemClick write FOnPopupMenuItemClick;
  end;

  TPTLibVirtualLogTree = class(TPTLibVirtualTree)
  private
    FOnLog: TOnLog;
    FOnAfterLog: TOnAfterLog;
    FOnClearLog: TNotifyEvent;
    FLog: TPTLibLog;

    FAutoScroll: Boolean;
    FRemoveControlCharacters: Boolean;
    FLogLevels: TLogSeverities;
    FAutoLogLevelColours: Boolean;
    FShowDateColumn: Boolean;
    FShowImages: Boolean;
    FMaximumLines: Integer;
    FPopupMenuItems: TLClogPopupMenuItems;
    FTotalLogLines: Int64;
    FActivatedTimetampUTC: TDateTime;
    FPaused: Boolean;
    FShowTimeOnly: Boolean;
    FThreadSafe: Boolean;
    FScrollLog: Boolean;

    procedure SeTLogSeverities(const Value: TLogSeverities);
    procedure OnPopupMenuItemClick(Sender: TObject; MenuItem: TMenuItem);
    procedure SetShowDateColumn(const Value: Boolean);
    procedure SetShowImages(const Value: Boolean);
    procedure InternalLog(LogItem: ILogEntry);
    procedure SetLClogPopupMenuItems(const Value: TLClogPopupMenuItems);
    procedure SetShowTimeOnly(const Value: Boolean);
    procedure SetDefaultProperties;
    procedure OnLogEx(Sender: TObject; LogEntry: ILogEntry; const Index, Count: Integer);
  protected
    procedure DoOnLog(var LogText: String; var CancelEntry: Boolean; LogLevel: TLogSeverity); virtual;
    procedure DoOnAfterLog(Node: PVirtualNode); virtual;

    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoFreeNode(Node: PVirtualNode); override;
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList; override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure Loaded; override;
    procedure DoOnClearLog; virtual;
    function IsNodeVisible(const Node: PVirtualNode): Boolean; override;
    procedure DoLog(LogEntry: ILogEntry; const Index, Count: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    function GetCellText(const Node: PVirtualNode; const Column: TColumnIndex): String;
    procedure Log(LogEntry: ILogEntry); overload;
    procedure Log(Value: String; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(Value: String; const Args: array of const; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure GetLogInfo(Node: PVirtualNode; var Text: String; var Date: TDateTime; var LogLevel: TLogSeverity);
    procedure Clear; override;
    procedure ToStrings(const Values: TStrings; const Options: TVTExportOptions; const FirstColumn: Integer = -1; const LastColumn: Integer = -1); override;
    procedure AddLogEntries(const LogEntries: IList<ILogEntry>);
    procedure ResizeTimestampColumn;
//    procedure LoadLogFromStrings(const Values: TStrings);

    property TotalLogLines: Int64 read FTotalLogLines;
    property ActivatedTimetampUTC: TDateTime read FActivatedTimetampUTC write FActivatedTimetampUTC;
    property Paused: Boolean read FPaused write FPaused;
    property LogLevels: TLogSeverities read FLogLevels write SeTLogSeverities;
    property Logger: TPTLibLog read FLog;
  published
    property OnLog: TOnLog read FOnLog write FOnLog;
    property OnAfterLog: TOnAfterLog read FOnAfterLog write FOnAfterLog;
    property OnClearLog: TNotifyEvent read FOnClearLog write FOnClearLog;

    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property RemoveControlCharacters: Boolean read FRemoveControlCharacters write FRemoveControlCharacters;
    property AutoLogLevelColours: Boolean read FAutoLogLevelColours write FAutoLogLevelColours;
    property ShowDateColumn: Boolean read FShowDateColumn write SetShowDateColumn;
    property ShowImages: Boolean read FShowImages write SetShowImages;
    property MaximumLines: Integer read FMaximumLines write FMaximumLines;
    property PopupMenuItems: TLClogPopupMenuItems read FPopupMenuItems write SetLClogPopupMenuItems;
    property ShowTimeOnly: Boolean read FShowTimeOnly write SetShowTimeOnly;
    property ThreadSafe: Boolean read FThreadSafe write FThreadSafe;
  end;

implementation

resourcestring
  StrSaveLog = '&Save All';
  StrCopyToClipboard = '&Copy All';
  StrDate = 'Date';
  StrLog = 'Log';
  StrSaveSelected = 'S&ave Selected';
  StrCopySelected = 'C&opy Selected';
  StrClear = 'C&lear';
  StrShowTimestamp = 'Show/Hide Timestamp';
  StrAutoScroll = 'Auto Scroll';

constructor TPTLibVirtualLogTree.Create(AOwner: TComponent);
begin
  inherited;

  FLog := TPTLibLog.Create(Self);
  FLog.OnLogEx := OnLogEx;

  FThreadSafe := True;
  FAutoScroll := True;
  FRemoveControlCharacters := True;
  FShowDateColumn := True;
  FShowImages := True;
  FLogLevels := [
    LogSeverityError,
    LogSeverityWarning,
    LogSeverityInfo,
    LogSeverityDebug,
    LogSeverityDebug2,
    LogSeverityDebug3
  ];

  FPopupMenuItems := [pmiSaveLog, pmiSaveSelected, pmiCopyToClipboard, pmiCopySelectedToClipBoard, pmiClear, pmiDividers, pmiAutoScroll, pmiShowDate];

  NodeDataSize := SizeOf(TLogNodeData);

  FActivatedTimetampUTC := nowUTC;

  SetDefaultProperties;
end;

procedure TPTLibVirtualLogTree.OnLogEx(Sender: TObject; LogEntry: ILogEntry; const Index, Count: Integer);
begin
  DoLog(LogEntry, Index, Count);
end;

procedure TPTLibVirtualLogTree.DoLog(LogEntry: ILogEntry; const Index, Count: Integer);
begin
  // If this is the first entry, begin updates
  if Index = 0 then
  begin
    FScrollLog :=
      (FAutoScroll) and
      ((not Focused) or
       (GetLastVisible = FocusedNode));

    BeginUpdate;
  end;

  try
    // Create the log node
    InternalLog(LogEntry);
  finally
    // If this is the last entry, finish up
    if Index >= pred(Count) then
    begin
      if FMaximumLines <> 0 then
      begin
        while RootNodeCount > Cardinal(FMaximumLines) do
        begin
          DeleteNode(GetFirst);
        end;
      end;

      EndUpdate;

      if FScrollLog then
      begin
        SelectNodeEx(GetLastVisible);

        ScrollIntoView(GetLastVisible, False);
      end;
    end;
  end;

  Invalidate;

  if RootNodeCount = 1 then
  begin
    Header.AutoFitColumns(False, TSmartAutoFitType.smaUseColumnOption, 0, 0);

    Header.Columns[1].Width := ScaledPixels(3000);
  end;
end;

function TPTLibVirtualLogTree.IsNodeVisible(const Node: PVirtualNode): Boolean;
var
  NodeData: PLogNodeData;
  LowerLogText: String;
begin
  NodeData := GetNodeData(Node);

  Result := FSearchStrings.Count = 0;

  if not Result then
  begin
    LowerLogText := LowerCase(NodeData.LogEntry.LogText);

    Result := IsSearchHit(FSearchStrings, LowerLogText);
  end;

  Result :=
    (Assigned(NodeData)) and
    ((FLogLevels = []) or
     (NodeData.LogEntry.Severity in FLogLevels)) and
    (Result);
end;

procedure TPTLibVirtualLogTree.ToStrings(const Values: TStrings; const Options: TVTExportOptions; const FirstColumn: Integer = -1; const LastColumn: Integer = -1);

  procedure AddText(var s: String; NewText: String);
  begin
    if s <> '' then
      s := concat(s, ',');

    s := concat(s, AnsiQuotedStr(NewText, '"'))
  end;

var
  Node: PVirtualNode;
  LineText: String;
  i: Integer;
begin
  if vteIncludeHeader in Options then
  begin
    LineText := '';

    for i := 0 to pred(Header.Columns.Count) do
      AddText(LineText, Header.Columns[i].Text);

    Values.Add(LineText);
  end;

  if vteSelectedOnly in Options then
    Node := GetFirstSelected
  else
    Node := GetFirst;

  ShowHourGlass;
  try
    while Assigned(Node) do
    begin
      Values.Add(concat(IfThen(FShowDateColumn, concat(GetCellText(Node, 0), #09), ''), GetCellText(Node, 1)));

      if vteSelectedOnly in Options then
        Node := GetNextSelected(Node)
      else
        Node := Node.NextSibling;
    end;
  finally
    HideHourGlass;
  end;
end;

procedure TPTLibVirtualLogTree.InternalLog(LogItem: ILogEntry);
var
  Node: PVirtualNode;
  NodeData: PLogNodeData;
  LogText: String;
begin
  Node := AddChild(nil);
  Node.NodeHeight := ScaledPixels(20);

  NodeData := GetNodeData(Node);

  if Assigned(NodeData) then
  begin
    NodeData.LogEntry := LogItem;

    LogText := LogItem.FullLogText;

    if FRemoveControlCharacters then
    begin
      LogText := CTRLCharsToOrdVals(LogText, False);
    end;

    if FAutoLogLevelColours then
    begin
      case LogItem.Severity of
        LogSeverityError: LogText := concat('<font-color=clRed>', LogText, '</font-color>');
        LogSeverityInfo: LogText := concat('<font-color=clBlack>', LogText, '</font-color>');
        LogSeverityWarning: LogText := concat('<font-color=clBlue>', LogText, '</font-color>');
        LogSeverityDebug, LogSeverityDebug2, LogSeverityDebug3: LogText := concat('<font-color=clGreen>', LogText, '</font-color>')
      end;
    end;

    LogItem.LogText := LogText;

    IsVisible[Node] := IsNodeVisible(Node);

    DoOnAfterLog(Node);
  end;
end;

procedure TPTLibVirtualLogTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellRect: TRect);
var
  ColWidth: Integer;
begin
  inherited;
   Exit;
  if Column = 1 then
  begin
    ColWidth := Canvas.TextWidth(GetCellText(Node, Column)) + Integer(Indent) + TextMargin;

    if not FShowDateColumn then
    begin
      ColWidth := ColWidth + 32; // Width of image
    end;

    ColWidth := ScaledPixels(ColWidth);

    if ColWidth > Header.Columns[1].MinWidth then
    begin
      Header.Columns[1].Width := ColWidth;
      Header.Columns[1].MinWidth := ColWidth;
    end;
  end;
end;
procedure TPTLibVirtualLogTree.DoFreeNode(Node: PVirtualNode);
begin
  inherited;
end;

function TPTLibVirtualLogTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList;
var
  NodeData: PLogNodeData;
begin
  if (Assigned(Images)) and
     ((FShowImages) and (Kind in [ikNormal, ikSelected])) and
     (((FShowDateColumn) and (Column <= 0)) or
      ((not FShowDateColumn) and (Column = 1))) then
  begin
    NodeData := GetNodeData(Node);

    if Assigned(NodeData) then
    begin
      case NodeData.LogEntry.Severity of
        LogSeverityError: Index := 1;
        LogSeverityWarning: Index := 2;
        LogSeverityInfo: Index := 3;
        LogSeverityDebug: Index := 4;
        LogSeverityDebug2: Index := 5;
        LogSeverityDebug3: Index := 6;
      else
        Index := 0;
      end;
    end;
  end;

  Result := Images;// inherited DoGetImageIndex(Node, Kind, Column, Ghosted, Index);
end;

procedure TPTLibVirtualLogTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
begin
  inherited;

  pEventArgs.CellText := GetCellText(pEventArgs.Node, pEventArgs.Column)
end;

procedure TPTLibVirtualLogTree.DoOnAfterLog(Node: PVirtualNode);
begin
  if Assigned(FOnAfterLog) then
    FOnAfterLog(Self, Node);
end;

procedure TPTLibVirtualLogTree.DoOnClearLog;
begin
  if Assigned(FOnClearLog) then
    FOnClearLog(Self);
end;

procedure TPTLibVirtualLogTree.DoOnLog(var LogText: String; var CancelEntry: Boolean; LogLevel: TLogSeverity);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, LogText, CancelEntry, LogLevel);
end;

procedure TPTLibVirtualLogTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;

  Canvas.Font.Color := clBlack;
end;

function TPTLibVirtualLogTree.GetCellText(const Node: PVirtualNode; const Column: TColumnIndex): String;
var
  NodeData: PLogNodeData;
begin
  NodeData := GetNodeData(Node);

  if Assigned(NodeData) then
    case Column of
      -1, 0:
        begin
          Result := FormatISO8601(
            NodeData.LogEntry.TimestampUTC,
            [TISO8601FormatOption.isoExcludeDate,
             TISO8601FormatOption.isoLocalize]);
        end;

      1: Result := NodeData.LogEntry.LogText;
    end;
end;

procedure TPTLibVirtualLogTree.Loaded;
begin
  inherited;

  SetDefaultProperties;
end;

procedure TPTLibVirtualLogTree.ResizeTimestampColumn;
var
  SizeText: String;
begin
Exit;

  if (Parent <> nil) and
     (FShowDateColumn) and
     (Header.Columns.Count > 0) then
  begin
    SizeText := '99:99:99.999';

    if not FShowTimeOnly then
      SizeText := DateToStr(now) + ' ' + SizeText;

    Header.Columns[0].MinWidth := ScaledPixels(Canvas.TextWidth(SizeText) + 50);
    Header.Columns[0].Width := Header.Columns[0].MinWidth;
  end
end;

procedure TPTLibVirtualLogTree.GetLogInfo(Node: PVirtualNode; var Text: String;
  var Date: TDateTime; var LogLevel: TLogSeverity);
var
  NodeData: PLogNodeData;
begin
  if Assigned(Node) then
  begin
    NodeData := GetNodeData(Node);

    if Assigned(NodeData) then
    begin
      Text := NodeData.LogEntry.LogText;
      Date := NodeData.LogEntry.TimestampUTC;
      LogLevel := NodeData.LogEntry.Severity;
    end;
  end;
end;

procedure TPTLibVirtualLogTree.SetDefaultProperties;
begin
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowTreeLines, toShowButtons] + [toUseBlendedSelection, toShowHorzGridLines, toHideFocusRect];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect, toRightClickSelect];

  AddDefaultColumns([StrDate,
                     StrLog],
                    [220,
                     120]);

  //Header.AutoSizeIndex := 1;
  Header.Columns[1].MinWidth := 300;
  Header.Options := Header.Options - [hoAutoResize];

  if (PopupMenu = nil) and (not (csDesigning in ComponentState)) then
  begin
    PopupMenu := TVirtualLogPopupmenu.Create(Self);
    TVirtualLogPopupmenu(PopupMenu).OnPopupMenuItemClick := OnPopupMenuItemClick;
  end;

  SetShowDateColumn(FShowDateColumn);

  //LoadPngImagesFromResource('VirtualLogTreeImgListPng');
end;

procedure TPTLibVirtualLogTree.OnPopupMenuItemClick(Sender: TObject; MenuItem: TMenuItem);
begin
  case MenuItem.Tag of
    1: SaveToFileWithDialog([]);
    3: SaveToFileWithDialog([vteSelectedOnly]);
    2: CopyToClipboard([]);
    4: CopyToClipboard([vteSelectedOnly]);
    5: Clear;
    6: ShowDateColumn := not ShowDateColumn;
    7: AutoScroll := not AutoScroll;
  end;
end;

procedure TPTLibVirtualLogTree.Clear;
begin
  inherited;

 (* if Header.Columns.Count >= 2 then
  begin
    Header.Columns[1].MinWidth := 0;
    Header.Columns[1].Width := 0;
  end;*)

  DoOnClearLog;
end;

procedure TPTLibVirtualLogTree.Log(LogEntry: ILogEntry);
begin
  if not FPaused then
  begin
    if FThreadSafe then
    begin
      FLog.Log(LogEntry);
    end
    else
    begin
      DoLog(LogEntry, 0, 0);
    end;
  end;
end;

procedure TPTLibVirtualLogTree.Log(Value: String; const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
var
  LogEntry: ILogEntry;
begin
  LogEntry := TLogEntry.Create;
  LogEntry.LogText := Value;
  LogEntry.Severity := Severity;
  LogEntry.TimeStampUTC := TimeStampUTC;

  Log(LogEntry);
end;

procedure TPTLibVirtualLogTree.Log(Value: String; const Args: array of const; const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  Log(
    format(Value, Args),
    Severity,
    TimeStampUTC);
end;

procedure TPTLibVirtualLogTree.AddLogEntries(const LogEntries: IList<ILogEntry>);
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to pred(LogEntries.Count) do
    begin
      Log(
        LogEntries[i].LogText,
        LogEntries[i].Severity,
        UTCToLocal(LogEntries[i].TimeStampUTC));
    end;
  finally
    EndUpdate;
  end;
end;

procedure TPTLibVirtualLogTree.SeTLogSeverities(const Value: TLogSeverities);
begin
  FLogLevels := Value;

  UpdateVisibleItems;
end;

procedure TPTLibVirtualLogTree.SetLClogPopupMenuItems(
  const Value: TLClogPopupMenuItems);
begin
  FPopupMenuItems := Value;
end;

procedure TPTLibVirtualLogTree.SetShowDateColumn(const Value: Boolean);
begin
  FShowDateColumn := Value;

  if Header.Columns.Count > 0 then
  begin
    if FShowDateColumn then
      Header.Columns[0].Options := Header.Columns[0].Options + [coVisible]
    else
      Header.Columns[0].Options := Header.Columns[0].Options - [coVisible]
  end;

  ResizeTimestampColumn;

  Invalidate;
end;

procedure TPTLibVirtualLogTree.SetShowImages(const Value: Boolean);
begin
  FShowImages := Value;

  Invalidate;
end;

procedure TPTLibVirtualLogTree.SetShowTimeOnly(const Value: Boolean);
begin
  FShowTimeOnly := Value;

  ResizeTimestampColumn;

  Invalidate;
end;

{ TVirtualLogPopupmenu }

constructor TVirtualLogPopupmenu.Create(AOwner: TComponent);

var
  PopupMenuItems: TLClogPopupMenuItems;

  function AddMenuItem(const ACaption: String; ATag: Integer; PopupMenuItem: TLClogPopupMenuItem): TMenuItem;
  begin
    //if PopupMenuItem in PopupMenuItems then
    begin
      Result := TMenuItem.Create(Self);

      Result.Caption := ACaption;
      Result.Tag := ATag;
      Result.OnClick := OnMenuItemClick;

      Items.Add(Result);
    end
    //else
    //  Result := nil;
  end;

begin
  inherited Create(AOwner);

  FOwner := AOwner;

  if FOwner is TPTLibVirtualLogTree then
    PopupMenuItems := TPTLibVirtualLogTree(FOwner).PopupMenuItems
  else
    PopupMenuItems := [];

  AddMenuItem(StrSaveLog, 1, pmiSaveLog);
  AddMenuItem(StrSaveSelected, 3, pmiSaveSelected);
  AddMenuItem('-', -1, pmiDividers);
  AddMenuItem(StrCopyToClipboard, 2, pmiCopyToClipboard);
  AddMenuItem(StrCopySelected, 4, pmiCopySelectedToClipBoard);
  AddMenuItem('-', -1, pmiDividers);
  AddMenuItem(StrShowTimestamp, 6, pmiShowTimestamp);
  AddMenuItem(StrAutoScroll, 7, pmiShowTimestamp);
  AddMenuItem('-', -1, pmiDividers);
  AddMenuItem(StrClear, 5, pmiClear);
end;

procedure TVirtualLogPopupmenu.OnMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnPopupMenuItemClick) then
    FOnPopupMenuItemClick(Self, TMenuItem(Sender));
end;

{ TLogNodeData }

procedure TLogNodeData.SetLogEntry(const Value: ILogEntry);
begin
  FLogEntry := Value;
end;

end.
