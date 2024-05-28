unit PTLib.VCL.VirtualTrees.TextHighlightTree;

interface

uses
  WinAPI.Windows,

  System.Classes, System.SysUtils, System.UITypes,

  Vcl.ExtCtrls, Vcl.Graphics, Vcl.Menus,

  VirtualTrees, VirtualTrees.Header, VirtualTrees.Types,

  PTLib.Common.Timers,
  PTLib.Common.Interfaces,
  PTLib.Common.InformationList,

  PTLib.VCL.VirtualTrees,
  PTLib.VCL.Graphics;

type
  TOnPopupMenuItemClick = procedure(Sender: TObject; MenuItem: TMenuItem) of object;

  TextHighlightTreePopupMenu = class(TPopupMenu)
  private
    FOwner: TComponent;
    FOnPopupMenuItemClick: TOnPopupMenuItemClick;

    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    property OnPopupMenuItemClick: TOnPopupMenuItemClick read FOnPopupMenuItemClick write FOnPopupMenuItemClick;
  end;

  TPTLibTextHighlightTree = class(TPTLibVirtualTree)
  private
    FFadeTimer: TTimer;
    FFadeInterval: Integer;
    FHighlightColor: TColor;
    FHighlightChanges: Boolean;
    FPreviousInformationList: IInformationList;
    FInformationList: IInformationList;
    FFadeStep: Integer;

    procedure FadeInfoStrings;
    procedure SetInfoStrings(NewInfo: String; const PropertySeperator: String);
    procedure OnFadeTimer(Sender: TObject);
    procedure SetFadeInterval(const Value: Integer);
    procedure OnPopupMenuItemClick(Sender: TObject; MenuItem: TMenuItem);
    function GetLines: TStringList;
    function GetInformationList: IInformationList;
    procedure Setup;
    procedure SetFadeStep(const Value: Integer);
    function FormatLineText(const Value: String): String;
  protected
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateText(const Text: String; const PropertySeperator: String = ': '); overload;
    procedure UpdateText(const InformationList: IInformationList); overload;
    procedure Clear; override;

    property Lines: TStringList read GetLines;
    property InformationList: IInformationList read GetInformationList;
  published
    property FadeInterval: Integer read FFadeInterval write SetFadeInterval;
    property FadeStep: Integer read FFadeStep write SetFadeStep;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor;
    property HighlightChanges: Boolean read FHighlightChanges write FHighlightChanges;
  end;

implementation

resourcestring
  StrCopyAll = 'Copy All';
  StrCopySelected = 'Copy Selected';

{ TTextUpdateTree }

constructor TPTLibTextHighlightTree.Create(AOwner: TComponent);
begin
  inherited;

  FInformationList := TInformationList.Create;

  FFadeTimer := TTimer.Create(Self);
  FFadeTimer.OnTimer := OnFadeTimer;

  FSearchStrings := TStringList.Create;

  FadeInterval := 20;
  HighlightColor := $00FFCB97;
  HighlightChanges := True;
  FadeStep := 3;

  Setup;
end;

destructor TPTLibTextHighlightTree.Destroy;
begin
  inherited;
end;

procedure TPTLibTextHighlightTree.DoAfterCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
begin
  inherited;

  if pos('[', FInformationList.Lines[Node.Index]) = 1 then
  begin
    Canvas.Brush.Color := Canvas.Font.Color;
    Canvas.FillRect(Rect(CellRect.Left, CellRect.Bottom - 1, CellRect.Right, CellRect.Bottom));
  end;
end;

procedure TPTLibTextHighlightTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  inherited;

  if pos('[', FInformationList.Lines[Node.Index]) = 0 then
  begin
    if FHighlightChanges then
    begin
      Canvas.Brush.Color := ColorBetween(clWhite, FHighlightColor, Integer(FInformationList.Lines.Objects[Node.Index]));
    end;
  end;

  Canvas.FillRect(CellRect);
end;

function TPTLibTextHighlightTree.FormatLineText(const Value: String): String;
begin
  Result := Value;

  if pos('[', Value) = 0 then
  begin
    Result := '  ' + Result;
  end
  else
  begin
    Result := Trim(copy(Result, 2, length(Result) - 2));
  end;
end;

procedure TPTLibTextHighlightTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
begin
  inherited;

  pEventArgs.CellText := FormatLineText(FInformationList.Lines[pEventArgs.Node.Index]);
end;

procedure TPTLibTextHighlightTree.DoPaintText(Node: PVirtualNode;
  const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited;

  if pos('[', FInformationList.Lines[Node.Index]) = 1 then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
end;

procedure TPTLibTextHighlightTree.OnFadeTimer(Sender: TObject);
begin
  FadeInfoStrings;
end;

procedure TPTLibTextHighlightTree.Clear;
begin
  inherited;

  UpdateText('');
end;

procedure TPTLibTextHighlightTree.FadeInfoStrings;
var
  i: Integer;
  NewVal: Integer;
begin
  if FHighlightChanges then
  begin
    for i := 0 to pred(FInformationList.Lines.Count) do
      if Integer(FInformationList.Lines.Objects[i]) > 0 then
      begin
        NewVal := Integer(FInformationList.Lines.Objects[i]) - FFadeStep;

        if NewVal < 0 then
        begin
          NewVal := 0;
        end;

        FInformationList.Lines.Objects[i] := TObject(NewVal);
      end;

    Invalidate;
  end;
end;

function TPTLibTextHighlightTree.GetInformationList: IInformationList;
begin
  Result := FInformationList;
end;

function TPTLibTextHighlightTree.GetLines: TStringList;
begin
  Result := FInformationList.Lines as TStringList;
end;

procedure TPTLibTextHighlightTree.Loaded;
begin
  inherited;

  Setup;
end;

procedure TPTLibTextHighlightTree.Setup;
begin
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions  + [toFullRowSelect, toMultiSelect];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot, toShowButtons, toShowTreeLines, toShowDropmark];

  if (PopupMenu = nil) and (not (csDesigning in ComponentState)) then
  begin
    PopupMenu := TextHighlightTreePopupMenu.Create(Self);
    TextHighlightTreePopupMenu(PopupMenu).OnPopupMenuItemClick := OnPopupMenuItemClick;
  end;
end;

procedure TPTLibTextHighlightTree.OnPopupMenuItemClick(Sender: TObject; MenuItem: TMenuItem);
begin
  case MenuItem.Tag of
    1: CopyToClipboard([]);
    2: CopyToClipboard([vteSelectedOnly]);
  end;
end;


procedure TPTLibTextHighlightTree.SetFadeInterval(const Value: Integer);
begin
  FFadeInterval := Value;

  FFadeTimer.Interval := Value;
end;

procedure TPTLibTextHighlightTree.SetFadeStep(const Value: Integer);
begin
  FFadeStep := Value;

  if FFadeStep < 0 then
  begin
    FFadeStep := 1;
  end;
end;

procedure TPTLibTextHighlightTree.SetInfoStrings(NewInfo: String; const PropertySeperator: String);
var
  InformationList: IInformationList;
begin
  InformationList := TInformationList.Create(PropertySeperator);
  InformationList.Lines.Text := NewInfo;

  UpdateText(InformationList);
 (*
  if (Assigned(FInformationList)) and (not (csDestroying in ComponentState)) then
  begin
    TempStrings := TStringList.Create;
    try
      TempStrings.Text := NewInfo;

      if FInformationList.Lines.Count <> TempStrings.Count then
      begin
        FInformationList.Lines := TempStrings;

        RootNodeCount := FInformationList.Lines.Count;
      end
      else
      begin
        for i := 0 to pred(FInformationList.Lines.Count) do
        begin
          if FInformationList.Lines[i] <> TempStrings[i] then
          begin
            FInformationList.Lines[i] := TempStrings[i];
            FInformationList.Lines.Objects[i] := TObject(100)
          end;
        end;
      end;
    finally
      FreeAndNil(TempStrings);
    end;

    Invalidate;
  end;

  UpdateVisibleItems(False);  *)
end;

procedure TPTLibTextHighlightTree.UpdateText(
  const InformationList: IInformationList);
var
  i: Integer;
begin
  FPreviousInformationList := FInformationList;
  FInformationList := InformationList;

  if (FPreviousInformationList <> nil) and
     (FPreviousInformationList.Lines.Count = FInformationList.Lines.Count) then
  begin
    for i := 0 to pred(FInformationList.Lines.Count) do
    begin
      if FInformationList.Lines[i] <> FPreviousInformationList.Lines[i] then
      begin
        FInformationList.Lines.Objects[i] := TObject(100);
      end;
    end;
  end;

  RootNodeCount := FInformationList.Lines.Count;
  Invalidate;
end;

procedure TPTLibTextHighlightTree.UpdateText(const Text: String; const PropertySeperator: String);
begin
  SetInfoStrings(Text, PropertySeperator);
end;

{ TextHighlightTreePopupMenu }

constructor TextHighlightTreePopupMenu.Create(AOwner: TComponent);

  function AddMenuItem(const ACaption: String; ATag: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);

    Result.Caption := ACaption;
    Result.Tag := ATag;
    Result.OnClick := OnMenuItemClick;

    Items.Add(Result);
  end;

begin
  inherited;

  FOwner := AOwner;

  AddMenuItem(StrCopyAll, 1);
  AddMenuItem(StrCopySelected, 2);
end;

procedure TextHighlightTreePopupMenu.OnMenuItemClick(Sender: TObject);
begin
  if Assigned(FOnPopupMenuItemClick) then
    FOnPopupMenuItemClick(Self, TMenuItem(Sender));
end;

end.
