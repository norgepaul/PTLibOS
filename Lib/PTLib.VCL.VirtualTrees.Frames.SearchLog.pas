unit PTLib.VCL.VirtualTrees.Frames.SearchLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, DateUtils,

  PTLib.Common.Dates,
  PTLib.Common.Strings,
  PTLib.Common.Log,
  PTLib.Common.Types,

  PTLib.VCL.AwesomeEdit,
  PTLib.VCL.SearchEdit,

  PTLib.VCL.VirtualTrees,
  PTLib.VCL.VirtualTrees.Log, System.ImageList, Vcl.ImgList,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.VirtualImageList;

type
  TframeSearchLog = class(TFrame)
    pnlSearchLogHeader: TPanel;
    cbLogLevel: TComboBox;
    chkAutoScrollLog: TCheckBox;
    vtLog: TPTLibVirtualLogTree;
    StatusBar1: TStatusBar;
    tmrStats: TTimer;
    chkShowTimestamp: TCheckBox;
    VirtualImageList1: TVirtualImageList;
    ImageCollection1: TImageCollection;
    edtStationLogSearch: TPTLibSearchEdit;
    procedure edtStationLogSearchEditChange(Sender: TObject);
    procedure cbLogLevelChange(Sender: TObject);
    procedure chkAutoScrollLogClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure tmrStatsTimer(Sender: TObject);
  private
    FLogName: String;
    FShowLoggingTime: Boolean;

    procedure UpdateLogAutoScroll;
    procedure UpdateLogLevel;
    procedure UpdateStatusBar;
  public
    constructor Create(AOwner: TComponent); override;

    property LogName: String read FLogName write FLogName;
    property Log: TPTLibVirtualLogTree read vtLog;
    property ShowLoggingTime: Boolean read FShowLoggingTime write FShowLoggingTime;
  end;

implementation

resourcestring
  StrLoggingFor = 'Logging for: ';
  StrCurrentLines = 'Visible Lines: ';
  StrTotalLines = 'Total Lines: ';
  StrLiness = 'Lines/s: ';
  StrUnlimited = 'Unlimited';

{$R *.dfm}

procedure TframeSearchLog.UpdateLogLevel;
var
  ll, LogLevel: TLogSeverity;
begin
  LogLevel := TLogSeverity(cbLogLevel.ItemIndex);

  vtLog.LogLevels := [];

  for ll := LogSeverityError to LogSeverityDebug3 do
  begin
    if (ll > LogLevel) and (LogLevel <> LogSeverityNone) then
    begin
      Break;
    end
    else
    begin
      vtLog.LogLevels := vtLog.LogLevels + [ll];
    end;
  end;
end;

procedure TframeSearchLog.cbLogLevelChange(Sender: TObject);
begin
  UpdateLogLevel;
end;

procedure TframeSearchLog.chkAutoScrollLogClick(Sender: TObject);
begin
  UpdateLogAutoScroll;
end;

constructor TframeSearchLog.Create(AOwner: TComponent);
begin
  inherited;

  tmrStats.Enabled := not (csDesigning in ComponentState);
  FShowLoggingTime := True;
end;

procedure TframeSearchLog.edtStationLogSearchEditChange(Sender: TObject);
begin
  vtLog.Search(edtStationLogSearch.Text);
end;

procedure TframeSearchLog.FrameResize(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to pred(StatusBar1.Panels.Count) do
    StatusBar1.Panels[i].Width := Width div StatusBar1.Panels.Count;

  edtStationLogSearch.Visible := edtStationLogSearch.Width > 30;
end;

procedure TframeSearchLog.tmrStatsTimer(Sender: TObject);
begin
  UpdateStatusBar;
end;

procedure TframeSearchLog.UpdateStatusBar;
var
  Seconds: Int64;
  MaxLines: String;
begin
  if not (csDestroying in ComponentState) then
  begin
    Seconds := SecondsBetween(nowUTC, vtLog.ActivatedTimetampUTC);

    StatusBar1.Panels.BeginUpdate;
    try
      if vtLog.MaximumLines = 0 then
        MaxLines := StrUnlimited
      else
        MaxLines := IntToStr(vtLog.MaximumLines);

      if FShowLoggingTime then
        StatusBar1.Panels[1].Text := StrLoggingFor + SecondsToTimeString(Seconds)
      else
        StatusBar1.Panels[1].Text := '';
      StatusBar1.Panels[2].Text := StrCurrentLines + IntToStr(vtLog.VisibleCount) + '/' + IntToStr(vtLog.RootNodeCount);
      StatusBar1.Panels[3].Text := StrTotalLines + IntToStr(vtLog.TotalLogLines) + '/' + MaxLines;
    finally
      StatusBar1.Panels.EndUpdate;
    end;
  end;
end;

procedure TframeSearchLog.UpdateLogAutoScroll;
begin
  vtLog.AutoScroll := chkAutoScrollLog.Checked;
  vtLog.ShowDateColumn := chkShowTimestamp.Checked;
end;

end.
