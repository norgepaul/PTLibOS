unit PTLib.FMX.LogViewer;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, FMX.Dialogs, System.Actions, System.DateUtils,

  FMX.ActnList, FMX.Controls, FMX.Menus, FMX.Types, System.ImageList,
  FMX.ImgList, FMX.Grid, FMX.Layouts, FMX.Graphics, FMX.Forms, FMX.StdCtrls,
  FMX.Platform, FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox,

  PTLib.Common.Interfaces,
  PTLib.Common.Dates,
  PTLib.Common.Types,
  PTLib.Common.Log;

type
  TGetExternalLog = reference to procedure(out Value: ILog);

  TScrollFixedGrid = class(TGrid)
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  end;

  TframeLogViewer = class(TFrame, ILog)
    PTLibLog1: TPTLibLog;
    grdLog: TGrid;
    ImageColumn1: TImageColumn;
    StringColumn1: TStringColumn;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actCopyToClipboard: TAction;
    actSaveSelected: TAction;
    SaveDialog1: TSaveDialog;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    actClear: TAction;
    MenuItem3: TMenuItem;
    StringColumn2: TStringColumn;
    procedure PTLibLog1Log(Sender: TObject; const LogEntry: ILogEntry);
    procedure FrameResize(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSaveSelectedExecute(Sender: TObject);
  private
    FExternalLog: TGetExternalLog;
    FSelectedRow: Integer;
    FSelectedColumn: Integer;

    procedure OnGetValue(Sender: TObject; const Col, Row: Integer; var Value: System.Rtti.TValue);
    function GetLog: ILog;
    procedure FixColumnSizes;
  protected
    property InternalLog: ILog read GetLog implements ILog;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Log(const LogText: String; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(const LogText: String; const Args: Array of const; const Severity: TLogSeverity = LogSeverityInfo; const TimeStampUTC: TDateTime = 0); overload;
    procedure Log(LogEntry: ILogEntry); overload;
    procedure Clear;
    procedure UpdateLogView;
    procedure SetExternalLog(const Value: TGetExternalLog);
    function LogController: ILog;
  end;

implementation

{$R *.fmx}

{ TframeLogViewer }

function TryGetClipboardService(out ClipboardService: IFMXClipboardService): boolean;
begin
  Result := TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService);

  if Result then
  begin
    ClipboardService := IFMXClipboardService(TPlatformServices.Current.GetPlatformService(IFMXClipboardService));
  end;
end;

procedure StringToClipboard(const Value: string);
var
  clp: IFMXClipboardService;
begin
  if TryGetClipboardService(clp) then
    clp.SetClipboard(Value);
end;

procedure TframeLogViewer.actClearExecute(Sender: TObject);
begin
  LogController.ClearHistory;
  grdLog.RowCount := 0;
end;

procedure TframeLogViewer.actCopyToClipboardExecute(Sender: TObject);
var
  Lines: String;
begin
  Lines := '';

  StringToClipboard(LogController.SaveToString);
end;

procedure TframeLogViewer.actSaveSelectedExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    LogController.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TframeLogViewer.Clear;
begin
  grdLog.RowCount := 0;
  InternalLog.ClearHistory;
end;

constructor TframeLogViewer.Create(AOwner: TComponent);
begin
  inherited;

  grdLog.OnGetValue := OnGetValue;
end;

procedure TframeLogViewer.OnGetValue(Sender: TObject; const Col, Row: Integer; var Value: System.Rtti.TValue);
var
  Bitmap: TBitmap;
  ImageIndex: Integer;
begin
  case Col of
    0:
      begin
        ImageIndex := Integer(InternalLog.GetLogHistoryItem(Row).Severity);

        if (ImageIndex < 0) or
           (ImageIndex >= ImageList1.Count) then
        begin
          ImageIndex := 0;
        end;

        Bitmap := ImageList1.Bitmap(
          TSize.Create(16, 16),
          ImageIndex);

        if Bitmap <> nil then
        begin
          Value := Bitmap;
        end;
      end;
    1:
      begin
        Value := FormatISO8601(
          InternalLog.GetLogHistoryItem(Row).TimeStampUTC,
          [TISO8601FormatOption.isoExcludeDate,
           TISO8601FormatOption.isoLocalize]);

        //  DateTimeToStr(UTCToLocal(InternalLog.GetLogHistoryItem(Row).TimeStampUTC)) +
        //  '.' +
        //  MilliSecondOf(InternalLog.GetLogHistoryItem(Row).TimeStampUTC).ToString.PadLeft(3, '0');
      end;

    2: Value := InternalLog.GetLogHistoryItem(Row).LogText;
  end;
end;

procedure TframeLogViewer.FrameResize(Sender: TObject);
begin
  FixColumnSizes;
end;

procedure TframeLogViewer.FixColumnSizes;
begin
  grdLog.Columns[2].Width := 2000;
end;

function TframeLogViewer.GetLog: ILog;
begin
  if Assigned(FExternalLog) then
  begin
    FExternalLog(Result);
  end
  else
  begin
    Result := PTLibLog1 as ILog;
  end;
end;

procedure TframeLogViewer.Log(LogEntry: ILogEntry);
begin
  InternalLog.Log(LogEntry);
end;

procedure TframeLogViewer.Log(const LogText: String;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  InternalLog.Log(
    LogText,
    '',
    Severity,
    TimeStampUTC);
end;

procedure TframeLogViewer.Log(const LogText: String; const Args: array of const;
  const Severity: TLogSeverity; const TimeStampUTC: TDateTime);
begin
  InternalLog.Log(
    LogText,
    Args,
    '',
    Severity,
    TimeStampUTC);
end;

function TframeLogViewer.LogController: ILog;
begin
  Result := InternalLog;
end;

procedure TframeLogViewer.PTLibLog1Log(Sender: TObject;
  const LogEntry: ILogEntry);
begin
  UpdateLogView;
end;

procedure TframeLogViewer.SetExternalLog(const Value: TGetExternalLog);
begin
  FExternalLog := Value;
end;

procedure TframeLogViewer.UpdateLogView;
var
  ViewPort, CellRect: TRectF;
begin
  grdLog.BeginUpdate;
  try
    if grdLog.Selected <> -1 then
    begin
      FSelectedRow := grdLog.Row;
      FSelectedColumn := grdLog.Col;

      if FSelectedColumn = 1 then
      begin
        FSelectedColumn := 0;
      end;
    end;

    grdLog.Selected := -1;

    grdLog.RowCount := InternalLog.GetLogHistoryItemCount;
  finally
   grdLog.EndUpdate;
  end;

  if not grdLog.IsFocused then
  begin
    grdLog.ScrollBy(0, MaxInt);
    grdLog.SelectRow(grdLog.RowCount - 1);
    grdLog.Repaint;
  end
  else
  begin
    // This is a tricky workaround for an awful FMX bug
    ViewPort := RectF(
      0,
      grdLog.ViewportPosition.Y,
      MaxInt,
      grdLog.ViewportPosition.Y + grdLog.ViewportSize.Height);

    CellRect := grdLog.CellRect(0, FSelectedRow);

    if ViewPort.Contains(CellRect) then
    begin
      grdLog.SelectCell(FSelectedColumn, FSelectedRow);
    end;
  end;
end;

{ TScrollFixedGrid }

procedure TScrollFixedGrid.DoBeginUpdate;
begin
  inherited;

end;

procedure TScrollFixedGrid.DoEndUpdate;
begin
  inherited;

end;

end.
