unit PTLib.VCL.VirtualTrees.Frames.Logs;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, System.Actions,
  Vcl.ActnList, Vcl.ComCtrls,

  PTLib.VCL.VirtualTrees.Frames.SearchLog,
  PTLib.VCL.VirtualTrees.Log,
  PTLib.VCL.VirtualTrees,

  PTLib.Common.Log,
  PTLib.Common.Interfaces,
  PTLib.Common.Types;

type
  TOnNewLogFrame = procedure(Sender: TObject; const LogName: String; LogFrame: TframeSearchLog) of object;

  TframeLogs = class(TFrame)
    ActionList1: TActionList;
    actShowDateColumn: TAction;
    actSaveLog: TAction;
    actCopyLog: TAction;
    actClear: TAction;
    actCopy: TAction;
    popLog: TPopupMenu;
    MenuItem9: TMenuItem;
    SaveSelected1: TMenuItem;
    N7: TMenuItem;
    MenuItem10: TMenuItem;
    Copyline1: TMenuItem;
    MenuItem11: TMenuItem;
    Clearlog1: TMenuItem;
    MenuItem13: TMenuItem;
    MaximumLogEntries1: TMenuItem;
    NoLimit1: TMenuItem;
    N1: TMenuItem;
    N10001: TMenuItem;
    N100001: TMenuItem;
    N1000001: TMenuItem;
    N10000001: TMenuItem;
    N100000001: TMenuItem;
    actSaveSelected: TAction;
    actPauseLogging: TAction;
    PauseLogging1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N5000001: TMenuItem;
    popTabs: TPopupMenu;
    actCloseTab: TAction;
    actCloseAllTabs: TAction;
    actCloseTabsToTheRight: TAction;
    CloseTab1: TMenuItem;
    CloseAllTabs1: TMenuItem;
    CloseTabstotheRight1: TMenuItem;
    pcLogs: TPageControl;
    procedure actShowDateColumnExecute(Sender: TObject);
    procedure actSaveLogExecute(Sender: TObject);
    procedure actCopyLogExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure N100000001Click(Sender: TObject);
    procedure actSaveSelectedExecute(Sender: TObject);
    procedure actPauseLoggingExecute(Sender: TObject);
    procedure popLogPopup(Sender: TObject);
    procedure pcLogsChange(Sender: TObject);
    procedure actCloseTabExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actCloseAllTabsExecute(Sender: TObject);
    procedure actCloseTabsToTheRightExecute(Sender: TObject);
  private
    FOnLogTabChanged: TNotifyEvent;
    FOnNewLogFrame: TOnNewLogFrame;
    FAutoSelectLog: Boolean;
    FDisableLogChangeEvent: Boolean;

    procedure UpdateControls;
    function GetLogTabName(const LogName: String): String;
    function SanitizeControlName(const ControlName: String): String;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoOnNewLogFrame(const LogName: String; LogFrame: TframeSearchLog); virtual;
    procedure DoOnLogTabChanged; virtual;
  public
    function GetLogFrameByServerName(const LogName: String): TframeSearchLog;
    function GetTabByServerName(const LogName: String): TTabSheet;
    function ActivateTabByServerName(const LogName: String): TTabSheet;
    function GetActiveLog: TPTLibVirtualLogTree;
    function GetActiveLogFrame: TframeSearchLog;
    procedure Log(const LogName: String; const LogEntry: String; LogLevel: TLogSeverity = LogSeverityInfo; TimeStamp: TDateTime = 0); overload;
    procedure Log(const LogName: String; const LogEntry: String; Args: Array of const; LogLevel: TLogSeverity = LogSeverityInfo; TimeStamp: TDateTime = 0); overload;
    procedure AddLogEntries(const LogName: String; const LogEntries: IList<ILogEntry>);
    procedure SetMaximumLines(const MaxLines: Integer);
    procedure LoadFromStrings(const LogName: String; const Values: TStrings);
    procedure CloseAllTabs;
  published
    property OnNewLogFrame: TOnNewLogFrame read FOnNewLogFrame write FOnNewLogFrame;
    property OnLogTabChanged: TNotifyEvent read FOnLogTabChanged write FOnLogTabChanged;

    property AutoSelectLog: Boolean read FAutoSelectLog write FAutoSelectLog;
  end;

implementation

uses
  PTLib.Common.Dates;

{$R *.dfm}

function TframeLogs.SanitizeControlName(const ControlName: String): String;
var
  i: Integer;
begin
  Result := ControlName;

  for i := 1 to length(Result) do
    if not CharInSet(Result[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Result[i] := '_';
end;


function TframeLogs.GetLogTabName(const LogName: String): String;
begin
  Result := 'tab' + SanitizeControlName(LogName);
end;

function TframeLogs.ActivateTabByServerName(const LogName: String): TTabSheet;
begin
  Result := GetTabByServerName(LogName);

  if Result <> nil then
  begin
    pcLogs.ActivePage := Result;
  end;
end;

function TframeLogs.GetTabByServerName(const LogName: String): TTabSheet;
var
  i: Integer;
  TabName: String;
  SanitisedServerName: String;
begin
  Result := nil;

  SanitisedServerName := SanitizeControlName(LogName);
  TabName := GetLogTabName(LogName);

  for i := 0 to pred(pcLogs.PageCount) do
    if pcLogs.Pages[i].Name = TabName then
    begin
      Result := pcLogs.Pages[i];

      Break;
    end;
end;

function TframeLogs.GetLogFrameByServerName(const LogName: String): TframeSearchLog;
var
  Tab: TTabSheet;
begin
  Tab := GetTabByServerName(LogName);

  if Tab = nil then
  begin
    Result := TframeSearchLog.Create(Self);
    Result.LogName := LogName;

    FDisableLogChangeEvent := True;
    try
      Tab := TTabSheet.Create(pcLogs);
      Tab.Parent := pcLogs;
      Tab.PageControl := pcLogs;
      Tab.Name := GetLogTabName(LogName);
      Tab.Caption := LogName;
    finally
      FDisableLogChangeEvent := False;
    end;

    Result.ParentFont := False;
    Result.Parent := Tab;
    Result.Align := alClient;
    Result.Name :=  'lof' + SanitizeControlName(LogName);
    Result.StatusBar1.Panels[0].Text := LogName;
    Result.vtLog.PopupMenu := popLog;
    Result.vtLog.ShowDateColumn := True;

    pcLogs.Visible := True;

    DoOnNewLogFrame(LogName, Result);
    DoOnLogTabChanged;
  end
  else
  begin
    Result := TframeSearchLog(Tab.Controls[0]);
  end;

  if (Tab <> nil) and (FAutoSelectLog) then
  begin
    pcLogs.ActivePage := Tab;
  end;
end;

procedure TframeLogs.LoadFromStrings(const LogName: String; const Values: TStrings);
var
  LogFrame: TframeSearchLog;
begin
  LogFrame := GetLogFrameByServerName(LogName);

  if Values <> nil then
  begin
    LogFrame.Log.Logger.LoadFromStrings(Values);
    //LogFrame.Log.ProcessQueue;
  end;
end;

procedure TframeLogs.Log(const LogName, LogEntry: String; Args: array of const;
  LogLevel: TLogSeverity; TimeStamp: TDateTime);
begin
  GetLogFrameByServerName(LogName).vtLog.Log(LogEntry, Args, LogLevel, TimeStamp);
end;

procedure TframeLogs.N100000001Click(Sender: TObject);
begin
  if GetActiveLog <> nil then
  begin
    GetActiveLog.MaximumLines := TMenuItem(Sender).Tag;

    case GetActiveLog.MaximumLines of
      0: NoLimit1.Checked := True;
      1000: N10001.Checked := True;
      10000: N100001.Checked := True;
      100000: N1000001.Checked := True;
      500000: N5000001.Checked := True;
      1000000: N10000001.Checked := True;
      10000000: N100000001.Checked := True;
    else
      N100001.Checked := True;
    end;
  end;
end;

procedure TframeLogs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
  end;
end;

procedure TframeLogs.pcLogsChange(Sender: TObject);
begin
  DoOnLogTabChanged;
end;

procedure TframeLogs.popLogPopup(Sender: TObject);
begin
  if GetActiveLog <> nil then
  begin
    actPauseLogging.Checked := GetActiveLog.Paused;
    actShowDateColumn.Checked := GetActiveLog.ShowDateColumn;
  end;
end;

procedure TframeLogs.SetMaximumLines(const MaxLines: Integer);
var
  i: Integer;
begin
  for i := 0 to pred(pcLogs.PageCount) do
    TframeSearchLog(pcLogs.ActivePage.Controls[0]).vtLog.MaximumLines := MaxLines;
end;

procedure TframeLogs.Log(const LogName, LogEntry: String;
  LogLevel: TLogSeverity; TimeStamp: TDateTime);
var
  LogFrame: TframeSearchLog;
begin
  LogFrame := GetLogFrameByServerName(LogName);

  if (LogFrame <> nil) and
     (LogEntry <> '') then
    LogFrame.vtLog.Log(LogEntry, LogLevel, TimeStamp);
end;

procedure TframeLogs.AddLogEntries(const LogName: String; const LogEntries: IList<ILogEntry>);
var
  LogFrame: TframeSearchLog;
begin
  LogFrame := GetLogFrameByServerName(LogName);

  if LogFrame <> nil then
  begin
    LogFrame.vtLog.AddLogEntries(LogEntries);
  end;
end;

procedure TframeLogs.actClearExecute(Sender: TObject);
begin
  GetActiveLog.Clear;
end;

procedure TframeLogs.actCloseAllTabsExecute(Sender: TObject);
begin
  CloseAllTabs;
end;

procedure TframeLogs.CloseAllTabs;
begin
  while pcLogs.PageCount > 0 do
  begin
    pcLogs.Pages[0].Free;
  end;
end;

procedure TframeLogs.actCloseTabExecute(Sender: TObject);
begin
  pcLogs.ActivePage.Free;
end;

procedure TframeLogs.actCloseTabsToTheRightExecute(Sender: TObject);
begin
  while pred(pcLogs.PageCount) > pcLogs.ActivePageIndex do
  begin
    pcLogs.Pages[pred(pcLogs.PageCount)].Free;
  end;
end;

procedure TframeLogs.actCopyExecute(Sender: TObject);
begin
  GetActiveLog.CopyToClipboard([vteSelectedOnly]);
end;

procedure TframeLogs.actCopyLogExecute(Sender: TObject);
begin
  GetActiveLog.CopyToClipboard([]);
end;

procedure TframeLogs.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  actCloseTab.Enabled := pcLogs.ActivePage <> nil;
  actCloseAllTabs.Enabled := pcLogs.ActivePage <> nil;
  actCloseTabsToTheRight.Enabled := pcLogs.ActivePageIndex < pred(pcLogs.PageCount);

  Handled := True;
end;

procedure TframeLogs.actPauseLoggingExecute(Sender: TObject);
begin
  if GetActiveLog <> nil then
    GetActiveLog.Paused := not GetActiveLog.Paused;
end;

procedure TframeLogs.actSaveLogExecute(Sender: TObject);
begin
  GetActiveLog.SaveToFileWithDialog([]);
end;

procedure TframeLogs.actSaveSelectedExecute(Sender: TObject);
begin
  GetActiveLog.SaveToFileWithDialog([vteSelectedOnly]);
end;

procedure TframeLogs.actShowDateColumnExecute(Sender: TObject);
begin
  UpdateControls;
end;

procedure TframeLogs.DoOnLogTabChanged;
begin
  if not FDisableLogChangeEvent then
  begin
    if Assigned(FOnLogTabChanged) then
      FOnLogTabChanged(Self);
  end;
end;

procedure TframeLogs.DoOnNewLogFrame(const LogName: String;
  LogFrame: TframeSearchLog);
begin
  if Assigned(FOnNewLogFrame) then
    FOnNewLogFrame(Self, LogName, LogFrame);
end;

function TframeLogs.GetActiveLog: TPTLibVirtualLogTree;
begin
  if (pcLogs.ActivePage = nil) or (pcLogs.ActivePage.ControlCount = 0) then
  begin
    Result := nil;
  end
  else
  begin
    Result := TframeSearchLog(pcLogs.ActivePage.Controls[0]).vtLog;
  end;
end;

function TframeLogs.GetActiveLogFrame: TframeSearchLog;
begin
  if pcLogs.ActivePage <> nil then
    Result := TframeSearchLog(pcLogs.ActivePage.Controls[0])
  else
    Result := nil;
end;

procedure TframeLogs.UpdateControls;
begin
  if GetActiveLog <> nil then
  begin
    GetActiveLog.ShowDateColumn := actShowDateColumn.Checked;
  end;
end;

end.
