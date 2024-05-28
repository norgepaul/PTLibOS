unit frmDemoMainU;

{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{                                                                    }
{           Copyright (c) 1998-2015 Easy-IP AS.                      }
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
{                                                                    }
{********************************************************************}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.ColorGrd, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Samples.Spin, Vcl.ExtCtrls,Vcl.ComCtrls, Vcl.Menus,

  PTLib.VCL.FileEdit,
  PTLib.VCL.ProportionalSplitter,
  PTLib.Common.Log,
  PTLib.Common.Classes,
  PTLib.Common.Dates,
  PTLib.Common.Types,
  PTLib.Common.Strings,
  PTLib.Common.Interfaces,
  PTLib.Common.Log.Transmitter,
  PTLib.Common.Log.Receiver,
  PTLib.Common.Utils,
  PTLib.VCL.SearchEdit,
  PTLib.Common.Network.SimpleHTTPServer,
  PTLib.VCL.AwesomeEdit.Types,
  PTLib.VCL.AwesomeEdit,
  PTLib.Common.Timers,
  PTLib.Common.APIs.MapQuest,
  PTLib.Common.PriorityQueue,
  PTLib.Common.SmartCard,
  PTLib.Common.SmartCard.Types,

  System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, PTLib.VCL.Console, VirtualTrees, PTLib.VCL.VirtualTrees.Log, System.ImageList, Vcl.ImgList, Vcl.Mask, System.Actions,
  Vcl.ActnList, PTLib.VCL.PageControl, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Vcl.VirtualImageList;

type
  TfrmDemoMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    PTLibOpenFileEdit1: TPTLibOpenFileEdit;
    PTLibSaveFileEdit1: TPTLibSaveFileEdit;
    PTLibDirectoryEdit1: TPTLibDirectoryEdit;
    Panel2: TPanel;
    Panel3: TPanel;
    PTLibProportionalSplitter1: TPTLibProportionalSplitter;
    PTLibProportionalSplitter2: TPTLibProportionalSplitter;
    Panel4: TPanel;
    chkEnableLogReceiver: TCheckBox;
    RichEdit2: TRichEdit;
    Panel5: TPanel;
    chkLogTransmitterEnabled: TCheckBox;
    RichEdit1: TRichEdit;
    Edit1: TEdit;
    TabSheet3: TTabSheet;
    Panel6: TPanel;
    SearchEdit1: TPTLibSearchEdit;
    GroupBox3: TGroupBox;
    cbAwesomeBorder: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    edtAwesomeRadius: TSpinEdit;
    tabQueues: TTabSheet;
    Panel7: TPanel;
    pnlLowPriorityQueue: TPanel;
    Panel8: TPanel;
    memLowPriorityQueue: TRichEdit;
    pnlHighPriorityQueue: TPanel;
    Panel10: TPanel;
    memHighPriorityQueue: TRichEdit;
    pnlMediumPriorityQueue: TPanel;
    Panel12: TPanel;
    memMediumPriorityQueue: TRichEdit;
    btnStart: TButton;
    btnStop: TButton;
    tmrQueuesSend: TTimer;
    chkConnected: TCheckBox;
    tmrCheckSend: TTimer;
    edtQueueReliability: TSpinEdit;
    NetHTTPRequest1: TNetHTTPRequest;
    NetHTTPClient1: TNetHTTPClient;
    PTLibMultiPriorityStringDataQueues1: TPTLibMultiPriorityStringDataQueues;
    TabSheet4: TTabSheet;
    Console1: TConsole;
    tmrConsoleReply: TTimer;
    Panel9: TPanel;
    PTLibProportionalSplitter3: TPTLibProportionalSplitter;
    Panel11: TPanel;
    Panel13: TPanel;
    PTLibProportionalSplitter4: TPTLibProportionalSplitter;
    Panel14: TPanel;
    tabMARS: TTabSheet;
    TabSheet5: TTabSheet;
    edtGeoAddress: TEdit;
    Button1: TButton;
    edtGeoLatRes: TEdit;
    edtMapQuestAPIKey: TEdit;
    edtGeoAddressRes: TEdit;
    Button2: TButton;
    edtGeoLat: TEdit;
    edtGeoLng: TEdit;
    edtGeoLngRes: TEdit;
    Button3: TButton;
    edtGeoElevation: TEdit;
    ImageList1: TImageList;
    tabSmartCard: TTabSheet;
    Panel15: TPanel;
    Button4: TButton;
    Button5: TButton;
    ActionList1: TActionList;
    actSmartCardEnable: TAction;
    actSmartCardDisable: TAction;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    memSmartCardErrors: TRichEdit;
    Panel16: TPanel;
    Label1: TLabel;
    lblSmartCardUID: TLabel;
    Panel17: TPanel;
    Label2: TLabel;
    lblSmartCardStatus: TLabel;
    Panel18: TPanel;
    Label6: TLabel;
    lblSmartCardATR: TLabel;
    Panel19: TPanel;
    Label8: TLabel;
    lblSmartCardProtocol: TLabel;
    Panel20: TPanel;
    Label10: TLabel;
    lblSmartCardType: TLabel;
    tabPageControl: TTabSheet;
    PTLibPageControl1: TPTLibPageControl;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    Button6: TButton;
    Memo1: TMemo;
    Button7: TButton;
    Button8: TButton;
    TabControl1: TTabControl;
    Button9: TButton;
    Button10: TButton;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    vtLog: TPTLibVirtualLogTree;
    Panel21: TPanel;
    Button11: TButton;
    VirtualImageList1: TVirtualImageList;
    ImageCollection1: TImageCollection;
    procedure PTLibLog1Log(Sender: TObject; const LogEntry: ILogEntry);
    procedure tmrLogTimer(Sender: TObject);
    procedure PTLibLog2Log(Sender: TObject; const LogEntry: ILogEntry);
    procedure chkLogTransmitterEnabledClick(Sender: TObject);
    procedure chkEnableLogReceiverClick(Sender: TObject);
    procedure hisisatest1Click(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CommonAwesomeEditPropertyChange(Sender: TObject);
    procedure tabQueuesResize(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tmrQueuesSendTimer(Sender: TObject);
    procedure tmrCheckSendTimer(Sender: TObject);
    procedure PTLibPriorityQueues1ProcessItem(Sender: TObject;
      const QueueItem: IPriorityQueueItem; out MessageSent: Boolean);
    procedure Console1CommandExecute(Sender: TCustomConsole; ACommand: string;
      var ACommandFinished: Boolean);
    procedure tmrConsoleReplyTimer(Sender: TObject);
    procedure Console1GetPrompt(Sender: TCustomConsole; var APrompt,
      ADefaultText: string; var ADefaultCaretPos: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actSmartCardEnableExecute(Sender: TObject);
    procedure actSmartCardDisableExecute(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    FSendingList: IList<IPriorityQueueItem>;
    FSmartCard: IPTLibSmartCard;

    procedure UpdateAwesomeEditProperties;
    procedure StartQueues(const DoStart: Boolean);
    procedure UpdateQueues;
  end;

var
  frmDemoMain: TfrmDemoMain;

implementation

{$R *.dfm}

procedure TfrmDemoMain.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actSmartCardEnable.Enabled := not FSmartCard.Active;
  actSmartCardDisable.Enabled := FSmartCard.Active;

  Handled := True;
end;

procedure TfrmDemoMain.actSmartCardDisableExecute(Sender: TObject);
begin
  FSmartCard.Active := False;
end;

procedure TfrmDemoMain.actSmartCardEnableExecute(Sender: TObject);
var
  PTLibSmartCardPCSC: IPTLibSmartCardPCSC;
begin
  FSmartCard.Active := True;

  if Supports(FSmartCard, IPTLibSmartCardPCSC, PTLibSmartCardPCSC) then
  begin
    PTLibSmartCardPCSC.ConnectCard(PTLibSmartCardPCSC.GetReaderList[0]);

    PTLibSmartCardPCSC.CheckStateASync(
      procedure(const State: TSmartCardReaderStates; const Data: TBytes)
      var
        CardData: TCardData;
      begin
        if not (csDestroying in ComponentState) then
        begin
          lblSmartCardStatus.Caption := PTLibSmartCardPCSC.GetReaderStateDescription;

          CardData := PTLibSmartCardPCSC.CardData;

          lblSmartCardUID.Caption := CardData.UID;
          lblSmartCardType.Caption := CardData.CardType;
          lblSmartCardProtocol.Caption := CardData.Protocol;
          lblSmartCardATR.Caption := BytesToOrdVals(CardData.ATR, True);
        end;
      end,
      procedure(e: Exception)
      begin
        if not (csDestroying in ComponentState) then
        begin
          memSmartCardErrors.Lines.Add(e.Message);
        end;
      end
    );
  end;
end;

procedure TfrmDemoMain.btnStartClick(Sender: TObject);
begin
  StartQueues(True);
end;

procedure TfrmDemoMain.btnStopClick(Sender: TObject);
begin
  StartQueues(False);
end;

procedure TfrmDemoMain.Button10Click(Sender: TObject);
begin
  PTLibPageControl1.ActivePageIndex := 2;
end;

procedure TfrmDemoMain.Button11Click(Sender: TObject);
begin
  vtLog.Log(
    'This is a test',
    TLogSeverity.LogSeverityWarning);
end;

procedure TfrmDemoMain.Button1Click(Sender: TObject);
var
  Latitude, Longitude: Double;
begin
  NewMapQuestAPI(edtMapQuestAPIKey.Text).GeoCode(edtGeoAddress.Text, Latitude, Longitude);

  edtGeoLatRes.Text := FloatToStrUseDot(Latitude);
  edtGeoLngRes.Text := FloatToStrUseDot(Longitude);

  edtGeoLat.Text := FloatToStrUseDot(Latitude);
  edtGeoLng.Text := FloatToStrUseDot(Longitude);
end;

procedure TfrmDemoMain.Button2Click(Sender: TObject);
var
  Address: String;
begin
  NewMapQuestAPI(edtMapQuestAPIKey.Text).ReverseGeoCode(
    StrToFloatDefUseDot(edtGeoLat.Text, 0),
    StrToFloatDefUseDot(edtGeoLng.Text, 0),
    Address);

  edtGeoAddressRes.Text := Address;
end;

procedure TfrmDemoMain.Button3Click(Sender: TObject);
var
  Elv: Integer;
begin
  NewMapQuestAPI(edtMapQuestAPIKey.Text).Elevation(
    StrToFloatDefUseDot(edtGeoLat.Text, 0),
    StrToFloatDefUseDot(edtGeoLng.Text, 0),
    Elv);

  edtGeoElevation.Text := Elv.ToString;
end;

procedure TfrmDemoMain.Button7Click(Sender: TObject);
begin
  PTLibPageControl1.ActivePageIndex := 0;
end;

procedure TfrmDemoMain.Button8Click(Sender: TObject);
begin
  PTLibPageControl1.ActivePageIndex := 1;
end;

procedure TfrmDemoMain.StartQueues(const DoStart: Boolean);
begin
  btnStart.Enabled := not DoStart;
  btnStop.Enabled := DoStart;

  PTLibMultiPriorityStringDataQueues1.Active := DoStart;
  tmrQueuesSend.Enabled := DoStart;
  tmrCheckSend.Enabled := DoStart;

  UpdateQueues;
end;

procedure TfrmDemoMain.chkEnableLogReceiverClick(Sender: TObject);
begin
  //PTLibLogReceiver1.Active := chkEnableLogReceiver.Checked;
end;

procedure TfrmDemoMain.chkLogTransmitterEnabledClick(Sender: TObject);
begin
  //tmrLog.Enabled := chkLogTransmitterEnabled.Checked;
  //PTLibLogTransmitter1.Active := chkLogTransmitterEnabled.Checked;
end;

procedure TfrmDemoMain.CommonAwesomeEditPropertyChange(Sender: TObject);
begin
  UpdateAwesomeEditProperties;
end;

procedure TfrmDemoMain.Console1CommandExecute(Sender: TCustomConsole;
  ACommand: string; var ACommandFinished: Boolean);
begin
  tmrConsoleReply.Enabled := True;

  ACommandFinished := False;
end;

procedure TfrmDemoMain.Console1GetPrompt(Sender: TCustomConsole; var APrompt,
  ADefaultText: string; var ADefaultCaretPos: Integer);
begin
  APrompt := '>';
end;

procedure TfrmDemoMain.FormCreate(Sender: TObject);
begin
  FSendingList := TList<IPriorityQueueItem>.Create;

  (*FSmartCard := NewSmartCardSpringCardSerial(
    'COM1',
    nil,
    True);*)

  UpdateAwesomeEditProperties;

  Console1.Boot;
end;

procedure TfrmDemoMain.UpdateAwesomeEditProperties;
begin
end;

procedure TfrmDemoMain.hisisatest1Click(Sender: TObject);
begin
  SearchEdit1.SearchDescription := (Sender as TMenuItem).Caption;
end;

procedure TfrmDemoMain.PTLibLog1Log(Sender: TObject; const LogEntry: ILogEntry);
begin
  RichEdit1.Lines.Add(
    DateTimeToCommonDateTimeStr(LogEntry.TimeStampUTC) +
    #09 +
    LogEntry.LogText);
end;

procedure TfrmDemoMain.PTLibLog2Log(Sender: TObject; const LogEntry: ILogEntry);
begin
  RichEdit2.Lines.Add(
    DateTimeToCommonDateTimeStr(LogEntry.TimeStampUTC) +
    #09 +
    LogEntry.LogText);
end;

procedure TfrmDemoMain.PTLibPriorityQueues1ProcessItem(Sender: TObject;
  const QueueItem: IPriorityQueueItem; out MessageSent: Boolean);
begin
  MessageSent := chkConnected.Checked;

  if MessageSent then
  begin
    FSendingList.Add(QueueItem);
  end;
end;

procedure TfrmDemoMain.SpinButton1DownClick(Sender: TObject);
begin
  SearchEdit1.Margins.Top := SearchEdit1.Margins.Top + 1;
  SearchEdit1.Margins.Bottom := SearchEdit1.Margins.Bottom + 1;
end;

procedure TfrmDemoMain.SpinButton1UpClick(Sender: TObject);
begin
  SearchEdit1.Margins.Top := SearchEdit1.Margins.Top - 1;
  SearchEdit1.Margins.Bottom := SearchEdit1.Margins.Bottom - 1;
end;

procedure TfrmDemoMain.tabQueuesResize(Sender: TObject);
begin
  pnlLowPriorityQueue.Width := tabQueues.Width div 3;
  pnlMediumPriorityQueue.Width := tabQueues.Width div 3;
end;

procedure TfrmDemoMain.tmrCheckSendTimer(Sender: TObject);
var
  i: Integer;
  MaxCount: Integer;
  PriorityQueueItem: IPriorityQueueItem;
begin
  MaxCount := pred(Random(FSendingList.Count));

  for i := 0 to MaxCount do
  begin
    PTLibMultiPriorityStringDataQueues1.ItemProcessed(
      FSendingList[i].ID,
      (Random(100) <= edtQueueReliability.Value) and (chkConnected.Checked),
      False,
      PriorityQueueItem);
  end;

  for i := MaxCount downto 0 do
  begin
    FSendingList.Delete(i);
  end;
end;

procedure TfrmDemoMain.tmrConsoleReplyTimer(Sender: TObject);
var
  i: Integer;
begin
  tmrConsoleReply.Enabled := False;

  Console1.BeginUpdate;
  try
    for i := 0 to 40 do
    begin
      Console1.Writeln('Test line #' + i.ToString + ' with a tab ' + #09 + 'here and' + #09 + 'here!');
    end;
  finally
    Console1.EndUpdate;
  end;

  Console1.EndExternalOutput;
end;

procedure TfrmDemoMain.tmrLogTimer(Sender: TObject);
begin
  //PTLibLog1.Log('Log Test', '', 1, nowUTC);
end;

procedure TfrmDemoMain.tmrQueuesSendTimer(Sender: TObject);
begin
  PTLibMultiPriorityStringDataQueues1.NewQueueItem(TPriorityQueuePriority.sqpLow, 'Test Low - ' + TThread.GetTickCount.ToString);
  PTLibMultiPriorityStringDataQueues1.NewQueueItem(TPriorityQueuePriority.sqpMedium, 'Test Medium - ' + TThread.GetTickCount.ToString);
  PTLibMultiPriorityStringDataQueues1.NewQueueItem(TPriorityQueuePriority.sqpHigh, 'Test High - ' + TThread.GetTickCount.ToString);

  UpdateQueues;
end;

procedure TfrmDemoMain.UpdateQueues;

  procedure UpdateQueue(const Queue: IList<IPriorityQueueItem>; const Lines: TStrings);
  var
    i: Integer;
    Status: String;
    PriorityQueueDataItem: IPriorityQueueDataItem;
  begin
    Lines.BeginUpdate;
    try
      Lines.Clear;

      for i := 0 to pred(Queue.Count) do
      begin
        if Supports(Queue[i], IPriorityQueueDataItem, PriorityQueueDataItem) then
        begin
          case PriorityQueueDataItem.Status of
            sqmQueued: Status := 'Queued';
            sqmReQueued: Status := 'Re-Queued';
            sqmSending: Status := 'Sending';
          end;

          Lines.Add(PriorityQueueDataItem.ID  + ' | ' + Status + ' | ' +  PriorityQueueDataItem.Data+ ' | Retry: ' + PriorityQueueDataItem.Retry.ToString);
        end;
      end;
    finally
      Lines.EndUpdate;
    end;
  end;

begin
  if PTLibMultiPriorityStringDataQueues1.Active then
  begin
    UpdateQueue(PTLibMultiPriorityStringDataQueues1.GetQueueItems(QueueLowPriority), memLowPriorityQueue.Lines);
    UpdateQueue(PTLibMultiPriorityStringDataQueues1.GetQueueItems(QueueMediumPriority), memMediumPriorityQueue.Lines);
    UpdateQueue(PTLibMultiPriorityStringDataQueues1.GetQueueItems(QueueHighPriority), memHighPriorityQueue.Lines);
  end;
end;

end.
