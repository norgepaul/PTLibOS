unit PTLib.Demo.FMX.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Actions,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ActnList, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Layouts, FMX.TabControl, FMX.Edit, FMX.ListBox, FMX.ComboEdit, FMX.Memo.Types,
  FMX.Objects,

  PTLib.Common.Strings,
  PTLib.Common.StringFilter,
  PTLib.Common.Dates,
  PTLib.Common.Log,
  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.SerialPort,
  PTLib.Common.SmartCard,
  PTLib.Common.SmartCard.Serial,
  PTLib.Common.SmartCard.Types;

type
  TfrmMain = class(TForm)
    TabControl1: TTabControl;
    tabSmartCard: TTabItem;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    btnSmartCardEnable: TButton;
    btnSmartCardDisable: TButton;
    ActionList1: TActionList;
    memSmartCardErrors: TMemo;
    Layout8: TLayout;
    Label9: TLabel;
    lblSmartCardUID: TLabel;
    actSmartCardEnable: TAction;
    actSmartCardDisable: TAction;
    cbSmartCardType: TComboBox;
    layPCSCSmartCard: TLayout;
    Layout4: TLayout;
    Label1: TLabel;
    lblSmartCardStatus: TLabel;
    Layout5: TLayout;
    lblSmartCardATR: TLabel;
    Layout6: TLayout;
    Label5: TLabel;
    lblSmartCardProtocol: TLabel;
    Layout7: TLayout;
    Label7: TLabel;
    lblSmartCardType: TLabel;
    Label3: TLabel;
    laySmartCard: TLayout;
    memSmartCardInfo: TMemo;
    tmrUpdate: TTimer;
    PTLibLog1: TPTLibLog;
    Layout9: TLayout;
    Splitter1: TSplitter;
    memLog: TMemo;
    edtSmartCardCOMPort: TComboEdit;
    chkReverseUID: TCheckBox;
    procedure actSmartCardEnableExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actSmartCardDisableExecute(Sender: TObject);
    procedure cbSmartCardTypeChange(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure PTLibLog1Log(Sender: TObject; const LogEntry: ILogEntry);
    procedure chkReverseUIDChange(Sender: TObject);
  private
    FSmartCard: IPTLibSmartCard;
    procedure CardUIDChanged(const CardUID: String);
    procedure UpdateControls;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actSmartCardEnable.Enabled := (FSmartCard = nil) or (not FSmartCard.Active);
  actSmartCardDisable.Enabled := not actSmartCardEnable.Enabled;
  edtSmartCardCOMPort.Enabled := actSmartCardEnable.Enabled;
  cbSmartCardType.Enabled := actSmartCardEnable.Enabled;

  Handled := True;
end;

procedure TfrmMain.actSmartCardDisableExecute(Sender: TObject);
begin
  lblSmartCardUID.Text := '';
  lblSmartCardStatus.Text := '';
  lblSmartCardType.Text := '';
  lblSmartCardProtocol.Text := '';
  lblSmartCardATR.Text := '';
  memSmartCardInfo.Text := '';

  FSmartCard.Active := False;
  //FSmartCard := nil;
  laySmartCard.Visible := False;
end;

procedure TfrmMain.CardUIDChanged(const CardUID: String);
var
  CardData: TCardData;
  PTLibSmartCardPCSC: IPTLibSmartCardPCSC;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      lblSmartCardUID.Text := CardUID;

      if Supports(FSmartCard, IPTLibSmartCardPCSC, PTLibSmartCardPCSC) then
      begin
        CardData := PTLibSmartCardPCSC.CardData;
        lblSmartCardType.Text := CardData.CardType;
        lblSmartCardProtocol.Text := CardData.Protocol;
        lblSmartCardATR.Text := BytesToOrdVals(CardData.ATR, True);
      end;
    end
  );
end;

procedure TfrmMain.cbSmartCardTypeChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmMain.chkReverseUIDChange(Sender: TObject);
begin
  if FSmartCard <> nil then
  begin
    FSmartCard.ReverseUID := chkReverseUID.IsChecked;
  end;
end;

procedure TfrmMain.UpdateControls;
begin
  edtSmartCardCOMPort.Visible := cbSmartCardType.ItemIndex <> 0;
end;

procedure TfrmMain.actSmartCardEnableExecute(Sender: TObject);
begin
  case cbSmartCardType.ItemIndex of
    0: FSmartCard := NewSmartCardPCSC(CardUIDChanged);
    1:
      begin
        FSmartCard := NewSmartCardSpringCardSerial(edtSmartCardCOMPort.Text, CardUIDChanged, True);
      end;
    2:
      begin
        FSmartCard := NewSmartCardGemProxSerial(edtSmartCardCOMPort.Text, CardUIDChanged, True);
      end;
  end;

  try
    FSmartCard.ReverseUID := chkReverseUID.IsChecked;
    FSmartCard.Active := True;
    laySmartCard.Visible := True;
    layPCSCSmartCard.Visible := cbSmartCardType.ItemIndex = 0;
  except
    on e: Exception do
    begin
      FSmartCard := nil;

      raise;
    end;
  end;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;

  TGLobalLog.OnLog :=
    procedure(LogEntry: ILogEntry)
    begin
      PTLibLog1.Log(LogEntry);
    end;

  TBaseSerialPortConnection.GetSerialPortNames(edtSmartCardCOMPort.Items, [], True);

  UpdateControls;
end;

procedure TfrmMain.PTLibLog1Log(Sender: TObject; const LogEntry: ILogEntry);
begin
  memLog.Lines.Add(DateTimeToStr(UTCToLocal(LogEntry.TimeStampUTC)) + #09 + LogEntry.LogTextWithParams);
end;

procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
begin
  if FSmartCard = nil then
  begin
    memSmartCardInfo.Lines.Clear;
  end
  else
  begin
    memSmartCardInfo.Text := FSmartCard.GetInformation.Lines.Text;
  end;
end;

end.
