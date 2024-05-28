unit PTLib.FMX.Frame.Console;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Math,

  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Memo.Types,
  FMX.Edit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Objects, FMX.Layouts,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TOnSendCommand = procedure(Sender: TObject; const Command: String) of object;

  TframeConsole = class(TFrame)
    layResponse: TLayout;
    Rectangle1: TRectangle;
    layInput: TLayout;
    lblPrompt: TLabel;
    edtInput: TEdit;
    StyleBook1: TStyleBook;
    memResponse: TMemo;
    procedure memResponseMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure edtInputEnter(Sender: TObject);
    procedure edtInputKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure layResponseResize(Sender: TObject);
    procedure lblPromptEnter(Sender: TObject);
    procedure FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    FOnSendCommand: TOnSendCommand;

    FHistoryList: IList<String>;
    FHistoryIndex: Integer;
    FPrompt: String;
    FDisableInputWhileWaitingForResponse: Boolean;
    FWaitingForResponse: Boolean;

    procedure SetHistoryIndex(const Value: Integer);
    procedure SetLastHistoryLine(const Value: String);
    procedure SetPrompt(const Value: String);
    procedure ProcessResponseControl;
    procedure UpdateSizes;
    property CommandHistoryIndex: Integer read FHistoryIndex write SetHistoryIndex;
  protected
    procedure DoOnSendCommand(const Command: String); virtual;
  public
    constructor Create(Owner: TComponent); override;

    procedure Response(const Text: String);
    procedure Clear;
    procedure Reset;
    procedure SendCommand;
    procedure DeleteKey;
    procedure AppendCommandText(const Value: String);
  published
    property OnSendCommand: TOnSendCommand read FOnSendCommand write FOnSendCommand;

    property Prompt: String read FPrompt write SetPrompt;
    property DisableInputWhileWaitingForResponse: Boolean read FDisableInputWhileWaitingForResponse write FDisableInputWhileWaitingForResponse;
  end;

implementation

{$R *.fmx}

procedure TframeConsole.AppendCommandText(const Value: String);
begin
  edtInput.Text := edtInput.Text + Value;
end;

procedure TframeConsole.Clear;
begin
  memResponse.Lines.Text := '';
  FWaitingForResponse := False;
  ProcessResponseControl;
end;

constructor TframeConsole.Create(Owner: TComponent);
begin
  inherited;

  FDisableInputWhileWaitingForResponse := True;

  FHistoryList := TList<String>.Create;

  Reset;

  Prompt := '>';
end;

procedure TframeConsole.DoOnSendCommand(const Command: String);
begin
  if Assigned(FOnSendCommand) then
  begin
    FOnSendCommand(Self, Command);
  end;
end;

procedure TframeConsole.edtInputEnter(Sender: TObject);
begin
  memResponse.SelLength := 0;
  memResponse.SelStart := length(memREsponse.Lines.Text);
end;

procedure TframeConsole.edtInputKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkUp:
      begin
        CommandHistoryIndex := CommandHistoryIndex - 1;
      end;

    vkDown:
      begin
        CommandHistoryIndex := CommandHistoryIndex + 1;
      end;

    vkReturn:
      begin
        Key := 0;
        KeyChar := #00;

        SendCommand;
      end;
  end;
end;

procedure TframeConsole.FrameMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  edtInput.SetFocus;
end;

procedure TframeConsole.layResponseResize(Sender: TObject);
begin
  UpdateSizes;
end;

procedure TframeConsole.lblPromptEnter(Sender: TObject);
begin
  edtInput.SetFocus;
end;

procedure TframeConsole.UpdateSizes;
begin
  layInput.Margins.Bottom := Max(0, layResponse.Height - memResponse.ContentBounds.Height - edtInput.Height - 1);
end;

procedure TframeConsole.memResponseMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if memResponse.SelLength = 0 then
  begin
    edtInput.SetFocus;
  end;
end;

procedure TframeConsole.Reset;
begin
  FHistoryList.Clear;

  FHistoryIndex := -1;

  edtInput.Enabled := True;

  Clear;
end;

procedure TframeConsole.Response(const Text: String);
begin
  FWaitingForResponse := False;

  memResponse.Lines.Add(Text);

  ProcessResponseControl;

  edtInput.SetFocus;
end;

procedure TframeConsole.ProcessResponseControl;
begin
  UpdateSizes;

  if not memResponse.IsFocused then
  begin
    memResponse.SelStart := length(memResponse.Lines.Text);
    memResponse.ScrollBy(0, MaxInt);
    memResponse.Repaint;
  end;
end;

procedure TframeConsole.DeleteKey;
begin
  edtInput.Text := copy(edtInput.Text, 1, length(edtInput.Text) - 1);
end;

procedure TframeConsole.SendCommand;
var
  Command: String;
begin
  if (not FWaitingForResponse) or
     (not FDisableInputWhileWaitingForResponse) then
  begin
    Command := Trim(edtInput.Text);

    if Command <> '' then
    begin
      SetLastHistoryLine(Command);

      Response(lblPrompt.Text + Command);

      edtInput.Text := '';

      FHistoryIndex := -1;
      SetLastHistoryLine('');

      FWaitingForResponse := True;

      DoOnSendCommand(Command);
    end;
  end;
end;

procedure TframeConsole.SetHistoryIndex(const Value: Integer);
begin
  FHistoryIndex := Value;

  if FHistoryIndex < 0 then
  begin
    FHistoryIndex := 0;
  end else
  if FHistoryIndex >= FHistoryList.Count then
  begin
    FHistoryIndex := pred(FHistoryList.Count);
  end;

  if (FHistoryIndex >= 0) and (FHistoryIndex < FHistoryList.Count) then
  begin
    edtInput.Text := FHistoryList[FHistoryIndex];
    edtInput.SelStart := length(edtInput.Text);
  end;
end;

procedure TframeConsole.SetLastHistoryLine(const Value: String);
begin
  if (FHistoryList.Count = 0) or
     (FHistoryIndex = -1) then
  begin
    if (Value <> '') or
       (FHistoryList[FHistoryList.Count - 1] <> '') then
    begin
      FHistoryList.Add(Value);
    end;

    FHistoryIndex := pred(FHistoryList.Count);
  end
  else
  begin
    if (Value <> '') and (FHistoryList[FHistoryList.Count - 2] <> Value) then
    begin
      FHistoryList[FHistoryList.Count - 1] := Value;
    end;
  end;
end;


procedure TframeConsole.SetPrompt(const Value: String);
begin
  FPrompt := Value;

  lblPrompt.Text := FPrompt + ' ';;
end;

end.
