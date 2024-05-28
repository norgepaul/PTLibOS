unit PTLib.VCL.Clipboard;

interface

uses
  System.Classes, System.SysUtils,

  Vcl.Clipbrd,

  PTLib.Common.Classes;

procedure SaveTextToClipboard(const Value: String);

implementation

resourcestring
  StrAnotherApplicationCurrentlyUsingClipboard = 'Another application is currently using the clipboard. Please try again in a few moments.';

procedure SaveTextToClipboard(const Value: String);
var
  RetryCount: Integer;
begin
  RetryCount := 0;

  while True do
  try
    Clipboard.Open;
    try
      // Store the text in the clipboard
      Clipboard.AsText := Value;
    finally
      Clipboard.Close;
    end;

    Break;
  except
    on Exception do
    begin
      Inc(RetryCount);

      if RetryCount < 3 then
      begin
        Sleep(RetryCount * 100)
      end
      else
      begin
        raise EPTLibClipboardError.Create('Another application is currently using the clipboard. Please try again in a few moments.');
      end;
    end;
  end;
end;

end.



