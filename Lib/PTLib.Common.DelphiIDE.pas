unit PTLib.Common.DelphiIDE;

interface

{$IFDEF MSWINDOWS}
uses
  WinAPI.Windows;
{$ENDIF}

type
  TDelphiIDE = class
    class function DelphiRunning: Boolean;
  end;

implementation

uses
  System.SysUtils;

class function TDelphiIDE.DelphiRunning: Boolean;
begin
  {$IFDEF MSWINDOWS}
    Result :=
      (FindWindow('TAppBuilder', nil) <> 0) and
      (not FindCmdLineSwitch('IgnoreIDE'));
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

end.
