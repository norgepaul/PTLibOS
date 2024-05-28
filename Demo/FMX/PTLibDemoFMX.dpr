program PTLibDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  PTLib.Demo.FMX.Main in 'PTLib.Demo.FMX.Main.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
