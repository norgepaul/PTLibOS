program PTLibDemoVCL;

uses
  Vcl.Forms,
  PTLib.VCL.SearchEdit in '..\Lib\PTLib.VCL.SearchEdit.pas',
  PTLib.VCL.AwesomeEdit in '..\Lib\PTLib.VCL.AwesomeEdit.pas',
  PTLib.VCL.ProportionalSplitter in '..\Lib\PTLib.VCL.ProportionalSplitter.pas',
  PTLib.VCL.HTMLLinkLabel.Tree in '..\Lib\PTLib.VCL.HTMLLinkLabel.Tree.pas',
  PTLib.VCL.HTMLLinkLabel.Tools in '..\Lib\PTLib.VCL.HTMLLinkLabel.Tools.pas',
  PTLib.VCL.HTMLLinkLabel.TextHandler in '..\Lib\PTLib.VCL.HTMLLinkLabel.TextHandler.pas',
  PTLib.VCL.HTMLLinkLabel.Renderer in '..\Lib\PTLib.VCL.HTMLLinkLabel.Renderer.pas',
  PTLib.VCL.HTMLLinkLabel in '..\Lib\PTLib.VCL.HTMLLinkLabel.pas',
  PTLib.VCL.HTMLLinkLabel.Parser in '..\Lib\PTLib.VCL.HTMLLinkLabel.Parser.pas',
  PTLib.VCL.GDIPlus in '..\Lib\PTLib.VCL.GDIPlus.pas',
  PTLib.VCL.FileEdit in '..\Lib\PTLib.VCL.FileEdit.pas',
  PTLib.VCL.AwesomeEdit.Classes in '..\Lib\PTLib.VCL.AwesomeEdit.Classes.pas',
  PTLib.VCL.AwesomeEdit.Interfaces in '..\Lib\PTLib.VCL.AwesomeEdit.Interfaces.pas',
  PTLib.VCL.GDIPlus.Interfaces in '..\Lib\PTLib.VCL.GDIPlus.Interfaces.pas',
  PTLib.VCL.AwesomeEdit.Types in '..\Lib\PTLib.VCL.AwesomeEdit.Types.pas',
  frmDemoMainU in 'frmDemoMainU.pas' {frmDemoMain};

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDemoMain, frmDemoMain);
  Application.Run;
end.
