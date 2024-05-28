unit PTLib.FMX.Register;

interface

uses
  SysUtils, DesignIntf, DesignEditors, Classes,

  PTLib.FMX.LogViewer,
  PTLib.FMX.InformationGrid;

procedure Register;

implementation

const
  ComponentTab = 'PTLib FMX';

procedure Register;
begin
  // Register components
  RegisterComponents(ComponentTab, [
    TInformationGrid,
    TframeLogViewer]);
end;

end.
