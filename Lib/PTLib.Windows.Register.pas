unit PTLib.Windows.Register;

interface

uses
  SysUtils, DesignIntf, DesignEditors,
  Classes,

  PTLib.Windows.Application.Instance;

procedure Register;

implementation

const
  ComponentsTab = 'PTLib Windows';

procedure Register;
begin
  RegisterComponents(ComponentsTab,
    [TPTLibAppInstances]);
end;

end.
