unit PTLib.Common.ServerContainer.Service;

interface

uses
  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TServerContainerService = class(TPTLibActiveInterfacedObject,
                                  IServerContainerService,
                                  IClassDescriptor)
  protected
    procedure DoGetServiceName(out Value: String); virtual;

    function GetServiceName: String;
    procedure GetClassDescriptor(out Value: String);
  end;

implementation

{ TServerContainerService }

procedure TServerContainerService.DoGetServiceName(out Value: String);
begin
  Value := Self.ClassName;
end;

procedure TServerContainerService.GetClassDescriptor(out Value: String);
begin
  DoGetServiceName(Value);
end;

function TServerContainerService.GetServiceName: String;
begin
  DoGetServiceName(Result);
end;

end.
