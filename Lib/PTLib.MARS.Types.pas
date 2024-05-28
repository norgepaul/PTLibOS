unit PTLib.MARS.Types;

interface

uses
  System.Classes, System.SysUtils, Web.HTTPApp,

  MARS.Client.Resource,

  PTLib.MARS.ServerContainer.Application;

type
  TShareBikeBridgeServiceRESTApplicationClass = class of TMarsServerContainerServiceApplication;

  TRequestID = String;

const
  NullRequestID = '';

const
  MethodTypeDescriptions: Array[TMethodType] of String = (
   'ANY',
   'GET',
   'PUT',
   'POST',
   'HEAD',
   'DELETE',
   'PATCH'
  );

implementation

end.
