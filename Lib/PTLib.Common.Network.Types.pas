unit PTLib.Common.Network.Types;

interface

uses
  Web.HTTPApp;

const
  HTTPMethodTypeDescriptions: Array[TMethodType] of String = (
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
