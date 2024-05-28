unit PTLib.XSuperObject.Interfaces;

interface

uses
  System.Classes,

  XSuperObject,

  PTLib.Common.Interfaces;

type
  IKeyValueStoreJson = interface(IKeyValueStore)
    ['{3E81A7A6-4697-4910-A360-E2CA2992DDCB}']
    function GetUpdated: Boolean;
    function Load(const Filename: String): Boolean; overload;
    procedure Load(const AStream: TStream); overload;
    procedure Save(const Filename: String; const FormatJson: Boolean = False); overload;
    procedure Save(const AStream: TStream; const FormatJson: Boolean = False); overload;

    function CloneJson: ISuperObject;
    property Updated: Boolean read GetUpdated;
  end;

implementation

end.
