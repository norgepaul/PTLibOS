unit PTLib.MARS.ReadersAndWriters.XSuperObject;

(*
  Add MARS_XSUPEROBJECT to your global defines to enable Mars XSuperObject
  JSON Readers and Writes
*)

interface

uses
  Classes, SysUtils, Rtti,

  MARS.Core.Attributes, MARS.Core.Declarations, MARS.Core.MediaType,
  MARS.Core.MessageBodyWriter, MARS.Core.MessageBodyReader,
  MARS.Core.Activation.Interfaces;

type
  [Produces(TMediaType.APPLICATION_JSON)]
  TXSuperObjectWriter = class(TInterfacedObject, IMessageBodyWriter)
  public
    procedure WriteTo(const AValue: TValue; const AMediaType: TMediaType;
      AOutputStream: TStream; const AActivation: IMARSActivation);
  end;

  [Consumes(TMediaType.APPLICATION_JSON)]
  TXSuperObjectReader = class(TInterfacedObject, IMessageBodyReader)
  public
    function ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject;
      const AMediaType: TMediaType; const AActivation: IMARSActivation): TValue;
  end;


implementation

uses
  {$IFDEF MARS_XSUPEROBJECT}
  XSuperObject,
  {$ENDIF}
  MARS.Core.Utils,
  MARS.Rtti.Utils;

{ TJsonDataObjectsWriter }

procedure TXSuperObjectWriter.WriteTo(const AValue: TValue; const AMediaType: TMediaType;
  AOutputStream: TStream; const AActivation: IMARSActivation);
{$IFDEF MARS_XSUPEROBJECT}
var
  X: TSuperObject;
  LEncoding: TEncoding;
{$ENDIF}
begin
  {$IFDEF MARS_XSUPEROBJECT}
  TMARSMessageBodyReader.GetDesiredEncoding(AActivation, LEncoding);

  X := AValue.AsObject as TSuperObject;

  StringToStream(AOutputStream, X.AsJSON, LEncoding);
  {$ENDIF}
end;

{ TXSuperObjectReader }

function TXSuperObjectReader.ReadFrom(const AInputData: TBytes; const ADestination: TRttiObject; const AMediaType: TMediaType;
  const AActivation: IMARSActivation): TValue;
{$IFDEF MARS_XSUPEROBJECT}
var
  X: TSuperObject;
{$ENDIF}
begin
  {$IFDEF MARS_XSUPEROBJECT}
  Result := TValue.Empty;

  X := TSuperObject.Create(AActivation.Request.Content);

  Result := X;
  {$ENDIF}
end;

initialization
  {$IFDEF MARS_XSUPEROBJECT}
  TMARSMessageBodyReaderRegistry.Instance.RegisterReader<TSuperObject>(TXSuperObjectReader);
  TMARSMessageBodyRegistry.Instance.RegisterWriter<TSuperObject>(TXSuperObjectWriter);
  {$ENDIF}

end.
