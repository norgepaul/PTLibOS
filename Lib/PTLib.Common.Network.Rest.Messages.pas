{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Network.Rest.Messages                       }
{                                                                    }
{           Copyright (c) 1998-2016 Easy-IP AS.                      }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF EASY-IP AS. THE REGISTERED DEVELOPER                  }
{   LICENSED TO DISTRIBUTE THE CODE HEREIN AND ANY ACCOMPANYING      }
{   VCL OR FMX CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.       }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM EASY-IP AS.                                  }
{                                                                    }
{********************************************************************}

unit PTLib.Common.Network.Rest.Messages;

interface

(*
  About REST messages
  ===================

  Our REST architecture has several layers of operation.
  At the bottom we have Indy, which provides the fundamental means of communication
  between a client and a server.

  On top of that we have a lightweight REST architecture which essentially
  is based around the concept of a "Schema".

  A schema is simply a collection of objects which describes the services we
  provide and also the methods each service exposes.

  Messages
  ========

  All of this is great, but what exactly is being passed from A to B?
  Unlike SOAP, REST has no standard when it comes to content. It does not
  tell us how a response should be formulated, formated or encoded.

  It is up to us to provide that standard, and this is exactly what
  our message classes do.

  Cleaning it up
  ==============

  Using message-objects to formulate requests and responses is ultimately
  optional. You dont need to use it, but I strongly suggest that you do because
  TIdRestMessage takes care of all the boring stuff.

  It also makes it much easier to work with for third-party users
  (JavaScript, .net, python etc.) because all messages have the same envelope
  and can be tested and consumed the same way.

  The envelope currently looks like this:


  {
    "message-type": "TIdRestMessage",
    "message-identifier": "{93FEDB6C-5BFB-479F-B03D-139B1296D694}",
    "tag-value": "",
    "build-time": "10.10.2015 07:57:04",
    "content": {
      /* Content goes here */
    }
  }

  Message-type:
    A unique identifier for the message type. This is not a value that is
    important for the API, but a value which helps differenciate between
    similar messages.

    For instance, you may want to use the same error class to send 100
    different errors. In which case the "message-identifier" recognizes the
    class as TIdRestErrorMessage and message-type defines what type of
    error it contains (syntax-error, internal-error, authentication-error).


  Message-identifier:
    Unique ID representing the class. This is required because while there
    may be many error messages, there is ultimately only one error class.
    Being able to pinpoint the exact error (if you use multiple derived
    error classes) is important.


  tag-value:
    User-definable value. Typically you put a trace ID of sorts in a tag-value,
    like the same suggests this ID follows the object and will also be
    present in the reply (!) from the server.

  build-time:
    The time at which the message was composed.
    Please note the TIdRestMessageOptions set with options for time/date
    formating and the use of local vs. UTC timestamps.

  content:
    Properties exclusive to the message goes here. By overriding the methods
    Exportvalues() and ImportValues() you can read and write to the content
    section of the message envelope.

*)


uses
  System.Sysutils, System.Classes, Generics.Collections, System.JSON,
  System.SyncObjs, Soap.XSBuiltIns,

  IdHTTP, IdContext, IdTCPConnection, IdMultipartFormData,

  PTLib.Common.LockedTypes,
  PTLib.Common.Dates
  ;

  (*
    Notes on message-identifiers:
    =============================
    Why is CNT_MSGID_INVALIDMESSAGE, which is the ID for the TIdRestMessage
    root ancestor class defined as "invalid" ?

    As mentioned this is the identifier for the root class, which is not
    a class you use directly; only inherit from.
    As such, should the ID of this class ever be encountered we know that
    something is wrong with the message, because all messages derived
    from it *MUST* override GetMessageIdentifier() and provide their own,
    unique identifier.
  *)

type

  // Forward declarations
  TIdRestMessage        = class;
  TIdRestErrorMessage   = class;
  TIdRestMessageClass   = class of TIdRestMessage;

  IRestMessage = interface
    ['{889F63BF-C5C3-4D92-9C18-965C95DFCD4A}']
    procedure ExportValues(out Buffer: TJSONValue);
    procedure ImportValues(const Buffer: TJSONValue);
  end;

  TIdRestMessage = Class(TInterfacedObject,
                         IRestMessage)
  strict private
    FKind:      String;
  strict protected
    function GetMessageType: String;virtual;
    procedure ExportValues(out Buffer: TJSONValue); virtual; abstract;
    procedure ImportValues(const Buffer: TJSONValue); virtual; abstract;
    procedure AddOrSet(const Obj: TJSONObject; Name, Value: String);
  public
    constructor Create; virtual;

    function Serialize: string;
    function Materialize(JSONData: String):String;  //Returns TagValue
    procedure SaveToStream(const Stream: TStream);virtual;
    procedure LoadFromStream(const Stream: TStream);virtual;
  public
    property MessageType: String read FKind write FKind;
  end;

  TIdRestErrorMessage = Class(TIdRestMessage)
  strict private
    FError: String;
    FExceptionClass: String;
  strict protected
    function GetMessageType: String;override;
  strict protected
    procedure ExportValues(out Buffer: TJSONValue); override;
    procedure ImportValues(const Buffer: TJSONValue); override;
  public
    constructor CreateWithError(const AException: Exception);
    constructor CreateEx(const ExceptionClass, ExceptionMessage: String);

    property ExceptionClass: String read FExceptionClass write FExceptionClass;
    property ErrorText:String read FError write FError;
  end;

  TIdRestSuccessMessage = Class(TIdRestMessage)
  strict protected
    function GetMessageType: String;override;
    procedure ExportValues(out Buffer: TJSONValue); override;
    procedure ImportValues(const Buffer: TJSONValue); override;
  public
    function ToText: String; virtual; abstract;
  end;

implementation

{ TIdRestSuccessMessage }

function TIdRestSuccessMessage.GetMessageType: String;
begin
  result := 'success';
end;

procedure TIdRestSuccessMessage.ExportValues(out Buffer: TJSONValue);
begin
  raise Exception.Create('TIdRestSuccessMessage.ExportValues must be overriden');
end;

procedure TIdRestSuccessMessage.ImportValues(const Buffer: TJSONValue);
begin
end;

{ TIdRestErrorMessage }

constructor TIdRestErrorMessage.CreateEx(const ExceptionClass, ExceptionMessage: String);
begin
  inherited Create;

  FExceptionClass := ExceptionClass;
  FError := ExceptionMessage;
end;

constructor TIdRestErrorMessage.CreateWithError(const AException: Exception);
begin
  inherited Create;

  if AException <> nil then
  Begin
    FExceptionClass := AException.ClassName;
    FError := AException.Message;
  end
  else
  begin
    raise Exception.Create('Failed to construct error message, exception instance was NIL error');
  end;
end;

function TIdRestErrorMessage.GetMessageType:String;
begin
  result := 'error';
end;

procedure TIdRestErrorMessage.ExportValues(out Buffer: TJSONValue);
begin
  Buffer := TJSONObject.Create;

  AddOrSet(TJSONObject(Buffer), 'error-class', FExceptionClass);
  AddOrSet(TJSONObject(Buffer), 'error-text', ErrorText);
end;

procedure TIdRestErrorMessage.ImportValues(const Buffer: TJSONValue);
begin
  if not (Buffer as TJSONObject).TryGetValue<string>('error-text', FError) then
    SetLength(FError, 0);

  if not (Buffer as TJSONObject).TryGetValue<string>('error-class', FExceptionClass) then
    SetLength(FExceptionClass, 0);
end;

{ TIdRestMessage }

constructor TIdRestMessage.Create;
begin
  inherited;
  FKind := GetMessageType;
end;

procedure TIdRestMessage.SaveToStream(const Stream: TStream);
var
  mWriter:  TWriter;
begin
  mWriter := TWriter.Create(Stream, 1024);
  try
    mWriter.WriteString(Serialize);
  finally
    mWriter.FlushBuffer;
    mWriter.Free;
  end;
end;

procedure TIdRestMessage.LoadFromStream(const Stream: TStream);
var
  mReader:  TReader;
begin
  mReader:=TReader.Create(Stream,1024);
  try
    Materialize(mReader.ReadString);
  finally
    mReader.Free;
  end;
end;

function TIdRestMessage.GetMessageType:String;
begin
  (* Use the class-name as message-identifier *)
  result := ClassName;
end;

function TIdRestMessage.Materialize(JSONData: String): String;
var
  mContent: TJSONValue;
  FBuffer:  TJSONObject;
begin
  JSONData:=JSONData.Trim();

  if JSONData.Length > 0 then
  Begin
    FBuffer:=TJSONObject.ParseJSONValue(JSONData) as TJSONObject;
    if FBuffer <> nil then
    begin
      try
        FBuffer.TryGetValue<string>('message-type',FKind);

        if FKind<>GetMessageType then
        Raise Exception.Create('JSON message deserialization error, incompatible message-type identifier error');

        // Read message content
        if FBuffer.TryGetValue<TJSONValue>('content', mContent) then
        begin
          try
            ImportValues(mContent);
          except
            on e: exception do
            begin
              Raise;
            end;
          end;
        end;

      finally
        FreeAndNIL(FBuffer);
      end;
    end;

  end;
end;

procedure TIdRestMessage.AddOrSet(const Obj: TJSONObject; Name, Value: String);
begin
  if Obj <> nil then
  begin
    Name := Name.Trim();
    if Name.Length>0 then
    Begin
      Value := Value.Trim;

      if Obj.Values[Name] <> nil then
      Obj.RemovePair(Name);

      Obj.addPair(TJSONPair.Create(Name,Value));
    end;
  end;
end;

function TIdRestMessage.Serialize: String;
var
  mContent: TJSONValue;
  FBuffer:      TJSONObject;
begin
  FBuffer := TJSONObject.Create;
  try
    // Write envelope
    AddOrSet(FBuffer,'message-type', FKind);

    // Export to content
    ExportValues(mContent);
    FBuffer.AddPair('content', mContent);

    // Return JSON code
    Result := FBuffer.ToJSON;
  finally
    FreeAndNil(FBuffer);
  end;
end;

end.
