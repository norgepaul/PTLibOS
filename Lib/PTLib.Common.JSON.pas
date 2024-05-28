{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.JSON                                        }
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

unit PTLib.Common.JSON;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.Generics.Collections,
  System.JSON, System.JSON.Types, System.JSON.Writers, System.JSON.Builders,

  PTLib.Common.Classes,
  PTLib.Common.Utils;

type
  IJSONBuilder = interface
    ['{22BC62AC-0156-4326-B64C-7543CA34344F}']
    function Build: TJSONObjectBuilder;
    function ToJSONString: String;
    function ToJSON: TJSONObject;
  end;

  TJSONBuilder = class(TInterfacedObject, IJSONBuilder)
  strict private
    FStringWriter: TStringWriter;
    FJSONTextWriter: TJsonTextWriter;
    FJSONObjectBuilder: TJSONObjectBuilder;
  private
    function Build: TJSONObjectBuilder;
    function ToJSONString: String;
    function ToJSON: TJSONObject;
  public
    constructor Create(const JsonFormatting: TJsonFormatting = TJsonFormatting.None);
    destructor Destroy; override;
  end;

function FindJSONValue(const AJSON: TJSONValue; const APath: string): TJsonValue;
function NewJSONBuilder(const JsonFormatting: TJsonFormatting = TJsonFormatting.None): IJSONBuilder;
function NewHTTPJSONObject(const URL: String): TJSONObject;

implementation

// Traverse a JSONObject or TJSONArray and find the TJSONValue identified by a path string
function FindJSONValue(const AJSON: TJSONValue; const APath: string): TJsonValue;
var
  LCurrentValue: TJSONValue;
  LParser: TJSONPathParser;
  LError: Boolean;
begin
  LParser := TJSONPathParser.Create(APath);
  try
    LCurrentValue := AJSON;
    LError := False;
    while (not LParser.IsEof) and (not LError) do
    begin
      case LParser.NextToken of
        TJSONPathParser.TToken.Name:
        begin
          if LCurrentValue is TJSONObject then
          begin
            LCurrentValue := TJSONObject(LCurrentValue).Values[LParser.TokenName];
            if LCurrentValue = nil then
              LError := True;
          end
          else
            LError := True;
        end;
        TJSONPathParser.TToken.ArrayIndex:
        begin
          if LCurrentValue is TJSONArray then
            if LParser.TokenArrayIndex < TJSONArray(LCurrentValue).Count then
              LCurrentValue := TJSONArray(LCurrentValue).Items[LParser.TokenArrayIndex]
            else
              LError := True
          else
            LError := True
        end;
        TJSONPathParser.TToken.Error:
          LError := True;
      else
        Assert(LParser.Token = TJSONPathParser.TToken.Eof); // case statement is not complete
      end;
    end;

    if LParser.IsEof and not LError then
      Result := LCurrentValue
    else
      Result := nil;

  finally
    {$if CompilerVersion < 33}
    LParser.Free;
    {$ifend}
  end;
end;

function NewHTTPJSONObject(const URL: String): TJSONObject;
var
  HTTPClient: THTTPClient;
  StringStream: TStringStream;
begin
  HTTPClient := THTTPClient.Create;
  StringStream := TStringStream.Create;
  try
    HTTPClient.Get(
      URL,
      StringStream);

    Result := TJSONObject.ParseJSONValue(StringStream.DataString) as TJSONObject;

    if Result = nil then
    begin
      raise EPTLibInvalidJSONObject.Create('Invalid JSON Object');
    end;
  finally
    FreeAndNil(HTTPClient);
    FreeAndNil(StringStream);
  end;
end;

function NewJSONBuilder(const JsonFormatting: TJsonFormatting = TJsonFormatting.None): IJSONBuilder;
begin
  Result := TJSONBuilder.Create(JsonFormatting);
end;

{ TJSONBuilder }

function TJSONBuilder.Build: TJSONObjectBuilder;
begin
  Result := FJSONObjectBuilder;
end;

constructor TJSONBuilder.Create(const JsonFormatting: TJsonFormatting = TJsonFormatting.None);
begin
  FStringWriter := TStringWriter.Create();
  FJSONTextWriter := TJsonTextWriter.Create(FStringWriter);
  FJSONObjectBuilder := TJSONObjectBuilder.Create(FJSONTextWriter);

  FJSONTextWriter.Formatting := JsonFormatting;
end;

destructor TJSONBuilder.Destroy;
begin
  FreeAndNil(FStringWriter);
  FreeAndNil(FJSONTextWriter);
  FreeAndNil(FJSONObjectBuilder);

  inherited;
end;

function TJSONBuilder.ToJSON: TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(ToJSONString) as TJSONObject;
end;

function TJSONBuilder.ToJSONString: String;
begin
  Result := FStringWriter.ToString;
end;

end.

