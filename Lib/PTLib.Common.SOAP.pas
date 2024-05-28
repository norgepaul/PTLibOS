{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.SOAP                                        }
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

unit PTLib.Common.SOAP;

// Common web service SOAP functions

interface

uses
  SysUtils, Classes, DateUtils, System.Types, System.NetEncoding,

  Soap.XSBuiltIns, Soap.EncdDecd,

  PTLib.Common.Types;

type
  TBase64EncodingEx = class(TBase64Encoding)
  public
    function Decode(const Input, Output: TStream): Integer;
  end;

function FormatNorwegianPersonalNumber(const Value: String): String;
function SOAPDateToDate(const Value: String): TDateTime;
function DateToSOAPDate(const Value: TDate): String;
function GetXSDateTime(const Value: TXSDateTime): TDateTime;
function GetXSDate(const Value: TXSDate): TDate;
procedure ByteDynArrayToStream(const Bytes: TByteDynArray; const Stream: TStream);

implementation

uses
  PTLib.Common.Strings;

procedure ByteDynArrayToStream(const Bytes: TByteDynArray; const Stream: TStream);
var
  BytesStream: TBytesStream;
begin
  BytesStream := TBytesStream.Create(TBytes(Bytes));
  try
    BytesStream.Position := 0;

    Stream.CopyFrom(BytesStream, BytesStream.Size);
  finally
    FreeAndNil(BytesStream);
  end;
end;

// Formats a Norwegian Personal Number in a human
// readable form
function FormatNorwegianPersonalNumber(const Value: String): String;
begin
  Result := Value;

  if Result <> '' then
    Result := copy(Value, 1,  2) + '.' +
              copy(Value, 3 , 2) + '.' +
              copy(Value, 5 , 2) + '-' +
              copy(Value, 7, 5);
end;

// Converts a SOAP date string to TDateTime
function SOAPDateToDate(const Value: String): TDateTime;
var
  Year: Word;
begin
  // Extract the Year
  Year := copy(Value, 5,  2).ToInteger;

  // If the year is greater than the current year, we're
  // probably in the 20 century!
  if Year > copy(YearOf(now).ToString, 3, 2).ToInteger then
    Year := Year + 1900
  else
    Year := Year + 2000;

  Result := EncodeDate(Year,
                       copy(Value, 3,  2).ToInteger,
                       copy(Value, 1,  2).ToInteger);
end;

// Converts TDateTime into a SOAP date string
function DateToSOAPDate(const Value: TDate): String;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);

  Result := Day.ToString.PadLeft(2, '0') +
            Month.ToString.PadLeft(2, '0') +
            copy(Year.ToString.PadLeft(4, '0'), 3, 2);
end;

function GetXSDateTime(const Value: TXSDateTime): TDateTime;
begin
  if (Value = nil) or
     (Value.NativeToXS = '') then
    Result := 0
  else
    Result := Value.AsDateTime;
end;

function GetXSDate(const Value: TXSDate): TDate;
begin
  if Value = nil then
    Result := 0
  else
    Result := Value.AsDate;
end;

{ TBase64EncodingEx }

function TBase64EncodingEx.Decode(const Input, Output: TStream): Integer;
begin
  Result := DoDecode(Input, Output);
end;

end.
