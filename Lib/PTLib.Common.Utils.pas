//{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Utils                                       }
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

unit PTLib.Common.Utils;

interface

uses
  System.Generics.Defaults,

  {$IFDEF MSWINDOWS}
  WinAPI.Windows,
  {$ELSEIF MACOS}
  Macapi.AppKit
  {$ENDIF}

  System.SysUtils, System.Variants, System.DateUtils;

type
  EPTLibByteError = class(Exception);

  TArrayUtils<T> = class
  public
    class function AppendArray(const Arr1, Arr2: TArray<T>): TArray<T>;
    class function Contains(const x : T; const anArray : array of T) : boolean;
    class function Reverse(const Values: TArray<T>): TArray<T>;
  end;

function IfThen(Condition: Boolean; TrueResult, FalseResult: Variant): Variant;
function TimeBetween(StartTime, EndTime: TDateTime): String;
function FloatToStrUseDot(Value: Double): String;
function StrToFloatDefUseDot(Value: String; Default: Double): Double;
function Int64OrNull(const Value: Int64): Variant;
function DateTimeOrNull(const Value: TDateTime): Variant;
function StringOrNull(const Value: String): Variant;
function CTRLKeyDown: Boolean;
function ShiftKeyDown: Boolean;
function SameBytes(const BytesA, BytesB: TBytes): Boolean;
function CopyBytes(const Data: TBytes; const Index: Integer; const Count: Integer): TBytes;
function BytesToEncodedASCIIBytes(const Data: TBytes): TBytes;
function EncodedASCIIBytesToBytes(const Data: TBytes): TBytes;
function RunningOnCitrix: Boolean;

implementation

function RunningOnCitrix: Boolean;
begin
  Result := GetEnvironmentVariable('SessionName').ToUpper.StartsWith('ICA')
end;


function CopyBytes(const Data: TBytes; const Index: Integer; const Count: Integer): TBytes;
var
  i: Integer;
begin
  Result := [];

  for i := Index to Index + Count - 1 do
  begin
    if i >= Length(Data) then
    begin
      break;
    end
    else
    begin
      Result := Result + [Data[i]];
    end;
  end;
end;

function BytesToEncodedASCIIBytes(const Data: TBytes): TBytes;
var
  i: Integer;
  Hex: String;
begin
  Result := [];

  for i := Low(Data) to High(Data) do
  begin
    Hex := IntToHex(Data[i], 2);

    Result :=
      Result + [
        ord(Hex[low(Hex)]),
        ord(Hex[low(Hex) + 1])
      ];
  end;
end;

function EncodedASCIIBytesToBytes(const Data: TBytes): TBytes;

var
  i: Integer;
begin
  if length(Data) mod 2 <> 0 then
  begin
    raise EPTLibByteError.Create('Data length must be divisible by 2');
  end;

  Result := [];

  i := 0;

  while i < high(Data) do
  begin
    Result := Result + [StrToInt('$' + (chr(Data[i]) + chr(Data[i + 1])))];

    Inc(i, 2);
  end;
end;

function SameBytes(const BytesA, BytesB: TBytes): Boolean;
var
  i: Integer;
begin
  Result := length(BytesA) = length(BytesB);

  if Result then
  begin
    for i := Low(BytesA) to High(BytesB) do
    begin
      if BytesA[i] <> BytesB[i] then
      begin
        Exit(False);
      end;
    end;
  end;
end;

function ShiftKeyDown: Boolean;
const
  kVK_Control = $3B;
begin
  {$IFDEF MSWINDOWS}
    Result := GetKeyState(VK_SHIFT) < 0;
  {$ELSEIF MACOS}
    Result := NSShiftKeyMask and TNSEvent.OCClass.modifierFlags = NSShiftKeyMask;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

function CTRLKeyDown: Boolean;
const
  kVK_Control = $3B;
begin
  {$IFDEF MSWINDOWS}
    Result := GetKeyState(VK_CONTROL) < 0;
  {$ELSEIF MACOS}
    Result := NSShiftKeyMask and TNSEvent.OCClass.modifierFlags = NSShiftKeyMask;
  {$ELSE}
    Result := False;
  {$ENDIF}
end;

function Int64OrNull(const Value: Int64): Variant;
begin
  if Value = 0 then
    Result := Null
  else
    Result := Value;
end;

function DateTimeOrNull(const Value: TDateTime): Variant;
begin
  if Value = 0 then
    Result := Null
  else
    Result := Value;
end;

function StringOrNull(const Value: String): Variant;
begin
  if Value = '' then
    Result := Null
  else
    Result := Value;
end;

function FloatToStrUseDot(Value: Double): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';

  Result := FloatToStr(Value, FormatSettings);
end;

function StrToFloatDefUseDot(Value: String; Default: Double): Double;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';

  Result := StrToFloatDef(Value, Default, FormatSettings)
end;

function AddLeadingZeros(const Value: String; const Len: Integer): String;
begin
  Result := Value;

  while length(Result) < Len do
    Result := concat('0', Result);
end;

function TimeBetween(StartTime, EndTime: TDateTime): String;
var
  TimeElapsed: TDateTime;
  Days: Integer;
begin
  TimeElapsed := EndTime - StartTime;
  Days := DaysBetween(0, TimeElapsed);
  result := Format('%.2d', [Days]) + FormatDateTime(':hh:nn:sss.zzzz', TimeElapsed);
end;

function IfThen(Condition: Boolean; TrueResult, FalseResult: Variant): Variant;
begin
  if Condition then
    Result := TrueResult
  else
    Result := FalseResult;
end;

{ TArrayAppender<T> }

class function TArrayUtils<T>.AppendArray(const Arr1, Arr2: TArray<T>): TArray<T>;
var
  i, Count: Integer;
begin
  Count := 0;

  SetLength(Result, length(Arr1) + length(Arr2));

  for i := low(Arr1) to high(Arr1) do
  begin
    Result[Count] := Arr1[i];

    Inc(Count);
  end;

  for i := low(Arr2) to high(Arr2) do
  begin
    Result[Count] := Arr2[i];

    Inc(Count);
  end;
end;

class function TArrayUtils<T>.Contains(const x: T; const anArray: array of T): boolean;
var
  y : T;
  lComparer: IEqualityComparer<T>;
begin
  lComparer := TEqualityComparer<T>.Default;
  for y in anArray do
  begin
    if lComparer.Equals(x, y) then
      Exit(True);
  end;
  Exit(False);
end;


class function TArrayUtils<T>.Reverse(const Values: TArray<T>): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, length(Values));

  for i := Low(Values) to High(Values) do
  begin
    Result[high(Values) - i] := Values[i];
  end;
end;

end.


