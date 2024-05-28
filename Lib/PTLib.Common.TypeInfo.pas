{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.TypeInfo                                    }
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

unit PTLib.Common.TypeInfo;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.RTTI, System.Character;

function GetEnumName(const Info: PTypeInfo; const Param): String;
function GetEnumValue(const Info: PTypeInfo; const Value: String): Integer;
function SetToStringEx(const Info: PTypeInfo; const SetParam; Brackets: Boolean): String;
procedure StringToSetEx(const Info: PTypeInfo; var SetParam; const Value: String);
function GetListProperties(const AList: TObject; out GetItemMethod: TRTTIMethod; out ItemCount: Integer): Boolean;
procedure GetListPropertiesEx(const AList: TObject; out GetItemMethod: TRTTIMethod; out ItemCount: Integer);
procedure GetPropertyNames(const AObject: TObject; const PropertyNames: TStrings);
function GetTypeInfoName(const Info: PTypeInfo): String;

implementation

function GetShortStringString(const ShortStringPointer: PByte): string;
var
  ShortStringLength: Byte;
  FirstShortStringCharacter: MarshaledAString;
  ConvertedLength: Cardinal;
  UnicodeCharacters: array[Byte] of Char; // cannot be more than 255 characters, reserve 1 character for terminating null
begin
  if not Assigned(ShortStringPointer) then
    Result := ''
  else
  begin
    ShortStringLength := ShortStringPointer^;
    if ShortStringLength = 0 then
      Result := ''
    else
    begin
      FirstShortStringCharacter := MarshaledAString(ShortStringPointer+1);
      ConvertedLength := UTF8ToUnicode(
          UnicodeCharacters,
          Length(UnicodeCharacters),
          FirstShortStringCharacter,
          ShortStringLength
        );
      // UTF8ToUnicode will always include the null terminator character in the Result:
      ConvertedLength := ConvertedLength-1;
      SetString(Result, UnicodeCharacters, ConvertedLength);
    end;
  end;
end;

function GetTypeInfoName(const Info: PTypeInfo): String;
begin
  {$IFDEF NEXTGEN}
    Result := GetShortStringString(@Info.Name);
  {$ELSE}
    Result := String(Info.Name);
  {$ENDIF}
end;

procedure GetPropertyNames(const AObject: TObject; const PropertyNames: TStrings);
var count:    integer;
    data:     PTypeData;
    i:        integer;
    info:     PTypeInfo;
    propList: PPropList;
begin
  PropertyNames.Clear;

  info := AObject.ClassInfo;
  data := GetTypeData(info);
  GetMem(propList, data^.PropCount * SizeOf(PPropInfo));
  try
    count := GetPropList(info, tkProperties,  propList);

    for i := 0 to pred(Count) do
      PropertyNames.Add(UTF8ToString(propList^[i]^.Name));
  finally
    FreeMem(propList, data^.PropCount * SizeOf(PPropInfo));
  end;
end;

function GetListProperties(const AList: TObject; out GetItemMethod: TRTTIMethod; out ItemCount: Integer): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
begin
  Result := True;
  ItemCount := -1;
  GetItemMethod := nil;

  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(AList.ClassType);

  RttiProperty := RttiType.GetProperty('Count');

  if Assigned(RttiProperty) then
  begin
    ItemCount := RttiProperty.GetValue(AList).AsInteger;
  end
  else
  begin
    Exit(False);
  end;

  GetItemMethod := RttiType.GetMethod('GetItem');

  if not Assigned(GetItemMethod) then
  begin
    GetItemMethod := RttiType.GetMethod('GetItemRef');

    if not Assigned(GetItemMethod) then
    begin
      Exit(False);
    end;
  end;
end;

procedure GetListPropertiesEx(const AList: TObject; out GetItemMethod: TRTTIMethod; out ItemCount: Integer);
begin
  if not GetListProperties(
    AList,
    GetItemMethod,
    ItemCount) then
  begin
    if ItemCount = -1 then
      raise Exception.Create('Class does not contain a Count property');

    if not Assigned(GetItemMethod) then
      raise Exception.Create('Class does not contain GetItem/GetItemRef methods');
  end;
end;

function GetOrdValue(const Info: PTypeInfo; const SetParam): Integer;
begin
  Result := 0;

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Result := Byte(SetParam);
    otSWord, otUWord:
      Result := Word(SetParam);
    otSLong, otULong:
      Result := Integer(SetParam);
  end;
end;

procedure SetOrdValue(const Info: PTypeInfo; var SetParam; Value: Integer);
begin
  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Byte(SetParam) := Value;
    otSWord, otUWord:
      Word(SetParam) := Value;
    otSLong, otULong:
      Integer(SetParam) := Value;
  end;
end;

function GetEnumName(const Info: PTypeInfo; const Param): String;
begin
  Result := System.TypInfo.GetEnumName(Info, GetOrdValue(Info, Param));
end;

function GetEnumValue(const Info: PTypeInfo; const Value: String): Integer;
begin
  Result := System.TypInfo.GetEnumValue(Info, Value);
end;

function SetToStringEx(const Info: PTypeInfo; const SetParam; Brackets: Boolean): String;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';

  Integer(S) := GetOrdValue(Info, SetParam);
  TypeInfo := GetTypeData(Info)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + String(GetEnumName(TypeInfo, I));
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

procedure StringToSetEx(const Info: PTypeInfo; var SetParam; const Value: String);
var
  P: String;
  EnumInfo: PTypeInfo;
  EnumName: String;
  EnumValue, SetValue: Longint;

  function NextWord(var P: String): String;
  var
    I: Integer;
  begin
    I := 1;
    // scan til whitespace
    while (i <= length(P)) and (not (P[I].IsInArray([',', ' ', #0,']']))) do
      Inc(I);

    Result := copy(P, 1, I - 1);

    // skip whitespace
    while (i <= length(P)) and (P[I].IsInArray([',', ' ',']'])) do
      Inc(I);

    P := copy(P, i, MaxInt);
  end;

begin
  SetOrdValue(Info, SetParam, 0);

  if Value = '' then
    Exit;

  SetValue := 0;
  P := Value;

  // skip leading bracket and whitespace
  while P[1].IsInArray(['[',' ']) do
    P := copy(P, 2, MaxInt);

  EnumInfo := GetTypeData(Info)^.CompType^;
  EnumName := NextWord(P);

  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
    begin
      SetOrdValue(Info, SetParam, 0);
      Exit;
    end;

    {$IFDEF LINUX}
      Assert(False, 'How do we fix this in Godzilla?');
    {$ELSE}
      Include(TIntegerSet(SetValue), EnumValue);
    {$ENDIF}

    EnumName := NextWord(P);
  end;

  SetOrdValue(Info, SetParam, SetValue);
end;

end.


