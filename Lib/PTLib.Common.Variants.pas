{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Variants                                    }
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

unit PTLib.Common.Variants;

interface

uses
  System.SysUtils, System.Variants,

  PTLib.Common.Utils,
  PTLib.Common.Dates;

function GetVariantTypeName(const Value: Variant): string;
function GetVariantFromTextDeclaration(const TypeName: string; ValueText: string): Variant;
function VariantToString(const Value: Variant): String;

implementation

function GetVariantFromTextDeclaration(
  const TypeName: string; ValueText: string): Variant;
begin
  ValueText := ValueText.Trim;

  if (ValueText.Length > 0) and (ValueText <> 'null') then
  Begin
    if TypeName = 'smallint' then
      Result := SmallInt(StrToInt(ValueText)) else
    if TypeName = 'integer' then
      Result := StrToInt(ValueText) else
    if TypeName = 'single' then
      Result := StrToFloatDefUseDot(ValueText, 0) else
    if TypeName = 'double' then
      Result := StrToFloatDefUseDot(ValueText, 0) else
    if TypeName = 'currency' then
      Result := StrToFloatDefUseDot(ValueText, 0) else
    if TypeName = 'date' then
      Result := CommonDateTimeStrToDateTime(ValueText) else
    if TypeName = 'string' then
      Result := ValueText else
    if TypeName = 'boolean' then
      Result := StrToBool(ValueText) else
    if TypeName = 'byte' then
      Result := Byte(StrToInt(ValueText)) else
    if TypeName = 'word' then
      Result := Word(StrToInt(ValueText)) else
    if TypeName = 'longword' then
      Result := LongWord(StrToInt(ValueText)) else
    if TypeName = 'int64' then
      Result := Int64(StrToInt(ValueText));
  end
  else
    Result := Null;
end;

function GetVariantTypeName(const Value: Variant): string;
begin
  Result := '';
  if not VarIsEmpty(Value) and
     not VarIsClear(Value) and
     not VarIsNull(Value) then
  begin
    case (VarType(Value) and VarTypeMask) of
      varSmallInt:  Result := 'smallint';
      varInteger:   Result := 'integer';
      varSingle:    Result := 'single';
      varDouble:    Result := 'double';
      varCurrency:  Result := 'currency';
      varDate:      Result := 'date';
      varOleStr,
      varUString,
      varString:    Result := 'string';
      varBoolean:   Result := 'boolean';
      varByte:      Result := 'byte';
      varWord:      Result := 'word';
      varLongWord:  Result := 'longword';
      varInt64:     Result := 'int64';
    end;
  end else
    result := 'void';
end;

function VariantToString(const Value: Variant): String;
begin
  Result := '';

  case (VarType(Value) and VarTypeMask) of
    varSmallInt:
      begin
        Result := Integer(Value).ToString;
      end;

    varInteger:
      begin
        Result := Integer(Value).ToString;
      end;

    varSingle:
      begin
        Result := FloatToStrUseDot(Value);
      end;

    varDouble:
      begin
        Result := FloatToStrUseDot(Value);
      end;

    varCurrency:
      begin
        Result := FloatToStrUseDot(Value);
      end;

    varDate:
      begin
        Result := DateTimeToCommonDateTimeStr(Value, True);
      end;

    varOleStr:
      begin
        Result := String(Value);
      end;

    varBoolean:
      begin
        Result := BoolToStr(Value, True);
      end;

    varByte:
      begin
        Result := byte(Value).ToString;
      end;

    varWord:
      begin
        Result := Word(Value).ToString;
      end;

    varLongWord:
      begin
        Result := IntToStr(longword(Value));
      end;

    varInt64:
      begin
        Result := IntToStr(Int64(Value));
      end;

    varString, varUString:
      begin
        Result := string(Value);
      end;
  end;
end;

end.

