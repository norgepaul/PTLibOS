{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Crypto                                      }
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

unit PTLib.Common.Crypto;

interface

(*
  Crypto code adapted from this post https://www.delphipraxis.net/603181-post7.html
*)

uses
{$IFDEF MSWINDOWS}
  WinAPi.Windows,
{$ENDIF}

  SysUtils;

{$IFDEF MSWINDOWS}
const
  crypt32 = 'crypt32.dll';
{$ENDIF}

function GeneratePassword(const Len: Integer; const IgnoreChars: TSysCharSet; const OnlyAlphaNum: Boolean): string;
function OSEncryptUserString(const Value: String): String;
function OSDecryptUserString(const Value: String): String;

{$IFDEF MSWINDOWS}
const
  CRYPTPROTECT_UI_FORBIDDEN = $1;
  {$EXTERNALSYM CRYPTPROTECT_UI_FORBIDDEN}

type
  LPLPWSTR = ^LPWSTR;

  _CRYPTOAPI_BLOB = record
    cbData: DWORD;
    pbData: LPBYTE;
  end;

  PCRYPTPROTECT_PROMPTSTRUCT = ^CRYPTPROTECT_PROMPTSTRUCT;
  {$EXTERNALSYM PCRYPTPROTECT_PROMPTSTRUCT}
  _CRYPTPROTECT_PROMPTSTRUCT = record
    cbSize: DWORD;
    dwPromptFlags: DWORD;
    hwndApp: HWND;
    szPrompt: LPCWSTR;
  end;

  {$EXTERNALSYM _CRYPTPROTECT_PROMPTSTRUCT}
  CRYPTPROTECT_PROMPTSTRUCT = _CRYPTPROTECT_PROMPTSTRUCT;

  {$EXTERNALSYM CRYPTPROTECT_PROMPTSTRUCT}
  TCryptProtectPromptStruct = CRYPTPROTECT_PROMPTSTRUCT;
  PCryptProtectPromptStruct = PCRYPTPROTECT_PROMPTSTRUCT;

  function CryptProtectData(pDataIn: PDATA_BLOB; szDataDescr: LPCWSTR;
    pOptionalEntropy: PDATA_BLOB; pvReserved: PVOID;
    pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT; dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall;
  {$EXTERNALSYM CryptProtectData}

  function CryptUnprotectData(pDataIn: PDATA_BLOB; ppszDataDescr: LPLPWSTR;
    pOptionalEntropy: PDATA_BLOB; pvReserved: PVOID;
    pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT; dwFlags: DWORD; pDataOut: PDATA_BLOB): BOOL; stdcall;
  {$EXTERNALSYM CryptUnprotectData}
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
  function CryptProtectData; external crypt32 name 'CryptProtectData';
  function CryptUnprotectData; external crypt32 name 'CryptUnprotectData';
{$ENDIF}

function GeneratePassword(const Len: Integer; const IgnoreChars: TSysCharSet; const OnlyAlphaNum: Boolean): string;
const
  STR_NORM = '1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  STR_EX = STR_NORM + '!#%?+-_<>@£${[]}';
var
  str: String;
  AChar: Char;
begin
  Randomize;

  if OnlyAlphaNum then
  begin
    str := STR_NORM;
  end
  else
  begin
    str := STR_EX;
  end;

  Result := '';

  repeat
    AChar := str[Random(Length(str)) + 1];

    while CharInSet(AChar, IgnoreChars) do
    begin
      AChar := str[Random(Length(str)) + 1];
    end;

    Result := Result + AChar;
  until (Length(Result) = Len)
end;

{$IFDEF MSWINDOWS}

function BlobDataToHexStr(P: PByte; const Len: Integer): string;
var
  HexStr: string;
  i:  Integer;
begin
  HexStr := '';
  i := Len;

  while i > 0 do
  begin
    Dec(i);
    HexStr := HexStr + IntToHex(P^, 2);
    Inc(P);
  end;

  Result := HexStr;
end;

function PasswordHashToBlobData(const Value: string): DATA_BLOB;

  function HexToByte(const Value : String) : Byte;
  const
    cs = '0123456789ABCDEF';
  begin
    if (length(Value) = 2) and
       (CharInSet(Value[1], ['0'..'9','A'..'F'])) and
       (CharInSet(Value[2], ['0'..'9','A'..'F'])) then
    begin
      Result := ((pos(Value[1],cs)-1) *16) + (pos(Value[2],cs)-1);
    end
    else
    begin
      raise EConvertError.CreateFmt('%s is not a Hexformatstring',[Value]);
    end;
  end;

var
  Buf: array of Byte;
  BufSize: Cardinal;
  i: Cardinal;
  j: Cardinal;
  HashSize: Cardinal;
begin
  BufSize := Length(Value) DIV 2;
  HashSize := Length(Value);
  SetLength(Buf, BufSize);

  i := 1;
  j := 0;

  while i < HashSize do
  begin
    Buf[j] := HexToByte(Value[i] + Value[i+1]);
    Inc(i, 2);
    Inc(j);
  end;

  GetMem(Result.pbData, BufSize);
  Result.cbData := BufSize;
  Result.pbData := PByte(Buf);
end;
{$ENDIF}

function OSEncryptUserString(const Value: String): String;
{$IFDEF MSWINDOWS}
var
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  Description: PWideChar;
  PwdHash: string;
begin
  PwdHash := '';

  DataOut.cbData := 0;
  DataOut.pbData := nil;

  // RDP uses UniCode
  DataIn.pbData := Pointer(WideString(Value));
  DataIn.cbData := Length(Value) * SizeOf(WChar);
  try
    // RDP always sets description to psw
    Description := WideString('psw');

    if CryptProtectData(
      @DataIn,
      Description,
      nil,
      nil,
      nil,
      CRYPTPROTECT_UI_FORBIDDEN,  // Never show interface
      @DataOut) then
    begin
      PwdHash := BlobDataToHexStr(DataOut.pbData, DataOut.cbData);
    end;

    Result := PwdHash;
  finally
    LocalFree(Cardinal(DataOut.pbData));
    LocalFree(Cardinal(DataIn.pbData));
  end;
{$ELSE}
begin
  raise Exception.Create('OSEncryptUserString is not supported on this platform');
{$ENDIF}
end;

function OSDecryptUserString(const Value: String): String;
{$IFDEF MSWINDOWS}
var
  DataIn: DATA_BLOB;
  DataOut: DATA_BLOB;
  Password: string;
  Decrypted: PWideChar;
  Description: PWideChar;
begin
  DataIn := PasswordHashToBlobData(Value);

  DataOut.cbData := 0;
  DataOut.pbData := nil;
  try
    if CryptUnprotectData(
      @DataIn,
      @Description,
      nil,
      nil,
      nil,
      CRYPTPROTECT_UI_FORBIDDEN, // Never show interface
      @DataOut) then
    begin
      Getmem(Decrypted, DataOut.cbData);
      lstrcpynW(Decrypted, PWideChar(DataOut.pbData), (DataOut.cbData DIV 2) + 1);
      Password := Decrypted;
      FreeMem(Decrypted);
    end
    else
    begin
      raise EConvertError.CreateFmt('Error decrypting: %s',[SysErrorMessage(GetLastError)]);
    end;

    Result := Password;
  finally
    if DataOut.cbData > 0 then
    begin
      LocalFree(Cardinal(DataOut.pbData));
    end;
  end;
{$ELSE}
begin
  raise Exception.Create('OSEncryptUserString is not supported on this platform');
{$ENDIF}
end;

end.
