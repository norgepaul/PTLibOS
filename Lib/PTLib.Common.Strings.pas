{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Strings                                     }
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

unit PTLib.Common.Strings;

interface

uses
  System.SysUtils, System.RTTI, System.TypInfo, System.Variants, System.Classes,
  System.RegularExpressions, System.Character,

  PTLib.Common.Classes;

type
  TObjectClass = class of TObject;

  (* TPTLibTabbedStringList:
     This stringlist allows for tabulator memory, making it easier t
     generate texts with tab indentation differences.
     Use BeginSection() to add a new level of tabulator, and EndSection()
     to subtract a tab.

     Ex:
        FList.text:='non-indented';
        FList.BeginSection;
        try
          FList.add('Indented');
          FList.BeginSection;
          try
            FList.add('Indented more');
          finally
            FList.endSection;
          end;
        finally
          FList.EndSection;
        end;

     Generates plain-text:

      Non-indented              (0 tab)
          Indented              (1 tab)
              Indented more     (2 tab) *)
  TPTLibTabbedStringList = Class(TStringList)
  private
    FTabs:    Integer;
    FTabStr:  String;
  public
    function    AddObject(const S: string; AObject: TObject): Integer;override;
    procedure   BeginSection;
    procedure   EndSection;
    procedure   Clear;override;
    Constructor Create;virtual;
  end;

function FixCapitalisation(const Value: String): String;
function BooleanToString(const Value: Boolean;
  const TrueValue: String = 'Yes'; const FalseValue: String = 'No'): String;
procedure AddToken(var Value: String; const Token: String; const Separator: String = ',');
function GetObjectPropertiesAsText(const AObject: TObject): String;
function IsValidEmail(const Email: String): Boolean;
function StringOrNull(const Value: String): Variant;
function StringOrNone(const Value: String; const EmptyStringValue: String = '-'): String;
function DateToStrOrNever(const Value: TDate; const NeverValue: String = 'Never'): String;
function DateTimeToStrOrNever(const Value: TDate; const NeverValue: String = 'Never'): String;
function NextBlock(var Value: String; const Delimiter: String = ','; const QuotedString: Boolean = False): String;
function LastBlock(var Value: String; const Delimiter: String = ','; const QuotedString: Boolean = False): String;
function AddTrailingDelimiter(const Value: String; const Delimiter: Char = '\'): String; //deprecated 'Use IncludeTrailingPathDelimiter';
function RemoveTrailingDelimiter(const Value: String; const Delimiter: Char = '\'): String;
function EPos(const substr: String; const str: String): Integer;
function RemoveQuotes(const QuotedStr: String; const Quote: Char): String;
procedure GetDelimitedText(const Value: string; const Strings: TStrings; const Delimiter: Char; const QuoteChar: Char; const StrictDelimiter: Boolean);
function RemoveMultipleWhiteSpace(const Value: String): String;
procedure AddLine(var Value: String; const Text: String);
function CTRLCharsToOrdVals(const Value: String; const ConvertAll: Boolean): String;
function CopyBytesToString(const Bytes: TBytes; const StartIndex, EndIndex: Integer): String;
function BytesToString(const Bytes: TBytes): String;
function BytesToHex(const Data: TBytes): String;
function BytesToOrdVals(const Values: TBytes; const ConvertAll: Boolean): String;
function GetChar(const Value: String; const Index: Integer): Char;
function GetByte(const Value: TBytes; const Index: Integer): Byte; overload;
function GetByte(const Value: String; const Index: Integer): Byte; overload;
function RemoveHTMLTags(const Value: string; const ReplaceWith: String = ''): string;
function CreateGUIDString: String;
function CreateGUIDStringAlphaOnly: String;
function RemoveCTRLChars(const Value: String): String;
function PadEndWithSpaces(const Value: String; const FinalLength: Integer): String;
function RemoveLeadingZeros(const Value: String): String; deprecated 'Use TrimLeft'; inline;
function FixPhoneNumber(const PhoneNumber: String; const DefaultCountryCode: String): String;
function StringReplaceEx(const Value: String; const OldPatterns: Array of String;
  NewPatterns: Array of Variant; Flags: TReplaceFlags = [rfReplaceAll, rfIgnoreCase]): String;
function ReplaceTags(const Value: String; const Tags: Array of String; const Values: Array of String): String;
function ParamsToStr(const ParamNames: Array of String; const ParamValues: Array of Variant): String;
function IsStringInStringArray(const Value: String; const Values: Array of String): Boolean;
function StringArrayToCommaText(const Values: Array of String): String;
function IsEmailValid(const EmailAddress: string): Boolean;
function StreamToString(const InStream: TStream): String;
function StringToStream(const InString: String; const OutStream: TStream): Boolean;
function ArrayOfCharToStr(const Chars: Array of Char): String;
function NextDataBlock(var Source: String; const StartDelimiter, EndDelimiter: String;
  const SkipLeadingEndDelimiters: Boolean = False; const MissingValueDelimiter: String = ''): String;
function ReplaceParams(const Text: String; const ParamsNames: Array of String;
  const ParamValues: Array of String; const ParamPreFix: String = ''; const ParamPostFix: String = ''): String;
function IncludeTrailingURLBackslash(const Value: String): String;
function TrimText(const Value: String; const MaxLength: Integer = 2000): String;
function ReverseByteString(const Value: String): String;
function IsMatch(const Text: String; const SearchTerms: String; const ExactMatch: Boolean = False; const CaseSensitive: Boolean = False): Boolean; overload;
function IsMatch(const Text: String; const SearchTokens: TStringList; const ExactMatch: Boolean = False): Boolean; overload;
function SanitizeString(const Value: String; const AllowedChars: TSysCharSet = ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']): String;

procedure AddInfoBlankLine(var Value: String);
procedure AddInfo(var Value: String; const Info: String); overload;
procedure AddInfo(var Value: String; const Info: String; const InfoParams: Array of const); overload;
procedure AddInfoHeading(var Value: String; const InfoHeading: String);

implementation

//############################################################################
//  TPTLibTabbedStringList
//############################################################################

constructor TPTLibTabbedStringList.Create;
begin
  inherited;
  FTabs:=0;
  FTabStr :='';
end;

procedure TPTLibTabbedStringList.Clear;
begin
  inherited;
  FTabs:=0;
  FTabStr :='';
end;

procedure TPTLibTabbedStringList.BeginSection;
begin
  inc(FTabs);
  FTabStr := '';
  while FTabStr.Length<FTabs do
  FTabStr:=FTabStr + ' ';
end;

procedure TPTLibTabbedStringList.EndSection;
begin
  if FTabs>0 then
  begin
    dec(FTabs);
    FTabStr := '';
    while FTabStr.Length<FTabs do
    FTabStr:=FTabStr + ' ';
  end;
end;


function TPTLibTabbedStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  result := inherited AddObject(FTabStr + S,AObject);
end;

//############################################################################
//  Utility functions
//############################################################################

function ReverseByteString(const Value: String): String;
var
  TempUID: String;
  i: Integer;
begin
  TempUID := Value;

  if length(TempUID) mod 2 <> 0 then
  begin
    TempUID := '0' + TempUID;
  end;

  Result := '';

  for i := length(TempUID) div 2 downto 1 do
  begin
    Result := Result + TempUID[(i * 2) - 1] + TempUID[i * 2];
  end;
end;

function GetChar(const Value: String; const Index: Integer): Char;
begin
  if (Value = '') or
     (Index >= length(Value)) then
  begin
    raise EPTLibInvalidData.CreateFmt('Invalid data index - data %s, index %d', [Value, Index]);
  end
  else
  begin
    Result := Value[low(Value) + Index];
  end;
end;

function GetByte(const Value: TBytes; const Index: Integer): Byte;
begin
  Result := Value[low(Value) + Index];
end;

function GetByte(const Value: String; const Index: Integer): Byte;
begin
  Result := Byte(Value[low(Value) + Index]);
end;

function CreateGUIDString: String;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);

  Result := GUIDToString(GUID);
end;

function CreateGUIDStringAlphaOnly: String;
var
  i: Integer;
  GUID: String;
begin
  Result := '';

  GUID := CreateGUIDString;

  for i := Low(GUID) to High(GUID) do
  begin
    if (CharInSet(GUID[i], ['0'..'9', 'a'..'f', 'A'..'F'])) then
    begin
      Result := Result + GUID[i];
    end;
  end;
end;

function SanitizeString(const Value: String; const AllowedChars: TSysCharSet): String;
var
  i: Integer;
begin
  Result := '';

  for i := Low(Value) to High(Value) do
  begin
    if CharInSet(Value[i], AllowedChars) then
    begin
      Result := Result + Value[i];
    end;
  end;
end;

function PadEndWithSpaces(const Value: String; const FinalLength: Integer): String;
begin
  Result := Value;

  While length(Result) < FinalLength do
    Result := Result + ' ';
end;

function RemoveCTRLChars(const Value: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := Low(Value) to High(Value) do
  begin
    if ord(Value[i]) >= 32 then
    begin
      Result := Result + Value[i];
    end;
  end;
end;

function RemoveLeadingZeros(const Value: String): String;
begin
  Result := Value.TrimLeft(['0']);
end;

function FixPhoneNumber(const PhoneNumber: String; const DefaultCountryCode: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to length(PhoneNumber) do
  begin
    if CharInSet(PhoneNumber[i], ['0'..'9', '+']) then
    begin
      Result := Result + PhoneNumber[i];
    end;
  end;

  if (Result <> '') and
     (DefaultCountryCode <> '') then
  begin
    if (copy(Result, 1, 1) <> '+') and
       (copy(Result, 1, 2) <> '00') then
    begin
      Result := DefaultCountryCode + Result;
    end;
  end;
end;

function StringReplaceEx(const Value: String; const OldPatterns: Array of String; NewPatterns: Array of Variant; Flags: TReplaceFlags): String;
var
  i: Integer;
begin
  Assert(length(OldPatterns) = length(NewPatterns));

  Result := Value;

  for i := Low(OldPatterns) to High(NewPatterns) do
    Result := StringReplace(Result, OldPatterns[i], VarToStr(NewPatterns[i]), Flags);
end;

function RemoveHTMLTags(const Value: string; const ReplaceWith: String): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  Result := StringReplace(Value, '<BR>', #10, [rfReplaceAll, rfIgnoreCase]);

  TagBegin := Pos( '<', Result);      // search position of first <

  while (TagBegin > 0) do
  begin
    TagEnd := Pos('>', Result);
    TagLength := TagEnd - TagBegin + 1;

    if TagLength < 1 then
      Break;

    Delete(Result, TagBegin, TagLength);

    if ReplaceWith <> '' then
    begin
      Result.Insert(TagBegin - 1, ReplaceWith);
    end;

    TagBegin:= Pos( '<', Result);
  end;
end;

function IsMatch(const Text: String; const SearchTerms: String; const ExactMatch: Boolean; const CaseSensitive: Boolean): Boolean;
var
  SearchTokens: TStringList;
  TextTemp: String;
begin
  SearchTokens := TStringList.Create;
  try
    SearchTokens.Delimiter := ' ';
    SearchTokens.StrictDelimiter := True;

    if CaseSensitive then
    begin
      SearchTokens.Text := Trim(SearchTerms);
      TextTemp := Text;
    end
    else
    begin
      SearchTokens.Text := AnsiLowerCase(SearchTerms);
      TextTemp := AnsiLowerCase(Text);
    end;

    Result := IsMatch(TextTemp, SearchTokens, ExactMatch);
  finally
    FreeAndNil(SearchTokens);
  end;
end;

function IsMatch(const Text: String; const SearchTokens: TStringList; const ExactMatch: Boolean): Boolean;
var
  i: Integer;
  IsAnd: Boolean;
begin
  Result := SearchTokens.Text = '';

  if not Result then
  begin;
    IsAnd := True;

    for i := 0 to pred(SearchTokens.Count) do
    begin
      if SameText(SearchTokens[i], 'or') then
      begin
        IsAnd := False;
      end
      else
      if SameText(SearchTokens[i], 'and') then
      begin
        IsAnd := True;
      end
      else
      begin
        if ((ExactMatch) and (SearchTokens[i] = Text)) or
           ((not ExactMatch) and (pos(SearchTokens[i], Text) <> 0)) then
        begin
          Result := True;
        end
        else
        begin
          if IsAnd then
          begin
            Result := False;
          end;
        end;
      end;
    end;
  end;
end;

function BytesToOrdVals(const Values: TBytes; const ConvertAll: Boolean): String;
var
  i: Integer;
  AChar: Char;
begin
  Result := '';

  for i := low(Values) to High(Values) do
  begin
    AChar := Chr(Values[i]);

    if (ord(AChar) > 30) and (not ConvertAll) then
    begin
      Result := Result + AChar;
    end
    else
    begin
      if Result <> '' then
      begin
        Result := Result + ' ';
      end;

      Result := Result + '$' + IntToHex(Values[i], 2);
    end;
  end;
end;

function TrimText(const Value: String; const MaxLength: Integer): String;
const
  TruncatedText = '%s [Truncated from %d chars]';
begin
  if (Length(Value) > MaxLength) and
     (not FindCmdLineSwitch('nologlinelimit')) then
  begin
    Result := format(TruncatedText, [copy(Value, 1, MaxLength), Length(Value)]);
  end
  else
  begin
    Result := Value;
  end;
end;

function IncludeTrailingURLBackslash(const Value: String): String;
begin
  Result := Value;

  if (Result = '') or
     (Result[High(Result)] <> '/') then
  begin
    Result := Result + '/';
  end;
end;

function ReplaceParams(const Text: String; const ParamsNames: Array of String;
  const ParamValues: Array of String; const ParamPreFix: String; const ParamPostFix: String): String;
var
  i: Integer;
begin
  Assert(length(ParamsNames) = length(ParamValues), 'ParamNames and ParamValues lengths must be the same');

  Result := Text;

  for i := low(ParamsNames) to high(ParamsNames) do
  begin
    Result := StringReplace(
      Result,
      ParamPreFix + ParamsNames[i] + ParamPostFix,
      ParamValues[i],
      [rfReplaceAll]);
  end;
end;

function NextDataBlock(var Source: String; const StartDelimiter, EndDelimiter: String;
  const SkipLeadingEndDelimiters: Boolean = False; const MissingValueDelimiter: String = ''): String;
var
  idx, idx2: Integer;
begin
  Result := '';

  if StartDelimiter = '' then
  begin
    idx := 1;
  end
  else
  begin
    idx := pos(StartDelimiter, Source);
  end;

  if idx > 0 then
  begin
    Source := Source.Remove(0, idx + length(StartDelimiter) - 1);

    if SkipLeadingEndDelimiters then
    begin
      while Source <> '' do
      begin
        if Source[low(Source)] = EndDelimiter then
        begin
          Source := Source.Remove(0, 1);
        end
        else
        begin
          break;
        end;
      end;
    end;

    if EndDelimiter = '' then
    begin
      idx := length(Source);
    end
    else
    begin
      idx := pos(EndDelimiter, Source);
    end;

    // Is the value missing?
    if MissingValueDelimiter <> '' then
    begin
      idx2 := pos(MissingValueDelimiter, Source);

      if (idx2 > 0) and
         (idx2 < idx) then
      begin
        Exit('');
      end;
    end;

    if idx > 0 then
    begin
      Result := Copy(Source, 1, idx - 1);
      Source := Source.Remove(0, idx + length(EndDelimiter) - 1);
    end;
  end;
end;

function BytesToString(const Bytes: TBytes): String;
begin
  Result := CopyBytesToString(Bytes, 0, high(Bytes));
end;

function BytesToHex(const Data: TBytes): String;
var
  i: Integer;
begin
  Result := '';

  for i := Low(Data) to High(Data) do
  begin
    Result := Result + IntToHex(Data[i], 2);
  end;
end;

function CopyBytesToString(const Bytes: TBytes; const StartIndex, EndIndex: Integer): String;
var
  i: Integer;
begin
  Result := '';

  for i := StartIndex to EndIndex do
  begin
    if i > high(Bytes) then
    begin
      Break;
    end
    else
    begin
      Result := Result + Chr(Bytes[i]);
    end;
  end;
end;

function CTRLCharsToOrdVals(const Value: String; const ConvertAll: Boolean): String;
var
  i: Integer;
begin
  Result := '';

  for i := low(Value) to High(Value) do
  begin
    if (ord(Value[i]) > 30) and (not ConvertAll) then
    begin
      Result := Result + Char(Value[i]);
    end
    else
    begin
      if Result <> '' then
      begin
        Result := Result + ' ';
      end;

      Result := Result + '$' + IntToHex(ord(Value[i]), 2);
    end;
  end;
end;

procedure AddLine(var Value: String; const Text: String);
begin
  if Value <> '' then
    Value := concat(Value, #13 + #10);

  Value := concat(Value, Text);
end;

function RemoveMultipleWhiteSpace(const Value: String): String;
var
  p: Integer;
  LastChar: Char;
begin
  Result := '';
  p := length(Value);
  LastChar := #00;

  while p >= 1 do
  begin
    if (LastChar <> ' ') or (Value[p] <> ' ') then
      Result := Value[p] + Result;

    LastChar := Value[p];

    Dec(p);
  end;
end;

function NextChar(P: PChar): PChar;
begin
  Result := P;
  if (Result <> nil) and (Result^ <> #0) then
  begin
    Inc(Result);
    if Result^.IsLowSurrogate then
      Inc(Result);
    while Result^.GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark do
      Inc(Result);
  end;
end;

function StringArrayToCommaText(const Values: Array of String): String;
var
  i: Integer;
begin
  Result := '';

  for i := Low(Values) to High(Values) do
  begin
    AddToken(Result, Values[i]);
  end;
end;

procedure GetDelimitedText(const Value: string; const Strings: TStrings; const Delimiter: Char; const QuoteChar: Char; const StrictDelimiter: Boolean);
var
  P, P1: PChar;
  S: string;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    P := PChar(Value);
    if not StrictDelimiter then
      while (CharInSet(P^, [#1..' '])) do
        P := NextChar(P);
    while P^ <> #0 do
    begin
      if (P^ = QuoteChar) and (QuoteChar <> #0) then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not StrictDelimiter and (P^ > ' ')) or
              (StrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          P := NextChar(P);
        SetString(S, P1, P - P1);
      end;
      Strings.Add(S);
      if not StrictDelimiter then
        while (CharInSet(P^, [#1..' '])) do
          P := NextChar(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        if NextChar(P1)^ = #0 then
          Strings.Add('');
        repeat
          P := NextChar(P);
        until not (not StrictDelimiter and (CharInSet(P^, [#1..' '])));
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function RemoveQuotes(const QuotedStr: String; const Quote: Char): String;
begin
  Result := QuotedStr;

  if (Result <> '') and (Result[1] = Quote) then
    Delete(Result, 1, 1);

  if (Result <> '') and (Result[length(Result)] = Quote) then
    Delete(Result, Length(Result), 1);
end;

function AddTrailingDelimiter(const Value: String; const Delimiter: Char): String;
begin
  Result := Value;

  if (Result <> '') and
     (Result[length(Result)] <> Delimiter) then
    Result := Result + Delimiter;
end;

function EPos(const substr: String; const str: String): Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := length(str) - length(substr) + 1 downto 1 do
    if substr = copy(str, i, length(substr)) then
      Exit(i);
end;

function RemoveTrailingDelimiter(const Value: String; const Delimiter: Char): String;
begin
  Result := Value;

  While (Result <> '') and (Result[length(Result)] = Delimiter) do
    Delete(Result, Length(Result), 1);
end;

function NextBlock(var Value: String; const Delimiter: String; const QuotedString: Boolean): String;
const
  Quote = '''';
var
  p: Integer;
  InQuotes: Boolean;
begin
  p := 1;
  InQuotes := False;

  while (p <= length(Value) - length(Delimiter) + 1) and
        ((copy(Value, p, length(Delimiter)) <> Delimiter) or
        (InQuotes)) do
  begin
    if Value[p] = Quote then
      InQuotes := not InQuotes;

    Inc(p);
  end;

  if p > length(Value) then
    Result := Value
  else
    Result := copy(Value, 1, p - 1);

  Value := Trim(copy(Value, p + length(Delimiter), MaxInt));

  if (QuotedString) and (Result <> '') then
  begin
    if Result[1] = Quote then
      Delete(Result, 1, 1);

    if (Result <> '') and (Result[length(Result)] = Quote) then
      Delete(Result, length(Result), 1);
  end;
end;

function LastBlock(var Value: String; const Delimiter: String; const QuotedString: Boolean): String;
const
  Quote = '''';
var
  p: Integer;
  InQuotes: Boolean;
begin
  p := length(Value);

  InQuotes := False;

  while (p > length(Delimiter)) and
        ((copy(Value, p, length(Delimiter)) <> Delimiter) or
        (InQuotes)) do
  begin
    if Value[p] = Quote then
      InQuotes := not InQuotes;

    Dec(p);
  end;

  if p = 1 then
    Result := Value
  else
    Result := copy(Value, p - 1, MaxInt);

  Value := Trim(copy(Value, 1, p - length(Delimiter)));

  if (QuotedString) and (Result <> '') then
  begin
    if Result[1] = Quote then
      Delete(Result, 1, 1);

    if (Result <> '') and (Result[length(Result)] = Quote) then
      Delete(Result, length(Result), 1);
  end;
end;

function DateToStrOrNever(const Value: TDate; const NeverValue: String = 'Never'): String;
begin
  if Value = 0 then
    Result := NeverValue
  else
    Result := DateToStr(Value);
end;

function DateTimeToStrOrNever(const Value: TDate; const NeverValue: String = 'Never'): String;
begin
  if Value = 0 then
    Result := NeverValue
  else
    Result := DateTimeToStr(Value);
end;

function StringOrNull(const Value: String): Variant;
begin
  if Value = '' then
    Result := Null
  else
    Result := Value;
end;

function StringOrNone(const Value: String; const EmptyStringValue: String): String;
begin
  if Value = '' then
    Result := EmptyStringValue
  else
    Result := Value;
end;

function IsValidEmail(const Email: String): Boolean;
begin
  Result := TRegEx.Create('^[\w\.=-]+@[\w\.-]+\.[\w]{2,3}$').IsMatch(Email);
end;

// Fixes the capitalisation of a string e.g. PAUL THORNTON
// returns Paul Thornton
function FixCapitalisation(const Value: String): String;
var
  i: Integer;
begin
  Result := Value;

  for i := 1 to length(Result) do
  begin
    if Result[i] = '_' then
    begin
      Result[i] := ' ';
    end;

    if (i = 1) or
       (CharInSet(Result[i - 1], [' ', '(', '[', '{', '-'])) then
    begin
      Result[i] := AnsiUpperCase(Result[i])[1]
    end
    else
    begin
      Result[i] := AnsiLowerCase(Result[i])[1];
    end;
  end;
end;

function BooleanToString(const Value: Boolean;
  const TrueValue: String; const FalseValue: String): String;
begin
  if Value then
    Result := TrueValue
  else
    Result := FalseValue;
end;

procedure AddToken(var Value: String; const Token: String; const Separator: String);
begin
  if Value <> '' then
    Value := concat(Value, Separator);

  Value := concat(Value, Token);
end;

function GetObjectPropertiesAsText(const AObject: TObject): String;

const
  CRLF = #13#10;

  procedure AddProperty(const Name: String; const Value: Variant;
    const Indent: String = '');
  var
    RealValue: String;
  begin
    if Value = Null then
      RealValue := 'Null'
    else
      RealValue := VarToStr(Value);

    AddToken(Result,
             Indent +
             format('%s=%s', [Name,
                              RealValue]),
             CRLF);
  end;

var
  PropIndex: Integer;
  PropCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
const
  TypeKinds: TTypeKinds = [tkEnumeration, tkString, tkLString, tkWString,
    tkUString];
begin
  GetMem(PropList, SizeOf(PropList^));
  try
    PropCount := GetPropList(AObject.ClassInfo, TypeKinds, PropList);

    AddProperty(AObject.ClassName, '');

    for PropIndex := 0 to pred(PropCount) do
    begin
      PropInfo := PropList^[PropIndex];

      if PropInfo^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkFloat,
          tkString, tkSet, tkWChar, tkLString, tkWString, tkVariant, tkInt64, tkUString] then
      begin
        AddProperty(PropInfo^.NameFld.ToString,
                    GetPropValue(AObject, String(PropInfo^.NameFld.ToString), True),
                    '  ');
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

function ReplaceTags(const Value: String; const Tags: Array of String; const Values: Array of String): String;
var
  i: integer;
begin
  if high(Tags) <> high(Values) then
    raise Exception.Create('The sizes of the Tags and Values arrays do not match.'); // Do not localise

  Result := Value;

  for i := low(Tags) to high(Tags) do
    Result := StringReplace(Result, Tags[i], Values[i], [rfReplaceAll]);
end;

function IsStringInStringArray(const Value: String; const Values: Array of String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := Low(Values) to High(Values) do
    if SameText(Value, Values[i]) then
      Exit(True);
end;

function StreamToString(const InStream: TStream): String;
var
  AByte: Byte;
begin
  InStream.Position := 0;

  While InStream.Position < InStream.Size do
  begin
    InStream.Read(AByte, SizeOf(Byte));
    Result := concat(Result, IntToHex(AByte, 2));
  end;
end;

function ArrayOfCharToStr(const Chars: Array of Char): String;
var
  i: Integer;
begin
  Result := '';

  for i := Low(Chars) to High(Chars) do
  begin
    if Chars[i] = #00 then
    begin
      Break;
    end
    else
    begin
      Result := Result + Chars[i];
    end;
  end;
end;

function StringToStream(const InString: String; const OutStream: TStream): Boolean;
var
  AByte: Byte;
  p: Integer;
begin
  Result := InString <> '';

  if Result then
  begin
    p := 1;

    while p < length(InString) do
    begin
      AByte := StrToInt(format('$%s%s', [InString[p], InString[p + 1]]));

      OutStream.Write(AByte, SizeOf(AByte));

      Inc(p, 2);
    end;
  end;
end;

function IsEmailValid(const EmailAddress: string): Boolean;
var
  Domain: string;
  AtIndex, DotIndex, i: integer;
  AtCount: integer;
begin
  result := True;

  AtCount := 0;
  i := 1;

  while result and (i <= length(EmailAddress)) do
  begin
    if EmailAddress[i] = '@' then
    begin
      inc(AtCount);

      if AtCount > 1 then
        Exit(False);
    end
    else
      if not CharInSet(EmailAddress[i], ['0'..'9', 'a'..'z', 'A'..'Z', '_', '-', '.', '~']) then
        Exit(False);

    inc(i);
  end;

  if AtCount = 0 then
    Exit(False)
  else
  begin
    AtIndex := pos('@', EmailAddress);

    result := (AtIndex > 1);

    if result then
    begin
      Domain := copy(EmailAddress, AtIndex + 1, maxint);

      DotIndex := pos('.', Domain);

      Result := not((DotIndex = 0) or
                    (DotIndex < 2) or
                    (DotIndex = length(Domain)));
    end;
  end;
end;

function ParamsToStr(const ParamNames: Array of String; const ParamValues: Array of Variant): String;
var
  i: Integer;
begin
  Result := '';

  for i := low(ParamNames) to high(ParamNames) do
    AddToken(Result, format('%s=%s', [ParamNames[i], VarToStr(ParamValues[i])]));
end;

procedure AddInfoBlankLine(var Value: String);
begin
  Value := concat(Value, #13, #10);
end;

procedure AddInfo(var Value: String; const Info: String);
begin
  if Value <> '' then
    Value := concat(Value, #13, #10);

  Value := concat(Value, Info);
end;

procedure AddInfo(var Value: String;
  const Info: String; const InfoParams: Array of const);
var
  InfoFormatted: String;
begin
  InfoFormatted := Trim(format(Info, InfoParams));

  AddInfo(Value, InfoFormatted);
end;

procedure AddInfoHeading(var Value: String; const InfoHeading: String);
begin
  if Value <> '' then
    AddInfoBlankLine(Value);

  AddInfo(Value, '[ %s ]', [InfoHeading]);
end;

end.


