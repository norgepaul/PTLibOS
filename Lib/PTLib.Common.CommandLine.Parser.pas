{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.CommandLine.Parser                          }
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

unit PTLib.Common.CommandLine.Parser;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  PTLib.Common.Interfaces;

type
  ETokenIndexOutOfRange = class(Exception);
  ETokenNotInteger = class(Exception);
  ETokenNotFound = class(Exception);

  TParamList = class(TDictionary<String, String>);

  TCommandLine = class(TObject)
  strict private
    FCommand: String;
    FParams: TParamList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;

    property Command: String read FCommand write FCommand;
    property Params: TParamList read FParams;
  end;

  TCommandLines = class(TInterfacedObject, ICommandLine)
  strict private
    FCommandLines: TObjectList<TCommandLine>;
    FComandTokens: TStringList;
    FCommandLineIndex: Integer;
    FSwitch: Char;
    FOriginalCommandLine: String;
  private
    procedure ParseCommandLine(const CommandLine: String);
    function BuildCommandLine(const CommandLine: TCommandLine; const UseOriginalText: Boolean): String;
    function QuoteStr(const Value: String): String;
    function ActiveCommandLine: TCommandLine;
    procedure CheckIndex(Index: Integer);
    function GetTokenIndex(const TokenName: String): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Clear: ICommandLine;
    function SetCommandLine(const CommandLine: String): ICommandLine;

    function GetCommand: String;
    function SetCommand(const Command: String): ICommandLine;
    function AddCommandLine: ICommandLine;
    function Append(CommandLine: ICommandLine): ICommandLine;
    function CommandIs(const Command: String): Boolean;
    function HasCommand: Boolean;

    function GetIndex: Integer;
    function SetIndex(Index: Integer): ICommandLine;

    function SetParam(const ParamName: String; const ParamValue: Variant): ICommandLine; overload;
    function SetParam(const ParamName: String): ICommandLine; overload;

    function Text: String;
    function CommandLineText(const UseOriginalText: Boolean = False): String; overload;
    function CommandLineText(const Index: Integer; const UseTokens: Boolean = False): String; overload;

    function ParamExists(const ParamName: String): Boolean;
    function GetParam(const ParamName: String; RaiseException: Boolean = True): Variant;
    function TokenExists(const TokenIndex: Integer): Boolean; overload;
    function TokenExists(const TokenName: String): Boolean; overload;
    function TokenCount: Integer;
    function GetToken(const TokenIndex: Integer; const IsInteger: Boolean = False;
      const MinIntegerValue: Integer = 0; const MaxIntegerValue: Integer = MaxInt): Variant;
    function GetTokenString(const TokenIndex: Integer; const Default: String): String;
    function GetTokenValue(const TokenName: String; const IsInteger: Boolean = False;
      const MinIntegerValue: Int64 = 0; const MaxIntegerValue: Int64 = MaxLongInt): Variant;
    function TokenIs(const TokenIndex: Integer; const TokenValue: String): Boolean;
    procedure RemoveTokensBefore(const TokenIndex: Integer);
    function DeleteParam(const ParamName: String): ICommandLine;
    function ParamCount: Integer;
    function First: ICommandLine;
    function Next: ICommandLine;
    function Previous: ICommandLine;
    function Last: ICommandLine;
    function Count: Integer;
    function Eof: Boolean;
    function SetSwitch(const SwitchChar: Char): ICommandLine;
  end;

function NewCommandLine(CommandLineText: String = ''): ICommandLine; overload;
function NewCommandLine(Params: TStrings; FirstParam: Integer = 0): ICommandLine; overload;

const
  ParamSwitch = '-';

implementation

uses
  PTLib.Common.Strings;

resourcestring
  StrSwitchedParamExpected = 'Switched parameter name expected, but "%s" found.';
  StrCommandLineCannot = 'Command Line cannot be empty.';
  StrParameterSNotFound = 'Parameter "%s" not found.';
  StrCommandLineIndexOutofBounds = 'Command line index out of bounds (%d)';

function NewCommandLine(CommandLineText: String = ''): ICommandLine;
begin
  Result := TCommandLines.Create;

  if CommandLineText <> '' then
    Result.SetCommandLine(CommandLineText);
end;

function NewCommandLine(Params: TStrings; FirstParam: Integer = 0): ICommandLine;
var
  CommandText: String;
  i: Integer;
begin
  CommandText := '';

  for i := FirstParam to pred(Params.Count) do
    CommandText := CommandText + ' ' + Params[i];

  Result := NewCommandLine(CommandText);
end;

{ TCommandLine }

procedure TCommandLines.CheckIndex(Index: Integer);
begin
  if Index >= FCommandLines.Count then
    raise Exception.CreateFmt(StrCommandLineIndexOutofBounds, [Index]);
end;

function TCommandLines.Clear: ICommandLine;
begin
  Result := Self;

  FCommandLines.Clear;
  FComandTokens.Clear;

  AddCommandLine;
end;

function TCommandLines.CommandIs(const Command: String): Boolean;
begin
  Result := SameText(ActiveCommandLine.Command, Command);
end;

function TCommandLines.CommandLineText(const UseOriginalText: Boolean): String;
begin
  CheckIndex(FCommandLineIndex);

  Result := BuildCommandLine(FCommandLines[FCommandLineIndex], UseOriginalText);
end;

constructor TCommandLines.Create;
begin
  FSwitch := ParamSwitch;
  FCommandLines := TObjectList<TCommandLine>.Create(True);
  FComandTokens := TStringList.Create;

  Clear;
end;

function TCommandLines.DeleteParam(const ParamName: String): ICommandLine;
begin
  Result := Self;

  ActiveCommandLine.Params.Remove(LowerCase(ParamName));
end;

destructor TCommandLines.Destroy;
begin
  FreeAndNil(FCommandLines);
  FreeAndNil(FComandTokens);

  inherited;
end;

function TCommandLines.Eof: Boolean;
begin
  Result := FCommandLineIndex >= FCommandLines.Count;
end;

function TCommandLines.First: ICommandLine;
begin
  Result := Self;

  FCommandLineIndex := 0;
end;

function TCommandLines.GetCommand: String;
begin
  Result := ActiveCommandLine.Command;
end;

function TCommandLines.Count: Integer;
begin
  Result := FCommandLines.Count;
end;

function TCommandLines.GetIndex: Integer;
begin
  Result := FCommandLineIndex;
end;

function TCommandLines.ParamCount: Integer;
begin
  Result := ActiveCommandLine.Params.Count;
end;

function TCommandLines.GetParam(const ParamName: String; RaiseException: Boolean): Variant;
var
  Value: String;
begin
  if (not ActiveCommandLine.Params.TryGetValue(LowerCase(ParamName), Value)) and (RaiseException) then
    raise Exception.CreateFmt(StrParameterSNotFound, [ParamName]);

  Result := Value;
end;

function TCommandLines.HasCommand: Boolean;
begin
  Result := ActiveCommandLine.Command <> '';
end;

function TCommandLines.Last: ICommandLine;
begin
  Result := Self;

  FCommandLineIndex := pred(FCommandLines.Count);
end;

function TCommandLines.Next: ICommandLine;
begin
  Result := Self;

  if FCommandLineIndex < FCommandLines.Count then
    Inc(FCommandLineIndex);
end;

function TCommandLines.ParamExists(const ParamName: String): Boolean;
begin
  Result := ActiveCommandLine.Params.ContainsKey(LowerCase(ParamName));
end;

function TCommandLines.SetCommand(const Command: String): ICommandLine;
begin
  Result := Self;

  ActiveCommandLine.Command := Command;
end;

function TCommandLines.SetCommandLine(const CommandLine: String): ICommandLine;
begin
  Result := Self;

  ParseCommandLine(CommandLine);
end;

function TCommandLines.SetIndex(Index: Integer): ICommandLine;
begin
  CheckIndex(Index);

  FCommandLineIndex := Index;

  Result := Self;
end;

function TCommandLines.SetParam(const ParamName: String): ICommandLine;
begin
  Result := SetParam(ParamName, '');
end;

function TCommandLines.SetSwitch(const SwitchChar: Char): ICommandLine;
begin
  Result := Self;

  FSwitch := SwitchChar;
end;

function ReplaceNLs(const CommandLine: String): String;
var
  CommandText: String;
  Block: String;
begin
  Result := '';
  CommandText := CommandLine;

  while CommandText <> '' do
  begin
    Block := NextBlock(CommandText, ' ', False);

    if Trim(Block) = '\n'  then
      Block := #13#10;

    AddToken(Result, Block, ' ');
  end;
end;

procedure TCommandLines.ParseCommandLine(const CommandLine: String);
var
  CommandText, ParamName, ParamValue: String;
  Commands: TStringList;
  i: Integer;
begin
  FOriginalCommandLine := CommandLine;

  ActiveCommandLine.Clear;

  Commands := TStringList.Create;
  try
    // Insert CR LF for \n
    CommandText := ReplaceNLs(CommandLine); //StringReplace(CommandLine, ' \n ', ' '+#10#13, [rfReplaceAll, rfIgnoreCase]);

    Commands.Text := Trim(CommandText);

    for i := 0 to pred(Commands.Count) do
    begin
      if i > FCommandLines.Count - 1 then
        AddCommandLine;

      CommandText := Trim(Commands[i]);

      if CommandText = '' then
        Continue;

      ActiveCommandLine.Command := NextBlock(CommandText, ' ');

      ActiveCommandLine.Params.Clear;

      CommandText := Trim(CommandText);

      while CommandText <> '' do
      begin
        // Get the name
        ParamName := NextBlock(CommandText, ' ', False);

        if (FSwitch <> #00) and
           (ParamName[1] <> FSwitch) then
        begin
          // Do nothing
        end
        else
        begin
          if FSwitch <> #00 then
            ParamName := LowerCase(copy(ParamName, 2, MaxInt));

          CommandText := Trim(CommandText);

          if (CommandText <> '') and
             ((FSwitch = #00) or
              (CommandText[1] <> FSwitch)) then
          begin
            ParamValue := NextBlock(CommandText, ' ', True);

            CommandText := RemoveQuotes(Trim(CommandText), '''');
          end
          else
          begin
            ParamValue := '';
          end;

          ActiveCommandLine.Params.Add(ParamName, ParamValue);
        end;
      end;
    end;

    FCommandLineIndex := 0;
  finally
    FreeAndNil(Commands);
  end;

  GetDelimitedText(
    RemoveMultipleWhiteSpace(Trim(CommandLine)),
    FComandTokens,
    ' ',
    '"',
    True);
end;

function TCommandLines.GetToken(const TokenIndex: Integer; const IsInteger: Boolean;
  const MinIntegerValue: Integer; const MaxIntegerValue: Integer): Variant;
var
  IntResult: Integer;
begin
  if TokenIndex >= FComandTokens.Count then
  begin
    raise ETokenIndexOutOfRange.CreateFmt('Token index out of range - %d', [TokenIndex]);
  end;

  if IsInteger then
  begin
    if (TryStrToInt(FComandTokens[TokenIndex], IntResult)) and
       (IntResult >= MinIntegerValue) and
       (IntResult <= MaxIntegerValue) then
    begin
      Result := IntResult;
    end
    else
    begin
      raise ETokenNotInteger.CreateFmt('Token %d must be an integer between %d and %d',
        [TokenIndex + 1,
         MinIntegerValue,
         MaxIntegerValue]);
    end;
  end
  else
  begin
    Result := FComandTokens[TokenIndex];
  end;
end;

function TCommandLines.GetTokenString(const TokenIndex: Integer;
  const Default: String): String;
begin
  if TokenIndex >= FComandTokens.Count then
  begin
    Result := Default
  end
  else
  begin
    Result := FComandTokens[TokenIndex];
  end;
end;

function TCommandLines.GetTokenIndex(const TokenName: String): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to pred(FComandTokens.Count) do
    if SameText(FComandTokens[i], TokenName) then
      Exit(i);
end;

function TCommandLines.GetTokenValue(const TokenName: String; const IsInteger: Boolean;
  const MinIntegerValue: Int64; const MaxIntegerValue: Int64): Variant;
var
  TokenIndex: Integer;
begin
  TokenIndex := GetTokenIndex(TokenName);

  if TokenIndex <> -1 then
  begin
    Result := GetToken(
      TokenIndex + 1,
      IsInteger,
      MinIntegerValue,
      MaxIntegerValue);
  end
  else
  begin
    raise ETokenNotFound.CreateFmt('Token not found: %s', [TokenName]);
  end;
end;

function TCommandLines.TokenExists(const TokenIndex: Integer): Boolean;
begin
  Result := TokenIndex < FComandTokens.Count;
end;

function TCommandLines.Previous: ICommandLine;
begin
  Result := Self;

  if FCommandLineIndex > 0 then
    Dec(FCommandLineIndex);
end;

function TCommandLines.SetParam(const ParamName: String; const
  ParamValue: Variant): ICommandLine;
begin
  Result := Self;

  if ParamExists(ParamName) then
    ActiveCommandLine.Params[LowerCase(ParamName)] := ParamValue
  else
    ActiveCommandLine.Params.Add(LowerCase(ParamName), ParamValue);
end;

function TCommandLines.CommandLineText(const Index: Integer; const UseTokens: Boolean): String;
begin
  CheckIndex(Index);

  Result := BuildCommandLine(FCommandLines[Index], UseTokens);
end;

function TCommandLines.ActiveCommandLine: TCommandLine;
begin
  CheckIndex(FCommandLineIndex);

  Result := FCommandLines[FCommandLineIndex];
end;

function TCommandLines.AddCommandLine: ICommandLine;
begin
  FCommandLines.Add(TCommandLine.Create);

  FCommandLineIndex := FCommandLines.Count - 1;

  Result := Self;
end;

function TCommandLines.Append(
  CommandLine: ICommandLine): ICommandLine;
var
  i: Integer;
begin
  Result := Self;

  if CommandLine <> nil then
  begin
    for i := 0 to pred(CommandLine.Count) do
    begin
      if GetCommand <> '' then
        AddCommandLine;

      CommandLine.SetIndex(i);

      ParseCommandLine(CommandLine.CommandLineText(i));
    end;
  end;
end;

function TCommandLines.BuildCommandLine(const CommandLine: TCommandLine; const UseOriginalText: Boolean): String;
var
  ParamName, ParamValue: String;
begin
  if UseOriginalText then
  begin
    Result := FOriginalCommandLine;
  end
  else
  begin
    Result := CommandLine.Command;

    for ParamName in CommandLine.Params.Keys do
    begin
      ParamValue := CommandLine.Params[ParamName];

      Result := Result + ' ' + FSwitch + ParamName;

      if ParamValue <> '' then
        Result := Result + ' ' + QuoteStr(ParamValue);
    end;
  end;
end;

function TCommandLines.QuoteStr(const Value: String): String;
begin
  if pos(' ', Value) <> -1 then
    Result := QuotedStr(Value)
  else
    Result := Value;
end;

procedure TCommandLines.RemoveTokensBefore(const TokenIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to pred(TokenIndex) do
  begin
    if FComandTokens.Count = 0 then
    begin
      Break
    end
    else
    begin
      FComandTokens.Delete(0);
    end;
  end;
end;

function TCommandLines.Text: String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to pred(FCommandLines.Count) do
    AddLine(Result, BuildCommandLine(FCommandLines[i], False));
end;

function TCommandLines.TokenCount: Integer;
begin
  Result := FComandTokens.Count;
end;

function TCommandLines.TokenExists(const TokenName: String): Boolean;
begin
  Result := GetTokenIndex(TokenName) <> -1;
end;

function TCommandLines.TokenIs(const TokenIndex: Integer;
  const TokenValue: String): Boolean;
begin
  Result :=
    (TokenIndex < FComandTokens.Count) and
    (SameText(GetToken(TokenIndex), TokenValue));
end;

{ TCommandLine }

procedure TCommandLine.Clear;
begin
  FCommand := '';
  FParams.Clear;
end;

constructor TCommandLine.Create;
begin
  FParams := TParamList.Create;
end;

destructor TCommandLine.Destroy;
begin
  FreeAndNil(FParams);

  inherited;
end;

end.

