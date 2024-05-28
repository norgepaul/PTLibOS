unit PTLib.Common.Strings.Filter;

interface

uses
  SysUtils, Classes,

  PTLib.Common.Interfaces;

type
  TTextFilter = class(TInterfacedObject, ITextFilter)
  strict private
    FFilter: String;
    FFilterStrings: TStringList;
    FFilterStringsUpper: TStringList;
    procedure SetFilter(const Value: String);
    function GetFilter: String;
  public
    constructor Create;
    destructor Destroy; override;

    function IsMatch(const Value: String; const CaseSensitive: Boolean = False; const UseFilterPrefixes: Boolean = False;
      const ExactMatch: Boolean = False; const MatchAll: Boolean = False): Boolean;

    property Filter: String read GetFilter write SetFilter;
  end;

implementation

{ TTextFilter }

constructor TTextFilter.Create;
begin
  FFilterStrings := TStringList.Create;
  FFilterStrings.Delimiter := ' ';
  FFilterStrings.StrictDelimiter := True;

  FFilterStringsUpper := TStringList.Create;
  FFilterStringsUpper.Delimiter := ' ';
  FFilterStringsUpper.StrictDelimiter := True;
end;

destructor TTextFilter.Destroy;
begin
  FreeAndNil(FFilterStrings);
  FreeAndNil(FFilterStringsUpper);

  inherited;
end;

function TTextFilter.GetFilter: String;
begin
  Result := FFilter;
end;

function TTextFilter.IsMatch(const Value: String; const CaseSensitive: Boolean;
  const UseFilterPrefixes: Boolean; const ExactMatch: Boolean; const MatchAll: Boolean): Boolean;
var
  i: Integer;
  MatchFilter: String;
  CompareValue: String;
  DoesNotContain: Boolean;
  HasMatch: Boolean;
  FilterStrings: TStrings;
begin
  HasMatch := False;

  if CaseSensitive then
  begin
    FilterStrings := FFilterStrings;
    MatchFilter := Value;
  end
  else
  begin
    MatchFilter := AnsiUpperCase(Value);
    FilterStrings := FFilterStringsUpper;
  end;

  for i := 0 to pred(FilterStrings.Count) do
  begin
    DoesNotContain := (length(FilterStrings[i]) <> 0) and (FilterStrings[i][low(FilterStrings[i])] = '-');

    if DoesNotContain then
    begin
      MatchFilter := copy(FilterStrings[i], low(FilterStrings[i]) + 1, MaxInt);
    end
    else
    begin
      MatchFilter := FilterStrings[i];
    end;

    if CaseSensitive then
    begin
      CompareValue := Value
    end
    else
    begin
      CompareValue := AnsiUpperCase(Value);
    end;

    if ExactMatch then
    begin
      HasMatch := CompareValue = MatchFilter;
    end
    else
    begin
      HasMatch := pos(MatchFilter, CompareValue) <> 0;
    end;

    if DoesNotContain then
    begin
      HasMatch := not HasMatch;
    end;

    if ((HasMatch) and (not MatchAll)) or
       ((not HasMatch) and (MatchAll)) then
    begin
      Break;
    end;
  end;

  Result := HasMatch;
end;

procedure TTextFilter.SetFilter(const Value: String);
begin
  FFilter := Value;

  FFilterStrings.DelimitedText := Trim(Value);
  FFilterStringsUpper.DelimitedText := AnsiUpperCase(Trim(Value));
end;

end.
