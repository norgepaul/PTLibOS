// 002 PTLib.CommonNames.StringFilter
// 22 feb 2018

// -- (C) Felix John COLIBRI 2018
// -- documentation: http://www.felix-colibri.com

(*
  var l_c_string_filter: TStringNode;

  procedure _build_filter;
  begin
  display_line;
  display('----- '+ p_filter);
  l_c_string_filter:= TStringNode.Create('');
  with l_c_string_filter do
  begin
  BuildAst(p_filter);
  DisplayAst;
  end;

  FilterString(l_string)
*)

(*$R+*)
unit PTLib.Common.StringFilter;

interface

type
  TFilterVariable = record
    Name: String;
    Contains: Boolean;
  end;

  TNamedClass = class
  protected
    FName: String;
  public
    constructor Create(const Name: String); virtual;

    property Name: String read FName;
  end;

  TFilterVariableArray = class(TNamedClass)
    FItems: array of TFilterVariable;
    FCount: Integer;

    function AddVariable(VariableName: String): Integer;
    function IndexOf(VariableName: String): Integer;
    procedure ResetValues;
    procedure Initialize(Value: String);
//    procedure DisplayVariableArray;
  end;

  TNode = class(TNamedClass)
    FFilterVariableArray: TFilterVariableArray;
    constructor Create(const name: String;
      const VariableArrayRef: TFilterVariableArray); reintroduce;
    function EvaluateNode: Boolean; Virtual; Abstract;
    procedure DisplayNode; Virtual; Abstract;
  end;

  TValueNode = class(TNode)
    FLiteralValue: String;
    FVariableIndex: Integer;

    constructor Create(const Name: String;
      const VariableArrayRef: TFilterVariableArray;
      const VariableIndex: Integer);
    function EvaluateNode: Boolean; Override;
    procedure DisplayNode; Override;
  end;

  TNotNode = class(TNode)
    FNotNode: TNode;

    constructor Create(const Name: String;
      const VariableArrayRef: TFilterVariableArray; const NotNode: TNode);
    function EvaluateNode: Boolean; Override;
//    procedure DisplayNode; Override;
    Destructor Destroy; Override;
  end;

  TBinaryNode = class(TNode)
    FLeftNode, FRightNode: TNode;
    constructor Create(const name: String;
      const VariableArrayRef: TFilterVariableArray;
      const LeftNode, RightNode: TNode);
//    procedure DisplayNode; Override;
    Destructor Destroy; Override;
  end;

  TOrNode = class(TBinaryNode)
    function EvaluateNode: Boolean; Override;
  end;

  TAndNode = class(TBinaryNode)
    function EvaluateNode: Boolean; Override;
  end;

  TStringNode = class(TNamedClass)
    FFilterVariableArray: TFilterVariableArray;
    FRootNode: TNode;

    constructor Create(const Name: String); override;

    procedure BuildAst(const Filter: String);
    procedure DisplayAst;
    function FilterString(Value: String): Boolean;

    Destructor Destroy; Override;
  end;

implementation

uses SysUtils, TypInfo;

{ TFilterVariableArray }

function TFilterVariableArray.AddVariable(VariableName: String): Integer;
begin
  Result := IndexOf(VariableName);

  if Result < 0 then
  begin
    Inc(FCount);
    SetLength(FItems, FCount);
    FItems[FCount - 1].name := VariableName;
    Result := FCount - 1;
  end;
end;

function TFilterVariableArray.IndexOf(VariableName: String): Integer;
var
  l_variable_index: Integer;
begin
  Result := -1;
  for l_variable_index := 0 to FCount - 1 do
    with FItems[l_variable_index] do
      if SameText(VariableName, name) then
      begin
        Result := l_variable_index;
        Break;
      end;
end;

procedure TFilterVariableArray.ResetValues;
var
  l_variable_index: Integer;
begin
  for l_variable_index := 0 to FCount - 1 do
    FItems[l_variable_index].contains := False;
end;

procedure TFilterVariableArray.Initialize(Value: String);
var
  l_variable_index: Integer;
  l_lowercase_string: String;
begin
  l_lowercase_string := LowerCase(Value);
  for l_variable_index := 0 to FCount - 1 do
    with FItems[l_variable_index] do
      contains := Pos(name, l_lowercase_string) > 0;
end;

(*procedure TFilterVariableArray.DisplayVariableArray;
var
  l_variable_index: Integer;
begin
  for l_variable_index := 0 to FCount - 1 do
    with FItems[l_variable_index] do
      display(Format('%-15s %s', [name, f_display_TF(contains)]));
end;*)

{ TNode }

constructor TNode.Create(const name: String;
  const VariableArrayRef: TFilterVariableArray);
begin
  inherited Create(Name);

  FFilterVariableArray := VariableArrayRef;
end;

{ TValueNode }

constructor TValueNode.Create(const name: String;
  const VariableArrayRef: TFilterVariableArray; const VariableIndex: Integer);
begin
  inherited Create(Name, VariableArrayRef);

  FLiteralValue := Name;
  FVariableIndex := FFilterVariableArray.IndexOf(FLiteralValue);
end;

function TValueNode.EvaluateNode: Boolean;
begin
  Result := FFilterVariableArray.FItems[FVariableIndex].contains;
end;

procedure TValueNode.DisplayNode;
begin
  //display(Format('%s ', [m_name]));
end;

{ TNotNode }

constructor TNotNode.Create(const name: String;
  const VariableArrayRef: TFilterVariableArray; const NotNode: TNode);
begin
  inherited Create(UpperCase(Name), VariableArrayRef);

  FNotNode := NotNode;
end;

function TNotNode.EvaluateNode: Boolean;
begin
  Result := Not FNotNode.EvaluateNode;
end;

(*procedure TNotNode.DisplayNode;
begin
  FNotNode.DisplayNode;
end;*)

Destructor TNotNode.Destroy;
begin
  FreeAndNil(FNotNode);

  inherited;
end;

{ TBinaryNode }

constructor TBinaryNode.Create(const name: String;
  const VariableArrayRef: TFilterVariableArray;
  const LeftNode, RightNode: TNode);
begin
  inherited Create(UpperCase(Name), VariableArrayRef);
  FLeftNode := LeftNode;
  FRightNode := RightNode;
end;

(*procedure TBinaryNode.DisplayNode;
begin
  FLeftNode.DisplayNode;
  FRightNode.DisplayNode;
end;*)

Destructor TBinaryNode.Destroy;
begin
  FreeAndNil(FLeftNode);
  FreeAndNil(FRightNode);

  inherited;
end;

{ TOrNode }

function TOrNode.EvaluateNode: Boolean;
begin
  (*
    display(Format('    OR %s %s ',
    [f_display_TF(FLeftNode.EvaluateNode), f_display_TF(FRightNode.EvaluateNode)]));
  *)
  Result := FLeftNode.EvaluateNode OR FRightNode.EvaluateNode
end;

{ TAndNode }

function TAndNode.EvaluateNode: Boolean;
begin
  Result := FLeftNode.EvaluateNode AND FRightNode.EvaluateNode;
  (*
    display(Format('    AND %s %s  %s',
    [f_display_TF(FLeftNode.EvaluateNode), f_display_TF(FRightNode.EvaluateNode),
    f_display_TF(Result) ]));
  *)
end;

{ TStringNode }

Constructor TStringNode.Create(const Name: String);
begin
  inherited Create(Name);

  FFilterVariableArray := TFilterVariableArray.Create('');
end;

procedure TStringNode.BuildAst(const Filter: String);
// -- "aaa bbb OR ccc OR ddd eee -fff
type
  TSymbolType = (stUnknown, stAnd, stOr,
    stLiteral, stEnd);
const
  k_tabulation= chr(9);
  k_blanks= [k_tabulation, ' '];
var
  Index, FilterLength: Integer;

  SymbolString: String;
  SymbolType: TSymbolType;
  IsTermStart, ReadNext: Boolean;

  function f_extract_string(const pk_string: String;
    p_start, p_end: Integer): String;
  var
    l_length: Integer;
  begin
    l_length := p_end + 1 - p_start;
    if l_length = 0 then
      Result := ''
    else
    begin
      SetLength(Result, l_length);
      Move(pk_string[p_start], Result[1], l_length);
    end;
  end; // f_extract_string

  function f_skip_spaces(p_string: String; p_index: Integer): Integer;
  begin
    Result := p_index;

    While (Result <= Length(p_string)) and (p_string[Result] = ' ') do
      Inc(Result);
  end;

  function f_string_extract_non_blank(p_string: String;
    var pv_index: Integer): String;
  var
    l_start_index: Integer;
  begin
    pv_index := f_skip_spaces(p_string, pv_index);
    l_start_index := pv_index;
    while (pv_index <= Length(p_string)) and
      not(p_string[pv_index] in k_blanks) do
      Inc(pv_index);

    Result := copy(p_string, l_start_index, pv_index - l_start_index)
  end; // f_string_extract_non_blank

  function GetSymbolType: String;
  begin
    case SymbolType of
      stUnknown:
        Result := 'unk';
      stAnd:
        Result := 'AND';
      stOr:
        Result := 'OR ';
      stLiteral:
        Result := 'str';
      stEnd:
        Result := 'end';
    end;
  end;

  procedure ReadSymbol;
  var
    l_string: String;
  begin
    if IsTermStart then
    begin
      if Index >= FilterLength then
      begin
        SymbolType := stEnd;
        SymbolString := 'END';
        Exit;
      end;
      SymbolString := f_string_extract_non_blank(Filter, Index);
      SymbolType := stLiteral;
      IsTermStart := False;
      ReadNext := True;
    end
    else
    begin
      if ReadNext then
      begin
        if Index >= FilterLength then
        begin
          SymbolType := stEnd;
          SymbolString := 'END';
          Exit;
        end;
        SymbolString := f_string_extract_non_blank(Filter, Index);

        if SameText(SymbolString, 'OR') then
        begin
          IsTermStart := True;
          SymbolType := stOr;
          ReadNext := True;
        end
        else
        begin
          SymbolType := stAnd;
          ReadNext := False;
        end;
      end
      else
      begin
        SymbolType := stLiteral;
        ReadNext := True;
      end;
    end;
  end; // f__read_symbol

  procedure DisplayTrace(p_text: String);
  begin
    // display(Format('%-9s %s %s', [p_text, GetSymbolType, SymbolString]));
  end; //

  procedure DisplayError(p_error: String);
  begin
    //display_bug_stop('string_filter ' + p_error);
  end;

  function TermNode: TNode;

    function GetFactorNode: TNode;
    var
      Index: Integer;
    begin
      DisplayTrace('> factor');
      case SymbolType OF
        stLiteral:
          begin
            if SymbolString[1] = '-' then
            begin
              System.Delete(SymbolString, 1, 1);
              Index := FFilterVariableArray.AddVariable
                (SymbolString);
              Result := TNotNode.Create('not', FFilterVariableArray,
                TValueNode.Create(SymbolString, FFilterVariableArray,
                Index));
            end
            else
            begin
              Index := FFilterVariableArray.AddVariable
                (SymbolString);
              Result := TValueNode.Create(SymbolString, FFilterVariableArray,
                Index);
            end;
            ReadSymbol;
          end;
      else
        DisplayError('string_litteral');
      end;
      DisplayTrace('< factor');
    end;

  begin
    DisplayTrace('> term');
    Result := GetFactorNode;

    while SymbolType = stAnd do
    begin
      ReadSymbol;
      Result := TAndNode.Create('and', nil, Result, GetFactorNode);
    end;
    DisplayTrace('< term');
  end;

begin
  Index := 1;
  FilterLength := Length(Filter);
  IsTermStart := True;
  ReadSymbol;

  FRootNode := TermNode;

  while SymbolType = stOr do
  begin
    ReadSymbol;
    FRootNode := TOrNode.Create('or', FFilterVariableArray, FRootNode,
      TermNode);
  end;
end;

procedure TStringNode.DisplayAst;
begin
  if FRootNode <> Nil then
    FRootNode.DisplayNode;
end;

function TStringNode.FilterString(Value: String): Boolean;
var
  l_index: Integer;
begin
  with FFilterVariableArray do
  begin
    ResetValues;
    Initialize(Value);
  end;

  Result := FRootNode.EvaluateNode;
end;

Destructor TStringNode.Destroy;
begin
  FRootNode.Free;
  FFilterVariableArray.Free;

  inherited;
end;

{ TNamedClass }

constructor TNamedClass.Create(const Name: String);
begin
  FName := Name;
end;

end.
