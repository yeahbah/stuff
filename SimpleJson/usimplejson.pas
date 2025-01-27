unit uSimpleJson;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uSimpleJsonInterface, Nullable, CustApp;

type
  TTokenType = (
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenBracket,
    CloseBracket,
    DoubleQuote,
    Backslash,
    Colon,
    Comma,
    Text,
    Bool,
    Null,
    Number
  );

  TJsonToken = record
    TokenType: TTokenType;
    TokenValue: string;
  end;

  TKeyValue = record
    Key: string;
    Value: string;
  end;

  { TJsonTokenizer }

  TJsonTokenizer = class
  private
    fCursor: integer;
    fRemaining: pchar;
    function HasMoreTokens(): boolean;
  public
    constructor Create(const aJsonText: pchar);
    function GetNextToken(): specialize TNullable<TJsonToken>;
  end;

  { TSimpleJsonBase }

  TSimpleJsonBase = class abstract(TInterfacedObject, ISimpleJson)
  private
    fTokenizer: TJsonTokenizer;
    fLookaheadToken: specialize TNullable<TJsonToken>;
    function Eat(const aTokenType: specialize TNullable<TTokenType>): TJsonToken;
  protected
    fJsonKeyValues: TStringList;
    procedure ParseJsonObject(const keyName: string; const aJsonValues: TStrings);
    procedure ParseJsonArray(const aKeyName: string);
    procedure ParseJsonExpression(const keyName: string; const aJsonKeyValues: TStrings);
    procedure Parse();
  public
    constructor Create();
    destructor Destroy(); override;
    procedure DeserializeString(const aJsonText: string);
    function Serialize(): string;
    property Json: TStringList read fJsonKeyValues;
  end;


const
  tokenTypeDescriptions: array of string = ('OpenCurlyBrace', 'CloseCurlyBrace',
                       'OpenBracket', 'CloseBracket', 'DoubleQuote',
                       'Backslash', 'Colon', 'Comma', 'Text', 'Bool', 'Null', 'Number');

implementation

uses
  Generics.Collections;

var
  symbols: specialize TDictionary<string, TTokenType>;

operator in (const aString: string; const aArray: array of string): boolean;
var
  s: string;
begin
  result := false;
  for s in aArray do
    if s = aString then exit(true);
end;

operator in (const aChar: char; const aArray: array of char): boolean;
var
  s: string;
begin
  result := false;
  for s in aArray do
    if s = aChar then exit(true);
end;

operator = (const aJsonToken: specialize TNullable<TJsonToken>; const aTokenType: TTokenType): boolean;
begin
  if not aJsonToken.HasValue then exit(false);

  result := aJsonToken.Value.TokenType = aTokenType;
end;


{ TJsonTokenizer }

function TJsonTokenizer.HasMoreTokens(): boolean;
begin
  Result := fCursor < StrLen(fRemaining);
end;

constructor TJsonTokenizer.Create(const aJsonText: pchar);
begin
  fRemaining := aJsonText;
  fCursor := 0;
end;

function TJsonTokenizer.GetNextToken(): specialize TNullable<TJsonToken>;
var
  jsonToken: TJsonToken;
  nextTermPosition: integer;
  d: double;
  number: integer;
  tmp: string;
  tokenType: TTokenType;
  numbers: TStringArray = ('1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.');
  //symbols: TCharArray = ('{', '}', '[', ']', ',', ':');

  //procedure SetRemaining;
  //begin
  //  remaining := string(fRemaining).Substring(fCursor, StrLen(fRemaining) - fCursor);
  //end;

  function StartsWith(aStartsWithText: array of string; c: integer): boolean;
  var
    startIndex, endIndex, j: integer;
    s: string;
  begin
    startIndex := fCursor;
    endIndex := startIndex + c;
    s := '';
    for j := startIndex to endIndex do
    begin
      s := s + fRemaining[j];
    end;
    for j := 0 to High(aStartsWithText) do
    begin
      if s.StartsWith(aStartsWithText[j], true)  then
      begin
        exit(true);
      end;
    end;
    result := false;
  end;

  function IndexOf(const aChar: char; aStartIndex: integer): integer;
  begin
    while aStartIndex < Length(fRemaining) do
    begin
      if fRemaining[aStartIndex] = aChar then exit(aStartIndex);
      Inc(aStartIndex);
    end;
  end;

  function Slice(aStartIndex, aEndIndex: integer): pchar;
  var
    s: string;
  begin
    s := '';
    while (aStartIndex <= aEndIndex) and (aStartIndex < Length(fRemaining)) do
    begin
      s := s + fRemaining[aStartIndex];
      Inc(aStartIndex);
    end;
    result := @s[1];
  end;

begin
  result.Clear;
  if not HasMoreTokens() then exit(result);

  //SetRemaining();
  while HasMoreTokens() do
  begin
    if fRemaining[fCursor] = ' ' then
    begin
      Inc(fCursor);
      if not HasMoreTokens() then exit(result);

      //SetRemaining();

      continue;
    end;

    result.Clear;
    if symbols.TryGetValue(fRemaining[fCursor], tokenType) then
    begin
      jsonToken.TokenType := tokenType;//symbols[fRemaining[fCursor]];
      jsonToken.TokenValue := fRemaining[fCursor];
      result := jsonToken;
    end else
    //if remaining.StartsWith('"') then
    if fRemaining[fCursor] = '"' then
    begin
      nextTermPosition := IndexOf('"', fCursor + 1);// remaining.IndexOf('"', fCursor + 1);
      jsonToken.TokenType := TTokenType.Text;
      jsonToken.TokenValue := Slice(fCursor, nextTermPosition);// fRmaining.Substring(fCursor, nextTermPosition + 1);
      result := jsonToken;
    end else
    if StartsWith(['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'], 1) (*fRemaining[fCursor] in numbers*) then
    begin
      number := fCursor;
      while fRemaining[number] in numbers do
      begin
        tmp := tmp + fRemaining[number];
        inc(number);
      end;
      if not tmp.IsEmpty then
      begin
        if not TryStrToFloat(tmp, d) then
        begin
          raise Exception.CreateFmt('%s is not a valid number', [tmp]);
        end;
        jsonToken.TokenValue := tmp;
        jsonToken.TokenType := TTokenType.Number;
        result := jsonToken;
      end;
    end else
    if StartsWith(['True', 'False'], 4) then
    begin
      nextTermPosition := IndexOf(',', fCursor + 1);// fRemaining.IndexOf(',', fCursor); //remaining.IndexOf(',', 1);
      if nextTermPosition < 0 then
      begin
        nextTermPosition := IndexOf('}', fCursor + 1);// remaining.IndexOf('}', 1);
      end;
      jsonToken.TokenType := TTokenType.Bool;
      jsonToken.TokenValue := Slice(fCursor, nextTermPosition - 1);// remaining.Substring(0, nextTermPosition);
      result := jsonToken;
    end else
    if StartsWith(['null'], 4) then
    begin
      nextTermPosition := IndexOf(',', fCursor + 1);// remaining.IndexOf(',', 1);
      if nextTermPosition < 0 then
      begin
        nextTermPosition := IndexOf('}', fCursor + 1);// remaining.IndexOf('}', 1);
      end;
      jsonToken.TokenValue := Slice(fCursor, nextTermPosition - 1);// remaining.Substring(0, nextTermPosition);
      jsonToken.TokenType := TTokenType.Null;
      result := jsonToken;
    end;

    //tokenValue := FindMatch(keyValuePair.Key, keyValuePair.Value);
    if not result.HasValue then
    begin
      Inc(fCursor);
      //SetRemaining;
      continue;
    end;

    Inc(fCursor, result.Value.TokenValue.Length);

    exit(result);

  end;

  //WriteLn(fCursor);
  //WriteLn(StrLen(fRemaining));
  //raise Exception.Create('Unexpected token');
end;

{ TSimpleJsonBase }

function TSimpleJsonBase.Eat(const aTokenType: specialize TNullable<TTokenType>): TJsonToken;
var
  token: specialize TNullable<TJsonToken>;
begin
  token := fLookaheadToken;
  if not token.HasValue then
  begin
    raise Exception.Create('Unexpected token. Token has no value.');
  end;

  if token.Value.TokenType <> aTokenType.Value then
  begin
    raise Exception.CreateFmt('Unexpected token: %s. Expected: %s',
          [tokenTypeDescriptions[Ord(token.Value.TokenType)], tokenTypeDescriptions[Ord(aTokenType.Value)]]);
  end;

  fLookaheadToken := fTokenizer.GetNextToken();
  exit(token.Value);
end;

// JsonObject
//     : JsonExpression COMMA JsonExpressioin COMMA ...
procedure TSimpleJsonBase.ParseJsonObject(const keyName: string; const aJsonValues: TStrings);
begin
  repeat
    if fLookaheadToken = TTokenType.Comma then Eat(TTokenType.Comma);

    if fLookaheadToken = TTokenType.CloseCurlyBrace then exit;

    ParseJsonExpression(keyName, aJsonValues);

    //aJsonValues.Add(Format('%s=%s', [keyValue.Key, keyValue.Value]));


  until not ((fLookaheadToken = TTokenType.Comma) or
        (fLookaheadToken = TTokenType.CloseCurlyBrace));
end;

// JsonArray
//     : JsonObject COMMA JsonObject COMMA .. JsonObject
procedure TSimpleJsonBase.ParseJsonArray(const aKeyName: string);
var
  jsonValues: TStringList;
  objIndex: integer;
  //guid: TGuid;
  key: string;
begin

  objIndex := 0;
  key := aKeyName;
  if key = '' then
  begin
    key := 'root'
  end;
  key := key + ':';
  jsonValues := TStringList.Create;
  try
    while fLookaheadToken <> TTokenType.CloseBracket do
    begin

      Eat(TTokenType.OpenCurlyBrace);
      ParseJsonObject(key + IntToStr(ObjIndex), jsonValues);
      fJsonKeyValues.AddStrings(jsonValues);
      Eat(TTokenType.CloseCurlyBrace);

      Inc(objIndex);
      jsonValues.Clear();
      //if CreateGUID(guid) = 0 then raise Exception.Create('Unable to create a guid');


      //fJsonKeyValues.Add(key +'='+

      if fLookaheadToken = TTokenType.Comma then Eat(TTokenType.Comma);
    end;

  finally
    jsonValues.Free;
  end;
end;

// JsonExpression
//     : TEXT COLON VALUE
//     | TEXT COLON OBJECT
//     | TEXT COLON ARRAY
//     | NOTHING
procedure TSimpleJsonBase.ParseJsonExpression(const keyName: string;
  const aJsonKeyValues: TStrings);
var
  jsonKey, jsonValue: TJsonToken;
  key: string;
begin
  jsonKey := Eat(TTokenType.Text);
  jsonKey.TokenValue := jsonKey.TokenValue.DeQuotedString('"');
  Eat(TTokenType.Colon);
  case fLookaheadToken.Value.TokenType of
    TTokenType.OpenCurlyBrace:
      begin
        Eat(TTokenType.OpenCurlyBrace);
        ParseJsonObject(jsonKey.TokenValue, aJsonKeyValues);
        Eat(TTokenType.CloseCurlyBrace);
      end;
    TTokenType.OpenBracket:
      begin
        Eat(TTokenType.OpenBracket);
        ParseJsonArray(jsonKey.TokenValue);
        Eat(TTokenType.CloseBracket);
      end;

    else
    begin
      jsonValue := Eat(fLookaheadToken.Value.TokenType);
      if jsonValue.TokenType = TTokenType.Text then
      begin
        jsonValue.TokenValue := jsonValue.TokenValue.DeQuotedString('"');
      end;

      if keyName <> '' then
        key := keyName +':'+ jsonKey.TokenValue
      else
        key := jsonKey.TokenValue;

      aJsonKeyValues.Add(key +'='+ jsonValue.TokenValue);
      //WriteLn(Format('Key: %s, Value: %s', [key, jsonValue.TokenValue]));
      //WriteLn(Format('lookahead: %s', [fLookaheadToken.Value.TokenValue]));
    end;
  end;

end;

procedure TSimpleJsonBase.Parse();
var
  jsonValues: TStringList;
begin
  case fLookaheadToken.Value.TokenType of
    TTokenType.OpenBracket:
      begin
        Eat(TTokenType.OpenBracket);
        ParseJsonArray(string.Empty);
        Eat(TTokenType.CloseBracket);
      end;

    TTokenType.OpenCurlyBrace:
      begin
        Eat(TTokenType.OpenCurlyBrace);
        jsonValues := TStringList.Create;
        try
          ParseJsonObject(string.Empty, jsonValues);
          fJsonKeyValues.AddStrings(jsonValues);
        finally
          jsonValues.Free;
        end;
        Eat(TTokenType.CloseCurlyBrace);
      end;
  end;

end;

constructor TSimpleJsonBase.Create();
begin
  fJsonKeyValues := TStringList.Create;
  fJsonKeyValues.OwnsObjects := true;
  //fJsonArrayObjects := specialize TDictionary<string, TStrings>.Create;
end;

destructor TSimpleJsonBase.Destroy;
begin
  fJsonKeyValues.Free;

  // TODO: free all of it
  //fJsonArrayObjects.Free;
  inherited Destroy;
end;

procedure TSimpleJsonBase.DeserializeString(const aJsonText: string);
begin
  fTokenizer := TJsonTokenizer.Create(pchar(aJsonText));

  fLookaheadToken := fTokenizer.GetNextToken();
  if not fLookaheadToken.HasValue then exit;

  Parse();


end;

function TSimpleJsonBase.Serialize(): string;
begin
  result := '';
end;

initialization
  symbols := specialize TDictionary<string, TTokenType>.Create();
  symbols.Add('[', TTokenType.OpenBracket);
  symbols.Add(']', TTokenType.CloseBracket);
  symbols.Add('\', TTokenType.Backslash);
  symbols.Add('{', TTokenType.OpenCurlyBrace);
  symbols.Add('}', TTokenType.CloseCurlyBrace);
  symbols.Add(',', TTokenType.Comma);
  symbols.Add(':', TTokenType.Colon);

finalization
  symbols.Free;

end.

