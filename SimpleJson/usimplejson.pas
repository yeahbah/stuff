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

  { TJsonTokenizer }

  TJsonTokenizer = class
  private
    fCursor: integer;
    fRemaining: string;
    function HasMoreTokens(): boolean;
  public
    constructor Create(const aJsonText: string);
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
    procedure ParseJsonObject(const keyName: string; var aKeyValues: TStringArray);
    procedure ParseJsonArray(const aKeyName: string);
    procedure ParseJsonExpression(const keyName: string; var aKeyValues: TStringArray);
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
  Result := fCursor <= fRemaining.Length;
end;

constructor TJsonTokenizer.Create(const aJsonText: string);
begin
  fRemaining := aJsonText;
  fCursor := 1;
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

begin
  result.Clear;
  //if not HasMoreTokens() then exit(result);

  //SetRemaining();
  while HasMoreTokens() do
  begin
    if fRemaining[fCursor] = ' ' then
    begin
      Inc(fCursor);
      //if not HasMoreTokens() then exit(result);

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
      nextTermPosition := Pos('"', fRemaining, fCursor + 1); //IndexOf('"', fCursor + 1);// remaining.IndexOf('"', fCursor + 1);
      jsonToken.TokenType := TTokenType.Text;
      jsonToken.TokenValue := Copy(fRemaining, fCursor, (nextTermPosition - fCursor) + 1);//Slice(fCursor, nextTermPosition);// fRmaining.Substring(fCursor, nextTermPosition + 1);
      result := jsonToken;
    end else
    if fRemaining[fCursor] in numbers then
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
      nextTermPosition := Pos(',', fRemaining, fCursor + 1); //IndexOf(',', fCursor + 1);// fRemaining.IndexOf(',', fCursor); //remaining.IndexOf(',', 1);
      if nextTermPosition < 0 then
      begin
        nextTermPosition := Pos('}', fCursor + 1);// remaining.IndexOf('}', 1);
      end;
      jsonToken.TokenType := TTokenType.Bool;
      jsonToken.TokenValue := Copy(fRemaining, fCursor, nextTermPosition - fCursor); //Slice(fCursor, nextTermPosition - 1);// remaining.Substring(0, nextTermPosition);
      result := jsonToken;
    end else
    if StartsWith(['null'], 4) then
    begin
        nextTermPosition := Pos(',', fRemaining, fCursor + 1);//IndexOf(',', fCursor + 1);// remaining.IndexOf(',', 1);
      if nextTermPosition < 0 then
      begin
        nextTermPosition := Pos('}', fRemaining, fCursor + 1);// remaining.IndexOf('}', 1);
      end;
      jsonToken.TokenValue := Copy(fRemaining, fCursor, nextTermPosition - fCursor);//Slice(fCursor, nextTermPosition - 1);// remaining.Substring(0, nextTermPosition);
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
procedure TSimpleJsonBase.ParseJsonObject(const keyName: string; var aKeyValues: TStringArray);
begin
  repeat
    if fLookaheadToken = TTokenType.Comma then Eat(TTokenType.Comma);

    if fLookaheadToken = TTokenType.CloseCurlyBrace then exit;

    ParseJsonExpression(keyName, aKeyValues);

    //aJsonValues.Add(Format('%s=%s', [keyValue.Key, keyValue.Value]));


  until not ((fLookaheadToken = TTokenType.Comma) or
        (fLookaheadToken = TTokenType.CloseCurlyBrace));
end;

// JsonArray
//     : JsonObject COMMA JsonObject COMMA .. JsonObject
procedure TSimpleJsonBase.ParseJsonArray(const aKeyName: string);
var
  objIndex: integer;
  key: string;
  keyValues: TStringArray;
begin

  objIndex := 0;
  key := aKeyName;
  if key = '' then
  begin
    key := 'root'
  end;
  key := key + ':';

  while fLookaheadToken <> TTokenType.CloseBracket do
  begin

    Eat(TTokenType.OpenCurlyBrace);
    ParseJsonObject(key + IntToStr(ObjIndex), keyValues);
    fJsonKeyValues.AddStrings(keyValues);
    Eat(TTokenType.CloseCurlyBrace);

    Inc(objIndex);
    SetLength(keyValues, 0);

    if fLookaheadToken = TTokenType.Comma then Eat(TTokenType.Comma);
  end;

end;

// JsonExpression
//     : TEXT COLON VALUE
//     | TEXT COLON OBJECT
//     | TEXT COLON ARRAY
//     | NOTHING
procedure TSimpleJsonBase.ParseJsonExpression(const keyName: string;
  var aKeyValues: TStringArray);
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
        ParseJsonObject(jsonKey.TokenValue, aKeyValues);
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

      //aJsonKeyValues.Add(key +'='+ jsonValue.TokenValue);
      SetLength(aKeyValues, Length(aKeyValues) + 1);
      aKeyValues[High(aKeyValues)] := key +'='+ jsonValue.TokenValue;
      //WriteLn(Format('Key: %s, Value: %s', [key, jsonValue.TokenValue]));
      //WriteLn(Format('lookahead: %s', [fLookaheadToken.Value.TokenValue]));
    end;
  end;

end;

procedure TSimpleJsonBase.Parse();
var
  jsonValues: TStringArray;
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
        //jsonValues := TStringList.Create;
        //try
          ParseJsonObject(string.Empty, jsonValues);
          fJsonKeyValues.AddStrings(jsonValues);
        //finally
        //  jsonValues.Free;
        //end;
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

