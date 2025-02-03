program simplejson;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, uSimpleJson, uJsonParser, uSimpleJsonInterface,
  Nullable, LCLIntf, uJsonModels, FileUtil
  { you can add units after this };

type

  { TSimpleJson }

  TSimpleJson = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure TestTokenizer(const aJson: string);
    procedure TestSimpleModel;
    procedure TestModelWithArray;
    procedure TestJsonArray;
    procedure TestBigFile;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSimpleJson }

procedure TSimpleJson.DoRun;
var
  json: string;
  //s: char;
  //i: integer;
  //Const P : PChar = 'This is a constant pchar string';




begin
  //Writeln ('P         : ',p);
  //Writeln ('length(P) : ',StrLen(P));
  //
  //i := 0;
  //while i < strLen(p) do
  //begin
  //  Write(p[i]);
  //  inc(i);
  //end;

  //json := '[{"prop1": "value1", "prop2": "value2"}, {"prop1": "value3", "prop2": "value4"}]';
  //json := '{ "hello": "world!", "nice": "one", "number": 123, "bool1": true, "bool2": false, "nullprop": null, '
  //        + '"obj": {"sub1": "value1", "sub2": "value1", } }';
  //TestTokenizer(json);

  TestSimpleModel();


  TestJsonArray();

  TestModelWithArray();

  //TestBigFile();

  // stop program loop
  Terminate;
end;

procedure TSimpleJson.TestTokenizer(const aJson: string);
var
  jsonToken: specialize TNullable<TJsonToken>;
  tokenizer: TJsonTokenizer;
begin
  WriteLn('*** Tokenize ***');

  tokenizer := TJsonTokenizer.Create(pchar(aJson));
  try
    repeat
      jsonToken := tokenizer.GetNextToken();
      if jsonToken.HasValue then
      begin
        WriteLn(Format('Token: %s, Value: %s',
          [tokenTypeDescriptions[Ord(jsonToken.Value.TokenType)], jsonToken.Value.TokenValue]));
      end;
    until not jsonToken.HasValue
  finally
    tokenizer.Free;
  end;
end;

procedure TSimpleJson.TestSimpleModel;
var
  model: TSimpleModel;
  json: string;
  ticks1, ticks2: longint;
begin
  WriteLn('*** Simple model test ***');

  model := TSimpleModel.Create();

  json := '{ "hello": "world!", "nice": "one", "number": 123, "bool1": true, "bool2": false, "nullprop": null, '
          + '"obj": {"sub1": "value1", "sub2": "value1", } }';

  ticks1 := GetTickCount();
  model.DeserializeString(json);
  ticks2 := GetTickCount();
  WriteLn(Format('Simple model took %d ms', [ticks2 - ticks1]));

  WriteLn(model.ToString());
end;

procedure TSimpleJson.TestModelWithArray;
var
  model: TSimpleModelWithArray;
  json: string;
  ticks1, ticks2: longint;
begin
  WriteLn('*** Simple model with array test ***');

  model := TSimpleModelWithArray.Create();

  json := '{ "hello": "world!", "nice": "one", "number": 123, "bool1": true, "bool2": false, "nullprop": null, '
          + '"obj": {"sub1": "value1", "sub2": "value1", }'
          + ', "myarray": ['
          +'  {"prop1": "value1", "prop2": "value2", "test": 1}'
          +', {"prop1": "value3", "prop2": "value4"}'
          +', {"prop1": "value3", "prop2": "value4"}]'
          + '}';

  ticks1 := GetTickCount();
  model.DeserializeString(json);
  ticks2 := GetTickCount();

  WriteLn(model.ToString());
  WriteLn(Format('Simple model with array took %dms', [ticks2 - ticks1]));


end;

procedure TSimpleJson.TestJsonArray;
var
  json: string;
  jsonArray: TSimpleJsonArrayModel;
  item: TSimpleArrayItem;
  i: integer;
  items: array of TSimpleArrayItem;
  ticks1, ticks2: longint;
begin
  WriteLn('*** Simple array test ***');

  json := '['
       +'{"prop1": "value1", "prop2": "value2", "test": 1}'
       +', {"prop1": "value3", "prop2": "value4"}'
       +', {"prop1": "value3", "prop2": "value4"}]';
  jsonArray := TSimpleJsonArrayModel.Create;
  try
    ticks1 := GetTickCount();
    jsonArray.DeserializeString(json);
    ticks2 := GetTickCount();
    WriteLn(Format('Json array took %dms', [ticks2 - ticks1]));

    items := jsonArray.Items;
    WriteLn('Found ' + IntToStr(Length(items)));
    for i := 0 to High(items) do
    begin
      item := items[i];
      WriteLn('----' + i.ToString + '----');
      WriteLn('Prop1: ' + item.Prop1);
      WriteLn('Prop2: ' + item.Prop2);
      WriteLn('Test: ' + item.Test.ToString());
    end;
  finally
    jsonArray.Free;
  end;

end;

procedure TSimpleJson.TestBigFile;
var
  jsonFile: TStrings;
  model: TBigJson;
  ticks1, ticks2: longint;
begin
  WriteLn('*** Deserilizing 5MB json file ***');

  jsonFile := TStringList.Create;
  jsonFile.LoadFromFile('/home/arnold-diaz/five-MB.json');

  model := TBigJson.Create;

  ticks1 := GetTickCount;
  model.DeserializeString(jsonFile.Text);
  ticks2 := GetTickCount;
  WriteLn(Format('5 MB json file took %dms', [ticks2 - ticks1]));

end;

constructor TSimpleJson.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSimpleJson.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TSimpleJson;
begin
  Application:=TSimpleJson.Create(nil);
  Application.Title:='Simple Json';
  Application.Run;
  Application.Free;
end.

