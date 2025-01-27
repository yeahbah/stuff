unit uJsonModels;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Nullable, uSimpleJson, CustApp;

type
  //  {
  //          "hello": "world!", "nice": "one", "number": 123, "bool1": true, "bool2": false, "nullprop": null, '
  //           '"obj": {"sub1": "value1", "sub2": "value1", } }

  { TSimpleJsonObjProperty }

  TSimpleJsonObjProperty = class
  private
    fJsonKeyValues: TStrings;
    function GetSub1: string;
    function GetSub2: string;
  public
    constructor Create(const aJsonKeyValues: TStrings);
    property Sub1: string read GetSub1;
    property Sub2: string read GetSub2;
  end;

  { TSimpleModel }

  TSimpleModel = class(TSimpleJsonBase)
  private
    fJsonObj: TSimpleJsonObjProperty;
    function GetBool1: boolean;
    function GetBool2: boolean;
    function GetHello: string;
    function GetJsonObjProperty: TSimpleJsonObjProperty;
    function GetNice: string;
    function GetNullProp: specialize TNullable<string>;
    function GetNumber: integer;
  public
    constructor Create();
    destructor Destroy(); override;
    property Hello: string read GetHello;
    property Nice: string read GetNice;
    property Number: integer read GetNumber;
    property Bool1: boolean read GetBool1;
    property Bool2: boolean read GetBool2;
    property NullProp: specialize TNullable<string> read GetNullProp;
    property Obj: TSimpleJsonObjProperty read GetJsonObjProperty;
    function ToString(): string; override;
  end;

  { TSimpleArrayItem }

  TSimpleArrayItem = class
  private
    fProp1, fProp2: string;
    fTest: integer;
    function GetProp1: string;
    function GetProp2: string;
    function GetTest: integer;
  public
    constructor Create(const aProp1, aProp2: string; const aTest: integer);
    property Prop1: string read GetProp1;
    property Prop2: string read GetProp2;
    property Test: integer read GetTest;
  end;

  TJsonArrayItems = array of TSimpleArrayItem;

  { TSimpleJsonArrayModel }

  TSimpleJsonArrayModel = class(TSimpleJsonBase)
  private
    function GetJsonArrayItems: TJsonArrayItems;
  public
    property Items: TJsonArrayItems read GetJsonArrayItems;
  end;

  { TSimpleModelWithArray }

  TSimpleModelWithArray = class(TSimpleJsonBase)
  private
    fJsonObj: TSimpleJsonObjProperty;
    function GetArray: TJsonArrayItems;
    function GetBool1: boolean;
    function GetBool2: boolean;
    function GetHello: string;
    function GetJsonObjProperty: TSimpleJsonObjProperty;
    function GetNice: string;
    function GetNullProp: specialize TNullable<string>;
    function GetNumber: integer;
  public
    constructor Create();
    destructor Destroy(); override;
    property Hello: string read GetHello;
    property Nice: string read GetNice;
    property Number: integer read GetNumber;
    property Bool1: boolean read GetBool1;
    property Bool2: boolean read GetBool2;
    property NullProp: specialize TNullable<string> read GetNullProp;
    property Obj: TSimpleJsonObjProperty read GetJsonObjProperty;
    property MyArray: TJsonArrayItems read GetArray;
    function ToString(): string; override;
  end;

  { TBigJson }

  TBigJson = class(TSimpleJsonBase)
  private
    function GetBio: string;
    function GetLanguage: string;
    function GetName: string;
    function GetVersion: double;
  public
    property Name: string read GetName;
    property Language: string read GetLanguage;
    property Bio: string read GetBio;
    property Version: double read GetVersion;
  end;

implementation

uses
  Generics.Collections;

{ TSimpleJsonObjProperty }

function TSimpleJsonObjProperty.GetSub1: string;
begin
  result := fJsonKeyValues.Values['obj:sub1'];
end;

function TSimpleJsonObjProperty.GetSub2: string;
begin
  result := fJsonKeyValues.Values['obj:sub2'];
end;

constructor TSimpleJsonObjProperty.Create(const aJsonKeyValues: TStrings);
begin
  fJsonKeyValues := aJsonKeyValues;
end;

{ TSimpleModel }

function TSimpleModel.GetBool1: boolean;
var
  b: string;
begin
  b := self.Json.Values['bool1'];
  result := StrToBool(b);
end;

function TSimpleModel.GetBool2: boolean;
var
  b: string;
begin
  b := self.Json.Values['bool2'];
  result := StrToBool(b);
end;

function TSimpleModel.GetHello: string;
begin
  result := self.Json.Values['hello'];
end;

function TSimpleModel.GetJsonObjProperty: TSimpleJsonObjProperty;
begin
  result := fJsonObj;
end;

function TSimpleModel.GetNice: string;
begin
  result := self.Json.Values['nice'];
end;

function TSimpleModel.GetNullProp: specialize TNullable<string>;
begin
  result := self.Json.Values['nullprop'];
end;

function TSimpleModel.GetNumber: integer;
begin
  result := StrToInt(self.Json.Values['number']);
end;

constructor TSimpleModel.Create();
begin
  inherited Create;
  //fJson := TStringList.Create;
  fJsonObj := TSimpleJsonObjProperty.Create(fJsonKeyValues);
end;

destructor TSimpleModel.Destroy;
begin
  //fJson.Free;
  fJsonObj.Free;
  inherited Destroy;
end;

function TSimpleModel.ToString: string;
var
  sb: TAnsiStringBuilder;
begin
  sb := TAnsiStringBuilder.Create;
  try
    sb.AppendLine('hello: ' + self.Hello);
    sb.AppendLine('nice: ' + self.Nice);
    sb.AppendLine('number: ' + self.Number.ToString);
    sb.AppendLine('bool1: ' + BoolToStr(self.Bool1, true));
    sb.AppendLine('bool2: ' + BoolToStr(self.Bool2, true));
    sb.AppendLine('nullprop: ' + self.NullProp.Value);
    sb.AppendLine('obj:sub1 -> ' + self.Obj.Sub1);
    sb.AppendLine('obj:sub2 -> ' + self.Obj.Sub2);
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ TSimpleArrayItem }

function TSimpleArrayItem.GetProp1: string;
begin
  result := fProp1;
end;

function TSimpleArrayItem.GetProp2: string;
begin
  result := fProp2;
end;

function TSimpleArrayItem.GetTest: integer;
begin
  result := fTest;
end;

constructor TSimpleArrayItem.Create(const aProp1, aProp2: string; const aTest: integer);
begin
  fProp1 := aProp1;
  fProp2 := aProp2;
  fTest := aTest;
end;

{ TSimpleJsonArrayModel }

function TSimpleJsonArrayModel.GetJsonArrayItems: TJsonArrayItems;
var
  list: specialize TList<TSimpleArrayItem>;
  i, current: integer;
  item: TSimpleArrayItem;
  s, key, prop1, prop2: string;
  test: integer;
begin
  i := 0;
  current := 0;
  list := specialize TList<TSimpleArrayItem>.Create;
  try
    while i < self.Json.Count do
    begin
      WriteLn(self.Json[current]);

      key := 'root:' + IntToStr(current) + ':prop1';
      prop1 := self.Json.Values[key];

      key := 'root:' + IntToStr(current) + ':prop2';
      prop2 := self.Json.Values[key];

      key := 'root:' + IntToStr(current) + ':test';
      integer.TryParse(self.Json.Values[key], test);

      Inc(current);
      Inc(i, 2);

      item := TSimpleArrayItem.Create(prop1, prop2, test);
      list.Add(item);
    end;
    result := list.ToArray;
  finally
    list.Free;
  end;
end;

{ TSimpleModelWithArray }

function TSimpleModelWithArray.GetBool1: boolean;
var
  b: string;
begin
  b := self.Json.Values['bool1'];
  result := StrToBool(b);
end;

function TSimpleModelWithArray.GetArray: TJsonArrayItems;
var
  list: specialize TList<TSimpleArrayItem>;
  i, current: integer;
  item: TSimpleArrayItem;
  key, prop1, prop2: string;
  test: integer;
const
  myArrayKey: string = 'myarray:';
begin
  i := 0;
  current := 0;
  list := specialize TList<TSimpleArrayItem>.Create;
  try
    while i < self.Json.Count do
    begin
      if not self.Json[i].StartsWith(myArrayKey) then
      begin
        Inc(i);
        continue;
      end;

      WriteLn(self.Json[i]);

      key := myArrayKey + IntToStr(current) + ':prop1';
      prop1 := self.Json.Values[key];

      key := myArrayKey + IntToStr(current) + ':prop2';
      prop2 := self.Json.Values[key];

      key := myArrayKey + IntToStr(current) + ':test';
      integer.TryParse(self.Json.Values[key], test);

      Inc(current);
      Inc(i, 3);

      item := TSimpleArrayItem.Create(prop1, prop2, test);
      list.Add(item);
    end;
    result := list.ToArray;
  finally
    list.Free;
  end;

end;

function TSimpleModelWithArray.GetBool2: boolean;
var
  b: string;
begin
  b := self.Json.Values['bool2'];
  result := StrToBool(b);

end;

function TSimpleModelWithArray.GetHello: string;
begin
  result := self.Json.Values['hello'];
end;

function TSimpleModelWithArray.GetJsonObjProperty: TSimpleJsonObjProperty;
begin
  result := fJsonObj;
end;

function TSimpleModelWithArray.GetNice: string;
begin
  result := self.Json.Values['nice'];
end;

function TSimpleModelWithArray.GetNullProp: specialize TNullable<string>;
begin
  result := self.Json.Values['nullprop'];
end;

function TSimpleModelWithArray.GetNumber: integer;
begin
  result := self.Json.Values['number'].ToInteger();
end;

constructor TSimpleModelWithArray.Create();
begin
  inherited Create;
  fJsonObj := TSimpleJsonObjProperty.Create(fJsonKeyValues);
end;

destructor TSimpleModelWithArray.Destroy;
begin
  fJsonObj.Free;
  inherited Destroy;
end;

function TSimpleModelWithArray.ToString: string;
var
  sb: TAnsiStringBuilder;
  item: TSimpleArrayItem;
  items: array of TSimpleArrayItem;
  i: integer;
begin
  sb := TAnsiStringBuilder.Create;
  try
    sb.AppendLine('hello: ' + self.Hello);
    sb.AppendLine('nice: ' + self.Nice);
    sb.AppendLine('number: ' + self.Number.ToString);
    sb.AppendLine('bool1: ' + BoolToStr(self.Bool1, true));
    sb.AppendLine('bool2: ' + BoolToStr(self.Bool2, true));
    sb.AppendLine('nullprop: ' + self.NullProp.Value);
    sb.AppendLine('obj:sub1 -> ' + self.Obj.Sub1);
    sb.AppendLine('obj:sub2 -> ' + self.Obj.Sub2);
    i := 0;
    items := myArray;
    for item in items do
    begin
      sb.AppendLine(Format('myarray:%d:%s : %s', [i, 'prop1', item.Prop1]));
      sb.AppendLine(Format('myarray:%d:%s : %s', [i, 'prop2', item.Prop2]));
      sb.AppendLine(Format('myarray:%d:%s : %d', [i, 'test', item.Test]));
      inc(i);
    end;

    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

{ TBigJson }

function TBigJson.GetBio: string;
begin
  result := Json.Values['bio'];
end;

function TBigJson.GetLanguage: string;
begin
  result := Json.Values['language'];
end;

function TBigJson.GetName: string;
begin
  result := Json.Values['name'];
end;

function TBigJson.GetVersion: double;
var
  f: double;
begin
  double.TryParse(Json.Values['version'], f);
  result := f;
end;

end.

