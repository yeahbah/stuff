unit uSimpleJsonInterface;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  ISimpleJson = interface
    procedure DeserializeString(const aJsonText: string);
    function Serialize(): string;
  end;

implementation

end.

