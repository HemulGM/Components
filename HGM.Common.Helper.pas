unit HGM.Common.Helper;

interface

uses
  System.Types, System.Generics.Collections, System.SysUtils,
  System.Generics.Defaults;

type
  ThArray = class(TArray)
    class function InArray(Target: array of string; Item: string): Boolean; overload;
    class function InArray(Target: array of Integer; Item: Integer): Boolean; overload;
  end;

implementation

class function ThArray.InArray(Target: array of string; Item: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Target) to High(Target) do
    if Target[i] = Item then
      Exit(True);
end;

class function ThArray.InArray(Target: array of Integer; Item: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(Target) to High(Target) do
    if Target[i] = Item then
      Exit(True);
end;

end.

