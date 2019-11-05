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

  TAppender<T> = class
    class procedure Append(var Arr: TArray<T>; Value: T);
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

class procedure TAppender<T>.Append;
begin
  SetLength(Arr, Length(Arr) + 1);
  Arr[High(Arr)] := Value;
end;

end.

