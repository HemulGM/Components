unit HGM.ArrayHelper;

interface

uses
  System.Generics.Collections, System.Generics.Defaults;

type
  TArrayWalker<T> = reference to procedure(const Item: T; Index: Integer; var Cancel: Boolean);

  TArrayWalkerWrite<T> = reference to procedure(var Item: T; Index: Integer; var Cancel: Boolean);

  TFuncCompare<T> = reference to function(const Left, Right: T): Integer;

  TArrayHelp = class(TArray)
  public
    class function SimpleArrayToTArray<T>(const Values: array of T): TArray<T>;
    class function StringArrayToTArray(const Values: array of WideString): TArray<string>;
    class function Add<T>(var Target: TArray<T>; const Items: array of T): Integer; overload;
    class function Add<T>(var Target: TArray<T>; const Item: T): Integer; overload;
    class function Append<T>(var Target: TArray<T>; Count: Integer = 1): Integer; inline; static;
    class function InArray<T>(Target: TArray<T>; Item: T): Boolean; overload; inline;
    class function Sorted<T>(const Target: TArray<T>; Compare: TFuncCompare<T>): TArray<T>; inline;
    class procedure FreeArrayOfObject<T: class>(var Target: TArray<T>); overload; inline; static;
    class procedure FreeArrayOfArrayOfObject<T: class>(var Target: TArray<TArray<T>>); overload; inline; static;
    class procedure Sort<T>(var Target: TArray<T>; Compare: TFuncCompare<T>); overload; inline;
    class procedure Sort(var Target: TArray<integer>); overload; inline;
    class procedure Walk<T>(const Target: TArray<T>; Proc: TArrayWalker<T>; Offset: Integer = 0); overload; inline; static;
    class procedure Walk<T>(const Target: array of T; Proc: TArrayWalker<T>; Offset: Integer = 0); overload; static;
    class procedure Walk<T>(var Target: TArray<T>; Proc: TArrayWalkerWrite<T>; Offset: Integer = 0); overload; inline; static;
    class procedure Walk<T>(var Target: array of T; Proc: TArrayWalkerWrite<T>; Offset: Integer = 0); overload; static;
    class function Exclude<T>(const Target, Excluding: TArray<T>): TArray<T>; overload; static;
    class function Unique<T>(const Target: TArray<T>): TArray<T>; overload; static;
  end;

implementation

{ TArrayHelp }

class procedure TArrayHelp.FreeArrayOfObject<T>(var Target: TArray<T>);
  {$IFNDEF AUTOREFCOUNT}
var
  Item: T;
  {$ENDIF}
begin
  {$IFNDEF AUTOREFCOUNT}
  for Item in Target do
    if Assigned(Item) then
      Item.Free;
  SetLength(Target, 0);
  {$ENDIF}
end;

class function TArrayHelp.InArray<T>(Target: TArray<T>; Item: T): Boolean;
var
  i: Integer;
  Comparer: IComparer<T>;
begin
  Comparer := TComparer<T>.Default;
  Result := False;
  for i := Low(Target) to High(Target) do
    if Comparer.Compare(Target[i], Item) = 0 then
      Exit(True);
end;

class procedure TArrayHelp.Sort(var Target: TArray<integer>);
var
  i, j: Integer;
  Buf: integer;
begin
  for i := 1 to High(Target) - 1 do
    for j := 0 to High(Target) - i do
      if Target[j] > Target[j + 1] then
      begin
        Buf := Target[j];
        Target[j] := Target[j + 1];
        Target[j + 1] := Buf;
      end;
end;

class procedure TArrayHelp.Sort<T>(var Target: TArray<T>; Compare: TFuncCompare<T>);
var
  i, j: Integer;
  Buf: T;
begin
  for i := 1 to High(Target) - 1 do
    for j := 0 to High(Target) - i do
      if Compare(Target[j], Target[j + 1]) <> 0 then
      begin
        Buf := Target[j];
        Target[j] := Target[j + 1];
        Target[j + 1] := Buf;
      end;
end;

class function TArrayHelp.Sorted<T>(const Target: TArray<T>; Compare: TFuncCompare<T>): TArray<T>;
begin
  Result := Target;
  Sort<T>(Result, Compare);
end;

class function TArrayHelp.SimpleArrayToTArray<T>(const Values: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Length(Values));
  for i := Low(Values) to High(Values) do
    Result[i] := Values[i];
end;

class function TArrayHelp.StringArrayToTArray(const Values: array of WideString): TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Length(Values));
  for i := Low(Values) to High(Values) do
    Result[i] := Values[i];
end;

class function TArrayHelp.Unique<T>(const Target: TArray<T>): TArray<T>;
var
  i: Integer;
  Hash: TDictionary<T, integer>;
begin
  Hash := TDictionary<T, integer>.Create(Length(Target));
  try
    for i := Low(Target) to High(Target) do
      Hash.AddOrSetValue(Target[i], 0);
    Result := Hash.Keys.ToArray;
  finally
    Hash.Free;
  end;
end;

class procedure TArrayHelp.Walk<T>(var Target: array of T; Proc: TArrayWalkerWrite<T>; Offset: Integer);
var
  i: Integer;
  Item: T;
  Cancel: Boolean;
begin
  Cancel := False;
  for i := Low(Target) + Offset to High(Target) do
  begin
    Item := Target[i];
    Proc(Item, i, Cancel);
    Target[i] := Item;
    if Cancel then
      Break;
  end;
end;

class function TArrayHelp.Append<T>(var Target: TArray<T>; Count: Integer): Integer;
begin
  Result := Length(Target);
  SetLength(Target, Result + 1);
end;

class function TArrayHelp.Exclude<T>(const Target, Excluding: TArray<T>): TArray<T>;
var
  i, c: Integer;
begin
  SetLength(Result, Length(Target));
  c := 0;
  for i := Low(Target) to High(Target) do
    if not TArrayHelp.InArray<T>(Excluding, Target[i]) then
    begin
      Result[c] := Target[i];
      Inc(c);
    end;
  SetLength(Result, c);
end;

class procedure TArrayHelp.Walk<T>(var Target: TArray<T>; Proc: TArrayWalkerWrite<T>; Offset: Integer);
var
  i: Integer;
  Item: T;
  Cancel: Boolean;
begin
  Cancel := False;
  for i := Low(Target) + Offset to High(Target) do
  begin
    Item := Target[i];
    Proc(Item, i, Cancel);
    Target[i] := Item;
    if Cancel then
      Break;
  end;
end;

class procedure TArrayHelp.Walk<T>(const Target: TArray<T>; Proc: TArrayWalker<T>; Offset: Integer);
var
  i: Integer;
  Cancel: Boolean;
begin
  Cancel := False;
  for i := Low(Target) + Offset to High(Target) do
  begin
    Proc(Target[i], i, Cancel);
    if Cancel then
      Break;
  end;
end;

class procedure TArrayHelp.Walk<T>(const Target: array of T; Proc: TArrayWalker<T>; Offset: Integer);
var
  i: Integer;
  Cancel: Boolean;
begin
  Cancel := False;
  for i := Low(Target) + Offset to High(Target) do
  begin
    Proc(Target[i], i, Cancel);
    if Cancel then
      Break;
  end;
end;

class function TArrayHelp.Add<T>(var Target: TArray<T>; const Item: T): Integer;
begin
  Result := Add<T>(Target, [Item]);
end;

class function TArrayHelp.Add<T>(var Target: TArray<T>; const Items: array of T): Integer;
var
  i: Integer;
begin
  Result := Length(Target);
  SetLength(Target, Length(Target) + Length(Items));
  for i := Low(Target) + (Result - 1) to High(Target) do
    Target[i] := Items[i - Result];
  Result := Length(Target);
end;

class procedure TArrayHelp.FreeArrayOfArrayOfObject<T>(var Target: TArray<TArray<T>>);
  {$IFNDEF AUTOREFCOUNT}
var
  Item: T;
  Items: TArray<T>;
  {$ENDIF}
begin
  {$IFNDEF AUTOREFCOUNT}
  for Items in Target do
    for Item in Items do
      if Assigned(Item) then
        Item.Free;
  SetLength(Target, 0);
  {$ENDIF}
end;

end.

