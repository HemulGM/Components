unit HGM.Common.Workspace;

interface

uses
  System.Classes, REST.JsonReflect, System.RTTI, System.SysUtils, System.Json;

type
  TConfig = class
  private
    FFileName: string;
    FJSON: TJSONObject;
    procedure SetFileName(const Value: string);
  public
    procedure Load;
    procedure Apply;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    function ReadStringDef(const Key: string; const Default: string = ''): string;
    function ReadIntegerDef(const Key: string; const Default: Integer = -1): Integer;
    function ReadBooleanDef(const Key: string; const Default: Boolean = False): Boolean;
    function ReadString(const Key: string; var Value: string): Boolean;
    function ReadInteger(const Key: string; var Value: Integer): Boolean;
    function ReadBoolean(const Key: string; var Value: Boolean): Boolean;
    function ReadValueDef<T>(const Key: string; const Default: T): T;
    function ReadValue<T>(const Key: string; var Value: T): Boolean;
    function Write(const Key: string; const Value: TValue): Boolean;
    function WriteString(const Key: string; const Value: string): Boolean;
    function WriteInteger(const Key: string; const Value: Integer): Boolean;
    function WriteBoolean(const Key: string; const Value: Boolean): Boolean;
    property FileName: string read FFileName write SetFileName;
    property JSON: TJSONObject read FJSON;
  end;

  TAppData = class(TComponent)
    const
      CONFIG_FILE = 'config.json';
  private
    FPath: string;
    FConfig: TConfig;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Initialize(const AppName: string): Boolean;
    property Path: string read FPath;
    property Config: TConfig read FConfig;
  end;

implementation

uses
  System.IOUtils;

{ TAppData }

constructor TAppData.Create(AOwner: TComponent);
begin
  inherited;
  FConfig := TConfig.Create;
end;

destructor TAppData.Destroy;
begin
  FConfig.Free;
  inherited;
end;

function TAppData.Initialize(const AppName: string): Boolean;
begin
  Result := False;
  FPath := TPath.Combine(TPath.GetCachePath, AppName);
  FConfig.FileName := TPath.Combine(FPath, CONFIG_FILE);
  if not TDirectory.Exists(FPath) then
  try
    TDirectory.CreateDirectory(FPath);
  except
    Exit;
  end;
  if not TFile.Exists(FConfig.FileName) then
  try
    {$HINTS OFF}
    TFile.Create(FConfig.FileName).Free;
    {$HINTS ON}
  except
    Exit;
  end;
  Result := True;
end;

{ TConfig }

procedure TConfig.Clear;
begin
  if Assigned(FJSON) then
  begin
    FJSON.Free;
    FJSON := nil;
  end;
  FJSON := TJSONObject.Create;
  Apply;
end;

constructor TConfig.Create;
begin
  FJSON := TJSONObject.Create;
end;

destructor TConfig.Destroy;
begin
  if Assigned(FJSON) then
  begin
    FJSON.Free;
    FJSON := nil;
  end;
  inherited;
end;

procedure TConfig.Load;
var
  Value: TJSONValue;
begin
  if Assigned(FJSON) then
  begin
    FJSON.Free;
    FJSON := nil;
  end;
  try
    if TFile.Exists(FFileName) then
    begin
      Value := TJSONObject.ParseJSONValue(TFile.ReadAllBytes(FFileName), 0);
      if Value is TJSONObject then
        FJSON := TJSONObject(Value)
      else
        Value.Free;
    end;
  except
  end;
  if not Assigned(FJSON) then
    FJSON := TJSONObject.Create;
end;

function TConfig.ReadBoolean(const Key: string; var Value: Boolean): Boolean;
begin
  Result := ReadValue(Key, Value);
end;

function TConfig.ReadBooleanDef(const Key: string; const Default: Boolean): Boolean;
begin
  Result := ReadValueDef(Key, Default);
end;

function TConfig.ReadInteger(const Key: string; var Value: Integer): Boolean;
begin
  Result := ReadValue(Key, Value);
end;

function TConfig.ReadIntegerDef(const Key: string; const Default: Integer): Integer;
begin
  Result := ReadValueDef(Key, Default);
end;

function TConfig.ReadString(const Key: string; var Value: string): Boolean;
begin
  Result := ReadValue(Key, Value);
end;

function TConfig.ReadStringDef(const Key, Default: string): string;
begin
  Result := ReadValueDef(Key, Default);
end;

function TConfig.ReadValue<T>(const Key: string; var Value: T): Boolean;
begin
  Result := JSON.TryGetValue<T>(Key, Value);
end;

function TConfig.ReadValueDef<T>(const Key: string; const Default: T): T;
begin
  Result := Default;
  if Assigned(FJSON) then
    Result := FJSON.GetValue(Key, Default);
end;

procedure TConfig.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

function TConfig.Write(const Key: string; const Value: TValue): Boolean;
var
  JsonValue: TJSONValue;
  Pair: TJSONPair;
begin
  Result := False;
  if Assigned(FJSON) then
  begin
    Pair := FJSON.RemovePair(Key);
    if Assigned(Pair) then
      Pair.Free;
    JsonValue := TJSONUnMarshal.TValueToJson(Value);
    FJSON.AddPair(Key, JsonValue);
    Result := True;
  end;
end;

function TConfig.WriteBoolean(const Key: string; const Value: Boolean): Boolean;
var
  JSONValue: TJSONBool;
  Pair: TJSONPair;
begin
  Result := False;
  if Assigned(FJSON) then
  begin
    Pair := FJSON.RemovePair(Key);
    if Assigned(Pair) then
      Pair.Free;
    JSONValue := TJSONBool.Create(Value);
    FJSON.AddPair(Key, JSONValue);
    Result := True;
  end;
end;

function TConfig.WriteInteger(const Key: string; const Value: Integer): Boolean;
var
  JSONValue: TJSONNumber;
  Pair: TJSONPair;
begin
  Result := False;
  if Assigned(FJSON) then
  begin
    Pair := FJSON.RemovePair(Key);
    if Assigned(Pair) then
      Pair.Free;
    JSONValue := TJSONNumber.Create(Value);
    FJSON.AddPair(Key, JSONValue);
    Result := True;
  end;
end;

function TConfig.WriteString(const Key, Value: string): Boolean;
var
  Pair: TJSONPair;
begin
  Result := False;
  if Assigned(FJSON) then
  begin
    Pair := FJSON.RemovePair(Key);
    if Assigned(Pair) then
      Pair.Free;
    FJSON.AddPair(Key, Value);
    Result := True;
  end;
end;

procedure TConfig.Apply;
begin
  if Assigned(FJSON) then
    TFile.WriteAllText(FFileName, FJSON.ToJSON);
end;

end.

