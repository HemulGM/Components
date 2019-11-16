unit HGM.Autorun;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Registry;

type
  TAutorun = class
  private
    FAppName, FAppCommand: string;
    function GetAutorunCurrentReg: Boolean;
    procedure SetAutorunCurrentReg(const Value: Boolean);
    function GetAutorunLocalReg: Boolean;
    procedure SetAutorunLocalReg(const Value: Boolean);
  public
    constructor Create(AppName, AppCommand: string);
    property AppName: string read FAppName write FAppName;
    property AppCommand: string read FAppCommand write FAppCommand;
    property AutorunCurrentReg: Boolean read GetAutorunCurrentReg write SetAutorunCurrentReg;
    property AutorunLocalReg: Boolean read GetAutorunLocalReg write SetAutorunLocalReg;
  end;

implementation

{ TAutorun }

constructor TAutorun.Create(AppName, AppCommand: string);
begin
  FAppName := AppName;
  FAppCommand := AppCommand;
end;

function TAutorun.GetAutorunCurrentReg: Boolean;
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(KEY_READ);
  Reg.RootKey := HKEY_CURRENT_USER;
  if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Run\') then
  begin
    Result := Reg.ValueExists(FAppName);
  end
  else
  begin
    Reg.Free;
    raise Exception.Create('Ошибка при чтении данных из реестра');
  end;

  Reg.Free;
end;

procedure TAutorun.SetAutorunCurrentReg(const Value: Boolean);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(KEY_WRITE);
  Reg.RootKey := HKEY_CURRENT_USER;
  Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion', True);
  Reg.WriteString('Run', FAppName, FAppCommand);
  Reg.Free;
end;

function TAutorun.GetAutorunLocalReg: Boolean;
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(KEY_READ);
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Run\') then
  begin
    Result := Reg.ValueExists(FAppName);
  end
  else
  begin
    Reg.Free;
    raise Exception.Create('Ошибка при чтении данных из реестра');
  end;
  Reg.Free;
end;

procedure TAutorun.SetAutorunLocalReg(const Value: Boolean);
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(KEY_WRITE);
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion', True);
  Reg.WriteString('Run', FAppName, FAppCommand);
  Reg.Free;
end;

end.

