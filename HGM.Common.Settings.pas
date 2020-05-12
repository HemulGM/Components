unit HGM.Common.Settings;

interface

uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes, IniFiles, Registry, System.Generics.Collections,
  {$IFDEF NEEDFMX}
  FMX.Forms,
  {$ELSE}
  Vcl.Forms, Vcl.Samples.Spin, Vcl.Grids, Vcl.ValEdit, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  {$ENDIF NEEDFMX}
  System.UITypes;

type
  TRegRoot = (rrHKLM, rrHKCU);

  TWindowParamSave = set of (wpsAll, wpsCoord, wpsSize, wpsState);

  TSettings = class
  private
    function ReadInt(Section, Param: string; var Value: Integer; Default: Integer = 0): Boolean; virtual; abstract;
    function ReadStr(Section, Param: string; var Value: string; Default: string = ''): Boolean; virtual; abstract;
    function ReadBool(Section, Param: string; var Value: Boolean; Default: Boolean = False): Boolean; virtual; abstract;
    function ReadDate(Section, Param: string; var Value: TDateTime; Default: TDateTime = 0): Boolean; virtual; abstract;
    function ReadFloat(Section, Param: string; var Value: Extended; Default: Extended = 0.0): Boolean; virtual; abstract;
    function WriteInt(Section, Param: string; Value: Integer): Boolean; virtual; abstract;
    function WriteStr(Section, Param: string; Value: string): Boolean; virtual; abstract;
    function WriteBool(Section, Param: string; Value: Boolean): Boolean; virtual; abstract;
    function WriteDate(Section, Param: string; Value: TDateTime): Boolean; virtual; abstract;
    function WriteFloat(Section, Param: string; Value: Extended): Boolean; virtual; abstract;
    function FValueExists(Section, Param: string): Boolean; virtual; abstract;
    function FGetSections(Section: string; List: TStringList): Boolean; virtual; abstract;
  public
    //function Get<T: TEnum>(Section, Param: string; Default: T): T;
    function GetInt(Section, Param: string; Default: Integer = 0): Integer;
    function GetStr(Section, Param: string; Default: string = ''): string;
    function GetBool(Section, Param: string; Default: Boolean = False): Boolean;
    function GetDate(Section, Param: string; Default: TDateTime = 0): TDateTime;
    function GetFloat(Section, Param: string; Default: Extended = 0.0): Extended;
    function SetInt(Section, Param: string; Value: Integer): Boolean;
    function SetStr(Section, Param: string; Value: string): Boolean;
    function SetBool(Section, Param: string; Value: Boolean): Boolean;
    function SetDate(Section, Param: string; Value: TDateTime): Boolean;
    function SetFloat(Section, Param: string; Value: Extended): Boolean;
    {$IFDEF NEEDFMX}
    function GetParamWindow(Section: string; AWindow: FMX.Forms.TForm; LoadItems: TWindowParamSave): Boolean; overload;
    function SetParamWindow(Section: string; AWindow: FMX.Forms.TForm; SaveItems: TWindowParamSave): Boolean; overload;
    {$ELSE}
    function GetParam(Section: string; AControl: TEdit; Default: string = ''): Boolean; overload;
    function GetParam(Section: string; AControl: TLabel; Default: string = ''): Boolean; overload;
    function GetParam(Section: string; AControl: TSpinEdit; Default: Integer = 0): Boolean; overload;
    function GetParam(Section: string; AControl: TCustomComboBox; Default: Integer = 0): Boolean; overload;
    function GetParam(Section: string; AControl: TCheckBox; Default: Boolean = False): Boolean; overload;
    function GetParam(Section: string; AControl: TPageControl; Default: Integer = 0): Boolean; overload;
    function SetParam(Section: string; AControl: TEdit): Boolean; overload;
    function SetParam(Section: string; AControl: TLabel): Boolean; overload;
    function SetParam(Section: string; AControl: TSpinEdit): Boolean; overload;
    function SetParam(Section: string; AControl: TCustomComboBox): Boolean; overload;
    function SetParam(Section: string; AControl: TCheckBox): Boolean; overload;
    function SetParam(Section: string; AControl: TPageControl): Boolean; overload;
    function GetParamWindow(Section: string; AWindow: Vcl.Forms.TForm; LoadItems: TWindowParamSave): Boolean; overload;
    function SetParamWindow(Section: string; AWindow: Vcl.Forms.TForm; SaveItems: TWindowParamSave): Boolean; overload;
    {$ENDIF NEEDFMX}

    function ValueExists(Section, Param: string): Boolean;
    function GetSections(Section: string; List: TStringList): Boolean;
  end;

  TSettingsReg = class(TSettings)
  private
    FRoot: TRegRoot;
    FPath: string;
    function CreateReg(Access: Cardinal; Root: HKEY): TRegistry;
    function ReadInt(Section, Param: string; var Value: Integer; Default: Integer = 0): Boolean; override;
    function ReadStr(Section, Param: string; var Value: string; Default: string = ''): Boolean; override;
    function ReadBool(Section, Param: string; var Value: Boolean; Default: Boolean = False): Boolean; override;
    function ReadDate(Section, Param: string; var Value: TDateTime; Default: TDateTime = 0): Boolean; override;
    function ReadFloat(Section, Param: string; var Value: Extended; Default: Extended = 0.0): Boolean; override;
    function WriteInt(Section, Param: string; Value: Integer): Boolean; override;
    function WriteStr(Section, Param: string; Value: string): Boolean; override;
    function WriteBool(Section, Param: string; Value: Boolean): Boolean; override;
    function WriteDate(Section, Param: string; Value: TDateTime): Boolean; override;
    function WriteFloat(Section, Param: string; Value: Extended): Boolean; override;
    function FValueExists(Section, Param: string): Boolean; override;
    function FGetSections(Section: string; List: TStringList): Boolean; override;
  public
    constructor Create(ARoot: TRegRoot; APath: string);
    property Root: TRegRoot read FRoot write FRoot;
    property Path: string read FPath write FPath;
  end;

  TSettingsIni = class(TSettings)
  private
    FFileName: string;
    function CreateIni: TIniFile;
    function ReadInt(Section, Param: string; var Value: Integer; Default: Integer = 0): Boolean; override;
    function ReadStr(Section, Param: string; var Value: string; Default: string = ''): Boolean; override;
    function ReadBool(Section, Param: string; var Value: Boolean; Default: Boolean = False): Boolean; override;
    function ReadDate(Section, Param: string; var Value: TDateTime; Default: TDateTime = 0): Boolean; override;
    function ReadFloat(Section, Param: string; var Value: Extended; Default: Extended = 0.0): Boolean; override;
    function WriteInt(Section, Param: string; Value: Integer): Boolean; override;
    function WriteStr(Section, Param: string; Value: string): Boolean; override;
    function WriteBool(Section, Param: string; Value: Boolean): Boolean; override;
    function WriteDate(Section, Param: string; Value: TDateTime): Boolean; override;
    function WriteFloat(Section, Param: string; Value: Extended): Boolean; override;
    function FValueExists(Section, Param: string): Boolean; override;
    function FGetSections(Section: string; List: TStringList): Boolean; override;
  public
    constructor Create(AFileName: string);
    property FileName: string read FFileName write FFileName;
  end;

function RegRootToHKEY(Value: TRegRoot): HKEY;

implementation

function RegRootToHKEY(Value: TRegRoot): HKEY;
begin
  case Value of
    rrHKLM:
      Result := HKEY_LOCAL_MACHINE;
    rrHKCU:
      Result := HKEY_CURRENT_USER;
  else
    Result := HKEY_CURRENT_USER;
  end;
end;

{ TSettingsReg }

constructor TSettingsReg.Create(ARoot: TRegRoot; APath: string);
begin
  inherited Create;
  FRoot := ARoot;
  FPath := APath;
end;

function TSettingsReg.CreateReg(Access: Cardinal; Root: HKEY): TRegistry;
begin
  Result := TRegistry.Create;
  Result.Access := Access;
  Result.RootKey := Root;
end;

function TSettingsReg.FGetSections(Section: string; List: TStringList): Boolean;
begin
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        GetKeyNames(List);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.FValueExists(Section, Param: string): Boolean;
begin
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        Result := ValueExists(Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.ReadBool(Section, Param: string; var Value: Boolean; Default: Boolean): Boolean;
begin
  Value := Default;
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        Value := ReadBool(Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.ReadDate(Section, Param: string; var Value: TDateTime; Default: TDateTime): Boolean;
begin
  Value := Default;
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        Value := ReadDateTime(Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.ReadFloat(Section, Param: string; var Value: Extended; Default: Extended): Boolean;
begin
  Value := Default;
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        Value := ReadFloat(Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.ReadInt(Section, Param: string; var Value: Integer; Default: Integer): Boolean;
begin
  Value := Default;
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        Value := ReadInteger(Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.ReadStr(Section, Param: string; var Value: string; Default: string): Boolean;
begin
  Value := Default;
  with CreateReg(KEY_READ, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKeyReadOnly(Path + '\' + Section);
      if Result then
        Value := ReadString(Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.WriteBool(Section, Param: string; Value: Boolean): Boolean;
begin
  with CreateReg(KEY_WRITE, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKey(Path + '\' + Section, True);
      if Result then
        WriteBool(Param, Value);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.WriteDate(Section, Param: string; Value: TDateTime): Boolean;
begin
  with CreateReg(KEY_WRITE, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKey(Path + '\' + Section, True);
      if Result then
        WriteDateTime(Param, Value);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.WriteFloat(Section, Param: string; Value: Extended): Boolean;
begin
  with CreateReg(KEY_WRITE, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKey(Path + '\' + Section, True);
      if Result then
        WriteFloat(Param, Value);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.WriteInt(Section, Param: string; Value: Integer): Boolean;
begin
  with CreateReg(KEY_WRITE, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKey(Path + '\' + Section, True);
      if Result then
        WriteInteger(Param, Value);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsReg.WriteStr(Section, Param, Value: string): Boolean;
begin
  with CreateReg(KEY_WRITE, RegRootToHKEY(FRoot)) do
  begin
    try
      Result := OpenKey(Path + '\' + Section, True);
      if Result then
        WriteString(Param, Value);
    except
      Result := False;
    end;
    Free;
  end;
end;

{ TSettingsIni }

constructor TSettingsIni.Create(AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
end;

function TSettingsIni.CreateIni: TIniFile;
begin
  if not FileExists(FileName) then
    FileClose(FileCreate(FileName));
  Result := TIniFile.Create(FileName);
end;

function TSettingsIni.FGetSections(Section: string; List: TStringList): Boolean;
begin
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        ReadSections(List);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.FValueExists(Section, Param: string): Boolean;
begin
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        Result := ValueExists(Section, Param);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.ReadBool(Section, Param: string; var Value: Boolean; Default: Boolean): Boolean;
begin
  Value := Default;
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        Value := ReadBool(Section, Param, Default);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.ReadDate(Section, Param: string; var Value: TDateTime; Default: TDateTime): Boolean;
begin
  Value := Default;
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        Value := ReadDateTime(Section, Param, Default);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.ReadFloat(Section, Param: string; var Value: Extended; Default: Extended): Boolean;
begin
  Value := Default;
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        Value := ReadFloat(Section, Param, Default);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.ReadInt(Section, Param: string; var Value: Integer; Default: Integer): Boolean;
begin
  Value := Default;
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        Value := ReadInteger(Section, Param, Default);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.ReadStr(Section, Param: string; var Value: string; Default: string): Boolean;
begin
  Value := Default;
  with CreateIni do
  begin
    try
      Result := FileExists(FileName);
      if Result then
        Value := ReadString(Section, Param, Default);
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.WriteBool(Section, Param: string; Value: Boolean): Boolean;
begin
  with CreateIni do
  begin
    try
      WriteBool(Section, Param, Value);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.WriteDate(Section, Param: string; Value: TDateTime): Boolean;
begin
  with CreateIni do
  begin
    try
      WriteDateTime(Section, Param, Value);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.WriteFloat(Section, Param: string; Value: Extended): Boolean;
begin
  with CreateIni do
  begin
    try
      WriteFloat(Section, Param, Value);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.WriteInt(Section, Param: string; Value: Integer): Boolean;
begin
  with CreateIni do
  begin
    try
      WriteInteger(Section, Param, Value);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

function TSettingsIni.WriteStr(Section, Param, Value: string): Boolean;
begin
  with CreateIni do
  begin
    try
      WriteString(Section, Param, Value);
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

{ TSettings }
   {
function TSettings.Get<T>(Section, Param: string; Default: T): T;
var Res: Integer;
    It: System.TFloatSpecial;
begin
  ReadInt(Section, Param, Res, Ord(T));

  Result := T(Res);
end;   }

function TSettings.GetBool(Section, Param: string; Default: Boolean): Boolean;
begin
  ReadBool(Section, Param, Result, Default);
end;

function TSettings.GetDate(Section, Param: string; Default: TDateTime): TDateTime;
begin
  ReadDate(Section, Param, Result, Default);
end;

function TSettings.GetFloat(Section, Param: string; Default: Extended): Extended;
begin
  ReadFloat(Section, Param, Result, Default);
end;

function TSettings.GetInt(Section, Param: string; Default: Integer): Integer;
begin
  ReadInt(Section, Param, Result, Default);
end;

function TSettings.GetSections(Section: string; List: TStringList): Boolean;
begin
  Result := FGetSections(Section, List);
end;

function TSettings.GetStr(Section, Param, Default: string): string;
begin
  ReadStr(Section, Param, Result, Default);
end;

function TSettings.SetBool(Section, Param: string; Value: Boolean): Boolean;
begin
  Result := WriteBool(Section, Param, Value);
end;

function TSettings.SetDate(Section, Param: string; Value: TDateTime): Boolean;
begin
  Result := WriteDate(Section, Param, Value);
end;

function TSettings.SetFloat(Section, Param: string; Value: Extended): Boolean;
begin
  Result := WriteFloat(Section, Param, Value);
end;

function TSettings.SetInt(Section, Param: string; Value: Integer): Boolean;
begin
  Result := WriteInt(Section, Param, Value);
end;

function TSettings.SetStr(Section, Param, Value: string): Boolean;
begin
  Result := WriteStr(Section, Param, Value);
end;

function TSettings.ValueExists(Section, Param: string): Boolean;
begin
  Result := FValueExists(Section, Param);
end;

{$IFNDEF NEEDFMX}
function TSettings.GetParam(Section: string; AControl: TEdit; Default: string = ''): Boolean;
var
  Str: string;
begin
  Result := ReadStr(Section, AControl.Name, Str, Default);
  AControl.Text := Str;
end;

function TSettings.SetParam(Section: string; AControl: TEdit): Boolean;
begin
  Result := WriteStr(Section, AControl.Name, AControl.Text);
end;

function TSettings.GetParam(Section: string; AControl: TLabel; Default: string): Boolean;
var
  Str: string;
begin
  Result := ReadStr(Section, AControl.Name, Str, Default);
  AControl.Caption := Str;
end;

function TSettings.SetParam(Section: string; AControl: TLabel): Boolean;
begin
  Result := WriteStr(Section, AControl.Name, AControl.Caption);
end;

function TSettings.GetParam(Section: string; AControl: TSpinEdit; Default: Integer = 0): Boolean;
var
  Value: Integer;
begin
  Result := ReadInt(Section, AControl.Name, Value, Default);
  AControl.Value := Value;
end;

function TSettings.SetParam(Section: string; AControl: TSpinEdit): Boolean;
begin
  Result := WriteInt(Section, AControl.Name, AControl.Value);
end;

function TSettings.GetParam(Section: string; AControl: TCheckBox; Default: Boolean): Boolean;
var
  Value: Boolean;
begin
  Result := ReadBool(Section, AControl.Name, Value, Default);
  AControl.Checked := Value;
end;

function TSettings.GetParam(Section: string; AControl: TCustomComboBox; Default: Integer): Boolean;
var
  Value: Integer;
begin
  Result := ReadInt(Section, AControl.Name, Value, Default);
  AControl.ItemIndex := Value;
end;

function TSettings.SetParam(Section: string; AControl: TCheckBox): Boolean;
begin
  Result := WriteBool(Section, AControl.Name, AControl.Checked);
end;

function TSettings.SetParam(Section: string; AControl: TCustomComboBox): Boolean;
begin
  Result := WriteInt(Section, AControl.Name, AControl.ItemIndex);
end;

function TSettings.GetParam(Section: string; AControl: TPageControl; Default: Integer): Boolean;
var
  Value: Integer;
begin
  Result := ReadInt(Section, AControl.Name, Value, Default);
  AControl.ActivePageIndex := Value;
end;

function TSettings.SetParam(Section: string; AControl: TPageControl): Boolean;
begin
  Result := WriteInt(Section, AControl.Name, AControl.ActivePageIndex);
end;
{$ENDIF}

{$IFDEF NEEDFMX}

function TSettings.GetParamWindow(Section: string; AWindow: FMX.Forms.TForm; LoadItems: TWindowParamSave): Boolean;
var
  IValue: Integer;
begin
  try
    if (wpsCoord in LoadItems) or (wpsAll in LoadItems) then
    begin
      if ReadInt(Section, AWindow.Name + '.Left', IValue, AWindow.Left) then
        AWindow.Left := IValue;
      if ReadInt(Section, AWindow.Name + '.Top', IValue, AWindow.Top) then
        AWindow.Top := IValue;
    end;
    if (wpsSize in LoadItems) or (wpsAll in LoadItems) then
    begin
      if ReadInt(Section, AWindow.Name + '.Width', IValue, AWindow.Width) then
        AWindow.Width := IValue;
      if ReadInt(Section, AWindow.Name + '.Height', IValue, AWindow.Height) then
        AWindow.Height := IValue;
    end;
    if (wpsState in LoadItems) or (wpsAll in LoadItems) then
    begin
      if ReadInt(Section, AWindow.Name + '.WindowState', IValue, Ord(AWindow.WindowState)) then
        AWindow.WindowState := TWindowState(IValue);
      if AWindow.WindowState = TWindowState.wsMinimized then
        AWindow.WindowState := TWindowState.wsNormal;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TSettings.SetParamWindow(Section: string; AWindow: FMX.Forms.TForm; SaveItems: TWindowParamSave): Boolean;
begin
  try
    if (wpsState in SaveItems) or (wpsAll in SaveItems) then
    begin
      WriteInt(Section, AWindow.Name + '.WindowState', Ord(AWindow.WindowState));
    end;
    if AWindow.WindowState = TWindowState.wsNormal then
    begin
      if (wpsCoord in SaveItems) or (wpsAll in SaveItems) then
      begin
        WriteInt(Section, AWindow.Name + '.Left', AWindow.Left);
        WriteInt(Section, AWindow.Name + '.Top', AWindow.Top);
      end;
      if (wpsSize in SaveItems) or (wpsAll in SaveItems) then
      begin
        WriteInt(Section, AWindow.Name + '.Width', AWindow.Width);
        WriteInt(Section, AWindow.Name + '.Height', AWindow.Height);
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;
{$ELSE}

function TSettings.GetParamWindow(Section: string; AWindow: Vcl.Forms.TForm; LoadItems: TWindowParamSave): Boolean;
var
  IValue: Integer;
begin
  try
    if (wpsCoord in LoadItems) or (wpsAll in LoadItems) then
    begin
      if ReadInt(Section, AWindow.Name + '.Left', IValue, AWindow.Left) then
        AWindow.Left := IValue;
      if ReadInt(Section, AWindow.Name + '.Top', IValue, AWindow.Top) then
        AWindow.Top := IValue;
    end;
    if (wpsSize in LoadItems) or (wpsAll in LoadItems) then
    begin
      if ReadInt(Section, AWindow.Name + '.Width', IValue, AWindow.Width) then
        AWindow.Width := IValue;
      if ReadInt(Section, AWindow.Name + '.Height', IValue, AWindow.Height) then
        AWindow.Height := IValue;
    end;
    if (wpsState in LoadItems) or (wpsAll in LoadItems) then
    begin
      if ReadInt(Section, AWindow.Name + '.WindowState', IValue, Ord(AWindow.WindowState)) then
        AWindow.WindowState := TWindowState(IValue);
      if AWindow.WindowState = wsMinimized then
        AWindow.WindowState := wsNormal;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TSettings.SetParamWindow(Section: string; AWindow: Vcl.Forms.TForm; SaveItems: TWindowParamSave): Boolean;
begin
  try
    if (wpsState in SaveItems) or (wpsAll in SaveItems) then
    begin
      WriteInt(Section, AWindow.Name + '.WindowState', Ord(AWindow.WindowState));
    end;
    if AWindow.WindowState = wsNormal then
    begin
      if (wpsCoord in SaveItems) or (wpsAll in SaveItems) then
      begin
        WriteInt(Section, AWindow.Name + '.Left', AWindow.Left);
        WriteInt(Section, AWindow.Name + '.Top', AWindow.Top);
      end;
      if (wpsSize in SaveItems) or (wpsAll in SaveItems) then
      begin
        WriteInt(Section, AWindow.Name + '.Width', AWindow.Width);
        WriteInt(Section, AWindow.Name + '.Height', AWindow.Height);
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;
{$ENDIF NEEDFMX}

end.

