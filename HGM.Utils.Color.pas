unit HGM.Utils.Color;

interface

uses
  System.Types, System.StrUtils, System.SysUtils, System.UIConsts,
  System.UITypes;

type
  TRGB = record
  private
    FB: Byte;
    FG: Byte;
    FColor: TColor;
    FR: Byte;
    procedure SetB(const Value: Byte);
    procedure SetColor(const Value: TColor);
    procedure SetG(const Value: Byte);
    procedure SetR(const Value: Byte);
  public
    class operator Implicit(const Color: TColor): TRGB;
    class operator Implicit(const RGB: TRGB): TColor;
    class operator Implicit(const RGB: TRGB): string;
    class function Create(const RGB: Cardinal): TRGB; overload; static;
    class function Create(const R, G, B: Byte): TRGB; overload; static;
    procedure SetRGB(const R, G, B: Byte);
    property Color: TColor read FColor write SetColor;
    property R: Byte read FR write SetR;
    property G: Byte read FG write SetG;
    property B: Byte read FB write SetB;
  end;

  TCMYK = record
  private
    FK: Byte;
    FM: Byte;
    FC: Byte;
    FColor: TColor;
    FY: Byte;
    procedure SetC(const Value: Byte);
    procedure SetColor(const Value: TColor);
    procedure SetK(const Value: Byte);
    procedure SetM(const Value: Byte);
    procedure SetY(const Value: Byte);
  public
    class operator Implicit(const Color: TColor): TCMYK;
    class operator Implicit(const CMYK: TCMYK): TColor;
    class function Create(const C, M, Y, K: Byte): TCMYK; static;
    procedure SetCMYK(const C, M, Y, K: Byte);
    property Color: TColor read FColor write SetColor;
    property C: Byte read FC write SetC;
    property M: Byte read FM write SetM;
    property Y: Byte read FY write SetY;
    property K: Byte read FK write SetK;
  end;

  THSV = record
  private
    FColor: TColor;
    FH: Double;
    FS: Double;
    FV: Double;
    procedure SetColor(const Value: TColor);
    procedure SetH(const Value: Double);
    procedure SetS(const Value: Double);
    procedure SetV(const Value: Double);
  public
    class operator Implicit(const Color: TColor): THSV;
    class operator Implicit(const HSV: THSV): TColor;
    class function Create(const H, S, V: Double): THSV; overload; static;
    procedure SetHSV(const H, S, V: Double);
    property Color: TColor read FColor write SetColor;
    property H: Double read FH write SetH;
    property S: Double read FS write SetS;
    property V: Double read FV write SetV;
  end;

function CMYKToColor(C, M, Y, K: Byte): TColor;

function RGBToColor(R, G, B: Byte): TColor;

function GetRValue(RGB: Cardinal): Byte;

function GetGValue(RGB: Cardinal): Byte;

function GetBValue(RGB: Cardinal): Byte;

function GetAValue(RGB: Cardinal): Byte;

function ColorToRGB(Color: TColor): TRGB;

function GrayColor(Color: TColor): TColor;

function RGBToHSV(R, G, B: Byte; var H, S, V: Double): Boolean;

function HSVToRGB(H, S, V: Double; var R, G, B: Byte): Boolean;

function HSVToColor(H, S, V: Double; var Value: TColor): Boolean;

procedure RGBToCMYK(const R, G, B: Byte; var C: Byte; var M: Byte; var Y: Byte; var K: Byte);

procedure CMYKToRGB(C, M, Y, K: Byte; var R: Byte; var G: Byte; var B: Byte);

procedure ColorCorrectCMYK(var C: Byte; var M: Byte; var Y: Byte; var K: Byte);

function HexToTColor(Value: string): TColor;

function ColorToHex(Color: TColor): string;

function ColorToHtml(Color: TColor): string;

function HtmlToColor(Color: string): TColor;

function ColorToString(Color: TColor): string;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.Math;

function ColorToString(Color: TColor): string;
begin
  Result := IntToHex(GetAValue(Color), 2) + IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue
    (Color), 2);
end;

function CMYKToColor(C, M, Y, K: Byte): TColor;
begin
  Result := (K or (Y shl 8) or (M shl 16) or (C shl 24));
end;

procedure ColorToCMYK(const Color: TColor; var C, M, Y, K: Byte);
var
  R, G, B: Byte;
begin
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  RGBToCMYK(R, G, B, C, M, Y, K);
end;

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (R or (G shl 8) or (B shl 16));
end;

function GetRValue(RGB: Cardinal): Byte;
begin
  Result := Byte(RGB);
end;

function GetGValue(RGB: Cardinal): Byte;
begin
  Result := Byte(RGB shr 8);
end;

function GetBValue(RGB: Cardinal): Byte;
begin
  Result := Byte(RGB shr 16);
end;

function GetAValue(RGB: Cardinal): Byte;
begin
  Result := Byte(RGB shr 24);
end;

function ColorToRGB(Color: TColor): TRGB;
begin
  Result := Color;
end;

function GrayColor(Color: TColor): TColor;
var
  Gr: Byte;
begin
  Gr := Trunc((GetBValue(Color) + GetGValue(Color) + GetRValue(Color)) / 3);
  Result := RGBToColor(Gr, Gr, Gr);
end;

function RGBToHSV(R, G, B: Byte; var H, S, V: Double): Boolean;
var
  minRGB, maxRGB, delta: Double;
begin
  H := 0.0;
  minRGB := Min(Min(R, G), B);
  maxRGB := Max(Max(R, G), B);
  delta := (maxRGB - minRGB);
  V := maxRGB;
  if (maxRGB <> 0.0) then
    S := 255.0 * delta / maxRGB
  else
    S := 0.0;

  if (S <> 0.0) then
  begin
    if R = maxRGB then
      H := (G - B) / delta
    else if G = maxRGB then
      H := 2.0 + (B - R) / delta
    else if B = maxRGB then
      H := 4.0 + (R - G) / delta
  end
  else
    H := -1.0;
  H := H * 60;
  if H < 0.0 then
    H := H + 360.0;

  S := S / 255 * 100;
  V := V / 255 * 100;

  Result := True;
end;

function HSVToRGB(H, S, V: Double; var R, G, B: Byte): Boolean;
var
  i: Integer;
  f, p, q, t: Double;

  procedure CopyOutput(const RV, GV, BV: Double);
  const
    RGBmax = 255;
  begin
    R := Round(RGBmax * RV);
    G := Round(RGBmax * GV);
    B := Round(RGBmax * BV);
  end;

begin
  S := S / 100;
  V := V / 100;
  H := H / 60;
 //Assert(InRange(H, 0.0, 1.0));
 //Assert(InRange(S, 0.0, 1.0));
 //Assert(InRange(V, 0.0, 1.0));
  if S = 0.0 then
  begin
    CopyOutput(B, B, B);
    Result := True;
    Exit;
  end;
 //H:=H*6.0;
  i := floor(H);
  f := H - i;
  p := V * (1.0 - S);
  q := V * (1.0 - S * f);
  t := V * (1.0 - S * (1.0 - f));
  case i of
    0:
      CopyOutput(V, t, p);
    1:
      CopyOutput(q, V, p);
    2:
      CopyOutput(p, V, t);
    3:
      CopyOutput(p, q, V);
    4:
      CopyOutput(t, p, V);
  else
    CopyOutput(V, p, q);
  end;
  Result := True;
end;

function HSVToColor(H, S, V: Double; var Value: TColor): Boolean;
var
  R, G, B: Byte;
begin
  Result := HSVToRGB(H, S, V, R, G, B);
  if Result then
    Value := RGBToColor(R, G, B)
  else
    Value := 0;
end;

procedure RGBToCMYK(const R, G, B: Byte; var C: Byte; var M: Byte; var Y: Byte; var K: Byte);
begin
  C := 255 - R;
  M := 255 - G;
  Y := 255 - B;
  if C < M then
    K := C
  else
    K := M;
  if Y < K then
    K := Y;
  if K > 0 then
  begin
    C := C - K;
    M := M - K;
    Y := Y - K;
  end;
end;

procedure CMYKToRGB(C, M, Y, K: Byte; var R: Byte; var G: Byte; var B: Byte);
begin
  if (Integer(C) + Integer(K)) < 255 then
    R := 255 - (C + K)
  else
    R := 0;
  if (Integer(M) + Integer(K)) < 255 then
    G := 255 - (M + K)
  else
    G := 0;
  if (Integer(Y) + Integer(K)) < 255 then
    B := 255 - (Y + K)
  else
    B := 0;
end;

procedure ColorCorrectCMYK(var C: Byte; var M: Byte; var Y: Byte; var K: Byte);
var
  MinColor: Byte;
begin
  if C < M then
    MinColor := C
  else
    MinColor := M;
  if Y < MinColor then
    MinColor := Y;
  if MinColor + K > 255 then
    MinColor := 255 - K;
  C := C - MinColor;
  M := M - MinColor;
  Y := Y - MinColor;
  K := K + MinColor;
end;

function HexToTColor(Value: string): TColor;
begin
  Result := RGBToColor(StrToInt('$' + Copy(Value, 1, 2)), StrToInt('$' + Copy(Value, 3, 2)), StrToInt('$' + Copy(Value, 5, 2)));
end;

function ColorToHex(Color: TColor): string;
begin
  Result := IntToHex(GetRValue(Color), 2) + IntToHex(GetGValue(Color), 2) + IntToHex(GetBValue(Color), 2);
end;

function ColorToHtml(Color: TColor): string;
begin
  Result := '#' + ColorToHex(Color);
end;

function HtmlToColor(Color: string): TColor;
begin
  Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4, 2) + Copy(Color, 2, 2));
end;

{ TRGB }

class operator TRGB.Implicit(const Color: TColor): TRGB;
begin
  {$IFDEF MSWINDOWS}
  if Color < 0 then
    Result.Color := GetSysColor(Color and $000000FF)
  else
  {$ENDIF}
    Result.Color := Color;
end;

class function TRGB.Create(const RGB: Cardinal): TRGB;
begin
  Result.Color := RGB;
end;

class function TRGB.Create(const R, G, B: Byte): TRGB;
begin
  Result.SetRGB(R, G, B);
end;

class operator TRGB.Implicit(const RGB: TRGB): string;
begin
  Result := IntToStr(RGB.Color);
end;

class operator TRGB.Implicit(const RGB: TRGB): TColor;
begin
  Result := RGB.Color;
end;

procedure TRGB.SetColor(const Value: TColor);
begin
  FColor := Value;
  FR := GetRValue(Value);
  FG := GetGValue(Value);
  FB := GetBValue(Value);
end;

procedure TRGB.SetB(const Value: Byte);
begin
  FB := Value;
  FColor := RGBToColor(FR, FG, FB);
end;

procedure TRGB.SetG(const Value: Byte);
begin
  FG := Value;
  FColor := RGBToColor(FR, FG, FB);
end;

procedure TRGB.SetR(const Value: Byte);
begin
  FR := Value;
  FColor := RGBToColor(FR, FG, FB);
end;

procedure TRGB.SetRGB(const R, G, B: Byte);
begin
  FR := R;
  FG := G;
  FB := B;
  FColor := RGBToColor(R, G, B);
end;

{ TCMYK }

class function TCMYK.Create(const C, M, Y, K: Byte): TCMYK;
begin
  Result.SetCMYK(C, M, Y, K);
end;

class operator TCMYK.Implicit(const CMYK: TCMYK): TColor;
begin
  Result := CMYK.Color;
end;

class operator TCMYK.Implicit(const Color: TColor): TCMYK;
begin
  Result.Color := Color;
end;

procedure TCMYK.SetColor(const Value: TColor);
begin
  FColor := Value;
  ColorToCMYK(FColor, FC, FM, FY, FK);
end;

procedure TCMYK.SetC(const Value: Byte);
begin
  FC := Value;
  FColor := CMYKToColor(FC, FM, FY, FK);
end;

procedure TCMYK.SetCMYK(const C, M, Y, K: Byte);
begin
  FC := C;
  FM := M;
  FY := Y;
  FK := K;
  FColor := CMYKToColor(C, M, Y, K);
end;

procedure TCMYK.SetK(const Value: Byte);
begin
  FK := Value;
  FColor := CMYKToColor(FC, FM, FY, FK);
end;

procedure TCMYK.SetM(const Value: Byte);
begin
  FM := Value;
  FColor := CMYKToColor(FC, FM, FY, FK);
end;

procedure TCMYK.SetY(const Value: Byte);
begin
  FY := Value;
  FColor := CMYKToColor(FC, FM, FY, FK);
end;

{ THSV }

class function THSV.Create(const H, S, V: Double): THSV;
begin
  Result.SetHSV(H, S, V);
end;

class operator THSV.Implicit(const Color: TColor): THSV;
begin
  Result.Color := Color;
end;

class operator THSV.Implicit(const HSV: THSV): TColor;
begin
  Result := HSV.Color;
end;

procedure THSV.SetColor(const Value: TColor);
var
  RGB: TRGB;
begin
  FColor := Value;
  RGB := ColorToRGB(FColor);
  RGBToHSV(RGB.R, RGB.G, RGB.B, FH, FS, FV);
end;

procedure THSV.SetHSV(const H, S, V: Double);
var
  R, G, B: Byte;
begin
  FH := H;
  FS := S;
  FV := V;
  if HSVToRGB(H, S, V, R, G, B) then
    FColor := RGBToColor(R, G, B)
  else
    FColor := 0;
end;

procedure THSV.SetH(const Value: Double);
begin
  FH := Value;
  if not HSVToColor(FH, FS, FV, FColor) then
    FColor := 0;
end;

procedure THSV.SetS(const Value: Double);
begin
  FS := Value;
  if not HSVToColor(FH, FS, FV, FColor) then
    FColor := 0;
end;

procedure THSV.SetV(const Value: Double);
begin
  FV := Value;
  if not HSVToColor(FH, FS, FV, FColor) then
    FColor := 0;
end;

end.

