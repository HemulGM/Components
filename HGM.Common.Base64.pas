unit HGM.Common.Base64;

interface

uses
  Windows, System.SysUtils;

function Base64EncodeString(const InText: string): string;

function Base64DecodeString(const InText: string): string;

procedure Base64DecodeStr(const InText: string; var OutText: string);

procedure Base64EncodeStr(const InText: string; var OutText: string);

implementation

const
  cBase64Codec: array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64Filler: AnsiChar = '=';

type
  TAByte = array[0..MaxInt - 1] of Byte;

  TPAByte = ^TAByte;

function CalcEncodedSize(InSize: DWord): DWord;
begin
  Result := (InSize div 3) shl 2;
  if (InSize mod 3) > 0 then
    Inc(Result, 4);
end;

function CalcDecodedSize(const InBuffer; InSize: DWord): DWord;
begin
  Result := 0;
  if InSize = 0 then
    Exit;
  if (InSize mod 4 <> 0) then
    Exit;

  Result := InSize div 4 * 3;
  if (PByte(DWord(InBuffer) + InSize - 2)^ = Ord(Base64Filler)) then
    Dec(Result, 2)
  else if (PByte(DWord(InBuffer) + InSize - 1)^ = Ord(Base64Filler)) then
    Dec(Result);
end;

procedure Base64Encode(const InBuffer; InSize: DWord; var OutBuffer);
var
  X: Integer;
  PIn, POut: TPAByte;
  Acc: Cardinal;
begin
  if InSize > 0 then
  begin
    PIn := @InBuffer;
    POut := @OutBuffer;

    for X := 1 to InSize div 3 do
    begin
      Acc := PIn^[0] shl 16 + PIn^[1] shl 8 + PIn^[2];

      POut^[0] := Byte(cBase64Codec[(Acc shr 18) and $3f]);
      POut^[1] := Byte(cBase64Codec[(Acc shr 12) and $3f]);
      POut^[2] := Byte(cBase64Codec[(Acc shr 6) and $3f]);
      POut^[3] := Byte(cBase64Codec[(Acc) and $3f]);

      Inc(Cardinal(POut), 4);
      Inc(Cardinal(PIn), 3);
    end;
    case InSize mod 3 of
      1:
        begin
          Acc := PIn^[0] shl 16;

          POut^[0] := Byte(cBase64Codec[(Acc shr 18) and $3f]);
          POut^[1] := Byte(cBase64Codec[(Acc shr 12) and $3f]);
          POut^[2] := Byte(Base64Filler);
          POut^[3] := Byte(Base64Filler);
        end;
      2:
        begin
          Acc := PIn^[0] shl 16 + PIn^[1] shl 8;

          POut^[0] := Byte(cBase64Codec[(Acc shr 18) and $3f]);
          POut^[1] := Byte(cBase64Codec[(Acc shr 12) and $3f]);
          POut^[2] := Byte(cBase64Codec[(Acc shr 6) and $3f]);
          POut^[3] := Byte(Base64Filler);
        end;
    end;
  end;
end;

procedure Base64Decode(const InBuffer; InSize: DWord; var OutBuffer);
const
  cBase64Codec: array[0..255] of Byte = ($FF, $FF, $FF, $FF, $FF, {005>} $FF, $FF, $FF, $FF, $FF, // 000..009
    $FF, $FF, $FF, $FF, $FF, {015>} $FF, $FF, $FF, $FF, $FF, // 010..019
    $FF, $FF, $FF, $FF, $FF, {025>} $FF, $FF, $FF, $FF, $FF, // 020..029
    $FF, $FF, $FF, $FF, $FF, {035>} $FF, $FF, $FF, $FF, $FF, // 030..039
    $FF, $FF, $FF, $3E, $FF, {045>} $FF, $FF, $3F, $34, $35, // 040..049
    $36, $37, $38, $39, $3A, {055>} $3B, $3C, $3D, $FF, $FF, // 050..059
    $FF, $00, $FF, $FF, $FF, {065>} $00, $01, $02, $03, $04, // 060..069
    $05, $06, $07, $08, $09, {075>} $0A, $0B, $0C, $0D, $0E, // 070..079
    $0F, $10, $11, $12, $13, {085>} $14, $15, $16, $17, $18, // 080..089
    $19, $FF, $FF, $FF, $FF, {095>} $FF, $FF, $1A, $1B, $1C, // 090..099
    $1D, $1E, $1F, $20, $21, {105>} $22, $23, $24, $25, $26, // 100..109
    $27, $28, $29, $2A, $2B, {115>} $2C, $2D, $2E, $2F, $30, // 110..119
    $31, $32, $33, $FF, $FF, {125>} $FF, $FF, $FF, $FF, $FF, // 120..129
    $FF, $FF, $FF, $FF, $FF, {135>} $FF, $FF, $FF, $FF, $FF, // 130..139
    $FF, $FF, $FF, $FF, $FF, {145>} $FF, $FF, $FF, $FF, $FF, // 140..149
    $FF, $FF, $FF, $FF, $FF, {155>} $FF, $FF, $FF, $FF, $FF, // 150..159
    $FF, $FF, $FF, $FF, $FF, {165>} $FF, $FF, $FF, $FF, $FF, // 160..169
    $FF, $FF, $FF, $FF, $FF, {175>} $FF, $FF, $FF, $FF, $FF, // 170..179
    $FF, $FF, $FF, $FF, $FF, {185>} $FF, $FF, $FF, $FF, $FF, // 180..189
    $FF, $FF, $FF, $FF, $FF, {195>} $FF, $FF, $FF, $FF, $FF, // 190..199
    $FF, $FF, $FF, $FF, $FF, {205>} $FF, $FF, $FF, $FF, $FF, // 200..209
    $FF, $FF, $FF, $FF, $FF, {215>} $FF, $FF, $FF, $FF, $FF, // 210..219
    $FF, $FF, $FF, $FF, $FF, {225>} $FF, $FF, $FF, $FF, $FF, // 220..229
    $FF, $FF, $FF, $FF, $FF, {235>} $FF, $FF, $FF, $FF, $FF, // 230..239
    $FF, $FF, $FF, $FF, $FF, {245>} $FF, $FF, $FF, $FF, $FF, // 240..249
    $FF, $FF, $FF, $FF, $FF, {255>} $FF                      // 250..255
    );
var
  X, Y: Integer;
  PIn, POut: TPAByte;
  Acc: dword;
begin
  if (InSize > 0) and (InSize mod 4 = 0) then
  begin
    InSize := InSize shr 2;
    PIn := @InBuffer;
    POut := @OutBuffer;

    for X := 1 to InSize - 1 do
    begin
      Acc := 0;
      Y := -1;

      repeat
        Inc(Y);
        Acc := Acc shl 6;
        Acc := Acc or cBase64Codec[PIn^[Y]];
      until Y = 3;

      POut^[0] := Acc shr 16;
      POut^[1] := Byte(Acc shr 8);
      POut^[2] := Byte(Acc);

      Inc(Cardinal(PIn), 4);
      Inc(Cardinal(POut), 3);
    end;
    Acc := 0;
    Y := -1;

    repeat
      Inc(Y);
      Acc := Acc shl 6;

      if PIn^[Y] = Byte(Base64Filler) then
      begin
        if Y = 3 then
        begin
          POut^[0] := Acc shr 16;
          POut^[1] := Byte(Acc shr 8);
        end
        else
          POut^[0] := Acc shr 10;
        Exit;
      end;

      Acc := Acc or cBase64Codec[PIn^[Y]];
    until Y = 3;

    POut^[0] := Acc shr 16;
    POut^[1] := Byte(Acc shr 8);
    POut^[2] := Byte(Acc);
  end;
end;

procedure Base64EncodeStr(const InText: string; var OutText: string);
var
  InSize, OutSize: DWord;
  PIn, POut: Pointer;
  FOut, FIn: AnsiString;
begin
  FIn := AnsiString(InText);
  InSize := Length(FIn);
  OutSize := CalcEncodedSize(InSize);
  SetLength(FOut, OutSize);

  if OutSize > 0 then
  begin
    PIn := @FIn[1];
    POut := @FOut[1];

    Base64Encode(PIn^, InSize, POut^);
    OutText := string(FOut);
  end;
end;

procedure Base64DecodeStr(const InText: string; var OutText: string);
var
  InSize, OutSize: DWord;
  PIn, POut: Pointer;
  FOut, FIn: AnsiString;
begin
  FIn := AnsiString(InText);
  InSize := Length(FIn);
  PIn := @FIn[1];
  OutSize := CalcDecodedSize(PIn, InSize);
  SetLength(FOut, OutSize);
  if OutSize > 0 then
  begin
    FillChar(FOut[1], OutSize, '.');
    POut := @FOut[1];

    Base64Decode(PIn^, InSize, POut^);
    OutText := string(FOut);
  end;
end;

function Base64EncodeString(const InText: string): string;
begin
  Base64EncodeStr(InText, Result);
end;

function Base64DecodeString(const InText: string): string;
begin
  Base64DecodeStr(InText, Result);
end;

function Base64EncodeToString(const InBuffer; InSize: DWord): string;
var
  POut: Pointer;
begin
  SetLength(Result, CalcEncodedSize(InSize));
  POut := @Result[1];
  Base64Encode(InBuffer, InSize, POut^);
end;

end.

