unit PNGWork;

interface

uses Windows, Messages, SysUtils, Graphics, Classes, pngimage, System.Types;

procedure DrawTo(X, Y:Integer; Src, Dest:TPngImage);
procedure CopyFrom(SrcPt, DestPt, Size:TPoint; Src:TPngImage; var Dest:TPngImage);
function MixBytes(FG, BG, TRANS:Byte):Byte;
function MixColors(FG, BG:TColor; T:Byte):TColor;
function CreateFrom(X, Y, W, H:Word; Src:TPngImage):TPngImage;
function CreatePNG(FName:string):TPngImage; overload;
function CreatePNG(Dll:Cardinal; ID:string):TPngImage; overload;
procedure PNGColored(X, Y:Integer; Src, Dest:TPngImage; MColor:TColor);

implementation

function CreatePNG(Dll:Cardinal; ID:string):TPngImage;
begin
 Result:=TPngImage.Create;
 try
  Result.LoadFromResourceName(DLL, ID);
 except
  begin
   Result:=TPngImage.CreateBlank(COLOR_RGBALPHA, 16, 32, 32);
   MessageBox(0, 'Ошибка', PChar('Файл отсутствует: '+ID), MB_ICONSTOP or MB_OK);
  end;
 end;
end;

function CreatePNG(FName:string):TPngImage;
begin
 Result:=TPngImage.Create;
 try
  Result.LoadFromFile(FName)
 except
  begin
   Result:=TPngImage.CreateBlank(COLOR_RGBALPHA, 16, 32, 32);
   MessageBox(0, 'Ошибка', PChar('Файл отсутствует: '+FName), MB_ICONSTOP or MB_OK);
  end;
 end;
end;

function CreateFrom(X, Y, W, H:Word; Src:TPngImage):TPngImage;
begin
 Result:=TPngImage.CreateBlank(COLOR_RGBALPHA, 16, W, H);
 CopyFrom(Point(X, Y), Point(0, 0), Point(W, H), Src, Result);
end;

//Автор: Я
procedure CopyFrom(SrcPt, DestPt, Size:TPoint; Src:TPngImage; var Dest:TPngImage);
var X, Y:Integer;
    DAS, SAS:pByteArray;
begin
 Size.X:=Size.X - 1;
 Size.Y:=Size.Y - 1;
 if DestPt.X + Size.X > Dest.Width  then Size.X:=Dest.Width  - DestPt.X;
 if DestPt.Y + Size.Y > Dest.Height then Size.Y:=Dest.Height - DestPt.Y;
 for Y:=0 to Size.Y - 1 do
  begin
   DAS:=Dest.AlphaScanline[Y + DestPt.Y];
   SAS:=Src.AlphaScanline[Y + SrcPt.Y];
   for X:=0 to Size.X - 1 do
    begin
     Dest.Canvas.Pixels[X + DestPt.X, Y + DestPt.Y]:=Src.Canvas.Pixels[X + SrcPt.X, Y + SrcPt.Y];
     DAS^[X + DestPt.X]:=SAS^[X + SrcPt.X];
    end;
  end;
end;

//Автор: http://www.swissdelphicenter.ch
function MixBytes(FG, BG, TRANS:Byte):Byte;
asm
 push bx       // Push some regs
 push cx
 push dx
 mov DH, TRANS // Remembering Transparency value (or Opacity - as you like)
 mov BL, FG    // Filling registers with our values
 mov AL, DH    // BL = ForeGround (FG)
 mov CL, BG    // CL = BackGround (BG)
 xor AH, AH    // Clear High-order parts of regs
 xor BH, BH
 xor CH, CH
 mul BL        // AL=AL*BL
 mov BX, AX    // BX=AX
 xor AH, AH
 mov AL, DH
 xor AL, $FF   // AX=(255-TRANS)
 mul CL        // AL=AL*CL
 add AX, BX    // AX=AX+BX
 shr AX, 8     // Fine! Here we have mixed value in AL
 pop dx        // Hm... No rubbish after us, ok?
 pop cx
 pop bx        //Get out
end;

//Автор: http://www.swissdelphicenter.ch
function MixColors(FG, BG:TColor; T:Byte):TColor;
var R, G, B:Byte;
begin
 R:=MixBytes(FG and 255, BG and 255, T);
 G:=MixBytes((FG shr 8) and 255, (BG shr 8) and 255, T);
 B:=MixBytes((FG shr 16) and 255, (BG shr 16) and 255, T);
 Result:=R + G * 256 + B * 65536;
end;

//Автор: Я (очень медленная процедура ~300 мсек. для ср. рисунка)
procedure DrawTo(X, Y:Integer; Src, Dest:TPngImage);
var dX, dY:Integer;
    DAS, SAS:pByteArray;
begin
 for dY:=0 to Src.Height - 1 do
  begin
   DAS:=Dest.AlphaScanline[dY + Y];
   SAS:=Src.AlphaScanline[dY];
   for dX:=0 to Src.Width - 1 do
    begin
     if SAS^[dX] <= 0 then Continue;
     DAS[dX + X]:=SAS^[dX] + DAS^[dX + X];
     Dest.Canvas.Pixels[dX + X, dY + Y]:=MixColors(Src.Canvas.Pixels[dX, dY], Dest.Canvas.Pixels[dX + X, dY + Y], DAS^[dX + X]);
    end;
  end;
end;

procedure PNGColored(X, Y:Integer; Src, Dest:TPngImage; MColor:TColor);
var dX, dY:Integer;
    DAS, SAS:pByteArray;
begin
 for dY:=0 to Src.Height - 1 do
  begin
   DAS:=Dest.AlphaScanline[dY + Y];
   SAS:=Src.AlphaScanline[dY];
   for dX:=0 to Src.Width - 1 do
    begin
     if SAS^[dX] <= 0 then Continue;
     DAS[dX + X]:=SAS^[dX];// + DAS^[dX + X];
     Dest.Canvas.Pixels[dX + X, dY + Y]:=MColor;//, Dest.Canvas.Pixels[dX + X, dY + Y], DAS^[dX + X]);
    end;
  end;
end;

end.
