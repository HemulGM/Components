unit HGM.BlurGaussian;

interface

uses
 Windows, Graphics, SysUtils;

type
 PRGBTriple = ^TRGBTriple;
 TRGBTriple = packed record
  R:Byte;
  G:Byte;
  B:Byte;
 end;

 PRow = ^TRow;
 TRow = array[0..1000000] of TRGBTriple;

 PPRows = ^TPRows;
 TPRows = array[0..1000000] of PRow;

const
 MaxKernelSize = 100;

type
 TKernelSize = 1..MaxKernelSize;

 TKernel = record
  Size:TKernelSize;
  Weights:array[-MaxKernelSize..MaxKernelSize] of Single;
 end;

procedure GBlur(BMP:TBitmap; Radius:Double);

implementation

function TrimValue(Lower, Upper, Value:Integer):Integer; overload;
begin
 if (Value <= Upper) and (Value >= Lower) then Result:=Value
 else if Value > Upper then Result:=Upper else Result:=Lower;
end;

function TrimValue(Lower, Upper:Integer; Value:Double):Integer; overload;
begin
 if (Value < Upper) and (Value >= Lower) then Result:=Trunc(Value)
 else if Value > Upper then Result:=Upper else Result:=Lower;
end;

procedure GBlur(BMP:TBitmap; Radius:Double);
var Row, Col:Integer;
    Rows:PPRows;
    K:TKernel;
    ACol:PRow;
    P:PRow;

procedure BlurRow(var Row:array of TRGBTriple; K:TKernel; P:PRow);
var j, n:Integer;
    tr, tg, tb:Double;
    w:Double;
begin
 for j:=0 to High(Row) do
  begin
   tb:=0;
   tg:=0;
   tr:=0;
   for n:= -K.Size to K.Size do
    begin
     w:=K.Weights[n];
     with Row[TrimValue(0, High(Row), j - n)] do
      begin
       tb:=tb + w * b;
       tg:=tg + w * g;
       tr:=tr + w * r;
      end;
    end;
   with P[j] do
    begin
     b:=TrimValue(0, 255, tb);
     g:=TrimValue(0, 255, tg);
     r:=TrimValue(0, 255, tr);
    end;
  end;
 Move(P[0], Row[0], (High(Row) + 1) * Sizeof(TRGBTriple));
end;

procedure MakeGaussianKernel(var K:TKernel; Radius:Double; MaxData, DataGranularity:Double);
var
 j:Integer;
 temp, delta:Double;
 KernelSize:TKernelSize;
begin
 for j:=Low(K.Weights) to High(K.Weights) do
  begin
   temp:=j / radius;
   K.Weights[j]:=exp(-temp * temp / 2);
  end;

 temp:=0;
 for j:=Low(K.Weights) to High(K.Weights) do temp:=temp + K.Weights[j];
 for j:=Low(K.Weights) to High(K.Weights) do K.Weights[j]:=K.Weights[j] / temp;

 KernelSize:=MaxKernelSize;
 delta:=DataGranularity / (2 * MaxData);
 temp:=0;
 while (temp < delta) and (KernelSize > 1) do
  begin
   temp:=temp + 2 * K.Weights[KernelSize];
   Dec(KernelSize);
  end;
 K.Size:=KernelSize;

 temp:=0;
 for j:= -K.Size to K.Size do temp:=temp + K.Weights[j];
 for j:= -K.Size to K.Size do K.Weights[j]:=K.Weights[j] / temp;
end;

begin
 if (BMP.HandleType <> bmDIB) or (BMP.PixelFormat <> pf24Bit) then
  begin
   raise Exception.Create('Необходимо 24-битное изображение');
  end;

 MakeGaussianKernel(K, Radius, 255, 1);
 GetMem(Rows, BMP.Height * SizeOf(PRow));
 GetMem(ACol, BMP.Height * SizeOf(TRGBTriple));

 for Row:=0 to BMP.Height - 1 do Rows[Row]:=BMP.Scanline[Row];

 P:=AllocMem(BMP.Width * SizeOf(TRGBTriple));
 for Row:=0 to BMP.Height - 1 do BlurRow(Slice(Rows[Row]^, BMP.Width), K, P);
 ReAllocMem(P, BMP.Height * SizeOf(TRGBTriple));

 for Col:=0 to BMP.Width - 1 do
  begin
   for Row:=0 to BMP.Height - 1 do ACol[Row]:=Rows[Row][Col];
   BlurRow(Slice(ACol^, BMP.Height), K, P);
   for Row:=0 to BMP.Height - 1 do Rows[Row][Col]:=ACol[Row];
  end;
 FreeMem(Rows);
 FreeMem(ACol);
 ReAllocMem(P, 0);
end;

end.

