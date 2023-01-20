unit HGM.Common.PngUtils;

interface

uses
  System.SysUtils, Vcl.Imaging.pngimage, Vcl.Graphics, Vcl.Controls,
  System.Types;

function CreateFrom(X, Y, W, H: Word; Src: TPngImage): TPngImage;

function CreatePNG(Dll: Cardinal; ID: string): TPngImage; overload;

function CreatePNG(FName: string): TPngImage; overload;

function IcoToPng(ICON: TIcon): TPngImage;

function PngToIco(PNGObj: TPngImage): TIcon;

procedure ApplyMask(X, Y: Integer; Mask, Target: TPngImage);

procedure ColorImages(IList: TImageList; ID: Integer; Color: TColor);

procedure CopyFrom(SrcPt, DestPt, Size: TPoint; Src: TPngImage; var Dest: TPngImage);

procedure DrawBitmapTo(X, Y: Integer; Src: TBitmap; Dest: TPngImage);

procedure DrawIconColorLine(IList: TImageList; ID: Integer; Color: TColor);

procedure DrawTo(X, Y: Integer; Src, Dest: TPngImage); overload;

procedure PNGColored(X, Y: Integer; Src, Dest: TPngImage; MColor: TColor);

procedure PNGColoredLine(X, Y: Integer; Src, Dest: TPngImage; MColor: TColor);

procedure SetImageListColor(ImgList: TImageList; Color: TColor);

procedure SmoothResizePNG(apng: TPngImage; NuWidth, NuHeight: integer);

implementation

uses
  PNGFunctions, PNGImageList, HGM.Common.Utils, System.Math;

function CreatePNG(Dll: Cardinal; ID: string): TPngImage;
begin
  Result := TPngImage.Create;
  try
    Result.LoadFromResourceName(Dll, ID);
  except
    Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, 32, 32);
  end;
end;

function CreatePNG(FName: string): TPngImage;
begin
  Result := TPngImage.Create;
  try
    Result.LoadFromFile(FName)
  except
    Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, 32, 32);
  end;
end;

function CreateFrom(X, Y, W, H: Word; Src: TPngImage): TPngImage;
begin
  Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, W, H);
  CopyFrom(Point(X, Y), Point(0, 0), Point(W, H), Src, Result);
end;

procedure SetImageListColor(ImgList: TImageList; Color: TColor);
var
  i: Integer;
begin
  for i := 0 to ImgList.Count - 1 do
    ColorImages(ImgList, i, Color);
end;

procedure ApplyMask(X, Y: Integer; Mask, Target: TPngImage);
var
  dX, dY: Integer;
  DAS, SAS: pByteArray;
begin
  for dY := 0 to Mask.Height - 1 do
  begin
    SAS := Mask.AlphaScanline[dY];
    DAS := Target.AlphaScanline[dY + Y];
    for dX := 0 to Mask.Width - 1 do
    begin
      DAS[dX + X] := Min(DAS[dX + X], SAS[dX]);
    end;
  end;
end;

function PngToIco(PNGObj: TPngImage): TIcon;
var
  PngImageList: TPngImageList;
begin
  PngImageList := TPngImageList.Create(nil);
  PngImageList.Width := PNGObj.Width;
  PngImageList.Height := PNGObj.Height;
  PngImageList.PngImages.Add(False);
  PngImageList.PngImages[0].PngImage := PNGObj;
  Result := TIcon.Create;
  PngImageList.GetIcon(0, Result);
  PngImageList.Clear;
  PngImageList.Free;
end;

function IcoToPng(ICON: TIcon): TPngImage;
var
  PngImageList: TPngImageList;
begin
  PngImageList := TPngImageList.Create(nil);
  PngImageList.Width := ICON.Width;
  PngImageList.Height := ICON.Height;
  PngImageList.PngImages.Add(False);
  PngImageList.PngImages[0].Assign(ICON);
  Result := PngImageList.PngImages[0].PngImage;
  PngImageList.Clear;
  PngImageList.Free;
end;

procedure PNGColored(X, Y: Integer; Src, Dest: TPngImage; MColor: TColor);
var
  dX, dY: Integer;
  DAS, SAS: pByteArray;
begin
  for dY := 0 to Src.Height - 1 do
  begin
    DAS := Dest.AlphaScanline[dY + Y];
    SAS := Src.AlphaScanline[dY];
    for dX := 0 to Src.Width - 1 do
    begin
      if SAS^[dX] <= 0 then
        Continue;
      DAS[dX + X] := SAS^[dX];
      Dest.Canvas.Pixels[dX + X, dY + Y] := MColor;
    end;
  end;
end;

procedure PNGColoredLine(X, Y: Integer; Src, Dest: TPngImage; MColor: TColor);
var
  dX, dY: Integer;
  DAS, SAS: pByteArray;
begin
  for dY := 0 to Src.Height - 1 do
  begin
    DAS := Dest.AlphaScanline[dY + Y];
    SAS := Src.AlphaScanline[dY];
    for dX := 0 to Src.Width - 1 do
    begin
      if SAS^[dX] <= 0 then
        Continue;
      if (dY > Src.Height - 5) then
      begin
        DAS[dX + X] := 255;
        Dest.Canvas.Pixels[dX + X, dY + Y] := MColor;
      end
      else
      begin
        DAS[dX + X] := SAS^[dX];
        Dest.Canvas.Pixels[dX + X, dY + Y] := Src.Canvas.Pixels[dX + X, dY + Y];
      end;
    end;
  end;
end;

procedure DrawIconColorLine(IList: TImageList; ID: Integer; Color: TColor);
var
  Icon: TIcon;
  PNG, PNGNew: TPngImage;
begin
  if (ID < 0) or (ID > IList.Count - 1) then
    Exit;
  Icon := TIcon.Create;
  try
    Icon.Width := IList.Width;
    Icon.Height := IList.Height;
    IList.GetIcon(ID, Icon);
    PNG := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, Icon.Width, Icon.Height);
    ConvertToPNG(Icon, PNG);
  finally
    Icon.Free;
  end;
  PNGNew := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, Icon.Width, Icon.Height);
  try
    PNGColoredLine(0, 0, PNG, PNGNew, Color);
  finally
    PNG.Free;
  end;
  try
    Icon := PngToIco(PNGNew);
    IList.ReplaceIcon(ID, Icon);
  finally
    Icon.Free;
    PNGNew.Free;
  end;
end;

procedure ColorImages(IList: TImageList; ID: Integer; Color: TColor);
var
  Icon, NewIcon: TIcon;
  PNG, PNGNew: TPngImage;
begin
  if (ID < 0) or (ID > IList.Count - 1) then
    Exit;
  Icon := TIcon.Create;
  PNG := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, IList.Width, IList.Height);
  PNGNew := TPngImage.CreateBlank(COLOR_RGBALPHA, 16, IList.Width, IList.Height);
  try
    Icon.Width := IList.Width;
    Icon.Height := IList.Height;
    IList.GetIcon(ID, Icon);
    ConvertToPNG(Icon, PNG);
    if PNG.TransparencyMode = ptmNone then
      PNG.CreateAlpha;
    PNGColored(0, 0, PNG, PNGNew, Color);
    NewIcon := PngToIco(PNGNew);
    IList.ReplaceIcon(ID, NewIcon);
    NewIcon.Free;
  finally
    begin
      Icon.Free;
      PNG.Free;
      PNGNew.Free;
    end;
  end;
end;

//Автор: Я (очень медленная процедура ~300 мсек. для ср. рисунка)
procedure DrawTo(X, Y: Integer; Src, Dest: TPngImage);
var
  dX, dY: Integer;
  DAS, SAS: pByteArray;
begin
  for dY := 0 to Src.Height - 1 do
  begin
    DAS := Dest.AlphaScanline[dY + Y];
    SAS := Src.AlphaScanline[dY];
    for dX := 0 to Src.Width - 1 do
    begin
      if SAS^[dX] <= 0 then
        Continue;
      if DAS[dX + X] > 0 then
      begin
        DAS[dX + X] := Min(255, (SAS^[dX] + DAS^[dX + X]));
        Dest.Canvas.Pixels[dX + X, dY + Y] := MixColorsValue(Src.Canvas.Pixels[dX, dY], Dest.Canvas.Pixels[dX + X, dY + Y], DAS^[dX + X]);
      end
      else
      begin
        DAS[dX + X] := SAS^[dX];
        Dest.Canvas.Pixels[dX + X, dY + Y] := Src.Canvas.Pixels[dX, dY];
      end;
    end;
  end;
end;

procedure DrawBitmapTo(X, Y: Integer; Src: TBitmap; Dest: TPngImage);
type
  TRGB32 = packed record
    B, G, R, Alpha: Byte;
  end;

  PRGBArray32 = ^TRGBArray32;

  TRGBArray32 = array[0..0] of TRGB32;
var
  dX, dY: Integer;
  DAS: pByteArray;
  Line: PRGBArray32;
begin
  for dY := 0 to Src.Height - 1 do
  begin
    DAS := Dest.AlphaScanline[dY + Y];
    Line := PRGBArray32(Src.ScanLine[dY]);

    for dX := 0 to Src.Width - 1 do
    begin
      {if Line[dX].Alpha <= 0 then
        Continue; }
      if DAS[dX + X] > 0 then
      begin
        DAS[dX + X] := Min(255, (Line[dX].Alpha + DAS^[dX + X]));
        Dest.Canvas.Pixels[dX + X, dY + Y] := MixColorsValue(Src.Canvas.Pixels[dX, dY], Dest.Canvas.Pixels[dX + X, dY + Y], DAS^[dX + X]);
      end
      else
      begin
        DAS[dX + X] := 255; //Line[dX].Alpha;
        Dest.Canvas.Pixels[dX + X, dY + Y] := Src.Canvas.Pixels[dX, dY];
      end;
    end;
  end;
end;

//Автор: Я
procedure CopyFrom(SrcPt, DestPt, Size: TPoint; Src: TPngImage; var Dest: TPngImage);
var
  X, Y: Integer;
  DAS, SAS: pByteArray;
begin
  Size.X := Size.X - 1;
  Size.Y := Size.Y - 1;
  if DestPt.X + Size.X > Dest.Width then
    Size.X := Dest.Width - DestPt.X;
  if DestPt.Y + Size.Y > Dest.Height then
    Size.Y := Dest.Height - DestPt.Y;
  for Y := 0 to Size.Y - 1 do
  begin
    DAS := Dest.AlphaScanline[Y + DestPt.Y];
    SAS := Src.AlphaScanline[Y + SrcPt.Y];
    for X := 0 to Size.X - 1 do
    begin
      Dest.Canvas.Pixels[X + DestPt.X, Y + DestPt.Y] := Src.Canvas.Pixels[X + SrcPt.X, Y + SrcPt.Y];
      DAS^[X + DestPt.X] := SAS^[X + SrcPt.X];
    end;
  end;
end;

procedure SmoothResizePNG(apng: TPngImage; NuWidth, NuHeight: integer);
var
  xscale, yscale: Single;
  sfrom_y, sfrom_x: Single;
  ifrom_y, ifrom_x: Integer;
  to_y, to_x: Integer;
  weight_x, weight_y: array[0..1] of Single;
  weight: Single;
  new_red, new_green: Integer;
  new_blue, new_alpha: Integer;
  new_colortype: Integer;
  total_red, total_green: Single;
  total_blue, total_alpha: Single;
  IsAlpha: Boolean;
  ix, iy: Integer;
  bTmp: TPngImage;
  sli, slo: pRGBLine;
  ali, alo: pbytearray;
begin
  if not (apng.Header.ColorType in [COLOR_RGBALPHA, COLOR_RGB]) then
    raise Exception.Create('Only COLOR_RGBALPHA and COLOR_RGB formats' + ' are supported');
  new_alpha := 0;
  ali := nil;
  alo := nil;
  IsAlpha := apng.Header.ColorType in [COLOR_RGBALPHA];
  if IsAlpha then
    new_colortype := COLOR_RGBALPHA
  else
    new_colortype := COLOR_RGB;
  bTmp := TPngImage.CreateBlank(new_colortype, 8, NuWidth, NuHeight);
  xscale := bTmp.Width / (apng.Width - 1);
  yscale := bTmp.Height / (apng.Height - 1);
  for to_y := 0 to bTmp.Height - 1 do
  begin
    sfrom_y := to_y / yscale;
    ifrom_y := Trunc(sfrom_y);
    weight_y[1] := sfrom_y - ifrom_y;
    weight_y[0] := 1 - weight_y[1];
    for to_x := 0 to bTmp.Width - 1 do
    begin
      sfrom_x := to_x / xscale;
      ifrom_x := Trunc(sfrom_x);
      weight_x[1] := sfrom_x - ifrom_x;
      weight_x[0] := 1 - weight_x[1];

      total_red := 0.0;
      total_green := 0.0;
      total_blue := 0.0;
      total_alpha := 0.0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          sli := apng.Scanline[ifrom_y + iy];
          if IsAlpha then
            ali := apng.AlphaScanline[ifrom_y + iy];
          new_red := sli[ifrom_x + ix].rgbtRed;
          new_green := sli[ifrom_x + ix].rgbtGreen;
          new_blue := sli[ifrom_x + ix].rgbtBlue;
          if IsAlpha then
            new_alpha := ali[ifrom_x + ix];
          weight := weight_x[ix] * weight_y[iy];
          total_red := total_red + new_red * weight;
          total_green := total_green + new_green * weight;
          total_blue := total_blue + new_blue * weight;
          if IsAlpha then
            total_alpha := total_alpha + new_alpha * weight;
        end;
      end;
      slo := bTmp.ScanLine[to_y];
      if IsAlpha then
        alo := bTmp.AlphaScanLine[to_y];
      slo[to_x].rgbtRed := Round(total_red);
      slo[to_x].rgbtGreen := Round(total_green);
      slo[to_x].rgbtBlue := Round(total_blue);
      if IsAlpha then
        alo[to_x] := Round(total_alpha);
    end;
  end;
  apng.Assign(bTmp);
  bTmp.Free;
end;

end.

