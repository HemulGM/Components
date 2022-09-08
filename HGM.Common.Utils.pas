unit HGM.Common.Utils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  System.Generics.Collections, Vcl.ValEdit, System.DateUtils, System.Math,
  Vcl.ComCtrls, Vcl.Imaging.jpeg, Vcl.Grids, Vcl.Imaging.pngimage, Vcl.StdCtrls,
  Vcl.Mask, Vcl.ExtCtrls, System.Types;

function Between(FMin, FValue, FMax: Integer): Boolean;

function ColorDarker(Color: TColor; Percent: Byte = 40): TColor;

function ColorLighter(Color: TColor; Percent: Byte = 40): TColor;

function ColorDarkerOr(Color: TColor; Percent: Byte = 40): TColor;

function ColorLighterOr(Color: TColor; Percent: Byte = 40): TColor;

function GetGroupID(LV: TListView; GrName: string): Integer;

function GetJPGFromDll(LibName, ResName: string): TJPEGImage;

function GetSeconds(Time: TTime): Cardinal;

function HumanDateTime(Value: TDateTime): string;

function PercentRound(Value: Extended): Extended;

function PngToIco(PNGObj: TPngImage): TIcon;

function IcoToPng(ICON: TIcon): TPngImage;

function SecondsToStr(Value: Cardinal): string;

function SimpleStrCompare(const Str1, Str2: string): Double;

function TranslitRus2Lat(const Str: string): string;

function GetLastDir(Path: string): string;

procedure AddToValueEdit(VE: TValueListEditor; Key, Value, ValueBU: string);

procedure AutoColumn(LV: TListView);

procedure ClearGrid(StringGrid: TStringGrid);

procedure ColorImages(IList: TImageList; ID: Integer; Color: TColor);

procedure SetImageListColor(ImgList: TImageList; Color: TColor);

function Centred(V1, V2: Integer): Integer;

procedure DrawIconColorLine(IList: TImageList; ID: Integer; Color: TColor);

procedure DrawTo(X, Y: Integer; Src, Dest: TPngImage); overload;

procedure CopyFrom(SrcPt, DestPt, Size: TPoint; Src: TPngImage; var Dest: TPngImage);

function CreateFrom(X, Y, W, H: Word; Src: TPngImage): TPngImage;

function CreatePNG(FName: string): TPngImage; overload;

function CreatePNG(Dll: Cardinal; ID: string): TPngImage; overload;

procedure PNGColored(X, Y: Integer; Src, Dest: TPngImage; MColor: TColor);

procedure PNGColoredLine(X, Y: Integer; Src, Dest: TPngImage; MColor: TColor);

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: string; FDrawFlags: Cardinal): Integer;

function ScaledRect(const Src: TRect; Delta: Integer): TRect;

function MixColors(Color1, Color2: TColor; Alpha: Byte): TColor;

function MixColorsValue(Color1, Color2: TColor; Alpha: Byte): TColor;

function IndexInList(const Index: Integer; ListCount: Integer): Boolean;

function FlashControl(Control: TControl): Boolean;

function CutString(Value: string; Count: Word): string;

procedure RichEditPopupMenu(Target: TRichEdit);

procedure RichEditSetTopLineText(Target: TRichEdit);

procedure RichEditSetBottomLineText(Target: TRichEdit);

function RichEditGetBottomLineText(Target: TRichEdit): Boolean;

function RichEditGetTopLineText(Target: TRichEdit): Boolean;

procedure RichEditSetResetText(Target: TRichEdit);

function RichEditGetBGCOlor(Target: TRichEdit; IfNone: TColor): TColor;

procedure RichEditSetBGCOlor(Target: TRichEdit; Color: TColor);

function DownloadURL(URL: string): TMemoryStream; overload;

function DownloadURL(URL: string; FileName: string): Boolean; overload;

function Reverse(s: string): string;

function GetFileNameWoE(FileName: TFileName): string;

function GetGroup(LV: TListView; GroupName: string; Expand: Boolean): Word;

procedure ApplyMask(X, Y: Integer; Mask, Target: TPngImage);

function ColorRedOrBlue(Precent: Byte): TColor;

function SmoothStrechDraw(Source: TGraphic; Rect: TRect; FillColor: TColor = clNone): TBitmap;

procedure SmoothResizePNG(apng: TPngImage; NuWidth, NuHeight: integer);

procedure DrawBitmapTo(X, Y: Integer; Src: TBitmap; Dest: TPngImage);

procedure WaitTime(MS: Int64);

implementation

uses
  PNGFunctions, PNGImageList, ClipBrd, System.Net.HttpClient, Winapi.RichEdit;

function GetLastDir(Path: string): string;
begin
  Result := Path;
  if Result[Length(Result)] = PathDelim then
    Delete(Result, Length(Result), 1);
  Delete(Result, 1, LastDelimiter(PathDelim, Result));
end;

procedure WaitTime(MS: Int64);
var
  TS: Cardinal;
begin
  if MS < 0 then
    Exit;
  if MS = 0 then
  begin
    Application.ProcessMessages;
    Exit;
  end;
  TS := GetTickCount;
  while TS + MS > GetTickCount do
    Application.ProcessMessages;
end;

function HumanDateTime(Value: TDateTime): string;
begin
  if IsSameDay(Value, Today) then
    Result := 'Сегодня'
  else if IsSameDay(Value, Yesterday) then
    Result := 'Вчера'
  else if IsSameDay(Value, Yesterday - 1) then
    Result := 'Позавчера'
  else
    Result := DateToStr(Value);

  Result := Result + FormatDateTime(' в HH:NN:SS', Value);
end;

function ColorRedOrBlue(Precent: Byte): TColor;
var
  R, B: Integer;
begin
  //255 000 000
  //000 000 255
  R := Round(255 * Precent / 100);
  B := Round(255 * (100 - Precent) / 100);
  Result := RGB(R, 100, B);
end;

function GetGroup(LV: TListView; GroupName: string; Expand: Boolean): Word;
var
  i: Word;
  NewGroup: TListGroup;
begin
  if GroupName = '' then
    GroupName := 'Без группы';
  if LV.Groups.Count > 0 then
    for i := 0 to LV.Groups.Count - 1 do
      if LV.Groups.Items[i].Header = GroupName then
      begin
        Result := i;
        Exit;
      end;
  NewGroup := LV.Groups.Add;
  with NewGroup do
  begin
    Header := GroupName;
    Result := GroupID;
    if not Expand then
      NewGroup.State := [lgsNormal, lgsCollapsible, lgsCollapsed]
    else
      NewGroup.State := [lgsNormal, lgsCollapsible];
  end;
end;

function Reverse(s: string): string;
var
  i: Word;
begin
  if Length(s) <= 1 then
    Exit(s);
  for i := Length(s) downto 1 do
    Result := Result + s[i];
end;

function GetFileNameWoE(FileName: TFileName): string;
var
  PPos: Integer;
  str: string;
begin
  str := ExtractFileName(FileName);
  if Length(str) < 3 then
    Exit;
  PPos := Pos('.', Reverse(str));
  if PPos > 0 then
    Result := Copy(str, 1, Length(str) - PPos);
end;

function DownloadURL(URL: string): TMemoryStream;
var
  HTTP: THTTPClient;
begin
  Result := TMemoryStream.Create;
  HTTP := THTTPClient.Create;
  try
    try
      HTTP.HandleRedirects := True;
      HTTP.Get(URL, Result);
      Result.Position := 0;
    except
      //Ну, ошибка... Поток всё равно создан и ошибки не должно возникнуть,
      //если проверить размер потока перед его использованием
    end;
  finally
    HTTP.Free;
  end;
end;

function DownloadURL(URL: string; FileName: string): Boolean;
var
  HTTP: THTTPClient;
  Mem: TFileStream;
begin
  HTTP := THTTPClient.Create;
  try
    try
      FileClose(FileCreate(FileName));
      Mem := TFileStream.Create(FileName, fmOpenWrite);
      try
        HTTP.HandleRedirects := True;
        HTTP.Get(URL, Mem);
      finally
        Mem.Free;
      end;
      Result := True;
    except
      Result := False;
      //Ну, ошибка... Поток всё равно создан и ошибки не должно возникнуть,
      //если проверить размер потока перед его использованием
    end;
  finally
    HTTP.Free;
  end;
end;

function RichEditGetTopLineText(Target: TRichEdit): Boolean;
var
  CF: TCharFormat2;
  Mask: integer;
begin
  Result := False;
  CF.cbSize := SizeOf(CF);
  Mask := Target.Perform(EM_GETCHARFORMAT, SCF_SELECTION, Integer(@CF));
  if (Mask and CFM_OFFSET) = CFM_OFFSET then
  begin
    Result := CF.yOffset > 0;
  end;
end;

function RichEditGetBottomLineText(Target: TRichEdit): Boolean;
var
  CF: TCharFormat;
  Mask: integer;
begin
  Result := False;
  CF.cbSize := SizeOf(CF);
  Mask := Target.Perform(EM_GETCHARFORMAT, SCF_SELECTION, Integer(@CF));
  if (Mask and CFM_OFFSET) = CFM_OFFSET then
  begin
    Result := CF.yOffset < 0;
  end;
end;

procedure RichEditSetResetText(Target: TRichEdit);
var
  CF: TCharFormat;
begin
  CF.cbSize := SizeOf(CF);
  CF.dwMask := CFM_OFFSET;
  CF.yOffset := 0;
  Target.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Integer(@CF));
end;

procedure RichEditSetBottomLineText(Target: TRichEdit);
var
  CF: TCharFormat;
begin
  CF.cbSize := SizeOf(CF);
  CF.dwMask := CFM_OFFSET;
  CF.yOffset := -70;
  Target.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Integer(@CF));
end;

procedure RichEditSetTopLineText(Target: TRichEdit);
var
  CF: TCharFormat;
begin
  CF.cbSize := SizeOf(CF);
  CF.dwMask := CFM_OFFSET;
  CF.yOffset := 70;
  Target.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Integer(@CF));
end;

function RichEditGetBGCOlor(Target: TRichEdit; IfNone: TColor): TColor;
var
  CF: TCharFormat2;
begin
  FillChar(CF, SizeOf(CF), 0);
  CF.cbSize := SizeOf(CF);
  Target.Perform(EM_GETCHARFORMAT, SCF_SELECTION, Integer(@CF));
  Result := CF.crBackColor;
  if Result = 0 then
    Result := IfNone;
end;

procedure RichEditSetBGCOlor(Target: TRichEdit; Color: TColor);
var
  CF: TCharFormat2;
begin
  CF.cbSize := SizeOf(CF);
  CF.dwMask := CFM_BACKCOLOR;
  if Color = clNone then
    CF.dwEffects := CFE_AUTOBACKCOLOR
  else
    CF.crBackColor := Color;
  Target.Perform(EM_SETCHARFORMAT, SCF_SELECTION, Integer(@CF));
end;

procedure RichEditPopupMenu(Target: TRichEdit);
const
  IDM_UNDO = WM_UNDO;
  IDM_CUT = WM_CUT;
  IDM_COPY = WM_COPY;
  IDM_PASTE = WM_PASTE;
  IDM_DELETE = WM_CLEAR;
  IDM_SELALL = EM_SETSEL;
  IDM_RTL = $8000; // WM_APP ?

  Enables: array[Boolean] of DWORD = (MF_DISABLED or MF_GRAYED, MF_ENABLED);
  Checks: array[Boolean] of DWORD = (MF_UNCHECKED, MF_CHECKED);
var
  hUser32: HMODULE;
  hmnu, hmenuTrackPopup: HMENU;
  Cmd: DWORD;
  Flags: Cardinal;
  HasSelText: Boolean;
  FormHandle: HWND;
  // IsRTL: Boolean;
begin
  hUser32 := LoadLibraryEx(user32, 0, LOAD_LIBRARY_AS_DATAFILE);
  if (hUser32 <> 0) then
  try
    hmnu := LoadMenu(hUser32, MAKEINTRESOURCE(1));
    if (hmnu <> 0) then
    try
      hmenuTrackPopup := GetSubMenu(hmnu, 0);
      HasSelText := Length(Target.SelText) <> 0;
      EnableMenuItem(hmnu, IDM_UNDO, Enables[Target.CanUndo]);
      EnableMenuItem(hmnu, IDM_CUT, Enables[HasSelText]);
      EnableMenuItem(hmnu, IDM_COPY, Enables[HasSelText]);
      EnableMenuItem(hmnu, IDM_PASTE, Enables[not Clipboard.HasFormat(0)]);
      EnableMenuItem(hmnu, IDM_DELETE, Enables[HasSelText]);
      EnableMenuItem(hmnu, IDM_SELALL, Enables[Length(Target.Text) <> 0]);


      // IsRTL := GetWindowLong(re.Handle, GWL_EXSTYLE) and WS_EX_RTLREADING <> 0;
      // EnableMenuItem(hmnu, IDM_RTL, Enables[True]);
      // CheckMenuItem(hmnu, IDM_RTL, Checks[IsRTL]);

      FormHandle := GetParentForm(Target).Handle;
      Flags := TPM_LEFTALIGN or TPM_RIGHTBUTTON or TPM_NONOTIFY or TPM_RETURNCMD;
      Cmd := DWORD(TrackPopupMenu(hmenuTrackPopup, Flags, Mouse.CursorPos.X, Mouse.CursorPos.Y, 0, FormHandle, nil));
      if Cmd <> 0 then
      begin
        case Cmd of
          IDM_UNDO:
            Target.Undo;
          IDM_CUT:
            Target.CutToClipboard;
          IDM_COPY:
            Target.CopyToClipboard;
          IDM_PASTE:
            Target.PasteFromClipboard;
          IDM_DELETE:
            Target.ClearSelection;
          IDM_SELALL:
            Target.SelectAll;
          IDM_RTL:
            ; // ?
        end;
      end;
    finally
      DestroyMenu(hmnu);
    end;
  finally
    FreeLibrary(hUser32);
  end;
end;

function CutString(Value: string; Count: Word): string;
begin
  if Value.Length > Count then
    Result := Copy(Value, 1, Count) + '...'
  else
    Result := Value;
end;

function Centred(V1, V2: Integer): Integer;
begin
  Result := (V1 div 2) - (V2 div 2);
end;

function SimpleStrCompare(const Str1, Str2: string): Double;
var
  Len1, Len2, i, j, k, P1: Integer;
  S1, S2: string;
begin
  if Str1 = Str2 then
    Exit(1);
  if (Str1.IsEmpty) or (Str2.IsEmpty) then
    Exit(0);
  if Length(Str1) > Length(Str2) then
  begin
    S1 := Str1;
    S2 := Str2;
  end
  else
  begin
    S1 := Str2;
    S2 := Str1;
  end;
  Len1 := Length(S1);
  Len2 := Length(S2);
  P1 := 0;
  for i := 0 to Len1 - Len2 do
  begin
    j := 0;
    for k := 1 to Len2 do
      if S1[k + i] = S2[k] then
        Inc(j);
    if j > P1 then
      P1 := j;
  end;
  Result := P1 / Len1;
end;

function PercentRound(Value: Extended): Extended;
begin
  Result := Max(0, Min(100, Value));
end;

function TranslitRus2Lat(const Str: string): string;
const
  RArrayL = 'абвгдеёжзийклмнопрстуфхцчшщьыъэюя';
  RArrayU = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ';
  colChar = 33;
  arr: array[1..2, 1..ColChar] of string = (('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y', 'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f', 'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya'), ('A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F', 'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya'));
var
  i, p: Integer;
  d: Byte;
begin
  Result := '';
  for i := 1 to Length(Str) do
  begin
    d := 1;
    p := Pos(Str[i], RArrayL);
    if p = 0 then
    begin
      p := Pos(Str[i], RArrayU);
      d := 2;
    end;
    if p <> 0 then
      Result := Result + arr[d, p]
    else
      Result := Result + Str[i];
  end;
end;

function GetSeconds(Time: TTime): Cardinal;
var
  H, M, S, MS: Word;
begin
  DecodeTime(Time, H, M, S, MS);
  Result := H * SecsPerHour + M * SecsPerMin + S;
end;

function SecondsToStr(Value: Cardinal): string;
var
  H, M, S, DC: Integer;
begin
  Result := '';
  H := Value div SecsPerHour;
  Value := Value mod SecsPerHour;

  M := Value div SecsPerMin;
  Value := Value mod SecsPerMin;

  S := Value;
  DC := 0;
  if H > 0 then
  begin
    Result := Result + IntToStr(H) + ' ч. ';
    Inc(DC);
  end;
  if M > 0 then
  begin
    Result := Result + IntToStr(M) + ' м. ';
    Inc(DC);
  end;
  if DC < 2 then
    if S > 0 then
      Result := Result + IntToStr(S) + ' с.';
end;

function Between(FMin, FValue, FMax: Integer): Boolean;
begin
  Result := (FValue >= FMin) and (FValue <= FMax);
end;

procedure SetImageListColor(ImgList: TImageList; Color: TColor);
var
  i: Integer;
begin
  for i := 0 to ImgList.Count - 1 do
    ColorImages(ImgList, i, Color);
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

procedure AddToValueEdit(VE: TValueListEditor; Key, Value, ValueBU: string);
begin
  if Key = '' then
    Key := 'Неизвестный параметр';
  if Length(Value) < 1 then
    if ValueBU <> '' then
      Value := ValueBU;
  if Value <> '' then
    VE.Strings.Add(Key + '=' + Value);
end;

procedure AutoColumn(LV: TListView);
var
  x, y, w: Integer;
  MaxWidth: Integer;
begin
  with LV do
  begin
    for x := 1 to LV.Columns.Count - 1 do
    begin
      MaxWidth := Canvas.TextWidth(LV.Columns[x].Caption);
      for y := 0 to LV.Items.Count - 1 do
      begin
        w := Canvas.TextWidth(LV.Items[y].SubItems[x - 1]);
        if w > MaxWidth then
          MaxWidth := w;
      end;
      LV.Columns[x].Width := MaxWidth + 15;
    end;
  end;
end;

procedure ClearGrid(StringGrid: TStringGrid);
var
  i, j: Integer;
begin
  for i := 0 to StringGrid.ColCount - 1 do
    for j := 0 to StringGrid.RowCount - 1 do
      StringGrid.Cells[i, j] := '';
end;

function ColorDarkerOr(Color: TColor; Percent: Byte = 40): TColor;
begin
  Result := ColorDarker(Color, Percent);
  if Result = Color then
    Result := ColorLighter(Color, Percent);
end;

function ColorLighterOr(Color: TColor; Percent: Byte = 40): TColor;
begin
  Result := ColorLighter(Color, Percent);
  if Result = Color then
    Result := ColorDarker(Color, Percent);
end;

function ColorDarker(Color: TColor; Percent: Byte): TColor;
var
  R, G, B: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  R := R - MulDiv(R, Percent, 100);
  G := G - MulDiv(G, Percent, 100);
  B := B - MulDiv(B, Percent, 100);
  Result := RGB(R, G, B);
end;

function ColorLighter(Color: TColor; Percent: Byte): TColor;
var
  R, G, B: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  R := R + MulDiv(255 - R, Percent, 100);
  G := G + MulDiv(255 - G, Percent, 100);
  B := B + MulDiv(255 - B, Percent, 100);
  Result := RGB(R, G, B);
end;

function GetGroupID(LV: TListView; GrName: string): Integer;
var
  i: Integer;

  function Add: Integer;
  begin
    with LV.Groups.Add do
    begin
      Result := ID;
      TitleImage := 0;
      State := [lgsCollapsible];
      //Footer:='Количество элементов: 1';
      Header := GrName;
    end;
  end;

begin
  if (LV.Groups.Count <= 0) then
    Exit(Add);
  for i := 0 to LV.Groups.Count - 1 do
    if GrName = LV.Groups[i].Header then
    begin
      with LV.Groups[i] do
      begin
        TitleImage := TitleImage + 1;
        //Footer:='Количество элементов: '+IntToStr(TitleImage);
      end;
      Exit(i);
    end;
  Result := Add;
end;

function GetJPGFromDll(LibName, ResName: string): TJPEGImage;
var
  DLLHandle: THandle;
  ResStream: TResourceStream;
begin
  try
    DLLHandle := LoadLibrary(PChar(LibName));
    if DLLHandle = 0 then
      Exit(nil);
    ResStream := TResourceStream.Create(DLLHandle, ResName, RT_RCDATA);
    Result := TJPEGImage.Create;
    Result.LoadFromStream(ResStream);
    ResStream.Free;
    FreeLibrary(DLLHandle);
  except
    Result := nil;
  end;
end;

function IndexInList(const Index: Integer; ListCount: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= ListCount - 1) and (ListCount > 0);
end;

function FlashControl(Control: TControl): Boolean;
var
  SaveColor, BufColor: TColor;

  procedure SetColorE(Control: TEdit; Color: TColor; var OldColor: TColor);
  begin
    OldColor := Control.Color;
    Control.Color := Color;
  end;

  procedure SetColorME(Control: TMaskEdit; Color: TColor; var OldColor: TColor);
  begin
    OldColor := Control.Color;
    Control.Color := Color;
  end;

  procedure SetColorC(Control: TComboBox; Color: TColor; var OldColor: TColor);
  begin
    OldColor := Control.Color;
    Control.Color := Color;
  end;

  procedure SetColorP(Control: TPanel; Color: TColor; var OldColor: TColor);
  begin
    OldColor := Control.Color;
    Control.Color := Color;
  end;

  procedure SetColorM(Control: TMemo; Color: TColor; var OldColor: TColor);
  begin
    OldColor := Control.Color;
    Control.Color := Color;
  end;

  procedure SetColorL(Control: TListBox; Color: TColor; var OldColor: TColor);
  begin
    OldColor := Control.Color;
    Control.Color := Color;
  end;

  procedure SetColor(Control: TControl; Color: TColor; var OldColor: TColor);
  begin
    if Control is TEdit then
      SetColorE(Control as TEdit, Color, OldColor);
    if Control is TMaskEdit then
      SetColorME(Control as TMaskEdit, Color, OldColor);
    if Control is TComboBox then
      SetColorC(Control as TComboBox, Color, OldColor);
    if Control is TPanel then
      SetColorP(Control as TPanel, Color, OldColor);
    if Control is TMemo then
      SetColorM(Control as TMemo, Color, OldColor);
    if Control is TListBox then
      SetColorL(Control as TListBox, Color, OldColor);
  end;

begin
  Result := True;
  with Control do
  begin
    SetColor(Control, clRed, SaveColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clMaroon, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clWhite, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clMaroon, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clRed, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clMaroon, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clWhite, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clMaroon, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clRed, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, clRed, BufColor);
    Repaint;
    Sleep(60);

    SetColor(Control, SaveColor, BufColor);
    Repaint;
    Sleep(60);
  end;
end;

function ScaledRect(const Src: TRect; Delta: Integer): TRect;
begin
  Result := Src;                           //Rect(1, 1, 4, 4)
  Result.Left := Result.Left - Delta;      //Scale 1 = Rect(0, 0, 5, 5)
  Result.Top := Result.Top - Delta;
  Result.Right := Result.Right + Delta;
  Result.Bottom := Result.Bottom + Delta;
end;

function MixColors(Color1, Color2: TColor; Alpha: Byte): TColor;
var
  C1, C2: LongInt;
  R, G, B, V1, V2: Byte;
begin
  Alpha := Round(2.55 * Alpha);
  C1 := ColorToRGB(Color1);
  C2 := ColorToRGB(Color2);
  V1 := Byte(C1);
  V2 := Byte(C2);
  R := Alpha * (V1 - V2) shr 8 + V2;
  V1 := Byte(C1 shr 8);
  V2 := Byte(C2 shr 8);
  G := Alpha * (V1 - V2) shr 8 + V2;
  V1 := Byte(C1 shr 16);
  V2 := Byte(C2 shr 16);
  B := Alpha * (V1 - V2) shr 8 + V2;
  Result := (B shl 16) + (G shl 8) + R;
end;

function MixColorsValue(Color1, Color2: TColor; Alpha: Byte): TColor;
var
  C1, C2: LongInt;
  R, G, B, V1, V2: Byte;
begin
  C1 := ColorToRGB(Color1);
  C2 := ColorToRGB(Color2);
  V1 := Byte(C1);
  V2 := Byte(C2);
  R := Alpha * (V1 - V2) shr 8 + V2;
  V1 := Byte(C1 shr 8);
  V2 := Byte(C2 shr 8);
  G := Alpha * (V1 - V2) shr 8 + V2;
  V1 := Byte(C1 shr 16);
  V2 := Byte(C2 shr 16);
  B := Alpha * (V1 - V2) shr 8 + V2;
  Result := (B shl 16) + (G shl 8) + R;
end;

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: string; FDrawFlags: Cardinal): Integer;
var
  DrawRect: TRect;
  DrawFlags: Cardinal;
  DrawParams: TDrawTextParams;
begin
  DrawRect := R;
  DrawFlags := DT_END_ELLIPSIS or DT_NOPREFIX or DT_EDITCONTROL or FDrawFlags;
  DrawText(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
  DrawRect.Right := R.Right;
  if DT_VCENTER and FDrawFlags = DT_VCENTER then
  begin
    if DrawRect.Bottom < R.Bottom then
      OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
    else
      DrawRect.Bottom := R.Bottom;
  end;
  ZeroMemory(@DrawParams, SizeOf(DrawParams));
  DrawParams.cbSize := SizeOf(DrawParams);
  DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
  Result := DrawParams.uiLengthDrawn;
end;

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

function SmoothStrechDraw(Source: TGraphic; Rect: TRect; FillColor: TColor): TBitmap;
var
  Pt: TPoint;
  Target: HDC;
  BMP: TBitmap;
  p: Pointer;
begin
  //!!! Главное юзать Vcl.Imaging.pngimage, если png будут, а не, например, acPNG от AlphaControls
  // В этот bitmap поместим наше изображение (любое)
  BMP := TBitmap.Create;
  BMP.AlphaFormat := afDefined;
  BMP.PixelFormat := pf32bit;
  BMP.HandleType := bmDIB;
  BMP.SetSize(Source.Width, Source.Height);
  if FillColor = clNone then
  begin
    BMP.ignorepalette := true;
    p := BMP.ScanLine[Source.Height - 1];
    ZeroMemory(p, Source.Width * Source.Height * 4);
    BMP.Assign(Source);
  end
  else
  begin
    BMP.Canvas.Brush.Color := FillColor;
    BMP.Canvas.FillRect(TRect.Create(0, 0, Source.Width, Source.Height));
    BMP.Canvas.Draw(0, 0, Source);
  end;


  // А это - результат
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.SetSize(Rect.Width, Rect.Height);

  // Сглаженное растягивание (или сжатие)
  Target := Result.Canvas.Handle;
  GetBrushOrgEx(Target, Pt);
  SetStretchBltMode(Target, HALFTONE);
  SetBrushOrgEx(Target, Pt.X, Pt.Y, @Pt);
  StretchBlt(Target, Rect.Left, Rect.Top, Rect.Width, Rect.Height, BMP.Canvas.Handle, 0, 0, BMP.Width, BMP.Height, SRCCOPY);
  BMP.Free;
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

end.

