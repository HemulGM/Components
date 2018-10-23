unit HGM.Common.Utils;

interface
 uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Generics.Collections, Winapi.Dwmapi, Vcl.ValEdit, System.DateUtils,
  System.Math, System.Rtti, System.TypInfo, Vcl.ComCtrls, Vcl.Imaging.jpeg, Vcl.Grids, Vcl.Imaging.pngimage,
  Vcl.StdCtrls, Vcl.Mask, Vcl.ExtCtrls;

 // function SendTelegram(BotToken, Chat_ID, SendText:string):Boolean;
  function AdvSelectDirectory(const Caption: string; const Root: WideString; var Directory: string; EditBox: Boolean = False; ShowFiles: Boolean = False; AllowCreateDirs: Boolean = True): Boolean;
  function Between(FMin, FValue, FMax:Integer):Boolean;
  function ColorDarker(Color:TColor; Percent:Byte = 40):TColor;
  function ColorLighter(Color:TColor; Percent:Byte = 40):TColor;
  function CompareFileTimeOwn(t1, t2:FILETIME):Int64;
  function CPUUsage(preIdleTime, preUserTime, preKrnlTime:TFileTime; ProcInterval:Integer):Extended;
  function GetAeroColor: TColor;
  function GetGroupID(LV:TListView; GrName:string):Integer;
  function GetJPGFromDll(LibName, ResName:string):TJPEGImage;
  function GetMachineName:string;
  function GetSeconds(Time:TTime):Cardinal;
  function GetUserName:string;
  function HumanDateTime(Value:TDateTime; ShowTime:Boolean = True):string;
  function PercentRound(Value:Extended):Extended;
  function PngToIco(PNGObj: TPngImage):TIcon;
  function SecondsToStr(Value:Cardinal):string;
  function SimpleStrCompare(const Str1, Str2:string):Double;
  function TranslitRus2Lat(const Str:string):string;
  procedure AddToValueEdit(VE:TValueListEditor; Key, Value, ValueBU:string);
  procedure AutoColumn(LV:TListView);
  procedure ClearGrid(StringGrid:TStringGrid);
  procedure ColorImages(IList:TImageList; ID:Integer; Color:TColor);
  procedure SetImageListColor(ImgList:TImageList; Color:TColor);
  function Centred(V1, V2:Integer):Integer;

  procedure DrawTo(X, Y:Integer; Src, Dest:TPngImage);
  procedure CopyFrom(SrcPt, DestPt, Size:TPoint; Src:TPngImage; var Dest:TPngImage);
  function MixBytes(FG, BG, TRANS:Byte):Byte;
  function CreateFrom(X, Y, W, H:Word; Src:TPngImage):TPngImage;
  function CreatePNG(FName:string):TPngImage; overload;
  function CreatePNG(Dll:Cardinal; ID:string):TPngImage; overload;
  procedure PNGColored(X, Y:Integer; Src, Dest:TPngImage; MColor:TColor);
  function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: String; FDrawFlags:Cardinal): Integer;
  function ScaledRect(const Src:TRect; Delta:Integer):TRect;
  function MixColors(Color1, Color2:TColor; Alpha:Byte):TColor;
  function IndexInList(const Index:Integer; ListCount:Integer):Boolean;
  function FlashControl(Control:TControl):Boolean;

implementation
 uses ShlObj, ActiveX, System.Win.ComObj, PNGFunctions, PNGImageList;

function Centred(V1, V2:Integer):Integer;
begin
 Result:=(V1 div 2) - (V2 div 2);
end;

function CPUUsage(preIdleTime, preUserTime, preKrnlTime:TFileTime; ProcInterval:Integer):Extended;
var idle, user, krnl:TFileTime;
    i, u:int64;
begin
 GetSystemTimes(idle, krnl, user);
 i:=Abs(CompareFileTimeOwn(idle, preIdleTime));
 u:=Abs(CompareFileTimeOwn(user, preUserTime))+Abs(CompareFileTimeOwn(krnl, preKrnlTime));
 Result:=((u-i) / ProcInterval) / 100;
 Result:=Max(0, Min(Result, 100));
 preIdleTime:=idle;
 preUserTime:=user;
 preKrnlTime:=krnl;
end;

function HumanDateTime(Value:TDateTime; ShowTime:Boolean):string;
begin
 if IsSameDay(Value, Today+2) then Result:='Послезавтра'
 else
 if IsSameDay(Value, Today+1) then Result:='Завтра'
 else
 if IsSameDay(Value, Today) then Result:='Сегодня'
 else
 if IsSameDay(Value, Yesterday) then Result:='Вчера'
 else
 if IsSameDay(Value, Yesterday-1) then Result:='Позавчера'
 else
  begin
   if YearOf(Value) = YearOf(Now) then Result:=FormatDateTime('DD mmm, ddd', Value)
   else Result:=FormatDateTime('DD mmm YYYY, ddd', Value);
  end;

 if ShowTime then
  Result:=Result+FormatDateTime(' в HH:NN:SS', Value);
end;

function SimpleStrCompare(const Str1, Str2:string):Double;
var Len1, Len2, i, j, k, P1:Integer;
    S1, S2:string;
begin
 if Str1 = Str2 then Exit(1);
 if (Str1.IsEmpty) or (Str2.IsEmpty) then Exit(0);
 if Length(Str1) > Length(Str2)
 then begin S1:=Str1; S2:=Str2; end
 else begin S1:=Str2; S2:=Str1; end;
 Len1:=Length(S1);
 Len2:=Length(S2);
 P1:=0;
 for i:=0 to Len1-Len2 do
  begin
   j:=0;
   for k:=1 to Len2 do if S1[k+i] = S2[k] then Inc(j);
   if j > P1 then P1:=j;
  end;
 Result:=P1/Len1;
end;

function PercentRound(Value:Extended):Extended;
begin
 Result:=Max(0, Min(100, Value));
end;

function CompareFileTimeOwn(t1, t2:FILETIME):Int64;
begin
 Result:=((t2.dwHighDateTime shl 32) or (t2.dwLowDateTime)) - ((t1.dwHighDateTime shl 32) or (t1.dwLowDateTime));
end;

function TranslitRus2Lat(const Str:string):string;
const
 RArrayL = 'абвгдеёжзийклмнопрстуфхцчшщьыъэюя';
 RArrayU = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ';
 colChar = 33;
 arr: array[1..2, 1..ColChar] of string = (
  ('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y', 'k', 'l', 'm', 'n', 'o',
   'p', 'r', 's', 't', 'u', 'f', 'kh', 'ts', 'ch', 'sh', 'shch', '''', 'y', '''', 'e', 'yu', 'ya'),
  ('A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'Y', 'K', 'L', 'M', 'N', 'O',
   'P', 'R', 'S', 'T', 'U', 'F', 'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '''', 'Y', '''', 'E', 'Yu', 'Ya'));
var i, p:Integer;
       d:Byte;
begin
 Result:='';
 for i:=1 to Length(Str) do
  begin
   d:=1;
   p:=Pos(Str[i], RArrayL);
   if p = 0 then
    begin
     p:=Pos(Str[i], RArrayU);
     d:=2;
    end;
   if p <> 0 then
    Result:=Result + arr[d, p]
   else
    Result:=Result + Str[i];
  end;
end;

function GetSeconds(Time:TTime):Cardinal;
var H, M, S, MS:Word;
begin
 DecodeTime(Time, H, M, S, MS);
 Result:=H * SecsPerHour + M * SecsPerMin + S;
end;

function SecondsToStr(Value:Cardinal):string;
var H, M, S, DC:Integer;
begin
 Result:='';
 H:=Value div SecsPerHour;
 Value:=Value mod SecsPerHour;

 M:=Value div SecsPerMin;
 Value:=Value mod SecsPerMin;

 S:=Value;
 DC:=0;
 if H > 0 then
  begin
   Result:=Result+IntToStr(H)+' ч. ';
   Inc(DC);
  end;
 if M > 0 then
  begin
   Result:=Result+IntToStr(M)+' м. ';
   Inc(DC);
  end;
 if DC < 2 then
  if S > 0 then Result:=Result+IntToStr(S)+' с.';
end;

function Between(FMin, FValue, FMax:Integer):Boolean;
begin
 Result:=(FValue >= FMin) and (FValue <= FMax);
end;

function GetMachineName:string;
var Size:Cardinal;
    PRes:PChar;
    BRes:Boolean;
begin
 Result:='Не определено';
 try
  Size:=MAX_COMPUTERNAME_LENGTH + 1;
  PRes:=StrAlloc(Size);
  BRes:=GetComputerName(PRes, Size);
  if BRes then Result:=StrPas(PRes);
 except
  Exit;
 end;
end;

function GetUserName:string;
var a:array[0..254] of Char;
    lenBuf:Cardinal;
begin
 Result:='Не определено';
 try
  lenBuf:=255;
  Winapi.Windows.GetUserName(a, lenBuf);
  Result:=StrPas(a);
 except
  Exit;
 end;
end;

procedure SetImageListColor(ImgList:TImageList; Color:TColor);
var i:Integer;
begin
 for i:= 0 to ImgList.Count - 1 do ColorImages(ImgList, i, Color);
end;

function PngToIco(PNGObj: TPngImage):TIcon;
var PngImageList: TPngImageList;
begin
 PngImageList:= TPngImageList.Create(nil);
 PngImageList.Width:= PNGObj.Width;
 PngImageList.Height:= PNGObj.Height;
 PngImageList.PngImages.Add(False);
 PngImageList.PngImages[0].PngImage:= PNGObj;
 Result:= TIcon.Create;
 PngImageList.GetIcon(0, Result);
 PngImageList.Free;
end;

procedure ColorImages(IList:TImageList; ID:Integer; Color:TColor);
var Icon:TIcon;
    PNG, PNGNew:TPngImage;
begin
 if (ID < 0) or (ID > IList.Count - 1) then Exit;
 Icon:=TIcon.Create;
 Icon.Width:=IList.Width;
 Icon.Height:=IList.Height;
 IList.GetIcon(ID, Icon);
 PNG:=TPngImage.CreateBlank(COLOR_RGBALPHA, 16, Icon.Width, Icon.Height);
 ConvertToPNG(Icon, PNG);
 Icon.Free;
 PNGNew:=TPngImage.CreateBlank(COLOR_RGBALPHA, 16, Icon.Width, Icon.Height);
 PNGColored(0, 0, PNG, PNGNew, Color);
 PNG.Free;
 IList.ReplaceIcon(ID, PngToIco(PNGNew));
 PNGNew.Free;
end;

procedure AddToValueEdit(VE:TValueListEditor; Key, Value, ValueBU:string);
begin
 if Key = '' then Key:='Неизвестный параметр';
 if Length(Value) < 1 then
  if ValueBU <> '' then Value:=ValueBU;
 if Value <> '' then
  VE.Strings.Add(Key+'='+Value);
end;

procedure AutoColumn(LV:TListView);
var x, y, w:Integer;
    MaxWidth:Integer;
begin
 with LV do
  begin
   for x:= 1 to LV.Columns.Count - 1 do
    begin
     MaxWidth:=Canvas.TextWidth(LV.Columns[x].Caption);
     for y:=0 to LV.Items.Count - 1 do
      begin
       w:=Canvas.TextWidth(LV.Items[y].SubItems[x-1]);
       if w > MaxWidth then MaxWidth:=w;
      end;
     LV.Columns[x].Width:=MaxWidth + 15;
    end;
  end;
end;

procedure ClearGrid(StringGrid:TStringGrid);
var i, j:Integer;
begin
 for i:= 0 to StringGrid.ColCount - 1 do
  for j:= 0 to StringGrid.RowCount - 1 do StringGrid.Cells[i, j]:='';
end;


// BotToken ='377291178:AAFp-s5BqhaCCRuez1ludpJBbnc8Qw7A5B8'
// Chat_ID = '-181582579'
// SendText = YourText
{function SendTelegram(BotToken, Chat_ID, SendText:string):Boolean;
var lHttp: THTTPClient;
    lHttpResponse: IHTTPResponse;
    LParamToDate: TMultipartFormData;
begin
 LParamToDate:=TMultipartFormData.Create;
 try
  LParamToDate.AddField('chat_id', Chat_ID);
  LParamToDate.AddField('text', SendText);
  LParamToDate.AddField('disable_web_page_preview', 'false');
  LParamToDate.AddField('disable_notification', 'false');
  LParamToDate.AddField('reply_to_message_id', '0');
  lHttp:= THTTPClient.Create;
  try
   lHttpResponse:= lHttp.Post('https://api.telegram.org/bot' + BotToken + '/sendMessage', LParamToDate);
   Result:=lHttpResponse.StatusCode = 200;
  finally
   FreeAndNil(lHttp);
  end;
 finally
  LParamToDate.Free;
 end;
end;            }

function AdvSelectDirectory(const Caption:string; const Root:WideString; var Directory:string; EditBox:Boolean = False; ShowFiles:Boolean = False; AllowCreateDirs:Boolean = True):Boolean;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: lParam):Integer; stdcall;
begin
 case uMsg of
  BFFM_INITIALIZED: SendMessage(Wnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
 end;
 Result:= 0;
end;

var
 WindowList: Pointer;
 BrowseInfo: TBrowseInfo;
 Buffer: PChar;
 RootItemIDList, ItemIDList: PItemIDList;
 ShellMalloc: IMalloc;
 IDesktopFolder: IShellFolder;
 Eaten, Flags: LongWord;

const
 BIF_USENEWUI = $0040;
 BIF_NOCREATEDIRS = $0200;

begin
 Result:= False;
 if not DirectoryExists(Directory) then Directory:= '';
 FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
 if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
   Buffer:= ShellMalloc.Alloc(MAX_PATH);
   try
    RootItemIDList:= nil;
    if Root <> '' then
     begin
      SHGetDesktopFolder(IDesktopFolder);
      IDesktopFolder.ParseDisplayName(Application.Handle, nil, POleStr(Root), Eaten, RootItemIDList, Flags);
     end;
    OleInitialize(nil);
    with BrowseInfo do
     begin
      hwndOwner:= Application.Handle;
      pidlRoot:= RootItemIDList;
      pszDisplayName := Buffer;
      lpszTitle:= PChar(Caption);
      ulFlags:= BIF_RETURNONLYFSDIRS or BIF_USENEWUI or BIF_EDITBOX * Ord(EditBox) or
                BIF_BROWSEINCLUDEFILES * Ord(ShowFiles) or BIF_NOCREATEDIRS * Ord(not AllowCreateDirs);
      lpfn:=@SelectDirCB;
      if Directory <> '' then lParam:=Integer(PChar(Directory));
     end;
    WindowList:= DisableTaskWindows(0);
    try
     ItemIDList:= ShBrowseForFolder(BrowseInfo);
    finally
     EnableTaskWindows(WindowList);
    end;
    Result:= ItemIDList <> nil;
    if Result then
     begin
      ShGetPathFromIDList(ItemIDList, Buffer);
      ShellMalloc.Free(ItemIDList);
      Directory:= Buffer;
     end;
   finally
    ShellMalloc.Free(Buffer);
   end;
  end;
end;

function GetAeroColor: TColor;
var OpaqueBlend:Bool;
    AColor:DWord;
    A, R, G, B:Integer;
    OSInfo:TOSVersionInfo;
begin
 ZeroMemory(@OSInfo, SizeOf(OSInfo));
 OSInfo.dwOSVersionInfoSize:= SizeOf(TOSVERSIONINFO);
 if (((not GetVersionEx(OSInfo)) and
      (OSInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT) and
      (OSInfo.dwMajorVersion < 5))) or
     (Winapi.Dwmapi.DwmGetColorizationColor(AColor, OpaqueBlend) = S_FALSE)
 then
  begin
   Result:=clNone;
   Exit;
  end;
 A:= (AColor and $FF000000) shr 24;
 R:= (AColor and $00FF0000) shr 16;
 G:= (AColor and $0000FF00) shr  8;
 B:= (AColor and $000000FF);

 R:= Max(0, Min(R + (255-A-40), 255));
 G:= Max(0, Min(G + (255-A-40), 255));
 B:= Max(0, Min(B + (255-A-40), 255));
 Result:= RGB(R, G, B);
end;

function ColorDarker(Color:TColor; Percent:Byte):TColor;
var R, G, B:Byte;
begin
 Color:=ColorToRGB(Color);
 R:=GetRValue(Color);
 G:=GetGValue(Color);
 B:=GetBValue(Color);
 R:=R - MulDiv(R, Percent, 100);
 G:=G - MulDiv(G, Percent, 100);
 B:=B - MulDiv(B, Percent, 100);
 Result:=RGB(R, G, B);
end;

function ColorLighter(Color:TColor; Percent:Byte):TColor;
var R, G, B:Byte;
begin
 Color:=ColorToRGB(Color);
 R:=GetRValue(Color);
 G:=GetGValue(Color);
 B:=GetBValue(Color);
 R:=R + MulDiv(255-R, Percent, 100);
 G:=G + MulDiv(255-G, Percent, 100);
 B:=B + MulDiv(255-B, Percent, 100);
 Result:=RGB(R, G, B);
end;

function GetGroupID(LV:TListView; GrName:string):Integer;
var i:Integer;

function Add:Integer;
begin
 with LV.Groups.Add do
  begin
   Result:=ID;
   TitleImage:=0;
   State:=[lgsCollapsible];
   //Footer:='Количество элементов: 1';
   Header:=GrName;
  end;
end;

begin
 if (LV.Groups.Count <= 0) then Exit(Add);
 for i:=0 to LV.Groups.Count - 1 do
  if GrName = LV.Groups[i].Header then
   begin
    with LV.Groups[i] do
     begin
      TitleImage:=TitleImage+1;
      //Footer:='Количество элементов: '+IntToStr(TitleImage);
     end;
    Exit(i);
   end;
 Result:=Add;
end;

function GetJPGFromDll(LibName, ResName:string):TJPEGImage;
var DLLHandle: THandle;
    ResStream: TResourceStream;
begin
 try
  DLLHandle:=LoadLibrary(PChar(LibName));
  if DLLHandle = 0 then Exit(nil);
  ResStream:=TResourceStream.Create(DLLHandle, ResName, RT_RCDATA);
  Result:=TJPEGImage.Create;
  Result.LoadFromStream(ResStream);
  ResStream.Free;
  FreeLibrary(DLLHandle);
 except
  Result:=nil;
 end;
end;


function IndexInList(const Index:Integer; ListCount:Integer):Boolean;
begin
 Result:=(Index >= 0) and (Index <= ListCount - 1) and (ListCount > 0);
end;

function FlashControl(Control:TControl):Boolean;
var SaveColor, BufColor:TColor;

procedure SetColorE(Control:TEdit; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorME(Control:TMaskEdit; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorC(Control:TComboBox; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorP(Control:TPanel; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorM(Control:TMemo; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorL(Control:TListBox; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColor(Control:TControl; Color:TColor; var OldColor:TColor);
begin
 if Control is TEdit then SetColorE(Control as TEdit, Color, OldColor);
 if Control is TMaskEdit then SetColorME(Control as TMaskEdit, Color, OldColor);
 if Control is TComboBox then SetColorC(Control as TComboBox, Color, OldColor);
 if Control is TPanel then SetColorP(Control as TPanel, Color, OldColor);
 if Control is TMemo then SetColorM(Control as TMemo, Color, OldColor);
 if Control is TListBox then SetColorL(Control as TListBox, Color, OldColor);
end;

begin
 Result:=True;
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

function ScaledRect(const Src:TRect; Delta:Integer):TRect;
begin
 Result:=Src;                           //Rect(1, 1, 4, 4)
 Result.Left:=Result.Left - Delta;      //Scale 1 = Rect(0, 0, 5, 5)
 Result.Top:=Result.Top - Delta;
 Result.Right:=Result.Right + Delta;
 Result.Bottom:=Result.Bottom + Delta;
end;

function MixColors(Color1, Color2:TColor; Alpha:Byte):TColor;
var C1, C2:LongInt;
    R, G, B, V1, V2:Byte;
begin
 Alpha:=Round(2.55 * Alpha);
 C1:=ColorToRGB(Color1);
 C2:=ColorToRGB(Color2);
 V1:=Byte(C1);
 V2:=Byte(C2);
 R:=Alpha * (V1 - V2) shr 8 + V2;
 V1:=Byte(C1 shr 8);
 V2:=Byte(C2 shr 8);
 G:=Alpha * (V1 - V2) shr 8 + V2;
 V1:=Byte(C1 shr 16);
 V2:=Byte(C2 shr 16);
 B:=Alpha * (V1 - V2) shr 8 + V2;
 Result:=(B shl 16) + (G shl 8) + R;
end;

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: String; FDrawFlags:Cardinal): Integer;
var DrawRect: TRect;
    DrawFlags: Cardinal;
    DrawParams: TDrawTextParams;
begin
 DrawRect:=R;
 DrawFlags:= DT_END_ELLIPSIS or DT_NOPREFIX or DT_EDITCONTROL or FDrawFlags;
 DrawText(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
 DrawRect.Right:= R.Right;
 if DT_VCENTER and FDrawFlags = DT_VCENTER then
  begin
   if DrawRect.Bottom < R.Bottom then
        OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
   else DrawRect.Bottom:= R.Bottom;
  end;
 ZeroMemory(@DrawParams, SizeOf(DrawParams));
 DrawParams.cbSize:= SizeOf(DrawParams);
 DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
 Result:= DrawParams.uiLengthDrawn;
end;


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
