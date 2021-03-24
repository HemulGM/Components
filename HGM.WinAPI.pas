unit HGM.WinAPI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Winapi.Dwmapi, System.Math;

function AdvSelectDirectory(const Caption: string; const Root: WideString; var Directory: string; EditBox: Boolean = False; ShowFiles: Boolean = False; AllowCreateDirs: Boolean = True): Boolean;

function CompareFileTimeOwn(t1, t2: FILETIME): Int64;

function CPUUsage(var preIdleTime, preUserTime, preKrnlTime: TFileTime; ProcInterval: Integer): Extended;

function GetAeroColor: TColor;

function GetMachineName: string;

function GetUserName: string;

function GetAppData: string;

function SIDToString(ASID: PSID): string;

function GetComputerSID: string;

function GetFromConsole(Caption: string; var Text: string): Boolean;

function GetFileNameFromLink(LinkFileName: string): string;

function GetFileDescription(const FileName, ExceptText: string): string;

function ShowModalFor(Parent: TWinControl; Form: TForm): TModalResult;

function GetVersion: string;

function ConvertSidToStringSid(Sid:PSID; out StringSid:PChar):BOOL; stdcall; external 'ADVAPI32.DLL' name {$IFDEF UNICODE} 'ConvertSidToStringSidW'{$ELSE} 'ConvertSidToStringSidA'{$ENDIF};

implementation

uses
  ShlObj, ComObj, ActiveX, HGM.Common.Utils;

function GetVersion: string;
var
  Res: TResourceStream;
  Info: PVSFixedFileInfo;
  InfoLen: Cardinal;
begin
  Result := '';
  Res := TResourceStream.Create(HInstance, '#1', RT_VERSION);
  try
    if Res.Size > 0 then
    begin
      if VerQueryValue(Res.Memory, '\', Pointer(Info), InfoLen) then
      begin
        Result := Format('%d.%d.%d.%d', [Info.dwFileVersionMS shr $10, Info.dwFileVersionMS and $FFFF, Info.dwFileVersionLS shr $10, Info.dwFileVersionLS and $FFFF]);
      end;
    end;
  finally
    Res.Free;
  end;
end;

function ShowModalFor(Parent: TWinControl; Form: TForm): TModalResult;
begin
  Form.Show;
  //Parent.Enabled := False;
  try
    SendMessage(Form.Handle, CM_ACTIVATE, 0, 0);
    Form.ModalResult := 0;
    Form.BringToFront;
    repeat
      Application.HandleMessage;
      if Application.Terminated then
        Form.ModalResult := mrCancel;
    until Form.ModalResult <> 0;
    Result := Form.ModalResult;
    SendMessage(Form.Handle, CM_DEACTIVATE, 0, 0);
  finally
    Form.Hide;
    //Parent.Enabled := True;
  end;
end;

function GetFileDescription(const FileName, ExceptText: string): string;
type
  TLangRec = array[0..1] of Word;
var
  InfoSize, zero: Cardinal;
  pbuff: Pointer;
  pk: Pointer;
  nk: Cardinal;
  lang_hex_str: string;
  LangID: Word;
  LangCP: Word;
begin
  pbuff := nil;
  Result := '';
  InfoSize := Winapi.Windows.GetFileVersionInfoSize(PChar(FileName), zero);
  if InfoSize <> 0 then
  try
    GetMem(pbuff, InfoSize);
    if Winapi.Windows.GetFileVersionInfo(PChar(FileName), 0, InfoSize, pbuff) then
    begin
      if VerQueryValue(pbuff, '\VarFileInfo\Translation', pk, nk) then
      begin
        LangID := TLangRec(pk^)[0];
        LangCP := TLangRec(pk^)[1];
        lang_hex_str := Format('%.4x', [LangID]) + Format('%.4x', [LangCP]);  //FileDescription
        if VerQueryValue(pbuff, PChar('\\StringFileInfo\\' + lang_hex_str + '\\FileDescription'), pk, nk) then
          Result := string(PChar(pk))
        else if VerQueryValue(pbuff, PChar('\\StringFileInfo\\' + lang_hex_str + '\\CompanyName'), pk, nk) then
          Result := string(PChar(pk));
      end;
    end;
  finally
    if pbuff <> nil then
      FreeMem(pbuff);
  end;
  if Result = '' then
    if (ExceptText <> '') then
      if (ExceptText <> '/') then
        Result := ExceptText
      else
        Exit('')
    else
      Result := GetFileNameWoE(FileName);
end;

function GetAppData: string;
var
  Path: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, Path) = S_OK then
    Result := IncludeTrailingPathDelimiter(Path)
  else
    Result := '';
end;

function GetFileNameFromLink(LinkFileName: string): string;
var
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  FileInfo: TWin32FINDDATA;
  Buff: array[0..MAX_PATH] of Char;
begin
  Result := '';
  if not FileExists(LinkFileName) then
    Exit;
  MyObject := CreateComObject(CLSID_ShellLink);
  MyPFile := MyObject as IPersistFile;
  MySLink := MyObject as IShellLink;
  MyPFile.Load(PWideChar(LinkFileName), STGM_READ);
  MySLink.GetPath(Buff, MAX_PATH, FileInfo, SLGP_UNCPRIORITY);
  Result := Buff;
end;

function GetFromConsole(Caption: string; var Text: string): Boolean;
begin
  try
    AllocConsole;
    Write(Caption);
    Readln(Text);
    Result := True;
  finally
    FreeConsole;
  end;
end;

function SIDToString(ASID: PSID): string;
var
  StringSid: PChar;
begin
  ConvertSidToStringSid(ASID, StringSid);
  Result := string(StringSid);
end;

function GetComputerSID: string;
var
  Sid: PSID;
  cbSid: DWORD;
  cbReferencedDomainName: DWORD;
  ReferencedDomainName: string;
  peUse: SID_NAME_USE;
  Success: BOOL;
  lpSystemName: string;
  lpAccountName: string;
begin
  Sid := nil;
  try
    lpSystemName := '';
    lpAccountName := GetMachineName;

    cbSid := 0;
    cbReferencedDomainName := 0;
    Success := LookupAccountName(PChar(lpSystemName), PChar(lpAccountName), nil, cbSid, nil, cbReferencedDomainName, peUse);
    if (not Success) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
    begin
      SetLength(ReferencedDomainName, cbReferencedDomainName);
      Sid := AllocMem(cbSid);
      Success := LookupAccountName(PChar(lpSystemName), PChar(lpAccountName), Sid, cbSid, PChar(ReferencedDomainName), cbReferencedDomainName, peUse);
      if not Success then
      begin
        FreeMem(Sid);
        Sid := nil;
        RaiseLastOSError;
      end
      else
        Result := SIDToString(Sid);
    end
    else
      RaiseLastOSError;
  finally
    if Assigned(Sid) then
      FreeMem(Sid);
  end;
end;

function CPUUsage(var preIdleTime, preUserTime, preKrnlTime: TFileTime; ProcInterval: Integer): Extended;
var
  idle, user, krnl: TFileTime;
  i, u: int64;
begin
  GetSystemTimes(idle, krnl, user);
  i := Abs(CompareFileTimeOwn(idle, preIdleTime));
  u := Abs(CompareFileTimeOwn(user, preUserTime)) + Abs(CompareFileTimeOwn(krnl, preKrnlTime));
  Result := ((u - i) / ProcInterval) / 100;
  Result := Max(0, Min(Result, 100));
  preIdleTime := idle;
  preUserTime := user;
  preKrnlTime := krnl;
end;

function CompareFileTimeOwn(t1, t2: FILETIME): Int64;
begin
  Result := ((t2.dwHighDateTime shl 32) or (t2.dwLowDateTime)) - ((t1.dwHighDateTime shl 32) or (t1.dwLowDateTime));
end;

function GetMachineName: string;
var
  Size: Cardinal;
  PRes: PChar;
  BRes: Boolean;
begin
  Result := 'Не определено';
  try
    Size := MAX_COMPUTERNAME_LENGTH + 1;
    PRes := StrAlloc(Size);
    BRes := GetComputerName(PRes, Size);
    if BRes then
      Result := StrPas(PRes);
  except
    Exit;
  end;
end;

function GetUserName: string;
var
  a: array[0..254] of Char;
  lenBuf: Cardinal;
begin
  Result := 'Не определено';
  try
    lenBuf := 255;
    Winapi.Windows.GetUserName(a, lenBuf);
    Result := StrPas(a);
  except
    Exit;
  end;
end;

function AdvSelectDirectory(const Caption: string; const Root: WideString; var Directory: string; EditBox: Boolean = False; ShowFiles: Boolean = False; AllowCreateDirs: Boolean = True): Boolean;

  function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: lParam): Integer; stdcall;
  begin
    case uMsg of
      BFFM_INITIALIZED:
        SendMessage(Wnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
    end;
    Result := 0;
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
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil, POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      OleInitialize(nil);
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI or BIF_EDITBOX * Ord(EditBox) or BIF_BROWSEINCLUDEFILES * Ord(ShowFiles) or BIF_NOCREATEDIRS * Ord(not AllowCreateDirs);
        lpfn := @SelectDirCB;
        if Directory <> '' then
          lParam := Integer(PChar(Directory));
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result := ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function GetAeroColor: TColor;
var
  OpaqueBlend: Bool;
  AColor: DWord;
  A, R, G, B: Integer;
  OSInfo: TOSVersionInfo;
begin
  ZeroMemory(@OSInfo, SizeOf(OSInfo));
  OSInfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
  if (((not GetVersionEx(OSInfo)) and (OSInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT) and (OSInfo.dwMajorVersion < 5))) or (Winapi.Dwmapi.DwmGetColorizationColor(AColor, OpaqueBlend) = S_FALSE) then
  begin
    Result := clNone;
    Exit;
  end;
  A := (AColor and $FF000000) shr 24;
  R := (AColor and $00FF0000) shr 16;
  G := (AColor and $0000FF00) shr 8;
  B := (AColor and $000000FF);

  R := Max(0, Min(R + (255 - A - 40), 255));
  G := Max(0, Min(G + (255 - A - 40), 255));
  B := Max(0, Min(B + (255 - A - 40), 255));
  Result := RGB(R, G, B);
end;

end.

