unit HGM.Screenshot;

interface

uses
  System.Classes, System.SysUtils, FMX.Graphics, FMX.Types, {$IFDEF MSWINDOWS}Winapi.Windows, {$ENDIF MSWINDOWS}
  Vcl.Forms, Vcl.Graphics;

type
  TOnReadyFrame = procedure(Sender: TObject; Stream: TStream) of object;

  TScreenRecorder = class
  protected
    procedure Execute;
  private
    FThread: TThread;
    FActive: Boolean;
    FStop: Boolean;
    FOnReadyFrame: TOnReadyFrame;
    FStream: TMemoryStream;
    FInterval: Cardinal;
    procedure SetActive(const Value: Boolean);
    procedure SetOnReadyFrame(const Value: TOnReadyFrame);
    procedure DoOnReadyFrame;
    procedure SetInterval(const Value: Cardinal);
  public
    procedure Start;
    procedure Stop(Wait: Boolean);
    constructor Create;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
    property Interval: Cardinal read FInterval write SetInterval;
    property OnReadyFrame: TOnReadyFrame read FOnReadyFrame write SetOnReadyFrame;
  end;

procedure TakeScreenshot(Stream: TStream);

implementation

{$IFDEF MSWINDOWS}
procedure WriteWindowsToStream(Stream: TStream);
var
  DC: HDC;
  Palette: TLogPalette;
  Bitmap: Vcl.Graphics.TBitmap;
begin
  Bitmap := Vcl.Graphics.TBitmap.Create;
  try
    Bitmap.SetSize(Screen.Width, Screen.Height);
    DC := GetDc(0);
    if DC <> 0 then
    begin
      try
        if (GetDeviceCaps(DC, RASTERCAPS) and RC_PALETTE = RC_PALETTE) then
        begin
          Palette.palVersion := $300;
          Palette.palNumEntries := GetSystemPaletteEntries(DC, 0, 256, Palette.palPalEntry);
          if Palette.PalNumEntries <> 0 then
            Bitmap.Palette := CreatePalette(Palette);
        end;
        BitBlt(Bitmap.Canvas.Handle, 0, 0, Screen.Width, Screen.Height, DC, 0, 0, SRCCOPY);
        Bitmap.SaveToStream(Stream);
      finally
        ReleaseDc(0, DC);
      end;
    end;
  finally
    Bitmap.Free;
  end;
end;
{$ENDIF MSWINDOWS}

procedure TakeScreenshot(Stream: TStream);
begin
  {$IFDEF MSWINDOWS}
  WriteWindowsToStream(Stream);
  {$ENDIF MSWINDOWS}
end;

{ TScreenRecorder }

constructor TScreenRecorder.Create;
begin
  inherited;
  FInterval := Round(1000 / 25);  {25 кадров в сек}
  FStream := TMemoryStream.Create;
end;

destructor TScreenRecorder.Destroy;
begin
  Stop(True);
  FStream.Free;
  inherited;
end;

procedure TScreenRecorder.DoOnReadyFrame;
begin
  if Assigned(FOnReadyFrame) then
    FOnReadyFrame(Self, FStream);
end;

procedure TScreenRecorder.Execute;
begin
  FActive := True;
  try
    while not FStop do
    begin
      FStream.Clear;
      TakeScreenshot(FStream);
      FStream.Position := 0;
      if Assigned(FOnReadyFrame) then
        TThread.Synchronize(nil, DoOnReadyFrame);
      Sleep(FInterval);
    end;
  finally
    FActive := False;
    FStop := False;
  end;
end;

procedure TScreenRecorder.SetActive(const Value: Boolean);
begin
  if Value then
    Start
  else
    FStop := True;
end;

procedure TScreenRecorder.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
end;

procedure TScreenRecorder.SetOnReadyFrame(const Value: TOnReadyFrame);
begin
  FOnReadyFrame := Value;
end;

procedure TScreenRecorder.Start;
begin
  Stop(True);
  FStop := False;
  FThread := TThread.CreateAnonymousThread(Execute);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

procedure TScreenRecorder.Stop(Wait: Boolean);
begin
  FStop := True;
  if Wait and Assigned(FThread) then
    while not FThread.Finished do
    begin
      FThread.Yield;
      Application.ProcessMessages;
    end;
  if Assigned(FThread) then
  begin
    FThread.Free;
    FThread := nil;
  end;
end;

end.

