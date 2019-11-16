unit HGM.Graphics.AnimateControl;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, HGM.Common;

type
  TAnimateControl = class(TThread)
  protected
    procedure Execute; override;
  public
    FHWnd: HWND;
    FShow: Boolean;
    constructor Create(Handle: HWND; Show: Boolean);
    class procedure Show(Handle: HWND);
    class procedure Hide(Handle: HWND);
  end;

implementation

{ TAnimateControl }

constructor TAnimateControl.Create(Handle: HWND; Show: Boolean);
begin
  FHWnd := Handle;
  FShow := Show;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TAnimateControl.Execute;
begin
  if FShow then
    AnimateWindow(FHWnd, 250, AW_VER_POSITIVE or AW_SLIDE)
  else
    AnimateWindow(FHWnd, 250, AW_HIDE or AW_VER_NEGATIVE or AW_SLIDE);
end;

class procedure TAnimateControl.Hide(Handle: HWND);
begin
  Create(Handle, False);
end;

class procedure TAnimateControl.Show(Handle: HWND);
begin
  Create(Handle, True);
end;

end.

