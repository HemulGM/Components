unit HGM.Popup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TPopupStyles = (psShowArrow, psAnimate, psShadow, psFrame);
  TPopupStyle = set of TPopupStyles;

  TFormPopup = class(TForm)
    imgUpArrow: TImage;
    imgDownArrow: TImage;
    Shape1: TShape;
    tmrAutoHide: TTimer;
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrAutoHideTimer(Sender: TObject);
  protected
    procedure WmActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  private
    FOwner: TForm;
    FControl: TWinControl;
    FAnimate: Boolean;
    FClosing: Boolean;
    FFreeMethod: TProc;
    procedure DoRelease;
  public
    constructor CreatePopup(AOwner: TForm; AControl: TWinControl; FreeMethod: TProc;
                            X, Y: Integer;
                            Style: TPopupStyle); overload;
    constructor CreateDown(AOwner: TForm; AControl: TWinControl; FreeMethod: TProc;
                           X, Y: Integer;
                           Style: TPopupStyle;
                           HideAfter: Integer = 0); overload;
  end;

implementation

{$R *.dfm}

{ TFormPopup }

constructor TFormPopup.CreateDown(AOwner: TForm; AControl: TWinControl; FreeMethod: TProc; X, Y: Integer; Style: TPopupStyle; HideAfter: Integer);
begin
  CreatePopup(AOwner, AControl, FreeMethod, X, Y, Style);
  if HideAfter > 0 then
  begin
    tmrAutoHide.Interval := HideAfter;
    tmrAutoHide.Enabled := True;
  end;
  imgUpArrow.Hide;
  imgDownArrow.Show;
  Top := Top - Height;
  FControl.Top := 1;
end;

procedure TFormPopup.DoRelease;
begin
  FControl.Hide;
  Winapi.Windows.SetParent(FControl.Handle, FOwner.Handle);
  Release;
end;

constructor TFormPopup.CreatePopup(AOwner: TForm; AControl: TWinControl; FreeMethod: TProc; X, Y: Integer; Style: TPopupStyle);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited Create(AOwner);
  FClosing := False;
  FOwner := AOwner;
  FControl := AControl;
  FAnimate := psAnimate in Style;
  FFreeMethod := FreeMethod;
  Winapi.Windows.SetParent(FControl.Handle, Handle);
  Left := X;
  Top := Y;
  if psShowArrow in Style then
  begin
    if psShadow in Style then SetClassLong(Handle, GCL_STYLE, GetWindowLong(Handle, GCL_STYLE) and CS_DROPSHADOW);
    ClientWidth := FControl.Width;
    ClientHeight := FControl.Height + imgUpArrow.Height;
    FControl.Left := 0;
    FControl.Top := imgUpArrow.Height;
    Color := clFuchsia;
    TransparentColor := True;
    imgUpArrow.Show;
  end
  else
  begin
    if psShadow in Style then
    begin
     SetClassLong(Handle, GCL_STYLE, GetWindowLong(Handle, GCL_STYLE) or CS_DROPSHADOW);
     Color := $00404040;
    end
    else
    begin
     Shape1.Hide;
     Color := clBtnFace;
    end;
    ClientWidth := FControl.Width;
    ClientHeight := FControl.Height;
    FControl.Left := 0;
    FControl.Top := 0;
    TransparentColor := False;
  end;
  if psFrame in Style then
  begin
    FControl.Left := FControl.Left + 1;
    FControl.Top := FControl.Top + 1;
    ClientWidth := ClientWidth + 2;
    ClientHeight := ClientHeight + 2;
  end;
  FControl.Show;
  FControl.BringToFront;
  FControl.SetFocus;
end;

procedure TFormPopup.FormActivate(Sender: TObject);
begin
  SendMessage(FOwner.Handle, WM_NCACTIVATE, Integer(True), 0);
end;

procedure TFormPopup.FormClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FClosing then
  begin
    Action := caNone;
    Exit;
  end
  else
   if Assigned(FFreeMethod) then FFreeMethod;
  FClosing := True;
end;

procedure TFormPopup.FormHide(Sender: TObject);
begin
  if FAnimate then
    AnimateWindow(Handle, 100, AW_BLEND or AW_HIDE);
  DoRelease;
end;

procedure TFormPopup.FormShow(Sender: TObject);
begin
  if Left + Width > Screen.DesktopRect.Right then
    Left := Screen.DesktopRect.Right - Width;
  if Top + Height > Screen.DesktopRect.Bottom then
    Top := Screen.DesktopRect.Bottom - Height;

  if Left < 0 then
    Left := 0;
  if Top < 0 then
    Top := 0;
  if FAnimate then
    AnimateWindow(Handle, 100, AW_BLEND);
end;

procedure TFormPopup.tmrAutoHideTimer(Sender: TObject);
begin
  if not Self.BoundsRect.Contains(Mouse.CursorPos) then
    Close;
end;

procedure TFormPopup.WmActivate(var Msg: TWMActivate);
begin
  SendMessage(FOwner.Handle, WM_NCACTIVATE, Ord(Msg.Active <> WA_INACTIVE), 0);
  inherited;
  if Msg.Active = WA_INACTIVE then
    DoRelease;
end;

end.
