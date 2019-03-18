unit HGM.Popup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TFormPopup = class(TForm)
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
  protected
   procedure WmActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  private
   FOwner:TForm;
   FControl:TWinControl;
   procedure DoRelease;
  public
   constructor Create(AOwner:TForm; AControl:TWinControl; X, Y:Integer); overload;
  end;

implementation

{$R *.dfm}

{ TFormPopup }

procedure TFormPopup.DoRelease;
begin
 FControl.Hide;
 Winapi.Windows.SetParent(FControl.Handle, FOwner.Handle);
 Release;
end;

constructor TFormPopup.Create(AOwner: TForm; AControl:TWinControl; X, Y:Integer);
begin
 inherited Create(AOwner);
 FOwner:=AOwner;
 FControl:=AControl;
 Winapi.Windows.SetParent(AControl.Handle, Handle);
 Left:=X;
 Top:=Y;
 ClientWidth:=AControl.Width+2;
 ClientHeight:=AControl.Height+2;
 AControl.Left:=1;
 AControl.Top:=1;
 AControl.Show;
 AControl.BringToFront;
 AControl.SetFocus;
end;

procedure TFormPopup.FormActivate(Sender: TObject);
begin
 SendMessage(FOwner.Handle, WM_NCACTIVATE, Integer(True), 0);
end;

procedure TFormPopup.FormClick(Sender: TObject);
begin
 Close;
end;

procedure TFormPopup.FormCreate(Sender: TObject);
const CS_DROPSHADOW = $00020000;
begin
 SetClassLong(Handle, GCL_STYLE, GetWindowLong(Handle, GCL_STYLE) or CS_DROPSHADOW);
end;

procedure TFormPopup.FormHide(Sender: TObject);
begin
 AnimateWindow(Handle, 100, AW_BLEND or AW_HIDE);
 DoRelease;
end;

procedure TFormPopup.FormShow(Sender: TObject);
begin
 if Left + Width > Screen.DesktopRect.Right then Left:=Screen.DesktopRect.Right - Width;
 if Top + Height > Screen.DesktopRect.Bottom then Top:=Screen.DesktopRect.Bottom - Height;

 AnimateWindow(Handle, 100, AW_BLEND);
end;

procedure TFormPopup.WmActivate(var Msg: TWMActivate);
begin
 SendMessage(FOwner.Handle, WM_NCACTIVATE, Ord(Msg.Active <> WA_INACTIVE), 0);
 inherited;
 if Msg.Active = WA_INACTIVE then
    DoRelease;
end;

end.
