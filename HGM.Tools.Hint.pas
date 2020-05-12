unit HGM.Tools.Hint;

interface

uses
  Vcl.Controls, System.Classes, Winapi.Messages, Winapi.Windows, System.SysUtils, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Themes,
  Vcl.Forms, Vcl.ImgList, Vcl.ActnList, System.SyncObjs, System.Types, System.UITypes, HGM.Controls.PanelExt, HGM.Common;

type
  TlkHint = class(TComponent)
  private
    FText: string;
    FTimerHide: TTimer;
    FTimerRepaint: TTimer;
    FRect: TRect;
    FHeight, FWidth: Integer;
    FPanel: TPanelExt;
    FAutoHide: Cardinal;
    FColor: TColor;
    FFont: TFont;
    FMaxWidth: Integer;
    FRounded: Boolean;
    FBorderColor: TColor;
    procedure FHide;
    procedure FShow;
    procedure SetHintSize;
    procedure OnTimerHideTimer(Sender: TObject);
    procedure OnTimerRepaintTime(Sender: TObject);
    procedure OnPanelPaint(Sender: TObject);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetMaxWidth(const Value: Integer);
    procedure SetRounded(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
  public
    procedure Show(Control: TControl); overload;
    procedure Show(PosPoint: TPoint); overload;
    procedure Hide;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Text: string read FText write FText;
    property AutoHide: Cardinal read FAutoHide write FAutoHide default 5000;
    property Color: TColor read FColor write SetColor default clWhite;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $00878787;
    property Font: TFont read FFont write SetFont;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 200;
    property Rounded: Boolean read FRounded write SetRounded default False;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  RegisterComponents(PackageName, [TlkHint]);
end;

{ TlkHint }

constructor TlkHint.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Color := clBlack;
  FFont.Size := 10;
  FFont.Name := 'Segoe UI';
  FText := '';
  FColor := clWhite;
  FTimerHide := TTimer.Create(Self);
  FTimerHide.Enabled := False;
  FTimerHide.OnTimer := OnTimerHideTimer;
  FTimerRepaint := TTimer.Create(Self);
  FTimerRepaint.Enabled := False;
  FTimerRepaint.Interval := 50;
  FTimerRepaint.OnTimer := OnTimerRepaintTime;
  FAutoHide := 5000;
  FMaxWidth := 200;
  FRounded := False;
  FBorderColor := $00878787;
end;

destructor TlkHint.Destroy;
begin
  FFont.Free;
  FHide;
  inherited;
end;

procedure TlkHint.FHide;
begin
  if Assigned(FPanel) then
    FreeAndNil(FPanel);
  FTimerHide.Enabled := False;
  FTimerRepaint.Enabled := False;
end;

procedure TlkHint.FShow;
var
  RGN: HRGN;
begin
  if not Assigned(FPanel) then
  begin
    FPanel := TPanelExt.Create(nil);
    FPanel.DoubleBuffered := True;
    FPanel.Visible := False;
    FPanel.OnPaint := OnPanelPaint;
    FPanel.ParentWindow := GetDesktopWindow;
  end;

  FPanel.Left := FRect.Left;
  FPanel.Top := FRect.Top;
  FPanel.Width := FWidth;
  FPanel.Height := FHeight;

  FTimerRepaint.Enabled := True;
  FTimerHide.Enabled := True;
  FTimerHide.Interval := FAutoHide;
  if FRounded then
  begin
    RGN := CreateRoundRectRgn(0, 0, FWidth, FHeight, FHeight, FHeight);
    SetWindowRgn(FPanel.Handle, RGN, True);
    DeleteObject(RGN);
  end;
  FPanel.Repaint;
  FPanel.Show;
  FPanel.BringToFront;
end;

procedure TlkHint.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
end;

procedure TlkHint.SetColor(const Value: TColor);
begin
  FColor := Value;
  if Assigned(FPanel) then
    FPanel.Repaint;
end;

procedure TlkHint.SetFont(const Value: TFont);
begin
  FFont := Value;
  if Assigned(FPanel) then
    FPanel.Canvas.Font.Assign(Value);
end;

procedure TlkHint.SetHintSize;
begin
  if not Assigned(FPanel) then
    Exit;
  FHeight := Max(FPanel.Canvas.TextHeight(FText) + 4, 10);
  FWidth := Min(Max(FPanel.Canvas.TextWidth(FText) + 4, 10), FMaxWidth) + 10;
end;

procedure TlkHint.SetMaxWidth(const Value: Integer);
begin
  FMaxWidth := Value;
end;

procedure TlkHint.SetRounded(const Value: Boolean);
begin
  FRounded := Value;
end;

procedure TlkHint.Show(Control: TControl);
begin
  SetHintSize;
  FRect.Left := (Control.Left + Control.Width div 2) - (FWidth div 2);
  FRect.Top := Control.Top - FHeight;
  FRect.Right := FRect.Left + FWidth;
  FRect.Bottom := FRect.Top + FHeight;
  FShow;
end;

procedure TlkHint.Show(PosPoint: TPoint);
begin
  SetHintSize;
  FRect.Left := Min(Max(PosPoint.X - (FWidth div 2), 0), Screen.DesktopWidth - FWidth);
  FRect.Top := PosPoint.Y - FHeight;
  FRect.Right := FRect.Left + FWidth;
  FRect.Bottom := FRect.Top + FHeight;
  FShow;
end;

procedure TlkHint.Hide;
begin
  FHide;
end;

procedure TlkHint.OnPanelPaint(Sender: TObject);
var
  R: TRect;
  S: string;
begin
  if not Assigned(FPanel) then
    Exit;
  with FPanel.Canvas do
  begin
    Brush.Color := FColor;
    Brush.Style := bsSolid;
    Pen.Color := FBorderColor;
    if FRounded then
      RoundRect(FPanel.ClientRect.Left, FPanel.ClientRect.Top, FPanel.ClientRect.Right, FPanel.ClientRect.Bottom, FHeight, FHeight)
    else
      Rectangle(FPanel.ClientRect);
    R := FPanel.ClientRect;
    R.Offset(-1, -1);
    S := FText;
    Font.Assign(FFont);
    Brush.Style := bsClear;
    TextRect(R, S, [tfSingleLine, tfVerticalCenter, tfCenter]);
  end;
end;

procedure TlkHint.OnTimerHideTimer(Sender: TObject);
begin
  FHide;
end;

procedure TlkHint.OnTimerRepaintTime(Sender: TObject);
begin
  if Assigned(FPanel) then
    FPanel.Repaint;
end;

end.

