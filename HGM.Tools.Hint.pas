unit HGM.Tools.Hint;

interface
 uses
  Vcl.Controls, System.Classes, Winapi.Messages, Winapi.Windows,
  System.SysUtils, Vcl.Graphics, Vcl.ExtCtrls, Vcl.Themes, Vcl.Forms,
  Vcl.ImgList, Vcl.ActnList, System.SyncObjs, System.Types, System.UITypes,

  HGM.Controls.PanelExt, HGM.Common;

 type
  TlkHint = class(TComponent)
   private
    FText:string;
    FTimerHide:TTimer;
    FTimerRepaint:TTimer;
    FRect:TRect;
    FHeight, FWidth:Integer;
    FPanel:TPanelExt;
    FAutoHide: Cardinal;
    FColor: TColor;
    FFont: TFont;
    FMaxWidth: Integer;
    procedure FHide;
    procedure FShow;
    procedure SetHintSize;
    procedure OnTimerHideTimer(Sender:TObject);
    procedure OnTimerRepaintTime(Sender:TObject);
    procedure OnPanelPaint(Sender:TObject);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetMaxWidth(const Value: Integer);
   public
    procedure Show(Control:TControl); overload;
    procedure Show(PosPoint:TPoint); overload;
    procedure Hide;
    constructor Create(AOwner: TComponent); override;
   published
    property Text:string read FText write FText;
    property AutoHide:Cardinal read FAutoHide write FAutoHide default 5000;
    property Color:TColor read FColor write SetColor default clWhite;
    property Font:TFont read FFont write SetFont;
    property MaxWidth:Integer read FMaxWidth write SetMaxWidth default 200;
  end;

procedure Register;

implementation
 uses Math;

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
 FTimerHide:=TTimer.Create(Self);
 FTimerHide.Enabled:=False;
 FTimerHide.OnTimer:=OnTimerHideTimer;
 FTimerRepaint:=TTimer.Create(Self);
 FTimerRepaint.Enabled:=False;
 FTimerRepaint.Interval := 50;
 FTimerRepaint.OnTimer:=OnTimerRepaintTime;
 FAutoHide := 5000;
 FMaxWidth := 200;
end;

procedure TlkHint.FHide;
begin
 if Assigned(FPanel) then FreeAndNil(FPanel);
 FTimerHide.Enabled:=False;
 FTimerRepaint.Enabled := False;
end;

procedure TlkHint.FShow;
var RGN:HRGN;
begin
 if not Assigned(FPanel) then
 begin
   FPanel:=TPanelExt.Create(nil);
   FPanel.DoubleBuffered:=True;
   FPanel.Visible:=False;
   FPanel.OnPaint:=OnPanelPaint;
   FPanel.ParentWindow:=GetDesktopWindow;
 end;

 FPanel.Left:=FRect.Left;
 FPanel.Top:=FRect.Top;
 FPanel.Width := FWidth;
 FPanel.Height := FHeight;

 FTimerRepaint.Enabled:=True;
 FTimerHide.Enabled:=True;
 FTimerHide.Interval:=FAutoHide;
 RGN:=CreateRoundRectRgn(0, 0, FWidth, FHeight, FHeight, FHeight);
 SetWindowRgn(FPanel.Handle, RGN, True);
 DeleteObject(RGN);
 FPanel.Repaint;
 FPanel.Show;
 FPanel.BringToFront;
end;

procedure TlkHint.SetColor(const Value: TColor);
begin
 FColor := Value;
 if Assigned(FPanel) then FPanel.Repaint;
end;

procedure TlkHint.SetFont(const Value: TFont);
begin
 FFont := Value;
 if Assigned(FPanel) then FPanel.Canvas.Font.Assign(Value);
end;

procedure TlkHint.SetHintSize;
begin
 if not Assigned(FPanel) then Exit;
 FHeight:=Max(FPanel.Canvas.TextHeight(FText), 10);
 FWidth:=Min(Max(FPanel.Canvas.TextWidth(FText), 10), FMaxWidth)+10;
end;

procedure TlkHint.SetMaxWidth(const Value: Integer);
begin
 FMaxWidth := Value;
end;

procedure TlkHint.Show(Control: TControl);
begin
 SetHintSize;
 FRect.Left:=(Control.Left+Control.Width div 2) - (FWidth div 2);
 FRect.Top:=Control.Top-FHeight;
 FRect.Right:=FRect.Left+FWidth;
 FRect.Bottom:=FRect.Top+FHeight;
 FShow;
end;

procedure TlkHint.Show(PosPoint: TPoint);
begin
 SetHintSize;
 FRect.Left:=Min(Max(PosPoint.X - (FWidth div 2), 0), Screen.DesktopWidth - FWidth);
 FRect.Top:=PosPoint.Y - FHeight;
 FRect.Right:=FRect.Left+FWidth;
 FRect.Bottom:=FRect.Top+FHeight;
 FShow;
end;

procedure TlkHint.Hide;
begin
 FHide;
end;

procedure TlkHint.OnPanelPaint(Sender: TObject);
var R:TRect;
    S:String;
begin
 if not Assigned(FPanel) then Exit;
 with FPanel.Canvas do
  begin
   Brush.Color := FColor;
   Brush.Style := bsSolid;
   FillRect(FPanel.ClientRect);
   R := FPanel.ClientRect;
   R.Offset(-1, -1);
   S := FText;
   Font.Assign(FFont);
   TextRect(R, S, [tfSingleLine, tfVerticalCenter, tfCenter]);
  end;
end;

procedure TlkHint.OnTimerHideTimer(Sender: TObject);
begin
 FHide;
end;

procedure TlkHint.OnTimerRepaintTime(Sender: TObject);
begin
 if Assigned(FPanel) then FPanel.Repaint;
end;

end.
