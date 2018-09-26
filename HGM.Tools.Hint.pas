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
    FTitle:string;
    FTimerHide:TTimer;
    FRect, FRectAnimate:TRect;
    FHeight, FWidth:Integer;
    FPanel:TPanelExt;
    FAutoHide: Cardinal;
    procedure FHide;
    procedure FShow;
    procedure SetHintSize;
    procedure OnTimerHideTimer(Sender:TObject);
    procedure OnPanelPaint(Sender:TObject);
   public
    procedure Show(Control:TControl); overload;
    procedure Show(PosPoint:TPoint); overload;
    procedure Hide;
    constructor Create(AOwner: TComponent); override;
   published
    property Text:string read FText write FText;
    property Title:string read FTitle write FTitle;
    property AutoHide:Cardinal read FAutoHide write FAutoHide;
  end;

procedure Register;

implementation
 uses Math;

procedure Register;
begin
 RegisterComponents(PackageName, [TlkHint]);
end;

{ TlkHint }

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: String; FDrawFlags:Cardinal = DT_CENTER): Integer;
var DrawRect: TRect;
    DrawFlags: Cardinal;
    DrawParams: TDrawTextParams;
begin
 DrawRect:= R;
 DrawFlags:= DT_END_ELLIPSIS or DT_NOPREFIX or DT_WORDBREAK or DT_EDITCONTROL or FDrawFlags;
 DrawText(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
 DrawRect.Right:= R.Right;
 if DrawRect.Bottom < R.Bottom then
      OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
 else DrawRect.Bottom:= R.Bottom;
 ZeroMemory(@DrawParams, SizeOf(DrawParams));
 DrawParams.cbSize:= SizeOf(DrawParams);
 DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
 Result:= DrawParams.uiLengthDrawn;
end;

constructor TlkHint.Create(AOwner: TComponent);
begin
 inherited;
 FTimerHide:=TTimer.Create(Self);
 FTimerHide.Enabled:=False;
 FTimerHide.OnTimer:=OnTimerHideTimer;
 FPanel:=TPanelExt.Create(Self);
 FPanel.DoubleBuffered:=True;
 FPanel.Visible:=False;
 FPanel.OnPaint:=OnPanelPaint;
 FPanel.ParentWindow:=GetDesktopWindow;
 AutoHide:=5000;
end;

procedure TlkHint.FHide;
begin
 FTimerHide.Enabled:=False;
 FPanel.Hide;
end;

procedure TlkHint.FShow;
begin
 FPanel.Left:=FRectAnimate.Left;
 FPanel.Width:=FRectAnimate.Right;
 //while FRect.Left <  do

 FTimerHide.Enabled:=True;
 FTimerHide.Interval:=FAutoHide;
 FPanel.Caption:=FText;
 FPanel.Show;
 FPanel.BringToFront;
end;

procedure TlkHint.SetHintSize;
var i:Integer;
    TxtH, TxtW:Integer;
    AText:string;
begin
 //DrawTextCentered
 AText:=FText;
 TxtH:=FPanel.Canvas.TextHeight(AText)+FPanel.Canvas.TextHeight(FTitle);
 TxtW:=FPanel.Canvas.TextWidth(AText);
 i:=0;
 while (TxtW / TxtH) > (5/1) do
  begin
   Delete(AText, 1, Length(AText) div 2);
   TxtW:=FPanel.Canvas.TextWidth(AText);
   Inc(i);
  end;
 TxtH:=TxtH+(TxtH*i);
 FHeight:=TxtH+15;
 FWidth:=TxtW+30;
end;

procedure TlkHint.Show(Control: TControl);
begin
 SetHintSize;
 FRect.Left:=(Control.Left+Control.Width div 2) - (FWidth div 2);
 FRect.Right:=FRect.Left+FWidth;
 FRect.Top:=Control.Top-FHeight;
 FRect.Bottom:=FRect.Top+FHeight;
 FRectAnimate:=FRect;
 FRectAnimate.Left:=(Control.Left+Control.Width div 2);
 FRectAnimate.Right:=FRectAnimate.Left;
 FShow;
end;

procedure TlkHint.Show(PosPoint: TPoint);
begin
 SetHintSize;
 FShow;
end;

procedure TlkHint.Hide;
begin
 FHide;
end;

procedure TlkHint.OnPanelPaint(Sender: TObject);
begin
 FPanel.Canvas.FillRect(FPanel.ClientRect);
 FPanel.Canvas.TextOut(0, 0, FTitle);
 DrawTextCentered(FPanel.Canvas, Rect(0, FPanel.Canvas.TextHeight(FTitle)+5, FPanel.Width, FPanel.Height), FText, DT_LEFT);
end;

procedure TlkHint.OnTimerHideTimer(Sender: TObject);
begin
 FHide;
end;

end.
