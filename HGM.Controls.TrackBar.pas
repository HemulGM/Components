unit HGM.Controls.TrackBar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  Vcl.Controls, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, HGM.Common, HGM.Controls.PanelExt;

type
  TTrackbarEvent = procedure(Sender: TObject; Position: Extended) of object;

  ThTrackbar = class(TCustomControl)
  private
    FMouseInScale: Boolean;
    FMouseInButton: Boolean;
    FMouseIsDown: Boolean;
    FMouseDownPos: TPoint;
    FMousePos: TPoint;
    FScalePercent: Extended;
    FScaleRect: TRect;
    FPosition: Extended;
    FOnChange: TTrackbarEvent;
    procedure DrawPanelTrackBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelTrackBarMouseEnter(Sender: TObject);
    procedure DrawPanelTrackBarMouseLeave(Sender: TObject);
    procedure DrawPanelTrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelTrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPaint;
    procedure Paint; override;
    function GetPosition: Extended;
    procedure SetPosition(const Value: Extended);
    procedure DoSetPosition(Value: Extended);
    procedure SetOnChange(const Value: TTrackbarEvent);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Position: Extended read GetPosition write SetPosition;
    property OnChange: TTrackbarEvent read FOnChange write SetOnChange;
  end;

procedure Register;

implementation

uses
  Math, Direct2D;

{ ThTrackbar }

procedure Register;
begin
  RegisterComponents(PackageName, [ThTrackbar]);
end;

constructor ThTrackbar.Create(AOwner: TComponent);
begin
  inherited;
  Width := 250;
  Height := 30;
  inherited OnMouseEnter := DrawPanelTrackBarMouseEnter;
  inherited OnMouseLeave := DrawPanelTrackBarMouseLeave;
  inherited OnMouseDown := DrawPanelTrackBarMouseDown;
  inherited OnMouseMove := DrawPanelTrackBarMouseMove;
  inherited OnMouseUp := DrawPanelTrackBarMouseUp;
  Color := 6901811;
  ParentBackground := False;
end;

procedure ThTrackbar.DrawPanelTrackBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownPos := Point(X, Y);
  case Button of
    TMouseButton.mbLeft:
      begin
        FMouseIsDown := True; //FScaleRect.Contains(FMousePos);
      end;
  end;
  DoPaint;
end;

procedure ThTrackbar.DrawPanelTrackBarMouseEnter(Sender: TObject);
begin
  FMouseInScale := True;
  DoPaint;
end;

procedure ThTrackbar.DrawPanelTrackBarMouseLeave(Sender: TObject);
begin
  FMouseInScale := False;
  DoPaint;
end;

procedure ThTrackbar.DoPaint;
begin
  Repaint;
end;

procedure ThTrackbar.DrawPanelTrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMousePos := Point(X, Y);
  Cursor := crHandPoint;
  DoPaint;
end;

procedure ThTrackbar.DrawPanelTrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    TMouseButton.mbLeft:
      begin
        if FMouseIsDown then
        begin
          DoSetPosition(FScalePercent);
          FMouseIsDown := False;
          FMouseDownPos := Point(-1, -1);
        end;
      end;
  end;
  DoPaint;
end;

procedure ThTrackbar.DoSetPosition(Value: Extended);
begin
  FPosition := Value;
  if Assigned(FOnChange) then
    FOnChange(Self, FPosition);
end;

function ThTrackbar.GetPosition: Extended;
begin
  Result := FPosition;
end;

procedure ThTrackbar.Paint;
const
  ColorScale = $00A5A3A2;
  ColorPos = $00F1E7DD;
  ColorPosBtn = $00BEB8B0;
  ColorBuf = $00DED6D1;
  PosMargin = 6;
  ScaleMarginSide = 12;
  ScaleMarginUpdown = 6;
var
  MRect, PosRect, PosScale: TRect;
  ScalePos, VisPos: Integer;
  xNewRgn, xOldRgn: HRGN;
begin
  inherited;
  MRect := ClientRect;
  with TDirect2DCanvas.Create(Canvas, MRect) do
  begin
    BeginDraw;
    Brush.Color := Color;
    FillRect(MRect);

    //Scale
    Pen.Color := ColorScale;
    Brush.Color := ColorScale;
    FScaleRect := MRect;
    FScaleRect.Inflate(-ScaleMarginSide, -ScaleMarginUpdown);
    if FMouseInScale then
      FScaleRect.Inflate(0, 0)
    else
      FScaleRect.Inflate(0, -2);
    RoundRect(FScaleRect, FScaleRect.Height, FScaleRect.Height);
    ScalePos := Max(FScaleRect.Left, Min(FMousePos.X, FScaleRect.Right));
    ScalePos := ScalePos - FScaleRect.Left;

    //Position
    if FMouseIsDown then
    begin
      VisPos := ScalePos;
      FScalePercent := (100 / FScaleRect.Width) * ScalePos;
    end
    else
    begin
      VisPos := Round((FScaleRect.Width / 100) * FPosition);
    end;

    xNewRgn := CreateRoundRectRgn(FScaleRect.Left, FScaleRect.Top, FScaleRect.Right, FScaleRect.Bottom, FScaleRect.Height, FScaleRect.Height);
    SelectObject(Canvas.Handle, xNewRgn);

    //Playing
    Pen.Color := ColorPos;
    Brush.Color := ColorPos;

    PosScale := FScaleRect;
    PosScale.Right := (FScaleRect.Left + VisPos);
    RoundRect(PosScale, PosScale.Height, PosScale.Height);

    Pen.Color := Color;
    Pen.Width := 3;

    Brush.Style := bsClear;
    //FScaleRect.Inflate(3, 2);
    RoundRect(FScaleRect, FScaleRect.Height, FScaleRect.Height);

    DeleteObject(xNewRgn);
    EndDraw;

    BeginDraw;
    xOldRgn := CreateRectRgn(ClientRect.Left, ClientRect.Top, ClientRect.Right, ClientRect.Bottom);
    SelectObject(Canvas.Handle, xOldRgn);

    //Ellipse
    if FMouseInScale then
    begin
      Pen.Width := 1;
      Pen.Color := ColorPosBtn;
      Brush.Color := ColorPosBtn;
      Brush.Style := bsSolid;
      PosRect := TRect.Create(TPoint.Create(0, 0), MRect.Height - PosMargin, MRect.Height - PosMargin);
      PosRect.SetLocation(PosScale.Right - PosRect.Width div 2, (PosScale.Top + PosScale.Height div 2)
        - PosRect.Height div 2);
      Ellipse(PosRect);
      if PosRect.Contains(FMousePos) then
      begin
        FMouseInButton := True;
      end;
    end;

    DeleteObject(xOldRgn);
    EndDraw;
    Free;
  end;
end;

procedure ThTrackbar.SetOnChange(const Value: TTrackbarEvent);
begin
  FOnChange := Value;
end;

procedure ThTrackbar.SetPosition(const Value: Extended);
begin
  FPosition := Value;
  FScalePercent := FPosition;
  DoPaint;
end;

end.

