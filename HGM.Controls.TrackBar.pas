unit HGM.Controls.TrackBar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  Vcl.Controls, System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, HGM.Common, System.Types;

type
  TTrackbarEvent = procedure(Sender: TObject; Position: Extended) of object;

  TTrackbarOnHint = procedure(Sender: TObject; HintPosition: Extended; var Text: string) of object;

  ThTrackbar = class(TCustomControl)
  private
    FMouseInScale: Boolean;
    FMouseInButton: Boolean;
    FMouseIsDown: Boolean;
    FMouseDownPos: TPoint;
    FMousePos: TPoint;
    FScalePercent: Extended;
    FUnderPrecent: Extended;
    FScaleRect: TRect;
    FPosition: Extended;
    FSecondPosition: Extended;
    FOnChange: TTrackbarEvent;
    FCanChange: Boolean;
    FOnAdvHint: TTrackbarOnHint;
    FShowEllipse: Boolean;
    FColorPos: TColor;
    FColorPosBtn: TColor;
    FColorScale: TColor;
    FMarginScaleSide: Integer;
    FMarginPos: Integer;
    FMarginScaleUpdown: Integer;
    FColorBuf: TColor;
    FHotZoom: Boolean;
    procedure DrawPanelTrackBarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelTrackBarMouseEnter(Sender: TObject);
    procedure DrawPanelTrackBarMouseLeave(Sender: TObject);
    procedure DrawPanelTrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelTrackBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoPaint;
    function GetPosition: Extended;
    procedure SetPosition(const Value: Extended);
    procedure DoSetPosition(Value: Extended);
    procedure SetOnChange(const Value: TTrackbarEvent);
    procedure SetCanChange(const Value: Boolean);
    procedure SetOnAdvHint(const Value: TTrackbarOnHint);
    function DoOnAdvHint(HintPosition: Extended; var Text: string): Boolean;
    procedure SetShowEllipse(const Value: Boolean);
    function GetSecondPosition: Extended;
    procedure SetSecondPosition(const Value: Extended);
    procedure SetColorBuf(const Value: TColor);
    procedure SetColorPos(const Value: TColor);
    procedure SetColorPosBtn(const Value: TColor);
    procedure SetColorScale(const Value: TColor);
    procedure SetMarginPos(const Value: Integer);
    procedure SetMarginScaleSide(const Value: Integer);
    procedure SetMarginScaleUpdown(const Value: Integer);
    procedure SetHotZoom(const Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property MouseInButton: Boolean read FMouseInButton;
    procedure InitChange;
  published
    property Align;
    property Position: Extended read GetPosition write SetPosition;
    property SecondPosition: Extended read GetSecondPosition write SetSecondPosition;
    property OnChange: TTrackbarEvent read FOnChange write SetOnChange;
    property CanChange: Boolean read FCanChange write SetCanChange default True;
    property OnAdvHint: TTrackbarOnHint read FOnAdvHint write SetOnAdvHint;
    property ShowHint;
    property ShowEllipse: Boolean read FShowEllipse write SetShowEllipse default True;
    property Color;
    property ParentColor;
    property ParentBackground default False;
    property DoubleBuffered;
    property ColorScale: TColor read FColorScale write SetColorScale default $00A5A3A2;
    property ColorPos: TColor read FColorPos write SetColorPos default $00F1E7DD;
    property ColorPosBtn: TColor read FColorPosBtn write SetColorPosBtn default $00BEB8B0;
    property ColorBuf: TColor read FColorBuf write SetColorBuf default $00DED6D1;
    property MarginPos: Integer read FMarginPos write SetMarginPos default 6;
    property MarginScaleSide: Integer read FMarginScaleSide write SetMarginScaleSide default 12;
    property MarginScaleUpdown: Integer read FMarginScaleUpdown write SetMarginScaleUpdown default 6;
    property HotZoom: Boolean read FHotZoom write SetHotZoom default True;
    property Visible;
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
  FCanChange := True;
  FSecondPosition := 0;
  FPosition := 0;
  FShowEllipse := True;
  FColorScale := $00A5A3A2;
  FColorPos := $00F1E7DD;
  FColorPosBtn := $00BEB8B0;
  FColorBuf := $00DED6D1;
  FMarginPos := 6;
  FHotZoom := True;
  FMarginScaleSide := 12;
  FMarginScaleUpdown := 6;
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
        FMouseIsDown := FCanChange; //FScaleRect.Contains(FMousePos);
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

function ThTrackbar.DoOnAdvHint(HintPosition: Extended; var Text: string): Boolean;
begin
  Text := '';
  Result := False;
  if Assigned(FOnAdvHint) then
  begin
    FOnAdvHint(Self, HintPosition, Text);
    Result := not Text.IsEmpty;
  end;
end;

procedure ThTrackbar.DoPaint;
begin
  Repaint;
end;

procedure ThTrackbar.DrawPanelTrackBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  HintText: string;
begin
  FMousePos := Point(X, Y);
  Cursor := crHandPoint;
  if ShowHint then
  begin
    if DoOnAdvHint(FUnderPrecent, HintText) then
    begin
      Hint := HintText;
      Application.ActivateHint(Mouse.CursorPos);
    end;
  end;
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
  InitChange;
end;

function ThTrackbar.GetPosition: Extended;
begin
  Result := FPosition;
end;

function ThTrackbar.GetSecondPosition: Extended;
begin
  Result := FSecondPosition;
end;

procedure ThTrackbar.InitChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self, FPosition);
end;

procedure ThTrackbar.Paint;
var
  MRect, PosRect, PosScale: TRect;
  ScalePos, VisPos, BufPos: Integer;
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
    FScaleRect.Inflate(-MarginScaleSide, -MarginScaleUpdown);
    if FHotZoom and FMouseInScale then
      FScaleRect.Inflate(0, 0)
    else
      FScaleRect.Inflate(0, -2);
    RoundRect(FScaleRect, FScaleRect.Height, FScaleRect.Height);
    ScalePos := Max(FScaleRect.Left, Min(FMousePos.X, FScaleRect.Right));
    ScalePos := ScalePos - FScaleRect.Left;

    //Position
    if FScaleRect.Width > 0 then
      FUnderPrecent := (100 / FScaleRect.Width) * ScalePos
    else
      FUnderPrecent := 0;
    if FMouseIsDown then
    begin
      VisPos := ScalePos;
      FScalePercent := FUnderPrecent;
    end
    else
    begin
      VisPos := Round((FScaleRect.Width / 100) * FPosition);
    end;

    xNewRgn := CreateRoundRectRgn(FScaleRect.Left, FScaleRect.Top, FScaleRect.Right, FScaleRect.Bottom, FScaleRect.Height, FScaleRect.Height);
    SelectObject(Canvas.Handle, xNewRgn);

    //Buffering
    BufPos := Round((FScaleRect.Width / 100) * FSecondPosition);
    Pen.Color := ColorBuf;
    Brush.Color := ColorBuf;

    PosScale := FScaleRect;
    PosScale.Right := (FScaleRect.Left + BufPos);
    RoundRect(PosScale, PosScale.Height, PosScale.Height);

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
    if FShowEllipse and FMouseInScale then
    begin
      Pen.Width := 1;
      Pen.Color := ColorPosBtn;
      Brush.Color := ColorPosBtn;
      Brush.Style := bsSolid;
      PosRect := TRect.Create(TPoint.Create(0, 0), MRect.Height - MarginPos, MRect.Height - MarginPos);
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

procedure ThTrackbar.SetCanChange(const Value: Boolean);
begin
  FCanChange := Value;
end;

procedure ThTrackbar.SetColorBuf(const Value: TColor);
begin
  FColorBuf := Value;
  DoPaint;
end;

procedure ThTrackbar.SetColorPos(const Value: TColor);
begin
  FColorPos := Value;
  DoPaint;
end;

procedure ThTrackbar.SetColorPosBtn(const Value: TColor);
begin
  FColorPosBtn := Value;
  DoPaint;
end;

procedure ThTrackbar.SetColorScale(const Value: TColor);
begin
  FColorScale := Value;
  DoPaint;
end;

procedure ThTrackbar.SetHotZoom(const Value: Boolean);
begin
  FHotZoom := Value;
  DoPaint;
end;

procedure ThTrackbar.SetMarginPos(const Value: Integer);
begin
  FMarginPos := Value;
  DoPaint;
end;

procedure ThTrackbar.SetMarginScaleSide(const Value: Integer);
begin
  FMarginScaleSide := Value;
  DoPaint;
end;

procedure ThTrackbar.SetMarginScaleUpdown(const Value: Integer);
begin
  FMarginScaleUpdown := Value;
  DoPaint;
end;

procedure ThTrackbar.SetOnAdvHint(const Value: TTrackbarOnHint);
begin
  FOnAdvHint := Value;
end;

procedure ThTrackbar.SetOnChange(const Value: TTrackbarEvent);
begin
  FOnChange := Value;
end;

procedure ThTrackbar.SetPosition(const Value: Extended);
begin
  FPosition := Max(0, Min(100, Value));
  FScalePercent := FPosition;
  DoPaint;
end;

procedure ThTrackbar.SetSecondPosition(const Value: Extended);
begin
  FSecondPosition := Value;
  DoPaint;
end;

procedure ThTrackbar.SetShowEllipse(const Value: Boolean);
begin
  FShowEllipse := Value;
end;

end.

