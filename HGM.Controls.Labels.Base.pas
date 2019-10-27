unit HGM.Controls.Labels.Base;

interface

uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.Menus, Vcl.Graphics, Winapi.CommCtrl, Vcl.ImgList, Vcl.Themes,
  Vcl.StdCtrls;

type
  ThCustomLabel = class(TGraphicControl)
  private
    FFocusControl: TWinControl;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FDrawTextProc: TFNDrawText;
    FGlowSize: Integer;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
    FShowAccelChar: Boolean;
    FTransparentSet: Boolean;
    FEllipsisPosition: TEllipsisPosition;
    procedure DoDrawThemeTextEx(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
    procedure DoDrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
    function GetTransparent: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetEllipsisPosition(Value: TEllipsisPosition);
    procedure SetFocusControl(Value: TWinControl);
    procedure SetGlowSize(const Value: Integer);
    procedure SetLayout(Value: TTextLayout);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    procedure UpdateDrawTextProc;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    procedure UpdateStyleElements; override;
    procedure AdjustBounds; dynamic;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); dynamic;
    function GetLabelText: string; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property EllipsisPosition: TEllipsisPosition read FEllipsisPosition write SetEllipsisPosition default epNone;
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property Transparent: Boolean read GetTransparent write SetTransparent stored FTransparentSet;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  public
    constructor Create(AOwner: TComponent); override;
    property Caption;
    property Canvas;
    property GlowSize: Integer read FGlowSize write SetGlowSize default 0;
  end;

implementation

uses
  Vcl.Consts, System.RTLConsts, Vcl.ActnList, Winapi.UxTheme, Winapi.DwmApi,
  System.Types, System.UITypes, System.StrUtils, Vcl.ExtCtrls;

{ ThCustomLabel }

constructor ThCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Width := 65;
  Height := 17;
  FAutoSize := True;
  FShowAccelChar := True;
  { The "default" value for the Transparent property depends on
    if you have Themes available and enabled or not. If you have
    ever explicitly set it, that will override the default value. }
  if StyleServices.Enabled then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  UpdateDrawTextProc;
end;

procedure ThCustomLabel.CMEnabledChanged(var Message: TMessage);
begin
  if TStyleManager.IsCustomStyleActive then
    UpdateDrawTextProc;
  inherited;
end;

function ThCustomLabel.GetLabelText: string;
begin
  Result := Caption;
end;

procedure ThCustomLabel.DoDrawThemeTextEx(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
const
  CStates: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);
var
  LFormat: TTextFormat;
  LOptions: TStyleTextOptions;
begin
  LFormat := TTextFormatFlags(TextFlags);
  if csGlassPaint in ControlState then
    Include(LFormat, tfComposited);

  LOptions.Flags := [stfTextColor, {stfGlowSize, stfShadowColor, stfShadowOffset,} stfBorderColor, stfBorderSize];
  LOptions.TextColor := Canvas.Font.Color;
  LOptions.GlowSize := FGlowSize;
  LOptions.ShadowColor := clRed;
  LOptions.ShadowOffset := TPoint.Create(2, 2);
  LOptions.BorderColor := clGreen;
  LOptions.BorderSize := 5;

  StyleServices.DrawText(DC, StyleServices.GetElementDetails(CStates[Enabled]), Text, TextRect, LFormat, LOptions);
end;

procedure ThCustomLabel.DoDrawNormalText(DC: HDC; const Text: UnicodeString; var TextRect: TRect; TextFlags: Cardinal);
begin
  Winapi.Windows.DrawTextW(DC, Text, Length(Text), TextRect, TextFlags);
end;

procedure ThCustomLabel.DoDrawText(var Rect: TRect; Flags: Longint);
const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS, DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  Text, DText: string;
  NewRect: TRect;
  Height, Delim: Integer;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or FShowAccelChar and (Text[1] = '&') and (Length(Text) = 1)) then
    Text := Text + ' ';

  if Text <> '' then
  begin
    if not FShowAccelChar then
      Flags := Flags or DT_NOPREFIX;
    Flags := DrawTextBiDiModeFlags(Flags);
    Canvas.Font := Font;
    if (FEllipsisPosition <> epNone) and not FAutoSize then
    begin
      DText := Text;
      Flags := Flags and not DT_EXPANDTABS;
      Flags := Flags or Ellipsis[FEllipsisPosition];
      if FWordWrap and (FEllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
      begin
        repeat
          NewRect := Rect;
          Dec(NewRect.Right, Canvas.TextWidth(EllipsisStr));
          FDrawTextProc(Canvas.Handle, DText, NewRect, Flags or DT_CALCRECT);
          Height := NewRect.Bottom - NewRect.Top;
          if (Height > ClientHeight) and (Height > Canvas.Font.Height) then
          begin
            Delim := LastDelimiter(' '#9, Text);
            if Delim = 0 then
              Delim := Length(Text);
            Dec(Delim);
  {$IF NOT DEFINED(CLR)}
            if ByteType(Text, Delim) = mbLeadByte then
              Dec(Delim);
  {$ENDIF}
            Text := Copy(Text, 1, Delim);
            DText := Text + EllipsisStr;
            if Text = '' then
              Break;
          end
          else
            Break;
        until False;
      end;
      if Text <> '' then
        Text := DText;
    end;

    if Enabled or StyleServices.Enabled then
      FDrawTextProc(Canvas.Handle, Text, Rect, Flags)
    else
    begin
      OffsetRect(Rect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      FDrawTextProc(Canvas.Handle, Text, Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      FDrawTextProc(Canvas.Handle, Text, Rect, Flags);
    end;
  end;
end;

procedure FillGlassRect(Canvas: TCanvas; Rect: TRect);
var
  MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
  try
    FillRect(MemDC, Rect, Canvas.Brush.Handle);
    BufferedPaintMakeOpaque(PaintBuffer, Rect);
  finally
    EndBufferedPaint(PaintBuffer, True);
  end;
end;

procedure ThCustomLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  R, CalcRect: TRect;
  DrawStyle: Longint;
  ax, ay, ax2, ay2, iAlpha: Double;
  xo, yo: Integer;
begin
  with Canvas do
  begin
    R := ClientRect;
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      if not (csGlassPaint in ControlState) then
        FillRect(R)
      else
        FillGlassRect(Canvas, R);
    end;

    Brush.Style := bsClear;
    { DoDrawText takes care of BiDi alignments }
    if Font.Orientation = 0 then
    begin
      DrawStyle := DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
      { Calculate vertical layout }
      if FLayout <> tlTop then
      begin
        CalcRect := R;
        DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
        if FLayout = tlBottom then
          OffsetRect(R, 0, Height - CalcRect.Bottom)
        else
          OffsetRect(R, 0, (Height - CalcRect.Bottom) div 2);
      end;
      DoDrawText(R, DrawStyle);
    end
    else
    begin
      DrawStyle := DT_LEFT or DT_EXPANDTABS or DT_NOCLIP;
      CalcRect := R;
      DoDrawText(CalcRect, DrawStyle or DT_CALCRECT);
      iAlpha := (pi / 180) * (-Canvas.Font.Orientation div 10);
      ay := sin(iAlpha) * CalcRect.Width;
      ax := cos(iAlpha) * CalcRect.Width;
      ay2 := sin(iAlpha + (pi / 180) * 90) * CalcRect.Height;
      ax2 := cos(iAlpha + (pi / 180) * 90) * CalcRect.Height;
      xo := 0;
      yo := 0;

      case (((Canvas.Font.Orientation div 10) mod 360) div 90) of
        0:
          begin
            xo := -1;
            yo := -Round(ay);
          end;
        1:
          begin
            xo := -Round(ax) - 1;
            yo := -Round(ay) - Round(ay2) - 1;
          end;
        2:
          begin
            xo := -Round(ax) - Round(ax2) - 1;
            yo := -Round(ay2) - 1;
          end;
        3:
          begin
            xo := -Round(ax2) - 1;
            yo := 0;
          end;
      end;

      R := ClientRect;
      OffsetRect(R, xo, yo);
      DoDrawText(R, DrawStyle);
    end;
  end;
end;

procedure ThCustomLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure ThCustomLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  R: TRect;
  AAlignment: TAlignment;
  LSin, LCos: Double;
  LRcx, LRcy: Integer;
  p1: TPoint;
  LBox: TRect;

  procedure CalcBox(AIdx, X, Y: Integer; Init: Boolean = FALSE);
  begin
    if Init then
      LBox := Rect(X, Y, X, Y)
    else
    begin
      if X < LBox.Left then
        LBox.Left := X
      else if X > LBox.Right then
        LBox.Right := X;

      if Y < LBox.Top then
        LBox.Top := Y
      else if Y > LBox.Bottom then
        LBox.Bottom := Y;
    end;
  end;

begin
  if not (csReading in ComponentState) and FAutoSize then
  begin
    R := ClientRect;
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      if Font.Orientation = 0 then
        DoDrawText(R, (DT_EXPANDTABS or DT_CALCRECT or MASK_TF_COMPOSITED) or WordWraps[FWordWrap])
      else
        DoDrawText(R, DT_EXPANDTABS or DT_CALCRECT);
      Canvas.Handle := 0;
    finally
      ReleaseDC(0, DC);
    end;
    if Font.Orientation = 0 then
    begin
      X := Left;
      AAlignment := FAlignment;
      if UseRightToLeftAlignment then
        ChangeBiDiModeAlignment(AAlignment);
      if AAlignment = taRightJustify then
        Inc(X, Width - R.Right);
      SetBounds(X, Top, R.Right, R.Bottom);
    end
    else
    begin
      LSin := Sin((Font.Orientation / 10) * Pi / 180);
      LCos := Cos((Font.Orientation / 10) * Pi / 180);
      LRcx := R.Left + R.Width div 2;
      LRcy := R.Top + R.Height div 2;
      p1.x := (R.Left - LRcx);
      p1.Y := (R.Top - LRcy);
      CalcBox(0, Round(p1.X * LCos - p1.Y * LSin) + LRcx, Round(p1.X * LSin - p1.Y * LCos) + LRcy, TRUE);
      p1.X := (R.Right - LRcx);
      CalcBox(1, Round(p1.X * LCos - p1.Y * LSin) + LRcx, Round(p1.X * LSin - p1.Y * LCos) + LRcy);
      p1.Y := (R.Bottom - LRcy);
      CalcBox(2, Round(p1.X * LCos - p1.Y * LSin) + LRcx, Round(p1.X * LSin - p1.Y * LCos) + LRcy);
      p1.X := (R.Left - LRcx);
      CalcBox(3, Round(p1.X * LCos - p1.Y * LSin) + LRcx, Round(p1.X * LSin - p1.Y * LCos) + LRcy);
      OffsetRect(LBox, -LBox.Left, -LBox.Top);
      SetBounds(Left + LBox.Left, Top + LBox.Top, LBox.Right, LBox.Bottom);
    end;
  end;
end;

procedure ThCustomLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure ThCustomLabel.SetEllipsisPosition(Value: TEllipsisPosition);
begin
  if FEllipsisPosition <> Value then
  begin
    FEllipsisPosition := Value;
    FAutoSize := False;
    Invalidate;
  end;
end;

procedure ThCustomLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    FEllipsisPosition := epNone;
    AdjustBounds;
  end;
end;

function ThCustomLabel.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure ThCustomLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure ThCustomLabel.SetGlowSize(const Value: Integer);
begin
  if Value <> FGlowSize then
  begin
    FGlowSize := Value;
    Invalidate;
  end;
end;

procedure ThCustomLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar <> Value then
  begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure ThCustomLabel.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
  FTransparentSet := True;
end;

procedure ThCustomLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure ThCustomLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure ThCustomLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure ThCustomLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;

procedure ThCustomLabel.UpdateStyleElements;
begin
  Perform(CM_STYLECHANGED, 0, 0);
end;

procedure ThCustomLabel.CMStyleChanged(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and (seClient in StyleElements) then
    ControlStyle := ControlStyle - [csOpaque]
  else if not Transparent then
    ControlStyle := ControlStyle + [csOpaque];
  UpdateDrawTextProc;
  Invalidate;
end;

procedure ThCustomLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure ThCustomLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

procedure ThCustomLabel.UpdateDrawTextProc;
begin
  if StyleServices.Enabled then
  begin
    if Enabled and TStyleManager.IsCustomStyleActive and not (seFont in StyleElements) then
      FDrawTextProc := DoDrawNormalText
    else
      FDrawTextProc := DoDrawThemeTextEx;
  end
  else
    FDrawTextProc := DoDrawNormalText;
end;

end.

