unit LabelButton;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,
  Vcl.ExtCtrls, System.UITypes, TableDraw, Vcl.Direct2D, Winapi.D2D1;

type
  TLabelButton = class(TLabel)
   private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
   public
    constructor Create(AOwner: TComponent); override;
   published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

  TTextAligns = (taLeft, taTop, taRight, taBottom, taVCenter, taCenter, taWordBrake, taSingleLine);
  TextAling = set of TTextAligns;

  TLabelEx = class(TShape)
   private
    FLabel:string;
    FTextFormat:TextAling;
    FEllipseRectVertical:Boolean;
    FIgnorBounds:Boolean;
    FFont:TFont;
    FOnPaint:TNotifyEvent;
    FRoundRectParam:TPoint;
    procedure SetLabel(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetTextFormat(const Value: TextAling);
    procedure SetEllipseRectVertical(const Value: Boolean);
    procedure SetRoundRectParam(const Value: TPoint);
    procedure SetIgnorBounds(const Value: Boolean);
   protected
    procedure Paint; override;
   public
    procedure StyledColor(Value:TColor);
    constructor Create(AOwner: TComponent); override;
   published
    property Caption:string read FLabel write SetLabel;
    property Font:TFont read FFont write SetFont;
    property OnPaint:TNotifyEvent read FOnPaint write FOnPaint;
    property TextFormat:TextAling read FTextFormat write SetTextFormat;
    property RoundRectParam:TPoint read FRoundRectParam write SetRoundRectParam;
    property IgnorBounds:Boolean read FIgnorBounds write SetIgnorBounds;
    property EllipseRectVertical:Boolean read FEllipseRectVertical write SetEllipseRectVertical;
  end;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('LKDU', [TLabelButton]);
 RegisterComponents('LKDU', [TLabelEx]);
end;


{ TLableButton }

constructor TLabelButton.Create(AOwner: TComponent);
begin
 inherited;
 inherited OnMouseEnter:=MouseEnter;
 inherited OnMouseLeave:=MouseLeave;
 Cursor:=crHandPoint;
end;

procedure TLabelButton.MouseEnter(Sender: TObject);
begin
 Font.Style:=Font.Style + [fsUnderLine];
 if Assigned(OnMouseEnter) then OnMouseEnter(Sender);
end;

procedure TLabelButton.MouseLeave(Sender: TObject);
begin
 Font.Style:=Font.Style - [fsUnderLine];
 if Assigned(OnMouseLeave) then OnMouseLeave(Sender);
end;

{ TLabelEx }

constructor TLabelEx.Create(AOwner: TComponent);
begin
 inherited;
 StyledColor($00996666);
 FLabel:='Текст';
 FFont:=TFont.Create;
 FFont.Color:=clWhite;
 FFont.Size:=10;
 Width:=90;
 Height:=30;
 FRoundRectParam:=Point(0, 0);
 FIgnorBounds:=True;
 FTextFormat:=[taCenter, taVCenter, taWordBrake];
end;

procedure TLabelEx.Paint;
var LD2DCanvas: TDirect2DCanvas;
var X, Y, W, H, S, DF, Rx, Ry:Integer;
    FRect:TRect;
    d:Double;
    Str:string;
begin
 LD2DCanvas:=TDirect2DCanvas.Create(Canvas, ClientRect);
 with LD2DCanvas do
  begin
   RenderTarget.BeginDraw;
   RenderTarget.SetTransform(TD2DMatrix3x2F.Identity);
   Pen.Assign(Self.Pen);
   Brush.Assign(Self.Brush);
   X:=Pen.Width div 2;
   Y:=X;
   W:=Width - Pen.Width + 1;
   H:=Height - Pen.Width + 1;
   if Pen.Width = 0 then begin Dec(W); Dec(H); end;
   if W < H then S:=W else S:=H;
   if Self.Shape in [stSquare, stRoundSquare, stCircle] then
    begin
     Inc(X, (W - S) div 2); W:=S;
     Inc(Y, (H - S) div 2); H:=S;
    end;
    case Self.Shape of
     stRectangle,
     stSquare: Rectangle(X, Y, X + W, Y + H);
     stRoundRect,
     stRoundSquare:
      begin
       if FRoundRectParam.X = 0 then Rx:=S div 4 else Rx:=FRoundRectParam.X;
       if FRoundRectParam.Y = 0 then Ry:=S div 4 else Ry:=FRoundRectParam.Y;

       RoundRect(X, Y, X + W, Y + H, Rx, Ry);
      end;
     stCircle, stEllipse: Ellipse(X, Y, X + W, Y + H);
    end;
   RenderTarget.EndDraw;
  end;
 LD2DCanvas.Free;

 Canvas.Font.Assign(FFont);
 Canvas.Brush.Style:=bsClear;
 if FIgnorBounds then FRect:=ClientRect
 else
  begin
   d:=1.6; //6.8
   if FEllipseRectVertical then
        FRect:=Rect(Round(X + W / (6.8 / d)), Round(Y + H / (6.8 * d)), Round(X + W - W / (6.8 / d)), Round(Y + H - H / (6.8 * d)))
   else FRect:=Rect(Round(X + W / (6.8 * d)), Round(Y + H / (6.8 / d)), Round(X + W - W / (6.8 * d)), Round(Y + H - H / (6.8 / d)));
  end;
 DF:=0;
 if taLeft in FTextFormat then DF:=DF or DT_LEFT;
 if taRight in FTextFormat then DF:=DF or DT_RIGHT;
 if taTop in FTextFormat then DF:=DF or DT_TOP;
 if taBottom in FTextFormat then DF:=DF or DT_BOTTOM;
 if taVCenter in FTextFormat then DF:=DF or DT_VCENTER;
 if taCenter in FTextFormat then DF:=DF or DT_CENTER;
 if taWordBrake in FTextFormat then DF:=DF or DT_WORDBREAK;
 if taSingleLine in FTextFormat then DF:=DF or DT_SINGLELINE;
 Str:=FLabel;
 DrawTextCentered(Canvas, FRect, Str, DF);
end;

procedure TLabelEx.SetEllipseRectVertical(const Value: Boolean);
begin
 FEllipseRectVertical:= Value;
 Repaint;
end;

procedure TLabelEx.SetFont(const Value: TFont);
begin
 FFont:=Value;
 Repaint;
end;

procedure TLabelEx.SetIgnorBounds(const Value: Boolean);
begin
 FIgnorBounds:=Value;
 Repaint;
end;

procedure TLabelEx.SetLabel(const Value: string);
begin
 FLabel:=Value;
 Repaint;
end;

procedure TLabelEx.SetRoundRectParam(const Value: TPoint);
begin
 FRoundRectParam:= Value;
 Repaint;
end;

procedure TLabelEx.SetTextFormat(const Value: TextAling);
begin
 FTextFormat:= Value;
 Repaint;
end;

procedure TLabelEx.StyledColor(Value: TColor);
begin
 Brush.Color:=Value;
 Pen.Color:=ColorDarker(Brush.Color);
end;

end.
