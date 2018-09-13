unit LKDU.Button;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.StdCtrls, System.Generics.Collections, Vcl.ExtCtrls, System.UITypes,
  TableDraw, Vcl.Direct2D, Winapi.D2D1;

type
  //TTextAligns = (taLeft, taTop, taRight, taBottom, taVCenter, taCenter, taWordBrake, taSingleLine);
  //TextAling = set of TTextAligns;
  TButtonFlatState = (bfsNormal, bfsOver, bfsPressed);

  TButtonFlatGroupItem = (giNone, giLeft, giCenter, giRight);

  TButtonFlat = class(TCustomControl)
   private
    FColors:array[TButtonFlatState] of TColor;
    FPen: TPen;
    FBrush: TBrush;
    FShape: TShapeType;
    FButtonState:TButtonFlatState;
    FDowned:Boolean;
    FMouseOver:Boolean;
    FLabel:string;
    FTextFormat:TTextFormat;
    FEllipseRectVertical:Boolean;
    FIgnorBounds:Boolean;
    FFont:TFont;
    FNeedColor:TColor;
    FAnimPerc:Integer;
    FStyledColor:TColor;
    FOnPaint:TNotifyEvent;
    FRoundRectParam:Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FFlat:Boolean;
    FImages:TImageList;
    FImageIndex:Integer;
    FImageOver:Integer;
    FImagePress:Integer;
    FTimerAnimate:TTimer;
    FImageIndentLeft: Integer;
    FImageIndentRight: Integer;
    FNotifyColor: TColor;
    FNotifyVisible: Boolean;
    FNotifyWidth: Integer;
    FTimedText:string;
    FTimerTT:TTimer;
    FDrawTimedText:Boolean;
    FGroupItemKind: TButtonFlatGroupItem;
    FImagesOver: TImageList;
    FImagesPress: TImageList;
    FFontOver: TFont;
    FFontDown: TFont;
    FFromColor: TColor;
    procedure SetLabel(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetStyledColor(const Value: TColor);
    procedure SetTextFormat(const Value: TTextFormat);
    procedure SetEllipseRectVertical(const Value: Boolean);
    procedure SetRoundRectParam(const Value: Integer);
    procedure SetIgnorBounds(const Value: Boolean);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetButtonState(const Value: TButtonFlatState);
    procedure OnTimerAnimateTime(Sender:TObject);
    procedure OnTimerTTTime(Sender:TObject);
    procedure StopAnimate;
    procedure SetNeedColor(const Value: TColor);
    procedure SetShape(Value: TShapeType);
    procedure SetColorNormal(const Value: TColor);
    procedure StyleChanged(Sender: TObject);
    function GetColorNormal: TColor;
    function GetColorOver: TColor;
    function GetColorPressed: TColor;
    procedure SetColorOver(const Value: TColor);
    procedure SetColorPressed(const Value: TColor);
    procedure SetImageIndentLeft(const Value: Integer);
    procedure SetImageIndentRight(const Value: Integer);
    procedure SetImageIndex(const Value: Integer);
    procedure SetNotifyColor(const Value: TColor);
    procedure SetNotifyVisible(const Value: Boolean);
    procedure SetNotifyWidth(const Value: Integer);
    procedure SetGroupItemKind(const Value: TButtonFlatGroupItem);
    procedure SetImages(const Value: TImageList);
    procedure SetImagesOver(const Value: TImageList);
    procedure SetImagesPress(const Value: TImageList);
    procedure SetFontDown(const Value: TFont);
    procedure SetFontOver(const Value: TFont);
    procedure SetFlat(const Value: Boolean);
    property ButtonState:TButtonFlatState read FButtonState write SetButtonState;
    property StyledColor:TColor read FStyledColor write SetStyledColor;
    property FromColor:TColor read FFromColor write FFromColor;
    property NeedColor:TColor read FNeedColor write SetNeedColor;
   protected
    procedure Paint; override;
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TimedText(Text:string; Delay:Cardinal);
   published
    property Align;
    property Anchors;
    property Canvas;
    property Caption:string read FLabel write SetLabel;
    property ColorNormal:TColor read GetColorNormal write SetColorNormal;
    property ColorOver:TColor read GetColorOver write SetColorOver;
    property ColorPressed:TColor read GetColorPressed write SetColorPressed;
    property Constraints;
    property Cursor default crHandPoint;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EllipseRectVertical:Boolean read FEllipseRectVertical write SetEllipseRectVertical default False;
    property Flat:Boolean read FFlat write SetFlat default True;
    property Font:TFont read FFont write SetFont;
    property FontOver:TFont read FFontOver write SetFontOver;
    property FontDown:TFont read FFontDown write SetFontDown;
    property GroupItemKind:TButtonFlatGroupItem read FGroupItemKind write SetGroupItemKind default giNone;
    property IgnorBounds:Boolean read FIgnorBounds write SetIgnorBounds default False;
    property ImageIndentLeft:Integer read FImageIndentLeft write SetImageIndentLeft default 3;
    property ImageIndentRight:Integer read FImageIndentRight write SetImageIndentRight default 0;
    property ImageIndex:Integer read FImageIndex write SetImageIndex default -1;
    property ImageOver:Integer read FImageOver write FImageOver default -1;
    property ImagePress:Integer read FImagePress write FImagePress default -1;
    property Images:TImageList read FImages write SetImages;
    property ImagesOver:TImageList read FImagesOver write SetImagesOver;
    property ImagesPress:TImageList read FImagesPress write SetImagesPress;
    property NotifyColor:TColor read FNotifyColor write SetNotifyColor default $0042A4FF;
    property NotifyVisible:Boolean read FNotifyVisible write SetNotifyVisible default False;
    property NotifyWidth:Integer read FNotifyWidth write SetNotifyWidth default 8;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnPaint:TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
    property ParentShowHint;
    property RoundRectParam:Integer read FRoundRectParam write SetRoundRectParam;
    property Shape: TShapeType read FShape write SetShape default stRectangle;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TextFormat:TTextFormat read FTextFormat write SetTextFormat;
    property Touch;
    property Visible;
  end;


procedure Register;

implementation
 uses Math;

procedure Register;
begin
 RegisterComponents('LKDU', [TButtonFlat]);
end;

{ TLabelEx }

procedure TButtonFlat.StyleChanged(Sender: TObject);
begin
 Invalidate;
end;

procedure TButtonFlat.TimedText(Text: string; Delay: Cardinal);
begin
 FTimedText:=Text;
 FDrawTimedText:=True;
 FTimerTT.Interval:=Delay;
 FTimerTT.Enabled:=True;
 Repaint;
end;

procedure TButtonFlat.SetShape(Value:TShapeType);
begin
 if FShape <> Value then
  begin
   FShape:=Value;
   Invalidate;
  end;
end;

procedure TButtonFlat.CMMouseEnter(var Message: TMessage);
begin
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
 FMouseOver:=True;
 ButtonState:=bfsOver;
end;

procedure TButtonFlat.CMMouseLeave(var Message: TMessage);
begin
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
 FMouseOver:=False;
 ButtonState:=bfsNormal;
end;

constructor TButtonFlat.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 inherited Cursor:=crHandPoint;
 ControlStyle:=ControlStyle + [csReplicatable, csOpaque];
 FTimerAnimate:=TTimer.Create(nil);
 FTimerAnimate.Interval:=10;
 FTimerAnimate.Enabled:=False;
 FTimerAnimate.OnTimer:=OnTimerAnimateTime;
 FTimerTT:=TTimer.Create(nil);
 FTimerTT.Interval:=100;
 FTimerTT.Enabled:=False;
 FTimerTT.OnTimer:=OnTimerTTTime;
 FDrawTimedText:=False;
 FAnimPerc:=0;
 FFlat:=True;
 FTimedText:='';
 FImageIndex:=-1;
 FImagePress:=-1;
 FImageOver:=-1;
 FImageIndentLeft:=3;
 FImageIndentRight:=0;
 FLabel:='Кнопка';
 FNotifyColor:=$0042A4FF;
 FNotifyWidth:=8;
 FNotifyVisible:=False;
 ParentColor:=False;
 TabStop:=True;
 ParentBackground:=False;

 FColors[bfsNormal]:=$00DFD3C4;
 FColors[bfsOver]:=$00AD8D64;
 FColors[bfsPressed]:=$009F7949;
 FDowned:=False;
 FMouseOver:=False;

 FFont:=TFont.Create;
 FFont.Color:=clWhite;
 FFont.Size:=10;

 FFontOver:=TFont.Create;
 FFontOver.Color:=clWhite;
 FFontOver.Size:=10;

 FFontDown:=TFont.Create;
 FFontDown.Color:=clWhite;
 FFontDown.Size:=10;

 Width:=90;
 Height:=30;
 FRoundRectParam:=0;
 FIgnorBounds:=True;
 FTextFormat:=[tfCenter, tfVerticalCenter, tfSingleLine];
 ButtonState:=bfsNormal;
 inherited OnMouseDown:=DoMouseDown;
 inherited OnMouseUp:=DoMouseUp;
end;

destructor TButtonFlat.Destroy;
begin
 FTimerAnimate.Free;
 FTimerTT.Free;
 inherited;
end;

procedure TButtonFlat.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 SetFocus;
 ButtonState:=bfsPressed;
 FDowned:=True;
 if Assigned(FOnMouseDown) then FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TButtonFlat.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FMouseOver then ButtonState:=bfsOver else ButtonState:=bfsNormal;
 FDowned:=False;
 if Assigned(FOnMouseUp) then FOnMouseUp(Sender, Button, Shift, X, Y);
end;

function TButtonFlat.GetColorNormal: TColor;
begin
 Result:=FColors[bfsNormal];
end;

function TButtonFlat.GetColorOver: TColor;
begin
 Result:=FColors[bfsOver];
end;

function TButtonFlat.GetColorPressed: TColor;
begin
 Result:=FColors[bfsPressed];
end;

procedure TButtonFlat.OnTimerAnimateTime(Sender: TObject);
var R, NR, G, NG, B, NB:Integer;
    pRGB:Cardinal;
    IncC:Integer;
begin
 IncC:=8;
 Inc(FAnimPerc, IncC);
 if FAnimPerc >= 100 then
  begin
   StopAnimate;
   StyledColor:=NeedColor;
  end
 else
  StyledColor:=MixColors(NeedColor, FromColor, FAnimPerc);

 {
 pRGB:=ColorToRGB(StyledColor);
 R:=GetRValue(pRGB);
 G:=GetGValue(pRGB);
 B:=GetBValue(pRGB);

 pRGB:=ColorToRGB(FNeedColor);
 NR:=GetRValue(pRGB);
 NG:=GetGValue(pRGB);
 NB:=GetBValue(pRGB);

 if Abs(R - NR) < IncC then R:=NR;
 if Abs(G - NG) < IncC then G:=NG;
 if Abs(B - NB) < IncC then B:=NB;
 if R < NR then R:=R+IncC else if R > NR then R:=R-IncC;
 if G < NG then G:=G+IncC else if G > NG then G:=G-IncC;
 if B < NB then B:=B+IncC else if B > NB then B:=B-IncC;

 R:=Max(0, Min(R, 255));
 G:=Max(0, Min(G, 255));
 B:=Max(0, Min(B, 255));
 StyledColor:=RGB(R, G, B);
 if StyledColor = FNeedColor then StopAnimate;}
end;

procedure TButtonFlat.OnTimerTTTime(Sender: TObject);
begin
 FDrawTimedText:=False;
 FTimerTT.Enabled:=False;
 Repaint;
end;

type
 TColorControl = class(TControl)
  public
   property Color;
 end;

procedure TButtonFlat.Paint;

var X, Y, W, H, S, Rx:Integer;
    DF:TDrawTextFlags;
    FRect:TRect;
    d:Double;
    FDrawImg:Integer;
begin
 Canvas.Lock;
 try
  with TDirect2DCanvas.Create(Canvas, ClientRect) do
   begin
    RenderTarget.BeginDraw;
    RenderTarget.SetTransform(TD2DMatrix3x2F.Identity);
    Brush.Style:=bsSolid;
    if Assigned(Parent) and (Parent is TControl) then
     begin
      Brush.Color:=TColorControl(Parent).Color;
      FillRect(ClientRect);
     end;
    Brush.Color:=StyledColor;
    // Плоский стиль
    if Flat then Pen.Color:=Brush.Color
    else Pen.Color:=ColorDarker(StyledColor);
    // Фигура
    X:=Pen.Width div 2;
    Y:=X;
    W:=Width - Pen.Width + 1;
    H:=Height - Pen.Width + 1;
    if Pen.Width = 0 then begin Dec(W); Dec(H); end;
    if W < H then S:=W else S:=H;
    if Self.Shape in [stSquare, stRoundSquare, stCircle] then
     begin
      if W < H then S:=W else S:=H;
      Inc(X, (W - S) div 2); W:=S;
      Inc(Y, (H - S) div 2); H:=S;
     end;
    case Self.Shape of
     stRectangle,
     stSquare: Rectangle(X, Y, X + W, Y + H);
     stRoundRect,
     stRoundSquare:
      begin
       if FRoundRectParam = 0 then Rx:=S div 4 else Rx:=FRoundRectParam;
       if GroupItemKind = giCenter then RoundRect(X, Y, X + W, Y + H, 0, 0)
       else RoundRect(X, Y, X + W, Y + H, Rx, Rx);
       case GroupItemKind of
        giLeft:
         begin
          FRect:=Rect(X, Y, X + W, Y + H);
          FRect.Offset(FRect.Width div 2 + 1, 0);
          FRect.Width:=FRect.Width div 2;
          Rectangle(FRect);
          FRect.Inflate(-1, -1);
          FRect.Left:=FRect.Left - 1;
          FillRect(FRect);
          FRect.Inflate(1, 1);
          FRect.Left:=FRect.Left + 1;
         end;
        giRight:
         begin
          FRect:=Rect(X, Y, X + W, Y + H);
          FRect.Width:=FRect.Width - (FRect.Width div 2);
          Rectangle(FRect);
          FRect.Inflate(0, -1);
          FRect.Left:=FRect.Right - 1;
          FillRect(FRect);
          FRect.Inflate(0, 1);
          FRect.Left:=FRect.Right + 1;
         end;
       end;
      end;
     stCircle, stEllipse: Ellipse(X, Y, X + W, Y + H);
    end;
    // Уведомление
    if FNotifyVisible then
     begin
      FRect:=Rect(0, 0, FNotifyWidth, FNotifyWidth);
      if Assigned(Images) then
       FRect.SetLocation(Min(ImageIndentLeft+Images.Width-4, ClientWidth-FNotifyWidth-2), (Height div 2 - Images.Height div 2)-4)
      else FRect.SetLocation(FNotifyWidth div 2, FNotifyWidth div 2);
      Brush.Style:=bsSolid;
      Pen.Style:=psSolid;
      Pen.Color:=FNotifyColor;
      Brush.Color:=FNotifyColor;
      Ellipse(FRect);
     end;
    //
    RenderTarget.EndDraw;
    Free;
   end;
  // Текст
  if Assigned(FFont) then Canvas.Font.Assign(FFont);
  case FButtonState of
   bfsOver: if Assigned(FFontOver) then Canvas.Font.Assign(FFontOver);
   bfsPressed: if Assigned(FFontDown) then Canvas.Font.Assign(FFontDown);
  end;
  Canvas.Brush.Color:=clWhite;
  Canvas.Brush.Style:=bsClear;
  if FIgnorBounds then FRect:=ClientRect
  else
   begin
    d:=1.6; //6.8
    if FEllipseRectVertical then
         FRect:=Rect(Round(X + W / (6.8 / d)), Round(Y + H / (6.8 * d)), Round(X + W - W / (6.8 / d)), Round(Y + H - H / (6.8 * d)))
    else FRect:=Rect(Round(X + W / (6.8 * d)), Round(Y + H / (6.8 / d)), Round(X + W - W / (6.8 * d)), Round(Y + H - H / (6.8 / d)));
   end;
  FRect.Offset(FImageIndentLeft, 0);
  FRect.Width:=FRect.Width - FImageIndentLeft;
  if Assigned(FImages) and (FImageIndex >= 0) then
   begin
    FRect.Offset(FImages.Width + FImageIndentRight, 0);
    FRect.Width:=FRect.Width - FImages.Width + FImageIndentRight;
    case FButtonState of
     bfsNormal:
      begin
       FDrawImg:=FImageIndex;
       if IndexInList(FDrawImg, FImages.Count) then FImages.Draw(Canvas, FImageIndentLeft, Height div 2 - FImages.Height div 2, FDrawImg, True);
      end;
     bfsOver:
      begin
       FDrawImg:=FImageOver;
       if FDrawImg < 0 then FDrawImg:=FImageIndex;
       if Assigned(FImagesOver) then
        begin
         if IndexInList(FDrawImg, FImagesOver.Count) then FImagesOver.Draw(Canvas, FImageIndentLeft, Height div 2 - FImagesOver.Height div 2, FDrawImg, True);
        end
       else
        begin
         if IndexInList(FDrawImg, FImages.Count) then FImages.Draw(Canvas, FImageIndentLeft, Height div 2 - FImages.Height div 2, FDrawImg, True);
        end;
      end;
     bfsPressed:
      begin
       FDrawImg:=FImagePress;
       if FDrawImg < 0 then FDrawImg:=FImageIndex;
       if Assigned(FImagesPress) then
        begin
         if IndexInList(FDrawImg, FImagesPress.Count) then FImagesPress.Draw(Canvas, FImageIndentLeft, Height div 2 - FImagesPress.Height div 2, FDrawImg, True);
        end
       else
        begin
         if IndexInList(FDrawImg, FImages.Count) then FImages.Draw(Canvas, FImageIndentLeft, Height div 2 - FImages.Height div 2, FDrawImg, True);
        end;
      end;
    end;
   end;
  ///
  Canvas.Brush.Style:=bsClear;
  DF:=TTextFormatFlags(FTextFormat);
  if FDrawTimedText then
   DrawTextCentered(Canvas, FRect, FTimedText, DF)
  else
   DrawTextCentered(Canvas, FRect, FLabel, DF);
  if Assigned(FOnPaint) then FOnPaint(Self);
 finally
  Canvas.Unlock;
 end;
end;

procedure TButtonFlat.SetButtonState(const Value: TButtonFlatState);
begin
 FButtonState:=Value;
 NeedColor:=FColors[FButtonState];
end;

procedure TButtonFlat.SetColorNormal(const Value: TColor);
begin
 FColors[bfsNormal]:=Value;
 ButtonState:=ButtonState;
end;

procedure TButtonFlat.SetColorOver(const Value: TColor);
begin
 FColors[bfsOver]:=Value;
 ButtonState:=ButtonState;
end;

procedure TButtonFlat.SetColorPressed(const Value: TColor);
begin
 FColors[bfsPressed]:=Value;
 ButtonState:=ButtonState;
end;

procedure TButtonFlat.SetEllipseRectVertical(const Value: Boolean);
begin
 FEllipseRectVertical:= Value;
 Repaint;
end;

procedure TButtonFlat.SetFlat(const Value: Boolean);
begin
 FFlat:=Value;
 Repaint;
end;

procedure TButtonFlat.SetFont(const Value: TFont);
begin
 FFont:=Value;
 Repaint;
end;

procedure TButtonFlat.SetFontDown(const Value: TFont);
begin
 FFontDown:=Value;
 Repaint;
end;

procedure TButtonFlat.SetFontOver(const Value: TFont);
begin
 FFontOver:=Value;
 Repaint;
end;

procedure TButtonFlat.SetGroupItemKind(const Value: TButtonFlatGroupItem);
begin
 FGroupItemKind := Value;
 Repaint;
end;

procedure TButtonFlat.SetIgnorBounds(const Value: Boolean);
begin
 FIgnorBounds:=Value;
 Repaint;
end;

procedure TButtonFlat.SetImageIndentLeft(const Value: Integer);
begin
 FImageIndentLeft:=Value;
 Repaint;
end;

procedure TButtonFlat.SetImageIndentRight(const Value: Integer);
begin
 FImageIndentRight:=Value;
 Repaint;
end;

procedure TButtonFlat.SetImageIndex(const Value: Integer);
begin
 FImageIndex:=Value;
 Repaint;
end;

procedure TButtonFlat.SetImages(const Value: TImageList);
begin
 FImages:= Value;
 Repaint;
end;

procedure TButtonFlat.SetImagesOver(const Value: TImageList);
begin
 FImagesOver:= Value;
 Repaint;
end;

procedure TButtonFlat.SetImagesPress(const Value: TImageList);
begin
 FImagesPress:= Value;
 Repaint;
end;

procedure TButtonFlat.SetLabel(const Value: string);
begin
 FLabel:=Value;
 Repaint;
end;

procedure TButtonFlat.SetNeedColor(const Value: TColor);
begin
 FNeedColor:=Value;
 if (csFreeNotification in ComponentState) then
  begin
   FromColor:=StyledColor;
   FTimerAnimate.Enabled:=True;
  end
 else StyledColor:=FNeedColor;
 Repaint;
end;

procedure TButtonFlat.SetNotifyColor(const Value: TColor);
begin
 FNotifyColor:=Value;
 Repaint;
end;

procedure TButtonFlat.SetNotifyVisible(const Value: Boolean);
begin
 FNotifyVisible:=Value;
 Repaint;
end;

procedure TButtonFlat.SetNotifyWidth(const Value: Integer);
begin
 FNotifyWidth := Value;
 Repaint;
end;

procedure TButtonFlat.StopAnimate;
begin
 FAnimPerc:=0;
 if (csFreeNotification in ComponentState) then FTimerAnimate.Enabled:=False;
end;

procedure TButtonFlat.SetRoundRectParam(const Value: integer);
begin
 FRoundRectParam:=Value;
 Repaint;
end;

procedure TButtonFlat.SetStyledColor(const Value: TColor);
begin
 FStyledColor:=Value;
 Repaint;
end;

procedure TButtonFlat.SetTextFormat(const Value: TTextFormat);
begin
 FTextFormat:=Value;
 Repaint;
end;

end.
