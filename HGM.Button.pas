unit HGM.Button;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.StdCtrls, System.Generics.Collections, Vcl.ExtCtrls, System.UITypes,
  System.Types, HGM.Controls.VirtualTable, Vcl.Direct2D, Winapi.D2D1, HGM.Common,
  HGM.Common.Utils, Vcl.Menus, System.SysUtils;

type
  TButtonFlatState = (bfsNormal, bfsOver, bfsPressed);

  TButtonFlatGroupItem = (giNone, giLeft, giCenter, giRight);

  TButtonFlat = class(TCustomControl)
  private
    FColors: array[TButtonFlatState] of TColor;
    FShape: TShapeType;
    FGettingTextWidth: Boolean;
    FTextWidth: Integer;
    FSubText: string;
    FButtonState: TButtonFlatState;
    FPrevState: TButtonFlatState;
    FDowned: Boolean;
    FMouseOver: Boolean;
    FLabel: string;
    FTextFormat: TTextFormat;
    FEllipseRectVertical: Boolean;
    FIgnorBounds: Boolean;
    FNeedColor: TColor;
    FAnimPerc: Integer;
    FTimerProcing: Boolean;
    FStyledColor: TColor;
    FOnPaint: TNotifyEvent;
    FRoundRectParam: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FFlat: Boolean;
    FImages: TImageList;
    FImageIndex: Integer;
    FImageOver: Integer;
    FImagePress: Integer;
    FTimerAnimate: TTimer;
    FImageIndentLeft: Integer;
    FImageIndentRight: Integer;
    FNotifyColor: TColor;
    FNotifyVisible: Boolean;
    FNotifyWidth: Integer;
    FTimedText: string;
    FTimerTT: TTimer;
    FTimerAutoClick: TTimer;
    FDrawTimedText: Boolean;
    FGroupItemKind: TButtonFlatGroupItem;
    FImagesOver: TImageList;
    FImagesPress: TImageList;
    FFontOver: TFont;
    FFontDown: TFont;
    FFromColor: TColor;
    FTransparent: Boolean;
    FShowFocusRect: Boolean;
    FVisibleSubText: Boolean;
    FDblClickTooClick: Boolean;
    FAutoClick: Cardinal;
    FPopup: TPopupMenu;
    FBorderColor: TColor;
    FEllipseAnimate: Boolean;
    FSubTextColor: TColor;
    FSubTextFont: TFont;
    FBorderWidth: Integer;
    FShowCaption: Boolean;
    procedure FOnDblClick(Sender: TObject);
    function FGetTextWidth: Integer;
    procedure SetLabel(const Value: string);
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
    procedure OnTimerAnimateTime(Sender: TObject);
    procedure OnTimerTTTime(Sender: TObject);
    procedure OnTimerAutoClickTime(Sender: TObject);
    procedure StopAnimate;
    procedure SetNeedColor(const Value: TColor);
    procedure SetShape(Value: TShapeType);
    procedure SetColorNormal(const Value: TColor);
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
    procedure SetTransparent(const Value: Boolean);
    procedure SetShowFocusRect(const Value: Boolean);
    procedure SetSubText(const Value: string);
    procedure SetVisibleSubText(const Value: Boolean);
    procedure SetDblClickTooClick(const Value: Boolean);
    procedure SetAutoClick(const Value: Cardinal);
    procedure SetPopup(const Value: TPopupMenu);
    procedure SetBorderColor(const Value: TColor);
    procedure SetEllipseAnimate(const Value: Boolean);
    procedure SetSubTextColor(const Value: TColor);
    procedure SetSubTextFont(const Value: TFont);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetShowCaption(const Value: Boolean);
    property ButtonState: TButtonFlatState read FButtonState write SetButtonState;
    property StyledColor: TColor read FStyledColor write SetStyledColor;
    property FromColor: TColor read FFromColor write FFromColor;
    property NeedColor: TColor read FNeedColor write SetNeedColor;
  protected
    procedure Paint; override;
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TimedText(Text: string; Delay: Cardinal);
    procedure ShowPopup;
    procedure Click; virtual;
  published
    property Align;
    property Anchors;
    property Caption: string read FLabel write SetLabel;
    property ColorNormal: TColor read GetColorNormal write SetColorNormal;
    property ColorOver: TColor read GetColorOver write SetColorOver;
    property ColorPressed: TColor read GetColorPressed write SetColorPressed;
    property Constraints;
    property Cursor default crHandPoint;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EllipseRectVertical: Boolean read FEllipseRectVertical write SetEllipseRectVertical default False;
    property Font;
    property Flat: Boolean read FFlat write SetFlat default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property FontOver: TFont read FFontOver write SetFontOver;
    property FontDown: TFont read FFontDown write SetFontDown;
    property GroupItemKind: TButtonFlatGroupItem read FGroupItemKind write SetGroupItemKind default giNone;
    property IgnorBounds: Boolean read FIgnorBounds write SetIgnorBounds default False;
    property ImageIndentLeft: Integer read FImageIndentLeft write SetImageIndentLeft default 3;
    property ImageIndentRight: Integer read FImageIndentRight write SetImageIndentRight default 0;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageOver: Integer read FImageOver write FImageOver default -1;
    property ImagePress: Integer read FImagePress write FImagePress default -1;
    property Images: TImageList read FImages write SetImages;
    property ImagesOver: TImageList read FImagesOver write SetImagesOver;
    property ImagesPress: TImageList read FImagesPress write SetImagesPress;
    property NotifyColor: TColor read FNotifyColor write SetNotifyColor default $0042A4FF;
    property NotifyVisible: Boolean read FNotifyVisible write SetNotifyVisible default False;
    property NotifyWidth: Integer read FNotifyWidth write SetNotifyWidth default 8;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
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
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
    property ParentShowHint;
    property RoundRectParam: Integer read FRoundRectParam write SetRoundRectParam;
    property Shape: TShapeType read FShape write SetShape default stRectangle;
    property ShowHint;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    property TabOrder;
    property TabStop;
    property TextFormat: TTextFormat read FTextFormat write SetTextFormat;
    property SubText: string read FSubText write SetSubText;
    property SubTextFont: TFont read FSubTextFont write SetSubTextFont;
    property SubTextColor: TColor read FSubTextColor write SetSubTextColor default clGrayText;
    property VisibleSubText: Boolean read FVisibleSubText write SetVisibleSubText default False;
    property Touch;
    property Visible;
    property GetTextWidth: Integer read FGetTextWidth;
    property AutoClick: Cardinal read FAutoClick write SetAutoClick default 0;
    property DblClickTooClick: Boolean read FDblClickTooClick write SetDblClickTooClick default False;
    property Popup: TPopupMenu read FPopup write SetPopup;
    property EllipseAnimate: Boolean read FEllipseAnimate write SetEllipseAnimate default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
  end;

  TCheckBoxFlat = class(TButtonFlat)
  private
    FChecked: Boolean;
    FImageCheck: Integer;
    FImageUncheck: Integer;
    procedure SetChecked(const Value: Boolean);
    procedure UpdateChecked;
    procedure SetImageCheck(const Value: Integer);
    procedure SetImageUncheck(const Value: Integer);
  public
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  published
    property Align;
    property Anchors;
    property Caption;
    property ColorNormal;
    property ColorOver;
    property ColorPressed;
    property Constraints;
    property Cursor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EllipseRectVertical;
    property Font;
    property Flat;
    property BorderColor;
    property FontOver;
    property FontDown;
    property GroupItemKind;
    property IgnorBounds;
    property ImageIndentLeft;
    property ImageIndentRight;
    property Images;
    property NotifyColor;
    property NotifyVisible;
    property NotifyWidth;
    property Transparent;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnStartDock;
    property OnStartDrag;
    property ParentShowHint;
    property RoundRectParam;
    property Shape;
    property ShowHint;
    property ShowFocusRect;
    property TabOrder;
    property TabStop;
    property TextFormat;
    property SubText;
    property SubTextFont;
    property SubTextColor;
    property VisibleSubText;
    property Touch;
    property Visible;
    property GetTextWidth;
    property AutoClick;
    property DblClickTooClick default True;
    property Popup;
    property EllipseAnimate;
    property Checked: Boolean read FChecked write SetChecked default False;
    property ImageCheck: Integer read FImageCheck write SetImageCheck default -1;
    property ImageUncheck: Integer read FImageUncheck write SetImageUncheck default -1;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  RegisterComponents(PackageName, [TButtonFlat]);
  RegisterComponents(PackageName, [TCheckBoxFlat]);
end;

{ TLabelEx }

procedure TButtonFlat.TimedText(Text: string; Delay: Cardinal);
begin
  FTimedText := Text;
  FDrawTimedText := True;
  FTimerTT.Interval := Delay;
  FTimerTT.Enabled := True;
  Repaint;
end;

procedure TButtonFlat.SetShape(Value: TShapeType);
begin
  if FShape <> Value then
  begin
    FShape := Value;
    Invalidate;
  end;
end;

procedure TButtonFlat.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  Repaint;
end;

procedure TButtonFlat.SetShowFocusRect(const Value: Boolean);
begin
  FShowFocusRect := Value;
  Repaint;
end;

procedure TButtonFlat.Click;
begin
  inherited;
end;

procedure TButtonFlat.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  FMouseOver := True;
  ButtonState := bfsOver;
end;

procedure TButtonFlat.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  FMouseOver := False;
  ButtonState := bfsNormal;
end;

constructor TButtonFlat.Create(AOwner: TComponent);
begin
  {$IFNDEF FORXP}
  //DOTO Нужна проверка на версию винды. Если XP, то требовать включения директивы FORXP. XP не хочет работать с Direct2D
  {$ENDIF}
  inherited Create(AOwner);
  inherited Cursor := crHandPoint;
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];
  OnDblClick := FOnDblClick;
  FDblClickTooClick := False;
  FAutoClick := 0;
  FTimerAnimate := TTimer.Create(nil);
  FTimerAnimate.Interval := 10;
  FTimerAnimate.Enabled := False;
  FTimerAnimate.OnTimer := OnTimerAnimateTime;
  FTimerTT := TTimer.Create(nil);
  FTimerTT.Interval := 100;
  FTimerTT.Enabled := False;
  FTimerTT.OnTimer := OnTimerTTTime;
  FTimerAutoClick := TTimer.Create(nil);
  FTimerAutoClick.Interval := FAutoClick;
  FTimerAutoClick.Enabled := False;
  FTimerAutoClick.OnTimer := OnTimerAutoClickTime;
  FDrawTimedText := False;
  FAnimPerc := 0;
  FShowCaption := True;
  FGettingTextWidth := False;
  FSubText := '';
  FVisibleSubText := False;
  FFlat := True;
  FEllipseAnimate := True;
  FTimedText := '';
  FImageIndex := -1;
  FImagePress := -1;
  FImageOver := -1;
  FImageIndentLeft := 3;
  FImageIndentRight := 0;
  FLabel := 'Кнопка';
  FNotifyColor := $0042A4FF;
  FNotifyWidth := 8;
  FNotifyVisible := False;
  FBorderColor := clNone;
  FBorderWidth := 1;
  ParentColor := False;
  TabStop := True;
  ParentBackground := False;

  FColors[bfsNormal] := $00DFD3C4;
  FColors[bfsOver] := $00AD8D64;
  FColors[bfsPressed] := $009F7949;
  FDowned := False;
  FMouseOver := False;
  Font.Size := 10;

  FFontOver := TFont.Create;
  FFontOver.Color := Font.Color;
  FFontOver.Size := Font.Size;

  FFontDown := TFont.Create;
  FFontDown.Color := Font.Color;
  FFontDown.Size := Font.Size;

  FSubTextFont := TFont.Create;
  FSubTextFont.Color := clWhite;
  FSubTextFont.Size := Font.Size;

  FSubTextColor := clGrayText;

  Width := 90;
  Height := 30;
  FRoundRectParam := 0;
  FIgnorBounds := True;
  FTextFormat := [tfCenter, tfVerticalCenter, tfSingleLine];
  ButtonState := bfsNormal;
  inherited OnMouseDown := DoMouseDown;
  inherited OnMouseUp := DoMouseUp;
end;

destructor TButtonFlat.Destroy;
begin
  FTimerAnimate.Free;
  FTimerTT.Free;
  FTimerAutoClick.Free;
  FFontOver.Free;
  FFontDown.Free;
  FSubTextFont.Free;
  inherited;
end;

procedure TButtonFlat.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  ButtonState := bfsPressed;
  FDowned := True;
  if FAutoClick > 0 then
  begin
    FTimerAutoClick.Interval := 1000;
    FTimerAutoClick.Enabled := True;
  end;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TButtonFlat.DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseOver then
    ButtonState := bfsOver
  else
    ButtonState := bfsNormal;
  FDowned := False;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button, Shift, X, Y);
end;

function TButtonFlat.FGetTextWidth: Integer;
begin
  FGettingTextWidth := True;
  Paint;
  Result := FTextWidth;
end;

procedure TButtonFlat.FOnDblClick(Sender: TObject);
begin
  if DblClickTooClick then
    Click;
end;

function TButtonFlat.GetColorNormal: TColor;
begin
  Result := FColors[bfsNormal];
end;

function TButtonFlat.GetColorOver: TColor;
begin
  Result := FColors[bfsOver];
end;

function TButtonFlat.GetColorPressed: TColor;
begin
  Result := FColors[bfsPressed];
end;

procedure TButtonFlat.OnTimerAnimateTime(Sender: TObject);
begin
 //if csFreeNotification in ComponentState then Exit;
  if FTimerProcing then
    Exit;

  FTimerProcing := True;
  Inc(FAnimPerc, 8);
  if FAnimPerc >= 100 then
  begin
    FAnimPerc := 100;
    StopAnimate;
    StyledColor := NeedColor;
  end
  else
    StyledColor := MixColors(NeedColor, FromColor, FAnimPerc);
  FTimerProcing := False;
end;

procedure TButtonFlat.OnTimerAutoClickTime(Sender: TObject);
begin
 //if csFreeNotification in ComponentState then Exit;
  if FDowned then
  begin
    FTimerAutoClick.Interval := FAutoClick;
    Click;
  end
  else
    FTimerAutoClick.Enabled := False;
end;

procedure TButtonFlat.OnTimerTTTime(Sender: TObject);
begin
 //if csFreeNotification in ComponentState then Exit;
  FDrawTimedText := False;
  FTimerTT.Enabled := False;
  Repaint;
end;

type
  TColorControl = class(TControl)
  public
    property Color;
  end;

function GetColor(Control: TControl): TColor;
begin
  if Assigned(Control) and (Control is TControl) then
  begin
    if (Control.HasParent) and TColorControl(Control).ParentColor then
      Result := GetColor(Control.Parent)
    else
      Result := TColorControl(Control).Color;
  end
  else
    Result := clDefault;
end;

procedure TButtonFlat.Paint;
var
  X, Y, W, H, S, Rx: Integer;
  FRect, FSubRect: TRect;
  d: Double;
  FDrawImg: Integer;
  FText: string;
begin
  try
    if FDrawTimedText then
      FText := FTimedText
    else
      FText := FLabel;
    case FButtonState of
      bfsOver:
        if Assigned(FontOver) then
          Canvas.Font.Assign(FontOver);
      bfsPressed:
        if Assigned(FontDown) then
          Canvas.Font.Assign(FontDown);
      bfsNormal:
        if Assigned(Font) then
          Canvas.Font.Assign(Font);
    end;

    with {$IFDEF FORXP} Canvas {$ELSE} TDirect2DCanvas.Create(Canvas, ClientRect) {$ENDIF} do
    begin
      {$IFNDEF FORXP}
      BeginDraw;
      {$ENDIF}
      Brush.Style := bsSolid;
      if Assigned(Parent) and (Parent is TControl) then
      begin
        //TColorControl(Parent).ParentColor
        Brush.Color := GetColor(Parent);
      end
      else
        Brush.Color := ColorNormal;
      FillRect(ClientRect);
      Brush.Color := StyledColor;
      // Плоский стиль
      if Flat then
        Pen.Color := Brush.Color
      else
      begin
        if FBorderColor = clNone then
          Pen.Color := ColorDarker(StyledColor)
        else
          Pen.Color := FBorderColor;
        Pen.Width := FBorderWidth;
      end;
      // Фигура
      X := Pen.Width div 2;
      Y := X;
      W := Width - Pen.Width + 1;
      H := Height - Pen.Width + 1;
      if Pen.Width = 0 then
      begin
        Dec(W);
        Dec(H);
      end;
      if W < H then
        S := W
      else
        S := H;
      if Shape in [stSquare, stRoundSquare, stCircle] then
      begin
        if W < H then
          S := W
        else
          S := H;
        Inc(X, (W - S) div 2);
        W := S;
        Inc(Y, (H - S) div 2);
        H := S;
      end;
      // Если не прозрачная кнопка, то рисуем фигуру
      if not FTransparent then
      begin
        case Shape of
          stRectangle, stSquare:
            Rectangle(X, Y, X + W, Y + H);
          stRoundRect, stRoundSquare:
            begin
              if FRoundRectParam = 0 then
                Rx := S div 4
              else
                Rx := FRoundRectParam;
              if GroupItemKind = giCenter then
                RoundRect(X, Y, X + W, Y + H, 0, 0)
              else
                RoundRect(X, Y, X + W, Y + H, Rx, Rx);
              case GroupItemKind of
                giLeft:
                  begin
                    FRect := Rect(X, Y, X + W, Y + H);
                    FRect.Offset(FRect.Width div 2 + 1, 0);
                    FRect.Width := FRect.Width div 2;
                    Rectangle(FRect);
                    FRect.Inflate(-1, -1);
                    FRect.Left := FRect.Left - 1;
                    FillRect(FRect);
                    FRect.Inflate(1, 1);
                    FRect.Left := FRect.Left + 1;
                  end;
                giRight:
                  begin
                    FRect := Rect(X, Y, X + W, Y + H);
                    FRect.Width := FRect.Width - (FRect.Width div 2);
                    Rectangle(FRect);
                    FRect.Inflate(0, -1);
                    FRect.Left := FRect.Right - 1;
                    FillRect(FRect);
                    FRect.Inflate(0, 1);
                    FRect.Left := FRect.Right + 1;
                  end;
              end;
            end;
          stCircle, stEllipse:
            begin
              FSubRect := Rect(X, Y, X + W, Y + H);
              if FEllipseAnimate then
              begin
                if (FButtonState <> bfsPressed) and (FPrevState <> bfsPressed) then
                  FSubRect.Inflate(-Round(W / 100 * FAnimPerc), -Round(H / 100 * FAnimPerc));
              end;
              Ellipse(FSubRect);
            end;
        end;
      end;
      // Уведомление
      if FNotifyVisible then
      begin
        FRect := Rect(0, 0, FNotifyWidth, FNotifyWidth);
        if Assigned(Images) then
          FRect.SetLocation(Min(ImageIndentLeft + Images.Width - 4, ClientWidth - FNotifyWidth - 2), (Height div 2 - Images.Height div 2) - 4)
        else
          FRect.SetLocation(FNotifyWidth div 2, FNotifyWidth div 2);
        //if FText <> '' then FRect.Offset(Canvas.TextWidth(FText), 0);

        Brush.Style := bsSolid;
        Pen.Style := psSolid;
        Pen.Color := FNotifyColor;
        Brush.Color := FNotifyColor;
        Ellipse(FRect);
      end;
      //
      //Доп. текст
      if FVisibleSubText then
      begin
        FSubRect := ClientRect;
        FSubRect.Inflate(0, -(FSubRect.Height - (Canvas.TextHeight(FSubText) + 4)) div 2);
        FSubRect.Width := Max(FSubRect.Height, Canvas.TextWidth(FSubText) + 10);
        FSubRect.Offset(ClientRect.Right - FSubRect.Width - 10, 0);
        Brush.Color := FSubTextColor;
        Pen.Color := FSubTextColor;
        RoundRect(FSubRect, FSubRect.Height, FSubRect.Height);
      end;
      //
      {$IFNDEF FORXP}
      EndDraw;
      Free;
      {$ENDIF}
    end;
    //Прямоугольник для текста
    if FIgnorBounds then
      FRect := ClientRect
    else
    begin
      d := 1.6; //6.8
      if FEllipseRectVertical then
        FRect := Rect(Round(X + W / (6.8 / d)), Round(Y + H / (6.8 * d)), Round(X + W - W / (6.8 / d)), Round(Y + H - H / (6.8 * d)))
      else
        FRect := Rect(Round(X + W / (6.8 * d)), Round(Y + H / (6.8 / d)), Round(X + W - W / (6.8 * d)), Round(Y + H - H / (6.8 / d)));
    end;
  //Изображение
    FRect.Offset(FImageIndentLeft, 0);
    FRect.Width := FRect.Width - FImageIndentLeft;
    if Assigned(FImages) and (FImageIndex >= 0) then
    begin
      FRect.Offset(FImages.Width + FImageIndentRight, 0);
      FRect.Width := FRect.Width - FImages.Width + FImageIndentRight;
      case FButtonState of
        bfsNormal:
          begin
            FDrawImg := FImageIndex;
            if IndexInList(FDrawImg, FImages.Count) then
              FImages.Draw(Canvas, FImageIndentLeft, Height div 2 - FImages.Height div 2, FDrawImg, True);
          end;
        bfsOver:
          begin
            FDrawImg := FImageOver;
            if FDrawImg < 0 then
              FDrawImg := FImageIndex;
            if Assigned(FImagesOver) then
            begin
              if IndexInList(FDrawImg, FImagesOver.Count) then
                FImagesOver.Draw(Canvas, FImageIndentLeft, Height div 2 - FImagesOver.Height div 2, FDrawImg, True);
            end
            else
            begin
              if IndexInList(FDrawImg, FImages.Count) then
                FImages.Draw(Canvas, FImageIndentLeft, Height div 2 - FImages.Height div 2, FDrawImg, True);
            end;
          end;
        bfsPressed:
          begin
            FDrawImg := FImagePress;
            if FDrawImg < 0 then
              FDrawImg := FImageIndex;
            if Assigned(FImagesPress) then
            begin
              if IndexInList(FDrawImg, FImagesPress.Count) then
                FImagesPress.Draw(Canvas, FImageIndentLeft, Height div 2 - FImagesPress.Height div 2, FDrawImg, True);
            end
            else
            begin
              if IndexInList(FDrawImg, FImages.Count) then
                FImages.Draw(Canvas, FImageIndentLeft, Height div 2 - FImages.Height div 2, FDrawImg, True);
            end;
          end;
      end;
    end;
  // Текст
    Canvas.Brush.Color := clWhite;
    Canvas.Brush.Style := bsClear;
  //Уменьшим размер для доп текста
    if FVisibleSubText then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := clWhite;
      FSubRect.Offset(0, -1);
      Canvas.TextRect(FSubRect, FSubText, [tfSingleLine, tfCenter, tfVerticalCenter]);
      FRect.Right := Min(FRect.Right, FSubRect.Left);
    end;

    if FShowCaption then
    begin
      Canvas.TextRect(FRect, FText, FTextFormat);
      if FGettingTextWidth then
      begin
        FGettingTextWidth := False;
        FTextWidth := Canvas.TextWidth(FText);
        if Assigned(FImages) and (FImageIndex >= 0) then
        begin
          FTextWidth := FTextWidth + FImages.Width + FImageIndentLeft + FImageIndentRight;
        end;
      end;
    end;

    if Assigned(FOnPaint) then
      FOnPaint(Self);

    if FShowFocusRect and Focused then
    begin
      DrawFocusRect(Canvas.Handle, ClientRect);
    end;
  finally

  end;
end;

procedure TButtonFlat.ShowPopup;
var
  MP: TPoint;
begin
  if Assigned(FPopup) then
  begin
    MP := ClientToScreen(Point(0, 0));
    FPopup.Popup(MP.X, MP.Y + Height);
  end;
end;

procedure TButtonFlat.SetAutoClick(const Value: Cardinal);
begin
  FAutoClick := Value;
  FTimerAutoClick.Interval := FAutoClick;
end;

procedure TButtonFlat.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Repaint;
end;

procedure TButtonFlat.SetBorderWidth(const Value: Integer);
begin
  if Value < 1 then
    raise Exception.Create('Значение должно быть больше 1');

  FBorderWidth := Value;
  Repaint;
end;

procedure TButtonFlat.SetButtonState(const Value: TButtonFlatState);
begin
  FPrevState := FButtonState;
  FButtonState := Value;
  NeedColor := FColors[FButtonState];
end;

procedure TButtonFlat.SetColorNormal(const Value: TColor);
begin
  FColors[bfsNormal] := Value;
  ButtonState := ButtonState;
end;

procedure TButtonFlat.SetColorOver(const Value: TColor);
begin
  FColors[bfsOver] := Value;
  ButtonState := ButtonState;
end;

procedure TButtonFlat.SetColorPressed(const Value: TColor);
begin
  FColors[bfsPressed] := Value;
  ButtonState := ButtonState;
end;

procedure TButtonFlat.SetDblClickTooClick(const Value: Boolean);
begin
  FDblClickTooClick := Value;
end;

procedure TButtonFlat.SetEllipseAnimate(const Value: Boolean);
begin
  FEllipseAnimate := Value;
end;

procedure TButtonFlat.SetEllipseRectVertical(const Value: Boolean);
begin
  FEllipseRectVertical := Value;
  Repaint;
end;

procedure TButtonFlat.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Repaint;
end;

procedure TButtonFlat.SetFontDown(const Value: TFont);
begin
  FFontDown := Value;
  Repaint;
end;

procedure TButtonFlat.SetFontOver(const Value: TFont);
begin
  FFontOver := Value;
  Repaint;
end;

procedure TButtonFlat.SetGroupItemKind(const Value: TButtonFlatGroupItem);
begin
  FGroupItemKind := Value;
  Repaint;
end;

procedure TButtonFlat.SetIgnorBounds(const Value: Boolean);
begin
  FIgnorBounds := Value;
  Repaint;
end;

procedure TButtonFlat.SetImageIndentLeft(const Value: Integer);
begin
  FImageIndentLeft := Value;
  Repaint;
end;

procedure TButtonFlat.SetImageIndentRight(const Value: Integer);
begin
  FImageIndentRight := Value;
  Repaint;
end;

procedure TButtonFlat.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Repaint;
end;

procedure TButtonFlat.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Repaint;
end;

procedure TButtonFlat.SetImagesOver(const Value: TImageList);
begin
  FImagesOver := Value;
  Repaint;
end;

procedure TButtonFlat.SetImagesPress(const Value: TImageList);
begin
  FImagesPress := Value;
  Repaint;
end;

procedure TButtonFlat.SetLabel(const Value: string);
begin
  FLabel := Value;
  Repaint;
end;

procedure TButtonFlat.SetNeedColor(const Value: TColor);
begin
  FNeedColor := Value;
  if (csFreeNotification in ComponentState) then
  begin
    FromColor := StyledColor;
    FTimerAnimate.Enabled := True;
  end
  else
    StyledColor := FNeedColor;
  Repaint;
end;

procedure TButtonFlat.SetNotifyColor(const Value: TColor);
begin
  FNotifyColor := Value;
  Repaint;
end;

procedure TButtonFlat.SetNotifyVisible(const Value: Boolean);
begin
  FNotifyVisible := Value;
  Repaint;
end;

procedure TButtonFlat.SetNotifyWidth(const Value: Integer);
begin
  FNotifyWidth := Value;
  Repaint;
end;

procedure TButtonFlat.SetPopup(const Value: TPopupMenu);
begin
  FPopup := Value;
end;

procedure TButtonFlat.StopAnimate;
begin
  FAnimPerc := 0;
  if (csFreeNotification in ComponentState) then
    FTimerAnimate.Enabled := False;
end;

procedure TButtonFlat.SetRoundRectParam(const Value: integer);
begin
  FRoundRectParam := Value;
  Repaint;
end;

procedure TButtonFlat.SetStyledColor(const Value: TColor);
begin
  FStyledColor := Value;
  Repaint;
end;

procedure TButtonFlat.SetSubText(const Value: string);
begin
  FSubText := Value;
  Repaint;
end;

procedure TButtonFlat.SetSubTextColor(const Value: TColor);
begin
  FSubTextColor := Value;
  Repaint;
end;

procedure TButtonFlat.SetSubTextFont(const Value: TFont);
begin
  FSubTextFont := Value;
  Repaint;
end;

procedure TButtonFlat.SetTextFormat(const Value: TTextFormat);
begin
  FTextFormat := Value;
  Repaint;
end;

procedure TButtonFlat.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Repaint;
end;

procedure TButtonFlat.SetVisibleSubText(const Value: Boolean);
begin
  FVisibleSubText := Value;
  Repaint;
end;

{ TCheckBoxFlat }

procedure TCheckBoxFlat.SetImageCheck(const Value: Integer);
begin
  FImageCheck := Value;
  UpdateChecked;
end;

procedure TCheckBoxFlat.SetImageUncheck(const Value: Integer);
begin
  FImageUncheck := Value;
  UpdateChecked;
end;

procedure TCheckBoxFlat.UpdateChecked;
begin
  if FChecked then
    ImageIndex := FImageCheck
  else
    ImageIndex := FImageUncheck;
  Repaint;
end;

procedure TCheckBoxFlat.Click;
begin
  Checked := not Checked;
  inherited;
end;

constructor TCheckBoxFlat.Create(AOwner: TComponent);
begin
  inherited;
  DblClickTooClick := True;
  FChecked := False;
  FImageCheck := -1;
  FImageUncheck := -1;
end;

procedure TCheckBoxFlat.SetChecked(const Value: Boolean);
begin
  FChecked := Value;
  UpdateChecked;
end;

end.

