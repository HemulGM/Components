unit HGM.Controls.ProgressBar;

interface

uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, System.Classes,
  System.Contnrs, System.Types, System.UITypes, Vcl.Controls, Vcl.Forms,
  Vcl.Menus, Vcl.Graphics, Vcl.StdCtrls, Vcl.GraphUtil, Vcl.ImgList, Vcl.Themes,
  Winapi.ShellAPI, System.Generics.Collections, HGM.Common, Vcl.Dialogs, Vcl.ExtCtrls;

type
  ThProgressBarKind = (pbkRect, pbkRoundRect, pbkEllipse);

  ThCustomProgrsssBar = class(TCustomControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParentBackground(Value: Boolean); override;
  private
    FProgOffset: Integer;
    FProgressText: string;
    class var TimerAnimate: TTimer;

    FColorScale: TColor;
    FPosition: Integer;
    FColorBackground: TColor;
    FKind: ThProgressBarKind;
    FRoundRadius: Integer;
    procedure SetColorScale(const Value: TColor);
    procedure SetPosition(const Value: Integer);
    procedure NeedRepaint;
    procedure SetColorBackground(const Value: TColor);
    procedure SetKind(const Value: ThProgressBarKind);
    procedure SetRoundRadius(const Value: Integer);
    procedure TimerAnimateTimer(Sender: TObject);
  public
    property ParentColor default False;
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    property ColorScale: TColor read FColorScale write SetColorScale;
    property ColorBackground: TColor read FColorBackground write SetColorBackground;
    property Position: Integer read FPosition write SetPosition;
    property ParentBackground default True;
    property Kind: ThProgressBarKind read FKind write SetKind;
    property RoundRadius: Integer read FRoundRadius write SetRoundRadius;
  end;

  ThProgrsssBar = class(ThCustomProgrsssBar)
  published
    property Align;
    property Color;
    property DoubleBuffered;
    property Visible;
    property BorderWidth;
    property ColorScale;
    property ColorBackground;
    property ParentBackground;
    property Position;
    property ParentColor;
    property Kind default pbkEllipse;
    property RoundRadius default 10;
  end;

procedure Register;

implementation

uses
  D2D1, Direct2D, Math;

procedure Register;
begin
  RegisterComponents(PackageName, [ThProgrsssBar]);
end;

{ ThCustomProgrsssBar }

procedure ThCustomProgrsssBar.SetParentBackground(Value: Boolean);
begin
  if Value then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  inherited;
end;

procedure ThCustomProgrsssBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

constructor ThCustomProgrsssBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csParentBackground, csGestures];
  Width := 250;
  Height := 10;
  FColorScale := $00A37349;
  FColorBackground := $0020160F;
  ParentBackground := True;
  DoubleBuffered := True;
  TabStop := False;
  FPosition := 43;
  FKind := pbkEllipse;
  FRoundRadius := 10;

  FProgressText := 'Прогресс';

  FProgOffset := 0;

   TimerAnimate := TTimer.Create(Self);

  TimerAnimate.Name := 'TimerAnimate';
  TimerAnimate.Interval := 30;
  TimerAnimate.OnTimer := TimerAnimateTimer;

end;

procedure ThCustomProgrsssBar.NeedRepaint;
begin
  Invalidate;
end;

procedure ThCustomProgrsssBar.Paint;    {
var
  PosRect: TRect;
  S: string;
  PaintStruct: TPaintStruct;   }
var
  R, PR: TRect;
  S: string;
  i: Integer;
begin
  with Canvas do
  begin
    R := ClientRect;
    //Общий фон
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(R);
    //Пустой прогресс бар
    Brush.Color := $00FCFCFA;
    Pen.Color := $00E6DACF;
    RoundRect(R.Left, R.Top, R.Right, R.Bottom, 5, 5);
    //Прогресс
    Brush.Color := $00EADED3;
    PR := R;
    PR.Inflate(-1, -1);
    PR.Width := Round((PR.Width / 100) * FPosition);
    for i := -60 to PR.Width do
    begin
      if i mod 20 = 0 then
        if Pen.Color = $00F0E6DD then
          Pen.Color := $00EADED3
        else
          Pen.Color := $00F0E6DD;

      MoveTo(i + FProgOffset, PR.Bottom - 1);
      LineTo(i + 20 + FProgOffset, PR.Top - 1);
    end;
    if FPosition < 100 then
    begin
      PR := R;
      PR.Inflate(-1, -1);
      PR.Left := Round((PR.Width / 100) * FPosition);
      Brush.Color := $00FCFCFA;
      FillRect(PR);
    end;
    //Обводка
    Pen.Color := $00E6DACF;
    Brush.Style := bsClear;
    RoundRect(R.Left, R.Top, R.Right, R.Bottom, 5, 5);
    S := FProgressText + ' ' + FPosition.ToString + '%';
    Font.Color := $00A88563;
    Font.Size := 9;
    TextRect(R, S, [tfSingleLine, tfCenter, tfVerticalCenter]);
  end;
 { inherited;
  BeginPaint(Handle, PaintStruct);
  with TDirect2DCanvas.Create(Canvas, ClientRect) do
  begin
    BeginDraw;
    Brush.Style := bsSolid;
    if not ParentBackground then
    begin
      Brush.Color := Color;
      Pen.Color := Brush.Color;
      FillRect(ClientRect);
    end;
    //calc
    PosRect := ClientRect;
    PosRect.Width := Round(PosRect.Width / 100 * FPosition);
    PosRect.Height := Max(1, Min(PosRect.Width, ClientRect.Height));
    PosRect.Offset(0, ClientRect.Height div 2 - PosRect.Height div 2);
    //draw bg
    Brush.Color := FColorBackground;
    Pen.Color := Brush.Color;
    case FKind of
      pbkRect:
        begin
          Rectangle(ClientRect);
          //draw scale
          Brush.Color := FColorScale;
          Pen.Color := Brush.Color;
          Rectangle(PosRect);
        end;
      pbkRoundRect:
        begin
          RoundRect(ClientRect, FRoundRadius, FRoundRadius);
          //draw scale
          Brush.Color := FColorScale;
          Pen.Color := Brush.Color;
          RoundRect(PosRect, Min(PosRect.Height, FRoundRadius), Min(PosRect.Height, FRoundRadius));
        end;
      pbkEllipse:
        begin
          RoundRect(ClientRect, ClientRect.Height, ClientRect.Height);
          //draw scale
          Brush.Color := FColorScale;
          Pen.Color := Brush.Color;
          RoundRect(PosRect, PosRect.Height, PosRect.Height);
        end;
    end;

    EndDraw;
    Free;
  end;
  with Canvas do
  begin
    S := FPosition.ToString + '%';
    Brush.Style := bsClear;
    PosRect := ClientRect;
    Font.Size := 7;
    Font.Color := clWhite;
    TextRect(PosRect, S, [tfSingleLine, tfVerticalCenter, tfCenter]);
  end;
  EndPaint(Handle, PaintStruct);  }
end;

procedure ThCustomProgrsssBar.SetColorBackground(const Value: TColor);
begin
  FColorBackground := Value;
  NeedRepaint;
end;

procedure ThCustomProgrsssBar.SetColorScale(const Value: TColor);
begin
  FColorScale := Value;
  NeedRepaint;
end;

procedure ThCustomProgrsssBar.SetKind(const Value: ThProgressBarKind);
begin
  FKind := Value;
  NeedRepaint;
end;

procedure ThCustomProgrsssBar.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  FPosition := Min(Max(Value, 0), 100);
  if FPosition > 0 then
  begin
    if FPosition = 100 then
    begin
      //Taskbar.ProgressState := TTaskBarProgressState.None;
    end
    else
    begin
      //Taskbar.ProgressValue := FOperProgress;
      //Taskbar.ProgressState := TTaskBarProgressState.Normal;
    end;
  end
  else
  begin
    //Taskbar.ProgressState := TTaskBarProgressState.None;
    //Taskbar.ProgressValue := 0;
  end;
  NeedRepaint;
end;

procedure ThCustomProgrsssBar.SetRoundRadius(const Value: Integer);
begin
  FRoundRadius := Min(ClientRect.Height, Max(0, Value));
  NeedRepaint;
end;

procedure ThCustomProgrsssBar.TimerAnimateTimer(Sender: TObject);
begin
  FProgOffset := FProgOffset + 1;
  if FProgOffset = 40 then
    FProgOffset := 0;
  NeedRepaint
end;

end.

