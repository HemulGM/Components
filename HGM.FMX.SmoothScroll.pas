unit HGM.FMX.SmoothScroll;

interface

uses
  System.Classes, System.Types, FMX.Types, FMX.Layouts;

type
  TSmoothScroll = class(TComponent)
  private
    FTimerUpdateScroll: TTimer;
    FScrollImpulse: Single;
    FScroll: TCustomScrollBox;
    FMaxSpeed: Integer;
    FScrollDelta: Integer;
    procedure FDoScroll(Delta: Single);
    procedure TimerUpdateScrollTimer(Sender: TObject);
    procedure SetScroll(const Value: TCustomScrollBox);
    procedure SetMaxSpeed(const Value: Integer);
    procedure SetScrollDelta(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ScrollEvent(WheelDelta: Single);
    property Scroll: TCustomScrollBox read FScroll write SetScroll;
    property MaxSpeed: Integer read FMaxSpeed write SetMaxSpeed;
    property ScrollDelta: Integer read FScrollDelta write SetScrollDelta;
  end;

implementation

uses
  System.Math;

{ TSmoothScroll }

constructor TSmoothScroll.Create(AOwner: TComponent);
begin
  inherited;
  FMaxSpeed := 40;
  FScrollImpulse := 0;
  FScrollDelta := 9;
  FTimerUpdateScroll := TTimer.Create(Self);
  with FTimerUpdateScroll do
  begin
    Interval := 10;
    OnTimer := TimerUpdateScrollTimer;
  end;
end;

procedure TSmoothScroll.FDoScroll(Delta: Single);
begin
  if not FTimerUpdateScroll.Enabled then
    FTimerUpdateScroll.Enabled := True;
  FScrollImpulse := Max(-FMaxSpeed, Min(FScrollImpulse - Delta, FMaxSpeed));
end;

procedure TSmoothScroll.ScrollEvent(WheelDelta: Single);
begin
  FDoScroll(WheelDelta / FScrollDelta);
end;

procedure TSmoothScroll.SetMaxSpeed(const Value: Integer);
begin
  FMaxSpeed := Value;
end;

procedure TSmoothScroll.SetScroll(const Value: TCustomScrollBox);
begin
  FScroll := Value;
end;

procedure TSmoothScroll.SetScrollDelta(const Value: Integer);
begin
  FScrollDelta := Value;
end;

procedure TSmoothScroll.TimerUpdateScrollTimer(Sender: TObject);
var
  Old: TPointF;
begin
  if not Assigned(FScroll) then
  begin
    FTimerUpdateScroll.Enabled := False;
    Exit;
  end;
  if Abs(FScrollImpulse) > 2 then
  begin
    if FScrollImpulse < 0 then
      FScrollImpulse := FScrollImpulse + 1
    else
      FScrollImpulse := FScrollImpulse - 1;

    Old := FScroll.ViewportPosition;
    FScroll.ViewportPosition := TPointF.Create(0, FScroll.ViewportPosition.Y + FScrollImpulse);
    if FScroll.ViewportPosition = Old then
    begin
      FScrollImpulse := 0;
      FTimerUpdateScroll.Enabled := False;
    end;
  end
  else
  begin
    FScrollImpulse := 0;
    FTimerUpdateScroll.Enabled := False;
  end;
end;

end.

