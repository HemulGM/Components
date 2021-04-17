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
    FIncrement: Single;
    procedure FDoScroll(Delta: Single);
    procedure TimerUpdateScrollTimer(Sender: TObject);
    procedure SetScroll(const Value: TCustomScrollBox);
    procedure SetMaxSpeed(const Value: Integer);
    procedure SetScrollDelta(const Value: Integer);
    procedure FOverMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure SetIncrement(const Value: Single);
    procedure SetUpdateInterval(const Value: Cardinal);
    function GetUpdateInterval: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFor(AScroll: TCustomScrollBox);
    procedure ScrollEvent(WheelDelta: Single);
    property Scroll: TCustomScrollBox read FScroll write SetScroll;
    property MaxSpeed: Integer read FMaxSpeed write SetMaxSpeed;
    property Increment: Single read FIncrement write SetIncrement;
    property UpdateInterval: Cardinal read GetUpdateInterval write SetUpdateInterval;
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
  FIncrement := 1;
  FTimerUpdateScroll := TTimer.Create(Self);
  with FTimerUpdateScroll do
  begin
    Enabled := False;
    Interval := 10;
    OnTimer := TimerUpdateScrollTimer;
  end;
end;

constructor TSmoothScroll.CreateFor(AScroll: TCustomScrollBox);
begin
  Create(AScroll);
  FScroll := AScroll;
  AScroll.OnMouseWheel := FOverMouseWheel;
end;

procedure TSmoothScroll.FOverMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  Handled := True;
  ScrollEvent(WheelDelta);
end;

function TSmoothScroll.GetUpdateInterval: Cardinal;
begin
  Result := FTimerUpdateScroll.Interval;
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

procedure TSmoothScroll.SetIncrement(const Value: Single);
begin
  FIncrement := Value;
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

procedure TSmoothScroll.SetUpdateInterval(const Value: Cardinal);
begin
  FTimerUpdateScroll.Interval := Value;
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
  if Abs(FScrollImpulse) > (FIncrement * 2) then
  begin
    if FScrollImpulse < 0 then
      FScrollImpulse := FScrollImpulse + FIncrement
    else
      FScrollImpulse := FScrollImpulse - FIncrement;

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

