unit HGM.FMX.SmoothScroll;

interface

uses
  System.Classes, System.Types, FMX.Types, FMX.Layouts;

type
  TSmoothScroll = class(TComponent)
  private
    FTimerUpdateScroll: TTimer;
    FTimerAutoScroll: TTimer;
    FScrollImpulse: Single;
    FScroll: TCustomScrollBox;
    FMaxSpeed: Integer;
    FScrollDelta: Integer;
    FIncrement: Single;
    FAutoScrollDown: Boolean;
    FAutoScrollOldPos: Single;
    procedure FDoScroll(Delta: Single);
    procedure TimerUpdateScrollTimer(Sender: TObject);
    procedure TimerAutoScrollTimer(Sender: TObject);
    procedure SetScroll(const Value: TCustomScrollBox);
    procedure SetMaxSpeed(const Value: Integer);
    procedure SetScrollDelta(const Value: Integer);
    procedure FOverMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure SetIncrement(const Value: Single);
    procedure SetUpdateInterval(const Value: Cardinal);
    function GetUpdateInterval: Cardinal;
    function GetIsEnd: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFor(AScroll: TCustomScrollBox);
    procedure ScrollEvent(WheelDelta: Single);
    procedure Boost(Value: Single);
    procedure ScrollDown;
    procedure ToEnd;
    property Scroll: TCustomScrollBox read FScroll write SetScroll;
    property MaxSpeed: Integer read FMaxSpeed write SetMaxSpeed;
    property Increment: Single read FIncrement write SetIncrement;
    property UpdateInterval: Cardinal read GetUpdateInterval write SetUpdateInterval;
    property ScrollDelta: Integer read FScrollDelta write SetScrollDelta;
    property IsEnd: Boolean read GetIsEnd;
  end;

implementation

uses
  System.Math;

{ TSmoothScroll }

procedure TSmoothScroll.Boost(Value: Single);
begin
  ScrollEvent(Value);
end;

constructor TSmoothScroll.Create(AOwner: TComponent);
begin
  inherited;
  FAutoScrollDown := True;
  FAutoScrollOldPos := 0;
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
  FTimerAutoScroll := TTimer.Create(Self);
  with FTimerAutoScroll do
  begin
    Enabled := False;
    Interval := 100;
    OnTimer := TimerAutoScrollTimer;
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
  FTimerAutoScroll.Enabled := False;
  ScrollEvent(WheelDelta);
end;

function TSmoothScroll.GetIsEnd: Boolean;
begin
  Result := FScroll.ViewportPosition.Y + FScroll.ClientHeight = FScroll.ContentBounds.Height;
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

procedure TSmoothScroll.ScrollDown;
begin
  FAutoScrollDown := True;
  FTimerAutoScroll.Enabled := True;
  FTimerUpdateScroll.Enabled := True;
  TimerAutoScrollTimer(nil);
end;

procedure TSmoothScroll.ToEnd;
begin
  FScroll.ViewportPosition := TPointF.Create(FScroll.ViewportPosition.X, FScroll.ContentBounds.Height - FScroll.ClientHeight);
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

procedure TSmoothScroll.TimerAutoScrollTimer(Sender: TObject);
begin
  if not FTimerUpdateScroll.Enabled then
  begin
    FTimerAutoScroll.Enabled := False;
    Exit;
  end;
  if FAutoScrollDown then
    ScrollEvent(-100)
  else
    ScrollEvent(+100);
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

