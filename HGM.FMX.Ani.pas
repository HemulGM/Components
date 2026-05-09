unit HGM.FMX.Ani;

interface

uses
  FMX.Ani, FMX.Types, System.Types, System.Classes, System.SysUtils;

type
  TAniFreeNotification = class(TInterfacedObject, IFreeNotification)
  private
    FProc: TProc;
  public
    procedure FreeNotification(AObject: TObject);
    constructor Create(Proc: TProc);
  end;

{ TRectFAnimation }

  TRectFAnimation = class(TCustomPropertyAnimation)
  private
    FStartRect: TRectF;
    FCurrent: TRectF;
    FStopRect: TRectF;
    FStartFromCurrent: Boolean;
    procedure SetStartRect(const Value: TRectF);
    procedure SetStopRect(const Value: TRectF);
  protected
    procedure ProcessAnimation; override;
    procedure FirstFrame; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AnimationType default TAnimationType.in;
    property AutoReverse default False;
    property Enabled default False;
    property Delay;
    property Duration nodefault;
    property Interpolation default TInterpolationType.Linear;
    property Inverse default False;
    property Loop default False;
    property OnProcess;
    property OnFinish;
    property PropertyName;
    property StartValue: TRectF read FStartRect write SetStartRect;
    property StartFromCurrent: Boolean read FStartFromCurrent write FStartFromCurrent default False;
    property StopValue: TRectF read FStopRect write SetStopRect;
    property Trigger;
    property TriggerInverse;
  end;

  TAnimatorHelper = class helper for TAnimator
    class procedure DetachPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
    class procedure AnimateBounds(const Target: TFmxObject; const APropertyName: string; const NewValue: TRectF; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateRect(const Target: TFmxObject; const APropertyName: string; const NewValue: TRectF; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
    class procedure AnimateFloatWithFinish(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Finish: TProc; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear); overload;
  end;

implementation

uses
  FMX.Utils, System.Rtti;

{ TAnimatorHelper }

class procedure TAnimatorHelper.AnimateFloat(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Update: TNotifyEvent; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.OnProcess := Update;
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.AnimateFloatWithFinish(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Finish: TProc; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear);
var
  Animation: TFloatAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TFloatAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.AddFreeNotify(TAniFreeNotification.Create(Finish));
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.AnimateRect(const Target: TFmxObject; const APropertyName: string; const NewValue: TRectF; Update: TNotifyEvent; Duration: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TRectFAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TRectFAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  Animation.StopValue := NewValue;
  Animation.OnProcess := Update;
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.AnimateBounds(const Target: TFmxObject; const APropertyName: string; const NewValue: TRectF; Update: TNotifyEvent; Duration: Single; AType: TAnimationType; AInterpolation: TInterpolationType);
var
  Animation: TRectAnimation;
begin
  StopPropertyAnimation(Target, APropertyName);

  with Self do
    CreateDestroyer;

  Animation := TRectAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;
  var Bounds := TBounds.Create(NewValue);
  try
    Animation.StopValue := Bounds;
  finally
    Bounds.Free;
  end;
  Animation.OnProcess := Update;
  with Self do
    FDestroyer.RegisterAnimation(Animation);
  Animation.Start;
end;

class procedure TAnimatorHelper.DetachPropertyAnimation(const Target: TFmxObject; const APropertyName: string);
var
  I: Integer;
begin
  I := Target.ChildrenCount - 1;
  while I >= 0 do
  begin
    if (Target.Children[I] is TCustomPropertyAnimation) and
      (CompareText(TCustomPropertyAnimation(Target.Children[I]).PropertyName, APropertyName) = 0) then
    begin
      var Anim := TFloatAnimation(Target.Children[I]);
      Anim.Parent := nil;
      Anim.Stop;
    end;
    if I > Target.ChildrenCount then
      I := Target.ChildrenCount;
    Dec(I);
  end;
end;

{ TAniFreeNotification }

constructor TAniFreeNotification.Create(Proc: TProc);
begin
  inherited Create;
  FProc := Proc;
end;

procedure TAniFreeNotification.FreeNotification(AObject: TObject);
begin
  if Assigned(FProc) then
    FProc;
  Free;
end;

{ TRectFAnimation }

procedure TRectFAnimation.AssignTo(Dest: TPersistent);
var
  DestAnimation: TRectFAnimation;
begin
  if Dest is TRectFAnimation then
  begin
    DestAnimation := TRectFAnimation(Dest);
    DestAnimation.StartValue := StartValue;
    DestAnimation.StopValue := StopValue;
    DestAnimation.StartFromCurrent := StartFromCurrent;
  end;
  inherited;
end;

constructor TRectFAnimation.Create(AOwner: TComponent);
begin
  inherited;
  Duration := 0.2;
  FStartRect := TRectF.Empty;
  FStopRect := TRectF.Empty;
  FCurrent := TRectF.Empty;
end;

destructor TRectFAnimation.Destroy;
begin
  inherited;
end;

procedure TRectFAnimation.FirstFrame;
begin
  if StartFromCurrent then
  begin
    if (FRttiProperty <> nil) and FRttiProperty.PropertyType.IsRecord then
      StartValue := FRttiProperty.GetValue(FInstance).AsType<TRectF>;
  end;
end;

procedure TRectFAnimation.ProcessAnimation;
begin
  if FInstance <> nil then
  begin
    { calc value }
    FCurrent.Left := InterpolateSingle(FStartRect.Left, FStopRect.Left, NormalizedTime);
    FCurrent.Top := InterpolateSingle(FStartRect.Top, FStopRect.Top, NormalizedTime);
    FCurrent.Right := InterpolateSingle(FStartRect.Right, FStopRect.Right, NormalizedTime);
    FCurrent.Bottom := InterpolateSingle(FStartRect.Bottom, FStopRect.Bottom, NormalizedTime);

    if (FRttiProperty <> nil) and FRttiProperty.PropertyType.IsRecord then
      FRttiProperty.SetValue(FInstance, TValue.From<TRectF>(FCurrent));
  end;
end;

procedure TRectFAnimation.SetStartRect(const Value: TRectF);
begin
  FStartRect := Value;
end;

procedure TRectFAnimation.SetStopRect(const Value: TRectF);
begin
  FStopRect := Value;
end;

end.

