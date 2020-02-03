unit HGM.Controls.PanelExt;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  System.Types, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, HGM.Common;

type
  TPanelExt = class(TCustomPanel)
  private
    FOnPanel: Boolean;
    FDefaultPaint: Boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FMouseCoord: TPoint;
    FMouseState: TShiftState;
    FRepaintOnMouseMove: Boolean;
    procedure SetDefaultPaint(const Value: Boolean);
    procedure SetRepaintOnMouseMove(const Value: Boolean);
  public
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure Paint; override;
    property Canvas;
    constructor Create(AOwner: TComponent); override;
    property OnPanel: Boolean read FOnPanel;
    property MouseCoord: TPoint read FMouseCoord;
    property MouseState: TShiftState read FMouseState;
  published
    property Caption;
    property DefaultPaint: Boolean read FDefaultPaint write SetDefaultPaint;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property RepaintOnMouseMove: Boolean read FRepaintOnMouseMove write SetRepaintOnMouseMove default False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

  TDrawPanel = class(TPanelExt)
  public
    FOnPaint: TNotifyEvent;
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property RepaintOnMouseMove default True;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnPanel;
    property DefaultPaint: Boolean read FDefaultPaint write SetDefaultPaint;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TDragPanel = class(TCustomPanel)
  private
    FOnMouseDown: TMouseEvent;
    procedure FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); overload;
    procedure SetOnMouseDown(const Value: TMouseEvent);
  public
    procedure DoDrag;
    property DockManager;
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property VerticalAlignment;
    property Visible;
    property StyleElements;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TDecoratePanel = class
  private
    FAnimate: Boolean;
    FOwner: TControl;
    FPanel: TPanel;
    FOpening: Boolean;
  public
    constructor Create(AOwner: TControl; APanel: TPanel; AOpen: Boolean = False);
    procedure Close;
    procedure CloseDelay(Interval: Integer; FProc: TProc = nil);
    procedure Open(Animate: Boolean = True);
    procedure UpdateSize;
    property Animate: Boolean read FAnimate write FAnimate default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents(PackageName, [TPanelExt]);
  RegisterComponents(PackageName, [TDrawPanel]);
  RegisterComponents(PackageName, [TDragPanel]);
end;

constructor TDrawPanel.Create(AOwner: TComponent);
begin
  inherited;
  ParentBackground := False;
  RepaintOnMouseMove := True;
end;

procedure TDrawPanel.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TPanelExt.Paint;
begin
  if Brush.Bitmap <> nil then
    Canvas.Draw(0, 0, Brush.Bitmap);
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TPanelExt.SetDefaultPaint(const Value: Boolean);
begin
  FDefaultPaint := Value;
  Paint;
end;

procedure TPanelExt.SetRepaintOnMouseMove(const Value: Boolean);
begin
  FRepaintOnMouseMove := Value;
end;

procedure TPanelExt.WMNCPaint(var Message: TMessage);
begin
  if FDefaultPaint then
    inherited;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TPanelExt.CMMouseEnter(var message: TMessage);
begin
  FOnPanel := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TPanelExt.CMMouseLeave(var message: TMessage);
begin
  FOnPanel := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TPanelExt.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
  begin
    Result := 0;
    if DoMouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos)) then
      Message.Result := 1
    else if Parent <> nil then
      with TMessage(Message) do
        Result := Parent.Perform(CM_MOUSEWHEEL, WParam, LParam);
  end;
end;

constructor TPanelExt.Create(AOwner: TComponent);
begin
  inherited;
  inherited OnMouseMove := FMouseMove;
  inherited OnMouseDown := FMouseDown;
  inherited OnMouseUp := FMouseUp;
end;

procedure TPanelExt.FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseState := Shift;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TPanelExt.FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMouseState := Shift;
  FMouseCoord := Point(X, Y);
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Sender, Shift, X, Y);
  if FRepaintOnMouseMove then
    Repaint;
end;

procedure TPanelExt.FMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseState := Shift;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button, Shift, X, Y);
end;

{ TDragPanel }

constructor TDragPanel.Create(AOwner: TComponent);
begin
  inherited;
  inherited OnMouseDown := FMouseDown;
end;

procedure TDragPanel.DoDrag;
begin
  ReleaseCapture;
  SendMessage(Self.Parent.Handle, WM_SYSCOMMAND, 61458, 0);
end;

procedure TDragPanel.FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DoDrag;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TDragPanel.SetOnMouseDown(const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
end;

{ TDecoratePanel }

procedure TDecoratePanel.Close;
var
  i: Integer;
begin
  for i := 0 to FOwner.ClientHeight div 30 do
  begin
    FPanel.Top := i * 30;
    FOwner.Repaint;
  end;
  FPanel.Hide;
  FOwner.Repaint;
end;

procedure TDecoratePanel.CloseDelay;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(3000);
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          Close;
          if Assigned(FProc) then
            FProc;
        end);
    end).Start;
end;

constructor TDecoratePanel.Create(AOwner: TControl; APanel: TPanel; AOpen: Boolean);
begin
  FOwner := AOwner;
  FPanel := APanel;
  FOpening := False;
  FPanel.Left := 0;
  FPanel.Top := 0;
  FPanel.Hide;
  if AOpen then
    Open(False);
end;

procedure TDecoratePanel.Open(Animate: Boolean);
begin
  UpdateSize;
  FOpening := True;
  FPanel.Show;
  FPanel.BringToFront;
  FPanel.Left := 0;
  FPanel.Top := 0;
  FOpening := False;
end;

procedure TDecoratePanel.UpdateSize;
begin
  if Assigned(FOwner) and Assigned(FPanel) then
  begin
    FPanel.Width := FOwner.ClientWidth;
    FPanel.Height := FOwner.ClientHeight;
  end;
end;

end.

