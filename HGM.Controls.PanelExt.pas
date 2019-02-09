unit HGM.Controls.PanelExt;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Types,  Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, HGM.Common;

type
  TPanelExt = class(TCustomPanel)
   private
    AOnPanel:Boolean;
    FDefaultPaint: Boolean;
    procedure SetDefaultPaint(const Value: Boolean);
   public
    FOnMouseEnter:TNotifyEvent;
    FOnMouseLeave:TNotifyEvent;
    FOnPaint:TNotifyEvent;
    procedure CMMouseEnter(var message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure Paint; override;
    property Canvas;
   published
    property Caption;
    property DefaultPaint:Boolean read FDefaultPaint write SetDefaultPaint;
    property OnMouseEnter:TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint:TNotifyEvent read FOnPaint write FOnPaint;
    property OnPanel:Boolean read AOnPanel;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
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
   FOnPaint:TNotifyEvent;
   procedure Paint; override;
   constructor Create(AOwner: TComponent); override;
  published
   property OnPaint:TNotifyEvent read FOnPaint write FOnPaint;
   property OnMouseWheel;
   property OnMouseWheelDown;
   property OnMouseWheelUp;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
  end;

  TDragPanel = class(TCustomPanel)
   private
    FOnMouseDown:TMouseEvent;
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
    property OnMouseDown:TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
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
 ParentBackground:=False;
end;

procedure TDrawPanel.Paint;
begin
 if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPanelExt.Paint;
begin
 if Brush.Bitmap <> nil then Canvas.Draw(0, 0, Brush.Bitmap);
 if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPanelExt.SetDefaultPaint(const Value: Boolean);
begin
 FDefaultPaint := Value;
 Paint;
end;

procedure TPanelExt.WMNCPaint(var Message: TMessage);
begin
 if FDefaultPaint then inherited;
 if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TPanelExt.CMMouseEnter(var message: TMessage);
begin
 AOnPanel:=True;
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TPanelExt.CMMouseLeave(var message: TMessage);
begin
 AOnPanel:=False;
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
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

{ TDragPanel }

constructor TDragPanel.Create(AOwner: TComponent);
begin
 inherited;
 inherited OnMouseDown:=FMouseDown;
end;

procedure TDragPanel.DoDrag;
begin
 ReleaseCapture;
 SendMessage(Self.Parent.Handle, WM_SYSCOMMAND, 61458, 0);
end;

procedure TDragPanel.FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 DoDrag;
 if Assigned(FOnMouseDown) then FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TDragPanel.SetOnMouseDown(const Value: TMouseEvent);
begin
 FOnMouseDown:=Value;
end;

end.
