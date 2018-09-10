unit ListBoxDraw;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,
  Vcl.ExtCtrls, System.UITypes, Vcl.Grids;

type
  TGetDataProc = procedure(Index:Integer; var Value:string) of object;
  TItemClick = procedure(Sender:TObject; MouseButton:TMouseButton; const Index:Integer) of object;

  TListBoxEx = class(TCustomDrawGrid)
   private
    FItemDowned:Boolean;
    FItemIndex:Integer;
    FCordHot:TGridCoord;
    FGetDataProc:TGetDataProc;
    FWheelDown:TMouseWheelUpDownEvent;
    FWheelUp:TMouseWheelUpDownEvent;
    FOnMouseMove:TMouseMoveEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnItemClick:TItemClick;
    FOnDrawCell:TDrawCellEvent;
    FDefDrawing: Boolean;
    procedure FDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State:TGridDrawState);
    procedure FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FMouseLeave(Sender: TObject);
    procedure FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetRowCount(Value: Integer);
    function GetRowCount: Longint;
    procedure SetColumnWidth;
    procedure SetWidth(const Value: Integer);
    function GetWidth: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetShowScrollBar(const Value: Boolean);
    function GetShowScrollBar: Boolean;
    procedure SetDefDrawing(const Value: Boolean);
   public
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    constructor Create(AOwner: TComponent); override;
   published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultRowHeight;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Touch;
    property Visible;
    property StyleElements;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseUp;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnMouseEnter;
    property ShowScrollBar:Boolean read GetShowScrollBar write SetShowScrollBar default True;
    property DefaultDataDrawing:Boolean read FDefDrawing write SetDefDrawing default True;
    property OnDrawCellData:TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property Width:Integer read GetWidth write SetWidth;
    property ItemIndex:Integer read FItemIndex write SetItemIndex;
    property ItemCount:Longint read GetRowCount write SetRowCount default 5;
    property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove:TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnItemClick:TItemClick read FOnItemClick write FOnItemClick;
    property GetData:TGetDataProc read FGetDataProc write FGetDataProc;
    property OnMouseWheelDown:TMouseWheelUpDownEvent read FWheelDown write FWheelDown;
    property OnMouseWheelUp:TMouseWheelUpDownEvent read FWheelUp write FWheelUp;
  end;


procedure Register;

implementation
 uses Math;

function MixColors(Color1, Color2:TColor; Alpha:Byte):TColor;
var C1, C2:LongInt;
    R, G, B, V1, V2:Byte;
begin
 Alpha:=Round(2.55 * Alpha);
 C1:=ColorToRGB(Color1);
 C2:=ColorToRGB(Color2);
 V1:=Byte(C1);
 V2:=Byte(C2);
 R:=Alpha * (V1 - V2) shr 8 + V2;
 V1:=Byte(C1 shr 8);
 V2:=Byte(C2 shr 8);
 G:=Alpha * (V1 - V2) shr 8 + V2;
 V1:=Byte(C1 shr 16);
 V2:=Byte(C2 shr 16);
 B:=Alpha * (V1 - V2) shr 8 + V2;
 Result:=(B shl 16) + (G shl 8) + R;
end;

procedure Register;
begin
 RegisterComponents('LKDU', [TListBoxEx]);
end;

function Between(FMin, FValue, FMax:Integer):Boolean;
begin
 Result:=(FValue >= FMin) and (FValue <= FMax);
end;

function ScaledRect(const Src:TRect; Delta:Integer):TRect;
begin
 Result:=Src;                           //Rect(1, 1, 4, 4)
 Result.Left:=Result.Left - Delta;      //Scale 1 = Rect(0, 0, 5, 5)
 Result.Top:=Result.Top - Delta;
 Result.Right:=Result.Right + Delta;
 Result.Bottom:=Result.Bottom + Delta;
end;

function ColorDarker(Color:TColor; Percent:Byte = 40):TColor;
var R, G, B:Byte;
begin
 if Percent > 100 then Percent:=100;
 Color:=ColorToRGB(Color);
 R:=GetRValue(Color);
 G:=GetGValue(Color);
 B:=GetBValue(Color);
 R:=R - MulDiv(R, Percent, 100);
 G:=G - MulDiv(G, Percent, 100);
 B:=B - MulDiv(B, Percent, 100);
 Result:=RGB(R, G, B);
end;

function ColorLighter(Color:TColor; Percent:Byte = 40):TColor;
var R, G, B:Byte;
begin
 Color:=ColorToRGB(Color);
 R:=GetRValue(Color);
 G:=GetGValue(Color);
 B:=GetBValue(Color);
 R:=R + MulDiv(255-R, Percent, 100);
 G:=G + MulDiv(255-G, Percent, 100);
 B:=B + MulDiv(255-B, Percent, 100);
 Result:=RGB(R, G, B);
end;

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: String; FDrawFlags:Cardinal = DT_CENTER): Integer;
var DrawRect: TRect;
    DrawFlags: Cardinal;
    DrawParams: TDrawTextParams;
begin
 DrawRect:= R;
 DrawFlags:= DT_END_ELLIPSIS or DT_NOPREFIX or DT_WORDBREAK or DT_EDITCONTROL or FDrawFlags;
 DrawText(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
 DrawRect.Right:= R.Right;
 if DrawRect.Bottom < R.Bottom then
      OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
 else DrawRect.Bottom:= R.Bottom;
 ZeroMemory(@DrawParams, SizeOf(DrawParams));
 DrawParams.cbSize:= SizeOf(DrawParams);
 DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
 Result:= DrawParams.uiLengthDrawn;
end;

{ TListBoxEx }

procedure TListBoxEx.SetRowCount(Value: Longint);
begin
 inherited RowCount:=Max(1, Value);
 SetColumnWidth;
end;

procedure TListBoxEx.SetShowScrollBar(const Value: Boolean);
begin
 case Value of
  True:ScrollBars:=ssVertical;
  False:ScrollBars:=ssNone;
 end;
 SetColumnWidth;
end;

procedure TListBoxEx.SetWidth(const Value: Integer);
begin
 inherited Width:=Value;
 SetColumnWidth;
end;

procedure TListBoxEx.SetColumnWidth;
var ColWidth:Integer;
begin
 if Width > 0 then
  begin
   if not Assigned(Parent) then
    begin
     DefaultColWidth:=Width-20;
    end
   else
    begin
     ColWidth:=ClientWidth;
     if ColWidth < 1 then ColWidth:= 1;
     DefaultColWidth:=ColWidth;
    end;
  end;
end;

procedure TListBoxEx.SetDefDrawing(const Value: Boolean);
begin
 FDefDrawing:= Value;
 Repaint;
end;

procedure TListBoxEx.SetItemIndex(const Value: Integer);
begin
 FItemIndex:= Value;
 if Value >= 0 then Row:=Value;
 Repaint;
end;

constructor TListBoxEx.Create(AOwner: TComponent);
begin
 inherited;
 inherited OnMouseMove:=FMouseMove;
 inherited OnMouseUp:=FMouseUp;
 inherited OnMouseDown:=FMouseDown;
 inherited OnMouseLeave:=FMouseLeave;
 inherited OnDrawCell:=FDrawCell;
 DefaultColWidth:=140;
 DrawingStyle:=gdsGradient;
 FDefDrawing:=True;
 Width:=160;
 FItemIndex:=-1;
 FItemDowned:=False;
 FCordHot.X:=-1;
 FCordHot.Y:=-1;
 ColCount:=1;
 FixedCols:=0;
 FixedRows:=0;
 inherited DefaultDrawing:=False;
 Options:=[goRowSelect, goThumbTracking];
 ScrollBars:=ssVertical;
end;

function TListBoxEx.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 Result:=True;
 Perform(WM_VSCROLL, SB_LINEDOWN, 0);
 Repaint;
end;

function TListBoxEx.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 Result:=True;
 Perform(WM_VSCROLL, SB_LINEUP, 0);
 Repaint;
end;

procedure TListBoxEx.FDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var BColor:TColor;
    ID:Integer;
    TextValue:string;

procedure DrawText(Value:string; Offset:Integer = 0);
var RText:TRect;
begin
 with Canvas do
  begin
   RText:=Rect;
   RText.Left:=RText.Left + 1 + Offset;
   RText.Top:=RText.Top + 1;
   RText.Right:=RText.Right - 1;
   RText.Bottom:=RText.Bottom - 1;
   DrawTextCentered(Canvas, RText, Value, DT_LEFT);
  end;
end;

begin
 with Canvas do
  begin
   TextValue:='?';
   Brush.Style:=bsSolid;
   Pen.Style:=psSolid;
   Brush.Color:=Color;
   Pen.Color:=Brush.Color;
   Rectangle(Rect);

   Brush.Color:=$00F1F2F2;

   Pen.Color:=Brush.Color;
   Rectangle(Rect);
   Pen.Color:=ColorDarker(Brush.Color);
   MoveTo(Rect.Left, Rect.Bottom-1);
   LineTo(Rect.Right, Rect.Bottom-1);
   if ARow mod 2 = 0 then Brush.Color:=$00E7E8E8 else Brush.Color:=$00F1F2F2;
   //if ARow = FItemIndex then Brush.Color:=ColorDarker($00E7E8E8);
   if ARow = Row then
    begin
     Brush.Color:=ColorDarker($00E7E8E8);
     Font.Color:=clWhite;
    end
   else
    begin
     Font.Color:=$00282828;
    end;
   Pen.Color:=Brush.Color;
   Rectangle(Rect);

   if (FCordHot.Y = ARow) and (ARow <> Row) then
    begin
     if FItemDowned then BColor:=ColorDarker($00DCDCDC, 20) else BColor:=$00DCDCDC;

     Brush.Color:=MixColors(BColor, Canvas.Brush.Color, 150);
     Pen.Color:=Brush.Color;

     Brush.Style:=bsSolid;
     Pen.Style:=psSolid;
     Rectangle(Rect);
    end;

   if not FDefDrawing then
    begin
     if Assigned(FOnDrawCell) then FOnDrawCell(Sender, ACol, ARow, Rect, State);
     Exit;
    end;

   Brush.Style:=bsClear;
   ID:=ARow;
   if Assigned(FGetDataProc) then FGetDataProc(ID, TextValue);
   DrawText(TextValue);
  end;
end;

procedure TListBoxEx.FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FItemDowned:=True;
end;

procedure TListBoxEx.FMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Row >= 0 then
  begin
   if FItemDowned then
    begin
     FItemIndex:=Row;
     if Assigned(FOnItemClick) then FOnItemClick(Sender, Button, FItemIndex);
    end;
  end;
 FItemDowned:=False;
end;

procedure TListBoxEx.FMouseLeave(Sender: TObject);
begin
 FCordHot.X:=-1;
 FCordHot.Y:=-1;
 FItemDowned:=False;
 Repaint;
end;

procedure TListBoxEx.FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if not Focused then SetFocus;
 FCordHot:=MouseCoord(X, Y);
 if Between(0, FCordHot.Y, RowCount-1) then
  begin
   Cursor:=crHandPoint;
  end
 else Cursor:=crDefault;
 Repaint;
 if Assigned(FOnMouseMove) then FOnMouseMove(Sender, Shift, X, Y);
end;

function TListBoxEx.GetRowCount:Longint;
begin
 Result:=RowCount;
end;

function TListBoxEx.GetShowScrollBar:Boolean;
begin
 Result:=ScrollBars <> ssNone;
end;

function TListBoxEx.GetWidth: Integer;
begin
 Result:=inherited Width;
end;

end.
