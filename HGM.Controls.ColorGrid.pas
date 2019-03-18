unit HGM.Controls.ColorGrid;

interface
 uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Grids, Vcl.ComCtrls, System.Types, Vcl.StdCtrls,
  System.Generics.Collections;

 type
  TColorItem = record
   Value:TColor;
   class function Create(Value:TColor):TColorItem; static;
  end;
  TColorColumn = TList<TColorItem>;
  TColorColumns = class(TList<TColorColumn>)
   public
    procedure Clear;
  end;

  TCustomColorGrid = class(TCustomDrawGrid)
   private
    FCreating:Boolean;
    FHotItem:TPoint;
    FSelectedItem:TPoint;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseLeave: TNotifyEvent;
    FColorColumns:TColorColumns;
    FOnExit: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FBorderItemsColor: TColor;
    FBorderItems: Boolean;
    FInlineColumn: Boolean;
    procedure SetOnMouseDown(const Value: TMouseEvent);
    procedure SetOnMouseMove(const Value: TMouseMoveEvent);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure UpdateCellSizes(DoRepaint: Boolean);
    procedure SetColorColumns(const Value: TColorColumns);
    procedure UpdateColorData;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function CheckCell(C, R: Integer): Boolean;
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure MouseLeave(Sender: TObject);
    procedure SetSelectedItem(const Value: TPoint);
    function GetSelectedColor: TColor;
    procedure CtrlExit(Sender: TObject);
    procedure SetOnExit(const Value: TNotifyEvent);
    procedure SetOnSelect(const Value: TNotifyEvent);
    procedure SetSelectedColor(const Value: TColor);
    function GetIsSelected: Boolean;
    procedure SetBorderItemsColor(const Value: TColor);
    procedure SetBorderItems(const Value: Boolean);
    procedure FillDefault;
    procedure SetInlineColumn(const Value: Boolean);
   protected
    property RowHeights;
    property ColWidths;
   public
    constructor Create(AOwner: TComponent); override;
    procedure Update; override;
    property SelectedColor:TColor read GetSelectedColor write SetSelectedColor;
    property IsSelected:Boolean read GetIsSelected;
    property SelectedItem:TPoint read FSelectedItem write SetSelectedItem;
    property ColorColumns:TColorColumns read FColorColumns write SetColorColumns;
   published
    property OnMouseDown:TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseMove:TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property OnExit:TNotifyEvent read FOnExit write SetOnExit;
    property OnSelect:TNotifyEvent read FOnSelect write SetOnSelect;
    property BorderItemsColor:TColor read FBorderItemsColor write SetBorderItemsColor default $00EBDCD0;
    property BorderItems:Boolean read FBorderItems write SetBorderItems default True;
    property InlineColumn:Boolean read FInlineColumn write SetInlineColumn default True;
  end;

  ThColorGrid = class(TCustomColorGrid)
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
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
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
    property OnDrawCell;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation
 uses HGM.Common, Math;

procedure Register;
begin
 RegisterComponents(PackageName, [ThColorGrid]);
end;

{ TColorGrid }

procedure TCustomColorGrid.FillDefault;
var Column:TColorColumn;
begin
 ColorColumns.Clear;
 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create(clWhite));
 Column.Add(TColorItem.Create($00CCCCCC));
 Column.Add(TColorItem.Create($00A5A5A5));
 Column.Add(TColorItem.Create($00666666));
 Column.Add(TColorItem.Create($00333333));
 Column.Add(TColorItem.Create(clBlack));
 ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create(clRed));
 Column.Add(TColorItem.Create($0000C0FF));
 Column.Add(TColorItem.Create(clYellow));
 Column.Add(TColorItem.Create($0050B000));
 Column.Add(TColorItem.Create($00BB4D00));
 Column.Add(TColorItem.Create($00D3009B));
 ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create($004D50C0));
 Column.Add(TColorItem.Create($004696F7));
 Column.Add(TColorItem.Create($0059BB9B));
 Column.Add(TColorItem.Create($00C6AC4B));
 Column.Add(TColorItem.Create($00BD814F));
 Column.Add(TColorItem.Create($00A26480));
 ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create($004963D1));
 Column.Add(TColorItem.Create($004990D1));
 Column.Add(TColorItem.Create($0000B4CC));
 Column.Add(TColorItem.Create($008CB08F));
 Column.Add(TColorItem.Create($00866B64));
 Column.Add(TColorItem.Create($007C7C9E));
 ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create($008484DD));
 Column.Add(TColorItem.Create($0047A4F3));
 Column.Add(TColorItem.Create($0004CEDF));
 Column.Add(TColorItem.Create($0092B5A5));
 Column.Add(TColorItem.Create($00C29E80));
 Column.Add(TColorItem.Create($00C0859C));
 ColorColumns.Add(Column);
end;

procedure TCustomColorGrid.DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var CRect:TRect;
begin
 with Canvas do
  begin
   Brush.Color:=Color;
   Brush.Style:=bsSolid;
   FillRect(Rect);
   Brush.Color:=FColorColumns[ACol][ARow].Value;

   //Brush.Color:=clRed + (ARow + 1) * 20 + (ACol + 1) * 100;
   CRect:=Rect;
   if FInlineColumn then
    CRect.Inflate(-2, 0)
   else CRect.Inflate(-2, -2);
   FillRect(CRect);

   if FBorderItems then
    begin
     Pen.Color:=FBorderItemsColor;
     Pen.Width:=1;
     MoveTo(CRect.Left, CRect.Top);
     LineTo(CRect.Left, CRect.Bottom);

     MoveTo(CRect.Right-1, CRect.Top);
     LineTo(CRect.Right-1, CRect.Bottom);

     if (ARow = 0) or (not FInlineColumn) then
      begin
       MoveTo(CRect.Left, CRect.Top);
       LineTo(CRect.Right-1, CRect.Top);
      end;

     if (ARow = (RowCount-1)) or (not FInlineColumn) then
      begin
       MoveTo(CRect.Left, CRect.Bottom-1);
       LineTo(CRect.Right-1, CRect.Bottom-1);
      end;
    end;

   if (ACol = FSelectedItem.X) and (ARow = FSelectedItem.Y) then
    begin
     Brush.Style:=bsClear;
     Pen.Color:=$001048EF;
     Rectangle(CRect);
     CRect.Inflate(-1, -1);
     Pen.Color:=$0094E2FF;
     Rectangle(CRect);
    end;

   CRect:=Rect;
   if FInlineColumn then
    CRect.Inflate(-2, 0)
   else CRect.Inflate(-2, -2);
   if (ACol = FHotItem.X) and (ARow = FHotItem.Y) then
    begin
     Brush.Style:=bsClear;
     Pen.Color:=$003694F2;
     Rectangle(CRect);
     CRect.Inflate(-1, -1);
     Pen.Color:=$0094E2FF;
     Rectangle(CRect);
    end;
  end;
end;

function TCustomColorGrid.GetIsSelected: Boolean;
begin
 Result:=(FSelectedItem.X >= 0) and (FSelectedItem.Y >= 0);
end;

function TCustomColorGrid.GetSelectedColor: TColor;
begin
 if CheckCell(FSelectedItem.X, FSelectedItem.Y) then
  Result:=FColorColumns[FSelectedItem.X][FSelectedItem.Y].Value
 else Result:=clNone;
end;

procedure TCustomColorGrid.UpdateColorData;
var i, m:Integer;
begin
 if FCreating then Exit;
 ColCount:=Max(1, FColorColumns.Count);
 if ColCount > 0 then
  begin
   m:=1;
   for i:= 0 to FColorColumns.Count-1 do
    begin
     m:=Max(FColorColumns[i].Count, m);
    end;
   RowCount:=m;
  end
 else RowCount:=1;
 UpdateCellSizes(False);
end;

procedure TCustomColorGrid.SetBorderItems(const Value: Boolean);
begin
 FBorderItems := Value;
 Repaint;
end;

procedure TCustomColorGrid.SetBorderItemsColor(const Value: TColor);
begin
 FBorderItemsColor := Value;
 Repaint;
end;

procedure TCustomColorGrid.SetColorColumns(const Value: TColorColumns);
begin
 FColorColumns:=Value;
 UpdateColorData;
end;

procedure TCustomColorGrid.SetInlineColumn(const Value: Boolean);
begin
 FInlineColumn := Value;
 Repaint;
end;

procedure TCustomColorGrid.Update;
begin
 inherited;
 UpdateColorData;
end;

procedure TCustomColorGrid.UpdateCellSizes(DoRepaint:Boolean);
begin
 if FCreating then Exit;
 DefaultColWidth:=ClientWidth div ColCount;
 ColWidths[ColCount-1]:=DefaultColWidth;// + ClientWidth mod ColCount;

 DefaultRowHeight:=ClientHeight div RowCount;
 RowHeights[RowCount-1]:=DefaultRowHeight;// + ClientHeight mod RowCount;
end;

function TCustomColorGrid.CheckCell(C, R:Integer):Boolean;
begin
 Result:=False;
 if FColorColumns.Count <= 0 then Exit;
 if (C >= 0) and (R >= 0) then
  begin
   if C < FColorColumns.Count then
    if FColorColumns[C].Count > 0 then
     if R < FColorColumns[C].Count then Exit(True);
  end;
 Result:=False;
end;

procedure TCustomColorGrid.CtrlExit(Sender: TObject);
begin
 FHotItem:=Point(-1, -1);
 Repaint;
 if Assigned(FOnExit) then FOnExit(Sender);
end;

procedure TCustomColorGrid.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if CheckCell(FHotItem.X, FHotItem.Y) then
  begin
   SelectedItem:=FHotItem;
   FOnSelect(Self);
  end;
 if Assigned(FOnMouseDown) then FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TCustomColorGrid.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var C, R:Integer;
begin
 MouseToCell(X, Y, C, R);
 if CheckCell(C, R)
 then FHotItem:=Point(C, R)
 else FHotItem:=Point(-1, -1);
 if Assigned(FOnMouseMove) then FOnMouseMove(Sender, Shift, X, Y);
 Repaint;
end;

procedure TCustomColorGrid.MouseLeave(Sender: TObject);
begin
 FHotItem:=Point(-1, -1);
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Sender);
end;

procedure TCustomColorGrid.DefineProperties(Filer: TFiler);
begin
 //
end;

procedure TCustomColorGrid.WMSize(var Message: TWMSize);
begin
 DisableAlign;
 try
  inherited;
  UpdateCellSizes(False);
 finally
  EnableAlign;
 end;
end;

constructor TCustomColorGrid.Create(AOwner: TComponent);
var i, j:Integer;
    Item:TColorColumn;
    CItem:TColorItem;
begin
 FCreating:=True;
 inherited;
 FBorderItemsColor:=$00EBDCD0;
 FBorderItems:=True;
 FSaveCellExtents:=False;
 FInlineColumn:=True;
 FHotItem:=Point(-1, -1);
 FSelectedItem:=Point(-1, -1);
 DoubleBuffered:=True;
 FColorColumns:=TColorColumns.Create;
 FillDefault;
 Width := 100;
 Height := 100;
 BorderStyle := bsNone;
 DefaultColWidth := 20;
 DefaultRowHeight := 16;
 DefaultDrawing := False;
 DrawingStyle := gdsGradient;
 FixedCols := 0;
 RowCount := 6;
 FixedRows := 0;
 GridLineWidth := 0;
 ScrollBars := ssNone;
 OnDrawCell := DrawCell;
 inherited OnMouseDown:=MouseDown;
 inherited OnMouseMove:=MouseMove;
 inherited OnMouseLeave:=MouseLeave;
 inherited OnExit:=CtrlExit;
 FCreating:=False;
end;

procedure TCustomColorGrid.SetOnExit(const Value: TNotifyEvent);
begin
 FOnExit := Value;
end;

procedure TCustomColorGrid.SetOnMouseDown(const Value: TMouseEvent);
begin
 FOnMouseDown := Value;
end;

procedure TCustomColorGrid.SetOnMouseLeave(const Value: TNotifyEvent);
begin
 FOnMouseLeave := Value;
end;

procedure TCustomColorGrid.SetOnMouseMove(const Value: TMouseMoveEvent);
begin
 FOnMouseMove := Value;
end;

procedure TCustomColorGrid.SetOnSelect(const Value: TNotifyEvent);
begin
 FOnSelect := Value;
end;

procedure TCustomColorGrid.SetSelectedColor(const Value: TColor);
var i, j: Integer;
begin
 for i:= 0 to FColorColumns.Count-1 do
  for j := 0 to FColorColumns[i].Count-1 do
   if FColorColumns[i][j].Value = Value then
    begin
     SelectedItem:=Point(i, j);
     //Repaint;
     Exit;
    end;
 SelectedItem:=Point(-1, -1);
end;

procedure TCustomColorGrid.SetSelectedItem(const Value: TPoint);
begin
 FSelectedItem := Value;
 Repaint;
end;

{ TColorItem }

class function TColorItem.Create(Value: TColor): TColorItem;
begin
 Result.Value:=Value;
end;

{ TColorColumns }

procedure TColorColumns.Clear;
var i:Integer;
begin
 for i:= 0 to Count-1 do Items[i].Clear;
 inherited;
end;

end.
