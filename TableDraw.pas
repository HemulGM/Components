unit TableDraw;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, System.Generics.Collections,
  Vcl.ExtCtrls, System.UITypes, Vcl.Grids, Vcl.Mask, Direct2D, D2D1;

type
  TTableEx = class;

  TTableData<T> = class(TList<T>)
   type TTables = TList<TTableEx>;
   private
    FTables:TTables;
    FUpdate:Integer;
    function GetTable(Index:Integer):TTableEx;
    procedure InitNotif(Sender:TObject; const Item:T; Action:TCollectionNotification);
   public
    procedure BeginUpdate;
    procedure EndUpdate;
    constructor Create(AOwner: TTableEx); overload; virtual;
    constructor Create; overload; virtual;
    function Add(Value:T):Integer; virtual;

    procedure Clear; virtual;
    procedure Delete(Index:Integer); virtual;
    //
    procedure AddTable(pTable:TTableEx);
    procedure UnAssignTables;
    procedure UpdateTable; virtual;
    property Tables[Index:Integer]:TTableEx read GetTable;
  end;

  TEditMode = (teText, teList, teDate, teMask, teTime, teInt, teFloat);

  TTableEditStruct = record
   EditMode:TEditMode;
   EditMask:string;
   TextValue:string;
   ItemValue:Integer;
   FixedList:Boolean;
   Items:TStringList;
   ReadOnly:Boolean;
   ListDrop:Boolean;
  end;

  TFieldMaskEdit = class(TMaskEdit)
   protected
    procedure ValidateError; override;
   public
    function Validate(const Value: string; var Pos: Integer): Boolean; override;
  end;

  TGetTableDataProc = procedure(FCol, FRow:Integer; var Value:string) of object;
  TItemClick = procedure(Sender:TObject; MouseButton:TMouseButton; const Index:Integer) of object;
  TColumnClick = procedure(Sender:TObject; MouseButton:TMouseButton; const Index:Integer) of object;
  TOnEdit = procedure(Sender:TObject; var Data:TTableEditStruct; ACol, ARow:Integer; var Allow:Boolean) of object;
  TOnEditOK = procedure(Sender:TObject; Value:string; ItemValue:Integer; ACol, ARow:Integer) of object;
  TOnEditCancel = procedure(Sender:TObject; ACol, ARow:Integer) of object;

  TTableColumn = class
   private
    FIndex:Cardinal;
    FOwner:TTableEx;
    FAsButton:Boolean;
    FCaption:string;
    FShowButtonOnlySelect: Boolean;
    FMinWidth: Cardinal;
    FTextFormat: TTextFormat;
    FFormatColumns: TTextFormat;
    function GetWidth:Cardinal; virtual;
    procedure SetWidth(const Value:Cardinal); virtual;
    function GetIndex:Cardinal; virtual;
    procedure SetCaption(const Value:string); virtual;
    procedure SetIndex(const Value: Cardinal);
    procedure SetTextFormat(const Value: TTextFormat);
    procedure SetFormatColumns(const Value: TTextFormat);
   public
    property Index:Cardinal read GetIndex write SetIndex;
    constructor Create(AOwner:TTableEx); virtual;
    property Caption:string read FCaption write SetCaption;
    property Width:Cardinal read GetWidth write SetWidth;
    property Format:TTextFormat read FTextFormat write SetTextFormat;
    property FormatColumns:TTextFormat read FFormatColumns write SetFormatColumns;
    property MinWidth:Cardinal read FMinWidth write FMinWidth;
    property AsButton:Boolean read FAsButton write FAsButton;
    property ShowButtonOnlySelect:Boolean read FShowButtonOnlySelect write FShowButtonOnlySelect;
  end;

  TTableColumns = TList<TTableColumn>;

  TTableEx = class(TCustomDrawGrid)
   private
    FUpdatesCount:Integer;
    FItemDowned:Boolean;
    FEditData:TTableEditStruct;
    FEditCellRect:TRect;
    FItemIndex:Integer;
    FOnEdit:TOnEdit;
    FOnEditCancel:TOnEditCancel;
    FOnEditOk:TOnEditOK;
    FCordHot:TGridCoord;
    FSetFocusOnEnter:Boolean;
    FGetDataProc:TGetTableDataProc;
    FWheelDown:TMouseWheelUpDownEvent;
    FWheelUp:TMouseWheelUpDownEvent;
    FOnMouseMove:TMouseMoveEvent;
    FOnMouseLeave:TNotifyEvent;
    FOnMouseDown:TMouseEvent;
    FOnItemClick:TItemClick;
    FOnItemColClick:TItemClick;
    FOnColumnClick:TColumnClick;
    FOnDrawCell:TDrawCellEvent;
    FAfterDrawText:TDrawCellEvent;
    FOnDrawColumn:TDrawCellEvent;
    FDefDrawing: Boolean;
    FColumns:TTableColumns;
    FShowColumns: Boolean;
    FColumnsHeight:Integer;
    FOnKeyUp:TKeyEvent;
    FOnKeyDown:TKeyEvent;
    FOnMouseUp: TMouseEvent;
    FLineColor: TColor;
    FLineColorXor: TColor;
    FLineHotColor: TColor;
    FLineSelColor: TColor;
    FColumnsColor: TColor;
    FFontHotLine: TFont;
    FFontLine: TFont;
    FFontSelLine: TFont;
    FColumnsFont: TFont;
    FShowFocus:Boolean;
    FOnActivate:TNotifyEvent;
    FOnDeactivate:TNotifyEvent;
    FEditingCell:TGridCoord;
    FEditing:Boolean;
    FFieldEdit:TFieldMaskEdit;
    FFieldCombo:TComboBox;
    FVisibleEdit:Boolean;
    FItemCount:Integer;
    FCanNoSelect:Boolean;
    FDrawColumnBorded: Boolean;

    FFlashSelectedCol: Boolean;
    FRoundLineRect: Integer;
    FProcEmpty: Boolean;
    FDrawColumnSections:Boolean;
    FMouseRightClickTooClick:Boolean;
    function DataRow:Integer;
    procedure CloseControl(Sender:TObject);
    procedure DoEditCancel;
    procedure DoEditOk;
    procedure HideEditField;
    procedure KeyPressControl(Sender: TObject; var Key: Char);
    procedure FFieldKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure FKeyUp(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure FKeyDown(Sender:TObject; var Key:Word; Shift:TShiftState);
    procedure FDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State:TGridDrawState);
    procedure FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FMouseLeave(Sender: TObject);
    procedure FOnEditChange(Sender: TObject);
    procedure FOnComboMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SetRowCount(Value: Integer);
    function GetRowCount: Longint;
    procedure SetWidth(const Value: Integer);
    function GetWidth: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetShowScrollBar(const Value: Boolean);
    function GetShowScrollBar: Boolean;
    procedure SetDefDrawing(const Value: Boolean);
    function GetColumnCount: Integer;
    procedure SetItemDowned(const Value: Boolean);
    procedure SetShowColumns(const Value: Boolean);
    procedure UpdateItemCount;
    function GetDefRowH: Integer;
    procedure SetDefRowH(const Value: Integer);
    function GetColumnsHeight: Integer;
    procedure SetColumnsHeight(const Value: Integer);
    procedure UpdateMouse(Cord:TGridCoord; Force:Boolean = False);
    procedure FListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetFocusedColumn: Integer;
    procedure SetFlashSelectedCol(const Value: Boolean);
    procedure SetRoundLineRect(const Value: Integer);
    procedure SetProcEmpty(const Value: Boolean);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetDrawColumnBorded(const Value: Boolean);
    procedure SetColumnsFont(const Value: TFont);
    procedure SetFontSelLine(const Value: TFont);
    procedure SetFontLine(const Value: TFont);
    procedure SetFontHotLine(const Value: TFont);
    procedure SetColumnsColor(const Value: TColor);
    procedure SetLineSelColor(const Value: TColor);
    procedure SetLineHotColor(const Value: TColor);
    procedure SetLineColorXor(const Value: TColor);
    procedure SetLineColor(const Value: TColor);
    procedure SetDrawColumnSections(const Value: Boolean);
    function GetItemUnderMouse: Integer;
    property ItemDowned:Boolean read FItemDowned write SetItemDowned;
    procedure UpdateColumnIndex;
    procedure FUpdateColumnsHeight;
    procedure WMReSize(var Msg:TWMSize); message WM_SIZE;
    property RowHeights;
    property ColWidths;
   protected
    procedure ColWidthsChanged; override;
    procedure LastFocus(var Msg:TMessage); message WM_ACTIVATE;
    procedure FOnDblClick;
    procedure SetEditing(Value:Boolean);
   public
    function AddColumn:Integer; overload;
    function FirstColumn(aCaption:string; aWidth:Integer; aButton:Boolean = False):Integer; deprecated;
    function AddColumn(aCaption:string; aWidth:Integer; aButton:Boolean = False):Integer; overload;
    procedure DeleteColumn(Index:Integer); override;
    property ColumnCount:Integer read GetColumnCount;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    procedure DoItemClick;
    procedure SetMaxColumn(ColumnID:Integer);
    procedure Repaint; override;
    procedure MouseToItem(Position:TPoint; var Index, Column:Integer);
    procedure CloseEdit;
    procedure CancelEdit;
    property Editing:Boolean read FEditing;
    function Edit(AItem, ACol:Integer):Boolean;
    property Col;
    property CordHot:TGridCoord read FCordHot;
    property FocusedColumn:Integer read GetFocusedColumn;
    property ItemUnderMouse:Integer read GetItemUnderMouse;
   published
    property OnActivate:TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate:TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property ColumnsHeight:Integer read GetColumnsHeight write SetColumnsHeight default 30;
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
    property DefaultRowHeight:Integer read GetDefRowH write SetDefRowH;
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
    property OnKeyPress;
    property OnMouseActivate;
    property OnSetEditText;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnMouseEnter;
    property Width:Integer read GetWidth write SetWidth;
    property OnMouseUp:TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseDown:TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseLeave:TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnKeyUp:TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnKeyDown:TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnMouseMove:TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseWheelDown:TMouseWheelUpDownEvent read FWheelDown write FWheelDown;
    property OnMouseWheelUp:TMouseWheelUpDownEvent read FWheelUp write FWheelUp;

    property AfterDrawText:TDrawCellEvent read FAfterDrawText write FAfterDrawText;
    property OnDrawCellData:TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawColumnData:TDrawCellEvent read FOnDrawColumn write FOnDrawColumn;

    property ItemIndex:Integer read FItemIndex write SetItemIndex;
    property OnColumnClick:TColumnClick read FOnColumnClick write FOnColumnClick;
    property OnItemClick:TItemClick read FOnItemClick write FOnItemClick;
    property OnItemColClick:TItemClick read FOnItemColClick write FOnItemColClick;
    property GetData:TGetTableDataProc read FGetDataProc write FGetDataProc;
    property OnEdit:TOnEdit read FOnEdit write FOnEdit;
    property OnEditCancel:TOnEditCancel read FOnEditCancel write FOnEditCancel;
    property OnEditOk:TOnEditOk read FOnEditOk write FOnEditOk;

    property ProcEmpty:Boolean read FProcEmpty write SetProcEmpty default False;
    property Columns:TTableColumns read FColumns write FColumns;
    property DefaultDataDrawing:Boolean read FDefDrawing write SetDefDrawing default True;
    property ShowScrollBar:Boolean read GetShowScrollBar write SetShowScrollBar default True;
    property CanNoSelect:Boolean read FCanNoSelect write FCanNoSelect default True;
    property VisibleEdit:Boolean read FVisibleEdit write FVisibleEdit default True;
    property ItemCount:Integer read GetRowCount write SetRowCount default 5;
    property LineColor:TColor read FLineColor write SetLineColor default $00F1F2F2;
    property LineColorXor:TColor read FLineColorXor write SetLineColorXor default $00E7E8E8;
    property LineHotColor:TColor read FLineHotColor write SetLineHotColor default $00DCDCDC;
    property LineSelColor:TColor read FLineSelColor write SetLineSelColor default $006C6C6C;
    property ColumnsColor:TColor read FColumnsColor write SetColumnsColor default $00DCDCDC;
    property FontHotLine:TFont read FFontHotLine write SetFontHotLine;
    property FontLine:TFont read FFontLine write SetFontLine;
    property FontSelLine:TFont read FFontSelLine write SetFontSelLine;
    property ShowColumns:Boolean read FShowColumns write SetShowColumns default True;
    property RoundLineRect:Integer read FRoundLineRect write SetRoundLineRect default 0;
    property ColumnsFont:TFont read FColumnsFont write SetColumnsFont;
    property SetFocusOnEnter:Boolean read FSetFocusOnEnter write FSetFocusOnEnter default False;
    property ShowFocus:Boolean read FShowFocus write SetShowFocus default False;
    property DrawColumnBorded:Boolean read FDrawColumnBorded write SetDrawColumnBorded default True;
    property DrawColumnSections:Boolean read FDrawColumnSections write SetDrawColumnSections default True;
    property FlashSelectedCol:Boolean read FFlashSelectedCol write SetFlashSelectedCol default False;
    property MouseRightClickTooClick:Boolean read FMouseRightClickTooClick write FMouseRightClickTooClick default False;
  end;

 function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: String; FDrawFlags:Cardinal): Integer;
 function Between(FMin, FValue, FMax:Integer):Boolean;
 function ScaledRect(const Src:TRect; Delta:Integer):TRect;
 function MixColors(Color1, Color2:TColor; Alpha:Byte):TColor;
 function ColorDarker(Color:TColor; Percent:Byte = 40):TColor;
 function ColorLighter(Color:TColor; Percent:Byte = 40):TColor;
 function IndexInList(const Index:Integer; ListCount:Integer):Boolean;
 function FlashControl(Control:TControl):Boolean;


procedure Register;

implementation
 uses Math;

function IndexInList(const Index:Integer; ListCount:Integer):Boolean;
begin
 Result:=(Index >= 0) and (Index <= ListCount - 1) and (ListCount > 0);
end;

procedure Register;
begin
 RegisterComponents('LKDU', [TTableEx]);
end;

{ TTableData<T> }

constructor TTableData<T>.Create(AOwner: TTableEx);
begin
 Create;
 if Assigned(AOwner) then FTables.Add(AOwner);
 UpdateTable;
end;

function TTableData<T>.Add(Value: T): Integer;
begin
 Result:=inherited Add(Value);
end;

procedure TTableData<T>.AddTable(pTable: TTableEx);
begin
 FTables.Add(pTable);
end;

procedure TTableData<T>.InitNotif(Sender:TObject; const Item:T; Action:TCollectionNotification);
begin
 UpdateTable;
end;

procedure TTableData<T>.BeginUpdate;
begin
 Inc(FUpdate);
end;

procedure TTableData<T>.Clear;
begin
 inherited Clear;
end;

constructor TTableData<T>.Create;
begin
 inherited;
 OnNotify:=InitNotif;
 FTables:=TTables.Create;
 FUpdate:=0;
end;

procedure TTableData<T>.Delete(Index: Integer);
begin
 inherited Delete(Index);
end;

procedure TTableData<T>.EndUpdate;
begin
 //FUpdate:=Max(0, FUpdate-1);
 FUpdate:=0;
 if FUpdate = 0 then UpdateTable;
end;

function TTableData<T>.GetTable(Index:Integer):TTableEx;
begin
 if not IndexInList(Index, FTables.Count) then Exit(nil);
 Result:=FTables[Index];
end;

procedure TTableData<T>.UnAssignTables;
begin
 FTables.Clear;
end;

procedure TTableData<T>.UpdateTable;
var i: Integer;
begin
 if FUpdate > 0 then Exit;
 if FTables.Count > 0 then
  for i:= 0 to FTables.Count-1 do
   if Assigned(FTables[i]) then
    begin
     FTables[i].ItemCount:=Count;
    end;
end;


function FlashControl(Control:TControl):Boolean;
var SaveColor, BufColor:TColor;

procedure SetColorE(Control:TEdit; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorME(Control:TMaskEdit; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorC(Control:TComboBox; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorP(Control:TPanel; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorM(Control:TMemo; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColorL(Control:TListBox; Color:TColor; var OldColor:TColor);
begin
 OldColor:=Control.Color;
 Control.Color:=Color;
end;

procedure SetColor(Control:TControl; Color:TColor; var OldColor:TColor);
begin
 if Control is TEdit then SetColorE(Control as TEdit, Color, OldColor);
 if Control is TMaskEdit then SetColorME(Control as TMaskEdit, Color, OldColor);
 if Control is TComboBox then SetColorC(Control as TComboBox, Color, OldColor);
 if Control is TPanel then SetColorP(Control as TPanel, Color, OldColor);
 if Control is TMemo then SetColorM(Control as TMemo, Color, OldColor);
 if Control is TListBox then SetColorL(Control as TListBox, Color, OldColor);
end;

begin
 Result:=True;
 with Control do
  begin
   SetColor(Control, clRed, SaveColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clMaroon, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clWhite, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clMaroon, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clRed, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clMaroon, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clWhite, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clMaroon, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clRed, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, clRed, BufColor);
   Repaint;
   Sleep(60);

   SetColor(Control, SaveColor, BufColor);
   Repaint;
   Sleep(60);
  end;
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

function DrawTextCentered(Canvas: TCanvas; const R: TRect; S: String; FDrawFlags:Cardinal): Integer;
var DrawRect: TRect;
    DrawFlags: Cardinal;
    DrawParams: TDrawTextParams;
begin
 DrawRect:=R;
 DrawFlags:= DT_END_ELLIPSIS or DT_NOPREFIX or DT_EDITCONTROL or FDrawFlags;
 DrawText(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
 DrawRect.Right:= R.Right;
 if DT_VCENTER and FDrawFlags = DT_VCENTER then
  begin
   if DrawRect.Bottom < R.Bottom then
        OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
   else DrawRect.Bottom:= R.Bottom;
  end;
 ZeroMemory(@DrawParams, SizeOf(DrawParams));
 DrawParams.cbSize:= SizeOf(DrawParams);
 DrawTextEx(Canvas.Handle, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
 Result:= DrawParams.uiLengthDrawn;
end;

{ TTableEx }

procedure TTableEx.SetRoundLineRect(const Value: Integer);
begin
 FRoundLineRect:=Value;
 Repaint;
end;

procedure TTableEx.SetRowCount(Value: Longint);
var msg:TWMSize;
begin
 if FItemCount <> Value then
  begin
   FItemCount:=Value;
   UpdateItemCount;
  end;
 if FEditing then DoEditCancel;
 WMReSize(msg);
 Repaint;
end;

function TTableEx.GetRowCount:Longint;
begin
 Result:=FItemCount;
end;

procedure TTableEx.SetShowColumns(const Value: Boolean);
begin
 if FShowColumns = Value then Exit;
 FShowColumns:=Value;
 UpdateItemCount;
 FUpdateColumnsHeight;
 Repaint;
end;

procedure TTableEx.SetShowFocus(const Value: Boolean);
begin
 FShowFocus := Value;
 Repaint;
end;

procedure TTableEx.SetShowScrollBar(const Value: Boolean);
begin
 case Value of
  True:ScrollBars:=ssBoth;
  False:ScrollBars:=ssNone;
 end;
end;

procedure TTableEx.SetWidth(const Value: Integer);
begin
 inherited Width:=Value;
end;

procedure TTableEx.UpdateColumnIndex;
var i:Integer;
begin
 if FColumns.Count > 0 then
  for i:= 0 to FColumns.Count - 1 do FColumns[i].Index:=i;
end;

procedure TTableEx.UpdateItemCount;
begin
 case FItemCount of
  0:begin
     FixedRows:=0;
     RowCount:=1;
    end;
  1:begin
     if FShowColumns then
      begin
       RowCount:=2;
       FixedRows:=1;
      end
     else
      begin
       FixedRows:=0;
       RowCount:=1;
      end;
    end;
 else
    begin
     if FShowColumns then
      begin
       RowCount:=FItemCount+1;
       FixedRows:=1;
      end
     else
      begin
       FixedRows:=0;
       RowCount:=FItemCount;
      end;
    end;
 end;
 if ItemIndex > (FItemCount-1) then ItemIndex:=Max(-1, FItemCount-1);
end;

procedure TTableEx.WMReSize(var Msg: TWMSize);
begin
 if not Assigned(Parent) then Exit;
 if ColumnCount <= 0 then Exit;
 SetMaxColumn(ColumnCount-1);
end;

procedure TTableEx.SetColumnsColor(const Value: TColor);
begin
 FColumnsColor := Value;
 Repaint;
end;

procedure TTableEx.SetColumnsFont(const Value: TFont);
begin
 FColumnsFont := Value;
 Repaint;
end;

procedure TTableEx.SetColumnsHeight(const Value: Integer);
begin
 FColumnsHeight:=Value;
 FUpdateColumnsHeight;
end;

procedure TTableEx.SetDefDrawing(const Value: Boolean);
begin
 FDefDrawing:= Value;
 Repaint;
end;

procedure TTableEx.SetDefRowH(const Value: Integer);
begin
 inherited DefaultRowHeight:=Value;
 FUpdateColumnsHeight;
end;

procedure TTableEx.SetDrawColumnBorded(const Value: Boolean);
begin
 FDrawColumnBorded := Value;
 Repaint;
end;

procedure TTableEx.SetDrawColumnSections(const Value: Boolean);
begin
 FDrawColumnSections := Value;
 Repaint;
end;

procedure TTableEx.SetEditing(Value: Boolean);
begin
 FEditing:=Value;
end;

procedure TTableEx.SetFlashSelectedCol(const Value: Boolean);
begin
 FFlashSelectedCol:=Value;
 Repaint;
end;

procedure TTableEx.SetFontHotLine(const Value: TFont);
begin
 FFontHotLine := Value;
 Repaint;
end;

procedure TTableEx.SetFontLine(const Value: TFont);
begin
 FFontLine := Value;
 Repaint;
end;

procedure TTableEx.SetFontSelLine(const Value: TFont);
begin
 FFontSelLine := Value;
 Repaint;
end;

procedure TTableEx.SetItemDowned(const Value: Boolean);
begin
 FItemDowned:=Value;
 Repaint;
end;

procedure TTableEx.SetItemIndex(const Value: Integer);
begin
 FItemIndex:= Value;
 if FItemIndex >= 0 then
  Row:=Max(0, Min(ItemCount, FItemIndex + Ord(FShowColumns)))
 else
  begin
   Col:=0;
   Row:=0;
   if not (csLoading in ComponentState) then
    SendMessage(Handle, WM_VSCROLL, SB_THUMBPOSITION, 0);
  end;
 UpdateMouse(FCordHot, True);
 Repaint;
end;

procedure TTableEx.SetLineColor(const Value: TColor);
begin
 FLineColor := Value;
 Repaint;
end;

procedure TTableEx.SetLineColorXor(const Value: TColor);
begin
 FLineColorXor := Value;
 Repaint;
end;

procedure TTableEx.SetLineHotColor(const Value: TColor);
begin
 FLineHotColor := Value;
 Repaint;
end;

procedure TTableEx.SetLineSelColor(const Value: TColor);
begin
 FLineSelColor := Value;
 Repaint;
end;

procedure TTableEx.SetMaxColumn(ColumnID: Integer);
var i:Integer;
    Sz:Integer;
begin
 if not Assigned(Parent) then Exit;
 if Parent.Handle = 0 then Exit;
 if ColumnID > Columns.Count-1 then Exit;
 Sz:=0;
 for i:= 0 to ColumnCount-1 do if i <> ColumnID then Sz:=Sz+Columns[i].Width;
 Columns[ColumnID].Width:=Max(Columns[ColumnID].MinWidth, ClientWidth - Sz - 1);
end;

procedure TTableEx.SetProcEmpty(const Value: Boolean);
begin
 FProcEmpty := Value;
 Repaint;
end;

function TTableEx.AddColumn:Integer;
begin
 Result:=FColumns.Add(TTableColumn.Create(Self));
 ColCount:=Max(FColumns.Count, 1);
 UpdateColumnIndex;
end;

function TTableEx.AddColumn(aCaption: string; aWidth: Integer; aButton: Boolean): Integer;
begin
 Result:=AddColumn;
 Columns[Result].Caption:=aCaption;
 Columns[Result].Width:=aWidth;
 Columns[Result].MinWidth:=aWidth;
 Columns[Result].AsButton:=aButton;
end;

procedure TTableEx.CancelEdit;
begin
 DoEditCancel;
end;

procedure TTableEx.CloseControl(Sender: TObject);
begin
 DoEditOk;
end;

procedure TTableEx.CloseEdit;
begin
 DoEditOk;
end;

procedure TTableEx.ColWidthsChanged;
begin
 inherited;
 if FEditing then DoEditCancel;
end;

constructor TTableEx.Create(AOwner: TComponent);
begin
 inherited;
 inherited OnMouseMove:=FMouseMove;
 inherited OnMouseUp:=FMouseUp;
 inherited OnMouseDown:=FMouseDown;
 inherited OnMouseLeave:=FMouseLeave;
 inherited OnDrawCell:=FDrawCell;
 inherited OnKeyUp:=FKeyUp;
 inherited OnKeyDown:=FKeyDown;
 inherited DefaultDrawing:=False;
 inherited DrawingStyle:=gdsGradient;
 inherited OnMouseWheel:=FOnComboMouseWheel;
 //inherited BevelInner:=bvNone;
 //inherited BevelKind:=bkFlat;
 //inherited BorderStyle:=bsNone;
 //FButtonOnItem:=-1;
 FUpdatesCount:=0;
 FDrawColumnBorded:=True;
 FDrawColumnSections:=True;
 FFlashSelectedCol:=False;

 FFieldEdit:=TFieldMaskEdit.Create(Self);
 with FFieldEdit do
  begin
   FFieldEdit.Parent:=Self;
   FFieldEdit.OnExit:=CloseControl;
   FFieldEdit.OnKeyPress:=KeyPressControl;
   FFieldEdit.OnKeyDown:=FFieldKeyUp;
   FFieldEdit.OnChange:=FOnEditChange;
   FFieldEdit.Visible:=False;
   FFieldEdit.DoubleBuffered:=True;
   FFieldEdit.ParentColor:=False;
   FFieldEdit.ParentBackground:=False;
   FFieldEdit.Ctl3D:=False;
   FFieldEdit.BevelInner:=bvNone;
   FFieldEdit.BevelKind:=bkSoft;
   FFieldEdit.BevelOuter:=bvSpace;
   FFieldEdit.BevelWidth:=3;
   FFieldEdit.BorderStyle:=bsNone;
   FFieldEdit.EditMask:='';
   FFieldEdit.Width:=0;
   FFieldEdit.Height:=0;
   ///FFieldEdit.v
  end;

 FFieldCombo:=TComboBox.Create(Self);
 with FFieldCombo do
  begin
   FFieldCombo.Parent:=Self;
   FFieldCombo.OnExit:=CloseControl;
   FFieldCombo.OnKeyPress:=KeyPressControl;
   FFieldCombo.OnKeyDown:=FListKeyUp;
   //FFieldCombo.OnChange:=CloseControl;
   FFieldCombo.OnClick:=CloseControl;
   FFieldCombo.Visible:=False;
   FFieldCombo.Ctl3D:=False;
   FFieldCombo.Width:=0;
   FFieldCombo.Height:=0;
   FFieldCombo.ParentFont:=False;
   FFieldCombo.AutoComplete:=False;
  end;

 DefaultDataDrawing:=False;
 FVisibleEdit:=True;
 FDefDrawing:=True;
 FSetFocusOnEnter:=False;
 FItemIndex:=-1;
 FItemDowned:=False;
 FCordHot.X:=-1;
 FCordHot.Y:=-1;
 FLineColorXor:=$00E7E8E8;
 FLineColor:=$00F1F2F2;
 FLineSelColor:=$006C6C6C;
 FLineHotColor:=$00DCDCDC;
 FColumnsColor:=$00DCDCDC;

 FFontSelLine:=TFont.Create;
 FFontSelLine.Assign(Font);
 FFontSelLine.Color:=clWhite;

 FFontHotLine:=TFont.Create;
 FFontHotLine.Assign(Font);
 FFontHotLine.Color:=$00282828;

 FFontLine:=TFont.Create;
 FFontLine.Assign(Font);
 FFontLine.Color:=$00282828;

 FColumnsFont:=TFont.Create;
 FColumnsFont.Assign(Font);
 FColumnsFont.Color:=$00282828;

 FColumns:=TTableColumns.Create;
 //FColumns.Add(TTableColumn.Create(Self));
 UpdateColumnIndex;

 ColCount:=1;
 RowCount:=2;
 FItemCount:=1;
 FixedCols:=0;
 FixedRows:=1;
 Width:=400;
 DefaultColWidth:=200;
 DefaultRowHeight:=25;
 Options:=[goThumbTracking, goColSizing];
 ColumnsHeight:=30;
 ShowColumns:=True;
end;

function TTableEx.DataRow: Integer;
begin
 Result:=ItemIndex + Ord(FShowColumns);
end;

procedure TTableEx.DeleteColumn(Index: Integer);
begin
 FColumns.Delete(Index);
 UpdateColumnIndex;
end;

procedure TTableEx.DoEditCancel;
begin
 if FEditing then
  begin
   SetEditing(False);
   HideEditField;
   SetFocus;
  end;
 if Assigned(FOnEditCancel) then FOnEditCancel(Self, FEditingCell.X, FEditingCell.Y);
end;

procedure TTableEx.DoEditOk;
var SPos:Integer;
    Float:Double;
    TryDate:TDateTime;
begin
 if not FEditing then Exit;
 case FEditData.EditMode of
  teText, teDate, teMask, teTime, teInt, teFloat:
   begin
    if not FFieldEdit.Modified then
     begin
      DoEditCancel;
      Exit;
     end;
    if not FFieldEdit.Validate(FFieldEdit.EditText, SPos) then
     begin
      FlashControl(FFieldEdit);
      Exit;
     end;
   end;
  teList: 
   begin
    if FEditData.FixedList then 
     if FFieldCombo.ItemIndex < 0 then
      begin                                
       DoEditCancel;
       Exit;    
      end;
   end;              
 end;
 if FEditData.EditMode = teDate then
  begin
   if not TryStrToDate(FFieldEdit.Text, TryDate) then
    begin
     FlashControl(FFieldEdit);
     Exit;
    end;
  end;
 if FEditData.EditMode = teTime then
  begin
   if not TryStrToTime(FFieldEdit.Text, TryDate) then
    begin
     FlashControl(FFieldEdit);
     Exit;
    end;
  end;
 if FEditData.EditMode = teInt then
  begin
   if not TryStrToInt(FFieldEdit.Text, SPos) then
    begin
     FlashControl(FFieldEdit);
     Exit;
    end;
  end;
 if FEditData.EditMode = teFloat then
  begin
   if not TryStrToFloat(FFieldEdit.Text, Float) then
    begin
     FlashControl(FFieldEdit);
     Exit;
    end;
  end;
 SetEditing(False);
 SetFocus;
 case FEditData.EditMode of
  teText, teDate, teMask, teTime, teInt, teFloat: if Assigned(FOnEditOk) then FOnEditOk(Self, FFieldEdit.Text, -1, FEditingCell.X, FEditingCell.Y);
  teList: if Assigned(FOnEditOk) then FOnEditOk(Self, FFieldCombo.Text, FFieldCombo.ItemIndex, FEditingCell.X, FEditingCell.Y);
 end;
 HideEditField;
end;

procedure TTableEx.DoItemClick;
begin
 if Assigned(FOnItemClick) then FOnItemClick(Self, mbLeft, FItemIndex);
end;

function TTableEx.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 if FEditing then Exit(False);
 Result:=True;
 Perform(WM_VSCROLL, SB_LINEDOWN, 0);
 Repaint;
end;

function TTableEx.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
 if FEditing then Exit(False);
 Result:=True;
 Perform(WM_VSCROLL, SB_LINEUP, 0);
 Repaint;
end;

function TTableEx.Edit(AItem, ACol: Integer): Boolean;
begin
 Result:=False;
 if not IndexInList(ACol, ColCount) then Exit;
 if not IndexInList(AItem, ItemCount) then Exit;

 ItemIndex:=AItem;
 Col:=ACol;
 DoEditOk;
 FOnDblClick;
 Result:=True;
end;

procedure TTableEx.FDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var BColor:TColor;
    TextValue:string;
    DataARow:Integer;

procedure DrawText(Value:string; Format:TTextFormat; Offset:Integer = 1);
var RText:TRect;
    Flags:TDrawTextFlags;
begin
 with Canvas do
  begin
   RText:=Rect;
   RText.Left:=RText.Left + 2 + Offset;
   RText.Top:=RText.Top + 2;
   RText.Right:=RText.Right - 2;
   RText.Bottom:=RText.Bottom - 2;
   Flags:=TTextFormatFlags(Format);
   DrawTextCentered(Canvas, RText, Value, Flags);
  end;
end;

procedure FillCell(ARect:TRect);
begin
 if RoundLineRect <= 0 then Canvas.Rectangle(ARect)
 else
  begin
   with TDirect2DCanvas.Create(Canvas, ClientRect) do
    begin
     BeginDraw;
     ARect.Inflate(0, -2);
     if ColumnCount > 1 then
      begin
       if ACol = 0 then
        begin
         RoundRect(ARect, FRoundLineRect, FRoundLineRect);
         Rectangle(System.Classes.Rect(ARect.Left + ARect.Width div 2, ARect.Top, ARect.Right, ARect.Bottom));
        end
       else
        if ACol = ColumnCount-1 then
         begin
          RoundRect(ARect, FRoundLineRect, FRoundLineRect);
          ARect.Width:=ARect.Width - ARect.Width div 2;
          Rectangle(ARect);
         end
        else
          begin
           Rectangle(ARect);
          end;
      end
     else
      begin
       RoundRect(ARect, FRoundLineRect, FRoundLineRect);
      end;
     EndDraw;
     Free;
    end;
  end;
end;

begin
 if (csDestroying in ComponentState) then Exit;
 with Canvas do
  try
   Lock;
   State:=[];
   if FShowColumns then DataARow:=ARow - 1 else DataARow:=ARow;

   if Editing then
    if (FEditingCell.X = ACol) and (FEditingCell.Y = DataARow) and FFieldEdit.Visible then
     begin
      Brush.Color:=FFieldEdit.Color;
      Pen.Color:=FLineSelColor;
      Rectangle(Rect);
      if FFieldEdit.Visible then
       FFieldEdit.PaintTo(Canvas.Handle, FFieldEdit.Left+FFieldEdit.BevelWidth, FFieldEdit.Top+FFieldEdit.BevelWidth);
     // if FFieldCombo.Visible then
      // FFieldCombo.PaintTo(Canvas.Handle, FFieldCombo.Left, FFieldCombo.Top);
      Unlock;
      Exit;
     end;
   TextValue:='?';
   Brush.Style:=bsSolid;
   Pen.Style:=psSolid;
   Brush.Color:=Color;
   Pen.Color:=Brush.Color;
   Pen.Width:=1;
   Rectangle(Rect);
                      //$00F1F2F2
   Brush.Color:=clBlack;
   {if FColumns.Count <= 0 then
    begin
     Unlock;
     Exit;
    end;   }
   if ARow mod 2 = 0 then Brush.Color:=FLineColorXor else Brush.Color:=FLineColor;

   //���� ������� ������ � ��� �� ���������
   if (ARow = DataRow) and (not ((ARow = 0) and FShowColumns)) then
    begin
     Font.Assign(FFontSelLine);
     Brush.Color:=FLineSelColor;
     Pen.Color:=Brush.Color;
     FillCell(Rect);
     Include(State, gdSelected);
    end
   else
    begin
     Font.Assign(FFontLine);
     if FFlashSelectedCol and (ACol = FCordHot.X) and (FCordHot.Y <> ARow) then Brush.Color:=ColorDarker(Brush.Color, 2);
     Pen.Color:=Brush.Color;
     Rectangle(Rect);
    end;

   //���� ������� ��� ��������
   if (FCordHot.Y = ARow) and (ARow <> DataRow) then
    begin
     Include(State, gdHotTrack);
     //���� ������ ������ "����"
     if ItemDowned then
      begin
       Font.Assign(FFontHotLine);
       Font.Color:=FFontSelLine.Color;
       BColor:=ColorDarker(FLineHotColor, 10);
       Include(State, gdPressed);
      end
     else
      begin
       BColor:=FLineHotColor;
       Font.Assign(FFontHotLine);
      end;
     Brush.Color:={MixColors(}BColor;//, Canvas.Brush.Color, 150);
     Pen.Color:=Brush.Color;

     Brush.Style:=bsSolid;
     Pen.Style:=psSolid;
     FillCell(Rect);
    end;

   //���� ��� ���������
   if FShowColumns and (ARow = 0) then
    begin
     Brush.Color:=FColumnsColor;
     Brush.Style:=bsSolid;
     Pen.Style:=psSolid;
     if Assigned(FOnColumnClick) then
      begin
       if (FCordHot.X = ACol) and (FCordHot.Y = ARow) then
        begin
         //if ItemDowned then Brush.Color:=clBtnHighlight else Brush.Color:=clBtnFace;
         if ItemDowned then Brush.Color:=ColorDarker(Brush.Color) else Brush.Color:=ColorDarker(Brush.Color, 20);
        end;
      end;
     if FFlashSelectedCol and (ACol = FCordHot.X) and (FCordHot.Y <> ARow) then Brush.Color:=ColorDarker(Brush.Color, 5);
     Pen.Color:=Brush.Color;

     Rectangle(Rect);
     if FDrawColumnBorded then
      begin
       Pen.Color:=ColorDarker(FColumnsColor, 40);
       MoveTo(Rect.Left, Rect.Bottom-3);
       LineTo(Rect.Right, Rect.Bottom-3);

       Pen.Color:=ColorDarker(FColumnsColor, 30);
       MoveTo(Rect.Left, Rect.Bottom-2);
       LineTo(Rect.Right, Rect.Bottom-2);

       Pen.Color:=ColorDarker(FColumnsColor, 20);
       MoveTo(Rect.Left, Rect.Bottom-1);
       LineTo(Rect.Right, Rect.Bottom-1);
      end;
     if FDrawColumnSections and (ACol <> 0) then
      begin
       Pen.Color:=ColorDarker(FColumnsColor, 10);
       MoveTo(Rect.Left, Rect.Top+1);
       LineTo(Rect.Left, Rect.Bottom-1);
      end;
     if csDesigning in ComponentState then
      if FDrawColumnSections then
       begin
        Pen.Color:=ColorDarker(FColumnsColor, 10);
        MoveTo(Rect.Left + Rect.Width div 2, Rect.Top+1);
        LineTo(Rect.Left + Rect.Width div 2, Rect.Bottom-1);
       end;
    end;
   if FDefDrawing then
    begin
     //���������
     if (ARow = 0) and (FShowColumns) then
      begin
       Brush.Style:=bsClear;
       if not (csDesigning in ComponentState) then
        TextValue:=Columns[ACol].Caption
       else TextValue:='���������';
       Font.Assign(FColumnsFont);
       if not (csDesigning in ComponentState) then
        DrawText(TextValue, Columns[ACol].FormatColumns)
       else DrawText(TextValue, [tfVerticalCenter, tfSingleLine]);
       if Assigned(FOnDrawColumn) then FOnDrawColumn(Sender, ACol, -1, Rect, State);
       //TextOut(Rect.Left, Rect.Top, IntToStr(ARow)+' '+IntToStr(RowCount));
       Unlock;
       Exit;
      end;
     if (ItemCount > 0) or ProcEmpty then
      begin
       if Assigned(FAfterDrawText) then FAfterDrawText(Sender, ACol, DataARow, Rect, State);
       if not (csDesigning in ComponentState) then
        begin
         if Columns[ACol].AsButton then
          begin
           if not (Columns[ACol].ShowButtonOnlySelect and (ARow <> DataRow)) then
            begin
             if Assigned(FGetDataProc) then FGetDataProc(ACol, DataARow, TextValue);
             if (FCordHot.X = ACol) and (FCordHot.Y = ARow) then
              begin
               //if ItemDowned then Brush.Color:=clBtnHighlight else Brush.Color:=clBtnFace;
               if ItemDowned then Brush.Color:=ColorDarker(Brush.Color) else Brush.Color:=ColorDarker(Brush.Color, 20);
              end;
             Brush.Style:=bsSolid;
             Pen.Color:=Brush.Color;
             FillCell(Rect);
             if TextValue <> '' then
              begin
               Brush.Style:=bsClear;
               DrawText(TextValue, Columns[ACol].Format);
              end;
            end;
          end
         else
          begin
           Brush.Style:=bsClear;
           if Assigned(FGetDataProc) then FGetDataProc(ACol, DataARow, TextValue);
           if TextValue <> '' then
            begin
             DrawText(TextValue, Columns[ACol].Format);
            end;
          end;
        end
       else
        begin
         Brush.Style:=bsClear;
         DrawText('��������', [tfVerticalCenter, tfSingleLine]);
        end;
      end;
     //Inc(FUpdatesCount);
    end;
   if not (csDesigning in ComponentState) then
    begin
     if (ItemCount > 0) or (ProcEmpty) then
      begin
       if Assigned(FOnDrawCell) then FOnDrawCell(Sender, ACol, DataARow, Rect, State);
       if FShowFocus then
        if (ACol = Col) and (ARow = DataRow) and Focused and (not Columns[ACol].AsButton) then
         begin
          DrawFocusRect(Rect);
         end;
      end;
    end;
  { if (ACol = 0) and (ARow = 1) then
    TextOut(Rect.Left, Rect.Top, IntToStr(FUpdatesCount));}
  finally
   Unlock;
  end;
end;

procedure TTableEx.FListKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
  VK_LEFT:
   begin
    if FFieldCombo.SelStart = 0 then
     begin
      if Col > 0 then
       begin
        DoEditOk;
        Col:=Col-1;
        FOnDblClick;
        Key:=0;
       end;
     end;
   end;
  VK_RIGHT:
   begin
    if FFieldCombo.SelStart = Length(FFieldCombo.Text) then
     begin
      if Col < (ColCount-1) then
       begin
        DoEditOk;
        Col:=Col+1;
        FOnDblClick;
        Key:=0;
       end;
     end;
   end;
 { VK_UP:
   begin
    if ItemIndex > 0 then
     begin
      DoEditOk;
      ItemIndex:=ItemIndex-1;
      FOnDblClick;
      Key:=0;
     end;
   end;
  VK_DOWN:
   begin
    if ItemIndex < (ItemCount-1) then
     begin
      DoEditOk;
      ItemIndex:=ItemIndex+1;
      FOnDblClick;
      Key:=0;
     end;
   end;  }
 end;
end;

procedure TTableEx.FFieldKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 case Key of
  VK_LEFT:
   begin
    if FFieldEdit.SelStart = 0 then
     begin
      if Col > 0 then
       begin
        DoEditOk;
        Col:=Col-1;
        FOnDblClick;
        Key:=0;
       end;
     end;
   end;
  VK_RIGHT:
   begin
    if FFieldEdit.SelStart = Length(FFieldEdit.Text) then
     begin
      if Col < (ColCount-1) then
       begin
        DoEditOk;
        Col:=Col+1;
        FOnDblClick;
        Key:=0;
       end;
     end;
   end;
  VK_UP:
   begin
    if ItemIndex > 0 then
     begin
      DoEditOk;
      ItemIndex:=ItemIndex-1;
      FOnDblClick;
      Key:=0;
     end;
   end;
  VK_DOWN:
   begin
    if ItemIndex < (ItemCount-1) then
     begin
      DoEditOk;
      ItemIndex:=ItemIndex+1;
      FOnDblClick;
      Key:=0;
     end;
   end;
 end;
end;

function TTableEx.FirstColumn(aCaption: string; aWidth: Integer; aButton: Boolean):Integer;
begin
 Result:=AddColumn(aCaption, aWidth, aButton);
end;

procedure TTableEx.FKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 Repaint;
 case Key of
  VK_DOWN:
   begin
    if ItemIndex+1 <= RowCount-1-(Ord(FShowColumns)) then ItemIndex:=ItemIndex+1;
    ItemDowned:=True;
   end;
  VK_UP:
   begin
    if ItemIndex-1 >= 0 then ItemIndex:=ItemIndex-1;
    ItemDowned:=True;
   end;
 end;
 if Assigned(FOnKeyDown) then FOnKeyDown(Sender, Key, Shift);
end;

procedure TTableEx.FKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 Repaint;
 if ItemDowned then
  begin
   ItemDowned:=False;
   case Key of
    VK_DOWN:
     begin
      if Assigned(FOnItemClick) then FOnItemClick(Sender, mbLeft, FItemIndex);
     end;
    VK_UP:
     begin
      if Assigned(FOnItemClick) then FOnItemClick(Sender, mbLeft, FItemIndex);
     end;
   end;
  end;
 if Assigned(FOnKeyUp) then FOnKeyUp(Sender, Key, Shift);
end;

procedure TTableEx.FMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if ((Button = mbLeft) or (FMouseRightClickTooClick and (Button = mbRight))) and (ssDouble in Shift) then
  begin
   if (FCordHot.X = Col) and (FCordHot.Y = Row) then FOnDblClick;
  end;
 ItemDowned:=True;
 if Assigned(FOnMouseDown) then FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TTableEx.FMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol, ARow, NewRow:Integer;
begin
 if (Button = mbLeft) or (FMouseRightClickTooClick and (Button = mbRight)) then
  begin
   MouseToCell(X, Y, ACol, ARow);
   if ARow >= 0 then
    begin
     if ItemDowned then
      begin
       if FShowColumns then
        begin
         if ARow = 0 then
          begin
           if Assigned(FOnColumnClick) then FOnColumnClick(Sender, Button, ACol);
           ItemDowned:=False;
           if Assigned(FOnMouseUp) then FOnMouseUp(Sender, Button, Shift, X, Y);
           Exit;
          end;
         NewRow:=ARow - 1;
        end
       else
        begin
         NewRow:=ARow;
        end;
       if ItemIndex = NewRow then
        begin
         if Assigned(FOnItemColClick) then FOnItemColClick(Sender, Button, ACol);
        end
       else
        begin
         ItemIndex:=NewRow;
        end;
       DoItemClick;
      end;
    end
   else
    begin
     if FCanNoSelect then ItemIndex:=-1;
    end;
   ItemDowned:=False;
  end;
 if Assigned(FOnMouseUp) then FOnMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TTableEx.FonComboMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 Handled:=FEditing;
end;

procedure TTableEx.FUpdateColumnsHeight;
begin
 if FShowColumns then
  RowHeights[0]:=FColumnsHeight
 else RowHeights[0]:=DefaultRowHeight;
end;

procedure TTableEx.FMouseLeave(Sender: TObject);
begin
 FCordHot.X:=-1;
 FCordHot.Y:=-1;
 ItemDowned:=False;
end;

procedure TTableEx.FMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if FSetFocusOnEnter then if not Focused then SetFocus;
 UpdateMouse(MouseCoord(X, Y));
 if Assigned(FOnMouseMove) then FOnMouseMove(Sender, Shift, X, Y);
end;

procedure TTableEx.UpdateMouse; 
var Last:TGridCoord;
begin
 if not MouseInClient then Exit;
 
 Last:=FCordHot;
 FCordHot:=Cord;
 if not Force then if (FCordHot.Y = Last.Y) and (FCordHot.X = Last.X) then Exit;
 if IndexInList(FCordHot.Y, RowCount) then
  begin
   if FShowColumns and (FCordHot.Y = 0) then
    begin
     if Assigned(FOnColumnClick) then Cursor:=crHandPoint else Cursor:=crDefault;
    end
   else Cursor:=crHandPoint;
  end
 else Cursor:=crDefault;
 Repaint;
end;

function TTableEx.GetColumnCount: Integer;
begin
 Result:=ColCount;
end;

function TTableEx.GetColumnsHeight: Integer;
begin
 Result:=FColumnsHeight;
end;

function TTableEx.GetDefRowH: Integer;
begin
 Result:=inherited DefaultRowHeight;
end;

function TTableEx.GetFocusedColumn: Integer;
begin
 Result:=Col;
end;

function TTableEx.GetShowScrollBar:Boolean;
begin
 Result:=ScrollBars <> ssNone;
end;

function TTableEx.GetWidth: Integer;
begin
 Result:=inherited Width;
end;

procedure TTableEx.HideEditField;
begin
 if FFieldEdit.Visible then
  begin
   FFieldEdit.EditMask:='';
   FFieldEdit.Hide;
  end;
 if FFieldCombo.Visible then FFieldCombo.Hide;
end;

procedure TTableEx.KeyPressControl(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
  begin
   Key:=#0;
   DoEditOk;
  end;
 if Key = #27 then
  begin
   Key:=#0;
   DoEditCancel;
  end;
end;

procedure TTableEx.LastFocus(var Msg: TMessage);
begin
 if Msg.wParam = WA_INACTIVE then
  begin
   if Assigned(FOnDeactivate) then FOnDeactivate(Self)
  end
 else
  begin
   if Assigned(FOnActivate) then FOnActivate(Self)
  end;
 inherited;
end;

procedure TTableEx.MouseToItem(Position: TPoint; var Index, Column: Integer);
begin
 MouseToCell(Position.X, Position.Y, Column, Index);
 if FShowColumns then Index:=Index-1;
end;

function TTableEx.GetItemUnderMouse:Integer;
begin
 Result:=FCordHot.Y - Ord(FShowColumns);
end;

procedure TTableEx.FOnDblClick;
var VAllow:Boolean;
    EData:TTableEditStruct;
begin
 if ItemCount <= 0 then Exit;
 if not Assigned(FOnEdit) then Exit;
 FEditingCell.X:=Col;
 FEditingCell.Y:=Row - Ord(FShowColumns);
 FEditCellRect:=CellRect(Col, Row);
 VAllow:=False;
 EData.EditMode:=teText;
 EData.TextValue:='';
 EData.ItemValue:=-1;
 EData.EditMask:='';
 EData.FixedList:=False;
 EData.ReadOnly:=False;
 EData.ListDrop:=True;
 FOnEdit(Self, EData, FEditingCell.X, FEditingCell.Y, VAllow);
 if not VAllow then Exit;
 FEditData:=EData;
 SetEditing(True);
 FFieldEdit.NumbersOnly:=False;
 case EData.EditMode of
  teText: FFieldEdit.EditMask:='';
  teDate: FFieldEdit.EditMask:='90\.90\.9000;1; ';
  teTime: FFieldEdit.EditMask:='!90:00;1; ';
  teMask: FFieldEdit.EditMask:=EData.EditMask;
  teInt:
   begin
    FFieldEdit.EditMask:='';
    FFieldEdit.NumbersOnly:=True;
   end;
  teFloat:
   begin
    FFieldEdit.EditMask:='';
    FFieldEdit.NumbersOnly:=False;
   end;
 end;
 case EData.EditMode of
  teText, teDate, teMask, teTime, teInt, teFloat:
   begin
    FFieldEdit.Font:=FontLine;
    FFieldEdit.ReadOnly:=EData.ReadOnly;
    if FVisibleEdit then
     begin
      FFieldEdit.Font.Color:=clBlack;
      FFieldEdit.Color:=clWhite;
     end
    else
     begin
      FFieldEdit.Font.Color:=clWhite;
      FFieldEdit.Color:=LineSelColor;
     end;
    FFieldEdit.Parent:=Self;
    FFieldEdit.ParentColor:=False;
    FFieldEdit.ParentBackground:=False;
    FFieldEdit.Left:=FEditCellRect.Left+1;
    FFieldEdit.Top:=FEditCellRect.Top+1;
    FFieldEdit.Width:=FEditCellRect.Width-2;
    FFieldEdit.Height:=FEditCellRect.Height-2;
    FFieldEdit.BevelWidth:=Round((FFieldEdit.Height - (Abs(FFieldEdit.Font.Height)+5)) / 2);
    FFieldEdit.Text:=EData.TextValue;
    FFieldEdit.Show;
    FFieldEdit.DoubleBuffered:=True;
    FFieldEdit.BringToFront;
    FFieldEdit.SetFocus;
   end;
  teList:
   begin
    if EData.FixedList then FFieldCombo.Style:=csDropDownList else FFieldCombo.Style:=csDropDown;
    FFieldCombo.Items.Clear;
    if Assigned(EData.Items) then FFieldCombo.Items.AddStrings(EData.Items);
    FFieldCombo.Font.Name:=FontLine.Name;
    FFieldCombo.Font.Size:=FontLine.Size;
    FFieldCombo.Parent:=Self;
    FFieldCombo.Left:=FEditCellRect.Left+1;
    FFieldCombo.Width:=FEditCellRect.Width-2;
    FFieldCombo.Height:=FEditCellRect.Height-2;
    FFieldCombo.Top:=FEditCellRect.Top+((FEditCellRect.Height div 2) - FFieldCombo.Height div 2);
    FFieldCombo.ItemIndex:=EData.ItemValue;
    if not EData.FixedList then FFieldCombo.Text:=EData.TextValue;
    FFieldCombo.Show;
    FFieldCombo.SetFocus;
    FFieldCombo.AutoDropDown:=False;
    FFieldCombo.DroppedDown:=EData.ListDrop;
   end;
 end;
 Repaint;
end;

procedure TTableEx.FOnEditChange(Sender: TObject);
begin
 //
end;

procedure TTableEx.Repaint;
begin
 if not Assigned(Parent) then Exit; 
 inherited;
end;

{ TTableColumn }

constructor TTableColumn.Create(AOwner: TTableEx);
begin
 FOwner:=AOwner;
 FShowButtonOnlySelect:=False;
 FAsButton:=False;
 Format:=[tfVerticalCenter, tfLeft];
 FormatColumns:=[tfVerticalCenter, tfCenter];
 if not Assigned(FOwner) then Exit;
end;

function TTableColumn.GetIndex:Cardinal;
begin
 Result:=FIndex;
end;

function TTableColumn.GetWidth:Cardinal;
begin
 Result:=FOwner.ColWidths[FIndex];
end;

procedure TTableColumn.SetCaption(const Value:string);
begin
 FCaption:=Value;
 FOwner.Repaint;
end;

procedure TTableColumn.SetFormatColumns(const Value: TTextFormat);
begin
 FFormatColumns:= Value;
 FOwner.Repaint;
end;

procedure TTableColumn.SetIndex(const Value:Cardinal);
begin
 FIndex:=Value;
end;

procedure TTableColumn.SetTextFormat(const Value: TTextFormat);
begin
 FTextFormat:=Value;
 FOwner.Repaint;
end;

procedure TTableColumn.SetWidth(const Value:Cardinal);
begin
 FOwner.ColWidths[Index]:=Value;
end;

{ TFieldMaskEdit }

function TFieldMaskEdit.Validate(const Value: string;
  var Pos: Integer): Boolean;
begin
 Result:=inherited;
end;

procedure TFieldMaskEdit.ValidateError;
begin
 //inherited;

end;

end.
