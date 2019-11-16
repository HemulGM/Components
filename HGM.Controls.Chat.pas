unit HGM.Controls.Chat;

interface

uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, System.Classes,
  System.Contnrs, System.Types, System.UITypes, Vcl.Controls, Vcl.Forms,
  Vcl.Menus, Vcl.Graphics, Vcl.StdCtrls, Vcl.GraphUtil, Vcl.ImgList, Vcl.Themes,
  Winapi.ShellAPI, System.Generics.Collections, HGM.Common, Vcl.Dialogs;

type
  TChatMessageType = (mtOpponent, mtMe);

  TChatItems = class;

  ThCustomChat = class;

  TChatItem = class
  private
    FOwner: TChatItems;
    FNeedCalc: Boolean;
    FCalcedRect: TRect;
    FSelected: Boolean;
    procedure SetOwner(const Value: TChatItems);
    procedure SetSelected(const Value: Boolean);
  public
    Text: string;
    Color: TColor;
    function DrawRect(Canvas: TCanvas; Rect: TRect): TRect; virtual;
    function CalcRect(Canvas: TCanvas; Rect: TRect): TRect; virtual;
    constructor Create(AOwner: TChatItems); virtual;
    destructor Destroy; override;
    property Owner: TChatItems read FOwner write SetOwner;
    property Selected: Boolean read FSelected write SetSelected;
  end;

  TChatMessage = class(TChatItem)
    From: string;
    FromType: TChatMessageType;
    FromColor: TColor;
    FCalcedFromHeight: Integer;
    function CalcRect(Canvas: TCanvas; Rect: TRect): TRect; override;
    function DrawRect(Canvas: TCanvas; Rect: TRect): TRect; override;
    constructor Create(AOwner: TChatItems); override;
    destructor Destroy; override;
  end;

  TChatInfo = class(TChatItem)
  end;

  TChatItems = class(TList<TChatItem>)
  private
    FOwner: ThCustomChat;
    procedure SetOwner(const Value: ThCustomChat);
  published
    function AddMessage: TChatMessage; overload;
    function AddInfo: TChatInfo; overload;
    procedure Delete(Index: Integer);
    procedure Clear;
    destructor Destroy; override;
    constructor Create(AOwner: ThCustomChat);
    procedure DoChanged(Item: TChatItem);
    property Owner: ThCustomChat read FOwner write SetOwner;
  end;

  ThCustomChat = class(TCustomControl)
  private
    FParentBackgroundSet: Boolean;
    FOffset: Integer;
    FMaxOffset: Integer;
    FItems: TChatItems;
    FMousePos: TPoint;
    FItemUnderMouse: Integer;
    FScrollBarVisible: Boolean;
    FMouseIn: Boolean;
    FMouseInScroll: Boolean;
    FScrollPosStart: TPoint;
    FScrollLentgh: Integer;
    FScrollPos: Integer;
    FScrolling: Boolean;
    FPaintCounter: Integer;
    function GetItem(Index: Integer): TChatItem;
    procedure SetItem(Index: Integer; const Value: TChatItem);
    procedure SetItems(const Value: TChatItems);
    procedure FOnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FOnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FOnMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FOnClick(Sender: TObject);
    procedure FOnMouseEnter(Sender: TObject);
    procedure FOnMouseLeave(Sender: TObject);
    procedure FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetScrollBarVisible(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure UpdateStyleElements; override;
    property Color default clBtnFace;
    property ParentColor default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Item[Index: Integer]: TChatItem read GetItem write SetItem;
    property Items: TChatItems read FItems write SetItems;
    property DoubleBuffered default True;
    property ScrollBarVisible: Boolean read FScrollBarVisible write SetScrollBarVisible default True;
  end;

  ThChat = class(ThCustomChat)
  public
    property Item;
    property Items;
  published
    property Align;
    property Color default $0021160E;
    property ScrollBarVisible;
    property DoubleBuffered;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  RegisterComponents(PackageName, [ThChat]);
end;

{ ThCustomChat }

constructor ThCustomChat.Create(AOwner: TComponent);
var
  i, j: Integer;
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks, csPannable, csGestures];
  Width := 200;
  Height := 400;
  Color := $0020160F;
  UseDockManager := True;
  ParentBackground := False;
  TabStop := True;

  OnMouseWheelDown := FOnMouseWheelDown;
  OnMouseWheelUp := FOnMouseWheelUp;
  OnMouseMove := FOnMouseMoveEvent;
  OnClick := FOnClick;
  OnMouseDown := FOnMouseDown;
  OnMouseUp := FOnMouseUp;
  OnMouseEnter := FOnMouseEnter;
  OnMouseLeave := FOnMouseLeave;

  FItemUnderMouse := -1;
  FPaintCounter := 0;
  FMouseIn := False;
  FMouseInScroll := False;
  FScrollBarVisible := True;
  FItems := TChatItems.Create(Self);

  if csDesigning in ComponentState then
  begin
    for i := 0 to 40 do
    begin
      with Items.AddMessage do
      begin
        From := 'UserName';
        if Random(40) in [20..30] then
          FromType := mtMe;
        Text := 'Text body';
        for j := 1 to Random(10) do
          Text := Text + 'Text body';
        Text := DateTimeToStr(Now) + #13#10 + Text;
      end;
      if i in [25..30] then
        with Items.AddInfo do
        begin
          Text := DateTimeToStr(Now);
        end;
    end;
  end;
end;

procedure ThCustomChat.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

destructor ThCustomChat.Destroy;
begin
  FItems.Clear;
  FItems.Free;
  inherited;
end;

procedure ThCustomChat.FOnClick(Sender: TObject);
begin
  if FItemUnderMouse >= 0 then
  begin
    ShowMessage(FItems[FItemUnderMouse].Text);
  end;
end;

procedure ThCustomChat.FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FScrollBarVisible and FMouseInScroll then
    begin
      FScrolling := True;
      FScrollPosStart := TPoint.Create(X, Y);
    end;
  end;
end;

procedure ThCustomChat.FOnMouseEnter(Sender: TObject);
begin
  FMouseIn := True;
  Repaint;
end;

procedure ThCustomChat.FOnMouseLeave(Sender: TObject);
begin
  FMouseIn := False;
  Repaint;
end;

procedure ThCustomChat.FOnMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  FMousePos := TPoint.Create(X, Y);
  if FScrolling then
  begin
    FScrollPos := FScrollPos + (FScrollPosStart.Y - FMousePos.Y);
    FOffset := Round((FScrollPos / (FScrollLentgh / 100)) / (100 / FMaxOffset));
    FOffset := Max(0, Min(FMaxOffset, FOffset));
    FScrollPosStart := FMousePos;
  end;
  Repaint;
end;

procedure ThCustomChat.FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FScrollBarVisible then
      FScrolling := False;
  end;
end;

procedure ThCustomChat.FOnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FOffset := Max(0, FOffset - 50);
  Repaint;
end;

procedure ThCustomChat.FOnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FOffset := Min(FMaxOffset, FOffset + 50);
  Repaint;
end;

function ThCustomChat.GetItem(Index: Integer): TChatItem;
begin
  Result := FItems[Index];
end;

procedure ThCustomChat.Paint;
const
  BorderWidth = 20;
  PaddingSize = 10;
var
  Rect, ItemRect, LastRect, R, TxtRect, Limit: TRect;
  S: string;
  BaseColor: TColor;
  i, Radius: Integer;
  FStartDraw, FSkip: Boolean;

  function NeedDraw(Item: TRect): Boolean;
  begin
    if Item.Bottom < Rect.Top then
      Exit(False);
    if Item.Top > Rect.Bottom then
      Exit(False);
    Result := True;
  end;

begin
  Inc(FPaintCounter);
  Rect := ClientRect;
  Rect.Inflate(-BorderWidth, -BorderWidth);
  Rect.Right := Rect.Right - 25; //scroll  
  BaseColor := Color;
  FStartDraw := False;
  FSkip := False;
  with Canvas do
  begin
    Brush.Color := BaseColor;
    FillRect(ClientRect);

    LastRect := TRect.Create(Rect.Left, Rect.Bottom, Rect.Right, Rect.Bottom);
    FItemUnderMouse := -1;
    if FItems.Count > 0 then
      for i := FItems.Count - 1 downto 0 do
      begin
        Limit := Rect;
        Limit.Width := Min(500, Limit.Width - PaddingSize);
        TxtRect := FItems[i].CalcRect(Canvas, Limit);
        Brush.Color := $00322519;

        ItemRect := TxtRect;
        ItemRect.Inflate(PaddingSize, PaddingSize);
        ItemRect.Location := TPoint.Create(LastRect.Left, LastRect.Top - ItemRect.Height);
        LastRect := ItemRect;
        LastRect.Offset(0, -10);
        if FSkip then
          Continue;

        if (FItems[i] is TChatMessage) then
        begin
          if (FItems[i] as TChatMessage).FromType = mtMe then
          begin
            if ClientRect.Width <= 1000 then
              ItemRect.Offset(Rect.Right - (ItemRect.Width + BorderWidth{ + 25}), 0); //25 scroll
            Brush.Color := $0077512C;
          end;
          Radius := 14;
        end
        else if (FItems[i] is TChatInfo) then
        begin
          if ClientRect.Width <= 1000 then
            ItemRect.Offset(Rect.CenterPoint.X - (ItemRect.Width div 2 + BorderWidth{ + 25}), 0) //25 scroll
          else
            ItemRect.Offset(500 div 2 - (ItemRect.Width div 2 + BorderWidth{ + 25}), 0);
          Brush.Color := $003A2C1D;
          Radius := ItemRect.Height;
        end;
        ItemRect.Offset(PaddingSize, FOffset);

        if NeedDraw(ItemRect) then
        begin
          if ItemRect.Contains(FMousePos) then
          begin
            FItemUnderMouse := i;
          end
          else
          begin
          //Brush.Color := clRed;
          end;
          FStartDraw := True;
          Brush.Style := bsSolid;
          Pen.Color := Brush.Color;
          RoundRect(ItemRect, Radius, Radius);
          Brush.Style := bsClear;
          TxtRect.Location := TPoint.Create(ItemRect.Left + PaddingSize, ItemRect.Top + PaddingSize);
          FItems[i].DrawRect(Canvas, TxtRect);
        end
        else if FStartDraw then
          FSkip := True;
      end;
    FMaxOffset := 0 - LastRect.Top;
    if FScrollBarVisible then
    begin
      FMouseInScroll := False;
      if FMouseIn then
      begin
        Rect := ClientRect;
        ItemRect := Rect;
        ItemRect.Left := ItemRect.Right - 10;
        ItemRect.Right := ItemRect.Right - 4;
        ItemRect.Inflate(0, -4);
        if ItemRect.Contains(FMousePos) then
          Brush.Color := $003C342E
        else
          Brush.Color := $00332A24;
        Pen.Color := Brush.Color;
        RoundRect(ItemRect, 6, 6);

        LastRect := ItemRect;
        LastRect.Height := 40;
        FScrollLentgh := ItemRect.Height - LastRect.Height;
        //FOffset
        //FMaxOffset
        FScrollPos := Round((FScrollLentgh / 100) * ((100 / FMaxOffset) * FOffset));

        LastRect.Location := TPoint.Create(LastRect.Left, (ItemRect.Bottom - LastRect.Height) - FScrollPos);

        if LastRect.Contains(FMousePos) then
          FMouseInScroll := True;
        Brush.Color := $00605B56;
        Pen.Color := Brush.Color;
        RoundRect(LastRect, 6, 6);
      end;
    end;             {
    TextOut(0, 0, FOffset.ToString);
    TextOut(0, 20, FMaxOffset.ToString);
    TextOut(0, 40, FPaintCounter.ToString);  }
  end;
end;

procedure ThCustomChat.UpdateStyleElements;
begin
  Invalidate;
end;

procedure ThCustomChat.SetItem(Index: Integer; const Value: TChatItem);
begin
  FItems[Index] := Value;
end;

procedure ThCustomChat.SetItems(const Value: TChatItems);
begin
  FItems := Value;
end;

procedure ThCustomChat.SetScrollBarVisible(const Value: Boolean);
begin
  FScrollBarVisible := Value;
end;

{ TChatItems }

function TChatItems.AddInfo: TChatInfo;
begin
  Result := TChatInfo.Create(Self);
  Add(Result);
end;

function TChatItems.AddMessage: TChatMessage;
begin
  Result := TChatMessage.Create(Self);
  Add(Result);
end;

procedure TChatItems.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited;
end;

constructor TChatItems.Create(AOwner: ThCustomChat);
begin
  inherited Create;

end;

procedure TChatItems.Delete(Index: Integer);
begin
  Items[Index].Free;
  inherited;
end;

destructor TChatItems.Destroy;
begin
  Clear;
  inherited;
end;

procedure TChatItems.DoChanged(Item: TChatItem);
begin
  Owner.Repaint;
end;

procedure TChatItems.SetOwner(const Value: ThCustomChat);
begin
  FOwner := Value;
end;

{ TChatItem }

function TChatItem.CalcRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  if FNeedCalc then
  begin
    R := Rect;
    S := Text;
    Canvas.Font.Size := 8;
    Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfWordBreak, tfEndEllipsis]);
    FCalcedRect := R;
    Result := R;
  end
  else
    Result := FCalcedRect;
end;

function TChatItem.DrawRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  R := Rect;
  S := Text;
  Canvas.Font.Size := 8;
  Canvas.Font.Color := Color;
  Canvas.TextRect(R, S, [tfLeft, tfWordBreak, tfEndEllipsis]);
  Result := R;
end;

constructor TChatItem.Create(AOwner: TChatItems);
begin
  inherited Create;
  FCalcedRect := TRect.Empty;
  FNeedCalc := True;
  FOwner := AOwner;
  Color := clWhite;
end;

destructor TChatItem.Destroy;
begin
  inherited;
end;

procedure TChatItem.SetOwner(const Value: TChatItems);
begin
  FOwner := Value;
end;

procedure TChatItem.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
  FOwner.DoChanged(Self);
end;

{ TChatMessage }

function TChatMessage.CalcRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  if FNeedCalc then
  begin
    R := Rect;
    S := Text;
    Canvas.Font.Size := 8;
    Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfWordBreak, tfEndEllipsis]);
    Rect.Width := R.Width;
    Rect.Height := R.Height;

    if FromType = mtOpponent then
    begin
      R := Rect;
      S := From;
      Canvas.Font.Size := 11;
      Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfSingleLine, tfEndEllipsis]);
      Rect.Width := Max(Rect.Width, R.Width);
      Rect.Height := Rect.Height + R.Height;
      FCalcedFromHeight := R.Height;
    end;

    FCalcedRect := Rect;
    Result := Rect;
  end
  else
    Result := FCalcedRect;
end;

function TChatMessage.DrawRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  O: Integer;
  S: string;
begin
  if FromType = mtOpponent then
  begin
    S := From;
    R := Rect;
    Canvas.Font.Size := 11;
    Canvas.Font.Color := FromColor;
    Canvas.TextRect(R, S, [tfLeft, tfSingleLine, tfEndEllipsis]);
  end;

  R := Rect;
  if FromType = mtOpponent then
    R.Offset(0, FCalcedFromHeight);
  S := Text;
  Canvas.Font.Size := 8;
  Canvas.Font.Color := Color;
  Canvas.TextRect(R, S, [tfLeft, tfWordBreak, tfEndEllipsis]);
  Rect.Width := Max(Rect.Width, R.Width);
  Rect.Height := Rect.Height + R.Height;

  Result := Rect;
end;

constructor TChatMessage.Create(AOwner: TChatItems);
begin
  inherited;
  FromColor := $00D4D4D4;
end;

destructor TChatMessage.Destroy;
begin
  inherited;
end;

end.

