unit HGM.Controls.Chat;

interface

uses
  Winapi.Messages, Winapi.Windows, System.SysUtils, System.Classes, System.Contnrs, System.Types,
  System.UITypes, Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Graphics, Vcl.StdCtrls, Vcl.GraphUtil,
  Vcl.ImgList, Vcl.Themes, Winapi.ShellAPI, System.Generics.Collections, HGM.Common, Vcl.Dialogs;

type
  TChatMessageType = (mtOpponent, mtMe);

  TChatItems = class;

  ThCustomChat = class;

  TChatItem = class abstract
  private
    FOwner: TChatItems;
    FNeedCalc: Boolean;
    FCalcedRect: TRect;
    FSelected: Boolean;
    FImageIndex: Integer;
    FCanSelected: Boolean;
    FDate: TDateTime;
    procedure SetOwner(const Value: TChatItems);
    procedure SetSelected(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetDate(const Value: TDateTime);
  public
    Text: string;
    Color: TColor;
    function DrawRect(Canvas: TCanvas; Rect: TRect): TRect; virtual;
    function DrawImage(Canvas: TCanvas; Rect: TRect): TRect; virtual;
    function CalcRect(Canvas: TCanvas; Rect: TRect): TRect; virtual;
    constructor Create(AOwner: TChatItems); virtual;
    destructor Destroy; override;
    property Owner: TChatItems read FOwner write SetOwner;
    property Selected: Boolean read FSelected write SetSelected;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Date: TDateTime read FDate write SetDate;
    property NeedCalc: Boolean read FNeedCalc write FNeedCalc;
    property CalcedRect: TRect read FCalcedRect write FCalcedRect;
  end;

  TChatMessage = class(TChatItem)
  private
    FCalcedFromHeight: Integer;
    FShowFrom: Boolean;
    FFrom: string;
    FFromType: TChatMessageType;
    FFromColor: TColor;
    FFromColorSelect: TColor;
    procedure SetCalcedFromHeight(const Value: Integer);
    procedure SetFrom(const Value: string);
    procedure SetFromColor(const Value: TColor);
    procedure SetFromColorSelect(const Value: TColor);
    procedure SetFromType(const Value: TChatMessageType);
    procedure SetShowFrom(const Value: Boolean); virtual;
    function GetFromType: TChatMessageType;
  public
    constructor Create(AOwner: TChatItems); override;
    destructor Destroy; override;
    function CalcRect(Canvas: TCanvas; Rect: TRect): TRect; override;
    function DrawRect(Canvas: TCanvas; Rect: TRect): TRect; override;
    property ShowFrom: Boolean read FShowFrom write SetShowFrom;
    property From: string read FFrom write SetFrom;
    property FromType: TChatMessageType read GetFromType write SetFromType;
    property FromColor: TColor read FFromColor write SetFromColor;
    property FromColorSelect: TColor read FFromColorSelect write SetFromColorSelect;
    property CalcedFromHeight: Integer read FCalcedFromHeight write SetCalcedFromHeight;
  end;

  TChatInfo = class(TChatItem)
  private
    FFillColor: TColor;
  public
    procedure SetFillColor(const Value: TColor);
    function CalcRect(Canvas: TCanvas; Rect: TRect): TRect; override;
    function DrawRect(Canvas: TCanvas; Rect: TRect): TRect; override;
    constructor Create(AOwner: TChatItems); override;
    property FillColor: TColor read FFillColor write SetFillColor;
  end;

  TChatItems = class(TList<TChatItem>)
  private
    FOwner: ThCustomChat;
    procedure SetOwner(const Value: ThCustomChat);
  public
    function AddMessage: TChatMessage; overload;
    function AddInfo: TChatInfo; overload;
    function Add(Value: TChatItem): Integer; overload;
    function Add<T: TChatItem>(): T; overload;
    function SelectCount: Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure DoChanged(Item: TChatItem);
    procedure NeedResize;
    constructor Create(AOwner: ThCustomChat);
    destructor Destroy; override;
    property Owner: ThCustomChat read FOwner write SetOwner;
  end;

  TOnSelectionEvent = procedure(Sender: TObject; Count: Integer) of object;

  ThCustomChat = class(TCustomControl)
  private
    FOffset: Integer;
    FMaxOffset: Integer;
    FItems: TChatItems;
    FMousePos: TPoint;
    FItemUnderMouse: Integer;
    FPreviousSelectedItem: Integer;
    FSelectionMode: Boolean;
    FScrollBarVisible: Boolean;
    FMouseIn: Boolean;
    FMouseInScroll: Boolean;
    FMouseInScrollButton: Boolean;
    FScrollPosStart: TPoint;
    FScrollLentgh: Integer;
    FScrollPos: Integer;
    FScrolling: Boolean;
    FPaintCounter: Integer;
    FImageList: TImageList;
    FDrawImages: Boolean;
    FDragItem: Boolean;
    FOnSelectionStart: TOnSelectionEvent;
    FOnSelectionEnd: TOnSelectionEvent;
    FOnSelectionChange: TOnSelectionEvent;
    FColorMe: TColor;
    FColorInfo: TColor;
    FColorSelection: TColor;
    FColorOpponent: TColor;
    FColorScrollActive: TColor;
    FColorScrollInactive: TColor;
    FColorScrollButton: TColor;
    FImageMargin: Integer;
    FPaddingSize: Integer;
    FBorderWidth: Integer;
    FRevertAdding: Boolean;
    function GetItem(Index: Integer): TChatItem;
    procedure SetItem(Index: Integer; const Value: TChatItem);
    procedure SetItems(const Value: TChatItems);
    procedure FOnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FOnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FOnMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FOnClick(Sender: TObject);
    procedure FOnResize(Sender: TObject);
    procedure FOnMouseEnter(Sender: TObject);
    procedure FOnMouseLeave(Sender: TObject);
    procedure FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetScrollBarVisible(const Value: Boolean);
    procedure CheckOffset;
    procedure SetImageList(const Value: TImageList);
    procedure NeedRepaint;
    procedure SetDrawImages(const Value: Boolean);
    procedure SelectionChange(Count: Integer);
    procedure SetOnSelectionChange(const Value: TOnSelectionEvent);
    procedure SetOnSelectionEnd(const Value: TOnSelectionEvent);
    procedure SetOnSelectionStart(const Value: TOnSelectionEvent);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetColorInfo(const Value: TColor);
    procedure SetColorMe(const Value: TColor);
    procedure SetColorOpponent(const Value: TColor);
    procedure SetColorSelection(const Value: TColor);
    procedure SetColorScrollActive(const Value: TColor);
    procedure SetColorScrollButton(const Value: TColor);
    procedure SetColorScrollInactive(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetImageMargin(const Value: Integer);
    procedure SetPaddingSize(const Value: Integer);
    procedure SetRevertAdding(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure UpdateStyleElements; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Item[Index: Integer]: TChatItem read GetItem write SetItem;
    property Items: TChatItems read FItems write SetItems;
    property DoubleBuffered default True;
    property ScrollBarVisible: Boolean read FScrollBarVisible write SetScrollBarVisible default True;
    property ImageList: TImageList read FImageList write SetImageList;
    property DrawImages: Boolean read FDrawImages write SetDrawImages default False;
    property OnSelectionStart: TOnSelectionEvent read FOnSelectionStart write SetOnSelectionStart;
    property OnSelectionChange: TOnSelectionEvent read FOnSelectionChange write SetOnSelectionChange;
    property OnSelectionEnd: TOnSelectionEvent read FOnSelectionEnd write SetOnSelectionEnd;
    property Color default $0020160F;
    property ColorInfo: TColor read FColorInfo write SetColorInfo default $003A2C1D;
    property ColorOpponent: TColor read FColorOpponent write SetColorOpponent default $00322519;
    property ColorMe: TColor read FColorMe write SetColorMe default $0078522B;
    property ColorSelection: TColor read FColorSelection write SetColorSelection default $00A5702E;
    property ColorScrollInactive: TColor read FColorScrollInactive write SetColorScrollInactive default $00332A24;
    property ColorScrollActive: TColor read FColorScrollActive write SetColorScrollActive default $003C342E;
    property ColorScrollButton: TColor read FColorScrollButton write SetColorScrollButton default $00605B56;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 4;
    property PaddingSize: Integer read FPaddingSize write SetPaddingSize default 10;
    property ImageMargin: Integer read FImageMargin write SetImageMargin default 36;
    property RevertAdding: Boolean read FRevertAdding write SetRevertAdding;
  end;

  ThChat = class(ThCustomChat)
  public
    property Item;
    property Items;
  published
    property Align;
    property Color;
    property ScrollBarVisible;
    property DoubleBuffered;
    property Visible;
    property ImageList;
    property DrawImages;
    property OnSelectionStart;
    property OnSelectionChange;
    property OnSelectionEnd;
    property ColorInfo;
    property ColorOpponent;
    property ColorMe;
    property ColorSelection;
    property ColorScrollInactive;
    property ColorScrollActive;
    property ColorScrollButton;
    property BorderWidth;
    property PaddingSize;
    property ImageMargin;
  end;

procedure Register;

implementation

uses
  Math, HGM.Common.Utils;

procedure Register;
begin
  RegisterComponents(PackageName, [ThChat]);
end;

{ ThCustomChat }

procedure ThCustomChat.CheckOffset;
begin
  FOffset := Max(0, Min(FMaxOffset, FOffset));
end;

constructor ThCustomChat.Create(AOwner: TComponent);
var
  i, j: Integer;
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csOpaque, csClickEvents, csDoubleClicks, csPannable, csGestures];
  Width := 200;
  Height := 400;
  Color := $0020160F;
  UseDockManager := True;
  ParentBackground := False;
  DoubleBuffered := True;
  TabStop := True;

  OnMouseWheelDown := FOnMouseWheelDown;
  OnMouseWheelUp := FOnMouseWheelUp;
  OnMouseMove := FOnMouseMoveEvent;
  OnClick := FOnClick;
  OnMouseDown := FOnMouseDown;
  OnMouseUp := FOnMouseUp;
  OnMouseEnter := FOnMouseEnter;
  OnMouseLeave := FOnMouseLeave;
  OnResize := FOnResize;

  FRevertAdding := False;
  FColorOpponent := $00322519;
  FColorMe := $0078522B;
  FColorInfo := $003A2C1D;
  FColorSelection := $00A5702E;
  FColorScrollActive := $003C342E;
  FColorScrollInactive := $00332A24;
  FColorScrollButton := $00605B56;

  FBorderWidth := 4;
  FPaddingSize := 10;
  FImageMargin := 36;

  FItemUnderMouse := -1;
  FDragItem := False;
  FSelectionMode := False;
  FPaintCounter := 0;
  FMouseIn := False;
  FMouseInScroll := False;
  FMouseInScrollButton := False;
  FScrollBarVisible := True;
  FPreviousSelectedItem := -100;
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
        FromColor := RGB(RandomRange(100, 240), RandomRange(100, 240), RandomRange(100, 240));
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
  if not Focused then
    SetFocus;
  if (not FDragItem) and (not FScrolling) and (FSelectionMode) then
    if FItemUnderMouse >= 0 then
    begin
    //ShowMessage(FItems[FItemUnderMouse].Text);
      FItems[FItemUnderMouse].Selected := not FItems[FItemUnderMouse].Selected;
    end;
end;

procedure ThCustomChat.FOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FScrollBarVisible then
    begin
      if FMouseInScroll and not FMouseInScrollButton then
      begin
        FScrollPos := FScrollLentgh - Y + 20 {20 - половина размер скрола};
        FOffset := Round((FScrollPos / (FScrollLentgh / 100)) / (100 / FMaxOffset));
        CheckOffset;
        FScrolling := True;
        FScrollPosStart := TPoint.Create(X, Y);
        NeedRepaint;
        Exit;
      end;
      if FMouseInScrollButton then
      begin
        FScrolling := True;
        FScrollPosStart := TPoint.Create(X, Y);
        NeedRepaint;
        Exit;
      end;
    end;
  end;
  NeedRepaint;
end;

procedure ThCustomChat.FOnMouseEnter(Sender: TObject);
begin
  FMouseIn := True;
  NeedRepaint;
end;

procedure ThCustomChat.FOnMouseLeave(Sender: TObject);
begin
  FMouseIn := False;
  NeedRepaint;
end;

procedure ThCustomChat.FOnMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  {if not Focused then
    SetFocus;  }

  FMousePos := TPoint.Create(X, Y);
  if FScrolling then
  begin
    FScrollPos := FScrollPos + (FScrollPosStart.Y - FMousePos.Y);
    FOffset := Round((FScrollPos / (FScrollLentgh / 100)) / (100 / FMaxOffset));
    CheckOffset;
    FScrollPosStart := FMousePos;
  end
  else
  begin
    if not FDragItem then
    begin
      if ssLeft in Shift then
        if FItemUnderMouse >= 0 then
        begin
          FDragItem := True;
          FPreviousSelectedItem := -1;
        end;
    end;
    if FDragItem then
    begin
      if (FItemUnderMouse >= 0) and (FItemUnderMouse <> FPreviousSelectedItem) then
      begin
        FPreviousSelectedItem := FItemUnderMouse;
        FItems[FItemUnderMouse].Selected := not FItems[FItemUnderMouse].Selected;
      end
      else
        NeedRepaint;
    end;
  end;
  NeedRepaint;
end;

procedure ThCustomChat.FOnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if FScrollBarVisible then
      FScrolling := False;
    if FDragItem then
    begin
      FDragItem := False;
      FPreviousSelectedItem := -1;
    end;
  end;
end;

procedure ThCustomChat.FOnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FOffset := FOffset - 50;
  CheckOffset;
  NeedRepaint;
end;

procedure ThCustomChat.FOnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FOffset := FOffset + 50;
  CheckOffset;
  NeedRepaint;
end;

procedure ThCustomChat.FOnResize(Sender: TObject);
begin
  //
end;

function ThCustomChat.GetItem(Index: Integer): TChatItem;
begin
  Result := FItems[Index];
end;

procedure ThCustomChat.NeedRepaint;
begin
  Invalidate;
end;

procedure ThCustomChat.Paint;
var
  Rect, ItemRect, LastRect, TxtRect, Limit, ImageRect: TRect;
  BaseColor: TColor;
  i, Radius: Integer;
  FStartDraw, FSkip, FNeedImage: Boolean;
  lpPaint: TPaintStruct;

  function NeedDraw(Item: TRect): Boolean;
  begin
    if Item.Bottom < ClientRect.Top then
      Exit(False);
    if Item.Top > ClientRect.Bottom then
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
  BeginPaint(Handle, lpPaint);
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
        Limit.Inflate(-PaddingSize, -PaddingSize);
        FNeedImage := False;
        if FDrawImages then
          if (FItems[i] is TChatMessage) then
          begin
            if (FItems[i] as TChatMessage).FromType = mtOpponent then
              FNeedImage := True;
            Limit.Width := Limit.Width - FImageMargin;
          end;

        Limit.Width := Min(500, Limit.Width);
        TxtRect := FItems[i].CalcRect(Canvas, Limit);

        Brush.Color := FColorOpponent;

        ItemRect := TxtRect;
        ItemRect.Inflate(PaddingSize, PaddingSize);
        ItemRect.Location := TPoint.Create(Rect.Left, LastRect.Top - ItemRect.Height);
        LastRect := ItemRect;
        if (FItems[i] is TChatInfo) then
        begin
          LastRect.Offset(0, -20);
        end
        else
          LastRect.Offset(0, -10);

        if FSkip then
          Continue;

        Radius := 14;
        if (FItems[i] is TChatMessage) then
        begin
          if (FItems[i] as TChatMessage).FromType = mtMe then
          begin
            if ClientRect.Width <= 1000 then
              ItemRect.Location := TPoint.Create(Rect.Right - ItemRect.Width, ItemRect.Top)
            else if FNeedImage then
              ItemRect.Location := TPoint.Create(ItemRect.Left + FImageMargin, ItemRect.Top);
            Brush.Color := FColorMe;
          end
          else if FNeedImage then
            ItemRect.Location := TPoint.Create(ItemRect.Left + FImageMargin, ItemRect.Top);
        end
        else if (FItems[i] is TChatInfo) then
        begin
          if ClientRect.Width <= 1000 then
            ItemRect.Offset(Rect.CenterPoint.X - (ItemRect.Width div 2 + BorderWidth), 0)
          else
            ItemRect.Offset(500 div 2 - (ItemRect.Width div 2 + BorderWidth), 0);
          if (FItems[i] as TChatInfo).FillColor = clNone then
            Brush.Color := FColorInfo
          else
            Brush.Color := (FItems[i] as TChatInfo).FillColor;
          Radius := ItemRect.Height;
        end;
        ItemRect.Offset(PaddingSize, FOffset);

        if NeedDraw(ItemRect) then
        begin
          Limit := ItemRect;
          Limit.Left := 0;
          Limit.Right := ClientRect.Right;
          if Limit.Contains(FMousePos) then
          begin
            FItemUnderMouse := i;
          end
          else
          begin
            //Brush.Color := clRed;
          end;
          FStartDraw := True;
          Brush.Style := bsSolid;
          if (FItems[i] is TChatMessage) and (FItems[i].Selected) then
            Brush.Color := FColorSelection;
          Pen.Color := ColorDarker(Brush.Color, 5);
          RoundRect(ItemRect, Radius, Radius);
          Brush.Style := bsClear;
          TxtRect.Location := TPoint.Create(ItemRect.Left + PaddingSize, ItemRect.Top + PaddingSize);
          FItems[i].DrawRect(Canvas, TxtRect);

          if FNeedImage then
          begin
            ImageRect := TRect.Create(0, 0, ImageMargin, ImageMargin);
            ImageRect.Location := TPoint.Create(ItemRect.Left - ImageRect.Width - 3, ItemRect.Bottom
              - ImageRect.Height - 3);
            FItems[i].DrawImage(Canvas, ImageRect);
          end;
        end
        else if FStartDraw then
          FSkip := True;
      end;
    FMaxOffset := 0 - LastRect.Top;

    if FScrollBarVisible then
    begin
      FMouseInScroll := False;
      FMouseInScrollButton := False;
      if FMouseIn or (csDesigning in ComponentState) then
      begin
        Rect := ClientRect;
        ItemRect := Rect;
        ItemRect.Left := ItemRect.Right - 10;
        ItemRect.Right := ItemRect.Right - 4;
        ItemRect.Inflate(0, -4);
        if ItemRect.Contains(FMousePos) or FScrolling then
        begin
          FMouseInScroll := True;
          Brush.Color := FColorScrollActive;
        end
        else
          Brush.Color := FColorScrollInactive;
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
          FMouseInScrollButton := True;
        Brush.Color := FColorScrollButton;
        Pen.Color := Brush.Color;
        RoundRect(LastRect, 6, 6);
      end;
    end;                {
    TextOut(0, 0, FOffset.ToString);
    TextOut(0, 20, FMaxOffset.ToString);
    TextOut(0, 40, FPaintCounter.ToString);  }
  end;
  EndPaint(Handle, lpPaint);
end;

procedure ThCustomChat.UpdateStyleElements;
begin
  Invalidate;
end;

procedure ThCustomChat.WMSize(var Message: TWMSize);
begin
  FItems.NeedResize;
  CheckOffset;
  NeedRepaint;
  inherited;
end;

procedure ThCustomChat.SelectionChange(Count: Integer);
begin
  if FSelectionMode and (Count = 0) then
  begin
    FSelectionMode := False;
    FPreviousSelectedItem := -1;
    if Assigned(FOnSelectionEnd) then
      FOnSelectionEnd(Self, 0);
  end
  else if FSelectionMode and (Count > 0) then
  begin
    if Assigned(FOnSelectionChange) then
      FOnSelectionChange(Self, Count);
  end
  else if (not FSelectionMode) and (Count > 0) then
  begin
    FSelectionMode := True;
    if Assigned(FOnSelectionStart) then
      FOnSelectionStart(Self, Count);
  end;
end;

procedure ThCustomChat.SetBorderWidth(const Value: Integer);
begin
  FBorderWidth := Value;
  if not (csLoading in ComponentState) then
  begin
    FItems.NeedResize;
    NeedRepaint;
  end;
end;

procedure ThCustomChat.SetColorInfo(const Value: TColor);
begin
  FColorInfo := Value;
end;

procedure ThCustomChat.SetColorMe(const Value: TColor);
begin
  FColorMe := Value;
end;

procedure ThCustomChat.SetColorOpponent(const Value: TColor);
begin
  FColorOpponent := Value;
end;

procedure ThCustomChat.SetColorScrollActive(const Value: TColor);
begin
  FColorScrollActive := Value;
end;

procedure ThCustomChat.SetColorScrollButton(const Value: TColor);
begin
  FColorScrollButton := Value;
end;

procedure ThCustomChat.SetColorScrollInactive(const Value: TColor);
begin
  FColorScrollInactive := Value;
end;

procedure ThCustomChat.SetColorSelection(const Value: TColor);
begin
  FColorSelection := Value;
end;

procedure ThCustomChat.SetDrawImages(const Value: Boolean);
begin
  FDrawImages := Value;
  if not (csLoading in ComponentState) then
  begin
    FItems.NeedResize;
    NeedRepaint;
  end;
end;

procedure ThCustomChat.SetImageList(const Value: TImageList);
begin
  FImageList := Value;
  NeedRepaint;
end;

procedure ThCustomChat.SetImageMargin(const Value: Integer);
begin
  FImageMargin := Value;
  if not (csLoading in ComponentState) then
  begin
    FItems.NeedResize;
    NeedRepaint;
  end;
end;

procedure ThCustomChat.SetItem(Index: Integer; const Value: TChatItem);
begin
  FItems[Index] := Value;
  NeedRepaint;
end;

procedure ThCustomChat.SetItems(const Value: TChatItems);
begin
  if Assigned(FItems) then
    FItems.Free;
  FItems := Value;
end;

procedure ThCustomChat.SetOnSelectionChange(const Value: TOnSelectionEvent);
begin
  FOnSelectionChange := Value;
end;

procedure ThCustomChat.SetOnSelectionEnd(const Value: TOnSelectionEvent);
begin
  FOnSelectionEnd := Value;
end;

procedure ThCustomChat.SetOnSelectionStart(const Value: TOnSelectionEvent);
begin
  FOnSelectionStart := Value;
end;

procedure ThCustomChat.SetPaddingSize(const Value: Integer);
begin
  FPaddingSize := Value;
  if not (csLoading in ComponentState) then
  begin
    FItems.NeedResize;
    NeedRepaint;
  end;
end;

procedure ThCustomChat.SetRevertAdding(const Value: Boolean);
begin
  FRevertAdding := Value;
end;

procedure ThCustomChat.SetScrollBarVisible(const Value: Boolean);
begin
  FScrollBarVisible := Value;
  NeedRepaint;
end;

{ TChatItems }

function TChatItems.Add(Value: TChatItem): Integer;
begin
  if not FOwner.RevertAdding then
    Result := inherited Add(Value)
  else
  begin
    Insert(0, Value);
    Result := 0;
  end;
  FOwner.NeedRepaint;
end;

function TChatItems.Add<T>(): T;
begin
  Result := T.Create(Self);
  Add(Result);
  FOwner.NeedRepaint;
end;

function TChatItems.AddInfo: TChatInfo;
begin
  Result := TChatInfo.Create(Self);
  Add(Result);
  FOwner.NeedRepaint;
end;

function TChatItems.AddMessage: TChatMessage;
begin
  Result := TChatMessage.Create(Self);
  Add(Result);
  FOwner.NeedRepaint;
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
  FOwner := AOwner;
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
  Owner.SelectionChange(SelectCount);
  Owner.NeedRepaint;
end;

procedure TChatItems.NeedResize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].FNeedCalc := True;
  end;
end;

function TChatItems.SelectCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].Selected then
      Inc(Result);
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
    Canvas.Font.Style := [];
    Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfWordBreak, tfEndEllipsis]);
    FCalcedRect := R;
    Result := R;
    FNeedCalc := False;
  end
  else
    Result := FCalcedRect;
end;

function TChatItem.DrawImage(Canvas: TCanvas; Rect: TRect): TRect;
begin
  with Canvas do
  begin
    Brush.Color := (Self as TChatMessage).FromColor;
    Pen.Color := Brush.Color;
    Ellipse(Rect);
  end;
end;

function TChatItem.DrawRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  R := Rect;
  S := Text;
  Canvas.Font.Size := 8;
  Canvas.Font.Style := [];
  Canvas.Font.Color := Color;
  Canvas.TextRect(R, S, [tfLeft, tfWordBreak, tfEndEllipsis]);
  Result := R;
end;

constructor TChatItem.Create(AOwner: TChatItems);
begin
  inherited Create;
  FCanSelected := True;
  FCalcedRect := TRect.Empty;
  FNeedCalc := True;
  FOwner := AOwner;
  Color := clWhite;
end;

destructor TChatItem.Destroy;
begin
  inherited;
end;

procedure TChatItem.SetDate(const Value: TDateTime);
begin
  FDate := Value;
end;

procedure TChatItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TChatItem.SetOwner(const Value: TChatItems);
begin
  FOwner := Value;
end;

procedure TChatItem.SetSelected(const Value: Boolean);
begin
  if not FCanSelected then
    Exit;
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
    Result := Rect;
    R := Rect;
    S := Text;
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];
    Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfWordBreak, tfEndEllipsis]);
    Result.Width := R.Width;
    Result.Height := R.Height;

    if (FromType = mtOpponent) and FShowFrom then
    begin
      R := Rect;
      S := From;
      Canvas.Font.Size := 11;
      Canvas.Font.Style := [];
      Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfSingleLine, tfEndEllipsis]);
      Result.Width := Max(Result.Width, R.Width);
      Result.Height := Result.Height + R.Height;
      FCalcedFromHeight := R.Height;
    end;

    FCalcedRect := Result;
    FNeedCalc := False;
  end
  else
    Result := FCalcedRect;
end;

function TChatMessage.DrawRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  if (FromType = mtOpponent) and FShowFrom then
  begin
    S := From;
    R := Rect;
    Canvas.Font.Size := 11;
    Canvas.Font.Style := [];
    if Selected then
      Canvas.Font.Color := FromColorSelect
    else
      Canvas.Font.Color := FromColor;
    Canvas.TextRect(R, S, [tfLeft, tfSingleLine, tfEndEllipsis]);
  end;

  R := Rect;
  if (FromType = mtOpponent) and FShowFrom then
    R.Offset(0, FCalcedFromHeight);
  S := Text;
  Canvas.Font.Size := 8;
  Canvas.Font.Style := [];
  Canvas.Font.Color := Color;
  Canvas.TextRect(R, S, [tfLeft, tfWordBreak, tfEndEllipsis]);
  Rect.Width := Max(Rect.Width, R.Width);
  Rect.Height := Rect.Height + R.Height;

  Result := Rect;
end;

function TChatMessage.GetFromType: TChatMessageType;
begin
  Result := FFromType;
end;

procedure TChatMessage.SetCalcedFromHeight(const Value: Integer);
begin
  FCalcedFromHeight := Value;
end;

procedure TChatMessage.SetFrom(const Value: string);
begin
  FFrom := Value;
end;

procedure TChatMessage.SetFromColor(const Value: TColor);
begin
  FFromColor := Value;
end;

procedure TChatMessage.SetFromColorSelect(const Value: TColor);
begin
  FFromColorSelect := Value;
end;

procedure TChatMessage.SetFromType(const Value: TChatMessageType);
begin
  FFromType := Value;
end;

procedure TChatMessage.SetShowFrom(const Value: Boolean);
begin
  FShowFrom := Value;
end;

constructor TChatMessage.Create(AOwner: TChatItems);
begin
  inherited;
  FShowFrom := True;
  FromColor := $00D4D4D4;
  FromColorSelect := clWhite;
end;

destructor TChatMessage.Destroy;
begin
  inherited;
end;

{ TChatInfo }

function TChatInfo.CalcRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  if FNeedCalc then
  begin
    R := Rect;
    S := Text;
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [fsBold];
    Canvas.TextRect(R, S, [tfLeft, tfCalcRect, tfWordBreak, tfEndEllipsis]);
    FCalcedRect := R;
    Result := R;
    FNeedCalc := False;
  end
  else
    Result := FCalcedRect;
end;

constructor TChatInfo.Create(AOwner: TChatItems);
begin
  inherited;
  FCanSelected := False;
  FFillColor := clNone;
end;

function TChatInfo.DrawRect(Canvas: TCanvas; Rect: TRect): TRect;
var
  R: TRect;
  S: string;
begin
  R := Rect;
  S := Text;
  Canvas.Font.Size := 8;
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Color := Color;
  Canvas.TextRect(R, S, [tfLeft, tfWordBreak, tfEndEllipsis]);
  Result := R;
end;

procedure TChatInfo.SetFillColor(const Value: TColor);
begin
  FFillColor := Value;
end;

end.

