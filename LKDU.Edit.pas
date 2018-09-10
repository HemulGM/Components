unit LKDU.Edit;

interface
 uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,
  Vcl.ExtCtrls, System.UITypes, Vcl.Buttons, Vcl.Mask, TableDraw, LKDU.Edit.Form;

 type
  TCentredEdit = class(TMaskEdit)
   procedure CreateParams (var Params: TCreateParams); override;
   procedure ValidateError; override;
  end;

  TEditExState = (exsNormal, exsError, exsNotEmpty);
  TOnPressEnter = procedure(Sender:TObject; Handled:Boolean) of object;
  TEditMode = (emText, emDate, emTime, emNumber, emMask);

  TEditEx = class(TCustomPanel)
   private
    FRightButton:TSpeedButton;
    FLeftButton:TSpeedButton;
    FEdit:TCentredEdit;
    FColor:TColor;
    FState:TEditExState;
    FOnChange:TNotifyEvent;
    FOnEditButtonClick:TNotifyEvent;
    FOnClearButtonClick:TNotifyEvent;
    FOnPressEnter:TOnPressEnter;
    FOnEditKeyPress: TKeyPressEvent;
    FEditMode: TEditMode;
    FMask: string;
    FText: string;
    procedure OnEditChange(Sender:TObject);
    procedure OnButtonClick(Sender:TObject);
    procedure OnEditEnter(Sender:TObject);
    procedure OnEditKeyPress(Sender: TObject; var Key: Char);
    procedure OnLeftButtonClick(Sender:TObject);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetShowClearButton: Boolean;
    procedure SetShowClearButton(const Value: Boolean);
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
    function GetNumberOnly: Boolean;
    procedure SetNumberOnly(const Value: Boolean);
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetShowEditButton: Boolean;
    procedure SetShowEditButton(const Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GoToControl(Prev:Boolean);
    procedure SetEditMode(const Value: TEditMode);
    procedure SetMask(const Value: string);
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(const Value: Boolean);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetNumber(Value:Integer);
    property State:TEditExState read FState;
    function Flash:Boolean;
    procedure Reset;
    procedure SetFocus;
    property Date:TDate read GetDate write SetDate;
    property Value:Integer read GetValue write SetValue;
   published
    property Align;
    property OnExit;
    property OnEnter;
    property Font:TFont read GetFont write SetFont;
    property AutoSelect:Boolean read GetAutoSelect write SetAutoSelect;
    property TabOrder;
    property ReadOnly:Boolean read GetReadOnly write SetReadOnly;
    property Color:TColor read GetColor write SetColor;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property OnEditButtonClick:TNotifyEvent read FOnEditButtonClick write FOnEditButtonClick;
    property OnClearButtonClick:TNotifyEvent read FOnClearButtonClick write FOnClearButtonClick;
    property Text:string read GetText write SetText;
    property Mode:TEditMode read FEditMode write SetEditMode;
    property Mask:string read FMask write SetMask;
    property TextHint:string read GetTextHint write SetTextHint;
    property OnPressEnter:TOnPressEnter read FOnPressEnter write FOnPressEnter;
    property ShowClearButton:Boolean read GetShowClearButton write SetShowClearButton;
    property ShowEditButton:Boolean read GetShowEditButton write SetShowEditButton;
    property NumberOnly:Boolean read GetNumberOnly write SetNumberOnly;
    property OnKeyPress:TKeyPressEvent read FOnEditKeyPress write FOnEditKeyPress;
    property Visible;
    property Enabled;
  end;

  TOnGetItemText = procedure(Sender:TObject; Index:Integer; var Value:string) of object;

  TlkComboBox = class(TCustomPanel)
   private
    FItemIndex:Integer;
    FListData:TTableEx;
    FListButton:TSpeedButton;
    FRightButton:TSpeedButton;
    FEdit:TCentredEdit;
    FOnChange:TNotifyEvent;
    FOnGetItemText:TOnGetItemText;
    FGetDataProc: TGetTableDataProc;
    FViewListSize: Integer;
    FViewListWidth:Integer;
    FListWindow:TFormList;

    procedure OnEditChange(Sender:TObject);
    procedure OnButtonClick(Sender:TObject);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetShowClearButton: Boolean;
    procedure SetShowClearButton(const Value: Boolean);
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
    procedure OnListButtonClick(Sender:TObject);
    procedure OnListExit(Sender:TObject);
    procedure OnListItemClick(Sender:TObject; MouseButton:TMouseButton; const Index:Integer);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetItemCount: Integer;
    procedure SetItemCount(const Value: Integer);
    procedure SetGetDataProc(const Value: TGetTableDataProc);
    function GetColumnCount: Integer;
    function GetColumns: TTableColumns;
    procedure SetItemIndex(const Value: Integer);
    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(const Value: Boolean);
   public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddColumn:Integer;
    procedure DeleteColumn(Index:Integer);
    property ColumnCount:Integer read GetColumnCount;
    property Columns:TTableColumns read GetColumns;
   published
    property Align;
    property AutoSelect:Boolean read GetAutoSelect write SetAutoSelect;
    property ViewListSize:Integer read FViewListSize write FViewListSize;
    property ViewListWidth:Integer read FViewListWidth write FViewListWidth default 0;
    property ItemCount:Integer read GetItemCount write SetItemCount;
    property GetData:TGetTableDataProc read FGetDataProc write SetGetDataProc;
    property Color:TColor read GetColor write SetColor;
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property Text:string read GetText write SetText;
    property ItemIndex:Integer read FItemIndex write SetItemIndex;
    property TextHint:string read GetTextHint write SetTextHint;
    property ShowClearButton:Boolean read GetShowClearButton write SetShowClearButton;
    property OnGetItemText:TOnGetItemText read FOnGetItemText write FOnGetItemText;
    property Visible;
    property Enabled;
  end;

procedure Register;

implementation
 uses Math;

procedure Register;
begin
 RegisterComponents('LKDU', [TEditEx]);
 RegisterComponents('LKDU', [TlkComboBox]);
end;

{ TEditEx }

constructor TEditEx.Create(AOwner: TComponent);
begin
 inherited;
 Width:=200;
 Height:=24;
 BorderStyle:=bsNone;
 BevelOuter:=bvNone;
 Color:=$00854F18;
 ParentBackground:=False;
 ParentColor:=False;
 FEdit:=TCentredEdit.Create(Self);
 with FEdit do
  begin
   Align:=alClient;
   Margins.Left:=1;
   Margins.Right:=0;
   Margins.Top:=1;
   Margins.Bottom:=1;
   Font.Size:=10;
   BorderStyle:=bsNone;
   BevelInner:=bvRaised;
   BevelKind:=bkFlat;
   BevelWidth:=3;
   BevelOuter:=bvNone;
   AutoSize:=False;
   AlignWithMargins:=True;
   OnChange:=OnEditChange;
   OnKeyPress:=OnEditKeyPress;
   OnKeyDown:=EditKeyDown;
   OnEnter:=OnEditEnter;
   Name:='Edit';
   Parent:=Self;
  end;
 FLeftButton:=TSpeedButton.Create(Self);
 with FLeftButton do
  begin
   Caption:='▼';
   Font.Name:='Arial';
   Font.Size:=10;
   Font.Style:=[fsBold];
   Font.Color:=clWhite;
   Align:=alRight;
   Width:=24;
   Flat:=True;
   Name:='LeftButton';
   OnClick:=OnLeftButtonClick;
   Parent:=Self;
  end;
 FRightButton:=TSpeedButton.Create(Self);
 with FRightButton do
  begin
   Caption:='×';
   Font.Name:='Arial';
   Font.Size:=14;
   Font.Style:=[fsBold];
   Font.Color:=clWhite;
   Align:=alRight;
   Width:=24;
   Flat:=True;
   Name:='RightButton';
   OnClick:=OnButtonClick;
   Parent:=Self;
  end;
 ShowClearButton:=True;
 ShowEditButton:=False;
 Mode:=emText;
 FEdit.Text:=FText;
end;

destructor TEditEx.Destroy;
begin
 inherited;
end;

function TEditEx.Flash:Boolean;
begin
 Result:=True;
 FState:=exsError;
 inherited Color:=clRed;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clMaroon;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clWhite;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clMaroon;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clRed;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clMaroon;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clWhite;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clMaroon;
 Application.ProcessMessages;
 Sleep(60);

 inherited Color:=clRed;
 Application.ProcessMessages;
 Sleep(60);
end;

function TEditEx.GetAutoSelect: Boolean;
begin
 Result:=FEdit.AutoSelect;
end;

function TEditEx.GetColor: TColor;
begin
 Result:=FColor;
end;

function TEditEx.GetDate: TDate;
var DT:TDateTime;
begin
 if TryStrToDate(FEdit.Text, DT) then Result:=DT else Result:=-1;
end;

function TEditEx.GetFont: TFont;
begin
 Result:=FEdit.Font;
end;

function TEditEx.GetNumberOnly: Boolean;
begin
 Result:=FEdit.NumbersOnly;
end;

function TEditEx.GetReadOnly: Boolean;
begin
 Result:=FEdit.ReadOnly;
end;

function TEditEx.GetShowClearButton: Boolean;
begin
 Result:=FRightButton.Visible;
end;

function TEditEx.GetShowEditButton: Boolean;
begin
 Result:=FLeftButton.Visible;
end;

function TEditEx.GetText: string;
begin
 Result:=FEdit.Text;
end;

function TEditEx.GetTextHint: string;
begin
 Result:=FEdit.TextHint;
end;

function TEditEx.GetValue: Integer;
begin
 if not TryStrToInt(FEdit.Text, Result) then Result:=-1;
end;

procedure TEditEx.GoToControl(Prev: Boolean);
var Frm: TCustomForm;
begin
 Frm:=GetParentForm(TCustomForm(Self));
 if Frm = nil then Exit;
 if Prev then SendMessage(Frm.Handle, WM_NEXTDLGCTL, 1, 0)
 else SendMessage(Frm.Handle, WM_NEXTDLGCTL, 0, 0);
end;

procedure TEditEx.OnButtonClick(Sender: TObject);
begin
 if Assigned(FOnClearButtonClick) then
  begin
   FOnClearButtonClick(Self);
   Exit;
  end;
 if FEdit.Modified then
  begin
   FEdit.Reset;
   Exit;
  end;
 FEdit.Clear;
end;

procedure TEditEx.OnEditChange(Sender: TObject);
begin
 Reset;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TEditEx.OnEditEnter(Sender: TObject);
begin
 FEdit.SelectFirst;
end;

procedure TEditEx.OnEditKeyPress(Sender: TObject; var Key: Char);
var Handled:Boolean;
begin
 if (Key = #13) then
  begin
   Key:=#0;
   Handled:=False;
   if Assigned(FOnPressEnter) then
    begin
     FOnPressEnter(Sender, Handled);
     if Handled then Exit;
    end;
   GoToControl(False);
  end;
 if Assigned(FOnEditKeyPress) then FOnEditKeyPress(Sender, Key); 
end;

procedure TEditEx.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key in [VK_UP, VK_LEFT] then
  begin
   Key:=0;
   GoToControl(True);
  end
 else
  if Key in [VK_DOWN, VK_RIGHT] then
   begin
    Key:=0;
    GoToControl(False);
   end;
end;

procedure TEditEx.OnLeftButtonClick(Sender: TObject);
begin
 if Assigned(FOnEditButtonClick) then FOnEditButtonClick(Self);
end;

procedure TEditEx.Reset;
begin
 FState:=exsNormal;
 inherited Color:=FColor;
end;

procedure TEditEx.SetAutoSelect(const Value: Boolean);
begin
 FEdit.AutoSelect:=Value;
end;

procedure TEditEx.SetColor(const Value: TColor);
begin
 FColor:=Value;
 Reset;
end;

procedure TEditEx.SetDate(const Value: TDate);
begin
 if Mode <> emDate then Exit;
 FEdit.Text:=FormatDateTime('DD.MM.YYYY', Value)
end;

procedure TEditEx.SetEditMode(const Value: TEditMode);
begin
 FEditMode:= Value;
 FEdit.EditMask:='';
 FEdit.Reset;
 FEdit.Clear;
 case FEditMode of
  emText:
   begin
    NumberOnly:=False;
    FEdit.EditMask:='';
   end;
  emDate:
   begin
    NumberOnly:=False;
    FEdit.EditMask:='90\.90\.9000;1;_';
   end;
  emTime:
   begin
    NumberOnly:=False;
    FEdit.EditMask:='!90:00;1;_';
   end;
  emNumber:
   begin
    NumberOnly:=True;
    FEdit.EditMask:='';
   end;
  emMask:
   begin
    NumberOnly:=False;
    SetMask(FMask);
   end;
 end;
end;

procedure TEditEx.SetFocus;
begin
 inherited;
 FEdit.SetFocus;
end;

procedure TEditEx.SetFont(const Value: TFont);
begin
 FEdit.Font:=Value;
end;

procedure TEditEx.SetMask(const Value: string);
begin
 FMask:= Value;
 if FEditMode = emMask then FEdit.EditMask:=FMask;
end;

procedure TEditEx.SetNumber(Value: Integer);
begin
 FEdit.Text:=IntToStr(Value);
end;

procedure TEditEx.SetNumberOnly(const Value: Boolean);
begin
 FEdit.NumbersOnly:=Value;
end;

procedure TEditEx.SetReadOnly(const Value: Boolean);
begin
 FEdit.ReadOnly:=Value;
end;

procedure TEditEx.SetShowClearButton(const Value: Boolean);
begin
 FRightButton.Visible:=Value;
 if not FLeftButton.Visible then
  if Value then FEdit.Margins.Right:=0 else FEdit.Margins.Right:=1;
 Repaint;
end;

procedure TEditEx.SetShowEditButton(const Value: Boolean);
begin
 FLeftButton.Visible:=Value;
 if not FRightButton.Visible then
  if Value then FEdit.Margins.Right:=0 else FEdit.Margins.Right:=1;
 Repaint;
end;

procedure TEditEx.SetText(const Value: string);
begin
 FText:=Value;
 FEdit.Text:=FText;
end;

procedure TEditEx.SetTextHint(const Value: string);
begin
 FEdit.TextHint:=Value;
end;

procedure TEditEx.SetValue(const Value: Integer);
begin
 if NumberOnly then SetNumber(Value);
end;

{ TCentredEdit }

procedure TCentredEdit.CreateParams(var Params: TCreateParams);
begin
 inherited;
end;

{ TComboBoxEx }

function TlkComboBox.GetAutoSelect: Boolean;
begin
 Result:=FEdit.AutoSelect;
end;

function TlkComboBox.GetColor: TColor;
begin
 Result:=inherited Color;
end;

function TlkComboBox.GetColumnCount: Integer;
begin
 Result:=FListData.ColumnCount;
end;

function TlkComboBox.GetColumns: TTableColumns;
begin
 Result:=FListData.Columns;
end;

function TlkComboBox.GetItemCount: Integer;
begin
 Result:=FListData.ItemCount;
end;

function TlkComboBox.GetShowClearButton: Boolean;
begin
 Result:=FRightButton.Visible;
end;

function TlkComboBox.GetText: string;
begin
 Result:=FEdit.Text;
end;

function TlkComboBox.GetTextHint: string;
begin
 Result:=FEdit.TextHint;
end;

procedure TlkComboBox.OnButtonClick(Sender: TObject);
begin
 FEdit.Clear;
 ItemIndex:=-1;
end;

procedure TlkComboBox.OnEditChange(Sender: TObject);
begin
 ItemIndex:=-1;
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TlkComboBox.OnListButtonClick(Sender: TObject);
begin
 {
 if FListWindow.Visible then
  begin
   //OnListExit(nil);
  end
 else
  begin
   //FListData.Parent:=Self.Parent;
   FListWindow.Width:=Self.Width;
   FListWindow.Left:=Self.ClientToScreen(Point(0, 0)).X;
   FListWindow.Top:=Self.ClientToScreen(Point(0, 0)).Y+Self.Height-1;
   FListData.DefaultRowHeight:=Self.Height;
   FListData.Height:=Min(FListData.ItemCount, FViewListSize)*FListData.DefaultRowHeight+2;
   //FListData.ShowColumns:=False;
   FListWindow.Show;
   FListWindow.SetFocus;
  end;  }
 if FListData.Visible then
  begin
   OnListExit(nil);
  end
 else
  begin
   FListData.Parent:=Self.Parent;
   if FViewListWidth <= 0 then
    FListData.Width:=Self.Width
   else FListData.Width:=FViewListWidth;
   FListData.Left:=Self.Left;
   FListData.Top:=Self.Top+Self.Height-1;
   FListData.DefaultRowHeight:=Self.Height;
   FListData.Height:=Min(FListData.ItemCount, FViewListSize)*FListData.DefaultRowHeight+2;
   FListData.ShowColumns:=False;
   FListData.Show;
   FListData.BringToFront;
   FListData.SetFocus;
  end;
end;

procedure TlkComboBox.OnListExit(Sender: TObject);
begin
 FListData.Visible:=False;
end;

procedure TlkComboBox.SetAutoSelect(const Value: Boolean);
begin
 FEdit.AutoSelect:=Value;
end;

procedure TlkComboBox.SetColor(const Value: TColor);
begin
 inherited Color:=Value;
 FListData.LineColor:=clWhite;
 FListData.LineColorXor:=clWhite;
 FListData.LineSelColor:=Value;
end;

procedure TlkComboBox.SetGetDataProc(const Value: TGetTableDataProc);
begin
 FGetDataProc:= Value;
 FListData.GetData:=FGetDataProc;
end;

procedure TlkComboBox.SetItemCount(const Value: Integer);
begin
 FListData.ItemCount:=Value;
end;

procedure TlkComboBox.SetItemIndex(const Value: Integer);
begin
 FItemIndex:= Value;
 if Assigned(FListData) then FListData.ItemIndex:=Value;
end;

procedure TlkComboBox.SetShowClearButton(const Value: Boolean);
begin
 FRightButton.Visible:=Value;
 if Value then FEdit.Margins.Right:=0 else FEdit.Margins.Right:=1;
 Repaint;
end;

procedure TlkComboBox.SetText(const Value: string);
begin
 FEdit.Text:=Value;
end;

procedure TlkComboBox.SetTextHint(const Value: string);
begin
 FEdit.TextHint:=Value;
end;

function TlkComboBox.AddColumn: Integer;
begin
 Result:=FListData.AddColumn;
end;

constructor TlkComboBox.Create(AOwner: TComponent);
begin
 inherited;
 Width:=200;
 Height:=24;
 BorderStyle:=bsNone;
 BevelOuter:=bvNone;
 FViewListSize:=10;
 FViewListWidth:=0;
 ParentBackground:=False;
 ParentColor:=False;
 FEdit:=TCentredEdit.Create(Self);
 with FEdit do
  begin
   Parent:=Self;
   Align:=alClient;
   Margins.Left:=1;
   Margins.Right:=0;
   Margins.Top:=1;
   Margins.Bottom:=1;
   Font.Size:=10;
   BorderStyle:=bsNone;
   BevelInner:=bvRaised;
   BevelKind:=bkFlat;
   BevelWidth:=3;
   BevelOuter:=bvNone;
   AlignWithMargins:=True;
   OnChange:=OnEditChange;
   Name:='Edit';
   Text:='';
  end;
 FListButton:=TSpeedButton.Create(Self);
 with FListButton do
  begin
   Parent:=Self;
   Caption:='▼';
   Font.Name:='Arial';
   Font.Size:=10;
   Font.Style:=[fsBold];
   Font.Color:=clWhite;
   Align:=alRight;
   Width:=24;
   Flat:=True;
   Name:='ListButton';
   OnClick:=OnListButtonClick;
  end;
 FRightButton:=TSpeedButton.Create(Self);
 with FRightButton do
  begin
   Caption:='×';
   Font.Name:='Arial';
   Font.Size:=14;
   Font.Style:=[fsBold];
   Font.Color:=clWhite;
   Align:=alRight;
   Width:=24;
   Flat:=True;
   Name:='Button';
   OnClick:=OnButtonClick;
   Parent:=Self;
  end;
 {FListWindow:=TFormList.Create(nil);
 with FListWindow do
  begin
   Parent:=nil;
   Visible:=False;
   OnDeactivate:=OnListExit;
  end; }

 FListData:=TTableEx.Create(Self);
 //FListData:=FListWindow.TableEx1;
 with FListData do
  begin
   Name:='FListData';
   Parent:=Self;
   Align:=alClient;
   ItemCount:=0;
   Visible:=False;
   //ShowColumns:=False;
   DefaultRowHeight:=20;
   OnExit:=OnListExit;
   BevelInner:=bvNone;
   BevelKind:=bkFlat;
   BorderStyle:=bsNone;
   OnMouseLeave:=OnListExit;
   OnItemClick:=OnListItemClick;
  end;
 FListData.Parent:=nil;
 Color:=$00854F18;
 ShowClearButton:=True;
end;

procedure TlkComboBox.DeleteColumn(Index: Integer);
begin
 FListData.DeleteColumn(Index);
end;

destructor TlkComboBox.Destroy;
begin
 inherited;
end;

procedure TlkComboBox.OnListItemClick(Sender: TObject; MouseButton: TMouseButton; const Index: Integer);
var Str:string;
begin
 Str:='Укажите OnGetItemText';
 if Assigned(FOnGetItemText) then FOnGetItemText(Self, Index, Str);
 Text:=Str;
 ItemIndex:=Index;
 OnListExit(nil);
end;

procedure TCentredEdit.ValidateError;
begin
 //inherited;

end;

end.
