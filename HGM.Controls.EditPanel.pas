unit HGM.Controls.EditPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, HGM.Common;

type
  TOnPressEnter = procedure(Sender:TObject; var AllowNext:Boolean) of object;

  TEditPanel = class(TPanel)
   private
    FEdit:TEdit;
    FLabel:TLabel;
    FEnterColor:TColor;
    FLeaveColor:TColor;
    FEnter:Boolean;
    FErrorColor:TColor;
    FOnPressEnter:TOnPressEnter;
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnPanelEnter(Sender:TObject);
    procedure OnPanelExit(Sender:TObject);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure EditMouseEnter(Sender:TObject);
    procedure EditMouseLeave(Sender:TObject);
    procedure SetEnterColor(const Value: TColor);
    procedure UpdateColor;
    function GetEditWidth: Integer;
    procedure SetEditWidth(const Value: Integer);
    procedure UpdateLableSise;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetupInternalLabel;
    procedure SetupInternalEdit;
    function GetLabelWidth: Integer;
    procedure SetLabelWidth(const Value: Integer);
    function GetLabelFont: TFont;
    procedure SetLabelFont(const Value: TFont);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
    function GetEditFont: TFont;
    procedure SetEditFont(const Value: TFont);
    procedure SetLeaveColor(const Value: TColor);
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    function GetLayout: TTextLayout;
    procedure SetLayout(const Value: TTextLayout);
  protected
    procedure SetParent(AParent:TWinControl); override;
   // property ShowCaption:Boolean; reintroduce;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ErrorFlash;
  published
    property Caption:string read GetCaption write SetCaption;
    property EnterColor:TColor read FEnterColor write SetEnterColor;
    property LeaveColor:TColor read FLeaveColor write SetLeaveColor;
    property EditWidth:Integer read GetEditWidth write SetEditWidth;
    property LebelWidth:Integer read GetLabelWidth write SetLabelWidth;
    property LabelFont:TFont read GetLabelFont write SetLabelFont;
    property EditFont:TFont read GetEditFont write SetEditFont;
    property Text:string read GetText write SetText;
    property TextHint:string read GetTextHint write SetTextHint;
    property OnPressEnter:TOnPressEnter read FOnPressEnter write FOnPressEnter;
    property Alignment:TAlignment read GetAlignment write SetAlignment;
    property VerticalAlignment:TTextLayout read GetLayout write SetLayout;
    property ErrorColor:TColor read FErrorColor write FErrorColor;
  end;


procedure Register;

implementation

procedure Register;
begin
 RegisterComponents(PackageName, [TEditPanel]);
end;

{ TEditPanel }

procedure TEditPanel.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Frm: TCustomForm;
begin
 if Key in [VK_UP] then
  begin
   Key:=0;
   Frm:=GetParentForm(TCustomForm(Self));
   if not (Frm = nil) then SendMessage(Frm.Handle, WM_NEXTDLGCTL, 1, 0);
  end
 else
  if Key in [VK_DOWN] then
   begin
    Key:=0;
    Frm:=GetParentForm(Self);
    if not (Frm = nil) then SendMessage(Frm.Handle, WM_NEXTDLGCTL, 0, 0);
   end;
end;

procedure TEditPanel.EditKeyPress(Sender: TObject; var Key: Char);
var AllowNext:Boolean;
var Frm: TCustomForm;
begin
 if Key = #13 then
  begin
   Key:=#0;
   AllowNext:=True;
   if Assigned(FOnPressEnter) then FOnPressEnter(Sender, AllowNext);
   if AllowNext then
    begin
     Frm:=GetParentForm(Self);
     if not (Frm = nil) then SendMessage(Frm.Handle, WM_NEXTDLGCTL, 0, 0);
    end;
  end;
end;

procedure TEditPanel.SetupInternalEdit;
begin
 //if Assigned(FEdit) then Exit;
 FEdit:=TEdit.Create(Self);
 FEdit.Parent:=Self;
 FEdit.AutoSize:=False;
 FEdit.Width:=200;
 FEdit.AlignWithMargins:=True;
 FEdit.Margins.Left:=5;
 FEdit.Margins.Top:=10;
 FEdit.Margins.Right:=10;
 FEdit.Margins.Bottom:=10;
 FEdit.Align:=alNone;
 FEdit.Name:=Name+'FEdit';
 FEdit.Text:='Текст';
 FEdit.OnKeyDown:=EditKeyDown;
 FEdit.OnKeyPress:=EditKeyPress;
 FEdit.OnEnter:=EditMouseEnter;
 FEdit.OnExit:=EditMouseLeave;
 FEdit.Visible:=True;
end;

procedure TEditPanel.SetupInternalLabel;
begin
 //if Assigned(FLabel) then Exit;
 FLabel:=TLabel.Create(Self);
 FLabel.Parent:=Self;
 FLabel.AutoSize:=False;
 FLabel.AlignWithMargins:=True;
 FLabel.Margins.Left:=5;
 FLabel.Margins.Top:=10;
 FLabel.Margins.Right:=0;
 FLabel.Margins.Bottom:=10;
 FLabel.Align:=alNone;
 FLabel.Alignment:=taLeftJustify;
 FLabel.WordWrap:=True;
 FLabel.Font.Color:=clGray;
 FLabel.Layout:=tlCenter;
 FLabel.Name:=Name+'FLabel';
 FLabel.Caption:='Заголовок';
 FLabel.OnClick:=OnPanelEnter;
 FLabel.Width:=200;
 FLabel.Visible:=True;
end;

constructor TEditPanel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FEdit:=nil;
 FLabel:=nil;
 FErrorColor:=$000033CC;
 Color:=clWhite;
 FEnter:=False;
 FEnterColor:=$0095E7FA;
 FLeaveColor:=Color;
 UpdateColor;
 Width:=420;
 BevelOuter:=bvNone;
 ShowCaption:=False;
 ParentBackground:=False;
 OnEnter:=OnPanelEnter;
 OnClick:=OnPanelEnter;
 OnExit:=OnPanelExit;
 SetupInternalLabel;
 SetupInternalEdit;
 VerticalAlignment:=tlCenter;
 Alignment:=taLeftJustify;
 Caption:='Заголовок';
 Text:='';
 TextHint:='';
 UpdateLableSise;
end;

procedure TEditPanel.SetParent(AParent: TWinControl);
begin
 inherited SetParent(AParent);
end;

procedure TEditPanel.SetText(const Value: string);
begin
 FEdit.Text:=Value;
end;

procedure TEditPanel.SetTextHint(const Value: string);
begin
 FEdit.TextHint:=Value;
end;

procedure TEditPanel.EditMouseEnter(Sender: TObject);
begin
 FEnter:=True;
 UpdateColor;
end;

procedure TEditPanel.EditMouseLeave(Sender: TObject);
begin
 FEnter:=False;
 UpdateColor;
end;

procedure TEditPanel.ErrorFlash;
var DefColor:TColor;
begin
 DefColor:=Color;

 Color:=FErrorColor;
 Repaint;
 Sleep(70);

 Color:=DefColor;
 Repaint;
 Sleep(70);

 Color:=FErrorColor;
 Repaint;
 Sleep(70);

 Color:=DefColor;
 Repaint;
end;

function TEditPanel.GetAlignment: TAlignment;
begin
 Result:=FLabel.Alignment;
end;

function TEditPanel.GetCaption: string;
begin
 Result:=FLabel.Caption;
end;

function TEditPanel.GetEditFont: TFont;
begin
 Result:=FEdit.Font;
end;

function TEditPanel.GetEditWidth: Integer;
begin
 Result:=FEdit.Width;
end;

function TEditPanel.GetLabelFont: TFont;
begin
 Result:=FLabel.Font;
end;

function TEditPanel.GetLabelWidth: Integer;
begin
 Result:=FLabel.Width;
end;

function TEditPanel.GetLayout: TTextLayout;
begin
 Result:=FLabel.Layout;
end;

function TEditPanel.GetText: string;
begin
 Result:=FEdit.Text;
end;

function TEditPanel.GetTextHint: string;
begin
 Result:=FEdit.TextHint;
end;

procedure TEditPanel.OnPanelEnter(Sender: TObject);
begin
 EditMouseEnter(Sender);
 FEdit.SetFocus;
end;

procedure TEditPanel.OnPanelExit(Sender: TObject);
begin
 EditMouseLeave(Sender);
end;

procedure TEditPanel.SetAlignment(const Value: TAlignment);
begin
 FLabel.Alignment:=Value;
 UpdateLableSise;
end;

procedure TEditPanel.SetCaption(const Value: string);
begin
 FLabel.Caption:=Value;
end;

procedure TEditPanel.SetEditFont(const Value: TFont);
begin
 FEdit.Font:=Value;
end;

procedure TEditPanel.SetEditWidth(const Value: Integer);
begin
 FEdit.Width:=Value;
 UpdateLableSise;
end;

procedure TEditPanel.SetEnterColor(const Value: TColor);
begin
 FEnterColor:=Value;
 UpdateColor;
end;

procedure TEditPanel.SetLabelFont(const Value: TFont);
begin
 FLabel.Font:=Value;
end;

procedure TEditPanel.SetLabelWidth(const Value: Integer);
begin
 FLabel.Width:=Value;
 UpdateLableSise;
end;

procedure TEditPanel.SetLayout(const Value: TTextLayout);
begin
 FLabel.Layout:=Value;
end;

procedure TEditPanel.SetLeaveColor(const Value: TColor);
begin
 FLeaveColor:=Value;
 UpdateColor;
end;

procedure TEditPanel.UpdateColor;
begin
 case FEnter of
  True: Color:=FEnterColor;
  False:Color:=FLeaveColor;
 end;
end;

procedure TEditPanel.UpdateLableSise;
begin
 if (not Assigned(FLabel)) or (not Assigned(FEdit)) then Exit;
 FLabel.Top:=FLabel.Margins.Top;
 FLabel.Height:=Height - (FLabel.Margins.Top + FLabel.Margins.Bottom);
 FLabel.Left:=FLabel.Margins.Left;
 FEdit.Left:=FLabel.Left + FLabel.Width + FEdit.Margins.Left;
 FEdit.Top:=FEdit.Margins.Top;
 FEdit.Height:=Self.Height-(FEdit.Margins.Top + FEdit.Margins.Bottom);
 Refresh;
end;

procedure TEditPanel.WMSize(var Message: TWMSize);
begin
 UpdateLableSise;
end;

end.
