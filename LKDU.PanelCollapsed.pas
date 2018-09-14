unit LKDU.PanelCollapsed;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, PanelExt, Vcl.Buttons, Vcl.Imaging.pngimage;

type
  TPanelCollapsed = class(TPanelExt)
   private
    FCaptionPanel:TPanel;
    FCollapsing:Boolean;
    FHeight:Integer;
    FCollapseButton:TSpeedButton;
   // FOnPaint: TNotifyEvent;
    FShowSimpleBorder: Boolean;
    FSimpleBorderColor: TColor;
    FCollapsed: Boolean;
    FCapHeight: Integer;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function GetShowCaption: Boolean;
    procedure SetShowCaption(const Value: Boolean);
    function GetFontCaption: TFont;
    procedure SetFontCaption(const Value: TFont);
    procedure SetAlignment(const Value: TAlignment);
    function GetAlignment: TAlignment;
    function GetCaptionColor: TColor;
    procedure SetCaptionColor(const Value: TColor);
    procedure SetShowSimpleBorder(const Value: Boolean);
    procedure OnCollapseButtonClick(Sender:TObject);
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetCollapsed(const Value: Boolean);
    procedure WMSize(var Msg:TWMSize); message WM_SIZE;
    function GetShowCollapseButton: Boolean;
    procedure SetShowCollapseButton(const Value: Boolean);
    procedure SetCapHeight(const Value: Integer);
   public
    procedure Paint; override;
   published
    property ShowCollapseButton:Boolean read GetShowCollapseButton write SetShowCollapseButton;
    property Collapsed:Boolean read FCollapsed write SetCollapsed;
    property Height:Integer read GetHeight write SetHeight;
    property Caption:string read GetCaption write SetCaption;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property Font;
    property SimpleBorderColor:TColor read FSimpleBorderColor write FSimpleBorderColor;
    property CaptionColor:TColor read GetCaptionColor write SetCaptionColor;
    property FontCaption:TFont read GetFontCaption write SetFontCaption;
    property ShowCaption:Boolean read GetShowCaption write SetShowCaption;
    property ShowSimpleBorder:Boolean read FShowSimpleBorder write SetShowSimpleBorder;
    property VerticalAlignment;
    property CaptionHeight:Integer read FCapHeight write SetCapHeight;
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

{$R CollPanel.res}

procedure Register;
begin
 RegisterComponents('LKDU', [TPanelCollapsed]);
end;

{ TPanelCollapsed }

constructor TPanelCollapsed.Create(AOwner: TComponent);
var PNG:TPngImage;
begin
 inherited;
 inherited ShowCaption:=False;
 inherited BorderStyle:=bsNone;
 inherited BevelInner:=bvNone;
 inherited BevelKind:=bkNone;
 inherited BevelOuter:=bvNone;
 inherited ParentBackground:=False;
 FCollapsing:=False;
 Height:=300;
 FCapHeight:=30;
 FCaptionPanel:=TPanel.Create(Self);
 with FCaptionPanel do
  begin
   Parent:=Self;
   Align:=alTop;
   Height:=FCapHeight;
   Caption:=' Caption';
   ShowCaption:=True;
   ParentBackground:=False;
   ParentColor:=False;
   BorderStyle:=bsNone;
   BevelInner:=bvNone;
   BevelKind:=bkNone;
   BevelOuter:=bvNone;
   BringToFront;
   Color:=clGray;
   Font.Color:=clWhite;
   Font.Style:=[fsBold];
   Font.Size:=10;
   Alignment:=taLeftJustify;
   OnClick:=OnCollapseButtonClick;
  end;
 FCollapseButton:=TSpeedButton.Create(FCaptionPanel);
 with FCollapseButton do
  begin
   Parent:=FCaptionPanel;
   Align:=alRight;
   AlignWithMargins:=True;
   PNG:=TPngImage.Create;
   PNG.LoadFromResourceName(HInstance, 'COLPANEL_HIDE');
   Glyph.Assign(PNG);
   PNG.Free;
   Flat:=True;
   OnClick:=OnCollapseButtonClick;
  end;
 ShowSimpleBorder:=False;
 SimpleBorderColor:=clSilver;
end;

function TPanelCollapsed.GetAlignment: TAlignment;
begin
 Result:=FCaptionPanel.Alignment;
end;

function TPanelCollapsed.GetCaption: string;
begin
 Result:=FCaptionPanel.Caption;
end;

function TPanelCollapsed.GetCaptionColor: TColor;
begin
 Result:=FCaptionPanel.Color;
end;

function TPanelCollapsed.GetFontCaption: TFont;
begin
 Result:=FCaptionPanel.Font;
end;

function TPanelCollapsed.GetHeight: Integer;
begin
 Result:=FHeight;
end;

function TPanelCollapsed.GetShowCaption: Boolean;
begin
 Result:=FCaptionPanel.ShowCaption;
end;

function TPanelCollapsed.GetShowCollapseButton: Boolean;
begin
 Result:=FCollapseButton.Visible;
end;

procedure TPanelCollapsed.OnCollapseButtonClick(Sender: TObject);
begin
 if not FCollapseButton.Visible then Exit;
 Collapsed:=not Collapsed;
end;

procedure TPanelCollapsed.Paint;
begin
 inherited;
 if FShowSimpleBorder then
  begin
   with Canvas do
    begin
     Pen.Color:=FSimpleBorderColor;
     Brush.Style:=bsClear;
     Rectangle(ClientRect);
    end;
  end;
end;

procedure TPanelCollapsed.SetAlignment(const Value: TAlignment);
begin
 FCaptionPanel.Alignment:= Value;
end;

procedure TPanelCollapsed.SetCapHeight(const Value: Integer);
begin
 FCapHeight := Value;
 FCaptionPanel.Height:=FCapHeight;
end;

procedure TPanelCollapsed.SetCaption(const Value: string);
begin
 FCaptionPanel.Caption:=Value;
end;

procedure TPanelCollapsed.SetCaptionColor(const Value: TColor);
begin
 FCaptionPanel.Color:=Value;
end;

procedure TPanelCollapsed.SetCollapsed(const Value: Boolean);
begin
 if FCollapsing then Exit;
 FCollapsing:=True;
 FCollapsed:= Value;
 if FCollapsed then
  begin    {
   while inherited Height > FCaptionPanel.Height+2 do
    begin
     inherited Height:=inherited Height - 3;
     Application.ProcessMessages;
     Sleep(1);
    end;           }
   inherited Height:=FCaptionPanel.Height+2;
  end
 else
  begin    {
   while inherited Height < FHeight do
    begin
     inherited Height:=inherited Height + 3;
     Application.ProcessMessages;
     Sleep(1);
    end;    }
   inherited Height:=FHeight;
  end;
 FCollapsing:=False;
end;

procedure TPanelCollapsed.SetFontCaption(const Value: TFont);
begin
 FCaptionPanel.Font:=Value;
end;

procedure TPanelCollapsed.SetHeight(const Value: Integer);
begin
 FHeight:=Value;
 if not Collapsed then inherited Height:=FHeight;
end;

procedure TPanelCollapsed.SetShowCaption(const Value: Boolean);
begin
 FCaptionPanel.ShowCaption:=Value;
end;

procedure TPanelCollapsed.SetShowCollapseButton(const Value: Boolean);
begin
 FCollapseButton.Visible:=Value;
end;

procedure TPanelCollapsed.SetShowSimpleBorder(const Value: Boolean);
begin
 FShowSimpleBorder := Value;
 if FShowSimpleBorder then
  begin
   FCaptionPanel.AlignWithMargins:=True;
   FCaptionPanel.Margins.Left:=1;
   FCaptionPanel.Margins.Top:=1;
   FCaptionPanel.Margins.Right:=1;
   FCaptionPanel.Margins.Bottom:=0;
  end
 else
  begin
   FCaptionPanel.AlignWithMargins:=False;
   FCaptionPanel.Margins.Left:=1;
   FCaptionPanel.Margins.Top:=1;
   FCaptionPanel.Margins.Right:=1;
   FCaptionPanel.Margins.Bottom:=0;
  end;
 Repaint;
end;

procedure TPanelCollapsed.WMSize(var Msg: TWMSize);
begin
 inherited;
 //if not FCollapsing then if Collapsed then Collapsed:=False;
end;

end.
