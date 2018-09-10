unit HGM.Common.Notify;

interface
 uses
  Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.Buttons;

 type

  TNotifyWindow = class
   type
    TLastRecord = record
     Empty:Boolean;
     Caption:string;
     Text:string;
     Image:string;
     Color:TColor;
    end;
   private
    FBottom:Integer;
    FButtonClose:TSpeedButton;
    FColor: TColor;
    FLabelCaption:TLabel;
    FLabelText:TLabel;
    FLastNotify:TLastRecord;
    FOwner:TForm;
    FPanel:TForm;
    FPanelClient:TPanel;
    FPanelLeft:TPanel;
    FPanelTop:TPanel;
    FRight:Integer;
    FTimerHide:TTimer;
    FVisible:Boolean;
    procedure AnimateWait;
    procedure BeforeShow;
    procedure HideNotify;
    procedure OnClickClose(Sender:TObject);
    procedure OnClickText(Sender:TObject);
    procedure OnTimerHide(Sender:TObject);
    procedure SetColor(const Value: TColor);
    procedure SetPanel(const Value: TForm);
    procedure ShowNotify;
    function GetFontCaption: TFont;
    function GetFontText: TFont;
    procedure SetFontCaption(const Value: TFont);
    procedure SetFontText(const Value: TFont);
   public
    constructor Create(AOwner: TForm);
    procedure Close;
    procedure Error(Caption, Text:string);
    procedure HideLastNotify;
    procedure Info(Caption, Text:string);
    procedure ShowLastNotify;
    procedure ShowTextMessage;
    procedure Update;
    procedure Warning(Caption, Text:string);
    property FontCaption:TFont read GetFontCaption write SetFontCaption;
    property FontText:TFont read GetFontText write SetFontText;
    property Bottom:Integer read FBottom write FBottom;
    property Color:TColor read FColor write SetColor;
    property LabelCaption:TLabel read FLabelCaption;
    property LabelText:TLabel read FLabelText;
    property LastNotify:TLastRecord read FLastNotify;
    property Owner:TForm read FOwner;
    property Panel:TForm read FPanel write SetPanel;
    property PanelClient:TPanel read FPanelClient;
    property PanelLeft:TPanel read FPanelLeft;
    property Right:Integer read FRight write FRight;
  end;

  TNotifyPanel = class
   type
    TLastRecord = record
     Empty:Boolean;
     Caption:string;
     Text:string;
     Image:string;
     Color:TColor;
    end;
   private
    FBottom:Integer;
    FButtonClose:TSpeedButton;
    FColor: TColor;
    FLabelCaption:TLabel;
    FLabelText:TLabel;
    FLastNotify:TLastRecord;
    FOwner:TForm;
    FPanel:TPanel;
    FPanelClient:TPanel;
    FPanelLeft:TPanel;
    FPanelTop:TPanel;
    FRight:Integer;
    FTimerHide:TTimer;
    FVisible:Boolean;
    FOldSize:TSize;
    procedure AnimateWait;
    procedure BeforeShow;
    procedure HideNotify;
    procedure OnClickClose(Sender:TObject);
    procedure OnClickText(Sender:TObject);
    procedure OnTimerHide(Sender:TObject);
    procedure SetColor(const Value: TColor);
    procedure SetPanel(const Value: Tpanel);
    procedure ShowNotify;
    procedure SetRight(const Value: Integer);
    function GetFontCaption: TFont;
    function GetFontText: TFont;
    procedure SetFontCaption(const Value: TFont);
    procedure SetFontText(const Value: TFont);
   public
    constructor Create(AOwner: TForm);
    procedure Close;
    procedure Error(Caption, Text:string);
    procedure Info(Caption, Text:string);
    procedure Warning(Caption, Text:string);
    procedure Ok(Caption, Text:string);
    procedure HideLastNotify;
    procedure ShowLastNotify;
    procedure ShowTextMessage;
    procedure UpdateGlobalSize;
    procedure Update;
    property FontCaption:TFont read GetFontCaption write SetFontCaption;
    property FontText:TFont read GetFontText write SetFontText;
    property Bottom:Integer read FBottom write FBottom;
    property Color:TColor read FColor write SetColor;
    property LabelCaption:TLabel read FLabelCaption;
    property LabelText:TLabel read FLabelText;
    property LastNotify:TLastRecord read FLastNotify;
    property Owner:TForm read FOwner;
    property Panel:TPanel read FPanel write SetPanel;
    property PanelClient:TPanel read FPanelClient;
    property PanelLeft:TPanel read FPanelLeft;
    property Right:Integer read FRight write SetRight;
  end;

implementation

{ TNotifyWindow }

constructor TNotifyWindow.Create(AOwner: TForm);
begin
 FOwner:=AOwner;
 FVisible:=False;
 FRight:=20;
 FBottom:=20;
 HideLastNotify;
 FPanel:=TForm.Create(nil);
 with FPanel do
  begin
   FormStyle:=fsStayOnTop;
   Visible:=False;
   Caption:='Notify';
   DoubleBuffered:=True;
   BorderStyle:=bsNone;
  end;
 SetPanel(FPanel);
 Color:=clGray;

 FPanelLeft:=TPanel.Create(FPanel);
 with FPanelLeft do
  begin
   Parent:=FPanel;
   Align:=alLeft;
   BevelOuter:=bvNone;
   ParentBackground:=False;
   ParentColor:=False;
   Color:=clMaroon;
   Caption:='!';
   Font.Size:=45;
   Width:=FPanel.Height;
  end;

 FPanelClient:=TPanel.Create(FPanel);
 with FPanelClient do
  begin
   Parent:=FPanel;
   Align:=alClient;
   BevelOuter:=bvNone;
   ParentBackground:=False;
   ParentColor:=True;
   //Color:=FColor;
   Caption:='';
  end;

 FPanelTop:=TPanel.Create(FPanelClient);
 with FPanelTop do
  begin
   Parent:=FPanelClient;
   Height:=25;
   Align:=alTop;
   BevelOuter:=bvNone;
   ParentBackground:=True;
   ParentColor:=True;
   Caption:='';
  end;

 FLabelCaption:=TLabel.Create(FPanelTop);
 with FLabelCaption do
  begin
   Parent:=FPanelTop;
   AlignWithMargins:=True;
   Align:=alClient;
   WordWrap:=False;
   OnClick:=OnClickText;
   Cursor:=crHandPoint;
   Font.Size:=12;
   Caption:='';
  end;

 FButtonClose:=TSpeedButton.Create(FPanelTop);
 with FButtonClose do
  begin
   Parent:=FPanelTop;
   Height:=25;
   Width:=25;
   Align:=alRight;
   Caption:='×';
   Font.Color:=clWhite;
   Font.Size:=14;
   Font.Style:=[fsBold];
   Flat:=True;
   OnClick:=OnClickClose;
  end;

 FLabelText:=TLabel.Create(FPanelClient);
 with FLabelText do
  begin
   Parent:=FPanelClient;
   AlignWithMargins:=True;
   Align:=alClient;
   Margins.Bottom:=10;
   Margins.Right:=10;
   Margins.Left:=10;
   Margins.Top:=3;
   Font.Size:=9;
   OnClick:=OnClickText;
   Cursor:=crHandPoint;
   Caption:='';
   WordWrap:=TRue;
  end;

 FTimerHide:=TTimer.Create(nil);
 FTimerHide.Enabled:=False;
 FTimerHide.Interval:=3500;
 FTimerHide.OnTimer:=OnTimerHide;
end;

procedure TNotifyWindow.AnimateWait;
begin
 Application.ProcessMessages;
 Sleep(5);
end;

procedure TNotifyWindow.BeforeShow;
begin
 if FVisible then HideNotify;
end;

procedure TNotifyWindow.Close;
begin
 if FVisible then HideNotify;
end;

procedure TNotifyWindow.HideLastNotify;
begin
 FLastNotify.Empty:=True;
end;

procedure TNotifyWindow.HideNotify;
var Incr:Integer;
begin
 FTimerHide.Enabled:=False;
 Incr:=2;
 while Panel.Top < Screen.Height do
  begin
   Incr:=Incr+2;
   Panel.Top:=Panel.Top + Incr;
   AnimateWait;
  end;
 Panel.Top:=Screen.Height;
 Panel.Visible:=False;
 FVisible:=False;
end;

procedure TNotifyWindow.ShowLastNotify;
begin
 if FLastNotify.Empty then Exit;
 ShowNotify;
end;

procedure TNotifyWindow.ShowNotify;
var incr, ToPos:Integer;
begin
 FLabelCaption.Caption:=FLastNotify.Caption;
 FLabelText.Caption:=FLastNotify.Text;
 FPanelLeft.Color:=FLastNotify.Color;
 FVisible:=True;
 FTimerHide.Enabled:=False;
 Panel.Visible:=True;
 Panel.BringToFront;
 Incr:=2;

 Panel.Left:=Screen.Width - Panel.Width - FRight;
 ToPos:=Screen.Height - Panel.Height - FBottom;
 while Panel.Top > ToPos do
  begin
   Incr:=Incr+2;
   Panel.Top:=Panel.Top - Incr;
   AnimateWait;
  end;
 Panel.Top:=ToPos;

 FTimerHide.Enabled:=True;
end;

procedure TNotifyWindow.ShowTextMessage;
begin
 MessageBox(Application.Handle, PWideChar(FLastNotify.Text), PWideChar(FLastNotify.Caption), MB_ICONQUESTION or MB_OK);
end;

procedure TNotifyWindow.Update;
begin
 if FVisible then ShowNotify;
end;

procedure TNotifyWindow.OnClickClose(Sender: TObject);
begin
 Close;
end;

procedure TNotifyWindow.OnClickText(Sender: TObject);
begin
 ShowTextMessage;
end;

procedure TNotifyWindow.OnTimerHide(Sender: TObject);
begin
 if Panel.MouseInClient or PtInRect(Panel.ClientRect, Panel.ScreenToClient(Mouse.CursorPos)) then Exit;
 HideNotify;
end;

procedure TNotifyWindow.Error(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='×';
 FLastNotify.Color:=clMaroon;
 ShowNotify;
end;

function TNotifyWindow.GetFontCaption: TFont;
begin
 Result:=FLabelCaption.Font;
end;

function TNotifyWindow.GetFontText: TFont;
begin
 Result:=FLabelText.Font;
end;

procedure TNotifyWindow.Info(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='i';
 FLastNotify.Color:=$00FFCC66;
 ShowNotify;
end;

procedure TNotifyWindow.Warning(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='!';
 FLastNotify.Color:=$000066FF;
 ShowNotify;
end;

procedure TNotifyWindow.SetColor(const Value: TColor);
begin
 FColor:=Value;
 FPanel.Color:=FColor;
end;

procedure TNotifyWindow.SetFontCaption(const Value: TFont);
begin
 FLabelCaption.Font:=Value;
end;

procedure TNotifyWindow.SetFontText(const Value: TFont);
begin
 FLabelText.Font:=Value;
end;

procedure TNotifyWindow.SetPanel(const Value: TForm);
begin
 FPanel:=Value;
 FPanel.Visible:=False;
 FPanel.Width:=350;
 FPanel.Height:=93;
 FPanel.Color:=FColor;
 FPanel.Left:=Screen.Width - FPanel.Width - FRight;
 FPanel.Top:=Screen.Height;
end;

{ TNotifyPanel }

constructor TNotifyPanel.Create(AOwner: TForm);
begin
 FOwner:=AOwner;
 FOldSize.Height:=FOwner.ClientHeight;
 FOldSize.Width:=FOwner.ClientWidth;
 FVisible:=False;
 FRight:=20;
 FBottom:=20;
 HideLastNotify;
 FPanel:=TPanel.Create(FOwner);
 with FPanel do
  begin
   //FormStyle:=fsStayOnTop;
   FPanel.Parent:=FOwner;
   Visible:=False;
   Caption:='';
   DoubleBuffered:=True;
   BorderStyle:=bsNone;
   BevelInner:=bvNone;
   BevelKind:=bkNone;
   BevelOuter:=bvNone;
   Left:=0;
   Top:=0;
  end;
 SetPanel(FPanel);
 Color:=clGray;

 FPanelLeft:=TPanel.Create(FPanel);
 with FPanelLeft do
  begin
   Parent:=FPanel;
   Align:=alLeft;
   BevelOuter:=bvNone;
   ParentBackground:=False;
   ParentColor:=False;
   Color:=clMaroon;
   Caption:='!';
   Font.Size:=45;
   Width:=FPanel.Height;
  end;

 FPanelClient:=TPanel.Create(FPanel);
 with FPanelClient do
  begin
   Parent:=FPanel;
   Align:=alClient;
   BevelOuter:=bvNone;
   ParentBackground:=False;
   ParentColor:=True;
   //Color:=FColor;
   Caption:='';
  end;

 FPanelTop:=TPanel.Create(FPanelClient);
 with FPanelTop do
  begin
   Parent:=FPanelClient;
   Height:=25;
   Align:=alTop;
   BevelOuter:=bvNone;
   ParentBackground:=True;
   ParentColor:=True;
   Caption:='';
  end;

 FLabelCaption:=TLabel.Create(FPanelTop);
 with FLabelCaption do
  begin
   Parent:=FPanelTop;
   AlignWithMargins:=True;
   Align:=alClient;
   WordWrap:=False;
   OnClick:=OnClickText;
   Cursor:=crHandPoint;
   Font.Size:=12;
   Font.Color:=clWhite;
   Caption:='';
  end;

 FButtonClose:=TSpeedButton.Create(FPanelTop);
 with FButtonClose do
  begin
   Parent:=FPanelTop;
   Height:=25;
   Width:=25;
   Align:=alRight;
   Caption:='×';
   Font.Color:=clWhite;
   Font.Size:=14;
   Font.Style:=[fsBold];
   Flat:=True;
   OnClick:=OnClickClose;
  end;

 FLabelText:=TLabel.Create(FPanelClient);
 with FLabelText do
  begin
   Parent:=FPanelClient;
   AlignWithMargins:=True;
   Align:=alClient;
   Margins.Bottom:=10;
   Margins.Right:=10;
   Margins.Left:=10;
   Margins.Top:=3;
   Font.Size:=9;
   Font.Color:=clWhite;
   OnClick:=OnClickText;
   Cursor:=crHandPoint;
   Caption:='';
   WordWrap:=TRue;
  end;

 FTimerHide:=TTimer.Create(nil);
 FTimerHide.Enabled:=False;
 FTimerHide.Interval:=3500;
 FTimerHide.OnTimer:=OnTimerHide;
end;

procedure TNotifyPanel.AnimateWait;
begin
 Application.ProcessMessages;
 Sleep(5);
end;

procedure TNotifyPanel.BeforeShow;
begin
 if FVisible then HideNotify;
end;

procedure TNotifyPanel.Close;
begin
 if FVisible then HideNotify;
end;

procedure TNotifyPanel.HideLastNotify;
begin
 FLastNotify.Empty:=True;
end;

procedure TNotifyPanel.HideNotify;
var Incr:Integer;
begin
 FTimerHide.Enabled:=False;
 Incr:=2;
 while Panel.Top < FOwner.ClientHeight do
  begin
   Incr:=Incr+2;
   Panel.Top:=Panel.Top + Incr;
   AnimateWait;
  end;
 Panel.Top:=FOwner.ClientHeight;
 Panel.Visible:=False;
 FVisible:=False;
end;

procedure TNotifyPanel.ShowLastNotify;
begin
 if FLastNotify.Empty then Exit;
 ShowNotify;
end;

procedure TNotifyPanel.ShowNotify;
var incr, ToPos:Integer;
begin
 FLabelCaption.Caption:=FLastNotify.Caption;
 FLabelText.Caption:=FLastNotify.Text;
 FPanelLeft.Color:=FLastNotify.Color;
 FPanelLeft.Caption:=FLastNotify.Image;
 FVisible:=True;
 FTimerHide.Enabled:=False;
 Panel.Visible:=True;
 Panel.BringToFront;
 Incr:=2;

 Panel.Left:=FOwner.ClientWidth - Panel.Width - FRight;
 ToPos:=FOwner.ClientHeight - Panel.Height - FBottom;
 while Panel.Top > ToPos do
  begin
   Incr:=Incr+2;
   Panel.Top:=Panel.Top - Incr;
   AnimateWait;
  end;
 Panel.Top:=ToPos;

 FTimerHide.Enabled:=True;
end;

procedure TNotifyPanel.ShowTextMessage;
begin
 MessageBox(Application.Handle, PWideChar(FLastNotify.Text), PWideChar(FLastNotify.Caption), MB_ICONQUESTION or MB_OK);
end;

procedure TNotifyPanel.Update;
begin
 if FVisible then ShowNotify;
end;

procedure TNotifyPanel.UpdateGlobalSize;
begin
 FPanel.Left:=FPanel.Left+(FOwner.ClientWidth - FOldSize.Width);
 FPanel.Top:=FPanel.Top+(FOwner.ClientHeight - FOldSize.Height);
 FOldSize.Width:=FOwner.ClientWidth;
 FOldSize.Height:=FOwner.ClientHeight;
end;

procedure TNotifyPanel.Ok(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='i';
 FLastNotify.Color:=$0000E686;
 ShowNotify;
end;

procedure TNotifyPanel.OnClickClose(Sender: TObject);
begin
 Close;
end;

procedure TNotifyPanel.OnClickText(Sender: TObject);
begin
 ShowTextMessage;
end;

procedure TNotifyPanel.OnTimerHide(Sender: TObject);
begin
 if Panel.MouseInClient or PtInRect(Panel.ClientRect, Panel.ScreenToClient(Mouse.CursorPos)) then Exit;
 HideNotify;
end;

procedure TNotifyPanel.Error(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='!';
 FLastNotify.Color:=clMaroon;
 ShowNotify;
end;

function TNotifyPanel.GetFontCaption: TFont;
begin
 Result:=FLabelCaption.Font;
end;

function TNotifyPanel.GetFontText: TFont;
begin
 Result:=FLabelText.Font;
end;

procedure TNotifyPanel.Info(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='i';
 FLastNotify.Color:=$00FFCC66;
 ShowNotify;
end;

procedure TNotifyPanel.Warning(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='!';
 FLastNotify.Color:=$000066FF;
 ShowNotify;
end;

procedure TNotifyPanel.SetColor(const Value: TColor);
begin
 FColor:=Value;
 FPanel.Color:=FColor;
end;

procedure TNotifyPanel.SetFontCaption(const Value: TFont);
begin
 FLabelCaption.Font:=Value;
 FButtonClose.Font.Color:=Value.Color;
end;

procedure TNotifyPanel.SetFontText(const Value: TFont);
begin
 FLabelText.Font:=Value;
end;

procedure TNotifyPanel.SetPanel(const Value: TPanel);
begin
 FPanel:=Value;
 FPanel.Visible:=False;
 FPanel.Width:=350;
 FPanel.Height:=93;
 FPanel.Color:=FColor;
 FPanel.Left:=(FOwner.ClientWidth - FPanel.Width - FRight);
 FPanel.Top:=FOwner.ClientHeight;
 FOldSize.Width:=FOwner.ClientWidth;
 FOldSize.Height:=FOwner.ClientHeight;
end;

procedure TNotifyPanel.SetRight(const Value: Integer);
begin
 FRight:=Value;
end;

end.
