unit HGM.Common.Notify;

interface
 uses
  Winapi.Windows, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.Buttons, System.SysUtils;

 type
  TNotifyControl = class(TWinControl)
   property Color;
  end;

  TNotify = class(TComponent)
   public type
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
    FOldSize:TSize;
    FOwner:TForm;
    FPanel:TWinControl;
    FPanelClient:TPanel;
    FPanelLeft:TPanel;
    FPanelTop:TPanel;
    FRight:Integer;
    FTime:Cardinal;
    FTimerHide:TTimer;
    FVisible:Boolean;

    procedure SetOwner(const Value: TForm); virtual;
    function GetFontCaption: TFont; virtual;
    function GetFontText: TFont; virtual;
    procedure AnimateWait; virtual;
    procedure BeforeShow; virtual;
    procedure OnClickClose(Sender:TObject); virtual;
    procedure OnClickText(Sender:TObject); virtual;
    procedure SetColor(const Value: TColor); virtual;
    procedure SetFontCaption(const Value: TFont); virtual;
    procedure SetFontText(const Value: TFont); virtual;
    procedure SetPanel(const Value: TWinControl); virtual;
    procedure SetRight(const Value: Integer); virtual;
    procedure SetTime(const Value: Cardinal); virtual;
    procedure OnTimerHide(Sender:TObject); virtual;
    procedure ShowNotify; virtual; abstract;
    procedure UpdatePanel; virtual; abstract;
   public
    property PanelLeft:TPanel read FPanelLeft;
    property PanelClient:TPanel read FPanelClient;
    property Panel:TWinControl read FPanel write SetPanel;

    property LabelCaption:TLabel read FLabelCaption;
    property LabelText:TLabel read FLabelText;
    property LastNotify:TLastRecord read FLastNotify;

    procedure Close; virtual;
    procedure HideLastNotify; virtual;
    procedure HideNotify; virtual; abstract;
    procedure ShowLastNotify; virtual;
    procedure ShowTextMessage; virtual;
    procedure Update; virtual;
    procedure UpdateGlobalSize; virtual;

    procedure Error(Caption, Text:string); virtual;
    procedure Info(Caption, Text:string); virtual;
    procedure Ok(Caption, Text: string); virtual;
    procedure Warning(Caption, Text:string); virtual;

    constructor Create(AOwner: TComponent); override;
   published
    property Bottom:Integer read FBottom write FBottom default 20;
    property Color:TColor read FColor write SetColor default clGray;
    property FontCaption:TFont read GetFontCaption write SetFontCaption;
    property FontText:TFont read GetFontText write SetFontText;
    property OwnerForm:TForm read FOwner write SetOwner;
    property Right:Integer read FRight write SetRight default 20;
    property Time:Cardinal read FTime write SetTime default 5;
  end;

  TNotifyWindow = class(TNotify)
   private
    procedure HideNotify; override;
    procedure ShowNotify; override;
    procedure UpdatePanel; override;
    procedure SetOwner(const Value: TForm); override;
   public
    constructor Create(AOwner: TComponent); override;
  end;

  TNotifyPanel = class(TNotify)
   private
    procedure HideNotify; override;
    procedure ShowNotify; override;
    procedure UpdatePanel; override;
   public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
 RegisterComponents('HGM Components', [TNotifyWindow, TNotifyPanel]);
end;

{ TNotify }

constructor TNotify.Create(AOwner: TComponent);
begin
 inherited;
 FVisible:=False;
 FRight:=20;
 FBottom:=20;
 HideLastNotify;
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
 FTimerHide.OnTimer:=OnTimerHide;
 //default
 Time:=5;
end;

procedure TNotify.AnimateWait;
begin
 Application.ProcessMessages;
 Sleep(5);
end;

procedure TNotify.BeforeShow;
begin
 if FVisible then HideNotify;
end;

procedure TNotify.Close;
begin
 if FVisible then HideNotify;
end;

procedure TNotify.HideLastNotify;
begin
 FLastNotify.Empty:=True;
end;

procedure TNotify.ShowLastNotify;
begin
 if FLastNotify.Empty then Exit;
 ShowNotify;
end;

procedure TNotify.ShowTextMessage;
begin
 MessageBox(Application.Handle, PWideChar(FLastNotify.Text), PWideChar(FLastNotify.Caption), MB_ICONQUESTION or MB_OK);
end;

procedure TNotify.Update;
begin
 if FVisible then ShowNotify;
end;

procedure TNotify.UpdateGlobalSize;
begin
 FPanel.Left:=FPanel.Left+(FOwner.ClientWidth - FOldSize.Width);
 FPanel.Top:=FPanel.Top+(FOwner.ClientHeight - FOldSize.Height);
 FOldSize.Width:=FOwner.ClientWidth;
 FOldSize.Height:=FOwner.ClientHeight;
end;

procedure TNotify.Ok(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='i';
 FLastNotify.Color:=$0000E686;
 ShowNotify;
end;

procedure TNotify.OnClickClose(Sender: TObject);
begin
 Close;
end;

procedure TNotify.OnClickText(Sender: TObject);
begin
 ShowTextMessage;
end;

procedure TNotify.OnTimerHide(Sender: TObject);
begin
 if TNotifyControl(Panel).MouseInClient or PtInRect(Panel.ClientRect, Panel.ScreenToClient(Mouse.CursorPos)) then Exit;
 HideNotify;
end;

procedure TNotify.Error(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='!';
 FLastNotify.Color:=clMaroon;
 ShowNotify;
end;

function TNotify.GetFontCaption: TFont;
begin
 Result:=FLabelCaption.Font;
end;

function TNotify.GetFontText: TFont;
begin
 Result:=FLabelText.Font;
end;

procedure TNotify.Info(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='i';
 FLastNotify.Color:=$00FFCC66;
 ShowNotify;
end;

procedure TNotify.Warning(Caption, Text: string);
begin
 BeforeShow;
 FLastNotify.Empty:=False;
 FLastNotify.Caption:=Caption;
 FLastNotify.Text:=Text;
 FLastNotify.Image:='!';
 FLastNotify.Color:=$000066FF;
 ShowNotify;
end;

procedure TNotify.SetColor(const Value: TColor);
begin
 FColor:=Value;
 TNotifyControl(FPanel).Color:=FColor;
end;

procedure TNotify.SetFontCaption(const Value: TFont);
begin
 FLabelCaption.Font:=Value;
 FButtonClose.Font.Color:=Value.Color;
end;

procedure TNotify.SetFontText(const Value: TFont);
begin
 FLabelText.Font:=Value;
end;

procedure TNotify.SetOwner(const Value: TForm);
begin
 FOwner:=Value;
 FPanel.Parent:=FOwner;
 UpdatePanel;
end;

procedure TNotify.SetPanel(const Value: TWinControl);
begin
 FPanel:=Value;
 UpdatePanel;
end;

procedure TNotify.SetRight(const Value: Integer);
begin
 FRight:=Value;
end;

procedure TNotify.SetTime(const Value: Cardinal);
begin
 FTime:=Value;
 FTimerHide.Interval:=FTime * 1000;
end;

{ TNotifyWindow }

constructor TNotifyWindow.Create(AOwner: TComponent);
begin
 FPanel:=TForm.Create(nil);
 with TForm(FPanel) do
  begin
   FormStyle:=fsStayOnTop;
   Visible:=False;
   Caption:='Notify';
   DoubleBuffered:=True;
   BorderStyle:=bsNone;
  end;
 inherited;
end;

procedure TNotifyWindow.HideNotify;
var Incr:Integer;
begin
 FTimerHide.Enabled:=False;
 Incr:=2;
 while TForm(Panel).Top < Screen.Height do
  begin
   Incr:=Incr+2;
   TForm(Panel).Top:=TForm(Panel).Top + Incr;
   AnimateWait;
  end;
 TForm(Panel).Top:=Screen.Height;
 TForm(Panel).Visible:=False;
 FVisible:=False;
end;

procedure TNotifyWindow.SetOwner(const Value: TForm);
begin
 FOwner:=Value;
 FOldSize.Height:=FOwner.ClientHeight;
 FOldSize.Width:=FOwner.ClientWidth;
 UpdatePanel;
end;

procedure TNotifyWindow.ShowNotify;
var incr, ToPos:Integer;
begin
 FLabelCaption.Caption:=FLastNotify.Caption;
 FLabelText.Caption:=FLastNotify.Text;
 FPanelLeft.Color:=FLastNotify.Color;
 FVisible:=True;
 FTimerHide.Enabled:=False;
 TForm(Panel).Visible:=True;
 TForm(Panel).BringToFront;
 Incr:=2;

 TForm(Panel).Left:=Screen.Width - TForm(Panel).Width - FRight;
 ToPos:=Screen.Height - TForm(Panel).Height - FBottom;
 while TForm(Panel).Top > ToPos do
  begin
   Incr:=Incr+2;
   TForm(Panel).Top:=TForm(Panel).Top - Incr;
   AnimateWait;
  end;
 TForm(Panel).Top:=ToPos;

 FTimerHide.Enabled:=True;
end;

procedure TNotifyWindow.UpdatePanel;
begin
 FPanel.Visible:=False;
 FPanel.Width:=350;
 FPanel.Height:=93;
 TNotifyControl(FPanel).Color:=FColor;
 FPanel.Left:=Screen.Width - FPanel.Width - FRight;
 FPanel.Top:=Screen.Height;
end;

{ TNotifyPanel }

constructor TNotifyPanel.Create(AOwner: TComponent);
begin

 FPanel:=TPanel.Create(nil);
 with TPanel(FPanel) do
  begin
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
 if AOwner is TForm then OwnerForm:=TForm(AOwner);
 inherited Create(AOwner);
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

procedure TNotifyPanel.ShowNotify;
var incr, ToPos:Integer;
begin
 if not Assigned(FOwner) then raise Exception.Create('Необходимо указать форму владельца (OwnerForm)');
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

procedure TNotifyPanel.UpdatePanel;
begin
 FPanel.Visible:=False;
 FPanel.Width:=350;
 FPanel.Height:=93;
 TNotifyControl(FPanel).Color:=FColor;
 if Assigned(FOwner) then
  begin
   FPanel.Left:=(FOwner.ClientWidth - FPanel.Width - FRight);
   FPanel.Top:=FOwner.ClientHeight;
   FOldSize.Width:=FOwner.ClientWidth;
   FOldSize.Height:=FOwner.ClientHeight;
  end;
end;

end.
