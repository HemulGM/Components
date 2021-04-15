unit Samples.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.Grids, HGM.Controls.ColorGrid, HGM.Controls.VirtualTable, System.Generics.Collections,
  Vcl.ExtCtrls, Vcl.StdCtrls, HGM.Controls.PanelExt, Direct2D, D2D1, HGM.AutoTextType, HGM.Controls.Edit,
  HGM.Controls.Chat, HGM.Button, Vcl.ComCtrls, HGM.Controls.ProgressBar, HGM.Controls.TrackBar;

type
  TForm8 = class(TForm)
    ProgressBar1: TProgressBar;
    hProgrsssBar1: ThProgrsssBar;
    TrackBar1: TTrackBar;
    Panel1: TPanel;
    hTrackbar1: ThTrackbar;
    procedure DrawPanel1Paint(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure hTrackbar1Change(Sender: TObject; Position: Extended);
  private
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

uses
  Math;

{$R *.dfm}

{ Fake sound graph

procedure TForm8.DrawPanel1Paint(Sender: TObject);
var PW, i, Cnt, P1, P2: Integer;
begin
  with TDirect2DCanvas.Create(DrawPanel1.Canvas, DrawPanel1.ClientRect) do
  begin
    PW := 1;
    BeginDraw;
    Pen.Width := PW+1;
    Pen.Color := clHighlight;
    Brush.Color := clBlack;
    FillRect(DrawPanel1.ClientRect);
    P1 := DrawPanel1.Height div 2;
    P2 := DrawPanel1.Height div 2;
    for i := 0 to DrawPanel1.Width div PW do
    begin
      MoveTo(i * PW, DrawPanel1.Height div 2+1);
      P1 := RandomRange(Max(P1-8, 0), Min(P1+8, DrawPanel1.Height div 2-Round(DrawPanel1.Height div 2 / 100 * 10)));
      LineTo(i * PW, P1);

      MoveTo(i * PW, DrawPanel1.Height div 2-1);
      //P2 := RandomRange(Max(P2-10, DrawPanel1.Height div 2), Min(P2+10, DrawPanel1.Height));
      P2 := DrawPanel1.Height div 2 + (DrawPanel1.Height div 2 - P1);
      P2 := RandomRange(Max(P2-4, DrawPanel1.Height div 2), Min(P2+4, DrawPanel1.Height));
      LineTo(i * PW, P2);
    end;
    EndDraw;
    Free;
  end;
end;
}

procedure TForm8.DrawPanel1Paint(Sender: TObject);
var
  Panel: TDrawPanel absolute Sender;
begin
  with Panel.Canvas do
  begin
    Brush.Color := $0020160F;
    FillRect(Panel.ClientRect);
  end;
end;

procedure TForm8.hTrackbar1Change(Sender: TObject; Position: Extended);
begin
  hProgrsssBar1.Position := Round(Position);
end;

procedure TForm8.TrackBar1Change(Sender: TObject);
begin
  hProgrsssBar1.Position := TrackBar1.Position;
end;

end.

