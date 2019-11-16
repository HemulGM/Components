unit Samples.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  HGM.Controls.ColorGrid, HGM.Controls.VirtualTable, System.Generics.Collections,
  Vcl.ExtCtrls, Vcl.StdCtrls, HGM.Controls.PanelExt, Direct2D, D2D1,
  HGM.AutoTextType, HGM.Controls.Edit, HGM.Controls.Chat;

type
  TForm8 = class(TForm)
    hChat1: ThChat;
    procedure DrawPanel1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
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

procedure TForm8.FormCreate(Sender: TObject);
var
  i: Integer;
  j: Integer;
begin
  with hChat1.Items.AddInfo do
  begin
    Text := DateTimeToStr(Now);
  end;
  for i := 0 to 100 do
  begin
    with hChat1.Items.AddMessage do
    begin
      From := 'HGM user';
      if Random(40) in [20..30] then
        FromType := mtMe;
      Text := 'Text body';
      for j := 1 to Random(10) do
        Text := Text + 'Text body';
      if Random(40) = 15 then
        for j := 1 to 150 do
          Text := Text + 'Text body';
      Text := DateTimeToStr(Now) + #13#10 + Text;
      FromColor := RGB(RandomRange(100, 240), RandomRange(100, 240), RandomRange(100, 240));
    end;
    if i mod 3 = 0 then
      with hChat1.Items.AddInfo do
      begin
        Text := DateTimeToStr(Now);
      end;
  end;
end;

end.

