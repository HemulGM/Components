unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, HGM.Controls.Chat, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    hChat1: ThChat;
    Panel1: TPanel;
    ButtonLAdd100: TButton;
    ButtonLAdd1: TButton;
    ButtonRAdd1: TButton;
    ButtonRAdd100: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ButtonLAdd100Click(Sender: TObject);
    procedure ButtonLAdd1Click(Sender: TObject);
    procedure ButtonRAdd100Click(Sender: TObject);
    procedure ButtonRAdd1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

function GetRandomText: string;
const
  t1 = 'Lorem ipsum dolor sit amet';
  t2 = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt';
  t3 = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.';
  t4 = 'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur';
  t5 = 'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.';
  t6 = 'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo';
begin
  case Random(6) of
    0:
      Exit(t1);
    1:
      Exit(t2);
    2:
      Exit(t3);
    3:
      Exit(t4);
    4:
      Exit(t5);
    5:
      Exit(t6);
  else
    Result := t1;
  end;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  with hChat1.Items.AddInfo do
  begin
    Text := 'info ' + hChat1.Items.Count.ToString;
  end;
end;

procedure TForm3.ButtonLAdd100Click(Sender: TObject);
begin
  for var i := 1 to 100 do
    with hChat1.Items.AddMessage do
    begin
      case Random(3) of
        0:
          begin
            From := 'Andy';
            FromColor := clRed;
          end;
        1:
          begin
            From := 'Mia';
            FromColor := clGreen;
          end;
        2:
          begin
            From := 'Bert';
            FromColor := clYellow;
          end;
      end;
      FromType := TChatMessageType.mtOpponent;
      Text := GetRandomText;
    end;
end;

procedure TForm3.ButtonLAdd1Click(Sender: TObject);
begin
  with hChat1.Items.AddMessage do
  begin
    From := 'Andy';
    FromColor := clRed;
    FromType := TChatMessageType.mtOpponent;
    Text := GetRandomText;
  end;
end;

procedure TForm3.ButtonRAdd100Click(Sender: TObject);
begin
  for var i := 1 to 100 do
    with hChat1.Items.AddMessage do
    begin
      From := 'HemulGM';
      FromType := TChatMessageType.mtMe;
      Text := GetRandomText;
    end;
end;

procedure TForm3.ButtonRAdd1Click(Sender: TObject);
begin
  with hChat1.Items.AddMessage do
  begin
    From := 'HemulGM';
    FromType := TChatMessageType.mtMe;
    Text := GetRandomText;
  end;
end;

end.

