unit Samples.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, HGM.Controls.ColorGrid,
  HGM.Controls.VirtualTable, System.Generics.Collections, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TForm8 = class(TForm)
    hColorGrid1: ThColorGrid;
    Panel1: TPanel;
    Button1: TButton;
    ColorDialog1: TColorDialog;
    ColorGrid1: ThColorGrid;
    procedure FormCreate(Sender: TObject);
    procedure hColorGrid1Select(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.Button1Click(Sender: TObject);
begin
 if ColorDialog1.Execute then hColorGrid1.SelectedColor:=ColorDialog1.Color;
end;

procedure TForm8.FormCreate(Sender: TObject);
var Column:TColorColumn;
begin
 hColorGrid1.ColorColumns.Clear;
 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create(clWhite));
 Column.Add(TColorItem.Create($00CCCCCC));
 Column.Add(TColorItem.Create($00A5A5A5));
 Column.Add(TColorItem.Create($00666666));
 Column.Add(TColorItem.Create($00333333));
 Column.Add(TColorItem.Create(clBlack));
 hColorGrid1.ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create(clRed));
 Column.Add(TColorItem.Create($0000C0FF));
 Column.Add(TColorItem.Create(clYellow));
 Column.Add(TColorItem.Create($0050B000));
 Column.Add(TColorItem.Create($00BB4D00));
 Column.Add(TColorItem.Create($00D3009B));
 hColorGrid1.ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create($004D50C0));
 Column.Add(TColorItem.Create($004696F7));
 Column.Add(TColorItem.Create($0059BB9B));
 Column.Add(TColorItem.Create($00C6AC4B));
 Column.Add(TColorItem.Create($00BD814F));
 Column.Add(TColorItem.Create($00A26480));
 hColorGrid1.ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create($004963D1));
 Column.Add(TColorItem.Create($004990D1));
 Column.Add(TColorItem.Create($0000B4CC));
 Column.Add(TColorItem.Create($008CB08F));
 Column.Add(TColorItem.Create($00866B64));
 Column.Add(TColorItem.Create($007C7C9E));
 hColorGrid1.ColorColumns.Add(Column);

 Column:=TColorColumn.Create;
 Column.Add(TColorItem.Create($008484DD));
 Column.Add(TColorItem.Create($0047A4F3));
 Column.Add(TColorItem.Create($0004CEDF));
 Column.Add(TColorItem.Create($0092B5A5));
 Column.Add(TColorItem.Create($00C29E80));
 Column.Add(TColorItem.Create($00C0859C));
 hColorGrid1.ColorColumns.Add(Column);

 hColorGrid1.Update;
end;

procedure TForm8.hColorGrid1Select(Sender: TObject);
begin
 if not hColorGrid1.IsSelected then Panel1.Color:=clBtnFace
 else
 Panel1.Color:=hColorGrid1.SelectedColor;
end;

end.
