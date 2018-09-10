unit LKDU.Edit.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, TableDraw,
  System.Generics.Collections;

type
  TFormList = class(TForm)
    TableEx1: TTableEx;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormList: TFormList;

implementation

{$R *.dfm}

end.
