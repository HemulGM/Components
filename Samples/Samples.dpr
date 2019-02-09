program Samples;

uses
  Vcl.Forms,
  Samples.Main in 'Samples.Main.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
