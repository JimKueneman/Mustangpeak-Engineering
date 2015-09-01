program HexToBin;

uses
  Forms,
  fMain in 'fMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HexToBinPIC24FJ';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
