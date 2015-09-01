program OpenMikroBootloader;

uses
  Forms,
  unitmainform in 'unitmainform.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'OpenMikroBootloader';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
