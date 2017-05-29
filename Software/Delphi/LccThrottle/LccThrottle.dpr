program LccThrottle;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form1},
  mustangpeak.vkbdhelper in '..\..\Ultibo\Mustangpeak\mustangpeak.vkbdhelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
