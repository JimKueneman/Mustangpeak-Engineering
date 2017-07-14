program LayoutBuilder;

{$R *.res}

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {FormLayoutBuilder},
  mustangpeak.tracksegment in 'mustangpeak.tracksegment.pas',
  mustangpeak.dragmanager in 'mustangpeak.dragmanager.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormLayoutBuilder, FormLayoutBuilder);
  Application.Run;
end.
