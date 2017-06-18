program LayoutBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {FormLayoutBuilder},
  mustangpeak.tracksegment in 'mustangpeak.tracksegment.pas',
  mustangpeak.sketchpad.frame in '..\CommonLibrary\mustangpeak.sketchpad.frame.pas' {SketchpadFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLayoutBuilder, FormLayoutBuilder);
  Application.Run;
end.
