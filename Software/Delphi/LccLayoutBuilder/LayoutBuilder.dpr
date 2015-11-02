program LayoutBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {FormLayoutBuilder};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormLayoutBuilder, FormLayoutBuilder);
  Application.Run;
end.
