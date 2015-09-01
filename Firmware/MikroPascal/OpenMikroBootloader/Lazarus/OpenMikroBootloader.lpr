program OpenMikroBootloader;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unitMainForm, mapform;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFormBootloader, FormBootloader);
  Application.CreateForm(TFormMemoryMap, FormMemoryMap);
  Application.Run;
end.

