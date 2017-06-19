program LccThrottle;

uses
  System.StartUpCopy,
  FMX.Forms,
  LccThrottleMain in 'LccThrottleMain.pas' {TabbedwithNavigationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTabbedwithNavigationForm, TabbedwithNavigationForm);
  Application.Run;
end.
