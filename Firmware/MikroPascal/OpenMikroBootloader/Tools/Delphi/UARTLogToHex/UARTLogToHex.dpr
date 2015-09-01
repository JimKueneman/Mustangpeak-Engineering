program UARTLogToHex;

uses
  Forms,
  UARTLogToHexmainForm in 'UARTLogToHexmainForm.pas' {frmUARTLogToHex};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmUARTLogToHex, frmUARTLogToHex);
  Application.Run;
end.
