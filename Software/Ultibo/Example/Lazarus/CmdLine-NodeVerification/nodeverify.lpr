program nodeverify;
{$APPTYPE CONSOLE}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  lcc.transfer.tcp.server,
  lcc.transfer.gridconnect.wire.synapse,
  lcc.transfer,
  lcc.message;

var
  TcpServer: TLccTransferManagerTcpServer;
  IP: string;
  Port: Integer;
  Verbose: Boolean;
  Message: TLccMessage;

type

  { TNodeVerificationApplication }

  TNodeVerificationApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TNodeVerificationApplication }

procedure TNodeVerificationApplication.DoRun;
var
  ErrorMsg: String;
begin
  Writeln('Mustangpeak NodeVerify Tool');

  // quick check parameters
  ErrorMsg := CheckOptions('h v p i', 'help verbose port ip');
  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('i', 'IP Address') then
    IP := GetOptionValue('i')
  else
    IP := '127.0.0.1';

  if HasOption('p', 'Port') then
    Port := StrToInt(GetOptionValue('p'))
  else
    Port := 12021;

  if HasOption('v', 'Verbose') then
    Verbose := True
  else
    Verbose := False;

  WriteLn('IP: ' + IP);
  WriteLn('Port: ' + IntToStr(Port));
  if Verbose then
    WriteLn('Verbose = ' + 'true')
  else
    WriteLn('Verbose = ' + 'false');

  WriteLn('Connecting');
  { add your program here }
  TcpServer := TLccTransferManagerTcpServer.Create;
  TcpServer.Start(IP, 12021, Verbose, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);


  while not TcpServer.Connected do
    Sleep(500);

  WriteLn('Connected');
  HooksEnabled := True;

  while TcpServer.Connected do
  begin
    Sleep(1);
    CheckSynchronize(0);
    HookQueueReceived.LockArray;
    try
       if HookQueueReceived.Count > 0 then
       begin
         Message := HookQueueReceived.FirstObject as TLccMessage;
         while Assigned(Message) do
         begin
           WriteLn(Message.ConvertToGridConnectStr(' '));
           Message := HookQueueReceived.NextObject as TLccMessage;
         end;
       end;
    finally
      HookQueueReceived.Clear;
      HookQueueReceived.UnLockArray;
    end;
  end;
end;

constructor TNodeVerificationApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TNodeVerificationApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TNodeVerificationApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TNodeVerificationApplication;

{$R *.res}

begin
  Application := TNodeVerificationApplication.Create(nil);
  Application.Title := 'NodeVerification';
  Application.Run;
  Application.Free;
end.

