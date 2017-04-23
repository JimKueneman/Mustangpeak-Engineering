unit lcc.transfer.tcp.server;


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  syncobjs,
  blcksock,
  synsock,
  {$ENDIF}
  lcc.node,
  lcc.message,
  mustangpeak.threadedcirculararray,
  mustangpeak.classes,
  lcc.transfer.gridconnect.message_assembler_disassembler,
  lcc.transfer,
  lcc.transfer.gridconnect.wire.synapse,
  lcc.transfer.tcp.client,
  lcc.utilities;

type

  { TLccTransferManagerTcpListener }

  TLccTransferManagerTcpListener = class(TThread)
    private
    FDone: Boolean;
    FListenerIP: string;
    FListenerPort: string;
    FRunning: Boolean;
    FSocket: TTCPBlockSocket;
    FTransferReceive: TLccTransferThread;
    FTransferReceiveClass: TLccTransferThreadClass;
    FTransferSend: TLccTransferThread;
    FTransferSendClass: TLccTransferThreadClass;
    FUseLoopbackAddress: Boolean;
    FVerbose: Boolean;
  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    property Done: Boolean read FDone write FDone;
    property ListenerIP: string read FListenerIP write FListenerIP;
    property ListenerPort: string read FListenerPort write FListenerPort;
    property Running: Boolean read FRunning write FRunning;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    property TransferSendClass: TLccTransferThreadClass read FTransferSendClass write FTransferSendClass;
    property TransferReceiveClass: TLccTransferThreadClass read FTransferReceiveClass write FTransferReceiveClass;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

  { TLccTransferManagerTcpServer }

  TLccTransferManagerTcpServer = class
  private
    FConnected: Boolean;
    FListener: TLccTransferManagerTcpListener;
    FTransferSend: TLccTransferThread;
    FTransferReceive: TLccTransferThread;
  protected
    property TransferSend: TLccTransferThread read FTransferSend write FTransferSend;
    property TransferReceive: TLccTransferThread read FTransferReceive write FTransferReceive;

    property Listener: TLccTransferManagerTcpListener read FListener write FListener;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(ServerIP: string; Port: Word; Verbose: Boolean; TransferSendClass: TLccTransferThreadClass; TransferReceiveClass: TLccTransferThreadClass);
    property Connected: Boolean read FConnected;
  end;

var
  GlobalTransferManagerTcpServer: TLccTransferManagerTcpServer;

implementation

var
  TcpListenerCount,
  TcpServerCount: Integer;

{ TLccTransferManagerTcpListener }

constructor TLccTransferManagerTcpListener.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited Create(CreateSuspended, StackSize);
  InterLockedIncrement(TcpListenerCount);
end;

destructor TLccTransferManagerTcpListener.Destroy;
begin
  InterLockedDecrement(TcpListenerCount);
  inherited Destroy;
end;

procedure TLccTransferManagerTcpListener.Execute;
var
  ConnectionSocketHandle: TSocket;
  ConnectionSocket: TTCPBlockSocket;
  SendThread: TLccTransferThread;
  ReceiveThread: TLccTransferThread;
  s: string;
begin
   Socket := TTCPBlockSocket.Create;          // Created in context of the thread

   Socket.Family := SF_IP4;                  // IP4
   Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string"
   Socket.HeartbeatRate := 0;
   Socket.SetTimeout(0);

   if Verbose then WriteLn('Starting TCP Server Listener: ' + ListenerIP + ':' + ListenerPort);

   Socket.Bind(String( ListenerIP), ListenerPort);
   if Socket.LastError = 0 then
   begin
     if Verbose then WriteLn('Listener Binding Successful');

     Socket.Listen;

     if Socket.LastError = 0 then
     begin
       if Verbose then WriteLn('Listener Listen Successful');

       Running := True;
       while not Terminated do
       begin

         if Socket.CanRead(500) then
         begin
           ErrorCode := Socket.LastError;
           s := Socket.LastErrorDesc;
           if not Terminated and (Socket.LastError <> WSAETIMEDOUT) then
           begin
             if Socket.LastError = 0 then
             begin
               if Verbose then WriteLn('Listener New Connection');
               ConnectionSocketHandle := Socket.Accept;
               ConnectionSocket := TTCPBlockSocket.Create;
               ConnectionSocket.Socket := ConnectionSocketHandle;
               SendThread := TransferSendClass.Create(True, ConnectionSocket, td_Out, Verbose);
               ReceiveThread := TransferReceiveClass.Create(True, ConnectionSocket, td_In, Verbose);
               // Setup to be able to free both Threads and the socket when an error or connection is lost
               ReceiveThread.FreeOnTerminate := True;
               ReceiveThread.FreeTransferThreadOnTerminate := SendThread;
               ReceiveThread.FreeSocketOnTerminate := True;
               SendThread.Start;
               ReceiveThread.Start;
             end else
               Terminate;
           end
         end;
       end;
     end else
       if Verbose then WriteLn(Socket.LastErrorDesc)
   end else
     if Verbose then WriteLn(Socket.LastErrorDesc);

   Socket.CloseSocket;
   FreeAndNil(FSocket);
   Done := True;
end;

{ TLccTransferManagerTcpListener }



{ TLccTransferManagerTcpServer }

constructor TLccTransferManagerTcpServer.Create;
begin
  inherited Create;
  InterLockedIncrement(TcpServerCount);
end;

destructor TLccTransferManagerTcpServer.Destroy;
begin
  Listener.Terminate;
  while not Listener.Done do
    Sleep(100);
  InterLockedDecrement(TcpServerCount);
  inherited Destroy;
end;

procedure TLccTransferManagerTcpServer.Start(ServerIP: string; Port: Word;
  Verbose: Boolean; TransferSendClass: TLccTransferThreadClass;
  TransferReceiveClass: TLccTransferThreadClass);
var
  Timeout: Integer;
begin
  if Verbose then WriteLn('Starting TCP Server: ' + ServerIP + ':' + IntToStr(Port));
  Listener := TLccTransferManagerTcpListener.Create(True);
  Listener.TransferSendClass := TransferSendClass;
  Listener.TransferReceiveClass := TransferReceiveClass;
  Listener.Verbose := Verbose;
  Listener.ListenerIP := ServerIP;
  Listener.ListenerPort := IntToStr(Port);
  Listener.Start;
  Timeout := 0;
  while not Listener.Running and (Timeout < 100) do
  begin
    Sleep(100);
    Inc(Timeout);
  end;
  FConnected := Listener.Running;
end;

initialization
  TcpListenerCount := 0;
  TcpServerCount := 0;
  GlobalTransferManagerTcpServer := TLccTransferManagerTcpServer.Create;

finalization
  FreeAndNil(GlobalTransferManagerTcpServer);

end.
