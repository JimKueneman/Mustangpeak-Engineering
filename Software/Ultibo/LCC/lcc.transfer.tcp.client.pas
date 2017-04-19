unit lcc.transfer.tcp.client;


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
  lcc.transfer.gridconnect.message_assembler_disassembler,
  lcc.transfer;

type

  { TLccTransferManagerTcpClient }

  TLccTransferManagerTcpClient = class
  private
    FConnected: Boolean;
    FTransferSend: TLccTransferThread;
    FTransferReceive: TLccTransferThread;
    FSocket: TTCPBlockSocket;
  protected
    property TransferSend: TLccTransferThread read FTransferSend write FTransferSend;
    property TransferReceive: TLccTransferThread read FTransferReceive write FTransferReceive;
    property Socket: TTCPBlockSocket read FSocket write FSocket;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    procedure Start(ServerIP: string; Port: Word; Verbose: Boolean; TransferSendClass: TLccTransferThreadClass; TransferReceiveClass: TLccTransferThreadClass);
    property Connected: Boolean read FConnected;
  end;

var
  GlobalTransferManagerTcpClient: TLccTransferManagerTcpClient;

implementation

var
  TcpClientCount: Integer;


{ TLccTransferManagerTcpClient }

constructor TLccTransferManagerTcpClient.Create;
begin
  inherited Create;
  InterLockedIncrement(TcpClientCount);
end;

procedure TLccTransferManagerTcpClient.Close;
begin
  if Connected then
  begin
    // Kill Threads

    // This will keep anything else from accessing the Send/Receive Threads
    TransferSend.Terminate;
    while not TransferSend.Done do
      Sleep(100);
    FreeAndNil(FTransferSend);

    TransferReceive.Terminate;
    Socket.CloseSocket;                               // This should release the Wait on the Receive with an error
    while not TransferReceive.Done do
      Sleep(100);
    FreeAndNil(FTransferReceive);
  end;
end;

destructor TLccTransferManagerTcpClient.Destroy;
begin
  Close;
  FreeAndNil(FSocket);
  InterLockedDecrement(TcpClientCount);
  inherited Destroy;
end;

procedure TLccTransferManagerTcpClient.Start(ServerIP: string; Port: Word;
  Verbose: Boolean; TransferSendClass: TLccTransferThreadClass;
  TransferReceiveClass: TLccTransferThreadClass);
var
  RetryCount: Integer;
  Peer: TVarSin;
begin

  if Verbose then WriteLn('Starting TCP Client: ' + ServerIP + ':' + IntToStr(Port));

  Socket := TTCPBlockSocket.Create;

  Socket.Family := SF_IP4;                  // IP4
  Socket.ConvertLineEnd := True;            // Use #10, #13, or both to be a "string" for GridConnect
  Socket.SetTimeout(0);

  if Verbose then WriteLn('Connecting TCP Client');
  Socket.Connect(String( ServerIP), String( IntToStr(Port)));

  // May not need for Ultibo, this was for Andriod mainly..
  RetryCount := 0;
  while ((Socket.LastError = WSAEINPROGRESS) or (Socket.LastError = WSAEALREADY)) and (RetryCount < 40) do   {20 Second Wait}
  begin
    Socket.ResetLastError;
    if GetPeerName(Socket.Socket, Peer) = 0 then
      Break;
    Inc(RetryCount);
    Sleep(500);
    if Verbose then Write('.');
  end;

  if Socket.LastError = 0 then
  begin
    if Verbose then WriteLn('Starting Threads');
    TransferSend := TransferSendClass.Create(True, Socket, td_Out);
    TransferReceive := TransferReceiveClass.Create(True, Socket, td_In);
    TransferSend.Start;
    TransferReceive.Start;
    FConnected := True;
  end else
    if Verbose then WriteLn('Failed to Connect: ' + Socket.LastErrorDesc);
end;

initialization
  TcpClientCount := 0;
  GlobalTransferManagerTcpClient := TLccTransferManagerTcpClient.Create;

finalization
  FreeAndNil(GlobalTransferManagerTcpClient);

end.
