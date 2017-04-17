unit lcc.transfer;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{.$UNDEF ULTIBO}

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  syncobjs,
    {$IFDEF SYNAPSE}
      blcksock,
      synsock,
    {$ENDIF}
    {$IFDEF ULTIBO}
    Winsock2,
    GlobalConst,
    {$ENDIF}
  {$ENDIF}
  lcc.node,
  lcc.message,
  mustangpeak.threadedcirculararray,
  lcc.transfer.gridconnect.message_assembler_disassembler;

{$IFNDEF SYNAPSE}
const
  MAX_TCP_BUFFER_LEN = 1024;
{$ENDIF}

type
   {$IFDEF SYNAPSE}
     TLccTCPSocket = TTCPBlockSocket;
   {$ELSE}


     { TLccWinSock2TCPBuffer }

     TLccWinSock2TCPBuffer = class
     private
       function GetByte: Byte;
       function GetEmpty: Boolean;
       function GetFull: Boolean;
     protected
       Buffer: array[0..MAX_TCP_BUFFER_LEN-1] of Byte;
       Head: Integer;
       Tail: Integer;
       function NextHead: Integer;
       function NextTail: Integer;
     public
       property Empty: Boolean read GetEmpty;
       property Full: Boolean read GetFull;
       property Next: Byte read GetByte;

       procedure Store(AByte: Byte);
     end;

     { TLccWinsock2TCPSocket }

     TLccWinsock2TCPSocket = class(TWinsock2TCPSocket)
     private
       FBuffer: TLccWinSock2TCPBuffer;
       FPeerAddress: String;
       FPeerPort: Word;
       FSocket: TSocket;
       procedure SetSocket(AValue: TSocket);
     protected
       property Buffer: TLccWinSock2TCPBuffer read FBuffer write FBuffer;
     public
       constructor Create;
       destructor Destroy; override;

       function Accept: TSocket;
       procedure Bind(IP, Port: string); overload;
       function CanRead(Timeout: Integer): Boolean;
       procedure Connect(IP, Port: string);
       function LastErrorDesc: string;
       procedure Listen; overload;
       function RecvByte(Timeout: Integer): Byte;
       procedure SendString(Data: AnsiString);

       property PeerPort:Word read FPeerPort;
       property PeerAddress:String read FPeerAddress;
       property Socket: TSocket read FSocket write SetSocket;
     end;

     TLccTCPSocket = TLccWinsock2TCPSocket;
   {$ENDIF}
  // This is a base class that is used to descend from in order to create a new type
  // of transfer type

  TTransferDirection = (td_In, td_Out);

  { TLccTransferThread }

  TLccTransferThread = class(TThread)
  private
    FBuffer: TThreadedCirularArrayObject;
    FDone: Boolean;
    FEvent: TSimpleEvent;
    FFreeSocketOnTerminate: Boolean;
    FFreeTransferThreadOnTerminate: TLccTransferThread;
    FTransferDirection: TTransferDirection;
    FSocketReference: TLccTCPSocket;
  protected
    function DispatchedToInternalNode(AMessage: TLccMessage): Boolean;
    procedure Execute; override;
    function TransferMessageToWire(AMessage: TLccMessage): Boolean; virtual;
    function TransferWireToMessage(AByte: Byte; var AMessage: TLccMessage; var SendAsError: Boolean): Boolean; virtual;
    property SocketReference: TLccTCPSocket read FSocketReference write FSocketReference;
    property Event: TSimpleEvent read FEvent write FEvent;
    property TransferDirection: TTransferDirection read FTransferDirection write FTransferDirection;
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TLccTCPSocket; ATransferDirection: TTransferDirection); reintroduce; virtual;
    destructor Destroy; override;
    property Buffer: TThreadedCirularArrayObject read FBuffer write FBuffer;
    property Done: Boolean read FDone write FDone;
    property FreeTransferThreadOnTerminate: TLccTransferThread read FFreeTransferThreadOnTerminate write FFreeTransferThreadOnTerminate;
    property FreeSocketOnTerminate: Boolean read FFreeSocketOnTerminate write FFreeSocketOnTerminate;
  end;

   TLccTransferThreadClass = class of TLccTransferThread;

implementation

var
  TransferThreadCount: Integer;

{ TLccWinSock2TCPBuffer }

function TLccWinSock2TCPBuffer.GetByte: Byte;
begin
  Result := Buffer[Tail];
  Tail := NextTail;
end;

function TLccWinSock2TCPBuffer.GetEmpty: Boolean;
begin
  Result := Head = Tail
end;

function TLccWinSock2TCPBuffer.GetFull: Boolean;
begin
  Result := NextHead <> Tail
end;

function TLccWinSock2TCPBuffer.NextHead: Integer;
begin
  Result := Head + 1;
  if Head >= MAX_TCP_BUFFER_LEN then
    Head := 0;
end;

function TLccWinSock2TCPBuffer.NextTail: Integer;
begin
  Result := Tail + 1;
  if Tail >= MAX_TCP_BUFFER_LEN then
    Tail := 0;
end;

procedure TLccWinSock2TCPBuffer.Store(AByte: Byte);
begin
  if not Full then
  begin
    Buffer[Head] := AByte;
    Head := NextHead;
  end;
end;


{ TLccWinsock2TCPSocket }
{$IFNDEF SYNAPSE}
procedure TLccWinsock2TCPSocket.SetSocket(AValue: TSocket);
var
  SockAddr, PeerAddr:PSockAddr;
  SockAddrLength, PeerAddrLength:Integer;
begin
  if FSocket=AValue then Exit;

  PeerAddr := AllocateAddress(PeerAddrLength);
  try
    if Winsock2.getpeername(Socket, PeerAddr^, PeerAddrLength) = S_OK then
    begin
      SockAddr := AllocateAddress(SockAddrLength);
      try
        if Winsock2.getsockname(AValue, SockAddr^, SockAddrLength) = S_OK then
        begin
          FPeerAddress := SockAddrToAddress(PeerAddr, PeerAddrLength);
          FPeerPort := SockAddrToPort(PeerAddr, PeerAddrLength);
          BoundAddress := SockAddrToAddress(SockAddr, SockAddrLength);
          BoundPort := SockAddrToPort(SockAddr, SockAddrLength);
          FSocket := AValue;
        end else
          FLastError := Winsock2.WSAGetLastError
      finally
        ReleaseAddress(SockAddr, SockAddrLength);
      end;
    end else
      FLastError := Winsock2.WSAGetLastError
  finally
    ReleaseAddress(PeerAddr, PeerAddrLength);
  end
end;

constructor TLccWinsock2TCPSocket.Create;
begin
  inherited Create;
  FBuffer := TLccWinSock2TCPBuffer.Create;
end;

destructor TLccWinsock2TCPSocket.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

function TLccWinsock2TCPSocket.RecvByte(Timeout: Integer): Byte;
begin
  Result := 0;
  if not Buffer.Empty then
    Result := Buffer.Next;
end;

function TLccWinsock2TCPSocket.LastErrorDesc: string;
begin
  // Wrapper to look like Synapse
  case ErrorCode of
    0:
      Result := '';
    WSAEINTR: {10004}
      Result := 'Interrupted system call';
    WSAEBADF: {10009}
      Result := 'Bad file number';
    WSAEACCES: {10013}
      Result := 'Permission denied';
    WSAEFAULT: {10014}
      Result := 'Bad address';
    WSAEINVAL: {10022}
      Result := 'Invalid argument';
    WSAEMFILE: {10024}
      Result := 'Too many open files';
    WSAEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    WSAEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    WSAEALREADY: {10037}
      Result := 'Operation already in progress';
    WSAENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    WSAEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    WSAEMSGSIZE: {10040}
      Result := 'Message too long';
    WSAEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    WSAENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    WSAEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    WSAEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    WSAEADDRINUSE: {10048}
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    WSAENETDOWN: {10050}
      Result := 'Network is down';
    WSAENETUNREACH: {10051}
      Result := 'Network is unreachable';
    WSAENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    WSAECONNRESET: {10054}
      Result := 'Connection reset by peer';
    WSAENOBUFS: {10055}
      Result := 'No Buffer space available';
    WSAEISCONN: {10056}
      Result := 'Socket is already connected';
    WSAENOTCONN: {10057}
      Result := 'Socket is not connected';
    WSAESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    WSAETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    WSAETIMEDOUT: {10060}
      Result := 'Connection timed out';
    WSAECONNREFUSED: {10061}
      Result := 'Connection refused';
    WSAELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG: {10063}
      Result := 'File name is too long';
    WSAEHOSTDOWN: {10064}
      Result := 'Host is down';
    WSAEHOSTUNREACH: {10065}
      Result := 'No route to host';
    WSAENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSAEUSERS: {10068}
      Result := 'Too many users';
    WSAEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    WSAESTALE: {10070}
      Result := 'Stale NFS file handle';
    WSAEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'Disconnect';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
  else
    Result := 'Other Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
end;

procedure TLccWinsock2TCPSocket.SendString(Data: AnsiString);
begin
  // Wrapper to look like Synapse
end;

procedure TLccWinsock2TCPSocket.Bind(IP, Port: string);
begin
  BoundAddress := IP;
  BoundPort := StrToInt(Port);
  FSocket := Bind;
end;

procedure TLccWinsock2TCPSocket.Listen;
begin
  Self.Listen(SOMAXCONN);
end;

function TLccWinsock2TCPSocket.CanRead(Timeout: Integer): Boolean;
begin
  // Synapse uses "Select" [x := synsock.Select(FSocket + 1, @FDSet, nil, nil, TimeVal)]
  // To check for a Read
//  ReadAvailable();
// Result := Winsock2.CanRead(Timeout)

  .;

  Result := not Buffer.Empty;
end;

function TLccWinsock2TCPSocket.Accept: TSocket;
var
  SockAddr:PSockAddr;
  SockAddrLength:Integer;
begin
  Result := INVALID_SOCKET;
  SockAddrLength := 0;
  SockAddr := AllocateAddress(SockAddrLength);
  try
    if Assigned(SockAddr) then
    begin
      FLastError := ERROR_SUCCESS;
      Result := Winsock2.accept(Self.Handle, SockAddr, SockAddrLength);
      if Result = INVALID_SOCKET then
       FLastError := Winsock2.WSAGetLastError
    end;
  finally
    ReleaseAddress(SockAddr, SockAddrLength);
  end;
end;

procedure TLccWinsock2TCPSocket.Connect(IP, Port: string);
begin
  BoundPort := StrToInt(Port);
  BoundAddress := IP;
  FSocket := AllocateSocket(AF_INET);
end;

{$ENDIF}

{ TLccTransferThread }

constructor TLccTransferThread.Create(CreateSuspended: Boolean; ASocket: TLccTCPSocket; ATransferDirection: TTransferDirection);
begin
  inherited Create(CreateSuspended, DefaultStackSize);
  InterLockedIncrement(TransferThreadCount);
  FTransferDirection := ATransferDirection;
  FSocketReference := ASocket;
  Buffer := TThreadedCirularArrayObject.Create;
  Event := TSimpleEvent.Create;
end;

destructor TLccTransferThread.Destroy;
begin
  FreeAndNil(FEvent);
  FreeAndNil(FBuffer);
  InterLockedDecrement(TransferThreadCount);
  inherited Destroy;
end;

function TLccTransferThread.DispatchedToInternalNode(AMessage: TLccMessage): Boolean;
begin
  Result := False;
  // Do not reuse the Message object, it will be destroyed on return
end;

procedure TLccTransferThread.Execute;
var
  Messages: TDynamicArrayObject;
  i: Integer;
  SendToWire, SendReceiveError: Boolean;
  Node: TLccNode;
  LccMessage: TLccMessage;
  NextByte: Byte;
  s: string;
  Code: Integer;

begin
  while not Terminated do
  begin
    case TransferDirection of
      td_Out :
        begin
          case GlobalSendEvent.WaitFor(INFINITE) of
            wrSignaled  : // Event was signaled (triggered)
              begin
                 // Need to run the global list looking for
                if not Terminated then
                begin
                  GlobalNodeList.LockArray;
                  try
                    Node := GlobalNodeList.FirstObject as TLccNode;
                    while Assigned(Node) do
                    begin
                      Messages := nil;

                      // Pull any messages being sent out of the current Node
                      Node.MsgQueueSending.LockArray;
                      try
                        GlobalSendEvent.ResetEvent;     // reset in the lock that way we know it cant be set under us
                        Node.MsgQueueSending.RemoveChunk(Messages);
                      finally
                        Node.MsgQueueSending.UnLockArray;
                      end;

                      for i := 0 to Length(Messages) - 1 do
                      begin
                        // If internal node then send the message back to our internal node
                        // if it was for an internal node only send broadcast messages to the wire..
                        SendToWire := True;
                        if DispatchedToInternalNode(Messages[i] as TLccMessage) then
                          SendToWire := not (Messages[i] as TLccMessage).IsAddressedMessage;
                        if SendToWire then
                          TransferMessageToWire(Messages[i] as TLccMessage);
                      end;
                      Node := GlobalNodeList.NextObject as TLccNode;
                    end;
                  finally
                    GlobalNodeList.UnLockArray;
                  end;
                  FreeDynamicArrayObjects(Messages);
                end;
              end;
            wrTimeout   : begin; end; // Time-out period expired
            wrAbandoned : begin  end; // Wait operation was abandoned.
            wrError     : begin  end; // An error occurred during the wait operation.
          end;
        end;
      td_In :
        begin
          NextByte := SocketReference.RecvByte(INFINITE);
          if not Terminated then
          begin
            case SocketReference.LastError of
              S_OK : begin
                  LccMessage := nil;
                  SendReceiveError := False;
                  if TransferWireToMessage(NextByte, LccMessage, SendReceiveError) then
                  begin
                    GlobalNodeList.LockArray;
                    try
                      Node := GlobalNodeList.FirstObject as TLccNode;
                      while Assigned(Node) do
                      begin
                        if SendReceiveError then
                          Node.MsgQueueSending.Add(LccMessage.Clone)
                        else
                          Node.MsgQueueReceived.Add(LccMessage.Clone);
                        Node := GlobalNodeList.NextObject as TLccNode;
                      end;
                      FreeAndNil(LccMessage);
                    finally
                      GlobalNodeList.UnLockArray;
                    end;
                  end;
                end
              else
                // Shut down for any error including WSACONNRESET
                s := SocketReference.LastErrorDesc;
                Code := SocketReference.LastError;

                // For a server application this allow the receive thread to own the
                // send thread and associated socket and free then if the client shuts
                // down the connection
                if Assigned(FreeTransferThreadOnTerminate) then
                begin
                  FreeTransferThreadOnTerminate.Terminate;
                  GlobalSendEvent.SetEvent;
                  while not FreeTransferThreadOnTerminate.Done do
                    Sleep(100);
                  FreeAndNil(FFreeTransferThreadOnTerminate);
                end;
                Terminate;
              end;
            end;
        end;
    end;
  end;
  if FreeSocketOnTerminate then
    FreeAndNil(FSocketReference);
  Done := True;
end;

function TLccTransferThread.TransferMessageToWire(AMessage: TLccMessage): Boolean;
begin

end;

function TLccTransferThread.TransferWireToMessage(AByte: Byte; var AMessage: TLccMessage; var SendAsError: Boolean): Boolean;
begin

end;

initialization
  TransferThreadCount := 0;

end.

