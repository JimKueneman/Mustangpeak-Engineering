unit lcc.transfer;


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  syncobjs,
    {$IFNDEF ULTIBO}
      blcksock,
      synsock,
    {$ENDIF}
  {$ENDIF}
  lcc.node, lcc.message, mustangpeak.threadedcirculararray,
  lcc.transfer.gridconnect.message_assembler_disassembler;

type

  // This is a base class that is used to descend from in order to create a new type
  // of transfer type

  TTransferDirection = (td_In, td_Out);

  { TLccTransferThread }

  TLccTransferThread = class(TThread)
  private
    FBuffer: TThreadedCirularArrayObject;
    FDone: Boolean;
    FEvent: TSimpleEvent;
    {$IFDEF ULTIBO}
    {$ELSE}
    FSocket: TTCPBlockSocket;
    FTransferDirection: TTransferDirection;
    {$ENDIF}
  protected
    function DispatchedToInternalNode(AMessage: TLccMessage): Boolean;
    procedure Execute; override;
    function TransferMessageToWire(AMessage: TLccMessage): Boolean; virtual;
    function TransferWireToMessage(AByte: Byte; var AMessage: TLccMessage): Boolean; virtual;
    {$IFDEF ULTIBO}
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property Event: TSimpleEvent read FEvent write FEvent;
    property TransferDirection: TTransferDirection read FTransferDirection write FTransferDirection;
  public
    {$IFDEF ULTIBO}
      constructor Create(CreateSuspended: Boolean); reintroduce;
    {$ELSE}
      constructor Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection); reintroduce; virtual;
    {$ENDIF}
    destructor Destroy; override;
    property Buffer: TThreadedCirularArrayObject read FBuffer write FBuffer;
    property Done: Boolean read FDone write FDone;
  end;

  TLccTransferThreadClass = class of TLccTransferThread;

  { TLccTransferManager }

  TLccTransferManager = class
  private
    FConnected: Boolean;
    FTransferSend: TLccTransferThread;
    FTransferReceive: TLccTransferThread;
    {$IFDEF ULTIBO}
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
  protected
    property TransferSend: TLccTransferThread read FTransferSend write FTransferSend;
    property TransferReceive: TLccTransferThread read FTransferReceive write FTransferReceive;
    {$IFDEF ULTIBO}
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    procedure Start(ServerIP: string; Port: Word; Verbose: Boolean; TransferSendClass: TLccTransferThreadClass; TransferReceiveClass: TLccTransferThreadClass);
    property Connected: Boolean read FConnected;
  end;

var
  GlobalTransferManager: TLccTransferManager;

implementation

{ TLccTransferThread }

{$IFDEF ULTIBO}
constructor TLccTransferThread.Create(CreateSuspended: Boolean);
{$ELSE}
constructor TLccTransferThread.Create(CreateSuspended: Boolean;
  ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection);
{$ENDIF}
begin
  inherited Create(CreateSuspended, DefaultStackSize);
  FTransferDirection := ATransferDirection;
  {$IFDEF ULTIBO}
  {$ELSE}
  FSocket := ASocket;
  {$ENDIF}
  Buffer := TThreadedCirularArrayObject.Create;
  Event := TSimpleEvent.Create;
end;

destructor TLccTransferThread.Destroy;
begin
  FreeAndNil(FEvent);
  FreeAndNil(FBuffer);
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
  SendToWire: Boolean;
  Node: TLccNode;
  LccMessage: TLccMessage;
  NextByte: Byte;
begin
  while not Terminated do
  begin
    case TransferDirection of
      td_Out :
        begin
          case GlobalSendEvent.WaitFor(INFINITE) of
            wrSignaled  : // Event was signaled (triggered)
              begin
                GlobalSendEvent.ResetEvent;     // reset BEFORE we remove that way new adds right after we unlock will be caught with the next SetEvent
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
            wrTimeout   : begin beep; end; // Time-out period expired
            wrAbandoned : begin beep end; // Wait operation was abandoned.
            wrError     : begin beep end; // An error occurred during the wait operation.
          end;
        end;
      td_In :
        begin
          NextByte := Socket.RecvByte(INFINITE);
          if not Terminated then
          begin
            if TransferWireToMessage(NextByte, LccMessage) then
            begin
              GlobalNodeList.LockArray;
              try
                Node := GlobalNodeList.FirstObject as TLccNode;
                while Assigned(Node) do
                begin
                  Node.MsgQueueReceived.Add(LccMessage.Clone);
                  Node := GlobalNodeList.NextObject as TLccNode;
                end;
                FreeAndNil(LccMessage);
              finally
                GlobalNodeList.UnLockArray;
              end;
            end;
          end;
        end;
    end;
  end;
  Done := True;
end;

function TLccTransferThread.TransferMessageToWire(AMessage: TLccMessage
  ): Boolean;
begin

end;

function TLccTransferThread.TransferWireToMessage(AByte: Byte;
  var AMessage: TLccMessage): Boolean;
begin
   Result := False;
end;

{ TLccTransferManager }

constructor TLccTransferManager.Create;
begin
  inherited Create;
end;

procedure TLccTransferManager.Close;
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
    {$IFDEF ULTIBO}
    {$ELSE}
    Socket.CloseSocket;                               // This should release the Wait on the Receive with an error
    {$ENDIF}
    while not TransferReceive.Done do
      Sleep(100);
    FreeAndNil(FTransferReceive);
  end;
end;

destructor TLccTransferManager.Destroy;
begin
  Close;
  {$IFDEF ULTIBO}
  {$ELSE}
  FreeAndNil(FSocket);
  {$ENDIF}
  inherited Destroy;
end;

procedure TLccTransferManager.Start(ServerIP: string; Port: Word;
  Verbose: Boolean; TransferSendClass: TLccTransferThreadClass;
  TransferReceiveClass: TLccTransferThreadClass);
{$IFDEF ULTIBO}
begin

end;
{$ELSE}
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
    if Verbose then WriteLn('Failed to Connect: ' + Socket.GetErrorDesc(Socket.LastError));
end;
{$ENDIF}

initialization
  GlobalTransferManager := TLccTransferManager.Create;

finalization
  FreeAndNil(GlobalTransferManager);

end.
