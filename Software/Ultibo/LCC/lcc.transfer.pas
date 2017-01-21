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
  transfer.gridconnect.message_assembler_disassembler;

type

  // This is a base class that is used to descend from in order to create a new type
  // of transfer type

  { TLccTransferThread }

  TLccTransferThread = class(TThread)
  private
    FBuffer: TThreadedCirularArrayInterface;
    FDone: Boolean;
    FEvent: TSimpleEvent;
    {$IFDEF ULTIBO}
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
    FTransfer: TLccTransferThread;
  protected
    {$IFDEF ULTIBO}
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
    property Event: TSimpleEvent read FEvent write FEvent;
    property Transfer: TLccTransferThread read FTransfer write FTransfer;
  public
    {$IFDEF ULTIBO}
      constructor Create(CreateSuspended: Boolean); reintroduce;
    {$ELSE}
      constructor Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket); reintroduce;
    {$ENDIF}
    destructor Destroy; override;
    property Buffer: TThreadedCirularArrayInterface read FBuffer write FBuffer;
    property Done: Boolean read FDone write FDone;
  end;

  TLccTransferThreadClass = class of TLccTransferThread;

  // This is the thread that runs and pumps messages into and grabs them out of
  // created nodes through the decendents of the TLccTransferThread

  { TLccTransferSendMessageDispatcher }

  TLccTransferSendMessageDispatcher = class(TLccTransferThread)
  public
    procedure Execute; override;
  end;

  { TLccTransferReceiveMessageDispatcher }

  TLccTransferReceiveMessageDispatcher = class(TLccTransferThread)
  public
    procedure Execute; override;
  end;


  // The main  manager that handles the threads to for message transfer

  { TLccTransferManager }

  TLccTransferManager = class
  private
    FConnected: Boolean;
    FDispatcherSend: TLccTransferSendMessageDispatcher;
    FDispatcherReceive: TLccTransferReceiveMessageDispatcher;
    {$IFDEF ULTIBO}
    {$ELSE}
    FSocket: TTCPBlockSocket;
    {$ENDIF}
  protected
    property DispatcherSend: TLccTransferSendMessageDispatcher read FDispatcherSend write FDispatcherSend;
    property DispatcherReceive: TLccTransferReceiveMessageDispatcher read FDispatcherReceive write FDispatcherReceive;
    {$IFDEF ULTIBO}
    {$ELSE}
    property Socket: TTCPBlockSocket read FSocket write FSocket;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    procedure Start(ServerIP: string; Port: Word; Verbose: Boolean; TransferSend: TLccTransferThreadClass; TransferReceive: TLccTransferThreadClass);
    property Connected: Boolean read FConnected;
  end;

var
  GlobalTransferManager: TLccTransferManager;

implementation

{ TLccTransferReceiveMessageDispatcher }

procedure TLccTransferReceiveMessageDispatcher.Execute;
var
  Node: TLccNode;
  Messages: TDynamicArrayInterface;
begin
  Transfer.Start;

  while not Terminated do
  begin
    case GlobalReceiveEvent.WaitFor(INFINITE) of
    wrSignaled  : // Event was signaled (triggered)
        begin
           // Need to run the global list
          if not Terminated then
          begin
            // Lock our buffer so we grab a the messages
            Transfer.Buffer.LockArray;
            try
              Messages := nil;
              Transfer.Buffer.RemoveChunk(Messages);
            finally
              Transfer.Buffer.UnLockArray;
            end;

            GlobalNodeList.LockArray;
            try
              Node := GlobalNodeList.FirstObject as TLccNode;
              while Assigned(Node) do
              begin
                Node.MsgQueueReceived.AddChunk(Messages);
                Node := GlobalNodeList.NextObject as TLccNode;
              end;
            finally
              GlobalNodeList.UnLockArray;
            end;
            Messages := nil;   // Release our interface counts

            GlobalReceiveEvent.ResetEvent;
          end;
        end;
      wrTimeout   : begin end; // Time-out period expired
      wrAbandoned : begin end; // Wait operation was abandoned.
      wrError     : begin end; // An error occurred during the wait operation.
    end;
  end;
end;

{ TLccTransferThread }

{$IFDEF ULTIBO}
constructor TLccTransferThread.Create(CreateSuspended: Boolean);
{$ELSE}
constructor TLccTransferThread.Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket);
{$ENDIF}
begin
  inherited Create(CreateSuspended, DefaultStackSize);
  {$IFDEF ULTIBO}
  {$ELSE}
  FSocket := ASocket;
  {$ENDIF}
  Buffer := TThreadedCirularArrayInterface.Create;
  Event := TSimpleEvent.Create;
end;

destructor TLccTransferThread.Destroy;
begin
  FreeAndNil(FEvent);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

{ TLccTransferSendMessageDispatcher }

procedure TLccTransferSendMessageDispatcher.Execute;
var
  Node: TLccNode;
  Messages: TDynamicArrayInterface;
begin
  Transfer.Start;

  while not Terminated do
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
                Node.MsgQueueSending.LockArray;
                try
                  if Node.MsgQueueSending.Count > 0 then
                  begin
                    Messages := nil;
                    Node.MsgQueueSending.RemoveChunk(Messages);

                    // Need an inner enumeration to see if the message is going back to an internal node... eventually
                    // begin
                    // end....

                    // Put them in the outgoing hardware transfer buffer
                    Transfer.Buffer.LockArray;
                    try
                      Transfer.Buffer.AddChunk(Messages);
                    finally
                      Transfer.Buffer.UnLockArray;
                    end;
                    // Tell the thread it has some work to do
                    Transfer.Event.SetEvent;
                  end;
                finally
                  Node.MsgQueueSending.UnLockArray;
                end;
                Node := GlobalNodeList.NextObject as TLccNode;
              end;
            finally
              GlobalNodeList.UnLockArray;
            end;
            GlobalSendEvent.ResetEvent;
          end;
        end;
      wrTimeout   : begin end; // Time-out period expired
      wrAbandoned : begin end; // Wait operation was abandoned.
      wrError     : begin end; // An error occurred during the wait operation.
    end;
  end;
  Done := True;
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
    DispatcherSend.Terminate;
    while not DispatcherSend.Done do
      Sleep(100);
    DispatcherSend.Transfer.Terminate;
    DispatcherSend.Transfer.Event.SetEvent;
    while not DispatcherSend.Transfer.Done do
      Sleep(100);
    FreeAndNil(DispatcherSend.FTransfer);
    FreeAndNil(FDispatcherSend);

    DispatcherReceive.Transfer.Terminate;
    {$IFDEF ULTIBO}
    {$ELSE}
    Socket.CloseSocket;                               // This should release the Wait on the Receive with an error
    {$ENDIF}
    while not DispatcherReceive.Transfer.Done do
      Sleep(100);
    FreeAndNil(DispatcherReceive.FTransfer);
    FreeAndNil(FDispatcherReceive);
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
  Verbose: Boolean; TransferSend: TLccTransferThreadClass;
  TransferReceive: TLccTransferThreadClass);
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
    DispatcherSend := TLccTransferSendMessageDispatcher.Create(True, Socket);
    DispatcherSend.Transfer := TransferSend.Create(True, Socket);
    DispatcherReceive := TLccTransferReceiveMessageDispatcher.Create(True, Socket);
    DispatcherReceive.Transfer := TransferReceive.Create(True, Socket);
    DispatcherSend.Start;
    DispatcherReceive.Start;
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
