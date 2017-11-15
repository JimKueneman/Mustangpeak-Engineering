unit lcc.transfer;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, syncobjs,
  blcksock,
  synsock,
  lcc.node,
  lcc.message,
  mustangpeak.threadedcirculararray,
  lcc.transfer.gridconnect.message_assembler_disassembler;


type
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
    FSocketReference: TTCPBlockSocket;
    FVerbose: Boolean;
  protected
    function DispatchedToInternalNode(AMessage: TLccMessage): Boolean;
    procedure Execute; override;
    function TransferMessageToWire(AMessage: TLccMessage): Boolean; virtual;
    function TransferWireToMessage(AByte: Byte; var AMessage: TLccMessage; var SendAsError: Boolean): Boolean; virtual;
    property SocketReference: TTCPBlockSocket read FSocketReference write FSocketReference;
    property Event: TSimpleEvent read FEvent write FEvent;
    property TransferDirection: TTransferDirection read FTransferDirection write FTransferDirection;
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection; IsVerbose: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    property Buffer: TThreadedCirularArrayObject read FBuffer write FBuffer;
    property Done: Boolean read FDone write FDone;
    property FreeTransferThreadOnTerminate: TLccTransferThread read FFreeTransferThreadOnTerminate write FFreeTransferThreadOnTerminate;
    property FreeSocketOnTerminate: Boolean read FFreeSocketOnTerminate write FFreeSocketOnTerminate;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

   TLccTransferThreadClass = class of TLccTransferThread;

var
 // Warning the application must pull the messages out of the Queues periodocially if used.
   HookQueueReceived: TThreadedCirularArrayObject;
   HookQueueSend: TThreadedCirularArrayObject;
   HooksEnabled: Boolean;

implementation

var
  TransferThreadCount: Integer;

{ TLccTransferThread }

constructor TLccTransferThread.Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection; IsVerbose: Boolean);
begin
  {$IFDEF FPC}
  inherited Create(CreateSuspended, DefaultStackSize);
  {$ELSE}
  inherited Create(CreateSuspended);
  {$ENDIF}
  FVerbose := IsVerbose;
  if Verbose then WriteLn('Creating Transfer Thread: ' + ClassName);
  {$IFDEF FPC}
  InterLockedIncrement(TransferThreadCount);
  {$ELSE}
  TInterlocked.Increment(TransferThreadCount);
  {$ENDIF}
  FTransferDirection := ATransferDirection;
  FSocketReference := ASocket;
  Buffer := TThreadedCirularArrayObject.Create;
  Event := TSimpleEvent.Create;
end;

destructor TLccTransferThread.Destroy;
begin
  if Verbose then WriteLn('Destroying Transfer Thread: ' + ClassName);
  FreeAndNil(FEvent);
  FreeAndNil(FBuffer);
  {$IFDEF FPC}
  InterLockedDecrement(TransferThreadCount);
  {$ELSE}
  TInterlocked.Decrement(TransferThreadCount);
  {$ENDIF}
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
begin
  if Verbose then WriteLn('Starting Transfer Thread: ' + ClassName);
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
                  if HooksEnabled then
                  begin
                    HookQueueSend.LockArray;
                    try
                      HookQueueSend.RemoveChunk(Messages);
                    finally
                      HookQueueSend.UnLockArray;
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
                  end;
                  FreeDynamicArrayObjects(Messages);
                end;
              end;
            wrTimeout   : begin if Verbose then WriteLn('wrTimeout Error Returned in Send Thread: ' + ClassName);  end; // Time-out period expired
            wrAbandoned : begin if Verbose then WriteLn('wrAbandoned Returned in Send Thread: ' + ClassName); end; // Wait operation was abandoned.
            wrError     : begin if Verbose then WriteLn('wrError Returned in Send Thread: ' + ClassName);   end; // An error occurred during the wait operation.
          end;
        end;
      td_In :
        begin
          NextByte := SocketReference.RecvByte(Integer( INFINITE));
          if not Terminated then
          begin
            case SocketReference.LastError of
              S_OK :
                begin
                  LccMessage := nil;
                  SendReceiveError := False;
                  if TransferWireToMessage(NextByte, LccMessage, SendReceiveError) then
                  try
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
                    finally
                      GlobalNodeList.UnLockArray;
                    end;
                    if HooksEnabled then
                    begin
                      if SendReceiveError then
                        HookQueueSend.Add(LccMessage.Clone)
                      else
                        HookQueueReceived.Add(LccMessage.Clone);
                    end;
                  finally
                    FreeAndNil(LccMessage);
                  end;
                end
              else
                // Shut down for any error including WSACONNRESET

                if Verbose then WriteLn('Error (maybe WSACONNRESET) Returned in Receive Thread: ' + ClassName);

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
  if Verbose then WriteLn('Finishing Transfer Thread: ' + ClassName);
end;

function TLccTransferThread.TransferMessageToWire(AMessage: TLccMessage): Boolean;
begin
  Result := False
end;

function TLccTransferThread.TransferWireToMessage(AByte: Byte; var AMessage: TLccMessage; var SendAsError: Boolean): Boolean;
begin
  Result := False
end;

initialization
  TransferThreadCount := 0;
  HookQueueReceived := TThreadedCirularArrayObject.Create;
  HookQueueReceived.OwnsObjects := True;
  HookQueueSend := TThreadedCirularArrayObject.Create;
  HookQueueSend.OwnsObjects := True;
  HooksEnabled := False;

finalization
  FreeAndNil(HookQueueReceived);
  FreeAndNil(HookQueueSend);

end.

