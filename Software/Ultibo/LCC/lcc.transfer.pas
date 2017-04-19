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
  blcksock,
  synsock,
  {$ENDIF}
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
  protected
    function DispatchedToInternalNode(AMessage: TLccMessage): Boolean;
    procedure Execute; override;
    function TransferMessageToWire(AMessage: TLccMessage): Boolean; virtual;
    function TransferWireToMessage(AByte: Byte; var AMessage: TLccMessage; var SendAsError: Boolean): Boolean; virtual;
    property SocketReference: TTCPBlockSocket read FSocketReference write FSocketReference;
    property Event: TSimpleEvent read FEvent write FEvent;
    property TransferDirection: TTransferDirection read FTransferDirection write FTransferDirection;
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection); reintroduce; virtual;
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

{ TLccTransferThread }

constructor TLccTransferThread.Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection);
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

