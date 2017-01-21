unit lcc.transfer.synapse.gridconnect;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  syncobjs,
  {$ENDIF}
  lcc.types, lcc.utilities, lcc.node, blcksock, synsock, lcc.message,
  mustangpeak.threadedcirculararray, transfer.gridconnect.message_assembler_disassembler,
  lcc.transfer, lcc.transfer.gridconnect;

type
  { TGridConnectSendTcpThread }

  TGridConnectSendTcpThread = class(TLccTransferThread)
  public
    procedure Execute; override;
  end;

  { TGridConnectReceiveTcpThread }

  TGridConnectReceiveTcpThread = class(TLccTransferThread)
  public
    procedure Execute; override;
  end;


implementation

{ TGridConnectReceiveTcpThread }

procedure TGridConnectReceiveTcpThread.Execute;
var
  RecvString: string;
  LccMessage: ILccMessage;
  GridConnectHelper: TGridConnectHelper;
  NextByte: Byte;
  GridConnectStrPtr: PGridConnectString;
begin
  GridConnectHelper := TGridConnectHelper.Create;
  while not Terminated do
  begin
    NextByte := Socket.RecvByte(INFINITE);
    if not Terminated then
    begin
      if GridConnectHelper.GridConnect_DecodeMachine(NextByte, GridConnectStrPtr) then
      begin
        LccMessage := TLccMessage.Create as ILccMessage;
        RecvString := GridConnectBufferToString(GridConnectStrPtr^);
        LccMessage.LoadByGridConnectStr(RecvString);
        Buffer.LockArray;
        try
          Buffer.Add(LccMessage);
        finally
          Buffer.UnLockArray;
        end;
        GlobalReceiveEvent.SetEvent;
      end
    end;
  end;
  FreeAndNil(GridConnectHelper);
  Done := True;
end;


{ TGridConnectSendTcpThread }

procedure TGridConnectSendTcpThread.Execute;
var
  MessageInfList: TDynamicArrayInterface;
  i: Integer;
  Temp: string;
begin
  while not Terminated Do
  begin
    case Event.WaitFor(INFINITE) of
      wrSignaled  :
        begin
          if not Terminated then
          begin
            // Copy out the data in the buffer to a local array, these are references to ILccMessage objects
            Buffer.LockArray;
            try
              Buffer.RemoveChunk(MessageInfList);
            finally
              Buffer.UnLockArray;
            end;

            // Send each one of the Messages down the pike after convering to Grid Connect
            for i := 0 to Length(MessageInfList) - 1 do
            begin
              Temp := (MessageInfList[i] as ILccMessage).ConvertToGridConnectStr(#13);
              Socket.SendString(String( Temp) + LF);
            end;

            // Reset the Event to wait for more data
            Event.ResetEvent;
            // Release the message objects
            MessageInfList := nil;
          end;
        end; // Event was signaled (triggered)
      wrTimeout   : begin end; // Time-out period expired
      wrAbandoned : begin Terminate; end; // Wait operation was abandoned.
      wrError     : begin Terminate; end; // An error occurred during the wait operation.
    end;
  end;
  Done := True;
end;

end.

