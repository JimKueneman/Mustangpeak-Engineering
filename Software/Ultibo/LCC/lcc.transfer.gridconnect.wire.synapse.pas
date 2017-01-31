unit lcc.transfer.gridconnect.wire.synapse;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  lcc.utilities, lcc.node, blcksock, synsock, lcc.message,  lcc.types.can,
  mustangpeak.threadedcirculararray, lcc.transfer.gridconnect.message_assembler_disassembler,
  lcc.transfer, lcc.transfer.gridconnect;

type
  { TGridConnectSendTcpThread }

  TGridConnectSendTcpThread = class(TLccTransferThread)
  protected
    function TransferMessageToWire(LccMessage: TLccMessage): Boolean; override;
  end;

  { TGridConnectReceiveTcpThread }

  TGridConnectReceiveTcpThread = class(TLccTransferThread)
  private
    FGridConnectHelper: TGridConnectHelper;
    FLccGridConnectAssembler: TLccMessageAssembler;
  protected
    function TransferWireToMessage(NextByte: Byte; var AMessage: TLccMessage; var SendAsError: Boolean): Boolean; override;
    property GridConnectHelper: TGridConnectHelper read FGridConnectHelper write FGridConnectHelper;
  public
    constructor Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection); override;
    destructor Destroy; override;

    property LccGridConnectAssembler: TLccMessageAssembler read FLccGridConnectAssembler write FLccGridConnectAssembler;
  end;


implementation

{ TGridConnectSendTcpThread }

function TGridConnectSendTcpThread.TransferMessageToWire(LccMessage: TLccMessage): Boolean;
var
  Temp: string;
begin
  Temp := LccMessage.ConvertToGridConnectStr(#10);
  SocketReference.SendString(String( Temp) + #10);
  Result := True;
end;

{ TGridConnectReceiveTcpThread }

constructor TGridConnectReceiveTcpThread.Create(CreateSuspended: Boolean; ASocket: TTCPBlockSocket; ATransferDirection: TTransferDirection);
begin
  inherited Create(CreateSuspended, ASocket, ATransferDirection);
  GridConnectHelper := TGridConnectHelper.Create;
  LccGridConnectAssembler := TLccMessageAssembler.Create;
end;

destructor TGridConnectReceiveTcpThread.Destroy;
begin
  FreeAndNil(FGridConnectHelper);
  FreeAndNil(FLccGridConnectAssembler);
  inherited Destroy;
end;

function TGridConnectReceiveTcpThread.TransferWireToMessage(NextByte: Byte;
  var AMessage: TLccMessage; var SendAsError: Boolean): Boolean;
var
  GridConnectStrPtr: PGridConnectString;
  RecvString: string;
  Node: TLccNode;
  ValidAddress, DuplicateAlias: Boolean;
begin
  Result := False;
  SendAsError := False;
  AMessage := nil;


  GridConnectStrPtr := nil;

  if GridConnectHelper.GridConnect_DecodeMachine(NextByte, GridConnectStrPtr) then
  begin
    AMessage := TLccMessage.Create;
    RecvString := GridConnectBufferToString(GridConnectStrPtr^);

    // Don't process addressed messages that are not for us.  If we do we will
    // send error messages for messages that are not for us and will cache and
    // assemble ALL datagram and stream messages even if they are not for us.
    AMessage.LoadByGridConnectStr(RecvString);

    DuplicateAlias := False;
    // Look for a duplicate Alias Problem
    GlobalNodeList.LockArray;
    try
      Node := GlobalNodeList.FirstObject as TLccNode;
      while Assigned(Node) do
      begin
        if EqualNodeIDRec(AMessage.Source, Node.NodeID, False)  then
        begin
          DuplicateAlias := True;
          Break
        end;
        Node := GlobalNodeList.NextObject as TLccNode;
      end;
    finally
      GlobalNodeList.UnLockArray;
    end;

    ValidAddress := True;
    // If we dont' have a duplicate alias problem check to see if this is addressed and
    // don't pass on something not addressed to us
    if not DuplicateAlias and AMessage.HasDestination then
    begin
      ValidAddress := False; // We don't know yet
      GlobalNodeList.LockArray;
      try
        Node := GlobalNodeList.FirstObject as TLccNode;
        while Assigned(Node) do
        begin
          // If the Destination is equal it is addressed to us
          if EqualNodeIDRec(AMessage.Destination, Node.NodeID, False) then
          begin
            ValidAddress := True;
            Break
          end;
          Node := GlobalNodeList.NextObject as TLccNode;
        end;
      finally
        GlobalNodeList.UnLockArray;
      end;
    end;

    if DuplicateAlias then
    begin
      // Just pass it on I guess even if it is not fully received.  Better to detect it
      // and get the node reset.  If we tried to assemble it and had a problem we can send an
      // error message, unless we dupicate the code below
      Result := True;
    end else
    begin
      if ValidAddress then
      begin
        case LccGridConnectAssembler.IncomingMessageGridConnect(AMessage) of
          imgcr_False :
            begin
              FreeAndNil(AMessage);
              Result := False;
            end;
          imgcr_True :
            begin
              Result := True;
            end;
          imgcr_ErrorToSend :
            begin
              SendAsError := True;
              Result := True;  // Have something to send
            end;
          imgcr_UnknownError :
            begin
              FreeAndNil(AMessage);
              Result := False;
            end;
        end;
      end else
        FreeAndNil(AMessage);
    end
  end;
end;


end.

