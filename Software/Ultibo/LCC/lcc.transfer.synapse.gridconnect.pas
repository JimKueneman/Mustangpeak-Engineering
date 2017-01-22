unit lcc.transfer.synapse.gridconnect;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  lcc.utilities, lcc.node, blcksock, synsock, lcc.message,
  mustangpeak.threadedcirculararray, transfer.gridconnect.message_assembler_disassembler,
  lcc.transfer, lcc.transfer.gridconnect, lcc.transfer.nodeidmap;

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
    function TransferWireToMessage(NextByte: Byte; var AMessage: TLccMessage): Boolean; override;
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

  LccMessage.CAN.SourceAlias := $ABC;

  Temp := LccMessage.ConvertToGridConnectStr(#13);
  Socket.SendString(String( Temp) + LF);
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

function TGridConnectReceiveTcpThread.TransferWireToMessage(NextByte: Byte; var AMessage: TLccMessage): Boolean;
var
  GridConnectStrPtr: PGridConnectString;
  RecvString: string;
begin
  Result := False;
  AMessage := nil;
  if GridConnectHelper.GridConnect_DecodeMachine(NextByte, GridConnectStrPtr) then
  begin
    AMessage := TLccMessage.Create;
    RecvString := GridConnectBufferToString(GridConnectStrPtr^);
    case LccGridConnectAssembler.IncomingMessageGridConnect(RecvString, AMessage) of
      imgcr_False :
        begin
          FreeAndNil(AMessage);
          Result := False;
        end;
      imgcr_True :
        begin

          AMessage.CAN.DestAlias := $ABC;

          Result := True;
        end;
      imgcr_ErrorToSend :
        begin
          FreeAndNil(AMessage);
          Result := False;
        end;
      imgcr_UnknownError :
        begin
          FreeAndNil(AMessage);
          Result := False;
        end;
    end;
  end;
end;


end.

