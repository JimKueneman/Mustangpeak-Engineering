unit lcc.objects;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, lcc.types, lcc.message;

type

  { TNodeProtocolBase }

  TNodeProtocolBase = class(TPersistent)
  private
    FErrorCode: Word;
    FWorkerMessage: TLccMessage;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    property ErrorCode: Word read FErrorCode write FErrorCode;

    constructor Create; virtual;
    destructor Destroy; override;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; virtual; abstract;
  end;

    { TStreamBasedProtocol }

  TStreamBasedProtocol = class(TNodeProtocolBase)
  private
    FInProcessAddress: DWord;
    FNullTerminatedString: Boolean;
    FStream: TMemoryStream;
    FAddressSpace: Byte;
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); virtual;

    property InProcessAddress: DWord read FInProcessAddress write FInProcessAddress;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
    property NullTerminatedString: Boolean read FNullTerminatedString write FNullTerminatedString;
  public
    property AStream: TMemoryStream read FStream write FStream;

    constructor Create(AnAddressSpace: Byte; IsStringBasedStream: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage); virtual;
    procedure WriteRequest(LccMessage: TLccMessage); virtual;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TNodeProtocolBase }

constructor TNodeProtocolBase.Create;
begin
  inherited Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TNodeProtocolBase.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

{ TStreamBasedProtocol }

procedure TStreamBasedProtocol.WriteRequest(LccMessage: TLccMessage);
begin

end;

constructor TStreamBasedProtocol.Create(AnAddressSpace: Byte; IsStringBasedStream: Boolean);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FAddressSpace := AnAddressSpace;
  IsStringBasedStream := NullTerminatedString;
end;

destructor TStreamBasedProtocol.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TStreamBasedProtocol.DoLoadComplete(LccMessage: TLccMessage);
begin

end;

procedure TStreamBasedProtocol.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
  i: Integer;
  iStart: Integer;
  AByte: Byte;
  Address, ReadCount: Int64;
begin
  // Assumption is this is a datagram message
  if LccMessage.DataArrayIndexer[1] and $03 = 0 then
    iStart := 7
  else
    iStart := 6;
  ReadCount := LccMessage.DataArrayIndexer[iStart];
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];    // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  if iStart = 7 then
    OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  if AStream.Size < Address + ReadCount then
  begin
    AStream.Position := AStream.Size;
    for i := 0 to ((Address + ReadCount) - AStream.Size) - 1 do
      {$IFDEF FPC}
      AStream.WriteByte(0);
     {$ELSE}
      AByte := 0;
      AStream.Write(AByte, 1);
     {$ENDIF}
  end;

  if AStream.Size = 0 then
  begin
    OutMessage.DataCount := iStart + 1;
    OutMessage.DataArrayIndexer[iStart] := Ord(#0);
  end else
  begin
    AStream.Position := Address;
    i := 0;
    while (AStream.Position < AStream.Size) and (i < ReadCount) do
    begin
      AByte := 0;
      AStream.Read(AByte, 1);
      OutMessage.DataArrayIndexer[iStart + i] := AByte;
      Inc(i);
    end;
    OutMessage.DataCount := iStart + i;

    if NullTerminatedString then
    begin
      if AStream.Position = AStream.Size then
      begin
        OutMessage.DataArrayIndexer[OutMessage.DataCount] := Ord(#0);
        OutMessage.DataCount := OutMessage.DataCount + 1
      end;
    end;
  end;
end;

function TStreamBasedProtocol.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  NullFound: Boolean;
  i: Integer;
  iStart: Integer;
  AByte: Byte;
begin
  Result := True;
 // if not Valid then
  begin
    NullFound := False;
    if LccMessage.DataArrayIndexer[1] and $03 = 0 then
      iStart := 7
    else
      iStart := 6;
    for i := iStart to LccMessage.DataCount - 1 do
    begin
      NullFound := LccMessage.DataArrayIndexer[i] = Ord(#0);
      AByte := LccMessage.DataArrayIndexer[i];
      AStream.WriteBuffer(AByte, 1);
      if NullFound then
        Break
    end;

    if NullFound then
    begin
      AStream.Position := 0;
 //     FValid := True;
      DoLoadComplete(LccMessage);
    end else
    begin
      WorkerMessage.Source := LccMessage.Destination;
      WorkerMessage.Destination := LccMessage.Source;
      WorkerMessage.DataCount := 0;
      WorkerMessage.DataArrayIndexer[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
      WorkerMessage.DataArrayIndexer[1] := MCP_READ;
      InProcessAddress := InProcessAddress + 64 {- iStart};
      WorkerMessage.InsertDWordAsDataBytes(InProcessAddress, 2);
      WorkerMessage.DataArrayIndexer[6] := AddressSpace;
      WorkerMessage.DataArrayIndexer[7] := 64;                     // Read until the end.....
      WorkerMessage.DataCount := 8;
      WorkerMessage.MTI := MTI_DATAGRAM;
    end;
  end;
end;

end.

