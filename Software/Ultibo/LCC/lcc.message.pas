unit lcc.message;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils,
  syncobjs, lcc.types, mustangpeak.half_float, lcc.utilities, strutils,
  lcc.types.can;


type
  TLccMessage = class;


  { TLccCANMessage }

  TLccCANMessage = class
  private
    FActive: Boolean;
    FFramingBits: Byte;
    FiTag: Integer;
    FMTI: DWord;
  public
    property Active: Boolean read FActive write FActive;
    property iTag: Integer read FiTag write FiTag;        // General purpose counter/integer depending on the message
    property MTI: DWord read FMTI write FMTI;
    property FramingBits: Byte read FFramingBits write FFramingBits;            // Bottom 2 bits, upper nibble
  end;

{ TLccMessage }

  TLccMessage = class(TObject)
  private
    FAbandonTimeout: Integer;
    FCAN: TLccCANMessage;
    FDataArray: TLccByteArray;
    FDataCount: Integer;
    FDestination: TNodeIDRec;
    FMTI: Word;
    FRetryAttempts: Integer;
    FSource: TNodeIDRec;

    function GetHasDestination: Boolean;
    function GetHasDestNodeID: Boolean;
    function GetHasSourceNodeID: Boolean;
    function GetIsCAN: Boolean;
    function GetIsDatagram: Boolean;
    function GetIsStream: Boolean;
    function GetDataArrayIndexer(iIndex: DWord): Byte;
    procedure SetDataArrayIndexer(iIndex: DWord; const Value: Byte);protected
  public
    property AbandonTimeout: Integer read FAbandonTimeout write FAbandonTimeout;
    property CAN: TLccCANMessage read FCAN write FCAN;
    property DataArray: TLccByteArray read FDataArray write FDataArray;
    property DataArrayIndexer[iIndex: DWord]: Byte read GetDataArrayIndexer write SetDataArrayIndexer;
    property DataCount: Integer read FDataCount write FDataCount;
    property HasDestination: Boolean read GetHasDestination;
    property HasDestNodeID: Boolean read GetHasDestNodeID;
    property HasSourceNodeID: Boolean read GetHasSourceNodeID;
    property IsCAN: Boolean read GetIsCAN;
    property IsDatagram: Boolean read GetIsDatagram;
    property IsStream: Boolean read GetIsStream;
    property MTI: Word read FMTI write FMTI;
    property RetryAttempts: Integer read FRetryAttempts write FRetryAttempts;

    property Destination: TNodeIDRec read FDestination write FDestination;
    property Source: TNodeIDRec read FSource write FSource;

    constructor Create;
    destructor Destroy; override;

    procedure AppendDataArray(LccMessage: TLccMessage);
    function AppendDataArrayAsString(LccMessage: TLccMessage; LastNull: Byte): Boolean;
    function Clone: TLccMessage;
    procedure InsertNodeID(StartIndex: Integer; var ANodeID: TNodeIDRec);
    procedure InsertEventID(StartIndex: Integer; var AnEventID: TEventID);
    procedure InsertDWordAsDataBytes(DoubleWord: DWord; StartByteIndex: Integer);
    procedure InsertWordAsDataBytes(AWord: DWord; StartByteIndex: Integer);
    function IsAddressedMessage: Boolean;
    function ExtractDataBytesAsEventID(StartIndex: Integer): PEventID;
    function ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
    function ExtractDataBytesAsNodeID(StartIndex: Integer; var ANodeID: TNodeID): PNodeID;
    function ExtractDataBytesAsString(StartIndex, Count: Integer): String;

    function LoadByGridConnectStr(GridConnectStr: String): Boolean;
    function LoadByLccTcp(var ByteArray: TDynamicByteArray): Boolean;
    function ConvertToGridConnectStr(Delimiter: String): String;
    function ConvertToLccTcp(var ByteArray: TDynamicByteArray): Boolean;
    procedure Copy(TargetMessage: TLccMessage);
    function ConvertToLccTcpString(var ByteArray: TDynamicByteArray): String;
    procedure ZeroFields;

    // CAN
    procedure LoadCID(ASource: TNodeIDRec; ACID: Byte);
    procedure LoadRID(ASource: TNodeIDRec);
    procedure LoadAMD(ASource: TNodeIDRec);
    procedure LoadAMR(ASource: TNodeIDRec);
    procedure LoadAME(ASource: TNodeIDRec);

    // Basic
    procedure LoadInitializationComplete(ASourceID: TNodeIDRec);
    procedure LoadVerifyNodeIDAddressed(ASourceID, ADestID: TNodeIDRec);
    procedure LoadVerifyNodeID(ASourceID: TNodeIDRec);
    procedure LoadVerifiedNodeID(ASourceID: TNodeIDRec);
    // Protocol Support (PIP)
    procedure LoadProtocolIdentifyInquiry(ASourceID, ADestID: TNodeIDRec);
    procedure LoadProtocolIdentifyReply(ASourceID, ADestID: TNodeIDRec; Flags: QWord);
    // Event Exchange
    procedure LoadConsumerIdentify(ASourceID: TNodeIDRec; var Event: TEventID);
    procedure LoadConsumerIdentified(ASourceID: TNodeIDRec; var Event: TEventID; EventState: TEventState);
    procedure LoadProducerIdentify(ASourceID: TNodeIDRec; var Event: TEventID);
    procedure LoadProducerIdentified(ASourceID: TNodeIDRec; var Event: TEventID; EventState: TEventState);
    procedure LoadIdentifyEventsAddressed(ASourceID, ADestID: TNodeIDRec);
    procedure LoadIdentifyEvents(ASourceID: TNodeIDRec);
    procedure LoadPCER(ASourceID: TNodeIDRec; AnEvent: PEventID);
    // Traction Control
    procedure LoadTractionSetSpeed(ASourceID, ADestID: TNodeIDRec; ASpeed: THalfFloat);
    procedure LoadTractionSetFunction(ASourceID, ADestID: TNodeIDRec; AnAddress: DWord; AValue: Word);
    procedure LoadTractionEStop(ASourceID, ADestID: TNodeIDRec);
    procedure LoadTractionQuerySpeed(ASourceID, ADestID: TNodeIDRec);
    procedure LoadTractionQueryFunction(ASourceID, ADestID: TNodeIDRec);
    procedure LoadTractionControllerAssign(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
    procedure LoadTractionControllerRelease(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
    procedure LoadTractionControllerQuery(ASourceID, ADestID: TNodeIDRec);
    procedure LoadTractionControllerChangeNotify(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
    procedure LoadTractionControllerChangeNotifyReply(ASourceID, ADestID: TNodeIDRec; Allow: Boolean);
    procedure LoadTractionConsistAttach(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
    procedure LoadTractionConsistDetach(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
    procedure LoadTractionConsistQuery(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
    procedure LoadTractionManage(ASourceID, ADestID: TNodeIDRec; Reserve: Boolean);
    // Remote Button

    // Traction Identification (STNIP)
    procedure LoadSimpleTrainNodeIdentInfoRequest(ASourceID, ADestID: TNodeIDRec);
    // Node Ident (SNIP)
    procedure LoadSimpleNodeIdentInfoReply(ASourceID, ADestID: TNodeIDRec; SimplePackedArray: TSimpleNodeInfoPacked);
    procedure LoadSimpleNodeIdentInfoRequest(ASourceID, ADestID: TNodeIDRec);
    // FDI
    procedure LoadFDIRequest(ASourceID, ADestID: TNodeIDRec);
    procedure LoadFunctionConfigurationRead(ASourceID, ADestID: TNodeIDRec; FunctionAddress: DWord; Count: Integer);
    procedure LoadFunctionConfigurationWrite(ASourceID, ADestID: TNodeIDRec; FunctionAddress: DWord; Count: Integer; Functions: TFunctionStatesArray);
    // CDI
    procedure LoadCDIRequest(ASourceID, ADestID: TNodeIDRec);
    // Datagram
    procedure LoadDatagram(ASourceID, ADestID: TNodeIDRec);
    procedure LoadDatagramAck(ASourceID, ADestID: TNodeIDRec; Ok: Boolean; ReplyPending: Boolean; TimeOutValueN: Byte);
    procedure LoadDatagramRejected(ASourceID, ADestID: TNodeIDRec; Reason: Word);
    // ConfigurationMemory
    procedure LoadConfigMemAddressSpaceInfo(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte);
    procedure LoadConfigMemOptions(ASourceID, ADestID: TNodeIDRec);
    procedure LoadConfigMemRead(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; ConfigMemSize: Byte);
    procedure LoadConfigMemWriteInteger(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; IntegerSize: Byte; DataInteger: Integer);
    procedure LoadConfigMemWriteString(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; AString: String);
    procedure LoadConfigMemWriteArray(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; ArraySize: Integer; AnArray: array of Byte);
    // MTIs
    procedure LoadOptionalInteractionRejected(ASourceID, ADestID: TNodeIDRec; Reason: Word; AnMTI: Word);

    procedure SwapDestAndSourceIDs;
  end;

var
  GlobalSendEvent: TSimpleEvent;
  ClassicSNIP: Boolean;

implementation

var
  CaptureTime: QWord;

{ TLccMessage }

procedure TLccMessage.AppendDataArray(LccMessage: TLccMessage);
var
  i: Integer;
begin
  for i := 0 to LccMessage.DataCount - 1 do
  begin
    FDataArray[DataCount] := LccMessage.DataArray[i];
    Inc(FDataCount);
  end;
end;

function TLccMessage.AppendDataArrayAsString(LccMessage: TLccMessage; LastNull: Byte): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to LccMessage.DataCount - 1 do
  begin
    FDataArray[DataCount] := LccMessage.DataArray[i];
    if DataArray[DataCount] = Ord(#0) then
    begin
      Inc(CAN.FiTag);
      if CAN.iTag = LastNull then
        Result := True;
    end;
    Inc(FDataCount);
  end;
end;

function TLccMessage.Clone: TLccMessage;
begin
  Result := TLccMessage.Create;
  Copy(Result);
end;

function TLccMessage.ConvertToGridConnectStr(Delimiter: String): String;
var
  i, iFrameCount, Offset: Integer;
  LocalMTI: DWord;
  FrameCount: Integer;
begin
  Result := '';

  // This can be longer than 6 to 8 bytes
  if (MTI = MTI_DATAGRAM) then
  begin
    // There are 2 ways a Datagram can enter here:
    // 1) A raw Message with a MTI_DATAGRAM MTI that has not been parsed into CAN frames yet
    // 2) A parsed CAN frame that effectively is a piece of a MTI_DATAGRAM but does not need to be reparsed
    if CAN.Active then
    begin
      LocalMTI := CAN.MTI or Source.Alias  or $10000000;
      LocalMTI := LocalMTI or (DWord( Destination.Alias) shl 12);
      Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
      for i := 0 to DataCount - 1 do
        Result := Result + IntToHex(DataArray[i], 2);
      Result := Result + ';' + Delimiter
    end else
    begin
      if DataCount < 9 then
      begin
        Result := Result + ':X' + IntToHex(DWord( $1A000000 or (Destination.Alias shl 12) or Source.Alias), 8) + 'N';
        for i := 0 to DataCount - 1 do
          Result := Result + IntToHex(DataArray[i], 2);
        Result := Result + ';'
      end else
      begin
        Offset := 0;
        while Offset < DataCount do
        begin
          if Offset = 0 then
            Result := Result + ':X' + IntToHex(DWord( $1B000000 or (Destination.Alias shl 12) or Source.Alias), 8) + 'N'
          else
          if Offset + 8 >= DataCount then
            Result := Result + ':X' + IntToHex(DWord( $1D000000 or (Destination.Alias shl 12) or Source.Alias), 8) + 'N'
          else
            Result := Result + ':X' + IntToHex(DWord( $1C000000 or (Destination.Alias shl 12) or Source.Alias), 8) + 'N';

          for i := 0 to 7 do
          begin
            Result := Result + IntToHex(DataArray[Offset], 2);
            Inc(Offset);
            if Offset >= DataCount then
              Break;
          end;
          if Offset < DataCount then
            Result := Result + ';' + Delimiter
          else
            Result := Result + ';'
        end;
      end;
    end;
  end else
  if MTI = MTI_STREAM_SEND then
  begin
    if CAN.Active then
    begin

    end else
    begin

    end;
  end else
  begin
    if CAN.Active then
      LocalMTI := CAN.MTI or Source.Alias or $10000000
    else
      LocalMTI := DWord(( MTI shl 12) or Source.Alias or MTI_CAN_FRAME_TYPE_GENERAL or $10000000);

    if LocalMTI and MTI_CAN_FRAME_TYPE_MASK > MTI_CAN_FRAME_TYPE_GENERAL then
    begin
      // Datagram or Stream
      LocalMTI := LocalMTI or (DWord( Destination.Alias) shl 12);
      Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
      for i := 0 to DataCount - 1 do
        Result := Result + IntToHex(DataArray[i], 2);
    end else
    begin
      if HasDestination then
      begin
        if DataCount > 6 then                                                     // Is it a multiframe message?
        begin
          Result := '';
          Offset := 0;
          FrameCount := DataCount div 6;
          if DataCount mod 6 <> 0 then
            Inc(FrameCount);

          for iFrameCount := 0 to FrameCount - 1 do
          begin
            Result := Result + ':X' + IntToHex(LocalMTI, 8) + 'N';

            if (MTI = MTI_SIMPLE_NODE_INFO_REPLY) and (ClassicSNIP) then
            begin
              Result := Result + IntToHex(((Destination.Alias shr 8)) and $00FF, 2);
            end else
            begin
              if iFrameCount = 0 then
                Result := Result + IntToHex(((Destination.Alias shr 8) or $10) and $00FF, 2)
              else
              if iFrameCount = FrameCount - 1 then
                Result := Result + IntToHex(((Destination.Alias shr 8) or $20) and $00FF, 2)
              else
                Result := Result + IntToHex(((Destination.Alias shr 8) or $30) and $00FF, 2);
            end;

            Result := Result + IntToHex(Destination.Alias and $00FF, 2);

            i := Offset;
            while i < DataCount do
            begin
              Result := Result + IntToHex(DataArray[i], 2);
              Inc(i);
              if (i - Offset) = 6 then
              begin
                Inc(Offset, 6);
                Break;
              end;
            end;

            if iFrameCount <  FrameCount - 1 then
              Result := Result  + ';' + Delimiter       // MultiFrame Message
            else
              Result := Result  + ';'
          end;
        end else
        begin
          Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
          Result := Result + IntToHex((Destination.Alias shr 8) or CAN.FramingBits and $00FF, 2);
          Result := Result + IntToHex(Destination.Alias and $00FF, 2);
          for i := 0 to DataCount - 1 do
            Result := Result + IntToHex(DataArray[i], 2);
          Result := Result  + ';'                    // Single Frame message
        end;
      end else
      begin
        // Non destination messages can't have multiple frames
        Result := ':X' + IntToHex(LocalMTI, 8) + 'N';
        for i := 0 to DataCount - 1 do
          Result := Result + IntToHex(DataArray[i], 2);
        Result := Result  + ';'
      end;
    end;
  end;
end;

procedure TLccMessage.InsertNodeID(StartIndex: Integer; var ANodeID: TNodeIDRec);
begin
  FDataArray[StartIndex]     := _Higher( ANodeID.ID[1]); // But these all need the 48 Bit Full ID in the Byte Fields
  FDataArray[StartIndex + 1] := _Hi(     ANodeID.ID[1]);
  FDataArray[StartIndex + 2] := _Lo(     ANodeID.ID[1]);
  FDataArray[StartIndex + 3] := _Higher( ANodeID.ID[0]);
  FDataArray[StartIndex + 4] := _Hi(     ANodeID.ID[0]);
  FDataArray[StartIndex + 5] := _Lo(     ANodeID.ID[0]);
end;

procedure TLccMessage.InsertEventID(StartIndex: Integer; var AnEventID: TEventID);
begin
  FDataArray[StartIndex]     := AnEventID[0]; // But these all need the 48 Bit Full ID in the Byte Fields
  FDataArray[StartIndex + 1] := AnEventID[1];
  FDataArray[StartIndex + 2] := AnEventID[2];
  FDataArray[StartIndex + 3] := AnEventID[3];
  FDataArray[StartIndex + 4] := AnEventID[4];
  FDataArray[StartIndex + 5] := AnEventID[5];
  FDataArray[StartIndex + 6] := AnEventID[6];
  FDataArray[StartIndex + 7] := AnEventID[7];
end;

procedure TLccMessage.InsertDWordAsDataBytes(DoubleWord: DWord; StartByteIndex: Integer);
begin
  FDataArray[StartByteIndex]   := _Highest(DoubleWord);
  FDataArray[StartByteIndex+1] := _Higher(DoubleWord);
  FDataArray[StartByteIndex+2] := _Hi(DoubleWord);
  FDataArray[StartByteIndex+3] := _Lo(DoubleWord);
end;

procedure TLccMessage.InsertWordAsDataBytes(AWord: DWord; StartByteIndex: Integer);
begin
  FDataArray[StartByteIndex]   := _Hi(AWord);
  FDataArray[StartByteIndex+1] := _Lo(AWord);
end;

function TLccMessage.IsAddressedMessage: Boolean;
begin
  Result := MTI and MTI_ADDRESSED_MASK <> 0;
end;

procedure TLccMessage.LoadAMD(ASource: TNodeIDRec);
begin
  ZeroFields;
  FSource.Alias := ASource.Alias;
  CAN.Active := True;
  CAN.MTI := MTI_CAN_AMD or ASource.Alias;
  FSource.ID := ASource.ID;
  InsertNodeID(0, ASource);
  DataCount := 6;
end;

procedure TLccMessage.LoadAME(ASource: TNodeIDRec);
begin
  ZeroFields;
  FSource.Alias := ASource.Alias;
  CAN.Active := True;
  CAN.MTI := MTI_CAN_AME or ASource.Alias;
  FSource.ID := ASource.ID;
  InsertNodeID(0, ASource);
  DataCount := 6;
end;

procedure TLccMessage.LoadAMR(ASource: TNodeIDRec);
begin
  ZeroFields;
  FSource.Alias := ASource.Alias;
  CAN.Active := True;
  CAN.MTI := MTI_CAN_AMR or ASource.Alias;
  FSource.ID := ASource.ID;
  InsertNodeID(0, ASource);
  DataCount := 6;
end;

function TLccMessage.LoadByGridConnectStr(GridConnectStr: String): Boolean;
var
  x, n, SemiColon, i, j: Integer;
  ByteStr: String;
  DestHi, DestLo: Byte;
  ZeroIndex: Boolean;
  CAN_FrameType: DWord;
begin
  Result := False;
  if GridConnectStr <> '' then
  begin
    ZeroFields;
    {$IFDEF FPC}
    ZeroIndex := False;
    {$ELSE}
    ZeroIndex := Low(GridConnectStr) = 0;
    {$ENDIF}
    GridConnectStr := UpperCase(GridConnectStr);
    x := Pos('X', GridConnectStr);                                              // Find were the "X" is in the string
    if ZeroIndex then Dec(x);
    if x > 0 then
    begin
      n := PosEx('N', GridConnectStr, x);                                       // Find where the "N" is in the string
      if ZeroIndex then Dec(n);
      if n > 0 then
      begin
        GridConnectStr[n] := #0;                                                // Set the "N" to a null to create a null string of the MTI
        Inc(n);                                                                 // Move just pass where the "N" was
        SemiColon := PosEx(';', GridConnectStr, n);                             // Look for the terminating ";"
        if ZeroIndex then Dec(SemiColon);
        if SemiColon > 0 then
        begin
          ByteStr := '';
          j := x+1;
          while GridConnectStr[j] <> #0 do
          begin
            ByteStr := ByteStr + GridConnectStr[j];
            Inc(j);
          end;
          CAN.MTI := StrToInt('$' + ByteStr);              // Convert the string MTI into a number  ;
          FSource.Alias := Word( CAN.MTI and $00000FFF);                      // Grab the Source Alias before it is stripped off
          CAN.MTI := CAN.MTI and not $10000000;                                 // Strip off the reserved bits
          CAN.MTI := CAN.MTI and $FFFFF000;                                     // Strip off the Source Alias
          // Was this an OpenLCB or CAN specific message? This covers special multiFrame LccMessage and CAN layer specific messages
          CAN.Active := (CAN.MTI and $07000000 <> $01000000);

          // Extract the General OpenLCB message if possible
          if CAN.Active then
          begin
            if ((CAN.MTI and $0F000000) <= MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END) and ((CAN.MTI and $0F000000) >= MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY) then
            begin
              FDestination.Alias := (CAN.MTI and $00FFF000) shr 12;
              CAN.MTI := CAN.MTI and $0F000000; // $FF000FFF;
              MTI := MTI_DATAGRAM
            end else
            if (CAN.MTI and $0F000000) = 0 then // Is core CAN msg
            begin
              CAN.MTI := CAN.MTI and $00FFF000;
              MTI := 0;
            end else
            begin
              CAN.MTI := CAN.MTI and $0F000000;
              MTI := 0;
            end
          end else
          begin
            if CAN.MTI and MTI_CAN_ADDRESS_PRESENT = MTI_CAN_ADDRESS_PRESENT then
            begin
              ByteStr := GridConnectStr[n] + GridConnectStr[n+1];
              DestHi := StrToInt('$' + ByteStr);
              ByteStr := GridConnectStr[n+2] + GridConnectStr[n+3];
              DestLo := StrToInt('$' + ByteStr);
              CAN.FramingBits := DestHi and $30;
              FDestination.Alias := Word(( DestHi shl 8) and $0FFF) or DestLo;
              Inc(n, 4);
            end else
            begin
              FDestination.Alias := 0;
              CAN.FramingBits := 0;
            end;
            MTI := Word( (CAN.MTI shr 12) and $0000FFF);
          end;

  //      FForwardingBitNotSet := GridConnect_MTI and $10000000 = $00000000;    // Check if the Forwarding Bit was set
  //      FUnimplementedBitsSet := GridConnect_MTI and $E0000000 <> $00000000;  // Check to see the state of the unimplemented bits


          FDataCount := 0;                                                       // Convert the CAN payload bytes into number
          i := n;
          while i < SemiColon do
          begin
            ByteStr := GridConnectStr[i] + GridConnectStr[i+1];
            FDataArray[FDataCount] := StrToInt('$' + ByteStr);
            Inc(i, 2);
            Inc(FDataCount);
          end;
        end;
        Result := True
      end
    end
  end;
end;

function TLccMessage.GetDataArrayIndexer(iIndex: DWord): Byte;
begin
  Result := FDataArray[iIndex]
end;

function TLccMessage.GetHasDestination: Boolean;
begin
  Result := ((Destination.ID[0] <> 0) and (Destination.ID[1] <> 0)) or (Destination.Alias <> 0);
end;

function TLccMessage.GetHasDestNodeID: Boolean;
begin
  Result := (Destination.ID[0] <> 0) and (Destination.ID[1] <> 0);
end;

function TLccMessage.GetHasSourceNodeID: Boolean;
begin
  Result := (Source.ID[0] <> 0) and (Source.ID[1] <> 0);
end;

function TLccMessage.GetIsCAN: Boolean;
begin
  Result := CAN.Active;
end;

function TLccMessage.GetIsDatagram: Boolean;
begin
  Result := MTI = MTI_DATAGRAM;
end;

function TLccMessage.GetIsStream: Boolean;
begin
  Result := MTI = MTI_STREAM_SEND;
end;

constructor TLccMessage.Create;
begin
  inherited Create;
  CAN := TLccCANMessage.Create;
  {$IFDEF FPC}
   InterLockedIncrement(AllocatedMessages);
  {$ELSE}
   TInterlocked.Increment(AllocatedMessages)
  {$ENDIF}
end;

destructor TLccMessage.Destroy;
begin
  FreeAndNil(FCAN);
  inherited Destroy;
  {$IFDEF FPC}
  InterLockedDecrement(AllocatedMessages);
  {$ELSE}
  TInterlocked.Decrement(AllocatedMessages)
  {$ENDIF}
end;

function TLccMessage.ExtractDataBytesAsEventID(StartIndex: Integer): PEventID;
begin
  Result := @DataArray[StartIndex];
end;

function TLccMessage.ExtractDataBytesAsInt(StartByteIndex, EndByteIndex: Integer): QWord;
var
  i, Offset, Shift: Integer;
  ByteAsQ, ShiftedByte: QWord;
begin
  Result := 0;
  Offset := EndByteIndex - StartByteIndex;
  for i := StartByteIndex to EndByteIndex do
  begin
    Shift := Offset * 8;
    ByteAsQ := QWord( DataArray[i]);
    ShiftedByte := ByteAsQ shl Shift;
    Result := Result or ShiftedByte;
    Dec(Offset)
  end;
end;

function TLccMessage.ExtractDataBytesAsNodeID(StartIndex: Integer; var ANodeID: TNodeID): PNodeID;
begin
  Result := @ANodeID;
  ANodeID[1] := (DataArray[StartIndex] shl 16) or (DataArray[StartIndex+1] shl 8) or DataArray[StartIndex+2];
  ANodeID[0] := (DataArray[StartIndex+3] shl 16) or (DataArray[StartIndex+4] shl 8) or DataArray[StartIndex+5];
end;

function TLccMessage.ExtractDataBytesAsString(StartIndex, Count: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := StartIndex to Count - 1 do
  begin
    if DataArray[i] <> Ord(#0) then
      Result := Result + Chr( DataArray[i])
    else
      Break
  end;
end;

function TLccMessage.LoadByLccTcp(var ByteArray: TDynamicByteArray): Boolean;
var
  Flags: Word;
  Size, Offset: DWord;
  i, Count: Int64;
begin
  Result := False;
  ZeroFields;

  Flags := (ByteArray[0] shl 8) or ByteArray[1];
  if Flags and TCP_FLAG_LCC_MESSAGE = TCP_FLAG_LCC_MESSAGE then
  begin
    Size := (ByteArray[2] shl 16) or (ByteArray[3] shl 8) or (ByteArray[4]);
    // Skip the Originating Node
    // Skip the Time
    MTI := (ByteArray[17] shl 8) or ByteArray[18];
    FSource.ID[1] := (ByteArray[19] shl 16) or (ByteArray[20] shl 8) or (ByteArray[21]);
    FSource.ID[0] := (ByteArray[22] shl 16) or (ByteArray[23] shl 8) or (ByteArray[24]);

    if MTI and MTI_ADDRESSED_MASK = MTI_ADDRESSED_MASK then
    begin
      FDestination.ID[1] := (ByteArray[25] shl 16) or (ByteArray[26] shl 8) or (ByteArray[27]);
      FDestination.ID[0] := (ByteArray[28] shl 16) or (ByteArray[29] shl 8) or (ByteArray[30]);
      Offset := 26;  //  31 - 5
    end else
      Offset := 20;  // 25 - 5
    Count := Size - Offset;
    Inc(Offset, 5);
    i := 0;
    FDataCount := 0;
    while i < Count do
    begin
      FDataArray[i] := ByteArray[Offset + i];
      Inc(FDataCount);
      Inc(i);
    end;
    Result := True;
  end;
end;

function TLccMessage.ConvertToLccTcp(var ByteArray: TDynamicByteArray): Boolean;
var
  Flags: Word;
  Size: DWord;
  i, Offset: Integer;
begin
  if HasDestination then
    SetLength(ByteArray, DataCount + TCP_HEADER_ONLY_MAX + TCP_MESSAGE_PREAMBLE_MAX)
  else
    SetLength(ByteArray, DataCount + TCP_HEADER_ONLY_MAX + TCP_MESSAGE_PREAMBLE_MIN);
  Size := Length(ByteArray) - 5;

  Flags := TCP_FLAG_LCC_MESSAGE;
  ByteArray[0] := _Hi(Flags);
  ByteArray[1] := _Lo(Flags);

  // Size
  ByteArray[2] := _Higher(Size);
  ByteArray[3] := _Hi(Size);
  ByteArray[4] := _Lo(Size);

  // Originating Node
  ByteArray[5] := _Higher(Source.ID[1]);
  ByteArray[6] := _Hi(Source.ID[1]);
  ByteArray[7] := _Lo(Source.ID[1]);
  ByteArray[8] := _Higher(Source.ID[0]);
  ByteArray[9] := _Hi(Source.ID[0]);
  ByteArray[10] := _Lo(Source.ID[0]);

  // Let the socket fill in the monotonicly increasing Capture Time
  ByteArray[11] := _Highest2(CaptureTime);
  ByteArray[12] := _Highest1(CaptureTime);
  ByteArray[13] := _Highest(CaptureTime);
  ByteArray[14] := _Higher(CaptureTime);  ;
  ByteArray[15] := _Hi(CaptureTime);
  ByteArray[16] := _Lo(CaptureTime);
  Inc(CaptureTime);

  // MTI
  ByteArray[17] := _Hi(MTI);
  ByteArray[18] := _Lo(MTI);

  // Source Node
  ByteArray[19] := _Higher(Source.ID[1]);
  ByteArray[20] := _Hi(Source.ID[1]);
  ByteArray[21] := _Lo(Source.ID[1]);
  ByteArray[22] := _Higher(Source.ID[0]);
  ByteArray[23] := _Hi(Source.ID[0]);
  ByteArray[24] := _Lo(Source.ID[0]);

  if HasDestination then
  begin
    ByteArray[25] := _Higher(Destination.ID[1]);
    ByteArray[26] := _Hi(Destination.ID[1]);
    ByteArray[27] := _Lo(Destination.ID[1]);
    ByteArray[28] := _Higher(Destination.ID[0]);
    ByteArray[29] := _Hi(Destination.ID[0]);
    ByteArray[30] := _Lo(Destination.ID[0]);
    Offset := 31;
  end else
    Offset := 25;

  for i := 0 to DataCount - 1 do
    ByteArray[Offset + i] := DataArray[i];

  Result := True;
end;

function TLccMessage.ConvertToLccTcpString(var ByteArray: TDynamicByteArray): String;
const
  LF = #13#10;
var
  i: Integer;
begin
  Result := '';
  Result := Result + LF + 'TCP Header: ';
  for i := 0 to TCP_HEADER_ONLY_MAX - 1 do
    Result := Result + ' ' + IntToHex(ByteArray[i], 2);

  Result := Result + LF + 'TCP Message: ';
  for i := TCP_HEADER_ONLY_MAX to Length(ByteArray) - 1 do
    Result := Result + ' ' + IntToHex(ByteArray[i], 2);
end;

procedure TLccMessage.Copy(TargetMessage: TLccMessage);
begin
    TargetMessage.FAbandonTimeout := 0;

  TargetMessage.Source := Source;
  TargetMessage.Destination := Destination;
  TargetMessage.FDataArray := FDataArray;
  TargetMessage.FDataCount := FDataCount;
  TargetMessage.FMTI := FMTI;
  TargetMessage.RetryAttempts := 0;

  TargetMessage.FCAN.Active := CAN.Active;
  TargetMessage.FCAN.FFramingBits := CAN.FramingBits;
  TargetMessage.FCAN.FiTag := CAN.iTag;
  TargetMessage.FCAN.FMTI := CAN.MTI;
end;

procedure TLccMessage.ZeroFields;
begin
  FDataCount := 0;
  FDestination.ID := NULL_NODE_ID;
  FSource.ID := NULL_NODE_ID;
  FDestination.Alias := 0;
  FSource.Alias := 0;
  FMTI := 0;
  CAN.MTI := 0;
  CAN.Active := False;
end;

procedure TLccMessage.LoadInitializationComplete(ASourceID: TNodeIDRec);
begin
  ZeroFields;
  FSource := ASourceID;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
  MTI := MTI_INITIALIZATION_COMPLETE;
end;

procedure TLccMessage.LoadOptionalInteractionRejected(ASourceID, ADestID: TNodeIDRec; Reason: Word; AnMTI: Word);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 4;
  MTI := MTI_OPTIONAL_INTERACTION_REJECTED;
  FDataArray[0] := _Hi(Reason);
  FDataArray[1] := _Lo(Reason);
  FDataArray[2] := _Hi(AnMTI);
  FDataArray[3] := _Lo(AnMTI);
end;

procedure TLccMessage.LoadPCER(ASourceID: TNodeIDRec; AnEvent: PEventID);
begin
  ZeroFields;
  FSource := ASourceID;
  InsertEventID(0, AnEvent^);
  DataCount := 8;
  MTI := MTI_PC_EVENT_REPORT;
end;

procedure TLccMessage.LoadProducerIdentified(ASourceID: TNodeIDRec;
  var Event: TEventID; EventState: TEventState);
begin
  ZeroFields;
  Source := ASourceID;
  InsertEventID(0, Event);
  DataCount := 8;
  case EventState of
    evs_Valid : MTI := MTI_PRODUCER_IDENTIFIED_SET;
    evs_InValid : MTI := MTI_PRODUCER_IDENTIFIED_CLEAR;
    evs_Unknown : MTI := MTI_PRODUCER_IDENTIFIED_UNKNOWN;
  end;
end;

procedure TLccMessage.LoadVerifyNodeIDAddressed(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  MTI := MTI_VERIFY_NODE_ID_NUMBER_DEST;
end;

procedure TLccMessage.SetDataArrayIndexer(iIndex: DWord; const Value: Byte);
begin
  FDataArray[iIndex] := Value
end;

procedure TLccMessage.SwapDestAndSourceIDs;
var
  TempNodeID: TNodeIDRec;
begin
  TempNodeID := Source;
  Source := Destination;
  Destination := TempNodeID;
end;

procedure TLccMessage.LoadVerifyNodeID(ASourceID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  MTI := MTI_VERIFY_NODE_ID_NUMBER;
end;

procedure TLccMessage.LoadProtocolIdentifyInquiry(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  MTI := MTI_PROTOCOL_SUPPORT_INQUIRY;
end;

procedure TLccMessage.LoadProtocolIdentifyReply(ASourceID, ADestID: TNodeIDRec; Flags: QWord);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  FDataArray[5] := _Lo(Flags);
  FDataArray[4] := _Hi(Flags);
  FDataArray[3] := _Higher(Flags);
  FDataArray[2] := _Highest(Flags);
  FDataArray[1] := _Highest1(Flags);
  FDataArray[0] := _Highest2(Flags);
  DataCount := 6;
  MTI := MTI_PROTOCOL_SUPPORT_REPLY;
end;

procedure TLccMessage.LoadRID(ASource: TNodeIDRec);
begin
  ZeroFields;
  FSource.Alias := ASource.Alias;
  CAN.Active := True;
  CAN.MTI := MTI_CAN_RID or ASource.Alias;
end;

procedure TLccMessage.LoadConsumerIdentified(ASourceID: TNodeIDRec;
  var Event: TEventID; EventState: TEventState);
begin
  ZeroFields;
  Source := ASourceID;
  InsertEventID(0, Event);
  DataCount := 8;
  case EventState of
    evs_Valid : MTI := MTI_CONSUMER_IDENTIFIED_SET;
    evs_InValid : MTI := MTI_CONSUMER_IDENTIFIED_CLEAR;
    evs_Unknown : MTI := MTI_CONSUMER_IDENTIFIED_UNKNOWN;
  end;
end;

procedure TLccMessage.LoadConsumerIdentify(ASourceID: TNodeIDRec;
  var Event: TEventID);
begin
  ZeroFields;
  Source := ASourceID;
  InsertEventID(0, Event);
  DataCount := 8;
  MTI := MTI_CONSUMER_IDENTIFY;
end;

procedure TLccMessage.LoadDatagram(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadProducerIdentify(ASourceID: TNodeIDRec; var Event: TEventID);
begin
  ZeroFields;
  Source := ASourceID;
  InsertEventID(0, Event);
  DataCount := 8;
  MTI := MTI_PRODUCER_IDENDIFY;
end;

procedure TLccMessage.LoadIdentifyEventsAddressed(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  MTI := MTI_EVENTS_IDENTIFY_DEST;
end;

procedure TLccMessage.LoadIdentifyEvents(ASourceID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := NULL_NODE_ID_REC;
  MTI := MTI_EVENTS_IDENTIFY;
end;

procedure TLccMessage.LoadTractionSetSpeed(ASourceID, ADestID: TNodeIDRec; ASpeed: THalfFloat);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 3;
  FDataArray[0] := TRACTION_SPEED_DIR;
  FDataArray[1] := Hi( ASpeed);
  FDataArray[2] := Lo( ASpeed);
  MTI := MTI_TRACTION_PROTOCOL;
end;


procedure TLccMessage.LoadVerifiedNodeID(ASourceID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  InsertNodeID(0, ASourceID);
  DataCount := 6;
  MTI := MTI_VERIFIED_NODE_ID_NUMBER;
end;

procedure TLccMessage.LoadTractionSetFunction(ASourceID, ADestID: TNodeIDRec; AnAddress: DWord; AValue: Word);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 6;
  FDataArray[0] := TRACTION_FUNCTION;
  FDataArray[1] := _Higher( AnAddress);
  FDataArray[2] := _Hi( AnAddress);
  FDataArray[3] := _Lo( AnAddress);
  FDataArray[4] := _Hi( AValue);
  FDataArray[5] := _Lo (AValue);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionEStop(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 1;
  FDataArray[0] := TRACTION_E_STOP;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionQuerySpeed(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 1;
  FDataArray[0] := TRACTION_QUERY_SPEED;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionQueryFunction(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 1;
  FDataArray[0] := TRACTION_QUERY_FUNCTION;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerAssign(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 9;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_ASSIGN;
  FDataArray[2] := 0;
  InsertNodeID(3, ANodeID);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerRelease(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 9;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_RELEASE;
  FDataArray[2] := 0;
  InsertNodeID(3, ANodeID);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerQuery(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 2;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_QUERY;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerChangeNotify(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 9;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_NOTIFY;
  FDataArray[2] := 0;
  InsertNodeID(3, ANodeID);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionControllerChangeNotifyReply(ASourceID,
  ADestID: TNodeIDRec; Allow: Boolean);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  MTI := MTI_TRACTION_REPLY;
  DataCount := 3;
  FDataArray[0] := TRACTION_CONTROLLER_CONFIG;
  FDataArray[1] := TRACTION_CONTROLLER_CONFIG_NOTIFY;
  if Allow then
    FDataArray[2] := 0
  else
    FDataArray[2] := $FF
end;

procedure TLccMessage.LoadTractionConsistAttach(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 9;
  FDataArray[0] := TRACTION_CONSIST;
  FDataArray[1] := TRACTION_CONSIST_ATTACH;
  FDataArray[2] := 0;
  InsertNodeID(3, ANodeID);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionConsistDetach(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 9;
  FDataArray[0] := TRACTION_CONSIST;
  FDataArray[1] := TRACTION_CONSIST_DETACH;
  FDataArray[2] := 0;
  InsertNodeID(3, ANodeID);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionConsistQuery(ASourceID, ADestID: TNodeIDRec; ANodeID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 9;
  FDataArray[0] := TRACTION_CONSIST;
  FDataArray[1] := TRACTION_CONSIST_QUERY;
  FDataArray[2] := 0;
  InsertNodeID(3, ANodeID);
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadTractionManage(ASourceID, ADestID: TNodeIDRec;Reserve: Boolean);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 2;
  FDataArray[0] := TRACTION_MANAGE;
  if Reserve then
    FDataArray[1] := TRACTION_MANAGE_RESERVE
  else
    FDataArray[1] := TRACTION_MANAGE_RELEASE;
  MTI := MTI_TRACTION_PROTOCOL;
end;

procedure TLccMessage.LoadSimpleTrainNodeIdentInfoRequest(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  MTI := MTI_SIMPLE_TRAIN_INFO_REQUEST;
end;

procedure TLccMessage.LoadSimpleNodeIdentInfoReply(ASourceID, ADestID: TNodeIDRec; SimplePackedArray: TSimpleNodeInfoPacked);
var
  i: Integer;
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  for i := 0 to Length(SimplePackedArray) - 1 do
    FDataArray[i] := SimplePackedArray[i];
  DataCount := Length(SimplePackedArray);
  MTI := MTI_SIMPLE_NODE_INFO_REPLY;
end;

procedure TLccMessage.LoadSimpleNodeIdentInfoRequest(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 0;
  MTI := MTI_SIMPLE_NODE_INFO_REQUEST;
end;

procedure TLccMessage.LoadFDIRequest(ASourceID, ADestID: TNodeIDRec);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 8;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_READ;
  FDataArray[2] := 0;
  FDataArray[3] := 0;
  FDataArray[4] := 0;
  FDataArray[5] := 0;
  FDataArray[6] := MSI_FDI;
  FDataArray[7] := 64;                     // Read until the end.....
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadFunctionConfigurationRead(ASourceID, ADestID: TNodeIDRec; FunctionAddress: DWord; Count: Integer);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 8;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_READ;
  FDataArray[2] := _Highest(FunctionAddress);
  FDataArray[3] := _Higher(FunctionAddress);  // F0..F28
  FDataArray[4] := _Hi(FunctionAddress);
  FDataArray[5] := _Lo(FunctionAddress);
  FDataArray[6] := MSI_FUNCTION_CONFIG;
  FDataArray[7] := Count*2;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadFunctionConfigurationWrite(ASourceID, ADestID: TNodeIDRec; FunctionAddress: DWord; Count: Integer; Functions: TFunctionStatesArray);
var
  i: Integer;
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_WRITE;
  FDataArray[2] := _Highest(FunctionAddress);
  FDataArray[3] := _Higher(FunctionAddress);  // F0..F28
  FDataArray[4] := _Hi(FunctionAddress);
  FDataArray[5] := _Lo(FunctionAddress);
  FDataArray[6] := MSI_FUNCTION_CONFIG;
  DataCount := 7;
  for i := 0 to Count - 1 do
  begin
    FDataArray[DataCount] := Hi(Functions[i]);
    Inc(FDataCount);
    FDataArray[DataCount] := Lo(Functions[i]);
    Inc(FDataCount);
  end;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadCDIRequest(ASourceID, ADestID: TNodeIDRec);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 8;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_READ;
  FDataArray[2] := 0;
  FDataArray[3] := 0;
  FDataArray[4] := 0;
  FDataArray[5] := 0;
  FDataArray[6] := MSI_CDI;
  FDataArray[7] := 64;                     // Read until the end.....
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadCID(ASource: TNodeIDRec; ACID: Byte);
begin
  ZeroFields;
  FSource.Alias := ASource.Alias;
  CAN.Active := True;
  FSource.ID := ASource.ID;
  case ACID of
    0 : CAN.MTI := MTI_CAN_CID0 or DWord(ASource.Alias) or (ASource.ID[1] and $00FFF000);
    1 : CAN.MTI := MTI_CAN_CID1 or DWord(ASource.Alias) or ((ASource.ID[1] shl 12) and $00FFF000);
    2 : CAN.MTI := MTI_CAN_CID2 or DWord(ASource.Alias) or (ASource.ID[0] and $00FFF000);
    3 : CAN.MTI := MTI_CAN_CID3 or DWord(ASource.Alias) or ((ASource.ID[0] shl 12) and $00FFF000);
  end;
end;

procedure TLccMessage.LoadDatagramAck(ASourceID, ADestID: TNodeIDRec; Ok: Boolean; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 1;
  if ReplyPending then
    DataArrayIndexer[0] := $80 or (TimeoutValueN and $0F)
  else
    DataArrayIndexer[0] := 0;
  if Ok then
    MTI := MTI_DATAGRAM_OK_REPLY
  else
    MTI := MTI_DATAGRAM_REJECTED_REPLY;
end;

procedure TLccMessage.LoadDatagramRejected(ASourceID, ADestID: TNodeIDRec; Reason: Word);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  DataCount := 2;
  MTI := MTI_DATAGRAM_REJECTED_REPLY;
  FDataArray[0] := _Hi(Reason);
  FDataArray[1] := _Lo(Reason);
end;

procedure TLccMessage.LoadConfigMemAddressSpaceInfo(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_OP_GET_ADD_SPACE_INFO;
  FDataArray[2] := AddressSpace;
  FDataCount := 3;
  FMTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemOptions(ASourceID, ADestID: TNodeIDRec);
begin
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  FDataArray[1] := MCP_OP_GET_CONFIG;
  FDataCount := 2;
  FMTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemRead(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; ConfigMemSize: Byte);
begin
  if ConfigMemSize > 64 then
    ConfigMemSize := 64;

  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if AddressSpace < MSI_CONFIG then
  begin
    FDataArray[1] := MCP_READ;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    FDataArray[6] := AddressSpace;
    FDataArray[7] := ConfigMemSize;
    DataCount := 8;
  end else
  begin
    case AddressSpace of
      MSI_CDI : FDataArray[1] := MCP_READ or MCP_CDI;
      MSI_ALL : FDataArray[1] := MCP_READ or MCP_ALL;
      MSI_CONFIG : FDataArray[1] := MCP_READ or MCP_CONFIGURATION;
    end;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    FDataArray[6] := ConfigMemSize;
    DataCount := 7;
  end;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemWriteArray(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; ArraySize: Integer; AnArray: array of Byte);
var
  i, iDatagram, DatagramCount, DatagramLength, iArrayPos: Integer;
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....

  if ArraySize mod 64 = 0 then
    DatagramCount := ArraySize div 64
  else
    DatagramCount := (ArraySize div 64) + 1;

  for iDatagram := 0 to DatagramCount - 1 do
  begin
    DatagramLength := ArraySize;
    if DatagramLength > 64 then
      DatagramLength := 64;

    ZeroFields;
    Source := ASourceID;
    Destination := ADestID;
    FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
    if AddressSpace < MSI_CONFIG then
    begin
      FDataArray[1] := MCP_WRITE;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      FDataArray[6] := AddressSpace;
      DataCount := 7;
    end else
    begin
      case AddressSpace of
        MSI_CDI : FDataArray[1] := MCP_WRITE or MCP_CDI;
        MSI_ALL : FDataArray[1] := MCP_WRITE or MCP_ALL;
        MSI_CONFIG : FDataArray[1] := MCP_WRITE or MCP_CONFIGURATION;
      end;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      DataCount := 6;
    end;

    iArrayPos := 0;
    for i := 0 to DatagramLength - 1 do
    begin
      FDataArray[DataCount] := AnArray[iArrayPos];
      Inc(FDataCount);
      Inc(iArrayPos);
      Inc(ConfigMemAddress);
    end;

    MTI := MTI_DATAGRAM;
  end;
end;

procedure TLccMessage.LoadConfigMemWriteInteger(ASourceID, ADestID: TNodeIDRec; AddressSpace: Byte; ConfigMemAddress: DWord; IntegerSize: Byte; DataInteger: Integer);
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....
  ZeroFields;
  Source := ASourceID;
  Destination := ADestID;
  FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
  if AddressSpace < MSI_CONFIG then
  begin
    FDataArray[1] := MCP_WRITE;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    FDataArray[6] := AddressSpace;
    DataCount := 7;
  end else
  begin
    case AddressSpace of
      MSI_CDI : FDataArray[1] := MCP_WRITE or MCP_CDI;
      MSI_ALL : FDataArray[1] := MCP_WRITE or MCP_ALL;
      MSI_CONFIG : FDataArray[1] := MCP_WRITE or MCP_CONFIGURATION;
    end;
    FDataArray[2] := _Highest(ConfigMemAddress);
    FDataArray[3] := _Higher(ConfigMemAddress);
    FDataArray[4] := _Hi(ConfigMemAddress);
    FDataArray[5] := _Lo(ConfigMemAddress);
    DataCount := 6;
  end;

  case IntegerSize of
    1 : begin
          FDataArray[DataCount] := _Lo(DataInteger);
          Inc(FDataCount, 1);
        end;
    2 : begin
          InsertWordAsDataBytes(Word( DataInteger), DataCount);
          Inc(FDataCount, 2);
        end;
    4 : begin
          InsertDWordAsDataBytes(DWord( DataInteger), DataCount);
          Inc(FDataCount, 4);
        end;
  end;
  MTI := MTI_DATAGRAM;
end;

procedure TLccMessage.LoadConfigMemWriteString(ASourceID, ADestID: TNodeIDRec;
  AddressSpace: Byte; ConfigMemAddress: DWord; AString: String);
var
  i, iDatagram, DatagramCount, iStringPos, DatagramLength, StrLength: Integer;
begin
  // Really should be a Get Address Space Info message here to make sure the start address is 0.....

  iStringPos := 1;         // 1 indexed

  StrLength := Length(AString) + 1;  // Include the Null

  if StrLength mod 64 = 0 then
    DatagramCount := StrLength div 64
  else
    DatagramCount := (StrLength div 64) + 1;

  for iDatagram := 0 to DatagramCount - 1 do
  begin
    DatagramLength := StrLength - (iStringPos - 1);
    if DatagramLength > 64 then
      DatagramLength := 64;

    ZeroFields;
    Source := ASourceID;
    Destination := ADestID;
    FDataArray[0] := DATAGRAM_PROTOCOL_CONFIGURATION;
    if AddressSpace < MSI_CONFIG then
    begin
      FDataArray[1] := MCP_WRITE;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      FDataArray[6] := AddressSpace;
      DataCount := 7;
    end else
    begin
      case AddressSpace of
        MSI_CDI : FDataArray[1] := MCP_WRITE or MCP_CDI;
        MSI_ALL : FDataArray[1] := MCP_WRITE or MCP_ALL;
        MSI_CONFIG : FDataArray[1] := MCP_WRITE or MCP_CONFIGURATION;
      end;
      FDataArray[2] := _Highest(ConfigMemAddress);
      FDataArray[3] := _Higher(ConfigMemAddress);
      FDataArray[4] := _Hi(ConfigMemAddress);
      FDataArray[5] := _Lo(ConfigMemAddress);
      DataCount := 6;
    end;

    if DatagramLength = 1 then
    begin
      FDataArray[DataCount] := Ord(#0);
      Inc(FDataCount);
    end else
    begin
      for i := 0 to DatagramLength - 1 do
      begin
        FDataArray[DataCount] := Ord( AString[iStringPos]);
        Inc(FDataCount);
        Inc(iStringPos);
        Inc(ConfigMemAddress);
      end;
    end;
    MTI := MTI_DATAGRAM;
  end;
end;

initialization
  CaptureTime := 1;
  GlobalSendEvent := TSimpleEvent.Create;
  ClassicSNIP := True;

finalization
  FreeAndNil(GlobalSendEvent);

end.

