unit protocol.datagram.configuration.memory;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}


uses
  Classes, SysUtils, lcc.objects, lcc.types, lcc.message;

type
   { TProtocolConfigurationMemory }

  TProtocolConfigurationMemory = class(TNodeProtocolBase)
  private
    FAddress: DWord;
    FAddressSpace: Byte;
    FDataCount: Integer;
    FDataRaw: TDatagramArray;
    FDataType: TLccConfigDataType;
    FDataTypeBit: Byte;
    FDataTypeEvent: TEventID;
    FDataTypeInteger: Integer;
    FDataTypeString: String;
    FInProcessAddress: DWord;
    function GetDataRawIndexer(iIndex: Word): Byte;

    procedure SetDataRawIndexer(iIndex: Word; const Value: Byte);
  protected
    property InProcessAddress: DWord read FInProcessAddress write FInProcessAddress;
  public
    property Address: DWord read FAddress write FAddress;
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
    property DataCount: Integer read FDataCount write FDataCount;
    property DataRaw: TDatagramArray read FDataRaw write FDataRaw;
    property DataRawIndexer[iIndex: Word]: Byte read GetDataRawIndexer write SetDataRawIndexer;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property DataTypeInteger: Integer read FDataTypeInteger;
    property DataTypeEvent: TEventID read FDataTypeEvent;
    property DataTypeBit: Byte read FDataTypeBit;
    property DataTypeString: String read FDataTypeString;
    procedure Initialize(AnAddress: DWord; AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TProtocolConfigurationMemory }

function TProtocolConfigurationMemory.GetDataRawIndexer(iIndex: Word): Byte;
begin
  Result := FDataRaw[iIndex]
end;

procedure TProtocolConfigurationMemory.Initialize(AnAddress: DWord; AnAddressSpace: Byte; DataSize: Integer; ADataType: TLccConfigDataType);
begin
  ErrorCode := 0;
  Address := AnAddress;
  DataCount := DataSize;
  AddressSpace := AnAddressSpace;
  InProcessAddress := AnAddress;
  DataType := ADataType;
  FDataTypeInteger := 0;
  FDataTypeEvent[0] := 0;
  FDataTypeEvent[1] := 0;
  FDataTypeEvent[2] := 0;
  FDataTypeEvent[3] := 0;
  FDataTypeEvent[4] := 0;
  FDataTypeEvent[5] := 0;
  FDataTypeEvent[6] := 0;
  FDataTypeEvent[7] := 0;
  FDataTypeBit := 0;
  FDataTypeString := '';
end;

function TProtocolConfigurationMemory.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  iStart, i, RemainingCount: Integer;

  LocalAddressSpace: Byte;
begin
  Result := True;
  LocalAddressSpace := 0;
  RemainingCount := 0;
  if LccMessage.DataArrayIndexer[1] and MCP_READ_REPLY = MCP_READ_REPLY then
  begin
    // First Block of Data
    if InProcessAddress = Address then
    begin
      if LccMessage.DataArrayIndexer[1] and $03 <> 0 then
      begin
         case LccMessage.DataArrayIndexer[1] and $03 of
           MCP_CDI           : LocalAddressSpace := MSI_CDI;
           MCP_ALL           : LocalAddressSpace := MSI_ALL;
           MCP_CONFIGURATION : LocalAddressSpace := MSI_CONFIG;
         end;
         iStart := 6;
      end else
      begin
         LocalAddressSpace := LccMessage.DataArrayIndexer[6];
         iStart := 7
      end;
      if LocalAddressSpace <> AddressSpace then
        ErrorCode := ErrorCode or ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH;
    end else
    begin
      // Subsequent Blocks of Data
      if LccMessage.DataArrayIndexer[1] and $03 <> 0 then
        iStart := 6
      else
        iStart := 7
    end;

    DataCount := 0;
    if ErrorCode = 0 then
    begin
      DataCount := LccMessage.DataCount - iStart;
      for i := 0 to DataCount - 1 do
        DataRawIndexer[i] := LccMessage.DataArrayIndexer[i + iStart];
      case DataType of
        cdt_String :
          begin
            InProcessAddress := InProcessAddress + DWord((LccMessage.DataCount - iStart));
            for i := 0 to LccMessage.DataCount - iStart - 1 do
              FDataTypeString := FDataTypeString + Chr(LccMessage.DataArrayIndexer[i+iStart]);

            RemainingCount := DataCount - Length(FDataTypeString);           // Strings are 1 indexed
            if RemainingCount > 64 then
              RemainingCount := 64;
            if RemainingCount > 0 then
            begin
              WorkerMessage.LoadConfigMemRead(LccMessage.Destination, LccMessage.Source, MSI_CONFIG, InProcessAddress, RemainingCount);
  //            OwnerManager.DoRequestMessageSend(WorkerMessage);
            end
          end;
        cdt_Int :
          begin
            FDataTypeInteger := LccMessage.ExtractDataBytesAsInt(iStart, LccMessage.DataCount-1);
            RemainingCount := 0;
          end;
        cdt_EventID :
          begin
            FDataTypeEvent := LccMessage.ExtractDataBytesAsEventID(iStart)^;
            RemainingCount := 0;
          end;
        cdt_Bit :
          begin
            // ToDo
          end;
       end
    end;

    if (ErrorCode = 0) or (RemainingCount <= 0) then
    begin
 {     SourceNode := OwnerManager.FindMirroredNodeBySourceID(LccMessage, True);
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available
        OwnerManager.CdiParser.DoConfigMemReadReply(SourceNode);
      OwnerManager.DoConfigMemReadReply(SourceNode, OwnerManager.FindMirroredNodeByDestID(LccMessage, True));
 }   end;
  end else
  if LccMessage.DataArrayIndexer[1] and MCP_WRITE_REPLY <> 0 then
  begin
    ErrorCode := 0;

    if ErrorCode = 0 then
    begin
 {     SourceNode := OwnerManager.FindMirroredNodeBySourceID(LccMessage, True);
      if Assigned(OwnerManager.CdiParser) then    // Callback on the CDI Parser if available
        OwnerManager.CdiParser.DoConfigMemWriteReply(SourceNode);
      OwnerManager.DoConfigMemWriteReply(SourceNode, OwnerManager.FindMirroredNodeByDestID(LccMessage, True));
 }   end;
  end;
end;

procedure TProtocolConfigurationMemory.SetDataRawIndexer(iIndex: Word; const Value: Byte);
begin
  FDataRaw[iIndex] := Value
end;

end.

