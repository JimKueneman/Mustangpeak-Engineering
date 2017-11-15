unit lcc.node;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I lcc_compilers.inc}

// DatagramQueue.TickTimeout;  NEED TO IMPLEMENT TIMER FOR TICKING THE DATAGRAM QUEUE

interface

uses
  Classes, SysUtils, lcc.types, {$IFDEF FPC}contnrs,{$ELSE}System.Generics.Collections,{$ENDIF}
  {$IFDEF FPC}fpTimer,{$ELSE}XMLDoc,{$ENDIF} lcc.types.can,
  lcc.message, protocol.snip, protocol.events, protocol.pip, protocol.acdi.mfg,
  protocol.acdi.user, lcc.utilities, mustangpeak.xmlutilities,
  mustangpeak.threadedcirculararray, protocol.datagram.configuration,
  protocol.datagram.configuration.memory, protocol.datagram.confguration.memory.options,
  protocol.datagram.configuration.memory.info, protocol.datagram.configuration.definition.information;

type
  TDatagramQueue = class;
  TLccNode = class;

  { TLccAliasIDEngine }

  TLccAliasIDEngine = class
  private
    FAliasRegistrationComplete: Boolean;
    FEnabled: Boolean;
    FLccNode: TLccNode;
    FLoginID: TNodeIDRec;   // ID holds the Seed and the Alias is the alias we are trying to register
    FPermitted: Boolean;
    FState: Integer;
    FTimer: TFPTImer;
    FTimerBlank: Boolean;
    procedure SetTimerBlank(AValue: Boolean);
  protected
    property LoginID: TNodeIDRec read FLoginID write FLoginID;
    property State: Integer read FState write FState;
    property LccNode: TLccNode read FLccNode write FLccNode;
    property Timer: TFPTimer read FTimer write FTimer;
    property TimerBlank: Boolean read FTimerBlank write SetTimerBlank;


    function CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
    function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
    procedure PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
    procedure OnTimerProc(Sender: TObject);

  public
    property AliasRegistationComplete: Boolean read FAliasRegistrationComplete;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Permitted: Boolean read FPermitted;
    constructor Create;
    destructor Destroy; override;
    function CheckForDuplicateAlias(LccMessage: TLccMessage): Boolean;
    procedure ProcessMessages;
  end;

  { TLccNodeTLccNode }

  TLccNode = class(TPersistent)
  private
    FAliasIDEngine: TLccAliasIDEngine;
    FCDI: TMustangpeakXmlDocument;
    FCdiFileName: string;
    FDatagramQueue: TDatagramQueue;
    FInitialized: Boolean;
    FMsgQueueReceived: TThreadedCirularArrayObject;
    FMsgQueueSending: TThreadedCirularArrayObject;
    FNodeID: TNodeIDRec;
    FProtocolAcdiMfg: TProtocolAcdiMfg;
    FProtocolAcdiUser: TProtocolAcdiUser;
    FProtocolConfigDefinitionInfo: TProtocolConfigDefinitionInfo;
    FProtocolConfigMemAddressSpaceInfo: TProtocolConfigMemAddressSpaceInfo;
    FProtocolConfiguration: TProtocolConfiguration;
    FProtocolConfigurationMemOptions: TProtocolConfigurationMemOptions;
    FProtocolConfigurationMemory: TProtocolConfigurationMemory;
    FProtocolEventsConsumed: TProtocolEvents;
    FProtocolEventsProduced: TProtocolEvents;
    FProtocolPip: TProtocolPip;
    FProtocolSnip: TProtocolSnip;
    function GetNodeIDStr: String;
  protected
    function ProcessNodeDefinitionXml(XML: TMustangpeakXmlDocument): Boolean;
    procedure MsgQueueSendAdded(Sender: TObject);
    procedure MsgQueueReceiveAdded(Sender: TObject);
    procedure SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
    procedure SendAllProducedEvents;
    procedure SendAllConsumedEvents;
    procedure SendAllEvents;
    property CDI: TMustangpeakXmlDocument read FCDI write FCDI;
    property CdiFileName: string read FCdiFileName write FCdiFileName;
    property DatagramQueue: TDatagramQueue read FDatagramQueue write FDatagramQueue;
  public
    constructor Create(NodeDefinitionXmlFile: string); {$IFNDEF FPC}overload;{$ENDIF}
    destructor Destroy; override;
    function Start: Boolean; virtual;
    function ProcessMessages: Boolean;
    function MessageAddressedToNode(LccMessage: TLccMessage): Boolean;

    property AliasIDEngine: TLccAliasIDEngine read FAliasIDEngine write FAliasIDEngine;
    property Initialized: Boolean read FInitialized;
    property MsgQueueReceived: TThreadedCirularArrayObject read FMsgQueueReceived write FMsgQueueReceived;
    property MsgQueueSending: TThreadedCirularArrayObject read FMsgQueueSending write FMsgQueueSending;
    property NodeID: TNodeIDRec read FNodeID;
    property NodeIDStr: String read GetNodeIDStr;
    property ProtocolAcdiMfg: TProtocolAcdiMfg read FProtocolAcdiMfg;
    property ProtocolAcdiUser: TProtocolAcdiUser read FProtocolAcdiUser;
    property ProtocolConfiguration: TProtocolConfiguration read FProtocolConfiguration write FProtocolConfiguration;
    property ProtocolConfigurationMemory: TProtocolConfigurationMemory read FProtocolConfigurationMemory write FProtocolConfigurationMemory;
    property ProtocolConfigurationMemOptions: TProtocolConfigurationMemOptions read FProtocolConfigurationMemOptions write FProtocolConfigurationMemOptions;
    property ProtocolConfigMemAddressSpaceInfo: TProtocolConfigMemAddressSpaceInfo read FProtocolConfigMemAddressSpaceInfo write FProtocolConfigMemAddressSpaceInfo;
    property ProtocolConfigDefinitionInfo: TProtocolConfigDefinitionInfo read FProtocolConfigDefinitionInfo write FProtocolConfigDefinitionInfo;
    property ProtocolEventsConsumed: TProtocolEvents read FProtocolEventsConsumed;
    property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced;
    property ProtocolPip: TProtocolPip read FProtocolPip;
    property ProtocolSnip: TProtocolSnip read FProtocolSnip;
  end;

  { TDatagramQueue }

  TDatagramQueue = class
  private
    FOwnerNode: TLccNode;
    FQueue: TObjectList{$IFNDEF FPC}<TLccMessage>{$ENDIF};
    FTimer: TFPTimer;
  protected
    property OwnerNode: TLccNode read FOwnerNode write FOwnerNode;
    property Queue: TObjectList{$IFNDEF FPC}<TLccMessage>{$ENDIF} read FQueue write FQueue;
    property Timer: TFPTimer read FTimer write FTimer;
    function FindBySourceNode(LccMessage: TLccMessage): Integer;
    procedure OnTimer(Sender: TObject);
  public
    constructor Create(ANode: TLccNode);
    destructor Destroy; override;
    procedure Add(LccMessage: TLccMessage);
    procedure Clear;
    function Resend(LccMessage: TLccMessage): TLccMessage;
    procedure Remove(LccMessage: TLccMessage);
  end;

var
  GlobalNodeList: TThreadedCirularArrayObject;

implementation

{ TLccAliasIDEngine }

constructor TLccAliasIDEngine.Create;
begin
  inherited Create;
  Timer := TFPTimer.Create(nil);
  Timer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnTimerProc;
  TimerBlank := True;
  {$IFDEF FPC}
  Timer.StartTimer;
  {$ELSE}
  Timer.Enabled := True;
  {$ENDIF}
end;

function TLccAliasIDEngine.CheckForDuplicateAlias(LccMessage: TLccMessage): Boolean;
var
  LocalLccMessage: TLccMessage;
begin
  Result := False;
  if Assigned(LccMessage) then
  begin
    if LccMessage.Source.Alias = LoginID.Alias then
    begin

      TimerBlank := True;  // Stop the timer if it is ticking

      case LccMessage.CAN.MTI of
        MTI_CAN_CID0,
        MTI_CAN_CID1,
        MTI_CAN_CID2,
        MTI_CAN_CID3 :
          begin
            LocalLccMessage := TLccMessage.Create;
            LocalLccMessage.LoadRID(LoginID);  // Only the Alias is used here so this is ok
            LccNode.MsgQueueSending.Add(LocalLccMessage);
          end
        else begin
          if Permitted then
          begin
            LocalLccMessage := TLccMessage.Create;
            LocalLccMessage.LoadAMR(LccNode.NodeID);  // Both the Alias and ID must be valid
            LccNode.MsgQueueSending.Add(LocalLccMessage);
          end;
          FAliasRegistrationComplete := False;
          FPermitted := False;   // Start over
          FState := 0;           // Will restart the timer
          Result := True;
        end;
      end;
    end;
  end;
end;

procedure TLccAliasIDEngine.SetTimerBlank(AValue: Boolean);
begin
  if FTimerBlank=AValue then Exit;
  FTimerBlank:=AValue;
  CheckSynchronize;  // Cause the event to be fired if the timer thread is already waiting to call it
end;

function TLccAliasIDEngine.CreateAliasID(var Seed: TNodeID; Regenerate: Boolean): Word;
begin
  if Regenerate then
    PsudoRandomNumberGeneratorOnSeed(Seed);
  Result := GenerateID_Alias_From_Seed(Seed);
  if Result = 0 then
  begin
    PsudoRandomNumberGeneratorOnSeed(Seed);
    Result := GenerateID_Alias_From_Seed(Seed);
  end;
end;

destructor TLccAliasIDEngine.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

function TLccAliasIDEngine.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccAliasIDEngine.OnTimerProc(Sender: TObject);
begin
  if not TimerBlank then
    Inc(FState);  // Don't try to modify the timer within the timer procedure it does not work on some platforms
end;

procedure TLccAliasIDEngine.ProcessMessages;
var
  LocalLccMessage: TLccMessage;
begin
  CheckSynchronize;  // Make the timer tick
  case State of
    0: begin
         FPermitted := False;
         FAliasRegistrationComplete := False;
         FLoginID.Alias := CreateAliasID(FLoginID.ID, True);
         LocalLccMessage := TLccMessage.Create;
         LocalLccMessage.LoadCID(LoginID, 0);
         LccNode.MsgQueueSending.Add(LocalLccMessage);
         LocalLccMessage := TLccMessage.Create;
         LocalLccMessage.LoadCID(LoginID, 1);
         LccNode.MsgQueueSending.Add(LocalLccMessage);
         LocalLccMessage := TLccMessage.Create;
         LocalLccMessage.LoadCID(LoginID, 2);
         LccNode.MsgQueueSending.Add(LocalLccMessage);
         LocalLccMessage := TLccMessage.Create;
         LocalLccMessage.LoadCID(LoginID, 3);
         LccNode.MsgQueueSending.Add(LocalLccMessage);
         TimerBlank := True;
         Timer.Interval := 200;
         TimerBlank := False;
         Inc(FState);
       end;
    1: begin
         //  increments when the timer expires

       end;
    2: begin
         // Called after the StartTimer expires
         LocalLccMessage := TLccMessage.Create;
         LocalLccMessage.LoadRID(LoginID);
         LccNode.MsgQueueSending.Add(LocalLccMessage);
         LocalLccMessage := TLccMessage.Create;
         LocalLccMessage.LoadAMD(LoginID);
         LccNode.MsgQueueSending.Add(LocalLccMessage);
         LccNode.FNodeID.Alias := LoginID.Alias;    // Copy the Alias over to the Node for use (the ID is the Seed)
         FPermitted := True;
         TimerBlank := True;
         Timer.Interval := 100;
         TimerBlank := False;
         Inc(FState);
       end;
    3: begin
        // Looking for an error, increments when the timer expires
       end;
    4: begin
         LccNode.SendAllEvents;
         FAliasRegistrationComplete := True;
         TimerBlank := True;
         Timer.Interval := 100;   // Timer will hang in Ultibo so leave it running but with a huge interval
         TimerBlank := False;
         Inc(FState);
       end;
    5: begin
    //     beep;
       end;
  end;
end;

procedure TLccAliasIDEngine.PsudoRandomNumberGeneratorOnSeed(var Seed: TNodeID);
var
  temp1,              // Upper 24 Bits of temp 48 bit number
  temp2: DWORD;       // Lower 24 Bits of temp 48 Bit number
begin
  temp1 := ((Seed[1] shl 9) or ((Seed[0] shr 15) and $000001FF)) and $00FFFFFF;   // x(i+1)(2^9 + 1)*x(i) + C  = 2^9 * x(i) + x(i) + C
  temp2 := (Seed[0] shl 9) and $00FFFFFF;                                                                  // Calculate 2^9 * x

  Seed[0] := Seed[0] + temp2 + $7A4BA9;   // Now y = 2^9 * x so all we have left is x(i+1) = y + x + c
  Seed[1] := Seed[1] + temp1 + $1B0CA3;

  Seed[1] := (Seed[1] and $00FFFFFF) or (Seed[0] and $FF000000) shr 24;   // Handle the carries of the lower 24 bits into the upper
  Seed[0] := Seed[0] and $00FFFFFF;
end;

{ TLccNode }

function TLccNode.GetNodeIDStr: String;
begin
   Result := NodeIDToString(FNodeID.ID, False)
end;

function TLccNode.MessageAddressedToNode(LccMessage: TLccMessage): Boolean;
begin
  if (LccMessage.Destination.ID[0] = 0) and (LccMessage.Destination.ID[1] = 0) then
    Result := LccMessage.Destination.Alias = NodeID.Alias
  else
    Result := (LccMessage.Destination.ID[0] = NodeID.ID[0]) and (LccMessage.Destination.ID[1] = NodeID.ID[1])
end;

procedure TLccNode.MsgQueueReceiveAdded(Sender: TObject);
begin

end;

procedure TLccNode.MsgQueueSendAdded(Sender: TObject);
begin
  GlobalSendEvent.SetEvent;          // This is called after adding an object so by definition it is within the Queue Lock mechinism (if the Queue is beeing used correctly)
end;

function TLccNode.ProcessMessages: Boolean;
var
  LccMessages: TDynamicArrayObject;
  LccMessage, LccMessageOut: TLccMessage;
  i: Integer;
  LocalNodeID: TNodeID;
  LocalEventID: TEventID;
  EventObj: TLccEvent;
begin
  Result := False;
  Sleep(1);

  LccMessages := nil;

  // If using aliases run the message loop to manage them.
  if AliasIDEngine.Enabled then
    AliasIDEngine.ProcessMessages;

  if not Initialized and (AliasIDEngine.Permitted or not AliasIDEngine.Enabled) then
  begin
    LccMessage := TLccMessage.Create;
    LccMessage.LoadInitializationComplete(NodeID);
    // The message can dissappear ANY instant after adding to the Queue!!!!
    MsgQueueSending.Add(LccMessage);
    FInitialized := True;
  end;

  // Extract the messages in one block to work on
  MsgQueueReceived.RemoveChunk(LccMessages);

  for i := 0 to Length(LccMessages) - 1 do
  begin
    LccMessage := LccMessages[i] as TLccMessage;

    // If using Aliases check to see if we have dupicate aliased and react if we do
    if AliasIDEngine.Enabled then
    begin
      if AliasIDEngine.CheckForDuplicateAlias(LccMessage) then
        FInitialized := False;
    end;

    // Run the real MTI message loop if the node is registered on the network if the
    // Alias is ready to use, if using GridConnect, and the node is initialized
    if (AliasIDEngine.AliasRegistationComplete or not AliasIDEngine.Enabled) and (Initialized) then
    begin
      if LccMessage.IsCAN then
      begin
        case LccMessage.CAN.MTI of
          MTI_CAN_AME  :
              begin   // Allowed while not Permitted
                if LccMessage.DataCount = 6 then
                begin
                  LccMessage.ExtractDataBytesAsNodeID(0, LocalNodeID);
                  if EqualNodeID(LocalNodeID, NodeID.ID, False) then
                  begin
                    LccMessageOut := TLccMessage.Create;
                    LccMessageOut.LoadAMD(NodeID);
                    // The message can dissappear ANY instant after adding to the Queue!!!!
                    MsgQueueSending.Add(LccMessageOut);
                  end
                end else
                begin
                  LccMessageOut := TLccMessage.Create;
                  LccMessageOut.LoadAMD(NodeID);
                  // The message can dissappear ANY instant after adding to the Queue!!!!
                  MsgQueueSending.Add(LccMessageOut);
                end;
                Result := True;
              end;
          MTI_CAN_AMD  :
              begin
                if LccMessage.DataCount = 6 then
                begin
                  LccMessage.ExtractDataBytesAsNodeID(0, LocalNodeID);
                  if EqualNodeID(LocalNodeID, NodeID.ID, False) then                  // some Dog has my Node ID!
                  begin
                    LccMessageOut := TLccMessage.Create;
                    LccMessageOut.LoadPCER(NodeID, @EVENT_DUPLICATE_ID_DETECTED);
                    // The message can dissappear ANY instant after adding to the Queue!!!!
                    MsgQueueSending.Add(LccMessageOut);
                  end
                end;
                Result := True;
              end;
          MTI_CAN_RID  : begin end;
        end;
      end;

      case LccMessage.MTI of
        MTI_OPTIONAL_INTERACTION_REJECTED :
            begin
            end;
        MTI_VERIFY_NODE_ID_NUMBER      :
            begin
              if LccMessage.DataCount = 6 then
              begin
                LccMessage.ExtractDataBytesAsNodeID(0, LocalNodeID);
                if EqualNodeID(LocalNodeID, NodeID.ID, False) then
                begin
                  LccMessageOut := TLccMessage.Create;
                  LccMessageOut.LoadVerifiedNodeID(NodeID);
                  // The message can dissappear ANY instant after adding to the Queue!!!!
                  MsgQueueSending.Add(LccMessageOut);
                end
              end else
              begin
                LccMessageOut := TLccMessage.Create;
                LccMessageOut.LoadVerifiedNodeID(NodeID);
                // The message can dissappear ANY instant after adding to the Queue!!!!
                MsgQueueSending.Add(LccMessageOut);
              end;
              Result := True;
            end;
        MTI_VERIFY_NODE_ID_NUMBER_DEST :
            begin
              if MessageAddressedToNode(LccMessage) then
              begin
                LccMessageOut := TLccMessage.Create;
                LccMessageOut.LoadVerifiedNodeID(NodeID);
                // The message can dissappear ANY instant after adding to the Queue!!!!
                MsgQueueSending.Add(LccMessageOut);
              end;
              Result := True;
            end;
        MTI_VERIFIED_NODE_ID_NUMBER :
            begin

            end;
        MTI_SIMPLE_NODE_INFO_REQUEST :
            begin
              if MessageAddressedToNode(LccMessage) then
              begin
                LccMessageOut := TLccMessage.Create;
                LccMessageOut.LoadSimpleNodeIdentInfoReply(NodeID, LccMessage.Source, ProtocolSnip.PackedFormat);
                // The message can dissappear ANY instant after adding to the Queue!!!!
                MsgQueueSending.Add(LccMessageOut);
              end;
              Result := True;
            end;
        MTI_SIMPLE_NODE_INFO_REPLY :
            begin               // Called if I send a SNIP;
              LccMessageOut := TLccMessage.Create;
              ProtocolPip.ProcessMessage(LccMessage);
              // The message can dissappear ANY instant after adding to the Queue!!!!
              MsgQueueSending.Add(LccMessageOut);
              Result := True;
            end;
        MTI_PROTOCOL_SUPPORT_INQUIRY :
            begin
              if MessageAddressedToNode(LccMessage) then
              begin
                LccMessageOut := TLccMessage.Create;
                LccMessageOut.LoadProtocolIdentifyReply(NodeID, LccMessage.Source, ProtocolPip.EncodeFlags);
                // The message can dissappear ANY instant after adding to the Queue!!!!
                MsgQueueSending.Add(LccMessageOut);
              end;
              Result := True;
            end;
        MTI_PROTOCOL_SUPPORT_REPLY :
            begin   // Called if I send a Protocol Support
              LccMessageOut := TLccMessage.Create;
              ProtocolPip.ProcessMessage(LccMessage);
              // The message can dissappear ANY instant after adding to the Queue!!!!
              MsgQueueSending.Add(LccMessageOut);
              Result := True;
            end;
        MTI_EVENTS_IDENTIFY :
            begin
              SendAllEvents;
              Result := True;
            end;
        MTI_EVENTS_IDENTIFY_DEST :
            begin
              if MessageAddressedToNode(LccMessage) then
                SendAllEvents;
              Result := True;
            end;
        MTI_PRODUCER_IDENDIFY :
            begin
              EventObj := ProtocolEventsProduced.Supports(LccMessage.ExtractDataBytesAsEventID(0)^);
              if Assigned(EventObj) then
              begin
                LccMessageOut := TLccMessage.Create;
                LocalEventID := EventObj.ID;
                LccMessageOut.LoadProducerIdentified(NodeID, LocalEventID, EventObj.State);
                // The message can dissappear ANY instant after adding to the Queue!!!!
                MsgQueueSending.Add(LccMessageOut);
              end;
              Result := True;
            end;
        MTI_CONSUMER_IDENTIFY :
            begin
              EventObj := ProtocolEventsConsumed.Supports(LccMessage.ExtractDataBytesAsEventID(0)^);
              if Assigned(EventObj) then
              begin
                LccMessageOut := TLccMessage.Create;
                LocalEventID := EventObj.ID;
                LccMessageOut.LoadConsumerIdentified(NodeID, LocalEventID, EventObj.State);
                // The message can dissappear ANY instant after adding to the Queue!!!!
                MsgQueueSending.Add(LccMessageOut);
              end;
              Result := True;
            end;
         MTI_CONSUMER_IDENTIFIED_CLEAR :
            begin
            end;
         MTI_CONSUMER_IDENTIFIED_SET :
            begin
            end;
         MTI_CONSUMER_IDENTIFIED_UNKNOWN :
            begin
            end;
         MTI_PRODUCER_IDENTIFIED_CLEAR :
            begin
            end;
         MTI_PRODUCER_IDENTIFIED_SET :
            begin
            end;
         MTI_PRODUCER_IDENTIFIED_UNKNOWN :
            begin
            end;
         MTI_DATAGRAM_REJECTED_REPLY :
           begin
             MsgQueueSending.Add( DatagramQueue.Resend(LccMessage));
           end;
         MTI_DATAGRAM_OK_REPLY :
           begin
             DatagramQueue.Remove(LccMessage);
           end;
         MTI_DATAGRAM :
           begin
             if MessageAddressedToNode(LccMessage) then
             begin
               case LccMessage.DataArrayIndexer[0] of
                 DATAGRAM_PROTOCOL_CONFIGURATION :
                   begin
                     case LccMessage.DataArrayIndexer[1] and $F0 of
                       MCP_WRITE :
                         begin
                           case LccMessage.DataArrayIndexer[1] and $03 of
                             MCP_NONE :
                                 begin
                                   case LccMessage.DataArrayIndexer[6] of
                                     MSI_CDI             :
                                         begin
                                         end;  // Not writeable
                                     MSI_ALL             :
                                         begin
                                         end;  // Not writeable
                                     MSI_CONFIG          :
                                         begin
                                           SendAckReply(LccMessage, False, 0);
                                           ProtocolConfiguration.WriteRequest(LccMessage);
                                           Result := True;
                                         end;
                                     MSI_ACDI_MFG        :
                                         begin
                                         end;  // Not writeable
                                     MSI_ACDI_USER       :
                                         begin
                                           SendAckReply(LccMessage, False, 0);
                                           ProtocolAcdiUser.WriteRequest(LccMessage, ProtocolConfiguration);
                                           Result := True;
                                         end;
                                     MSI_FDI             :
                                         begin
                                         end;  // Not writeable
                                     MSI_FUNCTION_CONFIG :
                                         begin
                                         end;
                                   end
                                 end;
                             MCP_CONFIGURATION :
                                 begin
                                   SendAckReply(LccMessage, False, 0);             // We will be sending a Write Reply
                                   ProtocolConfiguration.WriteRequest(LccMessage);
                                   Result := True;
                                 end;
                             MCP_ALL           :
                                 begin
                                 end; // Not writeable
                             MCP_CDI           :
                                 begin
                                 end; // Not writeable
                           end;
                         end;
                       MCP_WRITE_STREAM :
                           begin
                           end;
                       MCP_READ :
                           begin
                             case LccMessage.DataArrayIndexer[1] and $03 of
                               MCP_NONE :
                                   begin
                                     case LccMessage.DataArrayIndexer[6] of
                                       MSI_CDI             :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             LccMessageOut := TLccMessage.Create;
                                             LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                             ProtocolConfigDefinitionInfo.LoadReply(LccMessage, LccMessageOut);
                                             DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                             // The message can dissappear ANY instant after adding to the Queue!!!!
                                             MsgQueueSending.Add(LccMessageOut);
                                             Result := True;
                                           end;
                                       MSI_ALL             :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We won't be sending a Read Reply
                                           end;
                                       MSI_CONFIG          :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             LccMessageOut := TLccMessage.Create;
                                             LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                             ProtocolConfiguration.LoadReply(LccMessage, LccMessageOut);
                                             DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                             // The message can dissappear ANY instant after adding to the Queue!!!!
                                             MsgQueueSending.Add(LccMessageOut);
                                             Result := True;
                                           end;
                                       MSI_ACDI_MFG        :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             LccMessageOut := TLccMessage.Create;
                                             LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                             ProtocolACDIMfg.LoadReply(LccMessage, LccMessageOut, ProtocolSnip);
                                             DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                             // The message can dissappear ANY instant after adding to the Queue!!!!
                                             MsgQueueSending.Add(LccMessageOut);
                                             Result := True;
                                           end;
                                       MSI_ACDI_USER       :
                                           begin
                                             SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                             LccMessageOut := TLccMessage.Create;
                                             LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                             ProtocolACDIUser.LoadReply(LccMessage, LccMessageOut, ProtocolSnip);
                                             DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                             // The message can dissappear ANY instant after adding to the Queue!!!!
                                             MsgQueueSending.Add(LccMessageOut);
                                             Result := True;
                                           end;
                                       MSI_FDI             :
                                            begin
                                            end;
                                       MSI_FUNCTION_CONFIG :
                                            begin
                                            end;
                                     end
                                   end;
                               MCP_CONFIGURATION : begin
                                                     SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                                     LccMessageOut := TLccMessage.Create;
                                                     LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                                     ProtocolConfiguration.LoadReply(LccMessage, LccMessageOut);
                                                     DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                                     // The message can dissappear ANY instant after adding to the Queue!!!!
                                                     MsgQueueSending.Add(LccMessageOut);
                                                     Result := True;
                                                   end;
                               MCP_ALL           : begin  end;
                               MCP_CDI           : begin
                                                     SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply
                                                     LccMessageOut := TLccMessage.Create;
                                                     LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                                     ProtocolConfigDefinitionInfo.LoadReply(LccMessage, LccMessageOut);
                                                     DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                                     // The message can dissappear ANY instant after adding to the Queue!!!!
                                                     MsgQueueSending.Add(LccMessageOut);
                                                     Result := True;
                                                   end;
                             end;
                           end;
                       MCP_READ_STREAM :
                           begin
                           end;
                       MCP_OPERATION :
                           begin
                             case LccMessage.DataArrayIndexer[1] of
                               MCP_OP_GET_CONFIG :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                     LccMessageOut := TLccMessage.Create;
                                     LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                     ProtocolConfigurationMemOptions.LoadReply(LccMessageOut);
                                     DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                     // The message can dissappear ANY instant after adding to the Queue!!!!
                                     MsgQueueSending.Add(LccMessageOut);
                                     Result := True;
                                   end;
                               MCP_OP_GET_ADD_SPACE_INFO :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                     LccMessageOut := TLccMessage.Create;
                                     LccMessageOut.LoadDatagram(NodeID, LccMessage.Source);
                                     ProtocolConfigMemAddressSpaceInfo.LoadReply(LccMessage, LccMessageOut);
                                     DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                     // The message can dissappear ANY instant after adding to the Queue!!!!
                                     MsgQueueSending.Add(LccMessageOut);
                                     Result := True;
                                   end;
                               MCP_OP_LOCK :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                   end;
                               MCP_OP_GET_UNIQUEID :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                   end;
                               MCP_OP_FREEZE :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                   end;
                               MCP_OP_INDICATE :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                   end;
                               MCP_OP_RESET :
                                   begin
                                     SendAckReply(LccMessage, False, 0);
                                   end;
                             end // case
                           end
                     end; // case
                   end
               else begin
                   // Undknown Datagram Type
                   LccMessageOut := TLccMessage.Create;
                   LccMessageOut.LoadDatagramRejected(NodeID, LccMessage.Source, REJECTED_DATAGRAMS_NOT_ACCEPTED);
                   // The message can dissappear ANY instant after adding to the Queue!!!!
                   MsgQueueSending.Add(LccMessageOut);
                   Result := True;
                 end;
               end;  // case
             end
           end;
      else begin
          if LccMessage.HasDestination then
          begin
            LccMessageOut := TLccMessage.Create;
            LccMessageOut.LoadOptionalInteractionRejected(NodeID, LccMessage.Source, REJECTED_BUFFER_FULL, LccMessage.MTI);
            // The message can dissappear ANY instant after adding to the Queue!!!!
            MsgQueueSending.Add(LccMessageOut);
            Result := True;
          end;
        end;
      end; // case
    end;
  end;
  FreeDynamicArrayObjects(LccMessages);
end;

function TLccNode.ProcessNodeDefinitionXml(XML: TMustangpeakXmlDocument): Boolean;
var
  Node, RootNode: TMustangpeakXmlNode;
  s, EventState: string;
  Supported: Boolean;
  MemSpaceIsPresent, MemSpaceReadOnly, MemAssumedZeroAddressLo: Boolean;
  MemAddressLo, MemAddressHi: DWord;
begin
  Result := False;

  // Look for the Root Node
  RootNode := XmlFindRootNode(XML, 'node');
  if Assigned(RootNode) then
  begin

    // Find our NodeID
    Node := XmlFindChildNode(RootNode, 'nodeid');
    if Assigned(Node) then
    begin

      // Load our Node ID
      FNodeID.ID := StrToNodeID( XmlNodeTextContent(Node));

      // Load our Protocols that are supported
      Node := XmlFindChildNode(RootNode, 'protocols');
      if Assigned(Node) then
      begin
        Node := XmlFirstChild(Node);
        while Assigned(Node) do
        begin
          Supported := Lowercase(XmlNodeName(Node)) = 'supported';
          s := XmlNodeTextContent(Node);
          if Lowercase(s) = 'events' then ProtocolPip.EventExchange := Supported;
          if Lowercase(s) = 'datagram' then ProtocolPip.Datagram := Supported;
          if Lowercase(s) = 'datagram.cdi' then ProtocolPip.CDI := Supported;
          if Lowercase(s) = 'datagram.memoryconfiguration' then ProtocolPip.MemConfig := Supported;
          if Lowercase(s) = 'snip' then ProtocolPip.SimpleNodeInfo := Supported;
          if Lowercase(s) = 'acdi' then ProtocolPip.ACDI := Supported;
          if Lowercase(s) = 'traction' then ProtocolPip.TractionControl := Supported;
          if Lowercase(s) = 'traction.fdi' then ProtocolPip.FDI := Supported;
          if Lowercase(s) = 'traction.stni' then ProtocolPip.SimpleTrainNodeInfo := Supported;
          if Lowercase(s) = 'traction.fci' then ProtocolPip.FunctionConfiguration := Supported;
          Node := XmlNextSiblingNode(Node);
        end;
      end;

      // Load our Configuration Options
      Node := XmlFindChildNode(RootNode, 'configuration.options');
      if Assigned(Node) then
      begin
        Node := XmlFirstChild(Node);
        while Assigned(Node) do
        begin
          Supported := Lowercase(XmlNodeName(Node)) = 'supported';
          s := XmlNodeTextContent(Node);
          if lowercase(s) = 'write.under.mask' then ProtocolConfigurationMemOptions.WriteUnderMask := Supported;
          if lowercase(s) = 'unaligned.reads' then ProtocolConfigurationMemOptions.UnAlignedReads := Supported;
          if lowercase(s) = 'unaligned.writes' then ProtocolConfigurationMemOptions.UnAlignedWrites := Supported;
          if lowercase(s) = 'mfg.acdi.read' then ProtocolConfigurationMemOptions.SupportACDIMfgRead := Supported;
          if lowercase(s) = 'usr.acdi.read' then ProtocolConfigurationMemOptions.SupportACDIUserRead := Supported;
          if lowercase(s) = 'usr.acdi.write' then ProtocolConfigurationMemOptions.SupportACDIUserWrite := Supported;
          Node := XmlNextSiblingNode(Node);
        end;
      end;

      Node := XmlFindChildNode(RootNode, 'configuration.options');
      if Assigned(Node) then
      begin
        Node := XmlFindChildNode(Node, 'writelengths');
        if Assigned(Node) then
        begin
          Node := XmlFirstChild(Node);
          while Assigned(Node) do
          begin
            Supported := Lowercase(XmlNodeName(Node)) = 'supported';
            s := XmlNodeTextContent(Node);
            if lowercase(s) = 'one' then ProtocolConfigurationMemOptions.WriteLenOneByte := Supported;
            if lowercase(s) = 'two' then ProtocolConfigurationMemOptions.WriteLenTwoBytes := Supported;
            if lowercase(s) = 'four' then ProtocolConfigurationMemOptions.WriteLenFourBytes := Supported;
            if lowercase(s) = 'sixtyfour' then ProtocolConfigurationMemOptions.WriteLenSixyFourBytes := Supported;
            if lowercase(s) = 'arbitrary' then ProtocolConfigurationMemOptions.WriteArbitraryBytes := Supported;
            if lowercase(s) = 'stream' then ProtocolConfigurationMemOptions.WriteStream := Supported;
            Node := XmlNextSiblingNode(Node);
          end
        end;
      end;

      Node := XmlFindChildNode(RootNode, 'configuration.options');
      if Assigned(Node) then
      begin
        Node := XmlFindChildNode(Node, 'memspace.hi');
        if Assigned(Node) then
          ProtocolConfigurationMemOptions.HighSpace := StrToInt( XmlNodeTextContent(Node));
       end;

      Node := XmlFindChildNode(RootNode, 'configuration.options');
      if Assigned(Node) then
      begin
        Node := XmlFindChildNode(Node, 'memspace.lo');
        if Assigned(Node) then
          ProtocolConfigurationMemOptions.LowSpace := StrToInt( XmlNodeTextContent(Node));
      end;

      // Load our Memory Space Info
      Node := XmlFindChildNode(RootNode, 'configuration.memspace');
      if Assigned(Node) then
      begin
        Node := XmlFirstChild(Node);
        while Assigned(Node) do
        begin
          if XmlNodeName(Node) = 'memspace' then
          begin
            MemAssumedZeroAddressLo := False;
            MemSpaceIsPresent := Lowercase( XmlAttributeRead(Node, 'present')) = 'true';
            MemSpaceReadOnly := Lowercase( XmlAttributeRead(Node, 'readonly')) = 'true';
            MemAddressHi := StrToInt64(XmlAttributeRead(Node, 'addresshi'));
            MemAddressLo := 0;
            if XmlAttributeExists(Node, 'addresslo') then
              MemAddressLo := StrToInt64(XmlAttributeRead(Node, 'addresslo'))
            else
              MemAssumedZeroAddressLo := True;
            s := XmlNodeTextContent(Node);
            if MemAssumedZeroAddressLo or (MemAddressLo = 0) then
              ProtocolConfigMemAddressSpaceInfo.Add(StrToInt(s), MemSpaceIsPresent, MemSpaceReadOnly, True, MemAddressLo, MemAddressHi)
            else
              ProtocolConfigMemAddressSpaceInfo.Add(StrToInt(s), MemSpaceIsPresent, MemSpaceReadOnly, False, MemAddressLo, MemAddressHi)
          end;
          Node := XmlNextSiblingNode(Node);
        end;
        if Assigned(Node) then
          ProtocolConfigurationMemOptions.LowSpace := StrToInt( XmlNodeTextContent(Node));
      end;

      // Load our Events Produced
      Node := XmlFindChildNode(RootNode, 'events');
      if Assigned(Node) then
      begin
        Node := XmlFindChildNode(Node, 'produced');
        if Assigned(Node) then
        begin
          Node := XmlFirstChild(Node);
          while Assigned(Node) do
          begin
            if Lowercase(XmlNodeName(Node)) = 'event' then
            begin
              s := XmlNodeTextContent(Node);
              s := IntToHex(StrToInt(s), 4);
              s := NodeIDStr + s;
              EventState := XmlAttributeRead(Node, 'default');
              if (Lowercase(EventState) = 'unknown') or (EventState = '') then
                ProtocolEventsProduced.Add(StrToEventID(s), evs_Unknown)
              else
              if Lowercase(EventState) = 'valid' then
                ProtocolEventsProduced.Add(StrToEventID(s), evs_Valid)
              else
              if Lowercase(EventState) = 'invalid' then
                ProtocolEventsProduced.Add(StrToEventID(s), evs_InValid);
            end;
            Node := XmlNextSiblingNode(Node);
          end;
        end;
      end;

      // Load Datagram buffer options
      Node := XmlFindChildNode(RootNode, 'datagram');
      if Assigned(Node) then
      begin
        Node := XmlFindChildNode(Node, 'buffersize');
        if Assigned(Node) then
          Max_Allowed_Datagrams := StrToInt(XmlNodeTextContent(Node));
      end;

      // Load our Events Consumed
      Node := XmlFindChildNode(RootNode, 'events');
      if Assigned(Node) then
      begin
        Node := XmlFindChildNode(Node, 'consumed');
        if Assigned(Node) then
        begin
          Node := XmlFirstChild(Node);
          while Assigned(Node) do
          begin
            if Lowercase(XmlNodeName(Node)) = 'event' then
            begin
              s := XmlNodeTextContent(Node);
              s := IntToHex(StrToInt(s), 4);
              s := NodeIDStr + s;
              EventState := XmlAttributeRead(Node, 'default');
              if (Lowercase(EventState) = 'unknown') or (EventState = '') then
                ProtocolEventsConsumed.Add(StrToEventID(s), evs_Unknown)
              else
              if Lowercase(EventState) = 'valid' then
                ProtocolEventsConsumed.Add(StrToEventID(s), evs_Valid)
              else
              if Lowercase(EventState) = 'invalid' then
                ProtocolEventsConsumed.Add(StrToEventID(s), evs_InValid);
            end;
            Node := XmlNextSiblingNode(Node);
          end;
        end;
      end;

      // Load our CDI File
      CdiFileName := '';
      Node := XmlFindChildNode(RootNode, 'cdi');
      if Assigned(Node) then
      begin
        if Assigned(Node) then
          CdiFileName := XmlNodeTextContent(Node);
      end;

      // If we make it we are likely ok, should have a bit more checking....  1/18/17
      Result := True;
    end;
  end;
end;

procedure TLccNode.SendAckReply(LccMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
var
  LccMessageOut: TLccMessage;
begin
  LccMessageOut := TLccMessage.Create;
  LccMessageOut.LoadDatagramAck(NodeID, LccMessage.Source, True, ReplyPending, TimeOutValueN);
  // The message can dissappear ANY instant after adding to the Queue!!!!
  MsgQueueSending.Add(LccMessageOut);     // We will be sending a Write Reply
end;

procedure TLccNode.SendAllEvents;
begin
  SendAllConsumedEvents;
  SendAllProducedEvents;
end;

procedure TLccNode.SendAllConsumedEvents;
var
  LccEvent: TLccEvent;
  LccMessageOut: TLccMessage;
  LocalEventID: TEventID;
  i: Integer;
begin
  for i := 0 to ProtocolEventsConsumed.Count - 1 do
  begin
    LccEvent := ProtocolEventsConsumed.Event[i];
    LocalEventID := LccEvent.ID;
    LccMessageOut := TLccMessage.Create;
    LccMessageOut.LoadConsumerIdentified(NodeID, LocalEventID, LccEvent.State);
    // The message can dissappear ANY instant after adding to the Queue!!!!
    MsgQueueSending.Add(LccMessageOut);
  end;
end;

procedure TLccNode.SendAllProducedEvents;
var
  LccEvent: TLccEvent;
  LccMessageOut: TLccMessage;
  LocalEventID: TEventID;
  i: Integer;
begin
  for i := 0 to ProtocolEventsProduced.Count - 1 do
  begin
    LccEvent := ProtocolEventsProduced.Event[i];
    LocalEventID := LccEvent.ID;
    LccMessageOut := TLccMessage.Create;
    LccMessageOut.LoadProducerIdentified(NodeID, LocalEventID, LccEvent.State);
    // The message can dissappear ANY instant after adding to the Queue!!!!
    MsgQueueSending.Add(LccMessageOut);
  end;
end;

function TLccNode.Start: Boolean;
var
  LccMessage: TLccMessage;
begin
  // Create Thread Here
  Result := False;

  LccMessage := TLccMessage.Create;
  LccMessage.LoadInitializationComplete(NodeID);
  // The message can dissappear ANY instant after adding to the Queue!!!!
  MsgQueueSending.Add(LccMessage);
end;

constructor TLccNode.Create(NodeDefinitionXmlFile: string);
var
  XML: TMustangpeakXmlDocument;
begin
   if FileExists(NodeDefinitionXmlFile) then
   begin
     XML := XmlLoadFromFile(NodeDefinitionXmlFile);
     if Assigned(XML) then
     begin
       FAliasIDEngine := TLccAliasIDEngine.Create;
       AliasIDEngine.LccNode := Self;
       FDatagramQueue := TDatagramQueue.Create(Self);
       FMsgQueueReceived := TThreadedCirularArrayObject.Create;
       FMsgQueueSending := TThreadedCirularArrayObject.Create;
       MsgQueueReceived.OnAdd := {$IFDEF FPC}@{$ENDIF}MsgQueueReceiveAdded;
       MsgQueueSending.OnAdd := {$IFDEF FPC}@{$ENDIF}MsgQueueSendAdded;
       FProtocolSnip := TProtocolSnip.Create;
       FProtocolEventsConsumed := TProtocolEvents.Create;
       FProtocolEventsProduced := TProtocolEvents.Create;
       FProtocolPip := TProtocolPip.Create;
       FProtocolAcdiMfg := TProtocolAcdiMfg.Create(MSI_ACDI_MFG, False);
       FProtocolAcdiUser := TProtocolAcdiUser.Create(MSI_ACDI_USER, False);
       FProtocolConfiguration := TProtocolConfiguration.Create(MSI_CONFIG, False);
       FProtocolConfigurationMemory := TProtocolConfigurationMemory.Create;
       FProtocolConfigurationMemOptions := TProtocolConfigurationMemOptions.Create;
       FProtocolConfigMemAddressSpaceInfo := TProtocolConfigMemAddressSpaceInfo.Create;
       FProtocolConfigDefinitionInfo := TProtocolConfigDefinitionInfo.Create(MSI_CDI, True);

       FCDI := {$IFDEF FPC}TMustangpeakXmlDocument{$ELSE}TXMLDocument{$ENDIF}.Create{$IFNDEF FPC}(nil){$ENDIF};
       if ProcessNodeDefinitionXml(XML) then
       begin
          AliasIDEngine.LoginID := NodeID;
          if CdiFileName <> '' then
          begin
            CdiFileName := IncludeTrailingPathDelimiter( ExtractFileDir(NodeDefinitionXmlFile)) + CdiFileName;
            if FileExists(CdiFileName) then
            begin
              CDI := XmlLoadFromFile(CdiFileName);
              ProtocolConfigDefinitionInfo.LoadFromXml(CdiFileName, nil);
              ProtocolSnip.LoadFromCdiXmlDoc(CDI);
            end;
   {$IFDEF FPC}
          end;
       end else
         Fail;
     end else
       Fail;
   end else
     Fail;
   {$ELSE}
          end
       end
     end
   end;
   {$ENDIF}
end;

destructor TLccNode.Destroy;
begin
  FreeAndNil(FProtocolSnip);
  FreeAndNil(FProtocolEventsConsumed);
  FreeAndNil(FProtocolEventsProduced);
  FreeAndNil(FProtocolPip);
  FreeAndNil(FProtocolAcdiUser);
  FreeAndNil(FProtocolAcdiMfg);
  {$IFDEF FPC}
  FreeAndNil(FCDI);
  {$ELSE}
  FCDI := nil;
  {$ENDIF}
  FreeAndNil(FMsgQueueReceived);
  FreeAndNil(FMsgQueueSending);
  FreeAndNil(FDatagramQueue);
  FreeAndNil(FProtocolConfiguration);
  FreeAndNil(FProtocolConfigurationMemory);
  FreeAndNil(FProtocolConfigurationMemOptions);
  FreeAndNil(FProtocolConfigMemAddressSpaceInfo);
  FreeAndNil(FProtocolConfigDefinitionInfo);
  FreeAndNil(FAliasIDEngine);
  inherited Destroy;
end;

{ TDatagramQueue }

procedure TDatagramQueue.Remove(LccMessage: TLccMessage);
var
  iLocalMessage: Integer;
begin
  iLocalMessage := FindBySourceNode(LccMessage);
  if iLocalMessage > -1 then
    Queue.Delete(iLocalMessage);
end;

procedure TDatagramQueue.Add(LccMessage: TLccMessage);
begin
  Queue.Add(LccMessage);
  LccMessage.RetryAttempts := 0;
  LccMessage.AbandonTimeout := 0;
end;

constructor TDatagramQueue.Create(ANode: TLccNode);
begin
  Queue := TObjectList{$IFNDEF FPC}<TLccMessage>{$ENDIF}.Create;
  Queue.OwnsObjects := True;
  FOwnerNode := ANode;
  FTimer := TFPTimer.Create(nil);
  Timer.Interval := 1000;
  Timer.OnTimer := {$IFDEF FPC}@{$ENDIF}OnTimer;
  {$IFDEF FPC}
  Timer.StartTimer;
  {$ELSE}
  Timer.Enabled := True
  {$ENDIF}
end;

destructor TDatagramQueue.Destroy;
begin
  FreeAndNil(FTimer);
  {$IFDEF FPC}
  FreeAndNil(FQueue);
  {$ELSE}
  Queue.DisposeOf;
  {$ENDIF}

  inherited Destroy;
end;

function TDatagramQueue.FindBySourceNode(LccMessage: TLccMessage): Integer;
var
  i: Integer;
  QueueAlias: Word;
  QueueNodeID: TNodeID;
begin
  Result := -1;
  i := 0;
  while i < Queue.Count do
  begin
    QueueAlias := (Queue[i] as TLccMessage).Destination.Alias;
    QueueNodeID := (Queue[i] as TLccMessage).Destination.ID;
    if (QueueAlias <> 0) and (LccMessage.Source.Alias <> 0) then
    begin
      if QueueAlias = LccMessage.Source.Alias then
      begin
        Result := i;
        Break
      end;
    end else
    if not NullNodeID(QueueNodeID) and not NullNodeID(LccMessage.Source.ID) then
    begin
      if EqualNodeID(QueueNodeID, LccMessage.Source.ID, False) then
      begin
        Result := i;
        Break
      end;
    end;
    Inc(i)
  end;
end;

procedure TDatagramQueue.OnTimer(Sender: TObject);
var
  LocalMessage: TLccMessage;
  i: Integer;
begin
  if Assigned(Queue) then
  begin
    for i := Queue.Count - 1 downto 0 do
    begin
      LocalMessage := Queue[i] as TLccMessage;
      if LocalMessage.AbandonTimeout < 6 then
        LocalMessage.AbandonTimeout := LocalMessage.AbandonTimeout + 1
      else
        Queue.Delete(i);
    end;
  end;
end;

procedure TDatagramQueue.Clear;
begin
  Queue.Clear;
end;

function TDatagramQueue.Resend(LccMessage: TLccMessage): TLccMessage;
var
  iLocalMessage: Integer;
  LocalMessage: TLccMessage;
begin
  Result := nil;
  iLocalMessage := FindBySourceNode(LccMessage);
  if iLocalMessage > -1 then
  begin
    LocalMessage := Queue[iLocalMessage] as TLccMessage;
    if LocalMessage.RetryAttempts < 5 then
    begin
      LocalMessage := Queue[iLocalMessage] as TLccMessage;
      Result := LocalMessage.Clone;
      LocalMessage.RetryAttempts := LocalMessage.RetryAttempts + 1;
    end else
      Queue.Delete(iLocalMessage);
  end;
end;

initialization
  GlobalNodeList := TThreadedCirularArrayObject.Create;
  GlobalNodeList.OwnsObjects := True;

finalization
  FreeAndNil(GlobalNodeList)

end.

