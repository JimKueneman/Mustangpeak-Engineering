unit lcc.node;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I lcc_compilers.inc}

interface

uses
  Classes, SysUtils, lcc.types, contnrs,
  lcc.message, protocol.snip, protocol.events, protocol.pip, protocol.acdi.mfg,
  protocol.acdi.user, lcc.utilities, mustangpeak.xmlutilities,
  mustangpeak.threadedcirculararray, protocol.datagram.configuration,
  protocol.datagram.configuration.memory, protocol.datagram.confguration.memory.options,
  protocol.datagram.configuration.memory.info, protocol.datagram.configuration.definition.information;

type
  TDatagramQueue = class;

  { TLccNodeTLccNode }

  TLccNode = class(TPersistent)
  private
    FCDI: TMustangpeakXmlDocument;
    FDatagramQueue: TDatagramQueue;
    FMsgQueueReceived: TThreadedCirularArrayObject;
    FMsgQueueSending: TThreadedCirularArrayObject;
    FNodeID: TNodeID;
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
    property CDI: TMustangpeakXmlDocument read FCDI write FCDI;
    property DatagramQueue: TDatagramQueue read FDatagramQueue write FDatagramQueue;
  public
    constructor Create(NodeDefinitionXmlFile: string);
    constructor Create(NodeDefinitionXmlDoc: TMustangpeakXmlDocument);
    destructor Destroy; override;
    function Start: Boolean; virtual;
    function ProcessMessages: Boolean;

    property MsgQueueReceived: TThreadedCirularArrayObject read FMsgQueueReceived write FMsgQueueReceived;
    property MsgQueueSending: TThreadedCirularArrayObject read FMsgQueueSending write FMsgQueueSending;
    property NodeID: TNodeID read FNodeID;
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
    FQueue: TObjectList;
  protected
    property OwnerNode: TLccNode read FOwnerNode write FOwnerNode;
    property Queue: TObjectList read FQueue write FQueue;
    function FindBySourceNode(LccMessage: TLccMessage): Integer;
  public
    constructor Create(ANode: TLccNode);
    destructor Destroy; override;
    procedure Add(LccMessage: TLccMessage);
    procedure Clear;
    function Resend(LccMessage: TLccMessage): TLccMessage;
    procedure Remove(LccMessage: TLccMessage);
    procedure TickTimeout;
  end;

var
  GlobalNodeList: TThreadedCirularArrayObject;

implementation

{ TLccNode }

function TLccNode.GetNodeIDStr: String;
begin
   Result := NodeIDToString(FNodeID, False)
end;

procedure TLccNode.MsgQueueReceiveAdded(Sender: TObject);
begin

end;

procedure TLccNode.MsgQueueSendAdded(Sender: TObject);
begin
  GlobalSendEvent.SetEvent;
end;

function TLccNode.ProcessMessages: Boolean;
var
  LccMessages: TDynamicArrayObject;
  LccMessage, LccMessageOut: TLccMessage;
  i, j: Integer;
  LocalNodeID: TNodeID;
  LocalEventID: TEventID;
  EventObj: TLccEvent;
begin
  LccMessage := nil;
  LccMessages := nil;
  // Extract the messages in one block to work on
  MsgQueueReceived.LockArray;
  try
    MsgQueueReceived.RemoveChunk(LccMessages);
  finally
    MsgQueueReceived.UnLockArray;
  end;

  for i := 0 to Length(LccMessages) - 1 do
  begin
    LccMessage := LccMessages[i] as TLccMessage;


    case LccMessage.MTI of
      MTI_OPTIONAL_INTERACTION_REJECTED :
          begin
          end;
      MTI_VERIFY_NODE_ID_NUMBER      :
          begin
            if LccMessage.DataCount = 6 then
            begin
              LccMessage.ExtractDataBytesAsNodeID(0, LocalNodeID);
              if EqualNodeID(LocalNodeID, NodeID, False) then
              begin
                LccMessageOut := TLccMessage.Create;
                LccMessageOut.LoadVerifiedNodeID(NodeID);
                MsgQueueSending.Add(LccMessageOut);
              end
            end else
            begin
              LccMessageOut := TLccMessage.Create;
              LccMessageOut.LoadVerifiedNodeID(NodeID);
              MsgQueueSending.Add(LccMessageOut);
            end;
            Result := True;
          end;
      MTI_VERIFY_NODE_ID_NUMBER_DEST :
          begin
            LccMessageOut := TLccMessage.Create;
            LccMessageOut.LoadVerifiedNodeID(NodeID);
            MsgQueueSending.Add(LccMessageOut);
            Result := True;
          end;
      MTI_VERIFIED_NODE_ID_NUMBER :
          begin

          end;
      MTI_SIMPLE_NODE_INFO_REQUEST :
          begin
            LccMessageOut := TLccMessage.Create;
            LccMessageOut.LoadSimpleNodeIdentInfoReply(NodeID, LccMessage.SourceID, ProtocolSnip.PackedFormat);
            MsgQueueSending.Add(LccMessageOut);
            Result := True;
          end;
      MTI_SIMPLE_NODE_INFO_REPLY :
          begin               // Called if I send a SNIP;
            LccMessageOut := TLccMessage.Create;
            ProtocolPip.ProcessMessage(LccMessage);
            MsgQueueSending.Add(LccMessageOut);
            Result := True;
          end;
      MTI_PROTOCOL_SUPPORT_INQUIRY :
          begin
            LccMessageOut := TLccMessage.Create;
            LccMessageOut.LoadProtocolIdentifyReply(NodeID, LccMessage.SourceID, ProtocolPip.EncodeFlags);
            MsgQueueSending.Add(LccMessageOut);
            Result := True;
          end;
      MTI_PROTOCOL_SUPPORT_REPLY :
          begin   // Called if I send a Protocol Support
            LccMessageOut := TLccMessage.Create;
            ProtocolPip.ProcessMessage(LccMessage);
            MsgQueueSending.Add(LccMessageOut);
            Result := True;
          end;
      MTI_EVENTS_IDENTIFY :
          begin
            for j := 0 to ProtocolEventsConsumed.Count - 1 do
            begin
              LccMessageOut := TLccMessage.Create;
              LocalEventID := ProtocolEventsConsumed.Event[j].ID;
              LccMessageOut.LoadConsumerIdentified(NodeID, LocalEventID, ProtocolEventsConsumed.Event[j].State);
              MsgQueueSending.Add(LccMessageOut);
            end;
            for j := 0 to ProtocolEventsProduced.Count - 1 do
            begin
              LccMessageOut := TLccMessage.Create;
              LocalEventID := ProtocolEventsProduced.Event[j].ID;
              LccMessageOut.LoadProducerIdentified(NodeID, LocalEventID, ProtocolEventsProduced.Event[j].State);
              MsgQueueSending.Add(LccMessageOut);
            end;
            Result := True;
          end;
      MTI_EVENTS_IDENTIFY_DEST :
          begin
            LccMessageOut := TLccMessage.Create;
            if EqualNodeID(NodeID,  LccMessage.DestID, False) then
            begin
              for j := 0 to ProtocolEventsConsumed.Count - 1 do
              begin
                LccMessageOut := TLccMessage.Create;
                LocalEventID := ProtocolEventsConsumed.Event[j].ID;
                LccMessageOut.LoadConsumerIdentified(NodeID, LocalEventID, ProtocolEventsConsumed.Event[j].State);
                MsgQueueSending.Add(LccMessageOut);
              end;
              for j := 0 to ProtocolEventsProduced.Count - 1 do
              begin
                LccMessageOut := TLccMessage.Create;
                LocalEventID := ProtocolEventsProduced.Event[j].ID;
                LccMessageOut.LoadProducerIdentified(NodeID, LocalEventID, ProtocolEventsProduced.Event[j].State);
                MsgQueueSending.Add(LccMessageOut);
              end;
            end;
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
                                         LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                         ProtocolConfigDefinitionInfo.LoadReply(LccMessage, LccMessageOut);
                                         MsgQueueSending.Add(LccMessageOut);
                                         DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
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
                                         LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                         ProtocolConfiguration.LoadReply(LccMessage, LccMessageOut);
                                         MsgQueueSending.Add(LccMessageOut);
                                         DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                         Result := True;
                                       end;
                                   MSI_ACDI_MFG        :
                                       begin
                                         SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply

                                         LccMessageOut := TLccMessage.Create;
                                         LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                         ProtocolACDIMfg.LoadReply(LccMessage, LccMessageOut, ProtocolSnip);
                                         MsgQueueSending.Add(LccMessageOut);
                                         DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                         Result := True;
                                       end;
                                   MSI_ACDI_USER       :
                                       begin
                                         SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply

                                         LccMessageOut := TLccMessage.Create;
                                         LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                         ProtocolACDIUser.LoadReply(LccMessage, LccMessageOut, ProtocolSnip);
                                         MsgQueueSending.Add(LccMessageOut);
                                         DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
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
                                                 LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                                 ProtocolConfiguration.LoadReply(LccMessage, LccMessageOut);
                                                 MsgQueueSending.Add(LccMessageOut);
                                                 DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                                 Result := True;
                                               end;
                           MCP_ALL           : begin  end;
                           MCP_CDI           : begin
                                                 SendAckReply(LccMessage, False, 0);   // We will be sending a Read Reply

                                                 LccMessageOut := TLccMessage.Create;
                                                 LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                                 ProtocolConfigDefinitionInfo.LoadReply(LccMessage, LccMessageOut);
                                                 MsgQueueSending.Add(LccMessageOut);
                                                 DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
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
                                 LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                 ProtocolConfigurationMemOptions.LoadReply(LccMessageOut);
                                 MsgQueueSending.Add(LccMessageOut);
                                 DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                 Result := True;
                               end;
                           MCP_OP_GET_ADD_SPACE_INFO :
                               begin
                                 SendAckReply(LccMessage, False, 0);

                                 LccMessageOut := TLccMessage.Create;
                                 LccMessageOut.LoadDatagram(NodeID, LccMessage.SourceID);
                                 ProtocolConfigMemAddressSpaceInfo.LoadReply(LccMessage, LccMessageOut);
                                 MsgQueueSending.Add(LccMessageOut);
                                 DatagramQueue.Add(LccMessageOut.Clone);      // Waiting for an ACK
                                 Result := True;
                               end;
                           MCP_OP_LOCK :
                               begin
                               end;
                           MCP_OP_GET_UNIQUEID :
                               begin
                               end;
                           MCP_OP_FREEZE :
                               begin
                               end;
                           MCP_OP_INDICATE :
                               begin
                               end;
                           MCP_OP_RESETS :
                               begin
                               end;
                         end // case
                       end
                 end; // case
               end
           else begin
               // Undknown Datagram Type
               LccMessageOut := TLccMessage.Create;
               LccMessageOut.LoadDatagramRejected(NodeID, LccMessage.SourceID, REJECTED_DATAGRAMS_NOT_ACCEPTED);
               MsgQueueSending.Add(LccMessageOut);
               Result := True;
             end;
           end;  // case
         end;
    else begin
        if LccMessage.HasDestination then
        begin
          LccMessageOut := TLccMessage.Create;
          LccMessageOut.LoadOptionalInteractionRejected(NodeID, LccMessage.SourceID, REJECTED_BUFFER_FULL, LccMessage.MTI);
          MsgQueueSending.Add(LccMessageOut);
          Result := True;
        end;
      end;
    end; // case
  end;

  FreeDynamicArrayObjects(LccMessages);
end;

function TLccNode.ProcessNodeDefinitionXml(XML: TMustangpeakXmlDocument): Boolean;
var
  Node, RootNode: TMustangpeakXmlNode;
  s, CdiFile, EventState: string;
  Supported: Boolean;
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
      FNodeID := StrToNodeID( XmlNodeTextContent(Node));

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
              s := NodeIDStr + XmlNodeTextContent(Node);
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
              s := NodeIDStr + XmlNodeTextContent(Node);
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
      Node := XmlFindChildNode(RootNode, 'cdi');
      if Assigned(Node) then
      begin
        {$IFDEF LCC_WINDOWS}
        Node := XmlFindChildNode(Node, 'path_win');
        {$ELSE}
        Node := XmlFindChildNode(Node, 'path');
        {$ENDIF}
        if Assigned(Node) then
        begin
          CdiFile := XmlNodeTextContent(Node);
          if FileExists(CdiFile) then
          begin
            CDI :=  XmlLoadFromFile(CdiFile);
            Node := XmlFindChildNode(RootNode, 'snip');
            if Assigned(Node) then
              if Lowercase(XmlNodeTextContent(Node)) = 'true' then
                ProtocolSnip.LoadFromCdiXmlDoc(CDI);
          end;
        end;
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
  LccMessageOut.LoadDatagramAck(LccMessage.DestID, LccMessage.SourceID, True, ReplyPending, TimeOutValueN);
  MsgQueueSending.Add(LccMessageOut);     // We will be sending a Write Reply
end;

function TLccNode.Start: Boolean;
var
  LccMessage: TLccMessage;
begin
  // Create Thread Here
  Result := False;

  LccMessage := TLccMessage.Create;
  LccMessage.LoadInitializationComplete(NodeID);
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
       FDatagramQueue := TDatagramQueue.Create(Self);
       FMsgQueueReceived := TThreadedCirularArrayObject.Create;
       FMsgQueueSending := TThreadedCirularArrayObject.Create;
       MsgQueueReceived.OnAdd := @MsgQueueReceiveAdded;
       MsgQueueSending.OnAdd := @MsgQueueSendAdded;
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

       FCDI := TMustangpeakXmlDocument.Create;
       if ProcessNodeDefinitionXml(XML) then
       begin

       end else
         Fail;
     end else
       Fail;
   end else
     Fail;
end;

constructor TLccNode.Create(NodeDefinitionXmlDoc: TMustangpeakXmlDocument);
begin
  if not ProcessNodeDefinitionXml(NodeDefinitionXmlDoc) then
    Fail;
end;

destructor TLccNode.Destroy;
begin
  FreeAndNil(FProtocolSnip);
  FreeAndNil(FProtocolEventsConsumed);
  FreeAndNil(FProtocolEventsProduced);
  FreeAndNil(FProtocolPip);
  FreeAndNil(FProtocolAcdiUser);
  FreeAndNil(FProtocolAcdiMfg);
  FreeAndNil(FCDI);
  FreeAndNil(FMsgQueueReceived);
  FreeAndNil(FMsgQueueSending);
  FreeAndNil(FDatagramQueue);
  FreeAndNil(FProtocolConfiguration);
  FreeAndNil(FProtocolConfigurationMemory);
  FreeAndNil(FProtocolConfigurationMemOptions);
  FreeAndNil(FProtocolConfigMemAddressSpaceInfo);
  FreeAndNil(FProtocolConfigDefinitionInfo);
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
  Queue := TObjectList.Create;
  Queue.OwnsObjects := True;
  FOwnerNode := ANode;
end;

destructor TDatagramQueue.Destroy;
begin
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
    QueueAlias := (Queue[i] as TLccMessage).CAN.DestAlias;
    QueueNodeID := (Queue[i] as TLccMessage).DestID;
    if (QueueAlias <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
    begin
      if QueueAlias = LccMessage.CAN.SourceAlias then
      begin
        Result := i;
        Break
      end;
    end else
    if not NullNodeID(QueueNodeID) and not NullNodeID(LccMessage.SourceID) then
    begin
      if EqualNodeID(QueueNodeID, LccMessage.SourceID, False) then
      begin
        Result := i;
        Break
      end;
    end;
    Inc(i)
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

procedure TDatagramQueue.TickTimeout;
var
  LocalMessage: TLccMessage;
  i: Integer;
begin
  for i := Queue.Count - 1 downto 0 do
  begin
    LocalMessage := Queue[i] as TLccMessage;
    if LocalMessage.AbandonTimeout < 2 then
      LocalMessage.AbandonTimeout := LocalMessage.AbandonTimeout + 1
    else
      Queue.Delete(i);
  end;
end;

initialization
  GlobalNodeList := TThreadedCirularArrayObject.Create;
  GlobalNodeList.OwnsObjects := True;

finalization
  FreeAndNil(GlobalNodeList)

end.

