unit lcc.node;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I ..\Lcc\lcc_compilers.inc}

interface

uses
  Classes, SysUtils, lcc.types, lcc.message, protocol.snip, protocol.events,
  protocol.pip, protocol.acdi.mfg, protocol.acdi.user, lcc.utilities,
  mustangpeak.xmlutilities, mustangpeak.threadedcirculararray;

type

  { TLccNodeTLccNode }

  TLccNode = class(TPersistent)
  private
    FCDI: TMustangpeakXmlDocument;
    FMsgQueueReceived: TThreadedCirularArrayInterface;
    FMsgQueueSending: TThreadedCirularArrayInterface;
    FNodeID: TNodeID;
    FProtocolAcdiMfg: TProtocolAcdiMfg;
    FProtocolAcdiUser: TProtocolAcdiUser;
    FProtocolEventsConsumed: TProtocolEvents;
    FProtocolEventsProduced: TProtocolEvents;
    FProtocolPip: TProtocolPip;
    FProtocolSnip: TProtocolSnip;
    function GetNodeIDStr: String;
  protected
    function ProcessNodeDefinitionXml(XML: TMustangpeakXmlDocument): Boolean;
    procedure MsgQueueSendAdded(Sender: TObject);
    procedure MsgQueueReceiveAdded(Sender: TObject);
    property CDI: TMustangpeakXmlDocument read FCDI write FCDI;
  public
    constructor Create(NodeDefinitionXmlFile: string);
    constructor Create(NodeDefinitionXmlDoc: TMustangpeakXmlDocument);
    destructor Destroy; override;
    function Start: Boolean; virtual;

    property MsgQueueReceived: TThreadedCirularArrayInterface read FMsgQueueReceived write FMsgQueueReceived;
    property MsgQueueSending: TThreadedCirularArrayInterface read FMsgQueueSending write FMsgQueueSending;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr: String read GetNodeIDStr;
    property ProtocolAcdiMfg: TProtocolAcdiMfg read FProtocolAcdiMfg;
    property ProtocolAcdiUser: TProtocolAcdiUser read FProtocolAcdiUser;
    property ProtocolEventsConsumed: TProtocolEvents read FProtocolEventsConsumed;
    property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced;
    property ProtocolPip: TProtocolPip read FProtocolPip;
    property ProtocolSnip: TProtocolSnip read FProtocolSnip;
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

function TLccNode.Start: Boolean;
var
  MessageInf: ILccMessage;
begin
  // Create Thread Here
  Result := False;
  MessageInf := TLccMessage.Create as ILccMessage;
  MessageInf.LoadInitializationComplete(NodeID);
  MsgQueueSending.Add(MessageInf);
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
       FMsgQueueReceived := TThreadedCirularArrayInterface.Create;
       FMsgQueueSending := TThreadedCirularArrayInterface.Create;
       MsgQueueReceived.OnAdd := @MsgQueueReceiveAdded;
       MsgQueueSending.OnAdd := @MsgQueueSendAdded;
       FProtocolSnip := TProtocolSnip.Create;
       FProtocolEventsConsumed := TProtocolEvents.Create;
       FProtocolEventsProduced := TProtocolEvents.Create;
       FProtocolPip := TProtocolPip.Create;
       FProtocolAcdiMfg := TProtocolAcdiMfg.Create(MSI_ACDI_MFG, False);
       FProtocolAcdiUser := TProtocolAcdiUser.Create(MSI_ACDI_USER, False);
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
  inherited Destroy;
end;

initialization
  GlobalNodeList := TThreadedCirularArrayObject.Create;
  GlobalNodeList.OwnsObjects := True;

finalization
  FreeAndNil(GlobalNodeList)

end.

