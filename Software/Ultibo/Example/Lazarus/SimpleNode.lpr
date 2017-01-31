program SimpleNode;

{$R *.res}

{$I ..\..\Lcc\lcc_compilers.inc}

uses
  {$IFNDEF LCC_WINDOWS}
  cthreads,
  {$ENDIF}
  lcc.node,
  mustangpeak.classes,
  lcc.utilities,
  lcc.types.can,
  protocol.datagram.configuration.definition.information,
  lcc.transfer,
  lcc.transfer.tcp.server,
  lcc.transfer.tcp.client,
  lcc.transfer.gridconnect.wire.synapse;

var
  Node: TLccNode;
  IP: String;

begin
  {$IFDEF LCC_WINDOWS}
  Node := TLccNode.Create('Z:\Software\Ultibo\Example\Lazarus\NodeDefinitionFile.xml');
  {$ELSE}
  Node := TLccNode.Create('/Users/jimkueneman/Documents/Mustangpeak Engineering/Software/Ultibo/Example/Lazarus/NodeDefinitionFile.xml');
  IP := ResolveUnixIp;
  IP := '127.0.0.1';
  {$ENDIF}
  GlobalNodeList.Add(Node);
//  GlobalTransferManagerTcpClient.Start(IP, 12021, False, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);
  GlobalTransferManagerTcpServer.Start(IP, 12021, False, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);
 // if GlobalTransferManagerTcpClient.Connected then
  if GlobalTransferManagerTcpServer.Connected then
  begin
    Node.AliasIDEngine.Enabled := True; // Use CAN Aliases
    Node.Start;
    while True do
    begin
      Node.ProcessMessages;
    end;
  end;
end.

