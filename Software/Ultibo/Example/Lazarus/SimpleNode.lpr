program SimpleNode;

{$R *.res}

{$I ..\..\Lcc\lcc_compilers.inc}

uses
  {$IFNDEF LCC_WINDOWS}
  cthreads,
  {$ENDIF}
  lcc.node, mustangpeak.classes, lcc.transfer.synapse.gridconnect,
  lcc.utilities, lcc.can.types, lcc.transfer;

var
  Node: TLccNode;
  IP: String;

begin
  {$IFDEF LCC_WINDOWS}
  Node := TLccNode.Create('Z:\Software\Ultibo\Example\Lazarus\NodeDefinitionFile.xml');
  {$ELSE}
  Node := TLccNode.Create('/Users/jimkueneman/Documents/Mustangpeak Engineering/Software/Ultibo/Example/Lazarus/NodeDefinitionFile.xml');
  IP := ResolveUnixIp;
  {$ENDIF}
  GlobalNodeList.Add(Node);
  GlobalTransferManager.Start(IP, 12021, False, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);
  if GlobalTransferManager.Connected then
  begin
    Node.Start;
    while True do
    begin

    end;
  end;
end.

