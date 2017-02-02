program SimpleNode;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  { Add additional units here }
  lcc.node,
  lcc.objects,
  lcc.message,
  lcc.types,
  mustangpeak.half_float,
  lcc.utilities,
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
  {$IFDEF ULTIBO}
  WaitForNetworkConnection(True);
  IP := ResolveUltiboIp;
  {$ELSE}
  IP := ResolveUnixIp;
  {$ENDIF}
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

