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
  {$IFDEF ULTIBO}
  Console,
  HTTP,
  WebStatus,
  uTFTP,
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC,         {Include the MMC/SD core to access our SD card}
  BCM2709,     {And also include the MMC/SD driver for the Raspberry Pi 2}
  {$ENDIF}
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
  HTTPListener: THTTPListener;


{$IFDEF ULTIBO}
procedure FtpMsg(Sender : TObject; s : string);
begin
  ConsoleWriteLn('FTP: ' + s);
end;
{$ENDIF}

begin
  {$IFDEF LCC_WINDOWS}
  Node := TLccNode.Create('Z:\Software\Ultibo\Example\Lazarus\NodeDefinitionFile.xml');
  {$ELSE}
    {$IFDEF ULTIBO}
    ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, True);
    ConsoleWriteLn('Welcome to the Mustangpeak Simple LCC Node with Ultibo');
    SetOnMsg(@FtpMsg);  // TFTP

    ConsoleWriteLn('Waiting for Network...');
    WaitForNetworkConnection(True);
    ConsoleWriteLn('Network found');
    ConsoleWriteLn('Resolving IP Address...');
    IP := ResolveUltiboIp;

    ConsoleWriteLn('Waiting for drive C:\');
    while not DirectoryExists('C:\') do
    begin
     {Sleep for a second}
     Sleep(1000);
    end;
   ConsoleWriteLn('C:\ drive is ready');

   ConsoleWriteLn('Looking for XML file');
   if FileExists('C:\NodeDefinitionFile.xml') then
     ConsoleWriteLn('File Found')
   else
     ConsoleWriteLn('File not Found');




    {Create and start HTTP Listener}
    HTTPListener := THTTPListener.Create;
    HTTPListener.Active := True;
    {Register Web Status}
    WebStatusRegister(HTTPListener,'','',True);


    ConsoleWriteLn('Creating Node');
    Node := TLccNode.Create('C:\NodeDefinitionFile.xml');
    if Assigned(Node) then
      ConsoleWriteLn('Node created: ' + IntToHex(QWord( @Node), 8))
    else
      ConsoleWriteLn('Node creation failed: ' + IntToHex(QWord( @Node), 8));

    {$ELSE}
    // Must be OSX
    Node := TLccNode.Create('/Users/jimkueneman/Documents/Mustangpeak Engineering/Software/Ultibo/Example/Lazarus/NodeDefinitionFile.xml');
    IP := ResolveUnixIp;
    IP := '127.0.0.1';
    {$ENDIF}
  {$ENDIF}
  if Assigned(Node) then
  begin
    GlobalNodeList.Add(Node);
  //  GlobalTransferManagerTcpClient.Start(IP, 12021, True, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);
    WriteLn('Starting Node');
    GlobalTransferManagerTcpServer.Start(IP, 12021, True, TGridConnectSendTcpThread, TGridConnectReceiveTcpThread);
   // if GlobalTransferManagerTcpClient.Connected then
    if GlobalTransferManagerTcpServer.Connected then
    begin
      WriteLn('Node Started');
      Node.AliasIDEngine.Enabled := True; // Use CAN Aliases
      Node.Start;
      while True do
      begin
        Node.ProcessMessages;
      end;
    end;
  end
end.

