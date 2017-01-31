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
  lcc.node, lcc.objects, lcc.message, lcc.types, mustangpeak.half_float,
  lcc.utilities,
  lcc.transfer,
  lcc.transfer.tcp.server,
  lcc.transfer.tcp.client,
  lcc.transfer.gridconnect.wire.synapse;

begin
  { Add your program code here }
end.

