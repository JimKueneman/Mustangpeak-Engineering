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
  baseutilities, protocol.snip, protocol.pip, protocol.traction.stni,
  protocol.traction, protocol.events, protocol.traction.fdi,
  protocol.traction.fci, protocol.datagram.cdi, protocol.acdi.mfg,
  protocol.acdi.user, protocol.datagram.configuration,
  protocol.datagram.configuration.memory,
  protocol.datagram.configuration.memory.info,
  protocol.datagram.confguration.memory.options;

begin
  { Add your program code here }
end.

