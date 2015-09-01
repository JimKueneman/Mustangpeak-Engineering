unit common_settings;

// Version 0.1
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The initial developer of this code is Jim Kueneman <jimkueneman@yahoo.com> and Vcc
//

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  FileUtil,
  {$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ExtCtrls, Buttons, IniFiles;

const
  PORT_LOCAL_DEFAULT   = 12333;
  PORT_REMOTE_DEFAULT  = 12334;

const
  STR_INI_COMPORT_SECTION = 'ComPort';
  STR_INI_COMPORT_PORT_KEY = 'Port';
  STR_INI_COMPORT_BAUD_KEY = 'Baud';
  STR_INI_COMPORT_BITS_KEY = 'DataBits';
  STR_INI_COMPORT_PARITY_KEY = 'Parity';
  STR_INI_COMPORT_STOPBITS_KEY = 'StopBits';
  STR_INI_COMPORT_FLOWCONTROL_KEY = 'FlowControl';
  STR_INI_COMPORT_AUTOCONNECT = 'AutoConnect';

  STR_INT_GENERAL_SECTION = 'General';
  STR_INI_ALIASID = 'AliasID';
  STR_INI_NODEID = 'NodeID';
  STR_INI_SENDPACKETDELAY = 'SendDelay';
  STR_INI_AUTOSCAN = 'AutoScan';
  STR_INI_JMRI_FORMAT = 'JMRI_Format';
  STR_INI_LOGGING     = 'Logging';
  STR_INI_DETAILED_LOGGING = 'DetailedLogging';

  STR_INI_BOOTLOADER_SECTION = 'Bootloader';

  STR_INI_ETHERNET_SECTION = 'Ethernet';
  STR_INI_ETHERNET_LOCAL_IP = 'LocalIP';
  STR_INI_ETHERNET_REMOTE_IP = 'RemoteIP';
  STR_INI_ETHERNET_CLIENT_PORT = 'ClientPort';
  STR_INI_ETHERNET_LISTEN_PORT = 'ListenPort';



  MAX_MESSAGE_WAIT_TIME_DEFAULT = 5000;    // 5 seconds
  MAX_DATAGRAM_WAIT_TIME_DEFAULT = 20000;  // 20 seconds
  MAX_STREAM_WAIT_TIME_DEFAULT = 20000;


type
  TLoadSettingType = (
    lstEthernet,
    lstCom,
    lstUSB,
    lstGeneral
  );

type
  TComPortParity = (
    cpp_None,
    cpp_Even,
    cpp_Odd,
    cpp_Mark,
    cpp_Space
  );
const
  HI_PARTITY_TYPE = 4;

type
  TComPortFlowControl = (
    cpf_None,
    cpf_CTS_RTS,            // Hardware with CTS/RTS
    cpf_DTR_DSR,            // Hardware with DTR/DSR
    cpf_XON_XOFF            // Software
  );

const
  HI_FLOWCONTROL_TYPE = 3;

type

  { TComPortSettings }

  TComPortSettings = class
  private
    FAutoConnectAtBoot: Boolean;
    FBaudRate: LongWord;
    FDataBits: Byte;
    FFlowControl: TComPortFlowControl;
    FParity: TComPortParity;
    FPort: string;
    FStopBits: Byte;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);

    property AutoConnectAtBoot: Boolean read FAutoConnectAtBoot write FAutoConnectAtBoot;
    property BaudRate: LongWord read FBaudRate write FBaudRate;
    property DataBits: Byte read FDataBits write FDataBits;
    property Parity: TComPortParity read FParity write FParity;
    property Port: string read FPort write FPort;
    property StopBits: Byte read FStopBits write FStopBits;
    property FlowControl: TComPortFlowControl read FFlowControl write FFlowControl;
  end;

  { TGeneralSettings }

  TGeneralSettings = class
  private
    FAliasID: string;
    FAutoScanNetworkAtBoot: Boolean;
    FDatagramWaitTime: LongWord;
    FDetailedLogging: Boolean;
    FJMRILogFormat: Boolean;
    FLogging: Boolean;
    FMessageWaitTime: LongWord;
    FNodeID: string;
    FSendPacketDelay: Word;
    FStreamWaitTime: LongWord;
    procedure SetAliasID(AValue: string);
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    function AliasIDAsVal: Word;
    function NodeIDAsVal: LongWord;
    property AutoScanNetworkAtBoot: Boolean read FAutoScanNetworkAtBoot write FAutoScanNetworkAtBoot;
    property AliasID: string read FAliasID write SetAliasID;
    property DatagramWaitTime: LongWord read FDatagramWaitTime write FDatagramWaitTime;
    property NodeID: string read FNodeID write FNodeID;
    property SendPacketDelay: Word read FSendPacketDelay write FSendPacketDelay;
    property StreamWaitTime: LongWord read FStreamWaitTime write FStreamWaitTime;
    property MessageWaitTime: LongWord read FMessageWaitTime write FMessageWaitTime;
    property JMRILogFormat: Boolean read FJMRILogFormat write FJMRILogFormat;
    property Logging: Boolean read FLogging write FLogging;
    property DetailedLogging: Boolean read FDetailedLogging write FDetailedLogging;
  end;

  { TBootloaderSettings }

  TBootloaderSettings = class
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
  end;

  { TEthernetSettings }

  TEthernetSettings = class
  private
    FClientPort: Integer;
    FListenPort: Integer;
    FLocalIP: string;
    FRemoteIP: string;
  public
    constructor Create;
    procedure LoadFromFile(IniFile: TIniFile);
    procedure SaveToFile(IniFile: TIniFile);
    property LocalIP: string read FLocalIP write FLocalIP;
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    property ListenPort: Integer read FListenPort write FListenPort;            // Storage if the connection is a listener
    property ClientPort: Integer read FClientPort write FClientPort;             // Storage if the connection is a client
  end;

  // Settings that all OLCB Applications will have in common

  { TOlcbCommonSettings }

  TOlcbCommonSettings = class
  private
    FComPort: TComPortSettings;
    FEthernet: TEthernetSettings;
    FGeneral: TGeneralSettings;
    FBootloader: TBootloaderSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property ComPort: TComPortSettings read FComPort write FComPort;
    property Ethernet: TEthernetSettings read FEthernet write FEthernet;
    property General: TGeneralSettings read FGeneral write FGeneral;
    property Bootloader: TBootloaderSettings read FBootloader write FBootloader;
  end;

var
  GlobalSettings: TOlcbCommonSettings;

implementation

{ TEthernetSettings }

constructor TEthernetSettings.Create;
begin
  LocalIP := '127.0.0.1';
  RemoteIP := '127.0.0.1';
  ListenPort := 12021;
  ClientPort := 12022;
end;

procedure TEthernetSettings.LoadFromFile(IniFile: TIniFile);
begin
  LocalIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, '127.0.0.1');
  RemoteIP := IniFile.ReadString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_IP, '127.0.0.1');
  ListenPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LISTEN_PORT, PORT_LOCAL_DEFAULT);
  ClientPort := IniFile.ReadInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_CLIENT_PORT, PORT_REMOTE_DEFAULT);
end;

procedure TEthernetSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LOCAL_IP, FLocalIP);
  IniFile.WriteString(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_REMOTE_IP, FRemoteIP);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_CLIENT_PORT, FClientPort);
  IniFile.WriteInteger(STR_INI_ETHERNET_SECTION, STR_INI_ETHERNET_LISTEN_PORT, FListenPort);
end;

{ TBootloaderSettings }

constructor TBootloaderSettings.Create;
begin

end;

procedure TBootloaderSettings.LoadFromFile(IniFile: TIniFile);
begin

end;

procedure TBootloaderSettings.SaveToFile(IniFile: TIniFile);
begin

end;

{ TGeneralSettings }

procedure TGeneralSettings.SetAliasID(AValue: string);
begin
  if FAliasID=AValue then Exit;
  FAliasID:=AValue;
end;

constructor TGeneralSettings.Create;
begin
  FAliasID := '0x0AAA';
  FNodeID := '0x010203040506';
  FSendPacketDelay := 0;
  FMessageWaitTime := MAX_MESSAGE_WAIT_TIME_DEFAULT;
  FDatagramWaitTime := MAX_DATAGRAM_WAIT_TIME_DEFAULT;
  FStreamWaitTime := MAX_STREAM_WAIT_TIME_DEFAULT;
  FJMRILogFormat := False;
  FLogging := False;
  FDetailedLogging := False;
end;

procedure TGeneralSettings.LoadFromFile(IniFile: TIniFile);
begin
  AliasID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, '0x0AAA');
  NodeID := IniFile.ReadString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, '0x102030405006');
  SendPacketDelay := IniFile.ReadInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, 0);
  AutoScanNetworkAtBoot := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_AUTOSCAN, True);
  FJMRILogFormat := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_JMRI_FORMAT, False);
  FLogging := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_LOGGING, False);
  FDetailedLogging := IniFile.ReadBool(STR_INT_GENERAL_SECTION, STR_INI_DETAILED_LOGGING, False);
end;

procedure TGeneralSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_ALIASID, FAliasID);
  IniFile.WriteString(STR_INT_GENERAL_SECTION, STR_INI_NODEID, FNodeID);
  IniFile.WriteInteger(STR_INT_GENERAL_SECTION, STR_INI_SENDPACKETDELAY, FSendPacketDelay);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_AUTOSCAN, FAutoScanNetworkAtBoot);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_JMRI_FORMAT, FJMRILogFormat);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_LOGGING, FLogging);
  IniFile.WriteBool(STR_INT_GENERAL_SECTION, STR_INI_DETAILED_LOGGING, FDetailedLogging);
end;

function TGeneralSettings.AliasIDAsVal: Word;
begin
  Result := StrToInt(AliasID);
end;

function TGeneralSettings.NodeIDAsVal: LongWord;
begin
  Result := StrToInt(NodeID)
end;

{ TComPortSettings }

constructor TComPortSettings.Create;
begin
  FBaudRate := 333333;
  FDataBits := 8;
  FParity := cpp_None;
  FPort := 'COM2';
  FStopBits := 0;
  FFlowControl := cpf_None;
end;

procedure TComPortSettings.LoadFromFile(IniFile: TIniFile);
var
  Value: LongWord;
begin
  Port := IniFile.ReadString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, 'COM2');
  BaudRate := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, 333333);
  DataBits := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, 8);
  StopBits := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, 0);
  Value := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, 0);
  if Value > HI_PARTITY_TYPE then
    Value := 0;
  Parity := TComPortParity( Value);

  Value := IniFile.ReadInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, 0);
  if Value > HI_FLOWCONTROL_TYPE then
    Value := 0;
  FlowControl := TComPortFlowControl( Value);
  AutoConnectAtBoot := IniFile.ReadBool(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_AUTOCONNECT, False);
end;

procedure TComPortSettings.SaveToFile(IniFile: TIniFile);
begin
  IniFile.WriteString(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PORT_KEY, Port);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BAUD_KEY, BaudRate);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_BITS_KEY, DataBits);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_STOPBITS_KEY, StopBits);
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_PARITY_KEY, Integer( Parity));
  IniFile.WriteInteger(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_FLOWCONTROL_KEY, Integer( FlowControl));
  IniFile.WriteBool(STR_INI_COMPORT_SECTION, STR_INI_COMPORT_AUTOCONNECT, FAutoConnectAtBoot);
end;


{ TOlcbCommonSettings }

constructor TOlcbCommonSettings.Create;
begin
  inherited;
  FComPort := TComPortSettings.Create;
  FGeneral := TGeneralSettings.Create;
  FBootloader := TBootloaderSettings.Create;
  FEthernet := TEthernetSettings.Create;
end;

destructor TOlcbCommonSettings.Destroy;
begin
  FreeAndNil(FComPort);
  FreeAndNil(FGeneral);
  FreeAndNil(FBootloader);
  FreeAndNil(FEthernet);
  inherited Destroy;
end;

procedure TOlcbCommonSettings.LoadFromFile(FileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
   ComPort.LoadFromFile(IniFile);
   General.LoadFromFile(IniFile);
   Bootloader.LoadFromFile(IniFile);
   Ethernet.LoadFromFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

procedure TOlcbCommonSettings.SaveToFile(FileName: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    ComPort.SaveToFile(IniFile);
    General.SaveToFile(IniFile);
    Bootloader.SaveToFile(IniFile);
    Ethernet.SaveToFile(IniFile);
  finally
    IniFile.Free;
  end;
end;

initialization
  GlobalSettings := TOlcbCommonSettings.Create;

finalization
  FreeAndNil(GlobalSettings);

end.

