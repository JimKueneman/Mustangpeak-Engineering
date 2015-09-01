unit bootloader_driver_lazarus;

{$IFDEF FPC}
{$mode objfpc}{$H+}

interface
{$ENDIF}

{$I options_user.inc}

uses
  {$IFDEF FPC}
  Classes, SysUtils, FileUtil, file_utilities, connection_threads,
  {$ENDIF}
  bootloader_user,
  bootloader_common;

{$IFDEF FPC}
const
  PATH_MEMMAP_FILE     = 'memmap.ini';

var
  bootloader_driver_lazerus_Ethernet: TEthernetComponent;
{$ENDIF}

procedure Bootloader_Driver_Initialization;
procedure Bootloader_Driver_Finalization;
function Bootloader_Driver_ReadByte(var ReceivedByte: Byte): Boolean;
procedure Bootloader_Driver_SendByte(AByte: Byte);
procedure Bootloader_Driver_SendBytes(ABytePtr: PByte; Count: DWord);

var
  ReceivedByteStream: TMemoryStream;
  CriticalSection: TRTLCriticalSection;

implementation


procedure Bootloader_Driver_Initialization;
begin
  ReceivedByteStream := TMemoryStream.Create;
  InitCriticalSection(CriticalSection);
end;

procedure Bootloader_Driver_Finalization;
begin
  FreeAndNil(ReceivedByteStream);
  DoneCriticalsection(CriticalSection);
end;

function Bootloader_Driver_ReadByte(var ReceivedByte: Byte): Boolean;
var
  Temp: Byte;
  i: Integer;
begin
  Result := False;
  ReceivedByte := 0;
  EnterCriticalsection(CriticalSection);
  try
    if ReceivedByteStream.Size > 0 then
    begin
      ReceivedByteStream.Position := 0;
      ReceivedByte := ReceivedByteStream.ReadByte;
      for i := 1 to ReceivedByteStream.Size - 1 do
      begin
        ReceivedByteStream.Position := i;
        Temp := ReceivedByteStream.ReadByte;
        ReceivedByteStream.Position := i - 1;
        ReceivedByteStream.WriteByte(Temp);
      end;
      ReceivedByteStream.Size := ReceivedByteStream.Size - 1;
      Result := True;
    end;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
end;

procedure Bootloader_Driver_SendByte(AByte: Byte);
begin
  if Assigned(bootloader_driver_lazerus_Ethernet) then
    bootloader_driver_lazerus_Ethernet.SendByte(AByte);
end;

procedure Bootloader_Driver_SendBytes(ABytePtr: PByte; Count: DWord);
var
  i: Integer;
begin
  if Assigned(bootloader_driver_lazerus_Ethernet) then
  begin
    for i := 0 to Count - 1 do
    begin
      bootloader_driver_lazerus_Ethernet.SendByte(ABytePtr^);
      Inc(ABytePtr);
    end;
  end;
end;

end.

