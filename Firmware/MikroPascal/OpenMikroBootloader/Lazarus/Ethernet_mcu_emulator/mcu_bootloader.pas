unit mcu_bootloader;

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

interface
{$ENDIF}

{$I options_user.inc}

uses
  {$IFDEF FPC}
    Classes, SysUtils,
  {$ENDIF}

  // Define the PC to Mcu Connection Driver
  {$IFDEF FPC}
    bootloader_driver_lazarus,
  {$ELSE}
    {$IFDEF CONNECTION_UART}
      bootloader_driver_uart,
    {$ENDIF}
    {$IFDEF CONNECTION_ETHERNET}
    bootloader_driver_ethernet,
    {$ENDIF}
    {$IFDEF CONNECTION_USB}
    bootloader_driver_usb,
    {$ENDIF}
  {$ENDIF}

  // Define the mcu Flash Erase/Write methods
  {$IFDEF FPC}
  bootloader_mcu_lazarus,
  {$ELSE}
    {$IFDEF P32}
    bootloader_mcu_P32
    {$ENDIF}
    {$IFDEF PIC33}
    bootloader_mcu_PIC33,
    {$ENDIF}
    {$IFDEF PIC30}
    bootloader_mcu_PIC30,
    {$ENDIF}
    {$IFDEF PIC24}
    bootloader_mcu_PIC24,
    {$ENDIF}
    {$IFDEF PIC18}
     bootloader_mcu_PIC18,
    {$ENDIF}
    {$IFDEF PIC16}
    bootloader_mcu_PIC16,
    {$ENDIF}
  {$ENDIF}
  bootloader_user,
  bootloader_common;

const
  RESET_VECTOR_SIZE = 16;   // RETHINK WHERE THIS GOES AND WHAT IT DOES



procedure main;

{$IFDEF FPC}
type
  TAddMessageCallback = procedure(Msg: string) of object;

var
  AddMessageCallback: TAddMessageCallback;
  WaitForBootLinkDetectedTimout: Boolean;
{$ENDIF}

var
  BootInfo: TBootInfo;
  CurrentAddresses: TCurrentAddresses;
  RawBuffer: TRawBuffer;
  BootloaderEnabled, McuInfoLoaded: Boolean;      // For debugging in Lazarus

implementation

{$IFDEF FPC}
function Low(X: DWord): Byte;
begin
  Result := Byte(X and $000000FF)
end;

function High(X: DWord): Byte;
begin
  Result := Byte((X shr 8) and $000000FF)
end;

function Higher(X: DWord): Byte;
begin
  Result := Byte((X shr 16) and $000000FF)
end;

function Highest(X: DWord): Byte;
begin
  Result := Byte((X shr 24) and $000000FF)
end;
{$ENDIF}

 // Translate back and forth from Kernal to Physical address
 function KVA_TO_PA(Address: DWord): DWord;  //  ((v) & 0x1fffffff)
 begin
   Result := Address and $1FFFFFFF
 end;

 function PA_TO_KVA0(Address: DWord): DWord; // ((pa) | 0x80000000)
 begin
   Result := Address or $80000000
 end;

 function PA_TO_KVA1(Address: DWord): DWord; //((pa) | 0xa0000000)
 begin
   Result := Address or $A0000000
 end;

 // Translate between Kernal addresses
 function KVA0_TO_KVA1(Address: DWord): DWord; // ((v) | 0x20000000)
 begin
   Result := Address or $20000000
 end;

 function KVA1_TO_KVA0(Address: DWord): DWord; // ((v) & ~0x20000000)
 begin
   Result := Address and not $20000000
 end;

 // Tests for Kernal addresses
function  IS_KVA(Address: DWord): Boolean;  //     ((int)(v) < 0)
begin
  Result := Integer(Address) < 0
end;

function  IS_KVA0(Address: DWord): Boolean;  //       (((unsigned)(v) >> 29) == 0x4)
begin
  Result := Address shr 29 = 4
end;

function  IS_KVA1(Address: DWord): Boolean;  //       (((unsigned)(v) >> 29) == 0x5)
begin
  Result := Address shr 29 = 5
end;

function  IS_KVA01(Address: DWord): Boolean;  //     (((unsigned)(v) >> 30) == 0x2)
begin
  Result := Address shr 29 = 2
end;

procedure Initialize;
begin
  {$IFDEF FPC}AddMessageCallback := nil; WaitForBootLinkDetectedTimout := False;{$ENDIF}
  BootloaderEnabled := True;

  BootInfo.StructSize := LINKINFO_REC_SIZE;   // NOTE:  SizeOf(TBootInfo) does not work reliably because of padding the compiler may do
  BootInfo.EraseBlockSize := BOOTLOADER_ERASE_BLOCK;
  BootInfo.WriteBlockSize := BOOTLOADER_WRITE_BLOCK;
  BootInfo.ProgramFlashSize := BOOTLOADER_FLASH_SIZE;
  BootInfo.BootloaderAddress := BOOTLOADER_ADDRESS;
  BootInfo.BootloaderSize := BOOTLOADER_SIZE;
  BootInfo.McuFamily := BOOTLOADER_MCU_FAMILY;
  BootInfo.Revision[0] := BOOTLOADER_REV_LO;
  BootInfo.Revision[1] := BOOTLOADER_REV_HI;
  BootInfo.ApplicationName := BOOTLOADER_APP_NAME;

  McuInfoLoaded := True;

  CurrentAddresses.FlashWrite := $00000000;
  CurrentAddresses.Erase      := $00000000;
end;

procedure SendByte(AByte: Byte);
begin
  Bootloader_Driver_SendByte(AByte);
end;

procedure SendWord(AWord: Word);
begin
  {$IFDEF FPC}Bootloader_Driver_SendByte( Low(AWord)); Bootloader_Driver_SendByte( High(AWord));{$ELSE}
  Bootloader_Driver_SendByte( Lo(AWord));       // Little Endian
  Bootloader_Driver_SendByte( Hi(AWord));       // Little Endian
  {$ENDIF}
end;

procedure SendDWord(ADWord: DWord);
begin
  {$IFDEF FPC} Bootloader_Driver_SendByte( Low(ADWord));  Bootloader_Driver_SendByte( High(ADWord));{$ELSE}
  Bootloader_Driver_SendByte( Lo(ADWord));       // Little Endian
  Bootloader_Driver_SendByte( Hi(ADWord));       // Little Endian
  {$ENDIF}
  Bootloader_Driver_SendByte( Higher(ADWord));   // Little Endian
  Bootloader_Driver_SendByte( Highest(ADWord));  // Little Endian
end;

function WaitForBootLinkDetected: Boolean;
var
  DelayTime: Word;
  ReceivedByte: Byte;
begin
  Result := False;
  DelayTime := 0;
  ReceivedByte := 0;
  while (DelayTime < BOOTLOADER_WAIT_TIME) and BootloaderEnabled do
  begin
    if Bootloader_Driver_ReadByte(ReceivedByte) then
      Result :=  ReceivedByte = CMD_SYNC;
    if Result then
      Break;
    {$IFDEF FPC}
    if WaitForBootLinkDetectedTimout then
      DelayTime := BOOTLOADER_WAIT_TIME;
    {$ELSE}
    DelayTime := DelayTime + 100;
    Delay_ms(100);                   // This is okay because we are only sending one byte so the Buffer won't overrun
    {$ENDIF}
  end
end;

procedure ReadInMessage(var Buffer: TRawBuffer);
var
  iState: Byte;
  iBuffer: Word;
  LocalCount: DWord;
begin
  iState := $FF;
  iBuffer := 0;
  while BootloaderEnabled do
  begin
    if Bootloader_Driver_ReadByte(Buffer[iBuffer]) then
    begin
      case iState of
        $FF : begin
              iState := Buffer[iBuffer]; // First Byte is the Command
              if (iState and CMD_PARAMETERLESS_MASK <> 0) then
                Exit            // Single byte message
              else
                Inc(iBuffer);   // More coming
            end;
        CMD_SET_ADDRESS :
            begin
              Inc(iBuffer);
              if iBuffer > 5 then         // 6 bytes of data
                Exit;
            end;
        CMD_ERASE_BLOCKS :
            begin
              Inc(iBuffer);
              if iBuffer > 4 then         // 5 bytes of data
                Exit;
            end;
        CMD_WRITE_BLOCK :
            begin
              Inc(iBuffer);
              if (iBuffer > MAX_WRITE_BUFFER_SIZE + MAX_MESSAGE_OVERHEAD - 1) then         // 6 bytes of data
                Exit;
            end;
        CMD_WRITE :
            begin
              Inc(iBuffer);
              if (iBuffer = 5) then
                LocalCount := PDWord( @RawBuffer[1])^;
              if iBuffer > 5 then
              begin
                if (iBuffer > LocalCount + 3 - 1) then
                  Exit;
              end;
            end;
      end;
    end;
  end;
end;

var
  i: Integer;

procedure main;
begin
  Initialize;
  Bootloader_Driver_Initialization;
  Bootloader_Mcu_Initialization;
  SendByte(CMD_LINK);
  SendByte(CMD_LINK);
  SendByte(CMD_LINK);
  if WaitForBootLinkDetected then
  begin
    SendByte(CMD_SYNC);
    while BootloaderEnabled do   // Only way out of here is a reset
    begin
      ReadInMessage(RawBuffer);
      case RawBuffer[0] of
        CMD_REQEUST_FLASH_DATA :
            begin
              {$IFDEF FPC}if Assigned(AddMessageCallback) then AddMessageCallback('FlashData Requested');{$ENDIF}
              SendByte(CMD_REQEUST_FLASH_DATA);
              SendWord(BootInfo.StructSize);
              SendDWord( BootInfo.EraseBlockSize);
              SendDWord( BootInfo.WriteBlockSize);
              SendDWord( BootInfo.ProgramFlashSize);
              SendDWord( BootInfo.BootloaderAddress);
              SendDWord( BootInfo.BootloaderSize);
              SendByte(BootInfo.McuFamily);
              SendByte(BootInfo.Revision[0]);
              SendByte(BootInfo.Revision[1]);
              for i := 0 to 31 do
                SendByte(Ord( BootInfo.ApplicationName[i]));
            end;
        CMD_SET_ADDRESS :
            begin
              case RawBuffer[1] of
                CMD_SET_ADDRESS_WRITE : begin
                        CurrentAddresses.FlashWrite := PDWord( @RawBuffer[2])^;
                        {$IFDEF FPC}if Assigned(AddMessageCallback) then AddMessageCallback('SetAddress: FlashWrite = ' + IntToHex(CurrentAddresses.FlashWrite , 8) + '[' + IntToStr(CurrentAddresses.FlashWrite ) + ']');{$ENDIF}
                      end;
                CMD_SET_ADDRESS_ERASE : begin
                        CurrentAddresses.Erase := PDWord( @RawBuffer[2])^;
                        {$IFDEF FPC}if Assigned(AddMessageCallback) then AddMessageCallback('SetAddress: FlashWrite = ' + IntToHex(CurrentAddresses.Erase, 8) + '[' + IntToStr(CurrentAddresses.Erase) + ']');{$ENDIF}
                      end;
              end;
            end;
        CMD_RESET :
            begin
              {$IFNDEF FPC}
              Reset;
              {$ENDIF}
            end;
        CMD_ERASE_BLOCKS :
            begin
              for i := 0 to PDWord( @RawBuffer[1])^ do
              begin
               {$IFDEF FPC}if Assigned(AddMessageCallback) then AddMessageCallback('Erasing Address: ' + IntToHex(CurrentAddresses.Erase, 8) + '[' + IntToStr(CurrentAddresses.Erase) + ']');{$ENDIF}
                Bootloader_Mcu_EraseFlash(CurrentAddresses.Erase);
                CurrentAddresses.Erase := CurrentAddresses.Erase + BootInfo.EraseBlockSize;
              end;
              SendByte(RawBuffer[0]);  // Done
            end;
        CMD_WRITE_BLOCK :
            begin
              Bootloader_Mcu_WriteFlash(CurrentAddresses.FlashWrite, BootInfo.WriteBlockSize, @RawBuffer[1]);
              CurrentAddresses.FlashWrite := CurrentAddresses.FlashWrite + BootInfo.WriteBlockSize;
              {$IFDEF FPC}if Assigned(AddMessageCallback) then AddMessageCallback('Writing Address Block: ' + IntToHex(CurrentAddresses.FlashWrite, 8) + '[' + IntToStr(CurrentAddresses.FlashWrite) + ']');{$ENDIF}
              SendByte(RawBuffer[0]);  // Done
            end;
        CMD_WRITE :
            begin
              Bootloader_Mcu_WriteFlash(CurrentAddresses.FlashWrite, PDWord( @RawBuffer[1])^, @RawBuffer[3]);
              CurrentAddresses.FlashWrite := CurrentAddresses.FlashWrite + PDWord( @RawBuffer[1])^;
              {$IFDEF FPC}if Assigned(AddMessageCallback) then AddMessageCallback('Writing Address Count: ' + IntToStr(PDWord( @RawBuffer[1])^) + ', ' + IntToHex(CurrentAddresses.FlashWrite, 8) + '[' + IntToStr(CurrentAddresses.FlashWrite) + ']');{$ENDIF}
              SendByte(RawBuffer[0]);  // Done
            end;
      end;
    end;
  end else
  begin
    Bootloader_Driver_Finalization;
    SendByte(CMD_UNLINK);
    // Jump to code
    while BootloaderEnabled do
    begin
      {$IFDEF FPC}if Assigned(AddMessageCallback) and (WaitForBootLinkDetectedTimout) then begin WaitForBootLinkDetectedTimout := False; AddMessageCallback('Simulated App Running'); end;{$ELSE}
      Delay_ms(500);
      LATF13_bit := not LATF13_bit
      {$ENDIF}
    end;
  end;
end;

end.

