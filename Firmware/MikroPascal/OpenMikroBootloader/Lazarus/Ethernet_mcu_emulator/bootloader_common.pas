unit bootloader_common;

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

const
  MAX_MESSAGE_OVERHEAD = 3; // Largest Message is the Write Partial Block with the WriteBlock Size of Data + CommandByte + CountWord

// Type of Link used for the Bootloader
// Set this in you Options.inc file
const
  LINK_TYPE_UART      = 1;
  LINK_TYPE_USB       = 2;
  LINK_TYPE_ETHERNET  = 3;
  LINK_TYPE_SD_CARD   = 4;

const
  // $FF is reserved
  CMD_LINK               = $EF;
  CMD_UNLINK             = $EE;
  CMD_SYNC               = $ED;
  CMD_REQEUST_FLASH_DATA = $80;
  CMD_SET_ADDRESS        = $02;
  CMD_RESET              = $81;
  CMD_ERASE_BLOCKS       = $10;
  CMD_WRITE_BLOCK        = $30;
  CMD_WRITE              = $31;
  CMD_PARAMETERLESS_MASK = $80;  // Messages with no paramters have the high bit set

  CMD_SET_ADDRESS_WRITE  = $00;
  CMD_SET_ADDRESS_ERASE  = $01;

  FAMILY_PIC16F     = $00;
  FAMILY_PIC18F     = $01;
  FAMILY_PIC24      = $02;
  FAMILY_dsPIC30    = $03;
  FAMILY_dsPIC33    = $04;
  FAMILY_PIC32      = $05;

  _512K_FLASH = 524288;
  _256K_FLASH = 262144;
  _128K_FLASH = 131072;
  _64K_FLASH  = 65536;
  _32K_FLASH  = 32768;
  _16K_FLASH  = 16384;
  _8K_FLASH   = 8192;
  _4K_FLASH   = 4096;

const
  {$IFDEF PIC16}
  FlashAddressSize = 1;
  FlashDataSize = 1;
  {$ENDIF}
  {$IFDEF PIC18}
  FlashAddressSize = 1;
  FlashDataSize = 1;
  {$ENDIF}
  {$IFDEF PIC24}
  FlashAddressSize = 2;
  FlashDataSize = 2;
  {$ENDIF}
  {$IFDEF PIC30}
  FlashAddressSize = 2;
  FlashDataSize = 2;
  {$ENDIF}
  {$IFDEF PIC33}
  FlashAddressSize = 2;
  FlashDataSize = 2;
  {$ENDIF}
  {$IFDEF P32}
  FlashAddressSize = 4;
  FlashDataSize = 4;
  {$ENDIF}
  {$IFDEF FPC}
  FlashAddressSize = 4;
  FlashDataSize = 4;
  {$ENDIF}

type
  PByte = ^Byte;
  PWord = ^Word;
  PDWord = ^DWord;

  {$IFDEF PIC16}
  PAddressPtr = PByte;
  PFlashDataPtr = PByte;
  {$ENDIF}
  {$IFDEF PIC18}
  PAddressPtr = PByte;
  PFlashDataPtr = PByte;
  {$ENDIF}
  {$IFDEF PIC24}
  PAddressPtr = PWord;
  TFlashDataPtr = PWord;
  {$ENDIF}
  {$IFDEF PIC30}
  PAddressPtr = PWord;
  PFlashDataPtr = PWord;
  {$ENDIF}
  {$IFDEF PIC33}
  PAddressPtr = PWord;
  PFlashDataPtr = PWord;
  {$ENDIF}
  {$IFDEF P32}
  PAddressPtr = PDWord;
  PFlashDataPtr = PDWord;
  {$ENDIF}
  {$IFDEF FPC}
  PAddressPtr = PDWord;
  PFlashDataPtr = PDWord;
  {$ENDIF}

type
  TRevision = array[0..1] of Byte;

const
  LINKINFO_REC_SIZE = 57;

type
  TBootInfo = record
    StructSize: Word;     // Size of this Structure     2+(5*4)+1+2+32=57
    EraseBlockSize: DWord;
    WriteBlockSize: DWord;
    ProgramFlashSize: DWord;
    BootloaderAddress: DWord;
    BootloaderSize: DWord;
    McuFamily: Byte;
    Revision: TRevision;
    ApplicationName: string[32];
  end;

type
  TCurrentAddresses = record
    FlashWrite: DWord;
    Erase:      DWord;
  end;


implementation

end.

