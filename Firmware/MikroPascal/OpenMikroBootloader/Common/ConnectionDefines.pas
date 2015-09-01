unit ConnectionDefines;

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
interface
{$ENDIF}

// Type of Link used for the Bootloader
// Set this in you Options.inc file
const
  CONNECTION_TYPE_UART      = 1;
  CONNECTION_TYPE_USB       = 2;
  CONNECTION_TYPE_ETHERNET  = 3;
  CONNECTION_TYPE_SD_CARD   = 4;

const
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

  FAMILY_PIC16F          = $00;
  FAMILY_PIC16F_ENHANCED = $01;
  FAMILY_PIC18F          = $02;
  FAMILY_PIC24           = $03;
  FAMILY_dsPIC30         = $04;
  FAMILY_dsPIC33         = $05;
  FAMILY_PIC32           = $06;

 {$IFNDEF FPC}
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
{$ENDIF}
  
type
  PByte = ^Byte;
  PWord = ^Word;
  PDWord = ^DWord;

  {$IFNDEF FPC}
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
  {$ENDIF}

type
  TRevision = array[0..1] of Byte;
  
  TMcuInfo = record
    StructSize: Word;     // Size of this Structure    // 2
    EraseBlockSize: DWord;                             // 4
    WriteBlockSize: DWord;                             // 4
    ProgramFlashSize: DWord;                           // 4
    BootloaderAddress: DWord;                          // 4
    BootloaderSize: DWord;                             // 4
    McuFamily: Byte;                                   // 1
    Revision: TRevision;                               // 2
    ApplicationName: array[0..31] of Char;             // 32
  end;

{$IFNDEF FPC}
type
  TCurrentAddresses = record
    FlashWrite: DWord;
    Erase:      DWord;
  end;


type
  TRawBuffer = array[0..MAX_WRITE_BUFFER_SIZE + MAX_MESSAGE_OVERHEAD - 1] of Byte;
  
var
  LinkInfo: TLinkInfo;
  CurrentAddresses: TCurrentAddresses;
  RawBuffer: TRawBuffer;
{$ENDIF}

function McuFamilyToStr(Family: Byte): string;

implementation

function McuFamilyToStr(Family: Byte): string;
begin
  case Family of
    FAMILY_PIC16F          : Result := 'PIC16F';
    FAMILY_PIC16F_ENHANCED : Result := 'PIC16F Enhanced';
    FAMILY_PIC18F          : Result := 'PIC18F';
    FAMILY_PIC24           : Result := 'PIC24';
    FAMILY_dsPIC30         : Result := 'dsPIC30';
    FAMILY_dsPIC33         : Result := 'dsPIC33';
    FAMILY_PIC32           : Result := 'PIC32'
  else
    Result := 'Unknown family';
  end;
end;

end.
