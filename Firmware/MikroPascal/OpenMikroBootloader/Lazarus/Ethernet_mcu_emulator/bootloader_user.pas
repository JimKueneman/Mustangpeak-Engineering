unit bootloader_user;

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
  bootloader_common;

const
// If using the UART bootloader enter the baud rate you would like
  UART_BAUD_RATE = 115200;
// If using Ethernet enter the IP Address the Mcu is allocated
  ETHERNET_IP_ADDRESS = '192.168.0.077';
// If using Ethernet enter the port the local Mcu should use (may be automatic)
  ETHERNET_PORT = 14334;
// If using Ethernet enter the remote port the mcu should connect to
  ETHERNET_REMOTE_PORT = 14333;

// The length of time the mcu waits for a connection with the bootloader app then
// runs the application code
  BOOTLOADER_WAIT_TIME = 5000;   // in milli seconds

// Enter the size of the allocated buffer that will receive the bytes to be written to
// the Flash.  This is not the size of the page that the mcu can write this is the
// cache buffer that will be written to Flash in the max page sizes based on the mcu
  MAX_WRITE_BUFFER_SIZE = 4096;

// Enter Bootloader specifics
  BOOTLOADER_ERASE_BLOCK = 4096;
  BOOTLOADER_WRITE_BLOCK = MAX_WRITE_BUFFER_SIZE;
  BOOTLOADER_FLASH_SIZE = _512K_FLASH;
  BOOTLOADER_ADDRESS    = $9D07D120;          // _512K_FLASH - 12k
  BOOTLOADER_SIZE       = $2580;              // 9600
  BOOTLOADER_MCU_FAMILY = FAMILY_PIC32;
  BOOTLOADER_REV_LO     = 1;
  BOOTLOADER_REV_HI     = 0;
  BOOTLOADER_APP_NAME   = 'Sample Application';

type
  TRawBuffer = array[0..MAX_WRITE_BUFFER_SIZE + MAX_MESSAGE_OVERHEAD - 1] of Byte;


implementation


end.

