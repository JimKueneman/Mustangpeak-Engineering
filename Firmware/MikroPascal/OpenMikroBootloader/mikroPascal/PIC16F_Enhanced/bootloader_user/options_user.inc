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

// Select communication link to the MCU for the bootloader to connect to

//{$DEFINE CONNECTION_USB_CAN}
//{$DEFINE CONNECTION_USB_HID}
//{$DEFINE CONNECTION_USB_CDC}
//{$DEFINE CONNECTION_ETHERNET}
{$DEFINE CONNECTION_UART}
//{$DEFINE CONNECTION_SD}  //The PIC appears as a mass storage device to PC



//UART communication ports. Please use only one communication port, thus do not uncomment more than one line:

{$IFDEF CONNECTION_UART}
  {$DEFINE UART_1}
  //{$DEFINE UART_2}
  //{$DEFINE UART_3}
  //{$DEFINE UART_4}
  //{$DEFINE UART_5}
  //{$DEFINE UART_6}
{$ENDIF}



//Uncomment the following line if you want to link in the bootloader_userdebug file for custom debugging
// the default is UART5 that works on the PIC Fusion 7

//{$DEFINE USER_DEBUG}

