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

//{$DEFINE LINK_TYPE_USB_HID}
//{$DEFINE LINK_TYPE_USB_CDC}
//{$DEFINE LINK_TYPE_ETHERNET}
{$DEFINE LINK_TYPE_UART}
//{$DEFINE LINK_TYPE_SD}  //The PIC appears as a mass storage device to PC

//UART communication ports. Please use only one communication port, thus do not uncomment more than one line:
{$IFDEF LINK_TYPE_UART}
  //{$DEFINE UART1}
  {$DEFINE UART2}
  //{$DEFINE UART3}
  //{$DEFINE UART4}
  //{$DEFINE UART5}
  //{$DEFINE UART6}
{$ENDIF}

//Uncomment the following line if you want to initialize the UART module using UARTx_Init_Advaced, otherwise UARTx_Init is used.
//{$DEFINE UsingUARTInitAdvanced}

//Uncomment the following line if you want to use UART port for debugging
{$DEFINE UART_Debug}
{$IFDEF UART_Debug}
  //Please use only one debug port, thus do not uncomment more than one line.
  //Also, make sure the debug port is different than communication port.
  //{$DEFINE UART1_Dbg}
  //{$DEFINE UART2_Dbg}
  //{$DEFINE UART3_Dbg}
  //{$DEFINE UART4_Dbg}
  {$DEFINE UART5_Dbg}
  //{$DEFINE UART6_Dbg}
{$ENDIF}