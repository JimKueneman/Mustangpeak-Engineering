unit mcu_loader_base;

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ConnectionDefines, ComCtrls, intel_hex_parser, KGrids, Dialogs;

type
  // Override this class to create MCU specific bootloaders

  { TMcuLoaderBase }

  TMcuLoaderBase = class
  protected
    procedure AddMainProgramVectorCustomAddress(McuInfo: TMcuInfo; BlockList: TPhysicalAddressBlockList); virtual;
  public
    procedure FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser); virtual;
    procedure LoadHexMapInUI(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView); virtual;
    procedure CustomAddress(McuInfo: TMcuInfo; BlockList: TPhysicalAddressBlockList); virtual;
    procedure IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups); virtual;
    function AddressToPhysicalAddress(Address: DWord): DWord; virtual;
    function MainProgramVectorAddressSize: DWord; virtual;
  end;

  { TPic16FLoader }

  TPic16FLoader = class(TMcuLoaderBase)
  public
    procedure FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser); override;
    procedure LoadHexMapInUI(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView); override;
    procedure IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups); override;
    function MainProgramVectorAddressSize: DWord; override;
  end;

  { TPic16FEnhanced }

  TPic16FEnhanced = class(TPic16FLoader)
  public
    procedure FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser); override;
  end;

  { TPic18FLoader }

  TPic18FLoader = class(TMcuLoaderBase)
  public
  end;

  { TPic33Loader }

  TPic33Loader = class(TMcuLoaderBase)
  public
    procedure FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser); override;
    procedure LoadHexMapInUI(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView); override;
    procedure IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups); override;
    function MainProgramVectorAddressSize: DWord; override;
  end;

  { TPic24Loader }

  TPic24Loader = class(TPic33Loader)
  public
  end;

  { TPic30Loader }

  TPic30Loader = class(TPic33Loader)
  public
    procedure IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups); override;
  end;

  { TPic32Loader }

  TPic32Loader = class(TMcuLoaderBase)
  public
    function AddressToPhysicalAddress(Address: DWord): DWord; override;
    procedure FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser); override;
    procedure LoadHexMapInUI(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView); override;
    procedure IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups); override;
    function MainProgramVectorAddressSize: DWord; override;
  end;

{$IFDEF FPC}
function Low(X: DWord): Byte;
function High(X: DWord): Byte;
function Higher(X: DWord): Byte;
function Highest(X: DWord): Byte;
{$ENDIF}
// Translate back and forth from Kernal to Physical address
function KVA_TO_PA(Address: DWord): DWord;  //  ((v) & 0x1fffffff)
function PA_TO_KVA0(Address: DWord): DWord; // ((pa) | 0x80000000)
function PA_TO_KVA1(Address: DWord): DWord; //((pa) | 0xa0000000)
// Translate between Kernal addresses
function KVA0_TO_KVA1(Address: DWord): DWord; // ((v) | 0x20000000)
function KVA1_TO_KVA0(Address: DWord): DWord; // ((v) & ~0x20000000)
// Tests for Kernal addresses
function  IS_KVA(Address: DWord): Boolean;  //     ((int)(v) < 0)
function  IS_KVA0(Address: DWord): Boolean;  //       (((unsigned)(v) >> 29) == 0x4)
function  IS_KVA1(Address: DWord): Boolean;  //       (((unsigned)(v) >> 29) == 0x5)
function  IS_KVA01(Address: DWord): Boolean;  //     (((unsigned)(v) >> 30) == 0x2)

function PA_TO_BOOTFLASH(Address: DWord): DWord;

implementation

const
  PIC32_PROGRAM_FLASH = $1D000000;
  PIC32_BOOT_FLASH    = $1FC00000;
  PIC32_CONFIG_FLASH  = $1FC02FF0;

  PIC33_PROGRAM_FLASH = $000000;
  PIC33_CONFIG_FLASH = $F80000;

  PIC30_PROGRAM_FLASH = $000000;
  PIC30_CONFIG_FLASH = $F80000;


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

function PA_TO_BOOTFLASH(Address: DWord): DWord;
begin
  Result := Address or $B0000000
end;

{ TPic16FEnhanced }

procedure TPic16FEnhanced.FixupBootloader(McuInfo: TMcuInfo;
  IntelHexParser: TIntelHexParser);
begin
  inherited FixupBootloader(McuInfo, IntelHexParser);
end;

{ TPic16FLoader }

procedure TPic16FLoader.FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser);

// PIC16 may need to jump to a new page for the main, or it may not.  If it does not then it does not need to set
  // the high bits of the PC counter (PCLATH).
  //
  // [Main in Page 0]   0x0005-0x07FF (5-2047)
  // ;Address Opcode         ASM
  // 0x0001        0xxxx              GOTO       xxxx
  //
  // [Main in Page 1]   0x0800-0x0FFF (2048-4095)
  // ;Address Opcode         ASM
  // 0x0000        0x158A              BSF        PCLATH, 3
  // 0x0001        0xxxx              GOTO       xxxx
  //
  // [Main in Page 2]   0x1000-0x17FF (4096-6143)
  // ;Address Opcode         ASM
  // 0x0000        0x158A              BSF        PCLATH, 4
  // 0x0001        0xxxx              GOTO       xxxx
  //
  // [Main in Page 3]    0x1800-0x1FFF (6144-8191)
  // ;Address Opcode         ASM
  // 0x0000        0x158A              BSF        PCLATH, 3
  // 0x0000        0x160A              BSF        PCLATH, 4
  // 0x0001        0xxxx              GOTO       xxxx
  //
  // Because of this we can't fix up the code because our bootloader
  // is in the last page so we need the BSF and if we tried to shift the rest of the code
  // the precalculated addresses in the application would be incorrect. This will also
  // effect what needs to be done in the fixup in the bootloader to return to the application code.
  // Bottom line the PIC16 will not be able to load any Hex, it will need the application
  // compiled such that the main function is org'ed into the same bank as the bootloader
  // application, but small enough that it can fit in the bank along with the bootloader.
  // It must be set less than the bootloaderStart - EraseBlockSize AND fit within the last page
var
  TargetAddress: DWord;
  Block: TBaseBlock;
  Instruction0, Instruction1, Instruction2: DWord;
  BootLoaderPage, AppMainPage: Integer;
begin
  // Generate a jump to the start of the Bootloader code
  // Write it in the startup area of the bootloaded application so it will first jump to the booloader code
  TargetAddress := 0;
  Block := IntelHexParser.PhysicalWriteBlockList.FindBlock(TargetAddress);
  if Assigned(Block) then
  begin
    // Calculate what page the Bootloader Page is on
    if McuInfo.BootloaderAddress < $07FF then
      BootLoaderPage := 0
    else
    if McuInfo.BootloaderAddress < $0FFF then
      BootLoaderPage := 1
    else
    if McuInfo.BootloaderAddress < $17FF then
      BootLoaderPage := 2
    else
    if McuInfo.BootloaderAddress < $1FFF then
      BootLoaderPage := 3
    else
      BootLoaderPage := -1;

    // Determine what page the application Main is on
    Instruction0 := Block.ReadWordInstruction(TargetAddress);
    Instruction1 := Block.ReadWordInstruction(TargetAddress + 1);
    Instruction2 := Block.ReadWordInstruction(TargetAddress + 2);

    if Instruction0 and $2800 = $2800 then                                         // Is the first instruction a Goto?
      AppMainPage := 0
    else
    if (Instruction0 and $158A = $158A) and (Instruction1 and $2800 = $2800) then   // Is it a BSF PCLATH, 3 and a Goto instruction?
      AppMainPage := 1
    else
    if (Instruction0 and $160A = $160A) and (Instruction1 and $2800 = $2800) then   // Is it a BSF PCLATH, 4 and a Goto instruction?
      AppMainPage := 2
    else
    if ((Instruction0 and $160A = $160A) and (Instruction1 and $158A = $158A) and (Instruction2 and $2800 = $2800)) or
       ((Instruction1 and $160A = $160A) and (Instruction0 and $158A = $158A) and (Instruction2 and $2800 = $2800)) then   // Is it a BSF PCLATH, 4 and a Goto instruction?
      AppMainPage := 3
    else
      AppMainPage := -1;

    if AppMainPage = BootLoaderPage then
    begin
      // Fixup the reset jump to point to the bootloader
      if AppMainPage >= 0 then
      begin
        case BootLoaderPage of
          0    : TargetAddress := 0;
          1, 2 : TargetAddress := 1;
          3    : TargetAddress := 2;
        end;
        Block.WriteWordInstruction(TargetAddress, $2800 or (McuInfo.BootloaderAddress and $7FF));  // Only bottom 11 bits are placed in the goto the high 2 bits are based on the BSF instructions

        // Fixup the jump back to the original application main
        TargetAddress := McuInfo.BootloaderAddress - 2;
        Block := IntelHexParser.PhysicalWriteBlockList.FindBlock(TargetAddress);
        if Assigned(Block) then
        begin
          case BootLoaderPage of
            0    : begin
                     Block.WriteWordInstruction(McuInfo.BootloaderAddress - 2, Instruction0);         // goto to application main that was read earlier
                     Block.WriteWordInstruction(McuInfo.BootloaderAddress - 1, $0000);                // nop
                   end;
            1, 2 : begin
                     Block.WriteWordInstruction(McuInfo.BootloaderAddress - 2, Instruction1);         // goto to application main that was read earlier
                     Block.WriteWordInstruction(McuInfo.BootloaderAddress - 1, $0000);                // nop
                   end;
            3    : begin
                     Block.WriteWordInstruction(McuInfo.BootloaderAddress - 2, Instruction2);         // goto to application main that was read earlier
                     Block.WriteWordInstruction(McuInfo.BootloaderAddress - 1, $0000);                // nop
                   end;
          end;
        end;
      end else
        ShowMessage('Error in calculating the memory pages, ensure the openMikroBootloader MCU info for the bootloader code address is correct');
    end else
      ShowMessage('Bootloader code and Application "main" must be contained in the same flash memory page.  Please see the openMikroBootloader documentation on PIC16F devices for more information');
  end
end;

procedure TPic16FLoader.LoadHexMapInUI(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView);
begin
  inherited LoadHexMapInUI(McuInfo, IntelHexParser, TreeViewHexMap);
end;

procedure TPic16FLoader.IgnoreAddress(McuInfo: TMcuInfo;
  HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups);
var
  Group: TIgnoreBoundsList;
begin
  Group := IgnoreList.AddGroup;
  Group.AddBound($2007, $2007 + HexInfo.AddressIncrement);                      // Config, 2 Bytes smaller devices
  Group.AddBound($8007, $8007 + HexInfo.AddressIncrement);                      // Config, 2 Bytes larger devices
  inherited IgnoreAddress(McuInfo, HexInfo, IgnoreList);
end;

function TPic16FLoader.MainProgramVectorAddressSize: DWord;
begin
  Result := 2
end;

{ TPic30Loader }

procedure TPic30Loader.IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups);
var
  Group: TIgnoreBoundsList;
begin
  Group := IgnoreList.AddGroup;
  Group.AddBound($7FFC00, $F80000 - 2);                                         // EEPROM
  Group := IgnoreList.AddGroup;
  Group.AddBound($F80000, $F80000 + (16 * HexInfo.AddressIncrement));           // Config Bytes
  inherited IgnoreAddress(McuInfo, HexInfo, IgnoreList);
end;

{ TPic33Loader }

procedure TPic33Loader.IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups);
var
  Group: TIgnoreBoundsList;
begin
  Group := IgnoreList.AddGroup;
  Group.AddBound($F80000, $F80000  + (16 * HexInfo.AddressIncrement));          // Config
  inherited IgnoreAddress(McuInfo, HexInfo, IgnoreList);
end;

procedure TPic33Loader.FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser);
var
  TargetAddress: DWord;
  Block: TBaseBlock;
  Instruction0, Instruction1: DWord;
begin

  // Generate a jump to the start of the Bootloader code
  // Write it in the startup area of the bootloaded application so it will first jump to the booloader code
  TargetAddress := PIC33_PROGRAM_FLASH;
  Block := IntelHexParser.PhysicalWriteBlockList.FindBlock(TargetAddress);
  if Assigned(Block) then
  begin
    // Read in the original jump to the application MAIN
    Instruction0 := Block.Read24BitInstruction(TargetAddress);
    Instruction1 := Block.Read24BitInstruction(TargetAddress + 2);

    // Now generate a jump to the bootloader
    Block.Write24BitInstruction(TargetAddress, $00040000 or (McuInfo.BootloaderAddress and $0000FFFF));
    Block.Write24BitInstruction(TargetAddress + 2, (McuInfo.BootloaderAddress shr 16) and $000000FF);

    // Now write a jump from the bootloader to the bootloaded application
    TargetAddress := McuInfo.BootloaderAddress - 4;                             // Backup 2 Instructions (4 Addresses)
    Block := IntelHexParser.PhysicalWriteBlockList.FindBlock(TargetAddress);
    if Assigned(Block) then
    begin
      Block.Write24BitInstruction(TargetAddress, Instruction0);
      Block.Write24BitInstruction(TargetAddress + 2, Instruction1);
    end;
  end;
end;

procedure TPic33Loader.LoadHexMapInUI(McuInfo: TMcuInfo;
  IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView);
var
  i: Integer;
  ProgramFlashNode, ConfigurationNode, FlashNode: TTreeNode;
begin
  FlashNode := TreeViewHexMap.Items.AddChild(nil, 'Mcu Flash');
  ProgramFlashNode := TreeViewHexMap.Items.AddChild(FlashNode, 'Program Flash');
  ConfigurationNode := TreeViewHexMap.Items.AddChild(FlashNode, 'Configuration Flash');

  for i := 0 to IntelHexParser.PhysicalAddressBlockList.Count - 1 do
  begin
    if IntelHexParser.PhysicalAddressBlockList[i].AddressStart >= PIC33_CONFIG_FLASH then
      TreeViewHexMap.Items.AddChildObject(ConfigurationNode, '0x' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8) + ' to ' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressLast, 8), IntelHexParser.PhysicalAddressBlockList[i])
    else
    if IntelHexParser.PhysicalAddressBlockList[i].AddressStart >= PIC33_PROGRAM_FLASH then
      TreeViewHexMap.Items.AddChildObject(ProgramFlashNode, '0x' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8) + ' to ' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressLast, 8), IntelHexParser.PhysicalAddressBlockList[i])
  end;
end;

function TPic33Loader.MainProgramVectorAddressSize: DWord;
begin
  Result:= 4   // 2 instructions * 2 Addresses per instruction
end;

{ TPic32Loader }

procedure TPic32Loader.FixupBootloader(McuInfo: TMcuInfo;IntelHexParser: TIntelHexParser);
var
  JumpOffset, TargetAddress: DWord;
  Block: TBaseBlock;
begin
  // First decide where the program will branch to the Bootloader Start code
  if (PDWord(IntelHexParser.PhysicalWriteBlockList.JumpToInstruction(KVA_TO_PA(PIC32_BOOT_FLASH)))^ = $27BDFFFC) and
     (PDWord(IntelHexParser.PhysicalWriteBlockList.JumpToInstruction(KVA_TO_PA(PIC32_BOOT_FLASH) + 4))^ = $70000000) then
    JumpOffset := $40
  else
    JumpOffset := $00;
  // Generate a jump to the start of the Bootloader code
  // Write it in the startup area of the bootloaded application so it will first jump to the booloader code
  TargetAddress := KVA_TO_PA(PIC32_BOOT_FLASH) + JumpOffset;
  Block := IntelHexParser.PhysicalWriteBlockList.FindBlock(TargetAddress);
  if Assigned(Block) then
  begin
    Block.WriteDWordInstruction(TargetAddress, $3C1E0000 or (McuInfo.BootloaderAddress shr 16));              // lui $30,[addr>>16]
    Block.WriteDWordInstruction(TargetAddress + 4, $37DE0000 or (McuInfo.BootloaderAddress and $0000FFFF));   // ori $30,$30,[addr&0xffff]
    Block.WriteDWordInstruction(TargetAddress + 8, $03C00008);                                                // jr $30
    Block.WriteDWordInstruction(TargetAddress + 12, $70000000);                                               // nop
  end;


  // Generate a jump back to the bootloaded application at the start of the program start code.  Up to $50 is nothing but
  // nops (except for possibly the first 2 address).  The 4 addresses before this are the modified jump to the bootloader code which
  // jumps back to here
  TargetAddress := KVA_TO_PA(McuInfo.BootloaderAddress) - 16;     // 4 Address * 4 Bytes/Address
  Block := IntelHexParser.PhysicalWriteBlockList.FindBlock(TargetAddress);
  if Assigned(Block) then
  begin
    Block.WriteDWordInstruction(TargetAddress, $3C1E0000 or (PA_TO_BOOTFLASH(KVA_TO_PA(PIC32_BOOT_FLASH + $50)) shr 16));              // lui $30,[addr>>16]
    Block.WriteDWordInstruction(TargetAddress + 4, $37DE0000 or (PA_TO_BOOTFLASH(KVA_TO_PA(PIC32_BOOT_FLASH + $50)) and $0000FFFF));   // ori $30,$30,[addr&0xffff]
    Block.WriteDWordInstruction(TargetAddress + 8, $03C00008);                                                        // jr $30
    Block.WriteDWordInstruction(TargetAddress + 12, $70000000);                                                       // nop
  end;
end;

procedure TPic32Loader.LoadHexMapInUI(McuInfo: TMcuInfo;
  IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView);
var
  i: Integer;
  ProgramFlashNode, BootFlashNode, ConfigurationNode, FlashNode: TTreeNode;
begin
  FlashNode := TreeViewHexMap.Items.AddChild(nil, 'Mcu Flash');
  ProgramFlashNode := TreeViewHexMap.Items.AddChild(FlashNode, 'Program Flash');
  BootFlashNode := TreeViewHexMap.Items.AddChild(FlashNode, 'Boot Flash');
  ConfigurationNode := TreeViewHexMap.Items.AddChild(FlashNode, 'Configuration Flash');

  for i := 0 to IntelHexParser.PhysicalAddressBlockList.Count - 1 do
  begin
    if IntelHexParser.PhysicalAddressBlockList[i].AddressStart >= PIC32_CONFIG_FLASH then
      TreeViewHexMap.Items.AddChildObject(ConfigurationNode, '0x' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8) + ' to ' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressLast, 8), IntelHexParser.PhysicalAddressBlockList[i])
    else
    if IntelHexParser.PhysicalAddressBlockList[i].AddressStart >= PIC32_BOOT_FLASH then
      TreeViewHexMap.Items.AddChildObject(BootFlashNode, '0x' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8)+ ' to ' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8), IntelHexParser.PhysicalAddressBlockList[i])
    else
    if IntelHexParser.PhysicalAddressBlockList[i].AddressStart >= PIC32_PROGRAM_FLASH then
      TreeViewHexMap.Items.AddChildObject(ProgramFlashNode, '0x' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8) + ' to ' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8), IntelHexParser.PhysicalAddressBlockList[i])
  end;
end;

function TPic32Loader.MainProgramVectorAddressSize: DWord;
begin
  Result := 16;          // 4 instruction * 4 instructions per address
end;

function TPic32Loader.AddressToPhysicalAddress(Address: DWord): DWord;
begin
  Result := KVA_TO_PA(Address);
end;

procedure TPic32Loader.IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups);
var
  Group: TIgnoreBoundsList;
begin
  Group := IgnoreList.AddGroup;
  Group.AddBound($1FC02FF0, $1FC02FF0  + (16 * HexInfo.AddressIncrement));       // Config Bytes
  McuInfo.BootloaderAddress := KVA_TO_PA(McuInfo.BootloaderAddress);
  inherited IgnoreAddress(McuInfo, HexInfo, IgnoreList);
end;

{ TMcuLoaderBase }

procedure TMcuLoaderBase.FixupBootloader(McuInfo: TMcuInfo; IntelHexParser: TIntelHexParser);
begin

end;

procedure TMcuLoaderBase.LoadHexMapInUI(McuInfo: TMcuInfo;
  IntelHexParser: TIntelHexParser; TreeViewHexMap: TTreeView);
var
  i: Integer;
  FlashNode: TTreeNode;
begin
  FlashNode := TreeViewHexMap.Items.AddChild(nil, 'Program Flash');
  for i := 0 to IntelHexParser.PhysicalAddressBlockList.Count - 1 do
    TreeViewHexMap.Items.AddChildObject(FlashNode, '0x' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressStart, 8) + '  to ' + IntToHex( IntelHexParser.PhysicalAddressBlockList[i].AddressLast, 8), IntelHexParser.PhysicalAddressBlockList[i])
end;

function TMcuLoaderBase.MainProgramVectorAddressSize: DWord;
begin
  Result := 0;
end;

procedure TMcuLoaderBase.AddMainProgramVectorCustomAddress(McuInfo: TMcuInfo; BlockList: TPhysicalAddressBlockList);
var
  Block: TBaseBlock;
  TargetAddress: DWord;
  AddressesPerEraseBlock: DWord;
begin
  TargetAddress := AddressToPhysicalAddress(McuInfo.BootloaderAddress - MainProgramVectorAddressSize);
  Block := BlockList.FindBlock(TargetAddress);
  if Assigned(Block) then
    BlockList.DeleteBlock(Block);
  // Make sure the block is aligned correctly with the BootLoader address, PIC16 needs to write the same
  // number of bytes it erases to align with EraseBlock Size
  AddressesPerEraseBlock := McuInfo.EraseBlockSize div BlockList.BytesPerInstruction * BlockList.AddressIncrement;
  TargetAddress := AddressToPhysicalAddress(McuInfo.BootloaderAddress - AddressesPerEraseBlock);
  if TargetAddress <> TargetAddress div AddressesPerEraseBlock * AddressesPerEraseBlock then
    ShowMessage('Bootloader Address not aligned on a Erase/Write Boundry Address')
  else
    Block := BlockList.AddBlankBlock(TargetAddress, AddressesPerEraseBlock, $FF);
end;

function TMcuLoaderBase.AddressToPhysicalAddress(Address: DWord): DWord;
begin
  Result := Address
end;

procedure TMcuLoaderBase.CustomAddress(McuInfo: TMcuInfo; BlockList: TPhysicalAddressBlockList);
begin
  AddMainProgramVectorCustomAddress(McuInfo, BlockList);
end;

procedure TMcuLoaderBase.IgnoreAddress(McuInfo: TMcuInfo; HexInfo: TIntelHexInfo; IgnoreList: TIgnoreBoundsGroups);
var
  Group: TIgnoreBoundsList;
begin
  Group := IgnoreList.AddGroup;
  Group.AddBound(McuInfo.BootloaderAddress, McuInfo.BootloaderAddress + (McuInfo.BootloaderSize div HexInfo.BytesPerInstruction * HexInfo.AddressIncrement));
end;

end.

