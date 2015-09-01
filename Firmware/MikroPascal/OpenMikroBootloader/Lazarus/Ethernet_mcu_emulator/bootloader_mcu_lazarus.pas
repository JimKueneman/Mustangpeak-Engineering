unit bootloader_mcu_lazarus;

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
  Classes, SysUtils, FileUtil, file_utilities,
  {$ENDIF}
  bootloader_user,
  bootloader_common;

{$IFDEF FPC}
const
  PATH_MEMMAP_FILE = 'memmap.ini';
{$ENDIF}

procedure Bootloader_Mcu_Initialization;
procedure Bootloader_Mcu_EraseFlash(Address: DWord);
procedure Bootloader_Mcu_WriteFlash(Address: DWord; Count: DWord; DataPtr: PFlashDataPtr);

implementation

var
  SettingsFilePath: string;
  MemoryMap: TMemoryStream;


procedure Bootloader_Mcu_Initialization;
begin
  MemoryMap := TMemoryStream.Create;
  MemoryMap.Size := BOOTLOADER_FLASH_SIZE;
  {$IFDEF Linux}
    SettingsFilePath:= GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE;
    GlobalSettings.LoadFromFile(UTF8ToSys( GetSettingsPath + {PATH_LINUX_APP_FOLDER +} PATH_SETTINGS_FILE));
  {$ELSE}
    SettingsFilePath := GetSettingsPath + PATH_MEMMAP_FILE;
    {$IFDEF FPC}
      if FileExistsUTF8(GetSettingsPath + PATH_MEMMAP_FILE) then
        MemoryMap.LoadFromFile(UTF8ToSys( GetSettingsPath + PATH_MEMMAP_FILE));
    {$ELSE}
      GlobalSettings.LoadFromFile(GetSettingsPath + PATH_SETTINGS_FILE);
    {$ENDIF}
  {$ENDIF}
end;

procedure Bootloader_Mcu_EraseFlash(Address: DWord);
var
  i: Integer;
begin
  MemoryMap.Position := Address;
  for i := 0 to BOOTLOADER_ERASE_BLOCK - 1 do
    MemoryMap.WriteByte($00);
end;

procedure Bootloader_Mcu_WriteFlash(Address: DWord; Count: DWord; DataPtr: PFlashDataPtr);
var
  i: Integer;
begin
  MemoryMap.Position := Address;
  for i := 0 to Count - 1 do
    MemoryMap.WriteDWord(DataPtr^);
end;


end.

