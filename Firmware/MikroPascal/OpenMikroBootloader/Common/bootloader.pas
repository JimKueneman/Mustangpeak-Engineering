unit bootloader;

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
  Classes, SysUtils;

type

  { THexContent }

  THexEntry = record
    HAddr: DWord;
    HData: DWord;
  end;

  THexContent = array of THexEntry;

function BootloaderParseAndValidate(FileName: string): Boolean;

implementation

const
  CAddressOffset_Data1: DWord = 0;
  CAddressOffset_Data2: DWord = 4;
  CAddressOffset_Data3: DWord = 8;
  CAddressOffset_Data4: DWord = 12;

function BootloaderParseAndValidate(FileName: string): Boolean;
begin
  Result := True;
  //call here LoadPIC32HEXFile (see below)
end;


function HexaDigitToByte(ch: Char): Byte;
begin
  Result := 0;
  Ch := UpCase(Ch);
  if Ch in ['0'..'9'] then
    Result := Ord(Ch) - 48;

  if Ch in ['A'..'F'] then
    Result := Ord(Ch) - 65 + 10;
end;

function Pow16(x: Byte): Cardinal;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result * 16;
end;

function HexToInt(s: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16(Length(s) - i);
end;

{Converts from Byte3:Byte2:Byte1:Byte0 to Byte0:Byte1:Byte2:Byte3
}
function Swap4BytesHex(Number: DWord): DWord;
begin
  Result := (Number and $000000FF) shl 24 + (Number and $0000FF00) shl 8 + (Number and $00FF0000) shr 8 + (Number and $FF000000) shr 24;
end;

{PhysicalAddrToKseg - Converts from PHY address to KSEG address.
Program Flash addresses are converted to KSEG0.
RAM addresses are converted to KSEG1.
Boot Flash are converted to KSEG1.

Requirements: CurrentAddressKSEGStr should not contain characters like '$' or 'x'
}
procedure PhysicalAddrToKseg(var CurrentAddressKSEGStr: string);
begin
  if (CurrentAddressKSEGStr[1] = '0') and (CurrentAddressKSEGStr[2] = '0') then   //RAM
    CurrentAddressKSEGStr[1] := 'A';

  if (CurrentAddressKSEGStr[1] = '1') and (CurrentAddressKSEGStr[2] = 'D') then   //Program Flash
    CurrentAddressKSEGStr[1] := '9';

  if (CurrentAddressKSEGStr[1] = '1') and (CurrentAddressKSEGStr[2] = 'F') and (CurrentAddressKSEGStr[3] = 'C') then  //Bootflash
    CurrentAddressKSEGStr[1] := 'B';

  if (CurrentAddressKSEGStr[1] = '2') and (CurrentAddressKSEGStr[2] in ['0'..'3']) then   //EBI (PIC32MZ only)
    CurrentAddressKSEGStr[1] := 'C';

  if (CurrentAddressKSEGStr[1] = '3') and (CurrentAddressKSEGStr[2] in ['0'..'3']) then   //SQI (PIC32MZ only)
    CurrentAddressKSEGStr[1] := 'D';
end;


{LoadPIC32HEXFile - Loads a hex file from disk and decodes it into addresses and data.
Limitations: Function does not check for hex file integrity.
             Function assumes the hex file exists and is readable.

Returns true if successful.

Parameters:
FileName - .HEX file.
ConvertPHYToKSEG - if true, the physical addresses read from hex file are converted to virtual addresses.
DecodedHEX - content of the decoded hex file.
DebugMessage - contains an error message in case of error.

Details about hex file format: http://en.wikipedia.org/wiki/Intel_HEX
}
function LoadPIC32HEXFile(FileName: string; ConvertPHYToKSEG: Boolean; var DecodedHEX: THexContent; var DebugMessages: string): Boolean;
var
  i: Integer;
  ByteCount: Word;
  AddrWord: Word;   //read from hex
  CurrentAddress: Cardinal; //computed address
  BaseAddress: Cardinal;
  RecordType: Byte;
  Data: Cardinal; //string read from hex
  Data1: Cardinal;
  Data2: Cardinal;
  Data3: Cardinal;
  Data4: Cardinal;
  s, Field: string;
  Data2Found, Data3Found, Data4Found: Boolean;

  lstHexFile: TStringList;
  ResultMessages: TStringList;
begin
  Result := True;
  lstHexFile := TStringList.Create;
  ResultMessages := TStringList.Create;
  SetLength(DecodedHEX, 0); //or Finalize ?

  try
    BaseAddress := 0;
    lstHexFile.LoadFromFile(FileName);

    for i := 0 to lstHexFile.Count - 1 do
    begin
      s := lstHexFile.Strings[i];

      Field := Copy(s, 2, 2);
      ByteCount := HexToInt(Field);

      Field := Copy(s, 4, 4);
      AddrWord := HexToInt(Field);

      Field := Copy(s, 8, 2);
      RecordType := HexToInt(Field);

      Field := Copy(s, 10, ByteCount * 2);
      Data := 0;
      Data1 := 0;
      Data2 := 0;
      Data3 := 0;
      Data4 := 0;

      Data2Found := False;
      Data3Found := False;
      Data4Found := False;

      if RecordType = 0 then
      begin
        Data := HexToInt(Copy(Field, 1, 8));

        Data1 := HexToInt(Copy(Field, 1, 8));
          Delete(Field, 1, 8);

          if ByteCount > 4 then
          begin
            Data2 := HexToInt(Copy(Field, 1, 8));
            Delete(Field, 1, 8);
            Data2Found := True;
          end;

          if ByteCount > 8 then
          begin
            Data3 := HexToInt(Copy(Field, 1, 8));
            Delete(Field, 1, 8);
            Data3Found := True;
          end;

          if ByteCount > 12 then
          begin
            Data4 := HexToInt(Copy(Field, 1, 8));
            Delete(Field, 1, 8);
            Data4Found := True;
          end;
      end;

      if (RecordType = 2) or (RecordType = 4) then
        Data := HexToInt(Field);

      case RecordType of
        0:
        begin
          CurrentAddress := BaseAddress + AddrWord;

          SetLength(DecodedHEX, Length(DecodedHEX) + 1);
          DecodedHEX[Length(DecodedHEX) - 1].HAddr := CurrentAddress + CAddressOffset_Data1;
          DecodedHEX[Length(DecodedHEX) - 1].HData := Swap4BytesHex(Data1);

          if Data2Found then
          begin
            SetLength(DecodedHEX, Length(DecodedHEX) + 1);
            DecodedHEX[Length(DecodedHEX) - 1].HAddr := CurrentAddress + CAddressOffset_Data2;
            DecodedHEX[Length(DecodedHEX) - 1].HData := Swap4BytesHex(Data2);
          end;

          if Data3Found then
          begin
            SetLength(DecodedHEX, Length(DecodedHEX) + 1);
            DecodedHEX[Length(DecodedHEX) - 1].HAddr := CurrentAddress + CAddressOffset_Data3;
            DecodedHEX[Length(DecodedHEX) - 1].HData := Swap4BytesHex(Data3);
          end;

          if Data4Found then
          begin
            SetLength(DecodedHEX, Length(DecodedHEX) + 1);
            DecodedHEX[Length(DecodedHEX) - 1].HAddr := CurrentAddress + CAddressOffset_Data4;
            DecodedHEX[Length(DecodedHEX) - 1].HData := Swap4BytesHex(Data4);
          end;
        end;
        1: ResultMessages.Add('End of file');
        2:
        begin
          BaseAddress := Data * 16;
          ResultMessages.Add('Record type 2 encountered.');
        end;
        3:
        begin
          ResultMessages.Add('Record type 3 is not handled.');
          Result := False;
        end;
        4: BaseAddress := Data * 65536;
        5:
        begin
          ResultMessages.Add('Record type 4 is not handled.');
          Result := False;
        end;
      end; //case RecordType of
    end; //for i

    //convert to KSEG
    if ConvertPHYToKSEG then
      for i := 0 to Length(DecodedHEX) - 1 do
      begin
        s := IntToHex(DecodedHEX[i].HAddr, 8);
        PhysicalAddrToKseg(s);
        DecodedHEX[i].HAddr := HexToInt(s);
      end;
    //
    DebugMessages := ResultMessages.Text; //messages separated by CR+LF or LF depending on OS
  finally
    lstHexFile.Free;
    ResultMessages.Free;
  end;
end;

//LoadPIC32HEXFile test example:
{
procedure TForm1.Button1Click(Sender: TObject);
var
  DecodedHEX: THexContent;
  Msgs: string;
  i: Integer;
begin
  if not OpenDialog1.Execute then
    Exit;

  Memo1.Clear;
  Memo1.Hide;
  LoadPIC32HEXFile(OpenDialog1.FileName, True, DecodedHEX, Msgs);
  for i := 0 to Length(DecodedHEX) - 1 do
    Memo1.Lines.Add('0x' + IntToHex(DecodedHEX[i].HAddr, 8) + '  0x' + IntToHex(DecodedHEX[i].HData, 8));

  Memo1.Show;
end;
}
//////////////////////

end.

