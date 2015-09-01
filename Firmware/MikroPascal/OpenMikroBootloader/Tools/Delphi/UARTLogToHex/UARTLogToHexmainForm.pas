unit UARTLogToHexmainForm;

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
// The initial developer of this code is Vcc
//

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmUARTLogToHex = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    memPIC32UARTCode: TMemo;
    lblExampleCode: TLabel;
    lstUARTLogVariant1: TListBox;
    lstDecodedVariant1: TListBox;
    lblExplanationVariant2: TLabel;
    OpenDialog1: TOpenDialog;
    btnLoadLogVariant1: TButton;
    btnDecodeVariant1: TButton;
    lblFormatInfo: TLabel;
    btnSaveDecodedToFileVariant1: TButton;
    SaveDialog1: TSaveDialog;
    procedure btnLoadLogVariant1Click(Sender: TObject);
    procedure btnDecodeVariant1Click(Sender: TObject);
    procedure btnSaveDecodedToFileVariant1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUARTLogToHex: TfrmUARTLogToHex;

implementation

{$R *.dfm}

{This function replaces occurences of #13 with #13#10 and #10 with #13#10.
Such replacements are neccessary since Windows applications uses #13#10 for a line return.
#13 = CR
#10 = LF
}
function ReplaceCRLF_Problems(s: string): string;
var
  i: Integer;
begin
  if Length(s) = 0 then
  begin
    Result := s;
    Exit;
  end;

  if Length(s) = 1 then
  begin
    if s = #13 then
      Result := #13#10
    else
      if s = #10 then
        Result := #13#10;

    Exit;    
  end;

  i := 2;
  repeat
    if (s[i] = #13) and (s[i + 1] <> #10) then
    begin
      Insert(#10, s, i + 1);
      //Continue;
    end
    else
      if (s[i] = #10) and (s[i - 1] <> #13) then
      begin
        Insert(#13, s, i - 0);
        //Continue;
      end;

    Inc(i);
  until i >= Length(s) - 1; //from second to last - 1
  Result := s;
end;

function ReplaceDouble_CRLF_WithSingle(s: string): string;
begin
  while Pos(#13#10#13#10, s) > 0 do
    Delete(s, Pos(#13#10#13#10, s), 2);
  Result := s;  
end;

procedure TfrmUARTLogToHex.btnDecodeVariant1Click(Sender: TObject);
var
  i: Integer;
  n: Integer;  //number of entries
  s: string; //original list
  s1: string; //decoded list
  AddrString, DataString: string;
begin
  if lstUARTLogVariant1.Count = 0 then
    btnLoadLogVariant1Click(nil);

  if Pos('UART Log - Variant 1', lstUARTLogVariant1.Items.Strings[0]) = 0 then
    Exit;

  lstDecodedVariant1.Clear;  

  n := 0;
  for i := 0 to lstUARTLogVariant1.Count - 1 do
  begin
    s := lstUARTLogVariant1.Items.Strings[i];
    if Pos('FshWrWord addr = ', s) = 1 then
    begin
      AddrString := Copy(s, Length('FshWrWord addr = ') + 1, 8); //8 characters
      if Pos('1D', AddrString) = 1 then //ProgramFlash
        AddrString := '0x9' + Copy(AddrString, 2, 7) + ' / 0x' + AddrString;       //0x9D000000 / 0x1D000000 format

      if Pos('1F', AddrString) = 1 then //BootFlash
        AddrString := '0xB' + Copy(AddrString, 2, 7) + ' / 0x' + AddrString;       //0xBFC00000 / 0x1FC00000 format  
    end;  

    if Pos('FshWrWord data =', s) = 1 then
      DataString := Copy(s, Length('FshWrWord data = ') + 1, 8); //8 characters
      
    if s = '...' then
    begin
      s1 := AddrString + '  ' + DataString + '     ByteCount = 16 Data' + IntToStr((n mod 4) + 1);
      lstDecodedVariant1.Items.Add(s1);
      Inc(n);
    end;
  end;
end;

procedure TfrmUARTLogToHex.btnLoadLogVariant1Click(Sender: TObject);
var
  AStringList: TStringList;
begin
  if not OpenDialog1.Execute then
    Exit;

  AStringList := TStringList.Create;
  try
    AStringList.LoadFromFile(OpenDialog1.FileName);                          //if for some reason there extra CR or LF characters,
    lstUARTLogVariant1.Items.Text := ReplaceCRLF_Problems(AStringList.Text); //we need to fix this to avoid extra lines in the list
    lstUARTLogVariant1.Items.Text := ReplaceDouble_CRLF_WithSingle(lstUARTLogVariant1.Items.Text); //no replace double lines with single lines
  finally
    AStringList.Free;
  end;
end;

procedure TfrmUARTLogToHex.btnSaveDecodedToFileVariant1Click(Sender: TObject);
var
  Fnm: string;
begin
  if not SaveDialog1.Execute then
    Exit;

  Fnm := SaveDialog1.FileName;
  if Pos('.TXT', UpperCase(Fnm)) <> Length(Fnm) - 3 then
    Fnm := Fnm + '.txt';
    
  lstDecodedVariant1.Items.SaveToFile(Fnm);
end;

end.
