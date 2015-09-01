unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, IniFiles,
  Dialogs, StdCtrls, WinSock, ExtCtrls, ComCtrls, NMUDP;

type
  TForm1 = class(TForm)
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Edit1: TEdit;
    Button1: TButton;
    StatusBar1: TStatusBar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    ProgressBar1: TProgressBar;
    procedure Button3Click(Sender: TObject);
    procedure FileSearch();
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
     ApplicationName = 'HexToBinPIC24FJ';

CONST
  Table: ARRAY[0..255] OF WORD =
 ($0000,$C0C1,$C181,$0140,$C301,$03C0,$0280,$C241,$C601,$06C0,$0780,
  $C741,$0500,$C5C1,$C481,$0440,$CC01,$0CC0,$0D80,$CD41,$0F00,$CFC1,
  $CE81,$0E40,$0A00,$CAC1,$CB81,$0B40,$C901,$09C0,$0880,$C841,$D801,
  $18C0,$1980,$D941,$1B00,$DBC1,$DA81,$1A40,$1E00,$DEC1,$DF81,$1F40,
  $DD01,$1DC0,$1C80,$DC41,$1400,$D4C1,$D581,$1540,$D701,$17C0,$1680,
  $D641,$D201,$12C0,$1380,$D341,$1100,$D1C1,$D081,$1040,$F001,$30C0,
  $3180,$F141,$3300,$F3C1,$F281,$3240,$3600,$F6C1,$F781,$3740,$F501,
  $35C0,$3480,$F441,$3C00,$FCC1,$FD81,$3D40,$FF01,$3FC0,$3E80,$FE41,
  $FA01,$3AC0,$3B80,$FB41,$3900,$F9C1,$F881,$3840,$2800,$E8C1,$E981,
  $2940,$EB01,$2BC0,$2A80,$EA41,$EE01,$2EC0,$2F80,$EF41,$2D00,$EDC1,
  $EC81,$2C40,$E401,$24C0,$2580,$E541,$2700,$E7C1,$E681,$2640,$2200,
  $E2C1,$E381,$2340,$E101,$21C0,$2080,$E041,$A001,$60C0,$6180,$A141,
  $6300,$A3C1,$A281,$6240,$6600,$A6C1,$A781,$6740,$A501,$65C0,$6480,
  $A441,$6C00,$ACC1,$AD81,$6D40,$AF01,$6FC0,$6E80,$AE41,$AA01,$6AC0,
  $6B80,$AB41,$6900,$A9C1,$A881,$6840,$7800,$B8C1,$B981,$7940,$BB01,
  $7BC0,$7A80,$BA41,$BE01,$7EC0,$7F80,$BF41,$7D00,$BDC1,$BC81,$7C40,
  $B401,$74C0,$7580,$B541,$7700,$B7C1,$B681,$7640,$7200,$B2C1,$B381,
  $7340,$B101,$71C0,$7080,$B041,$5000,$90C1,$9181,$5140,$9301,$53C0,
  $5280,$9241,$9601,$56C0,$5780,$9741,$5500,$95C1,$9481,$5440,$9C01,
  $5CC0,$5D80,$9D41,$5F00,$9FC1,$9E81,$5E40,$5A00,$9AC1,$9B81,$5B40,
  $9901,$59C0,$5880,$9841,$8801,$48C0,$4980,$8941,$4B00,$8BC1,$8A81,
  $4A40,$4E00,$8EC1,$8F81,$4F40,$8D01,$4DC0,$4C80,$8C41,$4400,$84C1,
  $8581,$4540,$8701,$47C0,$4680,$8641,$8201,$42C0,$4380,$8341,$4100,
  $81C1,$8081,$4040);

type
  TSetup = record
     LastFile : string;
     Port  : string;
     Host  : string;
  end;

var
  Form1: TForm1;
  res : byte;
  Directory: string;
  IniFile: TIniFile;
  Setup: TSetup;
  addr_o,addr_n,final_address : Cardinal;
  buffer,buffer1 : array[0..499999] of byte;
  fisier : Textfile;
  fisier1 : File of byte;
  data1 : byte;
  Flag1,Flag2,flag5 : boolean;
  Upper_address: word;
  Nr_Line : word;

implementation

{$R *.dfm}

procedure TForm1.Button3Click(Sender: TObject);
var i : Word;
    Data: string;
    Code, II, JJ,max_data : integer;
    Address,CRC,addr1,addr2 : Cardinal;
    dat : byte;
    flag3 : boolean;
begin
  ProgressBar1.Position := 0;
  StatusBar1.SimpleText := 'Start!';
  if CheckBox1.Checked = true then addr1 := (($5800 div 2) * 3) - 1
  else if CheckBox2.Checked = true then addr1 := (($AC00 div 2) * 3) - 1
  else if CheckBox3.Checked = true then addr1 := (($10000 div 2) * 3) - 1
  else if CheckBox4.Checked = true then addr1 := (($15800 div 2) * 3) - 1
  else addr1 := (($2B000 div 2) * 3) - 1;
  for Address := 0 to 499999 do buffer[Address] := $FF;
  AssignFile(fisier, Setup.LastFile);
  Reset(fisier);
  Nr_line := 0;
  Upper_address := 0;
  flag3 := true;
  while not EOF(fisier) do
    begin
       Readln(fisier, Data);
       if flag3 = true then
       begin
       if (Length(Data) <> 0) and (Data[1] = ':') then
         begin
            Nr_Line := Nr_Line + 1;
            Val('$'+Data[4]+Data[5], II, Code);
            Val('$'+Data[6]+Data[7], JJ, Code);
            Address := ((Upper_address*65536)+(II*256) + JJ);
            if (Data[8] = '0') and (Data[9] = '0') and (Address <= ($FFFF + Upper_address*65536)) then
              begin
                Val('$'+Data[2]+Data[3], II, Code);
                if II <> 0 then
                  begin
                    max_data := II;
                    for i := 0 to max_data -1 do
                      begin
                        Val('$'+Data[10+(i*2)]+Data[11+(i*2)], II, Code);
                        buffer[Address+i]:=II;
                      end;
                  end;
              end
              else if (Data[8] = '0') and (Data[9] = '4') then
                begin
                  Val('$'+Data[10]+Data[11], II, Code);
                  Val('$'+Data[12]+Data[13], JJ, Code);
                  if II+jj = 0 then Upper_address := 0
                  else if II+jj = 1 then Upper_address := 1
                  else if II+jj = 2 then Upper_address := 2
                  else if II+jj = 3 then Upper_address := 3
                  else if II+jj = 4 then Upper_address := 4
                  else if II+jj = 5 then Upper_address := 5
                  else flag3 := false;
                end;
        end;
     end;
  end;
  CloseFile(fisier);
  
  address := 0;
  addr2 := 0;
  while true do
    begin
      buffer1[addr2] := buffer[Address];
      inc(Address);
      inc(addr2);
      if addr2 >= addr1 then break;
      buffer1[addr2] := buffer[Address];
      inc(Address);
      inc(addr2);
      if addr2 >= addr1 then break;
      buffer1[addr2] := buffer[Address];
      inc(Address);
      inc(addr2);
      if addr2 >= addr1 then break;
      inc(Address);
    end;

  AssignFile(fisier1, 'firmware.bin');
  ReWrite(fisier1);
  CRC := $FFFF;
  for Address := 0 to (addr1 - 2) do
    begin
      ProgressBar1.Position := (Address*100) div (addr1 - 2);
      data := '   ';
      data := data + IntToStr(ProgressBar1.Position);
      data := data + ' %    ';
      StatusBar1.SimpleText := data;
      dat := buffer1[Address];
      write(fisier1,dat);
      CRC := Hi(CRC) XOR Table[dat XOR Lo(CRC)];
    end;
  dat := Hi(CRC);
  write(fisier1,dat);
  dat := Lo(CRC);
  write(fisier1,dat);
  
  CloseFile(fisier1);

  IniFile.WriteString('Setup','File',Setup.LastFile);
  StatusBar1.SimpleText := 'Done!';
  Beep;
end;

procedure TForm1.FileSearch();
begin
  StatusBar1.SimpleText := '                ';
  OpenDialog1.Filter := 'HEX file (*.hex) | *.hex';
  OpenDialog1.InitialDir := ExtractFileDir (Setup.LastFile);
  OpenDialog1.FileName := ExtractFileName (Setup.LastFile);
  if OpenDialog1.Execute then
  try
    Setup.LastFile := OpenDialog1.FileName;
    Edit1.Text := ExtractFileName (Setup.LastFile);
  except
    Application.MessageBox('File read error !', ApplicationName, MB_OK);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  IniValueList: TStringList;
  S: string;
begin
  Form1.Caption := ApplicationName;
  Directory := GetCurrentDir;
  if (Length(Directory) > 3) then
  begin
    Directory := Directory + '\';
  end;
  if FileExists(Directory+'HexToBin.ini') then
  begin
    IniFile := TIniFile.Create (Directory+'HexToBin.ini');
    IniValueList := TStringList.Create;
    try
      IniFile.ReadSectionValues('Setup', IniValueList);
      Setup.LastFile := IniValueList.Values['File'];
    finally
      IniValueList.Free;
    end;
  end
  else
  begin
    IniFile := TIniFile.Create (Directory+'HexToBin.ini');  // ini file doesn't exist
    Setup.LastFile := Directory;
  end;

  S := ParamStr(1);
  if S <> '' then
  Setup.LastFile := S;
  Edit1.Text := ExtractFileName(Setup.LastFile);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FileSearch();
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
  CheckBox5.Checked := false;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  CheckBox1.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
  CheckBox5.Checked := false;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  CheckBox1.Checked := false;
  CheckBox2.Checked := false;
  CheckBox4.Checked := false;
  CheckBox5.Checked := false;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  CheckBox1.Checked := false;
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox5.Checked := false;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  CheckBox1.Checked := false;
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
end;

end.
