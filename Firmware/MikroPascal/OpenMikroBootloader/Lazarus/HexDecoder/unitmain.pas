unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Spin, KGrids, KLabels, KEdits, types, intel_hex_parser,
  form_notes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBoxIgnoreAddresses: TCheckBox;
    CheckBoxPivot: TCheckBox;
    CheckBoxPack: TCheckBox;
    CheckBoxSort: TCheckBox;
    EditWriteBuffer: TEdit;
    KFileNameEdit: TKFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog: TOpenDialog;
    PanelGrids: TPanel;
    PanelOptions: TPanel;
    PanelMain: TPanel;
    RadioGroupEraseBlock: TRadioGroup;
    RadioGroupWriteBlock: TRadioGroup;
    RadioGroupMCU: TRadioGroup;
    SpinEditWriteBufferMulitiplier: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StringGrid: TStringGrid;
    StringGridErase: TStringGrid;
    StringGridWrite: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure CheckBoxIgnoreAddressesChange(Sender: TObject);
    procedure CheckBoxPackChange(Sender: TObject);
    procedure CheckBoxPivotChange(Sender: TObject);
    procedure CheckBoxSortChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure KFileNameEditChange(Sender: TObject);
    procedure RadioGroupEraseBlockClick(Sender: TObject);
    procedure RadioGroupFormatClick(Sender: TObject);
    procedure RadioGroupMCUClick(Sender: TObject);
    procedure RadioGroupWriteBlockClick(Sender: TObject);
    procedure SpinEditWriteBufferMulitiplierChange(Sender: TObject);
  private
    FHexParser: TIntelHexParser;
    { private declarations }
  protected
    procedure DoParse;
    procedure DoUpdateWriteBufferMultiplier;
    function RadioToEraseBlock: DWord;
    function RadioToWriteBlock: DWord;
  public
    { public declarations }
    property HexParser: TIntelHexParser read FHexParser write FHexParser;
    procedure Parse(Sorted: Boolean; Pack: Boolean; Pivot: Boolean; HexInfo: TIntelHexInfo; AnEraseBlock, AWriteBlock, AWriteBuffer: DWord);
    function McuToHexInfo: TIntelHexInfo;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  FormNotes.ShowModal;
end;

procedure TForm1.CheckBoxIgnoreAddressesChange(Sender: TObject);
begin
  DoParse;
end;

procedure TForm1.CheckBoxPackChange(Sender: TObject);
begin
  DoParse;
end;

procedure TForm1.CheckBoxPivotChange(Sender: TObject);
begin
  DoParse;
end;

procedure TForm1.CheckBoxSortChange(Sender: TObject);
begin
  DoParse;
end;

procedure TForm1.DoParse;
begin
  Parse(CheckBoxSort.Checked, CheckBoxPack.Checked, CheckBoxPivot.Checked, McuToHexInfo, RadioToEraseBlock, RadioToWriteBlock, StrToInt(EditWriteBuffer.Text));
end;

procedure TForm1.DoUpdateWriteBufferMultiplier;
begin
  EditWriteBuffer.Text := IntToStr(RadioToWriteBlock * SpinEditWriteBufferMulitiplier.Value);
end;

function TForm1.RadioToEraseBlock: DWord;
begin
  case RadioGroupEraseBlock.ItemIndex of
    0 : Result := 2;
    1 : Result := 4;
    2 : Result := 8;
    3 : Result := 16;
    4 : Result := 32;
    5 : Result := 64;
    6 : Result := 128;
    7 : Result := 256;
    8 : Result := 512;
    9 : Result := 1024;
    10 : Result := 2048;
    11 : Result := 4096;
    12 : Result := 96; // dsPIC30
    13 : Result := 1536; // dsPIC33 PIC24
  end;
end;

function TForm1.RadioToWriteBlock: DWord;
begin
   case RadioGroupWriteBlock.ItemIndex of
     0 : Result := 2;
     1 : Result := 4;
     2 : Result := 8;
     3 : Result := 16;
     4 : Result := 32;
     5 : Result := 64;
     6 : Result := 128;
     7 : Result := 256;
     8 : Result := 512;
     9 : Result := 1024;
     10 : Result := 2048;
     11 : Result := 4096;
     12 : Result := 12;     // dsPIC30
     13 : Result := 192;    // dsPIC33 PIC24
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FHexParser := TIntelHexParser.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHexParser)
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  DoUpdateWriteBufferMultiplier;
end;

procedure TForm1.KFileNameEditChange(Sender: TObject);
begin
  DoParse;
end;

function TForm1.McuToHexInfo: TIntelHexInfo;
begin
  case RadioGroupMCU.ItemIndex of
    0 : begin
          Result.HexType := iht_INHX8;     // 1:1
          Result.AddressIncrement := 1;
          Result.BytesPerInstruction := 1;
          Result.DoubleAddress := False;
        end;
    1 : begin
          Result.HexType := iht_INHX8M;    // PIC16
          Result.AddressIncrement := 1;
          Result.BytesPerInstruction := 2;
          Result.DoubleAddress := True;
        end;
    2 : begin
          Result.HexType := iht_INHX8M;    // PIC18
          Result.AddressIncrement := 2;
          Result.BytesPerInstruction := 2;
          Result.DoubleAddress := False;
        end;
    3,                                    // PIC24
    4,                                    // dsPIC30
    5 : begin                             // dsPIC33
          Result.HexType := iht_INHX32;
          Result.AddressIncrement := 2;
          Result.BytesPerInstruction := 3;
          Result.DoubleAddress := True;
        end;
    6 : begin
          Result.HexType := iht_INHX32;    // PIC32
          Result.AddressIncrement := 4;
          Result.BytesPerInstruction := 4;
          Result.DoubleAddress := False;
        end;
  end;
end;

procedure TForm1.Parse(Sorted: Boolean; Pack: Boolean; Pivot: Boolean;
  HexInfo: TIntelHexInfo; AnEraseBlock, AWriteBlock, AWriteBuffer: DWord);
type
  PByte = ^Byte;
  PWord = ^Word;
  PDWord = ^DWord;
var
  Row, iColumn, iBlock: Integer;
  NextAddress, FixedAddressCount: DWord;
  AddressBlock: TPhysicalAddressBlock;
  EraseBlock: TPhysicalEraseBlock;
  WriteBlock: TPhysicalWriteBlock;
  InstructionPtr: PByte;
  IgnoreAddresses: TIgnoreBoundsArray;
begin
  HexParser.Sorted := Sorted;
  HexParser.Pack := Pack;

  IgnoreAddresses := nil;
  if CheckBoxIgnoreAddresses.Checked then
  begin
    case RadioGroupMCU.ItemIndex of
      0 : begin
            SetLength(IgnoreAddresses, 1);
            IgnoreAddresses[0].LoBound := $FFFFFFFF;
            IgnoreAddresses[0].HiBound := $FFFFFFFF;
          end;
      1 : begin
            SetLength(IgnoreAddresses, 1);
            IgnoreAddresses[0].LoBound := $FFFFFFFF;
            IgnoreAddresses[0].HiBound := $FFFFFFFF;
          end;
      2 : begin
            SetLength(IgnoreAddresses, 1);
            IgnoreAddresses[0].LoBound := $FFFFFFFF;
            IgnoreAddresses[0].HiBound := $FFFFFFFF;
          end;
      3 : begin
            SetLength(IgnoreAddresses, 1);
            IgnoreAddresses[0].LoBound := $FFFFFFFF;
            IgnoreAddresses[0].HiBound := $FFFFFFFF;
          end;
      4 : begin
            SetLength(IgnoreAddresses, 2);
            IgnoreAddresses[0].LoBound := $7FFC00;
            IgnoreAddresses[0].HiBound := $F80000   - (1 * HexInfo.AddressIncrement);     // EEPROM
            IgnoreAddresses[1].LoBound := $F80000;
            IgnoreAddresses[1].HiBound := $F80000   + (12 * HexInfo.AddressIncrement);
          end;
      5 : begin     // dsPIC33
            SetLength(IgnoreAddresses, 1);
            IgnoreAddresses[0].LoBound := $F80000;
            IgnoreAddresses[0].HiBound := $F80000   + (12 * HexInfo.AddressIncrement);
          end;
      6 : begin      // PIC32
            SetLength(IgnoreAddresses, 1);
            IgnoreAddresses[0].LoBound := $1FC02FF0;
            IgnoreAddresses[0].HiBound := $1FC02FF0  + (12 * HexInfo.AddressIncrement);
          end;
    end;
  end;
  if HexParser.ParseHex(KFileNameEdit.FileName, HexInfo, AnEraseBlock, AWriteBlock, AWriteBuffer, IgnoreAddresses) then
  begin

    if HexParser.BuildWriteBlockList = ERROR_WRITE_BLOCK_SMALLER_THAN_INSTRUCTION then
    begin
      StringGridWrite.BeginUpdateBounds;
      try
        StringGridWrite.Clear;
      finally
        StringGridWrite.EndUpdateBounds;
      end;
      ShowMessage('Error: WriteBlock Size can not be smaller than the size of an instruction');
    end else
    begin
      StringGridWrite.BeginUpdateBounds;
      try
        FixedAddressCount := 6;
        StringGridWrite.Clear;
        StringGridWrite.ColCount := FixedAddressCount;
        StringGridWrite.RowCount := HexParser.PhysicalWriteBlockList.Count + 1;
        StringGridWrite.Cells[0,0] := IntToStr(HexParser.PhysicalWriteBlockList.Count);
        for Row := 0 to HexParser.PhysicalWriteBlockList.Count - 1 do
        begin
          WriteBlock := HexParser.PhysicalWriteBlockList[Row];
          StringGridWrite.Cells[0, Row+1] := 'Block: ' + IntToStr( Row);
          StringGridWrite.Cells[1, Row+1] := '0x' + IntToHex( WriteBlock.AddressStart, 8);
          StringGridWrite.Cells[2, Row+1] := '0x' + IntToHex( WriteBlock.AddressLast, 8);
          StringGridWrite.Cells[3, Row+1] :=  IntToStr( WriteBlock.AddressCount);
          StringGridWrite.Cells[4, Row+1] :=  IntToStr( WriteBlock.ByteCount);
          StringGridWrite.Cells[5, Row+1] :=  IntToStr( WriteBlock.BytesPerInstruction);
          if StringGridWrite.ColCount < WriteBlock.AddressCount + FixedAddressCount then
            StringGridWrite.ColCount := WriteBlock.AddressCount + FixedAddressCount;
          NextAddress := WriteBlock.AddressStart;
          for iColumn := 0 to WriteBlock.AddressCount - 1 do
          begin
            InstructionPtr := WriteBlock.JumpToInstruction(NextAddress);
            case HexParser.HexInfo.BytesPerInstruction of
              1    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( InstructionPtr^, 2*HexInfo.BytesPerInstruction);
              2    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( PWord( InstructionPtr)^, 2*HexInfo.BytesPerInstruction);
              3    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( (PDWord( InstructionPtr)^) and $00FFFFFF, 2*HexInfo.BytesPerInstruction);
              4    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( PDWord( InstructionPtr)^, 2*HexInfo.BytesPerInstruction);
            end;
            Inc(NextAddress, WriteBlock.AddressIncrement);
          end;
        end;
        StringGridWrite.Cells[0, 0] := 'WriteBlock';
        StringGridWrite.Cells[1, 0] := 'Start Address';
        StringGridWrite.Cells[2, 0] := 'Last Address';
        StringGridWrite.Cells[3, 0] := 'Address Count';
        StringGridWrite.Cells[4, 0] := 'Byte Count';
        StringGridWrite.Cells[5, 0] := 'Bytes Per Address';
        for iColumn := 6 to StringGridWrite.ColCount - 1 do
          StringGridWrite.Cells[iColumn, 0] := 'Instruction ' + IntToStr(iColumn-6);
      finally
        StringGridWrite.EndUpdateBounds;
      end;
    end;


    if HexParser.BuildEraseBlockList = ERROR_ERASE_BLOCK_SMALLER_THAN_INSTRUCTION then
    begin
      StringGridErase.BeginUpdateBounds;
      try
        StringGridErase.Clear;
      finally
        StringGridErase.EndUpdateBounds;
      end;
      ShowMessage('Error: EraseBlock Size can not be smaller than the size of an instruction');
    end else
    begin
      StringGridErase.BeginUpdateBounds;
      try
        StringGridErase.Clear;
        StringGridErase.ColCount := 4;
        StringGridErase.RowCount := HexParser.PhysicalEraseBlockList.Count + 1;
        StringGridErase.Cells[0,0] := IntToStr(HexParser.PhysicalEraseBlockList.Count);
        for Row := 0 to HexParser.PhysicalEraseBlockList.Count - 1 do
        begin
          EraseBlock := HexParser.PhysicalEraseBlockList[Row];
          StringGridErase.Cells[0, Row + 1] := 'Block: ' + IntToStr( Row);
          StringGridErase.Cells[1, Row + 1] := '0x' + IntToHex( EraseBlock.AddressStart, 8);
          StringGridErase.Cells[2, Row + 1] := '0x' + IntToHex( EraseBlock.AddressLast, 8);
          StringGridErase.Cells[3, Row + 1] :=  IntToStr( EraseBlock.BlockCount);
        end;
        StringGridErase.Cells[0, 0] := 'EraseBlock';
        StringGridErase.Cells[1, 0] := 'Start Address';
        StringGridErase.Cells[2, 0] := 'End Address';
        StringGridErase.Cells[3, 0] := 'Block Count';
      finally
        StringGridErase.EndUpdateBounds;
      end;
    end;


  //  16F: config bytes are all over, $8007, $2007
  //  18F(J): config bytes are all over, $30000 - but address increments by 1 and only bottom nibble of word is valid!!!!
  //                                     plus OTHER THINGS.....

    StringGrid.BeginUpdateBounds;
    try
      StringGrid.Clear;
      StringGrid.RowCount := 2;
      if Pivot then
      begin
        FixedAddressCount := 4;
        StringGrid.ColCount := FixedAddressCount;
        StringGrid.Cells[0,0] := IntToStr(HexParser.PhysicalAddressBlockList.Count);
        for iBlock := 0 to HexParser.PhysicalAddressBlockList.Count - 1 do
        begin
          AddressBlock := HexParser.PhysicalAddressBlockList[iBlock];
          NextAddress := AddressBlock.AddressStart;
          for iColumn := 0 to AddressBlock.AddressCount - 1 do
          begin
            StringGrid.RowCount := StringGrid.RowCount + 1;
            InstructionPtr := AddressBlock.JumpToInstruction(NextAddress);
            StringGrid.Cells[0, StringGrid.RowCount-1] := '0x' + IntToHex( NextAddress, 8);
            StringGrid.Cells[1, StringGrid.RowCount-1] := IntToStr( AddressBlock.ByteCount);
            StringGrid.Cells[2, StringGrid.RowCount-1] := IntToStr( AddressBlock.AddressCount);
            case HexParser.HexInfo.BytesPerInstruction of
              1    : StringGrid.Cells[3, StringGrid.RowCount-1] := '0x' + IntToHex( InstructionPtr^, 2*HexInfo.BytesPerInstruction);
              2    : StringGrid.Cells[3, StringGrid.RowCount-1] := '0x' + IntToHex( PWord( InstructionPtr)^, 2*HexInfo.BytesPerInstruction);
              3    : StringGrid.Cells[3, StringGrid.RowCount-1] := '0x' + IntToHex( (PDWord( InstructionPtr)^) and $00FFFFFF, 2*HexInfo.BytesPerInstruction);
              4    : StringGrid.Cells[3, StringGrid.RowCount-1] := '0x' + IntToHex( PDWord( InstructionPtr)^, 2*HexInfo.BytesPerInstruction);
            end;
            Inc(NextAddress, AddressBlock.AddressIncrement);
          end;
        end;
      end else
      begin
        FixedAddressCount := 3;
        StringGrid.ColCount := FixedAddressCount;
        StringGrid.Cells[0,0] := IntToStr(HexParser.PhysicalAddressBlockList.Count);
        for iBlock := 0 to HexParser.PhysicalAddressBlockList.Count - 1 do
        begin
          AddressBlock := HexParser.PhysicalAddressBlockList[iBlock];
          NextAddress := AddressBlock.AddressStart;
          StringGrid.Cells[0, StringGrid.RowCount-1] := '0x' + IntToHex(AddressBlock.AddressStart, 8);
          StringGrid.Cells[1, StringGrid.RowCount-1] := IntToStr( AddressBlock.ByteCount);
          StringGrid.Cells[2, StringGrid.RowCount-1] := IntToStr( AddressBlock.AddressCount);
          if StringGrid.ColCount < AddressBlock.AddressCount + FixedAddressCount then
            StringGrid.ColCount := AddressBlock.AddressCount + FixedAddressCount;
          for iColumn := 0 to AddressBlock.AddressCount - 1 do
          begin
            InstructionPtr := AddressBlock.JumpToInstruction(NextAddress);
            case HexParser.HexInfo.BytesPerInstruction of
              1    : StringGrid.Cells[iColumn+FixedAddressCount, StringGrid.RowCount-1] := '0x' + IntToHex( InstructionPtr^, 2*HexInfo.BytesPerInstruction);
              2    : StringGrid.Cells[iColumn+FixedAddressCount, StringGrid.RowCount-1] := '0x' + IntToHex( PWord( InstructionPtr)^, 2*HexInfo.BytesPerInstruction);
              3    : StringGrid.Cells[iColumn+FixedAddressCount, StringGrid.RowCount-1] := '0x' + IntToHex( (PDWord( InstructionPtr)^) and $00FFFFFF, 2*HexInfo.BytesPerInstruction);
              4    : StringGrid.Cells[iColumn+FixedAddressCount, StringGrid.RowCount-1] := '0x' + IntToHex( PDWord( InstructionPtr)^, 2*HexInfo.BytesPerInstruction);
            end;
            Inc(NextAddress, AddressBlock.AddressIncrement);
          end;
          StringGrid.RowCount := StringGrid.RowCount + 1;
        end;
      end;

      for iColumn := 0 to StringGrid.ColCount - 1 do
      begin
        if iColumn = 0 then
          StringGrid.Cells[iColumn, 0] := 'Address'
        else
        if iColumn = 1 then
          StringGrid.Cells[iColumn, 0] := 'Bytes'
        else
        if iColumn = 2 then
          StringGrid.Cells[iColumn, 0] := 'Address Count'
        else
          StringGrid.Cells[iColumn, 0] := 'Instruction ' + IntToStr(iColumn-FixedAddressCount);
      end;
    finally
      StringGrid.EndUpdateBounds;
    end;
  end;
end;

procedure TForm1.RadioGroupEraseBlockClick(Sender: TObject);
begin
  DoUpdateWriteBufferMultiplier;
  DoParse;
end;

procedure TForm1.RadioGroupFormatClick(Sender: TObject);
begin
  DoParse;
end;

procedure TForm1.RadioGroupMCUClick(Sender: TObject);
begin
  DoParse;
end;

procedure TForm1.RadioGroupWriteBlockClick(Sender: TObject);
begin
  DoUpdateWriteBufferMultiplier;
  DoParse
end;

procedure TForm1.SpinEditWriteBufferMulitiplierChange(Sender: TObject);
begin
  DoUpdateWriteBufferMultiplier
end;

end.

