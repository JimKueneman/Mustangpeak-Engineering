unit mapform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, intel_hex_parser;

type

  { TFormMemoryMap }

  TFormMemoryMap = class(TForm)
    ButtonClose2: TButton;
    ButtonLoadFromFileErase: TButton;
    ButtonLoadFromFileWrite: TButton;
    ButtonSaveToFileErase: TButton;
    ButtonSaveToFileWrite: TButton;
    Label5: TLabel;
    LabelAddress: TLabel;
    OpenDialog: TOpenDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    StringGridErase: TStringGrid;
    StringGridWrite: TStringGrid;
    procedure ButtonLoadFromFileEraseClick(Sender: TObject);
    procedure ButtonLoadFromFileWriteClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonSaveToFileEraseClick(Sender: TObject);
    procedure ButtonSaveToFileWriteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGridEraseSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure StringGridWriteSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    FHexParser: TIntelHexParser;
    { private declarations }
  public
    { public declarations }
    procedure BuildGrids;
    property HexParser: TIntelHexParser read FHexParser write FHexParser;
  end;

var
  FormMemoryMap: TFormMemoryMap;

implementation

{$R *.lfm}

{ TFormMemoryMap }

procedure TFormMemoryMap.FormShow(Sender: TObject);
begin
  BuildGrids;
end;

procedure TFormMemoryMap.StringGridEraseSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin

end;

procedure TFormMemoryMap.StringGridWriteSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
var
  BaseAddress: DWord;
  s: string;
begin
  if Assigned(HexParser) then
  begin
    if (aCol > 5) and (aRow > 0) then
    begin
      if HexParser.HexInfo.AddressIncrement > 0 then
      begin
        s := StringGridWrite.Cells[1, aRow];
        if s <> '' then
        begin
          BaseAddress := StrToInt64( StringReplace(s, '0x', '$', [rfIgnoreCase]));
          Inc(BaseAddress, HexParser.HexInfo.AddressIncrement * (aCol - 6));
          LabelAddress.Caption := '0x' + IntToHex(BaseAddress, 8);
        end
      end else
        LabelAddress.Caption := 'MCU type Unknown';
    end else
     LabelAddress.Caption := 'None'
  end;
end;

procedure TFormMemoryMap.BuildGrids;
var
  Row, iColumn: Integer;
  NextAddress, FixedAddressCount: DWord;
  EraseBlock: TPhysicalEraseBlock;
  WriteBlock: TPhysicalWriteBlock;
  InstructionPtr: PByte;
begin
  if Assigned(HexParser) and Visible then
  begin
    StringGridErase.BeginUpdateBounds;
    try
      StringGridErase.Clear;
      StringGridErase.ColCount := 4;
      StringGridErase.RowCount := HexParser.PhysicalEraseBlockList.Count + 1;
      for Row := 0 to HexParser.PhysicalEraseBlockList.Count - 1 do
      begin
        EraseBlock := HexParser.PhysicalEraseBlockList[Row];
        StringGridErase.Cells[0, Row + 1] := 'Block: ' + IntToStr( Row);
        StringGridErase.Cells[1, Row + 1] := '0x' + IntToHex( EraseBlock.AddressStart, 2*HexParser.HexInfo.BytesPerInstruction);
        StringGridErase.Cells[2, Row + 1] := '0x' + IntToHex( EraseBlock.AddressLast, 2*HexParser.HexInfo.BytesPerInstruction);
        StringGridErase.Cells[3, Row + 1] :=  IntToStr( EraseBlock.BlockCount);
      end;
      StringGridErase.Cells[0, 0] := 'EraseBlock';
      StringGridErase.Cells[1, 0] := 'Start Address';
      StringGridErase.Cells[2, 0] := 'End Address';
      StringGridErase.Cells[3, 0] := 'Block Count';
    finally
      StringGridErase.EndUpdateBounds;
    end;

    StringGridWrite.BeginUpdateBounds;
    try
      FixedAddressCount := 6;
      StringGridWrite.Clear;
      StringGridWrite.ColCount := FixedAddressCount;
      StringGridWrite.RowCount := HexParser.PhysicalWriteBlockList.Count + 1;
      for Row := 0 to HexParser.PhysicalWriteBlockList.Count - 1 do
      begin
        WriteBlock := HexParser.PhysicalWriteBlockList[Row];
        StringGridWrite.Cells[0, Row+1] := 'Block: ' + IntToStr( Row);
        StringGridWrite.Cells[1, Row+1] := '0x' + IntToHex( WriteBlock.AddressStart, 2*HexParser.HexInfo.BytesPerInstruction);
        StringGridWrite.Cells[2, Row+1] := '0x' + IntToHex( WriteBlock.AddressLast, 2*HexParser.HexInfo.BytesPerInstruction);
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
            1    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( InstructionPtr^, 2*HexParser.HexInfo.BytesPerInstruction);
            2    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( PWord( InstructionPtr)^, 2*HexParser.HexInfo.BytesPerInstruction);
            3    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( (PDWord( InstructionPtr)^) and $00FFFFFF, 2*HexParser.HexInfo.BytesPerInstruction);
            4    : StringGridWrite.Cells[iColumn+FixedAddressCount, Row+1] := '0x' + IntToHex( PDWord( InstructionPtr)^, 2*HexParser.HexInfo.BytesPerInstruction);
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
end;

procedure TFormMemoryMap.FormCreate(Sender: TObject);
begin
  FHexParser := nil;
end;

procedure TFormMemoryMap.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMemoryMap.ButtonSaveToFileEraseClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    StringGridErase.SaveOptions := [soDesign, soAttributes, soContent, soPosition];
    Stream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    StringGridErase.SaveToStream(Stream);
    Stream.Free;
  end;
end;

procedure TFormMemoryMap.ButtonSaveToFileWriteClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if SaveDialog.Execute then
  begin
    StringGridWrite.SaveOptions := [soDesign, soAttributes, soContent, soPosition];
    Stream := TFileStream.Create(SaveDialog.FileName, fmCreate);
    StringGridWrite.SaveToStream(Stream);
    Stream.Free;
  end;
end;

procedure TFormMemoryMap.ButtonLoadFromFileEraseClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog.Execute then
  begin
    StringGridErase.SaveOptions := [soDesign, soAttributes, soContent, soPosition];
    Stream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    StringGridErase.LoadFromStream(Stream);
    Stream.Free;
  end;
end;

procedure TFormMemoryMap.ButtonLoadFromFileWriteClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog.Execute then
  begin
    StringGridWrite.SaveOptions := [soDesign, soAttributes, soContent, soPosition];
    Stream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    StringGridWrite.LoadFromStream(Stream);
    Stream.Free;
  end;
end;

end.

