unit protocol.datagram.configuration;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, lcc.objects, lcc.types, lcc.message;

type

  { TProtocolConfiguration }

  TProtocolConfiguration = class(TStreamBasedProtocol)
  private
    FAutoSaveOnWrite: Boolean;
    FFilePath: String;
  protected
  public
    property AutoSaveOnWrite: Boolean read FAutoSaveOnWrite write FAutoSaveOnWrite;
    property FilePath: String read FFilePath write FFilePath;

    constructor Create(AnAddressSpace: Byte; IsStringBasedStream: Boolean); override;
    destructor Destroy; override;
    procedure WriteRequest(LccMessage: TLccMessage); override;
    function ReadAsString(Address: DWord): String;
    procedure LoadFromFile;
  end;

implementation

{ TProtocolConfiguration }

constructor TProtocolConfiguration.Create(AnAddressSpace: Byte; IsStringBasedStream: Boolean);
begin
  inherited Create(AnAddressSpace, IsStringBasedStream);
  AutoSaveOnWrite := True;
end;

destructor TProtocolConfiguration.Destroy;
begin
  inherited Destroy;
end;

procedure TProtocolConfiguration.LoadFromFile;
begin
  if FileExists(String( FilePath)) then
    AStream.LoadFromFile(String( FilePath))
end;

function TProtocolConfiguration.ReadAsString(Address: DWord): String;
var
  i: DWord;
  C: Char;
  Done: Boolean;
begin
  Result := '';
  if AStream.Size > Address then
  begin
    AStream.Position := Address;
    i := 0;
    Done := False;
    while (i + Address < DWord( AStream.Size)) and not Done do
    begin
      {$IFDEF FPC}
      C := Chr(AStream.ReadByte);
      {$ELSE}
      AStream.Read(C, 1);
      {$ENDIF}
      if C <> #0 then
        Result := Result + C
      else
        Done := True;
      Inc(i)
    end;
  end;
end;

procedure TProtocolConfiguration.WriteRequest(LccMessage: TLccMessage);
var
  i: Integer;
  iStart : Integer;
  WriteCount,Address: DWord;
  {$IFNDEF FPC}
  AByte: Byte;
  {$ENDIF}
begin
  // Assumption is this is a datagram message
  if LccMessage.DataArrayIndexer[1] and $03 = 0 then
    iStart := 7
  else
    iStart := 6;
  WriteCount := LccMessage.DataCount - iStart;
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  if Address + WriteCount > DWord( AStream.Size) then
    AStream.Size := Int64( Address) + Int64(WriteCount);
  AStream.Position := Address;
  for i := iStart to LccMessage.DataCount - 1 do
  begin
    {$IFDEF FPC}
     AStream.WriteByte(LccMessage.DataArrayIndexer[i]);
    {$ELSE}
    AByte := LccMessage.DataArrayIndexer[i];
    AStream.Write(AByte, 1);
    {$ENDIF}
  end;
  if AutoSaveOnWrite then
  begin
    if not FileExists(String( FilePath)) then
    begin
      if DirectoryExists(ExtractFilePath(FilePath)) then
        AStream.SaveToFile(String( FilePath))
    end else
      AStream.SaveToFile(String( FilePath))
  end;
end;

end.

