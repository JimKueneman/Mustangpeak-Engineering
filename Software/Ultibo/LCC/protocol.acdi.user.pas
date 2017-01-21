unit protocol.acdi.user;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcc.types, lcc.message, lcc.utilities, protocol.acdi.mfg, protocol.snip,
  protocol.datagram.configuration;

type
{ TProtocolAcdiUser }

  TProtocolAcdiUser = class(TProtocolAcdiMfg)
  public
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage; SNIP: TProtocolSnip); override;
    procedure WriteRequest(LccMessage: TLccMessage; Configuration: TProtocolConfiguration); reintroduce; virtual;
  end;

implementation

{ TProtocolAcdiUser }

procedure TProtocolAcdiUser.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage; SNIP: TProtocolSnip);
var
  i, Offset: Integer;
  ReadCount: Integer;
  Address: DWord;
  FlatArray: array[0..ACDI_USER_SIZE - 1] of Byte;
begin
  FlatArray[0] := 0;
  // Assumption is this is a datagram message
  ReadCount := LccMessage.ExtractDataBytesAsInt(7, 7);
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArrayIndexer[0];
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArrayIndexer[1] or $10;   // Make it a reply
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];          // Copy the address
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArrayIndexer[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArrayIndexer[5];
  OutMessage.DataArrayIndexer[6] := LccMessage.DataArrayIndexer[6];

  FillChar(FlatArray, ACDI_USER_SIZE, #0);

  FlatArray[0] := SNIP.UserVersion;
  Offset := ACDI_USER_OFFSET_NAME;
  StringToNullArray(SNIP.UserName, FlatArray, Offset);
  Offset := ACDI_USER_OFFSET_DESCRIPTION;
  StringToNullArray(SNIP.UserDescription, FlatArray, Offset);

  OutMessage.DataCount := ReadCount + 7;
  for i := 0 to ReadCount - 1 do
    OutMessage.DataArrayIndexer[i + 7] := FlatArray[Address + DWord(i)];
end;

procedure TProtocolAcdiUser.WriteRequest(LccMessage: TLccMessage; Configuration: TProtocolConfiguration);
var
  Address: DWord;
begin
  // We should never allow the Version to be written too so never write to 0 offset
  Address := LccMessage.ExtractDataBytesAsInt(2, 5);
  if Address > 0 then
  begin
    Configuration.WriteRequest(LccMessage);
  end;
end;

end.

