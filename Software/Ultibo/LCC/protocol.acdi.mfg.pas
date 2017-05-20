unit protocol.acdi.mfg;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, lcc.objects, lcc.types, lcc.message, lcc.utilities, protocol.snip;

type
  { TProtocolAcdiMfg }

  TProtocolAcdiMfg = class(TStreamBasedProtocol)
  public
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage; SNIP: TProtocolSnip); reintroduce; virtual;
  end;

implementation

procedure TProtocolAcdiMfg.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage; SNIP: TProtocolSnip);
var
  i, Offset: Integer;
  ReadCount: Integer;
  Address: DWord;
  FlatArray: array[0..ACDI_MFG_SIZE - 1] of Byte;
begin
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

  FlatArray[0] := 0;
  FillChar(FlatArray, ACDI_MFG_SIZE, #0);

  FlatArray[0] := SNIP.Version;
  Offset := ACDI_MFG_OFFSET_MANUFACTURER;
  StringToNullArray(SNIP.Manufacturer, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_MODEL;
  StringToNullArray(SNIP.Model, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_HARDWARE_VERSION;
  StringToNullArray(SNIP.HardwareVersion, FlatArray, Offset);
  Offset := ACDI_MFG_OFFSET_SOFTWARE_VERSION;
  StringToNullArray(SNIP.SoftwareVersion, FlatArray, Offset);

  OutMessage.DataCount := ReadCount + 7;
  for i := 0 to ReadCount - 1 do
    OutMessage.DataArrayIndexer[i + 7] := FlatArray[Address + DWord( i)];
end;

end.

