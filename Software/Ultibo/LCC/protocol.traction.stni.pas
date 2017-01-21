unit protocol.traction.stni;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, baseobjects, basetypes, basemessage, protocol.traction;

type
  TProtocolStni = class(TNodeProtocolBase)
  private
    FManufacturer: string;
    FOwner: string;
    FRoadname: string;
    FRoadNumber: string;
    FTrainClass: string;
    FTrainName: string;
    FVersion: Word;
  public
    property Version: Word read FVersion;
    property Roadname: string read FRoadname;
    property TrainClass: string read FTrainClass;
    property RoadNumber: string read FRoadNumber;
    property TrainName: string read FTrainName;
    property Manufacturer: string read FManufacturer;
    property Owner: string read FOwner;

    function ProcessMessage(LccMessage: TLccMessage; Traction: TProtocolTraction): Boolean; reintroduce; virtual;
  end;

implementation


{ TProtocolStni }

function TProtocolStni.ProcessMessage(LccMessage: TLccMessage; Traction: TProtocolTraction): Boolean;

    function NextString(AStrPtr: PChar): PChar;
    begin
      Result := AStrPtr;
      while Result^ <> #0 do
        Inc(Result);
      Inc(Result);
    end;

var
  StrPtr: PChar;
begin
  Result := True;
  StrPtr := @LccMessage.DataArray[0];

  FVersion := Ord( StrPtr^);
  Inc(StrPtr);
  FRoadname := StrPtr;
  StrPtr := NextString(StrPtr);
  FTrainClass := StrPtr;
  StrPtr := NextString(StrPtr);
  FRoadNumber := StrPtr;
  StrPtr := NextString(StrPtr);
  FTrainName := StrPtr;
  StrPtr := NextString(StrPtr);
  FManufacturer := StrPtr;
  StrPtr := NextString(StrPtr);
  FOwner := StrPtr;
  StrPtr := NextString(StrPtr);
  Traction.LegacyTechnology := Ord(StrPtr^);
  Inc(StrPtr);
  Traction.LegacyTrainID := Ord( StrPtr^) shl 8;
  Inc(StrPtr);
  Traction.LegacyTrainID := Ord(StrPtr^) or Traction.LegacyTrainID;
  Inc(StrPtr);
  if Ord( StrPtr^) > 0 then
    Traction.LegacyTrainID := Traction.LegacyTrainID or $C000;
  Inc(StrPtr);
  case Ord(StrPtr^) of
    0 : Traction.LegacySpeedSteps := 14;
    1 : Traction.LegacySpeedSteps := 28;
    2 : Traction.LegacySpeedSteps := 128
  else
    Traction.LegacySpeedSteps := 28;
  end;
end;

end.

