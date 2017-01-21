unit protocol.traction;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, baseobjects, basetypes, basemessage, mustangpeak.half_float;

{ TProtocolTraction }

type
  TProtocolTraction = class(TNodeProtocolBase)
  private
    FLegacySpeedSteps: Byte;
    FLegacyTechnology: Byte;
    FLegacyTrainID: Word;
    FLinkedNode: TPersistent;                 // depends on the Node: Throttle Node = Linked Train Node, Train Node = Linked Throttle Node
    FScratchNode: TPersistent;
    FSpeed: THalfFloat;
    FSpeedActual: THalfFloat;
    FSpeedCommanded: THalfFloat;
    procedure SetFunctions(Index: DWord; AValue: Word);
    function GetFunctions(Index: DWord): Word;
  protected
    FunctionArray: array of Word;
    procedure GrowArray(NewSize: DWord);
  public
    property Speed: THalfFloat read FSpeed;
    property SpeedActual: THalfFloat read FSpeedActual;
    property SpeedCommanded: THalfFloat read FSpeedCommanded;
    property Functions[Index: DWord]: Word read GetFunctions;
    property LinkedNode: TPersistent read FLinkedNode write FLinkedNode;
    property LegacyTechnology: Byte read FLegacyTechnology write FLegacyTechnology;
    property LegacyTrainID: Word read FLegacyTrainID write FLegacyTrainID;
    property LegacySpeedSteps: Byte read FLegacySpeedSteps write FLegacySpeedSteps;
    property ScratchNode: TPersistent read FScratchNode write FScratchNode;

    function IsLinked: Boolean;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TProtocolTraction }

procedure TProtocolTraction.SetFunctions(Index: DWord; AValue: Word);
begin
  GrowArray(Index + 1);
  FunctionArray[Index] := AValue
end;

function TProtocolTraction.GetFunctions(Index: DWord): Word;
begin
  GrowArray(Index + 1);
  Result := FunctionArray[Index];
end;

procedure TProtocolTraction.GrowArray(NewSize: DWord);
var
  OldSize, i: DWord;
begin
  OldSize := Length(FunctionArray);
  if NewSize > OldSize then
  begin
    SetLength(FunctionArray, NewSize);
    i := OldSize;
    while i < NewSize do
    begin
      FunctionArray[i] := 0;
      Inc(i)
    end
  end;
end;

function TProtocolTraction.IsLinked: Boolean;
begin
  Result := Assigned(LinkedNode)
end;

function TProtocolTraction.ProcessMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  case LccMessage.DataArrayIndexer[0] of
    TRACTION_QUERY_SPEED :
        begin
          FSpeed := LccMessage.ExtractDataBytesAsInt(1, 2);
          FSpeedCommanded := LccMessage.ExtractDataBytesAsInt(4, 5);
          FSpeedActual := LccMessage.ExtractDataBytesAsInt(6, 7); ;
        end;
    TRACTION_QUERY_FUNCTION :
        begin
          SetFunctions(LccMessage.ExtractDataBytesAsInt(1, 3), LccMessage.ExtractDataBytesAsInt(4,5))
        end;
    TRACTION_CONTROLLER_CONFIG :
        begin
          case LccMessage.DataArrayIndexer[1] of
            TRACTION_CONTROLLER_CONFIG_ASSIGN :
                begin

                end;
            TRACTION_CONTROLLER_CONFIG_QUERY :
                begin

                end;
            TRACTION_CONTROLLER_CONFIG_NOTIFY :
                begin

                end;
          end;
        end;
    TRACTION_CONSIST :
        begin

        end;
    TRACTION_MANAGE :
        begin

        end;
  end;
end;

end.

