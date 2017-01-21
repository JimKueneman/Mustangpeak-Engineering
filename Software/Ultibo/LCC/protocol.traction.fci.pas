unit protocol.traction.fci;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, baseobjects, basetypes, basemessage;

type
  { TFunctionConfiguration }

  TFunctionConfiguration = class(TNodeProtocolBase)
  private
    FFunctionStatesArray: TFunctionStatesArray;
    function GetFunctionStates(iIndex: Integer): Boolean;
  protected
    property FunctionStatesArray: TFunctionStatesArray read FFunctionStatesArray write FFunctionStatesArray;
  public
    property FunctionStates[iIndex: Integer]: Boolean read GetFunctionStates;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TFunctionConfiguration }

function TFunctionConfiguration.GetFunctionStates(iIndex: Integer): Boolean;
begin
  if (iIndex > -1) and (iIndex < 30) then
    Result := FFunctionStatesArray[iIndex] = 1
  else
    Result := False;
end;

function TFunctionConfiguration.ProcessMessage(LccMessage: TLccMessage): Boolean;
var
  FunctionAddress: DWord;
  i: Integer;
begin
  Result := False;
  FunctionAddress := LccMessage.ExtractDataBytesAsInt(2, 5);
  FunctionAddress := FunctionAddress and $000000FF;
  i := 7;
  if (LccMessage.DataCount - i) mod 2 = 0 then   // Words are 2 bytes so make sure we are on even boundy of words
  begin
    while i < LccMessage.DataCount do
    begin
      FFunctionStatesArray[FunctionAddress] := (LccMessage.DataArrayIndexer[i+1] shl 8) or LccMessage.DataArrayIndexer[i]; // Little
      Inc(FunctionAddress);
      Inc(i, 2);
    end;
  end;
end;

end.

