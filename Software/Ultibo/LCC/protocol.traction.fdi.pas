unit protocol.traction.fdi;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, baseobjects, basetypes, basemessage;

type
  TFDI = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  end;


implementation

{ TFDI }

procedure TFDI.DoLoadComplete(LccMessage: TLccMessage);
begin

end;

end.

