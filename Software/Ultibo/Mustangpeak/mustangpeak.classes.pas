unit mustangpeak.classes;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\Lcc\lcc_compilers.inc}

uses
  Classes, SysUtils;

{$IFDEF FPC}
type
  TCriticalSection = class
  protected
    Lock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;
{$ENDIF}

implementation

{$IFDEF FPC}
{ TCriticalSection }

constructor TCriticalSection.Create;
begin
  System.InitCriticalSection(Lock);
end;

destructor TCriticalSection.Destroy;
begin
  DoneCriticalsection(Lock);
end;

procedure TCriticalSection.Enter;
begin
  System.EnterCriticalsection(Lock);
end;

procedure TCriticalSection.Leave;
begin
  System.LeaveCriticalsection(Lock);
end;
{$ENDIF}

end.

