unit protocol.datagram.cdi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baseobjects, basetypes, basemessage, mustangpeak.xmlutilities,
  protocol.snip;

type
  { TCDI }

  TCDI = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  public
    function LoadFromXml(CdiFilePath: String; Snip: TProtocolSnip): Boolean;
    function LoadSNIP(ASnip: TProtocolSnip): Boolean;
  end;

implementation

{TCDI}

procedure TCDI.DoLoadComplete(LccMessage: TLccMessage);
begin

end;

function TCDI.LoadFromXml(CdiFilePath: String; Snip: TProtocolSnip): Boolean;
var
  XmlFile: TStringList;
  i, j: Integer;
  {$IFNDEF FPC}
  AByte: Byte;
  {$ENDIF}
begin
  Result := False;
//  Valid := False;
  if FileExists(String( CdiFilePath)) then
  begin
    XmlFile := TStringList.Create;
    try
      XmlFile.LoadFromFile(String( CdiFilePath));
      XmlFile.Text := Trim(XmlFile.Text);
      AStream.Clear;
      for i := 0 to XmlFile.Count - 1 do
      begin
        if Length(XmlFile[i]) > 0 then
        begin
          for j := 1 to Length(XmlFile[i]) do
          begin
            {$IFDEF FPC}
            AStream.WriteByte(Ord(XmlFile[i][j]));
            {$ELSE}
            AByte := Ord(XmlFile[i][j]);
            AStream.Write(AByte, 1);
            {$ENDIF}
          end;
        end
      end;
      if Snip <> nil then
        Snip.LoadFromCdiXmlPath(CdiFilePath);
 //     Valid := True;
      Result := True;
    finally
      FreeAndNil(XmlFile);
    end;
  end;
end;

function TCDI.LoadSNIP(ASnip: TProtocolSnip): Boolean;
var
  XmlDoc: TMustangpeakXmlDocument;
begin
  if Assigned(ASnip) then
  begin
    XmlDoc := XmlLoadFromStream(AStream);
    ASnip.LoadFromCdiXmlDoc(XmlDoc);
    XmlDoc.Free;
  end;
end;

end.

