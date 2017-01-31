unit protocol.datagram.configuration.definition.information;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lcc.objects, lcc.message, protocol.snip, mustangpeak.xmlutilities;

type

  { TProtocolConfigDefinitionInfo }

  TProtocolConfigDefinitionInfo = class(TStreamBasedProtocol)
  protected
    procedure DoLoadComplete(LccMessage: TLccMessage); override;
  public
    function LoadFromXml(CdiFilePath: String; Snip: TProtocolSnip): Boolean;
    function LoadSNIP(ASnip: TProtocolSnip): Boolean;
  end;

implementation

{TProtocolConfigDefinitionInfo}

procedure TProtocolConfigDefinitionInfo.DoLoadComplete(LccMessage: TLccMessage);
//var
 // SourceNode, DestNode: TLccNode;
begin
 { if Assigned(OwnerManager) then
  begin
    SourceNode := OwnerManager.FindMirroredNodeBySourceID(LccMessage, True);
    DestNode := OwnerManager.FindMirroredNodeByDestID(LccMessage, True);
    if Assigned(SourceNode) and Assigned(DestNode) then
      OwnerManager.DoCDI(SourceNode, DestNode);
  end;     }
end;

function TProtocolConfigDefinitionInfo.LoadFromXml(CdiFilePath: String;
  Snip: TProtocolSnip): Boolean;
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
  //    Valid := True;
      Result := True;
    finally
      FreeAndNil(XmlFile);
    end;
  end;
end;

function TProtocolConfigDefinitionInfo.LoadSNIP(ASnip: TProtocolSnip): Boolean;
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

