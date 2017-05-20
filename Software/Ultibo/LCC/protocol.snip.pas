unit protocol.snip;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

{$I lcc_compilers.inc}

uses
  Classes, SysUtils, lcc.objects, lcc.types, mustangpeak.xmlutilities, lcc.message, lcc.utilities;

type
  { TProtocolSnip }

  TProtocolSnip = class(TNodeProtocolBase)
  private
    FHardwareVersion: String;
    FManufacturer: String;
    FModel: String;
    FSoftwareVersion: String;
    FPackedInfo: TSimpleNodeInfoPacked;
    FUserDescription: String;
    FUserName: String;
    FUserVersion: Word;
    FVersion: Word;

    function GetPackedFormat: TSimpleNodeInfoPacked;
    function GetUserDescription: String;
    function GetUserName: String;
  public
    property Version: Word read FVersion write FVersion;
    property Manufacturer: String read FManufacturer write FManufacturer;
    property Model: String read FModel write FModel;
    property HardwareVersion: String read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: String read FSoftwareVersion write FSoftwareVersion;
    property UserVersion: Word read FUserVersion write FUserVersion;
    property UserName: String read GetUserName write FUserName;
    property UserDescription: String read GetUserDescription write FUserDescription;

    property PackedFormat: TSimpleNodeInfoPacked read GetPackedFormat;

    function LoadFromCdiXmlPath(CdiFilePath: String): Boolean;
    function LoadFromCdiXmlDoc(CdiXMLDoc: TMustangpeakXmlDocument): Boolean;
    function ProcessMessage(LccMessage: TLccMessage): Boolean; override;
  end;

implementation

{ TProtocolSnip }

function TProtocolSnip.GetPackedFormat: TSimpleNodeInfoPacked;
const
  NULL_COUNT = 6;
  VERSION_COUNT = 2;
var
  iArray, i: Integer;
begin
  i :=  Length(Manufacturer) + Length(Model) + Length(HardwareVersion) + Length(SoftwareVersion) + Length(UserName) + Length(UserDescription);
  i := i + NULL_COUNT + VERSION_COUNT;
  SetLength(FPackedInfo, i);
  iArray := 0;

  FPackedInfo[iArray] := Version;      // 4 Items follow
  Inc(iArray);
  StringToNullArray(Manufacturer, FPackedInfo, iArray);
  StringToNullArray(Model, FPackedInfo, iArray);
  StringToNullArray(HardwareVersion, FPackedInfo, iArray);
  StringToNullArray(SoftwareVersion, FPackedInfo, iArray);

  FPackedInfo[iArray] := UserVersion;  // 2 items follow
  Inc(iArray);
  StringToNullArray(UserName, FPackedInfo, iArray);
  StringToNullArray(UserDescription, FPackedInfo, iArray);

  Result := FPackedInfo;
end;

function TProtocolSnip.GetUserDescription: String;
begin
  Result := FUserDescription;
end;

function TProtocolSnip.GetUserName: String;
begin
  Result := FUserName;
end;

function TProtocolSnip.LoadFromCdiXmlPath(CdiFilePath: String): Boolean;
var
  XMLDoc: TMustangpeakXmlDocument;
begin
  Result := False;
  if FileExists(CdiFilePath) then
  begin
    try
      XMLDoc := XmlLoadFromFile(CdiFilePath);
      Result := LoadFromCdiXmlDoc(XMLDoc);
      {$IFDEF FPC}
      XMLDoc.Free;
      {$ELSE}
      XMLDoc := nil;
      {$ENDIF}
    except
      // Quiet fail
    end;
  end;
end;

function TProtocolSnip.LoadFromCdiXmlDoc(CdiXMLDoc: TMustangpeakXmlDocument): Boolean;
var
  CdiNode, IdentificationNode, ChildNode: TMustangpeakXmlNode;
begin
  if Assigned(CdiXMLDoc) then
  begin
    CdiNode := XmlFindRootNode(CdiXMLDoc, 'cdi');
    if Assigned(CdiNode) then
    begin
      IdentificationNode := XmlFindChildNode(CdiNode, 'identification');
      if Assigned(IdentificationNode) then
      begin
         Version := 1;
         ChildNode := XmlFindChildNode(IdentificationNode, 'manufacturer');
         if Assigned(ChildNode) then FManufacturer := XmlFirstChildValue(ChildNode) else Exit;
         ChildNode := XmlFindChildNode(IdentificationNode, 'model');
         if Assigned(ChildNode) then FModel := XmlFirstChildValue(ChildNode) else Exit;
         ChildNode := XmlFindChildNode(IdentificationNode, 'hardwareVersion');
         if Assigned(ChildNode) then FHardwareVersion := XmlFirstChildValue(ChildNode) else Exit;
         ChildNode := XmlFindChildNode(IdentificationNode, 'softwareVersion');
         if Assigned(ChildNode) then FSoftwareVersion := XmlFirstChildValue(ChildNode) else Exit;
         UserVersion := 1;
         Result := True;
      end;
    end;
  end;
end;

function TProtocolSnip.ProcessMessage(LccMessage: TLccMessage): Boolean;

  {$IFDEF LCC_MOBILE}
  function NextString(AStrPtr: PChar): PChar;
  {$ELSE}
  function NextString(AStrPtr: PAnsiChar): PAnsiChar;
  {$ENDIF}

  begin
    Result := AStrPtr;
    while Result^ <> #0 do
      Inc(Result);
    Inc(Result);
  end;

{$IFDEF LCC_MOBILE}
var
  StrPtr: PChar;
{$ELSE}
var
  StrPtr: PAnsiChar;
{$ENDIF}
begin
  Result := True;
  StrPtr := @LccMessage.DataArray[0];
  FVersion := Ord(StrPtr^);
  Inc(StrPtr);
  FManufacturer := StrPtr;
  StrPtr := NextString(StrPtr);
  FModel := StrPtr;
  StrPtr := NextString(StrPtr);
  FHardwareVersion := StrPtr;
  StrPtr := NextString(StrPtr);
  FSoftwareVersion := StrPtr;
  StrPtr := NextString(StrPtr);
  FUserVersion := Ord(StrPtr^);
  Inc(StrPtr);
  FUserName := StrPtr;
  StrPtr := NextString(StrPtr);
  FUserDescription := StrPtr;
end;

end.

