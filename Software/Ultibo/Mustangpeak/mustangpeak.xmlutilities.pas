unit mustangpeak.xmlutilities;

//
// A compiler agnostic (Ultibo, FPC, Delphi) way to implement simple XML file functions.
//

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  {$IFDEF FPC}
    DOM,
    XMLRead,
    XMLWrite,
  {$ELSE}
  Xml.XMLDoc,
  Xml.xmldom,
  Xml.XMLIntf,
  {$ENDIF}
  SysUtils;

type
  TMustangpeakXmlNode ={$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
  TMustangpeakXmlDocument = {$IFDEF FPC}TXMLDocument{$ELSE}IXMLDocument{$ENDIF};
  TMustangpeakXmlAttribute = {$IFDEF FPC}TDOMAttr{$ELSE}IXMLNode{$ENDIF};


// Document functions
function XmlLoadFromFile(FilePath: string): TMustangpeakXmlDocument;
function XmlLoadFromStream(Stream: TStream): TMustangpeakXmlDocument;
function BuildConfigurationDocument(XMLFilePath: string): TMustangpeakXmlDocument;
procedure XmlFreeDocument(var XmlDoc: TMustangpeakXmlDocument);
function XmlCreateEmptyDocument: TMustangpeakXmlDocument;
procedure XmlWriteToFile(FilePath: string; XmlDoc: TMustangpeakXmlDocument);
function XmlCreateChildNode(XmlDoc: TMustangpeakXmlDocument; ParentNode: TMustangpeakXmlNode; Element, Content: string): TMustangpeakXmlNode;
function XmlCreateRootNode(XmlDoc: TMustangpeakXmlDocument; Element, Content: string): TMustangpeakXmlNode;

// Find functions
function XmlFindChildNode(XmlNode: TMustangpeakXmlNode; Name: string): TMustangpeakXmlNode;
function XmlFirstChild(XmlNode: TMustangpeakXmlNode): TMustangpeakXmlNode;
function XmlFindRootNode(XmlDoc: TMustangpeakXmlDocument; RootName: string): TMustangpeakXmlNode;

// Element value (name) functions
function XmlFirstChildValue(XmlNode: TMustangpeakXmlNode): string;
function XmlNextSiblingValue(XmlNode: TMustangpeakXmlNode): string;
function XmlNodeName(XmlNode: TMustangpeakXmlNode): string;

// Element content (text) functions
function XmlNodeTextContent(XmlNode: TMustangpeakXmlNode): string;
procedure XmlNodeSetTextContent(XmlNode: TMustangpeakXmlNode; Text: string);

// Enumerator functions
function XmlNextSiblingNode(XmlNode: TMustangpeakXmlNode): TMustangpeakXmlNode;

// Attribute functions
function XmlAttributeCreateAndSet(XmlDoc: TMustangpeakXmlDocument; TargetNode: TMustangpeakXmlNode; Attribute, Content: string): Boolean;
procedure XmlAttributeForce(XmlDoc: TMustangpeakXmlDocument; TargetNode: TMustangpeakXmlNode; Attribute, Content: string);
function XmlAttributeRead(TargetNode: TMustangpeakXmlNode; Attribute: string): string;
function XmlAttributeExists(TargetNode: TMustangpeakXmlNode; Attribute: string): Boolean;
procedure XmlAttributeRemove(TargetNode: TMustangpeakXmlNode; Attribute: string);


implementation

procedure XmlAttributeForce(XmlDoc: TMustangpeakXmlDocument; TargetNode: TMustangpeakXmlNode; Attribute, Content: string);
{$IFDEF FPC}
var
  AttributeNode: TMustangpeakXmlNode;
{$ENDIF}
begin
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
  begin
    AttributeNode := TargetNode.Attributes.GetNamedItem( DOMString(Attribute));
    if Assigned(AttributeNode) then
      XmlNodeSetTextContent(AttributeNode, Content)
    else
      XmlAttributeCreateAndSet(XmlDoc, TargetNode, Attribute, Content);
  end;
  {$ELSE}
    TargetNode.SetAttributeNS(Attribute, '', Content)
  {$ENDIF}
end;

function XmlAttributeRead(TargetNode: TMustangpeakXmlNode; Attribute: string): string;
var
  Node: TMustangpeakXmlNode;
begin
  Result := '';
  Node := nil;
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    Node := TargetNode.Attributes.GetNamedItem(DOMString(Attribute));
  if Assigned(Node) then
    Result := string( Node.NodeValue);
  {$ELSE}
  if TargetNode.HasAttribute(Attribute) then
    Result := TargetNode.Attributes[Attribute]
  else
    Result := '';
  {$ENDIF}
end;

function XmlAttributeExists(TargetNode: TMustangpeakXmlNode; Attribute: string): Boolean;
begin
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    Result := Assigned(TargetNode.Attributes.GetNamedItem(DOMString(Attribute)));
  {$ELSE}
  Result := TargetNode.Attributes[Attribute] <> ''
  {$ENDIF}
end;

procedure XmlAttributeRemove(TargetNode: TMustangpeakXmlNode; Attribute: string);
{$IFNDEF FPC}
var
  Node: IXMLNode;
{$ENDIF}
begin
  {$IFDEF FPC}
  if Assigned( TargetNode.Attributes) then
    if Assigned( TargetNode.Attributes.GetNamedItem(DOMString(Attribute))) then
      TargetNode.Attributes.RemoveNamedItem(DOMString(Attribute));
  {$ELSE}
  Node := TargetNode.AttributeNodes.FindNode(Attribute);
  if Assigned(Node) then
    TargetNode.AttributeNodes.Remove(Node)
  {$ENDIF}
end;

function XmlLoadFromStream(Stream: TStream): TMustangpeakXmlDocument;
begin
  Result := nil;
  Stream.Position := 0;
  {$IFDEF FPC}
  ReadXMLFile(Result, Stream);
  {$ELSE}
  Result := XmlLoadFromStream(Stream);
  {$ENDIF}
end;

function BuildConfigurationDocument(XMLFilePath: string): TMustangpeakXmlDocument;

  procedure RunCdi(ChildNode: TMustangpeakXmlNode; var CurrentAddress: Integer);
  var
    Attrib: string;
    ReplicationCount, i: Integer;
  begin
     while Assigned(ChildNode) do
     begin
       XmlAttributeRemove(ChildNode, 'offset');   // Remove any "offset" attribute not used
   //    XmlAttributeForce(Result, ChildNode, 'testing', 'wow');

       if ChildNode.NodeName = 'group' then    // If it is a group then recurse into it.
       begin
         XmlAttributeForce(Result, ChildNode, 'origin', IntToStr(CurrentAddress));
         ReplicationCount := 1;
         Attrib := XmlAttributeRead(ChildNode, 'replication');
         if Attrib <> '' then
           ReplicationCount := StrToInt(Attrib);
         for i := 0 to ReplicationCount-1 do
           RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'int' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt(Attrib)
         else
           Inc(CurrentAddress, 1);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'string' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt(Attrib);  // else broken
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'eventid' then
       begin
         Inc(CurrentAddress, 8);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'bit' then
       begin
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'map' then
       begin
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'relation' then
       begin
          RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end;
       ChildNode := XmlNextSiblingNode(ChildNode);
     end;
  end;

var
  RootNode, SegmentNode: TMustangpeakXmlNode;
  CurrentAddress: Integer;
begin
  Result := XmlLoadFromFile(XMLFilePath);
  if Assigned(Result) then
  begin
    CurrentAddress := 0;
    RootNode := XmlFindRootNode(Result, 'ndi');
    if Assigned(RootNode) then
    begin
      SegmentNode := XmlFindChildNode(RootNode, 'segment');
      while Assigned(SegmentNode) do              // Run all the Segements in the file
      begin
        XmlAttributeForce(Result, SegmentNode, 'origin', IntToStr(CurrentAddress));
        XmlAttributeRemove(SegmentNode, 'offset');
        // From here on it can be recursive down into groups, etc...
        RunCdi(XmlFirstChild(SegmentNode), CurrentAddress);
        SegmentNode := XmlNextSiblingNode(SegmentNode);
      end;
    end;
  end;

end;

procedure XmlFreeDocument(var XmlDoc: TMustangpeakXmlDocument);
begin
  {$IFDEF FPC}
  FreeAndNil(XmlDoc)
  {$ELSE}
     // Is and interface and will free itself
  {$ENDIF}
end;

function XmlCreateEmptyDocument: TMustangpeakXmlDocument;
begin
  {$IFDEF FPC}
  Result := TXMLDocument.Create;
  {$ELSE}
  Result := TXMLDocument.Create(nil) as IXMLDocument;
  {$ENDIF}
end;

procedure XmlWriteToFile(FilePath: string; XmlDoc: TMustangpeakXmlDocument);
begin
  {$IFDEF FPC}
  WriteXMLFile(XmlDoc, FilePath);
  {$ELSE}
  XmlDoc.SaveToXML(FilePath);
  {$ENDIF}
end;

function XmlCreateChildNode(XmlDoc: TMustangpeakXmlDocument; ParentNode: TMustangpeakXmlNode; Element, Content: string): TMustangpeakXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlDoc.CreateElement(DOMString(Element));
  ParentNode.AppendChild(Result);
  if Content <> '' then
    Result.TextContent := DOMString(Content);
  {$ELSE}
  Result := ParentNode.AddChild(Element);
  if Content <> '' then
    Result.Text := Content
  {$ENDIF}
end;

function XmlCreateRootNode(XmlDoc: TMustangpeakXmlDocument; Element, Content: string): TMustangpeakXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlDoc.CreateElement(DOMString(Element));
  XmlDoc.AppendChild(Result);
  Result.TextContent := DOMString(Content);
  {$ELSE}
  Result := XmlDoc.AddChild(Element);
  Result.Text := Content;
  {$ENDIF}
end;

function XmlLoadFromFile(FilePath: string): TMustangpeakXmlDocument;
begin
  Result := nil;
  {$IFDEF FPC}
  ReadXMLFile(Result, FilePath);
  {$ELSE}
  Result := LoadXMLDocument(FilePath);
  {$ENDIF}
end;

function XmlFindChildNode(XmlNode: TMustangpeakXmlNode; Name: string): TMustangpeakXmlNode;
begin
  Result := XmlNode.{$IFNDEF FPC}ChildNodes.{$ENDIF}FindNode(DOMString((Name)));
end;

function XmlFirstChild(XmlNode: TMustangpeakXmlNode): TMustangpeakXmlNode;
begin
  Result := XmlNode.{$IFDEF FPC}FirstChild{$ELSE}ChildNodes.First{$ENDIF}
end;

function XmlFirstChildValue(XmlNode: TMustangpeakXmlNode): string;
var
  Child: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Child := XmlFirstChild(XmlNode);
  if Assigned(Child) then
    Result := string(Child.NodeValue);
end;

function XmlNextSiblingValue(XmlNode: TMustangpeakXmlNode): string;
var
  Sibling: {$IFDEF FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Sibling := XmlNextSiblingNode(XmlNode);
  if Assigned(Sibling) then
    Result := string(Sibling.NodeValue);
end;

function XmlNodeName(XmlNode: TMustangpeakXmlNode): string;
begin
  Result := string(XmlNode.NodeName);
end;

function XmlNodeTextContent(XmlNode: TMustangpeakXmlNode): string;
begin
  {$IFDEF FPC}
  Result := string(XmlNode.TextContent)
  {$ELSE}
  if XmlNode.IsTextElement then
    Result := string(XmlNode.Text)
  {$ENDIF}
end;

procedure XmlNodeSetTextContent(XmlNode: TMustangpeakXmlNode; Text: string);
begin
  XmlNode.{$IFDEF FPC}TextContent{$ELSE}Text{$ENDIF} := DOMString(Text);
end;

function XmlNextSiblingNode(XmlNode: TMustangpeakXmlNode): TMustangpeakXmlNode;
begin
  Result := XmlNode.NextSibling;
end;

function XmlAttributeCreateAndSet(XmlDoc: TMustangpeakXmlDocument; TargetNode: TMustangpeakXmlNode; Attribute, Content: string): Boolean;
{$IFDEF FPC}
var
  AttributeNode: TMustangpeakXmlAttribute;
{$ENDIF}
begin
  Result := True;
  {$IFDEF FPC}
  AttributeNode := XmlDoc.CreateAttribute(DOMString(Attribute));
  XmlNodeSetTextContent(AttributeNode, Content);
  TargetNode.Attributes.SetNamedItem(AttributeNode);
  {$ELSE}
  TargetNode.SetAttributeNS(Attribute, '', Content);
  {$ENDIF}
end;

function XmlFindRootNode(XmlDoc: TMustangpeakXmlDocument; RootName: string): TMustangpeakXmlNode;
begin
  {$IFDEF FPC}
  Result := XmlFindChildNode(XmlDoc, RootName);
  {$ELSE}
  Result := XmlDoc.ChildNodes.FindNode(RootName);
  {$ENDIF}
end;

end.

