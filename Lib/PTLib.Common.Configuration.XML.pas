{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Configuration.XML                           }
{                                                                    }
{           Copyright (c) 1998-2016 Easy-IP AS.                      }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF EASY-IP AS. THE REGISTERED DEVELOPER                  }
{   LICENSED TO DISTRIBUTE THE CODE HEREIN AND ANY ACCOMPANYING      }
{   VCL OR FMX CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.       }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM EASY-IP AS.                                  }
{                                                                    }
{********************************************************************}

unit PTLib.Common.Configuration.XML;

interface

uses
  Classes, SysUtils, Windows, TypInfo,

  { TODO : Fix }
  // AV when installing if we use OnmiXML!
  (*OmniXML, OmniXML_Types, OmniXMLUtils,*)
  XMLIntf,

  PTLib.Common.Configuration,
  PTLib.Common.Crypto,
  PTLib.Common.TypeInfo,
  PTLib.Common.Streams,
  PTLib.Common.Interfaces;

Type
  IXMLConfiguration = interface(IConfiguration)
    procedure SetXMLDoc(Value: IXMLDocument);
    function GetXMLDoc: IXMLDocument;
  end;

  TPTLibXMLConfiguration = class(TBasePTLibConfiguration,
                                     IXMLConfiguration)
  private
    FInternalXMLDoc: IXMLDocument;
    FXMLDoc: IXMLDocument;
    FSectionNode: IXMLNode;
    FLastCanCreate: Boolean;

    function SetSectionNode(const AdditionalPath: String; CanCreate: Boolean = False; RaiseError: Boolean = False): Boolean;
    function FindXMLNodeFromPath(const Path: String; CreateNode: Boolean): IXMLNode;
    function GetValueNode(const SectionPath, ValueName: String; CanCreate, DeleteExisting: Boolean): IXMLNode;
    procedure WriteValue(const SectionPath, ValueName, AttributeName: String; Value: Variant);
    function ReadValue(const SectionPath, ValueName, AttributeName: String; DefaultValue: Variant): Variant;
    procedure GetSectionsOrValues(const SectionPath: String; const ValueNames: TStrings; GetSections: Boolean; IncludePath: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;

    procedure WriteString(const SectionPath, ValueName, Value: String); override;
    procedure WriteEncodedString(const SectionPath, ValueName, Value: String); override;
    procedure WriteInteger(const SectionPath, ValueName: String; const Value: Integer); override;
    procedure WriteBoolean(const SectionPath, ValueName: String; const Value: Boolean); override;
    procedure WriteStringList(const SectionPath, ValueName: String; const Values: TStrings); override;
    procedure WriteEnumeratedType(const SectionPath, ValueName: String; const EnumInfo: PTypeInfo; const EnumParam: Integer); override;
    procedure WriteSet(const SectionPath, ValueName: String; const SetInfo: PTypeInfo; const SetParam); override;
    procedure WriteStream(const SectionPath, ValueName: String; const Stream: TStream); override;

    function ReadString(const SectionPath, ValueName, DefaultValue: String): String; override;
    function ReadEncodedString(const SectionPath, ValueName, DefaultValue: String): String; override;
    function ReadInteger(const SectionPath, ValueName: String; const DefaultValue: Integer): Integer; override;
    function ReadBoolean(const SectionPath, ValueName: String; const DefaultValue: Boolean): Boolean; override;
    procedure ReadStringList(const SectionPath, ValueName: String; const Values: TStrings); override;
    function ReadEnumeratedType(const SectionPath, ValueName: String; EnumInfo: PTypeInfo): Integer; override;
    procedure ReadSet(const SectionPath, ValueName: String; const SetInfo: PTypeInfo; var SetParam); override;
    function ReadStream(const SectionPath, ValueName: String; const Stream: TStream): Boolean; override;

    function DeleteSection(const SectionPath: String): Boolean; override;
    function DeleteValue(const SectionPath: String; ValueName: String): Boolean; override;

    function SectionPathExists(const SectionPath: String): Boolean; override;
    function ValueNameExists(const SectionPath, ValueName: String): Boolean; override;

    procedure GetValueNames(const SectionPath: String; const ValueNames: TStrings); override;
    procedure GetSectionNames(const SectionPath: String; const SectionNames: TStrings; IncludePath: Boolean); override;

    function ToText: String; override;
    procedure FromText(Value: String); override;
    procedure SaveToFile(Filename: String); override;
    procedure LoadFromFile(Filename: String); override;
    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;

    procedure SetXMLDoc(Value: IXMLDocument);
    function GetXMLDoc: IXMLDocument;
  end;


(*procedure SaveToXMLFile(const Filename: String; ConfigurationProvider: IConfigurationProvider; FormatXML: Boolean);
function SaveToXML(ConfigurationProvider: IConfigurationProvider; FormatXML: Boolean): String;
procedure LoadFromXMLFile(const Filename: String; ConfigurationProvider: IConfigurationProvider);
procedure LoadFromXML(const XML: String; ConfigurationProvider: IConfigurationProvider);*)

implementation

uses
  PTLib.Common.Files,
  PTLib.Common.Strings;

const
  AttrStringName = 'String';
  AttrValueName = 'Value';
  AttrIntegerName = 'Int';
  AttrBooleanName = 'Bool';
  AttrStreamName = 'Stream';
  AttrEncStringName = 'EncString';
  NodeStringListName = 'Strings';
  SectionTypeName = 'Type';
  SectionType = 'section';

resourcestring
  StrAnErrorOccurredT = 'An error occurred: This is not a valid configuration file.';

{ TODO : Fix }
(*
function GetAttributeNode(const Node: IXMLNode; const Name: XmlString): IXMLAttr;
var
  i: Integer;
begin
  Result := nil;
  if Node.Attributes <> nil then
  begin
    i := 0;

    while i < Node.Attributes.Length do
    begin
      if Node.Attributes.Item[i].NodeName = Name then
      begin
        Result := Node.Attributes.Item[i] as IXMLAttr;

        Break;
      end;

      Inc(i);
    end;
  end;
end;

function AddChild(const XMLNode: IXMLNode; const aTagName: XmlString): IXMLElement;
begin
  Result := XMLNode.AppendChild(XMLNode.OwnerDocument.CreateElement(aTagName)) as IXMLElement;
end;

function FindNode(const NodeList: IXMLNodeList; const aNodeName: XmlString): IXMLNode;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(NodeList.GetLength) do
    if NodeList.GetItem(i).NodeName = aNodeName then
      Exit(NodeList.GetItem(i));
end; *)


{ TPTLibXMLConfiguration }

constructor TPTLibXMLConfiguration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Reset;
end;

procedure TPTLibXMLConfiguration.Reset;
begin
  if Assigned(FXMLDoc) then
    FXMLDoc := nil;

  { TODO : Fix }
//  FXMLDoc := CreateXMLDoc;
//  FXMLDoc.AppendChild(FXMLDoc.CreateProcessingInstruction('xml', 'version="1.0" encoding="UTF-8" standalone="yes"'));
//  FXMLDoc.DocumentElement := FXMLDoc.CreateElement('ShareBike');

  FLastPath := '';
end;

function TPTLibXMLConfiguration.DeleteSection(
  const SectionPath: String): Boolean;
begin
  Result := SetSectionNode(SectionPath, False);

  if Result then
    FSectionNode.ParentNode.ChildNodes.Remove(FSectionNode);
end;

function TPTLibXMLConfiguration.DeleteValue(const SectionPath: String;
  ValueName: String): Boolean;
var
  Node: IXMLNode;
begin
  Node := GetValueNode(SectionPath, ValueName, False, True);

  Result := Assigned(Node);

  if Result then
    Node.ParentNode.ChildNodes.Remove(Node);
end;

destructor TPTLibXMLConfiguration.Destroy;
begin

  inherited;
end;

procedure TPTLibXMLConfiguration.GetSectionNames(
  const SectionPath: String; const SectionNames: TStrings; IncludePath: Boolean);
begin
  GetSectionsOrValues(SectionPath, SectionNames, True, IncludePath);
end;

procedure TPTLibXMLConfiguration.GetSectionsOrValues(const SectionPath: String;
  const ValueNames: TStrings; GetSections: Boolean; IncludePath: Boolean);
var
  Node: IXMLNode;
begin
  ValueNames.Clear;

  SetSectionNode(SectionPath, False);

  if Assigned(FSectionNode) then
  begin
    { TODO : Fix }
    //Node := FSectionNode.Getfirstchild;

    while Assigned(Node) do
    begin
      { TODO : Fix }
//      if ((GetSections) and (GetAttributeNode(Node, SectionTypeName) <> nil) and (Node.Attributes.GetNamedItem(SectionTypeName).NodeValue = SectionType)) or
//         ((not GetSections) and (GetAttributeNode(Node, SectionTypeName) = nil)) then
      begin
        if IncludePath then
          ValueNames.Add(AppendSection(SectionPath, Node.NodeName))
        else
          ValueNames.Add(Node.NodeName);
      end;

      Node := Node.NextSibling;
    end;
  end;
end;

procedure TPTLibXMLConfiguration.GetValueNames(const SectionPath: String;
  const ValueNames: TStrings);
begin
  GetSectionsOrValues(SectionPath, Valuenames, False, False);
end;

function TPTLibXMLConfiguration.GetXMLDoc: IXMLDocument;
begin
  Result := FXMLDoc;
end;

procedure TPTLibXMLConfiguration.LoadFromFile(Filename: String);
begin
  if FileExists(Filename) then
  begin
    Reset;

    { TODO : Fix }
    //FXMLDoc.Load(Filename);
  end;
end;

procedure TPTLibXMLConfiguration.LoadFromStream(AStream: TStream);
begin
  FXMLDoc.LoadFromStream(AStream);
end;

function TPTLibXMLConfiguration.ReadBoolean(const SectionPath,
  ValueName: String; const DefaultValue: Boolean): Boolean;
begin
  Result := ReadValue(SectionPath, ValueName, AttrBooleanName, DefaultValue);
end;

function TPTLibXMLConfiguration.ReadEncodedString(const SectionPath,
  ValueName, DefaultValue: String): String;
begin
  Result := DecryptString(ReadValue(SectionPath, ValueName, AttrEncStringName, DefaultValue));
end;

function TPTLibXMLConfiguration.ReadEnumeratedType(const SectionPath,
  ValueName: String; EnumInfo: PTypeInfo): Integer;
begin
  Result := GetEnumValue(EnumInfo, ReadValue(SectionPath, ValueName, String(EnumInfo.Name), ''));

  if Result < 0 then
    Result := 0;
end;

function TPTLibXMLConfiguration.ReadInteger(const SectionPath,
  ValueName: String; const DefaultValue: Integer): Integer;
begin
  Result := ReadValue(SectionPath, ValueName, AttrIntegerName, DefaultValue);
end;

procedure TPTLibXMLConfiguration.ReadSet(const SectionPath,
  ValueName: String; const SetInfo: PTypeInfo; var SetParam);
begin
  if ValueNameExists(SectionPath, ValueName) then
    StringToSetEx(SetInfo, SetParam, ReadValue(SectionPath, ValueName, String(SetInfo.Name), ''));
end;

function TPTLibXMLConfiguration.ReadValue(const SectionPath, ValueName, AttributeName: String;
  DefaultValue: Variant): Variant;
begin
  SetSectionNode(SectionPath, False);

  if Assigned(FSectionNode) then
  begin
    { TODO : Fix }
    //ValueNode := FindNode(FSectionNode.ChildNodes, ValueName);

    //if (Assigned(ValueNode)) and (GetAttributeNode(ValueNode, AttributeName) <> nil) then
    //  Exit(ValueNode.Attributes.GetNamedItem(AttributeName).NodeValue);
  end;

  // If we didn't find anything, use the default
  Result := DefaultValue;
end;

function TPTLibXMLConfiguration.ReadStream(const SectionPath,
  ValueName: String; const Stream: TStream): Boolean;
var
  InString: String;
begin
  InString := ReadValue(SectionPath, ValueName, AttrStreamName, '');

  Result := StringToStream(InString, Stream);
end;

function TPTLibXMLConfiguration.ReadString(const SectionPath, ValueName,
  DefaultValue: String): String;
begin
  Result := ReadValue(SectionPath, ValueName, AttrStringName, DefaultValue);
end;

procedure TPTLibXMLConfiguration.ReadStringList(const SectionPath,
  ValueName: String; const Values: TStrings);
var
  StringSectionPath: String;
begin
  Values.Clear;

  StringSectionPath := concat(AddTrailingDelimiter(SectionPath, FSectionDelimiter), AddTrailingDelimiter(ValueName, FSectionDelimiter), NodeStringListName);

  { TODO : Fix }
  (*if (SetSectionNode(StringSectionPath, False)) and
     (GetAttributeNode(FSectionNode, StringCountName) <> nil) then
  begin
    for i := 0 to pred(StrToInt(FSectionNode.Attributes.GetNamedItem(StringCountName).NodeValue)) do
    begin
      ValueNode := GetValueNode(StringSectionPath, format('s-%d', [i]), False, False);

      if Assigned(ValueNode) then
      begin
         if GetAttributeNode(ValueNode, AttrStringName) <> nil then
         begin
           Values.Add(ValueNode.Attributes.GetNamedItem(AttrStringName).NodeValue);

           if GetAttributeNode(ValueNode, AttrValueName) <> nil then
             Values.Objects[Values.Count - 1] := TObject(StrToInt(ValueNode.Attributes.GetNamedItem(AttrValueName).NodeValue));
         end;
      end;
    end;
  end;  *)
end;

procedure TPTLibXMLConfiguration.SaveToFile(Filename: String);
var
  XMLStrings: TStringList;
begin
  XMLStrings := TStringList.Create;
  try
    // Format the XML when saving to a file.
    { TODO : Fix }
    //XMLStrings.Text := FXMLDoc.XML; //FormatXMLData(FXMLDoc.XML.Text);
    ForceDirectories(ExtractFileDir(Filename));
    XMLStrings.SaveToFile(Filename, TEncoding.UTF8);
  finally
    FreeAndNil(XMLStrings);
  end;
end;

procedure TPTLibXMLConfiguration.SaveToStream(AStream: TStream);
begin
  FXMLDoc.SaveToStream(AStream);
end;

function TPTLibXMLConfiguration.SectionPathExists(
  const SectionPath: String): Boolean;
begin
  Result := SetSectionNode(SectionPath, False);
end;

procedure TPTLibXMLConfiguration.SetXMLDoc(Value: IXMLDocument);
begin
  if Assigned(Value) then
    FXMLDoc := Value
  else
    FXMLDoc := FInternalXMLDoc;
end;

function TPTLibXMLConfiguration.ToText: String;
begin
  { TODO : Fix }
  //Result := FXMLDoc.XML;
end;

function TPTLibXMLConfiguration.SetSectionNode(const AdditionalPath: String;
  CanCreate: Boolean = False; RaiseError: Boolean = False): Boolean;
begin
  if (GetFullSectionPath(AdditionalPath) <> FLastPath) or (FLastCanCreate <> CanCreate) then
  begin
    FSectionNode := FindXMLNodeFromPath(GetFullSectionPath(AdditionalPath), CanCreate);

    FLastPath := GetFullSectionPath(AdditionalPath);
    FLastCanCreate := CanCreate;

    if not Assigned(FSectionNode) and (RaiseError) then
      raise Exception.CreateFmt('Missing node "%s"', [FLastPath]);
  end;

  Result := Assigned(FSectionNode);
end;

function TPTLibXMLConfiguration.FindXMLNodeFromPath(const Path: String; CreateNode: Boolean): IXMLNode;
var
  Block, XMLPath: String;
begin
  Result := FXMLDoc.DocumentElement;

  XMLPath := Path;
  Block := NextBlock(XMLPath, FSectionDelimiter);

  while Block <> '' do
  begin
    { TODO : Fix }
    (*Node := FindNode(Result.ChildNodes, Block);

    if Assigned(Node) then
      Result := Node
    else
    begin
      if CreateNode then
      begin
        Result := AddChild(Result, Block);

        SetNodeAttr(Result, SectionTypeName, SectionType);
      end
      else
        Exit(nil);
    end;  *)

    Block := NextBlock(XMLPath, FSectionDelimiter);
  end;
end;

procedure TPTLibXMLConfiguration.FromText(Value: String);
begin
  { TODO : Fix }
  //FXMLDoc.LoadXML(Value);
end;

function TPTLibXMLConfiguration.ValueNameExists(const SectionPath,
  ValueName: String): Boolean;
begin
  Result := GetValueNode(SectionPath, ValueName, False, False) <> nil;
end;

procedure TPTLibXMLConfiguration.WriteBoolean(const SectionPath,
  ValueName: String; const Value: Boolean);
begin
  WriteValue(SectionPath, ValueName, AttrBooleanName, Value);
end;

procedure TPTLibXMLConfiguration.WriteEncodedString(const SectionPath,
  ValueName, Value: String);
begin
  WriteValue(
    SectionPath,
    ValueName,
    AttrEncStringName,
    EncryptString(Value));
end;

procedure TPTLibXMLConfiguration.WriteEnumeratedType(const SectionPath,
  ValueName: String; const EnumInfo: PTypeInfo; const EnumParam: Integer);
begin
  WriteValue(SectionPath, ValueName, String(EnumInfo.Name), GetEnumName(EnumInfo, EnumParam));
end;

procedure TPTLibXMLConfiguration.WriteInteger(const SectionPath,
  ValueName: String; const Value: Integer);
begin
  WriteValue(SectionPath, ValueName, AttrIntegerName, Value);
end;

procedure TPTLibXMLConfiguration.WriteSet(const SectionPath,
  ValueName: String; const SetInfo: PTypeInfo; const SetParam);
begin
  WriteValue(SectionPath, ValueName, String(SetInfo.Name), SetToStringEx(SetInfo, SetParam, True));
end;

function TPTLibXMLConfiguration.GetValueNode(const SectionPath, ValueName: String; CanCreate, DeleteExisting: Boolean): IXMLNode;
begin
  Result := nil;

  SetSectionNode(SectionPath, CanCreate);

  { TODO : Fix }
  (*if Assigned(FSectionNode) then
  begin
    Result := FindNode(FSectionNode.ChildNodes, ValueName);

    if DeleteExisting and (Assigned(Result))then
      Result.Attributes.Clear;

    if CanCreate and (not Assigned(Result))  then
      Result := AddChild(FSectionNode, ValueName);
  end;*)
end;

procedure TPTLibXMLConfiguration.WriteValue(const SectionPath, ValueName,
  AttributeName: String; Value: Variant);
var
  ValueNode: IXMLNode;
begin
  ValueNode := GetValueNode(SectionPath, ValueName, True, True);

  { TODO : Fix }
  //SetNodeAttr(ValueNode, AttributeName, Value);
end;

procedure TPTLibXMLConfiguration.WriteStream(const SectionPath,
  ValueName: String; const Stream: TStream);
begin
  WriteValue(SectionPath, ValueName, AttrStreamName, StreamToString(Stream));
end;

procedure TPTLibXMLConfiguration.WriteString(const SectionPath, ValueName,
  Value: String);
begin
  WriteValue(SectionPath, ValueName, AttrStringName, Value);
end;

procedure TPTLibXMLConfiguration.WriteStringList(const SectionPath,
  ValueName: String; const Values: TStrings);
var
  StringSectionPath: String;
begin
  StringSectionPath := concat(AddTrailingDelimiter(SectionPath, FSectionDelimiter), AddTrailingDelimiter(ValueName, FSectionDelimiter), NodeStringListName);

  if SetSectionNode(StringSectionPath, True) then
  begin
    // Add the string count as an attribute
    { TODO : Fix }
    (*SetNodeAttr(FSectionNode, StringCountName, IntToStr(Values.Count));

    // Add the strings as child nodes
    for i := 0 to pred(Values.Count) do
    begin
      ValueNode := GetValueNode(StringSectionPath, format(StringValueName, [i]), True, True);

      SetNodeAttr(ValueNode, AttrStringName, Values[i]);

      if Values.Objects[i] <> TObject(0) then
        SetNodeAttr(ValueNode, AttrValueName, IntToStr(Integer(Values.Objects[i])));
    end; *)
  end;
end;

end.
