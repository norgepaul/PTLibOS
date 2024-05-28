unit PTLib.Common.Configuration;

interface

uses
  Classes, TypInfo, SysUtils,

  PTLib.Common.Strings,
  PTLib.Common.Interfaces;

Type
  TOnStringEncryptionEvent = procedure(Sender: TObject; const Value: String; out NewValue: String) of object;

  TBasePTLibConfiguration = class(TComponent,
                                  IConfiguration)
  private
    FOnEncryptString: TOnStringEncryptionEvent;
    FOnDecryptString: TOnStringEncryptionEvent;
  protected
    FBasePath: String;
    FLastPath: String;
    FSectionDelimiter: Char;
    FReadOnly: Boolean;
    FCreateSectionPaths: Boolean;
    FRaiseExceptions: Boolean;

    procedure DoOnEncryptString(const Value: String; out NewValue: String); virtual;
    procedure DoOnDecryptString(const Value: String; out NewValue: String); virtual;

    function GetFullSectionPath(const AdditionalPath: String): String;
    function AddTrailingDelimiter(const Value: String; const Delimiter: Char): String;

    function EncryptString(const Value: String): String;
    function DecryptString(const Value: String): String;
  public
    constructor Create(AOwner: TComponent); override;

    // IConfiguration
    procedure WriteString(const SectionPath, ValueName, Value: String); virtual; abstract;
    procedure WriteEncodedString(const SectionPath, ValueName, Value: String); virtual; abstract;
    procedure WriteInteger(const SectionPath, ValueName: String; const Value: Integer); virtual; abstract;
    procedure WriteBoolean(const SectionPath, ValueName: String; const Value: Boolean); virtual; abstract;
    procedure WriteStringList(const SectionPath, ValueName: String; const Values: TStrings); virtual; abstract;
    procedure WriteEnumeratedType(const SectionPath, ValueName: String; const EnumInfo: PTypeInfo; const EnumParam: Integer); virtual; abstract;
    procedure WriteSet(const SectionPath, ValueName: String; const SetInfo: PTypeInfo; const SetParam); virtual; abstract;
    procedure WriteStream(const SectionPath, ValueName: String; const Stream: TStream); virtual; abstract;
    function ReadString(const SectionPath, ValueName, DefaultValue: String): String; virtual; abstract;
    function ReadEncodedString(const SectionPath, ValueName, DefaultValue: String): String; virtual; abstract;
    function ReadInteger(const SectionPath, ValueName: String; const DefaultValue: Integer): Integer; virtual; abstract;
    function ReadBoolean(const SectionPath, ValueName: String; const DefaultValue: Boolean): Boolean; virtual; abstract;
    procedure ReadStringList(const SectionPath, ValueName: String; const Values: TStrings); virtual; abstract;
    function ReadEnumeratedType(const SectionPath, ValueName: String; EnumInfo: PTypeInfo): Integer; virtual; abstract;
    procedure ReadSet(const SectionPath, ValueName: String; const SetInfo: PTypeInfo; var SetParam); virtual; abstract;
    function ReadStream(const SectionPath, ValueName: String; const Stream: TStream): Boolean; virtual; abstract;
    function DeleteSection(const SectionPath: String): Boolean; virtual; abstract;
    function DeleteValue(const SectionPath: String; ValueName: String): Boolean; virtual; abstract;
    function SectionPathExists(const SectionPath: String): Boolean; virtual; abstract;
    function ValueNameExists(const SectionPath, ValueName: String): Boolean; virtual; abstract;
    procedure GetValueNames(const SectionPath: String; const ValueNames: TStrings); virtual; abstract;
    procedure GetSectionNames(const SectionPath: String; const SectionNames: TStrings; IncludePath: Boolean); virtual; abstract;
    function ToText: String; virtual; abstract;
    procedure FromText(Value: String); virtual; abstract;
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    procedure LoadFromStream(AStream: TStream); virtual; abstract;

    procedure SaveToFile(Filename: String); virtual;
    procedure LoadFromFile(Filename: String); virtual;
    function AppendSection(const RootSection, AdditionalPath: String): String;
    procedure SetBaseSectionPath(const Value: String);
    function GetBaseSectionPath: String;
    procedure SetSectionDelimiter(const Value: Char);
    function GetSectionDelimiter: Char;
    procedure SetCreateSectionPaths(const Value: Boolean);
    function GetCreateSectionPaths: Boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetReadOnly: Boolean;
    procedure SetRaiseExceptions(const Value: Boolean);
    function GetRaiseExceptions: Boolean;

    procedure ReadComponents(const BaseComponent: TComponent; const SectionPath: String); virtual;
    procedure WriteComponents(const BaseComponent: TComponent; const SectionPath: String); virtual;
  published
    property OnEncryptString: TOnStringEncryptionEvent read FOnEncryptString write FOnEncryptString;
    property OnDecryptString: TOnStringEncryptionEvent read FOnDecryptString write FOnDecryptString;
  end;

const
  StringCountName = 'Count';
  StringValueName = 's-%d';
  DefaultEncKey = 'ghSgf253fh2F93SDhj8GD';

implementation

{ TBasePTLibConfiguration }

function TBasePTLibConfiguration.AddTrailingDelimiter(const Value: String; const Delimiter: Char): String;
begin
  Result := Value;

  if (Result <> '') and
     (Result[length(Result)] <> Delimiter) then
    Result := Result + Delimiter;
end;

function TBasePTLibConfiguration.AppendSection(const RootSection, AdditionalPath: String): String;
begin
  Result := concat(AddTrailingDelimiter(RootSection, FSectionDelimiter), AdditionalPath);
end;

constructor TBasePTLibConfiguration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSectionDelimiter := '\';
  FCreateSectionPaths := True;
end;

function TBasePTLibConfiguration.DecryptString(const Value: String): String;
begin
  DoOnDecryptString(
    Value,
    Result);
end;

procedure TBasePTLibConfiguration.DoOnDecryptString(const Value: String; out NewValue: String);
begin
  if Assigned(FOnDecryptString) then
    FOnDecryptString(
      Self,
      Value,
      NewValue);
end;

procedure TBasePTLibConfiguration.DoOnEncryptString(const Value: String; out NewValue: String);
begin
  if Assigned(FOnEncryptString) then
    FOnEncryptString(
      Self,
      Value,
      NewValue);
end;

function TBasePTLibConfiguration.EncryptString(const Value: String): String;
begin
  DoOnEncryptString(
    Value,
    Result);
end;

procedure TBasePTLibConfiguration.ReadComponents(const BaseComponent: TComponent; const SectionPath: String);
var
  i: Integer;
  ConfigurationProvider: IConfigurationProvider;
begin
  // Load the component
  if Supports(BaseComponent, IConfigurationProvider, ConfigurationProvider) then
  begin
    ConfigurationProvider.LoadFromConfig(
      Self,
      SectionPath);
  end;

  // Load child components
  for i := 0 to pred(BaseComponent.ComponentCount) do
  begin
    if BaseComponent.Components[i].Name <> '' then
    begin
      ReadComponents(
        BaseComponent.Components[i],
        AppendSection(
          SectionPath,
          BaseComponent.Components[i].Name));
    end;
  end;
end;

procedure TBasePTLibConfiguration.WriteComponents(const BaseComponent: TComponent; const SectionPath: String);
var
  i: Integer;
  ConfigurationProvider: IConfigurationProvider;
begin
  // Save the component
  if Supports(BaseComponent, IConfigurationProvider, ConfigurationProvider) then
  begin
    // Assert that the component name is not blank
    Assert(
      BaseComponent.Name <> '',
      format('IConfigProvider component type "%s" does not have a name - Path = SectionPath',
      [BaseComponent.ClassName,
       SectionPath]));

    ConfigurationProvider.SaveToConfig(
      Self,
      SectionPath);
  end;

  // Save child components
  for i := 0 to pred(BaseComponent.ComponentCount) do
  begin
    WriteComponents(
      BaseComponent.Components[i],
      AppendSection(
        SectionPath,
        BaseComponent.Components[i].Name));
  end;
end;

function TBasePTLibConfiguration.GetBaseSectionPath: String;
begin
  Result := FBasePath;
end;

function TBasePTLibConfiguration.GetCreateSectionPaths: Boolean;
begin
  Result := FCreateSectionPaths;
end;

function TBasePTLibConfiguration.GetFullSectionPath(
  const AdditionalPath: String): String;
begin
  Result := RemoveTrailingDelimiter(concat(AddTrailingDelimiter(FBasePath, FSectionDelimiter), AdditionalPath), FSectionDelimiter);
end;

function TBasePTLibConfiguration.GetRaiseExceptions: Boolean;
begin
  Result := FRaiseExceptions;
end;

function TBasePTLibConfiguration.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TBasePTLibConfiguration.GetSectionDelimiter: Char;
begin
  Result := FSectionDelimiter;
end;

procedure TBasePTLibConfiguration.LoadFromFile(Filename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    LoadFromStream(FileStream);
  finally
    FreeAndNil(FileStream);
  end;
end;

procedure TBasePTLibConfiguration.SaveToFile(Filename: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FreeAndNil(FileStream);
  end;
end;

procedure TBasePTLibConfiguration.SetBaseSectionPath(const Value: String);
begin
  FBasePath := Value;
end;

procedure TBasePTLibConfiguration.SetCreateSectionPaths(const Value: Boolean);
begin
  FCreateSectionPaths := Value;
end;

procedure TBasePTLibConfiguration.SetRaiseExceptions(const Value: Boolean);
begin
  FRaiseExceptions := Value;
end;

procedure TBasePTLibConfiguration.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TBasePTLibConfiguration.SetSectionDelimiter(const Value: Char);
begin
  FSectionDelimiter := Value;
end;

(*
{ TPTLibConfigurationProvider }

function TPTLibConfigurationProvider.FindComponentLink(AComponent: TComponent): TPTLibComponentLink;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(FComponentLinks.Count) do
    if FComponentLinks[i].ComponentLink = AComponent then
    begin
      Result := FComponentLinks[i];

      Break;
    end;
end;

constructor TPTLibConfigurationProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOwner := AOwner;

  FComponentLinks := TPTLibComponentLinks.Create(Self);
end;

destructor TPTLibConfigurationProvider.Destroy;
begin
  FreeAndNil(FComponentLinks);

  inherited;
end;

procedure TPTLibConfigurationProvider.DoOnAfterLoadComponentConfiguration(
  ConfigurationComponent: TComponent);
begin
  if Assigned(FOnAfterLoadComponentConfiguration) then
    FOnAfterLoadComponentConfiguration(Self, ConfigurationComponent);
end;

procedure TPTLibConfigurationProvider.DoOnBeforeLoadComponentConfiguration(
  ConfigurationComponent: TComponent);
begin
  if Assigned(FOnBeforeLoadComponentConfiguration) then
    FOnBeforeLoadComponentConfiguration(Self, ConfigurationComponent);
end;

procedure TPTLibConfigurationProvider.DoLoadConfiguration(
  const Config: IConfiguration; RootKey: String);
begin
  if Assigned(FOnLoadConfiguration) then
    FOnLoadConfiguration(Self, Config, RootKey);
end;

procedure TPTLibConfigurationProvider.DoSaveConfiguration(
  const Config: IConfiguration; RootKey: String);
begin
  if Assigned(FOnSaveConfiguration) then
    FOnSaveConfiguration(Self, Config, RootKey);
end;

procedure TPTLibConfigurationProvider.LoadConfiguration(PTLibConfiguration: IConfiguration; ComponentLink: TPTLibComponentLink);
var
  RootKey: String;
  ConfigurationProvider: IConfigurationProvider;
begin
  if Supports(ComponentLink.ComponentLink, IConfigurationProvider, ConfigurationProvider) then
  begin
    if ComponentLink.RootKey <> '' then
      RootKey := ComponentLink.RootKey
    else
      RootKey := ComponentLink.ComponentLink.Name;

    if RootKey <> '' then
    begin
      DoOnBeforeLoadComponentConfiguration(ComponentLink.ComponentLink);

      ConfigurationProvider.LoadFromConfig(PTLibConfiguration, PTLibConfiguration.AppendSection(FRootKey, RootKey));

      DoOnAfterLoadComponentConfiguration(ComponentLink.ComponentLink);
    end;
  end;
end;

procedure TPTLibConfigurationProvider.LoadConfigurationByComponentName(const ComponentName: String);
var
  i: Integer;
  PTLibConfiguration: IConfiguration;
begin
  for i := 0 to (FComponentLinks.Count) do
    if (Assigned(FComponentLinks[i].ComponentLink)) and
       (SameText(FComponentLinks[i].ComponentLink.Name, ComponentName)) then
    begin
      if Supports(FComponentLinks[i].ComponentLink, IConfiguration, PTLibConfiguration)  then
        LoadConfiguration(PTLibConfiguration, FComponentLinks[i]);

      Exit;
    end;

  Assert(True, format('Invalid configuration component name: %s', [ComponentName]));
end;

function TPTLibConfigurationProvider.LoadConfigurations(const LoadFromStore: Boolean): Boolean;

  function LoadSpecificConfiguration(Configuration: TBasePTLibConfiguration): Boolean;
  var
    i: Integer;
    PTLibConfiguration: IConfiguration;
    PTLibStoreConfiguration: IConfigurationStore;
    RootKey: String;
  begin
    Result := False;

    if Assigned(Configuration) then
    begin
      if (LoadFromStore) and (Supports(Configuration, IConfigurationStore, PTLibStoreConfiguration)) then
        Result := PTLibStoreConfiguration.LoadFromStore;

      if Supports(Configuration, IConfiguration, PTLibConfiguration) then
      begin
        for i := 0 to pred(FComponentLinks.Count) do
        begin
          if FComponentLinks[i].RootKey <> '' then
            RootKey := FComponentLinks[i].RootKey
          else
            RootKey := FComponentLinks[i].ComponentLink.Name;

          if (FComponentLinks[i].Enabled) and
             (FComponentLinks[i].Version = PTLibConfiguration.ReadInteger(PTLibConfiguration.AppendSection(FRootKey, RootKey), 'Ver', 0)) then
            LoadConfiguration(PTLibConfiguration, FComponentLinks[i]);
        end;

        DoLoadConfiguration(PTLibConfiguration, FRootKey);
      end;
    end;
  end;

begin
  Result := Assigned(FConfiguration);

  if (Result) and (LoadSpecificConfiguration(FConfiguration)) then
    Result := True;
end;

procedure TPTLibConfigurationProvider.SaveConfigurations(const SaveToStore: Boolean);

  procedure SaveConfiguration(Configuration: TBasePTLibConfiguration);
  var
    i: Integer;
    ConfigurationProvider: IConfigurationProvider;
    PTLibConfiguration: IConfiguration;
    PTLibStoreConfiguration: IConfigurationStore;
    RootKey: String;
  begin
    if Assigned(Configuration) then
    begin
      if Supports(Configuration, IConfiguration, PTLibConfiguration) then
      begin
        for i := 0 to pred(FComponentLinks.Count) do
          if (FComponentLinks[i].Enabled) and
             (Supports(FComponentLinks[i].ComponentLink, IConfigurationProvider, ConfigurationProvider)) then
          begin
            if FComponentLinks[i].RootKey <> '' then
              RootKey := FComponentLinks[i].RootKey
            else
              RootKey := FComponentLinks[i].ComponentLink.Name;

            if RootKey <> '' then
            begin
              ConfigurationProvider.SaveToConfig(PTLibConfiguration, PTLibConfiguration.AppendSection(FRootKey, RootKey));

              if FComponentLinks[i].Version <> 0 then
                PTLibConfiguration.WriteInteger(PTLibConfiguration.AppendSection(FRootKey, RootKey), 'Ver', FComponentLinks[i].Version);
            end;
          end;
      end;

      DoSaveConfiguration(PTLibConfiguration, FRootKey);

      if (SaveToStore) and (Supports(Configuration, IConfigurationStore, PTLibStoreConfiguration)) then
        PTLibStoreConfiguration.SaveToStore;
    end;
  end;

begin
  if Assigned(FConfiguration) then
    SaveConfiguration(FConfiguration);
end;

procedure TPTLibConfigurationProvider.SetComponentLinks(
  const Value: TPTLibComponentLinks);
begin
  FComponentLinks.Assign(Value);
end;

procedure TPTLibConfigurationProvider.SetDefaultConfigurationProvider(
  const Value: Boolean);
begin
  FDefaultConfigurationProvider := Value;
end;

{ TPTLibComponentLink }

constructor TPTLibComponentLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FVersion := 0;
  FEnabled := True;
end;

function TPTLibComponentLink.GetDisplayName: string;
begin
  if Assigned(FComponentLink) then
    Result := concat('Link - ', FComponentLink.Name)
  else
    Result := '<Not assigned>';
end;

procedure TPTLibComponentLink.SetComponentLink(const Value: TComponent);
var
  ConfigurationProvider: IConfigurationProvider;
begin
  if (Assigned(Value)) and (not Supports(Value, IConfigurationProvider, ConfigurationProvider)) then
    raise Exception.CreateFmt('%s does not support the IConfigurationProvider interface.', [Value.ClassName]);

  FComponentLink := Value;

  if (FRootKey = '') and (Assigned(FComponentLink)) then
    FRootKey := FComponentLink.Name;
end;

{ TPTLibComponentLinks }

function TPTLibComponentLinks.Add: TPTLibComponentLink;
begin
  Result := TPTLibComponentLink(inherited Add);
end;

constructor TPTLibComponentLinks.Create(AOwner: TComponent);
begin
  inherited Create(TPTLibComponentLink);

  FOwner := AOwner;
end;

function TPTLibComponentLinks.GetItem(Index: Integer): TPTLibComponentLink;
begin
  Result := TPTLibComponentLink(inherited GetItem(Index));
end;

function TPTLibComponentLinks.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TPTLibConfigurationProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if AComponent = FConfiguration then
      FConfiguration := nil else

    if Assigned(FComponentLinks) then
      for i := pred(FComponentLinks.Count) downto 0 do
        if FComponentLinks.Items[i].ComponentLink = AComponent then
        begin
          FComponentLinks.Delete(i);

          Break;
        end;
  end;
end;

procedure TPTLibComponentLinks.SetItem(Index: Integer; const Value: TPTLibComponentLink);
begin
  inherited SetItem(Index, Value);
end;

procedure TPTLibComponentLinks.Update(Item: TCollectionItem);
begin

  inherited Update(Item);
end;
    *)
end.


