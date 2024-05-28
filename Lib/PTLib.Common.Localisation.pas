{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Localisation                                }
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

unit PTLib.Common.Localisation;

interface

uses
  Classes, SysUtils, Variants, UITypes,

  PTLib.Common.Interfaces,
  PTLib.Common.Types,
  PTLib.Common.Classes,
  PTLib.Common.Files;

type
  TPTLibLanguageLocalisations = class;

  TPTLibMemoTranslation = class(TCollectionItem)
  strict private
    FTranslationID: String;
    FTranslation: TStringList;
  private
    procedure SetTranslation(const Value: TStringList);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property TranslationID: String read FTranslationID write FTranslationID;
    property Translation: TStringList read FTranslation write SetTranslation;
  end;
  TPTLibMemoTranslationClass = class of TPTLibMemoTranslation;

  TPTLibMemoTranslations = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPTLibMemoTranslation;
    procedure SetItem(Index: Integer; const Value: TPTLibMemoTranslation);
  public
    constructor Create(AOwner: TPersistent); virtual;

    property Items[Index: Integer]: TPTLibMemoTranslation read GetItem write SetItem; default;
  end;

  TPTLibLanguageLocalisation = class(TCollectionItem)
  private
    FLanguageID: String;
    FTranslations: TStringList;
    FMemoTranslations: TPTLibMemoTranslations;
    FTranslationOverrides: TStringList;

    procedure SetTranslations(const Value: TStringList);
    procedure SetLanguageID(const Value: String);
    procedure SetMemoTranslations(const Value: TPTLibMemoTranslations);
    procedure SetTranslationOverrides(const Value: TStringList);
  protected
    function GetDisplayName: string; override;
  public
    property TranslationOverrides: TStringList read FTranslationOverrides write SetTranslationOverrides;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function FindTranslation(const TranslationID: String; var Translation: String): Boolean;

    property LanguageID: String read FLanguageID write SetLanguageID;
    property Translations: TStringList read FTranslations write SetTranslations;
    property MemoTranslations: TPTLibMemoTranslations read FMemoTranslations write SetMemoTranslations;
  end;
  TPTLibLanguageLocalisationClass = class of TPTLibLanguageLocalisation;

  TPTLibLanguageLocalisations = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPTLibLanguageLocalisation;
    procedure SetItem(Index: Integer; const Value: TPTLibLanguageLocalisation);
  public
    constructor Create(AOwner: TPersistent); virtual;

    function FindLocalisation(const LanguageID: String; var Localisation: TPTLibLanguageLocalisation; const IgnoreInstallationID: String = ''; const UseDefault: Boolean = False): Boolean;

    property Items[Index: Integer]: TPTLibLanguageLocalisation read GetItem write SetItem; default;
  end;

  TPTLibCityLocalisation = class(TCollectionItem)
  private
    FInstallationID: String;
    FInstallationParentID: String;
    FLocalisations: TPTLibLanguageLocalisations;

    procedure SetLocalisations(const Value: TPTLibLanguageLocalisations);
    procedure SetInstallationID(const Value: String);
    procedure SetInstallationParentID(const Value: String);
  protected
    function GetDisplayName: string; override;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property InstallationID: String read FInstallationID write SetInstallationID;
    property InstallationParentID: String read FInstallationParentID write SetInstallationParentID;
    property Localisations: TPTLibLanguageLocalisations read FLocalisations write SetLocalisations;
  end;
  TPTLibLocalisationsClass = class of TPTLibCityLocalisation;

  TPTLibCityLocalisations = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPTLibCityLocalisation;
    procedure SetItem(Index: Integer; const Value: TPTLibCityLocalisation);
  public
    constructor Create(AOwner: TPersistent); virtual;

    function FindCityLocalisation(const InstallationID: String; var Localisation: TPTLibCityLocalisation; const IgnoreInstallationID: String = ''; const UseDefault: Boolean = True): Boolean; overload;
    function FindCityLocalisation(const LanguageID, InstallationID: String; var Localisation: TPTLibLanguageLocalisation; const UseDefaultLanguage: Boolean = False): Boolean; overload;

    property Items[Index: Integer]: TPTLibCityLocalisation read GetItem write SetItem; default;
  end;

  TPTLibLocaliser = class(TComponent, IPTLibLocaliser)
  private
    FLocalisations: TPTLibCityLocalisations;

    procedure SetLocalisations(const Value: TPTLibCityLocalisations);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindCityLocalisation(const LanguageID, InstallationID: String; var Localisation: TPTLibLanguageLocalisation; const IgnoreInstallationID: String = ''): Boolean;
    function FindTranslation(const TranslationID, LanguageID, InstallationID: String; var Translation: String): Boolean;
    function LocaliseText(const TranslationID, LanguageID, InstallationID: String; const ParamNames: Array of String; const ParamValues: Array of Variant; var Translation: String): Boolean; overload;
    function LocaliseText(const TranslationID, LanguageID, InstallationID: String; const Params: IParameters; var Translation: String): Boolean; overload;
    procedure GetSupportedLanguageIDs(const Values: TStrings; const InstallationID: String = '');
    function GetSupportedLanguageIDString(const InstallationID: String = ''): String;
  published
    property Localisations: TPTLibCityLocalisations read FLocalisations write SetLocalisations;
  end;

implementation

resourcestring
  StrTheItemAlreadyExists = 'The item "%s" already exists';
  StrInvalidUserMessageParams = 'Invalid User Message [%s]: ParamName (%d) and ParamValue (%d) counts do not match';
  StrInvalidLocalisation = 'Invalid Localisation InstallationID: %s';
  StrLocalisationImageNotFound = 'Localisation Image not found: %s';

{ TPTLibLocalisations }

constructor TPTLibLanguageLocalisation.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FTranslations := TStringList.Create;
  FTranslationOverrides := TStringList.Create;
  FMemoTranslations := TPTLibMemoTranslations.Create(Self);
end;

destructor TPTLibLanguageLocalisation.Destroy;
begin
  FreeAndNil(FTranslations);
  FreeAndNil(FTranslationOverrides);
  FreeAndNil(FMemoTranslations);

  inherited;
end;

function TPTLibLanguageLocalisation.GetDisplayName: string;
begin
  if FLanguageID = '' then
    Result := inherited GetDisplayName
  else
    Result := FLanguageID;
end;

function TPTLibLanguageLocalisation.FindTranslation(const TranslationID: String;
  var Translation: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Try to find an override first
  for i := 0 to pred(FTranslationOverrides.Count) do
  begin
    if AnsiSameText(FTranslationOverrides.Names[i], TranslationID) then
    begin
      Result := True;

      Translation := FTranslationOverrides.Values[FTranslationOverrides.Names[i]];
    end;
  end;

  // If we didn't find an override, try to find a translation
  if not Result then
  begin
    for i := 0 to pred(FTranslations.Count) do
    begin
      if AnsiSameText(FTranslations.Names[i], TranslationID) then
      begin
        Result := True;

        Translation := FTranslations.Values[FTranslations.Names[i]];
      end;
    end;
  end;

  // If we didn't find a translation, try to find a memo translation
  if not Result then
  begin
    for i := 0 to pred(FMemoTranslations.Count) do
      if AnsiSameText(FMemoTranslations[i].TranslationID, TranslationID) then
      begin
        Result := True;

        Translation := FMemoTranslations[i].Translation.Text;
      end;
  end;
end;

procedure TPTLibLanguageLocalisation.SetLanguageID(const Value: String);
var
  Localisation: TPTLibLanguageLocalisation;
begin
  if (Collection <> nil) and
     (Value <> '') and
     (TPTLibLanguageLocalisations(Collection).FindLocalisation(Value, Localisation, FLanguageID)) then
  begin
    raise Exception.CreateFmt(StrTheItemAlreadyExists,[Value]);
  end;

  FLanguageID := AnsiUpperCase(Value);
end;

procedure TPTLibLanguageLocalisation.SetMemoTranslations(
  const Value: TPTLibMemoTranslations);
begin
  FMemoTranslations.Assign(Value);
end;

procedure TPTLibLanguageLocalisation.SetTranslationOverrides(
  const Value: TStringList);
begin
  FTranslationOverrides.Assign(Value);
end;

procedure TPTLibLanguageLocalisation.SetTranslations(const Value: TStringList);
begin
  FTranslations.Assign(Value);
end;


{ TPTLibLocalisations }

constructor TPTLibLanguageLocalisations.Create(AOwner: TPersistent);
var
  CollectionClass: TPTLibLanguageLocalisationClass;
begin
  CollectionClass := TPTLibLanguageLocalisation;

  inherited Create(AOwner, CollectionClass);
end;

function TPTLibLanguageLocalisations.FindLocalisation(
  const LanguageID: String; var Localisation: TPTLibLanguageLocalisation;
  const IgnoreInstallationID: String; const UseDefault: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to pred(Count) do
    if (AnsiSameText(LanguageID, Items[i].LanguageID)) and
       (Items[i].LanguageID <> '') and
       (not AnsiSameText(IgnoreInstallationID, Items[i].LanguageID)) then
    begin
      Localisation := Items[i];

      Result := True;

      Break;
    end;

  if not Result then
  begin
    for i := 0 to pred(Count) do
      if (AnsiSameText(LanguageID, Items[i].LanguageID)) and
         (Items[i].LanguageID <> '') and
         (not AnsiSameText(IgnoreInstallationID, Items[i].LanguageID)) then
      begin
        Localisation := Items[i];

        Result := True;

        Break;
      end;
  end;

  if (not Result) and
     (Count > 0) and
     (UseDefault) then
  begin
    Result := True;

    Localisation := Items[0];
  end;
end;

function TPTLibLanguageLocalisations.GetItem(
  Index: Integer): TPTLibLanguageLocalisation;
begin
  Result := TPTLibLanguageLocalisation(inherited Items[Index]);
end;

procedure TPTLibLanguageLocalisations.SetItem(Index: Integer;
  const Value: TPTLibLanguageLocalisation);
begin
  inherited SetItem(Index, Value);
end;

{ TPTLibLocalisations }

constructor TPTLibCityLocalisation.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FLocalisations := TPTLibLanguageLocalisations.Create(Self);
end;

destructor TPTLibCityLocalisation.Destroy;
begin
  FreeAndNil(FLocalisations);

  inherited;
end;

function TPTLibCityLocalisation.GetDisplayName: string;
begin
  if FInstallationID = '' then
    Result := inherited GetDisplayName
  else
    Result := FInstallationID;
end;

procedure TPTLibCityLocalisation.SetInstallationID(const Value: String);
var
  Localisation: TPTLibCityLocalisation;
begin
  if (Collection <> nil) and
     (Value <> '') and
     (TPTLibCityLocalisations(Collection).FindCityLocalisation(Value, Localisation, Value, False)) then
  begin
    raise Exception.CreateFmt(StrTheItemAlreadyExists,[Value]);
  end;

  FInstallationID := Value;
end;

procedure TPTLibCityLocalisation.SetInstallationParentID(const Value: String);
begin
  (*if (Collection <> nil) and
     (Value <> '') and
     (not TPTLibCityLocalisations(Collection).FindCityLocalisation(Value, Localisation, Value, False)) then
  begin
    raise Exception.CreateFmt('The parent localisation "%s" does not exist', [Value]);
  end;*)

  FInstallationParentID := Value;
end;

procedure TPTLibCityLocalisation.SetLocalisations(
  const Value: TPTLibLanguageLocalisations);
begin
  FLocalisations.Assign(Value);
end;

{ TPTLibLocaliser }

constructor TPTLibLocaliser.Create(AOwner: TComponent);
begin
  inherited;

  FLocalisations := TPTLibCityLocalisations.Create(Self);
end;

destructor TPTLibLocaliser.Destroy;
begin
  FreeAndNil(FLocalisations);

  inherited;
end;

procedure TPTLibLocaliser.GetSupportedLanguageIDs(
  const Values: TStrings; const InstallationID: String);
var
  i, n: Integer;
begin
  for i := 0 to pred(Localisations.Count) do
    if (InstallationID = '') or
       (AnsiSameText(Localisations[i].InstallationID, InstallationID)) then
    begin
      for n := 0 to pred(Localisations[i].Localisations.Count) do
        if Values.IndexOf(AnsiUpperCase(Localisations[i].Localisations[n].LanguageID)) = -1 then
          Values.Add(AnsiUpperCase(Localisations[i].Localisations[n].LanguageID));
    end;
end;

function TPTLibLocaliser.GetSupportedLanguageIDString(
  const InstallationID: String): String;
var
  LanguageIDs: TStringList;
begin
  LanguageIDs := TStringList.Create;
  try
    GetSupportedLanguageIDs(LanguageIDs, InstallationID);

    Result := LanguageIDs.CommaText;
  finally
    FreeAndNil(LanguageIDs);
  end;
end;

procedure TPTLibLocaliser.Loaded;
begin
  inherited;
end;

function TPTLibLocaliser.LocaliseText(const TranslationID, LanguageID,
  InstallationID: String; const Params: IParameters; var Translation: String): Boolean;
var
  i: Integer;
begin
  Result := FindTranslation(
    TranslationID,
    LanguageID,
    InstallationID,
    Translation);

  if Result then
  begin
    for i := 0 to pred(Params.Count) do
    begin
      Translation := StringReplace(
        Translation,
        '%' + Params.Param(i).Name + '%',
        VarToStr(Params.Param(i).Value),
        [rfReplaceAll, rfIgnoreCase]);
    end;

    Translation := StringReplace(
      Translation,
      '\n',
      #10,
      [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TPTLibLocaliser.LocaliseText(const TranslationID, LanguageID,
  InstallationID: String; const ParamNames: array of String;
  const ParamValues: array of Variant; var Translation: String): Boolean;
var
  Params: IParameters;
  i: Integer;
begin
  if Length(ParamNames) <> Length(ParamValues) then
  begin
    raise EPTLibError.CreateFmt(StrInvalidUserMessageParams, [Length(ParamNames), Length(ParamValues)]);
  end;

  Params := TParameters.Create;

  for i := Low(ParamNames) to High(ParamValues) do
  begin
    Params.SetParam(
      ParamNames[i],
      ParamValues[i]);
  end;

  Params.SetParam('LF', ''); // #10);  { TODO : Removed due to a bug in FMX TLabel - exception if the string contains ctrl chars }

  Result := LocaliseText(
    TranslationID,
    LanguageID,
    InstallationID,
    Params,
    Translation);
end;

function TPTLibLocaliser.FindCityLocalisation(const LanguageID,
  InstallationID: String; var Localisation: TPTLibLanguageLocalisation;
  const IgnoreInstallationID: String): Boolean;
begin
  Result := FLocalisations.FindCityLocalisation(LanguageID, InstallationID, Localisation);
end;

function TPTLibLocaliser.FindTranslation(const TranslationID, LanguageID, InstallationID: String;
  var Translation: String): Boolean;
var
  Localisation: TPTLibLanguageLocalisation;
  CurrentInstallationID: String;
  CityLocalisation: TPTLibCityLocalisation;
begin
  Result := False;

  CurrentInstallationID := InstallationID;

  // Keep going until we get to the root installation ID
  while not Result do
  begin
    // Find the city localisation
    if FindCityLocalisation(LanguageID, CurrentInstallationID, Localisation, '') then
    begin
      // We found the city, now find the translation
      Result := Localisation.FindTranslation(TranslationID, Translation);
    end;

    // Did we find a translation
    if not Result then
    begin
      // No, try the parent
      if FLocalisations.FindCityLocalisation(CurrentInstallationID, CityLocalisation, '') then
      begin
        CurrentInstallationID := CityLocalisation.FInstallationParentID;
      end
      else
      begin
        // Nothing we can do, so quit now
        Exit;
      end;
    end;
  end;
end;

procedure TPTLibLocaliser.SetLocalisations(
  const Value: TPTLibCityLocalisations);
begin
  FLocalisations.Assign(Value);
end;


{ TPTLibLocalisationsCollection }

constructor TPTLibCityLocalisations.Create(AOwner: TPersistent);
var
  CollectionClass: TPTLibLocalisationsClass;
begin
  CollectionClass := TPTLibCityLocalisation;

  inherited Create(AOwner, CollectionClass);
end;

function TPTLibCityLocalisations.FindCityLocalisation(const LanguageID, InstallationID: String;
  var Localisation: TPTLibLanguageLocalisation; const UseDefaultLanguage: Boolean): Boolean;
var
  Localisations: TPTLibCityLocalisation;
begin
  Result := FindCityLocalisation(InstallationID, Localisations);

  if Result then
    Result := Localisations.Localisations.FindLocalisation(LanguageID, Localisation, '', UseDefaultLanguage);
end;

function TPTLibCityLocalisations.FindCityLocalisation(
  const InstallationID: String; var Localisation: TPTLibCityLocalisation;
  const IgnoreInstallationID: String; const UseDefault: Boolean): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to pred(Count) do
  begin
    if ((AnsiSameText(InstallationID, Items[i].InstallationID)) and
        (Items[i].InstallationID <> '') and
        (not AnsiSameText(IgnoreInstallationID, Items[i].InstallationID))) then
    begin
      Localisation := Items[i];

      Result := True;

      Break;
    end;
  end;
end;

function TPTLibCityLocalisations.GetItem(
  Index: Integer): TPTLibCityLocalisation;
begin
  Result := TPTLibCityLocalisation(inherited Items[Index]);
end;

procedure TPTLibCityLocalisations.SetItem(Index: Integer;
  const Value: TPTLibCityLocalisation);
begin
  inherited SetItem(Index, Value);
end;

{ TMemoTranslation }

constructor TPTLibMemoTranslation.Create(Collection: TCollection);
begin
  inherited;

  FTranslation := TStringList.Create;
end;

destructor TPTLibMemoTranslation.Destroy;
begin
  FreeAndNil(FTranslation);

  inherited;
end;

function TPTLibMemoTranslation.GetDisplayName: string;
begin
  if FTranslationID = '' then
    Result := inherited GetDisplayName
  else
    Result := FTranslationID;
end;

procedure TPTLibMemoTranslation.SetTranslation(const Value: TStringList);
begin
  FTranslation.Assign(Value);
end;

{ TPTLibMemoTranslations }

constructor TPTLibMemoTranslations.Create(AOwner: TPersistent);
var
  CollectionClass: TPTLibMemoTranslationClass;
begin
  CollectionClass := TPTLibMemoTranslation;

  inherited Create(AOwner, CollectionClass);
end;

function TPTLibMemoTranslations.GetItem(
  Index: Integer): TPTLibMemoTranslation;
begin
  Result := TPTLibMemoTranslation(inherited Items[Index]);
end;

procedure TPTLibMemoTranslations.SetItem(Index: Integer;
  const Value: TPTLibMemoTranslation);
begin
  inherited SetItem(Index, Value);
end;

end.
