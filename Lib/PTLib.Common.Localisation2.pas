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

unit PTLib.Common.Localisation2;

interface

uses
  Classes, SysUtils, Variants, UITypes,

  PTLib.Common.Interfaces,
  PTLib.Common.Types,
  PTLib.Common.Classes,
  PTLib.Common.Files;

type
  TPTLibLanguageLocalisations = class;

  TBasePTLibLocalisationImage = class(TCollectionItem)
  protected
    FFilename: TFilename;
  public
    procedure SaveToStream(const AStream: TStream; const Scale: Single = 1);
  published
    property Filename: TFilename read FFilename write FFilename;
  end;

  TPTLibLocalisationImage = class(TBasePTLibLocalisationImage)
  private
    FImageID: String;

    procedure SetImageID(const Value: String);
  protected
    function GetDisplayName: string; override;
  public

  published
    property ImageID: String read FImageID write SetImageID;
  end;
  TPTLibLocalisationImageClass = class of TPTLibLocalisationImage;

  TPTLibLocalisationImages = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPTLibLocalisationImage;
    procedure SetItem(Index: Integer; const Value: TPTLibLocalisationImage);
  public
    constructor Create(AOwner: TPersistent); virtual;

    function FindImage(const ImageID: String; const AImage: TStream; const IgnoreImageID: String = ''; const Scale: Single = 1): Boolean;

    property Items[Index: Integer]: TPTLibLocalisationImage read GetItem write SetItem; default;
  end;

  TPTLibVideoLocalisation = class(TCollectionItem)
  private
    FVideoID: String;
    FFilename: TFilename;
  protected
    function GetDisplayName: string; override;
  published
    constructor Create(Collection: TCollection); override;

    property VideoID: String read FVideoID write FVideoID;
    property Filename: TFilename read FFilename write FFilename;
  end;
  TPTLibVideoLocalisationClass = class of TPTLibVideoLocalisation;

  TPTLibVideoLocalisations = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPTLibVideoLocalisation;
    procedure SetItem(Index: Integer; const Value: TPTLibVideoLocalisation);
  public
    constructor Create(AOwner: TPersistent); virtual;

    function FindVideoFilename(const VideoID: String; var Filename: String; const IgnoreInstallationID: String = ''): Boolean;

    property Items[Index: Integer]: TPTLibVideoLocalisation read GetItem write SetItem; default;
  end;

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
    FVideoLocalisations: TPTLibVideoLocalisations;
    FImages: TPTLibLocalisationImages;
    FDefault: Boolean;

    procedure SetLocalisations(const Value: TPTLibLanguageLocalisations);
    procedure SetInstallationID(const Value: String);
    procedure SetImages(const Value: TPTLibLocalisationImages);
    procedure SetDefault(const Value: Boolean);
    procedure SetVideoLocalisations(const Value: TPTLibVideoLocalisations);
    procedure SetInstallationParentID(const Value: String);
  protected
    function GetDisplayName: string; override;
  published
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property InstallationID: String read FInstallationID write SetInstallationID;
    property ParentInstallationID: String read FInstallationParentID write SetInstallationParentID;
    property Localisations: TPTLibLanguageLocalisations read FLocalisations write SetLocalisations;
    property Videos: TPTLibVideoLocalisations read FVideoLocalisations write SetVideoLocalisations;
    property Images: TPTLibLocalisationImages read FImages write SetImages;
    property Default: Boolean read FDefault write SetDefault;
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

  TPTLibDefaultLocalisations = class(TPersistent)
  private
    FLocalisations: TPTLibLanguageLocalisations;
    FImages: TPTLibLocalisationImages;

    procedure SetLocalisations(const Value: TPTLibLanguageLocalisations);
    procedure SetImages(const Value: TPTLibLocalisationImages);
  published
    constructor Create; virtual;
    destructor Destroy; override;

    property Localisations: TPTLibLanguageLocalisations read FLocalisations write SetLocalisations;
    property Images: TPTLibLocalisationImages read FImages write SetImages;
  end;

  [ComponentPlatformsAttribute(pidAllPlatforms)]
  TPTLibLocaliser2 = class(TComponent, IPTLibLocaliser)
  private
    FLocalisations: TPTLibCityLocalisations;

    procedure SetLocalisations(const Value: TPTLibCityLocalisations);
    function GetDefaultLocalisations: TPTLibCityLocalisation;
    function FindImage(const ImageID: String; const InstallationID: String; const AImageStream: TStream; const Scale: Single = 1): Boolean;
    function GetDefaultInstallationID: String;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FindCityLocalisation(const LanguageID, InstallationID: String; var Localisation: TPTLibLanguageLocalisation; const IgnoreInstallationID: String = ''; const UseDefaultLanguage: Boolean = False): Boolean;
    function FindTranslation(const TranslationID, LanguageID, InstallationID: String; var Translation: String; const UseDefaultLanguage: Boolean = False): Boolean;
    function LocaliseText(const TranslationID, LanguageID, InstallationID: String; const ParamNames: Array of String; const ParamValues: Array of Variant; const UseDefaultLanguage: Boolean; var Translation: String): Boolean; overload;
    function LocaliseText(const TranslationID, LanguageID, InstallationID: String; const Params: IParameters; const UseDefaultLanguage: Boolean; var Translation: String): Boolean; overload;
    function FindVideo(const VideoID, InstallationID: String; var Filename: String): Boolean;
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
  FVideoLocalisations := TPTLibVideoLocalisations.Create(Self);
  FImages := TPTLibLocalisationImages.Create(Self);
end;

destructor TPTLibCityLocalisation.Destroy;
begin
  FreeAndNil(FLocalisations);
  FreeAndNil(FVideoLocalisations);
  FreeAndNil(FImages);

  inherited;
end;

function TPTLibCityLocalisation.GetDisplayName: string;
begin
  if FInstallationID = '' then
    Result := inherited GetDisplayName
  else
    Result := FInstallationID;
end;

procedure TPTLibCityLocalisation.SetImages(
  const Value: TPTLibLocalisationImages);
begin
  FImages.Assign(Value);
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
var
  Localisation: TPTLibCityLocalisation;
begin
  (*if (Collection <> nil) and
     (Value <> '') and
     (not TPTLibCityLocalisations(Collection).FindCityLocalisation(Value, Localisation, Value, False)) then
  begin
    raise Exception.CreateFmt('The parent localisation "%s" does not exist', [Value]);
  end;*)

  FInstallationParentID := Value;
end;

procedure TPTLibCityLocalisation.SetDefault(const Value: Boolean);
var
  i: Integer;
begin
  if Value then
  begin
    for i := 0 to pred(Collection.Count) do
      TPTLibCityLocalisations(Collection).Items[i].Default := False;
  end;

  FDefault := Value;
end;

procedure TPTLibCityLocalisation.SetLocalisations(
  const Value: TPTLibLanguageLocalisations);
begin
  FLocalisations.Assign(Value);
end;

procedure TPTLibCityLocalisation.SetVideoLocalisations(
  const Value: TPTLibVideoLocalisations);
begin
  FVideoLocalisations.Assign(Value);
end;

{ TPTLibLocaliser }

constructor TPTLibLocaliser2.Create(AOwner: TComponent);
begin
  inherited;

  FLocalisations := TPTLibCityLocalisations.Create(Self);
end;

destructor TPTLibLocaliser2.Destroy;
begin
  FreeAndNil(FLocalisations);

  inherited;
end;

function TPTLibLocaliser2.GetDefaultLocalisations: TPTLibCityLocalisation;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to pred(FLocalisations.Count) do
    if FLocalisations[i].Default then
    begin
      Result := FLocalisations[i];

      Break;
    end;
end;

procedure TPTLibLocaliser2.GetSupportedLanguageIDs(
  const Values: TStrings; const InstallationID: String);
var
  i, n: Integer;
begin
  for i := 0 to pred(Localisations.Count) do
    if (Localisations[i].Default) or
       (InstallationID = '') or
       (AnsiSameText(Localisations[i].InstallationID, InstallationID)) then
    begin
      for n := 0 to pred(Localisations[i].Localisations.Count) do
        if Values.IndexOf(AnsiUpperCase(Localisations[i].Localisations[n].LanguageID)) = -1 then
          Values.Add(AnsiUpperCase(Localisations[i].Localisations[n].LanguageID));
    end;
end;

function TPTLibLocaliser2.GetSupportedLanguageIDString(
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

procedure TPTLibLocaliser2.Loaded;
begin
  inherited;
end;

function TPTLibLocaliser2.LocaliseText(const TranslationID, LanguageID,
  InstallationID: String; const Params: IParameters; const UseDefaultLanguage: Boolean;
  var Translation: String): Boolean;
var
  Localisation: TPTLibLanguageLocalisation;
  i: Integer;
begin
  Result := False;

  FindCityLocalisation(
    LanguageID,
    InstallationID,
    Localisation,
    '',
    False);

  if (Localisation = nil) and
     (UseDefaultLanguage) then
  begin
    FindCityLocalisation(
      LanguageID,
      InstallationID,
      Localisation,
      '',
      True);
  end;

  if Localisation = nil then
  begin
    raise EPTLibError.CreateFmt(StrInvalidLocalisation, [InstallationID]);
  end
  else
  begin
    if Localisation.FindTranslation(TranslationID, Translation) then
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

      Result := True;
    end else
    if InstallationID <> GetDefaultInstallationID then
    begin
      Result := LocaliseText(
        TranslationID,
        LanguageID,
        GetDefaultInstallationID,
        Params,
        UseDefaultLanguage,
        Translation);
    end;
  end;
end;

function TPTLibLocaliser2.GetDefaultInstallationID: String;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to pred(FLocalisations.Count) do
  begin
    if FLocalisations[i].Default then
    begin
      Result := FLocalisations[i].InstallationID;
    end;
  end;
end;

function TPTLibLocaliser2.LocaliseText(const TranslationID, LanguageID,
  InstallationID: String; const ParamNames: array of String;
  const ParamValues: array of Variant; const UseDefaultLanguage: Boolean; var Translation: String): Boolean;
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

(*    LocaliseText(
      TranslationID,
      LanguageID,
      InstallationID,
      Params,
      UseDefaultLanguage,
      Translation);*)
  end;

  Params.SetParam('LF', ''); // #10);  { TODO : Removed due to a bug in FMX TLabel - exception if the string contains ctrl chars }

  Result := LocaliseText(
    TranslationID,
    LanguageID,
    InstallationID,
    Params,
    UseDefaultLanguage,
    Translation);
end;

(*function TPTLibLocaliser2.FindImage(const ImageID: String;
  const InstallationID: String; var AImage: TPicture): Boolean;
var
  Localisation: TPTLibCityLocalisation;
begin
  Result := FLocalisations.FindCityLocalisation(InstallationID, Localisation);

  if Result then
    Result := Localisation.Images.FindImage(ImageID, AImage);

  if not Result then
    Result := (GetDefaultLocalisations <> nil) and
              (GetDefaultLocalisations.Images.FindImage(ImageID, AImage));

  Result := (Result) and (AImage <> nil)
end; *)

function TPTLibLocaliser2.FindCityLocalisation(const LanguageID,
  InstallationID: String; var Localisation: TPTLibLanguageLocalisation;
  const IgnoreInstallationID: String; const UseDefaultLanguage: Boolean): Boolean;
begin
  Result := (FLocalisations.FindCityLocalisation(LanguageID, InstallationID, Localisation, UseDefaultLanguage)) or
            ((GetDefaultLocalisations <> nil) and
             (GetDefaultLocalisations.Localisations.FindLocalisation(LanguageID, Localisation, '', UseDefaultLanguage)));
end;

function TPTLibLocaliser2.FindImage(const ImageID: String; const InstallationID: String;
  const AImageStream: TStream; const Scale: Single): Boolean;
var
  Localisation: TPTLibCityLocalisation;
begin
  Result := FLocalisations.FindCityLocalisation(
    InstallationID,
    Localisation);

  if Result then
  begin
    Result := Localisation.Images.FindImage(
      ImageID,
      AImageStream,
      '',
      Scale);
  end;

  if not Result then
  begin
    Result :=
      (GetDefaultLocalisations <> nil) and
      (GetDefaultLocalisations.Images.FindImage(ImageID, AImageStream));
  end;
end;

function TPTLibLocaliser2.FindTranslation(const TranslationID, LanguageID, InstallationID: String;
  var Translation: String; const UseDefaultLanguage: Boolean): Boolean;
var
  Localisation: TPTLibLanguageLocalisation;
  CurrentInstallationID: String;
  CityLocalisation: TPTLibCityLocalisation;
begin
  CurrentInstallationID := InstallationID;

  while not Result do
  begin
    // Find the city localisation
    if FindCityLocalisation(LanguageID, CurrentInstallationID, Localisation, '', UseDefaultLanguage) then
    begin
      // We found the city, now find the translation
      Result := Localisation.FindTranslation(TranslationID, Translation);

      if not Result then
      begin
        if FLocalisations.FindCityLocalisation(CurrentInstallationID, CityLocalisation, '', UseDefaultLanguage) then
        begin
          CurrentInstallationID := CityLocalisation.FInstallationParentID;
        end
        else
        begin
          Exit;
        end;
      end;
    end
    else
    begin
      // There was no installation ID, so quit now
      Break;
    end;
  end;
end;

function TPTLibLocaliser2.FindVideo(const VideoID, InstallationID: String;
  var Filename: String): Boolean;
var
  Localisation: TPTLibCityLocalisation;
begin
  Result := FLocalisations.FindCityLocalisation(InstallationID, Localisation);

  if Result then
    Result := Localisation.Videos.FindVideoFilename(VideoID, Filename);

  if not Result then
    Result := (GetDefaultLocalisations <> nil) and
              (GetDefaultLocalisations.Videos.FindVideoFilename(VideoID, Filename));
end;

procedure TPTLibLocaliser2.SetLocalisations(
  const Value: TPTLibCityLocalisations);
var
  i: Integer;
begin
  FLocalisations.Assign(Value);

  for i := 0 to pred(FLocalisations.Count) do
    if FLocalisations[i].Default then
      Exit;

  if FLocalisations.Count > 0 then
    FLocalisations[0].Default := True;
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
    if ((Items[i].Default) and (UseDefault)) or
       ((AnsiSameText(InstallationID, Items[i].InstallationID)) and
        (Items[i].InstallationID <> '') and
        (not AnsiSameText(IgnoreInstallationID, Items[i].InstallationID))) then
    begin
      Localisation := Items[i];

      Result := True;

      if not Items[i].Default then
      begin
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

{ TPTLibLocalisationImage }

function TPTLibLocalisationImage.GetDisplayName: string;
begin
  if FImageID = '' then
    Result := inherited GetDisplayName
  else
    Result := FImageID;
end;

procedure TPTLibLocalisationImage.SetImageID(const Value: String);
begin
  if (Collection <> nil) and
     (Value <> '') and
     (TPTLibLocalisationImages(Collection).FindImage(
       Value,
       nil,
       FImageID)) then
  begin
    raise Exception.CreateFmt(StrTheItemAlreadyExists,[Value]);
  end;

  FImageID := Value;
end;

{ TPTLibLocalisationImages }

constructor TPTLibLocalisationImages.Create(AOwner: TPersistent);
var
  CollectionClass: TPTLibLocalisationImageClass;
begin
  CollectionClass := TPTLibLocalisationImage;

  inherited Create(AOwner, CollectionClass);
end;

function TPTLibLocalisationImages.FindImage(const ImageID: String;
  const AImage: TStream; const IgnoreImageID: String; const Scale: Single): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to pred(Count) do
    if (AnsiSameText(ImageID, Items[i].ImageID)) and
       (Items[i].ImageID <> '') and
       (not AnsiSameText(IgnoreImageID, Items[i].ImageID)) then
    begin
      if AImage <> nil then
      begin
        AImage.Position := 0;

        Items[i].SaveToStream(AImage);
      end;

      Result := True;

      Break;
    end;
end;

function TPTLibLocalisationImages.GetItem(
  Index: Integer): TPTLibLocalisationImage;
begin
  Result := TPTLibLocalisationImage(inherited Items[Index]);
end;

procedure TPTLibLocalisationImages.SetItem(Index: Integer;
  const Value: TPTLibLocalisationImage);
begin
  inherited SetItem(Index, Value);
end;

{ TPTLibDefaultLocalisations }

constructor TPTLibDefaultLocalisations.Create;
begin
  inherited;

  FLocalisations := TPTLibLanguageLocalisations.Create(Self);
  FImages := TPTLibLocalisationImages.Create(Self);
end;

destructor TPTLibDefaultLocalisations.Destroy;
begin
  FreeAndNil(FImages);
  FreeAndNil(FLocalisations);

  inherited;
end;

procedure TPTLibDefaultLocalisations.SetImages(
  const Value: TPTLibLocalisationImages);
begin
  FImages.Assign(Value);
end;

procedure TPTLibDefaultLocalisations.SetLocalisations(
  const Value: TPTLibLanguageLocalisations);
begin
  FLocalisations.Assign(Value);
end;

{ TPTLibVideoLocalisation }

constructor TPTLibVideoLocalisation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

function TPTLibVideoLocalisation.GetDisplayName: string;
begin
  if FVideoID = '' then
    Result := inherited GetDisplayName
  else
    Result := FVideoID;
end;

{ TPTLibVideoLocalisations }

constructor TPTLibVideoLocalisations.Create(AOwner: TPersistent);
var
  CollectionClass: TPTLibVideoLocalisationClass;
begin
  CollectionClass := TPTLibVideoLocalisation;

  inherited Create(AOwner, CollectionClass);
end;

function TPTLibVideoLocalisations.FindVideoFilename(const VideoID: String;
  var Filename: String; const IgnoreInstallationID: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to pred(Count) do
    if (AnsiSameText(VideoID, Items[i].VideoID)) and
       (Items[i].VideoID <> '') and
       (not AnsiSameText(IgnoreInstallationID, Items[i].VideoID)) then
    begin
      Filename := Items[i].FFilename;

      Result := True;

      Break;
    end;
end;

function TPTLibVideoLocalisations.GetItem(
  Index: Integer): TPTLibVideoLocalisation;
begin
  Result := TPTLibVideoLocalisation(inherited Items[Index]);
end;

procedure TPTLibVideoLocalisations.SetItem(Index: Integer;
  const Value: TPTLibVideoLocalisation);
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

{ TBasePTLibLocalisationImage }

procedure TBasePTLibLocalisationImage.SaveToStream(const AStream: TStream; const Scale: Single);
var
  FileStream: TFileStream;
begin
  if FileExists(FFilename) then
  begin
    FileStream := TFileStream.Create(FFilename, fmOpenRead);
    try
      FileStream.Position := 0;

      AStream.CopyFrom(
        FileStream,
        FileStream.Size);
    finally
      FreeAndNil(FileStream);
    end;
  end
  else
  begin
    raise EPTLibError.CreateFmt(StrLocalisationImageNotFound, [FFilename]);
  end;
end;

end.
