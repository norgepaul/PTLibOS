unit PTLib.XSuperObject.Localisation;

interface

uses
  System.SysUtils, System.Classes,

  XSuperObject,

  PTLib.Common.Strings,
  PTLib.Common.Localisation;

type
  TLocalisationHelper = class helper for TPTLibLocaliser
  public
    procedure ExportTranslations(const Filename: String; const LanguageID: String = ''; const InstallationID: String = '');

    function ToJSON(const LanguageIDs: TArray<String> = []; const InstallationIDs: TArray<String> = []): ISuperObject;
    function FromJSON(JSON: ISuperObject): ISuperObject;
  end;

implementation

{ TLocalisationHelper }

procedure TLocalisationHelper.ExportTranslations(const Filename, LanguageID, InstallationID: String);
begin

end;

function TLocalisationHelper.FromJSON(JSON: ISuperObject): ISuperObject;
begin

end;

function TLocalisationHelper.ToJSON(const LanguageIDs: TArray<String>; const InstallationIDs: TArray<String>): ISuperObject;
var
  i, n, j: Integer;
  X, X2: ISuperObject;
begin
  Result := SO;

  for i := 0 to pred(Localisations.Count) do
  begin
    if (length(InstallationIDs) = 0) or
       (IsStringInStringArray(Localisations[i].InstallationID, InstallationIDs)) then
    begin
      X := Result.O[Localisations[i].InstallationID];

      X.S['parent'] := Localisations[i].InstallationParentID;

      for n := 0 to pred(Localisations[i].Localisations.Count) do
      begin
        if (length(LanguageIDs) = 0) or
           (IsStringInStringArray(Localisations[i].Localisations[n].LanguageID, LanguageIDs)) then
        begin
          X2 := X.O[Localisations[i].Localisations[n].LanguageID];

          for j := 0 to pred(Localisations[i].Localisations[n].Translations.Count) do
          begin
            if pos('=', Localisations[i].Localisations[n].Translations[j]) <> 0 then
            begin
              X2.O['strings'].S[Localisations[i].Localisations[n].Translations.Names[j]] := Localisations[i].Localisations[n].Translations.Values[Localisations[i].Localisations[n].Translations.Names[j]];
            end;
          end;

          for j := 0 to pred(Localisations[i].Localisations[n].MemoTranslations.Count) do
          begin
            X2.O['text'].S[Localisations[i].Localisations[n].MemoTranslations[j].TranslationID] := Localisations[i].Localisations[n].MemoTranslations[j].Translation.Text;
          end;
        end;
      end;
    end;
  end;
end;

end.
