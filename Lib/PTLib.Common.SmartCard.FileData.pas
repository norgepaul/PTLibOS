unit PTLib.Common.SmartCard.FileData;

interface

uses
  System.SysUtils, System.IOUtils,

  PTLib.Common.Timers,
  PTLib.Common.Interfaces,
  PTLib.Common.SmartCard,
  PTLib.Common.SmartCard.Types;

type
  TPTLibSmartCardFileData = class(TPTLibSmartCard,
                                  IPTLibSmartCardFileData)
  strict private
    FRFIDChangedTimer: IPTLibGlobalTimer;
    FFilename: String;

    procedure FileDataTimer(Sender: TObject);
  protected
    procedure DoSetActive(const Value: Boolean); override;
    procedure DoGetInformation(const Information: IInformationList); override;

    procedure DoReadRFIDFromFile; virtual;
  public
    constructor Create(const CardUIDChanged: TCardUIDChanged; const Filename: String); reintroduce;
  end;

implementation

{ TPTLibSmartCardFileData }

constructor TPTLibSmartCardFileData.Create(const CardUIDChanged: TCardUIDChanged; const Filename: String);
begin
  inherited Create(CardUIDChanged);

  FFilename := Filename;

  FRFIDChangedTimer := TPTLibGobalTimerController.NewTimer(Self, FileDataTimer, 200, False);
end;

procedure TPTLibSmartCardFileData.DoGetInformation(const Information: IInformationList);
begin
  inherited;

end;

procedure TPTLibSmartCardFileData.DoReadRFIDFromFile;
var
  NewCardRFID: String;
begin
  if FileExists(FFilename) then
  begin
    try
      NewCardRFID := TFile.ReadAllText(FFilename);
    except
      NewCardRFID := '';
    end;

    SetCardUID(NewCardRFID);
  end;
end;

procedure TPTLibSmartCardFileData.DoSetActive(const Value: Boolean);
begin
  inherited;

  FRFIDChangedTimer.Enabled := Value;
end;

procedure TPTLibSmartCardFileData.FileDataTimer(Sender: TObject);
begin
  DoReadRFIDFromFile;
end;

end.
