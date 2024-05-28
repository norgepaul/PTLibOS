unit PTLib.GCP.Logger;

interface

uses
  PTLib.GCP.Interfaces,
  PTLib.GCP.Types;

type
  TLogger = class(TInterfacedObject, IGCPLogger)
  protected
    procedure Log(const Text: String; const Severity: TLogSeverity = TLogSeverity.Info; const Timestamp: TDateTime = 0); overload; virtual;
    procedure Log(const Text: String; const Args: Array of const; const Severity: TLogSeverity = TLogSeverity.Info; const Timestamp: TDateTime = 0); overload; virtual;
  end;

implementation

{ TGCPLogger }

procedure TLogger.Log(const Text: String; const Severity: TLogSeverity;
  const Timestamp: TDateTime);
begin
  // Implement in descendant class
end;

procedure TLogger.Log(const Text: String; const Args: array of const;
  const Severity: TLogSeverity; const Timestamp: TDateTime);
begin
  // Implement in descendant class
end;

end.
