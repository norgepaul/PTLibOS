unit PTLib.VCL.TaskProgressBar;

interface

uses
  System.Classes, System.SysUtils,

  System.Win.Taskbar, System.Win.TaskbarCore,

  Vcl.Taskbar,

  PTLib.Common.Utils;

type
  TTaskProgressBar = class(TComponent)
  strict private
    FTaskBarAvailable: Boolean;
    FProgress: Integer;
    FProgressMax: Integer;
    FProgressState: TTaskBarProgressState;

    FTaskBar: TCustomTaskbar;
  private
    function GetTaskBar: TCustomTaskbar;
    function GetProgress: Integer;
    function GetProgressMax: Integer;
    function GetState: TTaskBarProgressState;
    procedure SetProgress(const Value: Integer);
    procedure SetProgressMax(const Value: Integer);
    procedure SetState(const Value: TTaskBarProgressState);
  protected
    procedure DoUpdated; virtual;

    property TaskBar: TCustomTaskbar read GetTaskBar;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Progress: Integer read GetProgress write SetProgress;
    property ProgressMax: Integer read GetProgressMax write SetProgressMax;
    property State: TTaskBarProgressState read GetState write SetState;
  end;

implementation

{ TTaskProgressBar }

constructor TTaskProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  FTaskBarAvailable :=
    (not RunningOnCitrix) and
    (not FindCmdLineSwitch('NoTaskBar'));
end;

procedure TTaskProgressBar.DoUpdated;
begin
  if (not (csDesigning in ComponentState)) and
     (TaskBar <> nil) then
  begin
    TaskBar.ProgressValue := FProgress;
    TaskBar.ProgressMaxValue := FProgressMax;
    TaskBar.ProgressState := FProgressState;
  end;
end;

function TTaskProgressBar.GetProgress: Integer;
begin
  Result := FProgress;
end;

function TTaskProgressBar.GetProgressMax: Integer;
begin
  Result := FProgressMax;
end;

function TTaskProgressBar.GetState: TTaskBarProgressState;
begin
  Result := FProgressState;
end;

function TTaskProgressBar.GetTaskBar: TCustomTaskbar;
begin
  if (FTaskBar = nil) and
     (not FTaskBarAvailable) then
  begin
    try
      FTaskBar := TTaskBar.Create(Self);
    except
      on e: Exception do
      begin
        // If we raise an exeption it means something went wrong creating the taskbar
        // Citrix? Set FTaskBar to nil.
        FTaskBarAvailable := False;
      end;
    end;
  end;

  Result := FTaskBar;
end;

procedure TTaskProgressBar.SetProgress(const Value: Integer);
begin
  FProgress := Value;

  DoUpdated;
end;

procedure TTaskProgressBar.SetProgressMax(const Value: Integer);
begin
  FProgressMax := Value;

  DoUpdated;
end;

procedure TTaskProgressBar.SetState(const Value: TTaskBarProgressState);
begin
  FProgressState := Value;

  DoUpdated;
end;

end.
