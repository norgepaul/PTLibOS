unit PTLib.Common.InterfacedDataModule;

interface

uses
  SysUtils, Classes;

type
  TInterfacedDataModule = class(TDataModule,
                                IInterface,
                                IInterfaceComponentReference)
  strict protected
    FOwnerIsComponent: Boolean;
    FRefCount: Integer;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance: TObject; override;

    property OwnerIsComponent: Boolean read FOwnerIsComponent;
    property RefCount: Integer read FRefCount;
  end;

implementation

uses
  Windows;

{$R *.dfm}

constructor TInterfacedDataModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TInterfacedDataModule.Destroy;
begin
  Destroying; // make everyone release references they have towards us

  if FOwnerIsComponent and (FRefCount > 1) then
  begin
    // perform cleanup of interface references that we refer to.
  end;

  inherited Destroy;
end;

procedure TInterfacedDataModule.AfterConstruction;
begin
  FOwnerIsComponent := Assigned(Owner) and (Owner is TComponent);

  // Release the NewInstance/constructor's implicit refcount
  InterlockedDecrement(FRefCount);

  inherited AfterConstruction;
end;

procedure TInterfacedDataModule.BeforeDestruction;
{$ifdef DEBUG}
var
  WarningMessage: string;
{$endif DEBUG}
begin
  if (RefCount <> 0) then
  begin
    if not OwnerIsComponent then
      System.Error(reInvalidPtr)
{$ifdef DEBUG}
    else
    begin
      WarningMessage := Format(
        'Trying to destroy an Owned TInterfacedDataModule of class %s named %s that still has %d interface references left',
        [ClassName, Name, RefCount]);
      OutputDebugString(PChar(WarningMessage));
    end;
{$endif DEBUG}
  end;
  inherited BeforeDestruction;
end;

class function TInterfacedDataModule.NewInstance: TObject;
begin
  // Set an implicit refcount so that refcounting
  // during construction won't destroy the object.
  Result := inherited NewInstance;
  TInterfacedDataModule(Result).FRefCount := 1;
end;

{ IInterface }

function TInterfacedDataModule._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedDataModule._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  { If we are not being used as a TComponent, then use refcount to manage our
    lifetime as with TPTLibInterfacedObject. }
  if (Result = 0) and not FOwnerIsComponent then
  begin
    Destroy;
  end;
end;

end.
