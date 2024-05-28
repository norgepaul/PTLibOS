{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Thread                                      }
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

unit PTLib.Common.Thread;

interface

uses
  System.Classes, System.SysUtils, System.Variants, System.SyncObjs,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TPTLibThread = class(TThread)
  private
    procedure SetupThread;
  protected
    {$IFDEF CONSOLE}
    class var FOnTerminateCS: TCriticalSection;
    {$ENDIF}
    procedure DoTerminate; override;
  public
    constructor Create; reintroduce; overload; virtual;
    constructor Create(CreateSuspended: Boolean); reintroduce; overload; virtual;
    destructor Destroy; override;
  end;

  TPTLibParamThread = class(TPTLibThread)
  strict private
    FParamCS: TMultiReadExclusiveWriteSynchronizer;
    FParams: IParameters;

    function GetParams: IParameters;
  protected
    property Params: IParameters read GetParams;
  public
    constructor Create; override;
    constructor Create(CreateSuspended: Boolean); override;
    destructor Destroy; override;

    function GetParam(const Name: String; const Default: Variant): Variant;
    procedure SetParam(const Name: String; const Value: Variant);
  end;

  TPTLibHandlerThread = class(TPTLibThread)
  strict private
    FError: Exception;
  protected
    procedure DoExecute; virtual; abstract;
  public
    procedure Execute; override;

    property Error: Exception read FError;
  end;

implementation

{ PTLibThread }

constructor TPTLibThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);

  SetupThread;
end;

destructor TPTLibThread.Destroy;
begin
  inherited;

  {$IFDEF CONSOLE}
  FreeAndNil(FOnTerminateCS);
  {$ENDIF}
end;

procedure TPTLibThread.SetupThread;
begin
  {$IFNDEF LINUX64}
  NameThreadForDebugging(ClassName);
  {$ENDIF}

  {$IFDEF CONSOLE}
  FOnTerminateCS := TCriticalSection.Create;
  {$ENDIF}
end;

procedure TPTLibThread.DoTerminate;
begin
  {$IFDEF CONSOLE}
  // We can't use Synchronize if we're a Console app, so just call OnTerminate directly.
  FOnTerminateCS.Enter;
  try
    if Assigned(OnTerminate) then
    begin
      OnTerminate(Self);
    end;
  finally
    FOnTerminateCS.Leave;
  end;
  {$ELSE}
  inherited;
  {$ENDIF}
end;

constructor TPTLibThread.Create;
begin
  inherited Create;

  SetupThread;
end;

{ TPTLibHandlerThread }

procedure TPTLibHandlerThread.Execute;
begin
  try
    DoExecute;
  except
    on e: Exception do
    begin
      FError := e;
    end;
  end;
end;

{ TPTLibParamThread }

constructor TPTLibParamThread.Create;
begin
  inherited;

   FParamCS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

constructor TPTLibParamThread.Create(CreateSuspended: Boolean);
begin
  inherited;

  FParamCS := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TPTLibParamThread.Destroy;
begin
  FreeAndNil(FParamCS);

  inherited;
end;

function TPTLibParamThread.GetParam(const Name: String; const Default: Variant): Variant;
begin
  FParamCS.BeginRead;
  try
    Result := Params.GetParam(Name, Default);
  finally
    FParamCS.EndRead;
  end;
end;

function TPTLibParamThread.GetParams: IParameters;
begin
  if FParams = nil then
  begin
    FParams := TParameters.Create;
  end;

  Result := FParams;
end;

procedure TPTLibParamThread.SetParam(const Name: String; const Value: Variant);
begin
  FParamCS.BeginWrite;
  try
    Params.SetParam(Name, Value);
  finally
    FParamCS.EndWrite;
  end;
end;

end.
