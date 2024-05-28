{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.DataHistory                                 }
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

unit PTLib.Common.DataHistory;

interface

uses
  SysUtils, Generics.Collections,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TDataHistory<T> = class(TInterfacedObject,
                          IDataHistory<T>)
  strict private
    FSampleCount: Integer;
    FSamples: IList<T>;
  private
    function GetSamples: IList<T>;
  protected
    function GetSampleCount: Integer;
    procedure SetSampleCount(const Value: Integer);
    procedure AdjustSamples;
    procedure AddSample(const Value: T); virtual;

    property SampleCount: Integer read GetSampleCount write SetSampleCount;
    function Samples: IList<T>;
  public
    constructor Create;
  end;

implementation

{ TDataHistory<T> }

procedure TDataHistory<T>.AddSample(const Value: T);
begin
  Samples.Add(Value);

  AdjustSamples;
end;

procedure TDataHistory<T>.AdjustSamples;
var
  Value: T;
begin
  // Pad with blank samples
  while Samples.Count < FSampleCount do
  begin
    Samples.Insert(0, Value);
  end;

  // Remove overflow
  while Samples.Count > FSampleCount do
  begin
    Samples.Delete(0);
  end;
end;

constructor TDataHistory<T>.Create;
begin
  FSampleCount := 1000;
end;

function TDataHistory<T>.GetSampleCount: Integer;
begin
  Result := FSampleCount;
end;

function TDataHistory<T>.GetSamples: IList<T>;
begin
  if FSamples = nil then
  begin
    FSamples := TList<T>.Create;

    AdjustSamples;
  end;

  Result := FSamples;
end;

function TDataHistory<T>.Samples: IList<T>;
begin
  Result := GetSamples;
end;

procedure TDataHistory<T>.SetSampleCount(const Value: Integer);
begin
  FSampleCount := Value;

  AdjustSamples;
end;

end.
