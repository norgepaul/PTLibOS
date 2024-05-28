{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Statistics                                  }
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

unit PTLib.Common.Statistics;

interface

uses
  Classes, SysUtils, Generics.Collections,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces;

type
  TUsageTimeStatistic = class(TInterfacedObject,
                              IUsageTimeStatistic)
  strict private
    FLowTimeStamp: TDateTime;
    FHighTimeStamp: TDateTime;
    FLowDuration: Single;
    FHighDuration: Single;
    FTotalDuration: Single;
    FCount: Integer;
    FID: String;
  private
    function GetAverageDuration: Single;
    function GetCount: Integer;
    function GetHighDuration: Single;
    function GetHighTimeStamp: TDateTime;
    function GetLowDuration: Single;
    function GetLowTimeStamp: TDateTime;
    function GetTotalDuration: Single;
    procedure SetCount(const Value: Integer);
    procedure SetHighDuration(const Value: Single);
    procedure SetHighTimeStamp(const Value: TDateTime);
    procedure SetLowDuration(const Value: Single);
    procedure SetLowTimeStamp(const Value: TDateTime);
    procedure SetTotalDuration(const Value: Single);
    function GetID: String;
    procedure SetID(const Value: String);
  public
    property LowTimeStamp: TDateTime read GetLowTimeStamp write SetLowTimeStamp;
    property HighTimeStamp: TDateTime read GetHighTimeStamp write SetHighTimeStamp;
    property LowDuration: Single read GetLowDuration write SetLowDuration;
    property HighDuration: Single read GetHighDuration write SetHighDuration;
    property TotalDuration: Single read GetTotalDuration write SetTotalDuration;
    property Count: Integer read GetCount write SetCount;
    property ID: String read GetID write SetID;
    property AverageDuration: Single read GetAverageDuration;
  end;

  TUsageTimeStatistics = class(TComponent)
  strict private
    FUsageTimeStatisticDictionary: TObjectDictionary<String, TDictionary<String, IUsageTimeStatistic>>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddStatistic(const Group, ID: String; const Timestamp: TDateTime; const Duration: Cardinal);
    function GetStatistics(const Group: String; const Max: Cardinal = 0): IList<IUsageTimeStatistic>;
    procedure Clear(const GroupName: String = '');
  end;

implementation

{ TUsageTimeStatistics }

procedure TUsageTimeStatistics.AddStatistic(const Group, ID: String; const Timestamp: TDateTime; const Duration: Cardinal);
var
  UsageTimeStatistic: IUsageTimeStatistic;
  StatisticDictionary: TDictionary<String, IUsageTimeStatistic>;
begin
  TMonitor.Enter(FUsageTimeStatisticDictionary);
  try
    if not FUsageTimeStatisticDictionary.TryGetValue(
      Group,
      StatisticDictionary) then
    begin
      StatisticDictionary := TDictionary<String, IUsageTimeStatistic>.Create;

      FUsageTimeStatisticDictionary.Add(
        Group,
        StatisticDictionary);
    end;

    if not StatisticDictionary.TryGetValue(ID, UsageTimeStatistic) then
    begin
      UsageTimeStatistic := TUsageTimeStatistic.Create;
      UsageTimeStatistic.ID := ID;

      StatisticDictionary.Add(
        ID,
        UsageTimeStatistic);
    end;

    if (UsageTimeStatistic.Count = 0) or
       (Duration < UsageTimeStatistic.LowDuration) then
    begin
      UsageTimeStatistic.LowDuration := Duration;
      UsageTimeStatistic.LowTimeStamp := Timestamp;
    end;

    if (UsageTimeStatistic.Count = 0) or
       (Duration > UsageTimeStatistic.HighDuration) then
    begin
      UsageTimeStatistic.HighDuration := Duration;
      UsageTimeStatistic.HighTimeStamp := TimeStamp;
    end;

    UsageTimeStatistic.Count := UsageTimeStatistic.Count + 1;
    UsageTimeStatistic.TotalDuration := UsageTimeStatistic.TotalDuration + Duration;
  finally
    TMonitor.Exit(FUsageTimeStatisticDictionary);
  end;
end;

procedure TUsageTimeStatistics.Clear(const GroupName: String);
begin
  TMonitor.Enter(FUsageTimeStatisticDictionary);
  try
    if GroupName = '' then
    begin
      FUsageTimeStatisticDictionary.Clear;
    end
    else
    begin
      FUsageTimeStatisticDictionary.Remove(GroupName);
    end;
  finally
    TMonitor.Exit(FUsageTimeStatisticDictionary);
  end;
end;

constructor TUsageTimeStatistics.Create(AOwner: TComponent);
begin
  inherited;

  FUsageTimeStatisticDictionary := TObjectDictionary<String, TDictionary<String, IUsageTimeStatistic>>.Create([doOwnsValues]);
end;

destructor TUsageTimeStatistics.Destroy;
begin
  FreeAndNil(FUsageTimeStatisticDictionary);

  inherited;
end;

function TUsageTimeStatistics.GetStatistics(const Group: String; const Max: Cardinal): IList<IUsageTimeStatistic>;
var
  ID: String;
  StatisticDictionary: TDictionary<String, IUsageTimeStatistic>;
begin
  Result := TList<IUsageTimeStatistic>.Create;

  TMonitor.Enter(FUsageTimeStatisticDictionary);
  try
    if FUsageTimeStatisticDictionary.TryGetValue(
      Group,
      StatisticDictionary) then
    begin
      for ID in StatisticDictionary.Keys do
        Result.Add(StatisticDictionary[ID]);
    end;
  finally
    TMonitor.Exit(FUsageTimeStatisticDictionary);
  end;
end;

{ TUsageTimeStatistic }

function TUsageTimeStatistic.GetAverageDuration: Single;
begin
  if Count = 0 then
    Result := 0
  else
    Result := Trunc(TotalDuration / Count)
end;

function TUsageTimeStatistic.GetCount: Integer;
begin
  Result := FCount;
end;

function TUsageTimeStatistic.GetHighDuration: Single;
begin
  Result := FHighDuration;
end;

function TUsageTimeStatistic.GetHighTimeStamp: TDateTime;
begin
  Result := FHighTimeStamp;
end;

function TUsageTimeStatistic.GetID: String;
begin
  Result := FID;
end;

function TUsageTimeStatistic.GetLowDuration: Single;
begin
  Result := FLowDuration;
end;

function TUsageTimeStatistic.GetLowTimeStamp: TDateTime;
begin
  Result := FLowTimeStamp;
end;

function TUsageTimeStatistic.GetTotalDuration: Single;
begin
  Result := FTotalDuration;
end;

procedure TUsageTimeStatistic.SetCount(const Value: Integer);
begin
  FCount := Value;
end;

procedure TUsageTimeStatistic.SetHighDuration(const Value: Single);
begin
  FHighDuration := Value;
end;

procedure TUsageTimeStatistic.SetHighTimeStamp(const Value: TDateTime);
begin
  FHighTimeStamp := Value;
end;

procedure TUsageTimeStatistic.SetID(const Value: String);
begin
  FID := Value;
end;

procedure TUsageTimeStatistic.SetLowDuration(const Value: Single);
begin
  FLowDuration := Value;
end;

procedure TUsageTimeStatistic.SetLowTimeStamp(const Value: TDateTime);
begin
  FLowTimeStamp := Value;
end;

procedure TUsageTimeStatistic.SetTotalDuration(const Value: Single);
begin
  FTotalDuration := Value;
end;

end.

