{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Dates                                       }
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
{******************************************************************* }

unit PTLib.Common.Dates;

interface

uses
  Soap.XSBuiltIns,
  System.TimeSpan,

  PTLib.Common.Types;

function DateTimeToISODateTimeStr(Value: TDateTime): String;
function DateTimeToCommonDateTimeStr(Value: TDateTime; IncludeMilliSeconds: Boolean = True): String;
function DateToCommonDateStr(Value: TDate): String;
function CommonDateTimeStrToDateTime(const Value: String): TDateTime; overload;
function CommonDateTimeStrToDateTime(const Value: String; out DateTime: TDateTime): Boolean; overload;
function TimeToCommonTimeStr(const Value: TTime; const IncludeMilliSeconds: Boolean = True): String;
function CommonTimeStrToTime(const Value: String; const Default: TTime): TTime;
function ParseDateTimeString(const FormatString: String; DateTime: TDateTime): String;
function SecondsToTimeString(Seconds: Int64; const Default: String = '-'): String;
function MinutesToTimeString(Minutes: Int64; const Default: String): String;
function MilliSecondsToTimeString(MilliSeconds: Integer; const Default: String = '0ms'): String;
function DateTimeToStringWithDifference(const Value, Current: TDateTime;
  const LocaliseTime: Boolean = False; const NoDateValue: String = 'Never'): String;
function DateTimeToStrOrNever(Value: TDateTime; const NeverString: String = 'Never'): String;
function StrWithMSToDateTimeDef(const Value: String): TDateTime;
function nowUTC: TDateTime;
function nowUTCWithLocalOffset: TDateTime;
function rawUTC: TDateTime;
function nowLocal: TDateTime;
function UTCToLocal(Value: TDateTime): TDateTime;
function LocalToUTC(Value: TDateTime): TDateTime;
function DateTimeToStringWithMS(Value: TDateTime): String;
procedure SetDateTimeUTC(Value: TDateTime);
procedure SetDateTimeUTCLocalOffset(const OffsetMinutes: Integer);
function GetDateTimeUTCLocalOffset: Integer;
function ETAString(StartTimeStamp, CurrentTimeStamp: TDateTime; CurrentPos, MaxPos: Int64): String;
function ETASeconds(StartTimeStamp, CurrentTimeStamp: TDateTime; CurrentPos, MaxPos: Int64): Int64;
function UTCOffsetMinutes: Integer;
function LongDayOfTheWeekName(const DayOfTheWeek: Integer): String;
function GetBirthDateShort(const BirthDate: TDateTime): String;
function GetMinuteString(const Seconds: Integer): String;
function FormatTime(const Time: TTime): String;
function ISO8601ToDateNoTimeZone(const Value: String): TDateTime;
function XSTimeStringToDateTime(const Value: String): TDateTime;
function FormatISO8601(const Value: TDateTime; const Options: TISO8601FormatOptions): String;

const
  CommonDateStr = 'DD/MM/YYYY HH:NN:SS';
  OneDay = 1;
  OneWeek = 7;

implementation

uses
  System.DateUtils,
  System.SysUtils,
  System.SyncObjs,

  PTLib.Common.Strings;

resourcestring
  StrInvalidDatePlaceholder = 'Invalid date placeholder: %s';
  StrInvalidDatePlaceholderValue = 'Invalid common datetime string: %s';
  StrDDays = '%d days';
  StrDDay = '%d day';
  StrFinished = 'Finished';
  StrAllDays = '<All Days>';

var
  UTCDateTimeOffset: Double;
  LocalOffsetMinutes: Integer;
  CSTimeZone: TMultiReadExclusiveWriteSynchronizer;
  CSTimeZoneLocal: TCriticalSection;

function FormatISO8601(const Value: TDateTime; const Options: TISO8601FormatOptions): String;
var
  ADateTime: TDateTime;
  idx: Integer;
begin
  if TISO8601FormatOption.isoLocalize in Options then
  begin
    ADateTime := UTCToLocal(Value);
  end
  else
  begin
    ADateTime := Value;
  end;

  Result := DateToISO8601(ADateTime, False);

  if TISO8601FormatOption.isoExcludeDate in Options then
  begin
    // Strip the date
    idx := pos('T', Result);

    if idx > 0 then
    begin
      Delete(Result, 1, idx);
    end;
  end;

  if TISO8601FormatOption.isoExcludeMillis in Options then
  begin
    idx := pos('.', Result);

    if idx <> 0 then
    begin
      Delete(Result, idx, 4);
    end;
  end;

  if TISO8601FormatOption.isoExcludeOffset in Options then
  begin
    idx := pos('Z', Result);

    if idx = 0 then
    begin
      idx := pos('+', Result);
    end;

    Delete(Result, idx, MaxInt);
  end;
end;

function FormatTime(const Time: TTime): String;
begin
  Result := HourOf(Time).ToString.PadLeft(2, '0') + ':' + MinuteOf(Time).ToString.PadLeft(2, '0')
end;

function ISO8601ToDateNoTimeZone(const Value: String): TDateTime;
var
  idx: Integer;
begin
  if Value = '' then
  begin
    Result := 0;
  end
  else
  begin
    idx := pos('+', Value);

    if idx = -1 then
    begin
      idx := pos('-', Value);
    end;

    if idx = -1 then
    begin
      Result := ISO8601ToDate(Value);
    end
    else
    begin
      Result := ISO8601ToDate(copy(Value, 1, idx - 1));
    end;
  end;
end;

function XSTimeStringToDateTime(const Value: String): TDateTime;
var
  obj:  TXSDateTime;
Begin
  obj := TXSDateTime.Create;
  try
    obj.XSToNative(Value);
    result := obj.AsDateTime;
  finally
    obj.Free;
  end;
end;

function InternalLocalDateTimeToUTCDateTime(Value: TDateTime): TDateTime;
begin
  CSTimeZoneLocal.Enter;
  try
    // TODO: For some reason, the result can be incorrect if we only do
    //       this once when the timezone/daylight saving changes.
    //       Delphi bug reported to Embarcadero.
    TTimeZone.Local.ToUniversalTime(Value);
    Result := TTimeZone.Local.ToUniversalTime(Value);
  finally
    CSTimeZoneLocal.Leave;
  end;
end;

function InternalUTCDateTimeToLocalDateTime(Value: TDateTime): TDateTime;
begin
  CSTimeZoneLocal.Enter;
  try
    Result := TTimeZone.Local.ToLocalTime(Value);
  finally
    CSTimeZoneLocal.Leave;
  end;
end;

function UTCOffsetMinutes: Integer;
begin
  CSTimeZoneLocal.Enter;
  try
    Result := Trunc(TTimeZone.Local.UtcOffset.TotalMinutes);
  finally
    CSTimeZoneLocal.Leave;
  end;
end;

procedure SetDateTimeUTCLocalOffset(const OffsetMinutes: Integer);
begin
  CSTimeZone.BeginWrite;
  try
    LocalOffsetMinutes := OffsetMinutes;
  finally
    CSTimeZone.EndWrite;
  end;
end;

function GetDateTimeUTCLocalOffset: Integer;
begin
  CSTimeZone.BeginRead;
  try
    Result := LocalOffsetMinutes;
  finally
    CSTimeZone.EndRead;
  end;
end;

procedure SetDateTimeUTC(Value: TDateTime);
var
  UTC: TDateTime;
begin
  UTC := InternalLocalDateTimeToUTCDateTime(now);

  CSTimeZone.BeginWrite;
  try
    UTCDateTimeOffset := Value - UTC;
  finally
    CSTimeZone.EndWrite;
  end;
end;

function GetDateTimeUTCOffset: Double;
begin
  CSTimeZone.BeginRead;
  try
    Result := UTCDateTimeOffset;
  finally
    CSTimeZone.EndRead;
  end;
end;

function DateTimeToStringWithMS(Value: TDateTime): String;
begin
  Result := DateTimeToStr(Value);

  Result := Result + '.' + MilliSecondOf(Value).ToString.PadLeft(3, '0');
end;

function nowLocal: TDateTime;
begin
  Result := InternalUTCDateTimeToLocalDateTime(nowUTC);
end;

function nowUTC: TDateTime;
begin
{$IF RtlVersion >= 35.0}
  Result := TDateTime.NowUTC;
{$ELSE}
  Result := InternalLocalDateTimeToUTCDateTime(now);
{$ENDIF}

  Result := Result + GetDateTimeUTCOffset;
end;

{ TODO : Never actually used due to wrong ifdef }
(*{$IFDEF WINDOWS}
var
  SysTime: TSystemTime;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetSystemTime(SysTime);

  Result := SystemTimeToDateTime(SysTime) + GetDateTimeOffset;
  {$ELSE}
  Result := InternalLocalDateTimeToUTCDateTime(now) + GetDateTimeUTCOffset;
  {$ENDIF}
end;  *)

function nowUTCWithLocalOffset: TDateTime;
begin
  if LocalOffsetMinutes < 0 then
    Result := nowUTC - (OneMinute * Abs(LocalOffsetMinutes))
  else
    Result := nowUTC + (OneMinute * LocalOffsetMinutes);
end;

function rawUTC: TDateTime;
begin
  Result := InternalLocalDateTimeToUTCDateTime(now);
end;

function UTCToLocal(Value: TDateTime): TDateTime;
begin
  if Value = 0 then
    Result := 0
  else
    Result := InternalUTCDateTimeToLocalDateTime(Value);
end;

function LocalToUTC(Value: TDateTime): TDateTime;
begin
  if Value = 0 then
    Result := 0
  else
    Result := InternalLocalDateTimeToUTCDateTime(Value);
end;

function ETASeconds(StartTimeStamp, CurrentTimeStamp: TDateTime; CurrentPos, MaxPos: Int64): Int64;
var
  ElapsedTime, EstimatedRemaining: TDateTime;
begin
  if CurrentPos = 0 then
    Result := 0
  else
  begin
    ElapsedTime := CurrentTimeStamp - StartTimeStamp;

    EstimatedRemaining := (ElapsedTime / CurrentPos) * (MaxPos - CurrentPos);

    Result := SecondsBetween(CurrentTimeStamp, CurrentTimeStamp + EstimatedRemaining);
  end;
end;

function GetMinuteString(const Seconds: Integer): String;
var
  Minutes: Integer;
begin
  Minutes := Seconds div 60;

  if Minutes = 0 then
    Minutes := 1;

  if Minutes = 1 then
    Result := format('%d min', [Minutes])
  else
    Result := format('%d mins', [Minutes])
end;

function ETAString(StartTimeStamp, CurrentTimeStamp: TDateTime; CurrentPos, MaxPos: Int64): String;
begin
  if CurrentPos = 0 then
    Result := '-' else
  if CurrentPos >= MaxPos then
  begin
    Result := StrFinished;
  end
  else
  begin
    Result := SecondsToTimeString(ETASeconds(StartTimeStamp, CurrentTimeStamp, CurrentPos, MaxPos));
  end;
end;

function DateTimeToStrOrNever(Value: TDateTime; const NeverString: String): String;
begin
  if Value = 0 then
    Result := NeverString
  else
    Result := DateTimeToStr(Value);
end;

function StrWithMSToDateTimeDef(const Value: String): TDateTime;
var
  s: String;
  ms: Integer;
begin
  Result := StrToDateTimeDef(Value, 0);

  s := copy(Value, EPos('.', Value) + 1, MaxInt);
  ms := StrToIntDef(s, -1);

  if ms <> -1 then
    Result := Result + (ms * OneMillisecond);
end;

function ParseDateTimeString(const FormatString: String; DateTime: TDateTime): String;
var
  AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: Word;
  LongYear, ShortYear, Day, Month, Hour, Minute, Second, MilliSecond: String;
begin
  Result := FormatString;

  DecodeDateTime(DateTime, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);

  LongYear := IntToStr(AYear);
  ShortYear := copy(LongYear, 3, 2);
  Day := ADay.ToString.PadLeft(2, '0');
  Month := AMonth.ToString.PadLeft(2, '0');
  Hour := AHour.ToString.PadLeft(2, '0');
  Minute := AMinute.ToString.PadLeft(2, '0');
  Second := ASecond.ToString.PadLeft(2, '0');
  MilliSecond := AMilliSecond.ToString.PadLeft(2, '0');

  Result := StringReplace(Result, 'YYYY', LongYear, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'YY', ShortYear, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'DD', Day, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'MM', Month, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'HH', Hour, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'NN', Minute, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'SS', Second, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, 'ZZZ', MilliSecond, [rfReplaceAll, rfIgnoreCase]);
end;

function TimeToCommonTimeStr(const Value: TTime; const IncludeMilliSeconds: Boolean): String;
begin
  if IncludeMilliSeconds then
  begin
    Result := ParseDateTimeString('HH:NN:SS.ZZZ', Value);
  end
  else
  begin
    Result := ParseDateTimeString('HH:NN:SS', Value);
  end;
end;

function CommonTimeStrToTime(const Value: String; const Default: TTime): TTime;
var
  TempDateTime: TDateTime;
begin
  if CommonDateTimeStrToDateTime('07/11/1969 ' +  Value, TempDateTime) then
  begin
    Result := Frac(TempDateTime);
  end
  else
  begin
    Result := Default;
  end;
end;

function LongDayOfTheWeekName(const DayOfTheWeek: Integer): String;
begin
  if DayOfTheWeek = 0 then
    Result := StrAllDays else
  if DayOfTheWeek = 7 then
    Result := FormatSettings.LongDayNames[1]
  else
    Result := FormatSettings.LongDayNames[DayOfTheWeek + 1];
end;

function MinutesToTimeString(Minutes: Int64; const Default: String): String;
var
  Days, Hours, Mins: Int64;
begin
  Result := '';

  Days := Trunc(1 / 1440 * Minutes);
  Hours := Trunc((Minutes mod 1440) / 60);
  Mins := Minutes mod 60;

  if Days = 1 then
    AddToken(Result, format(StrDDay, [Days]), ', ') else
  if Days > 1 then
    AddToken(Result, format(StrDDays, [Days]), ' ');

  if (Hours > 0) or (Days > 0) then
    AddToken(Result, format('%dh', [Hours]), ' ');

  if (Mins > 0) or (Days > 0) or (Hours > 0) then
    AddToken(Result, format('%dm', [Mins]), ' ');

  if Result = '' then
    Result := Default;
end;

function SecondsToTimeString(Seconds: Int64; const Default: String): String;
var
  Days, Hours, Minutes, Secs: Int64;
begin
  Result := '';

  Days := Trunc(1 / 86400 * Seconds);
  Hours := Trunc((Seconds mod 86400) / 3600);
  Minutes := Trunc((Seconds mod 3600) / 60);
  Secs := Seconds mod 60;

  if Days = 1 then
    AddToken(Result, format(StrDDay, [Days]), ', ') else
  if Days > 1 then
    AddToken(Result, format(StrDDays, [Days]), ' ');

  if (Hours > 0) or (Days > 0) then
    AddToken(Result, format('%dh', [Hours]), ' ');

  if (Minutes > 0) or (Days > 0) or (Hours > 0) then
    AddToken(Result, format('%dm', [Minutes]), ' ');

  if (Secs > 0) or (Days > 0) or (Hours > 0)  or (Minutes > 0) then
    AddToken(Result, format('%ds', [Secs]), ' ');

  if Result = '' then
    Result := Default;
end;

function MilliSecondsToTimeString(MilliSeconds: Integer; const Default: String): String;
var
  Days, Hours, Minutes, Secs, MilliSecs: Integer;
begin
  Result := '';

  Days := Trunc(1 / 86400000 * MilliSeconds);
  Hours := Trunc((MilliSeconds mod 86400000) / 3600000);
  Minutes := Trunc((MilliSeconds mod 3600000) / 60000);
  Secs := Trunc((MilliSeconds mod 3600000 mod 60000) / 1000);
  MilliSecs := Trunc(MilliSeconds mod 1000);

  if Days = 1 then
    AddToken(Result, format(StrDDay, [Days]), ', ') else
  if Days > 1 then
    AddToken(Result, format(StrDDays, [Days]), ' ');

  if (Hours > 0) or (Days > 0) then
    AddToken(Result, format('%dh', [Hours]), ' ');

  if (Minutes > 0) or (Days > 0) or (Hours > 0) then
    AddToken(Result, format('%dm', [Minutes]), ' ');

  if (Secs > 0) or (Days > 0) or (Hours > 0)  or (Minutes > 0) then
    AddToken(Result, format('%ds', [Secs]), ' ');

  if (MilliSecs > 0) or (Secs > 0) or (Days > 0) or (Hours > 0) or (Minutes > 0) then
  begin
    if Result <> '' then
    begin
      AddToken(Result, format('%sms', [IntToStr(MilliSecs).PadLeft(3, '0')]), ' ');
    end
    else
    begin
      AddToken(Result, format('%dms', [MilliSecs]), ' ');
    end;
  end;

  if Result = '' then
    Result := Default;
end;

function DateTimeToStringWithDifference(const Value, Current: TDateTime;
  const LocaliseTime: Boolean; const NoDateValue: String): String;
var
  Secs: Int64;
  RealValue, RealCurrent: TDateTime;
begin
  if Value = 0 then
  begin
    Result := NoDateValue;
  end
  else
  begin
    if LocaliseTime then
    begin
      RealValue := UTCToLocal(Value);
      RealCurrent := UTCToLocal(Current);
    end
    else
    begin
      RealValue := Value;
      RealCurrent := Current;
    end;

    Result := DateTimeToStr(RealValue);

    Secs := SecondsBetween(RealCurrent, RealValue);

    if Secs <> 0 then
    begin
      Result := Result + ' (' + SecondsToTimeString(Secs) + ')';
    end;
  end;
end;

function DateTimeToISODateTimeStr(Value: TDateTime): String;
begin
  Result := FormatDateTime('yyyy''-''mm''-''dd''T''hh'':''nn'':''ss', Value);
end;

function DateTimeToCommonDateTimeStr(Value: TDateTime; IncludeMilliSeconds: Boolean): String;
var
  CommonFormatSettings: TFormatSettings;
begin
  CommonFormatSettings := FormatSettings;
  CommonFormatSettings.DateSeparator := '/';
  CommonFormatSettings.TimeSeparator := ':';

  if IncludeMilliSeconds then
    Result := FormatDateTime(CommonDateStr + '.ZZZ', Value, CommonFormatSettings)
  else
    Result := FormatDateTime(CommonDateStr, Value, CommonFormatSettings);
end;

function DateToCommonDateStr(Value: TDate): String;
var
  CommonFormatSettings: TFormatSettings;
begin
  CommonFormatSettings := FormatSettings;
  CommonFormatSettings.DateSeparator := '/';

  Result := FormatDateTime(CommonDateStr, Value, CommonFormatSettings);
end;

function GetBirthDateShort(const BirthDate: TDateTime): String;
begin
  Result :=
    YearOf(BirthDate).ToString.PadLeft(4) +
    MonthOf(BirthDate).ToString.PadLeft(2) +
    DayOf(BirthDate).ToString.PadLeft(2);
end;

function CommonDateStrToDate(const Value: String; out DateTime: TDateTime): Boolean;

  function ValueFromPlaceHolder(const DateTimeStr, PlaceHolder: String; out Value: Word): Boolean;
  var
    p: Integer;
    DecodeStr: String;
  begin
    p := pos(PlaceHolder, CommonDateStr + '.ZZZ');

    Result := p <> -1;

    if Result then
    begin
      DecodeStr := copy(DateTimeStr, p, length(PlaceHolder));

      if DecodeStr = '' then
        Exit(False);

      Value := StrToIntDef(DecodeStr, 65535);

      if Value = 65535 then
        Exit(False);
    end;
  end;

var
  AYear, AMonth, ADay: Word;
begin
  if (ValueFromPlaceHolder(Value, 'YYYY', AYear)) and
     (ValueFromPlaceHolder(Value, 'MM', AMonth)) and
     (ValueFromPlaceHolder(Value, 'DD', ADay)) then
  begin
    Result := True;

    DateTime := EncodeDate(
      AYear,
      AMonth,
      ADay);
  end
  else
  begin
    Result := False;
  end;
end;

function CommonDateTimeStrToDateTime(const Value: String; out DateTime: TDateTime): Boolean;

  function ValueFromPlaceHolder(const DateTimeStr, PlaceHolder: String; out Value: Word): Boolean;
  var
    p: Integer;
    DecodeStr: String;
  begin
    p := pos(PlaceHolder, CommonDateStr + '.ZZZ');

    Result := p <> -1;

    if Result then
    begin
      DecodeStr := copy(DateTimeStr, p, length(PlaceHolder));

      if DecodeStr = '' then
        Exit(False);

      Value := StrToIntDef(DecodeStr, 65535);

      if Value = 65535 then
        Exit(False);
    end;
  end;

var
  AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word;
begin
  if (ValueFromPlaceHolder(Value, 'YYYY', AYear)) and
     (ValueFromPlaceHolder(Value, 'MM', AMonth)) and
     (ValueFromPlaceHolder(Value, 'DD', ADay)) and
     (ValueFromPlaceHolder(Value, 'HH', AHour)) and
     (ValueFromPlaceHolder(Value, 'NN', AMinute)) and
     (ValueFromPlaceHolder(Value, 'SS', ASecond)) then
  begin
    if not ValueFromPlaceHolder(Value, 'ZZZ', AMilliSecond) then
      AMilliSecond := 0;

    Result := True;

    DateTime := EncodeDateTime(
      AYear,
      AMonth,
      ADay,
      AHour,
      AMinute,
      ASecond,
      AMilliSecond);
  end
  else
  begin
    Result := False;
  end;
end;

function CommonDateTimeStrToDateTime(const Value: String): TDateTime;
begin
  if not CommonDateTimeStrToDateTime(Value, Result) then
    raise Exception.CreateFmt(StrInvalidDatePlaceholderValue, [Value]);
end;

initialization
  UTCDateTimeOffset := 0;
  CSTimeZone := TMultiReadExclusiveWriteSynchronizer.Create;
  CSTimeZoneLocal := TCriticalSection.Create;

finalization
  FreeAndNil(CSTimeZone);
  FreeAndNil(CSTimeZoneLocal);

end.



