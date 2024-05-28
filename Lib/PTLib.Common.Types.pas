{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Types                                       }
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

unit PTLib.Common.Types;

interface

uses
  System.Classes;

type
  TOnZipProgress = reference to procedure(Sender: TObject; const Filename: String; const Position: Int64; const Finished: Boolean);

  PIInterface = ^IInterface;

  TDateDisplayType = (
    ddtISO8601,
    ddtLocal,
    ddtUTC
  );

  THTTPRequestType = (
    ssGET,
    ssPOST
  );

  TDisplayOrientation = (
    orDefault,
    or90,
    or180,
    or270
  );

  TPriorityQueuePriority = (
    sqpASync,
    sqpLow,
    sqpMedium,
    sqpHigh,
    sqpMediumNonPersistent
  );

  TPriorityQueueMessageStatus = (
    sqmQueued,
    sqmReQueued,
    sqmSending
  );

  TLogSeverity = (
    LogSeverityNone,
    LogSeverityError,
    LogSeverityWarning,
    LogSeverityInfo,
    LogSeverityDebug,
    LogSeverityDebug2,
    LogSeverityDebug3
  );
  TLogSeverities = set of TLogSeverity;

  TLogFormatOption = (
    lfCreatedTimestamp,
    lfGeneratedTimestamp,
    lfIncludeMilliSeconds,
    lfLogType,
    lfLogText
  );
  TLogFormatOptions = set of TLogFormatOption;

  TGender = (
    Male,
    Female
  );

  TAllBoolean = (
    All,
    Yes,
    No
  );

  TScanFileOption = (
    IncludeFiles,
    IncludeDirectories,
    IncludePaths,
    RecursiveDirectories
  );

  TRectRotation = (
    rr0,
    rr90,
    rr180,
    rr270
  );

  TScanFileOptions = set of TScanFileOption;

  TSerialPortParity = (
    paNone,
    paOdd,
    paEven,
    paMark,
    paSpace
  );

  TSerialPortStopBits = (
    ssNone,
    ssOne,
    ssOnePointFive,
    ssTwo
  );

  TSerialPortOption = (
    spoAutoReconnect,
    spoUnlimitedWriteBuffer,
    spoWrapReadBuffer,
    spoSynchronizeEvents,
    spoFlushFullWriteBuffer
  );
  TSerialPortOptions = set of TSerialPortOption;

  TSerialPortState = (
    spsIdle,
    spsConnecting,
    spsConnected,
    spsDisconnected
  );

  TISO8601FormatOption = (
    isoExcludeDate,
    isoExcludeMillis,
    isoExcludeOffset,
    isoLocalize
  );
  TISO8601FormatOptions = set of TISO8601FormatOption;

const
  CRLF = #13#10;

  PriorityQueuePriorityDescriptions: Array[TPriorityQueuePriority] of String = (
    'ASync',
    'Low',
    'Medium',
    'High',
    'Medium Non Persistent'
  );

  ParityTypeDescriptions: Array[TSerialPortParity] of String = (
    'None',
    'Odd',
    'Even',
    'Mark',
    'Space'
  );

  SerialPortStateDescriptions: Array[TSerialPortState] of String = (
    'Idle',
    'Connecting',
    'Connected',
    'Disconnected'
  );

  LogSeverityStrings: Array[TLogSeverity] of String = (
    'NOLOG',
    'ERROR',
    'WARNING',
    'INFO',
    'DEBUG',
    'DEBUG2',
    'DEBUG3'
  );

procedure SetWeakReference(const aInterfaceField: PIInterface;
  const aValue: IInterface);

implementation

procedure SetWeakReference(const aInterfaceField: PIInterface;
  const aValue: IInterface);
begin
  PPointer(aInterfaceField)^ := Pointer(aValue);
end;

end.




