{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ PCSC interface unit                                              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1996 Microsoft Corporation.                        }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: WinSmCrd.h                                 }
{ The original Pascal code is: WinSmCrd.pas                        }
{ The initial developer of the Pascal code is Chris Dickerson      }
{ (chrisd@tsc.com).                                                }
{                                                                  }
{ Obtained through:                                                }
{                                                                  }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/MPL/MPL-1.1.html                          }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}
unit PTLib.Common.SmartCard.Pcsc.DLL.Types;

interface

uses
  System.Types;

(*

Copyright (c) 1996 - 1999  Microsoft Corporation

Module Name:

    winsmcrd.h

Abstract:
    Smart Card class/port IOCTL codes. This file is required for all code
    user mode and kernel mode, using Smart Card IOCTL's, defines,
    data structures

Revision History:

*)

(*
#ifdef _WINSCARD_H_
typedef DWORD ULONG;
typedef WORD UWORD;
typedef BYTE UCHAR;
#else
typedef ULONG DWORD;
// typedef UWORD WORD;
typedef UCHAR BYTE;
#endif *)

const
  FILE_DEVICE_SMARTCARD = $00000031;
  {$EXTERNALSYM FILE_DEVICE_SMARTCARD}

  SCARD_ATR_LENGTH = 33;  { ISO 7816-3 spec. }
  {$EXTERNALSYM SCARD_ATR_LENGTH}

  STATE_UNAWARE = $00000000;
  STATE_IGNORE = $00000001;
  STATE_CHANGED = $00000002;
  STATE_UNKNOWN = $00000004;
  STATE_UNAVAILABLE = $00000008;
  STATE_EMPTY = $00000010;
  STATE_PRESENT = $00000020;
  STATE_ATRMATCH = $00000040;
  STATE_EXCLUSIVE = $00000080;
  STATE_INUSE = $00000100;
  STATE_MUTE = $00000200;
  STATE_UNPOWERED = $00000400;

  IOCTL_CSB6_PCSC_ESCAPE = $00312000;
  IOCTL_MS_CCID_ESCAPE = $003136B0;

  S_SUCCESS = $00000000;
  F_INTERNAL_ERROR = $80100001;
  E_CANCELLED = $80100002;
  E_INVALID_HANDLE = $80100003;
  E_INVALID_PARAMETER = $80100004;
  E_INVALID_TARGET = $80100005;
  E_NO_MEMORY = $80100006;
  F_WAITED_TOO_LONG = $80100007;
  E_INSUFFICIENT_BUFFER = $80100008;
  E_UNKNOWN_READER = $80100009;
  E_TIMEOUT = $8010000A;
  E_SHARING_VIOLATION = $8010000B;
  E_NO_SMARTCARD = $8010000C;
  E_UNKNOWN_CARD = $8010000D;
  E_CANT_DISPOSE = $8010000E;
  E_PROTO_MISMATCH = $8010000F;
  E_NOT_READY = $80100010;
  E_INVALID_VALUE = $80100011;
  E_SYSTEM_CANCELLED = $80100012;
  F_COMM_ERROR = $80100013;
  F_UNKNOWN_ERROR = $80100014;
  E_INVALID_ATR = $80100015;
  E_NOT_TRANSACTED = $80100016;
  E_READER_UNAVAILABLE = $80100017;
  P_SHUTDOWN = $80100018;
  E_PCI_TOO_SMALL = $80100019;
  E_READER_UNSUPPORTED = $8010001A;
  E_DUPLICATE_READER = $8010001B;
  E_CARD_UNSUPPORTED = $8010001C;
  E_NO_SERVICE = $8010001D;
  E_SERVICE_STOPPED = $8010001E;
  E_UNEXPECTED = $8010001F;
  E_ICC_INSTALLATION = $80100020;
  E_ICC_CREATEORDER = $80100021;
  E_UNSUPPORTED_FEATURE = $80100022;
  E_DIR_NOT_FOUND = $80100023;
  E_FILE_NOT_FOUND = $80100024;
  E_NO_DIR = $80100025;
  E_NO_FILE = $80100026;
  E_NO_ACCESS = $80100027;
  E_WRITE_TOO_MANY = $80100028;
  E_BAD_SEEK = $80100029;
  E_INVALID_CHV = $8010002A;
  E_UNKNOWN_RES_MNG = $8010002B;
  E_NO_SUCH_CERTIFICATE = $8010002C;
  E_CERTIFICATE_UNAVAILABLE = $8010002D;
  E_NO_READERS_AVAILABLE = $8010002E;
  E_COMM_DATA_LOST = $8010002F;
  E_NO_KEY_CONTAINER = $80100030;
  W_UNSUPPORTED_CARD = $80100065;
  W_UNRESPONSIVE_CARD = $80100066;
  W_UNPOWERED_CARD = $80100067;
  W_RESET_CARD = $80100068;
  W_REMOVED_CARD = $80100069;
  W_SECURITY_VIOLATION = $8010006A;
  W_WRONG_CHV = $8010006B;
  W_CHV_BLOCKED = $8010006C;
  W_EOF = $8010006D;
  W_CANCELLED_BY_USER = $8010006E;
  W_CARD_NOT_AUTHENTICATED = $8010006F;

  SCARD_AUTOALLOCATE  = DWORD(-1);
  {$EXTERNALSYM SCARD_AUTOALLOCATE}

  SCARD_SCOPE_USER = 0;                 // The context is a user context, and any
  {$EXTERNALSYM SCARD_SCOPE_USER}       // database operations are performed within the
                                        // domain of the user.
  SCARD_SCOPE_TERMINAL = 1;             // The context is that of the current terminal,
  {$EXTERNALSYM SCARD_SCOPE_TERMINAL}   // and any database operations are performed
                                        // within the domain of that terminal.  (The
                                        // calling application must have appropriate
                                        // access permissions for any database actions.)
  SCARD_SCOPE_SYSTEM = 2;               // The context is the system context, and any
  {$EXTERNALSYM SCARD_SCOPE_SYSTEM}     // database operations are performed within the
                                        // domain of the system.  (The calling
                                        // application must have appropriate access
                                        // permissions for any database actions.)

   SCARD_STATE_UNAWARE = $0;                // The application is unaware of the
   {$EXTERNALSYM SCARD_STATE_UNAWARE}       // current state, and would like to
                                            // know.  The use of this value
                                            // results in an immediate return
                                            // from state transition monitoring
                                            // services.  This is represented by
                                            // all bits set to zero.
   SCARD_STATE_IGNORE  = $1;                // The application requested that
   {$EXTERNALSYM SCARD_STATE_IGNORE}        // this reader be ignored.  No other
                                            // bits will be set.
   SCARD_STATE_CHANGED = $2;                // This implies that there is a
   {$EXTERNALSYM SCARD_STATE_CHANGED}       // difference between the state
                                            // believed by the application, and
                                            // the state known by the Service
                                            // Manager.  When this bit is set,
                                            // the application may assume a
                                            // significant state change has
                                            // occurred on this reader.
   SCARD_STATE_UNKNOWN = $4;                // This implies that the given
   {$EXTERNALSYM SCARD_STATE_UNKNOWN}       // reader name is not recognized by
                                            // the Service Manager.  If this bit
                                            // is set, then SCARD_STATE_CHANGED
                                            // and SCARD_STATE_IGNORE will also
                                            // be set.
   SCARD_STATE_UNAVAILABLE = $8;            // This implies that the actual
   {$EXTERNALSYM SCARD_STATE_UNAVAILABLE}   // state of this reader is not
                                            // available.  If this bit is set,
                                            // then all the following bits are
                                            // clear.
   SCARD_STATE_EMPTY = $10;                 // This implies that there is not
   {$EXTERNALSYM SCARD_STATE_EMPTY}         // card in the reader.  If this bit
                                            // is set, all the following bits
                                            // will be clear.
   SCARD_STATE_PRESENT = $20;               // This implies that there is a card
   {$EXTERNALSYM SCARD_STATE_PRESENT}       // in the reader.
   SCARD_STATE_ATRMATCH = $40;              // This implies that there is a card
   {$EXTERNALSYM SCARD_STATE_ATRMATCH}      // in the reader with an ATR
                                            // matching one of the target cards.
                                            // If this bit is set,
                                            // SCARD_STATE_PRESENT will also be
                                            // set.  This bit is only returned
                                            // on the SCardLocateCard() service.
   SCARD_STATE_EXCLUSIVE = $80;             // This implies that the card in the
   {$EXTERNALSYM SCARD_STATE_EXCLUSIVE}     // reader is allocated for exclusive
                                            // use by another application.  If
                                            // this bit is set,
                                            // SCARD_STATE_PRESENT will also be
                                            // set.
   SCARD_STATE_INUSE = $100;                // This implies that the card in the
   {$EXTERNALSYM SCARD_STATE_INUSE}         // reader is in use by one or more
                                            // other applications, but may be
                                            // connected to in shared mode.  If
                                            // this bit is set,
                                            // SCARD_STATE_PRESENT will also be
                                            // set.
   SCARD_STATE_MUTE = $200;                 // This implies that the card in the
   {$EXTERNALSYM SCARD_STATE_MUTE}          // reader is unresponsive or not
                                            // supported by the reader or
                                            // software.
   SCARD_STATE_UNPOWERED = $400;            // This implies that the card in the
   {$EXTERNALSYM SCARD_STATE_UNPOWERED}     // reader has not been powered up.

   SCARD_SHARE_EXCLUSIVE = 1;            // This application is not willing to share this
   {$EXTERNALSYM SCARD_SHARE_EXCLUSIVE}  // card with other applications.
   SCARD_SHARE_SHARED = 2;               // This application is willing to share this
   {$EXTERNALSYM SCARD_SHARE_SHARED}     // card with other applications.
   SCARD_SHARE_DIRECT = 3;               // This application demands direct control of
   {$EXTERNALSYM SCARD_SHARE_DIRECT}     // the reader, so it is not available to other
                                         // applications.

   SCARD_LEAVE_CARD = 0;                 // Don't do anything special on close
   {$EXTERNALSYM SCARD_LEAVE_CARD}
   SCARD_RESET_CARD = 1;                 // Reset the card on close
   {$EXTERNALSYM SCARD_RESET_CARD}
   SCARD_UNPOWER_CARD = 2;               // Power down the card on close
   {$EXTERNALSYM SCARD_UNPOWER_CARD}
   SCARD_EJECT_CARD = 3;                 // Eject the card on close
   {$EXTERNALSYM SCARD_EJECT_CARD}

   SC_DLG_MINIMAL_UI = $01;
   {$EXTERNALSYM SC_DLG_MINIMAL_UI}
   SC_DLG_NO_UI = $02;
   {$EXTERNALSYM SC_DLG_NO_UI}
   SC_DLG_FORCE_UI = $04;
   {$EXTERNALSYM SC_DLG_FORCE_UI}

   SCERR_NOCARDNAME = $4000;
   {$EXTERNALSYM SCERR_NOCARDNAME}
   SCERR_NOGUIDS = $8000;
   {$EXTERNALSYM SCERR_NOGUIDS}

///////////////////////////////////////////////////////////////////////////////
//
//  Protocol Flag definitions
//

   SCARD_PROTOCOL_UNDEFINED = $00000000;  // There is no active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_UNDEFINED}
   SCARD_PROTOCOL_T0        = $00000001;  // T=0 is the active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_T0}
   SCARD_PROTOCOL_T1        = $00000002;  // T=1 is the active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_T1}
   SCARD_PROTOCOL_RAW       = $00010000;  // Raw is the active protocol.
   {$EXTERNALSYM SCARD_PROTOCOL_RAW}
//
// This is the mask of ISO defined transmission protocols
//
   SCARD_PROTOCOL_Tx: LongInt = SCARD_PROTOCOL_T0 or SCARD_PROTOCOL_T1;
   {$EXTERNALSYM SCARD_PROTOCOL_Tx}
//
// Use the default transmission parameters / card clock freq.
//
   SCARD_PROTOCOL_DEFAULT = $80000000;
   {$EXTERNALSYM SCARD_PROTOCOL_DEFAULT}
//
// Use optimal transmission parameters / card clock freq.
// Since using the optimal parameters is the default case no bit is defined to be 1
//
   SCARD_PROTOCOL_OPTIMAL = $00000000;
   {$EXTERNALSYM SCARD_PROTOCOL_OPTIMAL}

//
// Ioctl parameters 1 for IOCTL_SMARTCARD_POWER
//
   SCARD_POWER_DOWN = 0;          // Power down the card.
   {$EXTERNALSYM SCARD_POWER_DOWN}
   SCARD_COLD_RESET = 1;          // Cycle power and reset the card.
   {$EXTERNALSYM SCARD_COLD_RESET}
   SCARD_WARM_RESET = 2;          // Force a reset on the card.
   {$EXTERNALSYM SCARD_WARM_RESET}

//
///////////////////////////////////////////////////////////////////////////////
//
//  Reader Action IOCTLs
//

function SCARD_CTL_CODE(code: Integer): LongWord;
{$EXTERNALSYM SCARD_CTL_CODE}

function IOCTL_SMARTCARD_POWER: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_POWER}
function IOCTL_SMARTCARD_GET_ATTRIBUTE: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_ATTRIBUTE}
function IOCTL_SMARTCARD_SET_ATTRIBUTE: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_SET_ATTRIBUTE}
function IOCTL_SMARTCARD_CONFISCATE: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_CONFISCATE}
function IOCTL_SMARTCARD_TRANSMIT: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_TRANSMIT}
function IOCTL_SMARTCARD_EJECT: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_EJECT}
function IOCTL_SMARTCARD_SWALLOW: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_SWALLOW}
function IOCTL_SMARTCARD_IS_PRESENT: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_IS_PRESENT}
function IOCTL_SMARTCARD_IS_ABSENT: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_IS_ABSENT}
function IOCTL_SMARTCARD_SET_PROTOCOL: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_SET_PROTOCOL}
function IOCTL_SMARTCARD_GET_STATE: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_STATE}
function IOCTL_SMARTCARD_GET_LAST_ERROR: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_LAST_ERROR}
function IOCTL_SMARTCARD_GET_PERF_CNTR: LongWord;
{$EXTERNALSYM IOCTL_SMARTCARD_GET_PERF_CNTR}


//
///////////////////////////////////////////////////////////////////////////////
//
// Tags for requesting card and reader attributes
//

const
   MAXIMUM_ATTR_STRING_LENGTH = 32;   // Nothing bigger than this from getAttr
   {$EXTERNALSYM MAXIMUM_ATTR_STRING_LENGTH}
   MAXIMUM_SMARTCARD_READERS = 10;   // Limit the readers on the system
   {$EXTERNALSYM MAXIMUM_SMARTCARD_READERS}

function SCARD_ATTR_VALUE(ulClass, ulTag: Cardinal): Cardinal;
{$EXTERNALSYM SCARD_ATTR_VALUE}

const
   SCARD_CLASS_VENDOR_INFO = 1;     // Vendor information definitions
   {$EXTERNALSYM SCARD_CLASS_VENDOR_INFO}
   SCARD_CLASS_COMMUNICATIONS = 2;  // Communication definitions
   {$EXTERNALSYM SCARD_CLASS_COMMUNICATIONS}
   SCARD_CLASS_PROTOCOL = 3;        // Protocol definitions
   {$EXTERNALSYM SCARD_CLASS_PROTOCOL}
   SCARD_CLASS_POWER_MGMT = 4;      // Power Management definitions
   {$EXTERNALSYM SCARD_CLASS_POWER_MGMT}
   SCARD_CLASS_SECURITY = 5;        // Security Assurance definitions
   {$EXTERNALSYM SCARD_CLASS_SECURITY}
   SCARD_CLASS_MECHANICAL = 6;      // Mechanical characteristic definitions
   {$EXTERNALSYM SCARD_CLASS_MECHANICAL}
   SCARD_CLASS_VENDOR_DEFINED = 7;  // Vendor specific definitions
   {$EXTERNALSYM SCARD_CLASS_VENDOR_DEFINED}
   SCARD_CLASS_IFD_PROTOCOL = 8;    // Interface Device Protocol options
   {$EXTERNALSYM SCARD_CLASS_IFD_PROTOCOL}
   SCARD_CLASS_ICC_STATE = 9;       // ICC State specific definitions
   {$EXTERNALSYM SCARD_CLASS_ICC_STATE}
   SCARD_CLASS_PERF = $7ffe;        // performace counters
   {$EXTERNALSYM SCARD_CLASS_PERF}
   SCARD_CLASS_SYSTEM = $7fff;      // System-specific definitions
   {$EXTERNALSYM SCARD_CLASS_SYSTEM}

function SCARD_ATTR_VENDOR_NAME: Cardinal;
{$EXTERNALSYM SCARD_ATTR_VENDOR_NAME}
function SCARD_ATTR_VENDOR_IFD_TYPE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_VENDOR_IFD_TYPE}
function SCARD_ATTR_VENDOR_IFD_VERSION: Cardinal;
{$EXTERNALSYM SCARD_ATTR_VENDOR_IFD_VERSION}
function SCARD_ATTR_VENDOR_IFD_SERIAL_NO: Cardinal;
{$EXTERNALSYM SCARD_ATTR_VENDOR_IFD_SERIAL_NO}
function SCARD_ATTR_CHANNEL_ID: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CHANNEL_ID}
function SCARD_ATTR_PROTOCOL_TYPES: Cardinal;
{$EXTERNALSYM SCARD_ATTR_PROTOCOL_TYPES}
function SCARD_ATTR_DEFAULT_CLK: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEFAULT_CLK}
function SCARD_ATTR_MAX_CLK: Cardinal;
{$EXTERNALSYM SCARD_ATTR_MAX_CLK}
function SCARD_ATTR_DEFAULT_DATA_RATE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEFAULT_DATA_RATE}
function SCARD_ATTR_MAX_DATA_RATE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_MAX_DATA_RATE}
function SCARD_ATTR_MAX_IFSD: Cardinal;
{$EXTERNALSYM SCARD_ATTR_MAX_IFSD}
function SCARD_ATTR_POWER_MGMT_SUPPORT: Cardinal;
{$EXTERNALSYM SCARD_ATTR_POWER_MGMT_SUPPORT}
function SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE}
function SCARD_ATTR_USER_AUTH_INPUT_DEVICE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_USER_AUTH_INPUT_DEVICE}
function SCARD_ATTR_CHARACTERISTICS: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CHARACTERISTICS}
function SCARD_ATTR_CURRENT_PROTOCOL_TYPE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_PROTOCOL_TYPE}
function SCARD_ATTR_CURRENT_CLK: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_CLK}
function SCARD_ATTR_CURRENT_F: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_F}
function SCARD_ATTR_CURRENT_D: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_D}
function SCARD_ATTR_CURRENT_N: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_N}
function SCARD_ATTR_CURRENT_W: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_W}
function SCARD_ATTR_CURRENT_IFSC: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_IFSC}
function SCARD_ATTR_CURRENT_IFSD: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_IFSD}
function SCARD_ATTR_CURRENT_BWT: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_BWT}
function SCARD_ATTR_CURRENT_CWT: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_CWT}
function SCARD_ATTR_CURRENT_EBC_ENCODING: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_EBC_ENCODING}
function SCARD_ATTR_EXTENDED_BWT: Cardinal;
{$EXTERNALSYM SCARD_ATTR_EXTENDED_BWT}
function SCARD_ATTR_ICC_PRESENCE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ICC_PRESENCE}
function SCARD_ATTR_ICC_INTERFACE_STATUS: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ICC_INTERFACE_STATUS}
function SCARD_ATTR_CURRENT_IO_STATE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_CURRENT_IO_STATE}
function SCARD_ATTR_ATR_STRING: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ATR_STRING}
function SCARD_ATTR_ICC_TYPE_PER_ATR: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ICC_TYPE_PER_ATR}
function SCARD_ATTR_ESC_RESET: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ESC_RESET}
function SCARD_ATTR_ESC_CANCEL: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ESC_CANCEL}
function SCARD_ATTR_ESC_AUTHREQUEST: Cardinal;
{$EXTERNALSYM SCARD_ATTR_ESC_AUTHREQUEST}
function SCARD_ATTR_MAXINPUT: Cardinal;
{$EXTERNALSYM SCARD_ATTR_MAXINPUT}
function SCARD_ATTR_DEVICE_UNIT: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEVICE_UNIT}
function SCARD_ATTR_DEVICE_IN_USE: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEVICE_IN_USE}

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_W: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEVICE_FRIENDLY_NAME_W}
function SCARD_ATTR_DEVICE_SYSTEM_NAME_W: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEVICE_SYSTEM_NAME_W}

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_A: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEVICE_FRIENDLY_NAME_A}
function SCARD_ATTR_DEVICE_SYSTEM_NAME_A: Cardinal;
{$EXTERNALSYM SCARD_ATTR_DEVICE_SYSTEM_NAME_A}

function SCARD_ATTR_SUPRESS_T1_IFS_REQUEST: Cardinal;
{$EXTERNALSYM SCARD_ATTR_SUPRESS_T1_IFS_REQUEST}
function SCARD_PERF_NUM_TRANSMISSIONS: Cardinal;
{$EXTERNALSYM SCARD_PERF_NUM_TRANSMISSIONS}
function SCARD_PERF_BYTES_TRANSMITTED: Cardinal;
{$EXTERNALSYM SCARD_PERF_BYTES_TRANSMITTED}
function SCARD_PERF_TRANSMISSION_TIME: Cardinal;
{$EXTERNALSYM SCARD_PERF_TRANSMISSION_TIME}

//
// T=0 Protocol Defines
//

const
   SCARD_T0_HEADER_LENGTH = 7;
   {$EXTERNALSYM SCARD_T0_HEADER_LENGTH}
   SCARD_T0_CMD_LENGTH = 5;
   {$EXTERNALSYM SCARD_T0_CMD_LENGTH}


//
// T=1 Protocol Defines
//

   SCARD_T1_PROLOGUE_LENGTH = 3;
   {$EXTERNALSYM SCARD_T1_PROLOGUE_LENGTH}
   SCARD_T1_EPILOGUE_LENGTH = 2;
   {$EXTERNALSYM SCARD_T1_EPILOGUE_LENGTH}
   SCARD_T1_MAX_IFS = 254;
   {$EXTERNALSYM SCARD_T1_MAX_IFS}


//
///////////////////////////////////////////////////////////////////////////////
//
//  Reader states
//

   SCARD_UNKNOWN    = 0;           // This value implies the driver is unaware
   {$EXTERNALSYM SCARD_UNKNOWN}    // of the current state of the reader.
   SCARD_ABSENT     = 1;           // This value implies there is no card in
   {$EXTERNALSYM SCARD_ABSENT}     // the reader.
   SCARD_PRESENT    = 2;           // This value implies there is a card is
   {$EXTERNALSYM SCARD_PRESENT}    // present in the reader, but that it has
                                   // not been moved into position for use.
   SCARD_SWALLOWED  = 3;           // This value implies there is a card in the
   {$EXTERNALSYM SCARD_SWALLOWED}  // reader in position for use.  The card is
                                   // not powered.
   SCARD_POWERED    = 4;           // This value implies there is power is
   {$EXTERNALSYM SCARD_POWERED}    // being provided to the card, but the
                                   // Reader Driver is unaware of the mode of
                                   // the card.
   SCARD_NEGOTIABLE = 5;           // This value implies the card has been
   {$EXTERNALSYM SCARD_NEGOTIABLE} // reset and is awaiting PTS negotiation.
   SCARD_SPECIFIC   = 6;           // This value implies the card has been
   {$EXTERNALSYM SCARD_SPECIFIC}   // reset and specific communication
                                   // protocols have been established.

////////////////////////////////////////////////////////////////////////////////
//
//  I/O Services
//
//      The following services provide access to the I/O capabilities of the
//      reader drivers.  Services of the Smart Card are requested by placing the
//      following structure into the protocol buffer:
//
type
  PDWord = ^DWORD;
  LPDWORD = ^Longword;

  HWND = type UIntPtr;
  {$EXTERNALSYM HWND}
  HICON = type UIntPtr;
  {$EXTERNALSYM HICON}


  PSCARD_IO_REQUEST = ^SCARD_IO_REQUEST;
  {$EXTERNALSYM PSCARD_IO_REQUEST}
  SCARD_IO_REQUEST = record
    dwProtocol: LongWord;    { Protocol identifier }
    dbPciLength: LongWord;   { Protocol Control Information Length }
  end;
  {$EXTERNALSYM SCARD_IO_REQUEST}

  TErrSource         = (esInit, esConnect, esGetStatus, esTransmit);
  TNeededPIN         = (npPIN1, npPIN2, npPUK1, npPUK2);

  TPCSCErrorEvent    = procedure(Sender: TObject; ErrSource: TErrSource; ErrCode: cardinal) of object;
  TPCSCPinEvent      = procedure(Sender: TObject; NeedPIN: TNeededPIN) of object;

  LPCGUID = ^GUID;
  {$EXTERNALSYM LPCGUID}
  GUID = record
     Data1: LongInt;
     Data2: Integer;
     Data3: Integer;
     Data4: array[0..7] of Byte;
  end;
  {$EXTERNALSYM GUID}

  SCARDCONTEXT = DWORD;
  {$EXTERNALSYM SCARDCONTEXT}
  SCARDHANDLE = DWORD;
  {$EXTERNALSYM SCARDHANDLE}

  LPSCARD_READERSTATEA = ^SCARD_READERSTATEA;
  {$EXTERNALSYM LPSCARD_READERSTATEA}
  PSCARD_READERSTATEA = ^SCARD_READERSTATEA;
  {$EXTERNALSYM PSCARD_READERSTATEA}

  SCARD_READERSTATEA = record
     szReader:       MarshaledAString;       { the reader name from SCardListReaders }
     pvUserData:     Pointer;      { user defined data }
     dwCurrentState: LongInt;      { current state of reader at time of call }
     dwEventState:   LongInt;      { state of reader after state change }
     cbAtr:          LongInt;      { Number of bytes in the returned ATR }
     rgbAtr: array[0..35] of Byte; { Atr of inserted card, (extra alignment bytes) }
  end;
  {$EXTERNALSYM SCARD_READERSTATEA}

  LPSCARD_READERSTATEW = ^SCARD_READERSTATEW;
  {$EXTERNALSYM LPSCARD_READERSTATEW}
  PSCARD_READERSTATEW = ^SCARD_READERSTATEW;
  {$EXTERNALSYM PSCARD_READERSTATEW}
  SCARD_READERSTATEW = record
    szReader: PWideChar;                { reader name }
    pvUserData: Pointer;              { user defined data }
    dwCurrentState: LongInt;          { current state of reader at time of call }
    dwEventState: LongInt;            { state of reader after state change }
    cbAtr: LongInt;                   { Number of bytes in the returned ATR }
    rgbAtr: array[0..35] of Byte;     { Atr of inserted card, (extra alignment bytes) }
  end;
  {$EXTERNALSYM SCARD_READERSTATEW}

{$IFDEF UNICODE}
   SCARD_READERSTATE = SCARD_READERSTATEW;
   PSCARD_READERSTATE = PSCARD_READERSTATEW;
   LPSCARD_READERSTATE = LPSCARD_READERSTATEW;
{$ELSE}
   SCARD_READERSTATE = SCARD_READERSTATEA;
   PSCARD_READERSTATE = PSCARD_READERSTATEA;
   LPSCARD_READERSTATE = LPSCARD_READERSTATEA;
{$ENDIF}
{$EXTERNALSYM SCARD_READERSTATE}
{$EXTERNALSYM PSCARD_READERSTATE}
{$EXTERNALSYM LPSCARD_READERSTATE}

  PSCARD_ATRMASK = ^SCARD_ATRMASK;
  {$EXTERNALSYM PSCARD_ATRMASK}
  LPSCARD_ATRMASK = ^SCARD_ATRMASK;
  {$EXTERNALSYM LPSCARD_ATRMASK}
  SCARD_ATRMASK = record
     cbArt: DWORD;                  // Number of bytes in the ATR and the mask.
     rgbAtr: array[0..35] of Byte;  // Atr of card (extra alignment bytes)
     rgbMask: array[0..35] of Byte; // Mask for the Atr (extra alignment bytes)
  end;
  {$EXTERNALSYM SCARD_ATRMASK}

   LPOCNCONNPROCA = function(hSCardContext: LongInt; hCard: MarshaledString; pvUserData: Pointer): DWORD; cdecl;
   {$EXTERNALSYM LPOCNCONNPROCA}
   LPOCNCONNPROCW = function(hSCardContext: LongInt; hCard: PWideChar; pvUserData: Pointer): DWORD; cdecl;
   {$EXTERNALSYM LPOCNCONNPROCW}
   {$IFDEF UNICODE}
      LPOCNCONNPROC = LPOCNCONNPROCW;
   {$ELSE}
      LPOCNCONNPROC = LPOCNCONNPROCA;
   {$ENDIF}
   {$EXTERNALSYM LPOCNCONNPROC}

   LPOCNCHKPROC = function(hContext: LongInt; hCard: MarshaledString; pvUserData: Pointer): LongBool; cdecl;
   {$EXTERNALSYM LPOCNCHKPROC}
   LPOCNDSCPROC = procedure(hContext: LongInt; hCard: MarshaledString; pvUserData: Pointer); cdecl;
   {$EXTERNALSYM LPOCNDSCPROC}

//
// OPENCARD_SEARCH_CRITERIA: In order to specify a user-extended search,
// lpfnCheck must not be NULL.  Moreover, the connection to be made to the
// card before performing the callback must be indicated by either providing
// lpfnConnect and lpfnDisconnect OR by setting dwShareMode.
// If both the connection callbacks and dwShareMode are non-NULL, the callbacks
// will be used.
//

   POPENCARD_SEARCH_CRITERIAA = ^OPENCARD_SEARCH_CRITERIAA;
   {$EXTERNALSYM POPENCARD_SEARCH_CRITERIAA}
   LPOPENCARD_SEARCH_CRITERIAA = ^OPENCARD_SEARCH_CRITERIAA;
   {$EXTERNALSYM LPOPENCARD_SEARCH_CRITERIAA}
   OPENCARD_SEARCH_CRITERIAA = record
      dwStructSize: DWORD;
      lpstrGroupNames: MarshaledString;        // OPTIONAL reader groups to include in
      nMaxGroupNames: DWORD;         //          search.  NULL defaults to
                                     //          SCard$DefaultReaders
      rgguidInterfaces: LPCGUID;     // OPTIONAL requested interfaces
      cguidInterfaces: DWORD;        //          supported by card's SSP
      lpstrCardNames: MarshaledString;         // OPTIONAL requested card names; all cards w/
      nMaxCardNames: DWORD;          //          matching ATRs will be accepted
      lpfnCheck: LPOCNCHKPROC;       // OPTIONAL if NULL no user check will be performed.
      lpfnConnect: LPOCNCONNPROCA;   // OPTIONAL if lpfnConnect is provided,
      lpfnDisconnect: LPOCNDSCPROC;  //          lpfnDisconnect must also be set.
      pvUserData: Pointer;           // OPTIONAL parameter to callbacks
      dwShareMode: DWORD;            // OPTIONAL must be set if lpfnCheck is not null
      dwPreferredProtocols: DWORD;   // OPTIONAL
   end;
   {$EXTERNALSYM OPENCARD_SEARCH_CRITERIAA}

   POPENCARD_SEARCH_CRITERIAW = ^OPENCARD_SEARCH_CRITERIAW;
   {$EXTERNALSYM POPENCARD_SEARCH_CRITERIAW}
   LPOPENCARD_SEARCH_CRITERIAW = ^OPENCARD_SEARCH_CRITERIAW;
   {$EXTERNALSYM LPOPENCARD_SEARCH_CRITERIAW}
   OPENCARD_SEARCH_CRITERIAW = record
      dwStructSize: DWORD;
      lpstrGroupNames: PWideChar;       // OPTIONAL reader groups to include in
      nMaxGroupNames: DWORD;         //          search.  NULL defaults to
                                     //          SCard$DefaultReaders
      rgguidInterfaces: LPCGUID;     // OPTIONAL requested interfaces
      cguidInterfaces: DWORD;        //          supported by card's SSP
      lpstrCardNames: PWideChar;        // OPTIONAL requested card names; all cards w/
      nMaxCardNames: DWORD;          //          matching ATRs will be accepted
      lpfnCheck: LPOCNCHKPROC;       // OPTIONAL if NULL no user check will be performed.
      lpfnConnect: LPOCNCONNPROCA;   // OPTIONAL if lpfnConnect is provided,
      lpfnDisconnect: LPOCNDSCPROC;  //          lpfnDisconnect must also be set.
      pvUserData: Pointer;           // OPTIONAL parameter to callbacks
      dwShareMode: DWORD;            // OPTIONAL must be set if lpfnCheck is not null
      dwPreferredProtocols: DWORD;   // OPTIONAL
   end;
   {$EXTERNALSYM OPENCARD_SEARCH_CRITERIAW}

   {$IFDEF UNICODE}
      OPENCARD_SEARCH_CRITERIA = OPENCARD_SEARCH_CRITERIAW;
      POPENCARD_SEARCH_CRITERIA = POPENCARD_SEARCH_CRITERIAW;
      LPOPENCARD_SEARCH_CRITERIA = LPOPENCARD_SEARCH_CRITERIAW;
   {$ELSE}
      OPENCARD_SEARCH_CRITERIA = OPENCARD_SEARCH_CRITERIAA;
      POPENCARD_SEARCH_CRITERIA = POPENCARD_SEARCH_CRITERIAA;
      LPOPENCARD_SEARCH_CRITERIA = LPOPENCARD_SEARCH_CRITERIAA;
   {$ENDIF}
   {$EXTERNALSYM OPENCARD_SEARCH_CRITERIA}
   {$EXTERNALSYM POPENCARD_SEARCH_CRITERIA}
   {$EXTERNALSYM LPOPENCARD_SEARCH_CRITERIA}

//
// OPENCARDNAME_EX: used by SCardUIDlgSelectCard; replaces obsolete OPENCARDNAME
//
   POPENCARDNAME_EXA = ^OPENCARDNAME_EXA;
   {$EXTERNALSYM POPENCARDNAME_EXA}
   LPOPENCARDNAME_EXA = ^OPENCARDNAME_EXA;
   {$EXTERNALSYM LPOPENCARDNAME_EXA}
   OPENCARDNAME_EXA = record
      dwStructSize: DWORD;           // REQUIRED
      hSCardContext: SCARDCONTEXT;   // REQUIRED
      hwndOwner: HWND;               // OPTIONAL
      dwFlags: DWORD;                // OPTIONAL -- default is SC_DLG_MINIMAL_UI
      lpstrTitle: MarshaledAString;            // OPTIONAL
      lpstrSearchDesc: MarshaledAString;       // OPTIONAL (eg. "Please insert your <brandname> smart card.")
      hIcon: HICON;                  // OPTIONAL 32x32 icon for your brand insignia
      pOpenCardSearchCriteria: POPENCARD_SEARCH_CRITERIAA ; // OPTIONAL
      lpfnConnect: LPOCNCONNPROCA;   // OPTIONAL - performed on successful selection
      pvUserData: Pointer;           // OPTIONAL parameter to lpfnConnect
      dwShareMode: DWORD;            // OPTIONAL - if lpfnConnect is NULL, dwShareMode and
      dwPreferredProtocols: DWORD;   // OPTIONAL dwPreferredProtocols will be used to
                                     //          connect to the selected card
      lpstrRdr: MarshaledString;               // REQUIRED [IN|OUT] Name of selected reader
      nMaxRdr: DWORD;                // REQUIRED [IN|OUT]
      lpstrCard: MarshaledString;              // REQUIRED [IN|OUT] Name of selected card
      nMaxCard: DWORD;               // REQUIRED [IN|OUT]
      dwActiveProtocol: DWORD;       // [OUT] set only if dwShareMode not NULL
      hCardHandle: SCARDHANDLE;      // [OUT] set if a card connection was indicated
   end;
   {$EXTERNALSYM OPENCARDNAME_EXA}

   POPENCARDNAME_EXW = ^OPENCARDNAME_EXW;
   {$EXTERNALSYM POPENCARDNAME_EXW}
   LPOPENCARDNAME_EXW = ^OPENCARDNAME_EXW;
   {$EXTERNALSYM LPOPENCARDNAME_EXW}
   OPENCARDNAME_EXW = record
      dwStructSize: DWORD;           // REQUIRED
      hSCardContext: SCARDCONTEXT;   // REQUIRED
      hwndOwner: HWND;               // OPTIONAL
      dwFlags: DWORD;                // OPTIONAL -- default is SC_DLG_MINIMAL_UI
      lpstrTitle: PWideChar;           // OPTIONAL
      lpstrSearchDesc: PWideChar;      // OPTIONAL (eg. "Please insert your <brandname> smart card.")
      hIcon: HICON;                  // OPTIONAL 32x32 icon for your brand insignia
      pOpenCardSearchCriteria: POPENCARD_SEARCH_CRITERIAW ; // OPTIONAL
      lpfnConnect: LPOCNCONNPROCW;   // OPTIONAL - performed on successful selection
      pvUserData: Pointer;           // OPTIONAL parameter to lpfnConnect
      dwShareMode: DWORD;            // OPTIONAL - if lpfnConnect is NULL, dwShareMode and
      dwPreferredProtocols: DWORD;   // OPTIONAL dwPreferredProtocols will be used to
                                     //          connect to the selected card
      lpstrRdr: PWideChar;              // REQUIRED [IN|OUT] Name of selected reader
      nMaxRdr: DWORD;                // REQUIRED [IN|OUT]
      lpstrCard: PWideChar;             // REQUIRED [IN|OUT] Name of selected card
      nMaxCard: DWORD;               // REQUIRED [IN|OUT]
      dwActiveProtocol: DWORD;       // [OUT] set only if dwShareMode not NULL
      hCardHandle: SCARDHANDLE;      // [OUT] set if a card connection was indicated
   end;
   {$EXTERNALSYM OPENCARDNAME_EXW}

   {$IFDEF UNICODE}
      OPENCARDNAME_EX = OPENCARDNAME_EXW;
      POPENCARDNAME_EX = POPENCARDNAME_EXW;
      LPOPENCARDNAME_EX = LPOPENCARDNAME_EXW;
   {$ELSE}
      OPENCARDNAME_EX = OPENCARDNAME_EXA;
      POPENCARDNAME_EX = POPENCARDNAME_EXA;
      LPOPENCARDNAME_EX = LPOPENCARDNAME_EXA;
   {$ENDIF}
   {$EXTERNALSYM OPENCARDNAME_EX}
   {$EXTERNALSYM POPENCARDNAME_EX}
   {$EXTERNALSYM LPOPENCARDNAME_EX}

//
// SCardUIDlgSelectCard replaces GetOpenCardName
//


//
// "Smart Card Common Dialog" definitions for backwards compatibility
//  with the Smart Card Base Services SDK version 1.0
//

   POPENCARDNAMEA = ^OPENCARDNAMEA;
   {$EXTERNALSYM POPENCARDNAMEA}
   LPOPENCARDNAMEA = ^OPENCARDNAMEA;
   {$EXTERNALSYM LPOPENCARDNAMEA}
   OPENCARDNAMEA = record
      dwStructSize: DWORD;
      hwndOwner: HWND;
      hSCardContext: SCARDCONTEXT;
      lpstrGroupNames: MarshaledString;
      nMaxGroupNames: DWORD;
      lpstrCardNames: MarshaledString;
      nMaxCardNames: DWORD;
      rgguidInterfaces: LPCGUID;
      cguidInterfaces: DWORD;
      lpstrRdr: MarshaledString;
      nMaxRdr: DWORD;
      lpstrCard: MarshaledString;
      nMaxCard: DWORD;
      lpstrTitle: MarshaledAString;
      dwFlags: DWORD;
      pvUserData: Pointer;
      dwShareMode: DWORD;
      dwPreferredProtocols: DWORD;
      dwActiveProtocol: DWORD;
      lpfnConnect: LPOCNCONNPROCA;
      lpfnCheck: LPOCNCHKPROC;
      lpfnDisconnect: LPOCNDSCPROC;
      hCardHandle: SCARDHANDLE;
   end;
   {$EXTERNALSYM OPENCARDNAMEA}

   POPENCARDNAMEW = ^OPENCARDNAMEW;
   {$EXTERNALSYM POPENCARDNAMEW}
   LPOPENCARDNAMEW = ^OPENCARDNAMEW;
   {$EXTERNALSYM LPOPENCARDNAMEW}
   OPENCARDNAMEW = record
      dwStructSize: DWORD;
      hwndOwner: HWND;
      hSCardContext: SCARDCONTEXT;
      lpstrGroupNames: PWideChar;
      nMaxGroupNames: DWORD;
      lpstrCardNames: PWideChar;
      nMaxCardNames: DWORD;
      rgguidInterfaces: LPCGUID;
      cguidInterfaces: DWORD;
      lpstrRdr: PWideChar;
      nMaxRdr: DWORD;
      lpstrCard: PWideChar;
      nMaxCard: DWORD;
      lpstrTitle: PWideChar;
      dwFlags: DWORD;
      pvUserData: Pointer;
      dwShareMode: DWORD;
      dwPreferredProtocols: DWORD;
      dwActiveProtocol: DWORD;
      lpfnConnect: LPOCNCONNPROCW;
      lpfnCheck: LPOCNCHKPROC;
      lpfnDisconnect: LPOCNDSCPROC;
      hCardHandle: SCARDHANDLE;
   end;
   {$EXTERNALSYM OPENCARDNAMEW}

   {$IFDEF UNICODE}
      OPENCARDNAME = OPENCARDNAMEW;
      POPENCARDNAME = POPENCARDNAMEW;
      LPOPENCARDNAME = LPOPENCARDNAMEW;
   {$ELSE}
      OPENCARDNAME = OPENCARDNAMEA;
      POPENCARDNAME = POPENCARDNAMEA;
      LPOPENCARDNAME = LPOPENCARDNAMEA;
   {$ENDIF}
   {$EXTERNALSYM OPENCARDNAME}
   {$EXTERNALSYM POPENCARDNAME}
   {$EXTERNALSYM LPOPENCARDNAME}

const
  MAXAPDULENGTH      = 260; // CLA + INS + P1..3 + 255Bytes
  NOREADERSELECTED   = -1;
  SCARD_PCI_T0       : SCARD_IO_REQUEST = (dwProtocol:1; dbPciLength:8);
  SCARD_PCI_T1       : SCARD_IO_REQUEST = (dwProtocol:2; dbPciLength:8);
  SCARD_PROTOCOL_UNK = $00000000;

  GSMStatusOK           = $9000;
  GSMStatusMemoryError  = $9240;
  GSMStatusNoEFSelected = $9400;
  GSMStatusOutOfRange   = $9402;
  GSMStatusNotFound     = $9404;
  GSMStatusFCDoNotMatch = $9408;
  GSMStatusCHVNeeded    = $9802;
  GSMStatusAuthFailed   = $9804;
  GSMStatusAuthFailedBl = $9840;
  GSMStatusTechProblem  = $6F00;
  GSMStatusResponseData = $9F;

  GSMFileTypeRFU = 0;
  GSMFileTypeMF  = 1;
  GSMFileTypeDF  = 2;
  GSMFileTypeEF  = 4;

  GSMEfTransp    = 0;
  GSMEfLinFixed  = 1;
  GSMEfCyclic    = 3;

  SCARD_ALL_READERS     =  'SCard$AllReaders' + Chr(0) + Chr(0);
  {$EXTERNALSYM SCARD_ALL_READERS}
  SCARD_DEFAULT_READERS =  'SCard$DefaultReaders' + Chr(0) + Chr(0);
  {$EXTERNALSYM SCARD_DEFAULT_READERS}
  SCARD_LOCAL_READERS   =  'SCard$LocalReaders' + Chr(0) + Chr(0);
  {$EXTERNALSYM SCARD_LOCAL_READERS}
  SCARD_SYSTEM_READERS  =  'SCard$SystemReaders' + Chr(0) + Chr(0);
  {$EXTERNALSYM SCARD_SYSTEM_READERS}

  SCARD_PROVIDER_PRIMARY = 1;   // Primary Provider Id
  {$EXTERNALSYM SCARD_PROVIDER_PRIMARY}
  SCARD_PROVIDER_CSP     = 2;   // Crypto Service Provider Id
  {$EXTERNALSYM SCARD_PROVIDER_CSP}

//
// T=0 protocol services.
//
type
   PSCARD_T0_COMMAND = ^SCARD_T0_COMMAND;
   {$EXTERNALSYM PSCARD_T0_COMMAND}
   SCARD_T0_COMMAND = record
      bCla: Byte;   // The instruction class
      bIns: Byte;   // The instruction code within the instruction class
      bP1: Byte;
      bP2: Byte;    // Parameters to the instruction
      bP3: Byte;    // Size of I/O Transfer
   end;
   {$EXTERNALSYM SCARD_T0_COMMAND}


   PSCARD_T0_REQUEST = ^SCARD_T0_REQUEST;
   {$EXTERNALSYM PSCARD_T0_REQUEST}
   SCARD_T0_REQUEST = record
      ioRequest: SCARD_IO_REQUEST;
      bSw1: Byte;
      bSw2: Byte;    // Return codes from the instruction
      case Integer of
         0: (CmdBytes: SCARD_T0_COMMAND);
         1: (rgbHeader: array[0..4] of Byte);
   end;
   {$EXTERNALSYM SCARD_T0_REQUEST}

//
//  T=1 Protocol Services
//
type
   PSCARD_T1_REQUEST = ^SCARD_T1_REQUEST;
   {$EXTERNALSYM PSCARD_T1_REQUEST}
   SCARD_T1_REQUEST = record
      ioRequest: SCARD_IO_REQUEST;
   end;
   {$EXTERNALSYM SCARD_T1_REQUEST}

//
////////////////////////////////////////////////////////////////////////////////
//
//  Driver attribute flags
//
const
   SCARD_READER_SWALLOWS     = $00000001;  // Reader has a card swallowing
   {$EXTERNALSYM SCARD_READER_SWALLOWS}    // mechanism.
   SCARD_READER_EJECTS       = $00000002;  // Reader has a card ejection
   {$EXTERNALSYM SCARD_READER_EJECTS}      // mechanism.
   SCARD_READER_CONFISCATES  = $00000004;  // Reader has a card capture
   {$EXTERNALSYM SCARD_READER_CONFISCATES} // mechanism.

//
///////////////////////////////////////////////////////////////////////////////
//
// Type of reader
//
const
   SCARD_READER_TYPE_SERIAL   = $01;
   {$EXTERNALSYM SCARD_READER_TYPE_SERIAL}
   SCARD_READER_TYPE_PARALELL = $02;
   {$EXTERNALSYM SCARD_READER_TYPE_PARALELL}
   SCARD_READER_TYPE_KEYBOARD = $04;
   {$EXTERNALSYM SCARD_READER_TYPE_KEYBOARD}
   SCARD_READER_TYPE_SCSI     = $08;
   {$EXTERNALSYM SCARD_READER_TYPE_SCSI}
   SCARD_READER_TYPE_IDE      = $10;
   {$EXTERNALSYM SCARD_READER_TYPE_IDE}
   SCARD_READER_TYPE_USB      = $20;
   {$EXTERNALSYM SCARD_READER_TYPE_USB}
   SCARD_READER_TYPE_PCMCIA   = $40;
   {$EXTERNALSYM SCARD_READER_TYPE_PCMCIA}
   SCARD_READER_TYPE_VENDOR   = $F0;
   {$EXTERNALSYM SCARD_READER_TYPE_VENDOR}

implementation

function CTL_CODE(DeviceType, Func, Method, Access: WORD): LongWord;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

function SCARD_CTL_CODE(code: Integer): LongWord;
begin
   Result := CTL_CODE(FILE_DEVICE_SMARTCARD, code, 0{METHOD_BUFFERED}, 0{FILE_ANY_ACCESS});
end;

function IOCTL_SMARTCARD_POWER: LongWord;
begin
   Result := SCARD_CTL_CODE(1);
end;

function IOCTL_SMARTCARD_GET_ATTRIBUTE: LongWord;
begin
   Result := SCARD_CTL_CODE(2);
end;

function IOCTL_SMARTCARD_SET_ATTRIBUTE: LongWord;
begin
   Result := SCARD_CTL_CODE(3);
end;

function IOCTL_SMARTCARD_CONFISCATE: LongWord;
begin
   Result := SCARD_CTL_CODE(4);
end;

function IOCTL_SMARTCARD_TRANSMIT: LongWord;
begin
   Result := SCARD_CTL_CODE(5);
end;

function IOCTL_SMARTCARD_EJECT: LongWord;
begin
   Result := SCARD_CTL_CODE(6);
end;

function IOCTL_SMARTCARD_SWALLOW: LongWord;
begin
   Result := SCARD_CTL_CODE(7);
end;

function IOCTL_SMARTCARD_IS_PRESENT: LongWord;
begin
   Result := SCARD_CTL_CODE(10);
end;

function IOCTL_SMARTCARD_IS_ABSENT: LongWord;
begin
   Result := SCARD_CTL_CODE(11);
end;

function IOCTL_SMARTCARD_SET_PROTOCOL: LongWord;
begin
   Result := SCARD_CTL_CODE(12);
end;

function IOCTL_SMARTCARD_GET_STATE: LongWord;
begin
   Result := SCARD_CTL_CODE(14);
end;

function IOCTL_SMARTCARD_GET_LAST_ERROR: LongWord;
begin
   Result := SCARD_CTL_CODE(15);
end;

function IOCTL_SMARTCARD_GET_PERF_CNTR: LongWord;
begin
   Result := SCARD_CTL_CODE(16);
end;

function SCARD_ATTR_VALUE(ulClass, ulTag: Cardinal): Cardinal;
begin
   Result := (ulClass shl 16) or ulTag;
end;

function SCARD_ATTR_VENDOR_NAME: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0100);
end;

function SCARD_ATTR_VENDOR_IFD_TYPE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0101);
end;

function SCARD_ATTR_VENDOR_IFD_VERSION: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0102);
end;

function SCARD_ATTR_VENDOR_IFD_SERIAL_NO: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_INFO, $0103);
end;

function SCARD_ATTR_CHANNEL_ID: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_COMMUNICATIONS, $0110);
end;

function SCARD_ATTR_PROTOCOL_TYPES: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0120);
end;

function SCARD_ATTR_DEFAULT_CLK: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0121);
end;

function SCARD_ATTR_MAX_CLK: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0122);
end;

function SCARD_ATTR_DEFAULT_DATA_RATE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0123);
end;

function SCARD_ATTR_MAX_DATA_RATE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0124);
end;

function SCARD_ATTR_MAX_IFSD: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PROTOCOL, $0125);
end;

function SCARD_ATTR_POWER_MGMT_SUPPORT: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_POWER_MGMT, $0131);
end;

function SCARD_ATTR_USER_TO_CARD_AUTH_DEVICE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SECURITY, $0140);
end;

function SCARD_ATTR_USER_AUTH_INPUT_DEVICE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SECURITY, $0142);
end;

function SCARD_ATTR_CHARACTERISTICS: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_MECHANICAL, $0150);
end;

function SCARD_ATTR_CURRENT_PROTOCOL_TYPE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0201);
end;

function SCARD_ATTR_CURRENT_CLK: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0202);
end;

function SCARD_ATTR_CURRENT_F: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0203);
end;

function SCARD_ATTR_CURRENT_D: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0204);
end;

function SCARD_ATTR_CURRENT_N: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0205);
end;

function SCARD_ATTR_CURRENT_W: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0206);
end;

function SCARD_ATTR_CURRENT_IFSC: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0207);
end;

function SCARD_ATTR_CURRENT_IFSD: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0208);
end;

function SCARD_ATTR_CURRENT_BWT: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $0209);
end;

function SCARD_ATTR_CURRENT_CWT: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $020a);
end;

function SCARD_ATTR_CURRENT_EBC_ENCODING: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $020b);
end;

function SCARD_ATTR_EXTENDED_BWT: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_IFD_PROTOCOL, $020c);
end;

function SCARD_ATTR_ICC_PRESENCE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0300);
end;

function SCARD_ATTR_ICC_INTERFACE_STATUS: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0301);
end;

function SCARD_ATTR_CURRENT_IO_STATE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0302) ;
end;

function SCARD_ATTR_ATR_STRING: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0303);
end;

function SCARD_ATTR_ICC_TYPE_PER_ATR: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_ICC_STATE, $0304);
end;

function SCARD_ATTR_ESC_RESET: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A000);
end;

function SCARD_ATTR_ESC_CANCEL: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A003);
end;

function SCARD_ATTR_ESC_AUTHREQUEST: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A005);
end;

function SCARD_ATTR_MAXINPUT: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_VENDOR_DEFINED, $A007);
end;

function SCARD_ATTR_DEVICE_UNIT: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0001);
end;

function SCARD_ATTR_DEVICE_IN_USE: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0002);
end;

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_A: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0003);
end;

function SCARD_ATTR_DEVICE_SYSTEM_NAME_A: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0004);
end;

function SCARD_ATTR_DEVICE_FRIENDLY_NAME_W: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0005);
end;

function SCARD_ATTR_DEVICE_SYSTEM_NAME_W: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0006);
end;

function SCARD_ATTR_SUPRESS_T1_IFS_REQUEST: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_SYSTEM, $0007);
end;

function SCARD_PERF_NUM_TRANSMISSIONS: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PERF, $0001);
end;

function SCARD_PERF_BYTES_TRANSMITTED: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PERF, $0002);
end;

function SCARD_PERF_TRANSMISSION_TIME: Cardinal;
begin
   Result := SCARD_ATTR_VALUE(SCARD_CLASS_PERF, $0003);
end;

end.
