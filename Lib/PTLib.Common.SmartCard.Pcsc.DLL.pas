{******************************************************************}
{                                                                  }
{ Borland Delphi Runtime Library                                   }
{ PCSC interface unit                                              }
{                                                                  }
{ Portions created by Microsoft are                                }
{ Copyright (C) 1996 Microsoft Corporation.                        }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: WinSCard.h                                 }
{ The original Pascal code is: WinSCard.pas                        }
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
unit PTLib.Common.SmartCard.Pcsc.DLL; {Originally WinSCard}

interface

uses
  System.Types,

  PTLib.Common.SmartCard.Pcsc.DLL.Types;

{$if Defined(MSWINDOWS) or defined(LINUX64)}
const
  {$if Defined(MSWINDOWS)}
    SCardDLL = 'winscard.dll';

   function SCardEstablishContext(dwScope: DWORD; pvReserved1: Pointer; pvReserved2: Pointer; phContext: Pointer): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardEstablishContext}
   function SCardReleaseContext(hContext: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardReleaseContext}
   function SCardIsValidContext(hContext: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIsValidContext}
   function SCardListReaderGroupsA(hContext: LongInt; mszGroups: MarshaledString; var pcchGroups: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListReaderGroupsA}
   function SCardListReaderGroupsW(hContext: LongInt; mszGroups: PWideChar; var pcchGroups: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListReaderGroupsW}
   function SCardListReadersA(SCARDCONTEXT: LongInt; mszGroups: MarshaledString; mszReaders: MarshaledString; var pcchReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListReadersA}
   function SCardListReadersW(SCARDCONTEXT: LongInt; mszGroups: PWideChar; mszReaders: PWideChar; var pcchReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListReadersW}
   function SCardListCardsA(hContext: LongInt; var pbAtr: Byte; var rgguidInterfaces: GUID; cguidInterfaceCount: LongInt; mszCards: MarshaledString; var pcchCards: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListCardsA}
   function SCardListCardsW(hContext: LongInt; var pbAtr: Byte; var rgguidInterfaces: GUID; cguidInterfaceCount: LongInt; mszCards: PWideChar; var pcchCards: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListCardsW}
   function SCardListInterfacesA(hContext: LongInt; szCard: MarshaledString; var pguidInterfaces: GUID; var pcguidInterfaces: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListInterfacesA}
   function SCardListInterfacesW(hContext: LongInt; szCard: PWideChar; var pguidInterfaces: GUID; var pcguidInterfaces: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListInterfacesW}
   function SCardGetProviderIdA(hContext: LongInt; szCard: MarshaledString; var pguidProviderId: GUID): LongInt; stdcall;  external SCardDLL;
   {$EXTERNALSYM SCardGetProviderIdA}
   function SCardGetProviderIdW(hContext: LongInt; szCard: PWideChar; var pguidProviderId: GUID): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetProviderIdW}
   function SCardGetCardTypeProviderNameA(hContext: LongInt; szCardName: MarshaledString; dwProviderId: LongInt;
                                          szProvider: MarshaledString; var pcchProvider: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetCardTypeProviderNameA}
   function SCardGetCardTypeProviderNameW(hContext: LongInt; szCardName: PWideChar; dwProviderId: LongInt;
                                          szProvider: PWideChar; var pcchProvider: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetCardTypeProviderNameW}
   function SCardIntroduceReaderGroupA(hContext: LongInt; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderGroupA}
   function SCardIntroduceReaderGroupW(hContext: LongInt; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderGroupW}

   function SCardForgetReaderGroupA(hContext: LongInt; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderGroupA}
   function SCardForgetReaderGroupW(hContext: LongInt; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderGroupW}

   function SCardIntroduceReaderA(hContext: LongInt; szReadeName: MarshaledString; szDeviceName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderA}
   function SCardIntroduceReaderW(hContext: LongInt; szReadeName: PWideChar; szDeviceName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderW}

   function SCardForgetReaderA(hContext: LongInt; szReaderName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderA}
   function SCardForgetReaderW(hContext: LongInt; szReaderName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderW}

   function SCardAddReaderToGroupA(hContext: LongInt; szReaderName: MarshaledString; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardAddReaderToGroupA}
   function SCardAddReaderToGroupW(hContext: LongInt; szReaderName: PWideChar; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardAddReaderToGroupW}

   function SCardRemoveReaderFromGroupA(hContext: LongInt; szReaderName: MarshaledString; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardRemoveReaderFromGroupA}
   function SCardRemoveReaderFromGroupW(hContext: LongInt; szReaderName: PWideChar; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardRemoveReaderFromGroupW}

   function SCardIntroduceCardTypeA(hContext: LongInt; szCardName: MarshaledString; var pguidPrimaryProvider: GUID;
                                    var pguidInterfaces: GUID; dwInterfaceCount: LongInt; pbAtr: MarshaledString;
                                    pbAtrMask: MarshaledString; cbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceCardTypeA}
   function SCardIntroduceCardTypeW(hContext: LongInt; szCardName: PWideChar; var pguidPrimaryProvider: GUID;
                                    var pguidInterfaces: GUID; dwInterfaceCount: LongInt; pbAtr: PWideChar;
                                    pbAtrMask: PWideChar; cbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceCardTypeW}

   function SCardSetCardTypeProviderNameA(hContext: LongInt; szCardName: MarshaledString; dwProviderId: LongInt;
                                          szProvider: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardSetCardTypeProviderNameA}
   function SCardSetCardTypeProviderNameW(hContext: LongInt; szCardName: PWideChar; dwProviderId: LongInt;
                                          szProvider: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardSetCardTypeProviderNameW}
   function SCardForgetCardTypeA(hContext: LongInt; szCardName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetCardTypeA}
   function SCardForgetCardTypeW(hContext: LongInt; szCardName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetCardTypeW}
   function SCardFreeMemory(hContext: LongInt; pvMem: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardFreeMemory}

   function SCardAccessStartedEvent: LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardAccessStartedEvent}

   function SCardReleaseStartedEvent: LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardReleaseStartedEvent}
   function SCardLocateCardsA(hContext: LongInt; mszCards: MarshaledString; var rgReaderStates: PSCARD_READERSTATEA;
                              cReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsA}
   function SCardLocateCardsW(hContext: LongInt; mszCards: PWideChar; var rgReaderStates: PSCARD_READERSTATEW;
                              cReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsW}

   function SCardLocateCardsByATRA(hContext: LongInt; rgAtrMasks: LPSCARD_ATRMASK; cAtrs: DWORD;
                                   var rgReaderStates: SCARD_READERSTATEA; cReaders: DWORD): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsByATRA}
   function SCardLocateCardsByATRW(hContext: LongInt; rgAtrMasks: LPSCARD_ATRMASK; cAtrs: DWORD;
                                   var rgReaderStates: SCARD_READERSTATEW; cReaders: DWORD): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsByATRW}

   function  SCardGetStatusChangeA(    hContext:       SCARDCONTEXT;
                                      dwTimeout:      LongInt;
                                  var rgReaderStates: array of SCARD_READERSTATEA;
                                      cReaders:       LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetStatusChangeA}

   function SCardGetStatusChangeW(hContext: LongInt; dwTimeout: LongInt; var rgReaderStates: PSCARD_READERSTATEW;
                                  cReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetStatusChangeW}
   function SCardCancel (hContext: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardCancel}

   function SCardConnectA(hContext: LongInt; szReader: MarshaledString; dwShareMode: LongInt; dwPreferredProtocols: LongInt;
                          var phCard: LongInt; pdwActiveProtocol: PDword): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardConnectA}
   function SCardConnectW(hContext: LongInt; szReader: PWideChar; dwShareMode: LongInt; dwPreferredProtocols: LongInt;
                          var phCard: LongInt; pdwActiveProtocol: PDword): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardConnectW}

   function SCardReconnect(hCard: LongInt; dwShareMode: LongInt; dwPreferredProtocols: LongInt;
                           dwInitialization: LongInt; var pdwActiveProtocol: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardReconnect}

   function SCardDisconnect(hCard: LongInt; dwDisposition: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardDisconnect}

   function SCardBeginTransaction(hCard: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardBeginTransaction}

   function SCardEndTransaction(hCard: LongInt; dwDisposition: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardEndTransaction}

   function SCardStatusA(hCard: LongInt; mszReaderNames: MarshaledString; var pcchReaderLen: LongInt; var pdwState: LongInt;
                         pdwProtocol: PDWord; pbAtr: PByte; var pcbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardStatusA}
   function SCardStatusW(hCard: LongInt; mszReaderNames: PWideChar; var pcchReaderLen: LongInt; var pdwState: LongInt;
                         pdwProtocol: PDWord; pbAtr: PByte; var pcbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardStatusW}

   function SCardTransmit(hCard: LongInt; pioSendPci: Pointer; pbSendBuffer: PByte;
                          dwSendLength: DWORD; pioRecvPci: Pointer; pbRecvBuffer: PByte;
                          pcbRecvLength: PDWord): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardTransmit}
   function SCardControl(hCard: LongInt; dwControlCode: LongInt; var pvInBuffer: PByte; cbInBufferSize: LongInt;
                         var pvOutBuffer: PByte; cbOutBufferSize: LongInt;
                         var pcbBytesReturned: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardControl}
   function SCardGetAttrib(hCard: LongInt; dwAttrId: LongInt; pbAttr: PByte; pcbAttrLen: PDWord): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetAttrib}
   function SCardSetAttrib(hCard: LongInt; dwAttrId: LongInt; var pbAttr: PByte; cbAttrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardSetAttrib}
  {$endif}

  {$if defined(LINUX64)}
    SCardDLL = 'libpcsclite.so';

   function SCardEstablishContext(dwScope: DWORD; pvReserved1: Pointer; pvReserved2: Pointer; phContext: Pointer): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardEstablishContext}
   function SCardReleaseContext(hContext: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardReleaseContext}
   function SCardIsValidContext(hContext: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIsValidContext}
   function SCardListReaderGroupsA(hContext: LongInt; mszGroups: MarshaledString; var pcchGroups: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListReaderGroupsA}
   function SCardListReaderGroupsW(hContext: LongInt; mszGroups: PWideChar; var pcchGroups: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListReaderGroupsW}
   function SCardListReaders(SCARDCONTEXT: LongInt; mszGroups: PWideChar; mszReaders: PAnsiChar; var pcchReaders: LongInt): LongInt; stdcall; external SCardDLL;
   //{$EXTERNALSYM SCardListReaders}
   function SCardListCardsA(hContext: LongInt; var pbAtr: Byte; var rgguidInterfaces: GUID; cguidInterfaceCount: LongInt; mszCards: MarshaledString; var pcchCards: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListCardsA}
   function SCardListCardsW(hContext: LongInt; var pbAtr: Byte; var rgguidInterfaces: GUID; cguidInterfaceCount: LongInt; mszCards: PWideChar; var pcchCards: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListCardsW}
   function SCardListInterfacesA(hContext: LongInt; szCard: MarshaledString; var pguidInterfaces: GUID; var pcguidInterfaces: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListInterfacesA}
   function SCardListInterfacesW(hContext: LongInt; szCard: PWideChar; var pguidInterfaces: GUID; var pcguidInterfaces: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardListInterfacesW}
   function SCardGetProviderIdA(hContext: LongInt; szCard: MarshaledString; var pguidProviderId: GUID): LongInt; stdcall;  external SCardDLL;
   {$EXTERNALSYM SCardGetProviderIdA}
   function SCardGetProviderIdW(hContext: LongInt; szCard: PWideChar; var pguidProviderId: GUID): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetProviderIdW}
   function SCardGetCardTypeProviderNameA(hContext: LongInt; szCardName: MarshaledString; dwProviderId: LongInt;
                                          szProvider: MarshaledString; var pcchProvider: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetCardTypeProviderNameA}
   function SCardGetCardTypeProviderNameW(hContext: LongInt; szCardName: PWideChar; dwProviderId: LongInt;
                                          szProvider: PWideChar; var pcchProvider: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetCardTypeProviderNameW}
   function SCardIntroduceReaderGroupA(hContext: LongInt; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderGroupA}
   function SCardIntroduceReaderGroupW(hContext: LongInt; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderGroupW}

   function SCardForgetReaderGroupA(hContext: LongInt; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderGroupA}
   function SCardForgetReaderGroupW(hContext: LongInt; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderGroupW}

   function SCardIntroduceReaderA(hContext: LongInt; szReadeName: MarshaledString; szDeviceName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderA}
   function SCardIntroduceReaderW(hContext: LongInt; szReadeName: PWideChar; szDeviceName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceReaderW}

   function SCardForgetReaderA(hContext: LongInt; szReaderName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderA}
   function SCardForgetReaderW(hContext: LongInt; szReaderName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetReaderW}

   function SCardAddReaderToGroupA(hContext: LongInt; szReaderName: MarshaledString; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardAddReaderToGroupA}
   function SCardAddReaderToGroupW(hContext: LongInt; szReaderName: PWideChar; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardAddReaderToGroupW}

   function SCardRemoveReaderFromGroupA(hContext: LongInt; szReaderName: MarshaledString; szGroupName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardRemoveReaderFromGroupA}
   function SCardRemoveReaderFromGroupW(hContext: LongInt; szReaderName: PWideChar; szGroupName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardRemoveReaderFromGroupW}

   function SCardIntroduceCardTypeA(hContext: LongInt; szCardName: MarshaledString; var pguidPrimaryProvider: GUID;
                                    var pguidInterfaces: GUID; dwInterfaceCount: LongInt; pbAtr: MarshaledString;
                                    pbAtrMask: MarshaledString; cbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceCardTypeA}
   function SCardIntroduceCardTypeW(hContext: LongInt; szCardName: PWideChar; var pguidPrimaryProvider: GUID;
                                    var pguidInterfaces: GUID; dwInterfaceCount: LongInt; pbAtr: PWideChar;
                                    pbAtrMask: PWideChar; cbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardIntroduceCardTypeW}

   function SCardSetCardTypeProviderNameA(hContext: LongInt; szCardName: MarshaledString; dwProviderId: LongInt;
                                          szProvider: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardSetCardTypeProviderNameA}
   function SCardSetCardTypeProviderNameW(hContext: LongInt; szCardName: PWideChar; dwProviderId: LongInt;
                                          szProvider: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardSetCardTypeProviderNameW}
   function SCardForgetCardTypeA(hContext: LongInt; szCardName: MarshaledString): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetCardTypeA}
   function SCardForgetCardTypeW(hContext: LongInt; szCardName: PWideChar): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardForgetCardTypeW}
   function SCardFreeMemory(hContext: LongInt; pvMem: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardFreeMemory}

   function SCardAccessStartedEvent: LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardAccessStartedEvent}

   function SCardReleaseStartedEvent: LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardReleaseStartedEvent}
   function SCardLocateCardsA(hContext: LongInt; mszCards: MarshaledString; var rgReaderStates: PSCARD_READERSTATEA;
                              cReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsA}
   function SCardLocateCardsW(hContext: LongInt; mszCards: PWideChar; var rgReaderStates: PSCARD_READERSTATEW;
                              cReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsW}

   function SCardLocateCardsByATRA(hContext: LongInt; rgAtrMasks: LPSCARD_ATRMASK; cAtrs: DWORD;
                                   var rgReaderStates: SCARD_READERSTATEA; cReaders: DWORD): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsByATRA}
   function SCardLocateCardsByATRW(hContext: LongInt; rgAtrMasks: LPSCARD_ATRMASK; cAtrs: DWORD;
                                   var rgReaderStates: SCARD_READERSTATEW; cReaders: DWORD): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardLocateCardsByATRW}

   function SCardGetStatusChange(hContext: LongInt; dwTimeout: LongInt; var rgReaderStates: Array of SCARD_READERSTATEW;
                                  cReaders: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetStatusChange}
   function SCardCancel (hContext: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardCancel}

   function SCardConnect(hContext: LongInt; szReader: PAnsiChar; dwShareMode: LongInt; dwPreferredProtocols: LongInt;
                          var phCard: LongInt; pdwActiveProtocol: PDword): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardConnect}

   function SCardReconnect(hCard: LongInt; dwShareMode: LongInt; dwPreferredProtocols: LongInt;
                           dwInitialization: LongInt; var pdwActiveProtocol: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardReconnect}

   function SCardDisconnect(hCard: LongInt; dwDisposition: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardDisconnect}

   function SCardBeginTransaction(hCard: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardBeginTransaction}

   function SCardEndTransaction(hCard: LongInt; dwDisposition: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardEndTransaction}

   function SCardStatusA(hCard: LongInt; mszReaderNames: MarshaledString; var pcchReaderLen: LongInt; var pdwState: LongInt;
                         pdwProtocol: PDWord; pbAtr: PByte; var pcbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardStatusA}
   function SCardStatusW(hCard: LongInt; mszReaderNames: PWideChar; var pcchReaderLen: LongInt; var pdwState: LongInt;
                         pdwProtocol: PDWord; pbAtr: PByte; var pcbAtrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardStatusW}

   function SCardTransmit(hCard: LongInt; pioSendPci: Pointer; pbSendBuffer: PByte;
                          dwSendLength: DWORD; pioRecvPci: Pointer; pbRecvBuffer: PByte;
                          pcbRecvLength: LPDWord): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardTransmit}
   function SCardControl(hCard: LongInt; dwControlCode: LongInt; var pvInBuffer: PByte; cbInBufferSize: LongInt;
                         var pvOutBuffer: PByte; cbOutBufferSize: LongInt;
                         var pcbBytesReturned: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardControl}
   function SCardGetAttrib(hCard: LongInt; dwAttrId: LongInt; pbAttr: PByte; pcbAttrLen: PDWord): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardGetAttrib}
   function SCardSetAttrib(hCard: LongInt; dwAttrId: LongInt; var pbAttr: PByte; cbAttrLen: LongInt): LongInt; stdcall; external SCardDLL;
   {$EXTERNALSYM SCardSetAttrib}

  {$endif}
{$ENDIF}

implementation

end.
