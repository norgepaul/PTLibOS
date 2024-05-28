{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.MachineFingerprint                          }
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

unit PTLib.Common.MachineFingerprint;

interface

uses
  SysUtils, Classes, Variants, Hash

  {$IFDEF MSWINDOWS}
  , ActiveX, ComObj
  {$ENDIF}

  , PTLib.Common.Crypto;

type
  TMotherBoardInfo = (
    Mb_SerialNumber,
    Mb_Manufacturer,
    Mb_Product,
    Mb_Model);
  TMotherBoardInfoSet = set of TMotherBoardInfo;

  TProcessorInfo = (
    Pr_Description,
    Pr_Manufacturer,
    Pr_Name,
    Pr_ProcessorId,
    Pr_UniqueId);
  TProcessorInfoSet = set of TProcessorInfo;

  TBIOSInfo = (
    Bs_BIOSVersion,
    Bs_BuildNumber,
    Bs_Description,
    Bs_Manufacturer,
    Bs_Name,
    Bs_SerialNumber,
    Bs_Version);
  TBIOSInfoSet = set of TBIOSInfo;

  TOSInfo = (
    Os_BuildNumber,
    Os_BuildType,
    Os_Manufacturer,
    Os_Name,
    Os_SerialNumber,
    Os_Version);
  TOSInfoSet = set of TOSInfo;

const // properties names to get the data
  MotherBoardInfoArr: array [TMotherBoardInfo] of String = (
    'SerialNumber',
    'Manufacturer',
    'Product',
    'Model');

  OsInfoArr: array [TOSInfo] of String = (
    'BuildNumber',
    'BuildType',
    'Manufacturer',
    'Name',
    'SerialNumber',
    'Version');

  BiosInfoArr: array [TBIOSInfo] of String = (
    'BIOSVersion',
    'BuildNumber',
    'Description',
    'Manufacturer',
    'Name',
    'SerialNumber',
    'Version');

  ProcessorInfoArr: array [TProcessorInfo] of String = (
    'Description',
    'Manufacturer',
    'Name',
    'ProcessorId',
    'UniqueId');

type
  THardwareId = class
  private
    FOSInfo: TOSInfoSet;
    FBIOSInfo: TBIOSInfoSet;
    FProcessorInfo: TProcessorInfoSet;
    FMotherBoardInfo: TMotherBoardInfoSet;
    FBuffer: String;

    function GetHardwareIdHex: String;
  public
    // Set the properties to  be used in the generation of the hardware id
    property MotherBoardInfo: TMotherBoardInfoSet read FMotherBoardInfo
      write FMotherBoardInfo;
    property ProcessorInfo: TProcessorInfoSet read FProcessorInfo
      write FProcessorInfo;
    property BIOSInfo: TBIOSInfoSet read FBIOSInfo write FBIOSInfo;
    property OSInfo: TOSInfoSet read FOSInfo write FOSInfo;
    property Buffer: String read FBuffer;
    // return the content of the data collected in the system
    property HardwareIdHex: String read GetHardwareIdHex;
    // get a hexadecimal represntation of the data collected
    procedure GenerateHardwareId; // calculate the hardware id
    constructor Create(Generate: Boolean = True); overload;
  end;

function GetHardwareFingerprint: String;

implementation

function VarArrayToStr(const vArray: variant): String;

  function _VarToStr(const V: variant): String;
  var
    Vt: Integer;
  begin
    Vt := VarType(V);
    case Vt of
      varSmallint, varInteger:
        Result := String(IntToStr(Integer(V)));
      varSingle, varDouble, varCurrency:
        Result := String(FloatToStr(Double(V)));
      varDate:
        Result := String(VarToStr(V));
      varOleStr:
        Result := String(WideString(V));
      varBoolean:
        Result := String(VarToStr(V));
      varVariant:
        Result := String(VarToStr(variant(V)));
      varByte:
        Result := Char(byte(V));
      varString:
        Result := String(V);
      varArray:
        Result := VarArrayToStr(variant(V));
    end;
  end;

var
  i: Integer;
begin
  Result := '[';

  if (VarType(vArray) and varArray) = 0 then
  begin
    Result := _VarToStr(vArray)
  end
  else
  begin
    for i := VarArrayLowBound(vArray, 1) to VarArrayHighBound(vArray, 1) do
    begin
      if i = VarArrayLowBound(vArray, 1) then
      begin
        Result := Result + _VarToStr(vArray[i])
      end
      else
      begin
        Result := Result + '|' + _VarToStr(vArray[i]);
      end;
    end;
  end;

  Result := Result + ']';
end;

function VarStrNull(const V: OleVariant): String;
begin
  Result := '';

  if not VarIsNull(V) then
  begin
    if VarIsArray(V) then
    begin
      Result := VarArrayToStr(V)
    end
    else
    begin
      Result := String(VarToStr(V));
    end;
  end;
end;

{ THardwareId }

constructor THardwareId.Create(Generate: Boolean = True);
begin
  inherited Create;

  FBuffer := '';
  // Set the propeties to be used in the hardware id generation

  FMotherBoardInfo := [Mb_SerialNumber, Mb_Manufacturer, Mb_Product, Mb_Model];
  FOSInfo := [Os_BuildNumber, Os_BuildType, Os_Manufacturer, Os_Name, Os_SerialNumber, Os_Version];
  FBIOSInfo := [Bs_BIOSVersion, Bs_BuildNumber, Bs_Description, Bs_Manufacturer, Bs_Name, Bs_SerialNumber, Bs_Version];
  FProcessorInfo := [];

  // including the processor info is expensive [Pr_Description,Pr_Manufacturer,Pr_Name,Pr_ProcessorId,Pr_UniqueId];
  if Generate then
  begin
    GenerateHardwareId;
  end;
end;

// Main function which collect the system data.

{$IFDEF MSWINDOWS}
procedure THardwareId.GenerateHardwareId;
var
  objSWbemLocator: OleVariant;
  objWMIService: OleVariant;
  objWbemObjectSet: OleVariant;
  oWmiObject: OleVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
  SDummy: String;
  Mb: TMotherBoardInfo;
  Os: TOSInfo;
  Bs: TBIOSInfo;
  Pr: TProcessorInfo;
begin;
  objSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  objWMIService := objSWbemLocator.ConnectServer('localhost',
    'root\cimv2', '', '');

  if FMotherBoardInfo <> [] then // MotherBoard info
  begin
    objWbemObjectSet := objWMIService.ExecQuery('SELECT * FROM Win32_BaseBoard', 'WQL', 0);
    oEnum := IUnknown(objWbemObjectSet._NewEnum) as IEnumvariant;
    while oEnum.Next(1, oWmiObject, iValue) = 0 do
    begin
      for Mb := Low(TMotherBoardInfo) to High(TMotherBoardInfo) do
        if Mb in FMotherBoardInfo then
        begin
          SDummy := VarStrNull(oWmiObject.Properties_.Item(MotherBoardInfoArr
            [Mb]).Value);
          FBuffer := FBuffer + SDummy;
        end;
      oWmiObject := Unassigned;
    end;
  end;

  if FOSInfo <> [] then // Windows info
  begin
    objWbemObjectSet := objWMIService.ExecQuery
      ('SELECT * FROM Win32_OperatingSystem', 'WQL', 0);
    oEnum := IUnknown(objWbemObjectSet._NewEnum) as IEnumvariant;
    while oEnum.Next(1, oWmiObject, iValue) = 0 do
    begin
      for Os := Low(TOSInfo) to High(TOSInfo) do
        if Os in FOSInfo then
        begin
          SDummy := VarStrNull
            (oWmiObject.Properties_.Item(OsInfoArr[Os]).Value);
          FBuffer := FBuffer + SDummy;
        end;
      oWmiObject := Unassigned;
    end;
  end;

  if FBIOSInfo <> [] then // BIOS info
  begin
    objWbemObjectSet := objWMIService.ExecQuery
      ('SELECT * FROM Win32_BIOS', 'WQL', 0);
    oEnum := IUnknown(objWbemObjectSet._NewEnum) as IEnumvariant;
    while oEnum.Next(1, oWmiObject, iValue) = 0 do
    begin
      for Bs := Low(TBIOSInfo) to High(TBIOSInfo) do
        if Bs in FBIOSInfo then
        begin
          SDummy := VarStrNull
            (oWmiObject.Properties_.Item(BiosInfoArr[Bs]).Value);
          FBuffer := FBuffer + SDummy;
        end;
      oWmiObject := Unassigned;
    end;
  end;

  if FProcessorInfo <> [] then // CPU info
  begin
    objWbemObjectSet := objWMIService.ExecQuery
      ('SELECT * FROM Win32_Processor', 'WQL', 0);
    oEnum := IUnknown(objWbemObjectSet._NewEnum) as IEnumvariant;
    while oEnum.Next(1, oWmiObject, iValue) = 0 do
    begin
      for Pr := Low(TProcessorInfo) to High(TProcessorInfo) do
        if Pr in FProcessorInfo then
        begin
          SDummy := VarStrNull
            (oWmiObject.Properties_.Item(ProcessorInfoArr[Pr]).Value);
          FBuffer := FBuffer + SDummy;
        end;
      oWmiObject := Unassigned;
    end;
  end;
end;
{$ELSE}

procedure THardwareId.GenerateHardwareId;
begin
  // Unsupported O/S
end;

{$ENDIF}


function THardwareId.GetHardwareIdHex: String;
var
  i: Integer;
begin
  Result := '';

  for i := Low(FBuffer) to High(FBuffer) do
  begin
    Result := Result + IntToHex(Ord(FBuffer[i]), 2);
  end;
end;

function GetHardwareFingerprint: String;
var
  HWID: THardwareId;
begin
  HWID := THardwareId.Create(False);
  try
    HWID.GenerateHardwareId;

    Result := THashMD5.GetHashString(HWID.HardwareIdHex);
  finally
    FreeAndNil(HWID);
  end;
end;

end.
