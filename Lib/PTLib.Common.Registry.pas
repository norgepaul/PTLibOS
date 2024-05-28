{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.Registry                                    }
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

unit PTLib.Common.Registry;

interface

uses
  SysUtils, Registry;

const
  BROWSER_EMULATION_MSIE11_FORCED = 11001;
  BROWSER_EMULATION_MSIE11 = 11000; // currently this is the best rendering engine we can have
  BROWSER_EMULATION_MSIE10_FORCED = 10001;
  BROWSER_EMULATION_MSIE10 = 10000;
  BROWSER_EMULATION_MSIE9_FORCED = 9999;
  BROWSER_EMULATION_MSIE9 = 9000;
  BROWSER_EMULATION_MSIE8_FORCED = 8888;
  BROWSER_EMULATION_MSIE8 = 8000;
  BROWSER_EMULATION_MSIE7 = 7000;

procedure SetBrowserEmulation(const Value: Integer = BROWSER_EMULATION_MSIE11);

implementation

procedure ChangeFeatureControlRegValue(const Feature, ExeName: string; const Value: Integer);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
     if Reg.OpenKey('\Software\Microsoft\Internet Explorer\Main\FeatureControl\' + Feature, True) then
     begin
       try
         Reg.WriteInteger(ExeName, Value);
       finally
         Reg.CloseKey;
       end;
     end;
  finally
    Reg.Free;
  end;
end;

procedure SetBrowserEmulation(const Value: Integer = BROWSER_EMULATION_MSIE11);
var
  ExeName: String;
begin
  Exename := ExtractFilename(ParamStr(0));

  ChangeFeatureControlRegValue('FEATURE_BROWSER_EMULATION', ExeName, Value);
end;

end.
