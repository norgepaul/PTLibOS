unit PTLib.ORM.Register;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is PTLib.ORM.Register.pas, released April 2015.
//
// The initial developer of the original code is Easy-IP AS (Oslo, Norway, www.easy-ip.net),
// written by Paul Spencer Thornton (paul.thornton@easy-ip.net, www.easy-ip.net).
//
// Portions created by Easy-IP AS are Copyright
// (C) 2015 Easy-IP AS. All Rights Reserved.

interface
uses
  SysUtils, DesignIntf, DesignEditors,
  Classes,

  PTLib.ORM,
  PTLib.ORM.Cache,
  PTLib.ORM.SQL.Connection.FireDAC;

  procedure Register;

implementation

const
  TAB_PTLib_ORM = 'ORM';

procedure Register;
begin
  // Register common components
  RegisterComponents(TAB_PTLib_ORM, [
    TORM,
    TORMFireDACConnectionProvider,
    TORMObjectCache]);
end;


end.
