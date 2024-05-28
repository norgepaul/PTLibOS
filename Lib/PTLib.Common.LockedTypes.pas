{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.LockedTypes                                 }
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

unit PTLib.Common.LockedTypes;

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.Generics.Collections;

type


  TPTLockedDataAccessRights = set of (lvRead, lvWrite);

  EPTLockedValue = class(exception);
  EPTLockedObject = class(exception);


  (* Thread safe intrinsic datatype container.
     When sharing values between processes, use this class
     to make read/write access safe and protected. *)
  TPTLockedValue<T> = Class(TObject)
  strict private
    FLock: TCriticalSection;
    FData: T;
    FOptions: TPTLockedDataAccessRights;
  strict protected
    function    GetValue: T;virtual;
    procedure   SetValue(Value: T);virtual;
    function    GetAccessRights: TPTLockedDataAccessRights;
    procedure   SetAccessRights(Rights: TPTLockedDataAccessRights);
  public
    type
      TPTLockedValueEntry = reference to procedure (var Data: T);
  public
    Constructor Create(const AOptions: TPTLockedDataAccessRights = [lvRead, lvWrite]);virtual;
    Destructor  Destroy;override;

    Procedure Lock;
    procedure Unlock;
    procedure Synchronize(const Entry: TPTLockedValueEntry);

    Property AccessRights: TPTLockedDataAccessRights read GetAccessRights;
    Property Value: T read GetValue write SetValue;
  end;

  (* Thread safe object container.
     NOTE #1: This object container CREATES the instance and maintains it!
              Use Synchronize() to execute a protected block of code with access
              to the object.

     Note #2: SetValue() does not overwrite the object reference, but
              attempts to perform TPersistent.Assign(). If the instance
              does not inherit from TPersistent an exception is thrown. *)
  TPTLockedObject<T: class, constructor> = class(TObject)
  strict private
    FData:      T;
    FLock:      TCriticalSection;
    FOptions:   TPTLockedDataAccessRights;
  strict protected
    function    GetValue: T;virtual;
    procedure   SetValue(Value: T);virtual;
    function    GetAccessRights: TPTLockedDataAccessRights;
    procedure   SetAccessRights(Rights: TPTLockedDataAccessRights);
  public
    type
      TPTLockedObjectEntry = reference to procedure (const Data: T);
  public
    Property    Value: T read GetValue write SetValue;
    Property    AccessRights: TPTLockedDataAccessRights read GetAccessRights;

    Function    Lock:T;
    procedure   Unlock;
    procedure   Synchronize(const Entry: TPTLockedObjectEntry);

    Constructor Create(const AOptions:TPTLockedDataAccessRights = [lvRead,lvWrite]);virtual;
    Destructor  Destroy;override;
  end;


  (* TPTLockedObjectList:
     This is a thread-safe object list implementation.
     It works more or less like TThreadList, except it deals with objects *)
  TPTLockedObjectList = Class(TInterfacedPersistent)
  strict private
    FObjects:   TObjectList<TObject>;
    FLock:      TCriticalSection;
  strict protected
    function GetEmpty: Boolean;virtual;
    function GetCount: Integer;virtual;

    (* QueryObject Proxy: TInterfacedPersistent allows us to
       act as a proxy for QueryInterface/GetInterface. Override
       and provide another child instance here to expose
       interfaces from that instread *)
    function GetOwner: TPersistent;override;
  public
    type
      TPTLockedObjectListProc = reference to procedure (item:TObject;var Cancel:Boolean);
  public
    constructor Create(OwnsObjects: Boolean = True);virtual;
    destructor  Destroy;Override;

    function    Contains(Instance: TObject): Boolean;virtual;
    function    Lock: TObjectList<TObject>;virtual;
    Procedure   UnLock;
    Procedure   Clear;

    procedure   ForEach(Callback: TPTLockedObjectListProc);

    Property    Count:Integer read GetCount;
    Property    Empty:Boolean read GetEmpty;
  end;


implementation

//############################################################################
//  TLockedObjectList
//############################################################################

constructor TPTLockedObjectList.Create(OwnsObjects: Boolean = True);
begin
  inherited Create;
  FObjects := TObjectList<TObject>.Create(OwnsObjects);
  FLock := TCriticalSection.Create;
end;

destructor TPTLockedObjectList.Destroy;
begin
  FLock.Enter;
  FObjects.Free;
  FLock.Free;
  inherited;
end;

procedure TPTLockedObjectList.Clear;
begin
  FLock.Enter;
  try
    FObjects.Clear;
  finally
    FLock.Leave;
  end;
end;

function TPTLockedObjectList.GetOwner: TPersistent;
begin
  result := NIL;
end;

procedure TPTLockedObjectList.ForEach(Callback: TPTLockedObjectListProc);
var
  mItem:  TObject;
  mCancel:  Boolean;
begin
  if assigned(Callback) then
  begin
    FLock.Enter;
    try
      mCancel:=False;
      for mItem in FObjects do
      begin
        CallBack(mItem,mCancel);
        if mCancel then
        break;
      end;
    finally
      FLock.Leave;
    end;
  end;
end;

function TPTLockedObjectList.Contains(Instance: TObject): Boolean;
begin
  result := false;
  if assigned(Instance) then
  begin
    FLock.Enter;
    try
      result := FObjects.Contains(Instance);
    finally
      FLock.Leave;
    end;
  end;
end;

function TPTLockedObjectList.GetCount: Integer;
begin
  FLock.Enter;
  try
    result :=FObjects.Count;
  finally
    FLock.Leave;
  end;
end;

function TPTLockedObjectList.GetEmpty: Boolean;
begin
  FLock.Enter;
  try
    result := FObjects.Count<1;
  finally
    FLock.Leave;
  end;
end;

function TPTLockedObjectList.Lock: TObjectList<TObject>;
begin
  FLock.Enter;
  result:=FObjects;
end;

procedure TPTLockedObjectList.UnLock;
begin
  FLock.Leave;
end;


//############################################################################
//  TPTLockedObject
//############################################################################

constructor TPTLockedObject<T>.Create(const AOptions: TPTLockedDataAccessRights = [lvRead, lvWrite]);
begin
  inherited Create;
  FLock:=TCriticalSection.Create;
  FOptions:=AOptions;
  FData := T.create;
end;

destructor TPTLockedObject<T>.Destroy;
begin
  FData.free;
  FLock.Free;
  inherited;
end;

function TPTLockedObject<T>.GetAccessRights: TPTLockedDataAccessRights;
begin
  FLock.Enter;
  try
    result := FOptions;
  finally
    FLock.Leave;
  end;
end;

procedure TPTLockedObject<T>.SetAccessRights(Rights: TPTLockedDataAccessRights);
begin
  FLock.Enter;
  try
    FOptions := Rights;
  finally
    FLock.Leave;
  end;
end;

function TPTLockedObject<T>.Lock: T;
begin
  FLock.Enter;
  result := FData;
end;

procedure TPTLockedObject<T>.Unlock;
begin
  FLock.Leave;
end;

procedure TPTLockedObject<T>.Synchronize(const Entry: TPTLockedObjectEntry);
begin
  if assigned(Entry) then
  begin
    FLock.Enter;
    try
      Entry(FData);
    finally
      FLock.Leave;
    end;
  end;
end;

function TPTLockedObject<T>.GetValue: T;
begin
  FLock.Enter;
  try
    if (lvRead in FOptions) then
    Result := FData else
    Raise EPTLockedObject.CreateFmt('%s:Read not allowed error',[classname]);
  finally
    FLock.Leave;
  end;
end;

procedure TPTLockedObject<T>.SetValue(Value: T);
begin
  FLock.Enter;
  try
    if (lvWrite in FOptions) then
    begin

      if (TObject(FData) is TPersistent)
      or (TObject(FData).InheritsFrom(TPersistent)) then
      TPersistent(FData).Assign(TPersistent(Value)) else
      Raise EPTLockedObject.CreateFmt('Locked object assign failed, %s does not inherit from %s',
        [TObject(FData).ClassName,'TPersistent']);

    end else
    Raise EPTLockedObject.CreateFmt('%s:Write not allowed error',[classname]);
  finally
    FLock.Leave;
  end;
end;


//############################################################################
//  TPTLockedValue
//############################################################################

Constructor TPTLockedValue<T>.Create(const AOptions: TPTLockedDataAccessRights = [lvRead,lvWrite]);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FOptions:=AOptions;
end;

Destructor TPTLockedValue<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TPTLockedValue<T>.GetAccessRights: TPTLockedDataAccessRights;
begin
  FLock.Enter;
  try
    result := FOptions;
  finally
    FLock.Leave;
  end;
end;

procedure TPTLockedValue<T>.SetAccessRights(Rights: TPTLockedDataAccessRights);
begin
  FLock.Enter;
  try
    FOptions := Rights;
  finally
    FLock.Leave;
  end;
end;

procedure TPTLockedValue<T>.Lock;
begin
  FLock.Enter;
end;

procedure TPTLockedValue<T>.Unlock;
begin
  FLock.Leave;
end;

procedure TPTLockedValue<T>.Synchronize(const Entry: TPTLockedValueEntry);
begin
  if assigned(Entry) then
  Begin
    FLock.Enter;
    try
      Entry(FData);
    finally
      FLock.Leave;
    end;
  end;
end;

function TPTLockedValue<T>.GetValue: T;
begin
  FLock.Enter;
  try
    if (lvRead in FOptions) then
    result:=FData else
    Raise EPTLockedValue.CreateFmt('%s:Read not allowed error',[classname]);
  finally
    FLock.Leave;
  end;
end;

procedure TPTLockedValue<T>.SetValue(Value: T);
begin
  FLock.Enter;
  try
    if (lvWrite in FOptions) then
    FData:=Value else
    Raise EPTLockedValue.CreateFmt('%s:Write not allowed error',[classname]);
  finally
    FLock.Leave;
  end;
end;

end.
