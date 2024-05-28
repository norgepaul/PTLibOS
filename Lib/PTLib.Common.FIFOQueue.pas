{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.FIFOQueue                                   }
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

unit PTLib.Common.FIFOQueue;

interface

  (* Persistent FIFO Queue.
     This unit contains our class for persistent "on disk" lists.
     It is designed to be thread safe an
     d changes are mirrored to disk
     immediately.

     Note: I would normally used a memory-mapped file for this, but since
     we want our code to be platform independent I have used a very
     "brute-force" way of mirroring the data.
     This can be optimized at a later date. *)

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  System.Generics.Collections,

  PTLib.Common.Classes,
  PTLib.Common.Interfaces,
  PTLib.Common.LockedTypes;

type


  EPTFIFOQueueException = class(Exception);

  // Persistent, thread safe, queue list baseclass
  TPTCustomFIFOQueue = Class(TObject)
  private
    FLock:      TCriticalSection;
    FActive:    TPTLockedValue<Boolean>;
  protected
    Procedure   Lock;virtual;
    procedure   Unlock;virtual;
    function    GetActive:Boolean;
  protected
    function    GetCount:Integer;virtual;abstract;
    procedure   DoOpen;virtual;abstract;
    procedure   DoClose;virtual;abstract;
    procedure   DoPopElement(out Data: String);virtual;abstract;
    procedure   DoPushElement(Data: String);virtual;abstract;
    procedure   DoGetItemByIndex(Index: Integer; out Value: String);virtual;abstract;
    Function    DoGetIndexByValue(Value: String):Integer;virtual;abstract;
    procedure   DoDelete(Index: Integer);virtual;abstract;
  public
    Property    Active:Boolean read GetActive;
    Property    Count:Integer read GetCount;
    Function    GetItemByIndex(Index: Integer; out Value: String):Boolean;
    function    GetIndexByValue(Value: String):Integer;
    Procedure   Delete(Index: Integer);
    Procedure   Push(Data: String);
    Function    Pop:String;
    Procedure   Open;
    Procedure   Close;
    procedure   Reset;virtual;abstract;
    Procedure   BeforeDestruction;Override;
    Constructor Create;virtual;
    Destructor  Destroy;Override;
  end;

  // Stream based queue
  TPTStreamFIFOQueue = Class(TPTCustomFIFOQueue)
  strict private
    FFile:      TStream;
    FCache:     TStringList;
  strict protected
    procedure   ReadCache;
    procedure   WriteCache;
    function    GetCount:Integer;override;
    procedure   DoGetItemByIndex(Index: Integer; out Value: String);override;
    Function    DoGetIndexByValue(Value: String):Integer;override;
    procedure   DoDelete(Index: Integer);override;
  strict protected
    procedure   DoGetStream(out Stream:TStream);virtual;abstract;
    procedure   DoOpen;override;
    procedure   DoClose;override;
    procedure   DoPopElement(out Data: String);override;
    procedure   DoPushElement(Data: String);override;
  public
    procedure   Reset;override;
    Constructor Create;override;
    Destructor  Destroy;Override;
  end;

  //Memory based stream queue
  TPTMemoryFIFOQueue = Class(TPTStreamFIFOQueue)
  strict protected
    procedure   DoGetStream(out Stream:TStream);override;
  end;

  //Disk based stream queue
  TPTDiskFIFOQueue = Class(TPTStreamFIFOQueue)
  strict private
    FFilename:  TPTLockedValue<String>;
  strict protected
    procedure   DoGetStream(out Stream:TStream);override;
  public
    Property    FileName:TPTLockedValue<String> read FFilename;
    Procedure   Open(AFileName: String; CanCreate: Boolean = True);overload;
    Constructor Create;override;
    Destructor  Destroy;Override;
  end;

implementation

//#############################################################################
// TPTMemoryFIFOQueue
//#############################################################################

procedure TPTMemoryFIFOQueue.DoGetStream(out Stream: TStream);
begin
  Stream := TMemoryStream.Create;
end;

//#############################################################################
// TPTDiskFIFOQueue
//#############################################################################

Constructor TPTDiskFIFOQueue.Create;
begin
  inherited;
  FFilename := TPTLockedValue<String>.create;
end;

Destructor TPTDiskFIFOQueue.Destroy;
begin
  FreeAndNil(FFilename);
  inherited;
end;

procedure TPTDiskFIFOQueue.DoGetStream(out Stream:TStream);
begin
  // Open the file in exclusive mode, keep the stream open during operation
  try
    Stream:=TFileStream.Create(FFilename.Value,fmOpenReadWrite or fmShareExclusive);
  except
    on e: exception do
      Raise EPTFIFOQueueException.CreateFmt
      ('FIFO Disk-Queue failed, unable to access file [%s]',[FFilename.Value]);
  end;
end;

Procedure TPTDiskFIFOQueue.Open(AFileName: String; CanCreate: Boolean=True);
var
  Path:   String;
  Empty:  TStringList;
begin
  if not GetActive then
  begin

    // Validate filename
    AFilename := trim(AFilename);
    if length(AFilename)<1 then
      Raise EPTFIFOQueueException.Create('FIFO Disk-Queue failed, invalid or empty filename error');

    if CanCreate then
    begin
      if not FileExists(AFilename) then
      Begin

        // Ensure that path exists, attempt to create if not
        Path := ExtractFilePath(AFilename);
        if not DirectoryExists(Path) then
        begin
          if not ForceDirectories(Path) then
            Raise EPTFIFOQueueException.CreateFmt('FIFO Disk-Queue failed, could not enter path (%s)',[Path]);
        end;

        // Attempt to create empty text file
        Empty:=TStringlist.Create;
        try
          try
            Empty.SaveToFile(AFilename);
          except
            on e: exception do
              Raise EPTFIFOQueueException.CreateFmt('FIFO Disk-Queue failed, could not create file [%s]',[AFilename]);
          end;
        finally
          Empty.Free;
        end;

      end;
    end;

    FFilename.Value := AFilename;

    inherited Open;
  end else
    Raise EPTFIFOQueueException.Create('FIFO Disk-Queue already active error');
end;

//#############################################################################
// TPTCustomFIFOQueue
//#############################################################################

Constructor TPTCustomFIFOQueue.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FActive:=TPTLockedValue<Boolean>.Create;
end;

Destructor TPTCustomFIFOQueue.Destroy;
begin
  FLock.Free;
  FActive.Free;
  inherited;
end;

Procedure TPTCustomFIFOQueue.BeforeDestruction;
begin
  if GetActive then
  Close;
  inherited;
end;

function TPTCustomFIFOQueue.GetActive:Boolean;
begin
  result:=FActive.Value;
end;

Procedure TPTCustomFIFOQueue.Open;
begin
  if not GetActive then
  begin
    Lock;
    try
      DoOpen;
      FActive.Value := true;
    finally
      UnLock;
    end;
  end else
    Raise EPTFIFOQueueException.Create('FIFO Queue already active error');
end;

Procedure TPTCustomFIFOQueue.Close;
begin
  if GetActive then
  begin
    Lock;
    try
      DoClose;
    finally
      FActive.Value := false;
      UnLock;
    end;
  end;
end;

Procedure TPTCustomFIFOQueue.Push(Data: String);
begin
  if getActive then
  begin
    Lock;
    try
      DoPushElement(Data);
    finally
      Unlock;
    end;
  end else
    Raise EPTFIFOQueueException.Create('FIFO Queue write failed, not active error');
end;

Function TPTCustomFIFOQueue.Pop: String;
begin
  if GetActive then
  begin
    Lock;
    try
      DoPopElement(Result);
    finally
      Unlock;
    end;
  end else
    Raise EPTFIFOQueueException.Create('FIFO Queue read failed, not active error');
end;

Procedure TPTCustomFIFOQueue.Delete(Index: Integer);
begin
  if GetActive then
  Begin
    Lock;
    try
      DoDelete(index);
    finally
      UnLock;
    end;
  end;
end;

function TPTCustomFIFOQueue.GetIndexByValue(Value: String): Integer;
begin
  result := -1;
  if GetActive then
  begin
    Lock;
    try
      Result := DoGetIndexByValue(Value);
    finally
      UnLock;
    end;
  end;
end;

Function TPTCustomFIFOQueue.GetItemByIndex(Index: Integer;out Value: String): Boolean;
begin
  result := false;
  if getActive then
  begin
    Lock;
    try
      DoGetItemByIndex(index,value);
      result := true;
    finally
      unlock;
    end;
  end;
end;

Procedure TPTCustomFIFOQueue.Lock;
begin
  FLock.Enter;
end;

procedure TPTCustomFIFOQueue.Unlock;
begin
  FLock.Leave;
end;

//#############################################################################
// TPTStreamFIFOQueue
//#############################################################################

constructor TPTStreamFIFOQueue.Create;
begin
  inherited;
  FCache:=TStringList.Create;
end;

destructor TPTStreamFIFOQueue.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TPTStreamFIFOQueue.DoDelete(Index: Integer);
begin
  ReadCache;
  try
    FCache.Delete(index);
  finally
    WriteCache;
  end;
end;

// LOCK REQUIRED
function TPTStreamFIFOQueue.DoGetIndexByValue(Value: String): Integer;
begin
  result := FCache.IndexOf(Value);
end;

// LOCK REQUIRED
procedure TPTStreamFIFOQueue.DoGetItemByIndex(Index: Integer; out Value: String);
begin
  Value := FCache[index];
end;

procedure TPTStreamFIFOQueue.Reset;
begin
  if self.GetActive then
  begin
    Lock;
    try
      FCache.Clear;
      FFile.Size:=0;
    finally
      UnLock;
    end;
  end;
end;

procedure TPTStreamFIFOQueue.DoOpen;
begin
  DoGetStream(FFile);
  if not assigned(FFile) then
    Raise EPTFIFOQueueException.Create('Failed to open queue, stream created was NIL error');

  ReadCache;
end;

procedure TPTStreamFIFOQueue.DoClose;
begin
  FreeAndNIL(FFile);
  FCache.Clear;
end;

// LOCK REQUIRED
procedure TPTStreamFIFOQueue.DoPopElement(out Data: String);
begin
  if FCache.Count>0 then
  Begin
    try
      Data := FCache[0];
      FCache.Delete(0);
    finally
      WriteCache;
    end;
  end else
    Data := '';
end;

// LOCK REQUIRED
procedure TPTStreamFIFOQueue.DoPushElement(Data: String);
begin
  try
    FCache.Add(Data);
  finally
    WriteCache;
  end;
end;

function TPTStreamFIFOQueue.GetCount: Integer;
begin
  Lock;
  try
    result :=FCache.Count;
  finally
    UnLock;
  end;
end;

// LOCK REQUIRED
procedure TPTStreamFIFOQueue.ReadCache;
begin
  FFile.Position:=0;
  FCache.LoadFromStream(FFile);
end;

// LOCK REQUIRED
procedure TPTStreamFIFOQueue.WriteCache;
begin
  FFile.Size:=0;
  FCache.SaveToStream(FFile);
end;

end.
