{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.FIFOQueue.MessageList                       }
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

unit PTLib.Common.FIFOQueue.MessageList;

interface

uses
  System.SysUtils, System.SyncObjs, System.Classes, System.Generics.Collections,
  System.NetEncoding,

  PTLib.Common.Log,
  PTLib.Common.Files,
  PTLib.Common.LockedTypes,
  PTLib.Common.FIFOQueue;

type
  TMessageStorageAddedEvent = procedure (sender: TObject; Id: String) of object;
  TMessageDeletedEvent = procedure (sender: TObject; Id: String) of object;

  (*
  [26.10.2015 12:21:07] Paul Spencer Thornton: Async - Never retry, always retrun an error
[26.10.2015 12:21:39] Paul Spencer Thornton: High Priority - Always retry and persist after restart
[26.10.2015 12:22:06 | Redigert 12:22:24] Paul Spencer Thornton: Medium Priority - Retry, but don't persist after restart
[26.10.2015 12:22:48] Paul Spencer Thornton: Low Priority - Never retry, don't persist after restart
[26.10.2015 12:23:08] Paul Spencer Thornton: The priorities are decided on in the service class
  *)

  TStationMessagePriority = (
    spLowPriority,
    spMediumPriority,
    spHighPriority,
    spASyncPriority);

  TStationMessage = Class(TObject)
  strict private
    FId:        TPTLockedValue<String>;
    FCommand:   TPTLockedValue<String>;
    FData:      TPTLockedValue<String>;
    FCallBack:  TPTLockedValue<Boolean>;
    FPriority:  TPTLockedValue<TStationMessagePriority>;
  protected
    function    GetInfo:String;
    procedure   SetInfo(Value:String);
  public
    constructor Create; overload; virtual;
    constructor Create(aId: String; aCommand: String; aParams: String); overload; virtual;
    constructor Create(aId: String; aCommand: String; aParams: String; aInfo: String); overload; virtual;
    destructor  Destroy; override;

    function    ToString:String;override;

    procedure   Assign(Source:TStationMessage);

    (* NOTES:   The "info" field is basically a calculated field.
                It allows us to pack all extra properties, such as priority
                and callback requirements into a single string. *)
    property Info:String read GetInfo write SetInfo;
    Property Priority:TPTLockedValue<TStationMessagePriority> read FPriority;
    Property RequiresCallback:TPTLockedValue<Boolean> read FCallBack;
    Property Id:TPTLockedValue<String> read Fid;
    property Command:TPTLockedValue<String> read FCommand;
    property ParamData:TPTLockedValue<String> read FData;
  end;

  TMessageStorage = Class(TObject)
  private
    FOnAdded:       TMessageStorageAddedEvent;
    FOnDeleted:     TMessageDeletedEvent;
    FActive:        Boolean;
    FBaseFileName:  String;
    FBasePath:      String;
    FIdFileName:    String;
    FTypeFileName:  String;
    FMsgFileName:   String;
    FInfoFilename:  String;
    FIdFile:        TPTDiskFIFOQueue;
    FTypeFile:      TPTDiskFIFOQueue;
    FMsgFile:       TPTDiskFIFOQueue;
    FInfoFile:      TPTDiskFIFOQueue;
    FLock:          TCriticalSection;
  private
    procedure   SetBasePath(Value: String);
    function    GetbasePath:String;

    procedure   SetBaseFileName(Value: String);
    function    GetBaseFileName:String;
  protected
    procedure DoOnAdded(Id: String); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure   Reset;

    function    GetActive:Boolean;
    function    GetCount:Integer;
    procedure   AddMessage(Message: TStationMessage);overload;
    procedure   AddMessage(Id: String; aCommand: String; aParamData: String; Info: String);overload;
    Procedure   DeleteMessage(Id: String);
    function    PopMessage(out Id:String;out Msgtype,MsgData,Info:String):Boolean;overload;
    function    PopMessage(out Message: TStationMessage):Boolean;overload;
    Procedure   Open(CanCreate:Boolean=True);
    Procedure   Close;
  public
    Property    OnMessageAdded: TMessageStorageAddedEvent read FOnAdded write FOnAdded;
    property    OnMessageDeleted: TMessageDeletedEvent read FOnDeleted write FOnDeleted;

    Property    RootPath:String read GetBasePath write SetBasePath;
    Property    RootName:String read GetBaseFileName write SetBaseFileName;
  end;


implementation

resourcestring
  StrPriority = 'Priority: ';

//#############################################################################
// TMessageStorage
//#############################################################################

Constructor TMessageStorage.Create;
Begin
  inherited;
  FLock:=TCriticalSection.Create;
  FIdFile:=TPTDiskFIFOQueue.Create;
  FTypeFile:=TPTDiskFIFOQueue.Create;
  FMsgFile:=TPTDiskFIFOQueue.Create;
  FInfoFile:=TPTDiskFIFOQueue.Create;
end;

Destructor TMessageStorage.Destroy;
begin
  FIdFile.Free;
  FTypeFile.Free;
  FMsgFile.Free;
  FInfoFile.Free;
  FLock.Free;
  inherited;
end;

procedure TMessageStorage.Open(CanCreate: Boolean);
begin
  if GetActive then
  Begin
    Close;
  end;

  // Make sure path and base-filename exists
  if length(FBasePath)>0 then
  Begin
    if CanCreate then
    begin
      if not DirectoryExists(FBasePath) then
      begin
        if not forcedirectories(FBasePath) then
          Raise Exception.CreateFmt('Failed to open message-storage, invalid root path [%s] error',[FBasePath]);
      end;
    end else
    begin
      if not DirectoryExists(FBasePath) then
        Raise Exception.CreateFmt('Failed to open message-storage, invalid root path [%s] error',[FBasePath]);
    end;
  end else
    Raise Exception.Create('Failed open message-storage, invalid root path error');

  // Now check that we have write access
  //if not TFileUtils.QueryWriteAccessToFolder(FBasePath) then
  //  Raise Exception.CreateFmt('Failed to open message-storage, user does not have write access to path [%s]',[FBasePath]);

  FLock.Enter;
  try
    // calculate filenames based on our simple name-scheme
    FIdFileName   := FBasePath + FBaseFileName + '.list.txt';
    FTypeFileName := FBasePath + FBaseFileName + '.type.txt';
    FMsgFileName  := FBasePath + FBaseFilename + '.messages.txt';
    FInfoFilename := FBasePath + FbaseFilename + '.info.txt';

    // Validate base-filename just in case
    if length(FBaseFileName)>0 then
    begin
      if not FileExists(FIdFileName) then
      begin
        if not CanCreate then
          Raise Exception.CreateFmt('Failed open message-storage, file not found error [%s]',[FBasePath + FBaseFileName]);
      end;
    end else
      Raise Exception.CreateFmt('Failed open message-storage, invalid filename [%s]',[FBasePath + FBaseFileName]);

    try
      FIdFile.Open(FIdFileName,true);
    except
      on exception do
      raise;
    end;

    try
      FTypeFile.Open(FTypeFileName,true);
    except
      on exception do
      begin
        FidFile.Close;
        raise;
      end;
    end;

    try
      FMsgFile.Open(FMsgFileName,true);
    except
      on exception do
      begin
        FIdFile.Close;
        FTypeFile.Close;
        raise;
      end;
    end;

    try
      FInfoFile.Open(FInfoFilename,true);
    except
      on exception do
      begin
        FMsgFile.Close;
        FIdFile.Close;
        FTypeFile.Close;
        raise;
      end;
    end;

    // Mark object as active
    FActive := True;

  finally
    FLock.Leave;
  end;
end;

procedure TMessageStorage.Close;
begin
  if GetActive then
  Begin
    FLock.Enter;
    try
      FIdFile.Close;
      FTypeFile.Close;
      FMsgFile.Close;
      FInfoFile.Close;

      FActive := False;

      FIdFileName := '';
      FTypeFileName := '';
      FMsgFileName := '';
      FInfoFileName := '';

    finally
      FLock.Leave;
    end;
  end;
end;

//Note: We wrap this in a function since we may want to introduce a TPTLockedValue later
function TMessageStorage.GetActive:Boolean;
begin
  FLock.Enter;
  try
    result := FActive;
  finally
    FLock.Leave;
  end;
end;

function TMessageStorage.PopMessage(out Message: TStationMessage):Boolean;
var
  mId:    String;
  mType:  String;
  mData:  String;
  mInfo:  String;
begin
  result:=PopMessage(mId, mType, mData, mInfo);
  if result then
    Message := TStationMessage.Create(mId,mType,mData,mInfo);
end;

function TMessageStorage.PopMessage(out Id, Msgtype, MsgData, Info:String): Boolean;
begin
  result :=False;

  if getActive then
  begin

    FLock.Enter;
    try
      if FidFile.Count>0 then
      begin
        Id := FIdFile.Pop;
        MsgType := FTypeFile.Pop;
        MsgData := FMsgFile.Pop;
        Info    := FInfoFile.Pop;

        result := True;
      end;
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TMessageStorage.Reset;
begin
  if GetActive then
  begin
    FLock.Enter;
    try
      FidFile.Reset;
      FtypeFile.Reset;
      FMsgFile.Reset;
      FInfoFile.Reset;
    finally
      FLock.Leave;
    end;
  end;
end;

Procedure TMessageStorage.DeleteMessage(Id: String);
var
  mIndex: integer;
begin
  if GetActive then
  begin

    FLock.Enter;
    try

      mIndex := FidFile.GetIndexByValue(Id);
      if (mIndex >= 0) then
      begin
        FIdFile.Delete(mIndex);
        FTypeFile.Delete(mIndex);
        FMsgFile.Delete(mIndex);
        FInfoFile.Delete(mIndex);

        // Signal event on exit
        TThread.Queue(NIL,
          procedure
          begin
            if assigned(FOnDeleted) then
              FOnDeleted(self,Id);
          end);
      end;

    finally
      FLock.Leave;
    end;
  end else
  begin
    raise Exception.Create('Failed to delete message, storage not active error');
  end;
end;

procedure TMessageStorage.AddMessage(Message: TStationMessage);
begin
  AddMessage(Message.Id.Value, Message.Command.Value, Message.ParamData.Value, message.Info);
end;

procedure TMessageStorage.DoOnAdded(Id: String);
begin
  if Assigned(FOnAdded) then
  begin
    FOnAdded(Self,Id);
  end;
end;

procedure TMessageStorage.AddMessage(Id: String; aCommand: String;
          aParamData: String; Info: String);
begin
  if GetActive then
  begin
    FLock.Enter;
    try
      FIdFile.Push(id);
      FTypeFile.Push(aCommand);
      FMsgFile.Push(aParamData);
      FInfoFile.Push(Info);

      // Issue callback
      DoOnAdded(Id);

      // Queue event dispatch
      {TThread.Queue(nil,
        procedure
        begin
          if assigned(FOnAdded) then
          begin
            DoOnAdded(Id);
          end;
        end); }
    finally
      FLock.Leave;
    end;
  end else
  begin
    Raise Exception.Create('Failed to add message, message-storage not active error');
  end;
end;

function TMessageStorage.GetCount: Integer;
begin
  result := -1;

  if GetActive then
  begin
    FLock.Enter;
    try
      result := FidFile.Count
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TMessageStorage.SetBaseFileName(Value: String);
begin
  if not getActive then
  begin
    FLock.Enter;
    try
      FBaseFileName := trim(Value);
    finally
      FLock.Leave;
    end;
  end else
    raise Exception.Create('Failed to alter root-name, storage is active error');
end;

procedure TMessageStorage.SetBasePath(Value: String);
begin
  if not GetActive then
  Begin
    FLock.Enter;
    try
      FBasePath := IncludeTrailingPathDelimiter(trim(Value));
    finally
      FLock.Leave;
    end;
  end else
    raise Exception.Create('Failed to alter root-path, storage is active error');
end;

function TMessageStorage.GetBaseFileName:String;
begin
  FLock.Enter;
  try
    result := FBaseFileName;
  finally
    FLock.Leave;
  end;
end;

function TMessageStorage.GetbasePath:String;
begin
  FLock.Enter;
  try
    result := FBasePath;
  finally
    FLock.Leave;
  end;
end;

//#############################################################################
// TStationMessage
//#############################################################################

Constructor TStationMessage.Create;
var
  GUID: TGUID;
begin
  inherited;
  FId := TPTLockedValue<String>.Create;

  CreateGUID(GUID);
  FId.Value := GUIDToString(GUID);

  FCommand := TPTLockedValue<String>.Create;
  FData:=TPTLockedValue<String>.Create;
  FPriority:=TPTLockedValue<TStationMessagePriority>.Create;

  FCallBack:=TPTLockedValue<Boolean>.Create;
  FCallback.Value := false;
end;

Constructor TStationMessage.Create(aId: String; aCommand: String; aParams: String);
begin
  Create;
  FId.Value := aId;
  FCommand.Value := aCommand;
  FData.Value := aParams;
  FPriority.Value := TStationMessagePriority.spLowPriority;
  FCallback.Value := false;
end;

Constructor TStationMessage.Create(aId: String; aCommand: String; aParams: String; aInfo: String);
begin
  Create(aId,aCommand,aParams);
  SetInfo(aInfo);
end;

Destructor TStationMessage.Destroy;
begin
  FCallback.Free;
  FPriority.Free;
  FData.Free;
  FCommand.Free;
  FId.Free;
  inherited;
end;

procedure TStationMessage.Assign(Source:TStationMessage);
begin
  if Source <> nil then
  begin
    FId.Value := Source.Id.Value;
    FCommand.Value := Source.Command.Value;
    FData.Value := Source.ParamData.Value;
    FCallBack.Value := source.RequiresCallback.Value;
    SetInfo(Source.Info);
  end;
end;

function TStationMessage.GetInfo: String;
var
  mData:  TStringList;
begin
  mData := TStringList.Create;
  try
    mData.CaseSensitive := false;
    mData.Values['priority'] := Ord(FPriority.Value).ToString;
    mData.Values['callback'] := BoolToStr(FCallback.Value, True);
    Result := TURLEncoding.URL.Encode(mData.Text);
  finally
    mData.Free;
  end;
end;

procedure TStationMessage.SetInfo(Value:String);
var
  mData:  TStringList;
  mText:  String;
  mTemp:  Boolean;
begin
  Value := trim(value);
  if Value.Length > 0 then
  Begin
    mData := TStringList.Create;
    try
      mData.CaseSensitive := false;
      mData.Text := TURLEncoding.URL.Decode(Value);

      if mData.IndexOfName('priority')>=0 then
      begin
        mText := mData.Values['priority'];
        FPriority.Value :=  TStationMessagePriority( StrToInt(mText) );
      end;

      if mData.IndexOfName('callback')>=0 then
      begin
        mText := mData.Values['callback'];
        if TryStrToBool(mText,mTemp) then
        FCallback.Value := mTemp;
      end;

    finally
      mData.Free;
    end;
  end;
end;

function TStationMessage.ToString:String;
begin
  result :=
    StrPriority + Ord(FPriority.Value).ToString + #13 +
    'Id: ' + FId.Value + #13 +
    'Type: ' + FCommand.Value + #13 +
    'Data: ' + FData.Value + #13 +
    'Info: ' + GetInfo;
end;


end.
