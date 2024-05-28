{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.FileEdit                                       }
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

unit PTLib.VCL.FileEdit;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  WinAPI.Windows, WinAPI.Messages,

  System.SysUtils, System.Classes, System.UITypes,

  Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.FileCtrl;

type
  TDirectoryDialogOptions = class(TPersistent)
  strict private
    FTitle: String;
    FOptions: TSelectDirExtOpts;
  published
    constructor Create;

    property Title: String read FTitle write FTitle;
    property Options: TSelectDirExtOpts read FOptions write FOptions;
  end;

  TCustomPTLibFileEdit = class(TCustomEdit)
  private
    FButton: TButton;

    procedure OnRightButtonClick(Sender: TObject);
    procedure SetEditRect;
    function GetMinHeight: Integer;
  protected
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoOnButtonClick; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  end;

  TPTLibFileEdit = class(TCustomPTLibFileEdit)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Touch;
    property Visible;
    property StyleElements;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TCustomPTLibFileDialogEdit = class(TPTLibFileEdit)
  private
    function GetFilename: String;
    procedure SetFilename(const Value: String);
  published
    property Filename: String read GetFilename write SetFilename;
  end;

  TPTLibOpenFileEdit = class(TCustomPTLibFileDialogEdit)
  private
    FOpenDialog: TOpenDialog;
    procedure SetOpenDialog(const Value: TOpenDialog);
  protected
    procedure DoOnButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OpenDialog: TOpenDialog read FOpenDialog write SetOpenDialog;
  end;

  TPTLibSaveFileEdit = class(TCustomPTLibFileDialogEdit)
  private
    FSaveDialog: TSaveDialog;
    procedure SetSaveDialog(const Value: TSaveDialog);
  protected
    procedure DoOnButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SaveDialog: TSaveDialog read FSaveDialog write SetSaveDialog;
  end;

  TPTLibDirectoryEdit = class(TPTLibFileEdit)
  strict private
    FDirectoryDialogOptions: TDirectoryDialogOptions;
  private
    procedure SetDirectoryDialogOptions(const Value: TDirectoryDialogOptions);
  protected
    procedure DoOnButtonClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DirectoryDialogOptions: TDirectoryDialogOptions read FDirectoryDialogOptions write SetDirectoryDialogOptions;
  end;

implementation

{ TBaseFileEdit }

constructor TCustomPTLibFileEdit.Create(AOwner: TComponent);
begin
  inherited;

  FButton := TButton.Create(Self);
  FButton.Parent := Self;
  FButton.OnClick := OnRightButtonClick;
  FButton.Caption := '...';
end;

procedure TCustomPTLibFileEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TCustomPTLibFileEdit.CreateWnd;
begin
  inherited;

  SetEditRect;
end;

procedure TCustomPTLibFileEdit.DoOnButtonClick;
begin
  // Override;
end;

procedure TCustomPTLibFileEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // No children!
end;

procedure TCustomPTLibFileEdit.OnRightButtonClick(Sender: TObject);
begin
  DoOnButtonClick;
end;

procedure TCustomPTLibFileEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;

  MinHeight := GetMinHeight;

  if Height < MinHeight then
  begin
    Height := MinHeight
  end
  else
  if FButton <> nil then
  begin
    if (NewStyleControls) and (Ctl3D) then
    begin
      FButton.Height := Height - 5;
      FButton.Width := Height - 5;
      FButton.Left := Width - FButton.Width - 5;
      FButton.Top := 0;
    end
    else
    begin
      FButton.Height := Height - 3;
      FButton.Width := Height - 3;
      FButton.Left := Width - FButton.Width;
      FButton.Top := 1;
    end;

    SetEditRect;
  end;
end;

function TCustomPTLibFileEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);

  I := SysMetrics.tmHeight;

  if I > Metrics.tmHeight then
  begin
    I := Metrics.tmHeight;
  end;

  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TCustomPTLibFileEdit.SetEditRect;
var
  Loc: TRect;
begin
  if FButton <> nil then
  begin
    SendMessage(Handle, EM_GETRECT, 0, IntPtr(@Loc));

    Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
    Loc.Right := ClientWidth - FButton.Width - 2;
    Loc.Top := 0;
    Loc.Left := 0;

    SendMessage(Handle, EM_SETRECTNP, 0, IntPtr(@Loc));
    SendMessage(Handle, EM_GETRECT, 0, IntPtr(@Loc));  {debug}
  end;
end;

{ TPTLibOpenFileEdit }

constructor TPTLibOpenFileEdit.Create(AOwner: TComponent);
begin
  inherited;

  FOpenDialog := TOpenDialog.Create(Self);
  FOpenDialog.SetSubComponent(True);
  FOpenDialog.Title := 'Open File';
end;

procedure TPTLibOpenFileEdit.DoOnButtonClick;
begin
  inherited;

  if FOpenDialog.Execute then
  begin
    Caption := FOpenDialog.FileName;
  end;
end;

procedure TPTLibOpenFileEdit.SetOpenDialog(const Value: TOpenDialog);
begin
  FOpenDialog.Assign(Value);

  Caption := FOpenDialog.FileName;
end;

{ TPTLibFileEdit }

constructor TPTLibFileEdit.Create(AOwner: TComponent);
begin
  inherited;

  Caption := '';
end;

{ TPTLibSaveFileEdit }

constructor TPTLibSaveFileEdit.Create(AOwner: TComponent);
begin
  inherited;

  FSaveDialog := TSaveDialog.Create(Self);
  FSaveDialog.SetSubComponent(True);
  FSaveDialog.Title := 'Save File';
  FSaveDialog.Options := FSaveDialog.Options + [ofOverwritePrompt];
end;

procedure TPTLibSaveFileEdit.DoOnButtonClick;
begin
  inherited;

  if FSaveDialog.Execute then
  begin
    Caption := FSaveDialog.FileName;
  end;
end;

procedure TPTLibSaveFileEdit.SetSaveDialog(const Value: TSaveDialog);
begin
  FSaveDialog.Assign(Value);

  Caption := FSaveDialog.FileName;
end;

{ TPTLibDirectoryEdit }

constructor TPTLibDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited;

  FDirectoryDialogOptions := TDirectoryDialogOptions.Create;
end;

destructor TPTLibDirectoryEdit.Destroy;
begin
  FreeAndNil(FDirectoryDialogOptions);

  inherited;
end;

procedure TPTLibDirectoryEdit.DoOnButtonClick;
var
  RootDirectory: WideString;
  Directory: String;
begin
  inherited;

  if System.SysUtils.DirectoryExists(Caption) then
  begin
    RootDirectory := Caption;
  end
  else
  begin
    RootDirectory := '';
  end;

  if Vcl.FileCtrl.SelectDirectory(
    FDirectoryDialogOptions.Title,
    RootDirectory,
    Directory,
    FDirectoryDialogOptions.Options,
    Self) then
  begin
    Caption := Directory;
  end;
end;

procedure TPTLibDirectoryEdit.SetDirectoryDialogOptions(
  const Value: TDirectoryDialogOptions);
begin
  FDirectoryDialogOptions.Assign(Value);
end;

{ TDirectoryDialogOptions }

constructor TDirectoryDialogOptions.Create;
begin
  FTitle := 'Select Directory';
  FOptions := [
    sdNewFolder,
    sdShowEdit,
    sdShowShares,
    sdNewUI];
end;

{ TCustomPTLibFileDialogEdit }

function TCustomPTLibFileDialogEdit.GetFilename: String;
begin
  Result := Text;
end;

procedure TCustomPTLibFileDialogEdit.SetFilename(const Value: String);
begin
  Text := Value;
end;

{$WARN SYMBOL_PLATFORM ON}

end.
