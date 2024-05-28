{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.SearchEdit                                     }
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

unit PTLib.VCL.SearchEdit;

interface

uses
  WinAPI.Windows, WinAPI.Messages,

  System.Classes, System.SysUtils, System.Types,

  Vcl.Controls, Vcl.StdCtrls, Vcl.Forms, Vcl.Graphics, Vcl.Imaging.PngImage,
  Vcl.Menus,

  GDIPObj, GDIPAPI,

  PTLib.VCL.GDIPlus,
  PTLib.VCL.AwesomeEdit,
  PTLib.VCL.AwesomeEdit.Types;

type
  TSearchControl = (
    scSearch,
    scSearchDropdown,
    scClear
  );
  TSearchControls = set of TSearchControl;

  TPTLibCustomSearchEdit = class(TAwesomeEdit)
  private
    FOnSearch: TNotifyEvent;
    FOnClear: TNotifyEvent;
    FOnClearInformation: TNotifyEvent;

    FSearchPopupMenu: TPopupMenu;
    FSearchDescription: String;
    FTextButton: TAwesomeButton;
    FInformationButton: TAwesomeButton;
    FSearchButton: TAwesomeButton;
    FDropDownButton: TAwesomeButton;
    FClearButton: TAwesomeButton;
    FSearchControls: TSearchControls;
    FInformation: String;

    procedure UpdateControls;
    procedure SetSearchPopupMenu(const Value: TPopupMenu);
    procedure SetSearchDescription(const Value: String);
    procedure OnButtonVisibilityChanged(Sender: TObject);
    procedure SetSearchControls(const Value: TSearchControls);
    procedure CreateButtons;
    procedure SetInformation(const Value: String);
    procedure SetImages;
    procedure SetDropDownButton(const Value: TAwesomeButton);
    procedure SetInformationButton(const Value: TAwesomeButton);
    procedure SetSearchButton(const Value: TAwesomeButton);
    procedure SetTextButton(const Value: TAwesomeButton);
    procedure OnButtonClick(Sender: TObject; const ClickArea: TAwesomeButtonEditHitTestArea; var Handled: Boolean);
  protected
    procedure DoOnEditChange; override;
    procedure DoEditKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoOnSearch; virtual;
    procedure DoOnClear; virtual;
    procedure DoOnClearInformation; virtual;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; x, y: Integer); override;

    property OnSearch: TNotifyEvent read FOnSearch write FOnSearch;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
    property OnClearInformation: TNotifyEvent read FOnClearInformation write FOnClearInformation;

    property SearchPopupMenu: TPopupMenu read FSearchPopupMenu write SetSearchPopupMenu;
    property SearchControls: TSearchControls read FSearchControls write SetSearchControls;
    property SearchDescription: String read FSearchDescription write SetSearchDescription;
    property Information: String read FInformation write SetInformation;
    property TextButton: TAwesomeButton read FTextButton write SetTextButton;
    property InformationButton: TAwesomeButton read FInformationButton write SetInformationButton;
    property SearchButton: TAwesomeButton read FSearchButton write SetSearchButton;
    property DropDownButton: TAwesomeButton read FDropDownButton write SetDropDownButton;
    property ClearButton: TAwesomeButton read FClearButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPTLibSearchEdit = class(TPTLibCustomSearchEdit)
  published
    property OnSearch;
    property OnClear;
    property OnClearInformation;

    property SearchPopupMenu;
    property SearchControls;
    property SearchDescription;
    property Information;
  public
    property SearchButton;
    property DropDownButton;
    property ClearButton;
    property TextButton;
  end;

implementation

{$R PTLib_VCL_R.dres}

{ PTLibCustomSearchEdit }

procedure TPTLibCustomSearchEdit.CreateButtons;
begin
  inherited;

  FSearchButton := AddInternalButton<TAwesomeButton>(TAwesomeButtonPosition.ebpLeft);
  FSearchButton.Margins.Top := 0;
  FSearchButton.Margins.Bottom := 0;
  FSearchButton.Background.Visible := False;
  FSearchButton.Image.Visible := True;
  FSearchButton.OnClick := OnButtonClick;

  FDropDownButton := AddInternalButton<TAwesomeButton>(TAwesomeButtonPosition.ebpLeft);
  FDropDownButton.Margins.Top := 1;
  FDropDownButton.Margins.Bottom := 1;
  FDropDownButton.Background.Visible := False;
  FDropDownButton.Image.Visible := True;

  FTextButton := AddInternalButton<TAwesomeButton>(TAwesomeButtonPosition.ebpLeft);
  FTextButton.Visible := False;
  FTextButton.Caption.Font.Color := clWhite;
  FTextButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsNormal, clWhite, clGray);
  FTextButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsHot, clWhite, clGray);
  FTextButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsPressed, clWhite, clGray);
  FTextButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsDisabled, clWhite, clGray);
  FTextButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsNormal, clWhite, clGray);
  FTextButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsHot, clWhite, clGray);
  FTextButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsPressed, clWhite, clGray);
  FTextButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsDisabled, clWhite, clGray);
  FTextButton.Caption.Margins.SetBounds(6, 3, 0, 3);
  FTextButton.CloseButton.Visible := True;
  FTextButton.CloseButton.Colors.SetStateColors(TAwesomeButtonState.bsNormal, clGray, clWhite, 150);
  FTextButton.CloseButton.Colors.SetStateColors(TAwesomeButtonState.bsHot, clGray, clWhite, 200);
  FTextButton.CloseButton.Colors.SetStateColors(TAwesomeButtonState.bsPressed, clGray, clWhite, 255);
  FTextButton.CloseButton.Colors.SetStateColors(TAwesomeButtonState.bsDisabled, clGray, clWhite, 50);
  FTextButton.CloseButton.Margins.SetBounds(6, 6, 6, 6);
  FTextButton.Caption.Visible := True;
  FTextButton.OnVisibilityChanged := OnButtonVisibilityChanged;

  FClearButton := AddInternalButton<TAwesomeButton>(TAwesomeButtonPosition.ebpRight);
  FClearButton.OnClick := OnButtonClick;
  FClearButton.CloseButton.Visible := True;
  FClearButton.Background.Visible := False;

  FInformationButton := AddInternalButton<TAwesomeButton>(TAwesomeButtonPosition.ebpRight);
  FInformationButton.Caption.Visible := True;
  FInformationButton.Caption.Font.Color := clWhite;

  FInformationButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsNormal, clWhite, clGray);
  FInformationButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsHot, clWhite, clGray);
  FInformationButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsPressed, clWhite, clGray);
  FInformationButton.Caption.Colors.SetStateColors(TAwesomeButtonState.bsDisabled, clWhite, clGray);

  FInformationButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsNormal, clWhite, clGray);
  FInformationButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsHot, clWhite, clGray);
  FInformationButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsPressed, clWhite, clGray);
  FInformationButton.Background.Colors.SetStateColors(TAwesomeButtonState.bsDisabled, clWhite, clGray);
  FInformationButton.Visible := False;

  SetImages;

  UpdateControls;
end;

procedure TPTLibCustomSearchEdit.OnButtonClick(Sender: TObject;
  const ClickArea: TAwesomeButtonEditHitTestArea; var Handled: Boolean) ;
begin
  if Sender = FClearButton then
  begin
    FEdit.Clear;
    SearchDescription := '';
    Handled := True;

    DoOnClear;
  end else
  if Sender = FSearchButton then
  begin
    DoOnSearch;
  end;
end;

procedure TPTLibCustomSearchEdit.DoEditKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    DoOnSearch;
  end;

  inherited;
end;

procedure TPTLibCustomSearchEdit.DoOnClear;
begin
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TPTLibCustomSearchEdit.DoOnClearInformation;
begin
  if Assigned(FOnClearInformation) then
    FOnClearInformation(Self);
end;

procedure TPTLibCustomSearchEdit.DoOnEditChange;
begin
  inherited;

  UpdateControls;
end;

procedure TPTLibCustomSearchEdit.DoOnSearch;
begin
  if Assigned(FOnSearch) then
  begin
    FOnSearch(Self);
  end;
end;

procedure TPTLibCustomSearchEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
var
  MouseDownHitTest: TAwesomeEditHitTest;
begin
  inherited;

  MouseDownHitTest := HitTest(Point(x, y));

  if (MouseDownHitTest.ButtonIndex = 2) and
     (MouseDownHitTest.ButtonHitTestArea = TAwesomeButtonEditHitTestArea.bhtCloseButton) then
  begin
    DoOnClearInformation;
  end;
end;

procedure TPTLibCustomSearchEdit.OnButtonVisibilityChanged(Sender: TObject);
var
  Button: TAwesomeButton;
begin
  if Sender = FTextButton then
  begin
    Button := Sender as TAwesomeButton;

    if not Button.Visible then
    begin
      FSearchDescription := '';
    end;
  end;
end;

procedure TPTLibCustomSearchEdit.SetInformation(const Value: String);
begin
  if FInformation <> Value then
  begin
    FInformation := Value;

    FInformationButton.Caption.Text := FInformation;
    FInformationButton.Visible := FInformation <> '';

    UpdateControls;
  end;
end;

procedure TPTLibCustomSearchEdit.SetInformationButton(
  const Value: TAwesomeButton);
begin
  FInformationButton.Assign(Value);
end;

procedure TPTLibCustomSearchEdit.SetSearchButton(
  const Value: TAwesomeButton);
begin
  FSearchButton.Assign(Value);
end;

procedure TPTLibCustomSearchEdit.SetSearchControls(const Value: TSearchControls);
begin
  if FSearchControls <> Value then
  begin
    FSearchControls := Value;

    UpdateControls;
  end;
end;

procedure TPTLibCustomSearchEdit.SetSearchDescription(const Value: String);
begin
  if FSearchDescription <> Value then
  begin
    FSearchDescription := Value;
    FTextButton.Caption.Text := Value;
    FTextButton.Visible := Value <> '';
  end;
end;

procedure TPTLibCustomSearchEdit.SetSearchPopupMenu(const Value: TPopupMenu);
begin
  FSearchPopupMenu := Value;

  FDropDownButton.PopupMenu := FSearchPopupMenu;
  FTextButton.PopupMenu := FSearchPopupMenu;

  FDropDownButton.Visible := FSearchPopupMenu <> nil;
end;

procedure TPTLibCustomSearchEdit.SetTextButton(const Value: TAwesomeButton);
begin
  FTextButton.Assign(Value);
end;

procedure TPTLibCustomSearchEdit.UpdateControls;
begin
  FClearButton.Visible := scClear in FSearchControls;
  FSearchButton.Visible := scSearch in FSearchControls;
  FDropDownButton.Visible := scSearchDropdown in FSearchControls;
  FClearButton.Enabled := Text <> '';
end;

procedure TPTLibCustomSearchEdit.WMSize(var Message: TWMSize);
begin
  inherited;

  SetImages;
end;

procedure TPTLibCustomSearchEdit.SetDropDownButton(
  const Value: TAwesomeButton);
begin
  FClearButton.Assign(Value);
end;

procedure TPTLibCustomSearchEdit.SetImages;
begin
  FSearchButton.Image.StateImages.SetImagesFromResource('Search16x16', 'Search16x16', 'Search16x16', 'Search16x16', False);
  FSearchButton.Image.StateImages.SetImagesFromResource('Search32x32', 'Search32x32', 'Search32x32', 'Search32x32', True);
  FDropDownButton.Image.StateImages.SetImagesFromResource('DropDown16x16', 'DropDown16x16', 'DropDown16x16', 'DropDown16x16', False);
  FDropDownButton.Image.StateImages.SetImagesFromResource('DropDown32x32', 'DropDown32x32', 'DropDown32x32', 'DropDown32x32', True);
end;

constructor TPTLibCustomSearchEdit.Create(AOwner: TComponent);
begin
  inherited;

  TextHint := 'Search';

  CreateButtons;

  SearchControls := [scSearch, scSearchDropdown, scClear];
end;

end.

