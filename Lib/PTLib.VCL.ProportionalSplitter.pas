{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.VCL.ProportionalSplitter                           }
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

unit PTLib.VCL.ProportionalSplitter;

interface

uses
  Windows, SysUtils, Vcl.Controls, Messages, Classes, CommCtrl, Vcl.ExtCtrls,
  System.Types;

type
  TSPlitterHelper = class helper for TSplitter
  public
    function FindControlEx: TControl;
  end;

  TPTLibProportionalSplitter = class(TSplitter)
  private
    FOldWindowProc: TWndMethod;
    FControlRatio: Double;
    FProportionalResize: Boolean;
    FLoaded: Boolean;

    procedure SubclassedParentWndProc(var Msg: TMessage);
    procedure SetRatio;
    procedure SetProportionalResize(const Value: Boolean);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure StopSizing; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ProportionalResize: Boolean read FProportionalResize write SetProportionalResize;
  end;

implementation

{ TPTLibProportionalSplitter }

constructor TPTLibProportionalSplitter.Create(AOwner: TComponent);
begin
  inherited;

  FProportionalResize := True;
end;

procedure TPTLibProportionalSplitter.Loaded;
begin
  inherited;

  FLoaded := True;
end;

procedure TPTLibProportionalSplitter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and
     (AComponent = Parent) then
  begin
    Parent.WindowProc := FOldWindowProc;
    FOldWindowProc := nil;
  end;
end;

procedure TPTLibProportionalSplitter.SetParent(AParent: TWinControl);
begin
  FControlRatio := -1;

  if Assigned(Parent) then
  begin
    Parent.WindowProc := FOldWindowProc;
  end;

  inherited SetParent(AParent);

  if Assigned(AParent) then
  begin
    FOldWindowProc := Parent.WindowProc;
    Parent.WindowProc := SubclassedParentWndProc;

    SetRatio;
  end;
end;

procedure TPTLibProportionalSplitter.SetProportionalResize(const Value: Boolean);
begin
  FProportionalResize := Value;

  SetRatio;
end;

procedure TPTLibProportionalSplitter.SetRatio;
var
  ActiveControl: TControl;
begin
  if FProportionalResize then
  begin
    ActiveControl := FindControlEx;

    if (Parent <> nil) and
       (ActiveControl <> nil) and
       (Parent.Height <> 0) and
       (Parent.Width <> 0) then
    begin
      case Align of
        alTop,
        alBottom: FControlRatio := ActiveControl.Height / Parent.Height;
        alLeft,
        alRight: FControlRatio := ActiveControl.Width / Parent.Width;
      end;
    end;
  end
  else
  begin
    FControlRatio := -1;
  end;
end;

procedure TPTLibProportionalSplitter.StopSizing;
begin
  inherited;

  SetRatio;
end;

procedure TPTLibProportionalSplitter.SubclassedParentWndProc(Var Msg: TMessage);
var
  ActiveControl: TControl;
begin
  FOldWindowProc(Msg);

  if Msg.Msg = WM_SIZE then
  begin
    if (FControlRatio <> -1) and
       (Parent <> nil) and
       (FLoaded) and
       (not (csLoading in Parent.ComponentState)) then
    begin
      ActiveControl := FindControlEx;

      if ActiveControl <> nil then
      begin
        case Align of
          alTop,
          alBottom:
            begin
              ActiveControl.Height := Round(Parent.Height * FControlRatio);

              if ActiveControl.Height < MinSize then
                ActiveControl.Height := MinSize;
            end;

          alLeft,
          alRight:
            begin
              ActiveControl.Width := Round(Parent.Width * FControlRatio);

              if ActiveControl.Width < MinSize then
                ActiveControl.Width := MinSize;
            end;
        end;
      end;
    end
    else
    begin
      SetRatio;
    end;
  end;
end;


{ TSPlitterHelper }

function TSPlitterHelper.FindControlEx: TControl;
var
  P: TPoint;
  I: Integer;
  R: TRect;
begin
  Result := nil;
  P := Point(Left, Top);
  case Align of
    alLeft:
      if not AlignWithMargins then
        Dec(P.X)
      else
        Dec(P.X, Margins.Left + 1);
    alRight:
      if not AlignWithMargins then
        Inc(P.X, Width)
      else
        Inc(P.X, Width + Margins.Right + 1);
    alTop:
      if not AlignWithMargins then
        Dec(P.Y)
      else
        Dec(P.Y, Margins.Top + 1);
    alBottom:
      if not AlignWithMargins then
        Inc(P.Y, Height)
      else
        Inc(P.Y, Height + Margins.Bottom + 1);
  else
    Exit;
  end;
  for I := 0 to Parent.ControlCount - 1 do
  begin
    Result := Parent.Controls[I];
    if Result.Visible and Result.Enabled and
      (Result.Align in [alLeft, alRight, alTop, alBottom]) and
     ((Result.Align in [alLeft, alRight]) = (Align in [alLeft, alRight])) then
    begin
      R := Result.BoundsRect;
      if Result.AlignWithMargins then
      begin
        Inc(R.Right, Result.Margins.Right);
        Dec(R.Left, Result.Margins.Left);
        Inc(R.Bottom, Result.Margins.Bottom);
        Dec(R.Top, Result.Margins.Top);
      end;
      if (R.Right - R.Left) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Left)
        else
          Inc(R.Right);
      if (R.Bottom - R.Top) = 0 then
        if Align in [alTop, alLeft] then
          Dec(R.Top)
        else
          Inc(R.Bottom);
      if R.Contains(P) then Exit;
    end;
  end;

  Result := nil;
end;

end.

