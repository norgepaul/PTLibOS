{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.Common.InformationList                             }
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

unit PTLib.Common.InformationList;

interface

uses
  System.Classes, System.SysUtils, System.Variants,

  PTLib.Common.Interfaces,
  PTLib.Common.Strings,
  PTLib.Common.Classes;

type
  TInformationList = class(TInterfacedObject,
                           IInformationList)
  strict private
    FLines: TStringList;
    FPropertySeparator: String;
    FHeadingPrefix: String;
    FHeadingPostFix: String;
    FMetaData: IList<String>;
  private
    function GetPropertySeparator: String;
    procedure SetPropertySeparator(const Value: String);
    function GetHeadingPostFix: String;
    function GetHeadingPrefix: String;
    procedure SetHeadingPostFix(const Value: String);
    procedure SetHeadingPrefix(const Value: String);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    function IsHeading(const Value: String): Boolean;
    procedure InternalAddLine(const LineText, MetaData: String);
  public
    constructor Create; overload;
    constructor Create(const PropertySerparator: String); overload;
    destructor Destroy; override;

    function Clear: IInformationList;
    function AddHeading(const Text: String; const MetaData: String = ''): IInformationList;
    function AddProperty(const PropertyName: String; const PropertyValue: Variant; const EmptyValue: String = 'None'; const MetaData: String = ''): IInformationList; overload;
    function AddProperty(const PropertyName: String; const PropertyValue: String; const Args: Array of const; const MetaData: String = ''): IInformationList; overload;
    function AddLine(const LineText: String; const MetaData: String = ''): IInformationList; overload;
    function AddLine(const LineText: String; const Args: Array of const; const MetaData: String = ''): IInformationList; overload;
    function AppendInformation(const Information: IInformationList; const LinePrefix: String = ''): IInformationList;
    function GetProperty(const Index: Integer): String;
    function GetValue(const Index: Integer): String;
    function GetMetaData(const Index: Integer): String;

    property PropertySeparator: String read GetPropertySeparator write SetPropertySeparator;
    property HeadingPrefix: String read GetHeadingPrefix write SetHeadingPrefix;
    property HeadingPostFix: String read GetHeadingPostFix write SetHeadingPostFix;
    property Lines: TStrings read GetLines write SetLines;
  end;

  TInterfacedObjectInfo = class(TInterfacedObject,
                                IInformationProvider)
  protected
    procedure DoGetInformation(const InformationList: IInformationList); virtual;

    function GetInformation(const PropertySeparator: String = '='): IInformationList;
  end;

implementation

{ TInformationList }

function TInformationList.IsHeading(const Value: String): Boolean;
begin
  Result :=
     ((copy(Value, 1, length(FHeadingPrefix)) = FHeadingPrefix) and
      (copy(Value, length(Value) - length(FHeadingPrefix) + 1, MaxInt) = FHeadingPostfix));
end;

function TInformationList.AddHeading(const Text: String; const MetaData: String = ''): IInformationList;
var
  i: Integer;
begin
  Result := Self;

  // Remove empty headings
  for i := pred(FLines.Count) downto 0 do
  begin
    if (FLines[i] = '') or
       (IsHeading(FLines[i])) then
    begin
      FLines.Delete(i);
    end
    else
    begin
      Break;
    end;
  end;

  // Add an empty line before the heading
  if FLines.Count > 0 then
  begin
    InternalAddLine('', '');
  end;

  // Add the heading
  InternalAddLine(FHeadingPrefix + Text + FHeadingPostFix, MetaData);
end;

function TInformationList.AddProperty(const PropertyName: String;
  const PropertyValue: Variant; const EmptyValue: String; const MetaData: String): IInformationList;
var
  RealPropertyValue: String;
begin
  Result := Self;

  // Replace the value if it's empty
  if (PropertyValue = Null) or
     (VarToStr(PropertyValue) = '') then
  begin
    RealPropertyValue := EmptyValue;
  end
  else
  begin
    RealPropertyValue := VarToStr(PropertyValue);
  end;

  InternalAddLine(PropertyName + FPropertySeparator + RealPropertyValue, MetaData);
end;

procedure TInformationList.InternalAddLine(const LineText, MetaData: String);
begin
  Lines.Add(LineText);
  FMetaData.Add(MetaData);
end;

function TInformationList.AddLine(const LineText: String; const MetaData: String): IInformationList;
begin
  Result := Self;

  InternalAddLine(LineText, MetaData);
end;

function TInformationList.AddLine(const LineText: String;
  const Args: array of const; const MetaData: String = ''): IInformationList;
begin
  Result := AddLine(format(LineText, Args), MetaData);
end;

function TInformationList.AddProperty(const PropertyName,
  PropertyValue: String; const Args: array of const; const MetaData: String = ''): IInformationList;
begin
  Result := Self;

  AddProperty(
    PropertyName,
    Format(PropertyValue, Args),
    MetaData);
end;

function TInformationList.AppendInformation(
  const Information: IInformationList; const LinePrefix: String): IInformationList;
var
  i: Integer;
begin
  Result := Self;

  if (FLines.Count > 0) and
     (not IsHeading(FLines[pred(FLines.Count)])) and
     (Information.Lines.Count > 0) then
  begin
    FLines.Add('');
  end;

  for i := 0 to pred(Information.Lines.Count) do
  begin
    FLines.Add(LinePrefix + Information.Lines[i]);
  end;
end;

function TInformationList.Clear: IInformationList;
begin
  Result := Self;

  FLines.Clear;
end;

constructor TInformationList.Create(const PropertySerparator: String);
begin
  Create;

  FPropertySeparator := PropertySerparator;
end;

constructor TInformationList.Create;
begin
  FLines := TStringList.Create;
  FMetaData := TList<String>.Create;

  FPropertySeparator := ': ';
  FHeadingPrefix := '[ ';
  FHeadingPostFix := ' ]';
end;

destructor TInformationList.Destroy;
begin
  FreeAndNil(FLines);

  inherited;
end;

function TInformationList.GetHeadingPostFix: String;
begin
  Result := FHeadingPostFix;
end;

function TInformationList.GetHeadingPrefix: String;
begin
  Result := FHeadingPrefix;
end;

function TInformationList.GetLines: TStrings;
begin
  Result := FLines;
end;

function TInformationList.GetMetaData(const Index: Integer): String;
begin
  Result := FMetaData[Index];
end;

function TInformationList.GetProperty(const Index: Integer): String;
var
  p: Integer;
begin
  p := pos(FPropertySeparator, FLines[Index]);

  if p = 0 then
  begin
    Result := FLines[Index];
  end
  else
  begin
    Result := copy(FLines[Index], 1, p - 1);
  end;
end;

function TInformationList.GetPropertySeparator: String;
begin
  Result := FPropertySeparator;
end;

function TInformationList.GetValue(const Index: Integer): String;
var
  p: Integer;
begin
  if IsHeading(FLines[Index]) then
  begin
    Result := '';
  end
  else
  begin
    p := pos(FPropertySeparator, FLines[Index]);

    if p = 0 then
    begin
      Result := FLines[Index];
    end
    else
    begin
      Result := copy(FLines[Index], p + 1, MaxInt);
    end;
  end;
end;

procedure TInformationList.SetHeadingPostFix(const Value: String);
begin
   FHeadingPostFix := Value;
end;

procedure TInformationList.SetHeadingPrefix(const Value: String);
begin
  FHeadingPrefix := Value;
end;

procedure TInformationList.SetLines(const Value: TStrings);
begin
  FMetaData.Clear;

  FLines.Assign(Value);
end;

procedure TInformationList.SetPropertySeparator(const Value: String);
begin
  FPropertySeparator := Value;
end;

{ TInterfacedObjectInfo }

procedure TInterfacedObjectInfo.DoGetInformation(const InformationList: IInformationList);
begin
  // Override
end;

function TInterfacedObjectInfo.GetInformation(const PropertySeparator: String): IInformationList;
begin
  Result := TInformationList.Create;
  Result.PropertySeparator := PropertySeparator;

  DoGetInformation(Result);
end;

end.

