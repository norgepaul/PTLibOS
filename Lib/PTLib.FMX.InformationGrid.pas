unit PTLib.FMX.InformationGrid;

interface

uses
  System.Classes, System.Rtti, SysUtils, Generics.Collections,
  System.Types,

  FMX.Forms, FMX.Grid, FMX.Edit, FMX.Types, FMX.Layouts, FMX.StdCtrls,
  FMX.Grid.Style,

  PTLib.Common.InformationList,
  PTLib.Common.Interfaces;

type
  TInformationGrid = class(TLayout)
  private
    FPropertyColumnRatio: Single;
    FSrcInformationList: IInformationList;
    FInformationList: IInformationList;
    FSearchText: String;
    FGrid: TCustomGrid;
    FEdit: TEdit;
    FLabel: TLabel;
    FPreviousContentBounds: TRectF;
    FSelectedRow: Integer;

    procedure SetPropertyColumnRatio(const Value: Single);
    procedure UpdateColumnSizes;
    procedure CreateColumns;
    function RowVisible(const InformationList: IInformationList; const RowIndex: Integer): Boolean;
    procedure OnEditChange(Sender: TObject);
    procedure OnGridCalcContentBounds(Sender: TObject; var ContentBounds: TRectF);
    procedure OnGridResize(Sender: TObject);
  protected
    procedure Resize; override;

    procedure OnGetValue(Sender: TObject; const Col, Row: Integer; var Value: System.Rtti.TValue);
    procedure DoShowInformation(const InformationList: IInformationList);
    procedure DoSearch(const Text: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;

    procedure ShowInformation(const InformationList: IInformationList); virtual;
    procedure Search(const Text: String); virtual;
  published
    property PropertyColumnRatio: Single read FPropertyColumnRatio write SetPropertyColumnRatio;
  end;

implementation

{ TInformationGrid }

procedure TInformationGrid.CreateColumns;
var
  Column: TStringColumn;
begin
  inherited;

  while FGrid.ColumnCount > 0 do
  begin
    FGrid.Columns[0].Free;
  end;

  Column := TStringColumn.Create(FGrid);
  Column.Parent := FGrid;
  Column.Header := 'Property';

  Column := TStringColumn.Create(FGrid);
  Column.Parent := FGrid;
  Column.Header := 'Value';

  FGrid.Columns[0].Width := 250;
  FGrid.Columns[1].Width := 1000;
end;

destructor TInformationGrid.Destroy;
begin
  inherited;
end;

constructor TInformationGrid.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FLabel := TLabel.Create(Self);
    FLabel.Parent := Self;
    FLabel.Align := TAlignLayout.Center;
    FLabel.AutoSize := True;
    FLabel.Text := 'Nothing to display';

    FEdit := TEdit.Create(Self);
    FEdit.Parent := Self;
    FEdit.Align := TAlignLayout.Top;
    FEdit.OnChangeTracking := OnEditChange;
    FEdit.TextPrompt := 'Search';
    FEdit.Margins.Bottom := 2;
    FEdit.Visible := False;
    FEdit.SetSubComponent(True);

    FGrid := TCustomGrid.Create(Self);
    FGrid.Parent := Self;
    FGrid.Align := TAlignLayout.Client;
    FGrid.OnGetValue := OnGetValue;
    FGrid.Visible := False;
    FGrid.SetSubComponent(True);
    FGrid.OnCalcContentBounds := OnGridCalcContentBounds;
//    FGrid.Model.AutoCalculateContentSize := True;
    FGrid.OnResize := OnGridResize;

    FGrid.RowCount := 0;

    FGrid.Options := [
      TGridOption.ColumnResize,
      TGridOption.RowLines,
      TGridOption.RowSelect,
      TGridOption.AlwaysShowSelection
    ];

    CreateColumns;
  end;

  FInformationList := TInformationList.Create;

  FPropertyColumnRatio := 0.4;
end;

procedure TInformationGrid.OnGridResize(Sender: TObject);
begin
  if FGRid.ContentBounds.Width <> FPreviousContentBounds.Width then
  begin
    UpdateColumnSizes;
  end;

  FPreviousContentBounds := FGrid.ContentBounds;
end;

procedure TInformationGrid.OnGridCalcContentBounds(Sender: TObject; var ContentBounds: TRectF);
begin
  if ContentBounds.Width <> FPreviousContentBounds.Width then
  begin
    UpdateColumnSizes;
  end;

  FPreviousContentBounds := ContentBounds;
end;

procedure TInformationGrid.OnEditChange(Sender: TObject);
begin
  Search(FEdit.Text);
end;

procedure TInformationGrid.OnGetValue(Sender: TObject; const Col, Row: Integer;
  var Value: System.Rtti.TValue);
begin
  case Col of
    0: Value := FInformationList.GetProperty(Row);
    1: Value := FInformationList.GetValue(Row);
  end;
end;

procedure TInformationGrid.Resize;
begin
  inherited;
end;

procedure TInformationGrid.DoSearch(const Text: String);
begin
  FSearchText := UpperCase(Text);

  if FSrcInformationList <> nil then
  begin
    DoShowInformation(FSrcInformationList);
  end;
end;

procedure TInformationGrid.Search(const Text: String);
begin
  DoSearch(Text);
end;

procedure TInformationGrid.SetPropertyColumnRatio(const Value: Single);
begin
  FPropertyColumnRatio := Value;

  UpdateColumnSizes;
end;

procedure TInformationGrid.ShowInformation(
  const InformationList: IInformationList);
begin
  FSrcInformationList := InformationList;

  DoShowInformation(FSrcInformationList);

  UpdateColumnSizes;
end;

procedure TInformationGrid.UpdateColumnSizes;
begin
  (*
  if (FGrid <> nil) and
     (not (csDestroying in ComponentState)) and
     (FGrid.ColumnCount = 2) then
  begin
    { TODO : This feels wrong, but it seems like the only way to make FMX update the controls }
    Application.ProcessMessages;

    FGrid.Columns[0].Width := 150;// Trunc(FGrid.ViewportSize.Width * FPropertyColumnRatio);
    FGrid.Columns[1].Width := 440; //FGrid.ViewportSize.Width - FGrid.Columns[0].Width - 1;
  end;
  *)
end;

function TInformationGrid.RowVisible(const InformationList: IInformationList;
  const RowIndex: Integer): Boolean;
begin
  Result :=
    (InformationList <> nil) and
    (RowIndex >= 0) and
    (RowIndex < InformationList.Lines.Count) and
    ((FSearchText = '') or
     ((Pos(FSearchText, UpperCase(InformationList.Lines[RowIndex])) > 0)) and
      (InformationList.Lines[RowIndex] <> ''));
end;

procedure TInformationGrid.DoShowInformation(
  const InformationList: IInformationList);
var
  i: Integer;
  ViewPort, CellRect: TRectF;
begin
  FInformationList.Clear;

  if FGrid <> nil then
  begin
    FGrid.BeginUpdate;
    try
      FSelectedRow := FGrid.Selected;

      if (InformationList = nil) or
         (InformationList.Lines.Count = 0) then
      begin
        FLabel.Visible := True;
        FEdit.Visible := False;
        FGrid.Visible := False;
      end
      else
      begin
        FLabel.Visible := False;
        FEdit.Visible := True;
        FGrid.Visible := True;

        FInformationList.PropertySeparator := InformationList.PropertySeparator;

        for i := 0 to pred(InformationList.Lines.Count) do
        begin
          if RowVisible(InformationList, i) then
          begin
            FInformationList.Lines.Add(InformationList.Lines[i]);
          end;
        end;

        // Why is the FMX Grid so terrible!
        FGrid.RowCount := 0;
        FGrid.RowCount := FInformationList.Lines.Count;
      end;

      FGrid.Repaint;
    finally
      FGrid.EndUpdate;
    end;
  end;

  if FSelectedRow <> -1 then
  begin
    // This is a tricky workaround for an awful FMX bug
    ViewPort := RectF(
      0,
      FGrid.ViewportPosition.Y,
      MaxInt,
      FGrid.ViewportPosition.Y + FGrid.ViewportSize.Height);

    CellRect := FGrid.CellRect(0, FSelectedRow);

    if ViewPort.Contains(CellRect) then
    begin
      FGrid.SelectRow(FSelectedRow);
    end;
  end;
end;

procedure TInformationGrid.Loaded;
begin
  inherited;

  UpdateColumnSizes;
end;

end.
