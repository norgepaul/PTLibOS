unit PTLib.VCL.PageControl;

interface

uses
  System.SysUtils, System.Classes, System.Types,

  WinAPI.Messages, WinAPI.Windows, Winapi.CommCtrl,

  Vcl.Controls, Vcl.ComCtrls;

type
  THideTabOption = (
    htoDefault,
    htoAlways,
    htoRuntime
  );

  TPTLibPageControl = class(TPageControl)
  strict private
    FHideTabOption: THideTabOption;

    function TabsVisible: Boolean;
  private
    procedure SetHideTabOption(const Value: THideTabOption);
    procedure UpdateTabVisibility;

    procedure TCMAdjustRect(var Msg: TMessage); message TCM_ADJUSTRECT;
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Loaded; override;
  published
    property HideTabs: THideTabOption read FHideTabOption write SetHideTabOption;
  end;

implementation

constructor TPTLibPageControl.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TPTLibPageControl.UpdateTabVisibility;
var
  i: Integer;
  ActiveIndex: Integer;
begin
  ActiveIndex := ActivePageIndex;

  for i := 0 to pred(PageCount) do
  begin
    Pages[i].TabVisible := TabsVisible;
  end;

  ActivePageIndex := ActiveIndex;
end;

procedure TPTLibPageControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
end;

procedure TPTLibPageControl.WMNCPaint(var Message: TWMNCPaint);
begin
  Message.Result := 0;
end;

procedure TPTLibPageControl.Loaded;
begin
  inherited;

  UpdateTabVisibility;
end;

procedure TPTLibPageControl.SetHideTabOption(const Value: THideTabOption);
begin
  if FHideTabOption <> Value then
  begin
    FHideTabOption := Value;

    UpdateTabVisibility;

    RecreateWnd;
  end;
end;

function TPTLibPageControl.TabsVisible: Boolean;
begin
  Result :=
    (FHideTabOption = THideTabOption.htoDefault) or
    ((FHideTabOption = THideTabOption.htoRuntime) and
     (csDesigning in ComponentState));
end;

procedure TPTLibPageControl.TCMAdjustRect(var Msg: TMessage);
const
  BorderSize = 8;
begin
  inherited;

  if Msg.WParam = 0 then
  begin
    InflateRect(PRect(Msg.LParam)^, BorderSize, BorderSize)
  end
  else
  begin
    InflateRect(PRect(Msg.LParam)^, -BorderSize, -BorderSize)
  end;
end;

end.
