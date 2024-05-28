unit PTLib.FMX.SpriteSheetAnimation;

interface

uses
  SysUtils,

  FMX.Controls, FMX.Graphics, FMX.MultiResBitmap;

type
  TSpriteSheetAnimation = class(TControl, IBitmapObject, IMultiResBitmapObject)
  strict private
    FActive: Boolean;
    FSpiteColumns: Integer;
    FSpiteRows: Integer;
    FMultiResBitmap: TFixedMultiResBitmap;
  published

  end;

implementation

end.
