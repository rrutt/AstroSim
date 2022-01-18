unit AstroSimSpace;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType;

//const

type
  TAstroSimSpace = class(TCustomControl)
    private
    public
      procedure Initialize;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
  end;

implementation
  //const

  //var

  procedure TAstroSimSpace.Initialize;
  //var
  begin
  end;

  procedure TAstroSimSpace.EraseBackground(DC: HDC);
  begin
    // Uncomment this to enable default background erasing
    //inherited EraseBackground(DC);
  end;

  procedure TAstroSimSpace.Paint;
  var
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create;
    try
      { https://wiki.freepascal.org/Drawing_with_canvas }

      Bitmap.Height := Height;
      Bitmap.Width := Width;

      Bitmap.Canvas.Brush.Color := clGray;
      Bitmap.Canvas.FillRect(0, 0, Width, Height);

      Bitmap.Canvas.Pen.Color := clWhite;
      Bitmap.Canvas.Line(0, 0, Width, Height);

      Bitmap.Canvas.Pen.Color := clBlack;
      Bitmap.Canvas.Line(0, Height, Width, 0);

      Bitmap.Canvas.Brush.Color := clRed;
      Bitmap.Canvas.Ellipse(Width div 8, Height div 4, Width div 2, Height div 2);

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.
