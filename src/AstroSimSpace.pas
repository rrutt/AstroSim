unit AstroSimSpace;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType,
  Asteroid;

const
  MAXIMUM_ASTEROID_COUNT = 1000;
  DEFAULT_ASTEROID_COUNT = 100;

type
  TAstroSimSpace = class(TCustomControl)
    private
      InitialAsteroidCount: Integer;

    public
      ActiveAsteroidCount: Integer;

      procedure Initialize;
      procedure Randomize(const AsteroidCount: Integer);
      procedure Iterate;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
  end;

implementation

  var
    Asteroids: array[1..MAXIMUM_ASTEROID_COUNT] of TAsteroid;

  procedure TAstroSimSpace.Initialize;
  var
    i: Integer;
    a: TAsteroid;
  begin
    InitialAsteroidCount := MAXIMUM_ASTEROID_COUNT;

    for i := 1 to MAXIMUM_ASTEROID_COUNT do begin
      a := TAsteroid.Create;
      Asteroids[i] := a;
    end;

    ActiveAsteroidCount := 0;
  end;

  procedure TAstroSimSpace.Randomize(const AsteroidCount: Integer);
  var
    i: Integer;
  begin
    InitialAsteroidCount := AsteroidCount;

    for i := 1 to InitialAsteroidCount do begin
      Asteroids[i].Randomize(Width, Height);
    end;

    ActiveAsteroidCount := InitialAsteroidCount;
  end;

  procedure TAstroSimSpace.Iterate;
  var
    i: Integer;
    j: Integer;
    ai: TAsteroid;
    aj: TAsteroid;
  begin
    for i := 1 to InitialAsteroidCount do begin
      ai := Asteroids[i];
      if (ai.IsActive) then begin
        for j := 1 to InitialAsteroidCount do begin
          aj := Asteroids[j];
          if ((i <> j) and aj.IsActive) then begin
            if (ai.MergeIfAdjacent(aj)) then begin
              Dec(ActiveAsteroidCount);
            end;
          end;
        end;
      end;
    end;

    for i := 1 to InitialAsteroidCount do begin
      ai := Asteroids[i];
      if (ai.IsActive) then begin
        ai.AccelerationX := 0.0;
        ai.AccelerationY := 0.0;

        for j := 1 to InitialAsteroidCount do begin
          aj := Asteroids[j];
          if ((i <> j) and aj.IsActive) then begin
            ai.Accelerate(aj);
          end;
        end;
      end;
    end;

    for i := 1 to InitialAsteroidCount do begin
      ai := Asteroids[i];
      if (ai.IsActive) then begin
        ai.Move;
      end;
    end;
  end;

  procedure TAstroSimSpace.EraseBackground(DC: HDC);
  begin
    // Uncomment this to enable default background erasing
    //inherited EraseBackground(DC);
  end;

  procedure TAstroSimSpace.Paint;
  var
    i: Integer;
    x: Integer;
    y: Integer;
    a: TAsteroid;
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create;
    try
      { https://wiki.freepascal.org/Drawing_with_canvas }

      Bitmap.Height := Height;
      Bitmap.Width := Width;

      Bitmap.Canvas.Brush.Color := clWhite;
      for i := 1 to InitialAsteroidCount do begin
        a := Asteroids[i];
        if (a.IsActive) then begin
          x := Round(a.X);
          y := Round(a.Y);
          Bitmap.Canvas.Ellipse(x - a.Radius, y - a.Radius, x + a.Radius, y + a.Radius);
        end;
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

begin
end.
