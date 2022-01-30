unit AstroSimSpace;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Math,
  Asteroid;

const
  MAXIMUM_ASTEROID_COUNT = 10000;
  DEFAULT_ASTEROID_COUNT = 10;
  ZOOM_DIVISOR = 120;

type
  TAstroSimSpace = class(TCustomControl)
    private
      InitialAsteroidCount: Integer;
      CenterX: Integer;
      CenterY: Integer;
      ViewOffsetX: Integer;
      ViewOffsetY: Integer;
      ZoomFactor: Integer;
      InverseZoom: Single;

    public
      ActiveAsteroidCount: Integer;

      procedure Initialize;
      procedure Randomize(const AsteroidCount: Integer);
      procedure Iterate;
      procedure EraseBackground({%H-}DC: HDC); override;
      procedure Paint; override;
      procedure MouseDown(Sender: TObject; {%H-}Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer); overload;
      Procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
        WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
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

    CenterX := Width div 2;
    CenterY := Height div 2;

    ViewOffsetX := 0;
    ViewOffsetY := 0;

    ZoomFactor := 1;
    InverseZoom := 1.0 / ZoomFactor;

    for i := 1 to MAXIMUM_ASTEROID_COUNT do begin
      a := TAsteroid.Create;
      Asteroids[i] := a;
    end;

    ActiveAsteroidCount := 0;

    OnMouseDown := @MouseDown;
    OnMouseWheel := @MouseWheel;
  end;

  procedure TAstroSimSpace.Randomize(const AsteroidCount: Integer);
  var
    i: Integer;
  begin
    InitialAsteroidCount := AsteroidCount;

    ViewOffsetX := CenterX;
    ViewOffsetY := CenterY;

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
    zoomedRadius: Integer;
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
          x := Trunc(a.X * InverseZoom) + ViewOffsetX;
          y := Trunc(a.Y * InverseZoom) + ViewOffsetY;
          zoomedRadius := Max(2, Ceil(a.Radius * InverseZoom));
          Bitmap.Canvas.Ellipse(x - zoomedRadius, y - zoomedRadius, x + zoomedRadius, y + zoomedRadius);
        end;
      end;

      Canvas.Draw(0, 0, Bitmap);
    finally
      Bitmap.Free;
    end;

    inherited Paint;
  end;

  procedure TAstroSimSpace.MouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    offsetX: Integer;
    offsetY: Integer;
  begin
    { Re-center the space on the clicked point. }

    offsetX := CenterX - X;
    offsetY := CenterY - Y;

    ViewOffsetX := ViewOffsetX + offsetX;
    ViewOffsetY := ViewOffsetY + offsetY;

    Paint;
  end;

  Procedure TAstroSimSpace.MouseWheel(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    ZoomFactor := Max(ZoomFactor - (WheelDelta div ZOOM_DIVISOR), 1);
    InverseZoom := 1.0 / ZoomFactor;

    Paint;
  end;

begin
end.
