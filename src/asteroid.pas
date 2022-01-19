unit Asteroid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAsteroid = Class
    private
    public
      IsActive: Boolean;
      Radius: Integer;
      Mass: Single;
      X: Single;
      Y: Single;
      VelocityX: Single;
      VelocityY: Single;
      AccelerationX: Single;
      AccelerationY: Single;

      procedure Randomize(const MaxX: Integer; const MaxY: Integer);
      function IsAdjacent(const OtherAsteroid: TAsteroid; out NormalX: Single; out NormalY: Single; out DistanceSquared: Single): Boolean;
      procedure Accelerate(const OtherAsteroid: TAsteroid);
      procedure Move;
  end;

implementation
  const
    INITIAL_RADIUS = 2;
    INITIAL_MASS = 4.0;
    GRAVITY = 10.0;

  procedure TAsteroid.Randomize(const MaxX: Integer; const MaxY: Integer);
  begin
    IsActive := true;

    Radius := INITIAL_RADIUS;
    Mass := INITIAL_MASS;

    X := 1.0 + Random(MaxX - 1);
    Y := 1.0 + Random(MaxY - 1);

    VelocityX := 0.0;
    VelocityY := 0.0;

    AccelerationX := 0.0;
    AccelerationY := 0.0;
  end;

  function TAsteroid.IsAdjacent(const OtherAsteroid: TAsteroid; out NormalX: Single; out NormalY: Single; out DistanceSquared: Single): Boolean;
  var
    dx: Single;
    dy: Single;
    magnitude: Single;
    r: Single;
    r2: Single;
  begin
    dx := OtherAsteroid.X - X;
    dy := OtherAsteroid.Y - Y;
    DistanceSquared := (dx * dx) + (dy * dy);

    magnitude := Sqrt(DistanceSquared);
    NormalX := dx / magnitude;
    NormalY := dy / magnitude;

    r := Radius + OtherAsteroid.Radius;
    r2 := (r * r);

    Result := (DistanceSquared <= r2);
  end;

  procedure TAsteroid.Accelerate(const OtherAsteroid: TAsteroid);
  var
    nx: Single;
    ny: Single;
    d2: Single;
    a: Single;
  begin
    if (not IsAdjacent(OtherAsteroid, nx, ny, d2)) then begin
      a := (GRAVITY * OtherAsteroid.Mass) / d2;

      AccelerationX := AccelerationX + (a * nx);
      AccelerationY := AccelerationY + (a * ny);
    end;
  end;

  procedure TAsteroid.Move;
  begin
    VelocityX := VelocityX + AccelerationX;
    VelocityY := VelocityY + AccelerationY;

    X := X + VelocityX;
    Y := Y + VelocityY;
  end;

end.

