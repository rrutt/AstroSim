unit Asteroid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

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
      function IsAdjacent(const OtherAsteroid: TAsteroid; out DistanceX: Single; out DistanceY: Single; out DistanceSquared: Single): Boolean;
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

  function TAsteroid.IsAdjacent(const OtherAsteroid: TAsteroid; out DistanceX: Single; out DistanceY: Single; out DistanceSquared: Single): Boolean;
  var
    r: Single;
    r2: Single;
  begin
    DistanceX := OtherAsteroid.X - X;
    DistanceY := OtherAsteroid.Y - Y;
    DistanceSquared := (DistanceX * DistanceX) + (DistanceY * DistanceY);
    r := Radius + OtherAsteroid.Radius;
    r2 := (r * r);
    Result := (DistanceSquared <= r2);
  end;

  procedure TAsteroid.Accelerate(const OtherAsteroid: TAsteroid);
  var
    dx: Single;
    dy: Single;
    d2: Single;
    a: Single;
  begin
    if (not IsAdjacent(OtherAsteroid, dx, dy, d2)) then begin
      a := (GRAVITY * OtherAsteroid.Mass) / d2;
      AccelerationX := AccelerationX + (a * Sign(dx));
      AccelerationY := AccelerationY + (a * Sign(dy));
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

