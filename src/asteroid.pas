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
      procedure MergeIfAdjacent(const OtherAsteroid: TAsteroid);
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

  procedure TAsteroid.MergeIfAdjacent(const OtherAsteroid: TAsteroid);
  var
    dx: Single;
    dy: Single;
    distanceSquared: Single;
    r: Single;
    rSquared: Single;
    isAdjacent: Boolean;
  begin
    dx := OtherAsteroid.X - X;
    dy := OtherAsteroid.Y - Y;
    distanceSquared := (dx * dx) + (dy * dy);

    r := Radius + OtherAsteroid.Radius;
    rSquared := (r * r);

    isAdjacent := (distanceSquared <= rSquared);
    if (isAdjacent) then begin
      X := (X + OtherAsteroid.X) / 2;
      Y := (Y + OtherAsteroid.Y) / 2;

      // Combine momentum = Mass * Velocity.
      VelocityX := (VelocityX * Mass) + (OtherAsteroid.VelocityX * OtherAsteroid.Mass);
      VelocityY := (VelocityY * Mass) + (OtherAsteroid.VelocityY * OtherAsteroid.Mass);

      Mass := Mass + OtherAsteroid.Mass;

      VelocityX := VelocityX / Mass;
      VelocityY := VelocityY / Mass;

      Radius := Round(Sqrt(Mass) + 0.5);

      OtherAsteroid.IsActive := false;
    end;
  end;

  procedure TAsteroid.Accelerate(const OtherAsteroid: TAsteroid);
  var
    dx: Single;
    dy: Single;
    distanceSquared: Single;
    accelerationMagnitude: Single;
    distanceMagnitude: Single;
    dxNormalized: Single;
    dyNormalized: Single;
  begin
    dx := OtherAsteroid.X - X;
    dy := OtherAsteroid.Y - Y;
    distanceSquared := (dx * dx) + (dy * dy);

    distanceMagnitude := Sqrt(DistanceSquared);
    dxNormalized := dx / distanceMagnitude;
    dyNormalized := dy / distanceMagnitude;

    accelerationMagnitude := (GRAVITY * OtherAsteroid.Mass) / distanceSquared;

    AccelerationX := AccelerationX + (accelerationMagnitude * dxNormalized);
    AccelerationY := AccelerationY + (accelerationMagnitude * dyNormalized);
  end;

  procedure TAsteroid.Move;
  begin
    VelocityX := VelocityX + AccelerationX;
    VelocityY := VelocityY + AccelerationY;

    X := X + VelocityX;
    Y := Y + VelocityY;
  end;

end.

