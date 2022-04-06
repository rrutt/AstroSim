unit Asteroid;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math, SysUtils;

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

      procedure Clear;
      procedure Randomize(const Width: Integer; const Height: Integer);
      procedure Initialize(const newX: Integer; const newY: Integer);
      function MergeIfAdjacent(const OtherAsteroid: TAsteroid): Boolean;
      procedure Accelerate(const OtherAsteroid: TAsteroid);
      procedure Move;
  end;

implementation
  const
    INITIAL_RADIUS = 2;
    INITIAL_MASS = 4.0;
    GRAVITY = 10.0;

  procedure TAsteroid.Clear;
  begin
    IsActive := false;

    Radius := INITIAL_RADIUS;
    Mass := INITIAL_MASS;

    VelocityX := 0.0;
    VelocityY := 0.0;

    AccelerationX := 0.0;
    AccelerationY := 0.0;
  end;

  procedure TAsteroid.Randomize(const Width: Integer; const Height: Integer);
  begin
    Clear();

    X := 1.0 + Random(Width - 1) - (Width div 2);
    Y := 1.0 + Random(Height - 1) - (Height div 2);

    IsActive := true;
  end;

  procedure TAsteroid.Initialize(const newX: Integer; const newY: Integer);
  begin
    Clear();

    X := newX;
    Y := newY;

    IsActive := true;
  end;

  function TAsteroid.MergeIfAdjacent(const OtherAsteroid: TAsteroid): Boolean;
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

      Radius := Ceil(Sqrt(Mass));

      OtherAsteroid.Clear();
    end;

    Result := isAdjacent;
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

