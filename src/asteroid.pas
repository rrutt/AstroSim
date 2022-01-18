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
      Velocity: Single;
      Acceleration: Single;

      procedure Randomize(const MaxX: Integer; const MaxY: Integer);
  end;

implementation
  const
    INITIAL_RADIUS = 2;
    INITIAL_MASS = 4.0;

  procedure TAsteroid.Randomize(const MaxX: Integer; const MaxY: Integer);
  begin
    IsActive := true;
    Radius := INITIAL_RADIUS;
    Mass := INITIAL_MASS;
    X := 1.0 + Random(MaxX - 1);
    Y := 1.0 + Random(MaxY - 1);
    Velocity := 0.0;
    Acceleration := 0.0;
  end;

end.

