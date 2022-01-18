program AstroSim;

{$mode objfpc}{$H+}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  AstroSimMainForm,
  Asteroid;

begin
  Randomize;

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TAstroSimMainForm, MainForm);
  Application.Run;
end.

