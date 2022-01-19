unit AstroSimMainForm;

{$mode objfpc}{$H+}

interface

{ TODO : Show Space.ActiveAsteroidCount as label on main form. }

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  AstroSimSpace;

type

  { TAstroSimMainForm }

  TAstroSimMainForm = class(TForm)
    ButtonStep: TButton;
    ButtonResume: TButton;
    ButtonPause: TButton;
    ButtonStart: TButton;
    ButtonRandomize: TButton;
    Timer1: TTimer;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRandomizeClick(Sender: TObject);
    procedure ButtonResumeClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStepClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ResizeSpace;
  end;

  var
    MainForm: TAstroSimMainForm;

implementation

{$R *.lfm}

  var
    Space: TAstroSimSpace;
    //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

procedure TAstroSimMainForm.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 10; // milliseconds
  Timer1.Enabled := false;

  ResizeSpace;
end;

procedure TAstroSimMainForm.ResizeSpace;
const
  BORDER_SIZE = 10;
begin
  Space := TAstroSimSpace.Create(Self);
  Space.Top := BORDER_SIZE;
  Space.Left := BORDER_SIZE;
  Space.Width := Self.Width - (2 * BORDER_SIZE);
  Space.Height := ButtonRandomize.Top - (2 * BORDER_SIZE);
  Space.Parent := Self;
  Space.Initialize;
  Space.DoubleBuffered := True;

  Space.Paint;
end;

procedure TAstroSimMainForm.Timer1Timer(Sender: TObject);
begin
  // Disable the timer to avoid double fire during extended iteration processing.
  Timer1.Enabled := false;

  Space.Iterate;
  Space.Paint;

  // Reenable the timer.
  Timer1.Enabled := true;
end;

procedure TAstroSimMainForm.ButtonRandomizeClick(Sender: TObject);
begin
  ButtonRandomize.Enabled := false;
  ButtonResume.Enabled := false;

  ResizeSpace;

  Space.Randomize;
  Space.Paint;

  ButtonStart.Enabled := true;
  ButtonStep.Enabled := true;
end;

procedure TAstroSimMainForm.ButtonResumeClick(Sender: TObject);
begin
  ButtonResume.Enabled := false;
  ButtonRandomize.Enabled := false;
  ButtonStep.Enabled := false;

  Timer1.Enabled := true;

  ButtonPause.Enabled := true;
end;

procedure TAstroSimMainForm.ButtonPauseClick(Sender: TObject);
begin
  ButtonPause.Enabled := false;

  Timer1.Enabled := false;

  ButtonRandomize.Enabled := true;
  ButtonStep.Enabled := true;
  ButtonResume.Enabled := true;
end;

procedure TAstroSimMainForm.ButtonStartClick(Sender: TObject);
begin
  ButtonStart.Enabled := false;
  ButtonRandomize.Enabled := false;
  ButtonStep.Enabled := false;

  Timer1.Enabled := true;

  ButtonPause.Enabled := true;
end;

procedure TAstroSimMainForm.ButtonStepClick(Sender: TObject);
begin
  Space.Iterate;
  Space.Paint;
end;

end.

