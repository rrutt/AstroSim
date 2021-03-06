unit AstroSimMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, AstroSimSpace;

const
  PRODUCT_VERSION = '1.3.0+20220406';

type

  { TAstroSimMainForm }

  TAstroSimMainForm = class(TForm)
    ButtonStep: TButton;
    ButtonPause: TButton;
    ButtonStart: TButton;
    ButtonRandomize: TButton;
    Label1: TLabel;
    LabelAsteroidCount: TLabel;
    SpinEditAsteroidCount: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRandomizeClick(Sender: TObject);
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
  Caption := Caption + '  (Version ' + PRODUCT_VERSION + ')';

  Timer1.Interval := 10; // milliseconds
  Timer1.Enabled := false;

  SpinEditAsteroidCount.MinValue := 1;
  SpinEditAsteroidCount.Increment := 5;
  SpinEditAsteroidCount.MaxValue := MAXIMUM_ASTEROID_COUNT;
  SpinEditAsteroidCount.Value := DEFAULT_ASTEROID_COUNT;

  Space := TAstroSimSpace.Create(Self);
  ResizeSpace;

  LabelAsteroidCount.Caption := Format('%d', [Space.ActiveAsteroidCount]);
end;

procedure TAstroSimMainForm.ResizeSpace;
const
  BORDER_SIZE = 10;
begin
  Space.Top := ButtonRandomize.Top + ButtonRandomize.Height + BORDER_SIZE;
  Space.Left := BORDER_SIZE;
  Space.Width := Self.Width - (2 * BORDER_SIZE);
  Space.Height := Self.Height - (ButtonRandomize.Top + ButtonRandomize.Height + (2 * BORDER_SIZE));
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

  LabelAsteroidCount.Caption := Format('%d', [Space.ActiveAsteroidCount]);

  // Reenable the timer if more than one asteroid remains.
  if (Space.ActiveAsteroidCount > 1) then begin
    Timer1.Enabled := true;
  end;
end;

procedure TAstroSimMainForm.ButtonRandomizeClick(Sender: TObject);
begin
  ButtonRandomize.Enabled := false;

  ResizeSpace;

  Space.Randomize(SpinEditAsteroidCount.Value);
  Space.Paint;

  LabelAsteroidCount.Caption := Format('%d', [Space.ActiveAsteroidCount]);

  ButtonRandomize.Enabled := true;
  ButtonStart.Enabled := true;
  ButtonStep.Enabled := true;
end;

procedure TAstroSimMainForm.ButtonPauseClick(Sender: TObject);
begin
  ButtonPause.Enabled := false;

  Timer1.Enabled := false;

  ButtonRandomize.Enabled := true;
  ButtonStart.Enabled := true;
  ButtonStep.Enabled := true;
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

  LabelAsteroidCount.Caption := Format('%d', [Space.ActiveAsteroidCount]);
end;

end.

